#[path = "boundary.rs"]
pub mod boundary;
#[path = "comptime.rs"]
mod comptime;
#[path = "emit/mod.rs"]
mod emit;
#[path = "lowering/mod.rs"]
pub(super) mod lowering;
#[path = "model.rs"]
mod model;
#[path = "obligations.rs"]
mod obligations;
#[path = "planning.rs"]
pub mod planning;
#[path = "reachability.rs"]
mod reachability;
#[path = "reachability_graph.rs"]
mod reachability_graph;
#[path = "registry.rs"]
pub mod registry;
#[path = "storage.rs"]
mod storage;
#[path = "target.rs"]
mod target;
#[path = "validation/mod.rs"]
pub mod validation;

use std::collections::VecDeque;
use std::sync::Arc;

pub(crate) use mitki_codegen_core::StageIntrinsic;
use mitki_errors::Diagnostic;
use mitki_hir::hir::{ExprId, Function, NameId, NodeKind, NodeStore, PatId, StmtId, WasmLinkage};
use mitki_hir::ty::{Ty, TyKind};
use mitki_inputs::{File, PackageId};
use mitki_lower::HasPackageDecls as _;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{
    BoundaryInstanceKind, Declaration, FunctionLocation, enum_variants, struct_fields,
};
use mitki_parse::FileParse as _;
use mitki_resolve::{
    BindingId, CompilerIntrinsic, Resolver, RuntimeFunction, SignatureTypeResolver,
};
use mitki_span::Symbol;
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::SyntaxNodePtr;
use mitki_yellow::ast::{HasName as _, Node as _};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::plumbing::AsId as _;
use wasm_encoder::{BlockType, ValType};

use self::comptime::ComptimeValueKey;
pub(in crate::backend) use self::lowering::{
    function_kernel, function_legalize, function_ownership, function_wasm_ir, wrapper_mir,
};
use self::model::*;
pub(in crate::backend) use self::planning as plan;
#[allow(unused_imports)]
use self::storage::{
    Dest, DestBase, FramePlan, FramePlanBuilder, FrameSlot, FrameSlotId, FrameSlotPurpose,
    FunctionLayout, FunctionLayoutLookups, LocalPlan, LocalPlanBuilder, LocalPurpose, LocalSlot,
    MemAccess, MemAccessKind, ScratchLocalKind, TempSlot,
};
pub(crate) use self::target::{
    BoundaryTransportProfile, CallableLoweringStrategy, CapabilityMatrix, CompilationMode,
    ControlFlowStrategy, ImportProvider, MemoryModelStrategy, PhysicalWasmSignature,
    ReferenceRepresentationStrategy, ResultLoweringMode, SignatureStrategy, TargetDecisionSnapshot,
    TargetPolicies, TargetProfile,
};
pub use self::validation::{BoundaryLegalityValidator, CapabilityValidator};
use crate::abi::{
    AbiTy, BackendTy, FunctionSignature, RefKind, array_ty_bits, nominal_ty_bits,
    runtime_function_signature, stage_intrinsic_signature,
};
use crate::layout::{
    ARC_ALIGN, ARC_HEADER_SIZE, ARC_IMMORTAL_REFCNT, ARRAY_CAPACITY_OFFSET, ARRAY_LEN_OFFSET,
    AggregateKind, AggregateLayout, ArrayRuntimeLayout, EnumLayout, FieldLayout, align_to,
    layout_fields, symbol_bits,
};

pub struct Backend<'db> {
    db: &'db dyn salsa::Database,
    file: File,
    mode: CompilationMode,
    stage_root: Option<FunctionLocation<'db>>,
    pub(crate) comptime_evaluator: Arc<dyn crate::ComptimeEvaluator>,
    target: TargetPolicies,
    pub(crate) diagnostics: Vec<Diagnostic>,
    shadow_reachability: Option<plan::ReachabilityGraph<'db>>,
    shadow_obligations: Option<plan::EmissionObligations<'db>>,
}

#[derive(Clone, Default)]
struct EmissionObligationScratch<'db> {
    reachable_arrays: FxHashSet<Ty<'db>>,
    reachable_nominals: FxHashSet<Ty<'db>>,
    used_runtime_functions: FxHashSet<RuntimeFunction>,
    used_stage_intrinsics: FxHashSet<StageIntrinsic>,
    used_helpers: FxHashSet<HelperFunction>,
}

impl<'db> EmissionObligationScratch<'db> {
    fn into_plan(self, backend: &Backend<'db>) -> plan::EmissionObligations<'db> {
        let db = backend.db;
        let import_provider = backend.import_provider();
        let mut runtime_imports = self.used_runtime_functions.into_iter().collect::<Vec<_>>();
        runtime_imports.sort_by_key(|runtime| import_provider.runtime_import(*runtime));

        let mut stage_intrinsics = self.used_stage_intrinsics.into_iter().collect::<Vec<_>>();
        stage_intrinsics
            .sort_by_key(|intrinsic| import_provider.stage_intrinsic_import(*intrinsic));

        let mut helpers = self.used_helpers.into_iter().collect::<Vec<_>>();
        helpers.sort_by_key(|helper| helper.name());

        let mut reachable_arrays = self.reachable_arrays.into_iter().collect::<Vec<_>>();
        reachable_arrays.sort_by_key(|ty| {
            crate::capability::supported_type_runtime_descriptor(db, *ty)
                .map_or_else(|| array_ty_bits(*ty), |descriptor| descriptor.graph_node().0)
        });

        let mut reachable_nominals = self.reachable_nominals.into_iter().collect::<Vec<_>>();
        reachable_nominals.sort_by_key(|ty| {
            crate::capability::supported_type_runtime_descriptor(db, *ty)
                .map_or_else(|| nominal_ty_bits(*ty), |descriptor| descriptor.graph_node().0)
        });
        let runtime_import_needs =
            runtime_imports.iter().copied().map(plan::RuntimeImportNeed::Runtime).collect();
        let layout_needs = reachable_arrays
            .iter()
            .copied()
            .map(plan::LayoutNeed::Array)
            .chain(reachable_nominals.iter().copied().map(plan::LayoutNeed::Nominal))
            .collect();

        plan::EmissionObligations {
            runtime_imports,
            stage_intrinsics,
            helpers,
            reachable_arrays,
            reachable_nominals,
            runtime_import_needs,
            callable_adapters: Vec::new(),
            boundary_wrappers: Vec::new(),
            canonical_support: Vec::new(),
            layout_needs,
        }
    }

    fn register_nominals_in_ty(&mut self, db: &'db dyn salsa::Database, ty: Ty<'db>) {
        match ty.kind(db) {
            TyKind::String => {
                self.used_runtime_functions.insert(RuntimeFunction::Dealloc);
            }
            TyKind::Array(item) => {
                if !self.reachable_arrays.insert(ty) {
                    return;
                }
                self.used_runtime_functions.insert(RuntimeFunction::Alloc);
                self.used_runtime_functions.insert(RuntimeFunction::Dealloc);
                self.used_helpers.insert(HelperFunction::ArcRetain);
                self.used_helpers.insert(HelperFunction::ArcRelease);
                self.register_nominals_in_ty(db, *item);
            }
            TyKind::Struct(struct_ty) => {
                if !self.reachable_nominals.insert(ty) {
                    return;
                }
                self.used_runtime_functions.insert(RuntimeFunction::Alloc);
                self.used_runtime_functions.insert(RuntimeFunction::Dealloc);
                self.used_helpers.insert(HelperFunction::ArcRetain);
                self.used_helpers.insert(HelperFunction::ArcRelease);
                for (_, field_ty) in struct_fields(db, *struct_ty) {
                    self.register_nominals_in_ty(db, *field_ty);
                }
            }
            TyKind::Enum(enum_ty) => {
                if !self.reachable_nominals.insert(ty) {
                    return;
                }
                self.used_runtime_functions.insert(RuntimeFunction::Alloc);
                self.used_runtime_functions.insert(RuntimeFunction::Dealloc);
                self.used_helpers.insert(HelperFunction::ArcRetain);
                self.used_helpers.insert(HelperFunction::ArcRelease);
                for (_, fields) in enum_variants(db, *enum_ty) {
                    for field_ty in fields {
                        self.register_nominals_in_ty(db, *field_ty);
                    }
                }
            }
            TyKind::Tuple(items) | TyKind::Union(items) | TyKind::Inter(items) => {
                for &item in items {
                    self.register_nominals_in_ty(db, item);
                }
            }
            TyKind::Record(fields) => {
                for (_, field_ty) in fields {
                    self.register_nominals_in_ty(db, *field_ty);
                }
            }
            TyKind::ExternStruct(struct_ty) => {
                for (_, field_ty) in struct_fields(db, *struct_ty) {
                    self.register_nominals_in_ty(db, *field_ty);
                }
            }
            TyKind::Pointer { pointee, .. } => {
                self.register_nominals_in_ty(db, *pointee);
            }
            TyKind::Function { inputs, output } => {
                for &input in inputs {
                    self.register_nominals_in_ty(db, input);
                }
                self.register_nominals_in_ty(db, *output);
            }
            _ => {}
        }
    }
}

impl<'db> Backend<'db> {
    pub fn new_file_with_options(
        db: &'db dyn salsa::Database,
        file: File,
        options: crate::CompileOptions,
        comptime_evaluator: Arc<dyn crate::ComptimeEvaluator>,
    ) -> Self {
        Self::new_file_with_profile(
            db,
            file,
            TargetProfile::wasm_core_v2_m32(),
            options,
            comptime_evaluator,
        )
    }

    pub fn new_stage_with_options(
        db: &'db dyn salsa::Database,
        file: File,
        root: FunctionLocation<'db>,
        options: crate::CompileOptions,
        comptime_evaluator: Arc<dyn crate::ComptimeEvaluator>,
    ) -> Self {
        Self::new_stage_with_profile(
            db,
            file,
            root,
            TargetProfile::wasm_core_v2_m32(),
            options,
            comptime_evaluator,
        )
    }

    pub(crate) fn new_file_with_profile(
        db: &'db dyn salsa::Database,
        file: File,
        profile: TargetProfile,
        _options: crate::CompileOptions,
        comptime_evaluator: Arc<dyn crate::ComptimeEvaluator>,
    ) -> Self {
        Self::new(db, file, CompilationMode::Runtime, None, profile, comptime_evaluator)
    }

    pub(crate) fn new_stage_with_profile(
        db: &'db dyn salsa::Database,
        file: File,
        root: FunctionLocation<'db>,
        profile: TargetProfile,
        _options: crate::CompileOptions,
        comptime_evaluator: Arc<dyn crate::ComptimeEvaluator>,
    ) -> Self {
        Self::new(db, file, CompilationMode::Stage, Some(root), profile, comptime_evaluator)
    }

    fn new(
        db: &'db dyn salsa::Database,
        file: File,
        mode: CompilationMode,
        stage_root: Option<FunctionLocation<'db>>,
        profile: TargetProfile,
        comptime_evaluator: Arc<dyn crate::ComptimeEvaluator>,
    ) -> Self {
        Self {
            db,
            file,
            mode,
            stage_root,
            comptime_evaluator,
            target: TargetPolicies::for_profile(profile),
            diagnostics: Vec::new(),
            shadow_reachability: None,
            shadow_obligations: None,
        }
    }

    fn boundary_instance_key(
        &self,
        kind: BoundaryInstanceKind,
        instance: &mitki_lower::item::scope::BoundaryInstanceLocation<'db>,
    ) -> Option<InstanceKey<'db>> {
        (instance.kind(self.db) == kind).then_some(())?;
        let origin = instance.origin(self.db)?;
        Some(InstanceKey { location: origin, type_args: instance.type_args(self.db).clone() })
    }

    fn has_declared_boundary_instance(
        &self,
        kind: BoundaryInstanceKind,
        expected: &InstanceKey<'db>,
    ) -> bool {
        PackageId::new(self.db, self.file).package_decls(self.db).declarations().iter().any(
            |declaration| {
                let Declaration::BoundaryInstance(instance) = declaration else {
                    return false;
                };
                self.boundary_instance_key(kind, instance)
                    .is_some_and(|instance| instance == *expected)
            },
        )
    }
    fn function_return_ty(
        &self,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) -> Ty<'db> {
        if !function.ret_type().is_zero() {
            let signature = location.signature(self.db);
            let resolver = SignatureTypeResolver::new(self.db, location, signature);
            return resolver
                .resolve(signature.ret_type(self.db))
                .unwrap_or_else(|_| Ty::new(self.db, TyKind::Tuple(Vec::new())));
        }
        inference
            .type_of_node(function.body())
            .unwrap_or_else(|| Ty::new(self.db, TyKind::Tuple(Vec::new())))
    }

    fn specialize_ty(&self, instance: &InstanceKey<'db>, ty: Ty<'db>) -> Ty<'db> {
        specialize_ty(self.db, ty, &instance.type_args)
    }

    fn set_shadow_state(
        &mut self,
        reachability: plan::ReachabilityGraph<'db>,
        obligations: plan::EmissionObligations<'db>,
    ) {
        self.shadow_reachability = Some(reachability);
        self.shadow_obligations = Some(obligations);
    }

    fn specialized_expr_ty(
        &self,
        instance: &InstanceKey<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        expr: ExprId,
    ) -> Option<Ty<'db>> {
        let function = instance.location.hir_function(self.db).function(self.db);
        let nodes = function.node_store();
        if nodes.node_kind(expr) == NodeKind::Name
            && let Some(name_id) = nodes.as_name(expr)
        {
            let symbol = nodes.name(name_id);
            let mut resolver = Resolver::new(self.db, instance.location);
            let guard = resolver.scopes_for_node(expr);
            let resolution = resolver.resolve_value_binding(symbol);
            resolver.reset(guard);
            if let Some(BindingId::Local(binding) | BindingId::Param(binding)) = resolution {
                return inference
                    .type_of_node(binding.into())
                    .map(|ty| self.specialize_ty(instance, ty));
            }
        }

        inference.type_of_node(expr).map(|ty| self.specialize_ty(instance, ty))
    }

    pub fn file_range(&self) -> mitki_errors::TextRange {
        self.file.parse(self.db).syntax_node().trimmed_range()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub(in crate::backend) fn compilation_mode(&self) -> CompilationMode {
        self.mode
    }

    pub(in crate::backend) fn diagnostic_at_function(
        &self,
        location: FunctionLocation<'db>,
        message: impl Into<String>,
        range: mitki_errors::TextRange,
    ) -> Diagnostic {
        Diagnostic::error(message, range)
            .with_file(location.file(self.db).path(self.db).to_string())
    }

    pub(in crate::backend) fn stage_root(&self) -> Option<FunctionLocation<'db>> {
        self.stage_root
    }

    pub(in crate::backend) fn target_profile(&self) -> TargetProfile {
        self.target.profile()
    }

    pub(in crate::backend) fn target_policies(&self) -> TargetPolicies {
        self.target
    }

    pub(in crate::backend) fn import_provider(&self) -> ImportProvider {
        self.target.import_provider()
    }

    pub(in crate::backend) fn boundary_transport_profile(&self) -> BoundaryTransportProfile {
        self.target.boundary_transport()
    }

    pub(in crate::backend) fn callable_lowering_strategy(&self) -> CallableLoweringStrategy {
        self.target.callable_lowering()
    }

    pub(in crate::backend) fn memory_model_strategy(&self) -> MemoryModelStrategy {
        self.target.memory_model()
    }

    pub(in crate::backend) fn signature_strategy(&self) -> SignatureStrategy {
        self.target.signature()
    }

    pub(in crate::backend) fn control_flow_strategy(&self) -> ControlFlowStrategy {
        self.target.control_flow()
    }

    pub(in crate::backend) fn reference_representation_strategy(
        &self,
    ) -> ReferenceRepresentationStrategy {
        self.target.reference_representation()
    }

    pub(in crate::backend) fn capability_matrix(&self) -> CapabilityMatrix {
        self.target.capability_matrix()
    }

    pub(in crate::backend) fn target_decision_snapshot(&self) -> TargetDecisionSnapshot {
        self.target.decision_snapshot()
    }

    fn function_range(&self, location: FunctionLocation<'db>) -> mitki_errors::TextRange {
        SyntaxNodePtr::new(location.source(self.db).syntax()).range
    }
}

fn node_range(
    backend: &Backend<'_>,
    location: FunctionLocation<'_>,
    source_map: &mitki_lower::hir::FunctionSourceMap,
    expr: ExprId,
) -> mitki_errors::TextRange {
    source_map
        .try_node_syntax(expr)
        .map_or_else(|| backend.function_range(location), |ptr| ptr.range)
}

fn ranges_intersect(a: mitki_errors::TextRange, b: mitki_errors::TextRange) -> bool {
    a.start() < b.end() && b.start() < a.end()
}

fn stmt_as_expr(nodes: &NodeStore<'_>, stmt: StmtId) -> Option<ExprId> {
    match nodes.node_kind(stmt) {
        NodeKind::Name => nodes.as_name(stmt).map(Into::into),
        NodeKind::True => nodes.as_true(stmt).map(Into::into),
        NodeKind::False => nodes.as_false(stmt).map(Into::into),
        NodeKind::Error => nodes.as_error(stmt).map(Into::into),
        NodeKind::Int => nodes.as_int(stmt).map(Into::into),
        NodeKind::Float => nodes.as_float(stmt).map(Into::into),
        NodeKind::String => nodes.as_string(stmt).map(Into::into),
        NodeKind::Char => nodes.as_char(stmt).map(Into::into),
        NodeKind::Tuple => nodes.as_tuple(stmt).map(Into::into),
        NodeKind::Array => nodes.as_array(stmt).map(Into::into),
        NodeKind::ArrayRepeat => nodes.as_array_repeat(stmt).map(Into::into),
        NodeKind::Call => nodes.as_call(stmt).map(Into::into),
        NodeKind::Field => nodes.as_field(stmt).map(Into::into),
        NodeKind::Binary => nodes.as_binary(stmt).map(Into::into),
        NodeKind::Postfix => nodes.as_postfix(stmt).map(Into::into),
        NodeKind::Prefix => nodes.as_prefix(stmt).map(Into::into),
        NodeKind::LoopExpr => nodes.as_loop_expr(stmt).map(Into::into),
        NodeKind::BreakExpr => nodes.as_break_expr(stmt).map(Into::into),
        NodeKind::ContinueExpr => nodes.as_continue_expr(stmt).map(Into::into),
        NodeKind::If => nodes.as_if(stmt).map(Into::into),
        NodeKind::Match => nodes.as_match(stmt).map(Into::into),
        NodeKind::Closure => nodes.as_closure(stmt).map(Into::into),
        NodeKind::Block => nodes.as_block(stmt).map(Into::into),
        NodeKind::UnsafeBlock => nodes.as_unsafe_block(stmt).map(Into::into),
        NodeKind::StructExpr => nodes.as_struct_expr(stmt).map(Into::into),
        _ => None,
    }
}

fn specialize_ty<'db>(db: &'db dyn salsa::Database, ty: Ty<'db>, type_args: &[Ty<'db>]) -> Ty<'db> {
    match ty.kind(db) {
        TyKind::Var(id) => type_args.get(*id as usize).copied().unwrap_or(ty),
        TyKind::Array(item) => {
            let item = specialize_ty(db, *item, type_args);
            Ty::new(db, TyKind::Array(item))
        }
        TyKind::Tuple(items) => {
            let items = items.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            Ty::new(db, TyKind::Tuple(items))
        }
        TyKind::Record(fields) => {
            let fields = fields
                .iter()
                .map(|(name, field_ty)| (*name, specialize_ty(db, *field_ty, type_args)))
                .collect();
            Ty::new(db, TyKind::Record(fields))
        }
        TyKind::Function { inputs, output } => {
            let inputs = inputs.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            let output = specialize_ty(db, *output, type_args);
            Ty::new(db, TyKind::Function { inputs, output })
        }
        TyKind::Union(items) => {
            let items = items.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            Ty::new(db, TyKind::Union(items))
        }
        TyKind::Inter(items) => {
            let items = items.iter().map(|&item| specialize_ty(db, item, type_args)).collect();
            Ty::new(db, TyKind::Inter(items))
        }
        TyKind::Pointer { mutable, pointee } => {
            let pointee = specialize_ty(db, *pointee, type_args);
            Ty::new(db, TyKind::Pointer { mutable: *mutable, pointee })
        }
        _ => ty,
    }
}

fn parse_int_literal(literal: Option<Symbol<'_>>, db: &dyn salsa::Database) -> Result<i64, String> {
    let Some(literal) = literal else {
        return Err("Wasm backend could not read integer literal".to_owned());
    };

    let text = literal.text(db).replace('_', "");
    let (radix, digits) = if let Some(rest) = text.strip_prefix("0b") {
        (2, rest)
    } else if let Some(rest) = text.strip_prefix("0o") {
        (8, rest)
    } else if let Some(rest) = text.strip_prefix("0x") {
        (16, rest)
    } else {
        (10, text.as_str())
    };

    i64::from_str_radix(digits, radix)
        .map_err(|error| format!("Wasm backend could not parse integer literal `{text}`: {error}"))
}

fn parse_float_literal(
    literal: Option<Symbol<'_>>,
    db: &dyn salsa::Database,
) -> Result<f64, String> {
    let Some(literal) = literal else {
        return Err("Wasm backend could not read float literal".to_owned());
    };

    let text = literal.text(db).replace('_', "");
    text.parse::<f64>()
        .map_err(|error| format!("Wasm backend could not parse float literal `{text}`: {error}"))
}

fn decode_char_literal(
    literal: Option<Symbol<'_>>,
    db: &dyn salsa::Database,
) -> Result<char, String> {
    let Some(literal) = literal else {
        return Err("Wasm backend could not read char literal".to_owned());
    };

    let text = literal.text(db);
    let Some(content) = text.strip_prefix('\'').and_then(|text| text.strip_suffix('\'')) else {
        return Err(format!("Wasm backend could not decode char literal `{text}`"));
    };

    let mut chars = content.chars();
    let ch = match chars.next() {
        Some('\\') => match chars.next() {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('0') => '\0',
            Some('\\') => '\\',
            Some('"') => '"',
            Some('\'') => '\'',
            Some(other) => other,
            None => return Err(format!("Wasm backend found an incomplete escape in `{text}`")),
        },
        Some(ch) => ch,
        None => return Err(format!("Wasm backend could not decode empty char literal `{text}`")),
    };

    if chars.next().is_some() {
        return Err(format!("Wasm backend only supports single char literals, found `{text}`"));
    }

    Ok(ch)
}

fn decode_string_literal(
    literal: Option<Symbol<'_>>,
    db: &dyn salsa::Database,
) -> Result<Vec<u8>, String> {
    let Some(literal) = literal else {
        return Err("Wasm backend could not read string literal".to_owned());
    };

    let text = literal.text(db);
    let Some(content) = text.strip_prefix('"').and_then(|text| text.strip_suffix('"')) else {
        return Err(format!("Wasm backend could not decode string literal `{text}`"));
    };

    let mut decoded = String::new();
    let mut chars = content.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            decoded.push(ch);
            continue;
        }

        let escaped = chars
            .next()
            .ok_or_else(|| format!("Wasm backend found an incomplete escape in `{text}`"))?;
        match escaped {
            'n' => decoded.push('\n'),
            'r' => decoded.push('\r'),
            't' => decoded.push('\t'),
            '0' => decoded.push('\0'),
            '\\' => decoded.push('\\'),
            '"' => decoded.push('"'),
            '\'' => decoded.push('\''),
            other => decoded.push(other),
        }
    }

    Ok(decoded.into_bytes())
}

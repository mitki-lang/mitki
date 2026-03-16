mod match_analysis;
pub mod ownership;
pub mod semantics;
pub mod wasm_boundary_legality;

use std::collections::VecDeque;

use mitki_errors::Diagnostic;
use mitki_hir::hir::{ExprId, Function, NodeKind, NodeStore, PatId, StmtId, WasmLinkage};
use mitki_hir::ty::{Ty, TyKind};
use mitki_inputs::PackageId;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{BoundaryInstanceKind, Declaration, FunctionLocation};
use mitki_lower::{HasItemDecls as _, HasPackageDecls as _};
use mitki_parse::FileParse as _;
use mitki_resolve::{
    BindingId, Resolver, is_reserved_compiler_name, lookup_runtime_function,
    resolve_method_for_receiver,
};
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::ast::Node as _;
use rustc_hash::FxHashSet;
use salsa::plumbing::AsId as _;
pub use semantics::{ResolveIntent, Semantics};

#[salsa::tracked(returns(ref), no_eq)]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) -> Vec<Diagnostic> {
    use mitki_span::IntoSymbol as _;
    use mitki_yellow::ast::HasName as _;

    let mut diagnostics = file.parse(db).diagnostics().to_owned();
    let mut seen_boundary_instances = FxHashSet::default();

    for declaration in PackageId::new(db, file).package_decls(db).declarations() {
        match declaration {
            Declaration::Function(func) => {
                let source = func.source(db);
                if let Some(name_node) = source.name() {
                    let symbol = name_node.as_str().into_symbol(db);
                    if let Some(runtime) = lookup_runtime_function(db, symbol) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "`{}` is a reserved runtime function name",
                                runtime.source_name()
                            ),
                            name_node.text_range(),
                        ));
                    }
                    if is_reserved_compiler_name(db, symbol) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "`{}` is a reserved compiler intrinsic name",
                                name_node.as_str()
                            ),
                            name_node.text_range(),
                        ));
                    }
                }
                diagnostics.extend(check_function(db, *func).iter().cloned());
            }
            Declaration::BoundaryInstance(instance) => {
                diagnostics.extend(wasm_boundary_legality::check_boundary_instance_legality(
                    db, *instance,
                ));
                if let Some(origin) = instance.origin(db) {
                    let key = (
                        instance.kind(db),
                        origin.as_id().as_bits(),
                        instance.type_args(db).clone(),
                    );
                    if !seen_boundary_instances.insert(key) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "duplicate boundary instance declaration for `{}`",
                                instance.source(db).name().map_or("", |name| name.as_str())
                            ),
                            mitki_yellow::SyntaxNodePtr::new(instance.source(db).syntax()).range,
                        ));
                    }
                }
            }
            Declaration::Struct(struct_) => {
                let source = struct_.source(db);
                if let Some(name_node) = source.name() {
                    let symbol = name_node.as_str().into_symbol(db);
                    if is_reserved_compiler_name(db, symbol) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "`{}` is a reserved compiler intrinsic name",
                                name_node.as_str()
                            ),
                            name_node.text_range(),
                        ));
                    }
                }
                diagnostics.extend(check_struct_destructor_legality(db, *struct_));
            }
            Declaration::Enum(enum_) => {
                let source = enum_.source(db);
                if let Some(name_node) = source.name() {
                    let symbol = name_node.as_str().into_symbol(db);
                    if is_reserved_compiler_name(db, symbol) {
                        diagnostics.push(Diagnostic::error(
                            format!(
                                "`{}` is a reserved compiler intrinsic name",
                                name_node.as_str()
                            ),
                            name_node.text_range(),
                        ));
                    }
                }
                diagnostics.extend(check_enum_destructor_legality(db, *enum_));
            }
        }
    }

    diagnostics
}

fn check_struct_destructor_legality(
    db: &dyn salsa::Database,
    location: mitki_lower::item::scope::StructLocation<'_>,
) -> Vec<Diagnostic> {
    use mitki_yellow::ast::Node as _;

    let source = location.source(db);
    let Some(field_list) = source.field_list() else {
        return Vec::new();
    };
    let destructors = field_list.destructors().collect::<Vec<_>>();
    let mut diagnostics = Vec::new();
    if destructors.len() > 1 {
        for destructor in destructors.iter().skip(1) {
            diagnostics.push(Diagnostic::error(
                "duplicate destructor declaration",
                mitki_yellow::SyntaxNodePtr::new(destructor.syntax()).range,
            ));
        }
    }
    if source.is_extern() && !destructors.is_empty() {
        diagnostics.push(Diagnostic::error(
            "`extern struct` cannot declare a destructor",
            mitki_yellow::SyntaxNodePtr::new(destructors[0].syntax()).range,
        ));
    }
    diagnostics.extend(destructors.iter().flat_map(check_destructor_signature));
    diagnostics
}

fn check_enum_destructor_legality(
    db: &dyn salsa::Database,
    location: mitki_lower::item::scope::EnumLocation<'_>,
) -> Vec<Diagnostic> {
    use mitki_yellow::ast::Node as _;

    let source = location.source(db);
    let Some(variant_list) = source.variant_list() else {
        return Vec::new();
    };
    let destructors = variant_list.destructors().collect::<Vec<_>>();
    let mut diagnostics = Vec::new();
    if destructors.len() > 1 {
        for destructor in destructors.iter().skip(1) {
            diagnostics.push(Diagnostic::error(
                "duplicate destructor declaration",
                mitki_yellow::SyntaxNodePtr::new(destructor.syntax()).range,
            ));
        }
    }
    diagnostics.extend(destructors.iter().flat_map(check_destructor_signature));
    diagnostics
}

fn check_destructor_signature(
    destructor: &mitki_yellow::ast::DestructorDef<'_>,
) -> Vec<Diagnostic> {
    use mitki_yellow::ast::{Node as _, Pattern};

    let mut diagnostics = Vec::new();
    let params =
        destructor.params().map(|params| params.iter().collect::<Vec<_>>()).unwrap_or_default();
    let range = mitki_yellow::SyntaxNodePtr::new(destructor.syntax()).range;
    if params.len() != 1 {
        diagnostics.push(Diagnostic::error(
            "destructor must declare exactly one parameter: `drop(var self) { ... }`",
            range,
        ));
        return diagnostics;
    }

    let param = &params[0];
    if param.ty().is_some() {
        diagnostics.push(Diagnostic::error(
            "destructor parameter type is implicit; write `drop(var self) { ... }`",
            mitki_yellow::SyntaxNodePtr::new(param.syntax()).range,
        ));
    } else if !param.is_mutable() {
        diagnostics.push(Diagnostic::error(
            "destructor parameter must be declared as `var self`",
            mitki_yellow::SyntaxNodePtr::new(param.syntax()).range,
        ));
    }
    match param.pattern() {
        Some(Pattern::Binding(binding))
            if binding.name().is_some_and(|name| name.as_str() == "self") => {}
        _ => diagnostics.push(Diagnostic::error(
            "destructor parameter must be exactly `var self`",
            mitki_yellow::SyntaxNodePtr::new(param.syntax()).range,
        )),
    }

    diagnostics
}

#[salsa::tracked(returns(ref), no_eq)]
pub fn check_function(db: &dyn salsa::Database, func: FunctionLocation<'_>) -> Vec<Diagnostic> {
    use mitki_hir::ty::TyKind;
    use mitki_lower::hir::HasFunction as _;
    use mitki_typeck::infer;
    use mitki_typeck::infer::Inferable as _;
    use mitki_yellow::ast::Node as _;

    let source = func.source(db);
    let source_map = func.hir_function(db).source_map(db);
    let function = func.hir_function(db).function(db);
    let nodes = function.node_store();
    let fallback_range = mitki_yellow::SyntaxNodePtr::new(source.syntax()).range;
    let mut diagnostics = wasm_boundary_legality::check_function_boundary_legality(db, func);

    let node_range =
        |node_id| source_map.try_node_syntax(node_id).map_or(fallback_range, |ptr| ptr.range);
    let pat_range =
        |pat_id| source_map.try_pat_syntax(pat_id).map_or(fallback_range, |ptr| ptr.range);
    let type_range =
        |ty_id| source_map.try_type_syntax(ty_id).map_or(fallback_range, |ptr| ptr.range);

    let context_label = |node_id| match nodes.node_kind(node_id) {
        NodeKind::Call => "call expression",
        NodeKind::Closure => "closure",
        NodeKind::If => "if expression",
        NodeKind::Match => "match expression",
        NodeKind::LoopExpr => "loop expression",
        NodeKind::Tuple => "tuple expression",
        NodeKind::StructExpr => "struct expression",
        _ => "expression",
    };

    let inference = func.infer(db);

    for diagnostic in inference.diagnostics() {
        let (message, range) = salsa::plumbing::attach(db, || match diagnostic.kind() {
            infer::DiagnosticKind::UnresolvedIdent(node_id) => {
                ("Unresolved identifier".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::UnresolvedType(ty_id, name) => {
                (format!("Unknown type `{}`", name.text(db)), type_range(*ty_id))
            }
            infer::DiagnosticKind::TypeMismatch(node_id, actual, expected) => (
                format!(
                    "expected `{expected}`, found `{actual}`",
                    expected = expected.display(db),
                    actual = actual.display(db)
                ),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::PatternTypeMismatch(pattern_id, actual, expected) => (
                format!(
                    "expected `{expected}`, found `{actual}`",
                    expected = expected.display(db),
                    actual = actual.display(db)
                ),
                pat_range(*pattern_id),
            ),
            infer::DiagnosticKind::UnknownType(node_id) => {
                ("cannot infer type".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::ExpectedValueFoundType(node_id, ty) => {
                (format!("expected value, found type `{}`", ty.display(db)), node_range(*node_id))
            }
            infer::DiagnosticKind::CallArityMismatch(node_id, expected, actual) => {
                (format!("expected {expected} argument(s), found {actual}"), node_range(*node_id))
            }
            infer::DiagnosticKind::CallNonFunction(node_id, ty) => {
                (format!("expected function, found `{}`", ty.display(db)), node_range(*node_id))
            }
            infer::DiagnosticKind::ClosureArityMismatch(node_id, actual, expected) => {
                (format!("expected {expected} parameter(s), found {actual}"), node_range(*node_id))
            }
            infer::DiagnosticKind::InvalidBinaryOp(node_id, op, lhs_ty, rhs_ty) => (
                format!(
                    "cannot apply `{}` to `{}` and `{}`",
                    op.text(db),
                    lhs_ty.display(db),
                    rhs_ty.display(db)
                ),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::InvalidPrefixOp(node_id, op, ty) => (
                format!("cannot apply `{}` to `{}`", op.text(db), ty.display(db)),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::InvalidPostfixOp(node_id, op, ty) => (
                format!("cannot apply postfix `{}` to `{}`", op.text(db), ty.display(db)),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::MissingElseBranch(node_id) => {
                ("missing `else` branch".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::MissingParameterType(node_id) => {
                let message = inference.type_of_node(*node_id).map_or_else(
                    || "Parameter type annotation is required.".to_owned(),
                    |inferred| {
                        let inferred_kind = inferred.kind(db);
                        let is_unhelpful_hint = matches!(inferred_kind, TyKind::Unknown)
                            || matches!(inferred_kind, TyKind::Tuple(items) if items.is_empty());

                        if is_unhelpful_hint {
                            "Parameter type annotation is required.".to_owned()
                        } else {
                            format!(
                                "Parameter type annotation is required. Inferred `{}`.",
                                inferred.display(db)
                            )
                        }
                    },
                );
                (message, node_range(*node_id))
            }
            infer::DiagnosticKind::MissingInitializer(node_id) => {
                ("missing initializer".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::TupleArityMismatch(node_id, expected, actual) => {
                (format!("expected {expected} element(s), found {actual}"), node_range(*node_id))
            }
            infer::DiagnosticKind::MissingStructField(node_id, name) => {
                (format!("missing field `{}`", name.text(db)), node_range(*node_id))
            }
            infer::DiagnosticKind::UnknownStructField(node_id, name) => {
                (format!("unknown field `{}`", name.text(db)), node_range(*node_id))
            }
            infer::DiagnosticKind::NotAStruct(node_id, ty) => {
                (format!("`{}` is not a struct", ty.display(db)), node_range(*node_id))
            }
            infer::DiagnosticKind::NotAnEnum(node_id, ty) => {
                (format!("`{}` is not an enum", ty.display(db)), node_range(*node_id))
            }
            infer::DiagnosticKind::DuplicatePatternBinding(node_id) => {
                ("duplicate binding in pattern".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::AmbiguousUnionPattern(pattern_id, union_ty) => (
                format!("pattern matches multiple members of union `{}`", union_ty.display(db)),
                pat_range(*pattern_id),
            ),
            infer::DiagnosticKind::RefutablePattern(node_id) => (
                "refutable patterns are only allowed in `match` arms".to_owned(),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::UnsupportedFloatPattern(node_id) => {
                ("float patterns are not supported".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::BreakOutsideLoop(node_id) => {
                ("`break` is only allowed inside `loop`".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::ContinueOutsideLoop(node_id) => {
                ("`continue` is only allowed inside `loop`".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::CompilerIntrinsicMustBeCalled(node_id, intrinsic) => (
                format!("`{}` can only be used in call position", intrinsic.source_name()),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::InvalidComptimeCall(node_id) => (
                "comptime requires a direct call to a top-level `comptime fun`".to_owned(),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::ComptimeTargetMustBeZeroArg(node_id) => (
                "comptime requires the target function to have no parameters".to_owned(),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::ComptimeTargetMustNotBeGeneric(node_id) => {
                ("comptime does not support generic functions".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::ComptimeTargetMustReturnSupportedType(node_id, ty) => (
                format!(
                    "comptime requires the target function to return a runtime-lowerable value, \
                     found `{}`",
                    ty.display(db)
                ),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::ReflectionOnlyInComptime(node_id, intrinsic) => (
                format!("`{}` is only allowed inside `comptime fun`", intrinsic.source_name()),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::InvalidReflectionTarget(node_id, intrinsic, expected) => (
                format!("`{}` requires {}", intrinsic.source_name(), expected),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::UnsafeOperationRequiresUnsafeContext(node_id) => (
                "unsafe operation requires an `unsafe` block or `unsafe fun`".to_owned(),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::InvalidUnsafeIntrinsicArgument(node_id, intrinsic, expected) => {
                (
                    format!("`{}` requires {}", intrinsic.source_name(), expected),
                    node_range(*node_id),
                )
            }
            infer::DiagnosticKind::InvalidUnsafeIntrinsicResult(node_id, intrinsic, expected) => (
                format!("`{}` requires {}", intrinsic.source_name(), expected),
                node_range(*node_id),
            ),
            infer::DiagnosticKind::InvalidAssignmentTarget(node_id) => {
                ("invalid assignment target".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::AssignmentRequiresMutable(node_id) => {
                ("assignment requires a mutable binding".to_owned(), node_range(*node_id))
            }
            infer::DiagnosticKind::MutableArgumentRequiresPlace(node_id) => {
                ("mutable parameter requires a mutable place".to_owned(), node_range(*node_id))
            }
        });

        let message = if let Some(context) = diagnostic.context() {
            format!("In {}: {message}", context_label(context))
        } else {
            message
        };

        diagnostics.push(Diagnostic::error(message, range))
    }

    diagnostics.extend(match_analysis::check_function_matches(db, func));

    diagnostics
}

#[salsa::tracked(returns(ref), no_eq)]
pub fn check_runtime_file(db: &dyn salsa::Database, file: mitki_inputs::File) -> Vec<Diagnostic> {
    let roots = file
        .item_decls(db)
        .declarations()
        .iter()
        .filter_map(|declaration| match declaration {
            Declaration::Function(function) => {
                let linkage = function.hir_function(db).function(db).linkage();
                matches!(linkage, WasmLinkage::ImplicitMainExport | WasmLinkage::Export)
                    .then_some(*function)
            }
            Declaration::BoundaryInstance(instance) => (instance.kind(db)
                == BoundaryInstanceKind::Export)
                .then(|| instance.origin(db))
                .flatten(),
            Declaration::Struct(_) | Declaration::Enum(_) => None,
        })
        .collect::<Vec<_>>();

    runtime_reachability_diagnostics(db, roots)
}

#[salsa::tracked(returns(ref), no_eq)]
pub fn check_runtime_function(
    db: &dyn salsa::Database,
    root: FunctionLocation<'_>,
) -> Vec<Diagnostic> {
    runtime_reachability_diagnostics(db, [root])
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum RuntimeWorkItem<'db> {
    Function(FunctionLocation<'db>),
    Closure { owner: FunctionLocation<'db>, expr: ExprId },
}

fn runtime_reachability_diagnostics<'db>(
    db: &'db dyn salsa::Database,
    roots: impl IntoIterator<Item = FunctionLocation<'db>>,
) -> Vec<Diagnostic> {
    let boundary_roots = roots.into_iter().collect::<Vec<_>>();
    let mut checker = RuntimeReachabilityChecker {
        db,
        diagnostics: Vec::new(),
        pending: boundary_roots.iter().copied().map(RuntimeWorkItem::Function).collect(),
        boundary_roots: boundary_roots.iter().copied().collect(),
        seen: FxHashSet::default(),
    };

    while let Some(item) = checker.pending.pop_front() {
        if !checker.seen.insert(item) {
            continue;
        }
        checker.visit(item);
    }

    checker.diagnostics
}

struct RuntimeReachabilityChecker<'db> {
    db: &'db dyn salsa::Database,
    diagnostics: Vec<Diagnostic>,
    pending: VecDeque<RuntimeWorkItem<'db>>,
    boundary_roots: FxHashSet<FunctionLocation<'db>>,
    seen: FxHashSet<RuntimeWorkItem<'db>>,
}

impl<'db> RuntimeReachabilityChecker<'db> {
    fn visit(&mut self, item: RuntimeWorkItem<'db>) {
        match item {
            RuntimeWorkItem::Function(location) => {
                let hir_function = location.hir_function(self.db);
                let function = hir_function.function(self.db);
                let source_map = hir_function.source_map(self.db);
                let inference = location.infer(self.db);
                self.validate_function_signature(location, function, source_map, inference);
                if function.body() != ExprId::ZERO {
                    let resolver = Resolver::new(self.db, location);
                    let mut visitor = RuntimeBodyVisitor {
                        checker: self,
                        location,
                        function,
                        source_map,
                        inference,
                        resolver,
                    };
                    visitor.expr(function.body());
                }
            }
            RuntimeWorkItem::Closure { owner, expr } => {
                let hir_function = owner.hir_function(self.db);
                let function = hir_function.function(self.db);
                let source_map = hir_function.source_map(self.db);
                let inference = owner.infer(self.db);
                self.validate_closure_signature(owner, expr, function, source_map, inference);
                let nodes = function.node_store();
                let Some(closure) = nodes.as_closure(expr) else {
                    return;
                };
                let (_params, body) = nodes.closure_parts(closure);
                if body != ExprId::ZERO {
                    let resolver = Resolver::new(self.db, owner);
                    let mut visitor = RuntimeBodyVisitor {
                        checker: self,
                        location: owner,
                        function,
                        source_map,
                        inference,
                        resolver,
                    };
                    visitor.expr(body);
                }
            }
        }
    }

    fn validate_function_signature(
        &mut self,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) {
        let _crosses_wasm_boundary = !matches!(function.linkage(), WasmLinkage::Internal)
            || self.boundary_roots.contains(&location);
        let nodes = function.node_store();
        for &param in function.params() {
            let (pattern, _) = nodes.param(param);
            for name in nodes.pattern_binding_names(pattern) {
                let name_id = name.into();
                if let Some(ty) = inference.type_of_node(name_id) {
                    let range = source_map
                        .try_node_syntax(name_id)
                        .map_or_else(|| function_range(self.db, location), |ptr| ptr.range);
                    self.validate_ty(ty, range);
                }
            }
        }

        let return_ty = inference
            .type_of_node(function.body())
            .unwrap_or_else(|| Ty::new(self.db, TyKind::Tuple(Vec::new())));
        let return_range = if function.ret_type().is_zero() {
            function_range(self.db, location)
        } else {
            source_map
                .try_type_syntax(function.ret_type())
                .map_or_else(|| function_range(self.db, location), |ptr| ptr.range)
        };
        self.validate_ty(return_ty, return_range);
    }

    fn validate_closure_signature(
        &mut self,
        location: FunctionLocation<'db>,
        expr: ExprId,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) {
        let nodes = function.node_store();
        let Some(closure) = nodes.as_closure(expr) else {
            return;
        };
        let (params, _body) = nodes.closure_parts(closure);
        for param in params.iter() {
            let (pattern, _) = nodes.param(param);
            for name in nodes.pattern_binding_names(pattern) {
                let name_id = name.into();
                if let Some(ty) = inference.type_of_node(name_id) {
                    self.validate_ty(
                        ty,
                        source_map
                            .try_node_syntax(name_id)
                            .map_or_else(|| function_range(self.db, location), |ptr| ptr.range),
                    );
                }
            }
        }

        if let Some(TyKind::Function { output, .. }) =
            inference.type_of_node(expr).map(|ty| ty.kind(self.db))
        {
            self.validate_ty(
                *output,
                source_map
                    .try_node_syntax(expr)
                    .map_or_else(|| function_range(self.db, location), |ptr| ptr.range),
            );
        }
    }

    #[allow(clippy::unused_self)]
    fn validate_ty(&mut self, ty: Ty<'db>, range: mitki_errors::TextRange) {
        let _ = (ty, range);
    }
}

struct RuntimeBodyVisitor<'a, 'db> {
    checker: &'a mut RuntimeReachabilityChecker<'db>,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    resolver: Resolver<'db>,
}

impl<'a, 'db> RuntimeBodyVisitor<'a, 'db> {
    fn expr(&mut self, expr: ExprId) {
        let nodes = self.function.node_store();
        let kind = nodes.node_kind(expr);
        self.validate_expr_type(expr, kind);

        match kind {
            NodeKind::Name => self.name(expr),
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.expr(item);
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block mismatch"));
                for stmt in stmts.iter() {
                    self.stmt(stmt);
                }
                if tail != ExprId::ZERO {
                    self.expr(tail);
                }
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(expr).expect("UnsafeBlock mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body);
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                if let Some(function) = self.resolve_method_call(callee) {
                    self.checker.pending.push_back(RuntimeWorkItem::Function(function));
                } else if matches!(nodes.node_kind(callee), NodeKind::Name)
                    && let Some(resolution) = self.resolve_name(callee)
                {
                    match resolution {
                        BindingId::Function(function) => {
                            self.checker.pending.push_back(RuntimeWorkItem::Function(function));
                        }
                        BindingId::RuntimeFunction(_)
                        | BindingId::CompilerIntrinsic(_)
                        | BindingId::Struct(_)
                        | BindingId::Enum(_)
                        | BindingId::BuiltinType(_)
                        | BindingId::EnumVariant(_) => {}
                        BindingId::Local(_) | BindingId::Param(_) => self.expr(callee),
                    }
                } else if callee != ExprId::ZERO {
                    self.expr(callee);
                }

                if let Some(field_id) = nodes.as_field(callee) {
                    let (receiver, _) = nodes.field(field_id);
                    if receiver != ExprId::ZERO && self.resolve_method_call(callee).is_some() {
                        self.expr(receiver);
                    }
                }
                for arg in args.iter() {
                    self.expr(arg);
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.expr(binary.lhs);
                self.expr(binary.rhs);
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                self.expr(prefix.expr);
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                self.expr(if_expr.cond);
                if if_expr.then_branch != ExprId::ZERO {
                    self.expr(if_expr.then_branch);
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.expr(if_expr.else_branch);
                }
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(expr).expect("Match node mismatch"));
                self.expr(scrutinee);
                for arm in arms.iter() {
                    let (pattern, arm_expr) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    self.validate_pattern_bindings(pattern);
                    self.expr(arm_expr);
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body);
                }
            }
            NodeKind::LocalVar => {
                let var = nodes.local_var(nodes.as_local_var(expr).expect("LocalVar mismatch"));
                self.validate_pattern_bindings(var.pattern);
                if var.initializer != ExprId::ZERO {
                    self.expr(var.initializer);
                }
            }
            NodeKind::Array => {
                let array = nodes.array(nodes.as_array(expr).expect("Array node mismatch"));
                for item in array.iter() {
                    self.expr(item);
                }
            }
            NodeKind::ArrayRepeat => {
                let (value, len) =
                    nodes.array_repeat(nodes.as_array_repeat(expr).expect("ArrayRepeat mismatch"));
                self.expr(value);
                self.expr(len);
            }
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.expr(base);
                }
            }
            NodeKind::Closure => {
                self.checker
                    .pending
                    .push_back(RuntimeWorkItem::Closure { owner: self.location, expr });
            }
            NodeKind::StructExpr => {
                let items = nodes.struct_expr(nodes.as_struct_expr(expr).expect("Struct mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.expr(items.get(index).unwrap());
                    index += 2;
                }
            }
            NodeKind::True
            | NodeKind::False
            | NodeKind::Int
            | NodeKind::Float
            | NodeKind::String
            | NodeKind::Char
            | NodeKind::BreakExpr
            | NodeKind::ContinueExpr
            | NodeKind::Postfix
            | NodeKind::Error => {}
            _ => {}
        }
    }

    fn stmt(&mut self, stmt: StmtId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var = nodes.local_var(nodes.as_local_var(stmt).expect("LocalVar mismatch"));
                self.validate_pattern_bindings(var.pattern);
                if var.initializer != ExprId::ZERO {
                    self.expr(var.initializer);
                }
            }
            NodeKind::ReturnStmt => {
                let (value, _) =
                    nodes.return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
                if value != ExprId::ZERO {
                    self.expr(value);
                }
            }
            _ => {
                if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    self.expr(expr);
                }
            }
        }
    }

    fn name(&mut self, expr: ExprId) {
        match self.resolve_name(expr) {
            Some(BindingId::Function(function)) => {
                self.checker.pending.push_back(RuntimeWorkItem::Function(function));
            }
            Some(BindingId::RuntimeFunction(function)) => {
                self.checker.diagnostics.push(Diagnostic::error(
                    format!(
                        "runtime function `{}` can only be used in call position",
                        function.source_name()
                    ),
                    self.node_range(expr),
                ));
            }
            _ => {}
        }
    }

    fn validate_expr_type(&mut self, expr: ExprId, kind: NodeKind) {
        if matches!(
            kind,
            NodeKind::Name
                | NodeKind::True
                | NodeKind::False
                | NodeKind::Int
                | NodeKind::Float
                | NodeKind::String
                | NodeKind::Char
                | NodeKind::BreakExpr
                | NodeKind::ContinueExpr
                | NodeKind::Error
        ) {
            return;
        }

        let Some(ty) = self.inference.type_of_node(expr) else {
            return;
        };
        self.checker.validate_ty(ty, self.node_range(expr));
    }

    fn validate_local_binding(&mut self, node: ExprId) {
        let Some(ty) = self.inference.type_of_node(node) else {
            return;
        };
        self.checker.validate_ty(ty, self.node_range(node));
    }

    fn validate_pattern_bindings(&mut self, pattern: PatId) {
        for name in self.function.node_store().pattern_binding_names(pattern) {
            self.validate_local_binding(name.into());
        }
    }

    fn resolve_name(&mut self, expr: ExprId) -> Option<BindingId<'db>> {
        let nodes = self.function.node_store();
        let name = nodes.as_name(expr)?;
        let symbol = nodes.name(name);
        let guard = self.resolver.scopes_for_node(expr);
        let resolution = self.resolver.resolve_value_binding(symbol);
        self.resolver.reset(guard);
        resolution
    }

    fn resolve_method_call(&self, callee: ExprId) -> Option<FunctionLocation<'db>> {
        let nodes = self.function.node_store();
        let field = nodes.as_field(callee)?;
        let (receiver, field_name_expr) = nodes.field(field);
        if receiver == ExprId::ZERO {
            return None;
        }

        let field_name = nodes.as_name(field_name_expr)?;
        let receiver_ty = self.inference.type_of_node(receiver)?;
        resolve_method_for_receiver(self.checker.db, receiver_ty, nodes.name(field_name))
            .map(|method| method.function)
    }

    fn node_range(&self, expr: ExprId) -> mitki_errors::TextRange {
        self.source_map
            .try_node_syntax(expr)
            .map_or_else(|| function_range(self.checker.db, self.location), |ptr| ptr.range)
    }
}

fn function_range(
    db: &dyn salsa::Database,
    location: FunctionLocation<'_>,
) -> mitki_errors::TextRange {
    mitki_yellow::SyntaxNodePtr::new(location.source(db).syntax()).range
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

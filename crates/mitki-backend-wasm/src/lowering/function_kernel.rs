#![allow(dead_code)]

#[cfg(test)]
use std::sync::Arc;

use mitki_errors::Diagnostic;
use rustc_hash::FxHashMap;

use super::plan::ModulePlan;
use super::*;
use crate::layout::VariantLayout;

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelBundle<'db> {
    pub(in crate::backend) direct_functions: Vec<FunctionKernelFunction<'db>>,
    pub(in crate::backend) closures: Vec<FunctionKernelFunction<'db>>,
    pub(in crate::backend) direct_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) closure_indices: FxHashMap<ClosureInstanceKey<'db>, usize>,
}

impl<'db> FunctionKernelBundle<'db> {
    #[cfg(test)]
    pub(in crate::backend) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut out = String::new();
        out.push_str("function_kernel.direct:\n");
        for function in &self.direct_functions {
            function.dump_into(db, &mut out);
        }
        out.push_str("function_kernel.closures:\n");
        for function in &self.closures {
            function.dump_into(db, &mut out);
        }
        out
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct FunctionKernelId(pub(in crate::backend) u32);

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelFunction<'db> {
    pub(in crate::backend) id: FunctionKernelId,
    pub(in crate::backend) kind: FunctionKernelKind<'db>,
    pub(in crate::backend) debug_name: String,
    pub(in crate::backend) signature: FunctionSignature,
    pub(in crate::backend) layout: FunctionLayout,
    pub(in crate::backend) legalization: Option<function_legalize::FunctionLegalization<'db>>,
    pub(in crate::backend) param_inits: Vec<FunctionKernelBindingInit<'db>>,
    pub(in crate::backend) entry: FunctionKernelBlockId,
    pub(in crate::backend) blocks: Vec<FunctionKernelBlock<'db>>,
    pub(in crate::backend) decisions: Vec<String>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelLowered<'db> {
    pub(in crate::backend) param_inits: Vec<FunctionKernelBindingInit<'db>>,
    pub(in crate::backend) body: Option<FunctionKernelValue<'db>>,
}

impl<'db> FunctionKernelFunction<'db> {
    pub(in crate::backend) fn entry_block(&self) -> &FunctionKernelBlock<'db> {
        &self.blocks[self.entry.0 as usize]
    }

    #[cfg(test)]
    fn dump_into(&self, db: &dyn salsa::Database, out: &mut String) {
        use std::fmt::Write as _;

        writeln!(out, "  {}:", self.debug_name).expect("write string");
        writeln!(out, "    id: k{}", self.id.0).expect("write string");
        writeln!(out, "    source: {:?}", self.kind).expect("write string");
        writeln!(out, "    decisions: {:?}", self.decisions).expect("write string");
        for block in &self.blocks {
            writeln!(out, "    block b{}: {}", block.id.0, describe_abi(db, &block.result_abi))
                .expect("write string");
            for stmt in &block.stmts {
                writeln!(out, "      stmt: {}", dump_stmt(db, stmt)).expect("write string");
            }
            writeln!(out, "      term: {}", dump_terminator(db, &block.terminator))
                .expect("write string");
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) enum FunctionKernelKind<'db> {
    Direct(InstanceKey<'db>),
    Closure(ClosureInstanceKey<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct FunctionKernelBlockId(pub(in crate::backend) u32);

#[derive(Clone, Debug)]
#[cfg_attr(not(test), allow(dead_code))]
pub(in crate::backend) struct FunctionKernelBlock<'db> {
    pub(in crate::backend) id: FunctionKernelBlockId,
    pub(in crate::backend) source: ExprId,
    pub(in crate::backend) result_abi: AbiTy,
    pub(in crate::backend) stmts: Vec<FunctionKernelStmt<'db>>,
    pub(in crate::backend) terminator: FunctionKernelTerminator<'db>,
}

#[derive(Clone, Debug)]
#[cfg_attr(not(test), allow(dead_code))]
pub(in crate::backend) enum FunctionKernelStmt<'db> {
    Local { name: NameId, abi: AbiTy, initializer: Option<FunctionKernelValue<'db>> },
    Assign { name: NameId, abi: AbiTy, value: FunctionKernelValue<'db> },
    Pattern(FunctionKernelBindingInit<'db>),
    Expr(FunctionKernelValue<'db>),
    Return { source: ExprId, value: Option<FunctionKernelValue<'db>> },
    Retain { source: ExprId, abi: AbiTy, value: FunctionKernelValue<'db> },
    Release { source: ExprId, abi: AbiTy, value: FunctionKernelValue<'db> },
    Destroy { source: ExprId, abi: AbiTy, value: FunctionKernelValue<'db> },
    Copy { source: ExprId, abi: AbiTy, value: FunctionKernelValue<'db> },
    Move { source: ExprId, abi: AbiTy, value: FunctionKernelValue<'db> },
}

#[derive(Clone, Debug)]
#[cfg_attr(not(test), allow(dead_code))]
pub(in crate::backend) enum FunctionKernelTerminator<'db> {
    Return { source: ExprId, value: Option<FunctionKernelValue<'db>> },
    Unreachable { source: ExprId },
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelBindingInit<'db> {
    pub(in crate::backend) pattern: BackendPattern,
    pub(in crate::backend) source: FunctionKernelBindingSource<'db>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum FunctionKernelBindingSource<'db> {
    Value(FunctionKernelValue<'db>),
    Param { index: usize, abi: AbiTy, source: ExprId },
}

impl<'db> FunctionKernelBindingSource<'db> {
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::backend) fn source_expr(&self) -> ExprId {
        match self {
            Self::Value(value) => value.source,
            Self::Param { source, .. } => *source,
        }
    }
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelMatchArm<'db> {
    pub(in crate::backend) pattern: BackendPattern,
    pub(in crate::backend) body: FunctionKernelValue<'db>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelFieldValue<'db> {
    pub(in crate::backend) field: FieldLayout,
    pub(in crate::backend) value: FunctionKernelValue<'db>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelClosureEnvInit<'db> {
    pub(in crate::backend) layout: AggregateLayout,
    pub(in crate::backend) fields: Vec<FunctionKernelFieldValue<'db>>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionKernelValue<'db> {
    pub(in crate::backend) source: ExprId,
    pub(in crate::backend) abi: AbiTy,
    pub(in crate::backend) ownership: ValueOwnership,
    pub(in crate::backend) kind: FunctionKernelValueKind<'db>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum FunctionKernelValueKind<'db> {
    Local(NameId),
    Capture(FieldLayout),
    FunctionValue {
        target: FunctionValueTarget<'db>,
    },
    Clone {
        value: Box<FunctionKernelValue<'db>>,
    },
    ClosureValue {
        target: ClosureInstanceKey<'db>,
        env: FunctionKernelClosureEnvInit<'db>,
    },
    Bool(bool),
    Int(i64),
    Float(f64),
    String(u32),
    StringFromBytes {
        ptr: Box<FunctionKernelValue<'db>>,
        len: Box<FunctionKernelValue<'db>>,
    },
    Char(char),
    Unit,
    StackAddr {
        frame_slot: FrameSlotId,
    },
    AddrOffset {
        base: Box<FunctionKernelValue<'db>>,
        offset: u32,
    },
    MemoryRead {
        addr: Box<FunctionKernelValue<'db>>,
        access: Option<MemAccess>,
    },
    MemoryWrite {
        addr: Box<FunctionKernelValue<'db>>,
        value: Box<FunctionKernelValue<'db>>,
    },
    PointerAdd {
        ptr: Box<FunctionKernelValue<'db>>,
        count: Box<FunctionKernelValue<'db>>,
        stride: u32,
    },
    Array {
        layout: ArrayRuntimeLayout,
        items: Vec<FunctionKernelValue<'db>>,
    },
    ArrayRepeat {
        layout: ArrayRuntimeLayout,
        value: Box<FunctionKernelValue<'db>>,
        len: Box<FunctionKernelValue<'db>>,
    },
    Block {
        stmts: Vec<FunctionKernelStmt<'db>>,
        tail: Option<Box<FunctionKernelValue<'db>>>,
    },
    Call {
        target: BackendCallTarget<'db>,
        args: Vec<FunctionKernelValue<'db>>,
    },
    IndirectCall {
        callee: Box<FunctionKernelValue<'db>>,
        signature: FunctionSignature,
        args: Vec<FunctionKernelValue<'db>>,
    },
    Binary {
        op: BackendBinaryOp,
        lhs: Box<FunctionKernelValue<'db>>,
        rhs: Box<FunctionKernelValue<'db>>,
    },
    Prefix {
        op: BackendPrefixOp,
        expr: Box<FunctionKernelValue<'db>>,
    },
    If {
        cond: Box<FunctionKernelValue<'db>>,
        then_branch: Option<Box<FunctionKernelValue<'db>>>,
        else_branch: Option<Box<FunctionKernelValue<'db>>>,
    },
    Match {
        scrutinee: Box<FunctionKernelValue<'db>>,
        arms: Vec<FunctionKernelMatchArm<'db>>,
    },
    Loop {
        body: Option<Box<FunctionKernelValue<'db>>>,
    },
    Break,
    Continue,
    Field {
        base: Box<FunctionKernelValue<'db>>,
        field: FieldLayout,
    },
    Tuple {
        fields: Vec<FunctionKernelFieldValue<'db>>,
    },
    Struct {
        fields: Vec<FunctionKernelFieldValue<'db>>,
    },
    Union {
        variant: VariantLayout,
        value: Option<Box<FunctionKernelValue<'db>>>,
    },
    VariantValue {
        variant: VariantLayout,
    },
    VariantCall {
        variant: VariantLayout,
        args: Vec<FunctionKernelValue<'db>>,
    },
}

pub(in crate::backend) struct FunctionKernelBuilder;
pub(in crate::backend) struct FunctionKernelValidator;

impl FunctionKernelBuilder {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
    ) -> Result<FunctionKernelBundle<'db>, Diagnostic> {
        let static_data = backend.build_static_data_for(&plan.reachability)?;
        let mut output = FunctionKernelBundle {
            direct_functions: Vec::new(),
            closures: Vec::new(),
            direct_indices: FxHashMap::default(),
            closure_indices: FxHashMap::default(),
        };
        let mut next_id = 0u32;

        for instance in &plan.reachability.functions {
            let hir_function = instance.location.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            if matches!(
                function.linkage(),
                WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
            ) {
                continue;
            }

            let inference = instance.location.infer(backend.db);
            let signature = backend.function_signature(instance, function, inference)?;
            let layout = backend.build_layout(
                &ReachableInstance::Function(instance.clone()),
                function,
                inference,
                &signature,
            )?;
            let lowered = backend.build_lowered_kernel_function(
                &ReachableInstance::Function(instance.clone()),
                function,
                hir_function.source_map(backend.db),
                inference,
                &layout,
                &static_data,
            )?;
            output.direct_indices.insert(instance.clone(), output.direct_functions.len());
            output.direct_functions.push(build_function(
                FunctionKernelId(next_id),
                FunctionKernelKind::Direct(instance.clone()),
                format_instance_key(backend.db, instance),
                signature,
                layout,
                lowered,
            ));
            next_id += 1;
        }

        for closure in &plan.reachability.closures {
            let hir_function = closure.owner.hir_function(backend.db);
            let function = hir_function.function(backend.db);
            let inference = closure.owner.infer(backend.db);
            let info = backend.closure_info(closure, function, inference)?;
            let layout = backend.build_layout(
                &ReachableInstance::Closure(closure.clone()),
                function,
                inference,
                &info.signature,
            )?;
            let lowered = backend.build_lowered_kernel_function(
                &ReachableInstance::Closure(closure.clone()),
                function,
                hir_function.source_map(backend.db),
                inference,
                &layout,
                &static_data,
            )?;
            output.closure_indices.insert(closure.clone(), output.closures.len());
            output.closures.push(build_function(
                FunctionKernelId(next_id),
                FunctionKernelKind::Closure(closure.clone()),
                format_closure_key(backend.db, closure),
                info.signature,
                layout,
                lowered,
            ));
            next_id += 1;
        }

        FunctionKernelValidator::validate(backend, plan, &output)?;
        Ok(output)
    }
}

impl FunctionKernelValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<(), Diagnostic> {
        let expected_direct = plan
            .reachability
            .functions
            .iter()
            .filter(|instance| {
                let hir_function = instance.location.hir_function(backend.db);
                let function = hir_function.function(backend.db);
                !matches!(
                    function.linkage(),
                    WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        if bundle.direct_functions.len() != expected_direct.len() {
            return Err(Diagnostic::error(
                "internal error: function kernel direct body set drifted from ordinary \
                 reachability",
                backend.file_range(),
            ));
        }
        if bundle.closures.len() != plan.reachability.closures.len() {
            return Err(Diagnostic::error(
                "internal error: function kernel closure body set drifted from reachability",
                backend.file_range(),
            ));
        }
        for (index, function) in bundle.direct_functions.iter().enumerate() {
            let expected =
                FunctionKernelId(u32::try_from(index).expect("kernel ids should fit u32"));
            if function.id != expected {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: function kernel direct id drifted (expected k{}, found \
                         k{})",
                        expected.0, function.id.0
                    ),
                    backend.file_range(),
                ));
            }
            if function.blocks.len() != 1 || function.entry != FunctionKernelBlockId(0) {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: function kernel `{}` should currently lower to one entry \
                         block",
                        function.debug_name
                    ),
                    backend.file_range(),
                ));
            }
            match &function.kind {
                FunctionKernelKind::Direct(instance)
                    if expected_direct.get(index) == Some(instance) => {}
                _ => {
                    return Err(Diagnostic::error(
                        format!(
                            "internal error: function kernel direct ordering drifted for `{}`",
                            function.debug_name
                        ),
                        backend.file_range(),
                    ));
                }
            }
        }

        for (index, function) in bundle.closures.iter().enumerate() {
            match &function.kind {
                FunctionKernelKind::Closure(closure)
                    if plan.reachability.closures.get(index) == Some(closure) => {}
                _ => {
                    return Err(Diagnostic::error(
                        format!(
                            "internal error: function kernel closure ordering drifted for `{}`",
                            function.debug_name
                        ),
                        backend.file_range(),
                    ));
                }
            }
        }
        Ok(())
    }
}

fn format_instance_key(db: &dyn salsa::Database, instance: &InstanceKey<'_>) -> String {
    let name = instance.location.source(db).name().map_or("<anonymous>", |name| name.as_str());
    if instance.type_args.is_empty() {
        name.to_owned()
    } else {
        let type_args = instance
            .type_args
            .iter()
            .map(|ty| ty.display(db).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("{name}[{type_args}]")
    }
}

fn format_closure_key(db: &dyn salsa::Database, closure: &ClosureInstanceKey<'_>) -> String {
    let owner = format_instance_key(db, &closure.owner_instance());
    format!("{owner}::closure#{:?}", closure.closure)
}

fn build_function<'db>(
    id: FunctionKernelId,
    kind: FunctionKernelKind<'db>,
    debug_name: String,
    signature: FunctionSignature,
    layout: FunctionLayout,
    lowered: FunctionKernelLowered<'db>,
) -> FunctionKernelFunction<'db> {
    let (stmts, terminator, source, result_abi) = match lowered.body {
        Some(body) => lower_body(body),
        None => (
            Vec::new(),
            FunctionKernelTerminator::Return { source: ExprId::ZERO, value: None },
            ExprId::ZERO,
            signature.result.clone(),
        ),
    };
    FunctionKernelFunction {
        id,
        kind,
        debug_name,
        signature,
        layout,
        legalization: None,
        param_inits: lowered.param_inits,
        entry: FunctionKernelBlockId(0),
        blocks: vec![FunctionKernelBlock {
            id: FunctionKernelBlockId(0),
            source,
            result_abi,
            stmts,
            terminator,
        }],
        decisions: vec!["kernel=single-entry".to_owned()],
    }
}

fn lower_body<'db>(
    body: FunctionKernelValue<'db>,
) -> (Vec<FunctionKernelStmt<'db>>, FunctionKernelTerminator<'db>, ExprId, AbiTy) {
    let source = body.source;
    let result_abi = body.abi.clone();
    match body.kind {
        FunctionKernelValueKind::Block { stmts, tail } => {
            let terminator =
                FunctionKernelTerminator::Return { source, value: tail.map(|tail| *tail) };
            (stmts, terminator, source, result_abi)
        }
        _ => (
            Vec::new(),
            FunctionKernelTerminator::Return { source, value: Some(body) },
            source,
            result_abi,
        ),
    }
}

#[cfg(test)]
fn describe_abi(_db: &dyn salsa::Database, abi: &AbiTy) -> String {
    match abi {
        AbiTy::Scalar(ty) => format!("{ty:?}"),
        AbiTy::Aggregate(layout) => format!("aggregate(size={})", layout.size),
    }
}

#[cfg(test)]
fn dump_stmt(db: &dyn salsa::Database, stmt: &FunctionKernelStmt<'_>) -> String {
    match stmt {
        FunctionKernelStmt::Local { name, initializer, .. } => {
            format!("local {:?} = {}", name, dump_value(db, initializer.as_ref()))
        }
        FunctionKernelStmt::Assign { name, value, .. } => {
            format!("assign {:?} = {}", name, dump_value(db, Some(value)))
        }
        FunctionKernelStmt::Pattern(init) => format!("pattern {:?}", init.pattern),
        FunctionKernelStmt::Expr(expr) => format!("expr {}", dump_value(db, Some(expr))),
        FunctionKernelStmt::Return { value, .. } => {
            format!("return {}", dump_value(db, value.as_ref()))
        }
        FunctionKernelStmt::Retain { value, .. } => {
            format!("retain {}", dump_value(db, Some(value)))
        }
        FunctionKernelStmt::Release { value, .. } => {
            format!("release {}", dump_value(db, Some(value)))
        }
        FunctionKernelStmt::Destroy { value, .. } => {
            format!("destroy {}", dump_value(db, Some(value)))
        }
        FunctionKernelStmt::Copy { value, .. } => format!("copy {}", dump_value(db, Some(value))),
        FunctionKernelStmt::Move { value, .. } => format!("move {}", dump_value(db, Some(value))),
    }
}

#[cfg(test)]
fn dump_terminator(db: &dyn salsa::Database, term: &FunctionKernelTerminator<'_>) -> String {
    match term {
        FunctionKernelTerminator::Return { value, .. } => {
            format!("return {}", dump_value(db, value.as_ref()))
        }
        FunctionKernelTerminator::Unreachable { .. } => "unreachable".to_owned(),
    }
}

#[cfg(test)]
fn dump_value(db: &dyn salsa::Database, value: Option<&FunctionKernelValue<'_>>) -> String {
    let Some(value) = value else {
        return "unit".to_owned();
    };
    match &value.kind {
        FunctionKernelValueKind::Local(name) => format!("local({name:?})"),
        FunctionKernelValueKind::Bool(value) => format!("bool({value})"),
        FunctionKernelValueKind::Int(value) => format!("int({value})"),
        FunctionKernelValueKind::String(_) => "string".to_owned(),
        FunctionKernelValueKind::StringFromBytes { .. } => "string_from_bytes".to_owned(),
        FunctionKernelValueKind::Call { .. } => "call".to_owned(),
        FunctionKernelValueKind::If { .. } => "if".to_owned(),
        FunctionKernelValueKind::Match { .. } => "match".to_owned(),
        FunctionKernelValueKind::Loop { .. } => "loop".to_owned(),
        FunctionKernelValueKind::Block { .. } => "block".to_owned(),
        _ => describe_abi(db, &value.abi),
    }
}

#[cfg(test)]
mod tests {
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "function_kernel_fixture.mitki".into(), fixture.to_owned());
        let diagnostics = mitki_analysis::check_file(db, file);
        assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);
        let runtime_diagnostics = mitki_analysis::check_runtime_file(db, file);
        assert!(
            runtime_diagnostics.is_empty(),
            "unexpected runtime diagnostics: {:?}",
            runtime_diagnostics
        );
        let mut backend = Backend::new_file_with_options(
            db,
            file,
            crate::CompileOptions,
            Arc::new(crate::NoopComptimeEvaluator),
        );
        backend.collect_reachable_program();
        assert!(
            backend.diagnostics.is_empty(),
            "unexpected backend diagnostics: {:?}",
            backend.diagnostics
        );
        backend
    }

    #[test]
    fn function_kernel_dump_is_stable() {
        let backend = compiler_for_fixture(
            r#"
fun choose(flag: bool): int {
    if flag { 42 } else { 0 }
}

export fun main(): int {
    choose(true)
}
"#,
        );
        let plan = backend.build_module_plan().expect("module plan");
        let first = plan.function_kernel.as_ref().expect("kernel").dump(backend.db);
        let second = backend
            .build_module_plan()
            .expect("module plan")
            .function_kernel
            .as_ref()
            .expect("kernel")
            .dump(backend.db);
        assert_eq!(first, second);
        assert!(first.contains("function_kernel.direct:"));
        assert!(first.contains("block b0"));
    }
}

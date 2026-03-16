use mitki_errors::Diagnostic;
use rustc_hash::FxHashMap;

use super::function_kernel::{
    FunctionKernelBlock, FunctionKernelBundle, FunctionKernelFunction, FunctionKernelStmt,
    FunctionKernelTerminator, FunctionKernelValue, FunctionKernelValueKind,
};
use super::plan::ModulePlan;
use super::*;

pub(in crate::backend) struct FunctionLegalizer;
pub(in crate::backend) struct CallConvValidator;

#[derive(Clone, Debug)]
pub(in crate::backend) struct FunctionLegalization<'db> {
    pub(in crate::backend) signature: LegalizedFunctionSignature,
    pub(in crate::backend) callable_representation: LegalizedCallableRepresentation,
    pub(in crate::backend) call_sites: FxHashMap<ExprId, LegalizedCallSite<'db>>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct LegalizedFunctionSignature {
    pub(in crate::backend) params: Vec<LegalizedParam>,
    pub(in crate::backend) hidden_env_local: Option<u32>,
    pub(in crate::backend) hidden_result_local: Option<u32>,
    pub(in crate::backend) result: LegalizedResultPassing,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct LegalizedParam {
    pub(in crate::backend) ordinal: usize,
    pub(in crate::backend) abi: AbiTy,
    pub(in crate::backend) passing: LegalizedArgPassing,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum LegalizedArgPassing {
    Direct,
    ByAddress,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum LegalizedResultPassing {
    Unit,
    DirectScalar(BackendTy),
    IndirectOutPtr,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum LegalizedCallableRepresentation {
    HandleAndTable,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct LegalizedCallSite<'db> {
    pub(in crate::backend) kind: LegalizedCallKind<'db>,
    pub(in crate::backend) arg_passings: Vec<LegalizedArgPassing>,
    pub(in crate::backend) result: LegalizedResultPassing,
    pub(in crate::backend) callable_representation: LegalizedCallableRepresentation,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum LegalizedCallKind<'db> {
    Direct(BackendCallTarget<'db>),
    Indirect(FunctionSignature),
}

impl FunctionLegalizer {
    pub(in crate::backend) fn run<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<FunctionKernelBundle<'db>, Diagnostic> {
        let legalized = FunctionKernelBundle {
            direct_functions: bundle
                .direct_functions
                .iter()
                .map(|function| legalize_function(backend, function))
                .collect(),
            closures: bundle
                .closures
                .iter()
                .map(|function| legalize_function(backend, function))
                .collect(),
            direct_indices: bundle.direct_indices.clone(),
            closure_indices: bundle.closure_indices.clone(),
        };
        CallConvValidator::validate(backend, plan, &legalized)?;
        Ok(legalized)
    }
}

impl CallConvValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        _plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<(), Diagnostic> {
        let signature_strategy = backend.signature_strategy();
        for function in bundle.direct_functions.iter().chain(bundle.closures.iter()) {
            let lowered = signature_strategy.direct_signature(&function.signature);
            let legalization = function.legalization.as_ref().ok_or_else(|| {
                Diagnostic::error(
                    format!(
                        "internal error: function `{}` is missing call-convention legalization",
                        function.debug_name
                    ),
                    backend.file_range(),
                )
            })?;
            let has_result_ptr = function.layout.result_ptr_local().is_some();
            if legalization.signature.hidden_result_local.is_some() != has_result_ptr {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: legalization drifted result-pointer storage for `{}`",
                        function.debug_name
                    ),
                    backend.file_range(),
                ));
            }
            if lowered.params.is_empty()
                && function.signature.params.is_empty()
                && legalization.signature.hidden_result_local.is_some()
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: legalized signature for `{}` lost the hidden result \
                         pointer lane",
                        function.debug_name
                    ),
                    backend.file_range(),
                ));
            }
            if legalization.signature.hidden_env_local != function.layout.env_ptr_local() {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: legalization drifted env parameter storage for `{}`",
                        function.debug_name
                    ),
                    backend.file_range(),
                ));
            }
            if legalization.signature.params.len() != function.signature.params.len() {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: legalization drifted parameter arity for `{}`",
                        function.debug_name
                    ),
                    backend.file_range(),
                ));
            }
            for (ordinal, (expected_abi, param)) in
                function.signature.params.iter().zip(&legalization.signature.params).enumerate()
            {
                if param.ordinal != ordinal
                    || &param.abi != expected_abi
                    || param.passing != arg_passing_for(expected_abi)
                {
                    return Err(Diagnostic::error(
                        format!(
                            "internal error: legalization drifted parameter passing for `{}`",
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

fn legalize_function<'db>(
    backend: &Backend<'db>,
    function: &FunctionKernelFunction<'db>,
) -> FunctionKernelFunction<'db> {
    let mut legalized = function.clone();
    let signature_strategy = backend.signature_strategy();
    let lowered = signature_strategy.direct_signature(&legalized.signature);
    let callable_representation = LegalizedCallableRepresentation::HandleAndTable;
    legalized.legalization = Some(FunctionLegalization {
        signature: LegalizedFunctionSignature {
            params: legalized
                .signature
                .params
                .iter()
                .cloned()
                .enumerate()
                .map(|(ordinal, abi)| LegalizedParam {
                    ordinal,
                    passing: arg_passing_for(&abi),
                    abi,
                })
                .collect(),
            hidden_env_local: legalized.layout.env_ptr_local(),
            hidden_result_local: legalized.layout.result_ptr_local(),
            result: result_passing_for(&legalized.signature.result),
        },
        callable_representation,
        call_sites: collect_call_sites(&legalized.blocks, callable_representation),
    });
    legalized.decisions.push(format!("callconv={}", backend.target_profile().canonical_name()));
    legalized.decisions.push(format!("physical_params={}", lowered.params.len()));
    legalized.decisions.push(format!("physical_results={}", lowered.results.len()));
    if legalized.legalization.as_ref().and_then(|it| it.signature.hidden_env_local).is_some() {
        legalized.decisions.push("env_param=hidden".to_owned());
    }
    if legalized.legalization.as_ref().and_then(|it| it.signature.hidden_result_local).is_some() {
        legalized.decisions.push("result_ptr=hidden".to_owned());
    }
    legalized.decisions.push("aggregate_args=by-address".to_owned());
    legalized.decisions.push("callables=handle-and-table".to_owned());
    legalized
}

fn arg_passing_for(abi: &AbiTy) -> LegalizedArgPassing {
    match abi {
        AbiTy::Scalar(_) => LegalizedArgPassing::Direct,
        AbiTy::Aggregate(_) => LegalizedArgPassing::ByAddress,
    }
}

fn result_passing_for(abi: &AbiTy) -> LegalizedResultPassing {
    match abi {
        AbiTy::Scalar(BackendTy::Unit) => LegalizedResultPassing::Unit,
        AbiTy::Scalar(ty) => LegalizedResultPassing::DirectScalar(*ty),
        AbiTy::Aggregate(_) => LegalizedResultPassing::IndirectOutPtr,
    }
}

fn collect_call_sites<'db>(
    blocks: &[FunctionKernelBlock<'db>],
    callable_representation: LegalizedCallableRepresentation,
) -> FxHashMap<ExprId, LegalizedCallSite<'db>> {
    let mut sites = FxHashMap::default();
    for block in blocks {
        for stmt in &block.stmts {
            collect_stmt_call_sites(stmt, callable_representation, &mut sites);
        }
        collect_terminator_call_sites(&block.terminator, callable_representation, &mut sites);
    }
    sites
}

fn collect_stmt_call_sites<'db>(
    stmt: &FunctionKernelStmt<'db>,
    callable_representation: LegalizedCallableRepresentation,
    sites: &mut FxHashMap<ExprId, LegalizedCallSite<'db>>,
) {
    match stmt {
        FunctionKernelStmt::Local { initializer, .. } => {
            if let Some(initializer) = initializer {
                collect_value_call_sites(initializer, callable_representation, sites);
            }
        }
        FunctionKernelStmt::Assign { value, .. } => {
            collect_value_call_sites(value, callable_representation, sites);
        }
        FunctionKernelStmt::Pattern(init) => {
            if let function_kernel::FunctionKernelBindingSource::Value(value) = &init.source {
                collect_value_call_sites(value, callable_representation, sites);
            }
        }
        FunctionKernelStmt::Expr(value)
        | FunctionKernelStmt::Retain { value, .. }
        | FunctionKernelStmt::Release { value, .. }
        | FunctionKernelStmt::Destroy { value, .. }
        | FunctionKernelStmt::Copy { value, .. }
        | FunctionKernelStmt::Move { value, .. } => {
            collect_value_call_sites(value, callable_representation, sites);
        }
        FunctionKernelStmt::Return { value, .. } => {
            if let Some(value) = value {
                collect_value_call_sites(value, callable_representation, sites);
            }
        }
    }
}

fn collect_terminator_call_sites<'db>(
    terminator: &FunctionKernelTerminator<'db>,
    callable_representation: LegalizedCallableRepresentation,
    sites: &mut FxHashMap<ExprId, LegalizedCallSite<'db>>,
) {
    if let FunctionKernelTerminator::Return { value, .. } = terminator
        && let Some(value) = value
    {
        collect_value_call_sites(value, callable_representation, sites);
    }
}

fn collect_value_call_sites<'db>(
    value: &FunctionKernelValue<'db>,
    callable_representation: LegalizedCallableRepresentation,
    sites: &mut FxHashMap<ExprId, LegalizedCallSite<'db>>,
) {
    match &value.kind {
        FunctionKernelValueKind::Call { target, args } => {
            sites.insert(
                value.source,
                LegalizedCallSite {
                    kind: LegalizedCallKind::Direct(target.clone()),
                    arg_passings: args.iter().map(|arg| arg_passing_for(&arg.abi)).collect(),
                    result: result_passing_for(&target.signature.result),
                    callable_representation,
                },
            );
            for arg in args {
                collect_value_call_sites(arg, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::IndirectCall { callee, signature, args } => {
            sites.insert(
                value.source,
                LegalizedCallSite {
                    kind: LegalizedCallKind::Indirect(signature.clone()),
                    arg_passings: args.iter().map(|arg| arg_passing_for(&arg.abi)).collect(),
                    result: result_passing_for(&signature.result),
                    callable_representation,
                },
            );
            collect_value_call_sites(callee, callable_representation, sites);
            for arg in args {
                collect_value_call_sites(arg, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::Clone { value }
        | FunctionKernelValueKind::AddrOffset { base: value, .. }
        | FunctionKernelValueKind::MemoryRead { addr: value, .. }
        | FunctionKernelValueKind::Prefix { expr: value, .. }
        | FunctionKernelValueKind::Loop { body: Some(value) }
        | FunctionKernelValueKind::Field { base: value, .. } => {
            collect_value_call_sites(value, callable_representation, sites);
        }
        FunctionKernelValueKind::MemoryWrite { addr, value }
        | FunctionKernelValueKind::Binary { lhs: addr, rhs: value, .. } => {
            collect_value_call_sites(addr, callable_representation, sites);
            collect_value_call_sites(value, callable_representation, sites);
        }
        FunctionKernelValueKind::PointerAdd { ptr, count, .. } => {
            collect_value_call_sites(ptr, callable_representation, sites);
            collect_value_call_sites(count, callable_representation, sites);
        }
        FunctionKernelValueKind::StringFromBytes { ptr, len } => {
            collect_value_call_sites(ptr, callable_representation, sites);
            collect_value_call_sites(len, callable_representation, sites);
        }
        FunctionKernelValueKind::Array { items, .. }
        | FunctionKernelValueKind::VariantCall { args: items, .. } => {
            for item in items {
                collect_value_call_sites(item, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::ArrayRepeat { value, len, .. } => {
            collect_value_call_sites(value, callable_representation, sites);
            collect_value_call_sites(len, callable_representation, sites);
        }
        FunctionKernelValueKind::Block { stmts, tail } => {
            for stmt in stmts {
                collect_stmt_call_sites(stmt, callable_representation, sites);
            }
            if let Some(tail) = tail {
                collect_value_call_sites(tail, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::If { cond, then_branch, else_branch } => {
            collect_value_call_sites(cond, callable_representation, sites);
            if let Some(value) = then_branch {
                collect_value_call_sites(value, callable_representation, sites);
            }
            if let Some(value) = else_branch {
                collect_value_call_sites(value, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::Match { scrutinee, arms } => {
            collect_value_call_sites(scrutinee, callable_representation, sites);
            for arm in arms {
                collect_value_call_sites(&arm.body, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::Tuple { fields } | FunctionKernelValueKind::Struct { fields } => {
            for field in fields {
                collect_value_call_sites(&field.value, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::ClosureValue { env, .. } => {
            for field in &env.fields {
                collect_value_call_sites(&field.value, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::Union { value, .. } => {
            if let Some(value) = value {
                collect_value_call_sites(value, callable_representation, sites);
            }
        }
        FunctionKernelValueKind::Local(_)
        | FunctionKernelValueKind::Capture(_)
        | FunctionKernelValueKind::FunctionValue { .. }
        | FunctionKernelValueKind::Bool(_)
        | FunctionKernelValueKind::Int(_)
        | FunctionKernelValueKind::Float(_)
        | FunctionKernelValueKind::String(_)
        | FunctionKernelValueKind::Char(_)
        | FunctionKernelValueKind::Unit
        | FunctionKernelValueKind::StackAddr { .. }
        | FunctionKernelValueKind::Loop { body: None }
        | FunctionKernelValueKind::Break
        | FunctionKernelValueKind::Continue
        | FunctionKernelValueKind::VariantValue { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "function_legalize_fixture.mitki".into(), fixture.to_owned());
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
    fn legalization_tracks_hidden_result_pointers_and_closure_env_params() {
        let backend = compiler_for_fixture(
            r#"
fun apply(f: fun(int) -> int, x: int): int {
    f(x)
}

fun pair(): (int, int) {
    (1, 2)
}

export fun main(): int {
    val offset = 1
    val add: fun(int) -> int = { value in value + offset }
    val _ = pair()
    apply(add, 41)
}
"#,
        );
        let plan = backend.build_module_plan().expect("module plan");

        let direct = plan
            .function_kernel
            .as_ref()
            .expect("kernel")
            .direct_functions
            .iter()
            .find(|function| function.signature.result.is_aggregate())
            .expect("aggregate-returning function");
        assert!(direct.decisions.iter().any(|decision| decision == "result_ptr=hidden"));

        let closure =
            plan.function_kernel.as_ref().expect("kernel").closures.first().expect("closure");
        assert!(closure.decisions.iter().any(|decision| decision == "env_param=hidden"));
        assert!(
            closure
                .decisions
                .iter()
                .any(|decision| decision.starts_with("callconv=wasm-core-v2/m32"))
        );
    }
}

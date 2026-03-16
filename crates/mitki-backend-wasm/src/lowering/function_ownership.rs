#![allow(unused_imports)]

#[cfg(test)]
use std::sync::Arc;

use mitki_errors::Diagnostic;

use super::function_kernel::{
    FunctionKernelBindingSource, FunctionKernelBlock, FunctionKernelBundle, FunctionKernelFunction,
    FunctionKernelStmt, FunctionKernelTerminator, FunctionKernelValueKind,
};
use super::plan::ModulePlan;
use super::*;

pub(in crate::backend) struct OwnershipLowering;
pub(in crate::backend) struct OwnershipValidator;

impl OwnershipLowering {
    pub(in crate::backend) fn run<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<FunctionKernelBundle<'db>, Diagnostic> {
        let lowered = FunctionKernelBundle {
            direct_functions: bundle.direct_functions.iter().map(lower_function).collect(),
            closures: bundle.closures.iter().map(lower_function).collect(),
            direct_indices: bundle.direct_indices.clone(),
            closure_indices: bundle.closure_indices.clone(),
        };
        OwnershipValidator::validate(backend, plan, &lowered)?;
        Ok(lowered)
    }
}

impl OwnershipValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        _plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<(), Diagnostic> {
        for function in bundle.direct_functions.iter().chain(bundle.closures.iter()) {
            let has_ownership_stmt =
                function.blocks.iter().flat_map(|block| block.stmts.iter()).any(|stmt| {
                    matches!(
                        stmt,
                        FunctionKernelStmt::Retain { .. }
                            | FunctionKernelStmt::Release { .. }
                            | FunctionKernelStmt::Destroy { .. }
                            | FunctionKernelStmt::Copy { .. }
                            | FunctionKernelStmt::Move { .. }
                    )
                });
            if !has_ownership_stmt
                && function.blocks.iter().any(|block| block.result_abi.contains_heap_refs())
            {
                return Err(Diagnostic::error(
                    format!(
                        "internal error: ownership lowering did not materialize any ownership ops \
                         for `{}`",
                        function.debug_name
                    ),
                    backend.file_range(),
                ));
            }
        }
        Ok(())
    }
}

fn lower_function<'db>(function: &FunctionKernelFunction<'db>) -> FunctionKernelFunction<'db> {
    let mut lowered = function.clone();
    lowered.decisions.push("ownership=explicit".to_owned());
    lowered.blocks = lowered.blocks.iter().map(lower_block).collect();
    lowered
}

fn lower_block<'db>(block: &FunctionKernelBlock<'db>) -> FunctionKernelBlock<'db> {
    let mut stmts = Vec::new();
    for stmt in &block.stmts {
        lower_stmt(stmt, &mut stmts);
    }
    let mut terminator = block.terminator.clone();
    if let FunctionKernelTerminator::Return { source, value: Some(value) } = &block.terminator {
        if value.ownership.is_borrowed()
            && matches!(value.abi, AbiTy::Scalar(ty) if ty.is_heap_ref())
        {
            stmts.push(FunctionKernelStmt::Retain {
                source: *source,
                abi: value.abi.clone(),
                value: value.clone(),
            });
        } else if value.ownership.is_owned() && value.abi.contains_heap_refs() {
            stmts.push(FunctionKernelStmt::Move {
                source: *source,
                abi: value.abi.clone(),
                value: value.clone(),
            });
        }
        terminator =
            FunctionKernelTerminator::Return { source: *source, value: Some(value.clone()) };
    }
    FunctionKernelBlock { stmts, terminator, ..block.clone() }
}

fn lower_stmt<'db>(stmt: &FunctionKernelStmt<'db>, out: &mut Vec<FunctionKernelStmt<'db>>) {
    match stmt {
        FunctionKernelStmt::Local { name, abi, initializer } => {
            out.push(stmt.clone());
            if let Some(initializer) = initializer {
                if initializer.ownership.is_borrowed()
                    && matches!(abi, AbiTy::Scalar(ty) if ty.is_heap_ref())
                {
                    out.push(FunctionKernelStmt::Retain {
                        source: initializer.source,
                        abi: abi.clone(),
                        value: initializer.clone(),
                    });
                } else if initializer.ownership.is_owned() && abi.contains_heap_refs() {
                    out.push(FunctionKernelStmt::Move {
                        source: initializer.source,
                        abi: abi.clone(),
                        value: initializer.clone(),
                    });
                }
                if matches!(initializer.kind, FunctionKernelValueKind::Clone { .. }) {
                    out.push(FunctionKernelStmt::Copy {
                        source: initializer.source,
                        abi: initializer.abi.clone(),
                        value: initializer.clone(),
                    });
                }
            }
            let _ = name;
        }
        FunctionKernelStmt::Assign { .. } => {
            out.push(stmt.clone());
        }
        FunctionKernelStmt::Pattern(init) => {
            out.push(stmt.clone());
            if let FunctionKernelBindingSource::Value(value) = &init.source
                && value.ownership.is_owned()
                && value.abi.contains_heap_refs()
            {
                out.push(FunctionKernelStmt::Move {
                    source: value.source,
                    abi: value.abi.clone(),
                    value: value.clone(),
                });
            }
        }
        FunctionKernelStmt::Expr(expr) => {
            out.push(stmt.clone());
            if expr.ownership.is_owned() && expr.abi.contains_heap_refs() {
                out.push(FunctionKernelStmt::Release {
                    source: expr.source,
                    abi: expr.abi.clone(),
                    value: expr.clone(),
                });
            }
        }
        FunctionKernelStmt::Return { source, value } => {
            out.push(stmt.clone());
            if let Some(value) = value
                && value.ownership.is_borrowed()
                && matches!(value.abi, AbiTy::Scalar(ty) if ty.is_heap_ref())
            {
                out.push(FunctionKernelStmt::Retain {
                    source: *source,
                    abi: value.abi.clone(),
                    value: value.clone(),
                });
            }
        }
        FunctionKernelStmt::Retain { .. }
        | FunctionKernelStmt::Release { .. }
        | FunctionKernelStmt::Destroy { .. }
        | FunctionKernelStmt::Copy { .. }
        | FunctionKernelStmt::Move { .. } => out.push(stmt.clone()),
    }
}

#[cfg(test)]
mod tests {
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "function_ownership_fixture.mitki".into(), fixture.to_owned());
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
    fn ownership_lowering_materializes_explicit_moves_for_heap_values() {
        let backend = compiler_for_fixture(
            r#"
export fun main(): [str] {
    val xs = ["a", "b"];
    xs
}
"#,
        );
        let plan = backend.build_module_plan().expect("module plan");
        let kernel = plan
            .function_kernel
            .as_ref()
            .expect("kernel")
            .direct_functions
            .first()
            .expect("direct function");
        let has_move = kernel
            .entry_block()
            .stmts
            .iter()
            .any(|stmt| matches!(stmt, FunctionKernelStmt::Move { .. }));
        assert!(has_move, "expected ownership lowering to materialize a move op");
        assert!(kernel.decisions.iter().any(|decision| decision == "ownership=explicit"));
    }
}

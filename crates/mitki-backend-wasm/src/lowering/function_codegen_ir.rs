#[cfg(test)]
use std::sync::Arc;

use rustc_hash::{FxHashMap, FxHashSet};
use wasm_encoder::ValType;

use super::function_kernel::{
    FunctionKernelBindingInit, FunctionKernelBindingSource, FunctionKernelBundle,
    FunctionKernelFunction, FunctionKernelId, FunctionKernelKind, FunctionKernelMatchArm,
    FunctionKernelStmt, FunctionKernelTerminator, FunctionKernelValue, FunctionKernelValueKind,
};
use super::plan::ModulePlan;
use super::*;
use crate::abi::backend_ty_value_type;

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmBundle<'db> {
    pub(in crate::backend) direct_functions: Vec<StructuredWasmFunction<'db>>,
    pub(in crate::backend) closures: Vec<StructuredWasmFunction<'db>>,
    pub(in crate::backend) direct_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) closure_indices: FxHashMap<ClosureInstanceKey<'db>, usize>,
}

impl<'db> StructuredWasmBundle<'db> {
    #[allow(dead_code)]
    pub(in crate::backend) fn direct(
        &self,
        instance: &InstanceKey<'db>,
    ) -> Option<&StructuredWasmFunction<'db>> {
        self.direct_indices.get(instance).and_then(|&index| self.direct_functions.get(index))
    }

    #[allow(dead_code)]
    pub(in crate::backend) fn closure(
        &self,
        closure: &ClosureInstanceKey<'db>,
    ) -> Option<&StructuredWasmFunction<'db>> {
        self.closure_indices.get(closure).and_then(|&index| self.closures.get(index))
    }
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmFunction<'db> {
    pub(in crate::backend) id: FunctionKernelId,
    pub(in crate::backend) kind: FunctionKernelKind<'db>,
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::backend) debug_name: String,
    pub(in crate::backend) signature: FunctionSignature,
    pub(in crate::backend) layout: FunctionLayout,
    pub(in crate::backend) legalization: Option<function_legalize::FunctionLegalization<'db>>,
    pub(in crate::backend) extra_locals: Vec<StructuredWasmLocal>,
    pub(in crate::backend) param_inits: Vec<StructuredWasmBindingInit<'db>>,
    pub(in crate::backend) body: Option<StructuredWasmExpr<'db>>,
    pub(in crate::backend) body_ownership_ops: Vec<StructuredWasmOwnershipOp>,
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::backend) decisions: Vec<String>,
}

impl<'db> StructuredWasmFunction<'db> {
    pub(in crate::backend) fn wasm_locals(&self) -> Vec<(u32, ValType)> {
        let mut locals = self.layout.wasm_locals().to_vec();
        for local in &self.extra_locals {
            match locals.last_mut() {
                Some((count, value_type)) if *value_type == local.ty => *count += 1,
                _ => locals.push((1, local.ty)),
            }
        }
        locals
    }
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmLocal {
    pub(in crate::backend) index: u32,
    pub(in crate::backend) ty: ValType,
    #[cfg_attr(not(test), allow(dead_code))]
    pub(in crate::backend) reason: StructuredWasmLocalReason,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum StructuredWasmLocalReason {
    Join {
        #[cfg_attr(not(test), allow(dead_code))]
        source: ExprId,
        #[cfg_attr(not(test), allow(dead_code))]
        label: &'static str,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum StructuredWasmResultArity {
    Unit,
    Scalar(ValType),
    Aggregate,
}

impl StructuredWasmResultArity {
    fn for_abi(abi: &AbiTy, _mode: ResultLoweringMode) -> Result<Self, Diagnostic> {
        match abi {
            AbiTy::Scalar(BackendTy::Unit) => Ok(Self::Unit),
            AbiTy::Scalar(ty) => backend_ty_value_type(*ty).map(Self::Scalar).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing scalar value type",
                    mitki_errors::TextRange::default(),
                )
            }),
            AbiTy::Aggregate(_) => Ok(Self::Aggregate),
        }
    }
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmExpr<'db> {
    pub(in crate::backend) source: ExprId,
    pub(in crate::backend) abi: AbiTy,
    pub(in crate::backend) ownership: ValueOwnership,
    pub(in crate::backend) kind: StructuredWasmExprKind<'db>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum StructuredWasmExprKind<'db> {
    Leaf(FunctionKernelValue<'db>),
    WasmLocal(u32),
    Eqz {
        value: Box<StructuredWasmExpr<'db>>,
    },
    TeeLocal {
        local: u32,
        value: Box<StructuredWasmExpr<'db>>,
    },
    Select {
        cond: Box<StructuredWasmExpr<'db>>,
        then_value: Box<StructuredWasmExpr<'db>>,
        else_value: Box<StructuredWasmExpr<'db>>,
    },
    Block {
        region: StructuredWasmRegion<'db>,
        result_arity: StructuredWasmResultArity,
    },
    If {
        cond: Box<StructuredWasmExpr<'db>>,
        then_region: StructuredWasmRegion<'db>,
        else_region: StructuredWasmRegion<'db>,
        result_arity: StructuredWasmResultArity,
    },
    Match {
        scrutinee: Box<StructuredWasmExpr<'db>>,
        arms: Vec<StructuredWasmMatchArm<'db>>,
        result_arity: StructuredWasmResultArity,
        fallback_unreachable: bool,
    },
    Loop {
        body: Option<Box<StructuredWasmExpr<'db>>>,
        result_arity: StructuredWasmResultArity,
    },
    Break,
    Continue,
    #[allow(dead_code)]
    Unreachable,
}

#[derive(Clone, Debug, Default)]
pub(in crate::backend) struct StructuredWasmRegion<'db> {
    pub(in crate::backend) stmts: Vec<StructuredWasmStmt<'db>>,
    pub(in crate::backend) tail: Option<Box<StructuredWasmExpr<'db>>>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum StructuredWasmStmt<'db> {
    Local {
        name: NameId,
        abi: AbiTy,
        initializer: Option<StructuredWasmExpr<'db>>,
        ownership_ops: Vec<StructuredWasmOwnershipOp>,
    },
    Assign {
        name: NameId,
        abi: AbiTy,
        value: StructuredWasmExpr<'db>,
    },
    If {
        source: ExprId,
        abi: AbiTy,
        ownership: ValueOwnership,
        cond: StructuredWasmExpr<'db>,
        then_region: StructuredWasmRegion<'db>,
        else_region: StructuredWasmRegion<'db>,
        ownership_ops: Vec<StructuredWasmOwnershipOp>,
    },
    Match {
        source: ExprId,
        abi: AbiTy,
        ownership: ValueOwnership,
        scrutinee: StructuredWasmExpr<'db>,
        arms: Vec<StructuredWasmMatchArm<'db>>,
        fallback_unreachable: bool,
        ownership_ops: Vec<StructuredWasmOwnershipOp>,
    },
    Pattern(StructuredWasmBindingInit<'db>),
    Return {
        source: ExprId,
        value: Option<StructuredWasmExpr<'db>>,
        ownership_ops: Vec<StructuredWasmOwnershipOp>,
    },
    Expr {
        expr: StructuredWasmExpr<'db>,
        ownership_ops: Vec<StructuredWasmOwnershipOp>,
    },
    SetLocal {
        #[allow(dead_code)]
        source: ExprId,
        local: u32,
        ty: ValType,
        value: StructuredWasmExpr<'db>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::backend) enum StructuredWasmOwnershipOp {
    Retain,
    Release,
    Destroy,
    Copy,
    Move,
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmBindingInit<'db> {
    pub(in crate::backend) pattern: BackendPattern,
    pub(in crate::backend) source: StructuredWasmBindingSource<'db>,
}

#[derive(Clone, Debug)]
pub(in crate::backend) enum StructuredWasmBindingSource<'db> {
    Expr(StructuredWasmExpr<'db>),
    Param { index: usize, abi: AbiTy, source: ExprId },
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmMatchArm<'db> {
    pub(in crate::backend) pattern: BackendPattern,
    pub(in crate::backend) body: StructuredWasmRegion<'db>,
}

pub(in crate::backend) struct FunctionStackifier;
pub(in crate::backend) struct StructuredWasmPeephole;
pub(in crate::backend) struct StructuredWasmValidator;

impl FunctionStackifier {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<StructuredWasmBundle<'db>, Diagnostic> {
        let mut output = StructuredWasmBundle {
            direct_functions: Vec::new(),
            closures: Vec::new(),
            direct_indices: FxHashMap::default(),
            closure_indices: FxHashMap::default(),
        };

        for function in &bundle.direct_functions {
            let built = StackifyFunction::new(function, backend.control_flow_strategy()).build()?;
            output.direct_indices.insert(
                match &built.kind {
                    FunctionKernelKind::Direct(instance) => instance.clone(),
                    FunctionKernelKind::Closure(_) => unreachable!(),
                },
                output.direct_functions.len(),
            );
            output.direct_functions.push(built);
        }

        for function in &bundle.closures {
            let built = StackifyFunction::new(function, backend.control_flow_strategy()).build()?;
            output.closure_indices.insert(
                match &built.kind {
                    FunctionKernelKind::Closure(closure) => closure.clone(),
                    FunctionKernelKind::Direct(_) => unreachable!(),
                },
                output.closures.len(),
            );
            output.closures.push(built);
        }

        StructuredWasmValidator::validate(backend, plan, &output)?;
        Ok(output)
    }
}

struct StackifyFunction<'a, 'db> {
    function: &'a FunctionKernelFunction<'db>,
    control_flow: ControlFlowStrategy,
    next_local: u32,
    extra_locals: Vec<StructuredWasmLocal>,
    decisions: Vec<String>,
}

impl<'a, 'db> StackifyFunction<'a, 'db> {
    fn new(function: &'a FunctionKernelFunction<'db>, control_flow: ControlFlowStrategy) -> Self {
        Self {
            function,
            control_flow,
            next_local: function.layout.next_local_index(),
            extra_locals: Vec::new(),
            decisions: Vec::new(),
        }
    }

    fn build(mut self) -> Result<StructuredWasmFunction<'db>, Diagnostic> {
        self.decisions.push(format!(
            "result_lowering={}",
            self.control_flow.default_result_lowering().dump_name()
        ));
        self.decisions
            .push(format!("failure_lowering={}", self.control_flow.failure_lowering().dump_name()));
        let param_inits = self
            .function
            .param_inits
            .iter()
            .map(|init| self.stackify_binding_init(init))
            .collect::<Result<Vec<_>, _>>()?;
        let body = self.stackify_body_from_block()?;
        Ok(StructuredWasmFunction {
            id: self.function.id,
            kind: self.function.kind.clone(),
            debug_name: self.function.debug_name.clone(),
            signature: self.function.signature.clone(),
            layout: self.function.layout.clone(),
            legalization: None,
            extra_locals: self.extra_locals,
            param_inits,
            body,
            body_ownership_ops: Vec::new(),
            decisions: self.decisions,
        })
    }

    fn stackify_body_from_block(&mut self) -> Result<Option<StructuredWasmExpr<'db>>, Diagnostic> {
        let block = self.function.entry_block();
        let mut stmts = Vec::new();
        for stmt in &block.stmts {
            match stmt {
                FunctionKernelStmt::Retain { .. }
                | FunctionKernelStmt::Release { .. }
                | FunctionKernelStmt::Destroy { .. }
                | FunctionKernelStmt::Copy { .. }
                | FunctionKernelStmt::Move { .. } => {}
                _ => stmts.push(self.stackify_stmt(stmt)?),
            }
        }
        let tail = match &block.terminator {
            FunctionKernelTerminator::Return { value, .. } => {
                value.as_ref().map(|value| self.stackify_expr(value)).transpose()?.map(Box::new)
            }
            FunctionKernelTerminator::Unreachable { source } => {
                Some(Box::new(StructuredWasmExpr {
                    source: *source,
                    abi: block.result_abi.clone(),
                    ownership: ValueOwnership::None,
                    kind: StructuredWasmExprKind::Unreachable,
                }))
            }
        };

        if stmts.is_empty() && tail.is_none() {
            return Ok(None);
        }

        Ok(Some(StructuredWasmExpr {
            source: block.source,
            abi: block.result_abi.clone(),
            ownership: ValueOwnership::None,
            kind: StructuredWasmExprKind::Block {
                region: StructuredWasmRegion { stmts, tail },
                result_arity: self.result_arity(&block.result_abi)?,
            },
        }))
    }

    fn stackify_binding_init(
        &mut self,
        init: &FunctionKernelBindingInit<'db>,
    ) -> Result<StructuredWasmBindingInit<'db>, Diagnostic> {
        Ok(StructuredWasmBindingInit {
            pattern: init.pattern.clone(),
            source: match &init.source {
                FunctionKernelBindingSource::Value(expr) => {
                    StructuredWasmBindingSource::Expr(self.stackify_expr(expr)?)
                }
                FunctionKernelBindingSource::Param { index, abi, source } => {
                    StructuredWasmBindingSource::Param {
                        index: *index,
                        abi: abi.clone(),
                        source: *source,
                    }
                }
            },
        })
    }

    fn stackify_stmt(
        &mut self,
        stmt: &FunctionKernelStmt<'db>,
    ) -> Result<StructuredWasmStmt<'db>, Diagnostic> {
        Ok(match stmt {
            FunctionKernelStmt::Local { name, abi, initializer } => StructuredWasmStmt::Local {
                name: *name,
                abi: abi.clone(),
                initializer: initializer
                    .as_ref()
                    .map(|expr| self.stackify_expr(expr))
                    .transpose()?,
                ownership_ops: Vec::new(),
            },
            FunctionKernelStmt::Assign { name, abi, value } => StructuredWasmStmt::Assign {
                name: *name,
                abi: abi.clone(),
                value: self.stackify_expr(value)?,
            },
            FunctionKernelStmt::Pattern(init) => {
                StructuredWasmStmt::Pattern(self.stackify_binding_init(init)?)
            }
            FunctionKernelStmt::Return { source, value } => StructuredWasmStmt::Return {
                source: *source,
                value: value.as_ref().map(|expr| self.stackify_expr(expr)).transpose()?,
                ownership_ops: Vec::new(),
            },
            FunctionKernelStmt::Expr(expr) => self.stackify_stmt_expr(expr)?,
            FunctionKernelStmt::Retain { .. }
            | FunctionKernelStmt::Release { .. }
            | FunctionKernelStmt::Destroy { .. }
            | FunctionKernelStmt::Copy { .. }
            | FunctionKernelStmt::Move { .. } => {
                return Err(Diagnostic::error(
                    "internal error: ownership op reached structured Wasm stackification",
                    mitki_errors::TextRange::default(),
                ));
            }
        })
    }

    fn stackify_stmt_expr(
        &mut self,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<StructuredWasmStmt<'db>, Diagnostic> {
        Ok(match &expr.kind {
            FunctionKernelValueKind::If { cond, then_branch, else_branch } => {
                StructuredWasmStmt::If {
                    source: expr.source,
                    abi: expr.abi.clone(),
                    ownership: expr.ownership,
                    cond: self.stackify_expr(cond)?,
                    then_region: then_branch
                        .as_deref()
                        .map(|branch| self.stackify_region_from_expr(branch))
                        .transpose()?
                        .unwrap_or_default(),
                    else_region: else_branch
                        .as_deref()
                        .map(|branch| self.stackify_region_from_expr(branch))
                        .transpose()?
                        .unwrap_or_default(),
                    ownership_ops: Vec::new(),
                }
            }
            FunctionKernelValueKind::Match { scrutinee, arms } => StructuredWasmStmt::Match {
                source: expr.source,
                abi: expr.abi.clone(),
                ownership: expr.ownership,
                scrutinee: self.stackify_expr(scrutinee)?,
                arms: arms
                    .iter()
                    .map(|arm| {
                        Ok(StructuredWasmMatchArm {
                            pattern: arm.pattern.clone(),
                            body: self.stackify_region_from_expr(&arm.body)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?,
                fallback_unreachable: true,
                ownership_ops: Vec::new(),
            },
            _ => StructuredWasmStmt::Expr {
                expr: self.stackify_expr(expr)?,
                ownership_ops: Vec::new(),
            },
        })
    }

    fn stackify_region_from_expr(
        &mut self,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<StructuredWasmRegion<'db>, Diagnostic> {
        match &expr.kind {
            FunctionKernelValueKind::Block { stmts, tail } => Ok(StructuredWasmRegion {
                stmts: stmts
                    .iter()
                    .map(|stmt| self.stackify_stmt(stmt))
                    .collect::<Result<Vec<_>, _>>()?,
                tail: tail.as_ref().map(|tail| self.stackify_expr(tail)).transpose()?.map(Box::new),
            }),
            _ => Ok(StructuredWasmRegion {
                stmts: Vec::new(),
                tail: Some(Box::new(self.stackify_expr(expr)?)),
            }),
        }
    }

    fn stackify_expr(
        &mut self,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<StructuredWasmExpr<'db>, Diagnostic> {
        let result_arity = self.result_arity(&expr.abi)?;
        let ownership = match &expr.kind {
            FunctionKernelValueKind::If { .. } | FunctionKernelValueKind::Match { .. }
                if result_arity != StructuredWasmResultArity::Aggregate
                    && result_arity != StructuredWasmResultArity::Unit
                    && matches!(
                        expr.abi,
                        AbiTy::Scalar(ty) if ty.is_heap_ref() && expr.ownership.is_borrowed()
                    ) =>
            {
                self.decisions.push(format!(
                    "join {:?} promoted borrowed heap ref to owned temp",
                    expr.source
                ));
                ValueOwnership::Owned
            }
            _ => expr.ownership,
        };
        let kind = match &expr.kind {
            FunctionKernelValueKind::Block { stmts, tail } => StructuredWasmExprKind::Block {
                region: StructuredWasmRegion {
                    stmts: stmts
                        .iter()
                        .map(|stmt| self.stackify_stmt(stmt))
                        .collect::<Result<Vec<_>, _>>()?,
                    tail: tail
                        .as_ref()
                        .map(|tail| self.stackify_expr(tail))
                        .transpose()?
                        .map(Box::new),
                },
                result_arity,
            },
            FunctionKernelValueKind::If { cond, then_branch, else_branch } => {
                self.stackify_if_expr(expr, cond, then_branch.as_deref(), else_branch.as_deref())?
            }
            FunctionKernelValueKind::Match { scrutinee, arms } => {
                self.stackify_match_expr(expr, scrutinee, arms)?
            }
            FunctionKernelValueKind::Loop { body } => StructuredWasmExprKind::Loop {
                body: body.as_ref().map(|body| self.stackify_expr(body)).transpose()?.map(Box::new),
                result_arity,
            },
            FunctionKernelValueKind::Break => StructuredWasmExprKind::Break,
            FunctionKernelValueKind::Continue => StructuredWasmExprKind::Continue,
            _ => StructuredWasmExprKind::Leaf(expr.clone()),
        };
        Ok(StructuredWasmExpr { source: expr.source, abi: expr.abi.clone(), ownership, kind })
    }

    fn stackify_if_expr(
        &mut self,
        expr: &FunctionKernelValue<'db>,
        cond: &FunctionKernelValue<'db>,
        then_branch: Option<&FunctionKernelValue<'db>>,
        else_branch: Option<&FunctionKernelValue<'db>>,
    ) -> Result<StructuredWasmExprKind<'db>, Diagnostic> {
        let result_arity = self.result_arity(&expr.abi)?;
        match (result_arity, self.control_flow.result_lowering_mode(&expr.abi)) {
            (StructuredWasmResultArity::Scalar(value_type), ResultLoweringMode::SpillToLocals) => {
                let join = self.alloc_join_local(expr.source, value_type, "if");
                let then_region = self.stackify_join_region(then_branch, join, value_type)?;
                let else_region = self.stackify_join_region(else_branch, join, value_type)?;
                Ok(StructuredWasmExprKind::Block {
                    region: StructuredWasmRegion {
                        stmts: vec![StructuredWasmStmt::If {
                            source: expr.source,
                            abi: AbiTy::Scalar(BackendTy::Unit),
                            ownership: ValueOwnership::None,
                            cond: self.stackify_expr(cond)?,
                            then_region,
                            else_region,
                            ownership_ops: Vec::new(),
                        }],
                        tail: Some(Box::new(StructuredWasmExpr {
                            source: expr.source,
                            abi: expr.abi.clone(),
                            ownership: expr.ownership,
                            kind: StructuredWasmExprKind::WasmLocal(join),
                        })),
                    },
                    result_arity,
                })
            }
            _ => Ok(StructuredWasmExprKind::If {
                cond: Box::new(self.stackify_expr(cond)?),
                then_region: then_branch
                    .map(|branch| self.stackify_region_from_expr(branch))
                    .transpose()?
                    .unwrap_or_default(),
                else_region: else_branch
                    .map(|branch| self.stackify_region_from_expr(branch))
                    .transpose()?
                    .unwrap_or_default(),
                result_arity,
            }),
        }
    }

    fn stackify_match_expr(
        &mut self,
        expr: &FunctionKernelValue<'db>,
        scrutinee: &FunctionKernelValue<'db>,
        arms: &[FunctionKernelMatchArm<'db>],
    ) -> Result<StructuredWasmExprKind<'db>, Diagnostic> {
        let result_arity = self.result_arity(&expr.abi)?;
        match (result_arity, self.control_flow.result_lowering_mode(&expr.abi)) {
            (StructuredWasmResultArity::Scalar(value_type), ResultLoweringMode::SpillToLocals) => {
                let join = self.alloc_join_local(expr.source, value_type, "match");
                let arms = arms
                    .iter()
                    .map(|arm| {
                        Ok(StructuredWasmMatchArm {
                            pattern: arm.pattern.clone(),
                            body: self.stackify_join_region(Some(&arm.body), join, value_type)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?;
                Ok(StructuredWasmExprKind::Block {
                    region: StructuredWasmRegion {
                        stmts: vec![StructuredWasmStmt::Match {
                            source: expr.source,
                            abi: AbiTy::Scalar(BackendTy::Unit),
                            ownership: ValueOwnership::None,
                            scrutinee: self.stackify_expr(scrutinee)?,
                            arms,
                            fallback_unreachable: true,
                            ownership_ops: Vec::new(),
                        }],
                        tail: Some(Box::new(StructuredWasmExpr {
                            source: expr.source,
                            abi: expr.abi.clone(),
                            ownership: expr.ownership,
                            kind: StructuredWasmExprKind::WasmLocal(join),
                        })),
                    },
                    result_arity,
                })
            }
            _ => Ok(StructuredWasmExprKind::Match {
                scrutinee: Box::new(self.stackify_expr(scrutinee)?),
                arms: arms
                    .iter()
                    .map(|arm| {
                        Ok(StructuredWasmMatchArm {
                            pattern: arm.pattern.clone(),
                            body: self.stackify_region_from_expr(&arm.body)?,
                        })
                    })
                    .collect::<Result<Vec<_>, Diagnostic>>()?,
                result_arity,
                fallback_unreachable: true,
            }),
        }
    }

    fn result_arity(&self, abi: &AbiTy) -> Result<StructuredWasmResultArity, Diagnostic> {
        StructuredWasmResultArity::for_abi(abi, self.control_flow.result_lowering_mode(abi))
    }

    fn stackify_join_region(
        &mut self,
        branch: Option<&FunctionKernelValue<'db>>,
        local: u32,
        ty: ValType,
    ) -> Result<StructuredWasmRegion<'db>, Diagnostic> {
        let Some(branch) = branch else {
            return Ok(StructuredWasmRegion::default());
        };
        Ok(StructuredWasmRegion {
            stmts: vec![StructuredWasmStmt::SetLocal {
                source: branch.source,
                local,
                ty,
                value: self.stackify_expr(branch)?,
            }],
            tail: None,
        })
    }

    fn alloc_join_local(&mut self, source: ExprId, ty: ValType, label: &'static str) -> u32 {
        let local = self.next_local;
        self.next_local += 1;
        self.extra_locals.push(StructuredWasmLocal {
            index: local,
            ty,
            reason: StructuredWasmLocalReason::Join { source, label },
        });
        self.decisions.push(format!("join {:?} -> l{} ({label})", source, local));
        local
    }
}

impl StructuredWasmPeephole {
    pub(in crate::backend) fn run<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        mut bundle: StructuredWasmBundle<'db>,
    ) -> Result<StructuredWasmBundle<'db>, Diagnostic> {
        StructuredWasmValidator::validate(backend, plan, &bundle)?;
        for function in &mut bundle.direct_functions {
            peephole_function(function);
        }
        for function in &mut bundle.closures {
            peephole_function(function);
        }
        StructuredWasmValidator::validate(backend, plan, &bundle)?;
        Ok(bundle)
    }
}

impl StructuredWasmValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &StructuredWasmBundle<'db>,
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
                "internal error: structured Wasm direct body set drifted from ordinary \
                 reachability",
                backend.file_range(),
            ));
        }
        if bundle.closures.len() != plan.reachability.closures.len() {
            return Err(Diagnostic::error(
                "internal error: structured Wasm closure body set drifted from reachability",
                backend.file_range(),
            ));
        }

        for (index, function) in bundle.direct_functions.iter().enumerate() {
            let expected_id =
                FunctionKernelId(u32::try_from(index).expect("structured Wasm ids should fit u32"));
            if function.id != expected_id {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm id drifted from direct ordering",
                ));
            }
            match &function.kind {
                FunctionKernelKind::Direct(instance)
                    if expected_direct.get(index) == Some(instance) => {}
                _ => {
                    return Err(structured_wasm_error(
                        backend,
                        &function.kind,
                        "internal error: structured Wasm direct ordering drifted from reachability",
                    ));
                }
            }
            validate_structured_wasm_function(backend, function)?;
        }

        let direct_count = u32::try_from(bundle.direct_functions.len())
            .expect("structured Wasm ids should fit u32");
        for (index, function) in bundle.closures.iter().enumerate() {
            let expected_id =
                FunctionKernelId(direct_count + u32::try_from(index).expect("fit u32"));
            if function.id != expected_id {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm id drifted from closure ordering",
                ));
            }
            match &function.kind {
                FunctionKernelKind::Closure(closure)
                    if plan.reachability.closures.get(index) == Some(closure) => {}
                _ => {
                    return Err(structured_wasm_error(
                        backend,
                        &function.kind,
                        "internal error: structured Wasm closure ordering drifted from \
                         reachability",
                    ));
                }
            }
            validate_structured_wasm_function(backend, function)?;
        }

        Ok(())
    }
}

#[derive(Clone, Default)]
struct ValidationState {
    initialized_names: FxHashSet<NameId>,
    initialized_locals: FxHashSet<u32>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EmitTermination {
    Open,
    Closed,
}

fn validate_structured_wasm_function<'db>(
    backend: &Backend<'db>,
    function: &StructuredWasmFunction<'db>,
) -> Result<(), Diagnostic> {
    for (expected_local, local) in
        (function.layout.next_local_index()..).zip(function.extra_locals.iter())
    {
        if local.index != expected_local {
            return Err(structured_wasm_error(
                backend,
                &function.kind,
                "internal error: structured Wasm locals drifted from deterministic ordering",
            ));
        }
    }

    let mut state = ValidationState::default();
    state.initialized_names.extend(function.layout.param_names.iter().copied());
    for init in &function.param_inits {
        validate_binding_init(backend, function, &mut state, init)?;
    }

    match &function.body {
        Some(body) => {
            validate_emit_expr(backend, function, &state, 0, body)?;
            if body.abi != function.signature.result {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm body result drifted from signature",
                ));
            }
        }
        None if function.signature.result != AbiTy::Scalar(BackendTy::Unit) => {
            return Err(structured_wasm_error(
                backend,
                &function.kind,
                "internal error: structured Wasm body is missing for non-unit function",
            ));
        }
        None => {}
    }

    Ok(())
}

fn validate_binding_init<'db>(
    backend: &Backend<'db>,
    function: &StructuredWasmFunction<'db>,
    state: &mut ValidationState,
    init: &StructuredWasmBindingInit<'db>,
) -> Result<(), Diagnostic> {
    match &init.source {
        StructuredWasmBindingSource::Expr(expr) => {
            validate_emit_expr(backend, function, state, 0, expr)?;
        }
        StructuredWasmBindingSource::Param { index, abi, .. } => {
            let expected = function.signature.params.get(*index).ok_or_else(|| {
                structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm parameter binding referenced missing param",
                )
            })?;
            if expected != abi {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm parameter ABI drifted",
                ));
            }
        }
    }
    bind_pattern_names_emit(backend, function, state, &init.pattern)
}

fn validate_emit_region<'db>(
    backend: &Backend<'db>,
    function: &StructuredWasmFunction<'db>,
    state: &ValidationState,
    loop_depth: usize,
    region: &StructuredWasmRegion<'db>,
    expected_abi: Option<&AbiTy>,
) -> Result<EmitTermination, Diagnostic> {
    let mut state = state.clone();
    let mut terminated = false;
    for stmt in &region.stmts {
        if terminated {
            return Err(structured_wasm_error(
                backend,
                &function.kind,
                "internal error: structured Wasm region contains statements after a terminator",
            ));
        }
        terminated = validate_emit_stmt(backend, function, &mut state, loop_depth, stmt)?
            == EmitTermination::Closed;
    }
    if terminated && region.tail.is_some() {
        return Err(structured_wasm_error(
            backend,
            &function.kind,
            "internal error: structured Wasm region tail appears after a terminator",
        ));
    }
    match (&region.tail, expected_abi) {
        (Some(tail), Some(expected)) => {
            validate_emit_expr(backend, function, &state, loop_depth, tail)?;
            if &tail.abi != expected {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm region tail ABI drifted",
                ));
            }
            Ok(EmitTermination::Open)
        }
        (Some(tail), None) => validate_emit_expr(backend, function, &state, loop_depth, tail),
        (None, Some(expected)) if *expected != AbiTy::Scalar(BackendTy::Unit) => {
            Err(structured_wasm_error(
                backend,
                &function.kind,
                "internal error: structured Wasm region is missing a non-unit tail",
            ))
        }
        (None, _) => Ok(if terminated { EmitTermination::Closed } else { EmitTermination::Open }),
    }
}

fn validate_emit_stmt<'db>(
    backend: &Backend<'db>,
    function: &StructuredWasmFunction<'db>,
    state: &mut ValidationState,
    loop_depth: usize,
    stmt: &StructuredWasmStmt<'db>,
) -> Result<EmitTermination, Diagnostic> {
    match stmt {
        StructuredWasmStmt::Local { name, initializer, .. } => {
            ensure_emit_binding_slot(function, *name)
                .map_err(|message| structured_wasm_error(backend, &function.kind, message))?;
            if let Some(initializer) = initializer {
                validate_emit_expr(backend, function, state, loop_depth, initializer)?;
                state.initialized_names.insert(*name);
            }
            Ok(EmitTermination::Open)
        }
        StructuredWasmStmt::Assign { name, value, .. } => {
            ensure_emit_binding_slot(function, *name)
                .map_err(|message| structured_wasm_error(backend, &function.kind, message))?;
            validate_emit_expr(backend, function, state, loop_depth, value)?;
            state.initialized_names.insert(*name);
            Ok(EmitTermination::Open)
        }
        StructuredWasmStmt::Pattern(init) => {
            validate_binding_init(backend, function, state, init)?;
            Ok(EmitTermination::Open)
        }
        StructuredWasmStmt::If { abi, cond, then_region, else_region, .. } => {
            validate_emit_expr(backend, function, state, loop_depth, cond)?;
            let then_term =
                validate_emit_region(backend, function, state, loop_depth, then_region, Some(abi))?;
            let else_term =
                validate_emit_region(backend, function, state, loop_depth, else_region, Some(abi))?;
            let termination =
                if then_term == EmitTermination::Closed && else_term == EmitTermination::Closed {
                    EmitTermination::Closed
                } else {
                    EmitTermination::Open
                };
            if termination == EmitTermination::Open {
                state.initialized_locals.extend(definite_local_writes_stmt(stmt));
            }
            Ok(termination)
        }
        StructuredWasmStmt::Match { scrutinee, arms, abi, fallback_unreachable, .. } => {
            if !fallback_unreachable {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm statement match lost its explicit \
                     unreachable fallback",
                ));
            }
            validate_emit_expr(backend, function, state, loop_depth, scrutinee)?;
            let mut open_arm_count = 0usize;
            for arm in arms {
                let mut arm_state = state.clone();
                bind_pattern_names_emit(backend, function, &mut arm_state, &arm.pattern)?;
                let term = validate_emit_region(
                    backend,
                    function,
                    &arm_state,
                    loop_depth,
                    &arm.body,
                    Some(abi),
                )?;
                if term == EmitTermination::Open {
                    open_arm_count += 1;
                }
            }
            if open_arm_count > 0 {
                state.initialized_locals.extend(definite_local_writes_stmt(stmt));
                Ok(EmitTermination::Open)
            } else {
                Ok(EmitTermination::Closed)
            }
        }
        StructuredWasmStmt::Return { value, .. } => {
            match (value, &function.signature.result) {
                (Some(expr), _) => {
                    validate_emit_expr(backend, function, state, loop_depth, expr)?;
                    if expr.abi != function.signature.result {
                        return Err(structured_wasm_error(
                            backend,
                            &function.kind,
                            "internal error: structured Wasm return result drifted from signature",
                        ));
                    }
                }
                (None, AbiTy::Scalar(BackendTy::Unit)) => {}
                (None, _) => {
                    return Err(structured_wasm_error(
                        backend,
                        &function.kind,
                        "internal error: structured Wasm return is missing a value",
                    ));
                }
            }
            Ok(EmitTermination::Closed)
        }
        StructuredWasmStmt::Expr { expr, .. } => {
            let termination = validate_emit_expr(backend, function, state, loop_depth, expr)?;
            if termination == EmitTermination::Open {
                state.initialized_locals.extend(definite_local_writes_expr(expr));
            }
            Ok(termination)
        }
        StructuredWasmStmt::SetLocal { local, ty, value, .. } => {
            if !function
                .extra_locals
                .iter()
                .any(|candidate| candidate.index == *local && candidate.ty == *ty)
            {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm store referenced an unknown extra local",
                ));
            }
            validate_emit_expr(backend, function, state, loop_depth, value)?;
            state.initialized_locals.insert(*local);
            Ok(EmitTermination::Open)
        }
    }
}

fn validate_emit_expr<'db>(
    backend: &Backend<'db>,
    function: &StructuredWasmFunction<'db>,
    state: &ValidationState,
    loop_depth: usize,
    expr: &StructuredWasmExpr<'db>,
) -> Result<EmitTermination, Diagnostic> {
    match &expr.kind {
        StructuredWasmExprKind::Leaf(leaf) => {
            validate_leaf_expr(function, state, leaf)
                .map_err(|message| structured_wasm_error(backend, &function.kind, message))?;
            Ok(EmitTermination::Open)
        }
        StructuredWasmExprKind::WasmLocal(local) => {
            if !state.initialized_locals.contains(local) {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm used an extra local before definition",
                ));
            }
            Ok(EmitTermination::Open)
        }
        StructuredWasmExprKind::Eqz { value } => {
            validate_emit_expr(backend, function, state, loop_depth, value)
        }
        StructuredWasmExprKind::TeeLocal { local, value } => {
            if !function.extra_locals.iter().any(|candidate| candidate.index == *local) {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm tee referenced an unknown extra local",
                ));
            }
            validate_emit_expr(backend, function, state, loop_depth, value)
        }
        StructuredWasmExprKind::Select { cond, then_value, else_value } => {
            validate_emit_expr(backend, function, state, loop_depth, cond)?;
            validate_emit_expr(backend, function, state, loop_depth, then_value)?;
            validate_emit_expr(backend, function, state, loop_depth, else_value)?;
            Ok(EmitTermination::Open)
        }
        StructuredWasmExprKind::Block { region, result_arity } => {
            if *result_arity
                != StructuredWasmResultArity::for_abi(
                    &expr.abi,
                    backend.control_flow_strategy().result_lowering_mode(&expr.abi),
                )?
            {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm block result arity drifted from its ABI",
                ));
            }
            validate_emit_region(backend, function, state, loop_depth, region, Some(&expr.abi))
        }
        StructuredWasmExprKind::If { cond, then_region, else_region, result_arity } => {
            if *result_arity
                != StructuredWasmResultArity::for_abi(
                    &expr.abi,
                    backend.control_flow_strategy().result_lowering_mode(&expr.abi),
                )?
            {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm if result arity drifted from its ABI",
                ));
            }
            validate_emit_expr(backend, function, state, loop_depth, cond)?;
            let then_term = validate_emit_region(
                backend,
                function,
                state,
                loop_depth,
                then_region,
                Some(&expr.abi),
            )?;
            let else_term = validate_emit_region(
                backend,
                function,
                state,
                loop_depth,
                else_region,
                Some(&expr.abi),
            )?;
            Ok(if then_term == EmitTermination::Closed && else_term == EmitTermination::Closed {
                EmitTermination::Closed
            } else {
                EmitTermination::Open
            })
        }
        StructuredWasmExprKind::Match { scrutinee, arms, result_arity, fallback_unreachable } => {
            if *result_arity
                != StructuredWasmResultArity::for_abi(
                    &expr.abi,
                    backend.control_flow_strategy().result_lowering_mode(&expr.abi),
                )?
            {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm match result arity drifted from its ABI",
                ));
            }
            if !fallback_unreachable {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm match lost its explicit unreachable fallback",
                ));
            }
            validate_emit_expr(backend, function, state, loop_depth, scrutinee)?;
            for arm in arms {
                let mut arm_state = state.clone();
                bind_pattern_names_emit(backend, function, &mut arm_state, &arm.pattern)?;
                validate_emit_region(
                    backend,
                    function,
                    &arm_state,
                    loop_depth,
                    &arm.body,
                    Some(&expr.abi),
                )?;
            }
            Ok(EmitTermination::Open)
        }
        StructuredWasmExprKind::Loop { body, result_arity } => {
            if *result_arity != StructuredWasmResultArity::Unit {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm loop result arity is unsupported",
                ));
            }
            if let Some(body) = body {
                validate_emit_expr(backend, function, state, loop_depth + 1, body)?;
            }
            Ok(EmitTermination::Open)
        }
        StructuredWasmExprKind::Break | StructuredWasmExprKind::Continue => {
            if loop_depth == 0 {
                return Err(structured_wasm_error(
                    backend,
                    &function.kind,
                    "internal error: structured Wasm loop control escaped loop nesting",
                ));
            }
            Ok(EmitTermination::Closed)
        }
        StructuredWasmExprKind::Unreachable => Ok(EmitTermination::Closed),
    }
}

fn validate_leaf_expr(
    function: &StructuredWasmFunction<'_>,
    state: &ValidationState,
    expr: &FunctionKernelValue<'_>,
) -> Result<(), String> {
    match &expr.kind {
        FunctionKernelValueKind::Local(name) => {
            ensure_emit_binding_slot(function, *name)?;
            if !state.initialized_names.contains(name) {
                return Err("internal error: structured Wasm used a binding before initialization"
                    .to_owned());
            }
        }
        FunctionKernelValueKind::Clone { value }
        | FunctionKernelValueKind::Prefix { expr: value, .. }
        | FunctionKernelValueKind::AddrOffset { base: value, .. }
        | FunctionKernelValueKind::MemoryRead { addr: value, .. }
        | FunctionKernelValueKind::Field { base: value, .. } => {
            validate_leaf_expr(function, state, value)?
        }
        FunctionKernelValueKind::MemoryWrite { addr, value } => {
            validate_leaf_expr(function, state, addr)?;
            validate_leaf_expr(function, state, value)?;
        }
        FunctionKernelValueKind::PointerAdd { ptr, count, .. } => {
            validate_leaf_expr(function, state, ptr)?;
            validate_leaf_expr(function, state, count)?;
        }
        FunctionKernelValueKind::StringFromBytes { ptr, len } => {
            validate_leaf_expr(function, state, ptr)?;
            validate_leaf_expr(function, state, len)?;
        }
        FunctionKernelValueKind::Array { items, .. } => {
            for item in items {
                validate_leaf_expr(function, state, item)?;
            }
        }
        FunctionKernelValueKind::ArrayRepeat { value, len, .. } => {
            validate_leaf_expr(function, state, value)?;
            validate_leaf_expr(function, state, len)?;
        }
        FunctionKernelValueKind::Call { args, .. }
        | FunctionKernelValueKind::VariantCall { args, .. } => {
            for arg in args {
                validate_leaf_expr(function, state, arg)?;
            }
        }
        FunctionKernelValueKind::IndirectCall { callee, args, .. } => {
            validate_leaf_expr(function, state, callee)?;
            for arg in args {
                validate_leaf_expr(function, state, arg)?;
            }
        }
        FunctionKernelValueKind::Binary { lhs, rhs, .. } => {
            validate_leaf_expr(function, state, lhs)?;
            validate_leaf_expr(function, state, rhs)?;
        }
        FunctionKernelValueKind::Tuple { fields } | FunctionKernelValueKind::Struct { fields } => {
            for field in fields {
                validate_leaf_expr(function, state, &field.value)?;
            }
        }
        FunctionKernelValueKind::Union { value, .. } => {
            if let Some(value) = value {
                validate_leaf_expr(function, state, value)?;
            }
        }
        FunctionKernelValueKind::ClosureValue { env, .. } => {
            for field in &env.fields {
                validate_leaf_expr(function, state, &field.value)?;
            }
        }
        FunctionKernelValueKind::Block { .. }
        | FunctionKernelValueKind::If { .. }
        | FunctionKernelValueKind::Match { .. }
        | FunctionKernelValueKind::Loop { .. }
        | FunctionKernelValueKind::Break
        | FunctionKernelValueKind::Continue => {
            return Err(
                "internal error: structured Wasm leaf retained kernel control flow".to_owned()
            );
        }
        FunctionKernelValueKind::Capture(_)
        | FunctionKernelValueKind::FunctionValue { .. }
        | FunctionKernelValueKind::Bool(_)
        | FunctionKernelValueKind::Int(_)
        | FunctionKernelValueKind::Float(_)
        | FunctionKernelValueKind::String(_)
        | FunctionKernelValueKind::Char(_)
        | FunctionKernelValueKind::Unit
        | FunctionKernelValueKind::StackAddr { .. }
        | FunctionKernelValueKind::VariantValue { .. } => {}
    }
    Ok(())
}

fn ensure_emit_binding_slot(
    function: &StructuredWasmFunction<'_>,
    name: NameId,
) -> Result<(), String> {
    if function.layout.lookups.slots.contains_key(&name)
        || function.layout.lookups.param_names.contains(&name)
    {
        Ok(())
    } else {
        Err(format!(
            "internal error: structured Wasm referenced binding `{}` without a planned storage \
             slot",
            format_args!("{name:?}")
        ))
    }
}

fn bind_pattern_names_emit(
    backend: &Backend<'_>,
    function: &StructuredWasmFunction<'_>,
    state: &mut ValidationState,
    pattern: &BackendPattern,
) -> Result<(), Diagnostic> {
    let mut names = Vec::new();
    pattern.binding_names(&mut names);
    for name in names {
        ensure_emit_binding_slot(function, name)
            .map_err(|message| structured_wasm_error(backend, &function.kind, message))?;
        state.initialized_names.insert(name);
    }
    Ok(())
}

fn structured_wasm_error<'db>(
    backend: &Backend<'db>,
    kind: &FunctionKernelKind<'db>,
    message: impl Into<String>,
) -> Diagnostic {
    let location = match kind {
        FunctionKernelKind::Direct(instance) => instance.location,
        FunctionKernelKind::Closure(closure) => closure.owner,
    };
    backend.diagnostic_at_function(location, message.into(), backend.function_range(location))
}

fn peephole_function(function: &mut StructuredWasmFunction<'_>) {
    for init in &mut function.param_inits {
        if let StructuredWasmBindingSource::Expr(expr) = &mut init.source {
            peephole_expr(expr);
        }
    }
    if let Some(body) = &mut function.body {
        peephole_expr(body);
    }
}

fn peephole_expr(expr: &mut StructuredWasmExpr<'_>) {
    let expr_source = expr.source;
    let expr_abi = expr.abi.clone();
    let expr_ownership = expr.ownership;
    match &mut expr.kind {
        StructuredWasmExprKind::Leaf(backend) => {
            peephole_leaf_expr(backend);
            if let Some(rewritten) =
                rewrite_leaf_compare_to_zero(expr_source, expr_abi.clone(), expr_ownership, backend)
            {
                *expr = rewritten;
            }
        }
        StructuredWasmExprKind::Eqz { value } => peephole_expr(value),
        StructuredWasmExprKind::TeeLocal { value, .. } => peephole_expr(value),
        StructuredWasmExprKind::Select { cond, then_value, else_value } => {
            peephole_expr(cond);
            peephole_expr(then_value);
            peephole_expr(else_value);
        }
        StructuredWasmExprKind::Block { region, .. } => {
            peephole_region(region);
            apply_block_peepholes(expr_source, expr_abi, expr_ownership, region);
        }
        StructuredWasmExprKind::If { cond, then_region, else_region, .. } => {
            peephole_expr(cond);
            peephole_region(then_region);
            peephole_region(else_region);
            if region_is_empty(then_region) && !region_is_empty(else_region) {
                let inverted = StructuredWasmExpr {
                    source: cond.source,
                    abi: cond.abi.clone(),
                    ownership: cond.ownership,
                    kind: StructuredWasmExprKind::Eqz { value: cond.clone() },
                };
                **cond = inverted;
                std::mem::swap(then_region, else_region);
            }
        }
        StructuredWasmExprKind::Match { scrutinee, arms, .. } => {
            peephole_expr(scrutinee);
            for arm in arms {
                peephole_region(&mut arm.body);
            }
        }
        StructuredWasmExprKind::Loop { body, .. } => {
            if let Some(body) = body {
                peephole_expr(body);
            }
        }
        StructuredWasmExprKind::WasmLocal(_)
        | StructuredWasmExprKind::Break
        | StructuredWasmExprKind::Continue
        | StructuredWasmExprKind::Unreachable => {}
    }
}

fn peephole_region(region: &mut StructuredWasmRegion<'_>) {
    for stmt in &mut region.stmts {
        match stmt {
            StructuredWasmStmt::Local { initializer, .. } => {
                if let Some(initializer) = initializer {
                    peephole_expr(initializer);
                }
            }
            StructuredWasmStmt::Assign { value, .. } => peephole_expr(value),
            StructuredWasmStmt::If { cond, then_region, else_region, .. } => {
                peephole_expr(cond);
                peephole_region(then_region);
                peephole_region(else_region);
                if region_is_empty(then_region) && !region_is_empty(else_region) {
                    let inverted = StructuredWasmExpr {
                        source: cond.source,
                        abi: cond.abi.clone(),
                        ownership: cond.ownership,
                        kind: StructuredWasmExprKind::Eqz { value: Box::new(cond.clone()) },
                    };
                    *cond = inverted;
                    std::mem::swap(then_region, else_region);
                }
            }
            StructuredWasmStmt::Match { scrutinee, arms, .. } => {
                peephole_expr(scrutinee);
                for arm in arms {
                    peephole_region(&mut arm.body);
                }
            }
            StructuredWasmStmt::Pattern(init) => {
                if let StructuredWasmBindingSource::Expr(expr) = &mut init.source {
                    peephole_expr(expr);
                }
            }
            StructuredWasmStmt::Return { value, .. } => {
                if let Some(value) = value {
                    peephole_expr(value);
                }
            }
            StructuredWasmStmt::Expr { expr, .. } => peephole_expr(expr),
            StructuredWasmStmt::SetLocal { value, .. } => peephole_expr(value),
        }
    }
    if let Some(tail) = &mut region.tail {
        peephole_expr(tail);
    }
}

fn apply_block_peepholes(
    source: ExprId,
    abi: AbiTy,
    ownership: ValueOwnership,
    region: &mut StructuredWasmRegion<'_>,
) {
    if let Some(StructuredWasmExpr { kind: StructuredWasmExprKind::WasmLocal(local), .. }) =
        region.tail.as_deref()
    {
        if let Some(StructuredWasmStmt::SetLocal { local: set_local, value, .. }) =
            region.stmts.last()
            && *set_local == *local
            && matches!(value.kind, StructuredWasmExprKind::WasmLocal(_))
        {
            region.tail = Some(Box::new(value.clone()));
            region.stmts.pop();
            return;
        }
        if let Some(StructuredWasmStmt::SetLocal { local: set_local, value, .. }) =
            region.stmts.last()
            && *set_local == *local
        {
            region.tail = Some(Box::new(StructuredWasmExpr {
                source: value.source,
                abi: value.abi.clone(),
                ownership: value.ownership,
                kind: StructuredWasmExprKind::TeeLocal {
                    local: *local,
                    value: Box::new(value.clone()),
                },
            }));
            region.stmts.pop();
            return;
        }
    }

    let Some(StructuredWasmExpr { kind: StructuredWasmExprKind::WasmLocal(local), .. }) =
        region.tail.as_deref()
    else {
        return;
    };
    let Some(if_expr) = region.stmts.last().and_then(|stmt| match stmt {
        StructuredWasmStmt::If {
            source, abi, ownership, cond, then_region, else_region, ..
        } => Some(StructuredWasmExpr {
            source: *source,
            abi: abi.clone(),
            ownership: *ownership,
            kind: StructuredWasmExprKind::If {
                cond: Box::new(cond.clone()),
                then_region: then_region.clone(),
                else_region: else_region.clone(),
                result_arity: StructuredWasmResultArity::Unit,
            },
        }),
        StructuredWasmStmt::Expr { expr, .. } => Some(expr.clone()),
        _ => None,
    }) else {
        return;
    };
    let StructuredWasmExprKind::If { cond, then_region, else_region, result_arity } = &if_expr.kind
    else {
        return;
    };
    if *result_arity != StructuredWasmResultArity::Unit {
        return;
    }
    let Some(then_value) = single_region_set_local(then_region, *local) else {
        return;
    };
    let Some(else_value) = single_region_set_local(else_region, *local) else {
        return;
    };
    if !is_pure_scalar_expr(cond)
        || !is_pure_scalar_expr(&then_value)
        || !is_pure_scalar_expr(&else_value)
    {
        return;
    }
    region.stmts.pop();
    region.tail = Some(Box::new(StructuredWasmExpr {
        source,
        abi,
        ownership,
        kind: StructuredWasmExprKind::Select {
            cond: cond.clone(),
            then_value: Box::new(then_value),
            else_value: Box::new(else_value),
        },
    }));
}

fn single_region_set_local<'db>(
    region: &StructuredWasmRegion<'db>,
    local: u32,
) -> Option<StructuredWasmExpr<'db>> {
    if region.tail.is_some() || region.stmts.len() != 1 {
        return None;
    }
    match region.stmts.first()? {
        StructuredWasmStmt::SetLocal { local: set_local, value, .. } if *set_local == local => {
            Some(value.clone())
        }
        _ => None,
    }
}

fn region_is_empty(region: &StructuredWasmRegion<'_>) -> bool {
    region.stmts.is_empty() && region.tail.is_none()
}

fn is_pure_scalar_expr(expr: &StructuredWasmExpr<'_>) -> bool {
    match &expr.kind {
        StructuredWasmExprKind::WasmLocal(_) | StructuredWasmExprKind::Eqz { .. } => true,
        StructuredWasmExprKind::Leaf(backend) => is_pure_kernel_scalar_expr(backend),
        StructuredWasmExprKind::Select { cond, then_value, else_value } => {
            is_pure_scalar_expr(cond)
                && is_pure_scalar_expr(then_value)
                && is_pure_scalar_expr(else_value)
        }
        _ => false,
    }
}

fn is_pure_kernel_scalar_expr(expr: &FunctionKernelValue<'_>) -> bool {
    match &expr.kind {
        FunctionKernelValueKind::Local(_)
        | FunctionKernelValueKind::Bool(_)
        | FunctionKernelValueKind::Int(_)
        | FunctionKernelValueKind::Float(_)
        | FunctionKernelValueKind::String(_)
        | FunctionKernelValueKind::Char(_)
        | FunctionKernelValueKind::Unit => true,
        FunctionKernelValueKind::Binary { lhs, rhs, .. } => {
            is_pure_kernel_scalar_expr(lhs) && is_pure_kernel_scalar_expr(rhs)
        }
        FunctionKernelValueKind::Prefix { expr, .. } => is_pure_kernel_scalar_expr(expr),
        _ => false,
    }
}

fn rewrite_leaf_compare_to_zero<'db>(
    source: ExprId,
    abi: AbiTy,
    ownership: ValueOwnership,
    backend: &FunctionKernelValue<'db>,
) -> Option<StructuredWasmExpr<'db>> {
    let FunctionKernelValueKind::Binary { op: BackendBinaryOp::Eq, lhs, rhs } = &backend.kind
    else {
        return None;
    };
    let value = if is_zero_literal(lhs) {
        rhs.as_ref()
    } else if is_zero_literal(rhs) {
        lhs.as_ref()
    } else {
        return None;
    };
    match value.abi {
        AbiTy::Scalar(BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_))
        | AbiTy::Scalar(BackendTy::I64) => Some(StructuredWasmExpr {
            source,
            abi,
            ownership,
            kind: StructuredWasmExprKind::Eqz {
                value: Box::new(StructuredWasmExpr {
                    source: value.source,
                    abi: value.abi.clone(),
                    ownership: value.ownership,
                    kind: StructuredWasmExprKind::Leaf(value.clone()),
                }),
            },
        }),
        _ => None,
    }
}

fn is_zero_literal(expr: &FunctionKernelValue<'_>) -> bool {
    matches!(
        (&expr.abi, &expr.kind),
        (
            AbiTy::Scalar(BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_)),
            FunctionKernelValueKind::Int(0)
        ) | (AbiTy::Scalar(BackendTy::I64), FunctionKernelValueKind::Int(0))
    )
}

fn definite_local_writes_expr(expr: &StructuredWasmExpr<'_>) -> FxHashSet<u32> {
    let (falls_through, locals) = fallthrough_local_writes_expr(expr);
    if falls_through { locals } else { FxHashSet::default() }
}

fn definite_local_writes_stmt(stmt: &StructuredWasmStmt<'_>) -> FxHashSet<u32> {
    let (falls_through, locals) = fallthrough_local_writes_stmt(stmt);
    if falls_through { locals } else { FxHashSet::default() }
}

fn fallthrough_local_writes_expr(expr: &StructuredWasmExpr<'_>) -> (bool, FxHashSet<u32>) {
    match &expr.kind {
        StructuredWasmExprKind::Leaf(_)
        | StructuredWasmExprKind::WasmLocal(_)
        | StructuredWasmExprKind::Eqz { .. }
        | StructuredWasmExprKind::Select { .. } => (true, FxHashSet::default()),
        StructuredWasmExprKind::TeeLocal { local, .. } => {
            let mut locals = FxHashSet::default();
            locals.insert(*local);
            (true, locals)
        }
        StructuredWasmExprKind::Block { region, .. } => fallthrough_local_writes_region(region),
        StructuredWasmExprKind::If { then_region, else_region, .. } => {
            let (then_open, then_writes) = fallthrough_local_writes_region(then_region);
            let (else_open, else_writes) = fallthrough_local_writes_region(else_region);
            match (then_open, else_open) {
                (true, true) => (true, intersect_local_writes(&then_writes, &else_writes)),
                (true, false) => (true, then_writes),
                (false, true) => (true, else_writes),
                (false, false) => (false, FxHashSet::default()),
            }
        }
        StructuredWasmExprKind::Match { arms, fallback_unreachable, .. } => {
            let mut open_writes = Vec::new();
            for arm in arms {
                let (open, writes) = fallthrough_local_writes_region(&arm.body);
                if open {
                    open_writes.push(writes);
                }
            }
            if open_writes.is_empty() {
                if *fallback_unreachable {
                    (false, FxHashSet::default())
                } else {
                    (true, FxHashSet::default())
                }
            } else {
                let mut iter = open_writes.into_iter();
                let first = iter.next().expect("non-empty writes");
                let merged = iter.fold(first, |acc, writes| intersect_local_writes(&acc, &writes));
                (true, merged)
            }
        }
        StructuredWasmExprKind::Loop { .. } => (true, FxHashSet::default()),
        StructuredWasmExprKind::Break
        | StructuredWasmExprKind::Continue
        | StructuredWasmExprKind::Unreachable => (false, FxHashSet::default()),
    }
}

fn fallthrough_local_writes_region(region: &StructuredWasmRegion<'_>) -> (bool, FxHashSet<u32>) {
    let mut locals = FxHashSet::default();
    for stmt in &region.stmts {
        let (open, writes) = fallthrough_local_writes_stmt(stmt);
        if open {
            locals.extend(writes);
        } else {
            return (false, FxHashSet::default());
        }
    }
    if let Some(tail) = region.tail.as_deref() {
        let (open, writes) = fallthrough_local_writes_expr(tail);
        if open {
            locals.extend(writes);
            (true, locals)
        } else {
            (false, FxHashSet::default())
        }
    } else {
        (true, locals)
    }
}

fn fallthrough_local_writes_stmt(stmt: &StructuredWasmStmt<'_>) -> (bool, FxHashSet<u32>) {
    match stmt {
        StructuredWasmStmt::Return { .. } => (false, FxHashSet::default()),
        StructuredWasmStmt::SetLocal { local, .. } => {
            let mut locals = FxHashSet::default();
            locals.insert(*local);
            (true, locals)
        }
        StructuredWasmStmt::If { then_region, else_region, .. } => {
            let (then_open, then_writes) = fallthrough_local_writes_region(then_region);
            let (else_open, else_writes) = fallthrough_local_writes_region(else_region);
            match (then_open, else_open) {
                (true, true) => (true, intersect_local_writes(&then_writes, &else_writes)),
                (true, false) => (true, then_writes),
                (false, true) => (true, else_writes),
                (false, false) => (false, FxHashSet::default()),
            }
        }
        StructuredWasmStmt::Match { arms, fallback_unreachable, .. } => {
            let mut open_writes = Vec::new();
            for arm in arms {
                let (open, writes) = fallthrough_local_writes_region(&arm.body);
                if open {
                    open_writes.push(writes);
                }
            }
            if open_writes.is_empty() {
                if *fallback_unreachable {
                    (false, FxHashSet::default())
                } else {
                    (true, FxHashSet::default())
                }
            } else {
                let mut iter = open_writes.into_iter();
                let first = iter.next().expect("non-empty writes");
                let merged = iter.fold(first, |acc, writes| intersect_local_writes(&acc, &writes));
                (true, merged)
            }
        }
        StructuredWasmStmt::Expr { expr, .. } => fallthrough_local_writes_expr(expr),
        StructuredWasmStmt::Local { .. }
        | StructuredWasmStmt::Assign { .. }
        | StructuredWasmStmt::Pattern(_) => (true, FxHashSet::default()),
    }
}

fn intersect_local_writes(lhs: &FxHashSet<u32>, rhs: &FxHashSet<u32>) -> FxHashSet<u32> {
    lhs.intersection(rhs).copied().collect()
}

fn peephole_leaf_expr(expr: &mut FunctionKernelValue<'_>) {
    match &mut expr.kind {
        FunctionKernelValueKind::Clone { value }
        | FunctionKernelValueKind::Prefix { expr: value, .. }
        | FunctionKernelValueKind::AddrOffset { base: value, .. }
        | FunctionKernelValueKind::MemoryRead { addr: value, .. }
        | FunctionKernelValueKind::Field { base: value, .. } => peephole_leaf_expr(value),
        FunctionKernelValueKind::MemoryWrite { addr, value } => {
            peephole_leaf_expr(addr);
            peephole_leaf_expr(value);
        }
        FunctionKernelValueKind::PointerAdd { ptr, count, .. } => {
            peephole_leaf_expr(ptr);
            peephole_leaf_expr(count);
        }
        FunctionKernelValueKind::StringFromBytes { ptr, len } => {
            peephole_leaf_expr(ptr);
            peephole_leaf_expr(len);
        }
        FunctionKernelValueKind::Array { items, .. } => {
            for item in items {
                peephole_leaf_expr(item);
            }
        }
        FunctionKernelValueKind::ArrayRepeat { value, len, .. } => {
            peephole_leaf_expr(value);
            peephole_leaf_expr(len);
        }
        FunctionKernelValueKind::Block { stmts, tail } => {
            for stmt in stmts {
                peephole_leaf_stmt(stmt);
            }
            if let Some(tail) = tail {
                peephole_leaf_expr(tail);
            }
        }
        FunctionKernelValueKind::Call { args, .. }
        | FunctionKernelValueKind::VariantCall { args, .. } => {
            for arg in args {
                peephole_leaf_expr(arg);
            }
        }
        FunctionKernelValueKind::IndirectCall { callee, args, .. } => {
            peephole_leaf_expr(callee);
            for arg in args {
                peephole_leaf_expr(arg);
            }
        }
        FunctionKernelValueKind::Binary { lhs, rhs, .. } => {
            peephole_leaf_expr(lhs);
            peephole_leaf_expr(rhs);
        }
        FunctionKernelValueKind::If { cond, then_branch, else_branch } => {
            peephole_leaf_expr(cond);
            if let Some(then_branch) = then_branch {
                peephole_leaf_expr(then_branch);
            }
            if let Some(else_branch) = else_branch {
                peephole_leaf_expr(else_branch);
            }
        }
        FunctionKernelValueKind::Match { scrutinee, arms } => {
            peephole_leaf_expr(scrutinee);
            for arm in arms {
                peephole_leaf_expr(&mut arm.body);
            }
        }
        FunctionKernelValueKind::Loop { body } => {
            if let Some(body) = body {
                peephole_leaf_expr(body);
            }
        }
        FunctionKernelValueKind::Tuple { fields } | FunctionKernelValueKind::Struct { fields } => {
            for field in fields {
                peephole_leaf_expr(&mut field.value);
            }
        }
        FunctionKernelValueKind::Union { value, .. } => {
            if let Some(value) = value {
                peephole_leaf_expr(value);
            }
        }
        FunctionKernelValueKind::ClosureValue { env, .. } => {
            for field in &mut env.fields {
                peephole_leaf_expr(&mut field.value);
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
        | FunctionKernelValueKind::Break
        | FunctionKernelValueKind::Continue
        | FunctionKernelValueKind::VariantValue { .. } => {}
    }

    fold_leaf_mem_offset(expr);
}

fn peephole_leaf_stmt(stmt: &mut FunctionKernelStmt<'_>) {
    match stmt {
        FunctionKernelStmt::Local { initializer, .. } => {
            if let Some(initializer) = initializer {
                peephole_leaf_expr(initializer);
            }
        }
        FunctionKernelStmt::Assign { value, .. } => peephole_leaf_expr(value),
        FunctionKernelStmt::Pattern(init) => {
            if let FunctionKernelBindingSource::Value(expr) = &mut init.source {
                peephole_leaf_expr(expr);
            }
        }
        FunctionKernelStmt::Return { value, .. } => {
            if let Some(value) = value {
                peephole_leaf_expr(value);
            }
        }
        FunctionKernelStmt::Expr(expr) => peephole_leaf_expr(expr),
        FunctionKernelStmt::Retain { value, .. }
        | FunctionKernelStmt::Release { value, .. }
        | FunctionKernelStmt::Destroy { value, .. }
        | FunctionKernelStmt::Copy { value, .. }
        | FunctionKernelStmt::Move { value, .. } => peephole_leaf_expr(value),
    }
}

fn fold_leaf_mem_offset(expr: &mut FunctionKernelValue<'_>) {
    let FunctionKernelValueKind::MemoryRead { addr, access } = &mut expr.kind else {
        return;
    };
    let Some(existing) = access.take() else {
        return;
    };
    let FunctionKernelValueKind::AddrOffset { base, offset } = &mut addr.kind else {
        *access = Some(existing);
        return;
    };
    let Some(total) = existing.offset.checked_add(*offset) else {
        *access = Some(existing);
        return;
    };
    let folded = MemAccess { offset: total, ..existing };
    let replacement = (**base).clone();
    **addr = replacement;
    *access = Some(folded);
}

#[cfg(test)]
impl<'db> StructuredWasmBundle<'db> {
    pub(crate) fn dump(&self, _db: &dyn salsa::Database) -> String {
        use std::fmt::Write as _;

        let mut output = String::new();
        writeln!(&mut output, "structured_wasm.direct:").expect("write string");
        for function in &self.direct_functions {
            dump_structured_wasm_function(&mut output, function);
        }
        writeln!(&mut output, "structured_wasm.closures:").expect("write string");
        for function in &self.closures {
            dump_structured_wasm_function(&mut output, function);
        }
        output
    }
}

#[cfg(test)]
fn dump_structured_wasm_function(output: &mut String, function: &StructuredWasmFunction<'_>) {
    use std::fmt::Write as _;

    writeln!(
        output,
        "  - [m{}] {} :: ({}) -> {}",
        function.id.0,
        function.debug_name,
        function.signature.params.iter().map(format_abi_ty).collect::<Vec<_>>().join(", "),
        format_abi_ty(&function.signature.result)
    )
    .expect("write string");
    writeln!(output, "    storage: {}", function.layout.dump_storage().replace('\n', " | "))
        .expect("write string");
    if function.extra_locals.is_empty() {
        writeln!(output, "    extra_locals: _").expect("write string");
    } else {
        let locals = function
            .extra_locals
            .iter()
            .map(|local| {
                let reason = match &local.reason {
                    StructuredWasmLocalReason::Join { source, label } => {
                        format!("join({source:?},{label})")
                    }
                };
                format!("l{}:{:?}:{reason}", local.index, local.ty)
            })
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(output, "    extra_locals: {locals}").expect("write string");
    }
    if function.decisions.is_empty() {
        writeln!(output, "    decisions: _").expect("write string");
    } else {
        writeln!(output, "    decisions: {}", function.decisions.join(" | "))
            .expect("write string");
    }
    writeln!(output, "    body: {}", dump_emit_expr_summary(&function.body)).expect("write string");
}

#[cfg(test)]
fn dump_emit_expr_summary(expr: &Option<StructuredWasmExpr<'_>>) -> String {
    fn dump_region(region: &StructuredWasmRegion<'_>) -> String {
        let stmts = region.stmts.iter().map(dump_stmt).collect::<Vec<_>>().join("; ");
        let tail = region
            .tail
            .as_ref()
            .map_or_else(String::new, |tail| format!("; tail {}", dump_expr(tail)));
        format!("{{{stmts}{tail}}}")
    }

    fn dump_stmt(stmt: &StructuredWasmStmt<'_>) -> String {
        match stmt {
            StructuredWasmStmt::Local { name, initializer, .. } => {
                initializer.as_ref().map_or_else(
                    || format!("local {name:?}"),
                    |expr| format!("local {name:?} = {}", dump_expr(expr)),
                )
            }
            StructuredWasmStmt::Assign { name, value, .. } => {
                format!("assign {name:?} = {}", dump_expr(value))
            }
            StructuredWasmStmt::If { cond, then_region, else_region, .. } => format!(
                "if({}, {}, {})",
                dump_expr(cond),
                dump_region(then_region),
                dump_region(else_region)
            ),
            StructuredWasmStmt::Match { scrutinee, arms, .. } => format!(
                "match({}, [{}])",
                dump_expr(scrutinee),
                arms.iter().map(|arm| dump_region(&arm.body)).collect::<Vec<_>>().join(", ")
            ),
            StructuredWasmStmt::Pattern(init) => match &init.source {
                StructuredWasmBindingSource::Expr(expr) => {
                    format!("pattern = {}", dump_expr(expr))
                }
                StructuredWasmBindingSource::Param { index, .. } => {
                    format!("pattern = param[{index}]")
                }
            },
            StructuredWasmStmt::Return { value, .. } => value
                .as_ref()
                .map_or_else(|| "return".to_owned(), |expr| format!("return {}", dump_expr(expr))),
            StructuredWasmStmt::Expr { expr, .. } => dump_expr(expr),
            StructuredWasmStmt::SetLocal { local, value, .. } => {
                format!("set l{local} = {}", dump_expr(value))
            }
        }
    }

    fn dump_expr(expr: &StructuredWasmExpr<'_>) -> String {
        match &expr.kind {
            StructuredWasmExprKind::Leaf(backend) => format!("leaf({backend:?})"),
            StructuredWasmExprKind::WasmLocal(local) => format!("local.get({local})"),
            StructuredWasmExprKind::Eqz { value } => format!("eqz({})", dump_expr(value)),
            StructuredWasmExprKind::TeeLocal { local, value } => {
                format!("tee(l{local}, {})", dump_expr(value))
            }
            StructuredWasmExprKind::Select { cond, then_value, else_value } => {
                format!(
                    "select({}, {}, {})",
                    dump_expr(cond),
                    dump_expr(then_value),
                    dump_expr(else_value)
                )
            }
            StructuredWasmExprKind::Block { region, .. } => format!("block{}", dump_region(region)),
            StructuredWasmExprKind::If { cond, then_region, else_region, .. } => format!(
                "if({}, {}, {})",
                dump_expr(cond),
                dump_region(then_region),
                dump_region(else_region)
            ),
            StructuredWasmExprKind::Match { scrutinee, arms, .. } => format!(
                "match({}, [{}])",
                dump_expr(scrutinee),
                arms.iter().map(|arm| dump_region(&arm.body)).collect::<Vec<_>>().join(", ")
            ),
            StructuredWasmExprKind::Loop { body, .. } => {
                format!(
                    "loop({})",
                    body.as_ref().map_or_else(|| "_".to_owned(), |expr| dump_expr(expr))
                )
            }
            StructuredWasmExprKind::Break => "break".to_owned(),
            StructuredWasmExprKind::Continue => "continue".to_owned(),
            StructuredWasmExprKind::Unreachable => "unreachable".to_owned(),
        }
    }

    expr.as_ref().map_or_else(|| "_".to_owned(), dump_expr)
}

#[cfg(test)]
fn format_abi_ty(abi: &AbiTy) -> String {
    match abi {
        AbiTy::Scalar(BackendTy::Int) => "i32".to_owned(),
        AbiTy::Scalar(BackendTy::I64) => "i64".to_owned(),
        AbiTy::Scalar(BackendTy::Bool) => "bool".to_owned(),
        AbiTy::Scalar(BackendTy::Float) => "f64".to_owned(),
        AbiTy::Scalar(BackendTy::Char) => "char".to_owned(),
        AbiTy::Scalar(BackendTy::Ref(kind)) => format!("ref({kind:?})"),
        AbiTy::Scalar(BackendTy::Unit) => "unit".to_owned(),
        AbiTy::Aggregate(layout) => {
            format!("aggregate(size={}, align={})", layout.size, layout.align)
        }
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "structured_wasm_fixture.mitki".into(), fixture.to_owned());
        let diagnostics = mitki_analysis::check_file(db, file);
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        let runtime_diagnostics = mitki_analysis::check_runtime_file(db, file);
        assert!(
            runtime_diagnostics.is_empty(),
            "unexpected runtime diagnostics: {:?}",
            runtime_diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
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
            "unexpected Backend diagnostics: {:?}",
            backend.diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        backend
    }

    #[test]
    fn structured_wasm_dump_tracks_stackify_and_peephole_output() {
        let backend = compiler_for_fixture(
            r#"
enum Score {
    Good(int),
    Bad(int),
}

fun choose(flag: bool, value: Score): int {
    if flag {
        match value {
            .Good(v) => v,
            .Bad(v) => if v == 41 { 42 } else { v },
        }
    } else {
        0
    }
}

export fun main(): int {
    choose(true, Score.Bad(41))
}
"#,
        );
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        let pre = plan
            .function_kernel
            .as_ref()
            .expect("expected function kernel bundle")
            .dump(backend.db);
        let stackified = FunctionStackifier::build(
            &backend,
            &plan,
            plan.function_kernel.as_ref().expect("expected function kernel bundle"),
        )
        .unwrap_or_else(|diagnostic| panic!("stackify should succeed: {}", diagnostic.message()))
        .dump(backend.db);
        let post = plan
            .function_wasm_ir
            .as_ref()
            .expect("expected structured wasm bundle")
            .lowered_bundle()
            .dump(backend.db);

        expect!["function_kernel.direct:"]
            .assert_eq(&pre.lines().take(1).collect::<Vec<_>>().join("\n"));
        assert!(stackified.contains("structured_wasm.direct:"));
        assert!(stackified.contains("extra_locals:"));
        assert!(post.contains("decisions:"));
        assert!(post.contains("body: block"));
    }

    #[test]
    fn structured_wasm_dump_is_deterministic_across_repeated_planning() {
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
        let first = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .function_wasm_ir
            .as_ref()
            .expect("expected structured wasm bundle")
            .lowered_bundle()
            .dump(backend.db);
        let second = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .function_wasm_ir
            .as_ref()
            .expect("expected structured wasm bundle")
            .lowered_bundle()
            .dump(backend.db);
        assert_eq!(first, second);
    }

    #[test]
    fn peephole_rewrites_eqz_local_tee_select_and_mem_offsets() {
        let mut eqz = StructuredWasmExpr {
            source: ExprId::ZERO,
            abi: AbiTy::Scalar(BackendTy::Bool),
            ownership: ValueOwnership::None,
            kind: StructuredWasmExprKind::Leaf(FunctionKernelValue {
                source: ExprId::ZERO,
                abi: AbiTy::Scalar(BackendTy::Bool),
                ownership: ValueOwnership::None,
                kind: FunctionKernelValueKind::Binary {
                    op: BackendBinaryOp::Eq,
                    lhs: Box::new(FunctionKernelValue {
                        source: ExprId::ZERO,
                        abi: AbiTy::Scalar(BackendTy::Bool),
                        ownership: ValueOwnership::None,
                        kind: FunctionKernelValueKind::Local(NameId::ZERO),
                    }),
                    rhs: Box::new(FunctionKernelValue {
                        source: ExprId::ZERO,
                        abi: AbiTy::Scalar(BackendTy::Bool),
                        ownership: ValueOwnership::None,
                        kind: FunctionKernelValueKind::Int(0),
                    }),
                },
            }),
        };
        peephole_expr(&mut eqz);
        assert!(matches!(eqz.kind, StructuredWasmExprKind::Eqz { .. }));

        let mut tee_block = StructuredWasmExpr {
            source: ExprId::ZERO,
            abi: AbiTy::Scalar(BackendTy::Int),
            ownership: ValueOwnership::None,
            kind: StructuredWasmExprKind::Block {
                region: StructuredWasmRegion {
                    stmts: vec![StructuredWasmStmt::SetLocal {
                        source: ExprId::ZERO,
                        local: 5,
                        ty: ValType::I32,
                        value: StructuredWasmExpr {
                            source: ExprId::ZERO,
                            abi: AbiTy::Scalar(BackendTy::Int),
                            ownership: ValueOwnership::None,
                            kind: StructuredWasmExprKind::Leaf(FunctionKernelValue {
                                source: ExprId::ZERO,
                                abi: AbiTy::Scalar(BackendTy::Int),
                                ownership: ValueOwnership::None,
                                kind: FunctionKernelValueKind::Int(42),
                            }),
                        },
                    }],
                    tail: Some(Box::new(StructuredWasmExpr {
                        source: ExprId::ZERO,
                        abi: AbiTy::Scalar(BackendTy::Int),
                        ownership: ValueOwnership::None,
                        kind: StructuredWasmExprKind::WasmLocal(5),
                    })),
                },
                result_arity: StructuredWasmResultArity::Scalar(ValType::I32),
            },
        };
        peephole_expr(&mut tee_block);
        assert!(dump_emit_expr_summary(&Some(tee_block)).contains("tee("));

        let mut select_block = StructuredWasmExpr {
            source: ExprId::ZERO,
            abi: AbiTy::Scalar(BackendTy::Int),
            ownership: ValueOwnership::None,
            kind: StructuredWasmExprKind::Block {
                region: StructuredWasmRegion {
                    stmts: vec![StructuredWasmStmt::Expr {
                        expr: StructuredWasmExpr {
                            source: ExprId::ZERO,
                            abi: AbiTy::Scalar(BackendTy::Unit),
                            ownership: ValueOwnership::None,
                            kind: StructuredWasmExprKind::If {
                                cond: Box::new(StructuredWasmExpr {
                                    source: ExprId::ZERO,
                                    abi: AbiTy::Scalar(BackendTy::Bool),
                                    ownership: ValueOwnership::None,
                                    kind: StructuredWasmExprKind::Leaf(FunctionKernelValue {
                                        source: ExprId::ZERO,
                                        abi: AbiTy::Scalar(BackendTy::Bool),
                                        ownership: ValueOwnership::None,
                                        kind: FunctionKernelValueKind::Local(NameId::ZERO),
                                    }),
                                }),
                                then_region: StructuredWasmRegion {
                                    stmts: vec![StructuredWasmStmt::SetLocal {
                                        source: ExprId::ZERO,
                                        local: 6,
                                        ty: ValType::I32,
                                        value: StructuredWasmExpr {
                                            source: ExprId::ZERO,
                                            abi: AbiTy::Scalar(BackendTy::Int),
                                            ownership: ValueOwnership::None,
                                            kind: StructuredWasmExprKind::Leaf(
                                                FunctionKernelValue {
                                                    source: ExprId::ZERO,
                                                    abi: AbiTy::Scalar(BackendTy::Int),
                                                    ownership: ValueOwnership::None,
                                                    kind: FunctionKernelValueKind::Int(1),
                                                },
                                            ),
                                        },
                                    }],
                                    tail: None,
                                },
                                else_region: StructuredWasmRegion {
                                    stmts: vec![StructuredWasmStmt::SetLocal {
                                        source: ExprId::ZERO,
                                        local: 6,
                                        ty: ValType::I32,
                                        value: StructuredWasmExpr {
                                            source: ExprId::ZERO,
                                            abi: AbiTy::Scalar(BackendTy::Int),
                                            ownership: ValueOwnership::None,
                                            kind: StructuredWasmExprKind::Leaf(
                                                FunctionKernelValue {
                                                    source: ExprId::ZERO,
                                                    abi: AbiTy::Scalar(BackendTy::Int),
                                                    ownership: ValueOwnership::None,
                                                    kind: FunctionKernelValueKind::Int(0),
                                                },
                                            ),
                                        },
                                    }],
                                    tail: None,
                                },
                                result_arity: StructuredWasmResultArity::Unit,
                            },
                        },
                        ownership_ops: Vec::new(),
                    }],
                    tail: Some(Box::new(StructuredWasmExpr {
                        source: ExprId::ZERO,
                        abi: AbiTy::Scalar(BackendTy::Int),
                        ownership: ValueOwnership::None,
                        kind: StructuredWasmExprKind::WasmLocal(6),
                    })),
                },
                result_arity: StructuredWasmResultArity::Scalar(ValType::I32),
            },
        };
        peephole_expr(&mut select_block);
        assert!(dump_emit_expr_summary(&Some(select_block)).contains("select("));

        let mut mem = StructuredWasmExpr {
            source: ExprId::ZERO,
            abi: AbiTy::Scalar(BackendTy::Int),
            ownership: ValueOwnership::None,
            kind: StructuredWasmExprKind::Leaf(FunctionKernelValue {
                source: ExprId::ZERO,
                abi: AbiTy::Scalar(BackendTy::Int),
                ownership: ValueOwnership::None,
                kind: FunctionKernelValueKind::MemoryRead {
                    addr: Box::new(FunctionKernelValue {
                        source: ExprId::ZERO,
                        abi: AbiTy::Scalar(BackendTy::Int),
                        ownership: ValueOwnership::None,
                        kind: FunctionKernelValueKind::AddrOffset {
                            base: Box::new(FunctionKernelValue {
                                source: ExprId::ZERO,
                                abi: AbiTy::Scalar(BackendTy::Int),
                                ownership: ValueOwnership::None,
                                kind: FunctionKernelValueKind::Local(NameId::ZERO),
                            }),
                            offset: 4,
                        },
                    }),
                    access: Some(MemAccess::i32(8, 2)),
                },
            }),
        };
        peephole_expr(&mut mem);
        let StructuredWasmExprKind::Leaf(FunctionKernelValue {
            kind: FunctionKernelValueKind::MemoryRead { addr, access: Some(access) },
            ..
        }) = mem.kind
        else {
            panic!("expected folded memory read");
        };
        assert_eq!(access.offset, 12);
        assert!(matches!(addr.kind, FunctionKernelValueKind::Local(_)));
    }

    #[test]
    fn structured_wasm_validator_rejects_control_flow_leaf() {
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
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        let mut bundle = plan
            .function_wasm_ir
            .as_ref()
            .expect("expected structured wasm bundle")
            .lowered_bundle();
        let function = bundle
            .direct_functions
            .first_mut()
            .expect("expected one direct structured wasm function");
        function.body = Some(StructuredWasmExpr {
            source: ExprId::ZERO,
            abi: AbiTy::Scalar(BackendTy::Int),
            ownership: ValueOwnership::None,
            kind: StructuredWasmExprKind::Leaf(FunctionKernelValue {
                source: ExprId::ZERO,
                abi: AbiTy::Scalar(BackendTy::Int),
                ownership: ValueOwnership::None,
                kind: FunctionKernelValueKind::Block {
                    stmts: Vec::new(),
                    tail: Some(Box::new(FunctionKernelValue {
                        source: ExprId::ZERO,
                        abi: AbiTy::Scalar(BackendTy::Int),
                        ownership: ValueOwnership::None,
                        kind: FunctionKernelValueKind::Int(42),
                    })),
                },
            }),
        });

        let diagnostic = StructuredWasmValidator::validate(&backend, &plan, &bundle)
            .expect_err("structured Wasm validator should reject control-flow leaves");
        assert!(diagnostic.message().contains("structured Wasm leaf retained kernel control flow"));
    }
}

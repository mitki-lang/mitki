#![allow(unused_imports, unused_qualifications)]

use std::ops::Deref;

use mitki_errors::Diagnostic;
use rustc_hash::FxHashMap;

pub(in crate::backend) use super::function_codegen_ir::{
    FunctionStackifier, StructuredWasmBindingInit, StructuredWasmBindingSource, StructuredWasmExpr,
    StructuredWasmExprKind, StructuredWasmLocal, StructuredWasmLocalReason, StructuredWasmMatchArm,
    StructuredWasmOwnershipOp, StructuredWasmPeephole, StructuredWasmRegion,
    StructuredWasmResultArity, StructuredWasmStmt,
};
use super::function_kernel::{
    FunctionKernelBundle, FunctionKernelFunction, FunctionKernelKind, FunctionKernelValue,
    FunctionKernelValueKind,
};
use super::plan::ModulePlan;
use super::*;

pub(in crate::backend) struct StructuredWasmLowering;
pub(in crate::backend) struct StructuredWasmValidator;
struct StructuredWasmIndexResolver;

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmBundle<'db> {
    pub(in crate::backend) direct_functions: Vec<StructuredWasmFunction<'db>>,
    pub(in crate::backend) closures: Vec<StructuredWasmFunction<'db>>,
    pub(in crate::backend) direct_indices: FxHashMap<InstanceKey<'db>, usize>,
    pub(in crate::backend) closure_indices: FxHashMap<ClosureInstanceKey<'db>, usize>,
}

impl<'db> StructuredWasmBundle<'db> {
    pub(in crate::backend) fn direct(
        &self,
        instance: &InstanceKey<'db>,
    ) -> Option<&StructuredWasmFunction<'db>> {
        self.direct_indices.get(instance).and_then(|&index| self.direct_functions.get(index))
    }

    pub(in crate::backend) fn closure(
        &self,
        closure: &ClosureInstanceKey<'db>,
    ) -> Option<&StructuredWasmFunction<'db>> {
        self.closure_indices.get(closure).and_then(|&index| self.closures.get(index))
    }

    pub(in crate::backend) fn lowered_bundle(
        &self,
    ) -> super::function_codegen_ir::StructuredWasmBundle<'db> {
        super::function_codegen_ir::StructuredWasmBundle {
            direct_functions: self
                .direct_functions
                .iter()
                .map(|function| function.lowered.clone())
                .collect(),
            closures: self.closures.iter().map(|function| function.lowered.clone()).collect(),
            direct_indices: self.direct_indices.clone(),
            closure_indices: self.closure_indices.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub(in crate::backend) struct StructuredWasmFunction<'db> {
    lowered: super::function_codegen_ir::StructuredWasmFunction<'db>,
    pub(in crate::backend) resolved: StructuredWasmResolvedRefs<'db>,
}

impl<'db> StructuredWasmFunction<'db> {
    pub(in crate::backend) fn resolved(&self) -> &StructuredWasmResolvedRefs<'db> {
        &self.resolved
    }
}

impl<'db> Deref for StructuredWasmFunction<'db> {
    type Target = super::function_codegen_ir::StructuredWasmFunction<'db>;

    fn deref(&self) -> &Self::Target {
        &self.lowered
    }
}

#[derive(Clone, Debug, Default)]
pub(in crate::backend) struct StructuredWasmResolvedRefs<'db> {
    pub(in crate::backend) direct_calls: FxHashMap<ExprId, StructuredWasmResolvedDirectCall<'db>>,
    pub(in crate::backend) indirect_calls: FxHashMap<ExprId, StructuredWasmResolvedIndirectCall>,
    pub(in crate::backend) callable_values:
        FxHashMap<ExprId, StructuredWasmResolvedCallableValue<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct StructuredWasmResolvedDirectCall<'db> {
    pub(in crate::backend) function_index: u32,
    pub(in crate::backend) _marker: std::marker::PhantomData<&'db ()>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct StructuredWasmResolvedIndirectCall {
    pub(in crate::backend) signature: FunctionSignature,
    pub(in crate::backend) type_index: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct StructuredWasmResolvedCallableValue<'db> {
    pub(in crate::backend) target: FunctionValueTarget<'db>,
    pub(in crate::backend) table_slot: u32,
}

impl StructuredWasmLowering {
    pub(in crate::backend) fn build<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &FunctionKernelBundle<'db>,
    ) -> Result<StructuredWasmBundle<'db>, Diagnostic> {
        let stackified = StructuredWasmPeephole::run(
            backend,
            plan,
            FunctionStackifier::build(backend, plan, bundle)?,
        )?;
        let stackified = Self::inject_ownership_ops(stackified, bundle);
        StructuredWasmIndexResolver::resolve(backend, plan, &stackified)
    }
}

impl StructuredWasmValidator {
    pub(in crate::backend) fn validate<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &StructuredWasmBundle<'db>,
    ) -> Result<(), Diagnostic> {
        super::function_codegen_ir::StructuredWasmValidator::validate(
            backend,
            plan,
            &bundle.lowered_bundle(),
        )
    }
}

impl StructuredWasmLowering {
    fn inject_ownership_ops<'db>(
        mut bundle: super::function_codegen_ir::StructuredWasmBundle<'db>,
        kernel: &FunctionKernelBundle<'db>,
    ) -> super::function_codegen_ir::StructuredWasmBundle<'db> {
        for (function, kernel_function) in
            bundle.direct_functions.iter_mut().zip(&kernel.direct_functions)
        {
            Self::inject_function_ownership(function, kernel_function);
        }
        for (function, kernel_function) in bundle.closures.iter_mut().zip(&kernel.closures) {
            Self::inject_function_ownership(function, kernel_function);
        }
        bundle
    }

    fn inject_function_ownership<'db>(
        function: &mut super::function_codegen_ir::StructuredWasmFunction<'db>,
        kernel: &FunctionKernelFunction<'db>,
    ) {
        function.legalization = kernel.legalization.clone();
        let Some(body) = function.body.as_mut() else {
            return;
        };
        let StructuredWasmExprKind::Block { region, .. } = &mut body.kind else {
            return;
        };
        let block = kernel.entry_block();
        let original_stmts = std::mem::take(&mut region.stmts);
        let mut original_iter = original_stmts.into_iter();
        let mut rewritten = Vec::new();
        let mut index = 0usize;
        let mut return_ops = Vec::new();
        while index < block.stmts.len() {
            if Self::kernel_stmt_is_ownership(&block.stmts[index]) {
                index += 1;
                continue;
            }
            let Some(stmt) = original_iter.next() else {
                break;
            };
            let mut ops = Vec::new();
            index += 1;
            while index < block.stmts.len() && Self::kernel_stmt_is_ownership(&block.stmts[index]) {
                ops.push(Self::ownership_op_from_kernel_stmt(&block.stmts[index]));
                index += 1;
            }
            if index == block.stmts.len()
                && matches!(
                    block.terminator,
                    super::function_kernel::FunctionKernelTerminator::Return { .. }
                )
            {
                return_ops = ops;
                rewritten.push(stmt);
                break;
            }
            rewritten.push(Self::attach_ownership_ops(stmt, ops));
        }

        if let super::function_kernel::FunctionKernelTerminator::Return { .. } = &block.terminator {
            function.body_ownership_ops = return_ops;
        }
        rewritten.extend(original_iter);
        region.stmts = rewritten;
    }

    fn kernel_stmt_is_ownership<'db>(
        stmt: &super::function_kernel::FunctionKernelStmt<'db>,
    ) -> bool {
        matches!(
            stmt,
            super::function_kernel::FunctionKernelStmt::Retain { .. }
                | super::function_kernel::FunctionKernelStmt::Release { .. }
                | super::function_kernel::FunctionKernelStmt::Destroy { .. }
                | super::function_kernel::FunctionKernelStmt::Copy { .. }
                | super::function_kernel::FunctionKernelStmt::Move { .. }
        )
    }

    fn ownership_op_from_kernel_stmt<'db>(
        stmt: &super::function_kernel::FunctionKernelStmt<'db>,
    ) -> StructuredWasmOwnershipOp {
        match stmt {
            super::function_kernel::FunctionKernelStmt::Retain { .. } => {
                StructuredWasmOwnershipOp::Retain
            }
            super::function_kernel::FunctionKernelStmt::Release { .. } => {
                StructuredWasmOwnershipOp::Release
            }
            super::function_kernel::FunctionKernelStmt::Destroy { .. } => {
                StructuredWasmOwnershipOp::Destroy
            }
            super::function_kernel::FunctionKernelStmt::Copy { .. } => {
                StructuredWasmOwnershipOp::Copy
            }
            super::function_kernel::FunctionKernelStmt::Move { .. } => {
                StructuredWasmOwnershipOp::Move
            }
            _ => unreachable!(),
        }
    }

    fn attach_ownership_ops<'db>(
        stmt: StructuredWasmStmt<'db>,
        ownership_ops: Vec<StructuredWasmOwnershipOp>,
    ) -> StructuredWasmStmt<'db> {
        match stmt {
            StructuredWasmStmt::Local { name, abi, initializer, .. } => {
                StructuredWasmStmt::Local { name, abi, initializer, ownership_ops }
            }
            StructuredWasmStmt::If {
                source,
                abi,
                ownership,
                cond,
                then_region,
                else_region,
                ..
            } => StructuredWasmStmt::If {
                source,
                abi,
                ownership,
                cond,
                then_region,
                else_region,
                ownership_ops,
            },
            StructuredWasmStmt::Match {
                source,
                abi,
                ownership,
                scrutinee,
                arms,
                fallback_unreachable,
                ..
            } => StructuredWasmStmt::Match {
                source,
                abi,
                ownership,
                scrutinee,
                arms,
                fallback_unreachable,
                ownership_ops,
            },
            StructuredWasmStmt::Return { source, value, .. } => {
                StructuredWasmStmt::Return { source, value, ownership_ops }
            }
            StructuredWasmStmt::Expr { expr, .. } => {
                StructuredWasmStmt::Expr { expr, ownership_ops }
            }
            other => other,
        }
    }
}

impl StructuredWasmIndexResolver {
    fn resolve<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        bundle: &super::function_codegen_ir::StructuredWasmBundle<'db>,
    ) -> Result<StructuredWasmBundle<'db>, Diagnostic> {
        Ok(StructuredWasmBundle {
            direct_functions: bundle
                .direct_functions
                .iter()
                .map(|function| Self::resolve_function(backend, plan, function))
                .collect::<Result<_, _>>()?,
            closures: bundle
                .closures
                .iter()
                .map(|function| Self::resolve_function(backend, plan, function))
                .collect::<Result<_, _>>()?,
            direct_indices: bundle.direct_indices.clone(),
            closure_indices: bundle.closure_indices.clone(),
        })
    }

    fn resolve_function<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
    ) -> Result<StructuredWasmFunction<'db>, Diagnostic> {
        let mut resolved = StructuredWasmResolvedRefs::default();
        for init in &function.param_inits {
            Self::resolve_binding_init(backend, plan, function, &mut resolved, init)?;
        }
        if let Some(body) = &function.body {
            Self::resolve_emit_expr(backend, plan, function, &mut resolved, body)?;
        }
        Ok(StructuredWasmFunction { lowered: function.clone(), resolved })
    }

    fn resolve_binding_init<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        resolved: &mut StructuredWasmResolvedRefs<'db>,
        init: &StructuredWasmBindingInit<'db>,
    ) -> Result<(), Diagnostic> {
        if let StructuredWasmBindingSource::Expr(expr) = &init.source {
            Self::resolve_emit_expr(backend, plan, function, resolved, expr)?;
        }
        Ok(())
    }

    fn resolve_region<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        resolved: &mut StructuredWasmResolvedRefs<'db>,
        region: &StructuredWasmRegion<'db>,
    ) -> Result<(), Diagnostic> {
        for stmt in &region.stmts {
            Self::resolve_stmt(backend, plan, function, resolved, stmt)?;
        }
        if let Some(tail) = &region.tail {
            Self::resolve_emit_expr(backend, plan, function, resolved, tail)?;
        }
        Ok(())
    }

    fn resolve_stmt<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        resolved: &mut StructuredWasmResolvedRefs<'db>,
        stmt: &StructuredWasmStmt<'db>,
    ) -> Result<(), Diagnostic> {
        match stmt {
            StructuredWasmStmt::Local { initializer, .. } => {
                if let Some(initializer) = initializer {
                    Self::resolve_emit_expr(backend, plan, function, resolved, initializer)?;
                }
            }
            StructuredWasmStmt::Assign { value, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, value)?;
            }
            StructuredWasmStmt::If { cond, then_region, else_region, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, cond)?;
                Self::resolve_region(backend, plan, function, resolved, then_region)?;
                Self::resolve_region(backend, plan, function, resolved, else_region)?;
            }
            StructuredWasmStmt::Match { scrutinee, arms, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, scrutinee)?;
                for arm in arms {
                    Self::resolve_region(backend, plan, function, resolved, &arm.body)?;
                }
            }
            StructuredWasmStmt::Pattern(init) => {
                Self::resolve_binding_init(backend, plan, function, resolved, init)?;
            }
            StructuredWasmStmt::Return { value, .. } => {
                if let Some(value) = value {
                    Self::resolve_emit_expr(backend, plan, function, resolved, value)?;
                }
            }
            StructuredWasmStmt::Expr { expr, .. }
            | StructuredWasmStmt::SetLocal { value: expr, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, expr)?;
            }
        }
        Ok(())
    }

    fn resolve_emit_expr<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        resolved: &mut StructuredWasmResolvedRefs<'db>,
        expr: &StructuredWasmExpr<'db>,
    ) -> Result<(), Diagnostic> {
        match &expr.kind {
            StructuredWasmExprKind::Leaf(expr) => {
                Self::resolve_kernel_value(backend, plan, function, resolved, expr)?;
            }
            StructuredWasmExprKind::Eqz { value }
            | StructuredWasmExprKind::TeeLocal { value, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, value)?;
            }
            StructuredWasmExprKind::Select { cond, then_value, else_value } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, cond)?;
                Self::resolve_emit_expr(backend, plan, function, resolved, then_value)?;
                Self::resolve_emit_expr(backend, plan, function, resolved, else_value)?;
            }
            StructuredWasmExprKind::Block { region, .. } => {
                Self::resolve_region(backend, plan, function, resolved, region)?;
            }
            StructuredWasmExprKind::If { cond, then_region, else_region, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, cond)?;
                Self::resolve_region(backend, plan, function, resolved, then_region)?;
                Self::resolve_region(backend, plan, function, resolved, else_region)?;
            }
            StructuredWasmExprKind::Match { scrutinee, arms, .. } => {
                Self::resolve_emit_expr(backend, plan, function, resolved, scrutinee)?;
                for arm in arms {
                    Self::resolve_region(backend, plan, function, resolved, &arm.body)?;
                }
            }
            StructuredWasmExprKind::Loop { body, .. } => {
                if let Some(body) = body {
                    Self::resolve_emit_expr(backend, plan, function, resolved, body)?;
                }
            }
            StructuredWasmExprKind::WasmLocal(_)
            | StructuredWasmExprKind::Break
            | StructuredWasmExprKind::Continue
            | StructuredWasmExprKind::Unreachable => {}
        }
        Ok(())
    }

    fn resolve_kernel_value<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        resolved: &mut StructuredWasmResolvedRefs<'db>,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        match &expr.kind {
            FunctionKernelValueKind::Call { target, args } => {
                Self::insert_direct_call(
                    backend,
                    function,
                    &mut resolved.direct_calls,
                    expr.source,
                    StructuredWasmResolvedDirectCall {
                        function_index: Self::resolve_direct_call_index(
                            backend,
                            plan,
                            function,
                            target,
                            expr.source,
                        )?,
                        _marker: std::marker::PhantomData,
                    },
                )?;
                for arg in args {
                    Self::resolve_kernel_value(backend, plan, function, resolved, arg)?;
                }
            }
            FunctionKernelValueKind::IndirectCall { callee, signature, args } => {
                Self::insert_indirect_call(
                    backend,
                    function,
                    &mut resolved.indirect_calls,
                    expr.source,
                    StructuredWasmResolvedIndirectCall {
                        signature: signature.clone(),
                        type_index: Self::resolve_indirect_type_index(
                            backend,
                            plan,
                            function,
                            signature,
                            expr.source,
                        )?,
                    },
                )?;
                Self::resolve_kernel_value(backend, plan, function, resolved, callee)?;
                for arg in args {
                    Self::resolve_kernel_value(backend, plan, function, resolved, arg)?;
                }
            }
            FunctionKernelValueKind::FunctionValue { target } => {
                Self::insert_callable_value(
                    backend,
                    function,
                    &mut resolved.callable_values,
                    expr.source,
                    StructuredWasmResolvedCallableValue {
                        target: target.clone(),
                        table_slot: Self::resolve_table_slot(
                            backend,
                            plan,
                            function,
                            target,
                            expr.source,
                        )?,
                    },
                )?;
            }
            FunctionKernelValueKind::ClosureValue { target, env } => {
                let value_target = FunctionValueTarget::Closure(target.clone());
                Self::insert_callable_value(
                    backend,
                    function,
                    &mut resolved.callable_values,
                    expr.source,
                    StructuredWasmResolvedCallableValue {
                        target: value_target.clone(),
                        table_slot: Self::resolve_table_slot(
                            backend,
                            plan,
                            function,
                            &value_target,
                            expr.source,
                        )?,
                    },
                )?;
                for field in &env.fields {
                    Self::resolve_kernel_value(backend, plan, function, resolved, &field.value)?;
                }
            }
            FunctionKernelValueKind::Clone { value }
            | FunctionKernelValueKind::AddrOffset { base: value, .. }
            | FunctionKernelValueKind::MemoryRead { addr: value, .. }
            | FunctionKernelValueKind::Prefix { expr: value, .. }
            | FunctionKernelValueKind::Field { base: value, .. } => {
                Self::resolve_kernel_value(backend, plan, function, resolved, value)?;
            }
            FunctionKernelValueKind::MemoryWrite { addr, value }
            | FunctionKernelValueKind::Binary { lhs: addr, rhs: value, .. } => {
                Self::resolve_kernel_value(backend, plan, function, resolved, addr)?;
                Self::resolve_kernel_value(backend, plan, function, resolved, value)?;
            }
            FunctionKernelValueKind::PointerAdd { ptr, count, .. } => {
                Self::resolve_kernel_value(backend, plan, function, resolved, ptr)?;
                Self::resolve_kernel_value(backend, plan, function, resolved, count)?;
            }
            FunctionKernelValueKind::StringFromBytes { ptr, len } => {
                Self::resolve_kernel_value(backend, plan, function, resolved, ptr)?;
                Self::resolve_kernel_value(backend, plan, function, resolved, len)?;
            }
            FunctionKernelValueKind::Array { items, .. }
            | FunctionKernelValueKind::VariantCall { args: items, .. } => {
                for item in items {
                    Self::resolve_kernel_value(backend, plan, function, resolved, item)?;
                }
            }
            FunctionKernelValueKind::ArrayRepeat { value, len, .. } => {
                Self::resolve_kernel_value(backend, plan, function, resolved, value)?;
                Self::resolve_kernel_value(backend, plan, function, resolved, len)?;
            }
            FunctionKernelValueKind::Tuple { fields }
            | FunctionKernelValueKind::Struct { fields } => {
                for field in fields {
                    Self::resolve_kernel_value(backend, plan, function, resolved, &field.value)?;
                }
            }
            FunctionKernelValueKind::Union { value, .. } => {
                if let Some(value) = value {
                    Self::resolve_kernel_value(backend, plan, function, resolved, value)?;
                }
            }
            FunctionKernelValueKind::Local(_)
            | FunctionKernelValueKind::Capture(_)
            | FunctionKernelValueKind::Bool(_)
            | FunctionKernelValueKind::Int(_)
            | FunctionKernelValueKind::Float(_)
            | FunctionKernelValueKind::String(_)
            | FunctionKernelValueKind::Char(_)
            | FunctionKernelValueKind::Unit
            | FunctionKernelValueKind::StackAddr { .. }
            | FunctionKernelValueKind::VariantValue { .. } => {}
            FunctionKernelValueKind::Block { .. }
            | FunctionKernelValueKind::If { .. }
            | FunctionKernelValueKind::Match { .. }
            | FunctionKernelValueKind::Loop { .. }
            | FunctionKernelValueKind::Break
            | FunctionKernelValueKind::Continue => {
                return Err(Diagnostic::error(
                    "internal error: structured Wasm leaf retained kernel control flow during \
                     index resolution",
                    match &function.kind {
                        FunctionKernelKind::Direct(instance) => {
                            backend.function_range(instance.location)
                        }
                        FunctionKernelKind::Closure(closure) => {
                            backend.function_range(closure.owner)
                        }
                    },
                ));
            }
        }
        Ok(())
    }

    fn resolve_direct_call_index<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        target: &BackendCallTarget<'db>,
        source: ExprId,
    ) -> Result<u32, Diagnostic> {
        match &target.callable {
            BackendCallable::Runtime(runtime) => {
                plan.sections.runtime_function_indices.get(runtime).copied()
            }
            BackendCallable::StageIntrinsic(intrinsic) => {
                plan.sections.stage_function_indices.get(intrinsic).copied()
            }
            BackendCallable::Function(instance) => {
                plan.sections.direct_function_indices.get(instance).copied()
            }
        }
        .ok_or_else(|| {
            Self::error_for(
                backend,
                function,
                format!(
                    "internal error: missing late-resolved direct call index for expr {:?}",
                    source
                ),
            )
        })
    }

    fn resolve_indirect_type_index<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        signature: &FunctionSignature,
        source: ExprId,
    ) -> Result<u32, Diagnostic> {
        plan.sections.callable_type_indices.get(signature).copied().ok_or_else(|| {
            Self::error_for(
                backend,
                function,
                format!(
                    "internal error: missing late-resolved indirect-call type index for expr {:?}",
                    source
                ),
            )
        })
    }

    fn resolve_table_slot<'db>(
        backend: &Backend<'db>,
        plan: &ModulePlan<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        target: &FunctionValueTarget<'db>,
        source: ExprId,
    ) -> Result<u32, Diagnostic> {
        plan.sections.table_slots.get(target).copied().ok_or_else(|| {
            Self::error_for(
                backend,
                function,
                format!(
                    "internal error: missing late-resolved callable table slot for expr {:?}",
                    source
                ),
            )
        })
    }

    fn insert_direct_call<'db>(
        backend: &Backend<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        map: &mut FxHashMap<ExprId, StructuredWasmResolvedDirectCall<'db>>,
        source: ExprId,
        value: StructuredWasmResolvedDirectCall<'db>,
    ) -> Result<(), Diagnostic> {
        if let Some(existing) = map.get(&source)
            && existing != &value
        {
            return Err(Self::error_for(
                backend,
                function,
                "internal error: direct call resolution produced inconsistent indices",
            ));
        }
        map.insert(source, value);
        Ok(())
    }

    fn insert_indirect_call<'db>(
        backend: &Backend<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        map: &mut FxHashMap<ExprId, StructuredWasmResolvedIndirectCall>,
        source: ExprId,
        value: StructuredWasmResolvedIndirectCall,
    ) -> Result<(), Diagnostic> {
        if let Some(existing) = map.get(&source)
            && existing != &value
        {
            return Err(Self::error_for(
                backend,
                function,
                "internal error: indirect call resolution produced inconsistent indices",
            ));
        }
        map.insert(source, value);
        Ok(())
    }

    fn insert_callable_value<'db>(
        backend: &Backend<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        map: &mut FxHashMap<ExprId, StructuredWasmResolvedCallableValue<'db>>,
        source: ExprId,
        value: StructuredWasmResolvedCallableValue<'db>,
    ) -> Result<(), Diagnostic> {
        if let Some(existing) = map.get(&source)
            && existing != &value
        {
            return Err(Self::error_for(
                backend,
                function,
                "internal error: callable-value resolution produced inconsistent table slots",
            ));
        }
        map.insert(source, value);
        Ok(())
    }

    fn error_for<'db>(
        backend: &Backend<'db>,
        function: &super::function_codegen_ir::StructuredWasmFunction<'db>,
        message: impl Into<String>,
    ) -> Diagnostic {
        let range = match &function.kind {
            FunctionKernelKind::Direct(instance) => backend.function_range(instance.location),
            FunctionKernelKind::Closure(closure) => backend.function_range(closure.owner),
        };
        Diagnostic::error(message.into(), range)
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
        let file = File::new(db, "function_wasm_ir_fixture.mitki".into(), fixture.to_owned());
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
    fn structured_wasm_ir_carries_explicit_ownership_ops() {
        let backend = compiler_for_fixture(
            r#"
export fun main(): [str] {
    ["x", "y"];
    val xs = ["a", "b"];
    xs
}
"#,
        );
        let plan = backend.build_module_plan().expect("module plan");
        let function = plan
            .function_wasm_ir
            .as_ref()
            .expect("structured wasm bundle")
            .direct_functions
            .iter()
            .find(|function| function.debug_name.contains("main"))
            .expect("main function");
        let body = function.body.as_ref().expect("body");
        let StructuredWasmExprKind::Block { region, .. } = &body.kind else {
            panic!("expected structured block body");
        };
        assert!(
            region.stmts.iter().any(|stmt| match stmt {
                StructuredWasmStmt::Local { ownership_ops, .. }
                | StructuredWasmStmt::If { ownership_ops, .. }
                | StructuredWasmStmt::Match { ownership_ops, .. }
                | StructuredWasmStmt::Return { ownership_ops, .. }
                | StructuredWasmStmt::Expr { ownership_ops, .. } => {
                    ownership_ops.contains(&StructuredWasmOwnershipOp::Release)
                        || ownership_ops.contains(&StructuredWasmOwnershipOp::Move)
                }
                StructuredWasmStmt::Assign { .. }
                | StructuredWasmStmt::Pattern(_)
                | StructuredWasmStmt::SetLocal { .. } => false,
            }),
            "expected structured wasm stmts to carry explicit ownership ops"
        );
        assert!(
            function.body_ownership_ops.contains(&StructuredWasmOwnershipOp::Move),
            "expected explicit body ownership ops for the return value"
        );
    }

    #[test]
    fn structured_wasm_ir_resolves_direct_indirect_and_callable_indices() {
        let backend = compiler_for_fixture(
            r#"
fun apply(f: fun(int) -> int, x: int): int {
    f(x)
}

export fun main(): int {
    val offset = 1
    val add: fun(int) -> int = { value in value + offset }
    apply(add, 41)
}
"#,
        );
        let plan = backend.build_module_plan().expect("module plan");

        let apply = plan
            .function_wasm_ir
            .as_ref()
            .expect("structured wasm bundle")
            .direct_functions
            .iter()
            .find(|function| function.debug_name.contains("apply"))
            .expect("apply function");
        assert!(
            !apply.resolved().indirect_calls.is_empty(),
            "expected indirect call type indices to resolve in structured Wasm IR"
        );

        let main = plan
            .function_wasm_ir
            .as_ref()
            .expect("structured wasm bundle")
            .direct_functions
            .iter()
            .find(|function| function.debug_name.contains("main"))
            .expect("main function");
        assert!(
            !main.resolved().direct_calls.is_empty(),
            "expected direct call indices to resolve in structured Wasm IR"
        );
        assert!(
            !main.resolved().callable_values.is_empty(),
            "expected callable table slots to resolve in structured Wasm IR"
        );

        let closure = plan
            .function_wasm_ir
            .as_ref()
            .expect("structured wasm bundle")
            .closures
            .first()
            .expect("closure");
        assert!(
            closure.resolved().indirect_calls.is_empty(),
            "closure body should not invent unrelated indirect call sites"
        );
    }
}

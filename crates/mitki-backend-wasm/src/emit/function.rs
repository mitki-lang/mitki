use salsa::plumbing::FromId as _;

use super::function_kernel::{
    FunctionKernelClosureEnvInit, FunctionKernelFieldValue, FunctionKernelValue,
    FunctionKernelValueKind,
};
use super::function_legalize::{
    FunctionLegalization, LegalizedArgPassing, LegalizedCallKind, LegalizedCallSite,
    LegalizedCallableRepresentation, LegalizedResultPassing,
};
use super::function_wasm_ir::{
    StructuredWasmBindingInit, StructuredWasmBindingSource, StructuredWasmExpr,
    StructuredWasmExprKind, StructuredWasmFunction, StructuredWasmMatchArm,
    StructuredWasmOwnershipOp, StructuredWasmRegion, StructuredWasmResolvedRefs,
    StructuredWasmResultArity, StructuredWasmStmt,
};
use super::module::{emit_scalar_load, emit_scalar_store};
use super::*;
use crate::abi::backend_ty_block_type;
use crate::layout::VariantLayout;

pub(super) struct BackendEmitter<'a, 'db> {
    pub(super) backend: &'a Backend<'db>,
    pub(super) location: FunctionLocation<'db>,
    pub(super) source_map: &'db mitki_lower::hir::FunctionSourceMap,
    pub(super) function_indices: &'a FxHashMap<InstanceKey<'db>, u32>,
    pub(super) runtime_indices: &'a FxHashMap<RuntimeFunction, u32>,
    pub(super) stage_indices: &'a FxHashMap<StageIntrinsic, u32>,
    pub(super) helper_indices: &'a FxHashMap<HelperFunction, u32>,
    pub(super) nominal_destroyers: &'a FxHashMap<u32, u32>,
    pub(super) nominal_eq_helpers: &'a FxHashMap<u32, u32>,
    pub(super) array_destroyers: &'a FxHashMap<u32, u32>,
    pub(super) array_eq_helpers: &'a FxHashMap<u32, u32>,
    pub(super) callable_type_indices: &'a FxHashMap<FunctionSignature, u32>,
    pub(super) table_slots: &'a FxHashMap<FunctionValueTarget<'db>, u32>,
    pub(super) closure_destroyers: &'a [(u32, u32)],
    pub(super) resolved_wasm_refs: Option<&'a StructuredWasmResolvedRefs<'db>>,
    pub(super) function_legalization: Option<&'a FunctionLegalization<'db>>,
    pub(super) layout: &'a FunctionLayout,
    pub(super) function_result: &'a AbiTy,
    pub(super) control_depth: u32,
    pub(super) loop_stack: Vec<LoopTargets>,
    pub(super) scope_stack: Vec<ScopeFrame>,
    pub(super) return_target: Option<u32>,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct LoopTargets {
    break_target: u32,
    continue_target: u32,
    scope_depth: usize,
}

#[derive(Clone, Debug, Default)]
pub(super) struct ScopeFrame {
    locals: Vec<ScopeLocal>,
}

#[derive(Clone, Debug)]
pub(super) struct ScopeLocal {
    name: NameId,
    abi: AbiTy,
}

#[derive(Clone, Copy, Debug)]
enum PatternStorage {
    Unit,
    ScalarLocal(u32),
    PointerLocal(u32),
    FrameSlot(FrameSlotId),
}

#[derive(Clone, Debug)]
struct PatternValue {
    storage: PatternStorage,
    abi: AbiTy,
    offset: u32,
}

#[derive(Clone, Copy)]
struct StructuredStmtContext<'a> {
    abi: &'a AbiTy,
    ownership: ValueOwnership,
    ownership_ops: &'a [StructuredWasmOwnershipOp],
}

impl<'a, 'db> BackendEmitter<'a, 'db> {
    pub(super) fn structured_wasm_body(
        &mut self,
        function: &mut WasmFunction,
        lowered: &StructuredWasmFunction<'db>,
    ) -> Result<(), Diagnostic> {
        let block_type = match self.legalized_result_passing() {
            LegalizedResultPassing::Unit | LegalizedResultPassing::IndirectOutPtr => {
                BlockType::Empty
            }
            LegalizedResultPassing::DirectScalar(ty) => backend_ty_block_type(ty),
        };
        function.instruction(&Instruction::Block(block_type));
        let return_target = self.control_depth;
        self.return_target = Some(return_target);
        self.control_depth += 1;
        self.push_scope();
        self.register_param_locals()?;
        for init in &lowered.param_inits {
            self.emit_structured_wasm_binding_init(function, init)?;
        }

        let Some(expr) = lowered.body.as_ref() else {
            self.release_scopes_to(function, 0, ExprId::ZERO)?;
            self.control_depth -= 1;
            self.return_target = None;
            function.instruction(&Instruction::End);
            return Ok(());
        };

        match self.legalized_result_passing() {
            LegalizedResultPassing::Unit => {
                self.structured_wasm_expr(function, expr)?;
                if let AbiTy::Scalar(ty) = expr.abi
                    && !lowered.body_ownership_ops.is_empty()
                {
                    let consumed = self.emit_stack_ownership_ops(
                        function,
                        ty,
                        expr.source,
                        &lowered.body_ownership_ops,
                    )?;
                    if !consumed && !matches!(ty, BackendTy::Unit) {
                        function.instruction(&Instruction::Drop);
                    }
                }
                self.release_scopes_to(function, 0, expr.source)?;
            }
            LegalizedResultPassing::DirectScalar(BackendTy::Float) => {
                self.structured_wasm_expr(function, expr)?;
                let scratch = self.scratch_f64_local(expr.source)?;
                function.instruction(&Instruction::LocalSet(scratch));
                self.release_scopes_to(function, 0, expr.source)?;
                function.instruction(&Instruction::LocalGet(scratch));
            }
            LegalizedResultPassing::DirectScalar(BackendTy::I64) => {
                self.structured_wasm_expr(function, expr)?;
                let scratch = self.scratch_i64_local(expr.source)?;
                function.instruction(&Instruction::LocalSet(scratch));
                self.release_scopes_to(function, 0, expr.source)?;
                function.instruction(&Instruction::LocalGet(scratch));
            }
            LegalizedResultPassing::DirectScalar(ty) => {
                self.structured_wasm_expr(function, expr)?;
                if !lowered.body_ownership_ops.is_empty() {
                    if self.emit_stack_ownership_ops(
                        function,
                        ty,
                        expr.source,
                        &lowered.body_ownership_ops,
                    )? {
                        return Err(Diagnostic::error(
                            "internal error: explicit ownership ops consumed a non-unit function \
                             body result",
                            self.node_range(expr.source),
                        ));
                    }
                } else if expr.ownership.is_borrowed() && ty.is_heap_ref() {
                    self.retain_heap_ref_on_stack(function, expr.source)?;
                }
                let scratch = self.result_i32_local(expr.source)?;
                function.instruction(&Instruction::LocalSet(scratch));
                self.release_scopes_to(function, 0, expr.source)?;
                function.instruction(&Instruction::LocalGet(scratch));
            }
            LegalizedResultPassing::IndirectOutPtr => {
                let result_ptr = self.legalized_result_ptr_local().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: aggregate result is missing its out-pointer local",
                        self.node_range(expr.source),
                    )
                })?;
                self.emit_structured_wasm_expr_into_owner(
                    function,
                    Dest::pointer_local(result_ptr),
                    expr,
                )?;
                if !lowered.body_ownership_ops.is_empty() {
                    self.emit_dest_ownership_ops(
                        function,
                        Dest::pointer_local(result_ptr),
                        &expr.abi,
                        expr.source,
                        &lowered.body_ownership_ops,
                    )?;
                }
                self.release_scopes_to(function, 0, expr.source)?;
            }
        }

        self.control_depth -= 1;
        self.return_target = None;
        function.instruction(&Instruction::End);
        Ok(())
    }

    pub(super) fn emit_prologue(&self, function: &mut WasmFunction) {
        let Some(frame_base_local) = self.layout.frame_base_local() else {
            return;
        };

        function.instruction(&Instruction::GlobalGet(0));
        function.instruction(&Instruction::LocalSet(frame_base_local));
        function.instruction(&Instruction::LocalGet(frame_base_local));
        function.instruction(&Instruction::I32Const(self.layout.frame_size() as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::GlobalSet(0));
    }

    pub(super) fn emit_epilogue(&self, function: &mut WasmFunction) {
        let Some(frame_base_local) = self.layout.frame_base_local() else {
            return;
        };

        function.instruction(&Instruction::LocalGet(frame_base_local));
        function.instruction(&Instruction::GlobalSet(0));
    }

    pub(super) fn emit_closure_env_destructor(
        &mut self,
        function: &mut WasmFunction,
        env_layout: &AggregateLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let env_ptr_local = self.layout.env_ptr_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: closure env destructor is missing its env parameter",
                self.node_range(source),
            )
        })?;
        self.release_aggregate_at_local(
            function,
            env_ptr_local,
            &AbiTy::Aggregate(Box::new(env_layout.clone())),
            source,
        )?;
        self.dealloc_env_from_local(function, env_ptr_local, env_layout.size, source)
    }

    pub(super) fn emit_nominal_destructor(
        &mut self,
        function: &mut WasmFunction,
        payload_layout: &AggregateLayout,
        payload_local: u32,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        self.release_aggregate_at_local(
            function,
            payload_local,
            &AbiTy::Aggregate(Box::new(payload_layout.clone())),
            source,
        )?;
        self.dealloc_env_from_local(function, payload_local, payload_layout.size, source)
    }

    pub(super) fn emit_nominal_equality(
        &mut self,
        function: &mut WasmFunction,
        payload_layout: &AggregateLayout,
        lhs_local: u32,
        rhs_local: u32,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        function.instruction(&Instruction::LocalGet(lhs_local));
        function.instruction(&Instruction::LocalGet(rhs_local));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::Else);
        self.emit_abi_equality(
            function,
            lhs_local,
            rhs_local,
            0,
            &AbiTy::Aggregate(Box::new(payload_layout.clone())),
            source,
        )?;
        function.instruction(&Instruction::End);
        Ok(())
    }

    pub(super) fn emit_array_destructor(
        &mut self,
        function: &mut WasmFunction,
        layout: &ArrayRuntimeLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        const ARRAY_PTR_LOCAL: u32 = 0;

        if layout.item_abi.contains_heap_refs() {
            let index_local = self.object_local(source)?;
            let element_local = self.scratch_i32_local(source)?;
            let element_value_local = self.scratch_i32_aux_local(source)?;

            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::LocalSet(index_local));

            function.instruction(&Instruction::Block(BlockType::Empty));
            function.instruction(&Instruction::Loop(BlockType::Empty));

            function.instruction(&Instruction::LocalGet(index_local));
            function.instruction(&Instruction::LocalGet(ARRAY_PTR_LOCAL));
            MemAccess::array_len().emit_load(function);
            function.instruction(&Instruction::I32GeU);
            function.instruction(&Instruction::BrIf(1));

            Self::emit_array_element_addr(function, ARRAY_PTR_LOCAL, layout, index_local);
            function.instruction(&Instruction::LocalSet(element_local));

            match &layout.item_abi {
                AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                    function.instruction(&Instruction::LocalGet(element_local));
                    emit_scalar_load(function, *ty, 0);
                    function.instruction(&Instruction::LocalSet(element_value_local));
                    self.release_heap_ref_from_local(function, element_value_local, *ty, source)?;
                }
                AbiTy::Aggregate(_) => {
                    self.release_aggregate_at_local(
                        function,
                        element_local,
                        &layout.item_abi,
                        source,
                    )?;
                }
                _ => {}
            }

            function.instruction(&Instruction::LocalGet(index_local));
            function.instruction(&Instruction::I32Const(1));
            function.instruction(&Instruction::I32Add);
            function.instruction(&Instruction::LocalSet(index_local));
            function.instruction(&Instruction::Br(0));

            function.instruction(&Instruction::End);
            function.instruction(&Instruction::End);
        }

        self.dealloc_array_from_local(function, ARRAY_PTR_LOCAL, layout, source)
    }

    pub(super) fn emit_array_equality(
        &mut self,
        function: &mut WasmFunction,
        layout: &ArrayRuntimeLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        const LHS_PTR_LOCAL: u32 = 0;
        const RHS_PTR_LOCAL: u32 = 1;

        function.instruction(&Instruction::LocalGet(LHS_PTR_LOCAL));
        function.instruction(&Instruction::LocalGet(RHS_PTR_LOCAL));
        function.instruction(&Instruction::I32Eq);
        function.instruction(&Instruction::If(BlockType::Empty));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::Return);
        function.instruction(&Instruction::End);

        function.instruction(&Instruction::LocalGet(LHS_PTR_LOCAL));
        MemAccess::array_len().emit_load(function);
        function.instruction(&Instruction::LocalGet(RHS_PTR_LOCAL));
        MemAccess::array_len().emit_load(function);
        function.instruction(&Instruction::I32Ne);
        function.instruction(&Instruction::If(BlockType::Empty));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::Return);
        function.instruction(&Instruction::End);

        if !layout.item_abi.contains_heap_refs() {
            let helper = self.helper_index(HelperFunction::MemoryEq, source)?;
            Self::emit_array_data_addr(function, LHS_PTR_LOCAL, layout);
            Self::emit_array_data_addr(function, RHS_PTR_LOCAL, layout);
            function.instruction(&Instruction::LocalGet(LHS_PTR_LOCAL));
            MemAccess::array_len().emit_load(function);
            function.instruction(&Instruction::I32Const(layout.item_stride as i32));
            function.instruction(&Instruction::I32Mul);
            function.instruction(&Instruction::Call(helper));
            return Ok(());
        }

        let index_local = self.scratch_i32_local(source)?;
        let lhs_item_local = self.scratch_i32_aux_local(source)?;
        let rhs_item_local = self.object_local(source)?;

        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalSet(index_local));

        function.instruction(&Instruction::Block(BlockType::Empty));
        function.instruction(&Instruction::Loop(BlockType::Empty));

        function.instruction(&Instruction::LocalGet(index_local));
        function.instruction(&Instruction::LocalGet(LHS_PTR_LOCAL));
        MemAccess::array_len().emit_load(function);
        function.instruction(&Instruction::I32GeU);
        function.instruction(&Instruction::BrIf(1));

        Self::emit_array_element_addr(function, LHS_PTR_LOCAL, layout, index_local);
        function.instruction(&Instruction::LocalSet(lhs_item_local));
        Self::emit_array_element_addr(function, RHS_PTR_LOCAL, layout, index_local);
        function.instruction(&Instruction::LocalSet(rhs_item_local));
        self.emit_abi_equality(
            function,
            lhs_item_local,
            rhs_item_local,
            0,
            &layout.item_abi,
            source,
        )?;
        function.instruction(&Instruction::I32Eqz);
        function.instruction(&Instruction::If(BlockType::Empty));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::Return);
        function.instruction(&Instruction::End);

        function.instruction(&Instruction::LocalGet(index_local));
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(index_local));
        function.instruction(&Instruction::Br(0));

        function.instruction(&Instruction::End);
        function.instruction(&Instruction::End);
        function.instruction(&Instruction::I32Const(1));
        Ok(())
    }

    pub(super) fn emit_release_value_from_local(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match abi {
            AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                self.release_heap_ref_from_local(function, local, *ty, source)
            }
            AbiTy::Aggregate(_) if abi.contains_heap_refs() => {
                self.release_aggregate_at_local(function, local, abi, source)
            }
            _ => Ok(()),
        }
    }

    pub(super) fn emit_retain_value_from_local(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match abi {
            AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                function.instruction(&Instruction::LocalGet(local));
                self.retain_heap_ref_on_stack(function, source)?;
                function.instruction(&Instruction::Drop);
                Ok(())
            }
            AbiTy::Aggregate(_) if abi.contains_heap_refs() => {
                self.retain_aggregate_at_local(function, local, abi, source)
            }
            _ => Ok(()),
        }
    }

    fn expr(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        if expr.abi.is_aggregate() {
            return Err(Diagnostic::error(
                "internal error: aggregate expression reached the scalar Wasm emitter",
                self.node_range(expr.source),
            ));
        }

        match &expr.kind {
            FunctionKernelValueKind::Local(name) => self.emit_local(function, *name, expr.source),
            FunctionKernelValueKind::Capture(field) => {
                self.emit_capture_scalar(function, field, expr.source)
            }
            FunctionKernelValueKind::Clone { value } => {
                self.expr(function, value)?;
                if let AbiTy::Scalar(ty) = value.abi
                    && ty.is_heap_ref()
                {
                    self.retain_heap_ref_on_stack(function, expr.source)?;
                }
                Ok(())
            }
            FunctionKernelValueKind::Bool(value) => {
                function.instruction(&Instruction::I32Const(i32::from(*value)));
                Ok(())
            }
            FunctionKernelValueKind::Int(value) => {
                if matches!(expr.abi, AbiTy::Scalar(BackendTy::I64)) {
                    function.instruction(&Instruction::I64Const(*value));
                } else {
                    function.instruction(&Instruction::I32Const(*value as i32));
                }
                Ok(())
            }
            FunctionKernelValueKind::Float(value) => {
                function.instruction(&Instruction::F64Const((*value).into()));
                Ok(())
            }
            FunctionKernelValueKind::String(offset) => {
                function.instruction(&Instruction::I32Const(*offset as i32));
                Ok(())
            }
            FunctionKernelValueKind::StringFromBytes { ptr, len } => {
                self.emit_owned_string_from_bytes(function, expr.source, ptr, len)
            }
            FunctionKernelValueKind::Char(value) => {
                function.instruction(&Instruction::I32Const(*value as i32));
                Ok(())
            }
            FunctionKernelValueKind::Unit => Ok(()),
            FunctionKernelValueKind::StackAddr { frame_slot } => {
                self.emit_dest_addr(function, Dest::frame_slot(*frame_slot))
            }
            FunctionKernelValueKind::AddrOffset { base, offset } => {
                self.expr(function, base)?;
                if *offset != 0 {
                    function.instruction(&Instruction::I32Const(*offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
            FunctionKernelValueKind::MemoryRead { addr, access } => {
                self.expr(function, addr)?;
                if let Some(access) = access {
                    access.emit_load(function);
                }
                Ok(())
            }
            FunctionKernelValueKind::MemoryWrite { addr, value } => {
                let addr_local = self.scratch_i32_local(expr.source)?;
                match &value.abi {
                    AbiTy::Scalar(BackendTy::Unit) => {
                        self.expr(function, addr)?;
                        function.instruction(&Instruction::Drop);
                        self.expr(function, value)
                    }
                    AbiTy::Scalar(ty) => {
                        self.expr(function, value)?;
                        if value.ownership.is_borrowed() && ty.is_heap_ref() {
                            self.retain_heap_ref_on_stack(function, value.source)?;
                        }
                        let value_local = match ty {
                            BackendTy::Float => self.scratch_f64_local(value.source)?,
                            BackendTy::I64 => self.scratch_i64_local(value.source)?,
                            _ => self.scratch_i32_aux_local(value.source)?,
                        };
                        function.instruction(&Instruction::LocalSet(value_local));
                        self.expr(function, addr)?;
                        function.instruction(&Instruction::LocalSet(addr_local));
                        if ty.is_heap_ref() {
                            function.instruction(&Instruction::LocalGet(addr_local));
                            emit_scalar_load(function, *ty, 0);
                            self.release_heap_ref_from_stack(function, *ty, expr.source)?;
                        }
                        function.instruction(&Instruction::LocalGet(addr_local));
                        function.instruction(&Instruction::LocalGet(value_local));
                        emit_scalar_store(function, *ty, 0);
                        Ok(())
                    }
                    AbiTy::Aggregate(layout) => {
                        let temp = self.temp_dest(value.source)?;
                        self.emit_expr_into_owner(function, temp, value)?;
                        self.expr(function, addr)?;
                        function.instruction(&Instruction::LocalSet(addr_local));
                        if value.abi.contains_heap_refs() {
                            self.release_aggregate_at_local(
                                function,
                                addr_local,
                                &value.abi,
                                expr.source,
                            )?;
                        }
                        self.copy_dest_to_dest(
                            function,
                            Dest::pointer_local(addr_local),
                            temp,
                            layout.size,
                        )
                    }
                }
            }
            FunctionKernelValueKind::PointerAdd { ptr, count, stride } => {
                self.expr(function, ptr)?;
                self.expr(function, count)?;
                if *stride != 1 {
                    function.instruction(&Instruction::I32Const(*stride as i32));
                    function.instruction(&Instruction::I32Mul);
                }
                function.instruction(&Instruction::I32Add);
                Ok(())
            }
            FunctionKernelValueKind::Array { layout, items } => {
                self.emit_array_value(function, expr, layout, items)
            }
            FunctionKernelValueKind::ArrayRepeat { layout, value, len } => {
                self.emit_array_repeat(function, expr, layout, value, len)
            }
            FunctionKernelValueKind::Struct { fields } => {
                self.emit_nominal_struct_value(function, expr, fields)
            }
            FunctionKernelValueKind::Union { .. } => Err(Diagnostic::error(
                "internal error: union aggregate reached the scalar Wasm emitter",
                self.node_range(expr.source),
            )),
            FunctionKernelValueKind::VariantValue { variant } => {
                self.emit_nominal_variant_value(function, expr, variant)
            }
            FunctionKernelValueKind::VariantCall { variant, args } => {
                self.emit_nominal_variant_call(function, expr, variant, args)
            }
            FunctionKernelValueKind::Call { target, args } => {
                self.emit_call(function, expr.source, target, args)
            }
            FunctionKernelValueKind::IndirectCall { callee, signature, args } => {
                self.emit_indirect_call(function, expr.source, callee, signature, args)
            }
            FunctionKernelValueKind::Binary { op, lhs, rhs } => {
                self.emit_binary(function, expr.source, *op, lhs, rhs)
            }
            FunctionKernelValueKind::Prefix { op, expr: inner } => {
                self.emit_prefix(function, expr.source, *op, inner)
            }
            FunctionKernelValueKind::Block { .. }
            | FunctionKernelValueKind::Loop { .. }
            | FunctionKernelValueKind::Break
            | FunctionKernelValueKind::Continue
            | FunctionKernelValueKind::If { .. }
            | FunctionKernelValueKind::Match { .. } => Err(Diagnostic::error(
                "internal error: structured control flow reached raw kernel leaf emission",
                self.node_range(expr.source),
            )),
            FunctionKernelValueKind::Field { base, field } => {
                self.emit_field(function, expr.source, base, field)
            }
            kind => Err(Diagnostic::error(
                format!("internal error: unsupported IR node in scalar Wasm emitter: {kind:?}"),
                self.node_range(expr.source),
            )),
        }
    }

    fn structured_wasm_expr(
        &mut self,
        function: &mut WasmFunction,
        expr: &StructuredWasmExpr<'db>,
    ) -> Result<(), Diagnostic> {
        if expr.abi.is_aggregate() {
            return Err(Diagnostic::error(
                "internal error: aggregate expression reached the stackified scalar Wasm emitter",
                self.node_range(expr.source),
            ));
        }

        match &expr.kind {
            StructuredWasmExprKind::Leaf(backend) => self.expr(function, backend),
            StructuredWasmExprKind::WasmLocal(local) => {
                function.instruction(&Instruction::LocalGet(*local));
                Ok(())
            }
            StructuredWasmExprKind::Eqz { value } => {
                self.structured_wasm_expr(function, value)?;
                match value.abi {
                    AbiTy::Scalar(BackendTy::I64) => {
                        function.instruction(&Instruction::I64Eqz);
                    }
                    AbiTy::Scalar(_) => {
                        function.instruction(&Instruction::I32Eqz);
                    }
                    AbiTy::Aggregate(_) => {
                        return Err(Diagnostic::error(
                            "internal error: aggregate eqz operand reached Wasm emission",
                            self.node_range(value.source),
                        ));
                    }
                }
                Ok(())
            }
            StructuredWasmExprKind::TeeLocal { local, value } => {
                self.structured_wasm_expr(function, value)?;
                if value.ownership.is_borrowed()
                    && matches!(value.abi, AbiTy::Scalar(ty) if ty.is_heap_ref())
                {
                    self.retain_heap_ref_on_stack(function, value.source)?;
                }
                function.instruction(&Instruction::LocalTee(*local));
                Ok(())
            }
            StructuredWasmExprKind::Select { cond, then_value, else_value } => {
                self.structured_wasm_expr(function, then_value)?;
                self.structured_wasm_expr(function, else_value)?;
                self.structured_wasm_expr(function, cond)?;
                function.instruction(&Instruction::Select);
                Ok(())
            }
            StructuredWasmExprKind::Block { region, .. } => {
                self.emit_structured_wasm_region(function, region)
            }
            StructuredWasmExprKind::If { cond, then_region, else_region, result_arity } => self
                .emit_structured_wasm_if(
                    function,
                    expr.source,
                    cond,
                    then_region,
                    else_region,
                    *result_arity,
                ),
            StructuredWasmExprKind::Match {
                scrutinee,
                arms,
                result_arity,
                fallback_unreachable,
            } => self.emit_structured_wasm_match(
                function,
                expr.source,
                scrutinee,
                arms,
                *result_arity,
                *fallback_unreachable,
            ),
            StructuredWasmExprKind::Loop { body, .. } => {
                function.instruction(&Instruction::Block(BlockType::Empty));
                let break_target = self.control_depth;
                self.control_depth += 1;

                function.instruction(&Instruction::Loop(BlockType::Empty));
                let continue_target = self.control_depth;
                self.control_depth += 1;
                self.loop_stack.push(LoopTargets {
                    break_target,
                    continue_target,
                    scope_depth: self.scope_stack.len(),
                });

                if let Some(body) = body {
                    self.structured_wasm_expr(function, body)?;
                }
                function.instruction(&Instruction::Br(0));

                self.loop_stack.pop();
                self.control_depth -= 1;
                function.instruction(&Instruction::End);

                self.control_depth -= 1;
                function.instruction(&Instruction::End);
                Ok(())
            }
            StructuredWasmExprKind::Break => self.emit_break(function, expr.source),
            StructuredWasmExprKind::Continue => self.emit_continue(function, expr.source),
            StructuredWasmExprKind::Unreachable => {
                function.instruction(&Instruction::Unreachable);
                Ok(())
            }
        }
    }

    fn structured_wasm_stmt(
        &mut self,
        function: &mut WasmFunction,
        stmt: &StructuredWasmStmt<'db>,
    ) -> Result<(), Diagnostic> {
        match stmt {
            StructuredWasmStmt::Local { name, abi, initializer, ownership_ops } => {
                let Some(initializer) = initializer.as_ref() else {
                    return Ok(());
                };
                let slot = self.layout.slots.get(name).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing local slot",
                        self.node_range((*name).into()),
                    )
                })?;
                match abi {
                    AbiTy::Scalar(ty) => {
                        self.structured_wasm_expr(function, initializer)?;
                        if !ownership_ops.is_empty() {
                            if let Some(index) = slot.local_index {
                                function.instruction(&Instruction::LocalSet(index));
                                self.emit_local_ownership_ops(
                                    function,
                                    index,
                                    abi,
                                    initializer.source,
                                    ownership_ops,
                                )?;
                            } else if !matches!(ty, BackendTy::Unit) {
                                return Err(Diagnostic::error(
                                    "internal error: scalar local is missing a Wasm local index",
                                    self.node_range((*name).into()),
                                ));
                            }
                        } else {
                            if initializer.ownership.is_borrowed() && ty.is_heap_ref() {
                                self.retain_heap_ref_on_stack(function, initializer.source)?;
                            }
                            if let Some(index) = slot.local_index {
                                function.instruction(&Instruction::LocalSet(index));
                            } else if !matches!(ty, BackendTy::Unit) {
                                return Err(Diagnostic::error(
                                    "internal error: scalar local is missing a Wasm local index",
                                    self.node_range((*name).into()),
                                ));
                            }
                        }
                    }
                    AbiTy::Aggregate(_) => {
                        let dest = self.dest_for_slot(slot)?;
                        self.emit_structured_wasm_expr_into_owner(function, dest, initializer)?;
                        if !ownership_ops.is_empty() {
                            self.emit_dest_ownership_ops(
                                function,
                                dest,
                                abi,
                                initializer.source,
                                ownership_ops,
                            )?;
                        }
                    }
                }
                self.register_scope_local(*name, abi);
                Ok(())
            }
            StructuredWasmStmt::Pattern(init) => {
                self.emit_structured_wasm_binding_init_stmt(function, init)
            }
            StructuredWasmStmt::Assign { name, abi, value } => {
                let slot = self.layout.slots.get(name).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing mutable local slot during backend emission",
                        self.node_range((*name).into()),
                    )
                })?;
                match abi {
                    AbiTy::Scalar(ty) => {
                        self.structured_wasm_expr(function, value)?;
                        if value.ownership.is_borrowed() && ty.is_heap_ref() {
                            self.retain_heap_ref_on_stack(function, value.source)?;
                        }
                        let Some(index) = slot.local_index else {
                            return Err(Diagnostic::error(
                                "internal error: scalar mutable local is missing a Wasm local \
                                 index",
                                self.node_range((*name).into()),
                            ));
                        };
                        if ty.is_heap_ref() {
                            let value_local = self.scratch_i32_aux_local(value.source)?;
                            function.instruction(&Instruction::LocalSet(value_local));
                            function.instruction(&Instruction::LocalGet(index));
                            self.release_heap_ref_from_stack(function, *ty, value.source)?;
                            function.instruction(&Instruction::LocalGet(value_local));
                            function.instruction(&Instruction::LocalSet(index));
                        } else {
                            function.instruction(&Instruction::LocalSet(index));
                        }
                        Ok(())
                    }
                    AbiTy::Aggregate(layout) => {
                        let temp = self.temp_dest(value.source)?;
                        self.emit_structured_wasm_expr_into_owner(function, temp, value)?;
                        if abi.contains_heap_refs() {
                            self.release_aggregate_at_dest(
                                function,
                                self.dest_for_slot(slot)?,
                                abi,
                                value.source,
                            )?;
                        }
                        self.copy_dest_to_dest(
                            function,
                            self.dest_for_slot(slot)?,
                            temp,
                            layout.size,
                        )
                    }
                }
            }
            StructuredWasmStmt::If {
                source: _,
                abi,
                ownership,
                cond,
                then_region,
                else_region,
                ownership_ops,
            } => self.emit_structured_wasm_stmt_if(
                function,
                StructuredStmtContext { abi, ownership: *ownership, ownership_ops },
                cond,
                then_region,
                else_region,
            ),
            StructuredWasmStmt::Match {
                source,
                abi,
                ownership,
                scrutinee,
                arms,
                fallback_unreachable,
                ownership_ops,
            } => self.emit_structured_wasm_stmt_match(
                function,
                *source,
                StructuredStmtContext { abi, ownership: *ownership, ownership_ops },
                scrutinee,
                arms,
                *fallback_unreachable,
            ),
            StructuredWasmStmt::Expr { expr, ownership_ops } => {
                self.emit_structured_wasm_discarded_expr(function, expr, ownership_ops)
            }
            StructuredWasmStmt::Return { source, value, ownership_ops } => {
                self.emit_structured_wasm_return(function, *source, value.as_ref(), ownership_ops)
            }
            StructuredWasmStmt::SetLocal { local, value, .. } => {
                self.structured_wasm_expr(function, value)?;
                if value.ownership.is_borrowed()
                    && matches!(value.abi, AbiTy::Scalar(ty) if ty.is_heap_ref())
                {
                    self.retain_heap_ref_on_stack(function, value.source)?;
                }
                function.instruction(&Instruction::LocalSet(*local));
                Ok(())
            }
        }
    }

    fn emit_structured_wasm_binding_init_stmt(
        &mut self,
        function: &mut WasmFunction,
        init: &StructuredWasmBindingInit<'db>,
    ) -> Result<(), Diagnostic> {
        self.emit_structured_wasm_binding_init(function, init)?;
        self.register_pattern_locals(&init.pattern)
    }

    fn emit_structured_wasm_binding_init(
        &mut self,
        function: &mut WasmFunction,
        init: &StructuredWasmBindingInit<'db>,
    ) -> Result<(), Diagnostic> {
        let source_expr = match &init.source {
            StructuredWasmBindingSource::Expr(expr) => expr.source,
            StructuredWasmBindingSource::Param { source, .. } => *source,
        };
        let value = self.materialize_structured_wasm_binding_source(function, &init.source)?;
        self.emit_pattern_bindings(function, &value, &init.pattern, source_expr)?;
        self.release_pattern_value(function, &value, source_expr)
    }

    fn register_pattern_locals(&mut self, pattern: &BackendPattern) -> Result<(), Diagnostic> {
        let mut names = Vec::new();
        pattern.binding_names(&mut names);
        for name in names {
            let slot = self.layout.slots.get(&name).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing pattern binding slot during ARC registration",
                    self.node_range(name.into()),
                )
            })?;
            self.register_scope_local(name, &slot.abi);
        }
        Ok(())
    }

    fn materialize_structured_wasm_binding_source(
        &mut self,
        function: &mut WasmFunction,
        source: &StructuredWasmBindingSource<'db>,
    ) -> Result<PatternValue, Diagnostic> {
        match source {
            StructuredWasmBindingSource::Expr(expr) => match &expr.abi {
                AbiTy::Scalar(BackendTy::Unit) => Ok(PatternValue {
                    storage: PatternStorage::Unit,
                    abi: expr.abi.clone(),
                    offset: 0,
                }),
                AbiTy::Scalar(ty) => {
                    let local = self.pattern_source_local(expr.source)?;
                    self.structured_wasm_expr(function, expr)?;
                    if expr.ownership.is_borrowed() && ty.is_heap_ref() {
                        self.retain_heap_ref_on_stack(function, expr.source)?;
                    }
                    function.instruction(&Instruction::LocalSet(local));
                    Ok(PatternValue {
                        storage: PatternStorage::ScalarLocal(local),
                        abi: expr.abi.clone(),
                        offset: 0,
                    })
                }
                AbiTy::Aggregate(_) => {
                    let dest = self.temp_dest(expr.source)?;
                    self.emit_structured_wasm_expr_into_owner(function, dest, expr)?;
                    let DestBase::FrameSlot(frame_slot) = dest.base else {
                        return Err(Diagnostic::error(
                            "internal error: aggregate pattern source temp must be frame-backed",
                            self.node_range(expr.source),
                        ));
                    };
                    Ok(PatternValue {
                        storage: PatternStorage::FrameSlot(frame_slot),
                        abi: expr.abi.clone(),
                        offset: dest.offset,
                    })
                }
            },
            StructuredWasmBindingSource::Param { index, abi, source } => {
                let slot = self.layout.raw_params.get(*index).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing raw parameter slot during pattern emission",
                        self.node_range(*source),
                    )
                })?;
                Ok(PatternValue {
                    storage: match abi {
                        AbiTy::Scalar(BackendTy::Unit) => PatternStorage::Unit,
                        AbiTy::Scalar(_) => PatternStorage::ScalarLocal(
                            slot.local_index.expect("scalar param should have a Wasm local"),
                        ),
                        AbiTy::Aggregate(_) => PatternStorage::PointerLocal(
                            slot.local_index.expect("aggregate param should lower as a pointer"),
                        ),
                    },
                    abi: abi.clone(),
                    offset: 0,
                })
            }
        }
    }

    fn emit_pattern_bindings(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        pattern: &BackendPattern,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match pattern {
            BackendPattern::Binding(name) => {
                self.copy_pattern_value_to_binding(function, value, *name, source)
            }
            BackendPattern::Wildcard | BackendPattern::Literal(_) => Ok(()),
            BackendPattern::Tuple(fields) | BackendPattern::Struct(fields) => {
                for field in fields {
                    let field_value = self.project_pattern_field(value, &field.field, source)?;
                    let field_value =
                        self.materialize_pattern_value(function, &field_value, source)?;
                    self.emit_pattern_bindings(function, &field_value, &field.pattern, source)?;
                }
                Ok(())
            }
            BackendPattern::Variant { fields, .. } => {
                for field in fields {
                    let field_value = self.project_pattern_field(value, &field.field, source)?;
                    let field_value =
                        self.materialize_pattern_value(function, &field_value, source)?;
                    self.emit_pattern_bindings(function, &field_value, &field.pattern, source)?;
                }
                Ok(())
            }
        }
    }

    fn emit_pattern_test(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        pattern: &BackendPattern,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match pattern {
            BackendPattern::Binding(_) | BackendPattern::Wildcard => {
                function.instruction(&Instruction::I32Const(1));
                Ok(())
            }
            BackendPattern::Literal(BackendPatternLiteral::Bool(expected)) => {
                self.emit_pattern_scalar(function, value, source)?;
                function.instruction(&Instruction::I32Const(i32::from(*expected)));
                function.instruction(&Instruction::I32Eq);
                Ok(())
            }
            BackendPattern::Literal(BackendPatternLiteral::Int(expected)) => {
                self.emit_pattern_scalar(function, value, source)?;
                if let AbiTy::Scalar(BackendTy::I64) = value.abi {
                    function.instruction(&Instruction::I64Const(*expected));
                    function.instruction(&Instruction::I64Eq);
                } else {
                    function.instruction(&Instruction::I32Const(*expected as i32));
                    function.instruction(&Instruction::I32Eq);
                }
                Ok(())
            }
            BackendPattern::Literal(BackendPatternLiteral::Char(expected)) => {
                self.emit_pattern_scalar(function, value, source)?;
                function.instruction(&Instruction::I32Const(*expected as i32));
                function.instruction(&Instruction::I32Eq);
                Ok(())
            }
            BackendPattern::Literal(BackendPatternLiteral::String(expected)) => {
                let scratch = self.scratch_i32_local(source)?;
                self.emit_pattern_scalar(function, value, source)?;
                function.instruction(&Instruction::LocalSet(scratch));
                function.instruction(&Instruction::LocalGet(scratch));
                function.instruction(&Instruction::I32Const(*expected as i32));
                function.instruction(&Instruction::Call(
                    self.helper_index(HelperFunction::StringEq, source)?,
                ));
                Ok(())
            }
            BackendPattern::Tuple(fields) | BackendPattern::Struct(fields) => {
                if fields.is_empty() {
                    function.instruction(&Instruction::I32Const(1));
                    return Ok(());
                }
                for (index, field) in fields.iter().enumerate() {
                    let field_value = self.project_pattern_field(value, &field.field, source)?;
                    let field_value =
                        self.materialize_pattern_value(function, &field_value, source)?;
                    self.emit_pattern_test(function, &field_value, &field.pattern, source)?;
                    if index > 0 {
                        function.instruction(&Instruction::I32And);
                    }
                }
                Ok(())
            }
            BackendPattern::Variant { variant, fields } => {
                let container = self.addressable_pattern_value(value, source)?;
                self.emit_pattern_addr(function, &container, source)?;
                MemAccess::enum_tag(0).emit_load(function);
                function.instruction(&Instruction::I32Const(variant.tag));
                function.instruction(&Instruction::I32Eq);
                for field in fields {
                    let field_value =
                        self.project_pattern_field(&container, &field.field, source)?;
                    let field_value =
                        self.materialize_pattern_value(function, &field_value, source)?;
                    self.emit_pattern_test(function, &field_value, &field.pattern, source)?;
                    function.instruction(&Instruction::I32And);
                }
                Ok(())
            }
        }
    }

    fn copy_pattern_value_to_binding(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        name: NameId,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let slot = self.layout.slots.get(&name).ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing pattern binding slot",
                self.node_range(name.into()),
            )
        })?;
        match &slot.abi {
            AbiTy::Scalar(BackendTy::Unit) => Ok(()),
            AbiTy::Scalar(ty) => {
                self.emit_pattern_scalar(function, value, source)?;
                if ty.is_heap_ref() {
                    self.retain_heap_ref_on_stack(function, source)?;
                }
                if let Some(local) = slot.local_index {
                    function.instruction(&Instruction::LocalSet(local));
                    Ok(())
                } else {
                    Err(Diagnostic::error(
                        "internal error: scalar pattern binding is missing a Wasm local index",
                        self.node_range(name.into()),
                    ))
                }
            }
            AbiTy::Aggregate(layout) => {
                let dest = self.dest_for_slot(slot)?;
                self.emit_dest_addr(function, dest)?;
                self.emit_pattern_addr(function, value, source)?;
                function.instruction(&Instruction::I32Const(layout.size as i32));
                function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
                if slot.abi.contains_heap_refs() {
                    self.retain_aggregate_at_dest(function, dest, &slot.abi, source)?;
                }
                Ok(())
            }
        }
    }

    fn project_pattern_field(
        &self,
        value: &PatternValue,
        field: &FieldLayout,
        source: ExprId,
    ) -> Result<PatternValue, Diagnostic> {
        let storage = match value.storage {
            PatternStorage::ScalarLocal(local) => match value.abi {
                AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(_))) => {
                    PatternStorage::PointerLocal(local)
                }
                _ => {
                    return Err(Diagnostic::error(
                        "internal error: non-addressable pattern value cannot be projected",
                        self.node_range(source),
                    ));
                }
            },
            PatternStorage::PointerLocal(local) => PatternStorage::PointerLocal(local),
            PatternStorage::FrameSlot(slot) => PatternStorage::FrameSlot(slot),
            PatternStorage::Unit => {
                return Err(Diagnostic::error(
                    "internal error: unit pattern value cannot be projected",
                    self.node_range(source),
                ));
            }
        };
        Ok(PatternValue { storage, abi: field.ty.clone(), offset: value.offset + field.offset })
    }

    fn addressable_pattern_value(
        &self,
        value: &PatternValue,
        source: ExprId,
    ) -> Result<PatternValue, Diagnostic> {
        match value.storage {
            PatternStorage::PointerLocal(_) | PatternStorage::FrameSlot(_) => Ok(value.clone()),
            PatternStorage::ScalarLocal(local)
                if matches!(value.abi, AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(_)))) =>
            {
                Ok(PatternValue {
                    storage: PatternStorage::PointerLocal(local),
                    abi: value.abi.clone(),
                    offset: value.offset,
                })
            }
            _ => Err(Diagnostic::error(
                "internal error: pattern value is not addressable",
                self.node_range(source),
            )),
        }
    }

    fn materialize_pattern_value(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        source: ExprId,
    ) -> Result<PatternValue, Diagnostic> {
        let AbiTy::Scalar(ty) = value.abi else {
            return Ok(value.clone());
        };
        if matches!(value.storage, PatternStorage::ScalarLocal(_)) && value.offset == 0 {
            return Ok(value.clone());
        }

        let local = match ty {
            BackendTy::Float => self.scratch_f64_local(source)?,
            BackendTy::I64 => self.scratch_i64_local(source)?,
            BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_) => {
                self.scratch_i32_local(source)?
            }
            BackendTy::Unit => {
                return Ok(PatternValue {
                    storage: PatternStorage::Unit,
                    abi: value.abi.clone(),
                    offset: 0,
                });
            }
        };
        self.emit_pattern_addr(function, value, source)?;
        emit_scalar_load(function, ty, 0);
        function.instruction(&Instruction::LocalSet(local));
        Ok(PatternValue {
            storage: PatternStorage::ScalarLocal(local),
            abi: value.abi.clone(),
            offset: 0,
        })
    }

    fn emit_pattern_addr(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match value.storage {
            PatternStorage::PointerLocal(local) => {
                function.instruction(&Instruction::LocalGet(local));
                if value.offset != 0 {
                    function.instruction(&Instruction::I32Const(value.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
            PatternStorage::FrameSlot(slot) => {
                self.emit_dest_addr(function, Dest::frame_slot(slot).with_offset(value.offset))
            }
            PatternStorage::ScalarLocal(local)
                if matches!(value.abi, AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(_)))) =>
            {
                function.instruction(&Instruction::LocalGet(local));
                if value.offset != 0 {
                    function.instruction(&Instruction::I32Const(value.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
            _ => Err(Diagnostic::error(
                "internal error: attempted to take the address of a scalar pattern value",
                self.node_range(source),
            )),
        }
    }

    fn emit_pattern_scalar(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let AbiTy::Scalar(ty) = value.abi else {
            return Err(Diagnostic::error(
                "internal error: aggregate pattern value reached scalar emission",
                self.node_range(source),
            ));
        };
        if let (PatternStorage::ScalarLocal(local), 0) = (value.storage, value.offset) {
            function.instruction(&Instruction::LocalGet(local));
            Ok(())
        } else {
            self.emit_pattern_addr(function, value, source)?;
            emit_scalar_load(function, ty, 0);
            Ok(())
        }
    }

    fn release_pattern_value(
        &mut self,
        function: &mut WasmFunction,
        value: &PatternValue,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match (&value.abi, value.storage) {
            (AbiTy::Scalar(ty), PatternStorage::ScalarLocal(local)) if ty.is_heap_ref() => {
                self.release_heap_ref_from_local(function, local, *ty, source)
            }
            (AbiTy::Aggregate(_), PatternStorage::PointerLocal(local)) => {
                self.release_aggregate_at_local(function, local, &value.abi, source)
            }
            (AbiTy::Aggregate(_), PatternStorage::FrameSlot(slot)) => self
                .release_aggregate_at_dest(
                    function,
                    Dest::frame_slot(slot).with_offset(value.offset),
                    &value.abi,
                    source,
                ),
            _ => Ok(()),
        }
    }

    fn emit_structured_wasm_return(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        value: Option<&StructuredWasmExpr<'db>>,
        ownership_ops: &[StructuredWasmOwnershipOp],
    ) -> Result<(), Diagnostic> {
        match self.legalized_result_passing() {
            LegalizedResultPassing::Unit => {
                if let Some(value) = value {
                    self.structured_wasm_expr(function, value)?;
                    if !ownership_ops.is_empty() {
                        if let AbiTy::Scalar(ty) = value.abi {
                            let consumed = self.emit_stack_ownership_ops(
                                function,
                                ty,
                                value.source,
                                ownership_ops,
                            )?;
                            if !consumed && !matches!(ty, BackendTy::Unit) {
                                function.instruction(&Instruction::Drop);
                            }
                        }
                    } else {
                        match value.abi {
                            AbiTy::Scalar(ty) if value.ownership.is_owned() && ty.is_heap_ref() => {
                                self.release_heap_ref_from_stack(function, ty, value.source)?;
                            }
                            AbiTy::Scalar(BackendTy::Unit) => {}
                            _ => {
                                function.instruction(&Instruction::Drop);
                            }
                        }
                    }
                }
                self.release_scopes_to(function, 0, source)?;
            }
            LegalizedResultPassing::DirectScalar(BackendTy::Float) => {
                if let Some(value) = value {
                    self.structured_wasm_expr(function, value)?;
                    let scratch = self.scratch_f64_local(source)?;
                    function.instruction(&Instruction::LocalSet(scratch));
                    self.release_scopes_to(function, 0, source)?;
                    function.instruction(&Instruction::LocalGet(scratch));
                } else {
                    self.release_scopes_to(function, 0, source)?;
                }
            }
            LegalizedResultPassing::DirectScalar(ty) => {
                if let Some(value) = value {
                    self.structured_wasm_expr(function, value)?;
                    if !ownership_ops.is_empty() {
                        if self.emit_stack_ownership_ops(
                            function,
                            ty,
                            value.source,
                            ownership_ops,
                        )? {
                            return Err(Diagnostic::error(
                                "internal error: explicit ownership ops consumed a non-unit \
                                 scalar return value",
                                self.node_range(source),
                            ));
                        }
                    } else if value.ownership.is_borrowed() && ty.is_heap_ref() {
                        self.retain_heap_ref_on_stack(function, value.source)?;
                    }
                    let scratch = match ty {
                        BackendTy::I64 => self.scratch_i64_local(source)?,
                        _ => self.result_i32_local(source)?,
                    };
                    function.instruction(&Instruction::LocalSet(scratch));
                    self.release_scopes_to(function, 0, source)?;
                    function.instruction(&Instruction::LocalGet(scratch));
                } else {
                    self.release_scopes_to(function, 0, source)?;
                }
            }
            LegalizedResultPassing::IndirectOutPtr => {
                let result_ptr = self.legalized_result_ptr_local().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: aggregate result is missing its out-pointer local",
                        self.node_range(source),
                    )
                })?;
                if let Some(value) = value {
                    self.emit_structured_wasm_expr_into_owner(
                        function,
                        Dest::pointer_local(result_ptr),
                        value,
                    )?;
                    if !ownership_ops.is_empty() {
                        self.emit_dest_ownership_ops(
                            function,
                            Dest::pointer_local(result_ptr),
                            &value.abi,
                            value.source,
                            ownership_ops,
                        )?;
                    }
                }
                self.release_scopes_to(function, 0, source)?;
            }
        }
        function.instruction(&Instruction::Br(self.return_branch_depth(source)?));
        Ok(())
    }

    fn emit_structured_wasm_region(
        &mut self,
        function: &mut WasmFunction,
        region: &StructuredWasmRegion<'db>,
    ) -> Result<(), Diagnostic> {
        self.push_scope();
        for stmt in &region.stmts {
            self.structured_wasm_stmt(function, stmt)?;
        }
        if let Some(tail) = region.tail.as_deref() {
            self.structured_wasm_expr(function, tail)?;
        }
        let source = region.tail.as_deref().map_or(ExprId::ZERO, |tail| tail.source);
        self.release_scope(function, source)?;
        Ok(())
    }

    fn emit_structured_wasm_region_into_owner(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        region: &StructuredWasmRegion<'db>,
    ) -> Result<(), Diagnostic> {
        self.push_scope();
        for stmt in &region.stmts {
            self.structured_wasm_stmt(function, stmt)?;
        }
        if let Some(tail) = region.tail.as_deref() {
            self.emit_structured_wasm_expr_into_owner(function, dest, tail)?;
        }
        let source = region.tail.as_deref().map_or(ExprId::ZERO, |tail| tail.source);
        self.release_scope(function, source)?;
        Ok(())
    }

    fn emit_structured_wasm_region_as_stmt(
        &mut self,
        function: &mut WasmFunction,
        abi: &AbiTy,
        ownership: ValueOwnership,
        ownership_ops: &[StructuredWasmOwnershipOp],
        region: &StructuredWasmRegion<'db>,
    ) -> Result<(), Diagnostic> {
        self.push_scope();
        for stmt in &region.stmts {
            self.structured_wasm_stmt(function, stmt)?;
        }
        if let Some(tail) = region.tail.as_deref() {
            debug_assert_eq!(&tail.abi, abi);
            let tail = StructuredWasmExpr {
                source: tail.source,
                abi: tail.abi.clone(),
                ownership,
                kind: tail.kind.clone(),
            };
            self.emit_structured_wasm_discarded_expr(function, &tail, ownership_ops)?;
        }
        let source = region.tail.as_deref().map_or(ExprId::ZERO, |tail| tail.source);
        self.release_scope(function, source)?;
        Ok(())
    }

    fn emit_structured_wasm_discarded_expr(
        &mut self,
        function: &mut WasmFunction,
        expr: &StructuredWasmExpr<'db>,
        ownership_ops: &[StructuredWasmOwnershipOp],
    ) -> Result<(), Diagnostic> {
        match &expr.abi {
            AbiTy::Scalar(BackendTy::Unit) => self.structured_wasm_expr(function, expr),
            AbiTy::Scalar(ty) => {
                self.structured_wasm_expr(function, expr)?;
                if !ownership_ops.is_empty() {
                    let consumed =
                        self.emit_stack_ownership_ops(function, *ty, expr.source, ownership_ops)?;
                    if !consumed {
                        function.instruction(&Instruction::Drop);
                    }
                } else if expr.ownership.is_owned() && ty.is_heap_ref() {
                    self.release_heap_ref_from_stack(function, *ty, expr.source)?;
                } else {
                    function.instruction(&Instruction::Drop);
                }
                Ok(())
            }
            AbiTy::Aggregate(_) => {
                let dest = self.temp_dest(expr.source)?;
                self.emit_structured_wasm_expr_into_owner(function, dest, expr)?;
                if !ownership_ops.is_empty() {
                    self.emit_dest_ownership_ops(
                        function,
                        dest,
                        &expr.abi,
                        expr.source,
                        ownership_ops,
                    )?;
                } else if expr.ownership.is_owned() && expr.abi.contains_heap_refs() {
                    self.release_aggregate_at_dest(function, dest, &expr.abi, expr.source)?;
                }
                Ok(())
            }
        }
    }

    fn emit_structured_wasm_if(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        cond: &StructuredWasmExpr<'db>,
        then_region: &StructuredWasmRegion<'db>,
        else_region: &StructuredWasmRegion<'db>,
        result_arity: StructuredWasmResultArity,
    ) -> Result<(), Diagnostic> {
        let block_type = match result_arity {
            StructuredWasmResultArity::Unit | StructuredWasmResultArity::Aggregate => {
                BlockType::Empty
            }
            StructuredWasmResultArity::Scalar(ty) => BlockType::Result(ty),
        };
        self.structured_wasm_expr(function, cond)?;
        function.instruction(&Instruction::If(block_type));
        self.control_depth += 1;
        self.emit_structured_wasm_region(function, then_region)?;
        if !else_region.stmts.is_empty() || else_region.tail.is_some() {
            function.instruction(&Instruction::Else);
            self.emit_structured_wasm_region(function, else_region)?;
        }
        self.control_depth -= 1;
        function.instruction(&Instruction::End);
        if matches!(result_arity, StructuredWasmResultArity::Aggregate) {
            return Err(Diagnostic::error(
                "internal error: aggregate if reached scalar stackified emitter",
                self.node_range(source),
            ));
        }
        Ok(())
    }

    fn emit_structured_wasm_if_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        cond: &StructuredWasmExpr<'db>,
        then_region: &StructuredWasmRegion<'db>,
        else_region: &StructuredWasmRegion<'db>,
    ) -> Result<(), Diagnostic> {
        self.structured_wasm_expr(function, cond)?;
        function.instruction(&Instruction::If(BlockType::Empty));
        self.control_depth += 1;
        self.emit_structured_wasm_region_into_owner(function, dest, then_region)?;
        if !else_region.stmts.is_empty() || else_region.tail.is_some() {
            function.instruction(&Instruction::Else);
            self.emit_structured_wasm_region_into_owner(function, dest, else_region)?;
        }
        self.control_depth -= 1;
        function.instruction(&Instruction::End);
        Ok(())
    }

    fn emit_structured_wasm_stmt_if(
        &mut self,
        function: &mut WasmFunction,
        context: StructuredStmtContext<'_>,
        cond: &StructuredWasmExpr<'db>,
        then_region: &StructuredWasmRegion<'db>,
        else_region: &StructuredWasmRegion<'db>,
    ) -> Result<(), Diagnostic> {
        self.structured_wasm_expr(function, cond)?;
        function.instruction(&Instruction::If(BlockType::Empty));
        self.control_depth += 1;
        self.emit_structured_wasm_region_as_stmt(
            function,
            context.abi,
            context.ownership,
            context.ownership_ops,
            then_region,
        )?;
        if !else_region.stmts.is_empty() || else_region.tail.is_some() {
            function.instruction(&Instruction::Else);
            self.emit_structured_wasm_region_as_stmt(
                function,
                context.abi,
                context.ownership,
                context.ownership_ops,
                else_region,
            )?;
        }
        self.control_depth -= 1;
        function.instruction(&Instruction::End);
        Ok(())
    }

    fn emit_structured_wasm_match(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        scrutinee: &StructuredWasmExpr<'db>,
        arms: &[StructuredWasmMatchArm<'db>],
        result_arity: StructuredWasmResultArity,
        fallback_unreachable: bool,
    ) -> Result<(), Diagnostic> {
        let block_type = match result_arity {
            StructuredWasmResultArity::Unit | StructuredWasmResultArity::Aggregate => {
                BlockType::Empty
            }
            StructuredWasmResultArity::Scalar(ty) => BlockType::Result(ty),
        };
        let scrutinee_value = self.materialize_structured_wasm_binding_source(
            function,
            &StructuredWasmBindingSource::Expr(scrutinee.clone()),
        )?;

        function.instruction(&Instruction::Block(block_type));
        self.control_depth += 1;

        for arm in arms {
            function.instruction(&Instruction::Block(BlockType::Empty));
            self.control_depth += 1;

            self.emit_pattern_test(function, &scrutinee_value, &arm.pattern, source)?;
            function.instruction(&Instruction::I32Eqz);
            function.instruction(&Instruction::BrIf(0));

            self.push_scope();
            self.emit_pattern_bindings(function, &scrutinee_value, &arm.pattern, source)?;
            self.register_pattern_locals(&arm.pattern)?;
            self.emit_structured_wasm_region(function, &arm.body)?;
            self.release_scope(function, source)?;
            self.release_pattern_value(function, &scrutinee_value, source)?;
            function.instruction(&Instruction::Br(1));

            self.control_depth -= 1;
            function.instruction(&Instruction::End);
        }

        self.release_pattern_value(function, &scrutinee_value, source)?;
        if fallback_unreachable {
            function.instruction(&Instruction::Unreachable);
        }

        self.control_depth -= 1;
        function.instruction(&Instruction::End);
        Ok(())
    }

    fn emit_structured_wasm_stmt_match(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        context: StructuredStmtContext<'_>,
        scrutinee: &StructuredWasmExpr<'db>,
        arms: &[StructuredWasmMatchArm<'db>],
        fallback_unreachable: bool,
    ) -> Result<(), Diagnostic> {
        let scrutinee_value = self.materialize_structured_wasm_binding_source(
            function,
            &StructuredWasmBindingSource::Expr(scrutinee.clone()),
        )?;

        function.instruction(&Instruction::Block(BlockType::Empty));
        self.control_depth += 1;

        for arm in arms {
            function.instruction(&Instruction::Block(BlockType::Empty));
            self.control_depth += 1;

            self.emit_pattern_test(function, &scrutinee_value, &arm.pattern, source)?;
            function.instruction(&Instruction::I32Eqz);
            function.instruction(&Instruction::BrIf(0));

            self.push_scope();
            self.emit_pattern_bindings(function, &scrutinee_value, &arm.pattern, source)?;
            self.register_pattern_locals(&arm.pattern)?;
            self.emit_structured_wasm_region_as_stmt(
                function,
                context.abi,
                context.ownership,
                context.ownership_ops,
                &arm.body,
            )?;
            self.release_scope(function, source)?;
            self.release_pattern_value(function, &scrutinee_value, source)?;
            function.instruction(&Instruction::Br(1));

            self.control_depth -= 1;
            function.instruction(&Instruction::End);
        }

        self.release_pattern_value(function, &scrutinee_value, source)?;
        if fallback_unreachable {
            function.instruction(&Instruction::Unreachable);
        }

        self.control_depth -= 1;
        function.instruction(&Instruction::End);
        Ok(())
    }

    fn emit_structured_wasm_match_into(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        dest: Dest,
        scrutinee: &StructuredWasmExpr<'db>,
        arms: &[StructuredWasmMatchArm<'db>],
        fallback_unreachable: bool,
    ) -> Result<(), Diagnostic> {
        let scrutinee_value = self.materialize_structured_wasm_binding_source(
            function,
            &StructuredWasmBindingSource::Expr(scrutinee.clone()),
        )?;

        function.instruction(&Instruction::Block(BlockType::Empty));
        self.control_depth += 1;

        for arm in arms {
            function.instruction(&Instruction::Block(BlockType::Empty));
            self.control_depth += 1;

            self.emit_pattern_test(function, &scrutinee_value, &arm.pattern, source)?;
            function.instruction(&Instruction::I32Eqz);
            function.instruction(&Instruction::BrIf(0));

            self.push_scope();
            self.emit_pattern_bindings(function, &scrutinee_value, &arm.pattern, source)?;
            self.register_pattern_locals(&arm.pattern)?;
            self.emit_structured_wasm_region_into_owner(function, dest, &arm.body)?;
            self.release_scope(function, source)?;
            self.release_pattern_value(function, &scrutinee_value, source)?;
            function.instruction(&Instruction::Br(1));

            self.control_depth -= 1;
            function.instruction(&Instruction::End);
        }

        self.release_pattern_value(function, &scrutinee_value, source)?;
        if fallback_unreachable {
            function.instruction(&Instruction::Unreachable);
        }

        self.control_depth -= 1;
        function.instruction(&Instruction::End);
        Ok(())
    }

    fn emit_nominal_struct_value(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
        fields: &[FunctionKernelFieldValue<'db>],
    ) -> Result<(), Diagnostic> {
        let layout = self.nominal_layout_from_expr(expr)?;
        let payload_local = self.alloc_nominal_payload(function, expr.source, &layout)?;
        self.zero_dest(function, Dest::pointer_local(payload_local), layout.size)?;
        for field in fields {
            self.store_expr_to_dest(
                function,
                Dest::pointer_local(payload_local).with_offset(field.field.offset),
                &field.value,
                &field.field.ty,
            )?;
        }
        function.instruction(&Instruction::LocalGet(payload_local));
        Ok(())
    }

    fn emit_nominal_variant_value(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
        variant: &VariantLayout,
    ) -> Result<(), Diagnostic> {
        let layout = self.nominal_layout_from_expr(expr)?;
        let payload_local = self.alloc_nominal_payload(function, expr.source, &layout)?;
        self.zero_dest(function, Dest::pointer_local(payload_local), layout.size)?;
        function.instruction(&Instruction::LocalGet(payload_local));
        function.instruction(&Instruction::I32Const(variant.tag));
        MemAccess::enum_tag(0).emit_store(function);
        function.instruction(&Instruction::LocalGet(payload_local));
        Ok(())
    }

    fn emit_nominal_variant_call(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
        variant: &VariantLayout,
        args: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        let layout = self.nominal_layout_from_expr(expr)?;
        let payload_local = self.alloc_nominal_payload(function, expr.source, &layout)?;
        self.zero_dest(function, Dest::pointer_local(payload_local), layout.size)?;
        function.instruction(&Instruction::LocalGet(payload_local));
        function.instruction(&Instruction::I32Const(variant.tag));
        MemAccess::enum_tag(0).emit_store(function);
        for (field, value) in variant.fields.iter().zip(args.iter()) {
            self.store_expr_to_dest(
                function,
                Dest::pointer_local(payload_local).with_offset(field.offset),
                value,
                &field.ty,
            )?;
        }
        function.instruction(&Instruction::LocalGet(payload_local));
        Ok(())
    }

    pub(super) fn alloc_nominal_payload(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        layout: &AggregateLayout,
    ) -> Result<u32, Diagnostic> {
        let payload_local = self.nominal_local(source)?;
        let alloc = self.runtime_index(RuntimeFunction::Alloc, source)?;
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + layout.size) as i32));
        function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
        function.instruction(&Instruction::Call(alloc));
        function.instruction(&Instruction::LocalTee(payload_local));
        function.instruction(&Instruction::I32Const(1));
        MemAccess::arc_ref_count().emit_store(function);
        function.instruction(&Instruction::LocalGet(payload_local));
        function.instruction(&Instruction::I32Const(0));
        MemAccess::arc_type_bits().emit_store(function);
        function.instruction(&Instruction::LocalGet(payload_local));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(payload_local));
        Ok(payload_local)
    }

    fn emit_owned_string_from_bytes(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        ptr: &FunctionKernelValue<'db>,
        len: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        let src_local = self.scratch_i32_local(source)?;
        let len_local = self.scratch_i32_aux_local(source)?;
        let string_local = self.object_local(source)?;
        let alloc = self.runtime_index(RuntimeFunction::Alloc, source)?;

        self.expr(function, ptr)?;
        function.instruction(&Instruction::LocalSet(src_local));
        self.expr(function, len)?;
        function.instruction(&Instruction::LocalSet(len_local));

        function.instruction(&Instruction::LocalGet(len_local));
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + 4) as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
        function.instruction(&Instruction::Call(alloc));
        function.instruction(&Instruction::LocalTee(string_local));
        function.instruction(&Instruction::I32Const(1));
        MemAccess::arc_ref_count().emit_store(function);
        function.instruction(&Instruction::LocalGet(string_local));
        function.instruction(&Instruction::I32Const(0));
        MemAccess::arc_type_bits().emit_store(function);
        function.instruction(&Instruction::LocalGet(string_local));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalTee(string_local));
        function.instruction(&Instruction::LocalGet(len_local));
        MemAccess::i32(0, 2).emit_store(function);

        function.instruction(&Instruction::LocalGet(string_local));
        function.instruction(&Instruction::I32Const(4));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalGet(src_local));
        function.instruction(&Instruction::LocalGet(len_local));
        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
        function.instruction(&Instruction::LocalGet(string_local));
        Ok(())
    }

    fn emit_array_value(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
        layout: &ArrayRuntimeLayout,
        items: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        let len = u32::try_from(items.len()).map_err(|_overflow| {
            Diagnostic::error(
                "internal error: array literal exceeds the supported length",
                self.node_range(expr.source),
            )
        })?;
        let array_local = self.alloc_array_with_len_const(function, expr.source, layout, len)?;
        for (index, item) in items.iter().enumerate() {
            let index = u32::try_from(index).map_err(|_overflow| {
                Diagnostic::error(
                    "internal error: array literal index exceeds the supported range",
                    self.node_range(expr.source),
                )
            })?;
            let offset = layout
                .item_stride
                .checked_mul(index)
                .and_then(|offset| offset.checked_add(layout.data_offset))
                .ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: array literal layout overflowed during emission",
                        self.node_range(expr.source),
                    )
                })?;
            self.emit_expr_into_owner(
                function,
                Dest::pointer_local(array_local).with_offset(offset),
                item,
            )?;
        }
        function.instruction(&Instruction::LocalGet(array_local));
        Ok(())
    }

    fn emit_array_repeat(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
        layout: &ArrayRuntimeLayout,
        value: &FunctionKernelValue<'db>,
        len: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        let repeat_local = self.array_repeat_local(expr.source)?;
        let array_local = self.nominal_local(expr.source)?;

        self.expr(function, len)?;
        function.instruction(&Instruction::LocalSet(repeat_local));

        function.instruction(&Instruction::LocalGet(repeat_local));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32LtS);
        function.instruction(&Instruction::If(BlockType::Empty));
        function.instruction(&Instruction::Unreachable);
        function.instruction(&Instruction::End);

        self.alloc_array_with_len_local(function, expr.source, layout, repeat_local)?;

        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::I32Const(0));
        MemAccess::array_capacity().emit_store(function);

        function.instruction(&Instruction::Block(BlockType::Empty));
        function.instruction(&Instruction::Loop(BlockType::Empty));

        function.instruction(&Instruction::LocalGet(array_local));
        MemAccess::array_capacity().emit_load(function);
        function.instruction(&Instruction::LocalGet(array_local));
        MemAccess::array_len().emit_load(function);
        function.instruction(&Instruction::I32GeU);
        function.instruction(&Instruction::BrIf(1));

        function.instruction(&Instruction::LocalGet(array_local));
        if layout.data_offset != 0 {
            function.instruction(&Instruction::I32Const(layout.data_offset as i32));
            function.instruction(&Instruction::I32Add);
        }
        function.instruction(&Instruction::LocalGet(array_local));
        MemAccess::array_capacity().emit_load(function);
        if layout.item_stride != 0 {
            function.instruction(&Instruction::I32Const(layout.item_stride as i32));
            function.instruction(&Instruction::I32Mul);
            function.instruction(&Instruction::I32Add);
        } else {
            function.instruction(&Instruction::Drop);
        }
        function.instruction(&Instruction::LocalSet(repeat_local));
        self.emit_expr_into_owner(function, Dest::pointer_local(repeat_local), value)?;

        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::LocalGet(array_local));
        MemAccess::array_capacity().emit_load(function);
        function.instruction(&Instruction::I32Const(1));
        function.instruction(&Instruction::I32Add);
        MemAccess::array_capacity().emit_store(function);
        function.instruction(&Instruction::Br(0));

        function.instruction(&Instruction::End);
        function.instruction(&Instruction::End);

        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::LocalGet(array_local));
        MemAccess::array_len().emit_load(function);
        MemAccess::array_capacity().emit_store(function);
        function.instruction(&Instruction::LocalGet(array_local));
        Ok(())
    }

    fn alloc_array_with_len_const(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        layout: &ArrayRuntimeLayout,
        len: u32,
    ) -> Result<u32, Diagnostic> {
        layout.total_size_for_len(len).ok_or_else(|| {
            Diagnostic::error(
                "internal error: array literal allocation overflowed during Wasm emission",
                self.node_range(source),
            )
        })?;
        let len_local = self.array_repeat_local(source)?;
        function.instruction(&Instruction::I32Const(len as i32));
        function.instruction(&Instruction::LocalSet(len_local));
        self.alloc_array_with_len_local(function, source, layout, len_local)
    }

    fn alloc_array_with_len_local(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        layout: &ArrayRuntimeLayout,
        len_local: u32,
    ) -> Result<u32, Diagnostic> {
        let array_local = self.nominal_local(source)?;
        let alloc = self.runtime_index(RuntimeFunction::Alloc, source)?;

        function.instruction(&Instruction::LocalGet(len_local));
        function.instruction(&Instruction::I32Const(layout.item_stride as i32));
        function.instruction(&Instruction::I32Mul);
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + layout.data_offset) as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Const(layout.object_align() as i32));
        function.instruction(&Instruction::Call(alloc));
        function.instruction(&Instruction::LocalTee(array_local));
        function.instruction(&Instruction::I32Const(1));
        MemAccess::arc_ref_count().emit_store(function);
        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::I32Const(0));
        MemAccess::arc_type_bits().emit_store(function);
        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(array_local));

        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::LocalGet(len_local));
        function.instruction(&Instruction::I32Const(layout.item_stride as i32));
        function.instruction(&Instruction::I32Mul);
        function.instruction(&Instruction::I32Const(layout.data_offset as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::MemoryFill(0));

        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::LocalGet(len_local));
        MemAccess::array_len().emit_store(function);
        function.instruction(&Instruction::LocalGet(array_local));
        function.instruction(&Instruction::LocalGet(len_local));
        MemAccess::array_capacity().emit_store(function);
        Ok(array_local)
    }

    fn nominal_layout_from_expr(
        &self,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<AggregateLayout, Diagnostic> {
        let bits = match expr.abi {
            AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(bits))) => bits,
            _ => {
                return Err(Diagnostic::error(
                    "internal error: expected nominal ref ABI during nominal emission",
                    self.node_range(expr.source),
                ));
            }
        };
        let ty = Ty::from_id(salsa::Id::from_bits(u64::from(bits)));
        crate::capability::supported_internal_nominal_payload_layout_or_message(
            self.backend.db,
            ty,
            "Wasm backend does not support this nominal value type",
        )
        .map_err(|message| Diagnostic::error(message, self.node_range(expr.source)))
    }

    fn emit_local(
        &mut self,
        function: &mut WasmFunction,
        name: NameId,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let slot = self.layout.slots.get(&name).ok_or_else(|| {
            Diagnostic::error("internal error: missing local slot", self.node_range(source))
        })?;
        if let Some(index) = slot.local_index {
            function.instruction(&Instruction::LocalGet(index));
        }
        Ok(())
    }

    fn emit_capture_scalar(
        &mut self,
        function: &mut WasmFunction,
        field: &FieldLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let env_ptr_local = self.legalized_env_ptr_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: closure capture reached scalar emission without an env pointer",
                self.node_range(source),
            )
        })?;
        let AbiTy::Scalar(ty) = field.ty else {
            return Err(Diagnostic::error(
                "internal error: aggregate capture reached scalar emission",
                self.node_range(source),
            ));
        };

        function.instruction(&Instruction::LocalGet(env_ptr_local));
        if field.offset != 0 {
            function.instruction(&Instruction::I32Const(field.offset as i32));
            function.instruction(&Instruction::I32Add);
        }
        emit_scalar_load(function, ty, 0);
        Ok(())
    }

    fn emit_break(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let Some(targets) = self.loop_stack.last().copied() else {
            return Err(Diagnostic::error(
                "internal error: loop control reached the Wasm emitter outside a loop",
                self.node_range(source),
            ));
        };
        self.release_scopes_to(function, targets.scope_depth, source)?;
        let depth = self.loop_branch_depth(source, true)?;
        function.instruction(&Instruction::Br(depth));
        Ok(())
    }

    fn emit_continue(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let Some(targets) = self.loop_stack.last().copied() else {
            return Err(Diagnostic::error(
                "internal error: loop control reached the Wasm emitter outside a loop",
                self.node_range(source),
            ));
        };
        self.release_scopes_to(function, targets.scope_depth, source)?;
        let depth = self.loop_branch_depth(source, false)?;
        function.instruction(&Instruction::Br(depth));
        Ok(())
    }

    fn emit_call(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        target: &BackendCallTarget<'db>,
        args: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        let call_site = self.legalized_call_site(source).cloned();
        if let Some(site) = call_site.as_ref() {
            let LegalizedCallKind::Direct(legalized_target) = &site.kind else {
                return Err(Diagnostic::error(
                    "internal error: indirect call legalization reached direct emission",
                    self.node_range(source),
                ));
            };
            if legalized_target.signature != target.signature {
                return Err(Diagnostic::error(
                    "internal error: direct call legalization drifted from the call signature",
                    self.node_range(source),
                ));
            }
        }
        let result = call_site
            .as_ref()
            .map_or_else(|| result_passing_for_abi(&target.signature.result), |site| site.result);
        if matches!(result, LegalizedResultPassing::IndirectOutPtr) {
            return Err(Diagnostic::error(
                "internal error: aggregate call result reached the scalar Wasm emitter",
                self.node_range(source),
            ));
        }

        for (index, arg) in args.iter().enumerate() {
            let passing = call_site
                .as_ref()
                .and_then(|site| site.arg_passings.get(index))
                .copied()
                .unwrap_or_else(|| arg_passing_for_abi(&arg.abi));
            self.emit_legalized_call_arg(function, arg, passing)?;
        }

        function.instruction(&Instruction::Call(self.call_index(target, source)?));
        if matches!(result, LegalizedResultPassing::DirectScalar(BackendTy::Bool)) {
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Ne);
        }
        Ok(())
    }

    fn emit_binary(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        op: BackendBinaryOp,
        lhs: &FunctionKernelValue<'db>,
        rhs: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        if matches!(op, BackendBinaryOp::And | BackendBinaryOp::Or) {
            self.expr(function, lhs)?;
            function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
            self.control_depth += 1;
            if matches!(op, BackendBinaryOp::And) {
                self.expr(function, rhs)?;
            } else {
                function.instruction(&Instruction::I32Const(1));
            }
            function.instruction(&Instruction::Else);
            if matches!(op, BackendBinaryOp::And) {
                function.instruction(&Instruction::I32Const(0));
            } else {
                self.expr(function, rhs)?;
            }
            self.control_depth -= 1;
            function.instruction(&Instruction::End);
            return Ok(());
        }

        if matches!(op, BackendBinaryOp::Eq | BackendBinaryOp::Ne) {
            match (&lhs.abi, &rhs.abi) {
                (AbiTy::Scalar(BackendTy::Unit), AbiTy::Scalar(BackendTy::Unit)) => {
                    function.instruction(&Instruction::I32Const(
                        if matches!(op, BackendBinaryOp::Eq) { 1 } else { 0 },
                    ));
                    return Ok(());
                }
                (AbiTy::Scalar(BackendTy::Int), AbiTy::Scalar(BackendTy::Int))
                | (AbiTy::Scalar(BackendTy::I64), AbiTy::Scalar(BackendTy::I64))
                | (AbiTy::Scalar(BackendTy::Bool), AbiTy::Scalar(BackendTy::Bool))
                | (AbiTy::Scalar(BackendTy::Char), AbiTy::Scalar(BackendTy::Char)) => {
                    self.expr(function, lhs)?;
                    self.expr(function, rhs)?;
                    function.instruction(match (lhs.abi.clone(), op) {
                        (AbiTy::Scalar(BackendTy::I64), BackendBinaryOp::Eq) => &Instruction::I64Eq,
                        (AbiTy::Scalar(BackendTy::I64), BackendBinaryOp::Ne) => &Instruction::I64Ne,
                        (_, BackendBinaryOp::Eq) => &Instruction::I32Eq,
                        (_, BackendBinaryOp::Ne) => &Instruction::I32Ne,
                        _ => unreachable!("equality branch only"),
                    });
                    return Ok(());
                }
                (AbiTy::Scalar(BackendTy::Float), AbiTy::Scalar(BackendTy::Float)) => {
                    self.expr(function, lhs)?;
                    self.expr(function, rhs)?;
                    function.instruction(if matches!(op, BackendBinaryOp::Eq) {
                        &Instruction::F64Eq
                    } else {
                        &Instruction::F64Ne
                    });
                    return Ok(());
                }
                (
                    AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
                    AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
                ) => {
                    let helper = self.helper_index(HelperFunction::StringEq, source)?;
                    let lhs_local = self.scratch_i32_local(source)?;
                    let rhs_local = self.scratch_i32_aux_local(source)?;
                    self.expr(function, lhs)?;
                    function.instruction(&Instruction::LocalSet(lhs_local));
                    self.expr(function, rhs)?;
                    function.instruction(&Instruction::LocalSet(rhs_local));
                    function.instruction(&Instruction::LocalGet(lhs_local));
                    function.instruction(&Instruction::LocalGet(rhs_local));
                    function.instruction(&Instruction::Call(helper));
                    if lhs.ownership.is_owned() {
                        self.release_heap_ref_from_local(
                            function,
                            lhs_local,
                            BackendTy::Ref(RefKind::String),
                            source,
                        )?;
                    }
                    if rhs.ownership.is_owned() {
                        self.release_heap_ref_from_local(
                            function,
                            rhs_local,
                            BackendTy::Ref(RefKind::String),
                            source,
                        )?;
                    }
                    if matches!(op, BackendBinaryOp::Ne) {
                        function.instruction(&Instruction::I32Eqz);
                    }
                    return Ok(());
                }
                (
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(lhs_bits))),
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(rhs_bits))),
                ) if lhs_bits == rhs_bits => {
                    let helper = self.nominal_eq_index(*lhs_bits, source)?;
                    let lhs_local = self.scratch_i32_local(source)?;
                    let rhs_local = self.scratch_i32_aux_local(source)?;
                    self.expr(function, lhs)?;
                    function.instruction(&Instruction::LocalSet(lhs_local));
                    self.expr(function, rhs)?;
                    function.instruction(&Instruction::LocalSet(rhs_local));
                    function.instruction(&Instruction::LocalGet(lhs_local));
                    function.instruction(&Instruction::LocalGet(rhs_local));
                    function.instruction(&Instruction::Call(helper));
                    if lhs.ownership.is_owned() {
                        self.release_heap_ref_from_local(
                            function,
                            lhs_local,
                            self.expr_scalar_ty(lhs)?,
                            source,
                        )?;
                    }
                    if rhs.ownership.is_owned() {
                        self.release_heap_ref_from_local(
                            function,
                            rhs_local,
                            self.expr_scalar_ty(rhs)?,
                            source,
                        )?;
                    }
                    if matches!(op, BackendBinaryOp::Ne) {
                        function.instruction(&Instruction::I32Eqz);
                    }
                    return Ok(());
                }
                (
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Array(lhs_bits))),
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Array(rhs_bits))),
                ) if lhs_bits == rhs_bits => {
                    let helper = self.array_eq_index(*lhs_bits, source)?;
                    let lhs_local = self.scratch_i32_local(source)?;
                    let rhs_local = self.scratch_i32_aux_local(source)?;
                    self.expr(function, lhs)?;
                    function.instruction(&Instruction::LocalSet(lhs_local));
                    self.expr(function, rhs)?;
                    function.instruction(&Instruction::LocalSet(rhs_local));
                    function.instruction(&Instruction::LocalGet(lhs_local));
                    function.instruction(&Instruction::LocalGet(rhs_local));
                    function.instruction(&Instruction::Call(helper));
                    if lhs.ownership.is_owned() {
                        self.release_heap_ref_from_local(
                            function,
                            lhs_local,
                            self.expr_scalar_ty(lhs)?,
                            source,
                        )?;
                    }
                    if rhs.ownership.is_owned() {
                        self.release_heap_ref_from_local(
                            function,
                            rhs_local,
                            self.expr_scalar_ty(rhs)?,
                            source,
                        )?;
                    }
                    if matches!(op, BackendBinaryOp::Ne) {
                        function.instruction(&Instruction::I32Eqz);
                    }
                    return Ok(());
                }
                (AbiTy::Aggregate(lhs_layout), AbiTy::Aggregate(rhs_layout))
                    if lhs_layout == rhs_layout =>
                {
                    let lhs_local = self.scratch_i32_local(source)?;
                    let rhs_local = self.scratch_i32_aux_local(source)?;
                    self.emit_aggregate_addr(function, lhs)?;
                    function.instruction(&Instruction::LocalSet(lhs_local));
                    self.emit_aggregate_addr(function, rhs)?;
                    function.instruction(&Instruction::LocalSet(rhs_local));
                    self.emit_abi_equality(function, lhs_local, rhs_local, 0, &lhs.abi, source)?;
                    if lhs.ownership.is_owned() && lhs.abi.contains_heap_refs() {
                        self.release_aggregate_at_local(function, lhs_local, &lhs.abi, source)?;
                    }
                    if rhs.ownership.is_owned() && rhs.abi.contains_heap_refs() {
                        self.release_aggregate_at_local(function, rhs_local, &rhs.abi, source)?;
                    }
                    if matches!(op, BackendBinaryOp::Ne) {
                        function.instruction(&Instruction::I32Eqz);
                    }
                    return Ok(());
                }
                _ => {
                    return Err(Diagnostic::error(
                        "internal error: unsupported equality operator in staged backend IR",
                        self.node_range(source),
                    ));
                }
            }
        }

        let lhs_scalar = self.expr_scalar_ty(lhs)?;
        let rhs_scalar = self.expr_scalar_ty(rhs)?;
        self.expr(function, lhs)?;
        self.expr(function, rhs)?;

        match (lhs_scalar, rhs_scalar, op) {
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Add) => {
                function.instruction(&Instruction::I32Add);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Sub) => {
                function.instruction(&Instruction::I32Sub);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Mul) => {
                function.instruction(&Instruction::I32Mul);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Div) => {
                function.instruction(&Instruction::I32DivS);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Rem) => {
                function.instruction(&Instruction::I32RemS);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Lt) => {
                function.instruction(&Instruction::I32LtS);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Gt) => {
                function.instruction(&Instruction::I32GtS);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Le) => {
                function.instruction(&Instruction::I32LeS);
            }
            (BackendTy::Int, BackendTy::Int, BackendBinaryOp::Ge) => {
                function.instruction(&Instruction::I32GeS);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Add) => {
                function.instruction(&Instruction::F64Add);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Sub) => {
                function.instruction(&Instruction::F64Sub);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Mul) => {
                function.instruction(&Instruction::F64Mul);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Div) => {
                function.instruction(&Instruction::F64Div);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Lt) => {
                function.instruction(&Instruction::F64Lt);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Gt) => {
                function.instruction(&Instruction::F64Gt);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Le) => {
                function.instruction(&Instruction::F64Le);
            }
            (BackendTy::Float, BackendTy::Float, BackendBinaryOp::Ge) => {
                function.instruction(&Instruction::F64Ge);
            }
            (BackendTy::Char, BackendTy::Char, BackendBinaryOp::Lt) => {
                function.instruction(&Instruction::I32LtU);
            }
            (BackendTy::Char, BackendTy::Char, BackendBinaryOp::Gt) => {
                function.instruction(&Instruction::I32GtU);
            }
            (BackendTy::Char, BackendTy::Char, BackendBinaryOp::Le) => {
                function.instruction(&Instruction::I32LeU);
            }
            (BackendTy::Char, BackendTy::Char, BackendBinaryOp::Ge) => {
                function.instruction(&Instruction::I32GeU);
            }
            _ => {
                return Err(Diagnostic::error(
                    "internal error: unsupported binary operator in staged backend IR",
                    self.node_range(source),
                ));
            }
        }

        Ok(())
    }

    fn emit_abi_equality(
        &mut self,
        function: &mut WasmFunction,
        lhs_base_local: u32,
        rhs_base_local: u32,
        base_offset: u32,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match abi {
            AbiTy::Scalar(BackendTy::Int | BackendTy::Bool | BackendTy::Char) => {
                Self::emit_scalar_from_base(function, lhs_base_local, base_offset, BackendTy::Int)?;
                Self::emit_scalar_from_base(function, rhs_base_local, base_offset, BackendTy::Int)?;
                function.instruction(&Instruction::I32Eq);
            }
            AbiTy::Scalar(BackendTy::I64) => {
                Self::emit_scalar_from_base(function, lhs_base_local, base_offset, BackendTy::I64)?;
                Self::emit_scalar_from_base(function, rhs_base_local, base_offset, BackendTy::I64)?;
                function.instruction(&Instruction::I64Eq);
            }
            AbiTy::Scalar(BackendTy::Float) => {
                Self::emit_scalar_from_base(
                    function,
                    lhs_base_local,
                    base_offset,
                    BackendTy::Float,
                )?;
                Self::emit_scalar_from_base(
                    function,
                    rhs_base_local,
                    base_offset,
                    BackendTy::Float,
                )?;
                function.instruction(&Instruction::F64Eq);
            }
            AbiTy::Scalar(BackendTy::Unit) => {
                function.instruction(&Instruction::I32Const(1));
            }
            AbiTy::Scalar(BackendTy::Ref(RefKind::String)) => {
                let helper = self.helper_index(HelperFunction::StringEq, source)?;
                Self::emit_scalar_from_base(
                    function,
                    lhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::String),
                )?;
                Self::emit_scalar_from_base(
                    function,
                    rhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::String),
                )?;
                function.instruction(&Instruction::Call(helper));
            }
            AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(bits))) => {
                let helper = self.nominal_eq_index(*bits, source)?;
                Self::emit_scalar_from_base(
                    function,
                    lhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::Nominal(*bits)),
                )?;
                Self::emit_scalar_from_base(
                    function,
                    rhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::Nominal(*bits)),
                )?;
                function.instruction(&Instruction::Call(helper));
            }
            AbiTy::Scalar(BackendTy::Ref(RefKind::Array(bits))) => {
                let helper = self.array_eq_index(*bits, source)?;
                Self::emit_scalar_from_base(
                    function,
                    lhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::Array(*bits)),
                )?;
                Self::emit_scalar_from_base(
                    function,
                    rhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::Array(*bits)),
                )?;
                function.instruction(&Instruction::Call(helper));
            }
            AbiTy::Scalar(BackendTy::Ref(_)) => {
                Self::emit_scalar_from_base(
                    function,
                    lhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::Opaque),
                )?;
                Self::emit_scalar_from_base(
                    function,
                    rhs_base_local,
                    base_offset,
                    BackendTy::Ref(RefKind::Opaque),
                )?;
                function.instruction(&Instruction::I32Eq);
            }
            AbiTy::Aggregate(layout) if !abi.contains_heap_refs() => {
                let helper = self.helper_index(HelperFunction::MemoryEq, source)?;
                let size = i32::try_from(layout.size).map_err(|_overflow| {
                    Diagnostic::error(
                        "internal error: aggregate comparison exceeds the supported size",
                        self.node_range(source),
                    )
                })?;
                Self::emit_base_plus_offset(function, lhs_base_local, base_offset);
                Self::emit_base_plus_offset(function, rhs_base_local, base_offset);
                function.instruction(&Instruction::I32Const(size));
                function.instruction(&Instruction::Call(helper));
            }
            AbiTy::Aggregate(layout) => match &layout.kind {
                AggregateKind::Fields(fields) => {
                    function.instruction(&Instruction::I32Const(1));
                    for field in fields {
                        self.emit_abi_equality(
                            function,
                            lhs_base_local,
                            rhs_base_local,
                            base_offset + field.offset,
                            &field.ty,
                            source,
                        )?;
                        function.instruction(&Instruction::I32And);
                    }
                }
                AggregateKind::Enum(enum_layout) => {
                    Self::emit_base_plus_offset(function, lhs_base_local, base_offset);
                    MemAccess::enum_tag(0).emit_load(function);
                    Self::emit_base_plus_offset(function, rhs_base_local, base_offset);
                    MemAccess::enum_tag(0).emit_load(function);
                    function.instruction(&Instruction::I32Eq);
                    self.emit_enum_variant_equality(
                        function,
                        lhs_base_local,
                        rhs_base_local,
                        base_offset,
                        enum_layout,
                        source,
                    )?;
                    function.instruction(&Instruction::I32And);
                }
                AggregateKind::FunctionValue => {
                    function.instruction(&Instruction::I32Const(1));
                    for offset in [0, 4] {
                        Self::emit_base_plus_offset(function, lhs_base_local, base_offset + offset);
                        MemAccess::function_word(0).emit_load(function);
                        Self::emit_base_plus_offset(function, rhs_base_local, base_offset + offset);
                        MemAccess::function_word(0).emit_load(function);
                        function.instruction(&Instruction::I32Eq);
                        function.instruction(&Instruction::I32And);
                    }
                }
            },
        }
        Ok(())
    }

    fn emit_enum_variant_equality(
        &mut self,
        function: &mut WasmFunction,
        lhs_base_local: u32,
        rhs_base_local: u32,
        base_offset: u32,
        enum_layout: &EnumLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        for variant in enum_layout.variants.iter().rev() {
            Self::emit_base_plus_offset(function, lhs_base_local, base_offset);
            MemAccess::enum_tag(0).emit_load(function);
            function.instruction(&Instruction::I32Const(variant.tag));
            function.instruction(&Instruction::I32Eq);
            function.instruction(&Instruction::If(BlockType::Result(ValType::I32)));
            function.instruction(&Instruction::I32Const(1));
            for field in &variant.fields {
                self.emit_abi_equality(
                    function,
                    lhs_base_local,
                    rhs_base_local,
                    base_offset + field.offset,
                    &field.ty,
                    source,
                )?;
                function.instruction(&Instruction::I32And);
            }
            function.instruction(&Instruction::Else);
        }
        function.instruction(&Instruction::I32Const(0));
        for _ in 0..enum_layout.variants.len() {
            function.instruction(&Instruction::End);
        }
        Ok(())
    }

    fn emit_scalar_from_base(
        function: &mut WasmFunction,
        base_local: u32,
        offset: u32,
        ty: BackendTy,
    ) -> Result<(), Diagnostic> {
        Self::emit_base_plus_offset(function, base_local, offset);
        emit_scalar_load(function, ty, 0);
        Ok(())
    }

    fn emit_base_plus_offset(function: &mut WasmFunction, base_local: u32, offset: u32) {
        function.instruction(&Instruction::LocalGet(base_local));
        if offset != 0 {
            function.instruction(&Instruction::I32Const(offset as i32));
            function.instruction(&Instruction::I32Add);
        }
    }

    fn emit_array_data_addr(
        function: &mut WasmFunction,
        array_local: u32,
        layout: &ArrayRuntimeLayout,
    ) {
        function.instruction(&Instruction::LocalGet(array_local));
        if layout.data_offset != 0 {
            function.instruction(&Instruction::I32Const(layout.data_offset as i32));
            function.instruction(&Instruction::I32Add);
        }
    }

    fn emit_array_element_addr(
        function: &mut WasmFunction,
        array_local: u32,
        layout: &ArrayRuntimeLayout,
        index_local: u32,
    ) {
        Self::emit_array_data_addr(function, array_local, layout);
        if layout.item_stride != 0 {
            function.instruction(&Instruction::LocalGet(index_local));
            function.instruction(&Instruction::I32Const(layout.item_stride as i32));
            function.instruction(&Instruction::I32Mul);
            function.instruction(&Instruction::I32Add);
        }
    }

    fn emit_field(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        base: &FunctionKernelValue<'db>,
        field: &FieldLayout,
    ) -> Result<(), Diagnostic> {
        let AbiTy::Scalar(field_ty) = &field.ty else {
            return Err(Diagnostic::error(
                "internal error: aggregate field reached the scalar Wasm emitter",
                self.node_range(source),
            ));
        };

        let scratch = self.scratch_i32_local(source)?;
        self.emit_value_addr(function, base)?;
        function.instruction(&Instruction::LocalSet(scratch));
        function.instruction(&Instruction::LocalGet(scratch));
        if field.offset != 0 {
            function.instruction(&Instruction::I32Const(field.offset as i32));
            function.instruction(&Instruction::I32Add);
        }
        emit_scalar_load(function, *field_ty, 0);
        if base.ownership.is_owned() {
            if field_ty.is_heap_ref() {
                self.retain_heap_ref_on_stack(function, source)?;
            }
            self.emit_release_value_from_local(function, scratch, &base.abi, source)?;
        }
        Ok(())
    }

    fn emit_value_addr(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        if expr.abi.is_aggregate() {
            return self.emit_aggregate_addr(function, expr);
        }
        match expr.abi {
            AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(_))) => self.expr(function, expr),
            _ => Err(Diagnostic::error(
                "internal error: non-addressable value reached payload-address emission",
                self.node_range(expr.source),
            )),
        }
    }

    fn emit_prefix(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        op: BackendPrefixOp,
        inner: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        match op {
            BackendPrefixOp::Not => {
                self.expr(function, inner)?;
                function.instruction(&Instruction::I32Eqz);
            }
            BackendPrefixOp::Neg => match self.expr_scalar_ty(inner)? {
                BackendTy::Int => {
                    function.instruction(&Instruction::I32Const(0));
                    self.expr(function, inner)?;
                    function.instruction(&Instruction::I32Sub);
                }
                BackendTy::Float => {
                    function.instruction(&Instruction::F64Const(0.0.into()));
                    self.expr(function, inner)?;
                    function.instruction(&Instruction::F64Sub);
                }
                _ => {
                    return Err(Diagnostic::error(
                        "internal error: unsupported prefix operator in staged backend IR",
                        self.node_range(source),
                    ));
                }
            },
        }

        Ok(())
    }

    fn emit_legalized_call_arg(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
        passing: LegalizedArgPassing,
    ) -> Result<(), Diagnostic> {
        match passing {
            LegalizedArgPassing::Direct => {
                let AbiTy::Scalar(ty) = &expr.abi else {
                    return Err(Diagnostic::error(
                        "internal error: aggregate argument reached direct-call lowering",
                        self.node_range(expr.source),
                    ));
                };
                self.expr(function, expr)?;
                if expr.ownership.is_borrowed() && ty.is_heap_ref() {
                    self.retain_heap_ref_on_stack(function, expr.source)?;
                }
                Ok(())
            }
            LegalizedArgPassing::ByAddress => {
                if !expr.abi.is_aggregate() {
                    return Err(Diagnostic::error(
                        "internal error: scalar argument reached by-address lowering",
                        self.node_range(expr.source),
                    ));
                }
                self.emit_aggregate_addr(function, expr)?;
                if expr.ownership.is_borrowed() && expr.abi.contains_heap_refs() {
                    let scratch = self.scratch_i32_local(expr.source)?;
                    function.instruction(&Instruction::LocalTee(scratch));
                    self.retain_aggregate_at_local(function, scratch, &expr.abi, expr.source)?;
                }
                Ok(())
            }
        }
    }

    fn emit_aggregate_addr(
        &mut self,
        function: &mut WasmFunction,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        match &expr.kind {
            FunctionKernelValueKind::Local(name) => {
                self.emit_name_addr(function, *name, expr.source)
            }
            FunctionKernelValueKind::Field { base, field }
                if field.ty.is_aggregate() && !base.ownership.is_owned() =>
            {
                self.emit_value_addr(function, base)?;
                if field.offset != 0 {
                    function.instruction(&Instruction::I32Const(field.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
            FunctionKernelValueKind::Capture(field) if field.ty.is_aggregate() => {
                let env_ptr_local = self.legalized_env_ptr_local().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: closure capture reached aggregate emission without an \
                         env pointer",
                        self.node_range(expr.source),
                    )
                })?;
                function.instruction(&Instruction::LocalGet(env_ptr_local));
                if field.offset != 0 {
                    function.instruction(&Instruction::I32Const(field.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
            _ => {
                let dest = self.temp_dest(expr.source)?;
                self.emit_expr_into(function, dest, expr)?;
                self.emit_dest_addr(function, dest)
            }
        }
    }

    fn emit_expr_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        let layout = expr.abi.aggregate().ok_or_else(|| {
            Diagnostic::error(
                "internal error: scalar expression reached the aggregate Wasm emitter",
                self.node_range(expr.source),
            )
        })?;

        match &expr.kind {
            FunctionKernelValueKind::Field { base, field }
                if field.ty.is_aggregate() && base.ownership.is_owned() =>
            {
                self.emit_owned_aggregate_field_into(function, dest, expr, base, field, layout)
            }
            FunctionKernelValueKind::Local(_)
            | FunctionKernelValueKind::Field { .. }
            | FunctionKernelValueKind::Capture(_) => {
                self.copy_aggregate_expr_to_dest(function, dest, expr, layout.size)
            }
            FunctionKernelValueKind::Clone { value } => {
                self.emit_expr_into_owner(function, dest, value)
            }
            FunctionKernelValueKind::MemoryRead { addr, .. } => {
                self.emit_dest_addr(function, dest)?;
                self.expr(function, addr)?;
                function.instruction(&Instruction::I32Const(layout.size as i32));
                function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
                Ok(())
            }
            FunctionKernelValueKind::Tuple { fields } => {
                self.emit_tuple_into(function, dest, layout, fields)
            }
            FunctionKernelValueKind::Struct { fields } => {
                self.emit_struct_expr_into(function, dest, layout, fields)
            }
            FunctionKernelValueKind::Union { variant, value } => {
                self.emit_union_into(function, dest, layout, variant, value.as_deref())
            }
            FunctionKernelValueKind::VariantValue { variant } => {
                self.emit_variant_value_into(function, dest, layout, variant)
            }
            FunctionKernelValueKind::VariantCall { variant, args } => {
                self.emit_variant_call_into(function, dest, layout, variant, args)
            }
            FunctionKernelValueKind::Call { target, args } => {
                self.emit_call_into(function, dest, expr.source, target, args)
            }
            FunctionKernelValueKind::IndirectCall { callee, signature, args } => {
                self.emit_indirect_call_into(function, dest, expr.source, callee, signature, args)
            }
            FunctionKernelValueKind::FunctionValue { target } => {
                self.emit_function_value_into(function, dest, expr.source, target)
            }
            FunctionKernelValueKind::ClosureValue { target, env } => {
                self.emit_closure_value_into(function, dest, expr.source, target, env)
            }
            FunctionKernelValueKind::Block { .. }
            | FunctionKernelValueKind::If { .. }
            | FunctionKernelValueKind::Match { .. } => Err(Diagnostic::error(
                "internal error: structured control flow reached raw aggregate kernel emission",
                self.node_range(expr.source),
            )),
            kind => Err(Diagnostic::error(
                format!("internal error: unsupported IR node in aggregate Wasm emitter: {kind:?}"),
                self.node_range(expr.source),
            )),
        }
    }

    fn emit_structured_wasm_expr_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &StructuredWasmExpr<'db>,
    ) -> Result<(), Diagnostic> {
        let layout = expr.abi.aggregate().ok_or_else(|| {
            Diagnostic::error(
                "internal error: scalar stackified expression reached the aggregate Wasm emitter",
                self.node_range(expr.source),
            )
        })?;

        match &expr.kind {
            StructuredWasmExprKind::Leaf(backend) => self.emit_expr_into(function, dest, backend),
            StructuredWasmExprKind::Block { region, .. } => {
                self.emit_structured_wasm_region_into_owner(function, dest, region)
            }
            StructuredWasmExprKind::If { cond, then_region, else_region, .. } => {
                self.emit_structured_wasm_if_into(function, dest, cond, then_region, else_region)
            }
            StructuredWasmExprKind::Match { scrutinee, arms, fallback_unreachable, .. } => self
                .emit_structured_wasm_match_into(
                    function,
                    expr.source,
                    dest,
                    scrutinee,
                    arms,
                    *fallback_unreachable,
                ),
            StructuredWasmExprKind::Unreachable => {
                function.instruction(&Instruction::Unreachable);
                Ok(())
            }
            kind => Err(Diagnostic::error(
                format!(
                    "internal error: unsupported stackified node in aggregate Wasm emitter: \
                     {kind:?}"
                ),
                self.node_range(expr.source),
            )),
        }?;

        if layout.size == 0 {
            return Ok(());
        }
        Ok(())
    }

    fn emit_name_addr(
        &mut self,
        function: &mut WasmFunction,
        name: NameId,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let slot = self.layout.slots.get(&name).ok_or_else(|| {
            Diagnostic::error("internal error: missing local slot", self.node_range(source))
        })?;
        if !slot.abi.is_aggregate() && !self.source_map.is_mutable_binding(name) {
            return Err(Diagnostic::error(
                "internal error: scalar name reached the aggregate address emitter",
                self.node_range(source),
            ));
        }
        if let Some(local_index) = slot.local_index {
            function.instruction(&Instruction::LocalGet(local_index));
            return Ok(());
        }
        self.emit_dest_addr(function, self.dest_for_slot(slot)?)
    }

    fn copy_aggregate_expr_to_dest(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &FunctionKernelValue<'db>,
        size: u32,
    ) -> Result<(), Diagnostic> {
        self.emit_dest_addr(function, dest)?;
        self.emit_aggregate_addr(function, expr)?;
        function.instruction(&Instruction::I32Const(size as i32));
        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
        Ok(())
    }

    fn copy_dest_to_dest(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        src: Dest,
        size: u32,
    ) -> Result<(), Diagnostic> {
        self.emit_dest_addr(function, dest)?;
        self.emit_dest_addr(function, src)?;
        function.instruction(&Instruction::I32Const(size as i32));
        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });
        Ok(())
    }

    fn emit_owned_aggregate_field_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &FunctionKernelValue<'db>,
        base: &FunctionKernelValue<'db>,
        field: &FieldLayout,
        layout: &AggregateLayout,
    ) -> Result<(), Diagnostic> {
        let scratch = self.scratch_i32_local(expr.source)?;
        self.emit_value_addr(function, base)?;
        function.instruction(&Instruction::LocalSet(scratch));

        self.emit_dest_addr(function, dest)?;
        function.instruction(&Instruction::LocalGet(scratch));
        if field.offset != 0 {
            function.instruction(&Instruction::I32Const(field.offset as i32));
            function.instruction(&Instruction::I32Add);
        }
        function.instruction(&Instruction::I32Const(layout.size as i32));
        function.instruction(&Instruction::MemoryCopy { src_mem: 0, dst_mem: 0 });

        if expr.abi.contains_heap_refs() {
            self.retain_aggregate_at_dest(function, dest, &expr.abi, expr.source)?;
        }
        self.emit_release_value_from_local(function, scratch, &base.abi, expr.source)
    }

    fn emit_tuple_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        layout: &AggregateLayout,
        fields: &[FunctionKernelFieldValue<'db>],
    ) -> Result<(), Diagnostic> {
        self.zero_dest(function, dest, layout.size)?;
        for field in fields {
            self.store_expr_to_dest(
                function,
                dest.with_offset(field.field.offset),
                &field.value,
                &field.field.ty,
            )?;
        }
        Ok(())
    }

    fn emit_struct_expr_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        layout: &AggregateLayout,
        fields: &[FunctionKernelFieldValue<'db>],
    ) -> Result<(), Diagnostic> {
        self.zero_dest(function, dest, layout.size)?;
        for field in fields {
            self.store_expr_to_dest(
                function,
                dest.with_offset(field.field.offset),
                &field.value,
                &field.field.ty,
            )?;
        }
        Ok(())
    }

    fn emit_union_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        layout: &AggregateLayout,
        variant: &VariantLayout,
        value: Option<&FunctionKernelValue<'db>>,
    ) -> Result<(), Diagnostic> {
        self.zero_dest(function, dest, layout.size)?;
        self.emit_dest_addr(function, dest)?;
        function.instruction(&Instruction::I32Const(variant.tag));
        MemAccess::enum_tag(0).emit_store(function);
        if let (Some(field), Some(value)) = (variant.fields.first(), value) {
            self.store_expr_to_dest(function, dest.with_offset(field.offset), value, &field.ty)?;
        }
        Ok(())
    }

    fn emit_variant_value_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        layout: &AggregateLayout,
        variant: &VariantLayout,
    ) -> Result<(), Diagnostic> {
        self.zero_dest(function, dest, layout.size)?;
        self.emit_dest_addr(function, dest)?;
        function.instruction(&Instruction::I32Const(variant.tag));
        MemAccess::enum_tag(0).emit_store(function);
        Ok(())
    }

    fn emit_variant_call_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        layout: &AggregateLayout,
        variant: &VariantLayout,
        args: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        self.zero_dest(function, dest, layout.size)?;

        self.emit_dest_addr(function, dest)?;
        function.instruction(&Instruction::I32Const(variant.tag));
        MemAccess::enum_tag(0).emit_store(function);

        for (field, arg) in variant.fields.iter().zip(args.iter()) {
            self.store_expr_to_dest(function, dest.with_offset(field.offset), arg, &field.ty)?;
        }

        Ok(())
    }

    fn emit_call_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        source: ExprId,
        target: &BackendCallTarget<'db>,
        args: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        let call_site = self.legalized_call_site(source).cloned();
        if let Some(site) = call_site.as_ref() {
            let LegalizedCallKind::Direct(legalized_target) = &site.kind else {
                return Err(Diagnostic::error(
                    "internal error: indirect call legalization reached direct aggregate emission",
                    self.node_range(source),
                ));
            };
            if legalized_target.signature != target.signature {
                return Err(Diagnostic::error(
                    "internal error: direct aggregate call legalization drifted from the call \
                     signature",
                    self.node_range(source),
                ));
            }
        }
        let result = call_site
            .as_ref()
            .map_or_else(|| result_passing_for_abi(&target.signature.result), |site| site.result);
        if !matches!(result, LegalizedResultPassing::IndirectOutPtr) {
            return Err(Diagnostic::error(
                "internal error: scalar call result reached the aggregate Wasm emitter",
                self.node_range(source),
            ));
        }

        self.emit_dest_addr(function, dest)?;
        for (index, arg) in args.iter().enumerate() {
            let passing = call_site
                .as_ref()
                .and_then(|site| site.arg_passings.get(index))
                .copied()
                .unwrap_or_else(|| arg_passing_for_abi(&arg.abi));
            self.emit_legalized_call_arg(function, arg, passing)?;
        }
        function.instruction(&Instruction::Call(self.call_index(target, source)?));
        Ok(())
    }

    fn emit_indirect_call(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
        callee: &FunctionKernelValue<'db>,
        signature: &FunctionSignature,
        args: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        let call_site = self.legalized_call_site(source).cloned();
        let (indirect_signature, callable_representation, result) = match call_site.as_ref() {
            Some(site) => {
                let LegalizedCallKind::Indirect(indirect_signature) = &site.kind else {
                    return Err(Diagnostic::error(
                        "internal error: direct call legalization reached indirect emission",
                        self.node_range(source),
                    ));
                };
                (indirect_signature, site.callable_representation, site.result)
            }
            None => (
                signature,
                self.legalized_callable_representation(),
                result_passing_for_abi(&signature.result),
            ),
        };
        if matches!(result, LegalizedResultPassing::IndirectOutPtr) {
            return Err(Diagnostic::error(
                "internal error: aggregate indirect call result reached the scalar Wasm emitter",
                self.node_range(source),
            ));
        }

        let scratch = self.scratch_i32_local(source)?;
        self.emit_aggregate_addr(function, callee)?;
        function.instruction(&Instruction::LocalSet(scratch));

        Self::emit_callable_env_arg(function, scratch, callable_representation);
        for (index, arg) in args.iter().enumerate() {
            let passing = call_site
                .as_ref()
                .and_then(|site| site.arg_passings.get(index))
                .copied()
                .unwrap_or_else(|| arg_passing_for_abi(&arg.abi));
            self.emit_legalized_call_arg(function, arg, passing)?;
        }
        Self::emit_callable_table_index(function, scratch, callable_representation);
        function.instruction(&Instruction::CallIndirect {
            type_index: self.callable_type_index(indirect_signature, source)?,
            table_index: 0,
        });
        if callee.ownership.is_owned() && callee.abi.contains_heap_refs() {
            self.release_aggregate_at_local(function, scratch, &callee.abi, source)?;
        }
        if matches!(result, LegalizedResultPassing::DirectScalar(BackendTy::Bool)) {
            function.instruction(&Instruction::I32Const(0));
            function.instruction(&Instruction::I32Ne);
        }
        Ok(())
    }

    fn emit_indirect_call_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        source: ExprId,
        callee: &FunctionKernelValue<'db>,
        signature: &FunctionSignature,
        args: &[FunctionKernelValue<'db>],
    ) -> Result<(), Diagnostic> {
        let call_site = self.legalized_call_site(source).cloned();
        let (indirect_signature, callable_representation, result) = match call_site.as_ref() {
            Some(site) => {
                let LegalizedCallKind::Indirect(indirect_signature) = &site.kind else {
                    return Err(Diagnostic::error(
                        "internal error: direct call legalization reached indirect aggregate \
                         emission",
                        self.node_range(source),
                    ));
                };
                (indirect_signature, site.callable_representation, site.result)
            }
            None => (
                signature,
                self.legalized_callable_representation(),
                result_passing_for_abi(&signature.result),
            ),
        };
        if !matches!(result, LegalizedResultPassing::IndirectOutPtr) {
            return Err(Diagnostic::error(
                "internal error: scalar indirect call result reached the aggregate Wasm emitter",
                self.node_range(source),
            ));
        }

        let scratch = self.scratch_i32_local(source)?;
        self.emit_aggregate_addr(function, callee)?;
        function.instruction(&Instruction::LocalSet(scratch));

        Self::emit_callable_env_arg(function, scratch, callable_representation);
        self.emit_dest_addr(function, dest)?;
        for (index, arg) in args.iter().enumerate() {
            let passing = call_site
                .as_ref()
                .and_then(|site| site.arg_passings.get(index))
                .copied()
                .unwrap_or_else(|| arg_passing_for_abi(&arg.abi));
            self.emit_legalized_call_arg(function, arg, passing)?;
        }
        Self::emit_callable_table_index(function, scratch, callable_representation);
        function.instruction(&Instruction::CallIndirect {
            type_index: self.callable_type_index(indirect_signature, source)?,
            table_index: 0,
        });
        if callee.ownership.is_owned() && callee.abi.contains_heap_refs() {
            self.release_aggregate_at_local(function, scratch, &callee.abi, source)?;
        }
        Ok(())
    }

    fn emit_function_value_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        source: ExprId,
        target: &FunctionValueTarget<'db>,
    ) -> Result<(), Diagnostic> {
        match self.legalized_callable_representation() {
            LegalizedCallableRepresentation::HandleAndTable => self.emit_function_value_pair(
                function,
                dest,
                self.table_slot(target, source)?,
                None,
            ),
        }
    }

    fn emit_closure_value_into(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        source: ExprId,
        target: &ClosureInstanceKey<'db>,
        env: &FunctionKernelClosureEnvInit<'db>,
    ) -> Result<(), Diagnostic> {
        if !matches!(
            self.legalized_callable_representation(),
            LegalizedCallableRepresentation::HandleAndTable
        ) {
            return Err(Diagnostic::error(
                "internal error: unsupported callable representation for closure value emission",
                self.node_range(source),
            ));
        }
        let scratch = self.scratch_i32_local(source)?;
        if env.layout.size == 0 {
            return self.emit_function_value_pair(
                function,
                dest,
                self.table_slot(&FunctionValueTarget::Closure(target.clone()), source)?,
                None,
            );
        }

        let alloc = self.runtime_index(RuntimeFunction::Alloc, source)?;
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + env.layout.size) as i32));
        function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
        function.instruction(&Instruction::Call(alloc));
        function.instruction(&Instruction::LocalTee(scratch));
        function.instruction(&Instruction::I32Const(1));
        MemAccess::arc_ref_count().emit_store(function);
        function.instruction(&Instruction::LocalGet(scratch));
        function.instruction(&Instruction::I32Const(0));
        MemAccess::arc_type_bits().emit_store(function);
        function.instruction(&Instruction::LocalGet(scratch));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::LocalSet(scratch));

        self.zero_dest(function, Dest::pointer_local(scratch), env.layout.size)?;
        for field in &env.fields {
            self.store_expr_to_dest(
                function,
                Dest::pointer_local(scratch).with_offset(field.field.offset),
                &field.value,
                &field.field.ty,
            )?;
        }

        self.emit_function_value_pair(
            function,
            dest,
            self.table_slot(&FunctionValueTarget::Closure(target.clone()), source)?,
            Some(scratch),
        )
    }

    fn emit_function_value_pair(
        &self,
        function: &mut WasmFunction,
        dest: Dest,
        table_slot: u32,
        env_ptr_local: Option<u32>,
    ) -> Result<(), Diagnostic> {
        self.emit_dest_addr(function, dest)?;
        function.instruction(&Instruction::I32Const(table_slot as i32));
        MemAccess::function_word(0).emit_store(function);
        self.emit_dest_addr(function, dest.with_offset(4))?;
        match env_ptr_local {
            Some(local) => {
                function.instruction(&Instruction::LocalGet(local));
            }
            None => {
                function.instruction(&Instruction::I32Const(0));
            }
        };
        MemAccess::function_word(0).emit_store(function);
        Ok(())
    }

    fn emit_callable_env_arg(
        function: &mut WasmFunction,
        callable_local: u32,
        callable_representation: LegalizedCallableRepresentation,
    ) {
        match callable_representation {
            LegalizedCallableRepresentation::HandleAndTable => {
                function.instruction(&Instruction::LocalGet(callable_local));
                MemAccess::function_word(4).emit_load(function);
            }
        }
    }

    fn emit_callable_table_index(
        function: &mut WasmFunction,
        callable_local: u32,
        callable_representation: LegalizedCallableRepresentation,
    ) {
        match callable_representation {
            LegalizedCallableRepresentation::HandleAndTable => {
                function.instruction(&Instruction::LocalGet(callable_local));
                MemAccess::function_word(0).emit_load(function);
            }
        }
    }

    fn expr_scalar_ty(&self, expr: &FunctionKernelValue<'db>) -> Result<BackendTy, Diagnostic> {
        match expr.abi {
            AbiTy::Scalar(ty) => Ok(ty),
            AbiTy::Aggregate(_) => Err(Diagnostic::error(
                "internal error: aggregate expression reached a scalar type query",
                self.node_range(expr.source),
            )),
        }
    }

    fn store_expr_to_dest(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &FunctionKernelValue<'db>,
        _ty: &AbiTy,
    ) -> Result<(), Diagnostic> {
        self.emit_expr_into_owner(function, dest, expr)
    }

    fn emit_dest_addr(&self, function: &mut WasmFunction, dest: Dest) -> Result<(), Diagnostic> {
        match dest.base {
            DestBase::PointerLocal(local) => {
                function.instruction(&Instruction::LocalGet(local));
                if dest.offset != 0 {
                    function.instruction(&Instruction::I32Const(dest.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
            DestBase::FrameSlot(frame_slot) => {
                let frame_base_local = self.layout.frame_base_local().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: frame-backed address requested without a frame base",
                        self.backend.function_range(self.location),
                    )
                })?;
                function.instruction(&Instruction::LocalGet(frame_base_local));
                let frame_offset = self.layout.frame_slot_offset(frame_slot).ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: frame-backed address referenced a missing frame slot",
                        self.backend.function_range(self.location),
                    )
                })?;
                let total = frame_offset + dest.offset;
                if total != 0 {
                    function.instruction(&Instruction::I32Const(total as i32));
                    function.instruction(&Instruction::I32Add);
                }
                Ok(())
            }
        }
    }

    fn zero_dest(
        &self,
        function: &mut WasmFunction,
        dest: Dest,
        size: u32,
    ) -> Result<(), Diagnostic> {
        if size == 0 {
            return Ok(());
        }

        self.emit_dest_addr(function, dest)?;
        function.instruction(&Instruction::I32Const(0));
        function.instruction(&Instruction::I32Const(size as i32));
        function.instruction(&Instruction::MemoryFill(0));
        Ok(())
    }

    fn dest_for_slot(&self, slot: &LocalSlot) -> Result<Dest, Diagnostic> {
        if let Some(local_index) = slot.local_index {
            return Ok(Dest::pointer_local(local_index));
        }
        if let Some(frame_slot) = slot.frame_slot {
            return Ok(Dest::frame_slot(frame_slot));
        }
        Err(Diagnostic::error(
            "internal error: aggregate slot is missing storage",
            self.backend.function_range(self.location),
        ))
    }

    fn temp_dest(&self, expr: ExprId) -> Result<Dest, Diagnostic> {
        let temp = self.layout.temps.get(&expr).ok_or_else(|| {
            Diagnostic::error(
                "internal error: aggregate expression is missing a temporary slot",
                self.node_range(expr),
            )
        })?;
        Ok(Dest::frame_slot(temp.frame_slot))
    }

    fn pattern_source_local(&self, expr: ExprId) -> Result<u32, Diagnostic> {
        self.layout.pattern_scalar_locals.get(&expr).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: scalar pattern source is missing a temporary local",
                self.node_range(expr),
            )
        })
    }

    fn call_index(
        &self,
        target: &BackendCallTarget<'db>,
        source: ExprId,
    ) -> Result<u32, Diagnostic> {
        if let Some(index) = self
            .resolved_wasm_refs
            .and_then(|resolved| resolved.direct_calls.get(&source))
            .map(|resolved| resolved.function_index)
        {
            return Ok(index);
        }
        match &target.callable {
            BackendCallable::Runtime(function) => {
                self.runtime_indices.get(function).copied().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing function index for runtime call target",
                        self.node_range(source),
                    )
                })
            }
            BackendCallable::StageIntrinsic(intrinsic) => {
                self.stage_indices.get(intrinsic).copied().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing function index for stage intrinsic call target",
                        self.node_range(source),
                    )
                })
            }
            BackendCallable::Function(instance) => {
                self.function_indices.get(instance).copied().ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: missing function index for lowered call target",
                        self.node_range(source),
                    )
                })
            }
        }
    }

    fn runtime_index(&self, runtime: RuntimeFunction, source: ExprId) -> Result<u32, Diagnostic> {
        self.runtime_indices.get(&runtime).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing runtime function index",
                self.node_range(source),
            )
        })
    }

    fn callable_type_index(
        &self,
        signature: &FunctionSignature,
        source: ExprId,
    ) -> Result<u32, Diagnostic> {
        if let Some(index) = self
            .resolved_wasm_refs
            .and_then(|resolved| resolved.indirect_calls.get(&source))
            .map(|resolved| resolved.type_index)
        {
            return Ok(index);
        }
        self.callable_type_indices.get(signature).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing callable type index for indirect call",
                self.node_range(source),
            )
        })
    }

    fn table_slot(
        &self,
        target: &FunctionValueTarget<'db>,
        source: ExprId,
    ) -> Result<u32, Diagnostic> {
        if let Some(slot) = self
            .resolved_wasm_refs
            .and_then(|resolved| resolved.callable_values.get(&source))
            .map(|resolved| resolved.table_slot)
        {
            return Ok(slot);
        }
        self.table_slots.get(target).copied().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing table slot for function value",
                self.node_range(source),
            )
        })
    }

    fn scratch_i32_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout.scratch_i32_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing scratch local for indirect call or closure emission",
                self.node_range(source),
            )
        })
    }

    fn result_i32_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout.object_i32_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing i32 result local for ARC-sensitive return emission",
                self.node_range(source),
            )
        })
    }

    fn object_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout.object_i32_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing object scratch local for heap emission",
                self.node_range(source),
            )
        })
    }

    fn scratch_i32_aux_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout.scratch_i32_aux_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing auxiliary scratch local for ARC emission",
                self.node_range(source),
            )
        })
    }

    fn scratch_f64_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout.scratch_f64_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing float scratch local for ARC emission",
                self.node_range(source),
            )
        })
    }

    fn scratch_i64_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout.scratch_i64_local().ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing i64 scratch local for raw Wasm emission",
                self.node_range(source),
            )
        })
    }

    fn nominal_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout
            .nominal_locals
            .get(&source)
            .copied()
            .or(self.layout.object_i32_local())
            .ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing nominal payload local for object emission",
                    self.node_range(source),
                )
            })
    }

    fn array_repeat_local(&self, source: ExprId) -> Result<u32, Diagnostic> {
        self.layout
            .array_repeat_locals
            .get(&source)
            .copied()
            .or(self.layout.object_i32_local())
            .ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing repeat-array local for Wasm emission",
                    self.node_range(source),
                )
            })
    }

    fn helper_index(&self, helper: HelperFunction, source: ExprId) -> Result<u32, Diagnostic> {
        self.helper_indices.get(&helper).copied().ok_or_else(|| {
            Diagnostic::error(
                format!("internal error: missing function index for `{}` helper", helper.name()),
                self.node_range(source),
            )
        })
    }

    fn nominal_destroy_index(&self, bits: u32, source: ExprId) -> Result<u32, Diagnostic> {
        self.nominal_destroyers.get(&bits).copied().ok_or_else(|| {
            let ty = Ty::from_id(salsa::Id::from_bits(u64::from(bits)));
            Diagnostic::error(
                format!(
                    "internal error: missing nominal destroyer for `{}`",
                    ty.display(self.backend.db)
                ),
                self.node_range(source),
            )
        })
    }

    fn nominal_eq_index(&self, bits: u32, source: ExprId) -> Result<u32, Diagnostic> {
        self.nominal_eq_helpers.get(&bits).copied().ok_or_else(|| {
            let ty = Ty::from_id(salsa::Id::from_bits(u64::from(bits)));
            Diagnostic::error(
                format!(
                    "internal error: missing nominal equality helper for `{}`",
                    ty.display(self.backend.db)
                ),
                self.node_range(source),
            )
        })
    }

    fn array_destroy_index(&self, bits: u32, source: ExprId) -> Result<u32, Diagnostic> {
        self.array_destroyers.get(&bits).copied().ok_or_else(|| {
            let ty = Ty::from_id(salsa::Id::from_bits(u64::from(bits)));
            Diagnostic::error(
                format!(
                    "internal error: missing array destroyer for `{}`",
                    ty.display(self.backend.db)
                ),
                self.node_range(source),
            )
        })
    }

    fn array_eq_index(&self, bits: u32, source: ExprId) -> Result<u32, Diagnostic> {
        self.array_eq_helpers.get(&bits).copied().ok_or_else(|| {
            let ty = Ty::from_id(salsa::Id::from_bits(u64::from(bits)));
            Diagnostic::error(
                format!(
                    "internal error: missing array equality helper for `{}`",
                    ty.display(self.backend.db)
                ),
                self.node_range(source),
            )
        })
    }

    fn loop_branch_depth(&self, source: ExprId, break_branch: bool) -> Result<u32, Diagnostic> {
        let Some(targets) = self.loop_stack.last().copied() else {
            return Err(Diagnostic::error(
                "internal error: loop control reached the Wasm emitter outside a loop",
                self.node_range(source),
            ));
        };

        let target = if break_branch { targets.break_target } else { targets.continue_target };
        self.control_depth.checked_sub(target + 1).ok_or_else(|| {
            Diagnostic::error("internal error: invalid loop branch depth", self.node_range(source))
        })
    }

    fn return_branch_depth(&self, source: ExprId) -> Result<u32, Diagnostic> {
        let Some(target) = self.return_target else {
            return Err(Diagnostic::error(
                "internal error: return reached the Wasm emitter outside a function body",
                self.node_range(source),
            ));
        };

        self.control_depth.checked_sub(target + 1).ok_or_else(|| {
            Diagnostic::error(
                "internal error: invalid return branch depth",
                self.node_range(source),
            )
        })
    }

    fn push_scope(&mut self) {
        self.scope_stack.push(ScopeFrame::default());
    }

    fn register_param_locals(&mut self) -> Result<(), Diagnostic> {
        let param_names = self.layout.param_names.clone();
        for name in param_names {
            let slot = self.layout.slots.get(&name).ok_or_else(|| {
                Diagnostic::error(
                    "internal error: missing parameter slot during ARC registration",
                    self.backend.function_range(self.location),
                )
            })?;
            self.register_scope_local(name, &slot.abi);
        }
        Ok(())
    }

    fn register_scope_local(&mut self, name: NameId, abi: &AbiTy) {
        if !abi.contains_heap_refs() {
            return;
        }
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.locals.push(ScopeLocal { name, abi: abi.clone() });
        }
    }

    fn release_scope(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let Some(scope) = self.scope_stack.pop() else {
            return Ok(());
        };
        self.release_scope_frame(function, &scope, source)
    }

    fn release_scopes_to(
        &mut self,
        function: &mut WasmFunction,
        scope_depth: usize,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        while self.scope_stack.len() > scope_depth {
            self.release_scope(function, source)?;
        }
        Ok(())
    }

    fn release_scope_frame(
        &mut self,
        function: &mut WasmFunction,
        scope: &ScopeFrame,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        for local in scope.locals.iter().rev() {
            self.release_local(function, local, source)?;
        }
        Ok(())
    }

    fn release_local(
        &mut self,
        function: &mut WasmFunction,
        local: &ScopeLocal,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let slot = self.layout.slots.get(&local.name).ok_or_else(|| {
            Diagnostic::error(
                "internal error: missing local slot during ARC release",
                self.node_range(source),
            )
        })?;
        match &local.abi {
            AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                let index = slot.local_index.ok_or_else(|| {
                    Diagnostic::error(
                        "internal error: scalar heap local is missing a Wasm local index",
                        self.node_range(source),
                    )
                })?;
                function.instruction(&Instruction::LocalGet(index));
                self.release_heap_ref_from_stack(function, *ty, source)
            }
            AbiTy::Aggregate(_) => self.release_aggregate_at_dest(
                function,
                self.dest_for_slot(slot)?,
                &local.abi,
                source,
            ),
            _ => Ok(()),
        }
    }

    fn retain_heap_ref_on_stack(
        &mut self,
        function: &mut WasmFunction,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let scratch = self.scratch_i32_aux_local(source)?;
        let helper = self.helper_index(HelperFunction::ArcRetain, source)?;
        function.instruction(&Instruction::LocalTee(scratch));
        function.instruction(&Instruction::LocalGet(scratch));
        function.instruction(&Instruction::Call(helper));
        Ok(())
    }

    fn release_heap_ref_from_stack(
        &mut self,
        function: &mut WasmFunction,
        ty: BackendTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let scratch = self.scratch_i32_local(source)?;
        function.instruction(&Instruction::LocalSet(scratch));
        self.release_heap_ref_from_local(function, scratch, ty, source)
    }

    fn release_heap_ref_from_local(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        ty: BackendTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        if !ty.is_heap_ref() {
            return Ok(());
        }

        let helper = self.helper_index(HelperFunction::ArcRelease, source)?;
        function.instruction(&Instruction::LocalGet(local));
        function.instruction(&Instruction::Call(helper));
        function.instruction(&Instruction::If(BlockType::Empty));
        match ty {
            BackendTy::Ref(RefKind::String) => {
                self.dealloc_string_from_local(function, local, source)?
            }
            BackendTy::Ref(RefKind::Array(bits)) => {
                function.instruction(&Instruction::LocalGet(local));
                function.instruction(&Instruction::Call(self.array_destroy_index(bits, source)?));
            }
            BackendTy::Ref(RefKind::Nominal(bits)) => {
                function.instruction(&Instruction::LocalGet(local));
                function.instruction(&Instruction::Call(self.nominal_destroy_index(bits, source)?));
            }
            BackendTy::Ref(RefKind::Opaque) => {}
            _ => {}
        }
        function.instruction(&Instruction::End);
        Ok(())
    }

    fn dealloc_string_from_local(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let base = self.scratch_i32_other_local(source, local)?;
        let dealloc = self.runtime_index(RuntimeFunction::Dealloc, source)?;

        function.instruction(&Instruction::LocalGet(local));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Sub);
        function.instruction(&Instruction::LocalSet(base));

        function.instruction(&Instruction::LocalGet(base));
        function.instruction(&Instruction::LocalGet(local));
        MemAccess::arc_ref_count().emit_load(function);
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + 4) as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
        function.instruction(&Instruction::Call(dealloc));
        Ok(())
    }

    fn dealloc_array_from_local(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        layout: &ArrayRuntimeLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let base = self.scratch_i32_other_local(source, local)?;
        let dealloc = self.runtime_index(RuntimeFunction::Dealloc, source)?;

        function.instruction(&Instruction::LocalGet(local));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Sub);
        function.instruction(&Instruction::LocalSet(base));

        function.instruction(&Instruction::LocalGet(base));
        function.instruction(&Instruction::LocalGet(local));
        MemAccess::array_capacity().emit_load(function);
        function.instruction(&Instruction::I32Const(layout.item_stride as i32));
        function.instruction(&Instruction::I32Mul);
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + layout.data_offset) as i32));
        function.instruction(&Instruction::I32Add);
        function.instruction(&Instruction::I32Const(layout.object_align() as i32));
        function.instruction(&Instruction::Call(dealloc));
        Ok(())
    }

    fn dealloc_env_from_local(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        payload_size: u32,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let base = self.scratch_i32_other_local(source, local)?;
        let dealloc = self.runtime_index(RuntimeFunction::Dealloc, source)?;

        function.instruction(&Instruction::LocalGet(local));
        function.instruction(&Instruction::I32Const(ARC_HEADER_SIZE as i32));
        function.instruction(&Instruction::I32Sub);
        function.instruction(&Instruction::LocalSet(base));
        function.instruction(&Instruction::LocalGet(base));
        function.instruction(&Instruction::I32Const((ARC_HEADER_SIZE + payload_size) as i32));
        function.instruction(&Instruction::I32Const(ARC_ALIGN as i32));
        function.instruction(&Instruction::Call(dealloc));
        Ok(())
    }

    fn release_aggregate_at_dest(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        self.emit_dest_addr(function, dest)?;
        let scratch = self.scratch_i32_local(source)?;
        function.instruction(&Instruction::LocalSet(scratch));
        self.release_aggregate_at_local(function, scratch, abi, source)
    }

    fn release_aggregate_at_local(
        &mut self,
        function: &mut WasmFunction,
        base_local: u32,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let AbiTy::Aggregate(layout) = abi else {
            return Ok(());
        };

        if layout.is_function_value() {
            return self.release_function_value_at_local(function, base_local, source);
        }

        match &layout.kind {
            AggregateKind::Fields(fields) => {
                for field in fields {
                    self.release_field_from_local(function, base_local, field, source)?;
                }
            }
            AggregateKind::Enum(enum_layout) => {
                let tag_local = self.scratch_i32_other_local(source, base_local)?;
                function.instruction(&Instruction::LocalGet(base_local));
                MemAccess::enum_tag(0).emit_load(function);
                function.instruction(&Instruction::LocalSet(tag_local));

                for variant in &enum_layout.variants {
                    function.instruction(&Instruction::LocalGet(tag_local));
                    function.instruction(&Instruction::I32Const(variant.tag));
                    function.instruction(&Instruction::I32Eq);
                    function.instruction(&Instruction::If(BlockType::Empty));
                    for field in &variant.fields {
                        self.release_field_from_local(function, base_local, field, source)?;
                    }
                    function.instruction(&Instruction::End);
                }
            }
            AggregateKind::FunctionValue => unreachable!("function values are handled above"),
        }

        Ok(())
    }

    fn release_field_from_local(
        &mut self,
        function: &mut WasmFunction,
        base_local: u32,
        field: &FieldLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match &field.ty {
            AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                let scratch = self.scratch_i32_other_local(source, base_local)?;
                function.instruction(&Instruction::LocalGet(base_local));
                if field.offset != 0 {
                    function.instruction(&Instruction::I32Const(field.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                emit_scalar_load(function, *ty, 0);
                function.instruction(&Instruction::LocalSet(scratch));
                match ty {
                    BackendTy::Ref(RefKind::String | RefKind::Array(_) | RefKind::Nominal(_)) => {
                        self.release_heap_ref_from_local(function, scratch, *ty, source)?
                    }
                    BackendTy::Ref(RefKind::Opaque) => {}
                    _ => {}
                }
            }
            AbiTy::Aggregate(_) if field.ty.contains_heap_refs() => {
                let nested = self.scratch_i32_other_local(source, base_local)?;
                function.instruction(&Instruction::LocalGet(base_local));
                if field.offset != 0 {
                    function.instruction(&Instruction::I32Const(field.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                function.instruction(&Instruction::LocalSet(nested));
                self.release_aggregate_at_local(function, nested, &field.ty, source)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn retain_aggregate_at_dest(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        self.emit_dest_addr(function, dest)?;
        let scratch = self.scratch_i32_local(source)?;
        function.instruction(&Instruction::LocalSet(scratch));
        self.retain_aggregate_at_local(function, scratch, abi, source)
    }

    fn retain_aggregate_at_local(
        &mut self,
        function: &mut WasmFunction,
        base_local: u32,
        abi: &AbiTy,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let AbiTy::Aggregate(layout) = abi else {
            return Ok(());
        };

        if layout.is_function_value() {
            function.instruction(&Instruction::LocalGet(base_local));
            MemAccess::function_word(4).emit_load(function);
            self.retain_heap_ref_on_stack(function, source)?;
            function.instruction(&Instruction::Drop);
            return Ok(());
        }

        match &layout.kind {
            AggregateKind::Fields(fields) => {
                for field in fields {
                    self.retain_field_from_local(function, base_local, field, source)?;
                }
            }
            AggregateKind::Enum(_enum_layout) => {
                let tag_local = self.scratch_i32_other_local(source, base_local)?;
                function.instruction(&Instruction::LocalGet(base_local));
                MemAccess::enum_tag(0).emit_load(function);
                function.instruction(&Instruction::LocalSet(tag_local));

                for variant in match &layout.kind {
                    AggregateKind::Enum(enum_layout) => &enum_layout.variants,
                    AggregateKind::Fields(_) => unreachable!(),
                    AggregateKind::FunctionValue => unreachable!(),
                } {
                    function.instruction(&Instruction::LocalGet(tag_local));
                    function.instruction(&Instruction::I32Const(variant.tag));
                    function.instruction(&Instruction::I32Eq);
                    function.instruction(&Instruction::If(BlockType::Empty));
                    for field in &variant.fields {
                        self.retain_field_from_local(function, base_local, field, source)?;
                    }
                    function.instruction(&Instruction::End);
                }
            }
            AggregateKind::FunctionValue => unreachable!("function values are handled above"),
        }

        Ok(())
    }

    fn retain_field_from_local(
        &mut self,
        function: &mut WasmFunction,
        base_local: u32,
        field: &FieldLayout,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        match &field.ty {
            AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                function.instruction(&Instruction::LocalGet(base_local));
                if field.offset != 0 {
                    function.instruction(&Instruction::I32Const(field.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                emit_scalar_load(function, *ty, 0);
                self.retain_heap_ref_on_stack(function, source)?;
                function.instruction(&Instruction::Drop);
            }
            AbiTy::Aggregate(_) if field.ty.contains_heap_refs() => {
                let nested = self.scratch_i32_other_local(source, base_local)?;
                function.instruction(&Instruction::LocalGet(base_local));
                if field.offset != 0 {
                    function.instruction(&Instruction::I32Const(field.offset as i32));
                    function.instruction(&Instruction::I32Add);
                }
                function.instruction(&Instruction::LocalSet(nested));
                self.retain_aggregate_at_local(function, nested, &field.ty, source)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn emit_expr_into_owner(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &FunctionKernelValue<'db>,
    ) -> Result<(), Diagnostic> {
        match &expr.abi {
            AbiTy::Scalar(BackendTy::Unit) => self.expr(function, expr),
            AbiTy::Scalar(ty) => {
                self.expr(function, expr)?;
                if expr.ownership.is_borrowed() && ty.is_heap_ref() {
                    self.retain_heap_ref_on_stack(function, expr.source)?;
                }
                let scratch = match ty {
                    BackendTy::Float => self.scratch_f64_local(expr.source)?,
                    BackendTy::I64 => self.scratch_i64_local(expr.source)?,
                    _ => self.scratch_i32_aux_local(expr.source)?,
                };
                function.instruction(&Instruction::LocalSet(scratch));
                self.emit_dest_addr(function, dest)?;
                function.instruction(&Instruction::LocalGet(scratch));
                emit_scalar_store(function, *ty, 0);
                Ok(())
            }
            AbiTy::Aggregate(layout) => {
                if expr.ownership.is_borrowed() {
                    self.copy_aggregate_expr_to_dest(function, dest, expr, layout.size)?;
                    if expr.abi.contains_heap_refs() {
                        self.retain_aggregate_at_dest(function, dest, &expr.abi, expr.source)?;
                    }
                    Ok(())
                } else {
                    self.emit_expr_into(function, dest, expr)
                }
            }
        }
    }

    fn emit_structured_wasm_expr_into_owner(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        expr: &StructuredWasmExpr<'db>,
    ) -> Result<(), Diagnostic> {
        match &expr.abi {
            AbiTy::Scalar(BackendTy::Unit) => self.structured_wasm_expr(function, expr),
            AbiTy::Scalar(ty) => {
                self.structured_wasm_expr(function, expr)?;
                if expr.ownership.is_borrowed() && ty.is_heap_ref() {
                    self.retain_heap_ref_on_stack(function, expr.source)?;
                }
                let scratch = match ty {
                    BackendTy::Float => self.scratch_f64_local(expr.source)?,
                    BackendTy::I64 => self.scratch_i64_local(expr.source)?,
                    _ => self.scratch_i32_aux_local(expr.source)?,
                };
                function.instruction(&Instruction::LocalSet(scratch));
                self.emit_dest_addr(function, dest)?;
                function.instruction(&Instruction::LocalGet(scratch));
                emit_scalar_store(function, *ty, 0);
                Ok(())
            }
            AbiTy::Aggregate(layout) => {
                if expr.ownership.is_borrowed() {
                    let temp = self.temp_dest(expr.source)?;
                    self.emit_structured_wasm_expr_into(function, temp, expr)?;
                    self.copy_dest_to_dest(function, dest, temp, layout.size)?;
                    if expr.abi.contains_heap_refs() {
                        self.retain_aggregate_at_dest(function, dest, &expr.abi, expr.source)?;
                    }
                    Ok(())
                } else {
                    self.emit_structured_wasm_expr_into(function, dest, expr)
                }
            }
        }
    }

    fn legalized_signature(&self) -> Option<&FunctionLegalization<'db>> {
        self.function_legalization
    }

    fn legalized_result_passing(&self) -> LegalizedResultPassing {
        self.legalized_signature().map_or_else(
            || match self.function_result {
                AbiTy::Scalar(BackendTy::Unit) => LegalizedResultPassing::Unit,
                AbiTy::Scalar(ty) => LegalizedResultPassing::DirectScalar(*ty),
                AbiTy::Aggregate(_) => LegalizedResultPassing::IndirectOutPtr,
            },
            |legalization| legalization.signature.result,
        )
    }

    fn legalized_result_ptr_local(&self) -> Option<u32> {
        self.legalized_signature()
            .and_then(|legalization| legalization.signature.hidden_result_local)
            .or_else(|| self.layout.result_ptr_local())
    }

    fn legalized_env_ptr_local(&self) -> Option<u32> {
        self.legalized_signature()
            .and_then(|legalization| legalization.signature.hidden_env_local)
            .or_else(|| self.layout.env_ptr_local())
    }

    fn legalized_call_site(&self, source: ExprId) -> Option<&LegalizedCallSite<'db>> {
        self.legalized_signature().and_then(|legalization| legalization.call_sites.get(&source))
    }

    fn legalized_callable_representation(&self) -> LegalizedCallableRepresentation {
        self.legalized_signature()
            .map_or(LegalizedCallableRepresentation::HandleAndTable, |legalization| {
                legalization.callable_representation
            })
    }

    fn emit_local_ownership_ops(
        &mut self,
        function: &mut WasmFunction,
        local: u32,
        abi: &AbiTy,
        source: ExprId,
        ownership_ops: &[StructuredWasmOwnershipOp],
    ) -> Result<(), Diagnostic> {
        for op in ownership_ops {
            match op {
                StructuredWasmOwnershipOp::Retain => match abi {
                    AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                        function.instruction(&Instruction::LocalGet(local));
                        self.retain_heap_ref_on_stack(function, source)?;
                        function.instruction(&Instruction::Drop);
                    }
                    AbiTy::Aggregate(_) => {
                        self.retain_aggregate_at_local(function, local, abi, source)?;
                    }
                    AbiTy::Scalar(_) => {}
                },
                StructuredWasmOwnershipOp::Release | StructuredWasmOwnershipOp::Destroy => {
                    match abi {
                        AbiTy::Scalar(ty) if ty.is_heap_ref() => {
                            self.release_heap_ref_from_local(function, local, *ty, source)?;
                        }
                        AbiTy::Aggregate(_) => {
                            self.release_aggregate_at_local(function, local, abi, source)?;
                        }
                        AbiTy::Scalar(_) => {}
                    }
                }
                StructuredWasmOwnershipOp::Copy | StructuredWasmOwnershipOp::Move => {}
            }
        }
        Ok(())
    }

    fn emit_dest_ownership_ops(
        &mut self,
        function: &mut WasmFunction,
        dest: Dest,
        abi: &AbiTy,
        source: ExprId,
        ownership_ops: &[StructuredWasmOwnershipOp],
    ) -> Result<(), Diagnostic> {
        for op in ownership_ops {
            match op {
                StructuredWasmOwnershipOp::Retain if abi.contains_heap_refs() => {
                    self.retain_aggregate_at_dest(function, dest, abi, source)?;
                }
                StructuredWasmOwnershipOp::Release | StructuredWasmOwnershipOp::Destroy
                    if abi.contains_heap_refs() =>
                {
                    self.release_aggregate_at_dest(function, dest, abi, source)?;
                }
                StructuredWasmOwnershipOp::Copy
                | StructuredWasmOwnershipOp::Move
                | StructuredWasmOwnershipOp::Retain
                | StructuredWasmOwnershipOp::Release
                | StructuredWasmOwnershipOp::Destroy => {}
            }
        }
        Ok(())
    }

    fn emit_stack_ownership_ops(
        &mut self,
        function: &mut WasmFunction,
        ty: BackendTy,
        source: ExprId,
        ownership_ops: &[StructuredWasmOwnershipOp],
    ) -> Result<bool, Diagnostic> {
        let mut consumed = false;
        for op in ownership_ops {
            match op {
                StructuredWasmOwnershipOp::Retain if ty.is_heap_ref() => {
                    self.retain_heap_ref_on_stack(function, source)?;
                }
                StructuredWasmOwnershipOp::Release | StructuredWasmOwnershipOp::Destroy
                    if ty.is_heap_ref() =>
                {
                    self.release_heap_ref_from_stack(function, ty, source)?;
                    consumed = true;
                }
                StructuredWasmOwnershipOp::Copy
                | StructuredWasmOwnershipOp::Move
                | StructuredWasmOwnershipOp::Retain
                | StructuredWasmOwnershipOp::Release
                | StructuredWasmOwnershipOp::Destroy => {}
            }
        }
        Ok(consumed)
    }

    fn release_function_value_at_local(
        &mut self,
        function: &mut WasmFunction,
        value_local: u32,
        source: ExprId,
    ) -> Result<(), Diagnostic> {
        let env_local = self.scratch_i32_other_local(source, value_local)?;

        function.instruction(&Instruction::LocalGet(value_local));
        MemAccess::function_word(4).emit_load(function);
        function.instruction(&Instruction::LocalSet(env_local));

        function.instruction(&Instruction::LocalGet(env_local));
        let helper = self.helper_index(HelperFunction::ArcRelease, source)?;
        function.instruction(&Instruction::Call(helper));
        function.instruction(&Instruction::If(BlockType::Empty));

        for (slot, destroy_index) in self.closure_destroyers {
            function.instruction(&Instruction::LocalGet(value_local));
            MemAccess::function_word(0).emit_load(function);
            function.instruction(&Instruction::I32Const(*slot as i32));
            function.instruction(&Instruction::I32Eq);
            function.instruction(&Instruction::If(BlockType::Empty));
            function.instruction(&Instruction::LocalGet(env_local));
            function.instruction(&Instruction::Call(*destroy_index));
            function.instruction(&Instruction::End);
        }

        function.instruction(&Instruction::End);
        Ok(())
    }

    fn scratch_i32_other_local(&self, source: ExprId, local: u32) -> Result<u32, Diagnostic> {
        let primary = self.scratch_i32_local(source)?;
        let aux = self.scratch_i32_aux_local(source)?;
        if local == primary { Ok(aux) } else { Ok(primary) }
    }

    fn node_range(&self, expr: ExprId) -> mitki_errors::TextRange {
        self.source_map
            .try_node_syntax(expr)
            .map_or_else(|| self.backend.function_range(self.location), |ptr| ptr.range)
    }
}

fn arg_passing_for_abi(abi: &AbiTy) -> LegalizedArgPassing {
    match abi {
        AbiTy::Scalar(_) => LegalizedArgPassing::Direct,
        AbiTy::Aggregate(_) => LegalizedArgPassing::ByAddress,
    }
}

fn result_passing_for_abi(abi: &AbiTy) -> LegalizedResultPassing {
    match abi {
        AbiTy::Scalar(BackendTy::Unit) => LegalizedResultPassing::Unit,
        AbiTy::Scalar(ty) => LegalizedResultPassing::DirectScalar(*ty),
        AbiTy::Aggregate(_) => LegalizedResultPassing::IndirectOutPtr,
    }
}

use mitki_hir::hir::ParamId;
use mitki_resolve::{BindingId, resolve_method_for_receiver};

use super::obligations::ObligationCollector;
use super::reachability_graph::ReachabilityBuilder;
use super::*;

#[derive(Clone, Copy)]
struct ResolvedMethodCall<'db> {
    receiver: ExprId,
    function: FunctionLocation<'db>,
}

pub(super) fn walk_reachable_instance<'db>(
    backend: &mut Backend<'db>,
    instance: &ReachableInstance<'db>,
    pending: &mut VecDeque<ReachableInstance<'db>>,
    edges: &mut Vec<plan::ReachabilityEdge<'db>>,
    seen_edges: &mut FxHashSet<plan::ReachabilityEdge<'db>>,
) {
    let location = instance.owner_location();
    let hir_function = location.hir_function(backend.db);
    let function = hir_function.function(backend.db);
    let source_map = hir_function.source_map(backend.db);
    let inference = location.infer(backend.db);
    let body = match &instance {
        ReachableInstance::Function(_) => function.body(),
        ReachableInstance::Closure(closure) => {
            match backend.closure_info(closure, function, inference) {
                Ok(info) => info.body,
                Err(diagnostic) => {
                    backend.diagnostics.push(diagnostic);
                    return;
                }
            }
        }
    };
    if body == ExprId::ZERO {
        return;
    }

    let resolver = Resolver::new(backend.db, location);
    let mut validator = Validator {
        backend,
        owner_instance: instance.owner_instance(),
        location,
        function,
        source_map,
        inference,
        resolver,
        pending,
        edges,
        seen_edges,
        current_node: match instance {
            ReachableInstance::Function(instance) => {
                plan::ReachabilityNode::Function(instance.clone())
            }
            ReachableInstance::Closure(instance) => {
                plan::ReachabilityNode::Closure(instance.clone())
            }
        },
        loop_depth: 0,
    };
    validator.expr(body, ExprPosition::Value);
}

pub(super) fn collect_instance_emission_obligations<'db>(
    backend: &Backend<'db>,
    instance: &ReachableInstance<'db>,
    obligations: &mut EmissionObligationScratch<'db>,
) {
    let location = instance.owner_location();
    let hir_function = location.hir_function(backend.db);
    let function = hir_function.function(backend.db);
    let inference = location.infer(backend.db);
    let owner_instance = instance.owner_instance();
    collect_signature_emission_obligations(
        backend,
        instance,
        function,
        inference,
        &owner_instance,
        obligations,
    );

    if let ReachableInstance::Closure(closure) = instance
        && let Ok(info) = backend.closure_info(closure, function, inference)
        && info.env_layout.size > 0
    {
        obligations.used_runtime_functions.insert(RuntimeFunction::Alloc);
        obligations.used_runtime_functions.insert(RuntimeFunction::Dealloc);
    }

    let body = match instance {
        ReachableInstance::Function(_) => function.body(),
        ReachableInstance::Closure(closure) => backend
            .closure_info(closure, function, inference)
            .map_or(ExprId::ZERO, |info| info.body),
    };
    if body == ExprId::ZERO {
        return;
    }

    let resolver = Resolver::new(backend.db, location);
    let mut collector = ObligationVisitor {
        backend,
        obligations,
        owner_instance,
        location,
        function,
        inference,
        resolver,
    };
    collector.expr(body);
}

fn collect_signature_emission_obligations<'db>(
    backend: &Backend<'db>,
    instance: &ReachableInstance<'db>,
    function: &'db Function<'db>,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    owner_instance: &InstanceKey<'db>,
    obligations: &mut EmissionObligationScratch<'db>,
) {
    let location = instance.owner_location();
    let is_top_level = matches!(instance, ReachableInstance::Function(_));
    let is_typed_wasm_boundary = is_top_level
        && matches!(
            function.linkage(),
            WasmLinkage::Import { .. } | WasmLinkage::ImplicitMainExport | WasmLinkage::Export
        );
    let is_raw_import = is_top_level && matches!(function.linkage(), WasmLinkage::RawImport { .. });
    let is_stage_root = backend.compilation_mode().is_stage()
        && is_top_level
        && backend.stage_root() == Some(location);
    let crosses_wasm_boundary = is_typed_wasm_boundary || is_raw_import || is_stage_root;
    let mut boundary_params_need_runtime_allocs = false;
    let transport_profile = backend.boundary_transport_profile();
    let params = match instance {
        ReachableInstance::Function(_) => {
            match backend.function_signature_types(owner_instance, function, inference) {
                Ok((params, _)) => params,
                Err(_) => return,
            }
        }
        ReachableInstance::Closure(closure) => {
            let Some(closure_ty) =
                backend.specialized_expr_ty(owner_instance, inference, closure.closure)
            else {
                return;
            };
            let TyKind::Function { inputs, .. } = closure_ty.kind(backend.db) else {
                return;
            };
            inputs.clone()
        }
    };

    for ty in params {
        obligations.register_nominals_in_ty(backend.db, ty);
        if is_typed_wasm_boundary {
            boundary_params_need_runtime_allocs |= transport_profile
                .plan_or_message(
                    backend.db,
                    ty,
                    "Wasm backend does not support this function signature type",
                )
                .is_ok_and(|_| {
                    crate::capability::supported_value_abi(backend.db, ty).is_some_and(
                        |runtime_abi| {
                            transport_profile.requires_runtime_allocs(backend.db, ty, &runtime_abi)
                        },
                    )
                });
        }
    }

    let return_ty = match instance {
        ReachableInstance::Function(_) => backend.specialize_ty(
            owner_instance,
            backend.function_return_ty(owner_instance.location, function, inference),
        ),
        ReachableInstance::Closure(closure) => {
            let Some(closure_ty) =
                backend.specialized_expr_ty(owner_instance, inference, closure.closure)
            else {
                return;
            };
            let TyKind::Function { output, .. } = closure_ty.kind(backend.db) else {
                return;
            };
            *output
        }
    };
    obligations.register_nominals_in_ty(backend.db, return_ty);

    if crosses_wasm_boundary {
        let result_needs_runtime_allocs = transport_profile
            .plan_or_message(
                backend.db,
                return_ty,
                "Wasm backend does not support this function signature type",
            )
            .is_ok_and(|_| {
                crate::capability::supported_value_abi(backend.db, return_ty).is_some_and(
                    |runtime_abi| {
                        transport_profile.requires_runtime_allocs(
                            backend.db,
                            return_ty,
                            &runtime_abi,
                        )
                    },
                )
            });
        if boundary_params_need_runtime_allocs || result_needs_runtime_allocs {
            obligations.used_runtime_functions.insert(RuntimeFunction::Alloc);
            obligations.used_runtime_functions.insert(RuntimeFunction::Dealloc);
        }
    }
}

impl<'db> Backend<'db> {
    pub fn collect_reachable_program(&mut self) {
        let reachability = ReachabilityBuilder::new(self).build();
        let obligations = ObligationCollector::build(&*self, &reachability);
        self.set_shadow_state(reachability, obligations.shadow_obligations);
    }

    pub(super) fn is_stage_mode(&self) -> bool {
        self.compilation_mode().is_stage()
    }

    pub fn collect_stage_diagnostics(&mut self) {
        if !self.compilation_mode().is_stage() {
            return;
        }

        let Some(graph) = self.shadow_reachability.as_ref() else {
            return;
        };
        let relevant_ranges = graph
            .instances
            .iter()
            .map(|instance| match instance {
                plan::ReachabilityInstance::Function(instance) => {
                    self.function_range(instance.location)
                }
                plan::ReachabilityInstance::Closure(instance) => {
                    self.function_range(instance.owner)
                }
            })
            .collect::<Vec<_>>();
        let parse_diagnostics = self
            .file
            .parse(self.db)
            .diagnostics()
            .iter()
            .filter(|diagnostic| {
                relevant_ranges.iter().any(|range| ranges_intersect(*range, diagnostic.range()))
            })
            .cloned()
            .collect::<Vec<_>>();
        self.diagnostics.extend(parse_diagnostics);

        let mut seen_functions = FxHashSet::default();
        for instance in &graph.instances {
            let location = match instance {
                plan::ReachabilityInstance::Function(instance) => instance.location,
                plan::ReachabilityInstance::Closure(instance) => instance.owner,
            };
            if !seen_functions.insert(location) {
                continue;
            }
            self.diagnostics
                .extend(mitki_analysis::check_function(self.db, location).iter().cloned());
        }
    }

    pub(super) fn closure_info(
        &self,
        closure: &ClosureInstanceKey<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) -> Result<ClosureInfo<'db>, Diagnostic> {
        let nodes = function.node_store();
        let range = self.function_range(closure.owner);
        let closure_id = nodes.as_closure(closure.closure).ok_or_else(|| {
            Diagnostic::error(
                "internal error: expected closure instance to reference a closure expression",
                range,
            )
        })?;
        let (params, body) = nodes.closure_parts(closure_id);
        let owner_instance = closure.owner_instance();
        let closure_ty =
            self.specialized_expr_ty(&owner_instance, inference, closure.closure).ok_or_else(
                || Diagnostic::error("internal error: missing inferred closure type", range),
            )?;
        let signature = crate::capability::supported_function_signature_or_message(
            self.db,
            closure_ty,
            "Wasm backend does not support this closure type",
        )
        .map_err(|message| Diagnostic::error(message, range))?;

        let owned_bindings = self.closure_owned_bindings(function, body, params.iter().collect());
        let captures = self.collect_closure_captures(
            &owner_instance,
            closure.owner,
            function,
            inference,
            body,
            &owned_bindings,
        )?;
        let env_layout = layout_fields(captures.iter().map(|capture| {
            (Some(symbol_bits(nodes.name(capture.binding))), Some(capture.ty.clone()))
        }))
        .map_or_else(
            || AggregateLayout { size: 0, align: 1, kind: AggregateKind::Fields(Vec::new()) },
            |layout| AggregateLayout {
                size: layout.size,
                align: layout.align,
                kind: AggregateKind::Fields(layout.fields),
            },
        );

        let mut captures_with_fields = Vec::with_capacity(captures.len());
        let env_fields = env_layout.fields().unwrap_or(&[]);
        for (capture, field) in captures.into_iter().zip(env_fields.iter()) {
            captures_with_fields.push(ClosureCapture {
                binding: capture.binding,
                field: field.clone(),
                _marker: std::marker::PhantomData,
            });
        }

        Ok(ClosureInfo {
            body,
            params: params.iter().collect(),
            captures: captures_with_fields,
            env_layout,
            signature,
        })
    }

    fn closure_owned_bindings(
        &self,
        function: &'db Function<'db>,
        body: ExprId,
        params: Vec<ParamId>,
    ) -> FxHashSet<NameId> {
        let nodes = function.node_store();
        let mut owned = params
            .into_iter()
            .flat_map(|param| {
                let (pattern, _) = nodes.param(param);
                nodes.pattern_binding_names(pattern)
            })
            .collect::<FxHashSet<_>>();
        self.collect_owned_bindings_in_expr(function, body, &mut owned);
        owned
    }

    fn collect_owned_bindings_in_expr(
        &self,
        function: &'db Function<'db>,
        expr: ExprId,
        owned: &mut FxHashSet<NameId>,
    ) {
        let nodes = function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(expr).expect("LocalVar node mismatch"));
                owned.extend(nodes.pattern_binding_names(var.pattern));
                if var.initializer != ExprId::ZERO {
                    self.collect_owned_bindings_in_expr(function, var.initializer, owned);
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                for stmt in stmts.iter() {
                    if nodes.node_kind(stmt) == NodeKind::ReturnStmt {
                        let (value, _) = nodes
                            .return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
                        if value != ExprId::ZERO {
                            self.collect_owned_bindings_in_expr(function, value, owned);
                        }
                    } else if let Some(stmt_expr) = stmt_as_expr(nodes, stmt) {
                        self.collect_owned_bindings_in_expr(function, stmt_expr, owned);
                    }
                }
                if tail != ExprId::ZERO {
                    self.collect_owned_bindings_in_expr(function, tail, owned);
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                self.collect_owned_bindings_in_expr(function, callee, owned);
                for arg in args.iter() {
                    self.collect_owned_bindings_in_expr(function, arg, owned);
                }
            }
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.collect_owned_bindings_in_expr(function, item, owned);
                }
            }
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.collect_owned_bindings_in_expr(function, base, owned);
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.collect_owned_bindings_in_expr(function, binary.lhs, owned);
                self.collect_owned_bindings_in_expr(function, binary.rhs, owned);
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                self.collect_owned_bindings_in_expr(function, prefix.expr, owned);
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                self.collect_owned_bindings_in_expr(function, if_expr.cond, owned);
                if if_expr.then_branch != ExprId::ZERO {
                    self.collect_owned_bindings_in_expr(function, if_expr.then_branch, owned);
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.collect_owned_bindings_in_expr(function, if_expr.else_branch, owned);
                }
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(expr).expect("Match node mismatch"));
                self.collect_owned_bindings_in_expr(function, scrutinee, owned);
                for arm in arms.iter() {
                    let (pattern, body) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    owned.extend(nodes.pattern_binding_names(pattern));
                    self.collect_owned_bindings_in_expr(function, body, owned);
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.collect_owned_bindings_in_expr(function, body, owned);
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.collect_owned_bindings_in_expr(function, items.get(index).unwrap(), owned);
                    index += 2;
                }
            }
            NodeKind::Closure => {}
            _ => {}
        }
    }

    fn collect_closure_captures(
        &self,
        owner_instance: &InstanceKey<'db>,
        owner_location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        body: ExprId,
        owned_bindings: &FxHashSet<NameId>,
    ) -> Result<Vec<PendingClosureCapture>, Diagnostic> {
        let mut resolver = Resolver::new(self.db, owner_location);
        let mut captures = Vec::new();
        let mut seen = FxHashSet::default();
        self.collect_closure_captures_in_expr(
            function,
            body,
            inference,
            owner_instance,
            &mut resolver,
            owned_bindings,
            &mut seen,
            &mut captures,
        )?;
        Ok(captures)
    }

    #[allow(clippy::too_many_arguments)]
    fn collect_closure_captures_in_expr(
        &self,
        function: &'db Function<'db>,
        expr: ExprId,
        inference: &'db mitki_typeck::infer::Inference<'db>,
        owner_instance: &InstanceKey<'db>,
        resolver: &mut Resolver<'db>,
        owned_bindings: &FxHashSet<NameId>,
        seen: &mut FxHashSet<NameId>,
        captures: &mut Vec<PendingClosureCapture>,
    ) -> Result<(), Diagnostic> {
        let nodes = function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::Name => {
                let name = nodes.as_name(expr).expect("Name node mismatch");
                let symbol = nodes.name(name);
                let guard = resolver.scopes_for_node(expr);
                let resolution = resolver.resolve_value_binding(symbol);
                resolver.reset(guard);

                if let Some(BindingId::Local(binding) | BindingId::Param(binding)) = resolution
                    && !owned_bindings.contains(&binding)
                    && seen.insert(binding)
                {
                    let ty = inference.type_of_node(binding.into()).ok_or_else(|| {
                        Diagnostic::error(
                            "internal error: missing inferred capture type",
                            self.function_range(owner_instance.location),
                        )
                    })?;
                    let ty = self.specialize_ty(owner_instance, ty);
                    let abi = crate::capability::supported_value_abi_or_message(
                        self.db,
                        ty,
                        "Wasm backend does not support capturing this value type",
                    )
                    .map_err(|message| {
                        Diagnostic::error(message, self.function_range(owner_instance.location))
                    })?;
                    captures.push(PendingClosureCapture { binding, ty: abi });
                }
            }
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(expr).expect("LocalVar node mismatch"));
                if var.initializer != ExprId::ZERO {
                    self.collect_closure_captures_in_expr(
                        function,
                        var.initializer,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                for stmt in stmts.iter() {
                    if nodes.node_kind(stmt) == NodeKind::ReturnStmt {
                        let (value, _) = nodes
                            .return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
                        if value != ExprId::ZERO {
                            self.collect_closure_captures_in_expr(
                                function,
                                value,
                                inference,
                                owner_instance,
                                resolver,
                                owned_bindings,
                                seen,
                                captures,
                            )?;
                        }
                    } else if let Some(stmt_expr) = stmt_as_expr(nodes, stmt) {
                        self.collect_closure_captures_in_expr(
                            function,
                            stmt_expr,
                            inference,
                            owner_instance,
                            resolver,
                            owned_bindings,
                            seen,
                            captures,
                        )?;
                    }
                }
                if tail != ExprId::ZERO {
                    self.collect_closure_captures_in_expr(
                        function,
                        tail,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                self.collect_closure_captures_in_expr(
                    function,
                    callee,
                    inference,
                    owner_instance,
                    resolver,
                    owned_bindings,
                    seen,
                    captures,
                )?;
                for arg in args.iter() {
                    self.collect_closure_captures_in_expr(
                        function,
                        arg,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.collect_closure_captures_in_expr(
                        function,
                        item,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.collect_closure_captures_in_expr(
                        function,
                        base,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.collect_closure_captures_in_expr(
                    function,
                    binary.lhs,
                    inference,
                    owner_instance,
                    resolver,
                    owned_bindings,
                    seen,
                    captures,
                )?;
                self.collect_closure_captures_in_expr(
                    function,
                    binary.rhs,
                    inference,
                    owner_instance,
                    resolver,
                    owned_bindings,
                    seen,
                    captures,
                )?;
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                self.collect_closure_captures_in_expr(
                    function,
                    prefix.expr,
                    inference,
                    owner_instance,
                    resolver,
                    owned_bindings,
                    seen,
                    captures,
                )?;
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                self.collect_closure_captures_in_expr(
                    function,
                    if_expr.cond,
                    inference,
                    owner_instance,
                    resolver,
                    owned_bindings,
                    seen,
                    captures,
                )?;
                if if_expr.then_branch != ExprId::ZERO {
                    self.collect_closure_captures_in_expr(
                        function,
                        if_expr.then_branch,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.collect_closure_captures_in_expr(
                        function,
                        if_expr.else_branch,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.collect_closure_captures_in_expr(
                        function,
                        body,
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.collect_closure_captures_in_expr(
                        function,
                        items.get(index).unwrap(),
                        inference,
                        owner_instance,
                        resolver,
                        owned_bindings,
                        seen,
                        captures,
                    )?;
                    index += 2;
                }
            }
            NodeKind::Closure => {}
            _ => {}
        }
        Ok(())
    }

    pub(super) fn validate_function_signature(&mut self, instance: &ReachableInstance<'db>) {
        let location = instance.owner_location();
        let hir_function = location.hir_function(self.db);
        let function = hir_function.function(self.db);
        let source_map = hir_function.source_map(self.db);
        let inference = location.infer(self.db);
        let nodes = function.node_store();
        let owner_instance = instance.owner_instance();
        let is_top_level = matches!(instance, ReachableInstance::Function(_));
        let is_typed_wasm_boundary = is_top_level
            && matches!(
                function.linkage(),
                WasmLinkage::Import { .. } | WasmLinkage::ImplicitMainExport | WasmLinkage::Export
            );
        let is_raw_import =
            is_top_level && matches!(function.linkage(), WasmLinkage::RawImport { .. });
        let is_stage_root = self.compilation_mode().is_stage()
            && is_top_level
            && self.stage_root() == Some(location);
        let crosses_wasm_boundary = is_typed_wasm_boundary || is_raw_import || is_stage_root;
        let mut boundary_params_need_runtime_allocs = false;
        let transport_profile = self.boundary_transport_profile();
        let params = match instance {
            ReachableInstance::Function(_) => function.params().to_vec(),
            ReachableInstance::Closure(closure) => {
                match self.closure_info(closure, function, inference) {
                    Ok(info) => info.params,
                    Err(diagnostic) => {
                        self.diagnostics.push(diagnostic);
                        return;
                    }
                }
            }
        };
        let param_tys = match instance {
            ReachableInstance::Function(instance) => {
                match self.function_signature_types(instance, function, inference) {
                    Ok((params, _)) => params,
                    Err(diagnostic) => {
                        self.diagnostics.push(diagnostic);
                        return;
                    }
                }
            }
            ReachableInstance::Closure(closure) => {
                let Some(closure_ty) =
                    self.specialized_expr_ty(&owner_instance, inference, closure.closure)
                else {
                    self.diagnostics.push(Diagnostic::error(
                        "internal error: missing inferred closure type",
                        self.function_range(location),
                    ));
                    return;
                };
                let TyKind::Function { inputs, .. } = closure_ty.kind(self.db) else {
                    self.diagnostics.push(Diagnostic::error(
                        "internal error: closure value did not lower to a function type",
                        self.function_range(location),
                    ));
                    return;
                };
                inputs.clone()
            }
        };

        for (param, ty) in params.into_iter().zip(param_tys) {
            let (pattern, ty_id) = nodes.param(param);
            if is_typed_wasm_boundary {
                boundary_params_need_runtime_allocs |= transport_profile
                    .plan_or_message(
                        self.db,
                        ty,
                        "Wasm backend does not support this function signature type",
                    )
                    .is_ok_and(|_| {
                        crate::capability::supported_value_abi(self.db, ty).is_some_and(
                            |runtime_abi| {
                                transport_profile.requires_runtime_allocs(self.db, ty, &runtime_abi)
                            },
                        )
                    });
            }
            let range = if ty_id != mitki_hir::hir::TyId::ZERO {
                source_map
                    .try_type_syntax(ty_id)
                    .map_or_else(|| self.function_range(location), |ptr| ptr.range)
            } else {
                source_map
                    .try_pat_syntax(pattern)
                    .map_or_else(|| self.function_range(location), |ptr| ptr.range)
            };
            self.validate_signature_ty(ty, range, crosses_wasm_boundary, is_raw_import);
        }

        let return_ty = match instance {
            ReachableInstance::Function(_) => self.specialize_ty(
                &owner_instance,
                self.function_return_ty(owner_instance.location, function, inference),
            ),
            ReachableInstance::Closure(closure) => {
                let Some(closure_ty) =
                    self.specialized_expr_ty(&owner_instance, inference, closure.closure)
                else {
                    self.diagnostics.push(Diagnostic::error(
                        "internal error: missing inferred closure type",
                        self.function_range(location),
                    ));
                    return;
                };
                let TyKind::Function { output, .. } = closure_ty.kind(self.db) else {
                    self.diagnostics.push(Diagnostic::error(
                        "internal error: closure value did not lower to a function type",
                        self.function_range(location),
                    ));
                    return;
                };
                *output
            }
        };
        let return_range = self.return_range(location, function, source_map);
        self.validate_signature_ty(return_ty, return_range, crosses_wasm_boundary, is_raw_import);

        if crosses_wasm_boundary {
            let result_needs_runtime_allocs = transport_profile
                .plan_or_message(
                    self.db,
                    return_ty,
                    "Wasm backend does not support this function signature type",
                )
                .is_ok_and(|_| {
                    crate::capability::supported_value_abi(self.db, return_ty).is_some_and(
                        |runtime_abi| {
                            transport_profile.requires_runtime_allocs(
                                self.db,
                                return_ty,
                                &runtime_abi,
                            )
                        },
                    )
                });
            let _needs_runtime_allocs =
                boundary_params_need_runtime_allocs || result_needs_runtime_allocs;
        }
    }

    pub(in crate::backend) fn return_range(
        &self,
        location: FunctionLocation<'db>,
        function: &'db Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
    ) -> mitki_errors::TextRange {
        if function.ret_type().is_zero() {
            self.function_range(location)
        } else {
            source_map
                .try_type_syntax(function.ret_type())
                .map_or_else(|| self.function_range(location), |ptr| ptr.range)
        }
    }

    pub(super) fn function_signature(
        &self,
        instance: &InstanceKey<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) -> Result<FunctionSignature, Diagnostic> {
        let (inputs, output) = self.function_signature_types(instance, function, inference)?;
        let function_ty = Ty::new(self.db, TyKind::Function { inputs, output });
        crate::capability::supported_function_signature_or_message(
            self.db,
            function_ty,
            "Wasm backend does not support this function value type",
        )
        .map_err(|message| Diagnostic::error(message, self.function_range(instance.location)))
    }

    pub(super) fn function_signature_types(
        &self,
        instance: &InstanceKey<'db>,
        function: &'db Function<'db>,
        inference: &'db mitki_typeck::infer::Inference<'db>,
    ) -> Result<(Vec<Ty<'db>>, Ty<'db>), Diagnostic> {
        let location = instance.location;
        let signature = location.signature(self.db);
        let signature_resolver = SignatureTypeResolver::new(self.db, location, signature);
        let nodes = signature.nodes(self.db);
        let hir_function = location.hir_function(self.db);
        let source_map = hir_function.source_map(self.db);
        let params = signature
            .params(self.db)
            .iter()
            .enumerate()
            .map(|(index, &param)| {
                let (_, ty) = nodes.param(param);
                signature_resolver
                    .resolve(ty)
                    .map(|ty| self.specialize_ty(instance, ty))
                    .map_err(|error| {
                        Diagnostic::error(
                            format!("internal error: {error}"),
                            self.function_range(location),
                        )
                    })
                    .and_then(|ty| {
                        if function_param_binding_name(function, index)
                            .is_some_and(|name| source_map.is_mutable_binding(name))
                            && crate::capability::supported_value_abi_or_message(
                                self.db,
                                ty,
                                "Wasm backend does not support this value type",
                            )
                            .map_err(|message| {
                                Diagnostic::error(message, self.function_range(location))
                            })?
                            .is_aggregate()
                        {
                            Ok(Ty::new(self.db, TyKind::Pointer { mutable: true, pointee: ty }))
                        } else {
                            Ok(ty)
                        }
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let result =
            self.specialize_ty(instance, self.function_return_ty(location, function, inference));
        Ok((params, result))
    }

    fn validate_signature_ty(
        &mut self,
        ty: Ty<'db>,
        range: mitki_errors::TextRange,
        crosses_wasm_boundary: bool,
        raw_wasm_import: bool,
    ) {
        let result = if raw_wasm_import {
            crate::capability::supported_value_abi_or_message(
                self.db,
                ty,
                "Wasm backend does not support this raw import signature type",
            )
            .map(|_abi| ())
        } else if !crosses_wasm_boundary {
            crate::capability::supported_value_abi_or_message(
                self.db,
                ty,
                "Wasm backend does not support this reachable function signature type",
            )
            .map(|_abi| ())
        } else {
            Ok(())
        };

        if let Err(message) = result {
            self.diagnostics.push(Diagnostic::error(message, range));
        }
    }

    pub(super) fn instance_for_call(
        &self,
        caller_instance: &InstanceKey<'db>,
        caller_inference: &'db mitki_typeck::infer::Inference<'db>,
        target: FunctionLocation<'db>,
        call_expr: ExprId,
        args: &[ExprId],
        range: mitki_errors::TextRange,
    ) -> Result<InstanceKey<'db>, Diagnostic> {
        let hir_function = target.hir_function(self.db);
        let function = hir_function.function(self.db);
        let type_param_count = function.type_params().len();
        if type_param_count == 0 {
            return Ok(InstanceKey { location: target, type_args: Vec::new() });
        }

        let pattern = self.signature_pattern(target)?;
        let mut type_args = vec![None; type_param_count];

        for (&pattern, &arg) in pattern.params.iter().zip(args.iter()) {
            let actual = self
                .specialized_expr_ty(caller_instance, caller_inference, arg)
                .ok_or_else(|| {
                    Diagnostic::error("internal error: missing inferred call argument type", range)
                })?;
            self.collect_instance_type_args(pattern, actual, &mut type_args, range)?;
        }

        let actual_return = self
            .specialized_expr_ty(caller_instance, caller_inference, call_expr)
            .unwrap_or_else(|| Ty::new(self.db, TyKind::Tuple(Vec::new())));
        self.collect_instance_type_args(pattern.result, actual_return, &mut type_args, range)?;

        let type_args = type_args
            .into_iter()
            .map(|ty| {
                ty.ok_or_else(|| {
                    Diagnostic::error(
                        "Wasm backend could not determine concrete type arguments for a generic \
                         call",
                        range,
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(InstanceKey { location: target, type_args })
    }

    pub(super) fn instance_for_function_value(
        &self,
        caller_instance: &InstanceKey<'db>,
        caller_inference: &'db mitki_typeck::infer::Inference<'db>,
        target: FunctionLocation<'db>,
        value_expr: ExprId,
        range: mitki_errors::TextRange,
    ) -> Result<InstanceKey<'db>, Diagnostic> {
        let hir_function = target.hir_function(self.db);
        let function = hir_function.function(self.db);
        let type_param_count = function.type_params().len();
        if type_param_count == 0 {
            return Ok(InstanceKey { location: target, type_args: Vec::new() });
        }

        let pattern = self.signature_pattern(target)?;
        let actual =
            self.specialized_expr_ty(caller_instance, caller_inference, value_expr).ok_or_else(
                || Diagnostic::error("internal error: missing inferred function value type", range),
            )?;
        let pattern_ty = Ty::new(
            self.db,
            TyKind::Function { inputs: pattern.params.clone(), output: pattern.result },
        );
        let mut type_args = vec![None; type_param_count];
        self.collect_instance_type_args(pattern_ty, actual, &mut type_args, range)?;
        let type_args = type_args
            .into_iter()
            .map(|ty| {
                ty.ok_or_else(|| {
                    Diagnostic::error(
                        "Wasm backend could not determine concrete type arguments for a generic \
                         function value",
                        range,
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(InstanceKey { location: target, type_args })
    }

    fn signature_pattern(
        &self,
        location: FunctionLocation<'db>,
    ) -> Result<SignaturePattern<'db>, Diagnostic> {
        let signature = location.signature(self.db);
        let signature_resolver = SignatureTypeResolver::new(self.db, location, signature);
        let nodes = signature.nodes(self.db);
        let params = signature
            .params(self.db)
            .iter()
            .map(|&param| {
                let (_, ty) = nodes.param(param);
                signature_resolver.resolve(ty).map_err(|error| {
                    Diagnostic::error(
                        format!("internal error: {error}"),
                        self.function_range(location),
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let result = signature_resolver.resolve(signature.ret_type(self.db)).map_err(|error| {
            Diagnostic::error(format!("internal error: {error}"), self.function_range(location))
        })?;
        Ok(SignaturePattern { params, result })
    }

    fn collect_instance_type_args(
        &self,
        pattern: Ty<'db>,
        actual: Ty<'db>,
        type_args: &mut [Option<Ty<'db>>],
        range: mitki_errors::TextRange,
    ) -> Result<(), Diagnostic> {
        match pattern.kind(self.db) {
            TyKind::Var(id) if (*id as usize) < type_args.len() => {
                if let Some(existing) = type_args[*id as usize] {
                    if existing != actual {
                        return Err(Diagnostic::error(
                            "Wasm backend found incompatible concrete types for a generic call",
                            range,
                        ));
                    }
                } else {
                    type_args[*id as usize] = Some(actual);
                }
                Ok(())
            }
            TyKind::Tuple(pattern_items) => {
                let TyKind::Tuple(actual_items) = actual.kind(self.db) else {
                    return Err(Diagnostic::error(
                        "Wasm backend could not match tuple type arguments for a generic call",
                        range,
                    ));
                };
                if pattern_items.len() != actual_items.len() {
                    return Err(Diagnostic::error(
                        "Wasm backend found a tuple arity mismatch while specializing a generic \
                         call",
                        range,
                    ));
                }
                for (&pattern_item, &actual_item) in pattern_items.iter().zip(actual_items.iter()) {
                    self.collect_instance_type_args(pattern_item, actual_item, type_args, range)?;
                }
                Ok(())
            }
            TyKind::Record(pattern_fields) => {
                let TyKind::Record(actual_fields) = actual.kind(self.db) else {
                    return Err(Diagnostic::error(
                        "Wasm backend could not match record type arguments for a generic call",
                        range,
                    ));
                };
                if pattern_fields.len() != actual_fields.len() {
                    return Err(Diagnostic::error(
                        "Wasm backend found a record field mismatch while specializing a generic \
                         call",
                        range,
                    ));
                }
                for (pattern_name, pattern_ty) in pattern_fields {
                    let Some((_, actual_ty)) =
                        actual_fields.iter().find(|(actual_name, _)| actual_name == pattern_name)
                    else {
                        return Err(Diagnostic::error(
                            "Wasm backend found a record field mismatch while specializing a \
                             generic call",
                            range,
                        ));
                    };
                    self.collect_instance_type_args(*pattern_ty, *actual_ty, type_args, range)?;
                }
                Ok(())
            }
            TyKind::Function { inputs: pattern_inputs, output: pattern_output } => {
                let TyKind::Function { inputs: actual_inputs, output: actual_output } =
                    actual.kind(self.db)
                else {
                    return Err(Diagnostic::error(
                        "Wasm backend could not match function type arguments for a generic call",
                        range,
                    ));
                };
                if pattern_inputs.len() != actual_inputs.len() {
                    return Err(Diagnostic::error(
                        "Wasm backend found a function arity mismatch while specializing a \
                         generic call",
                        range,
                    ));
                }
                for (&pattern_input, &actual_input) in
                    pattern_inputs.iter().zip(actual_inputs.iter())
                {
                    self.collect_instance_type_args(pattern_input, actual_input, type_args, range)?;
                }
                self.collect_instance_type_args(*pattern_output, *actual_output, type_args, range)
            }
            _ if pattern == actual => Ok(()),
            _ => Err(Diagnostic::error(
                "Wasm backend could not match concrete type arguments for a generic call",
                range,
            )),
        }
    }
}

struct ObligationVisitor<'a, 'db> {
    backend: &'a Backend<'db>,
    obligations: &'a mut EmissionObligationScratch<'db>,
    owner_instance: InstanceKey<'db>,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    resolver: Resolver<'db>,
}

impl<'a, 'db> ObligationVisitor<'a, 'db> {
    fn expr(&mut self, expr: ExprId) {
        if let Some(ty) =
            self.backend.specialized_expr_ty(&self.owner_instance, self.inference, expr)
        {
            self.obligations.register_nominals_in_ty(self.backend.db, ty);
        }

        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
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
            | NodeKind::Postfix => {}
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.expr(item);
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                for stmt in stmts.iter() {
                    self.stmt(stmt);
                }
                if tail != ExprId::ZERO {
                    self.expr(tail);
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                let mut compiler_intrinsic = None;
                if !self.is_enum_variant_constructor_call(expr, callee)
                    && nodes.node_kind(callee) == NodeKind::Name
                {
                    match self.resolve_name(callee) {
                        Some(BindingId::CompilerIntrinsic(intrinsic)) => {
                            compiler_intrinsic = Some(intrinsic);
                            let args = args.iter().collect::<Vec<_>>();
                            self.collect_compiler_intrinsic_call(intrinsic, args.as_slice());
                        }
                        Some(BindingId::RuntimeFunction(function)) => {
                            if !self.backend.is_stage_mode() {
                                self.obligations.used_runtime_functions.insert(function);
                            }
                        }
                        Some(BindingId::Function(_)) => {}
                        Some(BindingId::Local(_))
                        | Some(BindingId::Param(_))
                        | Some(BindingId::Struct(_))
                        | Some(BindingId::Enum(_))
                        | Some(BindingId::EnumVariant(_))
                        | Some(BindingId::BuiltinType(_))
                        | None => self.expr(callee),
                    }
                } else if !self.is_enum_variant_constructor_call(expr, callee) {
                    self.expr(callee);
                }

                if compiler_intrinsic.is_none() {
                    for arg in args.iter() {
                        self.expr(arg);
                    }
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.expr(binary.lhs);
                self.expr(binary.rhs);
                self.collect_binary_helpers(binary.lhs, binary.op, binary.rhs);
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
                    let (_, body) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    self.expr(body);
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body);
                }
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(expr).expect("UnsafeBlock mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body);
                }
            }
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(expr).expect("LocalVar node mismatch"));
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
                if self.is_enum_variant_ref(expr) {
                    return;
                }

                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.expr(base);
                }
            }
            NodeKind::Closure => {
                let closure = ClosureInstanceKey {
                    owner: self.location,
                    type_args: self.owner_instance.type_args.clone(),
                    closure: expr,
                };
                if let Ok(info) = self.backend.closure_info(&closure, self.function, self.inference)
                    && info.env_layout.size > 0
                {
                    self.obligations.used_runtime_functions.insert(RuntimeFunction::Alloc);
                    self.obligations.used_runtime_functions.insert(RuntimeFunction::Dealloc);
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.expr(items.get(index).unwrap());
                    index += 2;
                }
            }
            _ => {}
        }
    }

    fn stmt(&mut self, stmt: StmtId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(stmt).expect("LocalVar node mismatch"));
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

    fn collect_binary_helpers(&mut self, lhs: ExprId, op: ExprId, rhs: ExprId) {
        let nodes = self.function.node_store();
        let op_sym = nodes.name(nodes.as_name(op).expect("op should be Name"));
        let op_text = op_sym.text(self.backend.db);
        let lhs_ty = self.expr_abi(lhs);
        let rhs_ty = self.expr_abi(rhs);

        if !matches!(op_text, "==" | "!=") {
            return;
        }

        match (lhs_ty.as_ref(), rhs_ty.as_ref()) {
            (
                Some(AbiTy::Scalar(BackendTy::Ref(RefKind::String))),
                Some(AbiTy::Scalar(BackendTy::Ref(RefKind::String))),
            ) => {
                self.obligations.used_helpers.insert(HelperFunction::MemoryEq);
                self.obligations.used_helpers.insert(HelperFunction::StringEq);
            }
            (Some(AbiTy::Aggregate(lhs)), Some(AbiTy::Aggregate(rhs))) if lhs == rhs => {
                self.obligations.used_helpers.insert(HelperFunction::MemoryEq);
            }
            _ => {}
        }
    }

    fn collect_compiler_intrinsic_call(&mut self, intrinsic: CompilerIntrinsic, args: &[ExprId]) {
        if intrinsic == CompilerIntrinsic::Comptime {
            if let Some(arg) = args.first().copied() {
                self.expr(arg);
            }
            return;
        }

        if intrinsic.is_reflection() {
            if self.backend.is_stage_mode()
                && let Some(stage_intrinsic) = StageIntrinsic::from_compiler_intrinsic(intrinsic)
            {
                self.obligations.used_stage_intrinsics.insert(stage_intrinsic);
            }
            if args.len() > 1 {
                self.expr(args[1]);
            }
            return;
        }

        if self.backend.is_stage_mode() {
            return;
        }

        if intrinsic == CompilerIntrinsic::StrFromUtf8Unchecked {
            self.obligations.used_runtime_functions.insert(RuntimeFunction::Alloc);
        }

        for &arg in args {
            self.expr(arg);
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

    fn resolve_name_ty(&mut self, expr: ExprId) -> Option<Ty<'db>> {
        let nodes = self.function.node_store();
        let name = nodes.as_name(expr)?;
        let symbol = nodes.name(name);
        let guard = self.resolver.scopes_for_node(expr);
        let ty = self
            .resolver
            .resolve_type_binding(symbol)
            .and_then(|binding| self.resolver.ty_for_binding(binding));
        self.resolver.reset(guard);
        ty
    }

    fn expr_abi(&mut self, expr: ExprId) -> Option<AbiTy> {
        self.lowerable_expr_ty(expr)
            .and_then(|ty| crate::capability::supported_value_abi(self.backend.db, ty))
    }

    fn lowerable_expr_ty(&mut self, expr: ExprId) -> Option<Ty<'db>> {
        let ty = self.backend.specialized_expr_ty(&self.owner_instance, self.inference, expr)?;
        Some(self.concrete_expr_member_ty(expr, ty).unwrap_or(ty))
    }

    fn concrete_expr_member_ty(&mut self, expr: ExprId, ty: Ty<'db>) -> Option<Ty<'db>> {
        let TyKind::Union(members) = ty.kind(self.backend.db) else {
            return None;
        };

        let mut compatible = Vec::new();
        for member in members {
            if self.expr_matches_ty(expr, *member) {
                compatible.push(*member);
            }
        }
        match compatible.as_slice() {
            [selected] => Some(*selected),
            _ => None,
        }
    }

    fn expr_matches_ty(&mut self, expr: ExprId, ty: Ty<'db>) -> bool {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::True | NodeKind::False => matches!(ty.kind(self.backend.db), TyKind::Bool),
            NodeKind::Int => matches!(ty.kind(self.backend.db), TyKind::Int | TyKind::ExactInt(_)),
            NodeKind::Float => matches!(ty.kind(self.backend.db), TyKind::Float),
            NodeKind::String => matches!(ty.kind(self.backend.db), TyKind::String),
            NodeKind::Char => matches!(ty.kind(self.backend.db), TyKind::Char),
            NodeKind::Tuple => {
                let items = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                match ty.kind(self.backend.db) {
                    TyKind::Tuple(member_items) if member_items.len() == items.len() => items
                        .iter()
                        .zip(member_items.iter())
                        .all(|(item, member_item)| self.expr_matches_ty(item, *member_item)),
                    _ => false,
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let struct_name = items.iter().next().unwrap_or(ExprId::ZERO);
                if struct_name == ExprId::ZERO {
                    return matches!(ty.kind(self.backend.db), TyKind::Record(_));
                }

                matches!(self.resolve_name_ty(struct_name), Some(struct_ty) if struct_ty == ty)
            }
            NodeKind::Field => self.expr_is_enum_variant(expr, ty),
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                self.expr_is_enum_variant(callee, ty)
                    && matches!(ty.kind(self.backend.db), TyKind::Enum(enum_ty)
                        if enum_variants(self.backend.db, *enum_ty)
                            .iter()
                            .find(|(name, payload)| {
                                let (_, variant_name_expr) =
                                    nodes.field(nodes.as_field(callee).expect("variant call"));
                                let variant_name = nodes
                                    .as_name(variant_name_expr)
                                    .expect("variant name should lower to Name");
                                *name == nodes.name(variant_name) && payload.len() == args.len()
                            })
                            .is_some())
            }
            _ => false,
        }
    }

    fn expr_is_enum_variant(&mut self, expr: ExprId, ty: Ty<'db>) -> bool {
        let nodes = self.function.node_store();
        let TyKind::Enum(enum_ty) = ty.kind(self.backend.db) else {
            return false;
        };
        if nodes.node_kind(expr) != NodeKind::Field {
            return false;
        }

        let (base, field_name_expr) =
            nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        let variant_name =
            nodes.as_name(field_name_expr).expect("variant name should lower to Name");
        let variant_sym = nodes.name(variant_name);
        if !enum_variants(self.backend.db, *enum_ty).iter().any(|(name, _)| *name == variant_sym) {
            return false;
        }
        if base == ExprId::ZERO {
            return true;
        }

        matches!(self.resolve_name_ty(base), Some(base_ty) if base_ty == ty)
    }

    fn is_enum_variant_ref(&mut self, expr: ExprId) -> bool {
        let nodes = self.function.node_store();
        if nodes.node_kind(expr) != NodeKind::Field {
            return false;
        }

        let Some(ty) = self.lowerable_expr_ty(expr) else {
            return false;
        };
        if !matches!(ty.kind(self.backend.db), TyKind::Enum(_)) {
            return false;
        }

        let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return true;
        }
        self.resolve_name_ty(base).is_some()
    }

    fn is_enum_variant_constructor_call(&mut self, call_expr: ExprId, callee: ExprId) -> bool {
        let nodes = self.function.node_store();
        if nodes.node_kind(callee) != NodeKind::Field {
            return false;
        }

        let Some(ty) = self.lowerable_expr_ty(call_expr) else {
            return false;
        };
        if !matches!(ty.kind(self.backend.db), TyKind::Enum(_)) {
            return false;
        }

        let (base, _) = nodes.field(nodes.as_field(callee).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return true;
        }
        self.resolve_name_ty(base).is_some()
    }
}

struct Validator<'a, 'db> {
    backend: &'a mut Backend<'db>,
    owner_instance: InstanceKey<'db>,
    location: FunctionLocation<'db>,
    function: &'db Function<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    inference: &'db mitki_typeck::infer::Inference<'db>,
    resolver: Resolver<'db>,
    pending: &'a mut VecDeque<ReachableInstance<'db>>,
    edges: &'a mut Vec<plan::ReachabilityEdge<'db>>,
    seen_edges: &'a mut FxHashSet<plan::ReachabilityEdge<'db>>,
    current_node: plan::ReachabilityNode<'db>,
    loop_depth: usize,
}

impl<'a, 'db> Validator<'a, 'db> {
    fn expr_diagnostic(&self, expr: ExprId, message: impl Into<String>) -> Diagnostic {
        self.backend.diagnostic_at_function(self.location, message.into(), self.node_range(expr))
    }

    fn push_expr_error(&mut self, expr: ExprId, message: impl Into<String>) {
        self.backend.diagnostics.push(self.expr_diagnostic(expr, message));
    }

    fn record_edge(&mut self, to: plan::ReachabilityNode<'db>, kind: plan::ReachabilityEdgeKind) {
        let edge = plan::ReachabilityEdge { from: self.current_node.clone(), to, kind };
        if self.seen_edges.insert(edge.clone()) {
            self.edges.push(edge);
        }
    }

    fn enqueue_function(&mut self, instance: InstanceKey<'db>, kind: plan::ReachabilityEdgeKind) {
        self.record_edge(plan::ReachabilityNode::Function(instance.clone()), kind);
        self.pending.push_back(ReachableInstance::Function(instance));
    }

    fn enqueue_closure(
        &mut self,
        closure: ClosureInstanceKey<'db>,
        kind: plan::ReachabilityEdgeKind,
    ) {
        self.record_edge(plan::ReachabilityNode::Closure(closure.clone()), kind);
        self.pending.push_back(ReachableInstance::Closure(closure));
    }

    fn expr(&mut self, expr: ExprId, position: ExprPosition) {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::Name => self.name(expr, position),
            NodeKind::True | NodeKind::False => {}
            NodeKind::Int => {
                if let Err(message) = parse_int_literal(
                    nodes.int(nodes.as_int(expr).expect("Int node mismatch")),
                    self.backend.db,
                ) {
                    self.push_expr_error(expr, message);
                }
            }
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.expr(item, ExprPosition::Value);
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                for stmt in stmts.iter() {
                    self.stmt(stmt);
                }
                if tail != ExprId::ZERO {
                    self.expr(tail, ExprPosition::Value);
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                if self.is_enum_variant_constructor_call(expr, callee) {
                } else if nodes.node_kind(callee) == NodeKind::Name
                    && let Some(BindingId::CompilerIntrinsic(intrinsic)) = self.resolve_name(callee)
                {
                    self.validate_compiler_intrinsic_call(
                        expr,
                        intrinsic,
                        args.iter().collect::<Vec<_>>().as_slice(),
                    );
                } else if let Some(method) = self.resolve_method_call(callee) {
                    let lowered =
                        method.function.hir_function(self.backend.db).function(self.backend.db);
                    if self.backend.is_stage_mode()
                        && matches!(
                            lowered.linkage(),
                            WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
                        )
                    {
                        self.push_expr_error(
                            expr,
                            "comptime functions cannot call imported Wasm functions",
                        );
                    }

                    let mut call_args = Vec::with_capacity(args.len() + 1);
                    call_args.push(method.receiver);
                    call_args.extend(args.iter());
                    match self.backend.instance_for_call(
                        &self.owner_instance,
                        self.inference,
                        method.function,
                        expr,
                        call_args.as_slice(),
                        self.node_range(expr),
                    ) {
                        Ok(instance) => {
                            self.enqueue_function(instance, plan::ReachabilityEdgeKind::DirectCall);
                        }
                        Err(diagnostic) => self.backend.diagnostics.push(diagnostic),
                    }
                    self.expr(method.receiver, ExprPosition::Value);
                    for arg in args.iter() {
                        self.expr(arg, ExprPosition::Value);
                    }
                } else if nodes.node_kind(callee) == NodeKind::Name {
                    match self.resolve_name(callee) {
                        Some(BindingId::RuntimeFunction(_function)) => {
                            if self.backend.is_stage_mode() {
                                self.push_expr_error(
                                    expr,
                                    "comptime functions cannot call runtime imports",
                                );
                            }
                        }
                        Some(BindingId::Function(function)) => {
                            let lowered =
                                function.hir_function(self.backend.db).function(self.backend.db);
                            if self.backend.is_stage_mode()
                                && matches!(
                                    lowered.linkage(),
                                    WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
                                )
                            {
                                self.push_expr_error(
                                    expr,
                                    "comptime functions cannot call imported Wasm functions",
                                );
                            }
                            match self.backend.instance_for_call(
                                &self.owner_instance,
                                self.inference,
                                function,
                                expr,
                                args.iter().collect::<Vec<_>>().as_slice(),
                                self.node_range(expr),
                            ) {
                                Ok(instance) => {
                                    self.enqueue_function(
                                        instance,
                                        plan::ReachabilityEdgeKind::DirectCall,
                                    );
                                }
                                Err(diagnostic) => self.backend.diagnostics.push(diagnostic),
                            }
                        }
                        Some(BindingId::Local(_) | BindingId::Param(_)) => {
                            self.validate_indirect_call(callee)
                        }
                        Some(BindingId::CompilerIntrinsic(_)) => self.push_expr_error(
                            expr,
                            "internal error: Backend intrinsic call validation fell through",
                        ),
                        Some(BindingId::Struct(_))
                        | Some(BindingId::Enum(_))
                        | Some(BindingId::BuiltinType(_))
                        | Some(BindingId::EnumVariant(_))
                        | None => self.validate_indirect_call(callee),
                    }
                } else {
                    self.validate_indirect_call(callee);
                }
                if !matches!(nodes.node_kind(callee), NodeKind::Name)
                    || !matches!(self.resolve_name(callee), Some(BindingId::CompilerIntrinsic(_)))
                {
                    for arg in args.iter() {
                        self.expr(arg, ExprPosition::Value);
                    }
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.expr(binary.lhs, ExprPosition::Value);
                self.expr(binary.rhs, ExprPosition::Value);
                self.validate_binary(expr, binary.lhs, binary.op, binary.rhs);
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                self.expr(prefix.expr, ExprPosition::Value);
                self.validate_prefix(expr, prefix.op, prefix.expr);
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                self.require_specific_type(
                    if_expr.cond,
                    BackendTy::Bool,
                    "Wasm backend requires `if` conditions to have type `bool`",
                );
                self.expr(if_expr.cond, ExprPosition::Value);
                if if_expr.then_branch != ExprId::ZERO {
                    self.expr(if_expr.then_branch, ExprPosition::Value);
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.expr(if_expr.else_branch, ExprPosition::Value);
                }
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(expr).expect("Match node mismatch"));
                self.expr(scrutinee, ExprPosition::Value);
                for arm in arms.iter() {
                    let (pattern, body) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    for name in nodes.pattern_binding_names(pattern) {
                        self.require_backend_type(
                            name.into(),
                            "Wasm backend does not support this pattern binding type",
                        );
                    }
                    self.expr(body, ExprPosition::Value);
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.loop_depth += 1;
                    self.expr(body, ExprPosition::Value);
                    self.loop_depth -= 1;
                }
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(expr).expect("UnsafeBlock mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body, ExprPosition::Value);
                }
            }
            NodeKind::BreakExpr | NodeKind::ContinueExpr => {
                if self.loop_depth == 0 {
                    self.internal_error(
                        expr,
                        "internal error: loop control reached Wasm validation outside a loop",
                    );
                }
            }
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(expr).expect("LocalVar node mismatch"));
                for name in nodes.pattern_binding_names(var.pattern) {
                    self.require_backend_type(
                        name.into(),
                        "Wasm backend does not support this local binding type",
                    );
                }
                if var.initializer != ExprId::ZERO {
                    self.expr(var.initializer, ExprPosition::Value);
                }
            }
            NodeKind::Array => {
                let array = nodes.array(nodes.as_array(expr).expect("Array node mismatch"));
                for item in array.iter() {
                    self.expr(item, ExprPosition::Value);
                }
            }
            NodeKind::ArrayRepeat => {
                let (value, len) =
                    nodes.array_repeat(nodes.as_array_repeat(expr).expect("ArrayRepeat mismatch"));
                self.require_specific_type(
                    len,
                    BackendTy::Int,
                    "Wasm backend requires array repeat lengths to have type `int`",
                );
                self.expr(value, ExprPosition::Value);
                self.expr(len, ExprPosition::Value);
            }
            NodeKind::Float => {
                let literal = nodes.float(nodes.as_float(expr).expect("Float node mismatch"));
                if let Err(message) = parse_float_literal(literal, self.backend.db) {
                    self.push_expr_error(expr, message);
                }
            }
            NodeKind::String => {
                let literal = nodes.string(nodes.as_string(expr).expect("String node mismatch"));
                if let Err(message) = decode_string_literal(literal, self.backend.db) {
                    self.push_expr_error(expr, message);
                }
            }
            NodeKind::Char => {
                let literal = nodes.char(nodes.as_char(expr).expect("Char node mismatch"));
                if let Err(message) = decode_char_literal(literal, self.backend.db) {
                    self.push_expr_error(expr, message);
                }
            }
            NodeKind::Field => {
                if self.is_enum_variant_ref(expr) {
                    return;
                }

                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.expr(base, ExprPosition::Value);
                }
            }
            NodeKind::Postfix => self
                .internal_error(expr, "internal error: postfix operator reached Wasm validation"),
            NodeKind::Closure => {
                let closure = ClosureInstanceKey {
                    owner: self.location,
                    type_args: self.owner_instance.type_args.clone(),
                    closure: expr,
                };
                match self.backend.closure_info(&closure, self.function, self.inference) {
                    Ok(_info) => {
                        self.enqueue_closure(closure, plan::ReachabilityEdgeKind::ClosureLiteral)
                    }
                    Err(diagnostic) => self.backend.diagnostics.push(diagnostic),
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.expr(items.get(index).unwrap(), ExprPosition::Value);
                    index += 2;
                }
            }
            NodeKind::Error => self
                .internal_error(expr, "internal error: invalid expression reached Wasm validation"),
            kind => self.internal_error(
                expr,
                format!("internal error: unsupported `{kind:?}` node reached Wasm validation"),
            ),
        }
    }

    fn stmt(&mut self, stmt: StmtId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var =
                    nodes.local_var(nodes.as_local_var(stmt).expect("LocalVar node mismatch"));
                for name in nodes.pattern_binding_names(var.pattern) {
                    self.require_backend_type(
                        name.into(),
                        "Wasm backend does not support this local binding type",
                    );
                }
                if var.initializer != ExprId::ZERO {
                    self.expr(var.initializer, ExprPosition::Value);
                }
            }
            NodeKind::ReturnStmt => {
                let (value, _) =
                    nodes.return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
                if value != ExprId::ZERO {
                    self.expr(value, ExprPosition::Value);
                }
            }
            _ => {
                if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    self.expr(expr, ExprPosition::Value);
                }
            }
        }
    }

    fn name(&mut self, expr: ExprId, _position: ExprPosition) {
        let Some(resolution) = self.resolve_name(expr) else {
            return;
        };

        match resolution {
            BindingId::Local(_) | BindingId::Param(_) => {
                self.require_backend_type(expr, "Wasm backend does not support this value type");
            }
            BindingId::Function(function) => {
                match self.backend.instance_for_function_value(
                    &self.owner_instance,
                    self.inference,
                    function,
                    expr,
                    self.node_range(expr),
                ) {
                    Ok(instance) => {
                        self.enqueue_function(instance, plan::ReachabilityEdgeKind::FunctionValue);
                    }
                    Err(diagnostic) => self.backend.diagnostics.push(diagnostic),
                }
            }
            BindingId::RuntimeFunction(_) => self.push_expr_error(
                expr,
                "Wasm backend does not support using runtime functions as values",
            ),
            BindingId::CompilerIntrinsic(intrinsic) => self.push_expr_error(
                expr,
                format!(
                    "Wasm backend does not support using Backend intrinsic `{}` as a value",
                    intrinsic.source_name()
                ),
            ),
            BindingId::Struct(_)
            | BindingId::Enum(_)
            | BindingId::EnumVariant(_)
            | BindingId::BuiltinType(_) => {
                self.push_expr_error(expr, "Wasm backend does not support using types as values")
            }
        }
    }

    fn validate_binary(&mut self, expr: ExprId, lhs: ExprId, op: ExprId, rhs: ExprId) {
        let nodes = self.function.node_store();
        let op_sym = nodes.name(nodes.as_name(op).expect("op should be Name"));
        let op_text = op_sym.text(self.backend.db);
        let lhs_ty = self.expr_abi(lhs);
        let rhs_ty = self.expr_abi(rhs);

        let ok = match op_text {
            "+" | "-" | "*" | "/" => {
                (matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int)))
                    && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int))))
                    || (matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Float)))
                        && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Float))))
            }
            "%" => {
                matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int)))
                    && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int)))
            }
            "<" | ">" | "<=" | ">=" => {
                matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int)))
                    && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int)))
                    || (matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Float)))
                        && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Float))))
                    || (matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Char)))
                        && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Char))))
            }
            "==" | "!=" => match (lhs_ty.as_ref(), rhs_ty.as_ref()) {
                (Some(AbiTy::Scalar(BackendTy::Int)), Some(AbiTy::Scalar(BackendTy::Int)))
                | (Some(AbiTy::Scalar(BackendTy::Bool)), Some(AbiTy::Scalar(BackendTy::Bool)))
                | (Some(AbiTy::Scalar(BackendTy::Float)), Some(AbiTy::Scalar(BackendTy::Float)))
                | (Some(AbiTy::Scalar(BackendTy::Char)), Some(AbiTy::Scalar(BackendTy::Char)))
                | (Some(AbiTy::Scalar(BackendTy::Unit)), Some(AbiTy::Scalar(BackendTy::Unit))) => {
                    true
                }
                (
                    Some(AbiTy::Scalar(BackendTy::Ref(RefKind::String))),
                    Some(AbiTy::Scalar(BackendTy::Ref(RefKind::String))),
                ) => true,
                (
                    Some(AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(lhs_bits)))),
                    Some(AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(rhs_bits)))),
                ) if lhs_bits == rhs_bits => true,
                (
                    Some(AbiTy::Scalar(BackendTy::Ref(RefKind::Array(lhs_bits)))),
                    Some(AbiTy::Scalar(BackendTy::Ref(RefKind::Array(rhs_bits)))),
                ) if lhs_bits == rhs_bits => true,
                (Some(AbiTy::Aggregate(lhs)), Some(AbiTy::Aggregate(rhs))) if lhs == rhs => true,
                _ => false,
            },
            "&&" | "||" => {
                matches!(lhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Bool)))
                    && matches!(rhs_ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Bool)))
            }
            _ => false,
        };

        if !ok {
            self.push_expr_error(
                expr,
                format!("Wasm backend does not support `{op_text}` for this expression"),
            );
        }
    }

    fn validate_prefix(&mut self, expr: ExprId, op: ExprId, inner: ExprId) {
        let nodes = self.function.node_store();
        let op_sym = nodes.name(nodes.as_name(op).expect("op should be Name"));
        let op_text = op_sym.text(self.backend.db);
        let ty = self.expr_abi(inner);
        let ok = match op_text {
            "!" => matches!(ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Bool))),
            "-" => {
                matches!(ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Int)))
                    || matches!(ty.as_ref(), Some(AbiTy::Scalar(BackendTy::Float)))
            }
            _ => false,
        };

        if !ok {
            self.push_expr_error(
                expr,
                format!("Wasm backend does not support unary `{op_text}` for this expression"),
            );
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

    fn resolve_name_ty(&mut self, expr: ExprId) -> Option<Ty<'db>> {
        let nodes = self.function.node_store();
        let name = nodes.as_name(expr)?;
        let symbol = nodes.name(name);
        let guard = self.resolver.scopes_for_node(expr);
        let ty = self
            .resolver
            .resolve_type_binding(symbol)
            .and_then(|binding| self.resolver.ty_for_binding(binding));
        self.resolver.reset(guard);
        ty
    }

    fn resolve_method_call(&mut self, callee: ExprId) -> Option<ResolvedMethodCall<'db>> {
        let nodes = self.function.node_store();
        let field = nodes.as_field(callee)?;
        let (receiver, field_name_expr) = nodes.field(field);
        if receiver == ExprId::ZERO {
            return None;
        }

        let field_name = nodes.as_name(field_name_expr)?;
        let receiver_ty =
            self.backend.specialized_expr_ty(&self.owner_instance, self.inference, receiver)?;
        resolve_method_for_receiver(self.backend.db, receiver_ty, nodes.name(field_name))
            .map(|method| ResolvedMethodCall { receiver, function: method.function })
    }

    fn validate_indirect_call(&mut self, callee: ExprId) {
        self.expr(callee, ExprPosition::Value);
        let Some(ty) =
            self.backend.specialized_expr_ty(&self.owner_instance, self.inference, callee)
        else {
            return;
        };
        if !matches!(ty.kind(self.backend.db), TyKind::Function { .. }) {
            self.push_expr_error(callee, "Wasm backend requires callees to have a function type");
        }
    }

    fn validate_compiler_intrinsic_call(
        &mut self,
        expr: ExprId,
        intrinsic: CompilerIntrinsic,
        args: &[ExprId],
    ) {
        if intrinsic == CompilerIntrinsic::Comptime {
            if let Some(arg) = args.first().copied() {
                self.expr(arg, ExprPosition::Value);
            }
            return;
        }

        if intrinsic.is_reflection() {
            if !self.backend.is_stage_mode() {
                self.push_expr_error(
                    expr,
                    format!(
                        "`{}` is only supported during `comptime` execution",
                        intrinsic.source_name()
                    ),
                );
                return;
            }
            if args.len() > 1 {
                self.expr(args[1], ExprPosition::Value);
            }
            return;
        }

        if self.backend.is_stage_mode() {
            self.push_expr_error(
                expr,
                format!(
                    "`{}` is not supported during `comptime` execution",
                    intrinsic.source_name()
                ),
            );
            return;
        }

        for &arg in args {
            self.expr(arg, ExprPosition::Value);
        }

        if intrinsic == CompilerIntrinsic::StackAlloc
            && let Some(&count_expr) = args.first()
        {
            let nodes = self.function.node_store();
            if nodes.node_kind(count_expr) != NodeKind::Int {
                self.push_expr_error(
                    count_expr,
                    "`stack_alloc` currently requires a constant integer count",
                );
            } else if let Err(message) = parse_int_literal(
                nodes.int(nodes.as_int(count_expr).expect("Int node mismatch")),
                self.backend.db,
            ) {
                self.push_expr_error(count_expr, message);
            }
        }
    }

    fn expr_abi(&mut self, expr: ExprId) -> Option<AbiTy> {
        self.lowerable_expr_ty(expr)
            .and_then(|ty| crate::capability::supported_value_abi(self.backend.db, ty))
    }

    fn require_backend_type(&mut self, expr: ExprId, message: &str) {
        let Some(ty) = self.lowerable_expr_ty(expr) else {
            return;
        };
        if let Err(message) =
            crate::capability::supported_value_abi_or_message(self.backend.db, ty, message)
        {
            self.push_expr_error(expr, message);
        }
    }

    fn require_specific_type(&mut self, expr: ExprId, expected: BackendTy, message: &str) {
        if !matches!(self.expr_abi(expr).as_ref(), Some(AbiTy::Scalar(actual)) if *actual == expected)
        {
            self.push_expr_error(expr, message);
        }
    }

    fn lowerable_expr_ty(&mut self, expr: ExprId) -> Option<Ty<'db>> {
        let ty = self.backend.specialized_expr_ty(&self.owner_instance, self.inference, expr)?;
        Some(self.concrete_expr_member_ty(expr, ty).unwrap_or(ty))
    }

    fn concrete_expr_member_ty(&mut self, expr: ExprId, ty: Ty<'db>) -> Option<Ty<'db>> {
        let TyKind::Union(members) = ty.kind(self.backend.db) else {
            return None;
        };

        let mut compatible = Vec::new();
        for member in members {
            if self.expr_matches_ty(expr, *member) {
                compatible.push(*member);
            }
        }
        match compatible.as_slice() {
            [selected] => Some(*selected),
            _ => None,
        }
    }

    fn expr_matches_ty(&mut self, expr: ExprId, ty: Ty<'db>) -> bool {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::True | NodeKind::False => matches!(ty.kind(self.backend.db), TyKind::Bool),
            NodeKind::Int => matches!(ty.kind(self.backend.db), TyKind::Int | TyKind::ExactInt(_)),
            NodeKind::Float => matches!(ty.kind(self.backend.db), TyKind::Float),
            NodeKind::String => matches!(ty.kind(self.backend.db), TyKind::String),
            NodeKind::Char => matches!(ty.kind(self.backend.db), TyKind::Char),
            NodeKind::Tuple => {
                let items = nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch"));
                match ty.kind(self.backend.db) {
                    TyKind::Tuple(member_items) if member_items.len() == items.len() => items
                        .iter()
                        .zip(member_items.iter())
                        .all(|(item, member_item)| self.expr_matches_ty(item, *member_item)),
                    _ => false,
                }
            }
            NodeKind::StructExpr => {
                let items =
                    nodes.struct_expr(nodes.as_struct_expr(expr).expect("StructExpr mismatch"));
                let struct_name = items.iter().next().unwrap_or(ExprId::ZERO);
                if struct_name == ExprId::ZERO {
                    return matches!(ty.kind(self.backend.db), TyKind::Record(_));
                }

                matches!(self.resolve_name_ty(struct_name), Some(struct_ty) if struct_ty == ty)
            }
            NodeKind::Field => self.expr_is_enum_variant(expr, ty),
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                self.expr_is_enum_variant(callee, ty)
                    && matches!(ty.kind(self.backend.db), TyKind::Enum(enum_ty)
                        if enum_variants(self.backend.db, *enum_ty)
                            .iter()
                            .find(|(name, payload)| {
                                let (_, variant_name_expr) =
                                    nodes.field(nodes.as_field(callee).expect("variant call"));
                                let variant_name = nodes
                                    .as_name(variant_name_expr)
                                    .expect("variant name should lower to Name");
                                *name == nodes.name(variant_name) && payload.len() == args.len()
                            })
                            .is_some())
            }
            _ => false,
        }
    }

    fn expr_is_enum_variant(&mut self, expr: ExprId, ty: Ty<'db>) -> bool {
        let nodes = self.function.node_store();
        let TyKind::Enum(enum_ty) = ty.kind(self.backend.db) else {
            return false;
        };
        if nodes.node_kind(expr) != NodeKind::Field {
            return false;
        }

        let (base, field_name_expr) =
            nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        let variant_name =
            nodes.as_name(field_name_expr).expect("variant name should lower to Name");
        let variant_sym = nodes.name(variant_name);
        if !enum_variants(self.backend.db, *enum_ty).iter().any(|(name, _)| *name == variant_sym) {
            return false;
        }
        if base == ExprId::ZERO {
            return true;
        }

        matches!(self.resolve_name_ty(base), Some(base_ty) if base_ty == ty)
    }

    fn is_enum_variant_ref(&mut self, expr: ExprId) -> bool {
        let nodes = self.function.node_store();
        if nodes.node_kind(expr) != NodeKind::Field {
            return false;
        }

        let Some(ty) = self.lowerable_expr_ty(expr) else {
            return false;
        };
        if !matches!(ty.kind(self.backend.db), TyKind::Enum(_)) {
            return false;
        }

        let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return true;
        }
        self.resolve_name_ty(base).is_some()
    }

    fn is_enum_variant_constructor_call(&mut self, call_expr: ExprId, callee: ExprId) -> bool {
        let nodes = self.function.node_store();
        if nodes.node_kind(callee) != NodeKind::Field {
            return false;
        }

        let Some(ty) = self.lowerable_expr_ty(call_expr) else {
            return false;
        };
        if !matches!(ty.kind(self.backend.db), TyKind::Enum(_)) {
            return false;
        }

        let (base, _) = nodes.field(nodes.as_field(callee).expect("Field node mismatch"));
        if base == ExprId::ZERO {
            return true;
        }
        self.resolve_name_ty(base).is_some()
    }

    fn internal_error(&mut self, expr: ExprId, message: impl Into<String>) {
        self.push_expr_error(expr, message);
    }

    fn node_range(&self, expr: ExprId) -> mitki_errors::TextRange {
        self.source_map
            .try_node_syntax(expr)
            .map_or_else(|| self.backend.function_range(self.location), |ptr| ptr.range)
    }
}

fn function_param_binding_name<'db>(function: &Function<'db>, index: usize) -> Option<NameId> {
    let &param = function.params().get(index)?;
    let (pattern, _) = function.node_store().param(param);
    let binding = function.node_store().as_pat_binding(pattern)?;
    let (name, _) = function.node_store().pat_binding(binding);
    Some(name)
}

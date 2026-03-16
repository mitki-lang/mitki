use std::collections::VecDeque;

use mitki_inputs::PackageId;
use mitki_lower::HasPackageDecls as _;
use mitki_span::IntoSymbol as _;
use rustc_hash::FxHashSet;

use super::plan::{
    ReachabilityEdge, ReachabilityEdgeKind, ReachabilityGraph, ReachabilityInstance,
    ReachabilityNode, ReachabilityRoot,
};
use super::reachability::walk_reachable_instance;
use super::*;

pub(in crate::backend) struct ReachabilityBuilder<'a, 'db> {
    backend: &'a mut Backend<'db>,
    roots: Vec<ReachableInstance<'db>>,
    exports: Vec<ExportedFunction<'db>>,
    pending: VecDeque<ReachableInstance<'db>>,
    reachable: Vec<ReachableInstance<'db>>,
    seen: FxHashSet<ReachableInstance<'db>>,
    edges: Vec<ReachabilityEdge<'db>>,
    seen_edges: FxHashSet<ReachabilityEdge<'db>>,
}

impl<'a, 'db> ReachabilityBuilder<'a, 'db> {
    pub(in crate::backend) fn new(backend: &'a mut Backend<'db>) -> Self {
        Self {
            backend,
            roots: Vec::new(),
            exports: Vec::new(),
            pending: VecDeque::new(),
            reachable: Vec::new(),
            seen: FxHashSet::default(),
            edges: Vec::new(),
            seen_edges: FxHashSet::default(),
        }
    }

    pub(in crate::backend) fn build(mut self) -> ReachabilityGraph<'db> {
        self.roots = self.collect_roots();
        let root_edges = self
            .roots
            .iter()
            .enumerate()
            .map(|(index, root)| {
                (ReachabilityNode::Root(index), instance_node(root), ReachabilityEdgeKind::Root)
            })
            .collect::<Vec<_>>();
        for (from, to, kind) in root_edges {
            self.record_edge(from, to, kind);
        }
        self.pending.extend(self.roots.iter().cloned());

        while let Some(instance) = self.pending.pop_front() {
            self.visit_instance(&instance);
        }

        self.shadow_reachability_graph()
    }

    fn visit_instance(&mut self, instance: &ReachableInstance<'db>) {
        if !self.seen.insert(instance.clone()) {
            return;
        }

        self.reachable.push(instance.clone());
        self.backend.validate_function_signature(instance);
        walk_reachable_instance(
            &mut *self.backend,
            instance,
            &mut self.pending,
            &mut self.edges,
            &mut self.seen_edges,
        );
    }

    fn record_edge(
        &mut self,
        from: ReachabilityNode<'db>,
        to: ReachabilityNode<'db>,
        kind: ReachabilityEdgeKind,
    ) {
        let edge = ReachabilityEdge { from, to, kind };
        if self.seen_edges.insert(edge.clone()) {
            self.edges.push(edge);
        }
    }

    fn shadow_reachability_graph(&self) -> ReachabilityGraph<'db> {
        let roots = self
            .exports
            .iter()
            .map(|export| ReachabilityRoot {
                logical_name: export.name.text(self.backend.db).to_owned(),
                instance: export.instance.clone(),
            })
            .collect::<Vec<_>>();
        let instances = self
            .reachable
            .iter()
            .map(|instance| match instance {
                ReachableInstance::Function(instance) => {
                    ReachabilityInstance::Function(instance.clone())
                }
                ReachableInstance::Closure(instance) => {
                    ReachabilityInstance::Closure(instance.clone())
                }
            })
            .collect::<Vec<_>>();
        let functions = self
            .reachable
            .iter()
            .filter_map(|instance| match instance {
                ReachableInstance::Function(instance) => Some(instance.clone()),
                ReachableInstance::Closure(_) => None,
            })
            .collect::<Vec<_>>();
        let closures = self
            .reachable
            .iter()
            .filter_map(|instance| match instance {
                ReachableInstance::Function(_) => None,
                ReachableInstance::Closure(instance) => Some(instance.clone()),
            })
            .collect::<Vec<_>>();
        let imports = functions
            .iter()
            .filter(|instance| {
                let function =
                    instance.location.hir_function(self.backend.db).function(self.backend.db);
                matches!(
                    function.linkage(),
                    WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        let exports = self.exports.iter().map(|export| export.instance.clone()).collect::<Vec<_>>();
        let types = collect_reachable_types(self.backend, &self.reachable);
        let callable_signatures = collect_callable_signatures(self.backend, &functions, &closures);
        ReachabilityGraph {
            mode: self.backend.compilation_mode(),
            target_profile: self.backend.target_profile(),
            roots,
            instances,
            functions,
            closures,
            imports,
            exports,
            types,
            callable_signatures,
            edges: self.edges.clone(),
        }
    }

    fn collect_roots(&mut self) -> Vec<ReachableInstance<'db>> {
        if self.backend.compilation_mode().is_stage()
            && let Some(root) = self.backend.stage_root()
        {
            let instance = InstanceKey { location: root, type_args: Vec::new() };
            self.exports.push(ExportedFunction {
                instance: instance.clone(),
                name: "__mitki_stage_root".into_symbol(self.backend.db),
            });
            return vec![ReachableInstance::Function(instance)];
        }

        let mut roots = Vec::new();
        let mut exported_instances = FxHashSet::default();

        for declaration in PackageId::new(self.backend.db, self.backend.file)
            .package_decls(self.backend.db)
            .declarations()
        {
            match *declaration {
                Declaration::Function(function) => {
                    let hir_function = function.hir_function(self.backend.db);
                    let lowered = hir_function.function(self.backend.db);
                    let Some(name) = function
                        .source(self.backend.db)
                        .name()
                        .map(|name| name.as_str().into_symbol(self.backend.db))
                    else {
                        continue;
                    };

                    match lowered.linkage() {
                        WasmLinkage::Internal => {}
                        WasmLinkage::Import { .. } => {}
                        WasmLinkage::RawImport { .. } => {}
                        WasmLinkage::ImplicitMainExport | WasmLinkage::Export => {
                            let instance =
                                InstanceKey { location: function, type_args: Vec::new() };
                            if !exported_instances.insert(instance.clone()) {
                                continue;
                            }
                            self.exports
                                .push(ExportedFunction { instance: instance.clone(), name });
                            roots.push(ReachableInstance::Function(instance));
                        }
                    }
                }
                Declaration::BoundaryInstance(instance_decl) => {
                    if instance_decl.kind(self.backend.db) != BoundaryInstanceKind::Export {
                        continue;
                    }
                    let Some(instance) = self
                        .backend
                        .boundary_instance_key(BoundaryInstanceKind::Export, &instance_decl)
                    else {
                        continue;
                    };
                    if !exported_instances.insert(instance.clone()) {
                        continue;
                    }
                    let Some(name) = instance_decl
                        .source(self.backend.db)
                        .name()
                        .map(|name| name.as_str().into_symbol(self.backend.db))
                    else {
                        continue;
                    };
                    self.exports.push(ExportedFunction { instance: instance.clone(), name });
                    roots.push(ReachableInstance::Function(instance));
                }
                Declaration::Struct(_) | Declaration::Enum(_) => {}
            }
        }

        if self.exports.is_empty() {
            self.backend.diagnostics.push(Diagnostic::error(
                "Wasm backend requires a top-level `main` function or at least one `export fun`",
                self.backend.file_range(),
            ));
        }

        roots
    }
}

fn instance_node<'db>(instance: &ReachableInstance<'db>) -> ReachabilityNode<'db> {
    match instance {
        ReachableInstance::Function(instance) => ReachabilityNode::Function(instance.clone()),
        ReachableInstance::Closure(instance) => ReachabilityNode::Closure(instance.clone()),
    }
}

fn collect_callable_signatures<'db>(
    backend: &Backend<'db>,
    functions: &[InstanceKey<'db>],
    closures: &[ClosureInstanceKey<'db>],
) -> Vec<FunctionSignature> {
    let mut seen = FxHashSet::default();
    let mut signatures = Vec::new();

    for instance in functions {
        let hir_function = instance.location.hir_function(backend.db);
        let function = hir_function.function(backend.db);
        let inference = instance.location.infer(backend.db);
        let Ok(signature) = backend.function_signature(instance, function, inference) else {
            continue;
        };
        if seen.insert(signature.clone()) {
            signatures.push(signature);
        }
    }

    for closure in closures {
        let hir_function = closure.owner.hir_function(backend.db);
        let function = hir_function.function(backend.db);
        let inference = closure.owner.infer(backend.db);
        let Ok(info) = backend.closure_info(closure, function, inference) else {
            continue;
        };
        if seen.insert(info.signature.clone()) {
            signatures.push(info.signature);
        }
    }

    signatures
}

fn collect_reachable_types<'db>(
    backend: &Backend<'db>,
    reachable: &[ReachableInstance<'db>],
) -> Vec<Ty<'db>> {
    let mut seen = FxHashSet::default();
    let mut types = Vec::new();

    for instance in reachable {
        match instance {
            ReachableInstance::Function(instance) => {
                let hir_function = instance.location.hir_function(backend.db);
                let function = hir_function.function(backend.db);
                let inference = instance.location.infer(backend.db);
                let Ok((params, result)) =
                    backend.function_signature_types(instance, function, inference)
                else {
                    continue;
                };
                for ty in params {
                    record_type_recursive(backend.db, ty, &mut seen, &mut types);
                }
                record_type_recursive(backend.db, result, &mut seen, &mut types);
            }
            ReachableInstance::Closure(closure) => {
                let owner_instance = closure.owner_instance();
                let inference = closure.owner.infer(backend.db);
                let Some(closure_ty) =
                    backend.specialized_expr_ty(&owner_instance, inference, closure.closure)
                else {
                    continue;
                };
                let TyKind::Function { inputs, output } = closure_ty.kind(backend.db) else {
                    continue;
                };
                for &ty in inputs {
                    record_type_recursive(backend.db, ty, &mut seen, &mut types);
                }
                record_type_recursive(backend.db, *output, &mut seen, &mut types);
            }
        }
    }

    types
}

fn record_type_recursive<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    seen: &mut FxHashSet<Ty<'db>>,
    types: &mut Vec<Ty<'db>>,
) {
    if !seen.insert(ty) {
        return;
    }

    types.push(ty);
    match ty.kind(db) {
        TyKind::Array(item) => record_type_recursive(db, *item, seen, types),
        TyKind::Tuple(items) | TyKind::Union(items) | TyKind::Inter(items) => {
            for &item in items {
                record_type_recursive(db, item, seen, types);
            }
        }
        TyKind::Record(fields) => {
            for (_, field_ty) in fields {
                record_type_recursive(db, *field_ty, seen, types);
            }
        }
        TyKind::Pointer { pointee, .. } => record_type_recursive(db, *pointee, seen, types),
        TyKind::Function { inputs, output } => {
            for &input in inputs {
                record_type_recursive(db, input, seen, types);
            }
            record_type_recursive(db, *output, seen, types);
        }
        TyKind::Rec(_, body) => record_type_recursive(db, *body, seen, types),
        TyKind::Struct(struct_ty) | TyKind::ExternStruct(struct_ty) => {
            for (_, field_ty) in struct_fields(db, *struct_ty) {
                record_type_recursive(db, *field_ty, seen, types);
            }
        }
        TyKind::Enum(enum_ty) => {
            for (_, fields) in enum_variants(db, *enum_ty) {
                for field_ty in fields {
                    record_type_recursive(db, *field_ty, seen, types);
                }
            }
        }
        TyKind::Bool
        | TyKind::Float
        | TyKind::Int
        | TyKind::ExactInt(_)
        | TyKind::String
        | TyKind::Char
        | TyKind::Unknown
        | TyKind::Var(_) => {}
    }
}

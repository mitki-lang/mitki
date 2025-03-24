use Expectation::{ExpectHasType, NoExpectation};
use mitki_errors::Diagnostic;
use rustc_hash::FxHashMap;
use salsa::{Accumulator as _, Database};

use crate::hir::{Function, HasFunction as _, NodeId, NodeKind};
use crate::item::scope::FunctionLocation;
use crate::resolver::{PathResolution, Resolver};
use crate::ty::{Ty, TyKind};

pub(crate) trait Inferable<'db> {
    fn infer(self, db: &'db dyn Database) -> &'db Inference<'db>;
}

#[salsa::tracked]
impl<'db> Inferable<'db> for FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn infer(self, db: &'db dyn Database) -> Inference<'db> {
        InferenceBuilder {
            db,
            resolver: Resolver::new(db, self),
            inference: Inference::default(),
            function: self.hir_function(db),
            unit: Ty::new(db, TyKind::Tuple(Vec::new())),
            unknown: Ty::new(db, TyKind::Unknown),
        }
        .build()
    }
}

#[derive(Debug, Default, salsa::Update)]
pub(crate) struct Inference<'db> {
    type_of_node: FxHashMap<NodeId, Ty<'db>>,
}

pub(crate) struct InferenceBuilder<'db> {
    db: &'db dyn Database,
    resolver: Resolver<'db>,
    inference: Inference<'db>,
    function: &'db Function<'db>,

    unit: Ty<'db>,
    unknown: Ty<'db>,
}

impl<'db> InferenceBuilder<'db> {
    fn infer_node(&mut self, node: NodeId, expected: Expectation) -> Ty<'db> {
        let actual = self.infer_node_inner(node, expected);

        if let ExpectHasType(expected) = expected {
            if actual != expected {
                let node = if NodeKind::Block == self.function.node_kind(node) {
                    let (_, tail) = self.function.block_stmts(node);
                    if tail != NodeId::ZERO { tail } else { node }
                } else {
                    node
                };

                let range = self.function.node_syntax(&node).range;
                Diagnostic::error(format!("expected `{expected}`, found `{actual}`"), range)
                    .accumulate(self.db);
            }
        }

        actual
    }

    fn infer_node_inner(&mut self, node: NodeId, _expected: Expectation) -> Ty<'db> {
        let ty = match self.function.node_kind(node) {
            NodeKind::Int => Ty::new(self.db, TyKind::Int),
            NodeKind::Float => Ty::new(self.db, TyKind::Float),
            NodeKind::True | NodeKind::False => Ty::new(self.db, TyKind::Bool),
            NodeKind::LocalVar => {
                let var = self.function.local_var(node);

                let ty = self.infer_node(var.initializer, NoExpectation);
                self.inference.type_of_node.insert(var.name, ty);

                self.unit
            }
            NodeKind::Name => {
                let path = self.function.name(node);

                let guard = self.resolver.scopes_for_node(node);
                let resolution = self.resolver.resolve_path(path);
                self.resolver.reset(guard);

                if let Some(binding) = resolution {
                    match binding {
                        PathResolution::Local(binding) => self.inference.type_of_node[&binding],
                        PathResolution::Function(_) => Ty::new(self.db, TyKind::Function),
                    }
                } else {
                    let range = self.function.node_syntax(&node).range;

                    Diagnostic::error(format!("unresolved name `{}`", path.text(self.db)), range)
                        .accumulate(self.db);
                    Ty::new(self.db, TyKind::Unknown)
                }
            }
            NodeKind::Block => {
                let (stmts, tail) = self.function.block_stmts(node);

                for &stmt in stmts {
                    self.infer_node(stmt, NoExpectation);
                }

                if tail != NodeId::ZERO {
                    self.infer_node_inner(tail, _expected)
                } else {
                    self.unit
                }
            }
            _ => Ty::new(self.db, TyKind::Unknown),
        };
        self.inference.type_of_node.insert(node, ty);
        ty
    }

    fn build(mut self) -> Inference<'db> {
        if self.function.body() == NodeId::ZERO {
            return self.inference;
        }

        let ret_ty =
            if self.function.ret_type() == NodeId::ZERO { self.unit } else { self.unknown };

        self.infer_node(self.function.body(), ExpectHasType(ret_ty));
        self.inference
    }
}

#[derive(Clone, Copy)]
enum Expectation<'db> {
    ExpectHasType(Ty<'db>),
    NoExpectation,
}

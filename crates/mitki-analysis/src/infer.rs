use Expectation::{ExpectHasType, NoExpectation};
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::hir::{Function, HasFunction as _, NodeId, NodeKind};
use crate::item::scope::FunctionLocation;
use crate::resolver::{PathResolution, Resolver};
use crate::ty::{Ty, TyKind};

pub(crate) trait Inferable<'db> {
    fn infer(self, db: &'db dyn Database) -> &'db Inference<'db>;
}

#[salsa::tracked]
impl<'db> Inferable<'db> for FunctionLocation<'db> {
    #[salsa::tracked(returns(ref))]
    fn infer(self, db: &'db dyn Database) -> Inference<'db> {
        InferenceBuilder {
            db,
            resolver: Resolver::new(db, self),
            inference: Inference::default(),
            function: self.hir_function(db).function(db),
            unit: Ty::new(db, TyKind::Tuple(Vec::new())),
            unknown: Ty::new(db, TyKind::Unknown),
        }
        .build()
    }
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub(crate) struct Inference<'db> {
    type_of_node: FxHashMap<NodeId, Ty<'db>>,
    diagnostics: Vec<Diagnostic<'db>>,
}

impl<'db> Inference<'db> {
    pub(crate) fn diagnostics(&self) -> &[Diagnostic<'db>] {
        &self.diagnostics
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(crate) enum Diagnostic<'db> {
    UnresolvedIdent(NodeId),
    TypeMismatch(NodeId, Ty<'db>, Ty<'db>),
}

pub(crate) struct InferenceBuilder<'func, 'db> {
    db: &'db dyn Database,
    resolver: Resolver<'db>,
    inference: Inference<'db>,
    function: &'func Function<'db>,

    unit: Ty<'db>,
    unknown: Ty<'db>,
}

impl<'db> InferenceBuilder<'_, 'db> {
    fn infer_node(&mut self, node: NodeId, expected: Expectation<'db>) -> Ty<'db> {
        let actual_ty = self.infer_node_inner(node, expected);

        if let ExpectHasType(expected_ty) = expected
            && actual_ty != expected_ty
        {
            self.inference.diagnostics.push(Diagnostic::TypeMismatch(node, actual_ty, expected_ty));
        }

        actual_ty
    }

    fn infer_node_inner(&mut self, node: NodeId, _expected: Expectation) -> Ty<'db> {
        let ty = match self.function.node_kind(node) {
            NodeKind::Int => Ty::new(self.db, TyKind::Int),
            NodeKind::Float => Ty::new(self.db, TyKind::Float),
            NodeKind::String => Ty::new(self.db, TyKind::String),
            NodeKind::Char => Ty::new(self.db, TyKind::Char),
            NodeKind::True | NodeKind::False => Ty::new(self.db, TyKind::Bool),
            NodeKind::Tuple => {
                let tys = self
                    .function
                    .tuple(node)
                    .iter()
                    .map(|&item| self.infer_node(item, NoExpectation))
                    .collect();
                Ty::new(self.db, TyKind::Tuple(tys))
            }
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

                let Some(binding) = resolution else {
                    self.inference.diagnostics.push(Diagnostic::UnresolvedIdent(node));
                    return Ty::new(self.db, TyKind::Unknown);
                };

                match binding {
                    PathResolution::Local(binding) => self.inference.type_of_node[&binding],
                    PathResolution::Function(_) => Ty::new(self.db, TyKind::Function),
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
            NodeKind::Binary => {
                let binary = self.function.binary(node);
                self.infer_node(binary.lhs, NoExpectation);
                self.infer_node(binary.rhs, NoExpectation);
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::Postfix => {
                let postfix = self.function.postfix(node);
                self.infer_node(postfix.expr, NoExpectation);
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::Prefix => {
                let prefix = self.function.prefix(node);
                self.infer_node(prefix.expr, NoExpectation);
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::If => {
                let if_expr = self.function.if_expr(node);
                self.infer_node(if_expr.cond, NoExpectation);
                if if_expr.then_branch != NodeId::ZERO {
                    self.infer_node(if_expr.then_branch, NoExpectation);
                }
                if if_expr.else_branch != NodeId::ZERO {
                    self.infer_node(if_expr.else_branch, NoExpectation);
                }
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::Closure => {
                let (params, body) = self.function.closure_parts(node);
                for &param in params {
                    self.inference.type_of_node.insert(param, self.unknown);
                }
                if body != NodeId::ZERO {
                    self.infer_node(body, NoExpectation);
                }
                Ty::new(self.db, TyKind::Function)
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

        for &param in self.function.params() {
            self.inference.type_of_node.insert(param, self.unknown);
        }

        let ret_ty =
            if self.function.ret_type() == NodeId::ZERO { self.unit } else { self.unknown };

        self.infer_node(self.function.body(), ExpectHasType(ret_ty));

        self.inference
    }
}

#[derive(Clone, Copy, Debug)]
enum Expectation<'db> {
    ExpectHasType(Ty<'db>),
    NoExpectation,
}

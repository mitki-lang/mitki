use Expectation::{ExpectHasType, NoExpectation};
use mitki_hir::hir::{ExprId, Function, NodeKind, NodeStore, StmtId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::FunctionLocation;
use mitki_resolve::{Resolution, Resolver};
use rustc_hash::FxHashMap;
use salsa::Database;

pub trait Inferable<'db> {
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
pub struct Inference<'db> {
    type_of_node: FxHashMap<ExprId, Ty<'db>>,
    diagnostics: Vec<Diagnostic<'db>>,
}

impl<'db> Inference<'db> {
    pub fn diagnostics(&self) -> &[Diagnostic<'db>] {
        &self.diagnostics
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub enum Diagnostic<'db> {
    UnresolvedIdent(ExprId),
    TypeMismatch(ExprId, Ty<'db>, Ty<'db>),
    ExpectedValueFoundType(ExprId, Ty<'db>),
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
    fn infer_node(&mut self, node: ExprId, expected: Expectation<'db>) -> Ty<'db> {
        let actual_ty = self.infer_node_inner(node, expected);

        if let ExpectHasType(expected_ty) = expected
            && actual_ty != expected_ty
        {
            self.inference.diagnostics.push(Diagnostic::TypeMismatch(node, actual_ty, expected_ty));
        }

        actual_ty
    }

    fn infer_node_inner(&mut self, node: ExprId, _expected: Expectation) -> Ty<'db> {
        let nodes = self.function.node_store();
        let ty = match nodes.node_kind(node) {
            NodeKind::Int => Ty::new(self.db, TyKind::Int),
            NodeKind::Float => Ty::new(self.db, TyKind::Float),
            NodeKind::String => Ty::new(self.db, TyKind::String),
            NodeKind::Char => Ty::new(self.db, TyKind::Char),
            NodeKind::True | NodeKind::False => Ty::new(self.db, TyKind::Bool),
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(node).expect("Tuple node mismatch"));
                let tys = tuple.iter().map(|item| self.infer_node(item, NoExpectation)).collect();
                Ty::new(self.db, TyKind::Tuple(tys))
            }
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(node).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let ty = self.infer_node(var.initializer, NoExpectation);
                self.inference.type_of_node.insert(var.name.into(), ty);
                self.unit
            }
            NodeKind::Name => {
                let name_id = nodes.as_name(node).expect("Name node mismatch");
                let path = nodes.name(name_id);

                let guard = self.resolver.scopes_for_node(node);
                let resolution = self.resolver.resolve_path(path);
                self.resolver.reset(guard);

                let Some(resolution) = resolution else {
                    self.inference.diagnostics.push(Diagnostic::UnresolvedIdent(node));
                    return Ty::new(self.db, TyKind::Unknown);
                };

                match resolution {
                    Resolution::Local(binding) => self.inference.type_of_node[&binding.into()],
                    Resolution::Function(function) => {
                        let resolver = Resolver::new(self.db, function);
                        let signature = function.signature(self.db);

                        let nodes = signature.nodes(self.db);
                        let params = signature.params(self.db);
                        let ret_type = signature.ret_type(self.db);

                        let inputs = params
                            .iter()
                            .map(|&param| {
                                let (_, ty) = nodes.param(param);
                                let name =
                                    nodes.as_type_path(ty).map(|ty_id| nodes.type_ref(ty_id));

                                match name.and_then(|name| resolver.resolve_path(name)) {
                                    Some(Resolution::Type(ty)) => ty,
                                    _ => self.unknown,
                                }
                            })
                            .collect();

                        let output = nodes
                            .as_type_path(ret_type)
                            .map(|ty_id| nodes.type_ref(ty_id))
                            .and_then(|name| resolver.resolve_path(name))
                            .and_then(|res| match res {
                                Resolution::Type(ty) => Some(ty),
                                _ => None,
                            })
                            .unwrap_or(self.unknown);

                        Ty::new(self.db, TyKind::Function { inputs, output })
                    }
                    Resolution::Type(ty) => {
                        self.inference
                            .diagnostics
                            .push(Diagnostic::ExpectedValueFoundType(node, ty));
                        self.unknown
                    }
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(node).expect("Block node mismatch"));

                for stmt in stmts.iter() {
                    self.infer_stmt(stmt);
                }

                if tail != ExprId::ZERO {
                    self.infer_node_inner(tail, _expected)
                } else {
                    self.unit
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(node).expect("Binary node mismatch"));
                self.infer_node(binary.lhs, NoExpectation);
                self.infer_node(binary.rhs, NoExpectation);
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::Postfix => {
                let postfix = nodes.postfix(nodes.as_postfix(node).expect("Postfix node mismatch"));
                self.infer_node(postfix.expr, NoExpectation);
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(node).expect("Prefix node mismatch"));
                self.infer_node(prefix.expr, NoExpectation);
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(node).expect("If node mismatch"));
                self.infer_node(if_expr.cond, NoExpectation);
                if if_expr.then_branch != ExprId::ZERO {
                    self.infer_node(if_expr.then_branch, NoExpectation);
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.infer_node(if_expr.else_branch, NoExpectation);
                }
                Ty::new(self.db, TyKind::Unknown)
            }
            NodeKind::Closure => {
                let (params, body) =
                    nodes.closure_parts(nodes.as_closure(node).expect("Closure node mismatch"));
                for param in params.iter() {
                    let (name, _) = nodes.param(param);
                    self.inference.type_of_node.insert(name.into(), self.unknown);
                }
                if body != ExprId::ZERO {
                    self.infer_node(body, NoExpectation);
                }
                Ty::new(self.db, TyKind::Function { inputs: vec![], output: self.unknown })
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(node).expect("Call node mismatch"));
                let callee_ty = self.infer_node(callee, NoExpectation);

                if let TyKind::Function { inputs, output } = callee_ty.kind(self.db) {
                    if inputs.len() != args.len() {
                        let arg_tys = args
                            .iter()
                            .map(|arg| self.infer_node(arg, NoExpectation))
                            .collect::<Vec<_>>();

                        let actual_ty = Ty::new(
                            self.db,
                            TyKind::Function { inputs: arg_tys.clone(), output: *output },
                        );

                        self.inference
                            .diagnostics
                            .push(Diagnostic::TypeMismatch(node, actual_ty, callee_ty));
                    }
                    for (arg_node, &expected_ty) in args.iter().zip(inputs.iter()) {
                        self.infer_node(arg_node, ExpectHasType(expected_ty));
                    }

                    *output
                } else {
                    let expected_fn = Ty::new(
                        self.db,
                        TyKind::Function { inputs: Vec::new(), output: self.unknown },
                    );

                    self.inference.diagnostics.push(Diagnostic::TypeMismatch(
                        node,
                        callee_ty,
                        expected_fn,
                    ));
                    self.unknown
                }
            }
            _ => Ty::new(self.db, TyKind::Unknown),
        };
        self.inference.type_of_node.insert(node, ty);
        ty
    }

    fn infer_stmt(&mut self, stmt: StmtId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(stmt).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let ty = self.infer_node(var.initializer, NoExpectation);
                self.inference.type_of_node.insert(var.name.into(), ty);
            }
            _ => {
                if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    self.infer_node(expr, NoExpectation);
                }
            }
        }
    }

    fn build(mut self) -> Inference<'db> {
        if self.function.body() == ExprId::ZERO {
            return self.inference;
        }

        for &param in self.function.params() {
            let (name, _) = self.function.node_store().param(param);
            self.inference.type_of_node.insert(name.into(), self.unknown);
        }

        let ret_ty = if self.function.ret_type() == TyId::ZERO { self.unit } else { self.unknown };

        self.infer_node(self.function.body(), ExpectHasType(ret_ty));

        self.inference
    }
}

fn stmt_as_expr(nodes: &NodeStore<'_>, stmt: StmtId) -> Option<ExprId> {
    match nodes.node_kind(stmt) {
        NodeKind::Name => nodes.as_name(stmt).map(Into::into),
        NodeKind::True => nodes.as_true(stmt).map(Into::into),
        NodeKind::False => nodes.as_false(stmt).map(Into::into),
        NodeKind::Error => nodes.as_error(stmt).map(Into::into),
        NodeKind::Int => nodes.as_int(stmt).map(Into::into),
        NodeKind::Float => nodes.as_float(stmt).map(Into::into),
        NodeKind::String => nodes.as_string(stmt).map(Into::into),
        NodeKind::Char => nodes.as_char(stmt).map(Into::into),
        NodeKind::Tuple => nodes.as_tuple(stmt).map(Into::into),
        NodeKind::Call => nodes.as_call(stmt).map(Into::into),
        NodeKind::Binary => nodes.as_binary(stmt).map(Into::into),
        NodeKind::Postfix => nodes.as_postfix(stmt).map(Into::into),
        NodeKind::Prefix => nodes.as_prefix(stmt).map(Into::into),
        NodeKind::If => nodes.as_if(stmt).map(Into::into),
        NodeKind::Closure => nodes.as_closure(stmt).map(Into::into),
        NodeKind::Block => nodes.as_block(stmt).map(Into::into),
        _ => None,
    }
}

#[derive(Clone, Copy, Debug)]
enum Expectation<'db> {
    ExpectHasType(Ty<'db>),
    NoExpectation,
}

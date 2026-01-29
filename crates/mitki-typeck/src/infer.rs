use mitki_hir::hir::{ExprId, Function, NodeKind, NodeStore, StmtId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::FunctionLocation;
use mitki_resolve::{Resolution, Resolver};
use mitki_span::Symbol;
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
            context: Vec::new(),
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
pub struct Diagnostic<'db> {
    kind: DiagnosticKind<'db>,
    context: Option<ExprId>,
}

impl<'db> Diagnostic<'db> {
    fn new(kind: DiagnosticKind<'db>, context: Option<ExprId>) -> Self {
        Self { kind, context }
    }

    pub fn kind(&self) -> &DiagnosticKind<'db> {
        &self.kind
    }

    pub fn context(&self) -> Option<ExprId> {
        self.context
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub enum DiagnosticKind<'db> {
    UnresolvedIdent(ExprId),
    UnresolvedType(TyId, Symbol<'db>),
    TypeMismatch(ExprId, Ty<'db>, Ty<'db>),
    ExpectedValueFoundType(ExprId, Ty<'db>),
    ClosureArityMismatch(ExprId, usize, usize),
}

pub(crate) struct InferenceBuilder<'func, 'db> {
    db: &'db dyn Database,
    resolver: Resolver<'db>,
    inference: Inference<'db>,
    function: &'func Function<'db>,
    context: Vec<ExprId>,

    unit: Ty<'db>,
    unknown: Ty<'db>,
}

impl<'db> InferenceBuilder<'_, 'db> {
    fn infer_expr(&mut self, node: ExprId) -> Ty<'db> {
        self.typecheck_expr(node, ExpectedType::None)
    }

    fn check_expr(&mut self, node: ExprId, expected: Ty<'db>) -> Ty<'db> {
        self.typecheck_expr(node, ExpectedType::Known(expected))
    }

    fn typecheck_expr(&mut self, node: ExprId, expected: ExpectedType<'db>) -> Ty<'db> {
        let nodes = self.function.node_store();
        let kind = nodes.node_kind(node);
        if Self::is_context_node(kind) {
            self.with_context(node, |this| this.typecheck_expr_with_expectation(node, expected))
        } else {
            self.typecheck_expr_with_expectation(node, expected)
        }
    }

    fn typecheck_expr_with_expectation(
        &mut self,
        node: ExprId,
        expected: ExpectedType<'db>,
    ) -> Ty<'db> {
        let actual_ty = self.typecheck_expr_inner(node, expected);
        let ty = self.finish_expected_type(node, expected, actual_ty);
        self.inference.type_of_node.insert(node, ty);
        ty
    }

    fn finish_expected_type(
        &mut self,
        node: ExprId,
        expected: ExpectedType<'db>,
        actual: Ty<'db>,
    ) -> Ty<'db> {
        match expected {
            ExpectedType::Known(expected_ty) => {
                if self.is_unknown(expected_ty) {
                    return actual;
                }
                if self.is_unknown(actual) {
                    return expected_ty;
                }
                if actual != expected_ty {
                    self.emit(DiagnosticKind::TypeMismatch(node, actual, expected_ty));
                }
                expected_ty
            }
            ExpectedType::None => actual,
        }
    }

    fn is_unknown(&self, ty: Ty<'db>) -> bool {
        matches!(ty.kind(self.db), TyKind::Unknown)
    }

    fn emit(&mut self, kind: DiagnosticKind<'db>) {
        let context = self.context.last().copied();
        self.inference.diagnostics.push(Diagnostic::new(kind, context));
    }

    fn with_context<T>(&mut self, node: ExprId, f: impl FnOnce(&mut Self) -> T) -> T {
        self.context.push(node);
        let out = f(self);
        self.context.pop();
        out
    }

    fn is_context_node(kind: NodeKind) -> bool {
        matches!(kind, NodeKind::Tuple | NodeKind::If | NodeKind::Closure | NodeKind::Call)
    }

    fn typecheck_expr_inner(&mut self, node: ExprId, expected: ExpectedType<'db>) -> Ty<'db> {
        let nodes = self.function.node_store();
        match nodes.node_kind(node) {
            NodeKind::Int => Ty::new(self.db, TyKind::Int),
            NodeKind::Float => Ty::new(self.db, TyKind::Float),
            NodeKind::String => Ty::new(self.db, TyKind::String),
            NodeKind::Char => Ty::new(self.db, TyKind::Char),
            NodeKind::True | NodeKind::False => Ty::new(self.db, TyKind::Bool),
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(node).expect("Tuple node mismatch"));
                match expected {
                    ExpectedType::Known(expected_ty) => match expected_ty.kind(self.db) {
                        TyKind::Tuple(expected_items) if expected_items.len() == tuple.len() => {
                            let tys = tuple
                                .iter()
                                .zip(expected_items.iter())
                                .map(|(item, &expected_item)| self.check_expr(item, expected_item))
                                .collect();
                            Ty::new(self.db, TyKind::Tuple(tys))
                        }
                        _ => {
                            let tys = tuple.iter().map(|item| self.infer_expr(item)).collect();
                            Ty::new(self.db, TyKind::Tuple(tys))
                        }
                    },
                    ExpectedType::None => {
                        let tys = tuple.iter().map(|item| self.infer_expr(item)).collect();
                        Ty::new(self.db, TyKind::Tuple(tys))
                    }
                }
            }
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(node).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let ty = self.infer_expr(var.initializer);
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
                    self.emit(DiagnosticKind::UnresolvedIdent(node));
                    return self.unknown;
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
                        self.emit(DiagnosticKind::ExpectedValueFoundType(node, ty));
                        self.unknown
                    }
                }
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(node).expect("Block node mismatch"));

                for stmt in stmts.iter() {
                    self.typecheck_stmt(stmt);
                }

                if tail != ExprId::ZERO { self.typecheck_expr(tail, expected) } else { self.unit }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(node).expect("Binary node mismatch"));
                self.infer_expr(binary.lhs);
                self.infer_expr(binary.rhs);
                self.unknown
            }
            NodeKind::Postfix => {
                let postfix = nodes.postfix(nodes.as_postfix(node).expect("Postfix node mismatch"));
                self.infer_expr(postfix.expr);
                self.unknown
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(node).expect("Prefix node mismatch"));
                self.infer_expr(prefix.expr);
                self.unknown
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(node).expect("If node mismatch"));
                self.check_expr(if_expr.cond, Ty::new(self.db, TyKind::Bool));
                match expected {
                    ExpectedType::Known(expected_ty) => {
                        if if_expr.then_branch != ExprId::ZERO {
                            self.check_expr(if_expr.then_branch, expected_ty);
                        }
                        if if_expr.else_branch != ExprId::ZERO {
                            self.check_expr(if_expr.else_branch, expected_ty);
                        }
                        expected_ty
                    }
                    ExpectedType::None => {
                        if if_expr.then_branch != ExprId::ZERO {
                            self.infer_expr(if_expr.then_branch);
                        }
                        if if_expr.else_branch != ExprId::ZERO {
                            self.infer_expr(if_expr.else_branch);
                        }
                        self.unknown
                    }
                }
            }
            NodeKind::Closure => {
                let (params, body) =
                    nodes.closure_parts(nodes.as_closure(node).expect("Closure node mismatch"));
                let mut expected_closure = None;

                if let ExpectedType::Known(expected_ty) = expected
                    && let TyKind::Function { inputs, output } = expected_ty.kind(self.db)
                {
                    if inputs.len() == params.len() {
                        for (param, &input_ty) in params.iter().zip(inputs.iter()) {
                            let (name, ty_id) = nodes.param(param);
                            let annotated = self.resolve_type_annotation(ty_id);
                            let param_ty = if let Some(annotated_ty) = annotated {
                                if !self.is_unknown(annotated_ty)
                                    && !self.is_unknown(input_ty)
                                    && annotated_ty != input_ty
                                {
                                    self.emit(DiagnosticKind::TypeMismatch(
                                        name.into(),
                                        annotated_ty,
                                        input_ty,
                                    ));
                                }
                                annotated_ty
                            } else {
                                input_ty
                            };
                            self.inference.type_of_node.insert(name.into(), param_ty);
                        }
                        if body != ExprId::ZERO {
                            self.check_expr(body, *output);
                        }
                        expected_closure = Some(expected_ty);
                    } else {
                        self.emit(DiagnosticKind::ClosureArityMismatch(
                            node,
                            params.len(),
                            inputs.len(),
                        ));
                    }
                }

                if let Some(expected_ty) = expected_closure {
                    expected_ty
                } else {
                    let mut inputs = Vec::with_capacity(params.len());
                    for param in params.iter() {
                        let (name, ty_id) = nodes.param(param);
                        let annotated = self.resolve_type_annotation(ty_id);
                        let param_ty = annotated.unwrap_or(self.unknown);
                        self.inference.type_of_node.insert(name.into(), param_ty);
                        inputs.push(param_ty);
                    }
                    let output =
                        if body != ExprId::ZERO { self.infer_expr(body) } else { self.unknown };
                    Ty::new(self.db, TyKind::Function { inputs, output })
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(node).expect("Call node mismatch"));
                let callee_ty = self.infer_expr(callee);

                if let TyKind::Function { inputs, output } = callee_ty.kind(self.db) {
                    if inputs.len() != args.len() {
                        let arg_tys =
                            args.iter().map(|arg| self.infer_expr(arg)).collect::<Vec<_>>();

                        let actual_ty = Ty::new(
                            self.db,
                            TyKind::Function { inputs: arg_tys.clone(), output: *output },
                        );

                        self.emit(DiagnosticKind::TypeMismatch(node, actual_ty, callee_ty));
                    }
                    for (arg_node, &expected_ty) in args.iter().zip(inputs.iter()) {
                        self.check_expr(arg_node, expected_ty);
                    }

                    *output
                } else if self.is_unknown(callee_ty) {
                    self.unknown
                } else {
                    let expected_fn = Ty::new(
                        self.db,
                        TyKind::Function { inputs: Vec::new(), output: self.unknown },
                    );

                    self.emit(DiagnosticKind::TypeMismatch(node, callee_ty, expected_fn));
                    self.unknown
                }
            }
            _ => self.unknown,
        }
    }

    fn typecheck_stmt(&mut self, stmt: StmtId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(stmt).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let expected_ty = self.resolve_type_annotation(var.ty);

                let binding_ty = if var.initializer != ExprId::ZERO {
                    match expected_ty {
                        Some(expected) => {
                            self.check_expr(var.initializer, expected);
                            expected
                        }
                        None => self.infer_expr(var.initializer),
                    }
                } else {
                    expected_ty.unwrap_or(self.unknown)
                };

                self.inference.type_of_node.insert(var.name.into(), binding_ty);
            }
            _ => {
                if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    self.infer_expr(expr);
                }
            }
        }
    }

    fn resolve_type_annotation(&mut self, ty: TyId) -> Option<Ty<'db>> {
        if ty == TyId::ZERO {
            return None;
        }

        let nodes = self.function.node_store();
        let type_path = nodes.as_type_path(ty)?;
        let name = nodes.type_ref(type_path);

        let guard = self.resolver.scopes_for_type(ty);
        let resolved = if let Some(Resolution::Type(ty)) = self.resolver.resolve_path(name) {
            Some(ty)
        } else {
            self.emit(DiagnosticKind::UnresolvedType(ty, name));
            None
        };
        self.resolver.reset(guard);

        resolved
    }

    fn build(mut self) -> Inference<'db> {
        if self.function.body() == ExprId::ZERO {
            return self.inference;
        }

        for &param in self.function.params() {
            let (name, ty_id) = self.function.node_store().param(param);
            let param_ty = self.resolve_type_annotation(ty_id).unwrap_or(self.unknown);
            self.inference.type_of_node.insert(name.into(), param_ty);
        }

        let ret_ty = if self.function.ret_type() == TyId::ZERO {
            self.unit
        } else {
            self.resolve_type_annotation(self.function.ret_type()).unwrap_or(self.unknown)
        };

        self.check_expr(self.function.body(), ret_ty);

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
enum ExpectedType<'db> {
    Known(Ty<'db>),
    None,
}

use mitki_hir::arena::{Arena, Key, Range};
use mitki_hir::hir::{ExprId, Function, NameId, NodeKind, ParamId, StmtId, TyId};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::FunctionLocation;
use mitki_span::Symbol;
use rustc_hash::FxHashMap;

pub trait HasExprScopes<'db> {
    fn expr_scopes<DB>(self, db: &'db DB) -> ExprScopes<'db>
    where
        DB: mitki_parse::ParseDb;
}

impl<'db> HasExprScopes<'db> for FunctionLocation<'db> {
    fn expr_scopes<DB>(self, db: &'db DB) -> ExprScopes<'db>
    where
        DB: mitki_parse::ParseDb,
    {
        let hir = self.hir_function(db);
        ExprScopesBuilder { function: hir.function(), scopes: ExprScopes::default() }.build()
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct ExprScopes<'db> {
    scopes: Arena<ScopeData<'db>>,
    scope_entries: Arena<ScopeEntry<'db>>,
    scope_by_node: FxHashMap<StmtId, Scope<'db>>,
    scope_by_type: FxHashMap<TyId, Scope<'db>>,
}

impl<'db> ExprScopes<'db> {
    pub fn scope_by_node(&self, node: StmtId) -> Option<Scope<'db>> {
        self.scope_by_node.get(&node).copied()
    }

    pub(crate) fn chain(&self, scope: Option<Scope<'db>>) -> impl Iterator<Item = Scope<'db>> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    pub(crate) fn entries(&self, scope: Scope<'db>) -> &[ScopeEntry<'db>] {
        &self.scope_entries[self.scopes[scope].entries]
    }

    pub(crate) fn scope_for(&self, expr: ExprId) -> Option<Scope<'db>> {
        self.scope_by_node.get(&expr.into()).copied()
    }

    pub(crate) fn scope_for_ty(&self, ty: TyId) -> Option<Scope<'db>> {
        self.scope_by_type.get(&ty).copied()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ScopeEntry<'db> {
    pub(crate) name: Symbol<'db>,
    pub(crate) binding: NameId,
}

pub type Scope<'db> = Key<ScopeData<'db>>;

#[derive(Debug, PartialEq, Eq)]
pub struct ScopeData<'db> {
    parent: Option<Scope<'db>>,
    entries: Range<ScopeEntry<'db>>,
}

pub(crate) struct ExprScopesBuilder<'func, 'db> {
    function: &'func Function<'db>,
    scopes: ExprScopes<'db>,
}

fn empty_entries<'db>(idx: usize) -> Range<ScopeEntry<'db>> {
    let idx = Key::new(idx as u32);
    Range::new(idx, idx)
}

impl<'db> ExprScopesBuilder<'_, 'db> {
    fn root_scope(&mut self) -> Scope<'db> {
        self.scope(None)
    }

    fn scope(&mut self, parent: impl Into<Option<Scope<'db>>>) -> Scope<'db> {
        self.scopes.scopes.alloc(ScopeData {
            parent: parent.into(),
            entries: empty_entries(self.scopes.scope_entries.len()),
        })
    }

    #[track_caller]
    fn add_binding(&mut self, name: NameId, scope: Key<ScopeData<'db>>) {
        let symbol = self.function.node_store().name(name);
        let entry = self.scopes.scope_entries.alloc(ScopeEntry { name: symbol, binding: name });
        self.scopes.scopes[scope].entries =
            Range::new_inclusive(self.scopes.scopes[scope].entries.start, entry);
    }

    #[track_caller]
    fn add_type(&mut self, ty: TyId, scope: Key<ScopeData<'db>>) {
        if ty != TyId::ZERO {
            self.scopes.scope_by_type.insert(ty, scope);
            let nodes = self.function.node_store();
            if let Some(tuple_id) = nodes.as_type_tuple(ty) {
                for item in nodes.type_tuple(tuple_id) {
                    self.add_type(item, scope);
                }
            }
        }
    }

    #[track_caller]
    fn build_node_scopes(&mut self, node: StmtId, scope: &mut Scope<'db>) {
        let nodes = self.function.node_store();
        self.scopes.scope_by_node.insert(node, *scope);

        match nodes.node_kind(node) {
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(node).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                self.add_type(var.ty, *scope);
                if var.initializer != ExprId::ZERO {
                    self.build_node_scopes(var.initializer.into(), scope);
                }

                *scope = self.scope(*scope);
                self.add_binding(var.name, *scope);
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(node).expect("Call node mismatch"));
                self.build_node_scopes(callee.into(), scope);
                for arg in args.iter() {
                    self.build_node_scopes(arg.into(), scope);
                }
            }
            NodeKind::Field => {
                let (expr, _) = nodes.field(nodes.as_field(node).expect("Field node mismatch"));
                if expr != ExprId::ZERO {
                    self.build_node_scopes(expr.into(), scope);
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(node).expect("Binary node mismatch"));
                self.build_node_scopes(binary.lhs.into(), scope);
                self.build_node_scopes(binary.rhs.into(), scope);
            }
            NodeKind::Postfix => {
                let postfix = nodes.postfix(nodes.as_postfix(node).expect("Postfix node mismatch"));
                self.build_node_scopes(postfix.expr.into(), scope);
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(node).expect("Prefix node mismatch"));
                self.build_node_scopes(prefix.expr.into(), scope);
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(node).expect("If node mismatch"));
                self.build_node_scopes(if_expr.cond.into(), scope);
                if if_expr.then_branch != ExprId::ZERO {
                    self.build_node_scopes(if_expr.then_branch.into(), scope);
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.build_node_scopes(if_expr.else_branch.into(), scope);
                }
            }
            NodeKind::Closure => {
                let (params, body) =
                    nodes.closure_parts(nodes.as_closure(node).expect("Closure node mismatch"));
                let mut closure_scope = self.scope(*scope);
                self.add_bindings(params.iter(), closure_scope);
                if body != ExprId::ZERO {
                    self.build_node_scopes(body.into(), &mut closure_scope);
                }
            }
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(node).expect("Tuple node mismatch"));
                for item in tuple.iter() {
                    self.build_node_scopes(item.into(), scope);
                }
            }
            NodeKind::StructExpr => {
                let struct_expr = nodes
                    .struct_expr(nodes.as_struct_expr(node).expect("StructExpr node mismatch"));
                for item in struct_expr.iter() {
                    self.build_node_scopes(item.into(), scope);
                }
            }
            NodeKind::Block => {
                let scope = &mut self.scope(*scope);
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(node).expect("Block node mismatch"));

                for stmt in stmts.iter() {
                    self.build_node_scopes(stmt, scope);
                }

                if tail != ExprId::ZERO {
                    self.build_node_scopes(tail.into(), scope);
                }
            }
            _ => {}
        }
    }

    fn build(mut self) -> ExprScopes<'db> {
        let mut scope = self.root_scope();

        self.add_bindings(self.function.params().iter().copied(), scope);
        self.add_type(self.function.ret_type(), scope);
        if self.function.body() != ExprId::ZERO {
            self.build_node_scopes(self.function.body().into(), &mut scope);
        }

        self.scopes
    }

    #[track_caller]
    fn add_bindings(&mut self, params: impl IntoIterator<Item = ParamId>, scope: Scope<'db>) {
        let nodes = self.function.node_store();
        for param in params {
            let (name, ty_id) = nodes.param(param);
            self.add_type(ty_id, scope);
            self.add_binding(name, scope);
        }
    }
}

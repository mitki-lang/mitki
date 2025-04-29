use mitki_span::Symbol;
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::arena::{Arena, Key, Range};
use crate::hir::{Function, HasFunction as _, NodeId, NodeKind};
use crate::item::scope::FunctionLocation;

pub(crate) trait HasExprScopes<'db> {
    fn expr_scopes(self, db: &'db dyn Database) -> &'db ExprScopes<'db>;
}

#[salsa::tracked]
impl<'db> HasExprScopes<'db> for FunctionLocation<'db> {
    #[salsa::tracked(return_ref)]
    fn expr_scopes(self, db: &'db dyn Database) -> ExprScopes<'db> {
        ExprScopesBuilder {
            function: self.hir_function(db).function(db),
            scopes: ExprScopes::default(),
        }
        .build()
    }
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub(crate) struct ExprScopes<'db> {
    pub scopes: Arena<ScopeData<'db>>,
    pub scope_entries: Arena<ScopeEntry<'db>>,
    pub scope_by_node: FxHashMap<NodeId, Scope<'db>>,
}

impl<'db> ExprScopes<'db> {
    pub(crate) fn chain(&self, scope: Option<Scope<'db>>) -> impl Iterator<Item = Scope<'db>> + '_ {
        std::iter::successors(scope, move |&scope| self.scopes[scope].parent)
    }

    pub(crate) fn entries(&self, scope: Scope<'db>) -> &[ScopeEntry<'db>] {
        &self.scope_entries[self.scopes[scope].entries]
    }

    pub(crate) fn scope_for(&self, expr: NodeId) -> Option<Scope<'db>> {
        self.scope_by_node.get(&expr).copied()
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(crate) struct ScopeEntry<'db> {
    pub(crate) name: Symbol<'db>,
    pub(crate) binding: NodeId,
}

pub(crate) type Scope<'db> = Key<ScopeData<'db>>;

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(crate) struct ScopeData<'db> {
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
    fn add_binding(&mut self, name: NodeId, scope: Key<ScopeData<'db>>) {
        let symbol = self.function.name(name);
        let entry = self.scopes.scope_entries.alloc(ScopeEntry { name: symbol, binding: name });
        self.scopes.scopes[scope].entries =
            Range::new_inclusive(self.scopes.scopes[scope].entries.start, entry);
    }

    #[track_caller]
    fn build_node_scopes(&mut self, node: NodeId, scope: &mut Scope<'db>) {
        self.scopes.scope_by_node.insert(node, *scope);

        match self.function.node_kind(node) {
            NodeKind::LocalVar => {
                let var = self.function.local_var(node);
                self.build_node_scopes(var.initializer, scope);

                *scope = self.scope(*scope);
                self.add_binding(var.name, *scope);
            }
            NodeKind::Call => {}
            NodeKind::Block => {
                let scope = &mut self.scope(*scope);
                let (stmts, tail) = self.function.block_stmts(node);

                for &stmt in stmts {
                    self.build_node_scopes(stmt, scope);
                }

                if tail != NodeId::ZERO {
                    self.build_node_scopes(tail, scope);
                }
            }
            _ => {}
        }
    }

    fn build(mut self) -> ExprScopes<'db> {
        let mut scope = self.root_scope();

        self.add_bindings(self.function.params(), scope);
        if self.function.body() != NodeId::ZERO {
            self.build_node_scopes(self.function.body(), &mut scope);
        }

        self.scopes
    }

    #[track_caller]
    fn add_bindings(&mut self, params: &[NodeId], scope: Scope<'db>) {
        for &name in params {
            self.add_binding(name, scope);
        }
    }
}

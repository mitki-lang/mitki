use la_arena::{Arena, Idx, IdxRange, RawIdx};
use mitki_span::Symbol;
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::hir::{Function, HasFunction, NodeId, NodeKind};
use crate::item::scope::FunctionLocation;

pub(crate) trait HasExprScopes<'db> {
    fn expr_scopes(self, db: &'db dyn Database) -> &'db ExprScopes<'db>;
}

#[salsa::tracked]
impl<'db> HasExprScopes<'db> for FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn expr_scopes(self, db: &'db dyn Database) -> ExprScopes<'db> {
        ExprScopesBuilder::new(self.hir_function(db)).build()
    }
}

#[derive(Debug, Default)]
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
        &self.scope_entries[self.scopes[scope].entries.clone()]
    }

    pub(crate) fn scope_for(&self, expr: NodeId) -> Option<Scope<'db>> {
        self.scope_by_node.get(&expr).copied()
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct ScopeEntry<'db> {
    pub(crate) name: Symbol<'db>,
    pub(crate) binding: NodeId,
}

pub(crate) type Scope<'db> = Idx<ScopeData<'db>>;

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ScopeData<'db> {
    parent: Option<Scope<'db>>,
    entries: IdxRange<ScopeEntry<'db>>,
}

pub(crate) struct ExprScopesBuilder<'db> {
    function: &'db Function<'db>,
    scopes: ExprScopes<'db>,
}

fn empty_entries<'db>(idx: usize) -> IdxRange<ScopeEntry<'db>> {
    let idx = Idx::from_raw(RawIdx::from(idx as u32));
    IdxRange::new(idx..idx)
}

impl<'db> ExprScopesBuilder<'db> {
    pub(crate) fn new(body: &'db Function<'db>) -> Self {
        Self { function: body, scopes: ExprScopes::default() }
    }

    fn root_scope(&mut self) -> Scope<'db> {
        self.scope(None)
    }

    fn scope(&mut self, parent: impl Into<Option<Scope<'db>>>) -> Scope<'db> {
        self.scopes.scopes.alloc(ScopeData {
            parent: parent.into(),
            entries: empty_entries(self.scopes.scope_entries.len()),
        })
    }

    fn add_binding(&mut self, name: NodeId, scope: Idx<ScopeData<'db>>) {
        let symbol = self.function.binding_symbol(name);
        let entry = self.scopes.scope_entries.alloc(ScopeEntry { name: symbol, binding: name });
        self.scopes.scopes[scope].entries =
            IdxRange::new_inclusive(self.scopes.scopes[scope].entries.start()..=entry);
    }

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
                for &stmt in self.function.block_stmts(node) {
                    self.build_node_scopes(stmt, scope);
                }
            }
            NodeKind::Int | NodeKind::Float | NodeKind::True | NodeKind::False | NodeKind::Name => {
            }
        }
    }

    fn build(mut self) -> ExprScopes<'db> {
        let mut scope = self.root_scope();

        self.add_bindings(self.function.params(), scope);
        self.build_node_scopes(self.function.body(), &mut scope);

        self.scopes
    }

    fn add_bindings(&mut self, params: &[NodeId], scope: Scope<'db>) {
        for &name in params {
            self.add_binding(name, scope);
        }
    }
}

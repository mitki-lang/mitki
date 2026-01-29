use mitki_span::Symbol;

use super::id::Raw;
use super::schema::{HirId, IxId, NodeKind, SymId};

#[derive(Default, Debug, PartialEq, Eq, salsa::Update)]
pub struct NodeStore<'db> {
    pub(crate) nodes: Vec<Node>,
    pub(crate) symbols: Vec<Symbol<'db>>,
    pub(crate) node_ids: Vec<u32>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub(crate) struct Node {
    pub(crate) kind: NodeKind,
    pub(crate) lhs: Raw,
    pub(crate) rhs: Raw,
}

impl<'db> NodeStore<'db> {
    pub(crate) fn push_node(&mut self, kind: NodeKind, lhs: Raw, rhs: Raw) -> HirId {
        let index = self.nodes.len();
        self.nodes.push(Node { kind, lhs, rhs });
        HirId::new(index)
    }

    pub(crate) fn intern_symbol(&mut self, symbol: Symbol<'db>) -> SymId {
        let index = self.symbols.len();
        self.symbols.push(symbol);
        SymId::new(index)
    }

    #[track_caller]
    pub(crate) fn symbol(&self, symbol: SymId) -> Symbol<'db> {
        self.symbols[symbol.get()]
    }

    pub(crate) fn alloc_list(&mut self, iter: impl IntoIterator<Item = Raw>) -> (IxId, IxId) {
        let start = IxId::new(self.node_ids.len());
        self.node_ids.extend(iter.into_iter().map(|raw| raw.0));
        let end = IxId::new(self.node_ids.len());
        (start, end)
    }

    pub(crate) fn alloc_triple(&mut self, a: Raw, b: Raw, c: Raw) -> IxId {
        let start = self.node_ids.len();
        self.node_ids.push(a.0);
        self.node_ids.push(b.0);
        self.node_ids.push(c.0);
        IxId::new(start)
    }

    #[track_caller]
    pub(crate) fn node_kind_raw(&self, node: HirId) -> NodeKind {
        self.nodes[node.get()].kind
    }

    #[track_caller]
    pub(crate) fn node(&self, node: HirId) -> &Node {
        &self.nodes[node.get()]
    }
}

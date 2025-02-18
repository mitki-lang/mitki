use mitki_span::Symbol;
use mitki_yellow::{RedToken, ast};

#[derive(Default, Debug)]
pub(crate) struct NodeStore<'db> {
    nodes: Vec<Node>,
    symbols: Vec<Symbol<'db>>,
    node_ids: Vec<NodeId>,
}

impl<'db> NodeStore<'db> {
    pub(crate) fn alloc_binding(&mut self, symbol: Symbol<'db>) -> NodeId {
        self.symbols.push_with_index(symbol).into()
    }

    pub(crate) fn alloc_name(&mut self, symbol: Symbol<'db>) -> NodeId {
        let binding = self.alloc_binding(symbol);
        self.alloc(NodeKind::Name, binding, NodeId::ZERO)
    }

    pub(crate) fn alloc_literal(
        &mut self,
        db: &'db dyn salsa::Database,
        literal: &ast::Literal<'db>,
    ) -> NodeId {
        let (kind, lhs) = match literal.kind(db) {
            ast::LiteralKind::Bool(true) => (NodeKind::True, NodeId::ZERO),
            ast::LiteralKind::Bool(false) => (NodeKind::False, NodeId::ZERO),
            ast::LiteralKind::Int(token) => (NodeKind::Int, self.alloc_token(db, &token)),
            ast::LiteralKind::Float(token) => (NodeKind::Float, self.alloc_token(db, &token)),
        };

        self.alloc(kind, lhs, NodeId::ZERO)
    }

    fn alloc_token(&mut self, db: &'db dyn salsa::Database, token: &RedToken<'db>) -> NodeId {
        let text = Symbol::new(db, token.green().text_trimmed(db));
        self.alloc_binding(text)
    }

    pub(crate) fn symbol(&self, binding: NodeId) -> Symbol<'db> {
        self.symbols[binding.get()]
    }

    pub(crate) fn alloc_local_var(
        &mut self,
        name: NodeId,
        ty: NodeId,
        initializer: NodeId,
    ) -> NodeId {
        let lhs: NodeId = self.node_ids.push_with_index(name).into();
        let rhs = self.node_ids.push_with_index(ty).into();
        self.node_ids.push(initializer);
        self.alloc(NodeKind::LocalVar, lhs, rhs)
    }

    pub(crate) fn alloc_block(&mut self, stmts: Vec<NodeId>) -> NodeId {
        let start = self.node_ids.len().into();
        self.node_ids.extend(stmts);
        let end = self.node_ids.len().into();
        self.alloc(NodeKind::Block, start, end)
    }

    pub(crate) fn alloc_error(&mut self) -> NodeId {
        self.alloc(NodeKind::Error, NodeId::ZERO, NodeId::ZERO)
    }

    pub(crate) fn alloc_call(&mut self, callee: NodeId, _args: Vec<NodeId>) -> NodeId {
        let lhs = self.node_ids.push_with_index(callee).into();
        self.alloc(NodeKind::Call, lhs, NodeId::ZERO)
    }

    fn alloc(&mut self, kind: NodeKind, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.push_with_index(Node { kind, data: NodeData { lhs, rhs } }).into()
    }

    pub(crate) fn node_kind(&self, node: NodeId) -> NodeKind {
        self.nodes[node.get()].kind
    }

    pub(crate) fn block_stmts(&self, block: NodeId) -> &[NodeId] {
        let block = &self.nodes[block.get()];
        assert_eq!(block.kind, NodeKind::Block);
        let start = block.data.lhs.get();
        let end = block.data.rhs.get();
        &self.node_ids[start..end]
    }

    pub(crate) fn local_var(&self, node: NodeId) -> LocalVar {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::LocalVar);

        LocalVar {
            name: self.node_ids[node.data.lhs.get()],
            _ty: self.node_ids[node.data.rhs.get()],
            initializer: self.node_ids[node.data.rhs.get() + 1],
        }
    }
}

pub(crate) struct LocalVar {
    pub(crate) name: NodeId,
    pub(crate) _ty: NodeId,
    pub(crate) initializer: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct NodeId {
    raw: u32,
}

impl NodeId {
    pub(crate) const ZERO: Self = Self { raw: 0 };

    fn new(raw: usize) -> Self {
        let raw: u32 = raw.try_into().unwrap();
        Self { raw: raw + 1 }
    }

    fn get(self) -> usize {
        self.raw as usize - 1
    }
}

impl From<usize> for NodeId {
    fn from(value: usize) -> Self {
        NodeId::new(value)
    }
}

#[derive(Debug)]
struct Node {
    kind: NodeKind,
    data: NodeData,
}

#[derive(Debug)]
struct NodeData {
    lhs: NodeId,
    rhs: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NodeKind {
    Name,
    True,
    False,
    LocalVar,
    Float,
    Int,
    Call,
    Block,
    Error,
}

trait VecExtension<T> {
    fn push_with_index(&mut self, element: T) -> usize;
}

impl<T> VecExtension<T> for Vec<T> {
    fn push_with_index(&mut self, element: T) -> usize {
        let index = self.len();
        self.push(element);
        index
    }
}

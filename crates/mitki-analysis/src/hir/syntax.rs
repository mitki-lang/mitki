use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::{RedToken, ast};

#[derive(Default, Debug, PartialEq, Eq, salsa::Update)]
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
            ast::LiteralKind::String(token) => (NodeKind::String, self.alloc_token(db, &token)),
            ast::LiteralKind::Char(token) => (NodeKind::Char, self.alloc_token(db, &token)),
        };

        self.alloc(kind, lhs, NodeId::ZERO)
    }

    fn alloc_token(&mut self, db: &'db dyn salsa::Database, token: &RedToken<'db>) -> NodeId {
        self.alloc_binding(token.green().text_trimmed(db).into_symbol(db))
    }

    #[track_caller]
    pub(crate) fn symbol(&self, binding: NodeId) -> Symbol<'db> {
        self.symbols[binding.get()]
    }

    pub(crate) fn alloc_local_var(
        &mut self,
        name: NodeId,
        ty: NodeId,
        initializer: NodeId,
    ) -> NodeId {
        let lhs = self.node_ids.push_with_index(name).into();
        let rhs = self.node_ids.push_with_index(ty).into();
        self.node_ids.push(initializer);
        self.alloc(NodeKind::LocalVar, lhs, rhs)
    }

    pub(crate) fn alloc_block(&mut self, stmts: Vec<NodeId>, tail: NodeId) -> NodeId {
        let start = self.node_ids.len().into();
        self.node_ids.extend(stmts);
        let end = self.node_ids.len().into();

        let range = self.node_ids.push_with_index(start).into();
        self.node_ids.push(end);

        self.alloc(NodeKind::Block, range, tail)
    }

    pub(crate) fn alloc_error(&mut self) -> NodeId {
        self.alloc(NodeKind::Error, NodeId::ZERO, NodeId::ZERO)
    }

    pub(crate) fn alloc_call(&mut self, callee: NodeId, _args: Vec<NodeId>) -> NodeId {
        let lhs = self.node_ids.push_with_index(callee).into();
        self.alloc(NodeKind::Call, lhs, NodeId::ZERO)
    }

    pub(crate) fn alloc_binary(&mut self, lhs: NodeId, op: NodeId, rhs: NodeId) -> NodeId {
        let lhs_idx = self.node_ids.push_with_index(lhs).into();
        let rhs_idx = self.node_ids.push_with_index(op).into();
        self.node_ids.push(rhs);
        self.alloc(NodeKind::Binary, lhs_idx, rhs_idx)
    }

    pub(crate) fn alloc_postfix(&mut self, expr: NodeId, op: NodeId) -> NodeId {
        self.alloc(NodeKind::Postfix, expr, op)
    }

    pub(crate) fn alloc_prefix(&mut self, op: NodeId, expr: NodeId) -> NodeId {
        self.alloc(NodeKind::Prefix, op, expr)
    }

    pub(crate) fn alloc_if(
        &mut self,
        cond: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
    ) -> NodeId {
        let lhs_idx = self.node_ids.push_with_index(cond).into();
        let rhs_idx = self.node_ids.push_with_index(then_branch).into();
        self.node_ids.push(else_branch);
        self.alloc(NodeKind::If, lhs_idx, rhs_idx)
    }

    pub(crate) fn alloc_closure(&mut self, params: Vec<NodeId>, body: NodeId) -> NodeId {
        let start = self.node_ids.len().into();
        self.node_ids.extend(params);
        let end = self.node_ids.len().into();

        let range = self.node_ids.push_with_index(start).into();
        self.node_ids.push(end);

        self.alloc(NodeKind::Closure, range, body)
    }

    fn alloc(&mut self, kind: NodeKind, lhs: NodeId, rhs: NodeId) -> NodeId {
        self.nodes.push_with_index(Node { kind, data: NodeData { lhs, rhs } }).into()
    }

    #[track_caller]
    pub(crate) fn node_kind(&self, node: NodeId) -> NodeKind {
        self.nodes[node.get()].kind
    }

    #[track_caller]
    pub(crate) fn name(&self, node: NodeId) -> Symbol<'db> {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Name);
        self.symbol(node.data.lhs)
    }

    pub(crate) fn block_stmts(&self, block: NodeId) -> (&[NodeId], NodeId) {
        let block = &self.nodes[block.get()];
        assert_eq!(block.kind, NodeKind::Block);

        let start = self.node_ids[block.data.lhs.get()].get();
        let end = self.node_ids[block.data.lhs.get() + 1].get();

        (&self.node_ids[start..end], block.data.rhs)
    }

    pub(crate) fn binary(&self, node: NodeId) -> BinaryExpr {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Binary);

        BinaryExpr {
            lhs: self.node_ids[node.data.lhs.get()],
            op: self.node_ids[node.data.rhs.get()],
            rhs: self.node_ids[node.data.rhs.get() + 1],
        }
    }

    pub(crate) fn postfix(&self, node: NodeId) -> PostfixExpr {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Postfix);

        PostfixExpr { expr: node.data.lhs, op: node.data.rhs }
    }

    pub(crate) fn prefix(&self, node: NodeId) -> PrefixExpr {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Prefix);

        PrefixExpr { op: node.data.lhs, expr: node.data.rhs }
    }

    pub(crate) fn if_expr(&self, node: NodeId) -> IfExpr {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::If);

        IfExpr {
            cond: self.node_ids[node.data.lhs.get()],
            then_branch: self.node_ids[node.data.rhs.get()],
            else_branch: self.node_ids[node.data.rhs.get() + 1],
        }
    }

    pub(crate) fn closure_parts(&self, node: NodeId) -> (&[NodeId], NodeId) {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Closure);

        let start = self.node_ids[node.data.lhs.get()].get();
        let end = self.node_ids[node.data.lhs.get() + 1].get();

        (&self.node_ids[start..end], node.data.rhs)
    }

    #[track_caller]
    pub(crate) fn local_var(&self, node: NodeId) -> LocalVar {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::LocalVar);

        LocalVar {
            name: self.node_ids[node.data.lhs.get()],
            _ty: self.node_ids[node.data.rhs.get()],
            initializer: self.node_ids[node.data.rhs.get() + 1],
        }
    }

    pub(crate) fn alloc_type_ref(&mut self, path: Symbol<'db>) -> NodeId {
        let lhs = self.alloc_binding(path);
        self.alloc(NodeKind::TypePath, lhs, NodeId::ZERO)
    }
}

pub struct LocalVar {
    pub name: NodeId,
    pub(crate) _ty: NodeId,
    pub(crate) initializer: NodeId,
}

pub struct BinaryExpr {
    pub lhs: NodeId,
    pub op: NodeId,
    pub rhs: NodeId,
}

pub struct PostfixExpr {
    pub expr: NodeId,
    pub op: NodeId,
}

pub struct PrefixExpr {
    pub op: NodeId,
    pub expr: NodeId,
}

pub struct IfExpr {
    pub cond: NodeId,
    pub then_branch: NodeId,
    pub else_branch: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, salsa::Update)]
pub struct NodeId {
    raw: u32,
}

impl NodeId {
    pub(crate) const ZERO: Self = Self { raw: 0 };

    fn new(raw: usize) -> Self {
        let raw: u32 = raw.try_into().unwrap();
        Self { raw: raw + 1 }
    }

    #[track_caller]
    fn get(self) -> usize {
        self.raw as usize - 1
    }
}

impl From<usize> for NodeId {
    fn from(value: usize) -> Self {
        Self::new(value)
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
struct Node {
    kind: NodeKind,
    data: NodeData,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
struct NodeData {
    lhs: NodeId,
    rhs: NodeId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeKind {
    Name,
    True,
    False,
    LocalVar,
    Float,
    Int,
    String,
    Char,
    Binary,
    Postfix,
    Prefix,
    Call,
    If,
    Closure,
    Block,
    Error,

    TypePath,
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

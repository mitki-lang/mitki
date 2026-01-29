use mitki_span::Symbol;

#[derive(Default, Debug, PartialEq, Eq, salsa::Update)]
pub struct NodeStore<'db> {
    nodes: Vec<Node>,
    symbols: Vec<Symbol<'db>>,
    node_ids: Vec<NodeId>,
}

impl<'db> NodeStore<'db> {
    pub fn alloc_binding(&mut self, symbol: Symbol<'db>) -> NodeId {
        self.symbols.push_with_index(symbol).into()
    }

    pub fn alloc_name(&mut self, symbol: Symbol<'db>) -> NodeId {
        let binding = self.alloc_binding(symbol);
        self.alloc(NodeKind::Name, binding, NodeId::ZERO)
    }

    pub fn alloc_literal(&mut self, kind: NodeKind, value: Option<Symbol<'db>>) -> NodeId {
        let lhs = value.map_or(NodeId::ZERO, |symbol| self.alloc_binding(symbol));
        self.alloc(kind, lhs, NodeId::ZERO)
    }

    #[track_caller]
    pub(crate) fn symbol(&self, binding: NodeId) -> Symbol<'db> {
        self.symbols[binding.get()]
    }

    pub fn alloc_tuple(&mut self, items: Vec<NodeId>) -> NodeId {
        let start = self.node_ids.len().into();
        self.node_ids.extend(items);
        let end = self.node_ids.len().into();

        let range = self.node_ids.push_with_index(start).into();
        self.node_ids.push(end);

        self.alloc(NodeKind::Tuple, range, NodeId::ZERO)
    }

    pub(crate) fn tuple(&self, node: NodeId) -> &[NodeId] {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Tuple);

        let start = self.node_ids[node.data.lhs.get()].get();
        let end = self.node_ids[node.data.lhs.get() + 1].get();

        &self.node_ids[start..end]
    }

    pub fn alloc_local_var(&mut self, name: NodeId, ty: NodeId, initializer: NodeId) -> NodeId {
        let lhs = self.node_ids.push_with_index(name).into();
        let rhs = self.node_ids.push_with_index(ty).into();
        self.node_ids.push(initializer);
        self.alloc(NodeKind::LocalVar, lhs, rhs)
    }

    pub fn alloc_block(&mut self, stmts: Vec<NodeId>, tail: NodeId) -> NodeId {
        let start = self.node_ids.len().into();
        self.node_ids.extend(stmts);
        let end = self.node_ids.len().into();

        let range = self.node_ids.push_with_index(start).into();
        self.node_ids.push(end);

        self.alloc(NodeKind::Block, range, tail)
    }

    pub fn alloc_error(&mut self) -> NodeId {
        self.alloc(NodeKind::Error, NodeId::ZERO, NodeId::ZERO)
    }

    pub fn alloc_call(&mut self, callee: NodeId, args: Vec<NodeId>) -> NodeId {
        let start = self.node_ids.len().into();
        self.node_ids.push_with_index(callee);
        self.node_ids.extend(args);
        let end = self.node_ids.len().into();
        self.alloc(NodeKind::Call, start, end)
    }

    pub(crate) fn call(&self, node: NodeId) -> (NodeId, &[NodeId]) {
        assert_eq!(self.node_kind(node), NodeKind::Call);

        let call = &self.nodes[node.get()];
        let ids = &self.node_ids[call.data.lhs.get()..call.data.rhs.get()];

        let (callee, args) = ids.split_first().expect("Call node must have at least one element");
        (*callee, args)
    }

    pub fn alloc_binary(&mut self, lhs: NodeId, op: NodeId, rhs: NodeId) -> NodeId {
        let lhs_idx = self.node_ids.push_with_index(lhs).into();
        let rhs_idx = self.node_ids.push_with_index(op).into();
        self.node_ids.push(rhs);
        self.alloc(NodeKind::Binary, lhs_idx, rhs_idx)
    }

    pub fn alloc_postfix(&mut self, expr: NodeId, op: NodeId) -> NodeId {
        self.alloc(NodeKind::Postfix, expr, op)
    }

    pub fn alloc_prefix(&mut self, op: NodeId, expr: NodeId) -> NodeId {
        self.alloc(NodeKind::Prefix, op, expr)
    }

    pub fn alloc_if(&mut self, cond: NodeId, then_branch: NodeId, else_branch: NodeId) -> NodeId {
        let lhs_idx = self.node_ids.push_with_index(cond).into();
        let rhs_idx = self.node_ids.push_with_index(then_branch).into();
        self.node_ids.push(else_branch);
        self.alloc(NodeKind::If, lhs_idx, rhs_idx)
    }

    pub fn alloc_closure(&mut self, params: Vec<NodeId>, body: NodeId) -> NodeId {
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

    pub fn alloc_param(&mut self, name: Symbol<'db>, ty: NodeId) -> NodeId {
        let name_id = self.alloc_name(name);
        let ty_id = self.node_ids.push_with_index(ty).into();
        self.alloc(NodeKind::Param, name_id, ty_id)
    }

    pub fn param(&self, node: NodeId) -> (Symbol<'db>, NodeId) {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::Param);

        let name = self.symbol(node.data.lhs);
        let ty = self.node_ids[node.data.rhs.get()];

        (name, ty)
    }

    pub fn alloc_type_ref(&mut self, path: Symbol<'db>) -> NodeId {
        let lhs = self.alloc_binding(path);
        self.alloc(NodeKind::TypePath, lhs, NodeId::ZERO)
    }

    pub fn type_ref(&self, node: NodeId) -> Symbol<'db> {
        let node = &self.nodes[node.get()];
        assert_eq!(node.kind, NodeKind::TypePath);
        self.symbol(node.data.lhs)
    }
}

pub struct LocalVar {
    pub name: NodeId,
    pub(crate) _ty: NodeId,
    pub initializer: NodeId,
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
    pub const ZERO: Self = Self { raw: 0 };

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
    Tuple,
    Binary,
    Postfix,
    Prefix,
    Call,
    If,
    Closure,
    Block,
    Error,

    Param,
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

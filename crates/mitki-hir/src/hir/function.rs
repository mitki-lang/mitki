use mitki_span::Symbol;

use super::syntax::{
    BinaryExpr, IfExpr, LocalVar, NodeId, NodeKind, NodeStore, PostfixExpr, PrefixExpr,
};

#[derive(Default, Debug, PartialEq, Eq, salsa::Update)]
pub struct Function<'db> {
    node_store: NodeStore<'db>,

    params: Vec<NodeId>,
    body: NodeId,
    ret_type: NodeId,
}

impl<'db> Function<'db> {
    pub fn node_store(&self) -> &NodeStore<'db> {
        &self.node_store
    }

    pub fn node_store_mut(&mut self) -> &mut NodeStore<'db> {
        &mut self.node_store
    }

    pub fn set_params(&mut self, params: Vec<NodeId>) {
        self.params = params;
    }

    pub fn set_ret_type(&mut self, ret_type: NodeId) {
        self.ret_type = ret_type;
    }

    pub fn set_body(&mut self, body: NodeId) {
        self.body = body;
    }

    pub fn params(&self) -> &[NodeId] {
        &self.params
    }

    pub fn ret_type(&self) -> NodeId {
        self.ret_type
    }

    pub fn body(&self) -> NodeId {
        self.body
    }

    #[track_caller]
    pub fn node_kind(&self, node: NodeId) -> NodeKind {
        self.node_store.node_kind(node)
    }

    pub fn block_stmts(&self, block: NodeId) -> (&[NodeId], NodeId) {
        self.node_store.block_stmts(block)
    }

    #[track_caller]
    pub fn binding_symbol(&self, binding: NodeId) -> Symbol<'db> {
        self.node_store.symbol(binding)
    }

    #[track_caller]
    pub fn local_var(&self, node: NodeId) -> LocalVar {
        self.node_store.local_var(node)
    }

    #[track_caller]
    pub fn binary(&self, node: NodeId) -> BinaryExpr {
        self.node_store.binary(node)
    }

    #[track_caller]
    pub fn postfix(&self, node: NodeId) -> PostfixExpr {
        self.node_store.postfix(node)
    }

    #[track_caller]
    pub fn prefix(&self, node: NodeId) -> PrefixExpr {
        self.node_store.prefix(node)
    }

    pub fn tuple(&self, node: NodeId) -> &[NodeId] {
        self.node_store.tuple(node)
    }

    #[track_caller]
    pub fn if_expr(&self, node: NodeId) -> IfExpr {
        self.node_store.if_expr(node)
    }

    #[track_caller]
    pub fn closure_parts(&self, node: NodeId) -> (&[NodeId], NodeId) {
        self.node_store.closure_parts(node)
    }

    #[track_caller]
    pub fn name(&self, node: NodeId) -> Symbol<'db> {
        self.node_store.name(node)
    }

    #[track_caller]
    pub fn call(&self, node: NodeId) -> (NodeId, &[NodeId]) {
        self.node_store.call(node)
    }
}

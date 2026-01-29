use super::{ExprId, NodeStore, ParamId, TyId};

#[derive(Default, Debug, PartialEq, Eq, salsa::Update)]
pub struct Function<'db> {
    node_store: NodeStore<'db>,

    params: Vec<ParamId>,
    body: ExprId,
    ret_type: TyId,
}

impl<'db> Function<'db> {
    pub fn node_store(&self) -> &NodeStore<'db> {
        &self.node_store
    }

    pub fn node_store_mut(&mut self) -> &mut NodeStore<'db> {
        &mut self.node_store
    }

    pub fn set_params(&mut self, params: Vec<ParamId>) {
        self.params = params;
    }

    pub fn set_ret_type(&mut self, ret_type: TyId) {
        self.ret_type = ret_type;
    }

    pub fn set_body(&mut self, body: ExprId) {
        self.body = body;
    }

    pub fn params(&self) -> &[ParamId] {
        &self.params
    }

    pub fn ret_type(&self) -> TyId {
        self.ret_type
    }

    pub fn body(&self) -> ExprId {
        self.body
    }
}

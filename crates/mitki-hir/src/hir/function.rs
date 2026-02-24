use mitki_span::Symbol;

use super::{ExprId, NodeStore, ParamId, TyId};

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct Function {
    node_store: NodeStore,

    type_params: Vec<Symbol>,
    params: Vec<ParamId>,
    body: ExprId,
    ret_type: TyId,
}

impl Function {
    pub fn node_store(&self) -> &NodeStore {
        &self.node_store
    }

    pub fn node_store_mut(&mut self) -> &mut NodeStore {
        &mut self.node_store
    }

    pub fn set_type_params(&mut self, type_params: Vec<Symbol>) {
        self.type_params = type_params;
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

    pub fn type_params(&self) -> &[Symbol] {
        &self.type_params
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

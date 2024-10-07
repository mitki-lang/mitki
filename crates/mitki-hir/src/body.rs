use crate::ExprData;

#[derive(Debug, Default)]
pub struct Body<'db> {
    pub exprs: la_arena::Arena<ExprData<'db>>,
}

#[derive(Debug, Default)]
pub struct BodySourceMap {}

mod function;
mod syntax;

pub(crate) use function::Function;
pub(crate) use syntax::{Binding, Block, Expr, ExprData, Stmt};

pub trait HasFunction<'db> {
    fn hir_function(self, db: &'db dyn salsa::Database) -> &'db Function<'db>;
}

#[salsa::tracked]
impl<'db> HasFunction<'db> for crate::item::scope::FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn hir_function(self, db: &'db dyn salsa::Database) -> Function<'db> {
        use mitki_parse::FileParse as _;
        use mitki_yellow::ast::{self, Node as _};

        use crate::ast_map::HasAstMap as _;
        use crate::item::tree::HasItemTree as _;

        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let ptr = ast_map.find_node(item);

        let syntax = ptr.to_node(db, &file.parse(db).syntax_node());
        let function = ast::Function::cast(db, syntax).unwrap();

        function::FunctionBuilder::new(db).build(function)
    }
}

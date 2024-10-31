#![feature(async_closure)]

mod ast_map;
mod hir;
mod infer;
mod item;
mod resolver;
mod ty;

#[salsa::tracked]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) {
    use infer::Inferable as _;
    use item::scope::HasItemScope as _;

    for declaration in file.item_scope(db).declarations() {
        declaration.infer(db);
    }
}

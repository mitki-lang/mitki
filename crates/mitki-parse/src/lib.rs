use std::marker::PhantomData;

use mitki_inputs::File;
use mitki_yellow::{GreenNode, RedNode, ast};

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

#[derive(Copy, salsa::Update)]
pub struct Parsed<'db, T> {
    root: GreenNode<'db>,
    phantom: PhantomData<fn() -> T>,
}

impl<T> std::fmt::Debug for Parsed<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parsed").field("root", &self.root).finish_non_exhaustive()
    }
}

impl<T> Clone for Parsed<'_, T> {
    fn clone(&self) -> Self {
        Self { root: self.root, phantom: self.phantom }
    }
}

impl<T> PartialEq for Parsed<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.root == other.root && self.phantom == other.phantom
    }
}

impl<T> Eq for Parsed<'_, T> {}

impl<'db, T: ast::Node<'db>> Parsed<'db, T> {
    pub fn syntax_node(self) -> RedNode<'db> {
        RedNode::new_root(self.root)
    }

    pub fn tree(self, db: &'db dyn salsa::Database) -> T {
        T::cast(db, self.syntax_node()).unwrap()
    }
}

pub trait FileParse {
    fn parse(self, db: &dyn salsa::Database) -> Parsed<ast::Module<'_>>;
}

#[salsa::tracked]
impl FileParse for File {
    #[salsa::tracked]
    fn parse(self, db: &dyn salsa::Database) -> Parsed<'_, ast::Module<'_>> {
        let mut parser = parser::Parser::new(db, self.text(db));
        grammar::items::module(&mut parser);
        Parsed { root: parser.build_tree(), phantom: PhantomData }
    }
}

use mitki_inputs::File;
use mitki_yellow::{GreenNode, RedNode};

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Parsed<'db> {
    root: GreenNode<'db>,
}

impl<'db> Parsed<'db> {
    pub fn syntax_node(self) -> RedNode<'db> {
        RedNode::new_root(self.root)
    }
}

pub trait FileParse {
    fn parse(self, db: &dyn salsa::Database) -> Parsed<'_>;
}

#[salsa::tracked]
impl FileParse for File {
    #[salsa::tracked]
    fn parse(self, db: &dyn salsa::Database) -> Parsed<'_> {
        let mut parser = parser::Parser::new(db, self.text(db));
        grammar::items::module(&mut parser);
        Parsed { root: parser.build_tree() }
    }
}

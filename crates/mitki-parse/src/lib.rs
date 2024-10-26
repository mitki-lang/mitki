use mitki_inputs::File;
use mitki_yellow::GreenNode;

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

pub trait FileParse {
    fn parse(self, db: &dyn salsa::Database) -> GreenNode<'_>;
}

#[salsa::tracked]
impl FileParse for File {
    #[salsa::tracked]
    fn parse(self, db: &dyn salsa::Database) -> GreenNode<'_> {
        let mut parser = parser::Parser::new(db, self.text(db));
        grammar::items::module(&mut parser);
        parser.build_tree()
    }
}

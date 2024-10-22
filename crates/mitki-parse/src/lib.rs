use mitki_yellow::GreenNode;

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

pub fn module<'db>(db: &'db dyn salsa::Database, text: &'db str) -> GreenNode<'db> {
    let mut parser = parser::Parser::new(db, text);
    grammar::items::module(&mut parser);
    parser.build_tree()
}

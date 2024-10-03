use mitki_yellow::GreenNode;
use salsa::Database;

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

pub fn module<'db>(db: &'db dyn Database, text: &'db str) -> GreenNode<'db> {
    let mut parser = parser::Parser::new(db, text);
    grammar::module(&mut parser);
    parser.build_tree()
}

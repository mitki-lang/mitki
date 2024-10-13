use mitki_yellow::ast;

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

pub fn module<'db>(db: &'db dyn salsa::Database, text: &'db str) -> ast::Module<'db> {
    let mut parser = parser::Parser::new(db, text);
    grammar::items::module(&mut parser);
    ast::Module::new(db, parser.build_tree())
}

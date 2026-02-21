use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_yellow::ast::{self, Node as _};
use mitki_yellow::{SyntaxNode, SyntaxTree};

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

#[derive(salsa::Update)]
pub struct Parsed {
    root: SyntaxTree,
    diagnostics: Vec<Diagnostic>,
}

impl std::fmt::Debug for Parsed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Parsed").field("root", &self.root).finish_non_exhaustive()
    }
}

impl PartialEq for Parsed {
    fn eq(&self, other: &Self) -> bool {
        self.root.text() == other.root.text()
    }
}

impl Eq for Parsed {}

impl Parsed {
    pub fn syntax_node(&self) -> SyntaxNode<'_> {
        self.root.root()
    }

    pub fn tree(&self) -> ast::Module<'_> {
        ast::Module::cast(self.syntax_node()).unwrap()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

pub trait FileParse {
    fn parse(self, db: &dyn salsa::Database) -> &Parsed;
}

#[salsa::tracked]
impl FileParse for File {
    #[salsa::tracked(returns(ref))]
    fn parse(self, db: &dyn salsa::Database) -> Parsed {
        let mut parser = parser::Parser::new(self.text(db));
        grammar::items::module(&mut parser);
        let (root, diagnostics) = parser.build_tree();
        Parsed { root, diagnostics }
    }
}

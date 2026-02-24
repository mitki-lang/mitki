use std::future::Future;

use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_yellow::ast::{self, Node as _};
use mitki_yellow::{SyntaxNode, SyntaxTree};

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

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

pub fn parse_text(text: &str) -> Parsed {
    let mut parser = parser::Parser::new(text);
    grammar::items::module(&mut parser);
    let (root, diagnostics) = parser.build_tree();
    Parsed { root, diagnostics }
}

#[picante::tracked]
pub async fn parse_file<DB: mitki_inputs::FileDatabase>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<u64> {
    let _text = file.text(db);
    Ok(file.revision(db))
}

pub trait ParseExecutor {
    fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future;
}

pub trait ParseDb:
    ParseExecutor + HasParseFileQuery + mitki_inputs::FileDatabase + mitki_hir::ty::TypeDatabase
{
}

impl<T> ParseDb for T where
    T: ParseExecutor + HasParseFileQuery + mitki_inputs::FileDatabase + mitki_hir::ty::TypeDatabase
{
}

pub trait FileParse {
    fn parse<DB>(self, db: &DB) -> Parsed
    where
        DB: ParseDb;
}

impl FileParse for File {
    fn parse<DB>(self, db: &DB) -> Parsed
    where
        DB: ParseDb,
    {
        db.block_on(parse_file(db, self)).expect("failed to compute parse query");

        let text = self.text(db);
        parse_text(text.as_ref())
    }
}

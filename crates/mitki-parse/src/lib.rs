use std::future::Future;
use std::sync::Arc;

use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_yellow::ast::{self, Node as _};
use mitki_yellow::{SyntaxNode, SyntaxTree};

mod grammar;
mod parser;
#[cfg(test)]
mod tests;

#[derive(facet::Facet)]
pub struct Parsed {
    #[facet(opaque)]
    root: SyntaxTree,
    #[facet(opaque)]
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

#[picante::tracked]
pub async fn parse<DB: mitki_inputs::FileDatabase + HasParseFileQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<Arc<Parsed>> {
    parse_file(db, file).await?;

    let text = file.text(db);
    Ok(Arc::new(parse_text(text.as_ref())))
}

pub trait ParseDb:
    HasParseFileQuery + HasParseQuery + mitki_inputs::FileDatabase + mitki_hir::ty::TypeDatabase
{
}

impl<T> ParseDb for T where
    T: HasParseFileQuery + HasParseQuery + mitki_inputs::FileDatabase + mitki_hir::ty::TypeDatabase
{
}

pub trait FileParse {
    fn parse<DB>(self, db: &DB) -> impl Future<Output = Arc<Parsed>> + Send
    where
        DB: ParseDb + Sync;
}

impl FileParse for File {
    #[allow(clippy::manual_async_fn)]
    fn parse<DB>(self, db: &DB) -> impl Future<Output = Arc<Parsed>> + Send
    where
        DB: ParseDb + Sync,
    {
        async move { parse(db, self).await.expect("failed to compute parse query") }
    }
}

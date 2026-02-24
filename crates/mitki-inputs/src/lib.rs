use std::marker::PhantomData;
use std::sync::Arc;

pub use line_index::LineCol;

#[picante::input]
pub struct SourceFile {
    #[key]
    pub path: String,
    pub text: String,
    pub revision: u64,
}

pub trait FileDatabase:
    picante::HasRuntime + HasSourceFileIngredient + Send + Sync + 'static
{
}

impl<T> FileDatabase for T where
    T: picante::HasRuntime + HasSourceFileIngredient + Send + Sync + 'static
{
}

#[derive(Debug, Clone, PartialEq, Eq, facet::Facet)]
pub struct LineIndex {
    #[facet(opaque)]
    inner: line_index::LineIndex,
}

impl LineIndex {
    pub fn new(text: &str) -> Self {
        Self { inner: line_index::LineIndex::new(text) }
    }

    pub fn line(&self, line: u32) -> Option<line_index::TextRange> {
        self.inner.line(line)
    }

    pub fn line_col(&self, offset: line_index::TextSize) -> LineCol {
        self.inner.line_col(offset)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, facet::Facet)]
pub struct File {
    source_file: SourceFile,
}

impl File {
    pub fn new<DB>(db: &DB, path: camino::Utf8PathBuf, text: String) -> Self
    where
        DB: FileDatabase,
    {
        let source_file = SourceFile::new(db, path.into_string(), text, 0)
            .expect("failed to create source file input");
        Self { source_file }
    }

    pub fn revision<DB>(self, db: &DB) -> u64
    where
        DB: FileDatabase,
    {
        self.source_file.revision(db).expect("failed to read source file revision")
    }

    pub fn path<DB>(self, db: &DB) -> camino::Utf8PathBuf
    where
        DB: FileDatabase,
    {
        let path = self.source_file.path(db).expect("failed to read source file path");
        camino::Utf8PathBuf::from(path.as_str())
    }

    pub fn text<DB>(self, db: &DB) -> Arc<str>
    where
        DB: FileDatabase,
    {
        let text = self.source_file.text(db).expect("failed to read source file text");
        Arc::from(text)
    }

    pub fn set_text<'db, DB>(self, db: &'db DB) -> SetText<'db, DB>
    where
        DB: FileDatabase,
    {
        SetText { file: self, db, _marker: PhantomData }
    }

    pub async fn line_index<DB>(self, db: &DB) -> Arc<LineIndex>
    where
        DB: FileDatabase + HasLineIndexQuery,
    {
        line_index(db, self).await.expect("failed to compute line index")
    }
}

#[picante::tracked]
pub async fn line_index<DB: FileDatabase>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<Arc<LineIndex>> {
    let text = file.text(db);
    Ok(Arc::new(LineIndex::new(text.as_ref())))
}

pub struct SetText<'db, DB>
where
    DB: FileDatabase,
{
    file: File,
    db: &'db DB,
    _marker: PhantomData<&'db DB>,
}

impl<DB> SetText<'_, DB>
where
    DB: FileDatabase,
{
    pub fn to(self, text: String) {
        let source_file = self.file.source_file;
        let current_text = source_file.text(self.db).expect("failed to read source file text");
        if current_text == text {
            return;
        }

        let path = source_file.path(self.db).expect("failed to read source file path");
        let revision = source_file.revision(self.db).expect("failed to read source file revision");

        SourceFile::new(self.db, path.as_ref().to_owned(), text, revision + 1)
            .expect("failed to update source file input");
    }
}

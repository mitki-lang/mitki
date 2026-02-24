use std::marker::PhantomData;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

pub use line_index::LineIndex;

#[picante::input]
pub struct SourceFile {
    #[key]
    pub id: u32,
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

static NEXT_FILE_ID: AtomicU32 = AtomicU32::new(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, facet::Facet)]
pub struct File {
    id: u32,
}

impl File {
    pub fn new<DB>(db: &DB, path: camino::Utf8PathBuf, text: String) -> Self
    where
        DB: FileDatabase,
    {
        let id = NEXT_FILE_ID.fetch_add(1, Ordering::Relaxed);
        SourceFile::new(db, id, path.into_string(), text, 0)
            .expect("failed to create source file input");
        Self { id }
    }

    pub fn id(self) -> u32 {
        self.id
    }

    pub fn revision<DB>(self, db: &DB) -> u64
    where
        DB: FileDatabase,
    {
        self.source_file(db).revision(db).expect("failed to read source file revision")
    }

    pub fn path<DB>(self, db: &DB) -> camino::Utf8PathBuf
    where
        DB: FileDatabase,
    {
        let path = self.source_file(db).path(db).expect("failed to read source file path");
        camino::Utf8PathBuf::from(path)
    }

    pub fn text<DB>(self, db: &DB) -> Arc<str>
    where
        DB: FileDatabase,
    {
        let text = self.source_file(db).text(db).expect("failed to read source file text");
        Arc::from(text)
    }

    pub fn set_text<'db, DB>(self, db: &'db DB) -> SetText<'db, DB>
    where
        DB: FileDatabase,
    {
        SetText { file: self, db, _marker: PhantomData }
    }

    pub fn line_index<DB>(self, db: &DB) -> Arc<LineIndex>
    where
        DB: FileDatabase,
    {
        Arc::new(LineIndex::new(self.text(db).as_ref()))
    }

    fn source_file<DB>(self, db: &DB) -> SourceFile
    where
        DB: FileDatabase,
    {
        let id = db.source_file_keys().intern(self.id).expect("failed to intern source file id");
        SourceFile(id)
    }
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
        let source_file = self.file.source_file(self.db);
        let current_text = source_file.text(self.db).expect("failed to read source file text");
        if current_text == text {
            return;
        }

        let path = source_file.path(self.db).expect("failed to read source file path");
        let revision = source_file.revision(self.db).expect("failed to read source file revision");

        SourceFile::new(self.db, self.file.id, path, text, revision + 1)
            .expect("failed to update source file input");
    }
}

pub use line_index::LineIndex;

#[salsa::input(debug)]
pub struct File {
    #[returns(ref)]
    pub path: camino::Utf8PathBuf,
    #[returns(deref)]
    pub text: String,
}

#[salsa::tracked]
impl File {
    #[salsa::tracked(returns(ref), no_eq)]
    pub fn line_index(self, db: &dyn salsa::Database) -> LineIndex {
        LineIndex::new(self.text(db))
    }
}

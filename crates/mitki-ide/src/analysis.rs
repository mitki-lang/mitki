mod goto_definition;
mod hover;
mod inlay_hints;
mod semantic_tokens;

pub use hover::HoverResult;
pub use inlay_hints::InlayHint;
use mitki_db::RootDatabase;
pub use semantic_tokens::{SemanticToken, SemanticTokenKind};

#[derive(Default)]
pub struct Analysis {
    db: RootDatabase,
}

impl Analysis {
    pub fn db(&self) -> &RootDatabase {
        &self.db
    }

    pub fn db_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }
}

use mitki_db::RootDatabase;
use mitki_parse::FileParse as _;
use mitki_yellow::SyntaxKind;
use mitki_yellow::ast::{Module, Node};

use crate::{FilePosition, pick_best_token};

#[derive(Default)]
pub(crate) struct Analysis {
    db: RootDatabase,
}

impl Analysis {
    pub(crate) fn db(&self) -> &RootDatabase {
        &self.db
    }

    pub(crate) fn db_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }

    pub(crate) fn goto_definition(
        &self,
        FilePosition { file, offset }: FilePosition,
    ) -> Option<Vec<FilePosition>> {
        let root = file.parse(self.db());
        let root = Module::new(root).syntax().clone();

        let tokens = root.token_at_offset(self.db(), offset);
        let _original_token = pick_best_token(self.db(), tokens, |kind| match kind {
            SyntaxKind::NAME => 2,
            _ => 1,
        })?;

        Some(Vec::new())
    }
}

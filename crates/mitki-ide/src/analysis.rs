use mitki_analysis::Semantics;
use mitki_analysis::hir::HasFunction as _;
use mitki_db::RootDatabase;
use mitki_parse::FileParse as _;
use mitki_span::Symbol;
use mitki_yellow::SyntaxKind;
use mitki_yellow::ast::{self, Node as _};
use text_size::TextRange;

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
    ) -> Option<(TextRange, TextRange)> {
        let db = self.db();
        let semantics = Semantics::new(db, file);
        let root = file.parse(db).syntax_node();

        let tokens = root.token_at_offset(db, offset);
        let original_token = pick_best_token(db, tokens, |kind| match kind {
            SyntaxKind::NAME => 2,
            _ => 1,
        })?;

        let path = original_token.parent().unwrap();
        let location = path
            .ancestors()
            .find_map(|syntax| ast::Item::cast(db, syntax))
            .map(|item| semantics.function(db, item.syntax()))?;

        let resolver = semantics.resolver(db, location, path);

        let path = Symbol::new(db, original_token.green().text_trimmed(db));
        let path = resolver.resolve_path(path)?;

        let function = location.hir_function(db);
        let ptr = function.binding_map_back[&path];
        let node = ptr.to_node(db, &root);

        Some((original_token.text_trimmed_range(db), node.text_trimmed_range(db)))
    }
}

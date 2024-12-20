use mitki_analysis::Semantics;
use mitki_analysis::hir::HasFunction as _;
use mitki_analysis::resolver::PathResolution;
use mitki_db::RootDatabase;
use mitki_parse::FileParse as _;
use mitki_span::Symbol;
use mitki_yellow::SyntaxKind;
use mitki_yellow::ast::{self, HasName, Node as _};
use text_size::TextRange;

use crate::{FilePosition, pick_best_token};

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

    pub fn goto_definition(
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
        if path.kind(db) != SyntaxKind::NAME_REF {
            return None;
        }

        let location = path
            .ancestors()
            .find_map(|syntax| ast::Item::cast(db, syntax))
            .map(|item| semantics.function(db, item.syntax()))?;

        let resolver = semantics.resolver(db, location, path);

        let path = Symbol::new(db, original_token.green().text_trimmed(db));

        match resolver.resolve_path(path)? {
            PathResolution::Local(path) => {
                let function = location.hir_function(db);
                let ptr = function.binding_syntax(&path);
                let node = ptr.to_node(db, &root);

                Some((original_token.text_trimmed_range(db), node.text_trimmed_range(db)))
            }
            PathResolution::Function(function_location) => {
                let function = function_location.source(db);
                let function_name_range =
                    function.name(db).unwrap().syntax().text_trimmed_range(db);

                Some((original_token.text_trimmed_range(db), function_name_range))
            }
        }
    }
}

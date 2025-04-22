use mitki_analysis::Semantics;
use mitki_analysis::hir::HasFunction as _;
use mitki_analysis::resolver::PathResolution;
use mitki_parse::FileParse as _;
use mitki_span::IntoSymbol as _;
use mitki_yellow::SyntaxKind;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use text_size::TextRange;

use crate::{FilePosition, pick_best_token};

impl super::Analysis {
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
        let path = original_token.green().text_trimmed(db).into_symbol(db);

        match resolver.resolve_path(path)? {
            PathResolution::Local(path) => {
                let source_map = location.hir_source_map(db);
                let range = source_map.node_syntax(&path).range;

                Some((original_token.trimmed_range(db), range))
            }
            PathResolution::Function(function_location) => {
                let function = function_location.source(db);
                let function_name_range = function.name(db).unwrap().syntax().trimmed_range(db);

                Some((original_token.trimmed_range(db), function_name_range))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use mitki_inputs::File;
    use text_size::{TextRange, TextSize};

    use crate::{Analysis, FilePosition};

    const CURSOR_MARKER: &str = "$0";

    fn extract_offset(text: &str) -> (TextSize, String) {
        let cursor_pos = text.find(CURSOR_MARKER).expect("Cursor marker not found");
        let mut new_text = String::with_capacity(text.len() - CURSOR_MARKER.len());
        new_text.push_str(&text[..cursor_pos]);
        new_text.push_str(&text[cursor_pos + CURSOR_MARKER.len()..]);
        let cursor_pos = TextSize::from(cursor_pos as u32);
        (cursor_pos, new_text)
    }

    fn extract_annotations(text: &str) -> Vec<TextRange> {
        let mut line_start_map = BTreeMap::new();
        let mut annotations = Vec::new();
        let mut line_start: TextSize = 0.into();

        for line in text.split_inclusive('\n') {
            let line_length = if let Some((prefix, suffix)) = line.split_once("//") {
                let ss_len = TextSize::of("//");
                let annotation_offset = TextSize::of(prefix) + ss_len;
                for mut annotation in extract_line_annotations(suffix.trim_end_matches('\n')) {
                    annotation += annotation_offset;
                    let line_start = line_start_map.range(annotation.end()..).next().unwrap();

                    annotations.push(annotation + line_start.1);
                }

                annotation_offset
            } else {
                TextSize::of(line)
            };

            line_start_map = line_start_map.split_off(&line_length);
            line_start_map.insert(line_length, line_start);
            line_start += TextSize::of(line);
        }

        annotations
    }

    fn extract_line_annotations(mut line: &str) -> Vec<TextRange> {
        let mut annotations = Vec::new();
        let mut offset: TextSize = 0.into();

        while let Some(idx) = line.find('^') {
            offset += TextSize::try_from(idx).unwrap();
            line = &line[idx..];

            let len = line.chars().take_while(|&it| it == '^').count();
            let range = TextRange::at(offset, len.try_into().unwrap());

            annotations.push(range);
            line = &line[len..];
        }

        annotations
    }

    #[track_caller]
    fn check(fixture: &str) {
        let analysis = Analysis::default();
        let (offset, fixture) = extract_offset(fixture);
        let annotations = extract_annotations(&fixture);
        let file = File::new(analysis.db(), "".into(), fixture.clone());
        let file_position = FilePosition { file, offset };

        assert_eq!(annotations.len(), 1);
        let expected = annotations.into_iter().next().unwrap();

        let (_, focus) = analysis.goto_definition(file_position).expect("no definition found");

        assert_eq!(focus, expected);
    }

    #[test]
    fn variable() {
        check(
            r#"
fun main() {
    val x = 42
      //^
    $0x
}
"#,
        );
    }

    #[test]
    fn function() {
        check(
            r#"
fun add() {}
  //^^^
fun main() {
    add$0();
}
"#,
        );
    }
}

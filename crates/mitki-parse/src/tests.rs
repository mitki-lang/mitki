use std::fs;
use std::path::{Path, PathBuf};

use expect_test::expect_file;
use mitki_tokenizer::Tokenizer;
use mitki_yellow::SyntaxKind;
use salsa::DatabaseImpl;

use crate::grammar;
use crate::parser::Parser;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
struct TestCase {
    input: PathBuf,
    expected: PathBuf,
    text: String,
}

impl TestCase {
    fn list() -> Vec<Self> {
        let test_data_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("test_data");

        let mut cases = fs::read_dir(&test_data_dir)
            .unwrap_or_else(|err| {
                panic!("Cannot read directory {}: {err}", test_data_dir.display())
            })
            .filter_map(|entry| {
                let path = entry.ok()?.path();
                if path.extension()? == "mitki" {
                    let expected = path.with_extension("ir");
                    let text = fs::read_to_string(&path).ok()?;
                    Some(Self { input: path, expected, text })
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        cases.sort();
        cases
    }
}

#[test]
fn parse() {
    let test_cases = TestCase::list();
    let db = DatabaseImpl::new();

    for case in test_cases {
        let mut parser = Parser::new(&db, &case.text);
        grammar::items::module(&mut parser);
        let (tree, diagnostics) = parser.debug_tree();
        let diagnostics =
            diagnostics.iter().map(|d| format!("  {}", d.message())).collect::<Vec<_>>().join("\n");

        let actual = format!("{tree}\nErrors:\n{diagnostics}");
        expect_file![&case.expected].assert_eq(&actual);
    }
}

#[test]
fn trivia_tokens_are_materialized() {
    let text = r#"
fun main() {
    val x = 1 // comment
}
"#;
    let mut saw_trivia = false;
    let mut saw_non_trivia = false;
    let mut tokenizer = Tokenizer::new(text);

    loop {
        let token_index = tokenizer.next_token_index();
        let token = tokenizer.token(token_index);

        if token.kind != SyntaxKind::EOF && !token.kind.is_trivia() {
            saw_non_trivia = true;
        }

        if !tokenizer.leading_trivia(token_index).is_empty()
            || !tokenizer.trailing_trivia(token_index).is_empty()
        {
            saw_trivia = true;
        }

        if token.kind == SyntaxKind::EOF {
            break;
        }
    }

    assert!(saw_trivia, "expected trivia pieces in the tokenizer");
    assert!(saw_non_trivia, "expected non-trivia tokens in the stream");
}

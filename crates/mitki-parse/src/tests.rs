use std::fmt::Debug;
use std::fs;
use std::path::{Path, PathBuf};

use expect_test::expect_file;
use mitki_inputs::File;
use mitki_yellow::SyntaxNode;
use salsa::{Database, DatabaseImpl};

use crate::FileParse as _;

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

struct Printer<'db>(SyntaxNode<'db>);

impl<'db> Debug for Printer<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt_rec(f, 0, self.0)
    }
}

fn fmt_rec(f: &mut std::fmt::Formatter<'_>, level: usize, node: SyntaxNode) -> std::fmt::Result {
    let indent = "  ".repeat(level);
    writeln!(f, "{}{:?}", indent, node.kind())?;
    for child in node.children_with_tokens() {
        match child {
            mitki_yellow::NodeOrToken::Node(node) => fmt_rec(f, level + 1, node)?,
            mitki_yellow::NodeOrToken::Token(token) => {
                if token.is_trivia() {
                    continue;
                }
                let kind = token.kind();
                let text = token.text_trimmed();

                writeln!(f, "{indent}  {kind:?}: {text:?}")?;
            }
        }
    }
    Ok(())
}

#[salsa::tracked]
fn parse_module(db: &dyn Database, file: File) -> String {
    format!("{:?}", Printer(file.parse(db).syntax_node()))
}

#[test]
fn parse() {
    let test_cases = TestCase::list();
    let db = DatabaseImpl::new();

    for case in test_cases {
        let actual = salsa::plumbing::attach(&db, || {
            let text = File::new(&db, "".into(), case.text.clone());

            let parsed = text.parse(&db);
            let tree = format!("{:?}", Printer(parsed.syntax_node()));
            let diagnostics = parsed
                .diagnostics
                .iter()
                .map(|d| format!("  {}", d.message()))
                .collect::<Vec<_>>()
                .join("\n");

            format!("{tree}\nErrors:\n{diagnostics}")
        });
        expect_file![&case.expected].assert_eq(&actual);
    }
}

#[test]
fn trivia_tokens_are_materialized() {
    let db = DatabaseImpl::new();
    let text = r#"
fun main() {
    val x = 1 // comment
}
"#;
    let file = File::new(&db, "".into(), text.into());
    let root = file.parse(&db).syntax_node();

    let mut token = root.first_token();
    let last = root.last_token();
    let mut saw_trivia = false;
    let mut saw_non_trivia = false;

    loop {
        if token.is_trivia() {
            saw_trivia = true;
        } else {
            saw_non_trivia = true;
        }

        if token.trimmed_range().end() == last.trimmed_range().end() {
            break;
        }

        token = token.next_token().expect("expected next token");
    }

    assert!(saw_trivia, "expected trivia tokens in the tree");
    assert!(saw_non_trivia, "expected non-trivia tokens in the tree");
}

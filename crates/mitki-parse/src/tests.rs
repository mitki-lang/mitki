use std::fmt::Debug;
use std::fs;
use std::path::{Path, PathBuf};

use expect_test::expect_file;
use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_yellow::{GreenChild, GreenNode};
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

struct Printer<'db>(GreenNode<'db>);

impl<'db> Debug for Printer<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        salsa::plumbing::with_attached_database(|db| fmt_rec(f, db, 0, self.0)).unwrap()
    }
}

fn fmt_rec(
    f: &mut std::fmt::Formatter<'_>,
    db: &dyn Database,
    level: usize,
    node: GreenNode,
) -> std::fmt::Result {
    let indent = "  ".repeat(level);
    writeln!(f, "{}{:?}", indent, node.kind(db))?;
    for &child in node.children(db) {
        match child {
            GreenChild::Node { node, .. } => fmt_rec(f, db, level + 1, node)?,
            GreenChild::Token { token, .. } => {
                let kind = token.kind(db);
                let text = token.text_trimmed(db);

                writeln!(f, "{indent}  {kind:?}: {text:?}")?
            }
        }
    }
    Ok(())
}

#[salsa::tracked]
fn parse_module(db: &dyn Database, file: File) -> String {
    format!("{:?}", Printer(file.parse(db).root))
}

#[test]
fn parse() {
    let test_cases = TestCase::list();
    let db = DatabaseImpl::new();

    for case in test_cases {
        let actual = salsa::plumbing::attach(&db, || {
            let text = File::new(&db, "".into(), case.text.clone());

            let tree = parse_module(&db, text);
            let diagnostics = parse_module::accumulated::<Diagnostic>(&db, text);
            let diagnostics = diagnostics
                .into_iter()
                .map(|d| format!("  {}", d.message()))
                .collect::<Vec<_>>()
                .join("\n");

            format!("{tree}\nErrors:\n{diagnostics}")
        });
        expect_file![&case.expected].assert_eq(&actual);
    }
}

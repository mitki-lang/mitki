use mitki_db::{RootDatabase, check_file};
use mitki_errors::Diagnostic;
use mitki_inputs::File;

#[derive(Debug, PartialEq, Eq)]
struct ExpectedDiag {
    line: usize,
    message: String,
}

#[derive(Debug, PartialEq, Eq)]
struct ActualDiag {
    line: usize,
    message: String,
}

fn parse_expectations(fixture: &str) -> Vec<ExpectedDiag> {
    let mut expected = Vec::new();

    for (idx, line) in fixture.lines().enumerate() {
        let Some((_, comment)) = line.split_once("//~") else {
            continue;
        };
        let comment = comment.trim();
        let comment = comment.strip_prefix("ERROR").unwrap_or(comment).trim();
        if comment.is_empty() {
            continue;
        }
        expected.push(ExpectedDiag { line: idx + 1, message: comment.to_owned() });
    }

    expected
}

fn collect_actual(db: &RootDatabase, file: File, diagnostics: &[Diagnostic]) -> Vec<ActualDiag> {
    let line_index = file.line_index(db);
    let mut actual = diagnostics
        .iter()
        .map(|diag| {
            let line = line_index.line_col(diag.range().start()).line as usize + 1;
            ActualDiag { line, message: diag.message().to_owned() }
        })
        .collect::<Vec<_>>();
    actual.sort_by_key(|diag| (diag.line, diag.message.clone()));
    actual
}

#[track_caller]
fn check(fixture: &str) {
    let db = RootDatabase::default();
    let file = File::new(&db, "typeck.mtk".into(), fixture.to_owned());

    let diagnostics = check_file(&db, file);
    let mut actual = collect_actual(&db, file, diagnostics);
    let mut expected = parse_expectations(fixture);

    expected.sort_by_key(|diag| (diag.line, diag.message.clone()));

    assert_eq!(
        expected.len(),
        actual.len(),
        "expected {} diagnostic(s), got {}\nexpected: {expected:#?}\nactual: {actual:#?}",
        expected.len(),
        actual.len(),
    );

    for expected_diag in expected {
        let Some(pos) = actual.iter().position(|diag| {
            diag.line == expected_diag.line && diag.message.contains(&expected_diag.message)
        }) else {
            panic!(
                "missing diagnostic on line {} containing `{}`\nactual: {actual:#?}",
                expected_diag.line, expected_diag.message
            );
        };
        actual.remove(pos);
    }

    assert!(actual.is_empty(), "unexpected diagnostics:\n{actual:#?}");
}

#[test]
fn unresolved_identifier() {
    check(
        r#"
fun main() {
    x //~ ERROR Unresolved identifier
}
"#,
    );
}

#[test]
fn unknown_type_annotation() {
    check(
        r#"
fun main() {
    val x: Nope = 1 //~ ERROR Unknown type `Nope`
}
"#,
    );
}

#[test]
fn mismatched_annotation() {
    check(
        r#"
fun main() {
    val x: int = true //~ ERROR expected `int`, found `bool`
}
"#,
    );
}

#[test]
fn if_condition_must_be_bool() {
    check(
        r#"
fun main() {
    if 1 { //~ ERROR expected `bool`, found `int`
    }
}
"#,
    );
}

#[test]
fn type_used_as_value() {
    check(
        r#"
fun main() {
    int //~ ERROR expected value, found type `int`
}
"#,
    );
}

#[test]
#[ignore = "TODO: binary operator type rules"]
fn binary_operator_type_mismatch() {
    check(
        r#"
fun main() {
    1 + true //~ ERROR cannot apply `+` to `int` and `bool`
}
"#,
    );
}

#[test]
#[ignore = "TODO: if branch type agreement without expected type"]
fn if_branch_type_mismatch() {
    check(
        r#"
fun main() {
    if true {
        1
    } else {
        false //~ ERROR if branches have incompatible types
    }
}
"#,
    );
}

#[test]
#[ignore = "TODO: missing else in value position"]
fn if_missing_else_in_value_position() {
    check(
        r#"
fun main() {
    val x: int = if true {
        1 //~ ERROR missing `else` branch
    }
}
"#,
    );
}

#[test]
#[ignore = "TODO: call arity diagnostics"]
fn call_arity_mismatch() {
    check(
        r#"
fun add(x: int) {}

fun main() {
    add(1, 2) //~ ERROR expected 1 argument(s), found 2
}
"#,
    );
}

#[test]
#[ignore = "TODO: prefix operator type rules"]
fn prefix_operator_type_mismatch() {
    check(
        r#"
fun main() {
    -true //~ ERROR cannot apply `-` to `bool`
}
"#,
    );
}

#[test]
#[ignore = "TODO: postfix operator type rules"]
fn postfix_operator_type_mismatch() {
    check(
        r#"
fun main() {
    1! //~ ERROR cannot apply postfix `!` to `int`
}
"#,
    );
}

#[test]
#[ignore = "TODO: call on non-function diagnostics"]
fn call_non_function() {
    check(
        r#"
fun main() {
    1() //~ ERROR expected function, found `int`
}
"#,
    );
}

#[test]
#[ignore = "TODO: missing initializer diagnostics"]
fn val_without_initializer() {
    check(
        r#"
fun main() {
    val x //~ ERROR missing initializer
}
"#,
    );
}

#[test]
#[ignore = "TODO: tuple arity diagnostics"]
fn tuple_arity_mismatch() {
    check(
        r#"
fun main() {
    val x: (int, bool) = (1,) //~ ERROR expected 2 element(s), found 1
}
"#,
    );
}

#[test]
#[ignore = "TODO: call arity diagnostics (too few)"]
fn call_arity_too_few() {
    check(
        r#"
fun add(x: int, y: int) {}

fun main() {
    add(1) //~ ERROR expected 2 argument(s), found 1
}
"#,
    );
}

#[test]
#[ignore = "TODO: prefix operator type rules"]
fn prefix_operator_type_mismatch_string() {
    check(
        r#"
fun main() {
    -"a" //~ ERROR cannot apply `-` to `str`
}
"#,
    );
}

#[test]
#[ignore = "TODO: postfix operator type rules"]
fn postfix_operator_type_mismatch_bool() {
    check(
        r#"
fun main() {
    true! //~ ERROR cannot apply postfix `!` to `bool`
}
"#,
    );
}

use std::collections::BTreeMap;
use std::sync::{LazyLock, Mutex};

use mitki_db::check_file;
use mitki_inputs::File;

static QUERY_RUNS: LazyLock<Mutex<BTreeMap<String, usize>>> =
    LazyLock::new(|| Mutex::new(BTreeMap::new()));

fn reset_query_runs() {
    QUERY_RUNS.lock().expect("query run lock poisoned").clear();
}

fn query_runs<DB>(db: &DB, file: File) -> usize
where
    DB: mitki_inputs::FileDatabase,
{
    QUERY_RUNS
        .lock()
        .expect("query run lock poisoned")
        .get(file.path(db).as_str())
        .copied()
        .unwrap_or(0)
}

fn bump_query_runs<DB>(db: &DB, file: File)
where
    DB: mitki_inputs::FileDatabase,
{
    let mut runs = QUERY_RUNS.lock().expect("query run lock poisoned");
    *runs.entry(file.path(db).into_string()).or_insert(0) += 1;
}

#[picante::tracked]
async fn dependency_probe_query<DB: mitki_inputs::FileDatabase + mitki_parse::HasParseFileQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<u64> {
    bump_query_runs(db, file);
    mitki_parse::parse_file(db, file).await
}

#[picante::db(
    inputs(mitki_inputs::SourceFile),
    interned(mitki_span::Symbol, mitki_hir::ty::TyData),
    tracked(
        mitki_inputs::line_index,
        mitki_parse::parse_file,
        mitki_parse::parse,
        mitki_lower::ast_map::ast_map,
        mitki_lower::item::tree::item_tree,
        mitki_lower::item::scope::item_scope,
        mitki_lower::item::scope::signature,
        mitki_lower::item::scope::signature_map,
        mitki_lower::hir::hir_function,
        mitki_resolve::scope::expr_scopes,
        mitki_resolve::resolver::builtin_scope,
        mitki_typeck::infer::infer,
        mitki_analysis::check_file,
        dependency_probe_query
    ),
    db_trait(TestDatabase)
)]
struct TestDb {}

impl Default for TestDb {
    fn default() -> Self {
        Self::new()
    }
}

async fn diagnostic_messages(db: &TestDb, file: File) -> Vec<String> {
    check_file(db, file)
        .await
        .expect("failed to compute diagnostics")
        .iter()
        .map(|diag| diag.message().to_owned())
        .collect()
}

#[tokio::test]
async fn set_text_invalidates_only_dependent_file_queries() {
    reset_query_runs();
    let db = TestDb::default();

    let edited_file = File::new(
        &db,
        "edited.mtk".into(),
        r#"
fun main() {
    missing
}
"#
        .to_owned(),
    );
    let stable_file = File::new(
        &db,
        "stable.mtk".into(),
        r#"
fun stable() {
    val x = 1
    x;
}
"#
        .to_owned(),
    );

    let edited_messages_before = diagnostic_messages(&db, edited_file).await;
    let stable_messages_before = diagnostic_messages(&db, stable_file).await;
    assert!(
        edited_messages_before.iter().any(|message| message.contains("Unresolved identifier")),
        "expected unresolved identifier before edit, got: {edited_messages_before:#?}"
    );
    assert!(
        stable_messages_before.is_empty(),
        "expected stable file to have no diagnostics before edit, got: {stable_messages_before:#?}"
    );

    let before_edited_revision = dependency_probe_query(&db, edited_file)
        .await
        .expect("failed to compute edited probe query");
    let before_stable_revision = dependency_probe_query(&db, stable_file)
        .await
        .expect("failed to compute stable probe query");
    assert_eq!(before_edited_revision, 0);
    assert_eq!(before_stable_revision, 0);
    assert_eq!(query_runs(&db, edited_file), 1);
    assert_eq!(query_runs(&db, stable_file), 1);

    edited_file.set_text(&db).to(r#"
fun main() {
    val missing = 1
    missing;
}
"#
    .to_owned());

    let after_edited_revision = dependency_probe_query(&db, edited_file)
        .await
        .expect("failed to recompute edited probe query");
    let after_stable_revision = dependency_probe_query(&db, stable_file)
        .await
        .expect("failed to compute stable probe query after unrelated edit");

    assert_eq!(after_edited_revision, 1);
    assert_eq!(after_stable_revision, 0);
    assert_eq!(query_runs(&db, edited_file), 2, "edited file query should recompute exactly once");
    assert_eq!(query_runs(&db, stable_file), 1, "stable file query should remain cached");

    let edited_messages_after = diagnostic_messages(&db, edited_file).await;
    let stable_messages_after = diagnostic_messages(&db, stable_file).await;
    assert!(
        edited_messages_after.is_empty(),
        "expected no diagnostics after edit, got: {edited_messages_after:#?}"
    );
    assert!(
        stable_messages_after.is_empty(),
        "expected stable file diagnostics to stay empty, got: {stable_messages_after:#?}"
    );
}

#[tokio::test]
async fn set_text_to_same_text_does_not_recompute_or_bump_revision() {
    reset_query_runs();
    let db = TestDb::default();

    let text = r#"
fun main() {
    val x = 1
    x;
}
"#;
    let file = File::new(&db, "same_text.mtk".into(), text.to_owned());

    let first_revision =
        dependency_probe_query(&db, file).await.expect("failed to compute probe query");
    assert_eq!(first_revision, 0);
    assert_eq!(query_runs(&db, file), 1);
    assert_eq!(file.revision(&db), 0);
    assert!(
        diagnostic_messages(&db, file).await.is_empty(),
        "expected no diagnostics before no-op edit"
    );

    file.set_text(&db).to(text.to_owned());

    let second_revision =
        dependency_probe_query(&db, file).await.expect("failed to reload probe query");
    assert_eq!(second_revision, 0);
    assert_eq!(query_runs(&db, file), 1, "no-op text update should keep query cached");
    assert_eq!(file.revision(&db), 0, "no-op text update should not bump revision");
    assert!(
        diagnostic_messages(&db, file).await.is_empty(),
        "expected no diagnostics after no-op edit"
    );
}

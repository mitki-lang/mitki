use std::collections::BTreeMap;
use std::future::Future;
use std::sync::{LazyLock, Mutex};

use mitki_db::check_file;
use mitki_inputs::File;
use mitki_parse::ParseExecutor as _;

static EXECUTOR: LazyLock<tokio::runtime::Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(1)
        .enable_all()
        .build()
        .expect("failed to build test parse runtime")
});

static QUERY_RUNS: LazyLock<Mutex<BTreeMap<u32, usize>>> =
    LazyLock::new(|| Mutex::new(BTreeMap::new()));

fn reset_query_runs() {
    QUERY_RUNS.lock().expect("query run lock poisoned").clear();
}

fn query_runs(file: File) -> usize {
    QUERY_RUNS.lock().expect("query run lock poisoned").get(&file.id()).copied().unwrap_or(0)
}

fn bump_query_runs(file: File) {
    let mut runs = QUERY_RUNS.lock().expect("query run lock poisoned");
    *runs.entry(file.id()).or_insert(0) += 1;
}

#[picante::tracked]
async fn dependency_probe_query<DB: mitki_inputs::FileDatabase + mitki_parse::HasParseFileQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<u64> {
    bump_query_runs(file);
    mitki_parse::parse_file(db, file).await
}

#[picante::db(
    inputs(mitki_inputs::SourceFile),
    interned(mitki_span::SymbolData, mitki_hir::ty::TyData),
    tracked(
        mitki_parse::parse_file,
        mitki_lower::item::scope::item_scope_cached,
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

impl mitki_parse::ParseExecutor for TestDb {
    fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future,
    {
        EXECUTOR.block_on(future)
    }
}

fn diagnostic_messages(db: &TestDb, file: File) -> Vec<String> {
    check_file(db, file).into_iter().map(|diag| diag.message().to_owned()).collect()
}

#[test]
fn set_text_invalidates_only_dependent_file_queries() {
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

    let edited_messages_before = diagnostic_messages(&db, edited_file);
    let stable_messages_before = diagnostic_messages(&db, stable_file);
    assert!(
        edited_messages_before.iter().any(|message| message.contains("Unresolved identifier")),
        "expected unresolved identifier before edit, got: {edited_messages_before:#?}"
    );
    assert!(
        stable_messages_before.is_empty(),
        "expected stable file to have no diagnostics before edit, got: {stable_messages_before:#?}"
    );

    let before_edited_revision = db
        .block_on(dependency_probe_query(&db, edited_file))
        .expect("failed to compute edited probe query");
    let before_stable_revision = db
        .block_on(dependency_probe_query(&db, stable_file))
        .expect("failed to compute stable probe query");
    assert_eq!(before_edited_revision, 0);
    assert_eq!(before_stable_revision, 0);
    assert_eq!(query_runs(edited_file), 1);
    assert_eq!(query_runs(stable_file), 1);

    edited_file.set_text(&db).to(r#"
fun main() {
    val missing = 1
    missing;
}
"#
    .to_owned());

    let after_edited_revision = db
        .block_on(dependency_probe_query(&db, edited_file))
        .expect("failed to recompute edited probe query");
    let after_stable_revision = db
        .block_on(dependency_probe_query(&db, stable_file))
        .expect("failed to compute stable probe query after unrelated edit");

    assert_eq!(after_edited_revision, 1);
    assert_eq!(after_stable_revision, 0);
    assert_eq!(query_runs(edited_file), 2, "edited file query should recompute exactly once");
    assert_eq!(query_runs(stable_file), 1, "stable file query should remain cached");

    let edited_messages_after = diagnostic_messages(&db, edited_file);
    let stable_messages_after = diagnostic_messages(&db, stable_file);
    assert!(
        edited_messages_after.is_empty(),
        "expected no diagnostics after edit, got: {edited_messages_after:#?}"
    );
    assert!(
        stable_messages_after.is_empty(),
        "expected stable file diagnostics to stay empty, got: {stable_messages_after:#?}"
    );
}

#[test]
fn set_text_to_same_text_does_not_recompute_or_bump_revision() {
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
        db.block_on(dependency_probe_query(&db, file)).expect("failed to compute probe query");
    assert_eq!(first_revision, 0);
    assert_eq!(query_runs(file), 1);
    assert_eq!(file.revision(&db), 0);
    assert!(diagnostic_messages(&db, file).is_empty(), "expected no diagnostics before no-op edit");

    file.set_text(&db).to(text.to_owned());

    let second_revision =
        db.block_on(dependency_probe_query(&db, file)).expect("failed to reload probe query");
    assert_eq!(second_revision, 0);
    assert_eq!(query_runs(file), 1, "no-op text update should keep query cached");
    assert_eq!(file.revision(&db), 0, "no-op text update should not bump revision");
    assert!(diagnostic_messages(&db, file).is_empty(), "expected no diagnostics after no-op edit");
}

use std::fs;
use std::path::Path;

use mitki_db::{RootDatabase, check_file};
use mitki_inputs::File;
use mitki_lower::item::package::{
    HasPackage as _, child_module_named, module_crate_path, package_modules, root_module,
};

fn write_file(path: &Path, text: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent directories");
    }
    fs::write(path, text).expect("write module file");
}

fn root_file(db: &RootDatabase, path: &Path, text: &str) -> File {
    write_file(path, text);
    File::new(db, path.to_str().expect("utf8 path").into(), text.to_owned())
}

#[test]
fn package_graph_loads_direct_and_nested_modules() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");
    let foo_path = tempdir.path().join("foo.mitki");
    let bar_path = tempdir.path().join("foo").join("bar.mitki");

    let root = root_file(&db, &root_path, "mod foo;\n");
    write_file(&foo_path, "mod bar;\n");
    write_file(&bar_path, "fun answer(): int { 42 }\n");

    let package = root.package(&db);
    let crate_root = root_module(&db, package);
    let foo = child_module_named(&db, crate_root, "foo".to_owned()).expect("foo module");
    let bar = child_module_named(&db, foo, "bar".to_owned()).expect("bar module");

    assert_eq!(crate_root.file(&db), root);
    assert_eq!(foo.file(&db).path(&db).as_str(), foo_path.to_str().expect("utf8 path"));
    assert_eq!(bar.file(&db).path(&db).as_str(), bar_path.to_str().expect("utf8 path"));
    assert_eq!(module_crate_path(&db, bar), "crate::foo::bar");
    assert_eq!(package_modules(&db, package).len(), 3);
}

#[test]
fn package_graph_falls_back_to_mod_file_layout() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");
    let foo_mod_path = tempdir.path().join("foo").join("mod.mitki");

    let root = root_file(&db, &root_path, "mod foo;\n");
    write_file(&foo_mod_path, "fun answer(): int { 42 }\n");

    let package = root.package(&db);
    let foo = child_module_named(&db, root_module(&db, package), "foo".to_owned())
        .expect("foo module from mod.mitki");

    assert_eq!(foo.file(&db).path(&db).as_str(), foo_mod_path.to_str().expect("utf8 path"));
    assert_eq!(module_crate_path(&db, foo), "crate::foo");
}

#[test]
fn typecheck_resolves_crate_paths_across_modules() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");
    let math_path = tempdir.path().join("math.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
mod math;

fun main(): int {
    crate::math::answer()
}
"#,
    );
    write_file(
        &math_path,
        r#"
fun answer(): int {
    42
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_use_imports_and_module_aliases() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::io::print_int;
use std::io as io;

fun main() {
    print_int(1);
    io::print_int(2)
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_std_alloc_modules() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::alloc as alloc;
use std::alloc as alloc;
use std::alloc::int as int_alloc;

fun main() {
    unsafe {
        val bytes: *mut u8 = alloc::alloc(4, 1)
        alloc::dealloc(bytes, 4, 1)

        val ints: *mut int = int_alloc::alloc(4, 4)
        int_alloc::dealloc(ints, 4, 4)
    }
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_std_alloc_copy_and_realloc_helpers() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::alloc as alloc;
use std::alloc::int as int_alloc;

fun main() {
    unsafe {
        val src: *mut int = int_alloc::alloc_items(2)
        val dst: *mut int = int_alloc::alloc_items(2)
        int_alloc::copy_nonoverlapping(dst, src, 2);

        val grown: *mut int = int_alloc::realloc(dst, 2, 4)
        val raw: *mut u8 = alloc::alloc(4, 1)
        val raw_grown: *mut u8 = alloc::realloc(raw, 4, 1, 8, 1)
        alloc::copy_nonoverlapping(raw_grown, raw_grown, 4);
        alloc::dealloc(raw_grown, 8, 1)

        int_alloc::dealloc_items(src, 2)
        int_alloc::dealloc_items(grown, 4)
    }
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_std_str_owned_string_helpers() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::str as strings;

fun main(): str {
    val bytes = str_bytes("hi")
    unsafe {
        strings::from_raw_parts_unchecked(bytes.ptr, bytes.len)
    }
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_std_lexer_debug_dump() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::lexer;

fun main() {
    lexer::debug_dump("fun")
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_std_env_var() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::env;

fun main(): str {
    match env::var("PATH") {
        (found, value) => if found { value } else { "" },
    }
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_std_vec_int_support_functions() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::vec::int as vec;

fun main() {
    var xs: vec::Vec = vec::new()
    vec::push(xs, 20);
    vec::push(xs, 22);
    vec::reserve(xs, 8);
    vec::set(xs, 1, 23);
    val size: u32 = vec::len(xs)
    val cap: u32 = vec::capacity(xs)

    val first: int = match vec::get(xs, 0) {
        .Some(value) => value,
        .None => 0,
    }

    vec::clear(xs);
    val cleared: bool = vec::is_empty(xs)

    if cleared {
        if size < cap {
            first;
        }
    }
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_resolves_method_calls_to_module_functions() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::vec::int as vec;

fun main() {
    var xs: vec::Vec = vec::new()
    xs.push(20);
    xs.push(22);
    xs.reserve(8);
    xs.set(1, 23);
    val size: u32 = xs.len()
    val cap: u32 = xs.capacity()

    val first: int = match xs.get(0) {
        .Some(value) => value,
        .None => 0,
    }

    if size < cap {
        first;
    }

    xs.free()
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_supports_in_place_method_updates_on_var_locals() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::vec::int as vec;

fun main() {
    var xs: vec::Vec = vec::new()
    xs.push(20);
    xs.push(22);
    xs.reserve(8);
    xs.set(1, 23);

    val size: u32 = xs.len()
    val cap: u32 = xs.capacity()
    val first: int = match xs.get(0) {
        .Some(value) => value,
        .None => 0,
    }

    if size < cap {
        first;
    }

    xs.free()
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_supports_direct_field_assignment_on_var_locals() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
struct Counter {
    value: int,
}

fun new(value: int): Counter {
    Counter { value: value }
}

fun main(): int {
    var counter: Counter = new(40)
    counter.value = counter.value + 2
    counter.value
}
"#,
    );

    let diagnostics = check_file(&db, root);
    assert!(diagnostics.is_empty(), "unexpected diagnostics: {diagnostics:#?}");
}

#[test]
fn typecheck_requires_var_for_in_place_method_updates() {
    let db = RootDatabase::default();
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_path = tempdir.path().join("main.mitki");

    let root = root_file(
        &db,
        &root_path,
        r#"
use std::vec::int as vec;

fun main() {
    val xs: vec::Vec = vec::new()
    xs.push(20);
    xs.free()
}
"#,
    );

    let diagnostics = check_file(&db, root);
    let messages = diagnostics.iter().map(|diag| diag.message()).collect::<Vec<_>>();
    assert!(
        messages
            .iter()
            .any(|message| message.contains("mutable parameter requires a mutable place")),
        "expected mutable-place diagnostic, got {messages:?}"
    );
}

use std::fs;

use assert_cmd::Command;
use mitki_abi::{LinkageKind, REQUIRED_FEATURE_HANDLES};
use mitki_wasm_runtime::describe_module_abi;

#[test]
fn build_writes_default_wasm_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("answer.mitki");
    let output = tempdir.path().join("answer.wasm");

    fs::write(
        &input,
        r#"
fun answer(): int {
    42
}

fun main() {
    val x = answer()
}
"#,
    )
    .expect("write input");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args([
            "build",
            "--target",
            "wasm32",
            "-o",
            output.to_str().expect("utf8 path"),
            input.to_str().expect("utf8 path"),
        ])
        .assert()
        .success();

    let bytes = fs::read(output).expect("expected `build` to write a wasm file");
    wasmparser::Validator::new().validate_all(&bytes).expect("emitted Wasm should validate");
}

#[test]
fn build_wasm_emits_start_for_unit_main() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("unit_main.mitki");
    let output = tempdir.path().join("unit_main.wasm");

    fs::write(
        &input,
        r#"
fun main() {
}
"#,
    )
    .expect("write unit program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["build", "--target", "wasm32", input.to_str().expect("utf8 path")])
        .assert()
        .success();

    let bytes = fs::read(output).expect("expected `build` to write a wasm file");
    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    assert!(abi.exports.iter().any(|export| export.name == "_start"));
}

#[test]
fn run_wasm_suppresses_unit_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let unit_program = tempdir.path().join("print_unit.mitki");

    fs::write(
        &unit_program,
        r#"
fun main() {
}
"#,
    )
    .expect("write unit program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", unit_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("");
}

#[test]
fn run_wasm_prints_i32_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let int_program = tempdir.path().join("print_i32.mitki");

    fs::write(
        &int_program,
        r#"
fun main(): int {
    42
}
"#,
    )
    .expect("write int program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", int_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42\n");
}

#[test]
fn run_wasm_prints_runtime_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let runtime_program = tempdir.path().join("runtime_output.mitki");

    fs::write(
        &runtime_program,
        r#"
fun main() {
    std::io::print_str("hello");
    std::io::print_int(7)
}
"#,
    )
    .expect("write runtime program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", runtime_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("hello7");
}

#[test]
fn run_wasm_supports_std_io_wasi_helpers() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("stdio_helpers.mitki");

    fs::write(
        &program,
        r#"
fun main() {
    std::io::println_str("hello");
    std::io::println_str("world");
    std::io::eprintln_str("oops");
}
"#,
    )
    .expect("write stdio helper program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("hello\nworld\n")
        .stderr("oops\n");
}

#[test]
fn run_wasm_supports_child_modules_via_crate_paths() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let root_program = tempdir.path().join("main.mitki");
    let math_program = tempdir.path().join("math.mitki");

    fs::write(
        &root_program,
        r#"
mod math;

fun main() {
    std::io::print_int(crate::math::answer())
}
"#,
    )
    .expect("write root program");
    fs::write(
        &math_program,
        r#"
fun answer(): int {
    42
}
"#,
    )
    .expect("write child module");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", root_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42");
}

#[test]
fn run_wasm_supports_use_imports_and_module_aliases() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("use_imports.mitki");

    fs::write(
        &program,
        r#"
use std::io::print_int;
use std::io as io;

fun main() {
    print_int(4);
    io::print_int(2)
}
"#,
    )
    .expect("write use-import program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42");
}

#[test]
fn run_wasm_orders_runtime_output_before_main_result() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let runtime_program = tempdir.path().join("runtime_and_main.mitki");

    fs::write(
        &runtime_program,
        r#"
fun main(): int {
    std::io::print_str("answer=");
    std::io::print_int(4);
    2
}
"#,
    )
    .expect("write runtime program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", runtime_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("answer=42\n");
}

#[test]
fn build_wasm_library_without_main() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("library.mitki");
    let output = tempdir.path().join("library.wasm");

    fs::write(
        &input,
        r#"
export fun answer(): int {
    42
}
"#,
    )
    .expect("write input");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["build", "--target", "wasm32", input.to_str().expect("utf8 path")])
        .assert()
        .success();

    let bytes = fs::read(output).expect("expected `build` to write a wasm file");
    wasmparser::Validator::new().validate_all(&bytes).expect("emitted Wasm should validate");
}

#[test]
fn build_wasm_supports_function_typed_boundaries() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("function_handles.mitki");
    let output = tempdir.path().join("function_handles.wasm");

    fs::write(
        &input,
        r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
    )
    .expect("write input");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["build", "--target", "wasm32", input.to_str().expect("utf8 path")])
        .assert()
        .success();

    let bytes = fs::read(output).expect("expected `build` to write a wasm file");
    wasmparser::Validator::new().validate_all(&bytes).expect("emitted Wasm should validate");

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI metadata");
    assert_ne!(metadata.required_features & REQUIRED_FEATURE_HANDLES, 0);
    assert!(metadata.function_instances.iter().any(|instance| {
        instance.linkage == LinkageKind::WasmExport && instance.wasm_field_name.is_some()
    }));
}

#[test]
fn build_wasm_preserves_typed_host_imports() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("typed_host_import.mitki");
    let output = tempdir.path().join("typed_host_import.wasm");

    fs::write(
        &input,
        r#"
import "env" fun round_trip(xs: [int]): [int];

export fun main(): [int] {
    round_trip([1, 2, 3])
}
"#,
    )
    .expect("write input");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["build", "--target", "wasm32", input.to_str().expect("utf8 path")])
        .assert()
        .success();

    let bytes = fs::read(output).expect("expected `build` to write a wasm file");
    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI metadata");
    assert!(metadata.function_instances.iter().any(|instance| {
        instance.linkage == LinkageKind::WasmImport
            && instance.wasm_module_name.is_some_and(|id| metadata.strings[id.0 as usize] == "env")
    }));
}

#[test]
fn run_wasm_prints_bool_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let bool_program = tempdir.path().join("print_bool.mitki");

    fs::write(
        &bool_program,
        r#"
fun main(): bool {
    true
}
"#,
    )
    .expect("write bool program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", bool_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("true\n");
}

#[test]
fn run_wasm_prints_parenthesized_logical_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let bool_program = tempdir.path().join("print_parenthesized_bool.mitki");

    fs::write(
        &bool_program,
        r#"
fun main(): bool {
    (1 < 2 && 2 < 3) || false
}
"#,
    )
    .expect("write bool program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", bool_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("true\n");
}

#[test]
fn run_wasm_prints_float_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let float_program = tempdir.path().join("print_float.mitki");

    fs::write(
        &float_program,
        r#"
fun main(): float {
    42.5
}
"#,
    )
    .expect("write float program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", float_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42.5\n");
}

#[test]
fn run_wasm_prints_char_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let char_program = tempdir.path().join("print_char.mitki");

    fs::write(
        &char_program,
        r#"
fun main(): char {
    'x'
}
"#,
    )
    .expect("write char program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", char_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("x\n");
}

#[test]
fn run_wasm_supports_char_comparisons() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let char_program = tempdir.path().join("char_compare.mitki");

    fs::write(
        &char_program,
        r#"
fun main(): bool {
    'a' < 'b'
}
"#,
    )
    .expect("write char compare program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", char_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("true\n");
}

#[test]
fn run_wasm_supports_char_match_patterns() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let char_program = tempdir.path().join("char_match.mitki");

    fs::write(
        &char_program,
        r#"
fun classify(ch: char): int {
    match ch {
        'a' => 1,
        'b' => 2,
        _ => 0,
    }
}

fun main(): int {
    classify('b')
}
"#,
    )
    .expect("write char match program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", char_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("2\n");
}

#[test]
fn run_wasm_prints_string_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let string_program = tempdir.path().join("print_string.mitki");

    fs::write(
        &string_program,
        r#"
fun main(): str {
    "hello"
}
"#,
    )
    .expect("write string program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", string_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("hello\n");
}

#[test]
fn run_wasm_prints_array_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let array_program = tempdir.path().join("print_array.mitki");

    fs::write(
        &array_program,
        r#"
fun main(): [int] {
    [1, 2, 3]
}
"#,
    )
    .expect("write array program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", array_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("[1, 2, 3]\n");
}

#[test]
fn run_wasm_prints_string_array_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let array_program = tempdir.path().join("print_string_array.mitki");

    fs::write(
        &array_program,
        r#"
fun main(): [str] {
    ["a", "b"]
}
"#,
    )
    .expect("write string array program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", array_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("[a, b]\n");
}

#[test]
fn run_wasm_prints_struct_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let struct_program = tempdir.path().join("print_struct.mitki");

    fs::write(
        &struct_program,
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(): Point {
    Point { x: 20, y: 22 }
}
"#,
    )
    .expect("write struct program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", struct_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("{ x: 20, y: 22 }\n");
}

#[test]
fn run_wasm_supports_struct_field_shorthand() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let struct_program = tempdir.path().join("struct_shorthand.mitki");

    fs::write(
        &struct_program,
        r#"
struct Point {
    x: int,
    y: int,
}

fun main(): Point {
    val x = 20
    Point { x, y: 22 }
}
"#,
    )
    .expect("write struct shorthand program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", struct_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("{ x: 20, y: 22 }\n");
}

#[test]
fn run_wasm_prints_tuple_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let tuple_program = tempdir.path().join("print_tuple.mitki");

    fs::write(
        &tuple_program,
        r#"
fun main(): (int, int) {
    (20, 22)
}
"#,
    )
    .expect("write tuple program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", tuple_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("(20, 22)\n");
}

#[test]
fn run_wasm_prints_nested_enum_output() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let enum_program = tempdir.path().join("print_enum.mitki");

    fs::write(
        &enum_program,
        r#"
enum Words {
    Some([str]),
    Empty,
}

fun main(): Words {
    Words.Some(["a", "b"])
}
"#,
    )
    .expect("write enum program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", enum_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("Some([a, b])\n");
}

#[test]
fn run_wasm_matches_literal_patterns_over_union() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let union_program = tempdir.path().join("union_literal_match.mitki");

    fs::write(
        &union_program,
        r#"
fun main(): int {
    val a: int | bool = if true { true } else { 0 }
    val b: int | bool = if true { false } else { 0 }
    val c: int | bool = if true { 7 } else { false }

    val x: int = match a {
        true => 1,
        false => 0,
        _ => 2,
    }
    val y: int = match b {
        true => 1,
        false => 0,
        _ => 2,
    }
    val z: int = match c {
        true => 1,
        false => 0,
        _ => 2,
    }
    x + y + z
}
"#,
    )
    .expect("write union literal match program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", union_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("3\n");
}

#[test]
fn run_wasm_matches_literal_patterns_for_union_parameters() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let union_program = tempdir.path().join("union_param_match.mitki");

    fs::write(
        &union_program,
        r#"
fun score(value: int | bool): int {
    match value {
        42 => 10,
        false => 20,
        _ => 30,
    }
}

fun main(): int {
    score(42) + score(false)
}
"#,
    )
    .expect("write union parameter match program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", union_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("30\n");
}

#[test]
fn run_wasm_matches_union_tuple_struct_and_enum_members() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let union_program = tempdir.path().join("union_member_match.mitki");

    fs::write(
        &union_program,
        r#"
struct Box {
    value: int,
}

enum Choice {
    Some(int),
    None,
}

fun main(): int {
    tuple_case() + struct_case() + enum_case()
}

fun tuple_case(): int {
    val tuple_value: int | (bool, int) = if true { (true, 5) } else { 0 }
    match tuple_value {
        (flag, n) => if flag { n } else { 0 },
        _ => 0,
    }
}

fun struct_case(): int {
    val struct_value: int | Box = if true { Box { value: 7 } } else { 0 }
    match struct_value {
        Box { value } => value,
        _ => 0,
    }
}

fun enum_case(): int {
    val enum_value: int | Choice = if true { Choice.Some(11) } else { 0 }
    match enum_value {
        .Some(n) => n,
        .None => 0,
        _ => 0,
    }
}
"#,
    )
    .expect("write union member match program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", union_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("23\n");
}

#[test]
fn run_wasm_matches_typed_patterns_over_union() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let union_program = tempdir.path().join("union_typed_match.mitki");

    fs::write(
        &union_program,
        r#"
fun describe(value: int | str): str {
    match value {
        s: str => s,
        _: int => "int",
    }
}

fun main(): [str] {
    val a: int | str = if true { "hello" } else { 0 }
    val b: int | str = if true { 7 } else { "unused" }
    [describe(a), describe(b)]
}
"#,
    )
    .expect("write typed union match program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", union_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("[hello, int]\n");
}

#[test]
fn run_wasm_matches_nested_typed_union_members() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let union_program = tempdir.path().join("union_nested_typed_match.mitki");

    fs::write(
        &union_program,
        r#"
fun describe(value: int | (int, str)): str {
    match value {
        (n: int, _: str) => "pair",
        _: int => "int",
    }
}

fun main(): [str] {
    val a: int | (int, str) = if true { (7, "s") } else { 0 }
    val b: int | (int, str) = if true { 9 } else { (1, "x") }
    [describe(a), describe(b)]
}
"#,
    )
    .expect("write nested typed union match program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", union_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("[pair, int]\n");
}

#[test]
fn run_wasm_evaluates_nested_comptime() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let comptime_program = tempdir.path().join("nested_comptime.mitki");

    fs::write(
        &comptime_program,
        r#"
comptime fun base(): int {
    40
}

comptime fun build(): int {
    comptime(base()) + 2
}

fun main(): int {
    comptime(build())
}
"#,
    )
    .expect("write comptime program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", comptime_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42\n");
}

#[test]
fn run_wasm_links_wasi_preview1_imports() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let wasi_program = tempdir.path().join("wasi_main.mitki");

    fs::write(
        &wasi_program,
        r#"
import "wasi_snapshot_preview1" fun sched_yield(): int;

fun main(): int {
    sched_yield()
}
"#,
    )
    .expect("write wasi program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", wasi_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("0\n");
}

#[test]
fn run_wasm_opens_a_file_via_raw_wasi_preview1() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let wasi_program = tempdir.path().join("wasi_open_file.mitki");
    let message_path = tempdir.path().join("message.txt");

    fs::write(&message_path, "Hello from WASI").expect("write message file");
    fs::write(
        &wasi_program,
        r#"
extern struct ByteSlice {
    ptr: *const u8,
    len: u32,
}

import "wasi_snapshot_preview1" unsafe fun path_open(
    dirfd: u32,
    dirflags: u32,
    path_ptr: *const u8,
    path_len: u32,
    oflags: u16,
    rights_base: u64,
    rights_inheriting: u64,
    fdflags: u16,
    opened_fd: *mut u32,
): u16;

import "wasi_snapshot_preview1" unsafe fun fd_close(fd: u32): u16;

unsafe fun open_status(path: str): int {
    val opened_fd: *mut u32 = stack_alloc(1)
    ptr_write(opened_fd, 0);
    val path_bytes = str_bytes(path)
    if path_bytes.len != 11 {
        1100
    } else {
        val open_errno = path_open(3, 0, path_bytes.ptr, path_bytes.len, 0, 2, 0, 0, opened_fd)
        if open_errno != 0 {
            1000
        } else {
            val fd = ptr_read(opened_fd)
            if fd == 0 {
                1300
            } else {
                if fd_close(fd) != 0 {
                    1400
                } else {
                    1
                }
            }
        }
    }
}

fun main(): int {
    unsafe {
        open_status("message.txt")
    }
}
"#,
    )
    .expect("write wasi read program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .current_dir(tempdir.path())
        .args(["run-wasm", wasi_program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("1\n");
}

#[test]
fn run_wasm_rejects_non_wasi_imports() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let env_program = tempdir.path().join("env_main.mitki");

    fs::write(
        &env_program,
        r#"
import "env" fun host(): int;

fun main(): int {
    host()
}
"#,
    )
    .expect("write env program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", env_program.to_str().expect("utf8 path")])
        .assert()
        .failure()
        .stderr(predicates::str::contains("env::host"));
}

#[test]
fn run_wasm_supports_std_alloc_int_module() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("alloc_int.mitki");

    fs::write(
        &program,
        r#"
use std::alloc::int as alloc;

fun main(): int {
    unsafe {
        val ptr: *mut int = alloc::alloc(4, 4)
        ptr_write(ptr, 42);
        val value: int = ptr_read(ptr)
        alloc::dealloc(ptr, 4, 4);
        value
    }
}
"#,
    )
    .expect("write alloc program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42\n");
}

#[test]
fn run_wasm_supports_std_lexer_new() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("main.mitki");

    fs::write(
        &program,
        r#"
use std::lexer;

fun main() {
    var n = lexer::new("def");
}
"#,
    )
    .expect("write std lexer program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("");
}

#[test]
fn run_wasm_supports_std_alloc_copy_and_realloc_helpers() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("alloc_realloc_int.mitki");

    fs::write(
        &program,
        r#"
use std::alloc::int as int_alloc;

fun main(): int {
    unsafe {
        val src: *mut int = int_alloc::alloc_items(2)
        ptr_write(src, 20);
        ptr_write(ptr_add(src, 1), 22);

        val dst: *mut int = int_alloc::alloc_items(2)
        int_alloc::copy_nonoverlapping(dst, src, 2);

        val grown: *mut int = int_alloc::realloc(dst, 2, 3)
        ptr_write(ptr_add(grown, 2), 7);

        val total: int =
            ptr_read(grown) + ptr_read(ptr_add(grown, 1)) + ptr_read(ptr_add(grown, 2))
        int_alloc::dealloc_items(src, 2);
        int_alloc::dealloc_items(grown, 3);
        total
    }
}
"#,
    )
    .expect("write alloc program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("49\n");
}

#[test]
fn run_wasm_supports_std_vec_int_mutation() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("vec_int.mitki");

    fs::write(
        &program,
        r#"
use std::vec::int as vec;

fun main(): int {
    var xs: vec::Vec = vec::new()
    xs.push(20);
    xs.push(22);
    xs.reserve(8);
    xs.set(1, 23);

    val first: int = match xs.get(0) {
        .Some(value) => value,
        .None => 0,
    }
    val second: int = match xs.get(1) {
        .Some(value) => value,
        .None => 0,
    }

    xs.clear();
    val empty_bonus: int = if xs.is_empty() { 1 } else { 0 }
    xs.free();

    first + second + empty_bonus
}
"#,
    )
    .expect("write vec program");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("44\n");
}

#[test]
fn run_wasm_supports_method_call_syntax_for_module_functions() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("main.mitki");
    let counter_module = tempdir.path().join("counter.mitki");

    fs::write(
        &program,
        r#"
mod counter;

fun main(): int {
    crate::counter::new(40).bump(2).get()
}
"#,
    )
    .expect("write method program");

    fs::write(
        &counter_module,
        r#"
struct Counter {
    value: int,
}

fun new(value: int): Counter {
    Counter { value: value }
}

fun bump(counter: Counter, delta: int): Counter {
    Counter { value: counter.value + delta }
}

fun get(counter: Counter): int {
    counter.value
}
"#,
    )
    .expect("write counter module");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42\n");
}

#[test]
fn run_wasm_supports_in_place_method_updates_on_var_locals() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("main.mitki");
    let counter_module = tempdir.path().join("counter.mitki");

    fs::write(
        &program,
        r#"
mod counter;

fun main(): int {
    var counter: crate::counter::Counter = crate::counter::new(40)
    counter.bump(2);
    counter.get()
}
"#,
    )
    .expect("write mutable counter program");

    fs::write(
        &counter_module,
        r#"
struct Counter {
    value: int,
}

fun new(value: int): Counter {
    Counter { value: value }
}

fun bump(var counter: Counter, delta: int) {
    counter.value = counter.value + delta
}

fun get(counter: Counter): int {
    counter.value
}
"#,
    )
    .expect("write counter module");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42\n");
}

#[test]
fn run_wasm_supports_method_calls_inside_loop_bodies() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let program = tempdir.path().join("main.mitki");
    let counter_module = tempdir.path().join("counter.mitki");

    fs::write(
        &program,
        r#"
mod counter;

fun read(counter: crate::counter::Counter): int {
    var result: int = 0
    loop {
        val current: int = counter.get()
        result = current
        break
    }
    result
}

fun main(): int {
    read(crate::counter::new(42))
}
"#,
    )
    .expect("write loop program");

    fs::write(
        &counter_module,
        r#"
struct Counter {
    value: int,
}

fun new(value: int): Counter {
    Counter { value: value }
}

fun get(counter: Counter): int {
    counter.value
}
"#,
    )
    .expect("write counter module");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["run-wasm", program.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stdout("42\n");
}

#[test]
fn build_wasm_rejects_recursive_boundary_types() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("recursive_boundary.mitki");
    let output = tempdir.path().join("recursive_boundary.wasm");

    fs::write(
        &input,
        r#"
enum List {
    Nil,
    Cons(int, List),
}

export fun echo(xs: List): List {
    xs
}
"#,
    )
    .expect("write input");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["build", "--target", "wasm32", input.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stderr(predicates::str::contains("non-copy types like `List`"));

    assert!(
        !output.exists(),
        "build should not leave behind a wasm artifact when it emitted boundary diagnostics"
    );
}

#[test]
fn build_wasm_rejects_by_value_use_of_non_copy_values() {
    let tempdir = tempfile::tempdir().expect("tempdir");
    let input = tempdir.path().join("non_copy_value_use.mitki");
    let output = tempdir.path().join("non_copy_value_use.wasm");

    fs::write(
        &input,
        r#"
struct Token {
    items: [str],

    drop(var self) {
    }
}

fun take(token: Token) {
}

fun main() {
    val token = Token { items: ["a"] }
    take(token)
}
"#,
    )
    .expect("write input");

    Command::cargo_bin("mitki")
        .expect("binary")
        .args(["build", "--target", "wasm32", input.to_str().expect("utf8 path")])
        .assert()
        .success()
        .stderr(predicates::str::contains("non-copy value `Token` cannot be used by value yet"));

    assert!(
        !output.exists(),
        "build should not leave behind a wasm artifact when it emitted non-copy diagnostics"
    );
}

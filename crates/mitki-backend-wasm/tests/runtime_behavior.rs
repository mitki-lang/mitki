mod common;

use std::sync::{Arc, Mutex};

use common::{
    compile_ok, compile_runtime_with_host_callback, decode_i32_array, decode_i32_pair,
    decode_some_string_array, decode_string, decode_string_array, expect_immediate_i32,
    import_instance, symbol_name,
};
use mitki_abi::{AbiScalar, AbiTypeKind, AbiValue, LinkageKind};
use mitki_wasm_runtime::{WasmRuntime, describe_module_abi, invoke_export, invoke_export_instance};

#[test]
fn invoke_export_resolves_logical_names_through_metadata() {
    let bytes = compile_ok(
        r#"
export fun answer(): int {
    42
}
"#,
    );

    let output = invoke_export(&bytes, "answer", &[]).expect("logical export should execute");
    assert!(output.stdout.is_empty(), "expected no runtime stdout");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let typed_export = metadata
        .function_instances
        .iter()
        .find(|instance| {
            instance.linkage == LinkageKind::WasmExport
                && symbol_name(metadata, instance.logical_symbol) == "answer"
        })
        .and_then(|instance| instance.wasm_field_name)
        .map(|id| common::string_value(metadata, id).to_owned())
        .expect("expected generated typed export name");

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::from_binary(&engine, &bytes).expect("valid module");
    let mut store = wasmtime::Store::new(&engine, ());
    let instance = wasmtime::Instance::new(&mut store, &module, &[]).expect("instance");
    assert!(instance.get_func(&mut store, "answer").is_none());
    assert!(instance.get_func(&mut store, typed_export.as_str()).is_some());
}

#[test]
fn invoke_export_returns_canonical_structured_values() {
    let bytes = compile_ok(
        r#"
export fun words(): [str] {
    ["a", "b"]
}
"#,
    );

    let output = invoke_export(&bytes, "words", &[]).expect("export should execute");
    assert!(output.stdout.is_empty(), "expected no runtime stdout");
    let result = output.result.expect("expected canonical array result");
    assert_eq!(decode_string_array(&result), ["a", "b"]);
}

#[test]
fn invoke_export_returns_owned_string_from_raw_bytes() {
    let bytes = compile_ok(
        r#"
use std::str as strings;

export fun main(): str {
    val bytes = str_bytes("hello")
    unsafe {
        strings::from_raw_parts_unchecked(bytes.ptr, bytes.len)
    }
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    let result = output.result.expect("expected string result");
    assert_eq!(decode_string(&result), "hello");
}

#[test]
fn invoke_export_lowers_if_block_branches() {
    let bytes = compile_ok(
        r#"
export fun main(): str {
    val found = true
    if found { "yes" } else { "no" }
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    let result = output.result.expect("expected string result");
    assert_eq!(decode_string(&result), "yes");
}

#[test]
fn invoke_export_reads_wasi_environment_variable_via_std_env_var() {
    let expected = std::env::var("PATH").expect("PATH should exist for inherited WASI env");
    let bytes = compile_ok(
        r#"
use std::env;

export fun main(): str {
    match env::var("PATH") {
        (found, value) => if found { value } else { "" },
    }
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    let result = output.result.expect("expected string result");
    assert_eq!(decode_string(&result), expected);
}

#[test]
fn std_io_print_str_uses_wasi_fd_write_instead_of_mitki_print_str_import() {
    let bytes = compile_ok(
        r#"
export fun main() {
    std::io::print_str("hello");
}
"#,
    );

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::from_binary(&engine, &bytes).expect("valid module");
    let imports = module
        .imports()
        .map(|import| (import.module().to_owned(), import.name().to_owned()))
        .collect::<Vec<_>>();

    assert!(
        imports
            .iter()
            .any(|(module, name)| module == "wasi_snapshot_preview1" && name == "fd_write"),
        "expected wasi fd_write import, got {imports:?}"
    );
    assert!(
        !imports.iter().any(|(module, name)| module == "mitki" && name == "print_str"),
        "std::io::print_str should not use mitki print_str import, got {imports:?}"
    );
}

#[test]
fn invoke_export_returns_canonical_nested_enum_values() {
    let bytes = compile_ok(
        r#"
enum Words {
    Some([str]),
    Empty,
}

export fun words(): Words {
    Words.Some(["a", "b"])
}
"#,
    );

    let output = invoke_export(&bytes, "words", &[]).expect("export should execute");
    let result = output.result.expect("expected canonical enum result");
    assert_eq!(decode_some_string_array(&result), ["a", "b"]);
}

#[test]
fn host_function_callbacks_use_abivalue_for_canonical_round_trips() {
    let bytes = compile_ok(
        r#"
import "env" fun round_trip(xs: [int]): [int];

export fun main(): [int] {
    round_trip([20, 22])
}
"#,
    );

    let seen = Arc::new(Mutex::new(None));
    let mut runtime = compile_runtime_with_host_callback(&bytes, &seen);

    let output = runtime.invoke_export("main", &[]).expect("main should execute");
    assert!(output.stdout.is_empty(), "expected no runtime stdout");

    let received =
        seen.lock().expect("host arg lock").clone().expect("host arg should be captured");
    assert_eq!(decode_i32_array(&received), [20, 22]);
    assert_eq!(decode_i32_array(output.result.as_ref().expect("expected result")), [20, 22]);
}

#[test]
fn invoke_export_preserves_tuple_fields() {
    let bytes = compile_ok(
        r#"
export fun make_pair(): (int, int) {
    (20, 22)
}
"#,
    );

    let output = invoke_export(&bytes, "make_pair", &[]).expect("export should execute");
    assert_eq!(decode_i32_pair(output.result.as_ref().expect("expected tuple result")), [20, 22]);
}

#[test]
fn wasm_runtime_supports_internal_union_values() {
    let bytes = compile_ok(
        r#"
export fun main() {
    val id = if true { 42 } else { true };
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    assert!(output.stdout.is_empty(), "expected no runtime stdout");
    assert!(output.result.is_none(), "unit export should not produce a result");
}

#[test]
fn invoke_export_instance_disambiguates_monomorphized_exports() {
    let bytes = compile_ok(
        r#"
fun id[T](value: T): T {
    value
}

export instance id[int];
export instance id[bool];
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let export_instances = metadata
        .function_instances
        .iter()
        .filter(|instance| {
            instance.linkage == LinkageKind::WasmExport
                && symbol_name(metadata, instance.logical_symbol) == "id"
        })
        .collect::<Vec<_>>();
    assert_eq!(export_instances.len(), 2, "expected two export instances");

    let int_instance = export_instances
        .iter()
        .find(|instance| {
            matches!(metadata.types[instance.type_args[0].0 as usize].kind, AbiTypeKind::Int { .. })
        })
        .expect("expected int export instance");
    let bool_instance = export_instances
        .iter()
        .find(|instance| {
            matches!(metadata.types[instance.type_args[0].0 as usize].kind, AbiTypeKind::Bool)
        })
        .expect("expected bool export instance");

    let int_output = invoke_export_instance(
        &bytes,
        int_instance.id,
        &[AbiValue::Immediate(AbiScalar::Int { signed: true, bits: 32, value: 42 })],
    )
    .expect("int instance should execute");
    expect_immediate_i32(int_output.result.as_ref().expect("expected int result"), 42);

    let bool_output = invoke_export_instance(
        &bytes,
        bool_instance.id,
        &[AbiValue::Immediate(AbiScalar::Bool(true))],
    )
    .expect("bool instance should execute");
    assert_eq!(bool_output.result, Some(AbiValue::Immediate(AbiScalar::Bool(true))));

    let ambiguous = invoke_export(&bytes, "id", &[AbiValue::Immediate(AbiScalar::Bool(true))])
        .expect_err("logical-name invocation should reject ambiguous generic export instances");
    assert!(ambiguous.to_string().contains("multiple exports named `id`"));
}

#[test]
fn compile_supports_inherent_style_method_calls() {
    let bytes = compile_ok(
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

export fun main(): int {
    new(40).bump(2).get()
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn compile_supports_in_place_method_updates_on_var_locals() {
    let bytes = compile_ok(
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

export fun main(): int {
    var counter: Counter = new(40)
    counter.bump(2);
    counter.get()
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn compile_preserves_mutable_aggregate_writes_after_statement_if() {
    let bytes = compile_ok(
        r#"
use std::alloc::int as alloc;

struct Bag {
    ptr: *mut int,
    len: u32,
    cap: u32,
}

fun new(): Bag {
    unsafe {
        Bag { ptr: alloc::alloc_items(1), len: 0, cap: 1 }
    }
}

fun free(var bag: Bag) {
    unsafe {
        alloc::dealloc_items(bag.ptr, bag.cap);
    }
}

fun push(var bag: Bag, value: int) {
    val index: u32 = bag.len
    if index == 99 {
        val then_noop: u32 = index
    } else {
        val else_noop: u32 = index
    }
    unsafe {
        val ptr: *mut int = bag.ptr
        ptr_write(ptr_add(ptr, index), value);
    }
    bag.len = bag.len + 1
}

export fun main(): int {
    var bag: Bag = new()
    bag.push(42);
    val written: int = unsafe { ptr_read(bag.ptr) }
    bag.free();
    written
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn compile_supports_direct_field_assignment_on_var_locals() {
    let bytes = compile_ok(
        r#"
struct Counter {
    value: int,
}

fun new(value: int): Counter {
    Counter { value: value }
}

export fun main(): int {
    var counter: Counter = new(40)
    counter.value = counter.value + 2
    counter.value
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn compile_supports_std_vec_int_mutation() {
    let bytes = compile_ok(
        r#"
use std::vec::int as vec;

export fun main(): int {
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
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 44);
}

#[test]
fn compile_supports_std_vec_int_push_reallocation_write() {
    let bytes = compile_ok(
        r#"
use std::vec::int as vec;

export fun main(): int {
    var xs: vec::Vec = vec::with_capacity(1)
    xs.push(1);
    xs.push(2);
    xs.push(3);
    xs.push(4);
    xs.push(5);

    val last: int = match xs.get(4) {
        .Some(value) => value,
        .None => 0,
    }
    xs.free();
    last
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 5);
}

#[test]
fn compile_supports_std_vec_int_bounds_checked_get_and_set() {
    let bytes = compile_ok(
        r#"
use std::vec::int as vec;

export fun main(): int {
    var xs: vec::Vec = vec::new()
    xs.push(20);
    xs.push(22);
    xs.set(5, 99);

    val second: int = match xs.get(1) {
        .Some(value) => value,
        .None => 0,
    }
    val missing_bonus: int = match xs.get(5) {
        .Some(_) => 1000,
        .None => 1,
    }

    xs.free();
    second + missing_bonus
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 23);
}

#[test]
fn enum_constructors_still_work_alongside_method_syntax() {
    let bytes = compile_ok(
        r#"
enum Answer {
    Some(int),
    None,
}

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

export fun main(): int {
    val answer: Answer = Answer.Some(new(40).bump(2).get())
    match answer {
        .Some(value) => value,
        .None => 0,
    }
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn host_function_instance_binds_one_generic_import_instance() {
    let bytes = compile_ok(
        r#"
import "env" fun id[T](value: T): T;
import instance id[int];

export fun main(): int {
    id(41)
}
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let import = import_instance(metadata, "env", "id");
    assert_eq!(import.type_args.len(), 1, "expected one type argument");
    assert!(matches!(metadata.types[import.type_args[0].0 as usize].kind, AbiTypeKind::Int { .. }));

    let mut runtime = WasmRuntime::builder()
        .host_function_instance(import.id, |args| {
            assert_eq!(args.len(), 1, "expected one argument");
            let AbiValue::Immediate(AbiScalar::Int { value, .. }) = args[0] else {
                panic!("expected i32 argument, got {:?}", args[0]);
            };
            Ok(Some(AbiValue::Immediate(AbiScalar::Int {
                signed: true,
                bits: 32,
                value: value + 1,
            })))
        })
        .instantiate(&bytes)
        .expect("runtime should instantiate with concrete import instance handler");

    let output = runtime.invoke_export("main", &[]).expect("main should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn unsafe_pointer_intrinsics_round_trip_scalars() {
    let bytes = compile_ok(
        r#"
unsafe fun scalar_round_trip(): int {
    val out: *mut u32 = stack_alloc(1)
    ptr_write(out, 42);
    if ptr_read(out) == 42 {
        1
    } else {
        0
    }
}

export fun main(): int {
    unsafe {
        scalar_round_trip()
    }
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("unsafe pointer round-trip should run");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 1);
}

#[test]
fn canonical_pipeline_handles_nested_if_and_match_values() {
    let bytes = compile_ok(
        r#"
enum Score {
    Good(int),
    Bad(int),
}

fun choose(flag: bool, value: Score): int {
    if flag {
        match value {
            .Good(v) => v,
            .Bad(v) => if v == 41 { 42 } else { v },
        }
    } else {
        0
    }
}

export fun main(): int {
    choose(true, Score.Bad(41))
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("main should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn canonical_pipeline_handles_aggregate_joins_and_field_access() {
    let bytes = compile_ok(
        r#"
struct Pair {
    left: int,
    right: int,
}

fun choose(flag: bool): Pair {
    if flag {
        Pair { left: 20, right: 22 }
    } else {
        Pair { left: 1, right: 2 }
    }
}

export fun main(): int {
    val pair: Pair = choose(true)
    pair.left + pair.right
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("main should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn canonical_pipeline_handles_branch_owned_arrays() {
    let bytes = compile_ok(
        r#"
fun choose(flag: bool): [str] {
    if flag {
        ["a", "b"]
    } else {
        ["c"]
    }
}

export fun main(): [str] {
    choose(true)
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("main should execute");
    assert_eq!(decode_string_array(output.result.as_ref().expect("expected result")), ["a", "b"]);
}

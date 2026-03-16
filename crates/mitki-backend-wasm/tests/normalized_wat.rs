mod common;

use common::{
    assert_func_signature, compile_err, compile_ok, compile_ok_with_options, expect_diagnostic,
    normalized_wat,
};
use mitki_backend_wasm::CompileOptions;

#[test]
fn wasm_codegen_is_deterministic_across_repeated_compilation() {
    let fixture = r#"
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
"#;

    let first = compile_ok(fixture);
    let second = compile_ok(fixture);
    assert_eq!(first, second, "expected emitted module bytes to stay deterministic");
}

#[test]
fn default_backend_matches_explicit_canonical_mir_pipeline() {
    let fixture = r#"
fun choose(flag: bool): int {
    if flag { 42 } else { 0 }
}

export fun main(): int {
    choose(true)
}
"#;

    let default_bytes = compile_ok(fixture);
    let explicit_bytes = compile_ok_with_options(fixture, CompileOptions);
    assert_eq!(default_bytes, explicit_bytes);
}

#[test]
fn canonical_pipeline_emits_structured_loops() {
    let bytes = compile_ok(
        r#"
export fun main() {
    loop {
        ()
    }
}
"#,
    );
    let wat = normalized_wat(&bytes);
    assert!(wat.contains("loop"), "WAT missing expected loop construct:\n{wat}");
}

#[test]
fn canonical_pipeline_handles_closure_bodies_and_indirect_calls() {
    let bytes = compile_ok(
        r#"
fun apply(f: fun(int) -> int, x: int): int {
    f(x)
}

export fun main(): int {
    val offset: int = 20
    val add: fun(int) -> int = { value in value + offset }
    apply(add, 22)
}
"#,
    );
    let wat = normalized_wat(&bytes);
    assert!(wat.contains("call_indirect"), "WAT missing indirect call:\n{wat}");
}

#[test]
fn canonical_pipeline_handles_unsafe_pointer_ops() {
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
    let wat = normalized_wat(&bytes);
    assert!(wat.contains("i32.store"), "WAT missing expected store:\n{wat}");
    assert!(wat.contains("i32.load"), "WAT missing expected load:\n{wat}");
}

#[test]
fn raw_wasi_imports_lower_pointers_to_i32_and_rights_to_i64() {
    let bytes = compile_ok(
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

unsafe fun open_readme_errno(): u16 {
    val path: ByteSlice = str_bytes("README.md")
    val out: *mut u32 = stack_alloc(1)
    path_open(3, 0, path.ptr, path.len, 0, 2, 0, 0, out)
}

export fun main(): int {
    unsafe {
        if open_readme_errno() == 0 {
            1
        } else {
            0
        }
    }
}
"#,
    );

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::from_binary(&engine, &bytes).expect("valid module");
    let import_ty = module
        .imports()
        .find(|import| import.module() == "wasi_snapshot_preview1" && import.name() == "path_open")
        .map(|import| import.ty())
        .expect("expected raw WASI import");
    assert_func_signature(
        &import_ty,
        &[
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
            wasmtime::ValType::I64,
            wasmtime::ValType::I64,
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
        ],
        &[wasmtime::ValType::I32],
    );
}

#[test]
fn canonical_pipeline_preserves_compile_failure_diagnostics() {
    let errors = compile_err(
        r#"
export fun id[T](value: T): T {
    value
}
"#,
    );
    expect_diagnostic(&errors, "Wasm imports and exports do not support generic functions");
}

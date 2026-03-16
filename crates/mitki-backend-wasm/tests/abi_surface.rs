mod common;

use common::{
    assert_func_signature, assert_metadata_requires_handles, assert_metadata_version_bump_rejected,
    assert_shared_abi_contract, compile_err, compile_ok, expect_diagnostic, export_instance,
    import_instance, string_value, unsupported_imports,
};
use mitki_abi::AbiTypeKind;
use mitki_wasm_runtime::describe_module_abi;

#[test]
fn canonical_pipeline_emits_wasm_matching_shared_abi_contract() {
    let bytes = compile_ok(
        r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];

export fun main(): int {
    round_trip(42)
}
"#,
    );
    assert_shared_abi_contract(&bytes);
}

#[test]
fn describe_module_abi_exposes_metadata_and_generated_typed_exports() {
    let bytes = compile_ok(
        r#"
export fun answer(): int {
    42
}
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let instance = export_instance(metadata, "answer");
    let typed_export = instance
        .wasm_field_name
        .map(|id| string_value(metadata, id).to_owned())
        .expect("expected generated typed export name");

    assert_eq!(instance.domain, mitki_abi::ExecutionDomain::Runtime);
    assert!(typed_export.starts_with("mitki:typed/2/f$"));
    assert!(abi.exports.iter().any(|export| export.name == typed_export));
    assert!(!abi.exports.iter().any(|export| export.name == "answer"));
}

#[test]
fn typed_wasm_signatures_follow_v2_transport_refs() {
    let bytes = compile_ok(
        r#"
import "env" fun round_trip(pair: (int, int)): (int, int);

export fun make_pair(): (int, int) {
    round_trip((20, 22))
}
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let export_name = export_instance(metadata, "make_pair")
        .wasm_field_name
        .map(|id| string_value(metadata, id).to_owned())
        .expect("expected generated typed export name");
    let import = import_instance(metadata, "env", "round_trip");
    let import_field =
        string_value(metadata, import.wasm_field_name.expect("expected import field"));

    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::from_binary(&engine, &bytes).expect("valid module");
    let import_ty = module
        .imports()
        .find(|entry| entry.module() == "env" && entry.name() == import_field)
        .map(|entry| entry.ty())
        .expect("expected typed import");
    assert_func_signature(&import_ty, &[wasmtime::ValType::I32], &[wasmtime::ValType::I32]);

    let export_ty = module
        .exports()
        .find(|entry| entry.name() == export_name)
        .map(|entry| entry.ty())
        .expect("expected generated typed export");
    assert_func_signature(&export_ty, &[], &[wasmtime::ValType::I32]);
}

#[test]
fn collect_unsupported_imports_reports_unconfigured_host_imports() {
    let bytes = compile_ok(
        r#"
import "env" fun host(): int;

export fun answer(): int {
    host()
}
"#,
    );

    assert_eq!(unsupported_imports(&bytes), ["env::host"]);
}

#[test]
fn describe_module_abi_rejects_newer_metadata_encodings() {
    let bytes = compile_ok(
        r#"
export fun answer(): int {
    42
}
"#,
    );

    assert_metadata_version_bump_rejected(&bytes);
}

#[test]
fn wasm_boundary_supports_top_level_function_typed_parameters() {
    let bytes = compile_ok(
        r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
    );

    assert_metadata_requires_handles(&bytes);

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let export_name = export_instance(metadata, "id")
        .wasm_field_name
        .map(|id| string_value(metadata, id).to_owned())
        .expect("expected generated typed export name");
    let import = import_instance(metadata, "env", "round_trip");
    let import_field =
        string_value(metadata, import.wasm_field_name.expect("expected import field"));

    let module =
        wasmtime::Module::from_binary(&wasmtime::Engine::default(), &bytes).expect("valid module");
    let import_ty = module
        .imports()
        .find(|entry| entry.module() == "env" && entry.name() == import_field)
        .map(|entry| entry.ty())
        .expect("expected typed function import");
    assert_func_signature(&import_ty, &[wasmtime::ValType::I32], &[wasmtime::ValType::I32]);

    let export_ty = module
        .exports()
        .find(|entry| entry.name() == export_name)
        .map(|entry| entry.ty())
        .expect("expected generated typed export");
    assert_func_signature(&export_ty, &[wasmtime::ValType::I32], &[wasmtime::ValType::I32]);
}

#[test]
fn wasm_boundary_supports_structs_with_function_fields() {
    let bytes = compile_ok(
        r#"
struct Wrapper {
    f: fun(int) -> int,
}

fun add_one(x: int): int {
    x + 1
}

export fun make_wrapper(): Wrapper {
    Wrapper { f: add_one }
}

export fun echo_wrapper(value: Wrapper): Wrapper {
    value
}
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let make_instance = export_instance(metadata, "make_wrapper");
    let make_signature = &metadata.signatures[make_instance.signature.0 as usize];
    let wrapper_ty = make_signature.result.semantic_type;
    let AbiTypeKind::Struct { fields, .. } = &metadata.types[wrapper_ty.0 as usize].kind else {
        panic!("expected Wrapper semantic type");
    };
    let handle_ty = fields.first().expect("expected Wrapper field").ty;
    assert!(matches!(metadata.types[handle_ty.0 as usize].kind, AbiTypeKind::Function { .. }));
    assert_eq!(make_signature.result.transport_class, mitki_abi::TransportClass::CanonicalValue);
}

#[test]
fn wasm_boundary_rejects_recursive_boundary_types() {
    let diagnostics = compile_err(
        r#"
enum List {
    Nil,
    Cons(int, List),
}

export fun echo(xs: List): List {
    xs
}
"#,
    );

    expect_diagnostic(&diagnostics, "the current `wasm-core-v2/m32` backend does not support");
    expect_diagnostic(&diagnostics, "recursive types are not supported yet");
}

#[test]
fn wasm_boundary_supports_union_boundary_types() {
    let bytes = compile_ok(
        r#"
export fun echo(value: int | str): int | str {
    value
}
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let instance = export_instance(metadata, "echo");
    let signature = &metadata.signatures[instance.signature.0 as usize];
    let union_ty = signature.params[0].semantic_type;
    let AbiTypeKind::Union { members } = &metadata.types[union_ty.0 as usize].kind else {
        panic!("expected union semantic type");
    };
    assert_eq!(signature.params[0].transport_class, mitki_abi::TransportClass::CanonicalValue);
    assert_eq!(signature.result.transport_class, mitki_abi::TransportClass::CanonicalValue);
    assert_eq!(members.len(), 2);
}

#[test]
fn wasm_boundary_round_trips_erased_intersection_boundary_types() {
    let bytes = compile_ok(
        r#"
export fun echo(value: int & int): int & int {
    value
}
"#,
    );

    let abi = describe_module_abi(&bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    let instance = export_instance(metadata, "echo");
    let signature = &metadata.signatures[instance.signature.0 as usize];
    assert_eq!(signature.params[0].transport_class, mitki_abi::TransportClass::Immediate);
    assert_eq!(signature.result.transport_class, mitki_abi::TransportClass::Immediate);
}

#[test]
fn wasm_exports_reject_generic_functions() {
    let diagnostics = compile_err(
        r#"
export fun id[T](value: T): T {
    value
}
"#,
    );
    expect_diagnostic(&diagnostics, "Wasm imports and exports do not support generic functions");
}

#[test]
fn wasm_imports_require_explicit_generic_instances() {
    let diagnostics = compile_err(
        r#"
import "env" fun id[T](value: T): T;

export fun main(value: int): int {
    id(value)
}
"#,
    );
    expect_diagnostic(&diagnostics, "`import instance` declaration is required");
}

#[test]
fn wasm_imports_with_bodies_fail_boundary_legality() {
    let diagnostics = compile_err(
        r#"
import "env" fun host(): int {
    42
}
"#,
    );
    expect_diagnostic(&diagnostics, "Imported Wasm functions cannot have a body");
}

#[test]
fn safe_wasm_exports_reject_pointer_types() {
    let diagnostics = compile_err(
        r#"
unsafe fun leak(): *mut u32 {
    val out: *mut u32 = stack_alloc(1)
    out
}

export fun main(): *mut u32 {
    unsafe { leak() }
}
"#,
    );
    expect_diagnostic(
        &diagnostics,
        "typed Wasm imports/exports do not allow pointer types like `*mut u32`",
    );
}

#[test]
fn raw_wasi_fd_read_helpers_can_build_iovecs_in_linear_memory() {
    let bytes = compile_ok(
        r#"
extern struct MutByteSlice {
    ptr: *mut u8,
    len: u32,
}

extern struct WasiIovec {
    buf: *mut u8,
    buf_len: u32,
}

import "wasi_snapshot_preview1" unsafe fun fd_read(
    fd: u32,
    iovs: *const WasiIovec,
    iovs_len: u32,
    nread: *mut u32,
): u16;

unsafe fun read_once(fd: u32): u16 {
    val chunk: [u8] = [0, 0, 0, 0]
    val bytes: MutByteSlice = array_mut_bytes(chunk)
    val iov_out: *mut WasiIovec = stack_alloc(1)
    val nread_out: *mut u32 = stack_alloc(1)

    ptr_write(nread_out, 0);
    ptr_write(iov_out, WasiIovec { buf: bytes.ptr, buf_len: bytes.len });
    fd_read(fd, iov_out, 1, nread_out)
}

export fun main(): int {
    unsafe {
        if read_once(0) == 0 {
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
        .find(|import| import.module() == "wasi_snapshot_preview1" && import.name() == "fd_read")
        .map(|import| import.ty())
        .expect("expected raw WASI fd_read import");
    assert_func_signature(
        &import_ty,
        &[
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
            wasmtime::ValType::I32,
        ],
        &[wasmtime::ValType::I32],
    );
}

#[test]
fn unsafe_operations_require_unsafe_context() {
    let diagnostics = compile_err(
        r#"
import "wasi_snapshot_preview1" unsafe fun fd_close(fd: u32): u16;

export fun main(): int {
    fd_close(3)
    0
}
"#,
    );

    expect_diagnostic(&diagnostics, "unsafe operation requires an `unsafe` block or `unsafe fun`");
}

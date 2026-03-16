mod common;

use common::{compile_err, compile_ok, expect_diagnostic, expect_immediate_i32};
use mitki_wasm_runtime::invoke_export;

#[test]
fn comptime_evaluates_direct_values_into_runtime_exports() {
    let bytes = compile_ok(
        r#"
comptime fun build(): int {
    42
}

export fun main(): int {
    comptime(build())
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn comptime_supports_nested_evaluation() {
    let bytes = compile_ok(
        r#"
comptime fun base(): int {
    40
}

comptime fun build(): int {
    comptime(base()) + 2
}

export fun main(): int {
    comptime(build())
}
"#,
    );

    let output = invoke_export(&bytes, "main", &[]).expect("export should execute");
    expect_immediate_i32(output.result.as_ref().expect("expected result"), 42);
}

#[test]
fn comptime_rejects_non_lowerable_results() {
    let diagnostics = compile_err(
        r#"
fun add_one(x: int): int {
    x + 1
}

comptime fun build(): fun(int) -> int {
    add_one
}

export fun main(): int {
    comptime(build())
    42
}
"#,
    );

    expect_diagnostic(
        &diagnostics,
        "comptime requires the target function to return a runtime-lowerable value",
    );
}

#[test]
fn comptime_rejects_runtime_imports_in_stage_code() {
    let diagnostics = compile_err(
        r#"
comptime fun build() {
    std::io::print_int(7)
}

export fun main() {
    comptime(build())
}
"#,
    );

    expect_diagnostic(&diagnostics, "comptime functions cannot call imported Wasm functions");
}

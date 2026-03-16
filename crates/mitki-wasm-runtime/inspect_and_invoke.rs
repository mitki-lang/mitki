use mitki_abi::{AbiScalar, AbiValue};
use mitki_comptime_wasm::compile_file_to_wasm;
use mitki_wasm_runtime::{WasmRuntime, describe_module_abi};
use mitki_db::RootDatabase;
use mitki_inputs::File;

fn main() -> anyhow::Result<()> {
    let db = RootDatabase::default();
    let file = File::new(
        &db,
        "inspect_and_invoke.mitki".into(),
        r#"
import "env" fun host_base(): int;

export fun sum_host_base(): int {
    host_base() + 22
}
"#
        .to_owned(),
    );

    let bytes = compile_file_to_wasm(&db, file).map_err(|diagnostics| {
        let messages = diagnostics
            .iter()
            .map(|diagnostic| diagnostic.message().to_owned())
            .collect::<Vec<_>>();
        anyhow::anyhow!("expected Wasm compilation to succeed, got diagnostics: {messages:?}")
    })?;

    let abi = describe_module_abi(&bytes)?;
    println!("abi v2 metadata: {}", if abi.metadata.is_some() { "present" } else { "missing" });
    println!("imports:");
    for import in &abi.imports {
        println!("  {}::{}", import.module, import.name);
    }
    println!("exports:");
    for export in &abi.exports {
        println!("  {}", export.name);
    }

    let mut runtime = WasmRuntime::builder()
        .host_function("env", "host_base", |_args| {
            Ok(Some(AbiValue::Immediate(AbiScalar::Int { signed: true, bits: 32, value: 20 })))
        })
        .instantiate(&bytes)?;
    let output = runtime.invoke_export("sum_host_base", &[])?;

    println!("stdout: {:?}", output.stdout);
    println!("result: {:?}", output.result);
    Ok(())
}

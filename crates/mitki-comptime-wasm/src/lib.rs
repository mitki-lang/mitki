mod stage;
mod stage_runtime;

pub use mitki_backend_wasm::CompileOptions;
pub type WasmCodegenOptions = CompileOptions;
pub use stage::{
    compile_file_to_wasm, compile_file_to_wasm_with_options, compile_function_to_wasm,
    compile_function_to_wasm_with_options,
};

use std::sync::Arc;

use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_lower::item::scope::FunctionLocation;

use crate::{Backend, BoundaryLegalityValidator, CapabilityValidator};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CompileOptions;

pub trait ComptimeEvaluator: Send + Sync {
    fn eval_comptime_function(
        &self,
        db: &dyn salsa::Database,
        function: FunctionLocation<'_>,
    ) -> Result<crate::AbiValue, String>;
}

#[derive(Default)]
pub struct NoopComptimeEvaluator;

impl ComptimeEvaluator for NoopComptimeEvaluator {
    fn eval_comptime_function(
        &self,
        _db: &dyn salsa::Database,
        _function: FunctionLocation<'_>,
    ) -> Result<crate::AbiValue, String> {
        Err("comptime evaluation requires the `mitki-comptime-wasm` integration layer".to_owned())
    }
}

pub struct CompileConfig {
    pub options: CompileOptions,
    pub comptime_evaluator: Arc<dyn ComptimeEvaluator>,
}

impl CompileConfig {
    pub fn with_options(options: CompileOptions) -> Self {
        Self { options, ..Self::default() }
    }
}

impl Default for CompileConfig {
    fn default() -> Self {
        Self { options: CompileOptions, comptime_evaluator: Arc::new(NoopComptimeEvaluator) }
    }
}

fn format_internal_wasm_validation_error(error: &wasmparser::BinaryReaderError) -> String {
    let message = error.to_string();
    message.split_once(" (at offset ").map_or(message.clone(), |(prefix, _)| prefix.to_owned())
}

fn validate_final_wasm(bytes: &[u8], range: mitki_errors::TextRange) -> Result<(), Diagnostic> {
    wasmparser::Validator::new().validate_all(bytes).map(|_| ()).map_err(|error| {
        Diagnostic::error(
            format!(
                "internal Wasm validation failed: {}",
                format_internal_wasm_validation_error(&error)
            ),
            range,
        )
    })
}

fn compile_checked_module<'db>(
    mut backend: Backend<'db>,
    collect_stage_diagnostics: bool,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    backend.collect_reachable_program();
    if collect_stage_diagnostics {
        backend.collect_stage_diagnostics();
    }
    if !backend.diagnostics.is_empty() {
        return Err(backend.diagnostics);
    }
    let legality_diagnostics = BoundaryLegalityValidator::check(&backend);
    if !legality_diagnostics.is_empty() {
        return Err(legality_diagnostics);
    }
    if let Err(diagnostic) = CapabilityValidator::check(&backend) {
        return Err(vec![diagnostic]);
    }

    let bytes = match backend.emit_module() {
        Ok(bytes) => bytes,
        Err(diagnostic) => return Err(vec![diagnostic]),
    };
    validate_final_wasm(&bytes, backend.file_range()).map_err(|diagnostic| vec![diagnostic])?;
    Ok(bytes)
}

pub fn compile_file(
    db: &dyn salsa::Database,
    file: File,
    config: CompileConfig,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    let diagnostics = mitki_analysis::check_file(db, file);
    if !diagnostics.is_empty() {
        return Err(diagnostics.to_owned());
    }
    let runtime_diagnostics = mitki_analysis::check_runtime_file(db, file);
    if !runtime_diagnostics.is_empty() {
        return Err(runtime_diagnostics.to_owned());
    }

    compile_checked_module(
        Backend::new_file_with_options(db, file, config.options, config.comptime_evaluator),
        false,
    )
}

pub fn compile_file_with_options(
    db: &dyn salsa::Database,
    file: File,
    options: CompileOptions,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    compile_file(db, file, CompileConfig::with_options(options))
}

pub fn compile_function(
    db: &dyn salsa::Database,
    function: FunctionLocation<'_>,
    config: CompileConfig,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    let diagnostics = mitki_analysis::check_function(db, function);
    if !diagnostics.is_empty() {
        return Err(diagnostics.to_owned());
    }
    let runtime_diagnostics = mitki_analysis::check_runtime_function(db, function);
    if !runtime_diagnostics.is_empty() {
        return Err(runtime_diagnostics.to_owned());
    }

    compile_checked_module(
        Backend::new_stage_with_options(
            db,
            function.file(db),
            function,
            config.options,
            config.comptime_evaluator,
        ),
        true,
    )
}

pub fn compile_function_with_options(
    db: &dyn salsa::Database,
    function: FunctionLocation<'_>,
    options: CompileOptions,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    compile_function(db, function, CompileConfig::with_options(options))
}

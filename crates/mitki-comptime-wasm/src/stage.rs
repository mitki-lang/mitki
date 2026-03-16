use std::cell::RefCell;
use std::sync::Arc;

use mitki_abi::AbiValue;
use mitki_backend_wasm::{
    CompileConfig, CompileOptions, ComptimeEvaluator, compile_file, compile_function,
};
use mitki_errors::Diagnostic;
use mitki_inputs::File;
use mitki_lower::item::scope::FunctionLocation;
use rustc_hash::FxHashMap;
use salsa::plumbing::AsId as _;

use crate::stage_runtime::run_stage_export;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct EvalCacheKey {
    db_ptr: usize,
    function_bits: u64,
}

thread_local! {
    static COMPTIME_EVAL_STACK: RefCell<Vec<EvalCacheKey>> = const { RefCell::new(Vec::new()) };
    static COMPTIME_EVAL_CACHE: RefCell<FxHashMap<EvalCacheKey, Result<AbiValue, String>>> =
        RefCell::new(FxHashMap::default());
}

#[derive(Default)]
struct RuntimeComptimeEvaluator;

impl ComptimeEvaluator for RuntimeComptimeEvaluator {
    fn eval_comptime_function(
        &self,
        db: &dyn salsa::Database,
        function: FunctionLocation<'_>,
    ) -> Result<AbiValue, String> {
        eval_comptime_function(db, function)
    }
}

fn shared_evaluator() -> Arc<dyn ComptimeEvaluator> {
    Arc::new(RuntimeComptimeEvaluator)
}

fn eval_cache_key(db: &dyn salsa::Database, function: FunctionLocation<'_>) -> EvalCacheKey {
    EvalCacheKey {
        db_ptr: (db as *const dyn salsa::Database as *const ()) as usize,
        function_bits: function.as_id().as_bits(),
    }
}

fn eval_comptime_function(
    db: &dyn salsa::Database,
    function: FunctionLocation<'_>,
) -> Result<AbiValue, String> {
    if COMPTIME_EVAL_STACK.with(|stack| stack.borrow().is_empty()) {
        COMPTIME_EVAL_CACHE.with(|cache| cache.borrow_mut().clear());
    }

    let key = eval_cache_key(db, function);
    if let Some(result) = COMPTIME_EVAL_CACHE.with(|cache| cache.borrow().get(&key).cloned()) {
        return result;
    }
    if COMPTIME_EVAL_STACK.with(|stack| stack.borrow().contains(&key)) {
        return Err("recursive `comptime` evaluation is not supported".to_owned());
    }

    COMPTIME_EVAL_STACK.with(|stack| stack.borrow_mut().push(key));
    let result = (|| {
        let bytes = compile_function_to_wasm_with_options(db, function, CompileOptions).map_err(
            |diagnostics| {
                diagnostics.first().map_or_else(
                    || "comptime evaluation failed".to_owned(),
                    |diagnostic| diagnostic.message().to_owned(),
                )
            },
        )?;
        run_stage_export(db, &bytes, "__mitki_stage_root").map_err(|error| error.to_string())
    })();
    COMPTIME_EVAL_STACK.with(|stack| {
        stack.borrow_mut().pop();
    });
    COMPTIME_EVAL_CACHE.with(|cache| {
        cache.borrow_mut().insert(key, result.clone());
    });
    result
}

pub fn compile_file_to_wasm(
    db: &dyn salsa::Database,
    file: File,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    compile_file_to_wasm_with_options(db, file, CompileOptions)
}

pub fn compile_file_to_wasm_with_options(
    db: &dyn salsa::Database,
    file: File,
    options: CompileOptions,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    compile_file(db, file, CompileConfig { options, comptime_evaluator: shared_evaluator() })
}

pub fn compile_function_to_wasm(
    db: &dyn salsa::Database,
    function: FunctionLocation<'_>,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    compile_function_to_wasm_with_options(db, function, CompileOptions)
}

pub fn compile_function_to_wasm_with_options(
    db: &dyn salsa::Database,
    function: FunctionLocation<'_>,
    options: CompileOptions,
) -> Result<Vec<u8>, Vec<Diagnostic>> {
    compile_function(
        db,
        function,
        CompileConfig { options, comptime_evaluator: shared_evaluator() },
    )
}

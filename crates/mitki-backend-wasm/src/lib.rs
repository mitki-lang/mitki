pub mod abi;
pub mod capability;
pub mod layout;

mod api;
mod backend;

pub use api::{
    CompileConfig, CompileOptions, ComptimeEvaluator, NoopComptimeEvaluator, compile_file,
    compile_file_with_options, compile_function, compile_function_with_options,
};
pub use backend::{
    Backend, BoundaryLegalityValidator, CapabilityValidator, boundary, planning, registry,
    validation,
};
pub use mitki_abi::{AbiScalar, AbiValue, CanonicalBlobView, CanonicalGraph, TransportClass};
use mitki_errors::Diagnostic;

pub use self::boundary::BoundaryPlan;
pub use self::planning::{EmissionObligations, ModulePlan, ReachabilityGraph};

pub fn collect_reachability_and_obligations<'db>(
    backend: &mut Backend<'db>,
    collect_stage_diagnostics: bool,
) -> Result<(), Vec<Diagnostic>> {
    backend.collect_reachable_program();
    if collect_stage_diagnostics {
        backend.collect_stage_diagnostics();
    }
    if backend.diagnostics().is_empty() { Ok(()) } else { Err(backend.diagnostics().to_vec()) }
}

pub fn check_boundary_legality<'db>(backend: &Backend<'db>) -> Result<(), Vec<Diagnostic>> {
    let diagnostics = BoundaryLegalityValidator::check(backend);
    if diagnostics.is_empty() { Ok(()) } else { Err(diagnostics) }
}

pub fn check_capability<'db>(backend: &Backend<'db>) -> Result<(), Diagnostic> {
    CapabilityValidator::check(backend)
}

pub fn build_module_plan<'db>(backend: &Backend<'db>) -> Result<ModulePlan<'db>, Diagnostic> {
    backend.build_module_plan()
}

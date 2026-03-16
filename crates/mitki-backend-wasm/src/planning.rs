#[cfg(test)]
use std::fmt::Write as _;
#[cfg(test)]
use std::sync::Arc;

#[cfg(test)]
use mitki_abi::LinkageKind as AbiV2LinkageKind;
use mitki_abi::SigId;
use mitki_abi_lower::BuiltAbiV2;
use mitki_errors::Diagnostic;
use mitki_hir::ty::Ty;

use super::boundary::{BoundaryPlan, BoundaryPlanner, BoundarySig, InternalSig};
#[cfg(test)]
use super::boundary::{BoundarySlot, TransportOp};
use super::function_kernel::{FunctionKernelBuilder, FunctionKernelBundle};
use super::function_legalize::FunctionLegalizer;
use super::function_ownership::OwnershipLowering;
use super::function_wasm_ir::{StructuredWasmBundle, StructuredWasmLowering};
#[cfg(test)]
use super::registry::SectionExportTarget;
use super::registry::{
    CallableRegistry, CallableRegistryPlan, HelperRegistry, HelperRegistryPlan, NameAssigner,
    SectionAssigner, SectionPlan,
};
use super::wrapper_mir::{WrapperMirBuilder, WrapperMirBundle};
use super::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct ReachabilityRoot<'db> {
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) instance: InstanceKey<'db>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReachabilityGraph<'db> {
    pub(in crate::backend) mode: CompilationMode,
    pub(in crate::backend) target_profile: TargetProfile,
    pub(in crate::backend) roots: Vec<ReachabilityRoot<'db>>,
    pub(in crate::backend) instances: Vec<ReachabilityInstance<'db>>,
    pub(in crate::backend) functions: Vec<InstanceKey<'db>>,
    pub(in crate::backend) closures: Vec<ClosureInstanceKey<'db>>,
    pub(in crate::backend) imports: Vec<InstanceKey<'db>>,
    pub(in crate::backend) exports: Vec<InstanceKey<'db>>,
    pub(in crate::backend) types: Vec<Ty<'db>>,
    pub(in crate::backend) callable_signatures: Vec<FunctionSignature>,
    pub(in crate::backend) edges: Vec<ReachabilityEdge<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum ReachabilityInstance<'db> {
    Function(InstanceKey<'db>),
    Closure(ClosureInstanceKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum ReachabilityNode<'db> {
    Root(usize),
    Function(InstanceKey<'db>),
    Closure(ClosureInstanceKey<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum ReachabilityEdgeKind {
    Root,
    DirectCall,
    ClosureLiteral,
    FunctionValue,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) struct ReachabilityEdge<'db> {
    pub(in crate::backend) from: ReachabilityNode<'db>,
    pub(in crate::backend) to: ReachabilityNode<'db>,
    pub(in crate::backend) kind: ReachabilityEdgeKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum RuntimeImportNeed {
    Runtime(RuntimeFunction),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum CallableAdapterNeed<'db> {
    ClosureEnv(ClosureInstanceKey<'db>),
    BoundaryInvoke(InstanceKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum BoundaryWrapperNeed<'db> {
    Import(InstanceKey<'db>),
    Export(InstanceKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum CanonicalSupportNeed<'db> {
    Type(Ty<'db>),
    BlobHelpers,
    HandleHelpers,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(in crate::backend) enum LayoutNeed<'db> {
    Array(Ty<'db>),
    Nominal(Ty<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EmissionObligations<'db> {
    pub(in crate::backend) runtime_imports: Vec<RuntimeFunction>,
    pub(in crate::backend) stage_intrinsics: Vec<StageIntrinsic>,
    pub(in crate::backend) helpers: Vec<HelperFunction>,
    pub(in crate::backend) reachable_arrays: Vec<Ty<'db>>,
    pub(in crate::backend) reachable_nominals: Vec<Ty<'db>>,
    pub(in crate::backend) runtime_import_needs: Vec<RuntimeImportNeed>,
    pub(in crate::backend) callable_adapters: Vec<CallableAdapterNeed<'db>>,
    pub(in crate::backend) boundary_wrappers: Vec<BoundaryWrapperNeed<'db>>,
    pub(in crate::backend) canonical_support: Vec<CanonicalSupportNeed<'db>>,
    pub(in crate::backend) layout_needs: Vec<LayoutNeed<'db>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) struct FunctionInstanceId(pub(in crate::backend) u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct FunctionInstancePlan<'db> {
    pub(in crate::backend) id: FunctionInstanceId,
    pub(in crate::backend) instance: InstanceKey<'db>,
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) internal_signature: InternalSig,
    pub(in crate::backend) boundary_signature: Option<BoundarySig<'db>>,
    pub(in crate::backend) metadata_index: Option<usize>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(in crate::backend) enum HelperNeed {
    MemoryEq,
    StringEq,
    ArcRetain,
    ArcRelease,
    AbiAlloc,
    AbiBlobRelease,
    AbiHandleRetain,
    AbiHandleRelease,
    NominalDestroy(u32),
    NominalEq(u32),
    ArrayDestroy(u32),
    ArrayEq(u32),
    HandleInvoke(SigId),
}

impl HelperNeed {
    pub(in crate::backend) fn from_helper_function(helper: HelperFunction) -> Self {
        match helper {
            HelperFunction::MemoryEq => Self::MemoryEq,
            HelperFunction::StringEq => Self::StringEq,
            HelperFunction::ArcRetain => Self::ArcRetain,
            HelperFunction::ArcRelease => Self::ArcRelease,
        }
    }

    pub(in crate::backend) fn dump_name(self) -> String {
        match self {
            Self::MemoryEq => "memory_eq".to_owned(),
            Self::StringEq => "string_eq".to_owned(),
            Self::ArcRetain => "arc_retain".to_owned(),
            Self::ArcRelease => "arc_release".to_owned(),
            Self::AbiAlloc => mitki_abi::ABI_V2_ALLOC_EXPORT.to_owned(),
            Self::AbiBlobRelease => mitki_abi::ABI_V2_BLOB_RELEASE_EXPORT.to_owned(),
            Self::AbiHandleRetain => mitki_abi::ABI_V2_HANDLE_RETAIN_EXPORT.to_owned(),
            Self::AbiHandleRelease => mitki_abi::ABI_V2_HANDLE_RELEASE_EXPORT.to_owned(),
            Self::NominalDestroy(bits) => format!("nominal_destroy({bits})"),
            Self::NominalEq(bits) => format!("nominal_eq({bits})"),
            Self::ArrayDestroy(bits) => format!("array_destroy({bits})"),
            Self::ArrayEq(bits) => format!("array_eq({bits})"),
            Self::HandleInvoke(signature) => mitki_abi::handle_invoke_export_name(signature),
        }
    }

    pub(in crate::backend) fn export_name(self) -> Option<String> {
        match self {
            Self::AbiAlloc
            | Self::AbiBlobRelease
            | Self::AbiHandleRetain
            | Self::AbiHandleRelease
            | Self::HandleInvoke(_) => Some(self.dump_name()),
            Self::MemoryEq
            | Self::StringEq
            | Self::ArcRetain
            | Self::ArcRelease
            | Self::NominalDestroy(_)
            | Self::NominalEq(_)
            | Self::ArrayDestroy(_)
            | Self::ArrayEq(_) => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct ImportPlan {
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) metadata_index: usize,
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) module_name: String,
    pub(in crate::backend) field_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct ExportPlan {
    pub(in crate::backend) function_id: FunctionInstanceId,
    pub(in crate::backend) metadata_index: usize,
    pub(in crate::backend) logical_name: String,
    pub(in crate::backend) export_name: String,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct MemoryPlan {
    pub(in crate::backend) memory_index: u32,
    pub(in crate::backend) export_name: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct TablePlan;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct GlobalPlan;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct DataSegmentPlan;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::backend) struct NamePlan {
    pub(in crate::backend) logical_functions: Vec<(FunctionInstanceId, String)>,
    pub(in crate::backend) typed_exports: Vec<(String, FunctionInstanceId)>,
    pub(in crate::backend) helper_exports: Vec<String>,
}

#[derive(Clone)]
pub struct ModulePlan<'db> {
    pub(in crate::backend) mode: CompilationMode,
    pub(in crate::backend) target_profile: TargetProfile,
    pub(in crate::backend) target_decisions: TargetDecisionSnapshot,
    pub(in crate::backend) reachability: ReachabilityGraph<'db>,
    pub(in crate::backend) obligations: EmissionObligations<'db>,
    pub(in crate::backend) function_instances: Vec<FunctionInstancePlan<'db>>,
    pub(in crate::backend) imports: Vec<ImportPlan>,
    pub(in crate::backend) exports: Vec<ExportPlan>,
    pub(in crate::backend) helpers: HelperRegistryPlan,
    pub(in crate::backend) callables: CallableRegistryPlan<'db>,
    pub(in crate::backend) sections: SectionPlan<'db>,
    pub(in crate::backend) memories: Vec<MemoryPlan>,
    pub(in crate::backend) tables: Vec<TablePlan>,
    pub(in crate::backend) globals: Vec<GlobalPlan>,
    pub(in crate::backend) data_segments: Vec<DataSegmentPlan>,
    pub(in crate::backend) abi_preview: BuiltAbiV2,
    pub(in crate::backend) names: NamePlan,
    pub(in crate::backend) boundary: BoundaryPlan<'db>,
    pub(in crate::backend) wrapper_mir: WrapperMirBundle<'db>,
    pub(in crate::backend) function_kernel: Option<FunctionKernelBundle<'db>>,
    pub(in crate::backend) function_wasm_ir: Option<StructuredWasmBundle<'db>>,
}

impl<'db> Backend<'db> {
    pub fn build_module_plan(&self) -> Result<ModulePlan<'db>, Diagnostic> {
        self.target_policies().validate_backend_support(self.file_range())?;
        let reachability = self.shadow_reachability_graph()?;
        let obligations = self.shadow_emission_obligations()?;
        let function_ids = reachability
            .functions
            .iter()
            .enumerate()
            .map(|(index, instance)| {
                (
                    instance.clone(),
                    FunctionInstanceId(
                        u32::try_from(index).expect("function count should fit u32"),
                    ),
                )
            })
            .collect::<FxHashMap<_, _>>();
        let boundary = BoundaryPlanner::build(self, &reachability, &function_ids)?;
        let function_instances =
            self.build_function_instances(&reachability.functions, &function_ids, &boundary)?;
        let abi_preview = boundary
            .metadata
            .build_preview(self.db)
            .map_err(|message| Diagnostic::error(message, self.file_range()))?;
        let helpers = HelperRegistry::build(&obligations, &boundary, &abi_preview);
        let callables = CallableRegistry::build(
            self,
            &reachability,
            &function_instances,
            &boundary,
            &obligations,
            &abi_preview,
        )?;
        let sections = SectionAssigner::build(
            self,
            &reachability,
            &obligations,
            &boundary,
            &abi_preview,
            &helpers,
            &callables,
        )?;
        let imports = Self::build_import_plan(&boundary);
        let exports = Self::build_export_plan(&boundary);
        let memories = vec![MemoryPlan { memory_index: 0, export_name: Some("memory".to_owned()) }];
        let names = NameAssigner::build(&function_instances, &boundary, &helpers);
        let mut plan = ModulePlan {
            mode: reachability.mode,
            target_profile: self.target_profile(),
            target_decisions: self.target_decision_snapshot(),
            reachability,
            obligations,
            function_instances,
            imports,
            exports,
            helpers,
            callables,
            sections,
            memories,
            tables: Vec::new(),
            globals: Vec::new(),
            data_segments: Vec::new(),
            abi_preview,
            names,
            boundary,
            wrapper_mir: WrapperMirBundle {
                imports: Vec::new(),
                callable_adapters: Vec::new(),
                trampolines: Vec::new(),
                exports: Vec::new(),
                import_indices: FxHashMap::default(),
                callable_adapter_indices: FxHashMap::default(),
                trampoline_indices: FxHashMap::default(),
                export_indices: FxHashMap::default(),
            },
            function_kernel: None,
            function_wasm_ir: None,
        };
        plan.wrapper_mir = WrapperMirBuilder::build(self, &plan)?;
        let function_kernel = FunctionKernelBuilder::build(self, &plan)?;
        let function_kernel = OwnershipLowering::run(self, &plan, &function_kernel)?;
        let function_kernel = FunctionLegalizer::run(self, &plan, &function_kernel)?;
        let function_wasm_ir = StructuredWasmLowering::build(self, &plan, &function_kernel)?;
        plan.function_kernel = Some(function_kernel);
        plan.function_wasm_ir = Some(function_wasm_ir.clone());
        Ok(plan)
    }

    #[cfg(test)]
    pub(in crate::backend) fn build_reachability_graph(&self) -> ReachabilityGraph<'db> {
        self.shadow_reachability_graph().unwrap_or_else(|diagnostic| {
            panic!(
                "reachability shadow state should exist before module planning: {}",
                diagnostic.message()
            )
        })
    }

    #[cfg(test)]
    pub(in crate::backend) fn build_emission_obligations(&self) -> EmissionObligations<'db> {
        self.shadow_emission_obligations().unwrap_or_else(|diagnostic| {
            panic!(
                "emission obligation shadow state should exist before module planning: {}",
                diagnostic.message()
            )
        })
    }

    fn shadow_reachability_graph(&self) -> Result<ReachabilityGraph<'db>, Diagnostic> {
        self.shadow_reachability.clone().ok_or_else(|| {
            Diagnostic::error(
                "internal error: module planning requires collected reachability shadow state",
                self.file_range(),
            )
        })
    }

    fn shadow_emission_obligations(&self) -> Result<EmissionObligations<'db>, Diagnostic> {
        self.shadow_obligations.clone().ok_or_else(|| {
            Diagnostic::error(
                "internal error: module planning requires collected emission obligations",
                self.file_range(),
            )
        })
    }

    fn build_function_instances(
        &self,
        reachable_functions: &[InstanceKey<'db>],
        function_ids: &FxHashMap<InstanceKey<'db>, FunctionInstanceId>,
        boundary: &BoundaryPlan<'db>,
    ) -> Result<Vec<FunctionInstancePlan<'db>>, Diagnostic> {
        let boundary_entries = boundary
            .instances
            .iter()
            .map(|entry| (entry.instance.clone(), entry))
            .collect::<FxHashMap<_, _>>();
        let mut functions = Vec::with_capacity(reachable_functions.len());
        for instance in reachable_functions {
            let hir_function = instance.location.hir_function(self.db);
            let function = hir_function.function(self.db);
            let inference = instance.location.infer(self.db);
            let logical_name = instance
                .location
                .source(self.db)
                .name()
                .map_or("<anonymous>".to_owned(), |name| name.as_str().to_owned());
            let internal_signature = InternalSig::from_function_signature(
                self.function_signature(instance, function, inference)?,
            );
            let boundary_entry = boundary_entries.get(instance);
            functions.push(FunctionInstancePlan {
                id: *function_ids
                    .get(instance)
                    .expect("reachable function should have an assigned id"),
                instance: instance.clone(),
                logical_name,
                internal_signature,
                boundary_signature: boundary_entry.map(|entry| entry.signature.clone()),
                metadata_index: boundary_entry.map(|entry| entry.metadata_index),
            });
        }
        Ok(functions)
    }

    fn build_import_plan(boundary: &BoundaryPlan<'db>) -> Vec<ImportPlan> {
        boundary
            .imports
            .iter()
            .map(|entry| ImportPlan {
                function_id: entry.function_id,
                metadata_index: entry.metadata_index,
                logical_name: entry.logical_name.clone(),
                module_name: entry.module_name.clone(),
                field_name: entry.field_name.clone(),
            })
            .collect()
    }

    fn build_export_plan(boundary: &BoundaryPlan<'db>) -> Vec<ExportPlan> {
        boundary
            .exports
            .iter()
            .map(|entry| ExportPlan {
                function_id: entry.function_id,
                metadata_index: entry.metadata_index,
                logical_name: entry.logical_name.clone(),
                export_name: entry.export_name.clone(),
            })
            .collect()
    }
}

impl<'db> EmissionObligations<'db> {
    pub(in crate::backend) fn needs_blob_helpers(&self) -> bool {
        self.canonical_support.iter().any(|need| matches!(need, CanonicalSupportNeed::BlobHelpers))
    }

    pub(in crate::backend) fn needs_handle_helpers(&self) -> bool {
        self.canonical_support
            .iter()
            .any(|need| matches!(need, CanonicalSupportNeed::HandleHelpers))
    }
}

#[cfg(test)]
impl<'db> ReachabilityGraph<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        let mode = self.mode.dump_name();
        writeln!(&mut output, "reachability.mode: {mode}").expect("write to string");
        writeln!(&mut output, "reachability.target: {}", self.target_profile.canonical_name())
            .expect("write to string");
        writeln!(&mut output, "reachability.roots:").expect("write to string");
        for root in &self.roots {
            writeln!(
                &mut output,
                "  - {} => {}",
                root.logical_name,
                format_instance_key(db, &root.instance)
            )
            .expect("write to string");
        }
        writeln!(&mut output, "reachability.functions:").expect("write to string");
        for function in &self.functions {
            writeln!(&mut output, "  - {}", format_instance_key(db, function))
                .expect("write to string");
        }
        writeln!(&mut output, "reachability.closures:").expect("write to string");
        for closure in &self.closures {
            writeln!(&mut output, "  - {}", format_closure_key(db, closure))
                .expect("write to string");
        }
        output
    }

    pub(crate) fn dump_details(&self, db: &dyn salsa::Database) -> String {
        let mut output = self.dump(db);
        writeln!(&mut output, "reachability.instances:").expect("write to string");
        for instance in &self.instances {
            match instance {
                ReachabilityInstance::Function(instance) => {
                    writeln!(&mut output, "  - function {}", format_instance_key(db, instance))
                        .expect("write to string");
                }
                ReachabilityInstance::Closure(instance) => {
                    writeln!(&mut output, "  - closure {}", format_closure_key(db, instance))
                        .expect("write to string");
                }
            }
        }
        writeln!(&mut output, "reachability.imports:").expect("write to string");
        for instance in &self.imports {
            writeln!(&mut output, "  - {}", format_instance_key(db, instance))
                .expect("write to string");
        }
        writeln!(&mut output, "reachability.exports:").expect("write to string");
        for instance in &self.exports {
            writeln!(&mut output, "  - {}", format_instance_key(db, instance))
                .expect("write to string");
        }
        writeln!(&mut output, "reachability.types:").expect("write to string");
        for ty in &self.types {
            writeln!(&mut output, "  - {}", ty.display(db)).expect("write to string");
        }
        writeln!(&mut output, "reachability.callables:").expect("write to string");
        for signature in &self.callable_signatures {
            writeln!(
                &mut output,
                "  - ({}) -> {}",
                signature.params.iter().map(format_abi_ty).collect::<Vec<_>>().join(", "),
                format_abi_ty(&signature.result)
            )
            .expect("write to string");
        }
        writeln!(&mut output, "reachability.edges:").expect("write to string");
        for edge in &self.edges {
            writeln!(
                &mut output,
                "  - {} -> {} ({})",
                format_reachability_node(db, &self.roots, &edge.from),
                format_reachability_node(db, &self.roots, &edge.to),
                format_reachability_edge_kind(edge.kind)
            )
            .expect("write to string");
        }
        output
    }
}

#[cfg(test)]
impl<'db> EmissionObligations<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "obligations.runtime_imports:").expect("write to string");
        for runtime in &self.runtime_imports {
            writeln!(&mut output, "  - {}::{}", runtime.import_module(), runtime.import_name())
                .expect("write to string");
        }
        writeln!(&mut output, "obligations.stage_intrinsics:").expect("write to string");
        for intrinsic in &self.stage_intrinsics {
            writeln!(&mut output, "  - {}", intrinsic.name()).expect("write to string");
        }
        writeln!(&mut output, "obligations.helpers:").expect("write to string");
        for helper in &self.helpers {
            writeln!(&mut output, "  - {}", helper.name()).expect("write to string");
        }
        writeln!(&mut output, "obligations.arrays:").expect("write to string");
        for ty in &self.reachable_arrays {
            writeln!(&mut output, "  - {}", ty.display(db)).expect("write to string");
        }
        writeln!(&mut output, "obligations.nominals:").expect("write to string");
        for ty in &self.reachable_nominals {
            writeln!(&mut output, "  - {}", ty.display(db)).expect("write to string");
        }
        output
    }

    pub(crate) fn dump_details(&self, db: &dyn salsa::Database) -> String {
        let mut output = self.dump(db);
        writeln!(&mut output, "obligations.runtime_import_needs:").expect("write to string");
        for need in &self.runtime_import_needs {
            let RuntimeImportNeed::Runtime(runtime) = need;
            writeln!(&mut output, "  - {}::{}", runtime.import_module(), runtime.import_name())
                .expect("write to string");
        }
        writeln!(&mut output, "obligations.callable_adapters:").expect("write to string");
        for need in &self.callable_adapters {
            match need {
                CallableAdapterNeed::ClosureEnv(closure) => {
                    writeln!(&mut output, "  - closure_env {}", format_closure_key(db, closure))
                        .expect("write to string");
                }
                CallableAdapterNeed::BoundaryInvoke(instance) => {
                    writeln!(
                        &mut output,
                        "  - boundary_invoke {}",
                        format_instance_key(db, instance)
                    )
                    .expect("write to string");
                }
            }
        }
        writeln!(&mut output, "obligations.boundary_wrappers:").expect("write to string");
        for need in &self.boundary_wrappers {
            match need {
                BoundaryWrapperNeed::Import(instance) => {
                    writeln!(&mut output, "  - import {}", format_instance_key(db, instance))
                        .expect("write to string");
                }
                BoundaryWrapperNeed::Export(instance) => {
                    writeln!(&mut output, "  - export {}", format_instance_key(db, instance))
                        .expect("write to string");
                }
            }
        }
        writeln!(&mut output, "obligations.canonical_support:").expect("write to string");
        for need in &self.canonical_support {
            match need {
                CanonicalSupportNeed::Type(ty) => {
                    writeln!(&mut output, "  - type {}", ty.display(db)).expect("write to string");
                }
                CanonicalSupportNeed::BlobHelpers => {
                    writeln!(&mut output, "  - blob_helpers").expect("write to string");
                }
                CanonicalSupportNeed::HandleHelpers => {
                    writeln!(&mut output, "  - handle_helpers").expect("write to string");
                }
            }
        }
        writeln!(&mut output, "obligations.layout_needs:").expect("write to string");
        for need in &self.layout_needs {
            match need {
                LayoutNeed::Array(ty) => {
                    writeln!(&mut output, "  - array {}", ty.display(db)).expect("write to string");
                }
                LayoutNeed::Nominal(ty) => {
                    writeln!(&mut output, "  - nominal {}", ty.display(db))
                        .expect("write to string");
                }
            }
        }
        output
    }
}

#[cfg(test)]
impl<'db> BoundaryPlan<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "boundary.entries:").expect("write to string");
        for entry in &self.instances {
            let module = entry.wasm_module_name.as_deref().unwrap_or("-");
            writeln!(
                &mut output,
                "  - [{}] [f{}] {} {} {} -> {}",
                entry.metadata_index,
                entry.function_id.0,
                match entry.linkage {
                    AbiV2LinkageKind::WasmImport => "import",
                    AbiV2LinkageKind::WasmExport => "export",
                    AbiV2LinkageKind::StageEntry => "stage",
                    AbiV2LinkageKind::RawImport => "raw-import",
                },
                entry.logical_name,
                format_instance_key(db, &entry.instance),
                entry.wasm_field_name
            )
            .expect("write to string");
            writeln!(&mut output, "    module: {module}").expect("write to string");
            if let Some(origin) = &entry.generic_origin_name {
                writeln!(&mut output, "    generic_origin: {origin}").expect("write to string");
            }
            writeln!(
                &mut output,
                "    internal: ({}) -> {}",
                entry
                    .signature
                    .internal
                    .params
                    .iter()
                    .map(format_abi_ty)
                    .collect::<Vec<_>>()
                    .join(", "),
                format_internal_results(&entry.signature.internal.results)
            )
            .expect("write to string");
            writeln!(&mut output, "    boundary.params:").expect("write to string");
            for param in &entry.signature.params {
                writeln!(&mut output, "      - {}", format_boundary_slot(db, param))
                    .expect("write to string");
            }
            writeln!(&mut output, "    boundary.results:").expect("write to string");
            if entry.signature.results.is_empty() {
                writeln!(&mut output, "      - <none>").expect("write to string");
            } else {
                for result in &entry.signature.results {
                    writeln!(&mut output, "      - {}", format_boundary_slot(db, result))
                        .expect("write to string");
                }
            }
        }
        writeln!(&mut output, "boundary.export_aliases:").expect("write to string");
        for alias in &self.aliases {
            writeln!(
                &mut output,
                "  - {} => {}",
                alias.alias,
                format_instance_key(db, &alias.instance)
            )
            .expect("write to string");
        }
        output
    }

    pub(crate) fn dump_aliases(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "boundary.aliases:").expect("write to string");
        for alias in &self.aliases {
            writeln!(
                &mut output,
                "  - [{}] [f{}] {} => {} ({})",
                alias.metadata_index,
                alias.function_id.0,
                alias.alias,
                format_instance_key(db, &alias.instance),
                alias.logical_name
            )
            .expect("write to string");
        }
        output
    }

    pub(crate) fn dump_metadata(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "boundary.metadata:").expect("write to string");
        for (index, function) in self.metadata.functions.iter().enumerate() {
            let module = function.wasm_module_name.as_deref().unwrap_or("-");
            writeln!(
                &mut output,
                "  - [{}] {:?} {} {} -> {}",
                index, function.linkage, function.logical_name, module, function.wasm_field_name
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    params: {}",
                function
                    .param_tys
                    .iter()
                    .map(|ty| ty.display(db).to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
            .expect("write to string");
            writeln!(&mut output, "    result: {}", function.result_ty.display(db))
                .expect("write to string");
        }
        output
    }

    pub(crate) fn dump_wrapper_ops(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "boundary.wrapper_ops:").expect("write to string");
        for import in &self.imports {
            writeln!(
                &mut output,
                "  - import {} ({})",
                import.logical_name,
                format_instance_key(db, &import.instance)
            )
            .expect("write to string");
            writeln!(&mut output, "    params:").expect("write to string");
            for op in &import.wrapper.param_ops {
                writeln!(&mut output, "      - {}", format_transport_op(db, op))
                    .expect("write to string");
            }
            writeln!(&mut output, "    results:").expect("write to string");
            for op in &import.wrapper.result_ops {
                writeln!(&mut output, "      - {}", format_transport_op(db, op))
                    .expect("write to string");
            }
        }
        for export in &self.exports {
            writeln!(
                &mut output,
                "  - export {} ({})",
                export.logical_name,
                format_instance_key(db, &export.instance)
            )
            .expect("write to string");
            writeln!(&mut output, "    params:").expect("write to string");
            for op in &export.wrapper.param_ops {
                writeln!(&mut output, "      - {}", format_transport_op(db, op))
                    .expect("write to string");
            }
            writeln!(&mut output, "    results:").expect("write to string");
            for op in &export.wrapper.result_ops {
                writeln!(&mut output, "      - {}", format_transport_op(db, op))
                    .expect("write to string");
            }
        }
        output
    }
}

#[cfg(test)]
impl<'db> ModulePlan<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        let mode = self.mode.dump_name();
        writeln!(&mut output, "module.mode: {mode}").expect("write to string");
        writeln!(&mut output, "module.target: {}", self.target_profile.canonical_name())
            .expect("write to string");
        writeln!(&mut output, "module.target_decisions: {}", self.target_decisions.dump())
            .expect("write to string");
        output.push_str(&self.reachability.dump(db));
        output.push_str(&self.obligations.dump(db));
        writeln!(&mut output, "function_instances:").expect("write to string");
        for function in &self.function_instances {
            writeln!(
                &mut output,
                "  - [f{}] {} => {}",
                function.id.0,
                function.logical_name,
                format_instance_key(db, &function.instance)
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    internal: ({}) -> {}",
                function
                    .internal_signature
                    .params
                    .iter()
                    .map(format_abi_ty)
                    .collect::<Vec<_>>()
                    .join(", "),
                format_internal_results(&function.internal_signature.results)
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    metadata_index: {}",
                function.metadata_index.map_or_else(|| "-".to_owned(), |index| index.to_string())
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    boundary: {}",
                if function.boundary_signature.is_some() { "present" } else { "none" }
            )
            .expect("write to string");
        }
        writeln!(&mut output, "imports:").expect("write to string");
        for import in &self.imports {
            writeln!(
                &mut output,
                "  - [f{}] [{}] {}::{} ({})",
                import.function_id.0,
                import.metadata_index,
                import.module_name,
                import.field_name,
                import.logical_name
            )
            .expect("write to string");
        }
        writeln!(&mut output, "exports:").expect("write to string");
        for export in &self.exports {
            writeln!(
                &mut output,
                "  - [f{}] [{}] {} ({})",
                export.function_id.0,
                export.metadata_index,
                export.export_name,
                export.logical_name
            )
            .expect("write to string");
        }
        writeln!(&mut output, "helpers:").expect("write to string");
        for helper in &self.helpers.needs {
            writeln!(&mut output, "  - {}", helper.dump_name()).expect("write to string");
        }
        writeln!(&mut output, "memories:").expect("write to string");
        for memory in &self.memories {
            let export_name = memory.export_name.as_deref().unwrap_or("-");
            writeln!(&mut output, "  - memory#{} export={export_name}", memory.memory_index)
                .expect("write to string");
        }
        writeln!(&mut output, "tables: {}", self.tables.len()).expect("write to string");
        writeln!(&mut output, "globals: {}", self.globals.len()).expect("write to string");
        writeln!(&mut output, "data_segments: {}", self.data_segments.len())
            .expect("write to string");
        writeln!(&mut output, "abi_preview.instances:").expect("write to string");
        for (index, function) in self.abi_preview.functions.iter().enumerate() {
            writeln!(
                &mut output,
                "  - [{}] {:?} {} sig=s{}",
                index, function.linkage, function.wasm_field_name, function.signature_id.0
            )
            .expect("write to string");
        }
        writeln!(&mut output, "names.logical_functions:").expect("write to string");
        for (function_id, logical_name) in &self.names.logical_functions {
            writeln!(&mut output, "  - [f{}] {}", function_id.0, logical_name)
                .expect("write to string");
        }
        writeln!(&mut output, "names.typed_exports:").expect("write to string");
        for (alias, function_id) in &self.names.typed_exports {
            writeln!(&mut output, "  - {alias} => f{}", function_id.0).expect("write to string");
        }
        writeln!(&mut output, "names.helper_exports:").expect("write to string");
        for helper in &self.names.helper_exports {
            writeln!(&mut output, "  - {helper}").expect("write to string");
        }
        output.push_str(&self.boundary.dump(db));
        output
    }
}

#[cfg(test)]
impl HelperRegistryPlan {
    pub(crate) fn dump(&self) -> String {
        let mut output = String::new();
        writeln!(&mut output, "helper_registry.needs:").expect("write to string");
        for helper in &self.needs {
            writeln!(&mut output, "  - {}", helper.dump_name()).expect("write to string");
        }
        writeln!(&mut output, "helper_registry.builtin:").expect("write to string");
        for helper in &self.builtin_helpers {
            writeln!(&mut output, "  - {}", helper.name()).expect("write to string");
        }
        writeln!(&mut output, "helper_registry.exports:").expect("write to string");
        for helper in &self.exported_helpers {
            writeln!(
                &mut output,
                "  - {}",
                helper.export_name().expect("exported helper should have an export name")
            )
            .expect("write to string");
        }
        output
    }
}

#[cfg(test)]
impl<'db> CallableRegistryPlan<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "callable_registry.signatures:").expect("write to string");
        for callable in &self.signatures {
            writeln!(
                &mut output,
                "  - [c{}] ({}) -> {}",
                callable.id.0,
                callable.signature.params.iter().map(format_abi_ty).collect::<Vec<_>>().join(", "),
                format_abi_ty(&callable.signature.result)
            )
            .expect("write to string");
        }
        writeln!(&mut output, "callable_registry.trampolines:").expect("write to string");
        for trampoline in &self.invoke_trampolines {
            writeln!(
                &mut output,
                "  - s{} [f{}] {} [{}]",
                trampoline.signature_id.0,
                trampoline.function_id.0,
                format_instance_key(db, &trampoline.instance),
                trampoline.metadata_index
            )
            .expect("write to string");
        }
        output
    }
}

#[cfg(test)]
impl<'db> SectionPlan<'db> {
    pub(crate) fn dump(&self, db: &dyn salsa::Database) -> String {
        let mut output = String::new();
        writeln!(&mut output, "sections.runtime_imports:").expect("write to string");
        for runtime in &self.runtime_imports {
            writeln!(
                &mut output,
                "  - {}::{} [t{}] [f{}]",
                runtime.import_module(),
                runtime.import_name(),
                self.runtime_type_indices[runtime],
                self.runtime_function_indices[runtime]
            )
            .expect("write to string");
        }
        writeln!(&mut output, "sections.raw_imports:").expect("write to string");
        for instance in &self.raw_imports {
            writeln!(
                &mut output,
                "  - {} [t{}] [f{}]",
                format_instance_key(db, instance),
                self.external_type_indices[instance],
                self.raw_import_function_indices[instance]
            )
            .expect("write to string");
        }
        writeln!(&mut output, "sections.callables:").expect("write to string");
        let mut callables = self.callable_type_indices.iter().collect::<Vec<_>>();
        callables.sort_by_key(|(_, index)| **index);
        for (signature, index) in callables {
            writeln!(
                &mut output,
                "  - [t{}] ({}) -> {}",
                index,
                signature.params.iter().map(format_abi_ty).collect::<Vec<_>>().join(", "),
                format_abi_ty(&signature.result)
            )
            .expect("write to string");
        }
        writeln!(&mut output, "sections.functions:").expect("write to string");
        for instance in &self.direct_functions {
            writeln!(
                &mut output,
                "  - direct {} [t{}] [f{}] wrapper=[f{}]",
                format_instance_key(db, instance),
                self.direct_type_indices[instance],
                self.direct_function_indices[instance],
                self.wrapper_function_indices[instance]
            )
            .expect("write to string");
        }
        for closure in &self.closure_functions {
            writeln!(
                &mut output,
                "  - closure {} [f{}]",
                format_closure_key(db, closure),
                self.closure_function_indices[closure]
            )
            .expect("write to string");
        }
        writeln!(&mut output, "sections.exports:").expect("write to string");
        for export in &self.exports {
            match export.target {
                SectionExportTarget::Func(index) => {
                    writeln!(&mut output, "  - {} => func[{}]", export.name, index)
                        .expect("write to string");
                }
                SectionExportTarget::Memory(index) => {
                    writeln!(&mut output, "  - {} => memory[{}]", export.name, index)
                        .expect("write to string");
                }
            }
        }
        writeln!(&mut output, "sections.table_slots:").expect("write to string");
        let mut slots = self.table_slots.iter().collect::<Vec<_>>();
        slots.sort_by_key(|(_, slot)| **slot);
        for (target, slot) in slots {
            match target {
                FunctionValueTarget::Function(instance) => {
                    writeln!(
                        &mut output,
                        "  - slot[{}] function {} -> f{}",
                        slot,
                        format_instance_key(db, instance),
                        self.wrapper_function_indices[instance]
                    )
                    .expect("write to string");
                }
                FunctionValueTarget::Closure(closure) => {
                    writeln!(
                        &mut output,
                        "  - slot[{}] closure {} -> f{}",
                        slot,
                        format_closure_key(db, closure),
                        self.closure_function_indices[closure]
                    )
                    .expect("write to string");
                }
            }
        }
        output
    }
}

#[cfg(test)]
fn format_instance_key(db: &dyn salsa::Database, instance: &InstanceKey<'_>) -> String {
    let name = instance.location.source(db).name().map_or("<anonymous>", |name| name.as_str());
    if instance.type_args.is_empty() {
        name.to_owned()
    } else {
        let type_args = instance
            .type_args
            .iter()
            .map(|ty| ty.display(db).to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("{name}[{type_args}]")
    }
}

#[cfg(test)]
fn format_closure_key(db: &dyn salsa::Database, closure: &ClosureInstanceKey<'_>) -> String {
    let owner = format_instance_key(db, &closure.owner_instance());
    format!("{owner}::closure#{:?}", closure.closure)
}

#[cfg(test)]
fn format_abi_ty(abi: &AbiTy) -> String {
    match abi {
        AbiTy::Scalar(BackendTy::Int) => "i32".to_owned(),
        AbiTy::Scalar(BackendTy::I64) => "i64".to_owned(),
        AbiTy::Scalar(BackendTy::Bool) => "bool".to_owned(),
        AbiTy::Scalar(BackendTy::Float) => "f64".to_owned(),
        AbiTy::Scalar(BackendTy::Char) => "char".to_owned(),
        AbiTy::Scalar(BackendTy::Ref(kind)) => format!("ref({kind:?})"),
        AbiTy::Scalar(BackendTy::Unit) => "unit".to_owned(),
        AbiTy::Aggregate(layout) => {
            format!("aggregate(size={}, align={})", layout.size, layout.align)
        }
    }
}

#[cfg(test)]
fn format_boundary_slot(db: &dyn salsa::Database, slot: &BoundarySlot<'_>) -> String {
    format!(
        "{} | {:?} | {}",
        slot.semantic_ty.display(db),
        slot.transport.transport_class,
        format_abi_ty(&slot.runtime_abi)
    )
}

#[cfg(test)]
fn format_transport_op(db: &dyn salsa::Database, op: &TransportOp<'_>) -> String {
    match op {
        TransportOp::ReadLane { lane, ty } => {
            format!("read lane {lane} as {}", ty.display(db))
        }
        TransportOp::NormalizeBool { lane } => {
            format!("normalize bool lane {lane}")
        }
        TransportOp::DecodeCanonical { lane, ty } => {
            format!("decode canonical lane {lane} as {}", ty.display(db))
        }
        TransportOp::EncodeCanonical { lane, ty } => {
            format!("encode canonical {} into lane {lane}", ty.display(db))
        }
        TransportOp::HandleToFunction { lane, ty } => {
            format!("handle lane {lane} -> {}", ty.display(db))
        }
        TransportOp::FunctionToHandle { lane, ty } => {
            format!("{} -> handle lane {lane}", ty.display(db))
        }
        TransportOp::RetainNestedHandles { ty } => {
            format!("retain nested handles in {}", ty.display(db))
        }
        TransportOp::ReleaseCanonicalTemp { lane } => {
            format!("release canonical temp lane {lane}")
        }
    }
}

#[cfg(test)]
fn format_internal_results(results: &[AbiTy]) -> String {
    match results {
        [] => "()".to_owned(),
        [result] => format_abi_ty(result),
        many => format!("({})", many.iter().map(format_abi_ty).collect::<Vec<_>>().join(", ")),
    }
}

#[cfg(test)]
fn format_reachability_node(
    db: &dyn salsa::Database,
    roots: &[ReachabilityRoot<'_>],
    node: &ReachabilityNode<'_>,
) -> String {
    match node {
        ReachabilityNode::Root(index) => roots.get(*index).map_or_else(
            || format!("root[{index}] <missing>"),
            |root| format!("root[{index}] {}", root.logical_name),
        ),
        ReachabilityNode::Function(instance) => {
            format!("function {}", format_instance_key(db, instance))
        }
        ReachabilityNode::Closure(closure) => {
            format!("closure {}", format_closure_key(db, closure))
        }
    }
}

#[cfg(test)]
fn format_reachability_edge_kind(kind: ReachabilityEdgeKind) -> &'static str {
    match kind {
        ReachabilityEdgeKind::Root => "root",
        ReachabilityEdgeKind::DirectCall => "direct_call",
        ReachabilityEdgeKind::ClosureLiteral => "closure_literal",
        ReachabilityEdgeKind::FunctionValue => "function_value",
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write as _;

    use expect_test::{Expect, expect};
    use mitki_comptime_wasm::compile_file_to_wasm;
    use mitki_db::RootDatabase;
    use mitki_inputs::File;
    use mitki_wasm_runtime::{WasmExternAbiKind, describe_module_abi};

    use super::*;

    fn new_compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let db = Box::leak(Box::new(RootDatabase::default()));
        let file = File::new(db, "module_plan_fixture.mitki".into(), fixture.to_owned());
        let diagnostics = mitki_analysis::check_file(db, file);
        assert!(
            diagnostics.is_empty(),
            "unexpected diagnostics: {:?}",
            diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        let runtime_diagnostics = mitki_analysis::check_runtime_file(db, file);
        assert!(
            runtime_diagnostics.is_empty(),
            "unexpected runtime diagnostics: {:?}",
            runtime_diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        Backend::new_file_with_options(
            db,
            file,
            crate::CompileOptions,
            Arc::new(crate::NoopComptimeEvaluator),
        )
    }

    fn compiler_for_fixture(fixture: &str) -> Backend<'_> {
        let mut backend = new_compiler_for_fixture(fixture);
        backend.collect_reachable_program();
        assert!(
            backend.diagnostics.is_empty(),
            "unexpected Backend diagnostics: {:?}",
            backend.diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        backend
    }

    fn assert_module_plan_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.dump(backend.db));
    }

    fn assert_reachability_graph_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let reachability = backend.build_reachability_graph();
        expected.assert_eq(&reachability.dump(backend.db));
    }

    fn assert_reachability_graph_details_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let reachability = backend.build_reachability_graph();
        expected.assert_eq(&reachability.dump_details(backend.db));
    }

    fn assert_emission_obligations_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let obligations = backend.build_emission_obligations();
        expected.assert_eq(&obligations.dump(backend.db));
    }

    fn assert_emission_obligations_details_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let obligations = backend.build_emission_obligations();
        expected.assert_eq(&obligations.dump_details(backend.db));
    }

    fn assert_function_instance_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        let mut output = String::new();
        writeln!(&mut output, "function_instances:").expect("write to string");
        for function in &plan.function_instances {
            writeln!(
                &mut output,
                "  - [f{}] {} => {}",
                function.id.0,
                function.logical_name,
                format_instance_key(backend.db, &function.instance)
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    internal: ({}) -> {}",
                function
                    .internal_signature
                    .params
                    .iter()
                    .map(format_abi_ty)
                    .collect::<Vec<_>>()
                    .join(", "),
                format_internal_results(&function.internal_signature.results)
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    metadata_index: {}",
                function.metadata_index.map_or_else(|| "-".to_owned(), |index| index.to_string())
            )
            .expect("write to string");
            writeln!(
                &mut output,
                "    boundary: {}",
                if function.boundary_signature.is_some() { "present" } else { "none" }
            )
            .expect("write to string");
        }
        expected.assert_eq(&output);
    }

    fn assert_helper_needs_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        let mut output = String::new();
        writeln!(&mut output, "helpers:").expect("write to string");
        for helper in &plan.helpers.needs {
            writeln!(&mut output, "  - {}", helper.dump_name()).expect("write to string");
        }
        expected.assert_eq(&output);
    }

    fn assert_helper_registry_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.helpers.dump());
    }

    fn assert_callable_registry_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.callables.dump(backend.db));
    }

    fn assert_section_plan_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.sections.dump(backend.db));
    }

    fn assert_boundary_alias_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.boundary.dump_aliases(backend.db));
    }

    fn assert_boundary_metadata_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.boundary.dump_metadata(backend.db));
    }

    fn assert_boundary_wrapper_ops_dump(fixture: &str, expected: &Expect) {
        let backend = compiler_for_fixture(fixture);
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        expected.assert_eq(&plan.boundary.dump_wrapper_ops(backend.db));
    }

    fn assert_abi_summary(fixture: &str, expected: &Expect) {
        let db = RootDatabase::default();
        let file = File::new(&db, "module_plan_abi.mitki".into(), fixture.to_owned());
        let bytes = compile_file_to_wasm(&db, file).unwrap_or_else(|diagnostics| {
            panic!(
                "fixture should compile: {:?}",
                diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
            )
        });
        let abi = describe_module_abi(&bytes).expect("ABI should decode");
        let mut summary = String::new();
        writeln!(&mut summary, "imports:").expect("write to string");
        for import in &abi.imports {
            writeln!(
                &mut summary,
                "  - {}::{} {}",
                import.module,
                import.name,
                format_extern_kind(&import.kind)
            )
            .expect("write to string");
        }
        writeln!(&mut summary, "exports:").expect("write to string");
        for export in &abi.exports {
            writeln!(&mut summary, "  - {} {}", export.name, format_extern_kind(&export.kind))
                .expect("write to string");
        }
        if let Some(metadata) = abi.metadata.as_ref() {
            writeln!(&mut summary, "metadata.instances:").expect("write to string");
            for instance in &metadata.function_instances {
                let name = metadata
                    .nominal_symbols
                    .get(instance.logical_symbol.0 as usize)
                    .and_then(|&string_id| metadata.strings.get(string_id.0 as usize))
                    .cloned()
                    .unwrap_or_else(|| "<missing>".to_owned());
                let wasm_name = instance
                    .wasm_field_name
                    .and_then(|id| metadata.strings.get(id.0 as usize))
                    .map_or("-", String::as_str);
                writeln!(&mut summary, "  - {:?} {} -> {}", instance.linkage, name, wasm_name)
                    .expect("write to string");
            }
        }
        expected.assert_eq(&summary);
    }

    fn assert_normalized_wat(fixture: &str, expected: &Expect) {
        let db = RootDatabase::default();
        let file = File::new(&db, "module_plan_wat.mitki".into(), fixture.to_owned());
        let bytes = compile_file_to_wasm(&db, file).unwrap_or_else(|diagnostics| {
            panic!(
                "fixture should compile: {:?}",
                diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
            )
        });
        let wat = wasmprinter::print_bytes(&bytes).expect("WAT should print");
        let normalized = wat
            .lines()
            .map(str::trim)
            .filter(|line| {
                line.starts_with("(import")
                    || line.starts_with("(memory")
                    || line.starts_with("(export")
            })
            .collect::<Vec<_>>()
            .join("\n");
        let normalized = format!("{}\n", normalized.trim_end());
        expected.assert_eq(&normalized);
    }

    fn format_extern_kind(kind: &WasmExternAbiKind) -> String {
        match kind {
            WasmExternAbiKind::Function(function) => {
                format!("fn({}) -> ({})", function.params.join(", "), function.results.join(", "))
            }
            WasmExternAbiKind::Table => "table".to_owned(),
            WasmExternAbiKind::Memory => "memory".to_owned(),
            WasmExternAbiKind::Global => "global".to_owned(),
            WasmExternAbiKind::Tag => "tag".to_owned(),
        }
    }

    #[test]
    fn module_plan_dump_tracks_roots_boundary_instances_and_obligations() {
        assert_module_plan_dump(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
module.mode: runtime
module.target: wasm-core-v2/m32
module.target_decisions: guest_word=i32 result_lowering=spill_to_locals callable_representation=handle_and_table boundary_transport=abi_v2_wrappers reference_representation=linear_memory_managed failure_lowering=trap_only
reachability.mode: runtime
reachability.target: wasm-core-v2/m32
reachability.roots:
  - id => id[int]
  - id => id[bool]
reachability.functions:
  - id[int]
  - id[bool]
  - round_trip[int]
  - round_trip[bool]
reachability.closures:
obligations.runtime_imports:
obligations.stage_intrinsics:
obligations.helpers:
obligations.arrays:
obligations.nominals:
function_instances:
  - [f0] id => id[int]
    internal: (i32) -> i32
    metadata_index: 2
    boundary: present
  - [f1] id => id[bool]
    internal: (bool) -> bool
    metadata_index: 3
    boundary: present
  - [f2] round_trip => round_trip[int]
    internal: (i32) -> i32
    metadata_index: 0
    boundary: present
  - [f3] round_trip => round_trip[bool]
    internal: (bool) -> bool
    metadata_index: 1
    boundary: present
imports:
  - [f2] [0] env::mitki:typed/2/f$0 (round_trip)
  - [f3] [1] env::mitki:typed/2/f$1 (round_trip)
exports:
  - [f0] [2] mitki:typed/2/f$2 (id)
  - [f1] [3] mitki:typed/2/f$3 (id)
helpers:
memories:
  - memory#0 export=memory
tables: 0
globals: 0
data_segments: 0
abi_preview.instances:
  - [0] WasmImport mitki:typed/2/f$0 sig=s0
  - [1] WasmImport mitki:typed/2/f$1 sig=s1
  - [2] WasmExport mitki:typed/2/f$2 sig=s2
  - [3] WasmExport mitki:typed/2/f$3 sig=s3
names.logical_functions:
  - [f0] id
  - [f1] id
  - [f2] round_trip
  - [f3] round_trip
names.typed_exports:
  - mitki:typed/2/f$2 => f0
  - mitki:typed/2/f$3 => f1
names.helper_exports:
boundary.entries:
  - [0] [f2] import round_trip round_trip[int] -> mitki:typed/2/f$0
    module: env
    generic_origin: round_trip
    internal: (i32) -> i32
    boundary.params:
      - int | Immediate | i32
    boundary.results:
      - int | Immediate | i32
  - [1] [f3] import round_trip round_trip[bool] -> mitki:typed/2/f$1
    module: env
    generic_origin: round_trip
    internal: (bool) -> bool
    boundary.params:
      - bool | Immediate | bool
    boundary.results:
      - bool | Immediate | bool
  - [2] [f0] export id id[int] -> mitki:typed/2/f$2
    module: -
    generic_origin: id
    internal: (i32) -> i32
    boundary.params:
      - int | Immediate | i32
    boundary.results:
      - int | Immediate | i32
  - [3] [f1] export id id[bool] -> mitki:typed/2/f$3
    module: -
    generic_origin: id
    internal: (bool) -> bool
    boundary.params:
      - bool | Immediate | bool
    boundary.results:
      - bool | Immediate | bool
boundary.export_aliases:
  - mitki:typed/2/f$2 => id[int]
  - mitki:typed/2/f$3 => id[bool]
"#]],
        );
    }

    #[test]
    fn module_plan_target_decisions_are_stable_across_repeated_planning() {
        let backend = compiler_for_fixture(
            r#"
export fun main(): int {
    42
}
"#,
        );
        let first = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .target_decisions
            .dump();
        let second = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .target_decisions
            .dump();
        assert_eq!(first, second);
        assert_eq!(
            second,
            "guest_word=i32 result_lowering=spill_to_locals \
             callable_representation=handle_and_table boundary_transport=abi_v2_wrappers \
             reference_representation=linear_memory_managed failure_lowering=trap_only"
        );
    }

    #[test]
    fn reachability_graph_dump_tracks_roots_and_reachable_instances() {
        assert_reachability_graph_dump(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
reachability.mode: runtime
reachability.target: wasm-core-v2/m32
reachability.roots:
  - id => id[int]
  - id => id[bool]
reachability.functions:
  - id[int]
  - id[bool]
  - round_trip[int]
  - round_trip[bool]
reachability.closures:
"#]],
        );
    }

    #[test]
    fn reachability_graph_details_dump_tracks_edges_imports_exports_types_and_callables() {
        assert_reachability_graph_details_dump(
            r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
            &expect![[r#"
reachability.mode: runtime
reachability.target: wasm-core-v2/m32
reachability.roots:
  - id => id
reachability.functions:
  - id
  - round_trip
reachability.closures:
reachability.instances:
  - function id
  - function round_trip
reachability.imports:
  - round_trip
reachability.exports:
  - id
reachability.types:
  - fun(int) -> int
  - int
reachability.callables:
  - (aggregate(size=8, align=4)) -> aggregate(size=8, align=4)
reachability.edges:
  - root[0] id -> function id (root)
  - function id -> function round_trip (direct_call)
"#]],
        );
    }

    #[test]
    fn abi_summary_dump_is_stable_for_typed_exports_and_helpers() {
        assert_abi_summary(
            r#"
export fun words(): [str] {
    ["a", "b"]
}
"#,
            &expect![[r#"
imports:
  - mitki::alloc fn(i32, i32) -> (i32)
  - mitki::dealloc fn(i32, i32, i32) -> ()
exports:
  - mitki:typed/2/f$0 fn() -> (i32)
  - mitki:abi/2/alloc fn(i32, i32) -> (i32)
  - mitki:abi/2/blob_release fn(i32) -> ()
  - memory memory
metadata.instances:
  - WasmExport words -> mitki:typed/2/f$0
"#]],
        );
    }

    #[test]
    fn normalized_wat_fragments_track_imports_and_exports() {
        assert_normalized_wat(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];

export fun main(): int {
    round_trip(41)
}
"#,
            &expect![[r#"
(import "env" "mitki:typed/2/f$0" (func (;0;) (type 7)))
(memory (;0;) 1)
(export "mitki:typed/2/f$1" (func 9))
(export "memory" (memory 0))
"#]],
        );
    }

    #[test]
    fn repeated_reachability_collection_rebuilds_graph_state() {
        let mut backend = new_compiler_for_fixture(
            r#"
fun id[T](value: T): T {
    value
}

export instance id[int];
export instance id[bool];
"#,
        );

        backend.collect_reachable_program();
        let first = backend.build_reachability_graph().dump(backend.db);

        backend.collect_reachable_program();
        assert!(
            backend.diagnostics.is_empty(),
            "unexpected Backend diagnostics: {:?}",
            backend.diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        let second = backend.build_reachability_graph().dump(backend.db);

        assert_eq!(first, second);
    }

    #[test]
    fn repeated_obligation_collection_rebuilds_obligation_details_stably() {
        let mut backend = new_compiler_for_fixture(
            r#"
export fun words(): [str] {
    ["a", "b"]
}
"#,
        );

        backend.collect_reachable_program();
        let first = backend.build_emission_obligations().dump_details(backend.db);

        backend.collect_reachable_program();
        assert!(
            backend.diagnostics.is_empty(),
            "unexpected Backend diagnostics: {:?}",
            backend.diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>()
        );
        let second = backend.build_emission_obligations().dump_details(backend.db);

        assert_eq!(first, second);
    }

    #[test]
    fn function_instance_ids_are_stable_across_repeated_collection() {
        let mut backend = new_compiler_for_fixture(
            r#"
fun id[T](value: T): T {
    value
}

export instance id[int];
export instance id[bool];
"#,
        );

        backend.collect_reachable_program();
        let first = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .function_instances
            .into_iter()
            .map(|function| (function.id, format_instance_key(backend.db, &function.instance)))
            .collect::<Vec<_>>();

        backend.collect_reachable_program();
        let second = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .function_instances
            .into_iter()
            .map(|function| (function.id, format_instance_key(backend.db, &function.instance)))
            .collect::<Vec<_>>();

        assert_eq!(first, second);
        assert_eq!(
            second,
            vec![
                (FunctionInstanceId(0), "id[int]".to_owned()),
                (FunctionInstanceId(1), "id[bool]".to_owned()),
            ]
        );
    }

    #[test]
    fn function_instance_dump_tracks_passive_ids_and_metadata() {
        assert_function_instance_dump(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
function_instances:
  - [f0] id => id[int]
    internal: (i32) -> i32
    metadata_index: 2
    boundary: present
  - [f1] id => id[bool]
    internal: (bool) -> bool
    metadata_index: 3
    boundary: present
  - [f2] round_trip => round_trip[int]
    internal: (i32) -> i32
    metadata_index: 0
    boundary: present
  - [f3] round_trip => round_trip[bool]
    internal: (bool) -> bool
    metadata_index: 1
    boundary: present
"#]],
        );
    }

    #[test]
    fn emission_obligations_dump_tracks_runtime_helpers_arrays_and_nominals() {
        assert_emission_obligations_dump(
            r#"
struct Boxed {
    items: [str],
}

export fun build(): Boxed {
    val same = "a" == "b";
    Boxed { items: ["a", "b"] }
}
"#,
            &expect![[r#"
obligations.runtime_imports:
  - mitki::alloc
  - mitki::dealloc
obligations.stage_intrinsics:
obligations.helpers:
  - arc_release
  - arc_retain
  - memory_eq
  - string_eq
obligations.arrays:
  - [str]
obligations.nominals:
  - Boxed
"#]],
        );
    }

    #[test]
    fn emission_obligations_details_dump_tracks_canonical_boundary_support() {
        assert_emission_obligations_details_dump(
            r#"
export fun words(): [str] {
    ["a", "b"]
}
"#,
            &expect![[r#"
obligations.runtime_imports:
  - mitki::alloc
  - mitki::dealloc
obligations.stage_intrinsics:
obligations.helpers:
  - arc_release
  - arc_retain
obligations.arrays:
  - [str]
obligations.nominals:
obligations.runtime_import_needs:
  - mitki::alloc
  - mitki::dealloc
obligations.callable_adapters:
obligations.boundary_wrappers:
  - export words
obligations.canonical_support:
  - type [str]
  - blob_helpers
obligations.layout_needs:
  - array [str]
"#]],
        );
    }

    #[test]
    fn emission_obligations_details_dump_tracks_callable_boundary_support() {
        assert_emission_obligations_details_dump(
            r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
            &expect![[r#"
obligations.runtime_imports:
  - mitki::alloc
  - mitki::dealloc
obligations.stage_intrinsics:
obligations.helpers:
obligations.arrays:
obligations.nominals:
obligations.runtime_import_needs:
  - mitki::alloc
  - mitki::dealloc
obligations.callable_adapters:
  - boundary_invoke id
  - boundary_invoke round_trip
obligations.boundary_wrappers:
  - import round_trip
  - export id
obligations.canonical_support:
  - handle_helpers
obligations.layout_needs:
"#]],
        );
    }

    #[test]
    fn helper_needs_dump_tracks_semantic_and_abi_helpers() {
        assert_helper_needs_dump(
            r#"
struct Boxed {
    items: [str],
}

export fun build(): Boxed {
    val same = "a" == "b";
    Boxed { items: ["a", "b"] }
}
"#,
            &expect![[r#"
helpers:
  - memory_eq
  - string_eq
  - arc_retain
  - arc_release
  - mitki:abi/2/alloc
  - mitki:abi/2/blob_release
  - nominal_destroy(11265)
  - nominal_eq(11265)
  - array_destroy(11280)
  - array_eq(11280)
"#]],
        );
    }

    #[test]
    fn helper_needs_dump_tracks_handle_helpers_and_invoke_exports() {
        assert_helper_needs_dump(
            r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
            &expect![[r#"
helpers:
  - mitki:abi/2/handle_retain
  - mitki:abi/2/handle_release
  - mitki:abi/2/invoke$0
  - mitki:abi/2/invoke$1
"#]],
        );
    }

    #[test]
    fn helper_registry_dump_tracks_builtin_and_exported_helpers() {
        assert_helper_registry_dump(
            r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
            &expect![[r#"
helper_registry.needs:
  - mitki:abi/2/handle_retain
  - mitki:abi/2/handle_release
  - mitki:abi/2/invoke$0
  - mitki:abi/2/invoke$1
helper_registry.builtin:
  - memory_eq
  - string_eq
  - arc_retain
  - arc_release
helper_registry.exports:
  - mitki:abi/2/handle_retain
  - mitki:abi/2/handle_release
  - mitki:abi/2/invoke$0
  - mitki:abi/2/invoke$1
"#]],
        );
    }

    #[test]
    fn callable_registry_dump_deduplicates_repeated_signatures() {
        assert_callable_registry_dump(
            r#"
fun first(value: int): int {
    value
}

fun second(value: int): int {
    first(value)
}

export fun third(value: int): int {
    second(value)
}
"#,
            &expect![[r#"
callable_registry.signatures:
  - [c0] (i32) -> i32
callable_registry.trampolines:
"#]],
        );
    }

    #[test]
    fn section_plan_dump_tracks_indices_and_exports() {
        assert_section_plan_dump(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
sections.runtime_imports:
sections.raw_imports:
  - round_trip[int] [t10] [f0]
  - round_trip[bool] [t11] [f1]
sections.callables:
  - [t12] (i32) -> i32
  - [t13] (bool) -> bool
sections.functions:
  - direct id[int] [t4] [f6] wrapper=[f10]
  - direct id[bool] [t5] [f7] wrapper=[f11]
  - direct round_trip[int] [t6] [f8] wrapper=[f12]
  - direct round_trip[bool] [t7] [f9] wrapper=[f13]
sections.exports:
  - mitki:typed/2/f$2 => func[14]
  - mitki:typed/2/f$3 => func[15]
  - memory => memory[0]
sections.table_slots:
  - slot[0] function id[int] -> f10
  - slot[1] function id[bool] -> f11
  - slot[2] function round_trip[int] -> f12
  - slot[3] function round_trip[bool] -> f13
"#]],
        );
    }

    #[test]
    fn boundary_alias_dump_tracks_logical_to_physical_mapping() {
        assert_boundary_alias_dump(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
boundary.aliases:
  - [2] [f0] mitki:typed/2/f$2 => id[int] (id)
  - [3] [f1] mitki:typed/2/f$3 => id[bool] (id)
"#]],
        );
    }

    #[test]
    fn boundary_metadata_dump_tracks_metadata_facing_instances() {
        assert_boundary_metadata_dump(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
            &expect![[r#"
boundary.metadata:
  - [0] WasmImport round_trip env -> mitki:typed/2/f$0
    params: int
    result: int
  - [1] WasmImport round_trip env -> mitki:typed/2/f$1
    params: bool
    result: bool
  - [2] WasmExport id - -> mitki:typed/2/f$2
    params: int
    result: int
  - [3] WasmExport id - -> mitki:typed/2/f$3
    params: bool
    result: bool
"#]],
        );
    }

    #[test]
    fn boundary_wrapper_ops_dump_tracks_bool_normalization() {
        assert_boundary_wrapper_ops_dump(
            r#"
export fun invert(value: bool): bool {
    value
}
"#,
            &expect![[r#"
boundary.wrapper_ops:
  - export invert (invert)
    params:
      - read lane 0 as bool
      - normalize bool lane 0
    results:
      - read lane 0 as bool
      - normalize bool lane 0
"#]],
        );
    }

    #[test]
    fn boundary_wrapper_ops_dump_tracks_canonical_blob_transport() {
        assert_boundary_wrapper_ops_dump(
            r#"
export fun words(): [str] {
    ["a", "b"]
}
"#,
            &expect![[r#"
boundary.wrapper_ops:
  - export words (words)
    params:
    results:
      - encode canonical [str] into lane 0
      - retain nested handles in [str]
      - release canonical temp lane 0
"#]],
        );
    }

    #[test]
    fn boundary_wrapper_ops_dump_tracks_function_handle_transport() {
        assert_boundary_wrapper_ops_dump(
            r#"
import "env" fun round_trip(f: fun(int) -> int): fun(int) -> int;

export fun id(f: fun(int) -> int): fun(int) -> int {
    round_trip(f)
}
"#,
            &expect![[r#"
boundary.wrapper_ops:
  - import round_trip (round_trip)
    params:
      - fun(int) -> int -> handle lane 0
    results:
      - handle lane 0 -> fun(int) -> int
  - export id (id)
    params:
      - handle lane 0 -> fun(int) -> int
    results:
      - fun(int) -> int -> handle lane 0
"#]],
        );
    }

    #[test]
    fn boundary_signatures_compare_by_embedded_transport_plan() {
        let backend = compiler_for_fixture(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
        );
        let plan = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        assert_eq!(plan.boundary.instances[0].signature, plan.boundary.instances[2].signature);
        assert_ne!(plan.boundary.instances[0].signature, plan.boundary.instances[1].signature);
    }

    #[test]
    fn boundary_planner_is_stable_across_repeated_planning() {
        let mut backend = new_compiler_for_fixture(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
        );

        backend.collect_reachable_program();
        let first = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
        backend.collect_reachable_program();
        let second = backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });

        assert_eq!(first.boundary.metadata.functions, second.boundary.metadata.functions);
        assert_eq!(first.boundary.import_indices, second.boundary.import_indices);
        assert_eq!(first.boundary.export_indices, second.boundary.export_indices);
        assert_eq!(first.boundary.aliases, second.boundary.aliases);
    }

    #[test]
    fn boundary_alias_order_is_stable_across_repeated_collection() {
        let mut backend = new_compiler_for_fixture(
            r#"
fun id[T](value: T): T {
    value
}

export instance id[int];
export instance id[bool];
"#,
        );

        backend.collect_reachable_program();
        let first = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .boundary
            .dump_aliases(backend.db);

        backend.collect_reachable_program();
        let second = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .boundary
            .dump_aliases(backend.db);

        assert_eq!(first, second);
    }

    #[test]
    fn module_plan_builds_for_else_if_value_branches() {
        let backend = compiler_for_fixture(
            r#"
fun choose(first: bool, second: bool, start: int): int {
    if first {
        start + 1
    } else if second {
        start + 2
    } else {
        start
    }
}

export fun main(): int {
    choose(false, true, 10)
}
"#,
        );

        backend.build_module_plan().unwrap_or_else(|diagnostic| {
            panic!("module plan should build: {}", diagnostic.message())
        });
    }

    #[test]
    fn section_plan_order_is_stable_across_repeated_collection() {
        let mut backend = new_compiler_for_fixture(
            r#"
import "env" fun round_trip[T](value: T): T;
import instance round_trip[int];
import instance round_trip[bool];

fun id[T](value: T): T {
    round_trip(value)
}

export instance id[int];
export instance id[bool];
"#,
        );

        backend.collect_reachable_program();
        let first = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .sections
            .dump(backend.db);

        backend.collect_reachable_program();
        let second = backend
            .build_module_plan()
            .unwrap_or_else(|diagnostic| {
                panic!("module plan should build: {}", diagnostic.message())
            })
            .sections
            .dump(backend.db);

        assert_eq!(first, second);
    }
}

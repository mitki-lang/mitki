use mitki_abi::{FunctionSignature as AbiV2FunctionSignature, SemanticTypeGraph, TransportClass};
use mitki_abi_lower::boundary_transport_class;
use mitki_errors::{Diagnostic, TextRange};
use mitki_resolve::RuntimeFunction;
use wasm_encoder::{ConstExpr, RefType, ValType};

use super::boundary::semantic_type_uses_recursive_group;
use super::{BoundaryTransportPlan, StageIntrinsic};
#[cfg(test)]
use crate::abi::BackendTy;
use crate::abi::{AbiTy, BackendTy as RuntimeBackendTy, FunctionSignature, backend_ty_value_type};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum PointerWidth {
    M32,
    #[allow(dead_code)]
    M64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum GuestWord {
    I32,
    I64,
}

impl GuestWord {
    pub(crate) const fn val_type(self) -> ValType {
        match self {
            Self::I32 => ValType::I32,
            Self::I64 => ValType::I64,
        }
    }

    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::I32 => "i32",
            Self::I64 => "i64",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum ResultLoweringMode {
    SingleValueOnly,
    MultiValue,
    SpillToLocals,
}

impl ResultLoweringMode {
    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::SingleValueOnly => "single_value_only",
            Self::MultiValue => "multi_value",
            Self::SpillToLocals => "spill_to_locals",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CallableRepresentation {
    HandleAndTable,
    TypedFuncRef,
}

impl CallableRepresentation {
    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::HandleAndTable => "handle_and_table",
            Self::TypedFuncRef => "typed_funcref",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum ReferenceRep {
    LinearMemoryManaged,
    GcRef,
}

impl ReferenceRep {
    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::LinearMemoryManaged => "linear_memory_managed",
            Self::GcRef => "gc_ref",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum FailureLowering {
    TrapOnly,
    #[allow(dead_code)]
    ExplicitErrorCarrier,
    Exceptions,
}

impl FailureLowering {
    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::TrapOnly => "trap_only",
            Self::ExplicitErrorCarrier => "explicit_error_carrier",
            Self::Exceptions => "exceptions",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum BoundaryTransportModel {
    AbiV2Wrappers,
}

impl BoundaryTransportModel {
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::AbiV2Wrappers => "abi_v2_wrappers",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PhysicalWasmSignature {
    pub(crate) params: Vec<ValType>,
    pub(crate) results: Vec<ValType>,
}

impl PhysicalWasmSignature {
    pub(crate) fn new(params: Vec<ValType>, results: Vec<ValType>) -> Self {
        Self { params, results }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct SignatureStrategy {
    profile: TargetProfile,
}

impl SignatureStrategy {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) const fn guest_word(self) -> GuestWord {
        match self.profile.pointer_width() {
            PointerWidth::M32 => GuestWord::I32,
            PointerWidth::M64 => GuestWord::I64,
        }
    }

    pub(crate) const fn result_pointer_lane(self) -> ValType {
        self.guest_word().val_type()
    }

    pub(crate) fn direct_signature(self, signature: &FunctionSignature) -> PhysicalWasmSignature {
        let mut params = Vec::with_capacity(
            signature.params.len() + usize::from(signature.result.is_aggregate()),
        );
        if signature.result.is_aggregate() {
            params.push(self.result_pointer_lane());
        }
        params.extend(signature.params.iter().filter_map(|ty| self.param_lane(ty)));
        let results = Self::result_lanes(&signature.result);
        PhysicalWasmSignature::new(params, results)
    }

    fn param_lane(self, ty: &AbiTy) -> Option<ValType> {
        match ty {
            AbiTy::Scalar(ty) => backend_ty_value_type(*ty),
            AbiTy::Aggregate(_) => Some(self.result_pointer_lane()),
        }
    }

    fn result_lanes(ty: &AbiTy) -> Vec<ValType> {
        match ty {
            AbiTy::Scalar(result) => backend_ty_value_type(*result).into_iter().collect(),
            AbiTy::Aggregate(_) => Vec::new(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct TargetProfile {
    pointer_width: PointerWidth,
    multi_value: bool,
    reference_types: bool,
    typed_funcref: bool,
    wasm_gc: bool,
    memory64: bool,
    exceptions: bool,
}

impl TargetProfile {
    pub(crate) const fn wasm_core_v2_m32() -> Self {
        Self {
            pointer_width: PointerWidth::M32,
            multi_value: false,
            reference_types: true,
            typed_funcref: false,
            wasm_gc: false,
            memory64: false,
            exceptions: false,
        }
    }

    pub(crate) const fn pointer_width(self) -> PointerWidth {
        self.pointer_width
    }

    pub(crate) const fn supports_multi_value(self) -> bool {
        self.multi_value
    }

    pub(crate) const fn uses_reference_types(self) -> bool {
        self.reference_types
    }

    pub(crate) const fn supports_typed_funcref(self) -> bool {
        self.typed_funcref
    }

    pub(crate) const fn supports_wasm_gc(self) -> bool {
        self.wasm_gc
    }

    pub(crate) const fn uses_memory64(self) -> bool {
        self.memory64
    }

    pub(crate) const fn supports_exceptions(self) -> bool {
        self.exceptions
    }

    pub(crate) const fn canonical_name(self) -> &'static str {
        match self.pointer_width {
            PointerWidth::M32 => "wasm-core-v2/m32",
            PointerWidth::M64 => "wasm-core-v2/m64",
        }
    }

    #[cfg(test)]
    pub(crate) const fn with_pointer_width(mut self, pointer_width: PointerWidth) -> Self {
        self.pointer_width = pointer_width;
        self
    }

    #[cfg(test)]
    pub(crate) const fn with_memory64(mut self, memory64: bool) -> Self {
        self.memory64 = memory64;
        self
    }

    #[cfg(test)]
    pub(crate) const fn with_multi_value(mut self, multi_value: bool) -> Self {
        self.multi_value = multi_value;
        self
    }

    #[cfg(test)]
    pub(crate) const fn with_reference_types(mut self, reference_types: bool) -> Self {
        self.reference_types = reference_types;
        self
    }

    #[cfg(test)]
    pub(crate) const fn with_typed_funcref(mut self, typed_funcref: bool) -> Self {
        self.typed_funcref = typed_funcref;
        self
    }

    #[cfg(test)]
    pub(crate) const fn with_wasm_gc(mut self, wasm_gc: bool) -> Self {
        self.wasm_gc = wasm_gc;
        self
    }

    #[cfg(test)]
    pub(crate) const fn with_exceptions(mut self, exceptions: bool) -> Self {
        self.exceptions = exceptions;
        self
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) enum CompilationMode {
    Runtime,
    Stage,
}

impl CompilationMode {
    #[cfg(test)]
    pub(crate) const fn dump_name(self) -> &'static str {
        match self {
            Self::Runtime => "runtime",
            Self::Stage => "stage",
        }
    }

    pub(crate) const fn is_stage(self) -> bool {
        matches!(self, Self::Stage)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ImportProvider {
    profile: TargetProfile,
}

impl ImportProvider {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) fn runtime_import(self, runtime: RuntimeFunction) -> (&'static str, &'static str) {
        let _ = self.profile;
        (runtime.import_module(), runtime.import_name())
    }

    pub(crate) fn stage_intrinsic_import(
        self,
        intrinsic: StageIntrinsic,
    ) -> (&'static str, &'static str) {
        let _ = self.profile;
        (StageIntrinsic::module(), intrinsic.name())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct BoundaryTransportProfile {
    profile: TargetProfile,
}

impl BoundaryTransportProfile {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) const fn model(self) -> BoundaryTransportModel {
        let _ = self.profile;
        BoundaryTransportModel::AbiV2Wrappers
    }

    pub(crate) fn boundary_signature_context(self) -> String {
        format!(
            "the current `{}` backend does not support this Wasm boundary signature",
            self.profile.canonical_name()
        )
    }

    pub(crate) fn plan_or_message(
        self,
        db: &dyn salsa::Database,
        ty: mitki_hir::ty::Ty<'_>,
        context: &str,
    ) -> Result<BoundaryTransportPlan, String> {
        let _ = self.profile;
        crate::capability::supported_value_abi_or_message(db, ty, context)?;
        Ok(BoundaryTransportPlan { transport_class: boundary_transport_class(db, ty) })
    }

    pub(crate) fn requires_runtime_allocs(
        self,
        db: &dyn salsa::Database,
        ty: mitki_hir::ty::Ty<'_>,
        runtime_abi: &AbiTy,
    ) -> bool {
        let _ = self.profile;
        match boundary_transport_class(db, ty) {
            TransportClass::Immediate => {
                matches!(
                    ty.kind(db),
                    mitki_hir::ty::TyKind::Enum(enum_ty)
                        if mitki_lower::item::scope::enum_variants(db, *enum_ty)
                            .iter()
                            .all(|(_, fields)| fields.is_empty())
                ) && !matches!(
                    runtime_abi,
                    AbiTy::Scalar(
                        RuntimeBackendTy::Unit
                            | RuntimeBackendTy::Int
                            | RuntimeBackendTy::Bool
                            | RuntimeBackendTy::Float
                            | RuntimeBackendTy::Char
                    )
                )
            }
            TransportClass::CanonicalValue | TransportClass::CapabilityHandle => {
                !matches!(runtime_abi, AbiTy::Scalar(RuntimeBackendTy::Unit))
            }
        }
    }

    pub(crate) fn ensure_supported(
        self,
        graph: &SemanticTypeGraph,
        signature: &AbiV2FunctionSignature,
        range: TextRange,
    ) -> Result<(), Diagnostic> {
        for transport in signature.params.iter().chain(std::iter::once(&signature.result)) {
            if matches!(transport.transport_class, TransportClass::CanonicalValue)
                && semantic_type_uses_recursive_group(graph, transport.semantic_type)?
            {
                return Err(Diagnostic::error(
                    format!(
                        "the current `{}` backend does not support this boundary shape yet; \
                         recursive types are not supported yet",
                        self.profile.canonical_name()
                    ),
                    range,
                ));
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CallableLoweringStrategy {
    profile: TargetProfile,
}

impl CallableLoweringStrategy {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) const fn representation(self) -> CallableRepresentation {
        if self.profile.supports_typed_funcref() {
            CallableRepresentation::TypedFuncRef
        } else {
            CallableRepresentation::HandleAndTable
        }
    }

    pub(crate) const fn uses_table_slots(self) -> bool {
        matches!(self.representation(), CallableRepresentation::HandleAndTable)
    }

    pub(crate) const fn requires_reference_types(self) -> bool {
        self.uses_table_slots()
    }

    pub(crate) const fn table_element_type(self) -> RefType {
        let _ = self.profile;
        RefType::FUNCREF
    }

    pub(crate) const fn table64(self) -> bool {
        let _ = self;
        false
    }

    pub(crate) const fn environment_lane(
        self,
        memory_model: MemoryModelStrategy,
    ) -> Option<ValType> {
        match self.representation() {
            CallableRepresentation::HandleAndTable => Some(memory_model.word_type()),
            CallableRepresentation::TypedFuncRef => None,
        }
    }

    pub(crate) const fn table_index_lane(self, memory_model: MemoryModelStrategy) -> ValType {
        let _ = self.profile;
        memory_model.word_type()
    }

    pub(crate) const fn handle_lane(self, memory_model: MemoryModelStrategy) -> ValType {
        let _ = self.profile;
        memory_model.word_type()
    }

    pub(crate) fn callable_signature(
        self,
        signature_strategy: SignatureStrategy,
        memory_model: MemoryModelStrategy,
        signature: &FunctionSignature,
    ) -> PhysicalWasmSignature {
        let direct = signature_strategy.direct_signature(signature);
        let mut params = Vec::with_capacity(
            direct.params.len() + usize::from(self.environment_lane(memory_model).is_some()),
        );
        if let Some(env_lane) = self.environment_lane(memory_model) {
            params.push(env_lane);
        }
        params.extend(direct.params);
        PhysicalWasmSignature::new(params, direct.results)
    }

    pub(crate) fn boundary_invoke_signature(
        self,
        memory_model: MemoryModelStrategy,
        signature: PhysicalWasmSignature,
    ) -> PhysicalWasmSignature {
        match self.representation() {
            CallableRepresentation::HandleAndTable => {
                let mut params = Vec::with_capacity(signature.params.len() + 1);
                params.push(self.handle_lane(memory_model));
                params.extend(signature.params);
                PhysicalWasmSignature::new(params, signature.results)
            }
            CallableRepresentation::TypedFuncRef => signature,
        }
    }

    pub(crate) fn handle_retain_signature(
        self,
        memory_model: MemoryModelStrategy,
    ) -> PhysicalWasmSignature {
        let lane = self.handle_lane(memory_model);
        PhysicalWasmSignature::new(vec![lane], vec![lane])
    }

    pub(crate) fn handle_release_signature(
        self,
        memory_model: MemoryModelStrategy,
    ) -> PhysicalWasmSignature {
        PhysicalWasmSignature::new(vec![self.handle_lane(memory_model)], Vec::new())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct MemoryModelStrategy {
    profile: TargetProfile,
}

impl MemoryModelStrategy {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) const fn guest_word(self) -> GuestWord {
        match self.profile.pointer_width() {
            PointerWidth::M32 => GuestWord::I32,
            PointerWidth::M64 => GuestWord::I64,
        }
    }

    pub(crate) const fn word_type(self) -> ValType {
        self.guest_word().val_type()
    }

    #[cfg(test)]
    pub(crate) const fn pointer_backend_ty(self) -> BackendTy {
        match self.profile.pointer_width() {
            PointerWidth::M32 => BackendTy::Int,
            PointerWidth::M64 => BackendTy::I64,
        }
    }

    pub(crate) const fn memory64(self) -> bool {
        self.profile.uses_memory64()
    }

    pub(crate) fn const_expr_from_u32(self, value: u32) -> ConstExpr {
        match self.profile.pointer_width() {
            PointerWidth::M32 => ConstExpr::i32_const(value as i32),
            PointerWidth::M64 => ConstExpr::i64_const(i64::from(value)),
        }
    }

    pub(crate) const fn alloc_align_type(self) -> ValType {
        let _ = self.profile;
        ValType::I32
    }

    pub(crate) fn alloc_helper_signature(self) -> PhysicalWasmSignature {
        PhysicalWasmSignature::new(
            vec![self.word_type(), self.alloc_align_type()],
            vec![self.word_type()],
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ControlFlowStrategy {
    profile: TargetProfile,
}

impl ControlFlowStrategy {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) const fn failure_lowering(self) -> FailureLowering {
        if self.profile.supports_exceptions() {
            FailureLowering::Exceptions
        } else {
            FailureLowering::TrapOnly
        }
    }

    pub(crate) fn result_lowering_mode(self, abi: &AbiTy) -> ResultLoweringMode {
        match abi {
            AbiTy::Scalar(RuntimeBackendTy::Unit) | AbiTy::Aggregate(_) => {
                ResultLoweringMode::SingleValueOnly
            }
            AbiTy::Scalar(_) if self.profile.supports_multi_value() => {
                ResultLoweringMode::MultiValue
            }
            AbiTy::Scalar(_) => ResultLoweringMode::SpillToLocals,
        }
    }

    pub(crate) const fn default_result_lowering(self) -> ResultLoweringMode {
        if self.profile.supports_multi_value() {
            ResultLoweringMode::MultiValue
        } else {
            ResultLoweringMode::SpillToLocals
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct ReferenceRepresentationStrategy {
    profile: TargetProfile,
}

impl ReferenceRepresentationStrategy {
    pub(crate) const fn new(profile: TargetProfile) -> Self {
        Self { profile }
    }

    pub(crate) const fn representation(self) -> ReferenceRep {
        if self.profile.supports_wasm_gc() {
            ReferenceRep::GcRef
        } else {
            ReferenceRep::LinearMemoryManaged
        }
    }

    #[allow(dead_code)]
    pub(crate) const fn uses_linear_memory_objects(self) -> bool {
        matches!(self.representation(), ReferenceRep::LinearMemoryManaged)
    }

    #[allow(dead_code)]
    pub(crate) const fn requires_runtime_retain_release(self) -> bool {
        self.uses_linear_memory_objects()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct CapabilityMatrix {
    profile: TargetProfile,
    guest_word: GuestWord,
    result_lowering: ResultLoweringMode,
    callable_representation: CallableRepresentation,
    boundary_transport: BoundaryTransportModel,
    reference_representation: ReferenceRep,
    failure_lowering: FailureLowering,
}

impl CapabilityMatrix {
    pub(crate) const fn new(
        profile: TargetProfile,
        signature: SignatureStrategy,
        boundary_transport: BoundaryTransportProfile,
        callable: CallableLoweringStrategy,
        control_flow: ControlFlowStrategy,
        reference_representation: ReferenceRepresentationStrategy,
    ) -> Self {
        Self {
            profile,
            guest_word: signature.guest_word(),
            result_lowering: control_flow.default_result_lowering(),
            callable_representation: callable.representation(),
            boundary_transport: boundary_transport.model(),
            reference_representation: reference_representation.representation(),
            failure_lowering: control_flow.failure_lowering(),
        }
    }

    pub(crate) const fn guest_word(self) -> GuestWord {
        self.guest_word
    }

    pub(crate) const fn result_lowering(self) -> ResultLoweringMode {
        self.result_lowering
    }

    pub(crate) const fn callable_representation(self) -> CallableRepresentation {
        self.callable_representation
    }

    pub(crate) const fn boundary_transport(self) -> BoundaryTransportModel {
        self.boundary_transport
    }

    pub(crate) const fn reference_representation(self) -> ReferenceRep {
        self.reference_representation
    }

    pub(crate) const fn failure_lowering(self) -> FailureLowering {
        self.failure_lowering
    }

    #[allow(dead_code)]
    pub(crate) const fn uses_reference_types(self) -> bool {
        self.profile.uses_reference_types()
    }

    pub(crate) const fn uses_memory64(self) -> bool {
        self.profile.uses_memory64()
    }

    pub(crate) const fn supports_multi_value(self) -> bool {
        self.profile.supports_multi_value()
    }

    pub(crate) const fn supports_typed_funcref(self) -> bool {
        self.profile.supports_typed_funcref()
    }

    pub(crate) const fn supports_wasm_gc(self) -> bool {
        self.profile.supports_wasm_gc()
    }

    pub(crate) const fn supports_exceptions(self) -> bool {
        self.profile.supports_exceptions()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TargetDecisionSnapshot {
    pub(crate) guest_word: GuestWord,
    pub(crate) result_lowering: ResultLoweringMode,
    pub(crate) callable_representation: CallableRepresentation,
    pub(crate) boundary_transport: BoundaryTransportModel,
    pub(crate) reference_representation: ReferenceRep,
    pub(crate) failure_lowering: FailureLowering,
}

impl TargetDecisionSnapshot {
    pub(crate) const fn from_matrix(matrix: CapabilityMatrix) -> Self {
        Self {
            guest_word: matrix.guest_word(),
            result_lowering: matrix.result_lowering(),
            callable_representation: matrix.callable_representation(),
            boundary_transport: matrix.boundary_transport(),
            reference_representation: matrix.reference_representation(),
            failure_lowering: matrix.failure_lowering(),
        }
    }

    #[cfg(test)]
    pub(crate) fn dump(&self) -> String {
        format!(
            "guest_word={} result_lowering={} callable_representation={} boundary_transport={} \
             reference_representation={} failure_lowering={}",
            self.guest_word.dump_name(),
            self.result_lowering.dump_name(),
            self.callable_representation.dump_name(),
            self.boundary_transport.dump_name(),
            self.reference_representation.dump_name(),
            self.failure_lowering.dump_name(),
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct TargetPolicies {
    profile: TargetProfile,
    import_provider: ImportProvider,
    boundary_transport: BoundaryTransportProfile,
    callable_lowering: CallableLoweringStrategy,
    memory_model: MemoryModelStrategy,
    signature: SignatureStrategy,
    control_flow: ControlFlowStrategy,
    reference_representation: ReferenceRepresentationStrategy,
    capability_matrix: CapabilityMatrix,
}

impl TargetPolicies {
    pub(crate) const fn for_profile(profile: TargetProfile) -> Self {
        let import_provider = ImportProvider::new(profile);
        let boundary_transport = BoundaryTransportProfile::new(profile);
        let callable_lowering = CallableLoweringStrategy::new(profile);
        let memory_model = MemoryModelStrategy::new(profile);
        let signature = SignatureStrategy::new(profile);
        let control_flow = ControlFlowStrategy::new(profile);
        let reference_representation = ReferenceRepresentationStrategy::new(profile);
        let capability_matrix = CapabilityMatrix::new(
            profile,
            signature,
            boundary_transport,
            callable_lowering,
            control_flow,
            reference_representation,
        );
        Self {
            profile,
            import_provider,
            boundary_transport,
            callable_lowering,
            memory_model,
            signature,
            control_flow,
            reference_representation,
            capability_matrix,
        }
    }

    pub(crate) const fn profile(self) -> TargetProfile {
        self.profile
    }

    pub(crate) const fn import_provider(self) -> ImportProvider {
        self.import_provider
    }

    pub(crate) const fn boundary_transport(self) -> BoundaryTransportProfile {
        self.boundary_transport
    }

    pub(crate) const fn callable_lowering(self) -> CallableLoweringStrategy {
        self.callable_lowering
    }

    pub(crate) const fn memory_model(self) -> MemoryModelStrategy {
        self.memory_model
    }

    pub(crate) const fn signature(self) -> SignatureStrategy {
        self.signature
    }

    pub(crate) const fn control_flow(self) -> ControlFlowStrategy {
        self.control_flow
    }

    pub(crate) const fn reference_representation(self) -> ReferenceRepresentationStrategy {
        self.reference_representation
    }

    pub(crate) const fn capability_matrix(self) -> CapabilityMatrix {
        self.capability_matrix
    }

    pub(crate) const fn decision_snapshot(self) -> TargetDecisionSnapshot {
        TargetDecisionSnapshot::from_matrix(self.capability_matrix)
    }

    pub(crate) fn validate_backend_support(self, range: TextRange) -> Result<(), Diagnostic> {
        if self.capability_matrix.guest_word() != GuestWord::I32
            || self.capability_matrix.uses_memory64()
        {
            return Err(Diagnostic::error(
                format!(
                    "the target profile `{}` selected guest word `{}` and memory64={}, but the \
                     current memory model, helper ABI, and boundary transport still only \
                     implement 32-bit guest pointers",
                    self.profile.canonical_name(),
                    self.capability_matrix.guest_word().dump_name(),
                    self.capability_matrix.uses_memory64()
                ),
                range,
            ));
        }
        if self.capability_matrix.supports_multi_value() {
            return Err(Diagnostic::error(
                format!(
                    "the target profile `{}` selected result lowering mode `{}`, but the current \
                     stackifier and emitter still only implement the conservative spill-to-locals \
                     path for scalar joins",
                    self.profile.canonical_name(),
                    self.capability_matrix.result_lowering().dump_name()
                ),
                range,
            ));
        }
        if self.capability_matrix.supports_typed_funcref() {
            return Err(Diagnostic::error(
                format!(
                    "the target profile `{}` selected callable representation `{}`, but callable \
                     registry planning, wrapper generation, and indirect-call emission still only \
                     implement handle-and-table lowering",
                    self.profile.canonical_name(),
                    self.capability_matrix.callable_representation().dump_name()
                ),
                range,
            ));
        }
        if self.capability_matrix.supports_wasm_gc() {
            return Err(Diagnostic::error(
                format!(
                    "the target profile `{}` selected reference representation `{}`, but layout, \
                     retain/release planning, and runtime interop still assume \
                     linear-memory-managed references",
                    self.profile.canonical_name(),
                    self.capability_matrix.reference_representation().dump_name()
                ),
                range,
            ));
        }
        if self.capability_matrix.supports_exceptions() {
            return Err(Diagnostic::error(
                format!(
                    "the target profile `{}` selected failure lowering `{}`, but wrapper and \
                     function emission still only implement trap-style failure handling",
                    self.profile.canonical_name(),
                    self.capability_matrix.failure_lowering().dump_name()
                ),
                range,
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wasm_core_v2_m32_profile_matches_current_backend_defaults() {
        let profile = TargetProfile::wasm_core_v2_m32();
        assert_eq!(profile.pointer_width(), PointerWidth::M32);
        assert!(!profile.supports_multi_value());
        assert!(profile.uses_reference_types());
        assert!(!profile.supports_typed_funcref());
        assert!(!profile.supports_wasm_gc());
        assert!(!profile.uses_memory64());
        assert!(!profile.supports_exceptions());
        assert_eq!(profile.canonical_name(), "wasm-core-v2/m32");
    }

    #[test]
    fn default_policies_match_current_table_and_memory_model() {
        let policies = TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32());
        assert_eq!(policies.memory_model().word_type(), ValType::I32);
        assert_eq!(policies.memory_model().pointer_backend_ty(), BackendTy::Int);
        assert!(policies.callable_lowering().uses_table_slots());
        assert_eq!(policies.callable_lowering().table_element_type(), RefType::FUNCREF);
        assert_eq!(policies.signature().guest_word(), GuestWord::I32);
        assert_eq!(
            policies.control_flow().result_lowering_mode(&AbiTy::Scalar(RuntimeBackendTy::Int)),
            ResultLoweringMode::SpillToLocals
        );
        assert_eq!(
            policies.reference_representation().representation(),
            ReferenceRep::LinearMemoryManaged
        );
        assert_eq!(
            policies.decision_snapshot().dump(),
            "guest_word=i32 result_lowering=spill_to_locals \
             callable_representation=handle_and_table boundary_transport=abi_v2_wrappers \
             reference_representation=linear_memory_managed failure_lowering=trap_only"
        );
    }

    #[test]
    fn future_profiles_select_expected_target_strategies() {
        let multi_value =
            TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32().with_multi_value(true));
        assert_eq!(
            multi_value.control_flow().result_lowering_mode(&AbiTy::Scalar(RuntimeBackendTy::Int)),
            ResultLoweringMode::MultiValue
        );

        let typed_funcref =
            TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32().with_typed_funcref(true));
        assert_eq!(
            typed_funcref.callable_lowering().representation(),
            CallableRepresentation::TypedFuncRef
        );
        assert!(!typed_funcref.callable_lowering().uses_table_slots());

        let gc = TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32().with_wasm_gc(true));
        assert_eq!(gc.reference_representation().representation(), ReferenceRep::GcRef);

        let exceptions =
            TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32().with_exceptions(true));
        assert_eq!(exceptions.control_flow().failure_lowering(), FailureLowering::Exceptions);

        let memory64 = TargetPolicies::for_profile(
            TargetProfile::wasm_core_v2_m32()
                .with_pointer_width(PointerWidth::M64)
                .with_memory64(true),
        );
        assert_eq!(memory64.memory_model().guest_word(), GuestWord::I64);
    }

    #[test]
    fn callable_strategy_signatures_follow_selected_representation() {
        let signature = FunctionSignature {
            params: vec![AbiTy::Scalar(RuntimeBackendTy::Int)],
            result: AbiTy::Scalar(RuntimeBackendTy::Int),
        };
        let default = TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32());
        let lowered = default.callable_lowering().callable_signature(
            default.signature(),
            default.memory_model(),
            &signature,
        );
        assert_eq!(lowered.params, vec![ValType::I32, ValType::I32]);
        assert_eq!(lowered.results, vec![ValType::I32]);

        let typed_funcref =
            TargetPolicies::for_profile(TargetProfile::wasm_core_v2_m32().with_typed_funcref(true));
        let lowered = typed_funcref.callable_lowering().callable_signature(
            typed_funcref.signature(),
            typed_funcref.memory_model(),
            &signature,
        );
        assert_eq!(lowered.params, vec![ValType::I32]);
        assert_eq!(lowered.results, vec![ValType::I32]);
    }
}

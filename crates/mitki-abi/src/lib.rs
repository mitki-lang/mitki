mod canonical;
mod codec;
mod contract;
mod metadata;

pub use canonical::{
    AbiScalar, AbiValue, ArrayElements, CANONICAL_BLOB_ENCODING_VERSION, CANONICAL_BLOB_MAGIC,
    CanonicalBlobHeader, CanonicalBlobView, CanonicalGraph, CanonicalNode, CanonicalNodeHeader,
    CanonicalNodeKind, HandleSlot, HandleSlotId, NodeId, PackedScalarKind, TransportClass,
    ValueRef, decode_canonical_blob, encode_canonical_blob,
};
pub use contract::{
    ABI_V2_ALLOC_EXPORT, ABI_V2_BLOB_RELEASE_EXPORT, ABI_V2_HANDLE_RELEASE_EXPORT,
    ABI_V2_HANDLE_RETAIN_EXPORT, ABI_V2_INVOKE_EXPORT_PREFIX, ContractValType, GraphSupport,
    ModuleLinkage, TYPED_BOUNDARY_EXPORT_PREFIX, WASM_CORE_V2_M32_PROFILE,
    WASM_CORE_V2_M32_PROFILE_MAJOR, WASM_CORE_V2_M32_PROFILE_MINOR,
    WASM_CORE_V2_M32_RUNTIME_SUPPORT, collect_module_linkage, export_wasm_name, field_name,
    find_export_instance, find_export_instance_by_id, find_import_instance_by_linkage,
    function_instance_wasm_field_name, function_instance_wasm_module_name,
    handle_invoke_export_name, helper_export_signature, immediate_is_unit_like, signature,
    signature_uses_canonical_transport, string_value, symbol_name, transport_carrier_type,
    transport_has_wasm_lane, transport_wasm_lane, typed_boundary_wasm_name,
    typed_signature_wasm_shape, validate_graph_schema, validate_graph_support,
    validate_wasm_module_contract, variant_name,
};
pub use metadata::{
    ABI_SEMANTIC_MAJOR, ABI_SEMANTIC_MINOR, AbiTypeKind, BoundaryMemory, CapabilityId, DebugNameId,
    EnumVariant, ExecutionDomain, FacetPlan, FacetPlanEntry, FacetPlanEntryKind, FacetPlanId,
    FieldNameId, FunctionInstance, FunctionSignature, GenericOrigin, GenericOriginId, InstanceId,
    LinkageKind, METADATA_ENCODING_VERSION, METADATA_V2_MAGIC, REQUIRED_FEATURE_HANDLES,
    REQUIRED_FEATURE_INTERSECTION_TRANSPORT, REQUIRED_FEATURE_RECURSIVE_CANONICAL,
    REQUIRED_FEATURE_UNION_TRANSPORT, RawExportRecord, RawImportRecord, RecordField,
    RecursiveGroup, RecursiveGroupId, SemanticTypeGraph, SigId, StringId, SymbolId, TransportRef,
    TypeFingerprint, TypeId, TypeNode, VariantNameId, decode_semantic_type_graph,
    encode_semantic_type_graph,
};

pub const CANONICAL_ABI_SEMANTIC_MAJOR: u16 = ABI_SEMANTIC_MAJOR;
pub const CANONICAL_ABI_SEMANTIC_MINOR: u16 = ABI_SEMANTIC_MINOR;

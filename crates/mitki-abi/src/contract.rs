use std::collections::{BTreeMap, BTreeSet};

use anyhow::{anyhow, bail};

use crate::{
    ABI_SEMANTIC_MAJOR, ABI_SEMANTIC_MINOR, AbiTypeKind, FunctionInstance, FunctionSignature,
    InstanceId, LinkageKind, METADATA_ENCODING_VERSION, REQUIRED_FEATURE_HANDLES,
    REQUIRED_FEATURE_INTERSECTION_TRANSPORT, REQUIRED_FEATURE_RECURSIVE_CANONICAL,
    REQUIRED_FEATURE_UNION_TRANSPORT, SemanticTypeGraph, SigId, StringId, SymbolId, TransportClass,
    TransportRef, TypeId, TypeNode,
};

pub const WASM_CORE_V2_M32_PROFILE: &str = "wasm-core-v2/m32";
pub const WASM_CORE_V2_M32_PROFILE_MAJOR: u16 = 1;
pub const WASM_CORE_V2_M32_PROFILE_MINOR: u16 = 0;

pub const ABI_V2_ALLOC_EXPORT: &str = "mitki:abi/2/alloc";
pub const ABI_V2_BLOB_RELEASE_EXPORT: &str = "mitki:abi/2/blob_release";
pub const ABI_V2_HANDLE_RETAIN_EXPORT: &str = "mitki:abi/2/handle_retain";
pub const ABI_V2_HANDLE_RELEASE_EXPORT: &str = "mitki:abi/2/handle_release";
pub const ABI_V2_INVOKE_EXPORT_PREFIX: &str = "mitki:abi/2/invoke$";
pub const TYPED_BOUNDARY_EXPORT_PREFIX: &str = "mitki:typed/2/f$";

pub(crate) const WASM_CORE_V2_M32_SUPPORTED_REQUIRED_FEATURES: u64 = REQUIRED_FEATURE_HANDLES
    | REQUIRED_FEATURE_RECURSIVE_CANONICAL
    | REQUIRED_FEATURE_UNION_TRANSPORT
    | REQUIRED_FEATURE_INTERSECTION_TRANSPORT;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GraphSupport {
    pub required_features: u64,
    pub transport_profile: &'static str,
    pub profile_version_major: u16,
    pub profile_version_minor: u16,
}

pub const WASM_CORE_V2_M32_RUNTIME_SUPPORT: GraphSupport = GraphSupport {
    required_features: WASM_CORE_V2_M32_SUPPORTED_REQUIRED_FEATURES,
    transport_profile: WASM_CORE_V2_M32_PROFILE,
    profile_version_major: WASM_CORE_V2_M32_PROFILE_MAJOR,
    profile_version_minor: WASM_CORE_V2_M32_PROFILE_MINOR,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ContractValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref,
}

impl ContractValType {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::V128 => "v128",
            Self::Ref => "ref",
        }
    }
}

impl std::fmt::Display for ContractValType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WasmSignatureShape {
    pub params: Vec<ContractValType>,
    pub results: Vec<ContractValType>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModuleLinkage {
    pub imports: BTreeSet<(String, String)>,
    pub function_imports: BTreeMap<(String, String), WasmSignatureShape>,
    pub exports: BTreeSet<String>,
    pub function_exports: BTreeMap<String, WasmSignatureShape>,
    pub memory_exports: BTreeSet<String>,
}

pub fn typed_boundary_wasm_name(metadata_index: usize) -> String {
    format!("{TYPED_BOUNDARY_EXPORT_PREFIX}{metadata_index}")
}

pub fn handle_invoke_export_name(signature_id: SigId) -> String {
    format!("{ABI_V2_INVOKE_EXPORT_PREFIX}{}", signature_id.0)
}

pub fn helper_export_signature(
    name: &str,
    graph: Option<&SemanticTypeGraph>,
) -> anyhow::Result<Option<WasmSignatureShape>> {
    Ok(match name {
        ABI_V2_ALLOC_EXPORT => Some(WasmSignatureShape {
            params: vec![ContractValType::I32, ContractValType::I32],
            results: vec![ContractValType::I32],
        }),
        ABI_V2_BLOB_RELEASE_EXPORT => {
            Some(WasmSignatureShape { params: vec![ContractValType::I32], results: Vec::new() })
        }
        ABI_V2_HANDLE_RETAIN_EXPORT => Some(WasmSignatureShape {
            params: vec![ContractValType::I32],
            results: vec![ContractValType::I32],
        }),
        ABI_V2_HANDLE_RELEASE_EXPORT => {
            Some(WasmSignatureShape { params: vec![ContractValType::I32], results: Vec::new() })
        }
        _ if name.starts_with(ABI_V2_INVOKE_EXPORT_PREFIX) => {
            let graph =
                graph.ok_or_else(|| anyhow!("invoke helper signature lookup requires metadata"))?;
            let raw = name.trim_start_matches(ABI_V2_INVOKE_EXPORT_PREFIX);
            let sig_id = raw
                .parse::<u32>()
                .map(SigId)
                .map_err(|error| anyhow!("invalid invoke helper signature id `{raw}`: {error}"))?;
            let mut shape = typed_signature_wasm_shape(graph, signature(graph, sig_id)?)?;
            shape.params.insert(0, ContractValType::I32);
            Some(shape)
        }
        _ => None,
    })
}

pub fn string_value(graph: &SemanticTypeGraph, id: StringId) -> anyhow::Result<&str> {
    graph
        .strings
        .get(id.0 as usize)
        .map(String::as_str)
        .ok_or_else(|| anyhow!("unknown ABI v2 string `{}`", id.0))
}

pub fn symbol_name(graph: &SemanticTypeGraph, id: SymbolId) -> anyhow::Result<&str> {
    let string_id = *graph
        .nominal_symbols
        .get(id.0 as usize)
        .ok_or_else(|| anyhow!("unknown ABI v2 symbol `{}`", id.0))?;
    string_value(graph, string_id)
}

pub fn field_name(graph: &SemanticTypeGraph, id: crate::FieldNameId) -> anyhow::Result<&str> {
    let string_id = *graph
        .field_names
        .get(id.0 as usize)
        .ok_or_else(|| anyhow!("unknown ABI v2 field name `{}`", id.0))?;
    string_value(graph, string_id)
}

pub fn variant_name(graph: &SemanticTypeGraph, id: crate::VariantNameId) -> anyhow::Result<&str> {
    let string_id = *graph
        .variant_names
        .get(id.0 as usize)
        .ok_or_else(|| anyhow!("unknown ABI v2 variant name `{}`", id.0))?;
    string_value(graph, string_id)
}

pub(crate) fn type_node(graph: &SemanticTypeGraph, ty: TypeId) -> anyhow::Result<&TypeNode> {
    graph.types.get(ty.0 as usize).ok_or_else(|| anyhow!("unknown ABI v2 type `{}`", ty.0))
}

pub fn signature(graph: &SemanticTypeGraph, id: SigId) -> anyhow::Result<&FunctionSignature> {
    graph
        .signatures
        .get(id.0 as usize)
        .ok_or_else(|| anyhow!("unknown ABI v2 signature `{}`", id.0))
}

pub fn function_instance_wasm_module_name<'a>(
    graph: &'a SemanticTypeGraph,
    instance: &'a FunctionInstance,
) -> anyhow::Result<Option<&'a str>> {
    instance.wasm_module_name.map(|id| string_value(graph, id)).transpose()
}

pub fn function_instance_wasm_field_name<'a>(
    graph: &'a SemanticTypeGraph,
    instance: &'a FunctionInstance,
) -> anyhow::Result<Option<&'a str>> {
    instance.wasm_field_name.map(|id| string_value(graph, id)).transpose()
}

pub fn export_wasm_name<'a>(
    graph: &'a SemanticTypeGraph,
    instance: &'a FunctionInstance,
) -> anyhow::Result<&'a str> {
    function_instance_wasm_field_name(graph, instance)?
        .ok_or_else(|| anyhow!("typed v2 export is missing its Wasm linkage name"))
}

pub fn find_export_instance<'a>(
    graph: &'a SemanticTypeGraph,
    name: &str,
) -> anyhow::Result<&'a FunctionInstance> {
    let mut matches = graph.function_instances.iter().filter(|instance| {
        instance.linkage == LinkageKind::WasmExport
            && (symbol_name(graph, instance.logical_symbol).is_ok_and(|symbol| symbol == name)
                || function_instance_wasm_field_name(graph, instance)
                    .ok()
                    .flatten()
                    .is_some_and(|field| field == name))
    });
    let Some(instance) = matches.next() else {
        bail!("typed v2 invocation could not find export `{name}`");
    };
    if matches.next().is_some() {
        bail!("typed v2 invocation found multiple exports named `{name}`");
    }
    Ok(instance)
}

pub fn find_export_instance_by_id(
    graph: &SemanticTypeGraph,
    id: InstanceId,
) -> anyhow::Result<&FunctionInstance> {
    graph
        .function_instances
        .iter()
        .find(|instance| instance.linkage == LinkageKind::WasmExport && instance.id == id)
        .ok_or_else(|| anyhow!("typed v2 invocation could not find export instance `{}`", id.0))
}

pub fn find_import_instance_by_linkage<'a>(
    graph: &'a SemanticTypeGraph,
    module: &str,
    name: &str,
) -> Option<&'a FunctionInstance> {
    graph.function_instances.iter().find(|instance| {
        instance.linkage == LinkageKind::WasmImport
            && function_instance_wasm_module_name(graph, instance)
                .ok()
                .flatten()
                .is_some_and(|candidate| candidate == module)
            && function_instance_wasm_field_name(graph, instance)
                .ok()
                .flatten()
                .is_some_and(|candidate| candidate == name)
    })
}

pub fn transport_carrier_type(transport: &TransportRef) -> TypeId {
    transport.transport_type.unwrap_or(transport.semantic_type)
}

pub fn signature_uses_canonical_transport(signature: &FunctionSignature) -> bool {
    signature
        .params
        .iter()
        .chain(std::iter::once(&signature.result))
        .any(|transport| transport.transport_class == TransportClass::CanonicalValue)
}

pub fn immediate_is_unit_like(kind: &AbiTypeKind) -> bool {
    match kind {
        AbiTypeKind::Unit => true,
        AbiTypeKind::Tuple { elems } => elems.is_empty(),
        _ => false,
    }
}

pub fn transport_wasm_lane(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
) -> anyhow::Result<Option<ContractValType>> {
    let transport_ty = transport_carrier_type(transport);
    Ok(match transport.transport_class {
        TransportClass::Immediate => match &type_node(graph, transport_ty)?.kind {
            AbiTypeKind::Unit => None,
            AbiTypeKind::Tuple { elems } if elems.is_empty() => None,
            AbiTypeKind::Bool
            | AbiTypeKind::Int { .. }
            | AbiTypeKind::Char
            | AbiTypeKind::Enum { .. } => Some(ContractValType::I32),
            AbiTypeKind::Float { .. } => Some(ContractValType::F64),
            other => bail!("immediate ABI v2 transport cannot lower `{other:?}`"),
        },
        TransportClass::CanonicalValue | TransportClass::CapabilityHandle => {
            Some(ContractValType::I32)
        }
    })
}

pub fn transport_has_wasm_lane(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
) -> anyhow::Result<bool> {
    Ok(transport_wasm_lane(graph, transport)?.is_some())
}

pub fn typed_signature_wasm_shape(
    graph: &SemanticTypeGraph,
    signature: &FunctionSignature,
) -> anyhow::Result<WasmSignatureShape> {
    let params = signature
        .params
        .iter()
        .map(|transport| transport_wasm_lane(graph, transport))
        .collect::<anyhow::Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .collect();
    let results = transport_wasm_lane(graph, &signature.result)?.into_iter().collect();
    Ok(WasmSignatureShape { params, results })
}

pub fn validate_graph_support(
    graph: &SemanticTypeGraph,
    support: GraphSupport,
) -> anyhow::Result<()> {
    if graph.semantic_version_major != ABI_SEMANTIC_MAJOR {
        bail!(
            "runtime supports ABI v2 semantic major {}, found {}",
            ABI_SEMANTIC_MAJOR,
            graph.semantic_version_major
        );
    }
    if graph.semantic_version_minor > ABI_SEMANTIC_MINOR {
        bail!(
            "runtime supports ABI v2 semantic minor up to {}, found {}",
            ABI_SEMANTIC_MINOR,
            graph.semantic_version_minor
        );
    }
    if graph.encoding_version != METADATA_ENCODING_VERSION {
        bail!(
            "runtime supports ABI v2 metadata encoding version {}, found {}",
            METADATA_ENCODING_VERSION,
            graph.encoding_version
        );
    }
    let unsupported = graph.required_features & !support.required_features;
    if unsupported != 0 {
        bail!("runtime does not support required ABI v2 feature bits `0x{unsupported:x}`");
    }
    let profile = string_value(graph, graph.transport_profile)?;
    if profile != support.transport_profile {
        bail!("runtime does not support ABI v2 transport profile `{profile}`");
    }
    if graph.profile_version_major > support.profile_version_major
        || (graph.profile_version_major == support.profile_version_major
            && graph.profile_version_minor > support.profile_version_minor)
    {
        bail!(
            "runtime supports ABI v2 transport profile version up to {}.{}, found {}.{}",
            support.profile_version_major,
            support.profile_version_minor,
            graph.profile_version_major,
            graph.profile_version_minor
        );
    }
    Ok(())
}

pub fn validate_graph_schema(graph: &SemanticTypeGraph) -> anyhow::Result<()> {
    if graph.semantic_version_major != ABI_SEMANTIC_MAJOR {
        bail!(
            "ABI v2 metadata semantic major drifted: expected {}, found {}",
            ABI_SEMANTIC_MAJOR,
            graph.semantic_version_major
        );
    }
    if graph.semantic_version_minor > ABI_SEMANTIC_MINOR {
        bail!(
            "ABI v2 metadata semantic minor drifted past supported maximum {}",
            ABI_SEMANTIC_MINOR
        );
    }
    if graph.encoding_version != METADATA_ENCODING_VERSION {
        bail!(
            "ABI v2 metadata encoding version drifted: expected {}, found {}",
            METADATA_ENCODING_VERSION,
            graph.encoding_version
        );
    }

    let mut normalized = graph.clone();
    normalized.normalize_commutative_members()?;
    for (index, (expected, actual)) in normalized.types.iter().zip(graph.types.iter()).enumerate() {
        if expected.kind != actual.kind {
            bail!("ABI v2 type `{index}` was not normalized deterministically");
        }
    }

    let mut derived = graph.clone();
    derived.populate_recursive_groups();
    if derived.recursive_groups != graph.recursive_groups {
        bail!("ABI v2 recursive groups drifted from the derived contract state");
    }

    derived.populate_fingerprints()?;
    for (expected, actual) in derived.types.iter().zip(graph.types.iter()) {
        if expected.fingerprint != actual.fingerprint {
            bail!(
                "ABI v2 fingerprint drifted for type `{}`: expected {}, found {}",
                actual.id.0,
                expected.fingerprint.to_hex(),
                actual.fingerprint.to_hex()
            );
        }
    }

    derived.recompute_required_features();
    if derived.required_features != graph.required_features {
        bail!(
            "ABI v2 required feature bits drifted: expected `0x{:x}`, found `0x{:x}`",
            derived.required_features,
            graph.required_features
        );
    }

    for (index, field_name_id) in graph.field_names.iter().copied().enumerate() {
        let _ = string_value(graph, field_name_id)
            .map_err(|error| anyhow!("invalid field name id at index {index}: {error}"))?;
    }
    for (index, variant_name_id) in graph.variant_names.iter().copied().enumerate() {
        let _ = string_value(graph, variant_name_id)
            .map_err(|error| anyhow!("invalid variant name id at index {index}: {error}"))?;
    }
    for (index, symbol_id) in graph.nominal_symbols.iter().copied().enumerate() {
        let _ = string_value(graph, symbol_id)
            .map_err(|error| anyhow!("invalid nominal symbol id at index {index}: {error}"))?;
    }

    for (expected_index, node) in graph.types.iter().enumerate() {
        if node.id.0 as usize != expected_index {
            bail!("ABI v2 type id drifted at index {expected_index}: found {}", node.id.0);
        }
        let _ = type_node(graph, node.id)?;
        validate_type_refs(graph, &node.kind)?;
    }

    for (expected_index, group) in graph.recursive_groups.iter().enumerate() {
        if group.id.0 as usize != expected_index {
            bail!(
                "ABI v2 recursive-group id drifted at index {expected_index}: found {}",
                group.id.0
            );
        }
        for &member in &group.members {
            let node = type_node(graph, member)?;
            if node.recursive_group != Some(group.id) {
                bail!("ABI v2 recursive-group membership drifted for type `{}`", member.0);
            }
        }
    }

    for (expected_index, signature_entry) in graph.signatures.iter().enumerate() {
        if signature_entry.id.0 as usize != expected_index {
            bail!(
                "ABI v2 signature id drifted at index {expected_index}: found {}",
                signature_entry.id.0
            );
        }
        if !matches!(
            type_node(graph, signature_entry.function_type)?.kind,
            AbiTypeKind::Function { .. }
        ) {
            bail!("ABI v2 signature `{}` did not point at a function type", signature_entry.id.0);
        }
        for transport in
            signature_entry.params.iter().chain(std::iter::once(&signature_entry.result))
        {
            let _ = type_node(graph, transport.semantic_type)?;
            if let Some(transport_type) = transport.transport_type {
                let _ = type_node(graph, transport_type)?;
            }
            let _ = transport_wasm_lane(graph, transport)?;
        }
    }

    for (expected_index, origin) in graph.generic_origins.iter().enumerate() {
        if origin.id.0 as usize != expected_index {
            bail!(
                "ABI v2 generic-origin id drifted at index {expected_index}: found {}",
                origin.id.0
            );
        }
        let _ = symbol_name(graph, origin.symbol)?;
    }

    for (expected_index, plan) in graph.facet_plans.iter().enumerate() {
        if plan.id.0 as usize != expected_index {
            bail!("ABI v2 facet-plan id drifted at index {expected_index}: found {}", plan.id.0);
        }
        for entry in &plan.entries {
            let _ = type_node(graph, entry.member)?;
        }
    }

    for (expected_index, instance) in graph.function_instances.iter().enumerate() {
        if instance.id.0 as usize != expected_index {
            bail!(
                "ABI v2 function-instance id drifted at index {expected_index}: found {}",
                instance.id.0
            );
        }
        let _ = symbol_name(graph, instance.logical_symbol)?;
        if let Some(origin) = instance.generic_origin {
            graph
                .generic_origins
                .get(origin.0 as usize)
                .ok_or_else(|| anyhow!("unknown ABI v2 generic origin `{}`", origin.0))?;
        }
        for &type_arg in &instance.type_args {
            let _ = type_node(graph, type_arg)?;
        }
        let _ = signature(graph, instance.signature)?;
        let _ = function_instance_wasm_module_name(graph, instance)?;
        let _ = function_instance_wasm_field_name(graph, instance)?;
        match instance.linkage {
            LinkageKind::WasmExport | LinkageKind::StageEntry => {
                if instance.wasm_field_name.is_none() {
                    bail!(
                        "ABI v2 function instance `{}` is missing its Wasm field name",
                        instance.id.0
                    );
                }
            }
            LinkageKind::WasmImport => {
                if instance.wasm_module_name.is_none() || instance.wasm_field_name.is_none() {
                    bail!("ABI v2 import instance `{}` is missing its Wasm linkage", instance.id.0);
                }
            }
            LinkageKind::RawImport => {}
        }
    }

    for raw_import in &graph.raw_imports {
        let _ = string_value(graph, raw_import.module)?;
        let _ = string_value(graph, raw_import.field)?;
        if let Some(symbol) = raw_import.symbol {
            let _ = symbol_name(graph, symbol)?;
        }
        if let Some(signature_id) = raw_import.signature {
            let _ = signature(graph, signature_id)?;
        }
    }
    for raw_export in &graph.raw_exports {
        let _ = string_value(graph, raw_export.name)?;
        if let Some(symbol) = raw_export.symbol {
            let _ = symbol_name(graph, symbol)?;
        }
        if let Some(signature_id) = raw_export.signature {
            let _ = signature(graph, signature_id)?;
        }
    }
    if let Some(export_name) = graph.boundary_memory.export_name {
        let _ = string_value(graph, export_name)?;
    }

    Ok(())
}

pub fn collect_module_linkage(bytes: &[u8]) -> anyhow::Result<ModuleLinkage> {
    let mut function_types = Vec::new();
    let mut function_type_indices = Vec::new();
    let mut linkage = ModuleLinkage::default();

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload?;
        match payload {
            wasmparser::Payload::TypeSection(reader) => {
                for ty in reader.into_iter_err_on_gc_types() {
                    let ty = ty?;
                    function_types.push(WasmSignatureShape {
                        params: ty.params().iter().copied().map(contract_val_type).collect(),
                        results: ty.results().iter().copied().map(contract_val_type).collect(),
                    });
                }
            }
            wasmparser::Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
                    let import = import?;
                    let key = (import.module.to_owned(), import.name.to_owned());
                    linkage.imports.insert(key.clone());
                    match import.ty {
                        wasmparser::TypeRef::Func(type_index)
                        | wasmparser::TypeRef::FuncExact(type_index) => {
                            function_type_indices.push(type_index);
                            let shape = function_types
                                .get(type_index as usize)
                                .cloned()
                                .ok_or_else(|| anyhow!("missing function type `{type_index}`"))?;
                            linkage.function_imports.insert(key, shape);
                        }
                        wasmparser::TypeRef::Table(_)
                        | wasmparser::TypeRef::Memory(_)
                        | wasmparser::TypeRef::Global(_)
                        | wasmparser::TypeRef::Tag(_) => {}
                    }
                }
            }
            wasmparser::Payload::FunctionSection(reader) => {
                for function in reader {
                    function_type_indices.push(function?);
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    let export = export?;
                    let name = export.name.to_owned();
                    linkage.exports.insert(name.clone());
                    match export.kind {
                        wasmparser::ExternalKind::Func | wasmparser::ExternalKind::FuncExact => {
                            let type_index =
                                *function_type_indices.get(export.index as usize).ok_or_else(
                                    || anyhow!("missing function index `{}`", export.index),
                                )?;
                            let shape = function_types
                                .get(type_index as usize)
                                .cloned()
                                .ok_or_else(|| anyhow!("missing function type `{type_index}`"))?;
                            linkage.function_exports.insert(name, shape);
                        }
                        wasmparser::ExternalKind::Memory => {
                            linkage.memory_exports.insert(name);
                        }
                        wasmparser::ExternalKind::Table
                        | wasmparser::ExternalKind::Global
                        | wasmparser::ExternalKind::Tag => {}
                    }
                }
            }
            _ => {}
        }
    }

    Ok(linkage)
}

pub fn validate_wasm_module_contract(
    bytes: &[u8],
    graph: &SemanticTypeGraph,
) -> anyhow::Result<()> {
    validate_graph_schema(graph)?;
    let linkage = collect_module_linkage(bytes)?;
    validate_boundary_memory(graph, &linkage)?;
    validate_raw_linkage(graph, &linkage)?;
    validate_function_instances(graph, &linkage)?;
    validate_helper_exports(graph, &linkage)?;
    Ok(())
}

fn validate_type_refs(graph: &SemanticTypeGraph, kind: &AbiTypeKind) -> anyhow::Result<()> {
    match kind {
        AbiTypeKind::Array { elem } => {
            let _ = type_node(graph, *elem)?;
        }
        AbiTypeKind::Tuple { elems } | AbiTypeKind::Union { members: elems } => {
            for &elem in elems {
                let _ = type_node(graph, elem)?;
            }
        }
        AbiTypeKind::Record { fields } | AbiTypeKind::Struct { fields, .. } => {
            for field in fields {
                let _ = field_name(graph, field.name)?;
                let _ = type_node(graph, field.ty)?;
            }
        }
        AbiTypeKind::Enum { variants, .. } => {
            for variant in variants {
                let _ = variant_name(graph, variant.name)?;
                for &field in &variant.fields {
                    let _ = type_node(graph, field)?;
                }
            }
        }
        AbiTypeKind::Intersection { members, carrier, facet_plan } => {
            for &member in members {
                let _ = type_node(graph, member)?;
            }
            let _ = type_node(graph, *carrier)?;
            if let Some(plan_id) = facet_plan {
                graph
                    .facet_plans
                    .get(plan_id.0 as usize)
                    .ok_or_else(|| anyhow!("unknown ABI v2 facet plan `{}`", plan_id.0))?;
            }
        }
        AbiTypeKind::Function { params, result, .. } => {
            for &param in params {
                let _ = type_node(graph, param)?;
            }
            let _ = type_node(graph, *result)?;
        }
        AbiTypeKind::Unit
        | AbiTypeKind::Bool
        | AbiTypeKind::Int { .. }
        | AbiTypeKind::Float { .. }
        | AbiTypeKind::Char
        | AbiTypeKind::String
        | AbiTypeKind::Opaque { .. } => {}
    }
    Ok(())
}

fn validate_boundary_memory(
    graph: &SemanticTypeGraph,
    linkage: &ModuleLinkage,
) -> anyhow::Result<()> {
    if let Some(export_name) = graph.boundary_memory.export_name {
        let export_name = string_value(graph, export_name)?;
        if !linkage.memory_exports.contains(export_name) {
            bail!("ABI v2 boundary memory export `{export_name}` was not found in the module");
        }
    }
    Ok(())
}

fn validate_raw_linkage(graph: &SemanticTypeGraph, linkage: &ModuleLinkage) -> anyhow::Result<()> {
    for import in &graph.raw_imports {
        let module = string_value(graph, import.module)?;
        let field = string_value(graph, import.field)?;
        if !linkage.imports.contains(&(module.to_owned(), field.to_owned())) {
            bail!("ABI v2 raw import `{module}::{field}` was not found in the module");
        }
    }
    for export in &graph.raw_exports {
        let name = string_value(graph, export.name)?;
        if !linkage.exports.contains(name) {
            bail!("ABI v2 raw export `{name}` was not found in the module");
        }
    }
    Ok(())
}

fn validate_function_instances(
    graph: &SemanticTypeGraph,
    linkage: &ModuleLinkage,
) -> anyhow::Result<()> {
    for instance in &graph.function_instances {
        let signature = signature(graph, instance.signature)?;
        if !matches!(type_node(graph, signature.function_type)?.kind, AbiTypeKind::Function { .. })
        {
            bail!("ABI v2 function instance `{}` did not point at a function type", instance.id.0);
        }
        let expected_shape = typed_signature_wasm_shape(graph, signature)?;
        match instance.linkage {
            LinkageKind::WasmExport | LinkageKind::StageEntry => {
                let export_name = export_wasm_name(graph, instance)?;
                let actual_shape = linkage.function_exports.get(export_name).ok_or_else(|| {
                    anyhow!("ABI v2 export `{export_name}` was not found in the module")
                })?;
                if actual_shape != &expected_shape {
                    bail!(
                        "ABI v2 export `{export_name}` signature drifted from metadata: expected \
                         {:?}, found {:?}",
                        expected_shape,
                        actual_shape
                    );
                }
            }
            LinkageKind::WasmImport => {
                let module_name = function_instance_wasm_module_name(graph, instance)?
                    .ok_or_else(|| anyhow!("ABI v2 import instance is missing its module name"))?;
                let field_name = function_instance_wasm_field_name(graph, instance)?
                    .ok_or_else(|| anyhow!("ABI v2 import instance is missing its field name"))?;
                let actual_shape = linkage
                    .function_imports
                    .get(&(module_name.to_owned(), field_name.to_owned()))
                    .ok_or_else(|| {
                        anyhow!(
                            "ABI v2 import `{module_name}::{field_name}` was not found in the \
                             module"
                        )
                    })?;
                if actual_shape != &expected_shape {
                    bail!(
                        "ABI v2 import `{module_name}::{field_name}` signature drifted from \
                         metadata: expected {:?}, found {:?}",
                        expected_shape,
                        actual_shape
                    );
                }
            }
            LinkageKind::RawImport => {}
        }
    }
    Ok(())
}

fn validate_helper_exports(
    graph: &SemanticTypeGraph,
    linkage: &ModuleLinkage,
) -> anyhow::Result<()> {
    let mut required_helpers = Vec::new();
    if graph.function_instances.iter().any(|instance| {
        matches!(instance.linkage, LinkageKind::WasmExport | LinkageKind::StageEntry)
            && signature(graph, instance.signature).is_ok_and(signature_uses_canonical_transport)
    }) {
        required_helpers.push(ABI_V2_ALLOC_EXPORT);
        required_helpers.push(ABI_V2_BLOB_RELEASE_EXPORT);
    }
    if graph.required_features & REQUIRED_FEATURE_HANDLES != 0 {
        required_helpers.push(ABI_V2_HANDLE_RETAIN_EXPORT);
        required_helpers.push(ABI_V2_HANDLE_RELEASE_EXPORT);
    }

    for helper in required_helpers {
        let Some(actual_shape) = linkage.function_exports.get(helper) else {
            bail!("ABI v2 helper export `{helper}` was not found in the module");
        };
        let expected_shape = helper_export_signature(helper, Some(graph))?
            .ok_or_else(|| anyhow!("missing ABI v2 helper contract for `{helper}`"))?;
        if actual_shape != &expected_shape {
            bail!(
                "ABI v2 helper export `{helper}` signature drifted from the contract: expected \
                 {:?}, found {:?}",
                expected_shape,
                actual_shape
            );
        }
    }

    for (export_name, actual_shape) in &linkage.function_exports {
        let Some(expected_shape) = helper_export_signature(export_name, Some(graph))? else {
            continue;
        };
        if actual_shape != &expected_shape {
            bail!(
                "ABI v2 helper export `{export_name}` signature drifted from the contract: \
                 expected {:?}, found {:?}",
                expected_shape,
                actual_shape
            );
        }
    }

    Ok(())
}

fn contract_val_type(value_type: wasmparser::ValType) -> ContractValType {
    match value_type {
        wasmparser::ValType::I32 => ContractValType::I32,
        wasmparser::ValType::I64 => ContractValType::I64,
        wasmparser::ValType::F32 => ContractValType::F32,
        wasmparser::ValType::F64 => ContractValType::F64,
        wasmparser::ValType::V128 => ContractValType::V128,
        wasmparser::ValType::Ref(_) => ContractValType::Ref,
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::{
        BoundaryMemory, ExecutionDomain, FieldNameId, RawExportRecord, RecordField, SymbolId,
    };

    fn sample_graph() -> SemanticTypeGraph {
        let mut graph = SemanticTypeGraph::default();
        let profile = graph.insert_string(WASM_CORE_V2_M32_PROFILE);
        let memory = graph.insert_string("memory");
        let point = graph.insert_string("Point");
        let x = graph.insert_string("x");
        let y = graph.insert_string("y");
        let make = graph.insert_string("make_point");
        let typed_export = graph.insert_string(typed_boundary_wasm_name(0));
        graph.transport_profile = profile;
        graph.profile_version_major = WASM_CORE_V2_M32_PROFILE_MAJOR;
        graph.profile_version_minor = WASM_CORE_V2_M32_PROFILE_MINOR;
        graph.boundary_memory = BoundaryMemory { memory_index: 0, export_name: Some(memory) };
        graph.field_names = vec![x, y];
        graph.nominal_symbols = vec![point, make];

        let int = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let point = graph.push_type(AbiTypeKind::Struct {
            nominal: SymbolId(0),
            fields: vec![
                RecordField { name: FieldNameId(0), ty: int },
                RecordField { name: FieldNameId(1), ty: int },
            ],
        });
        let function_type = graph.push_type(AbiTypeKind::Function {
            params: Vec::new(),
            result: point,
            domain: ExecutionDomain::Runtime,
        });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type,
            params: Vec::new(),
            result: TransportRef {
                semantic_type: point,
                transport_class: TransportClass::CanonicalValue,
                transport_type: None,
            },
        });
        graph.function_instances.push(FunctionInstance {
            id: InstanceId(0),
            logical_symbol: SymbolId(1),
            generic_origin: None,
            type_args: Vec::new(),
            signature: SigId(0),
            domain: ExecutionDomain::Runtime,
            linkage: LinkageKind::WasmExport,
            wasm_module_name: None,
            wasm_field_name: Some(typed_export),
        });
        graph.raw_exports.push(RawExportRecord {
            name: typed_export,
            symbol: Some(SymbolId(1)),
            signature: Some(SigId(0)),
        });
        graph.populate_recursive_groups();
        graph.normalize_commutative_members().expect("normalized graph");
        graph.populate_fingerprints().expect("fingerprints");
        graph.recompute_required_features();
        graph
    }

    fn contract_dump(graph: &SemanticTypeGraph) -> String {
        let instance = find_export_instance(graph, "make_point").expect("export instance");
        let signature = typed_signature_wasm_shape(
            graph,
            signature(graph, instance.signature).expect("signature"),
        )
        .expect("typed signature");
        let mut output = String::new();
        output.push_str("contract.profile: ");
        output.push_str(string_value(graph, graph.transport_profile).expect("profile"));
        output.push('\n');
        output.push_str("contract.functions:\n");
        output.push_str("  - ");
        output.push_str(symbol_name(graph, instance.logical_symbol).expect("symbol"));
        output.push_str(" => ");
        output.push_str(export_wasm_name(graph, instance).expect("export name"));
        output.push('\n');
        output.push_str("contract.signature: (");
        output.push_str(
            &signature.params.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "),
        );
        output.push_str(") -> (");
        output.push_str(
            &signature.results.iter().map(ToString::to_string).collect::<Vec<_>>().join(", "),
        );
        output.push_str(")\n");
        output.push_str("contract.fingerprints:\n");
        for node in &graph.types {
            output.push_str("  - [");
            output.push_str(&node.id.0.to_string());
            output.push_str("] ");
            output.push_str(&node.fingerprint.to_hex());
            output.push('\n');
        }
        output
    }

    #[test]
    fn contract_helpers_snapshot_stays_stable() {
        let graph = sample_graph();
        expect![[r#"
contract.profile: wasm-core-v2/m32
contract.functions:
  - make_point => mitki:typed/2/f$0
contract.signature: () -> (i32)
contract.fingerprints:
  - [0] d3b476ed5070a8014b52821b10c03e6c
  - [1] 51bb14d9577e66538d9ecdba93907b15
  - [2] 4630744f55067020fab30a8204531198
"#]]
        .assert_eq(&contract_dump(&graph));
    }

    #[test]
    fn graph_schema_validation_recomputes_fingerprints_and_feature_bits() {
        let mut graph = sample_graph();
        graph.required_features ^= REQUIRED_FEATURE_RECURSIVE_CANONICAL;
        let error = validate_graph_schema(&graph).expect_err("graph schema should fail");
        assert!(error.to_string().contains("required feature bits drifted"));
    }

    #[test]
    fn typed_boundary_names_and_helper_signatures_are_stable() {
        assert_eq!(typed_boundary_wasm_name(7), "mitki:typed/2/f$7");
        assert_eq!(handle_invoke_export_name(SigId(3)), "mitki:abi/2/invoke$3");
        assert_eq!(
            helper_export_signature(ABI_V2_HANDLE_RETAIN_EXPORT, None).expect("helper signature"),
            Some(WasmSignatureShape {
                params: vec![ContractValType::I32],
                results: vec![ContractValType::I32],
            })
        );
    }
}

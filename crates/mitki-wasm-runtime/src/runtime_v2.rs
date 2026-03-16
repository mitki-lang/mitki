#[cfg(test)]
use std::collections::BTreeMap;

use anyhow::{Context as _, anyhow, bail};
use mitki_abi::{
    AbiScalar, AbiTypeKind, AbiValue, ContractValType, ExecutionDomain, SemanticTypeGraph,
    TransportClass, TransportRef, TypeId, WASM_CORE_V2_M32_RUNTIME_SUPPORT, decode_canonical_blob,
    decode_semantic_type_graph, encode_canonical_blob, export_wasm_name, find_export_instance,
    find_export_instance_by_id, immediate_is_unit_like, signature, string_value, symbol_name,
    transport_carrier_type, transport_wasm_lane, validate_graph_support,
    validate_wasm_module_contract,
};
#[cfg(test)]
use mitki_abi::{
    ArrayElements, CanonicalGraph, CanonicalNode, FacetPlan, FacetPlanEntry, FacetPlanEntryKind,
    SymbolId, ValueRef, field_name, variant_name,
};

use crate::runtime::WasmRuntime;
#[cfg(test)]
use crate::runtime::WasmValue;

const MITKI_ABI_V2_CUSTOM_SECTION: &str = "mitki.abi.v2";

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MitkiModuleAbiV2 {
    pub metadata: Option<SemanticTypeGraph>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct AbiRunOutput {
    pub stdout: String,
    pub result: Option<AbiValue>,
}

pub fn describe_module_abi_v2(bytes: &[u8]) -> anyhow::Result<MitkiModuleAbiV2> {
    let mut metadata = None;
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload?;
        if let wasmparser::Payload::CustomSection(reader) = payload
            && reader.name() == MITKI_ABI_V2_CUSTOM_SECTION
        {
            let graph = decode_semantic_type_graph(reader.data())?;
            validate_module_abi_v2(bytes, &graph)?;
            metadata = Some(graph);
        }
    }
    Ok(MitkiModuleAbiV2 { metadata })
}

fn validate_module_abi_v2(bytes: &[u8], graph: &SemanticTypeGraph) -> anyhow::Result<()> {
    validate_graph_support(graph, WASM_CORE_V2_M32_RUNTIME_SUPPORT)?;
    validate_wasm_module_contract(bytes, graph)?;
    Ok(())
}

impl WasmRuntime {
    pub(crate) fn invoke_export_v2(
        &mut self,
        export: &str,
        args: &[AbiValue],
    ) -> anyhow::Result<AbiRunOutput> {
        let metadata = self
            .abi_v2
            .metadata
            .clone()
            .ok_or_else(|| anyhow!("typed v2 invocation requires `mitki.abi.v2` metadata"))?;
        let instance = find_export_instance(&metadata, export)?;
        if instance.domain == ExecutionDomain::Stage {
            bail!("stage entry `{export}` cannot be invoked through the runtime export API");
        }
        let signature = signature(&metadata, instance.signature)?;
        if signature.params.len() != args.len() {
            bail!(
                "typed v2 invocation expected {} argument(s) for `{export}`, found {}",
                signature.params.len(),
                args.len()
            );
        }
        let typed_export_name = instance
            .wasm_field_name
            .and_then(|field| string_value(&metadata, field).ok())
            .ok_or_else(|| anyhow!("typed v2 export `{export}` is missing its Wasm linkage name"))?
            .to_owned();
        self.clear_stdout();

        let mut params = Vec::with_capacity(signature.params.len());
        let mut owned_arg_blobs = Vec::new();
        for (arg, transport) in args.iter().zip(signature.params.iter()) {
            match transport.transport_class {
                TransportClass::Immediate => {
                    if let Some(val) =
                        abi_immediate_to_val(&metadata, transport_carrier_type(transport), arg)?
                    {
                        params.push(val);
                    }
                }
                TransportClass::CanonicalValue => {
                    let retained_handles = self.retain_abi_value_handles(arg)?;
                    let bytes = match encode_canonical_blob(arg) {
                        Ok(bytes) => bytes,
                        Err(error) => {
                            self.release_handle_ids(&retained_handles)?;
                            return Err(error);
                        }
                    };
                    let ptr = match self.call_abi_v2_alloc(
                        i32::try_from(bytes.len())
                            .map_err(|_error| anyhow!("canonical argument blob was too large"))?,
                        4,
                    ) {
                        Ok(ptr) => ptr,
                        Err(error) => {
                            self.release_handle_ids(&retained_handles)?;
                            return Err(error);
                        }
                    };
                    let offset = usize::try_from(ptr)
                        .map_err(|_error| anyhow!("canonical argument pointer was negative"))?;
                    if let Err(error) = self.write_memory(offset, &bytes) {
                        self.release_handle_ids(&retained_handles)?;
                        return Err(error);
                    }
                    params.push(wasmtime::Val::I32(ptr));
                    owned_arg_blobs.push(ptr);
                }
                TransportClass::CapabilityHandle => {
                    params.push(wasmtime::Val::I32(abi_value_to_handle_lane(transport, arg)?));
                }
            }
        }

        let mut results = typed_result_slots(&metadata, &signature.result)?;
        let call_result = self.call_dynamic_export(&typed_export_name, &params, &mut results);
        if let Err(error) = call_result {
            for ptr in owned_arg_blobs.into_iter().rev() {
                let _ = self.call_abi_v2_blob_release(ptr);
            }
            return Err(error);
        }

        let result = decode_typed_v2_result(self, &metadata, &signature.result, &results)?;
        for ptr in owned_arg_blobs.into_iter().rev() {
            self.call_abi_v2_blob_release(ptr)
                .with_context(|| format!("while releasing typed v2 argument blob `{ptr}`"))?;
        }
        let stdout = self.take_stdout();
        Ok(AbiRunOutput { stdout, result })
    }

    pub(crate) fn invoke_export_instance_v2(
        &mut self,
        instance_id: mitki_abi::InstanceId,
        args: &[AbiValue],
    ) -> anyhow::Result<AbiRunOutput> {
        let metadata = self
            .abi_v2
            .metadata
            .clone()
            .ok_or_else(|| anyhow!("typed v2 invocation requires `mitki.abi.v2` metadata"))?;
        let instance = find_export_instance_by_id(&metadata, instance_id)?;
        let logical_name = symbol_name(&metadata, instance.logical_symbol)?.to_owned();
        let typed_export_name = export_wasm_name(&metadata, instance)?.to_owned();
        let signature = signature(&metadata, instance.signature)?;
        if signature.params.len() != args.len() {
            bail!(
                "typed v2 invocation expected {} argument(s) for export instance `{}` \
                 (`{logical_name}`), found {}",
                signature.params.len(),
                instance_id.0,
                args.len()
            );
        }
        self.clear_stdout();

        let mut params = Vec::with_capacity(signature.params.len());
        let mut owned_arg_blobs = Vec::new();
        for (arg, transport) in args.iter().zip(signature.params.iter()) {
            match transport.transport_class {
                TransportClass::Immediate => {
                    if let Some(val) =
                        abi_immediate_to_val(&metadata, transport_carrier_type(transport), arg)?
                    {
                        params.push(val);
                    }
                }
                TransportClass::CanonicalValue => {
                    let retained_handles = self.retain_abi_value_handles(arg)?;
                    let bytes = match encode_canonical_blob(arg) {
                        Ok(bytes) => bytes,
                        Err(error) => {
                            self.release_handle_ids(&retained_handles)?;
                            return Err(error);
                        }
                    };
                    let ptr = match self.call_abi_v2_alloc(
                        i32::try_from(bytes.len())
                            .map_err(|_error| anyhow!("canonical argument blob was too large"))?,
                        4,
                    ) {
                        Ok(ptr) => ptr,
                        Err(error) => {
                            self.release_handle_ids(&retained_handles)?;
                            return Err(error);
                        }
                    };
                    let offset = usize::try_from(ptr)
                        .map_err(|_error| anyhow!("canonical argument pointer was negative"))?;
                    if let Err(error) = self.write_memory(offset, &bytes) {
                        self.release_handle_ids(&retained_handles)?;
                        return Err(error);
                    }
                    params.push(wasmtime::Val::I32(ptr));
                    owned_arg_blobs.push(ptr);
                }
                TransportClass::CapabilityHandle => {
                    params.push(wasmtime::Val::I32(abi_value_to_handle_lane(transport, arg)?));
                }
            }
        }

        let mut results = typed_result_slots(&metadata, &signature.result)?;
        let call_result = self.call_dynamic_export(&typed_export_name, &params, &mut results);
        if let Err(error) = call_result {
            for ptr in owned_arg_blobs.into_iter().rev() {
                let _ = self.call_abi_v2_blob_release(ptr);
            }
            return Err(error);
        }

        let result = decode_typed_v2_result(self, &metadata, &signature.result, &results)?;
        for ptr in owned_arg_blobs.into_iter().rev() {
            self.call_abi_v2_blob_release(ptr)
                .with_context(|| format!("while releasing typed v2 argument blob `{ptr}`"))?;
        }
        let stdout = self.take_stdout();
        Ok(AbiRunOutput { stdout, result })
    }
}

pub fn typed_result_slots(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
) -> anyhow::Result<Vec<wasmtime::Val>> {
    Ok(match transport_wasm_lane(graph, transport)? {
        None => Vec::new(),
        Some(ContractValType::I32) => vec![wasmtime::Val::I32(0)],
        Some(ContractValType::F64) => vec![wasmtime::Val::F64(0)],
        Some(other) => bail!("unexpected ABI v2 result lane `{other}`"),
    })
}

pub(crate) fn abi_immediate_to_val(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    value: &AbiValue,
) -> anyhow::Result<Option<wasmtime::Val>> {
    let AbiValue::Immediate(scalar) = value else {
        bail!("ABI v2 immediate transport expected an immediate value")
    };
    Ok(match scalar {
        AbiScalar::Unit => {
            if immediate_is_unit_like(&type_kind(graph, ty)?.kind) {
                None
            } else {
                bail!("ABI v2 unit scalar did not match the semantic transport type")
            }
        }
        AbiScalar::Bool(value) => Some(wasmtime::Val::I32(i32::from(*value))),
        AbiScalar::Int { value, .. } => Some(wasmtime::Val::I32(
            i32::try_from(*value)
                .map_err(|_error| anyhow!("ABI v2 integer scalar exceeded i32 range"))?,
        )),
        AbiScalar::Float { raw_bits, .. } => Some(wasmtime::Val::F64(*raw_bits)),
        AbiScalar::Char { unicode_scalar } => Some(wasmtime::Val::I32(
            i32::try_from(*unicode_scalar)
                .map_err(|_error| anyhow!("ABI v2 char scalar exceeded i32 range"))?,
        )),
        AbiScalar::EnumTag { variant_index, .. } => {
            let AbiTypeKind::Enum { variants, .. } = &type_kind(graph, ty)?.kind else {
                bail!("ABI v2 enum-tag immediate expected an enum type")
            };
            if *variant_index as usize >= variants.len() {
                bail!("ABI v2 enum tag `{variant_index}` was out of range")
            }
            Some(wasmtime::Val::I32(
                i32::try_from(*variant_index)
                    .map_err(|_error| anyhow!("ABI v2 enum tag exceeded i32 range"))?,
            ))
        }
    })
}

fn decode_typed_v2_result(
    runtime: &mut WasmRuntime,
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
    results: &[wasmtime::Val],
) -> anyhow::Result<Option<AbiValue>> {
    Ok(match transport.transport_class {
        TransportClass::Immediate => {
            let carrier = transport_carrier_type(transport);
            if immediate_is_unit_like(&type_kind(graph, carrier)?.kind) {
                None
            } else {
                Some(AbiValue::Immediate(read_immediate_result(graph, carrier, results)?))
            }
        }
        TransportClass::CanonicalValue => {
            let ptr = results
                .first()
                .and_then(wasmtime::Val::i32)
                .ok_or_else(|| anyhow!("expected canonical ABI v2 pointer result"))?;
            let bytes = runtime.read_canonical_blob(ptr)?;
            let value = decode_canonical_blob(&bytes)?;
            let retained_handles = runtime.retain_abi_value_handles(&value);
            runtime
                .call_abi_v2_blob_release(ptr)
                .with_context(|| format!("while releasing typed v2 result blob `{ptr}`"))?;
            retained_handles?;
            Some(value)
        }
        TransportClass::CapabilityHandle => Some(AbiValue::Handle {
            type_id: transport_carrier_type(transport),
            handle_id: i32_to_handle_id(
                results
                    .first()
                    .and_then(wasmtime::Val::i32)
                    .ok_or_else(|| anyhow!("expected ABI v2 handle result"))?,
            )?,
        }),
    })
}

pub fn read_immediate_result(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    results: &[wasmtime::Val],
) -> anyhow::Result<AbiScalar> {
    let kind = &type_kind(graph, ty)?.kind;
    Ok(match kind {
        AbiTypeKind::Bool => AbiScalar::Bool(
            results
                .first()
                .and_then(wasmtime::Val::i32)
                .ok_or_else(|| anyhow!("expected i32 boolean result"))?
                != 0,
        ),
        AbiTypeKind::Int { signed, bits } => AbiScalar::Int {
            signed: *signed,
            bits: *bits,
            value: i64::from(
                results
                    .first()
                    .and_then(wasmtime::Val::i32)
                    .ok_or_else(|| anyhow!("expected i32 integer result"))?,
            ),
        },
        AbiTypeKind::Float { bits } => AbiScalar::Float {
            bits: *bits,
            raw_bits: results
                .first()
                .and_then(wasmtime::Val::f64)
                .ok_or_else(|| anyhow!("expected f64 result"))?
                .to_bits(),
        },
        AbiTypeKind::Char => AbiScalar::Char {
            unicode_scalar: u32::try_from(
                results
                    .first()
                    .and_then(wasmtime::Val::i32)
                    .ok_or_else(|| anyhow!("expected i32 char result"))?,
            )
            .map_err(|_error| anyhow!("ABI v2 char result was negative"))?,
        },
        AbiTypeKind::Enum { .. } => AbiScalar::EnumTag {
            type_id: ty,
            variant_index: u32::try_from(
                results
                    .first()
                    .and_then(wasmtime::Val::i32)
                    .ok_or_else(|| anyhow!("expected i32 enum-tag result"))?,
            )
            .map_err(|_error| anyhow!("ABI v2 enum tag result was negative"))?,
        },
        kind if immediate_is_unit_like(kind) => AbiScalar::Unit,
        other => bail!("unexpected immediate ABI v2 result kind `{other:?}`"),
    })
}

#[cfg(test)]
pub(crate) fn abi_value_to_wasm_value(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
    value: &AbiValue,
) -> anyhow::Result<WasmValue> {
    match transport.transport_class {
        TransportClass::Immediate => {
            immediate_abi_value_to_wasm(graph, transport_carrier_type(transport), value)
        }
        TransportClass::CanonicalValue => canonical_abi_value_to_wasm(graph, transport, value),
        TransportClass::CapabilityHandle => handle_abi_value_to_wasm(transport, value),
    }
}

#[cfg(test)]
pub(crate) fn wasm_value_to_abi_value(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
    value: &WasmValue,
) -> anyhow::Result<AbiValue> {
    match transport.transport_class {
        TransportClass::Immediate => {
            wasm_value_to_immediate_abi(graph, transport_carrier_type(transport), value)
        }
        TransportClass::CanonicalValue => wasm_value_to_canonical_abi(graph, transport, value),
        TransportClass::CapabilityHandle => wasm_value_to_handle_abi(transport, value),
    }
}

#[cfg(test)]
fn immediate_abi_value_to_wasm(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    value: &AbiValue,
) -> anyhow::Result<WasmValue> {
    let AbiValue::Immediate(scalar) = value else {
        bail!("ABI v2 immediate transport expected an immediate value")
    };
    scalar_to_wasm_value(graph, ty, scalar)
}

#[cfg(test)]
fn wasm_value_to_immediate_abi(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    value: &WasmValue,
) -> anyhow::Result<AbiValue> {
    Ok(AbiValue::Immediate(wasm_value_to_scalar(graph, ty, value)?))
}

#[cfg(test)]
fn canonical_abi_value_to_wasm(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
    value: &AbiValue,
) -> anyhow::Result<WasmValue> {
    let AbiValue::Canonical { transport_type, graph: canonical } = value else {
        bail!("ABI v2 canonical transport expected a canonical value")
    };
    let carrier = transport_carrier_type(transport);
    if *transport_type != carrier {
        bail!(
            "ABI v2 canonical value type mismatch: expected type `{}`, found `{}`",
            carrier.0,
            transport_type.0
        );
    }
    decode_canonical_value_ref(graph, canonical, transport.semantic_type, &canonical.root)
}

#[cfg(test)]
fn wasm_value_to_canonical_abi(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
    value: &WasmValue,
) -> anyhow::Result<AbiValue> {
    let mut nodes = Vec::new();
    let mut handles = Vec::new();
    let root =
        encode_wasm_value_ref(graph, transport.semantic_type, value, &mut nodes, &mut handles)?;
    Ok(AbiValue::Canonical {
        transport_type: transport_carrier_type(transport),
        graph: CanonicalGraph { root, nodes, handles },
    })
}

#[cfg(test)]
fn scalar_to_wasm_value(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    scalar: &AbiScalar,
) -> anyhow::Result<WasmValue> {
    Ok(match scalar {
        AbiScalar::Unit => WasmValue::Unit,
        AbiScalar::Bool(value) => WasmValue::Bool(*value),
        AbiScalar::Int { value, .. } => WasmValue::I32(
            i32::try_from(*value)
                .map_err(|_error| anyhow!("ABI v2 integer scalar exceeded i32 range"))?,
        ),
        AbiScalar::Float { raw_bits, .. } => WasmValue::F64(f64::from_bits(*raw_bits)),
        AbiScalar::Char { unicode_scalar } => WasmValue::Char(
            char::from_u32(*unicode_scalar)
                .ok_or_else(|| anyhow!("invalid ABI v2 char scalar `{unicode_scalar}`"))?,
        ),
        AbiScalar::EnumTag { variant_index, .. } => {
            let AbiTypeKind::Enum { variants, .. } = &type_kind(graph, ty)?.kind else {
                bail!("ABI v2 enum tag transport expected an enum type")
            };
            let variant = variants
                .get(*variant_index as usize)
                .ok_or_else(|| anyhow!("ABI v2 enum tag `{variant_index}` was out of range"))?;
            WasmValue::Enum {
                variant: variant_name(graph, variant.name)?.to_owned(),
                fields: Vec::new(),
            }
        }
    })
}

#[cfg(test)]
fn wasm_value_to_scalar(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    value: &WasmValue,
) -> anyhow::Result<AbiScalar> {
    Ok(match (value, &type_kind(graph, ty)?.kind) {
        (WasmValue::Unit, AbiTypeKind::Unit) => AbiScalar::Unit,
        (WasmValue::Bool(value), AbiTypeKind::Bool) => AbiScalar::Bool(*value),
        (WasmValue::I32(value), AbiTypeKind::Int { signed, bits }) => {
            AbiScalar::Int { signed: *signed, bits: *bits, value: i64::from(*value) }
        }
        (WasmValue::F64(value), AbiTypeKind::Float { bits }) => {
            AbiScalar::Float { bits: *bits, raw_bits: value.to_bits() }
        }
        (WasmValue::Char(value), AbiTypeKind::Char) => {
            AbiScalar::Char { unicode_scalar: u32::from(*value) }
        }
        (WasmValue::Enum { variant, fields }, AbiTypeKind::Enum { variants, .. })
            if fields.is_empty() =>
        {
            let variant_index = variants
                .iter()
                .position(|candidate| {
                    variant_name(graph, candidate.name).is_ok_and(|name| name == variant)
                })
                .ok_or_else(|| anyhow!("unknown nullary enum variant `{variant}`"))?;
            AbiScalar::EnumTag { type_id: ty, variant_index: variant_index as u32 }
        }
        _ => bail!("value `{value:?}` does not match immediate ABI v2 type `{}`", ty.0),
    })
}

#[cfg(test)]
fn handle_abi_value_to_wasm(
    transport: &TransportRef,
    value: &AbiValue,
) -> anyhow::Result<WasmValue> {
    let AbiValue::Handle { type_id, handle_id } = value else {
        bail!("ABI v2 capability transport expected a handle value");
    };
    let carrier = transport_carrier_type(transport);
    if *type_id != carrier {
        bail!("ABI v2 handle type mismatch: expected `{}`, found `{}`", carrier.0, type_id.0);
    }
    Ok(WasmValue::Handle { type_id: type_id.0, handle_id: *handle_id })
}

#[cfg(test)]
fn wasm_value_to_handle_abi(
    transport: &TransportRef,
    value: &WasmValue,
) -> anyhow::Result<AbiValue> {
    let WasmValue::Handle { type_id, handle_id } = value else {
        bail!("ABI v2 capability transport expected a handle value");
    };
    let carrier = transport_carrier_type(transport);
    if *type_id != carrier.0 {
        bail!("ABI v2 handle type mismatch: expected `{}`, found `{}`", carrier.0, type_id);
    }
    Ok(AbiValue::Handle { type_id: carrier, handle_id: *handle_id })
}

#[cfg(test)]
fn decode_canonical_value_ref(
    graph: &SemanticTypeGraph,
    canonical: &CanonicalGraph,
    ty: TypeId,
    value: &ValueRef,
) -> anyhow::Result<WasmValue> {
    if let AbiTypeKind::Intersection { carrier, facet_plan, .. } = &type_kind(graph, ty)?.kind {
        return match value {
            ValueRef::NodeRef(node_id) => {
                let node = canonical
                    .nodes
                    .get(node_id.0 as usize)
                    .ok_or_else(|| anyhow!("canonical node `{}` was out of range", node_id.0))?;
                match node {
                    CanonicalNode::Intersection { carrier: carrier_value, facets, .. } => {
                        let live_entries = live_facet_entries(graph, *facet_plan)?;
                        if live_entries.len() != facets.len() {
                            bail!("intersection facet count did not match the facet plan");
                        }
                        Ok(WasmValue::Intersection {
                            carrier: Box::new(decode_canonical_value_ref(
                                graph,
                                canonical,
                                *carrier,
                                carrier_value,
                            )?),
                            facets: live_entries
                                .iter()
                                .zip(facets.iter())
                                .map(|(entry, facet)| {
                                    decode_canonical_value_ref(
                                        graph,
                                        canonical,
                                        entry.member,
                                        facet,
                                    )
                                })
                                .collect::<anyhow::Result<Vec<_>>>()?,
                        })
                    }
                    _ => Ok(WasmValue::Intersection {
                        carrier: Box::new(decode_canonical_value_ref(
                            graph, canonical, *carrier, value,
                        )?),
                        facets: Vec::new(),
                    }),
                }
            }
            ValueRef::InlineScalar(_) | ValueRef::HandleRef(_) => Ok(WasmValue::Intersection {
                carrier: Box::new(decode_canonical_value_ref(graph, canonical, *carrier, value)?),
                facets: Vec::new(),
            }),
        };
    }

    match value {
        ValueRef::InlineScalar(scalar) => scalar_to_wasm_value(graph, ty, scalar),
        ValueRef::NodeRef(node_id) => {
            let node = canonical
                .nodes
                .get(node_id.0 as usize)
                .ok_or_else(|| anyhow!("canonical node `{}` was out of range", node_id.0))?;
            decode_node_to_wasm_value(graph, canonical, ty, node)
        }
        ValueRef::HandleRef(handle_id) => {
            let slot = canonical.handles.get(handle_id.0 as usize).ok_or_else(|| {
                anyhow!("canonical handle slot `{}` was out of range", handle_id.0)
            })?;
            Ok(WasmValue::Handle { type_id: slot.type_id.0, handle_id: slot.handle_id })
        }
    }
}

#[cfg(test)]
fn decode_node_to_wasm_value(
    graph: &SemanticTypeGraph,
    canonical: &CanonicalGraph,
    ty: TypeId,
    node: &CanonicalNode,
) -> anyhow::Result<WasmValue> {
    Ok(match (&type_kind(graph, ty)?.kind, node) {
        (AbiTypeKind::String, CanonicalNode::String { value, .. }) => {
            WasmValue::String(value.clone())
        }
        (AbiTypeKind::Array { elem }, CanonicalNode::Array { elements, .. }) => {
            let values = match elements {
                ArrayElements::Values(values) => values
                    .iter()
                    .map(|value| decode_canonical_value_ref(graph, canonical, *elem, value))
                    .collect::<anyhow::Result<Vec<_>>>()?,
                ArrayElements::PackedScalars { kind, len, bytes } => {
                    decode_packed_array(graph, *elem, *kind, *len, bytes)?
                }
            };
            WasmValue::Array(values)
        }
        (AbiTypeKind::Tuple { elems }, CanonicalNode::Tuple { fields, .. }) => WasmValue::Tuple(
            elems
                .iter()
                .zip(fields.iter())
                .map(|(&ty, value)| decode_canonical_value_ref(graph, canonical, ty, value))
                .collect::<anyhow::Result<Vec<_>>>()?,
        ),
        (AbiTypeKind::Record { fields: shape }, CanonicalNode::Record { fields, .. }) => {
            WasmValue::Record(
                shape
                    .iter()
                    .zip(fields.iter())
                    .map(|(field, value)| {
                        Ok((
                            field_name(graph, field.name)?.to_owned(),
                            decode_canonical_value_ref(graph, canonical, field.ty, value)?,
                        ))
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?,
            )
        }
        (AbiTypeKind::Struct { fields: shape, .. }, CanonicalNode::Struct { fields, .. }) => {
            WasmValue::Struct(
                shape
                    .iter()
                    .zip(fields.iter())
                    .map(|(field, value)| {
                        Ok((
                            field_name(graph, field.name)?.to_owned(),
                            decode_canonical_value_ref(graph, canonical, field.ty, value)?,
                        ))
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?,
            )
        }
        (AbiTypeKind::Enum { variants, .. }, CanonicalNode::Enum { variant_index, fields, .. }) => {
            let variant = variants
                .get(*variant_index as usize)
                .ok_or_else(|| anyhow!("enum variant index `{variant_index}` was out of range"))?;
            WasmValue::Enum {
                variant: variant_name(graph, variant.name)?.to_owned(),
                fields: variant
                    .fields
                    .iter()
                    .zip(fields.iter())
                    .map(|(&field_ty, value)| {
                        decode_canonical_value_ref(graph, canonical, field_ty, value)
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?,
            }
        }
        (AbiTypeKind::Union { members }, CanonicalNode::Union { arm_index, payload, .. }) => {
            let payload_ty = *members
                .get(*arm_index as usize)
                .ok_or_else(|| anyhow!("union arm index `{arm_index}` was out of range"))?;
            WasmValue::Union {
                arm_index: *arm_index,
                value: Box::new(decode_canonical_value_ref(graph, canonical, payload_ty, payload)?),
            }
        }
        (AbiTypeKind::Intersection { .. }, CanonicalNode::Intersection { .. }) => {
            bail!("intersection nodes are decoded at the value-ref layer")
        }
        _ => bail!("canonical node did not match ABI v2 type `{}`", ty.0),
    })
}

#[cfg(test)]
fn encode_wasm_value_ref(
    graph: &SemanticTypeGraph,
    ty: TypeId,
    value: &WasmValue,
    nodes: &mut Vec<CanonicalNode>,
    handles: &mut Vec<mitki_abi::HandleSlot>,
) -> anyhow::Result<ValueRef> {
    match &type_kind(graph, ty)?.kind {
        AbiTypeKind::Unit
        | AbiTypeKind::Bool
        | AbiTypeKind::Int { .. }
        | AbiTypeKind::Float { .. }
        | AbiTypeKind::Char => Ok(ValueRef::InlineScalar(wasm_value_to_scalar(graph, ty, value)?)),
        AbiTypeKind::Enum { variants, .. }
            if variants.iter().all(|variant| variant.fields.is_empty()) =>
        {
            Ok(ValueRef::InlineScalar(wasm_value_to_scalar(graph, ty, value)?))
        }
        AbiTypeKind::String => {
            let WasmValue::String(text) = value else { bail!("expected string value") };
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::String { transport_type: ty, value: text.clone() });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Array { elem } => {
            let WasmValue::Array(items) = value else { bail!("expected array value") };
            let elements = ArrayElements::Values(
                items
                    .iter()
                    .map(|item| encode_wasm_value_ref(graph, *elem, item, nodes, handles))
                    .collect::<anyhow::Result<Vec<_>>>()?,
            );
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Array { transport_type: ty, elements });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Tuple { elems } => {
            let WasmValue::Tuple(items) = value else { bail!("expected tuple value") };
            let fields = elems
                .iter()
                .zip(items.iter())
                .map(|(&field_ty, item)| {
                    encode_wasm_value_ref(graph, field_ty, item, nodes, handles)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Tuple { transport_type: ty, fields });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Record { fields: shape } => {
            let WasmValue::Record(items) = value else { bail!("expected record value") };
            let map = items.iter().cloned().collect::<BTreeMap<_, _>>();
            let fields = shape
                .iter()
                .map(|field| {
                    let name = field_name(graph, field.name)?;
                    let item =
                        map.get(name).ok_or_else(|| anyhow!("missing record field `{name}`"))?;
                    encode_wasm_value_ref(graph, field.ty, item, nodes, handles)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Record { transport_type: ty, fields });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Struct { fields: shape, .. } => {
            let WasmValue::Struct(items) = value else { bail!("expected struct value") };
            let map = items.iter().cloned().collect::<BTreeMap<_, _>>();
            let fields = shape
                .iter()
                .map(|field| {
                    let name = field_name(graph, field.name)?;
                    let item =
                        map.get(name).ok_or_else(|| anyhow!("missing struct field `{name}`"))?;
                    encode_wasm_value_ref(graph, field.ty, item, nodes, handles)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Struct { transport_type: ty, fields });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Enum { variants, .. } => {
            let WasmValue::Enum { variant, fields } = value else { bail!("expected enum value") };
            let variant_index = variants
                .iter()
                .position(|candidate| {
                    variant_name(graph, candidate.name).is_ok_and(|name| name == variant)
                })
                .ok_or_else(|| anyhow!("unknown enum variant `{variant}`"))?;
            let variant_shape = &variants[variant_index];
            let payload = variant_shape
                .fields
                .iter()
                .zip(fields.iter())
                .map(|(&field_ty, value)| {
                    encode_wasm_value_ref(graph, field_ty, value, nodes, handles)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Enum {
                transport_type: ty,
                variant_index: variant_index as u32,
                fields: payload,
            });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Union { members } => {
            let WasmValue::Union { arm_index, value } = value else {
                bail!("expected union value")
            };
            let payload_ty = *members
                .get(*arm_index as usize)
                .ok_or_else(|| anyhow!("union arm index `{arm_index}` was out of range"))?;
            let payload = encode_wasm_value_ref(graph, payload_ty, value, nodes, handles)?;
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Union { transport_type: ty, arm_index: *arm_index, payload });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Intersection { carrier, facet_plan, .. } => {
            let WasmValue::Intersection { carrier: carrier_value, facets } = value else {
                bail!("expected intersection value")
            };
            let live_entries = live_facet_entries(graph, *facet_plan)?;
            if live_entries.is_empty() {
                return encode_wasm_value_ref(graph, *carrier, carrier_value, nodes, handles);
            }
            if live_entries.len() != facets.len() {
                bail!("intersection facet count did not match the facet plan");
            }
            let carrier = encode_wasm_value_ref(graph, *carrier, carrier_value, nodes, handles)?;
            let facet_values = live_entries
                .iter()
                .zip(facets.iter())
                .map(|(entry, facet)| {
                    encode_wasm_value_ref(graph, entry.member, facet, nodes, handles)
                })
                .collect::<anyhow::Result<Vec<_>>>()?;
            let node_id = mitki_abi::NodeId(nodes.len() as u32);
            nodes.push(CanonicalNode::Intersection {
                transport_type: ty,
                carrier,
                facets: facet_values,
            });
            Ok(ValueRef::NodeRef(node_id))
        }
        AbiTypeKind::Function { .. } | AbiTypeKind::Opaque { .. } => {
            let WasmValue::Handle { type_id, handle_id } = value else {
                bail!("expected handle value")
            };
            if *type_id != ty.0 {
                bail!("expected handle value for type `{}`", ty.0);
            }
            let slot_id = mitki_abi::HandleSlotId(handles.len() as u32);
            handles.push(mitki_abi::HandleSlot { type_id: ty, handle_id: *handle_id });
            Ok(ValueRef::HandleRef(slot_id))
        }
    }
}

#[cfg(test)]
fn decode_packed_array(
    graph: &SemanticTypeGraph,
    elem_ty: TypeId,
    kind: mitki_abi::PackedScalarKind,
    len: u32,
    bytes: &[u8],
) -> anyhow::Result<Vec<WasmValue>> {
    let _ = graph;
    match kind {
        mitki_abi::PackedScalarKind::Bool => {
            Ok(bytes.iter().take(len as usize).map(|byte| WasmValue::Bool(*byte != 0)).collect())
        }
        mitki_abi::PackedScalarKind::I32 => Ok(bytes
            .chunks_exact(4)
            .take(len as usize)
            .map(|chunk| WasmValue::I32(i32::from_le_bytes(chunk.try_into().expect("i32 chunk"))))
            .collect()),
        mitki_abi::PackedScalarKind::F64 => Ok(bytes
            .chunks_exact(8)
            .take(len as usize)
            .map(|chunk| WasmValue::F64(f64::from_le_bytes(chunk.try_into().expect("f64 chunk"))))
            .collect()),
        mitki_abi::PackedScalarKind::Char => Ok(bytes
            .chunks_exact(4)
            .take(len as usize)
            .map(|chunk| {
                let scalar = u32::from_le_bytes(chunk.try_into().expect("char chunk"));
                WasmValue::Char(char::from_u32(scalar).expect("packed scalar char"))
            })
            .collect()),
        mitki_abi::PackedScalarKind::I64 | mitki_abi::PackedScalarKind::F32 => {
            bail!(
                "ABI v2 baseline runtime facade cannot decode packed scalar array kind `{kind:?}` \
                 for element type `{}`",
                elem_ty.0
            )
        }
    }
}

fn type_kind(graph: &SemanticTypeGraph, ty: TypeId) -> anyhow::Result<&mitki_abi::TypeNode> {
    graph.types.get(ty.0 as usize).ok_or_else(|| anyhow!("unknown ABI v2 type `{}`", ty.0))
}

#[cfg(test)]
fn live_facet_entries(
    graph: &SemanticTypeGraph,
    plan_id: Option<mitki_abi::FacetPlanId>,
) -> anyhow::Result<Vec<FacetPlanEntry>> {
    let Some(plan_id) = plan_id else {
        return Ok(Vec::new());
    };
    let plan = facet_plan(graph, plan_id)?;
    Ok(plan
        .entries
        .iter()
        .filter(|entry| entry.kind != FacetPlanEntryKind::Erased)
        .cloned()
        .collect())
}

#[cfg(test)]
fn facet_plan(graph: &SemanticTypeGraph, id: mitki_abi::FacetPlanId) -> anyhow::Result<&FacetPlan> {
    graph
        .facet_plans
        .get(id.0 as usize)
        .ok_or_else(|| anyhow!("unknown ABI v2 facet plan `{}`", id.0))
}

fn abi_value_to_handle_lane(transport: &TransportRef, value: &AbiValue) -> anyhow::Result<i32> {
    let AbiValue::Handle { type_id, handle_id } = value else {
        bail!("ABI v2 capability transport expected a handle value");
    };
    let carrier = transport_carrier_type(transport);
    if *type_id != carrier {
        bail!("ABI v2 handle type mismatch: expected `{}`, found `{}`", carrier.0, type_id.0);
    }
    i32::try_from(*handle_id).map_err(|_error| anyhow!("ABI v2 handle id exceeded i32 range"))
}

fn i32_to_handle_id(raw: i32) -> anyhow::Result<u32> {
    u32::try_from(raw).map_err(|_error| anyhow!("ABI v2 handle id was negative"))
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use mitki_abi::{
        AbiTypeKind, FieldNameId, RecordField, TransportRef, encode_semantic_type_graph,
    };
    use mitki_comptime_wasm::compile_file_to_wasm;
    use mitki_db::RootDatabase;
    use mitki_inputs::File;

    use super::*;
    use crate::{RunConfig, WasmRuntime, describe_module_abi};

    fn compile_fixture(fixture: &str) -> Vec<u8> {
        let db = RootDatabase::default();
        let file = File::new(&db, "runtime_v2_tests.mitki".into(), fixture.to_owned());
        compile_file_to_wasm(&db, file).unwrap_or_else(|diagnostics| {
            let messages =
                diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>();
            panic!("expected Wasm compilation to succeed, got diagnostics: {messages:#?}");
        })
    }

    fn rewrite_v2_metadata(bytes: &[u8], mutate: impl FnOnce(&mut SemanticTypeGraph)) -> Vec<u8> {
        let mut module = wasm_encoder::Module::new();
        let mut mutated = false;
        let mut mutate = Some(mutate);

        for payload in wasmparser::Parser::new(0).parse_all(bytes) {
            let payload = payload.expect("valid wasm payload");
            match payload {
                wasmparser::Payload::CustomSection(reader)
                    if reader.name() == MITKI_ABI_V2_CUSTOM_SECTION =>
                {
                    let mut graph =
                        decode_semantic_type_graph(reader.data()).expect("valid v2 metadata");
                    mutate.take().expect("v2 metadata mutate closure")(&mut graph);
                    let encoded =
                        encode_semantic_type_graph(&graph).expect("re-encoded v2 metadata");
                    module.section(&wasm_encoder::CustomSection {
                        name: reader.name().into(),
                        data: Cow::Owned(encoded),
                    });
                    mutated = true;
                }
                other => {
                    if let Some((id, range)) = other.as_section() {
                        module.section(&wasm_encoder::RawSection { id, data: &bytes[range] });
                    }
                }
            }
        }

        assert!(mutated, "expected ABI v2 metadata section to exist");
        module.finish()
    }

    fn opaque_handle_transport() -> (SemanticTypeGraph, TransportRef) {
        let mut graph = SemanticTypeGraph::default();
        let opaque =
            graph.push_type(AbiTypeKind::Opaque { capability_id: mitki_abi::CapabilityId(0) });
        (
            graph,
            TransportRef {
                semantic_type: opaque,
                transport_class: TransportClass::CapabilityHandle,
                transport_type: None,
            },
        )
    }

    fn struct_with_handle_transport() -> (SemanticTypeGraph, TransportRef, TypeId) {
        let mut graph = SemanticTypeGraph::default();
        let field_name = graph.insert_string("callback");
        graph.field_names.push(field_name);
        let handle_ty =
            graph.push_type(AbiTypeKind::Opaque { capability_id: mitki_abi::CapabilityId(0) });
        let struct_ty = graph.push_type(AbiTypeKind::Struct {
            nominal: SymbolId(0),
            fields: vec![RecordField { name: FieldNameId(0), ty: handle_ty }],
        });
        (
            graph,
            TransportRef {
                semantic_type: struct_ty,
                transport_class: TransportClass::CanonicalValue,
                transport_type: None,
            },
            handle_ty,
        )
    }

    fn union_transport() -> (SemanticTypeGraph, TransportRef) {
        let mut graph = SemanticTypeGraph::default();
        let int_ty = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let string_ty = graph.push_type(AbiTypeKind::String);
        let union_ty = graph.push_type(AbiTypeKind::Union { members: vec![int_ty, string_ty] });
        (
            graph,
            TransportRef {
                semantic_type: union_ty,
                transport_class: TransportClass::CanonicalValue,
                transport_type: None,
            },
        )
    }

    fn intersection_transport() -> (SemanticTypeGraph, TransportRef, TypeId) {
        let mut graph = SemanticTypeGraph::default();
        let field_name = graph.insert_string("value");
        graph.field_names.push(field_name);
        let int_ty = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let carrier_ty = graph.push_type(AbiTypeKind::Record {
            fields: vec![RecordField { name: FieldNameId(0), ty: int_ty }],
        });
        let facet_ty =
            graph.push_type(AbiTypeKind::Opaque { capability_id: mitki_abi::CapabilityId(0) });
        let facet_plan = mitki_abi::FacetPlanId(graph.facet_plans.len() as u32);
        graph.facet_plans.push(FacetPlan {
            id: facet_plan,
            entries: vec![
                FacetPlanEntry { member: carrier_ty, kind: FacetPlanEntryKind::Erased },
                FacetPlanEntry { member: facet_ty, kind: FacetPlanEntryKind::HandleFacet },
            ],
        });
        let intersection_ty = graph.push_type(AbiTypeKind::Intersection {
            members: vec![carrier_ty, facet_ty],
            carrier: carrier_ty,
            facet_plan: Some(facet_plan),
        });
        (
            graph,
            TransportRef {
                semantic_type: intersection_ty,
                transport_class: TransportClass::CanonicalValue,
                transport_type: Some(carrier_ty),
            },
            facet_ty,
        )
    }

    #[test]
    fn describe_module_abi_v2_exposes_typed_export_metadata() {
        let bytes = compile_fixture(
            r#"
export fun make_pair(): (int, int) {
    val pair = (20, 22)
    pair
}
"#,
        );
        let summary = describe_module_abi(&bytes).expect("module ABI summary should decode");
        assert!(!summary.exports.iter().any(|export| export.name == "make_pair"));
        assert!(summary.exports.iter().any(|export| export.name == "mitki:typed/2/f$0"));

        let abi_v2 = describe_module_abi_v2(&bytes).expect("v2 ABI metadata should decode");
        let graph = abi_v2.metadata.expect("expected v2 metadata");
        assert_eq!(graph.function_instances.len(), 1);
        let instance = &graph.function_instances[0];
        assert_eq!(
            symbol_name(&graph, instance.logical_symbol).expect("logical symbol"),
            "make_pair"
        );
        assert_eq!(
            string_value(&graph, instance.wasm_field_name.expect("typed export name"))
                .expect("typed export linkage"),
            "mitki:typed/2/f$0"
        );
        assert!(graph.raw_exports.iter().any(|export| {
            string_value(&graph, export.name).is_ok_and(|name| name == "mitki:typed/2/f$0")
        }));
    }

    #[test]
    fn invoke_export_v2_round_trips_canonical_arrays() {
        let bytes = compile_fixture(
            r#"
export fun echo(value: [int]): [int] {
    value
}
"#,
        );
        let mut runtime = WasmRuntime::new(&bytes, RunConfig::without_wasi())
            .expect("runtime should instantiate");
        let metadata = runtime.abi_v2.metadata.clone().expect("expected v2 metadata");
        let instance = find_export_instance(&metadata, "echo").expect("echo export instance");
        let signature = signature(&metadata, instance.signature).expect("signature");
        let input = wasm_value_to_abi_value(
            &metadata,
            &signature.params[0],
            &WasmValue::Array(vec![WasmValue::I32(20), WasmValue::I32(22)]),
        )
        .expect("input should canonicalize");

        let output =
            runtime.invoke_export_v2("echo", &[input]).expect("v2 invocation should succeed");
        let result = output.result.expect("echo should return a value");
        let decoded = abi_value_to_wasm_value(&metadata, &signature.result, &result)
            .expect("result should decode through v2 facade");
        assert_eq!(decoded, WasmValue::Array(vec![WasmValue::I32(20), WasmValue::I32(22)]));
    }

    #[test]
    fn describe_module_abi_v2_rejects_unsupported_required_features() {
        let bytes = compile_fixture(
            r#"
export fun answer(): int {
    42
}
"#,
        );
        let bytes = rewrite_v2_metadata(&bytes, |graph| {
            graph.required_features |= 1 << 63;
        });

        let error = describe_module_abi_v2(&bytes).expect_err("unsupported feature bits");
        assert!(error.to_string().contains("required ABI v2 feature bits"));
    }

    #[test]
    fn describe_module_abi_v2_rejects_invalid_boundary_memory_export() {
        let bytes = compile_fixture(
            r#"
export fun answer(): int {
    42
}
"#,
        );
        let bytes = rewrite_v2_metadata(&bytes, |graph| {
            let memory = graph.insert_string("not_the_exported_memory");
            graph.boundary_memory.export_name = Some(memory);
        });

        let error = match WasmRuntime::new(&bytes, RunConfig::without_wasi()) {
            Ok(_) => panic!("invalid boundary memory should be rejected"),
            Err(error) => error,
        };
        assert!(error.to_string().contains("boundary memory export"));
    }

    #[test]
    fn describe_module_abi_v2_rejects_mismatched_raw_export_records() {
        let bytes = compile_fixture(
            r#"
export fun answer(): int {
    42
}
"#,
        );
        let bytes = rewrite_v2_metadata(&bytes, |graph| {
            let bogus = graph.insert_string("missing:v2/export");
            graph.raw_exports.push(mitki_abi::RawExportRecord {
                name: bogus,
                symbol: None,
                signature: None,
            });
        });

        let error = describe_module_abi_v2(&bytes).expect_err("bogus raw export should fail");
        assert!(error.to_string().contains("raw export"));
    }

    #[test]
    fn tuple_blob_encoding_preserves_all_fields() {
        let bytes = compile_fixture(
            r#"
export fun make_pair(): (int, int) {
    (20, 22)
}
"#,
        );
        let mut runtime = WasmRuntime::new(&bytes, RunConfig::without_wasi())
            .expect("runtime should instantiate");
        let metadata = runtime.abi_v2.metadata.clone().expect("expected v2 metadata");
        let instance = find_export_instance(&metadata, "make_pair").expect("make_pair export");
        let typed_export_name =
            string_value(&metadata, instance.wasm_field_name.expect("typed export linkage"))
                .expect("typed export name")
                .to_owned();
        let mut results = vec![wasmtime::Val::I32(0)];
        runtime
            .call_dynamic_export(&typed_export_name, &[], &mut results)
            .expect("typed export should execute");
        let ptr = results[0].i32().expect("typed export should return i32");
        let blob = runtime.read_canonical_blob(ptr).expect("blob should read");
        let value = decode_canonical_blob(&blob).expect("blob should decode");
        let AbiValue::Canonical { graph, .. } = value else {
            panic!("expected canonical tuple result");
        };
        let ValueRef::NodeRef(root) = graph.root else {
            panic!("expected tuple node root");
        };
        let CanonicalNode::Tuple { fields, .. } =
            graph.nodes.get(root.0 as usize).expect("tuple root node")
        else {
            panic!("expected tuple root");
        };
        assert_eq!(
            fields,
            &[
                ValueRef::InlineScalar(AbiScalar::Int { signed: true, bits: 32, value: 20 }),
                ValueRef::InlineScalar(AbiScalar::Int { signed: true, bits: 32, value: 22 }),
            ]
        );
    }

    #[test]
    fn capability_handles_round_trip_through_runtime_facade() {
        let (graph, transport) = opaque_handle_transport();
        let value = AbiValue::Handle { type_id: transport.semantic_type, handle_id: 77 };

        let lowered =
            abi_value_to_wasm_value(&graph, &transport, &value).expect("handle should lower");
        assert_eq!(
            lowered,
            WasmValue::Handle { type_id: transport.semantic_type.0, handle_id: 77 }
        );

        let lifted =
            wasm_value_to_abi_value(&graph, &transport, &lowered).expect("handle should lift");
        assert_eq!(lifted, value);
    }

    #[test]
    fn canonical_facade_preserves_nested_handle_slots() {
        let (graph, transport, handle_ty) = struct_with_handle_transport();
        let value = WasmValue::Struct(vec![(
            "callback".to_owned(),
            WasmValue::Handle { type_id: handle_ty.0, handle_id: 91 },
        )]);

        let abi = wasm_value_to_abi_value(&graph, &transport, &value)
            .expect("canonical value should preserve nested handles");
        let AbiValue::Canonical { graph: canonical, .. } = &abi else {
            panic!("expected canonical value");
        };
        assert_eq!(
            canonical.handles,
            vec![mitki_abi::HandleSlot { type_id: handle_ty, handle_id: 91 }]
        );
        let decoded = abi_value_to_wasm_value(&graph, &transport, &abi)
            .expect("canonical value should decode");
        assert_eq!(decoded, value);
    }

    #[test]
    fn canonical_facade_round_trips_union_nodes() {
        let (graph, transport) = union_transport();
        let value =
            WasmValue::Union { arm_index: 1, value: Box::new(WasmValue::String("hi".to_owned())) };

        let abi =
            wasm_value_to_abi_value(&graph, &transport, &value).expect("union should canonicalize");
        let decoded =
            abi_value_to_wasm_value(&graph, &transport, &abi).expect("union should decode");
        assert_eq!(decoded, value);
    }

    #[test]
    fn canonical_facade_round_trips_intersection_nodes() {
        let (graph, transport, facet_ty) = intersection_transport();
        let value = WasmValue::Intersection {
            carrier: Box::new(WasmValue::Record(vec![("value".to_owned(), WasmValue::I32(7))])),
            facets: vec![WasmValue::Handle { type_id: facet_ty.0, handle_id: 12 }],
        };

        let abi = wasm_value_to_abi_value(&graph, &transport, &value)
            .expect("intersection should canonicalize");
        let AbiValue::Canonical { transport_type, .. } = &abi else {
            panic!("expected canonical intersection value");
        };
        assert_eq!(*transport_type, transport.transport_type.expect("carrier type"));
        let decoded =
            abi_value_to_wasm_value(&graph, &transport, &abi).expect("intersection should decode");
        assert_eq!(decoded, value);
    }
}

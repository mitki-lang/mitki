use std::collections::{BTreeMap, BTreeSet};

use mitki_abi::{AbiTypeKind, SemanticTypeGraph, SigId, TransportClass, TransportRef, TypeId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::{enum_variants, struct_fields};
use serde::{Deserialize, Serialize};

const ARC_ALIGN: u32 = 8;
const ARC_HEADER_SIZE: u32 = 8;
const ARRAY_HEADER_SIZE: u32 = 8;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiFunctionAbi {
    pub params: Vec<MitkiParamAbi>,
    pub result: MitkiResultAbi,
    #[serde(skip)]
    pub type_values: BTreeMap<TypeId, MitkiValueAbi>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiParamAbi {
    pub name: Option<String>,
    pub passing: MitkiPassingAbi,
    pub value: MitkiValueAbi,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiResultAbi {
    pub passing: MitkiPassingAbi,
    pub value: MitkiValueAbi,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MitkiPassingAbi {
    Unit,
    I32,
    F64,
    Pointer,
    OutPointer,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiValueAbi {
    pub kind: MitkiValueKind,
    pub boundary: MitkiLoweringAbi,
    pub runtime: MitkiLoweringAbi,
    pub pointee: Option<MitkiPointeeAbi>,
    #[serde(skip)]
    pub runtime_member_order: Option<Vec<TypeId>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MitkiValueKind {
    Unit,
    Int,
    Bool,
    Float,
    Char,
    Function,
    Opaque,
    String,
    Array,
    Tuple,
    Record,
    Struct,
    Enum,
    Union,
    Intersection,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "kind", content = "layout")]
pub enum MitkiLoweringAbi {
    Unit,
    I32,
    F64,
    Pointer,
    Aggregate(MitkiAggregateLayoutAbi),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiAggregateLayoutAbi {
    pub size: u32,
    pub align: u32,
    pub kind: MitkiAggregateKindAbi,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "kind", content = "layout")]
pub enum MitkiAggregateKindAbi {
    Fields(Vec<MitkiFieldAbi>),
    Enum(MitkiEnumLayoutAbi),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiFieldAbi {
    pub name: Option<String>,
    pub offset: u32,
    pub value: Box<MitkiValueAbi>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiEnumLayoutAbi {
    pub payload_offset: u32,
    pub variants: Vec<MitkiVariantAbi>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiVariantAbi {
    pub name: String,
    pub tag: i32,
    pub fields: Vec<MitkiFieldAbi>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "kind", content = "layout")]
pub enum MitkiPointeeAbi {
    String,
    Array(MitkiArrayLayoutAbi),
    Aggregate(MitkiAggregateLayoutAbi),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct MitkiArrayLayoutAbi {
    pub object_align: u32,
    pub item_size: u32,
    pub item_align: u32,
    pub item_stride: u32,
    pub data_offset: u32,
    pub item: Box<MitkiValueAbi>,
}

#[derive(Clone, Copy)]
pub enum LoweringMode {
    Boundary,
    Runtime,
}

impl MitkiFunctionAbi {
    pub fn from_v2_signature(
        graph: &SemanticTypeGraph,
        signature_id: SigId,
    ) -> Result<Self, String> {
        let signature = graph
            .signatures
            .get(signature_id.0 as usize)
            .ok_or_else(|| format!("unknown ABI v2 signature `{}`", signature_id.0))?;
        let mut builder = V2AbiBuilder::new(graph);
        let params = signature
            .params
            .iter()
            .map(|transport| {
                let passing = v2_passing(graph, transport)?;
                let value = builder.value_abi(builder.transport_value_type(transport)?)?;
                Ok(MitkiParamAbi { name: None, passing, value })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let result_passing = v2_passing(graph, &signature.result)?;
        let result_value = builder.value_abi(builder.transport_value_type(&signature.result)?)?;
        Ok(Self {
            params,
            result: MitkiResultAbi { passing: result_passing, value: result_value },
            type_values: builder.cache.clone(),
        })
    }

    pub fn from_v2_signature_with_runtime_types<'db>(
        db: &'db dyn salsa::Database,
        graph: &'db SemanticTypeGraph,
        signature_id: SigId,
        param_runtime_tys: &[Ty<'db>],
        result_runtime_ty: Ty<'db>,
    ) -> Result<Self, String> {
        let signature = graph
            .signatures
            .get(signature_id.0 as usize)
            .ok_or_else(|| format!("unknown ABI v2 signature `{}`", signature_id.0))?;
        if signature.params.len() != param_runtime_tys.len() {
            return Err(format!(
                "internal error: ABI v2 signature `{}` expected {} runtime param type(s), found {}",
                signature_id.0,
                signature.params.len(),
                param_runtime_tys.len()
            ));
        }
        let mut abi = Self::from_v2_signature(graph, signature_id)?;
        for ((transport, runtime_ty), param) in signature
            .params
            .iter()
            .zip(param_runtime_tys.iter().copied())
            .zip(abi.params.iter_mut())
        {
            apply_top_level_runtime_ty(
                db,
                graph,
                transport.semantic_type,
                runtime_ty,
                &mut param.value,
            )?;
        }
        apply_top_level_runtime_ty(
            db,
            graph,
            signature.result.semantic_type,
            result_runtime_ty,
            &mut abi.result.value,
        )?;
        Ok(abi)
    }
}

impl MitkiValueAbi {
    pub fn lowering(&self, mode: LoweringMode) -> &MitkiLoweringAbi {
        match mode {
            LoweringMode::Boundary => &self.boundary,
            LoweringMode::Runtime => &self.runtime,
        }
    }
}

fn v2_passing(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
) -> Result<MitkiPassingAbi, String> {
    ensure_runtime_core_transport(graph, transport)?;
    match transport.transport_class {
        TransportClass::Immediate => match type_kind(graph, transport_passing_type(transport))? {
            AbiTypeKind::Unit => Ok(MitkiPassingAbi::Unit),
            AbiTypeKind::Tuple { elems } if elems.is_empty() => Ok(MitkiPassingAbi::Unit),
            AbiTypeKind::Bool
            | AbiTypeKind::Int { .. }
            | AbiTypeKind::Char
            | AbiTypeKind::Enum { .. } => Ok(MitkiPassingAbi::I32),
            AbiTypeKind::Float { .. } => Ok(MitkiPassingAbi::F64),
            other => {
                Err(format!("internal error: immediate ABI v2 transport cannot lower `{other:?}`"))
            }
        },
        TransportClass::CanonicalValue => Ok(MitkiPassingAbi::Pointer),
        TransportClass::CapabilityHandle => Ok(MitkiPassingAbi::I32),
    }
}

fn transport_passing_type(transport: &TransportRef) -> TypeId {
    transport.transport_type.unwrap_or(transport.semantic_type)
}

fn ensure_runtime_core_transport(
    graph: &SemanticTypeGraph,
    transport: &TransportRef,
) -> Result<(), String> {
    let mut stack = vec![transport.semantic_type];
    if let Some(transport_type) = transport.transport_type {
        stack.push(transport_type);
    }
    let mut seen = BTreeSet::new();
    while let Some(ty) = stack.pop() {
        if !seen.insert(ty) {
            continue;
        }
        let node = graph
            .types
            .get(ty.0 as usize)
            .ok_or_else(|| format!("unknown ABI v2 type `{}`", ty.0))?;
        match &node.kind {
            AbiTypeKind::Function { .. } | AbiTypeKind::Opaque { .. } => {}
            _ => stack.extend(type_edges(&node.kind)),
        }
    }
    Ok(())
}

fn type_kind(graph: &SemanticTypeGraph, ty: TypeId) -> Result<&AbiTypeKind, String> {
    graph
        .types
        .get(ty.0 as usize)
        .map(|node| &node.kind)
        .ok_or_else(|| format!("unknown ABI v2 type `{}`", ty.0))
}

fn type_edges(kind: &AbiTypeKind) -> Vec<TypeId> {
    match kind {
        AbiTypeKind::Array { elem } => vec![*elem],
        AbiTypeKind::Tuple { elems } => elems.clone(),
        AbiTypeKind::Record { fields } | AbiTypeKind::Struct { fields, .. } => {
            fields.iter().map(|field| field.ty).collect()
        }
        AbiTypeKind::Enum { variants, .. } => {
            variants.iter().flat_map(|variant| variant.fields.iter().copied()).collect()
        }
        AbiTypeKind::Union { members } => members.clone(),
        AbiTypeKind::Intersection { members, carrier, .. } => {
            let mut edges = members.clone();
            edges.push(*carrier);
            edges
        }
        AbiTypeKind::Function { params, result, .. } => {
            let mut edges = params.clone();
            edges.push(*result);
            edges
        }
        AbiTypeKind::Unit
        | AbiTypeKind::Bool
        | AbiTypeKind::Int { .. }
        | AbiTypeKind::Float { .. }
        | AbiTypeKind::Char
        | AbiTypeKind::String
        | AbiTypeKind::Opaque { .. } => Vec::new(),
    }
}

struct V2AbiBuilder<'a> {
    graph: &'a SemanticTypeGraph,
    cache: BTreeMap<TypeId, MitkiValueAbi>,
    visiting: BTreeSet<TypeId>,
}

impl<'a> V2AbiBuilder<'a> {
    fn new(graph: &'a SemanticTypeGraph) -> Self {
        Self { graph, cache: BTreeMap::new(), visiting: BTreeSet::new() }
    }

    fn value_abi(&mut self, ty: TypeId) -> Result<MitkiValueAbi, String> {
        if let Some(value) = self.cache.get(&ty) {
            return Ok(value.clone());
        }
        if !self.visiting.insert(ty) {
            return self.recursive_placeholder(ty);
        }

        let kind = self.type_kind(ty)?.clone();
        let value = match kind {
            AbiTypeKind::Unit => MitkiValueAbi {
                kind: MitkiValueKind::Unit,
                boundary: MitkiLoweringAbi::Unit,
                runtime: MitkiLoweringAbi::Unit,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Int { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Int,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::I32,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Bool => MitkiValueAbi {
                kind: MitkiValueKind::Bool,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::I32,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Float { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Float,
                boundary: MitkiLoweringAbi::F64,
                runtime: MitkiLoweringAbi::F64,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Char => MitkiValueAbi {
                kind: MitkiValueKind::Char,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::I32,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Function { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Function,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::Aggregate(function_runtime_layout_abi()),
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Opaque { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Opaque,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::String => MitkiValueAbi {
                kind: MitkiValueKind::String,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: Some(MitkiPointeeAbi::String),
                runtime_member_order: None,
            },
            AbiTypeKind::Array { elem } => MitkiValueAbi {
                kind: MitkiValueKind::Array,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: Some(MitkiPointeeAbi::Array(self.array_layout(elem)?)),
                runtime_member_order: None,
            },
            AbiTypeKind::Tuple { elems } if elems.is_empty() => MitkiValueAbi {
                kind: MitkiValueKind::Unit,
                boundary: MitkiLoweringAbi::Unit,
                runtime: MitkiLoweringAbi::Unit,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Tuple { elems } => MitkiValueAbi {
                kind: MitkiValueKind::Tuple,
                boundary: MitkiLoweringAbi::Aggregate(self.fields_layout(
                    elems.iter().copied().map(|field| (None, field)),
                    LoweringMode::Boundary,
                )?),
                runtime: MitkiLoweringAbi::Aggregate(self.fields_layout(
                    elems.iter().copied().map(|field| (None, field)),
                    LoweringMode::Runtime,
                )?),
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Record { fields } => MitkiValueAbi {
                kind: MitkiValueKind::Record,
                boundary: MitkiLoweringAbi::Aggregate(
                    self.fields_layout(self.named_fields(&fields)?, LoweringMode::Boundary)?,
                ),
                runtime: MitkiLoweringAbi::Aggregate(
                    self.fields_layout(self.named_fields(&fields)?, LoweringMode::Runtime)?,
                ),
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Struct { fields, .. } => {
                let runtime_layout =
                    self.fields_layout(self.named_fields(&fields)?, LoweringMode::Runtime)?;
                MitkiValueAbi {
                    kind: MitkiValueKind::Struct,
                    boundary: MitkiLoweringAbi::Aggregate(
                        self.fields_layout(self.named_fields(&fields)?, LoweringMode::Boundary)?,
                    ),
                    runtime: MitkiLoweringAbi::Pointer,
                    pointee: Some(MitkiPointeeAbi::Aggregate(runtime_layout)),
                    runtime_member_order: None,
                }
            }
            AbiTypeKind::Enum { variants, .. } => {
                let runtime_layout = self.enum_layout(&variants, LoweringMode::Runtime)?;
                MitkiValueAbi {
                    kind: MitkiValueKind::Enum,
                    boundary: MitkiLoweringAbi::Aggregate(
                        self.enum_layout(&variants, LoweringMode::Boundary)?,
                    ),
                    runtime: MitkiLoweringAbi::Pointer,
                    pointee: Some(MitkiPointeeAbi::Aggregate(runtime_layout)),
                    runtime_member_order: None,
                }
            }
            AbiTypeKind::Union { members } => MitkiValueAbi {
                kind: MitkiValueKind::Union,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Aggregate(
                    self.union_layout(&members, LoweringMode::Runtime)?,
                ),
                pointee: None,
                runtime_member_order: Some(members),
            },
            AbiTypeKind::Intersection { carrier, facet_plan, .. } => {
                let live_members = intersection_live_members(self.graph, facet_plan)?;
                if live_members.is_empty() {
                    self.value_abi(carrier)?
                } else {
                    let mut runtime_members = Vec::with_capacity(1 + live_members.len());
                    runtime_members.push(carrier);
                    runtime_members.extend(live_members.iter().copied());
                    MitkiValueAbi {
                        kind: MitkiValueKind::Intersection,
                        boundary: MitkiLoweringAbi::Aggregate(self.fields_layout(
                            runtime_members.iter().copied().map(|member| (None, member)),
                            LoweringMode::Boundary,
                        )?),
                        runtime: MitkiLoweringAbi::Aggregate(self.fields_layout(
                            runtime_members.iter().copied().map(|member| (None, member)),
                            LoweringMode::Runtime,
                        )?),
                        pointee: None,
                        runtime_member_order: Some(runtime_members),
                    }
                }
            }
        };

        self.visiting.remove(&ty);
        self.cache.insert(ty, value.clone());
        Ok(value)
    }

    fn type_kind(&self, ty: TypeId) -> Result<&AbiTypeKind, String> {
        self.graph
            .types
            .get(ty.0 as usize)
            .map(|node| &node.kind)
            .ok_or_else(|| format!("unknown ABI v2 type `{}`", ty.0))
    }

    fn transport_value_type(&self, transport: &TransportRef) -> Result<TypeId, String> {
        if let Some(transport_type) = transport.transport_type
            && (!matches!(transport.transport_class, TransportClass::CanonicalValue)
                || intersection_reuses_carrier_transport(self.graph, transport.semantic_type)?)
        {
            return Ok(transport_type);
        }
        Ok(transport.semantic_type)
    }

    fn recursive_placeholder(&self, ty: TypeId) -> Result<MitkiValueAbi, String> {
        Ok(match self.type_kind(ty)? {
            AbiTypeKind::Unit => MitkiValueAbi {
                kind: MitkiValueKind::Unit,
                boundary: MitkiLoweringAbi::Unit,
                runtime: MitkiLoweringAbi::Unit,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Bool => MitkiValueAbi {
                kind: MitkiValueKind::Bool,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::I32,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Int { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Int,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::I32,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Float { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Float,
                boundary: MitkiLoweringAbi::F64,
                runtime: MitkiLoweringAbi::F64,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Char => MitkiValueAbi {
                kind: MitkiValueKind::Char,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::I32,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Function { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Function,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::Aggregate(function_runtime_layout_abi()),
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Opaque { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Opaque,
                boundary: MitkiLoweringAbi::I32,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::String => MitkiValueAbi {
                kind: MitkiValueKind::String,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: Some(MitkiPointeeAbi::String),
                runtime_member_order: None,
            },
            AbiTypeKind::Array { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Array,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Struct { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Struct,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Enum { .. } => MitkiValueAbi {
                kind: MitkiValueKind::Enum,
                boundary: MitkiLoweringAbi::Pointer,
                runtime: MitkiLoweringAbi::Pointer,
                pointee: None,
                runtime_member_order: None,
            },
            AbiTypeKind::Intersection { carrier, facet_plan, .. }
                if intersection_live_members(self.graph, *facet_plan)?.is_empty() =>
            {
                self.recursive_placeholder(*carrier)?
            }
            AbiTypeKind::Tuple { .. }
            | AbiTypeKind::Record { .. }
            | AbiTypeKind::Union { .. }
            | AbiTypeKind::Intersection { .. } => {
                return Err("recursive types are not supported yet".to_owned());
            }
        })
    }

    fn array_layout(&mut self, elem: TypeId) -> Result<MitkiArrayLayoutAbi, String> {
        let item = self.value_abi(elem)?;
        let (item_size, item_align) = lowering_size_align(item.lowering(LoweringMode::Runtime))?;
        let item_stride = align_to(item_size, item_align.max(1));
        let data_offset = align_to(ARC_HEADER_SIZE + ARRAY_HEADER_SIZE, item_align.max(1))
            .checked_sub(ARC_HEADER_SIZE)
            .ok_or_else(|| "array layout underflowed".to_owned())?;
        Ok(MitkiArrayLayoutAbi {
            object_align: ARC_ALIGN.max(item_align),
            item_size,
            item_align,
            item_stride,
            data_offset,
            item: Box::new(item),
        })
    }

    fn fields_layout<I>(
        &mut self,
        fields: I,
        mode: LoweringMode,
    ) -> Result<MitkiAggregateLayoutAbi, String>
    where
        I: IntoIterator<Item = (Option<String>, TypeId)>,
    {
        let mut abi_fields = Vec::new();
        let mut size = 0u32;
        let mut align = 1u32;

        for field in fields {
            let (name, ty) = field;
            let value = self.value_abi(ty)?;
            let (field_size, field_align) = lowering_size_align(value.lowering(mode))?;
            let offset = align_to(size, field_align.max(1));
            size = offset
                .checked_add(field_size)
                .ok_or_else(|| "aggregate layout overflowed".to_owned())?;
            align = align.max(field_align);
            abi_fields.push(MitkiFieldAbi { name, offset, value: Box::new(value) });
        }

        Ok(MitkiAggregateLayoutAbi {
            size: align_to(size, align.max(1)),
            align: align.max(1),
            kind: MitkiAggregateKindAbi::Fields(abi_fields),
        })
    }

    fn enum_layout(
        &mut self,
        variants: &[mitki_abi::EnumVariant],
        mode: LoweringMode,
    ) -> Result<MitkiAggregateLayoutAbi, String> {
        let mut variant_layouts = Vec::with_capacity(variants.len());
        let mut max_variant_align = 1u32;
        let mut max_variant_size = 0u32;

        for variant in variants {
            let fields_layout = self
                .fields_layout(variant.fields.iter().copied().map(|field| (None, field)), mode)?;
            let MitkiAggregateKindAbi::Fields(fields) = fields_layout.kind else {
                unreachable!();
            };
            max_variant_align = max_variant_align.max(fields_layout.align);
            max_variant_size = max_variant_size.max(fields_layout.size);
            variant_layouts.push((self.variant_name(variant.name)?.to_owned(), fields));
        }

        let payload_offset = align_to(4, max_variant_align.max(1));
        let align = 4u32.max(max_variant_align);
        let size = align_to(
            payload_offset
                .checked_add(max_variant_size)
                .ok_or_else(|| "enum layout overflowed".to_owned())?,
            align,
        );
        let abi_variants = variant_layouts
            .into_iter()
            .enumerate()
            .map(|(index, (name, fields))| {
                Ok(MitkiVariantAbi {
                    name,
                    tag: i32::try_from(index)
                        .map_err(|_error| "enum tag exceeded i32 range".to_owned())?,
                    fields: fields
                        .into_iter()
                        .map(|mut field| {
                            field.offset = field
                                .offset
                                .checked_add(payload_offset)
                                .ok_or_else(|| "enum field offset overflowed".to_owned())?;
                            Ok(field)
                        })
                        .collect::<Result<Vec<_>, String>>()?,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok(MitkiAggregateLayoutAbi {
            size,
            align,
            kind: MitkiAggregateKindAbi::Enum(MitkiEnumLayoutAbi {
                payload_offset,
                variants: abi_variants,
            }),
        })
    }

    fn union_layout(
        &mut self,
        members: &[TypeId],
        mode: LoweringMode,
    ) -> Result<MitkiAggregateLayoutAbi, String> {
        let mut variant_fields = Vec::with_capacity(members.len());
        let mut max_variant_align = 1u32;
        let mut max_variant_size = 0u32;

        for &member in members {
            let fields_layout = self.fields_layout(std::iter::once((None, member)), mode)?;
            let MitkiAggregateKindAbi::Fields(fields) = fields_layout.kind else {
                unreachable!();
            };
            max_variant_align = max_variant_align.max(fields_layout.align);
            max_variant_size = max_variant_size.max(fields_layout.size);
            variant_fields.push(fields);
        }

        let payload_offset = align_to(4, max_variant_align.max(1));
        let align = 4u32.max(max_variant_align);
        let size = align_to(
            payload_offset
                .checked_add(max_variant_size)
                .ok_or_else(|| "union layout overflowed".to_owned())?,
            align,
        );
        let variants = variant_fields
            .into_iter()
            .enumerate()
            .map(|(index, fields)| {
                Ok(MitkiVariantAbi {
                    name: format!("arm{index}"),
                    tag: i32::try_from(index)
                        .map_err(|_error| "union arm tag exceeded i32 range".to_owned())?,
                    fields: fields
                        .into_iter()
                        .map(|mut field| {
                            field.offset = field
                                .offset
                                .checked_add(payload_offset)
                                .ok_or_else(|| "union arm field offset overflowed".to_owned())?;
                            Ok(field)
                        })
                        .collect::<Result<Vec<_>, String>>()?,
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        Ok(MitkiAggregateLayoutAbi {
            size,
            align,
            kind: MitkiAggregateKindAbi::Enum(MitkiEnumLayoutAbi { payload_offset, variants }),
        })
    }

    fn named_fields(
        &self,
        fields: &[mitki_abi::RecordField],
    ) -> Result<Vec<(Option<String>, TypeId)>, String> {
        fields
            .iter()
            .map(|field| Ok((Some(self.field_name(field.name)?.to_owned()), field.ty)))
            .collect()
    }

    fn field_name(&self, id: mitki_abi::FieldNameId) -> Result<&str, String> {
        let string_id = *self
            .graph
            .field_names
            .get(id.0 as usize)
            .ok_or_else(|| format!("unknown ABI v2 field name `{}`", id.0))?;
        self.string_value(string_id)
    }

    fn variant_name(&self, id: mitki_abi::VariantNameId) -> Result<&str, String> {
        let string_id = *self
            .graph
            .variant_names
            .get(id.0 as usize)
            .ok_or_else(|| format!("unknown ABI v2 variant name `{}`", id.0))?;
        self.string_value(string_id)
    }

    fn string_value(&self, id: mitki_abi::StringId) -> Result<&str, String> {
        self.graph
            .strings
            .get(id.0 as usize)
            .map(String::as_str)
            .ok_or_else(|| format!("unknown ABI v2 string `{}`", id.0))
    }
}

fn apply_top_level_runtime_ty(
    db: &dyn salsa::Database,
    graph: &SemanticTypeGraph,
    semantic_type: TypeId,
    runtime_ty: Ty<'_>,
    value: &mut MitkiValueAbi,
) -> Result<(), String> {
    if value.kind != MitkiValueKind::Union && value.kind != MitkiValueKind::Intersection {
        return Ok(());
    }
    let mut builder = V2AbiBuilder::new(graph);
    match type_kind(graph, semantic_type)? {
        AbiTypeKind::Union { members } => {
            let runtime_member_order = runtime_union_member_order(db, graph, members, runtime_ty)?;
            value.runtime = MitkiLoweringAbi::Aggregate(
                builder.union_layout(&runtime_member_order, LoweringMode::Runtime)?,
            );
            value.runtime_member_order = Some(runtime_member_order);
        }
        AbiTypeKind::Intersection { carrier, facet_plan, .. } => {
            let runtime_member_order =
                runtime_intersection_member_order(db, graph, *carrier, *facet_plan, runtime_ty)?;
            if runtime_member_order.len() == 1 {
                *value = builder.value_abi(runtime_member_order[0])?;
            } else {
                value.runtime = MitkiLoweringAbi::Aggregate(builder.fields_layout(
                    runtime_member_order.iter().copied().map(|member| (None, member)),
                    LoweringMode::Runtime,
                )?);
                value.runtime_member_order = Some(runtime_member_order);
            }
        }
        _ => {}
    }
    Ok(())
}

fn runtime_union_member_order(
    db: &dyn salsa::Database,
    graph: &SemanticTypeGraph,
    members: &[TypeId],
    runtime_ty: Ty<'_>,
) -> Result<Vec<TypeId>, String> {
    let TyKind::Union(runtime_members) = runtime_ty.kind(db) else {
        return Ok(members.to_vec());
    };
    runtime_members
        .iter()
        .map(|&runtime_member| {
            members
                .iter()
                .copied()
                .find(|&semantic_member| {
                    semantic_type_matches_runtime_ty(db, graph, semantic_member, runtime_member)
                })
                .ok_or_else(|| {
                    format!(
                        "internal error: union runtime member `{}` did not match semantic ABI \
                         members",
                        runtime_member.display(db)
                    )
                })
        })
        .collect()
}

fn semantic_type_matches_runtime_ty(
    db: &dyn salsa::Database,
    graph: &SemanticTypeGraph,
    semantic_type: TypeId,
    runtime_ty: Ty<'_>,
) -> bool {
    let Ok(kind) = type_kind(graph, semantic_type) else {
        return false;
    };
    match (kind, runtime_ty.kind(db)) {
        (AbiTypeKind::Unit, TyKind::Tuple(items)) => items.is_empty(),
        (AbiTypeKind::Bool, TyKind::Bool)
        | (AbiTypeKind::Char, TyKind::Char)
        | (AbiTypeKind::String, TyKind::String) => true,
        (AbiTypeKind::Int { signed, bits }, TyKind::Int) => *signed && *bits == 32,
        (AbiTypeKind::Float { bits }, TyKind::Float) => *bits == 64,
        (AbiTypeKind::Array { elem }, TyKind::Array(runtime_elem)) => {
            semantic_type_matches_runtime_ty(db, graph, *elem, *runtime_elem)
        }
        (AbiTypeKind::Tuple { elems }, TyKind::Tuple(runtime_items)) => {
            elems.len() == runtime_items.len()
                && elems.iter().zip(runtime_items.iter()).all(|(&semantic_elem, &runtime_elem)| {
                    semantic_type_matches_runtime_ty(db, graph, semantic_elem, runtime_elem)
                })
        }
        (AbiTypeKind::Record { fields }, TyKind::Record(runtime_fields)) => {
            let mut runtime_fields = runtime_fields.clone();
            runtime_fields.sort_by_key(|(name, _)| name.text(db).to_owned());
            fields.len() == runtime_fields.len()
                && fields.iter().zip(runtime_fields.iter()).all(
                    |(semantic_field, (runtime_name, runtime_field_ty))| {
                        field_name_matches(graph, semantic_field.name, runtime_name.text(db))
                            && semantic_type_matches_runtime_ty(
                                db,
                                graph,
                                semantic_field.ty,
                                *runtime_field_ty,
                            )
                    },
                )
        }
        (AbiTypeKind::Struct { nominal, fields }, TyKind::Struct(struct_ty)) => {
            symbol_matches(graph, *nominal, struct_ty.name(db).text(db))
                && fields.len() == struct_fields(db, *struct_ty).len()
                && fields.iter().zip(struct_fields(db, *struct_ty).iter()).all(
                    |(semantic_field, (runtime_name, runtime_field_ty))| {
                        field_name_matches(graph, semantic_field.name, runtime_name.text(db))
                            && semantic_type_matches_runtime_ty(
                                db,
                                graph,
                                semantic_field.ty,
                                *runtime_field_ty,
                            )
                    },
                )
        }
        (AbiTypeKind::Enum { nominal, variants }, TyKind::Enum(enum_ty)) => {
            let runtime_variants = enum_variants(db, *enum_ty);
            symbol_matches(graph, *nominal, enum_ty.name(db).text(db))
                && variants.len() == runtime_variants.len()
                && variants.iter().zip(runtime_variants.iter()).all(
                    |(semantic_variant, (runtime_name, runtime_fields))| {
                        variant_name_matches(graph, semantic_variant.name, runtime_name.text(db))
                            && semantic_variant.fields.len() == runtime_fields.len()
                            && semantic_variant.fields.iter().zip(runtime_fields.iter()).all(
                                |(&semantic_field_ty, runtime_field_ty)| {
                                    semantic_type_matches_runtime_ty(
                                        db,
                                        graph,
                                        semantic_field_ty,
                                        *runtime_field_ty,
                                    )
                                },
                            )
                    },
                )
        }
        (AbiTypeKind::Union { members }, TyKind::Union(runtime_members)) => {
            members.len() == runtime_members.len()
                && members.iter().all(|&semantic_member| {
                    runtime_members.iter().any(|&runtime_member| {
                        semantic_type_matches_runtime_ty(db, graph, semantic_member, runtime_member)
                    })
                })
        }
        (AbiTypeKind::Function { params, result, .. }, TyKind::Function { inputs, output }) => {
            params.len() == inputs.len()
                && params.iter().zip(inputs.iter()).all(|(&semantic_param, &runtime_param)| {
                    semantic_type_matches_runtime_ty(db, graph, semantic_param, runtime_param)
                })
                && semantic_type_matches_runtime_ty(db, graph, *result, *output)
        }
        (AbiTypeKind::Intersection { carrier, facet_plan, .. }, TyKind::Inter(runtime_members)) => {
            let Ok(expected) =
                runtime_intersection_member_order(db, graph, *carrier, *facet_plan, runtime_ty)
            else {
                return false;
            };
            expected.len() == runtime_members.len()
                && expected.iter().zip(runtime_members.iter()).all(
                    |(&semantic_member, &runtime_member)| {
                        semantic_type_matches_runtime_ty(db, graph, semantic_member, runtime_member)
                    },
                )
        }
        _ => false,
    }
}

fn intersection_reuses_carrier_transport(
    graph: &SemanticTypeGraph,
    semantic_type: TypeId,
) -> Result<bool, String> {
    let AbiTypeKind::Intersection { facet_plan, .. } = type_kind(graph, semantic_type)? else {
        return Ok(false);
    };
    Ok(intersection_live_members(graph, *facet_plan)?.is_empty())
}

fn intersection_live_members(
    graph: &SemanticTypeGraph,
    plan_id: Option<mitki_abi::FacetPlanId>,
) -> Result<Vec<TypeId>, String> {
    let Some(plan_id) = plan_id else {
        return Ok(Vec::new());
    };
    let plan = graph
        .facet_plans
        .get(plan_id.0 as usize)
        .ok_or_else(|| format!("unknown ABI v2 facet plan `{}`", plan_id.0))?;
    Ok(plan
        .entries
        .iter()
        .filter(|entry| entry.kind != mitki_abi::FacetPlanEntryKind::Erased)
        .map(|entry| entry.member)
        .collect())
}

fn runtime_intersection_member_order(
    db: &dyn salsa::Database,
    graph: &SemanticTypeGraph,
    carrier: TypeId,
    facet_plan: Option<mitki_abi::FacetPlanId>,
    runtime_ty: Ty<'_>,
) -> Result<Vec<TypeId>, String> {
    let TyKind::Inter(runtime_members) = runtime_ty.kind(db) else {
        let mut members = vec![carrier];
        members.extend(intersection_live_members(graph, facet_plan)?);
        return Ok(members);
    };

    let mut members = Vec::with_capacity(runtime_members.len());
    for &runtime_member in runtime_members {
        let semantic_member = std::iter::once(carrier)
            .chain(intersection_live_members(graph, facet_plan)?)
            .find(|&semantic_member| {
                semantic_type_matches_runtime_ty(db, graph, semantic_member, runtime_member)
            })
            .ok_or_else(|| {
                format!(
                    "internal error: intersection runtime member `{}` did not match semantic ABI \
                     members",
                    runtime_member.display(db)
                )
            })?;
        members.push(semantic_member);
    }
    Ok(members)
}

fn field_name_matches(graph: &SemanticTypeGraph, id: mitki_abi::FieldNameId, name: &str) -> bool {
    graph
        .field_names
        .get(id.0 as usize)
        .and_then(|string_id| graph.strings.get(string_id.0 as usize))
        .is_some_and(|value| value == name)
}

fn variant_name_matches(
    graph: &SemanticTypeGraph,
    id: mitki_abi::VariantNameId,
    name: &str,
) -> bool {
    graph
        .variant_names
        .get(id.0 as usize)
        .and_then(|string_id| graph.strings.get(string_id.0 as usize))
        .is_some_and(|value| value == name)
}

fn symbol_matches(graph: &SemanticTypeGraph, id: mitki_abi::SymbolId, name: &str) -> bool {
    graph
        .nominal_symbols
        .get(id.0 as usize)
        .and_then(|string_id| graph.strings.get(string_id.0 as usize))
        .is_some_and(|value| value == name)
}

fn lowering_size_align(lowering: &MitkiLoweringAbi) -> Result<(u32, u32), String> {
    Ok(match lowering {
        MitkiLoweringAbi::Unit => (0, 1),
        MitkiLoweringAbi::I32 | MitkiLoweringAbi::Pointer => (4, 4),
        MitkiLoweringAbi::F64 => (8, 8),
        MitkiLoweringAbi::Aggregate(layout) => (layout.size, layout.align),
    })
}

fn align_to(offset: u32, align: u32) -> u32 {
    let align = align.max(1);
    let mask = align - 1;
    (offset + mask) & !mask
}

fn function_value_layout() -> MitkiAggregateLayoutAbi {
    MitkiAggregateLayoutAbi { size: 8, align: 4, kind: MitkiAggregateKindAbi::Fields(Vec::new()) }
}

fn function_runtime_layout_abi() -> MitkiAggregateLayoutAbi {
    function_value_layout()
}

#[cfg(test)]
mod tests {
    use mitki_abi::{
        AbiTypeKind, CapabilityId, ExecutionDomain, FunctionSignature, SemanticTypeGraph, SigId,
        TransportClass, TransportRef,
    };

    use super::{
        MitkiFunctionAbi, MitkiLoweringAbi, MitkiPassingAbi, MitkiValueKind,
        function_runtime_layout_abi,
    };

    #[test]
    fn from_v2_signature_uses_handle_lanes_for_function_values() {
        let mut graph = SemanticTypeGraph::default();
        let int_ty = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let function_ty = graph.push_type(AbiTypeKind::Function {
            params: Vec::new(),
            result: int_ty,
            domain: ExecutionDomain::Runtime,
        });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type: function_ty,
            params: vec![TransportRef {
                semantic_type: function_ty,
                transport_class: TransportClass::CapabilityHandle,
                transport_type: None,
            }],
            result: TransportRef {
                semantic_type: function_ty,
                transport_class: TransportClass::CapabilityHandle,
                transport_type: None,
            },
        });

        let abi = MitkiFunctionAbi::from_v2_signature(&graph, SigId(0))
            .expect("function handles should lower through ABI v2 signatures");

        assert_eq!(abi.params[0].passing, MitkiPassingAbi::I32);
        assert_eq!(abi.result.passing, MitkiPassingAbi::I32);
        assert_eq!(abi.params[0].value.kind, MitkiValueKind::Function);
        assert_eq!(abi.params[0].value.boundary, MitkiLoweringAbi::I32);
        assert_eq!(
            abi.params[0].value.runtime,
            MitkiLoweringAbi::Aggregate(function_runtime_layout_abi())
        );
    }

    #[test]
    fn from_v2_signature_uses_handle_lanes_for_opaque_values() {
        let mut graph = SemanticTypeGraph::default();
        let opaque_ty = graph.push_type(AbiTypeKind::Opaque { capability_id: CapabilityId(0) });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type: opaque_ty,
            params: vec![TransportRef {
                semantic_type: opaque_ty,
                transport_class: TransportClass::CapabilityHandle,
                transport_type: None,
            }],
            result: TransportRef {
                semantic_type: opaque_ty,
                transport_class: TransportClass::CapabilityHandle,
                transport_type: None,
            },
        });

        let abi = MitkiFunctionAbi::from_v2_signature(&graph, SigId(0))
            .expect("opaque handles should lower through ABI v2 signatures");

        assert_eq!(abi.params[0].passing, MitkiPassingAbi::I32);
        assert_eq!(abi.result.passing, MitkiPassingAbi::I32);
        assert_eq!(abi.params[0].value.kind, MitkiValueKind::Opaque);
        assert_eq!(abi.params[0].value.boundary, MitkiLoweringAbi::I32);
        assert_eq!(abi.params[0].value.runtime, MitkiLoweringAbi::Pointer);
    }

    #[test]
    fn from_v2_signature_uses_canonical_pointers_for_union_values() {
        let mut graph = SemanticTypeGraph::default();
        let int_ty = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let string_ty = graph.push_type(AbiTypeKind::String);
        let union_ty = graph.push_type(AbiTypeKind::Union { members: vec![int_ty, string_ty] });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type: union_ty,
            params: vec![TransportRef {
                semantic_type: union_ty,
                transport_class: TransportClass::CanonicalValue,
                transport_type: None,
            }],
            result: TransportRef {
                semantic_type: union_ty,
                transport_class: TransportClass::CanonicalValue,
                transport_type: None,
            },
        });

        let abi = MitkiFunctionAbi::from_v2_signature(&graph, SigId(0))
            .expect("union values should lower through ABI v2 signatures");

        assert_eq!(abi.params[0].passing, MitkiPassingAbi::Pointer);
        assert_eq!(abi.result.passing, MitkiPassingAbi::Pointer);
        assert_eq!(abi.params[0].value.kind, MitkiValueKind::Union);
        assert_eq!(abi.params[0].value.boundary, MitkiLoweringAbi::Pointer);
        assert!(matches!(abi.params[0].value.runtime, MitkiLoweringAbi::Aggregate(_)));
    }
}

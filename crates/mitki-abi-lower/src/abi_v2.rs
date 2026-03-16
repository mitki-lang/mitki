use std::collections::BTreeMap;

use mitki_abi::{
    AbiTypeKind, BoundaryMemory, EnumVariant, ExecutionDomain, FacetPlan, FacetPlanEntry,
    FacetPlanEntryKind, FacetPlanId, FieldNameId, FunctionInstance,
    FunctionSignature as AbiFunctionSignature, GenericOrigin, GenericOriginId, InstanceId,
    LinkageKind, RecordField, SemanticTypeGraph, SigId, StringId, SymbolId, TransportClass,
    TransportRef, TypeFingerprint, TypeId, TypeNode, VariantNameId, WASM_CORE_V2_M32_PROFILE,
    WASM_CORE_V2_M32_PROFILE_MAJOR, WASM_CORE_V2_M32_PROFILE_MINOR, encode_semantic_type_graph,
    validate_graph_schema,
};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::{enum_variants, struct_fields};
use salsa::plumbing::AsId as _;

pub const MITKI_ABI_V2_CUSTOM_SECTION: &str = "mitki.abi.v2";
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundaryFunctionMetadata<'db> {
    pub logical_name: String,
    pub generic_origin_name: Option<String>,
    pub wasm_module_name: Option<String>,
    pub wasm_field_name: String,
    pub param_tys: Vec<Ty<'db>>,
    pub result_ty: Ty<'db>,
    pub type_args: Vec<Ty<'db>>,
    pub domain: ExecutionDomain,
    pub linkage: LinkageKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltFunctionInstance {
    pub signature_id: SigId,
    pub symbol_id: SymbolId,
    pub wasm_field_name: String,
    pub linkage: LinkageKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuiltAbiV2 {
    pub graph: SemanticTypeGraph,
    pub functions: Vec<BuiltFunctionInstance>,
}

pub fn build_module_abi_v2<'db>(
    db: &'db dyn salsa::Database,
    functions: &[BoundaryFunctionMetadata<'db>],
) -> Result<BuiltAbiV2, String> {
    let mut builder = AbiGraphBuilder::new(db);
    let mut built_functions = Vec::with_capacity(functions.len());

    for function in functions {
        let symbol_id = builder.intern_symbol(&function.logical_name);
        let function_type = builder.intern_function_type(
            &function.param_tys,
            function.result_ty,
            function.domain,
        )?;
        let signature_id = SigId(builder.graph.signatures.len() as u32);
        let params = function
            .param_tys
            .iter()
            .map(|&ty| builder.transport_ref(ty))
            .collect::<Result<Vec<_>, _>>()?;
        let result = builder.transport_ref(function.result_ty)?;
        builder.graph.signatures.push(AbiFunctionSignature {
            id: signature_id,
            function_type,
            params,
            result,
        });

        let wasm_module_name =
            function.wasm_module_name.as_ref().map(|name| builder.intern_string(name));
        let wasm_field_name = builder.intern_string(&function.wasm_field_name);
        let generic_origin =
            function.generic_origin_name.as_deref().map(|name| builder.intern_generic_origin(name));
        let type_args = function
            .type_args
            .iter()
            .map(|&ty| builder.intern_type(ty, None))
            .collect::<Result<Vec<_>, _>>()?;
        let instance_id = InstanceId(builder.graph.function_instances.len() as u32);
        builder.graph.function_instances.push(FunctionInstance {
            id: instance_id,
            logical_symbol: symbol_id,
            generic_origin,
            type_args,
            signature: signature_id,
            domain: function.domain,
            linkage: function.linkage,
            wasm_module_name,
            wasm_field_name: Some(wasm_field_name),
        });
        built_functions.push(BuiltFunctionInstance {
            signature_id,
            symbol_id,
            wasm_field_name: function.wasm_field_name.clone(),
            linkage: function.linkage,
        });
    }

    builder.graph.populate_recursive_groups();
    builder.graph.populate_fingerprints().map_err(|error| error.to_string())?;
    builder.graph.normalize_commutative_members().map_err(|error| error.to_string())?;
    builder.graph.populate_fingerprints().map_err(|error| error.to_string())?;
    finalize_intersections(&mut builder.graph)?;
    builder.graph.populate_fingerprints().map_err(|error| error.to_string())?;
    builder.graph.recompute_required_features();
    validate_graph_schema(&builder.graph).map_err(|error| error.to_string())?;
    Ok(BuiltAbiV2 { graph: builder.graph, functions: built_functions })
}

pub fn encode_module_abi_v2(graph: &SemanticTypeGraph) -> anyhow::Result<Vec<u8>> {
    encode_semantic_type_graph(graph)
}

struct AbiGraphBuilder<'db> {
    db: &'db dyn salsa::Database,
    graph: SemanticTypeGraph,
    strings: BTreeMap<String, StringId>,
    field_names: BTreeMap<String, FieldNameId>,
    variant_names: BTreeMap<String, VariantNameId>,
    symbols: BTreeMap<String, SymbolId>,
    generic_origins: BTreeMap<String, GenericOriginId>,
    types: BTreeMap<(u32, u8), TypeId>,
}

impl<'db> AbiGraphBuilder<'db> {
    fn new(db: &'db dyn salsa::Database) -> Self {
        let mut graph = SemanticTypeGraph::default();
        let mut strings = BTreeMap::new();
        let profile = StringId(0);
        strings.insert(WASM_CORE_V2_M32_PROFILE.to_owned(), profile);
        graph.strings.push(WASM_CORE_V2_M32_PROFILE.to_owned());
        graph.transport_profile = profile;
        graph.profile_version_major = WASM_CORE_V2_M32_PROFILE_MAJOR;
        graph.profile_version_minor = WASM_CORE_V2_M32_PROFILE_MINOR;
        let memory = StringId(1);
        strings.insert("memory".to_owned(), memory);
        graph.strings.push("memory".to_owned());
        graph.boundary_memory = BoundaryMemory { memory_index: 0, export_name: Some(memory) };
        Self {
            db,
            graph,
            strings,
            field_names: BTreeMap::new(),
            variant_names: BTreeMap::new(),
            symbols: BTreeMap::new(),
            generic_origins: BTreeMap::new(),
            types: BTreeMap::new(),
        }
    }

    fn intern_string(&mut self, value: &str) -> StringId {
        if let Some(&id) = self.strings.get(value) {
            return id;
        }
        let id = StringId(self.graph.strings.len() as u32);
        self.graph.strings.push(value.to_owned());
        self.strings.insert(value.to_owned(), id);
        id
    }

    fn intern_field_name(&mut self, value: &str) -> FieldNameId {
        if let Some(&id) = self.field_names.get(value) {
            return id;
        }
        let string_id = self.intern_string(value);
        let id = FieldNameId(self.graph.field_names.len() as u32);
        self.graph.field_names.push(string_id);
        self.field_names.insert(value.to_owned(), id);
        id
    }

    fn intern_variant_name(&mut self, value: &str) -> VariantNameId {
        if let Some(&id) = self.variant_names.get(value) {
            return id;
        }
        let string_id = self.intern_string(value);
        let id = VariantNameId(self.graph.variant_names.len() as u32);
        self.graph.variant_names.push(string_id);
        self.variant_names.insert(value.to_owned(), id);
        id
    }

    fn intern_symbol(&mut self, value: &str) -> SymbolId {
        if let Some(&id) = self.symbols.get(value) {
            return id;
        }
        let string_id = self.intern_string(value);
        let id = SymbolId(self.graph.nominal_symbols.len() as u32);
        self.graph.nominal_symbols.push(string_id);
        self.symbols.insert(value.to_owned(), id);
        id
    }

    fn intern_generic_origin(&mut self, value: &str) -> GenericOriginId {
        if let Some(&id) = self.generic_origins.get(value) {
            return id;
        }
        let symbol = self.intern_symbol(value);
        let id = GenericOriginId(self.graph.generic_origins.len() as u32);
        self.graph.generic_origins.push(GenericOrigin { id, symbol });
        self.generic_origins.insert(value.to_owned(), id);
        id
    }

    fn transport_ref(&mut self, ty: Ty<'db>) -> Result<TransportRef, String> {
        let semantic_type = self.intern_type(ty, None)?;
        Ok(TransportRef {
            semantic_type,
            transport_class: boundary_transport_class(self.db, ty),
            transport_type: None,
        })
    }

    fn type_key(&self, ty: Ty<'db>, domain: Option<ExecutionDomain>) -> (u32, u8) {
        let discriminator = if matches!(ty.kind(self.db), TyKind::Function { .. }) {
            domain.unwrap_or(ExecutionDomain::Both).as_bits()
        } else {
            0
        };
        (ty_bits(ty), discriminator)
    }

    fn intern_function_type(
        &mut self,
        params: &[Ty<'db>],
        result: Ty<'db>,
        domain: ExecutionDomain,
    ) -> Result<TypeId, String> {
        let function_ty =
            Ty::new(self.db, TyKind::Function { inputs: params.to_vec(), output: result });
        self.intern_type(function_ty, Some(domain))
    }

    fn intern_type(
        &mut self,
        ty: Ty<'db>,
        function_domain: Option<ExecutionDomain>,
    ) -> Result<TypeId, String> {
        self.intern_type_with_bindings(ty, function_domain, &mut BTreeMap::new())
    }

    fn intern_type_with_bindings(
        &mut self,
        ty: Ty<'db>,
        function_domain: Option<ExecutionDomain>,
        recursive_bindings: &mut BTreeMap<u32, TypeId>,
    ) -> Result<TypeId, String> {
        if let TyKind::Var(id) = ty.kind(self.db)
            && let Some(&bound) = recursive_bindings.get(id)
        {
            return Ok(bound);
        }
        if let TyKind::Rec(id, body) = ty.kind(self.db) {
            let key = self.type_key(ty, function_domain);
            if let Some(&existing) = self.types.get(&key) {
                return Ok(existing);
            }
            let type_id = TypeId(self.graph.types.len() as u32);
            let debug_name = self.intern_debug_name(&ty.display(self.db).to_string());
            self.graph.types.push(TypeNode {
                id: type_id,
                fingerprint: TypeFingerprint::ZERO,
                kind: AbiTypeKind::Unit,
                flags: 0,
                recursive_group: None,
                debug_name: Some(debug_name),
            });
            self.types.insert(key, type_id);
            let previous = recursive_bindings.insert(*id, type_id);
            let kind =
                self.lower_type_kind_with_bindings(*body, function_domain, recursive_bindings)?;
            if let Some(previous) = previous {
                recursive_bindings.insert(*id, previous);
            } else {
                recursive_bindings.remove(id);
            }
            self.graph.types[type_id.0 as usize].kind = kind;
            return Ok(type_id);
        }
        let key = self.type_key(ty, function_domain);
        if let Some(&id) = self.types.get(&key) {
            return Ok(id);
        }
        let id = TypeId(self.graph.types.len() as u32);
        let debug_name = self.intern_debug_name(&ty.display(self.db).to_string());
        self.graph.types.push(TypeNode {
            id,
            fingerprint: TypeFingerprint::ZERO,
            kind: AbiTypeKind::Unit,
            flags: 0,
            recursive_group: None,
            debug_name: Some(debug_name),
        });
        self.types.insert(key, id);
        let kind = self.lower_type_kind_with_bindings(ty, function_domain, recursive_bindings)?;
        self.graph.types[id.0 as usize].kind = kind;
        Ok(id)
    }

    fn intern_debug_name(&mut self, value: &str) -> mitki_abi::DebugNameId {
        let string_id = self.intern_string(value);
        let id = mitki_abi::DebugNameId(self.graph.debug_names.len() as u32);
        self.graph.debug_names.push(string_id);
        id
    }

    fn lower_type_kind_with_bindings(
        &mut self,
        ty: Ty<'db>,
        function_domain: Option<ExecutionDomain>,
        recursive_bindings: &mut BTreeMap<u32, TypeId>,
    ) -> Result<AbiTypeKind, String> {
        Ok(match ty.kind(self.db) {
            TyKind::Bool => AbiTypeKind::Bool,
            TyKind::Float => AbiTypeKind::Float { bits: 64 },
            TyKind::Int => AbiTypeKind::Int { signed: true, bits: 32 },
            TyKind::ExactInt(_) => {
                return Err(format!(
                    "cannot encode exact-width integer `{}` into ABI v2 metadata yet",
                    ty.display(self.db)
                ));
            }
            TyKind::Char => AbiTypeKind::Char,
            TyKind::String => AbiTypeKind::String,
            TyKind::Pointer { .. } => {
                return Err(format!(
                    "cannot encode raw pointer type `{}` into ABI v2 metadata",
                    ty.display(self.db)
                ));
            }
            TyKind::Array(elem) => AbiTypeKind::Array {
                elem: self.intern_type_with_bindings(*elem, None, recursive_bindings)?,
            },
            TyKind::Tuple(elems) => AbiTypeKind::Tuple {
                elems: elems
                    .iter()
                    .map(|&elem| self.intern_type_with_bindings(elem, None, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            TyKind::Record(fields) => {
                let mut ordered = fields.clone();
                ordered.sort_by_key(|(name, _)| name.text(self.db).to_owned());
                AbiTypeKind::Record {
                    fields: ordered
                        .iter()
                        .map(|(name, ty)| {
                            Ok(RecordField {
                                name: self.intern_field_name(name.text(self.db)),
                                ty: self.intern_type_with_bindings(
                                    *ty,
                                    None,
                                    recursive_bindings,
                                )?,
                            })
                        })
                        .collect::<Result<Vec<_>, String>>()?,
                }
            }
            TyKind::ExternStruct(_) => {
                return Err(format!(
                    "cannot encode extern struct `{}` into ABI v2 metadata",
                    ty.display(self.db)
                ));
            }
            TyKind::Function { inputs, output } => AbiTypeKind::Function {
                params: inputs
                    .iter()
                    .map(|&input| self.intern_type_with_bindings(input, None, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?,
                result: self.intern_type_with_bindings(*output, None, recursive_bindings)?,
                domain: function_domain.unwrap_or(ExecutionDomain::Both),
            },
            TyKind::Struct(struct_ty) => {
                let nominal = self.intern_symbol(struct_ty.name(self.db).text(self.db));
                AbiTypeKind::Struct {
                    nominal,
                    fields: struct_fields(self.db, *struct_ty)
                        .iter()
                        .map(|(name, ty)| {
                            Ok(RecordField {
                                name: self.intern_field_name(name.text(self.db)),
                                ty: self.intern_type_with_bindings(
                                    *ty,
                                    None,
                                    recursive_bindings,
                                )?,
                            })
                        })
                        .collect::<Result<Vec<_>, String>>()?,
                }
            }
            TyKind::Enum(enum_ty) => {
                let nominal = self.intern_symbol(enum_ty.name(self.db).text(self.db));
                AbiTypeKind::Enum {
                    nominal,
                    variants: enum_variants(self.db, *enum_ty)
                        .iter()
                        .map(|(name, fields)| {
                            Ok(EnumVariant {
                                name: self.intern_variant_name(name.text(self.db)),
                                fields: fields
                                    .iter()
                                    .map(|ty| {
                                        self.intern_type_with_bindings(
                                            *ty,
                                            None,
                                            recursive_bindings,
                                        )
                                    })
                                    .collect::<Result<Vec<_>, _>>()?,
                            })
                        })
                        .collect::<Result<Vec<_>, String>>()?,
                }
            }
            TyKind::Union(members) => AbiTypeKind::Union {
                members: members
                    .iter()
                    .map(|&member| self.intern_type_with_bindings(member, None, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?,
            },
            TyKind::Inter(members) => {
                let members = members
                    .iter()
                    .map(|&member| self.intern_type_with_bindings(member, None, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                let carrier = *members.first().ok_or_else(|| {
                    format!("intersection `{}` does not have a carrier member", ty.display(self.db))
                })?;
                AbiTypeKind::Intersection { members, carrier, facet_plan: None }
            }
            TyKind::Rec(id, body) => {
                let existing = recursive_bindings.get(id).copied();
                let kind =
                    self.lower_type_kind_with_bindings(*body, function_domain, recursive_bindings)?;
                if let Some(existing) = existing {
                    recursive_bindings.insert(*id, existing);
                } else {
                    recursive_bindings.remove(id);
                }
                kind
            }
            TyKind::Var(id) if recursive_bindings.contains_key(id) => {
                return Err(format!(
                    "ABI v2 metadata builder lost recursive binding for `{}`",
                    ty.display(self.db)
                ));
            }
            TyKind::Unknown | TyKind::Var(_) => {
                return Err(format!(
                    "cannot encode unresolved type `{}` into ABI v2 metadata",
                    ty.display(self.db)
                ));
            }
        })
    }
}

fn finalize_intersections(graph: &mut SemanticTypeGraph) -> Result<(), String> {
    let fingerprints = graph.types.iter().map(|node| node.fingerprint).collect::<Vec<_>>();

    for index in 0..graph.types.len() {
        let AbiTypeKind::Intersection { members, .. } = &graph.types[index].kind else {
            continue;
        };
        let mut normalized_members = members.clone();
        normalized_members.sort_by_key(|member| {
            (fingerprints.get(member.0 as usize).copied().unwrap_or(TypeFingerprint::ZERO), *member)
        });
        normalized_members.dedup_by_key(|member| {
            fingerprints.get(member.0 as usize).copied().unwrap_or(TypeFingerprint::ZERO)
        });

        if normalized_members.is_empty() {
            return Err(format!(
                "intersection type `{}` did not contain any members after normalization",
                index
            ));
        }

        let chosen_carrier = choose_intersection_carrier(graph, &normalized_members, &fingerprints)
            .ok_or_else(|| {
                format!("intersection type `{}` did not have a valid carrier member", index)
            })?;

        let mut entries = Vec::with_capacity(normalized_members.len());
        let mut live_entries = 0usize;
        for &member in &normalized_members {
            let kind = if member == chosen_carrier {
                FacetPlanEntryKind::Erased
            } else if matches!(
                semantic_transport_class(graph, member)?,
                TransportClass::CapabilityHandle
            ) {
                live_entries += 1;
                FacetPlanEntryKind::HandleFacet
            } else {
                live_entries += 1;
                FacetPlanEntryKind::ValueFacet
            };
            entries.push(FacetPlanEntry { member, kind });
        }

        let plan_id = if live_entries == 0 {
            None
        } else {
            let id = FacetPlanId(graph.facet_plans.len() as u32);
            graph.facet_plans.push(FacetPlan { id, entries });
            Some(id)
        };
        if let AbiTypeKind::Intersection { members, carrier, facet_plan } =
            &mut graph.types[index].kind
        {
            *members = normalized_members;
            *carrier = chosen_carrier;
            *facet_plan = plan_id;
        };
    }

    for index in 0..graph.signatures.len() {
        let mut params = graph.signatures[index].params.clone();
        for transport in &mut params {
            finalize_transport_ref(graph, transport)?;
        }
        let mut result = graph.signatures[index].result.clone();
        finalize_transport_ref(graph, &mut result)?;
        graph.signatures[index].params = params;
        graph.signatures[index].result = result;
    }

    Ok(())
}

fn finalize_transport_ref(
    graph: &SemanticTypeGraph,
    transport: &mut TransportRef,
) -> Result<(), String> {
    let AbiTypeKind::Intersection { carrier, facet_plan, .. } =
        &type_kind(graph, transport.semantic_type)?.kind
    else {
        return Ok(());
    };
    let live_entries = facet_plan
        .and_then(|plan_id| graph.facet_plans.get(plan_id.0 as usize))
        .map_or(0, |plan| {
            plan.entries.iter().filter(|entry| entry.kind != FacetPlanEntryKind::Erased).count()
        });
    transport.transport_type = Some(*carrier);
    transport.transport_class = if live_entries == 0 {
        semantic_transport_class(graph, *carrier)?
    } else {
        TransportClass::CanonicalValue
    };
    Ok(())
}

fn choose_intersection_carrier(
    graph: &SemanticTypeGraph,
    members: &[TypeId],
    fingerprints: &[TypeFingerprint],
) -> Option<TypeId> {
    members.iter().copied().min_by_key(|member| {
        (
            intersection_carrier_priority(graph, *member),
            fingerprints.get(member.0 as usize).copied().unwrap_or(TypeFingerprint::ZERO),
            *member,
        )
    })
}

fn intersection_carrier_priority(graph: &SemanticTypeGraph, ty: TypeId) -> u8 {
    match type_kind(graph, ty).map(|node| &node.kind) {
        Ok(AbiTypeKind::Record { .. }) => 0,
        Ok(AbiTypeKind::Struct { .. }) => 1,
        Ok(AbiTypeKind::Tuple { .. }) => 2,
        Ok(AbiTypeKind::Enum { .. }) => 3,
        Ok(AbiTypeKind::Union { .. }) => 4,
        Ok(AbiTypeKind::Array { .. }) => 5,
        Ok(AbiTypeKind::String) => 6,
        Ok(AbiTypeKind::Unit)
        | Ok(AbiTypeKind::Bool)
        | Ok(AbiTypeKind::Int { .. })
        | Ok(AbiTypeKind::Float { .. })
        | Ok(AbiTypeKind::Char) => 7,
        Ok(AbiTypeKind::Function { .. }) => 8,
        Ok(AbiTypeKind::Opaque { .. }) => 9,
        Ok(AbiTypeKind::Intersection { .. }) => 10,
        Err(_) => 11,
    }
}

fn semantic_transport_class(
    graph: &SemanticTypeGraph,
    ty: TypeId,
) -> Result<TransportClass, String> {
    let node = type_kind(graph, ty)?;
    Ok(match &node.kind {
        AbiTypeKind::Unit
        | AbiTypeKind::Bool
        | AbiTypeKind::Int { .. }
        | AbiTypeKind::Float { .. }
        | AbiTypeKind::Char => TransportClass::Immediate,
        AbiTypeKind::Enum { variants, .. }
            if variants.iter().all(|variant| variant.fields.is_empty()) =>
        {
            TransportClass::Immediate
        }
        AbiTypeKind::Function { .. } | AbiTypeKind::Opaque { .. } => {
            TransportClass::CapabilityHandle
        }
        AbiTypeKind::Intersection { carrier, facet_plan, .. } => {
            let live_entries = facet_plan
                .and_then(|plan_id| graph.facet_plans.get(plan_id.0 as usize))
                .map_or(0, |plan| {
                    plan.entries
                        .iter()
                        .filter(|entry| entry.kind != FacetPlanEntryKind::Erased)
                        .count()
                });
            if live_entries == 0 {
                semantic_transport_class(graph, *carrier)?
            } else {
                TransportClass::CanonicalValue
            }
        }
        AbiTypeKind::String
        | AbiTypeKind::Array { .. }
        | AbiTypeKind::Tuple { .. }
        | AbiTypeKind::Record { .. }
        | AbiTypeKind::Struct { .. }
        | AbiTypeKind::Union { .. } => TransportClass::CanonicalValue,
        AbiTypeKind::Enum { .. } => TransportClass::CanonicalValue,
    })
}

fn type_kind(graph: &SemanticTypeGraph, ty: TypeId) -> Result<&TypeNode, String> {
    graph.types.get(ty.0 as usize).ok_or_else(|| format!("unknown ABI v2 type `{}`", ty.0))
}

fn ty_bits(ty: Ty<'_>) -> u32 {
    u32::try_from(ty.as_id().as_bits()).expect("type id should fit into u32")
}

fn boundary_transport_class(db: &dyn salsa::Database, ty: Ty<'_>) -> TransportClass {
    match ty.kind(db) {
        TyKind::Bool | TyKind::Float | TyKind::Int | TyKind::Char => TransportClass::Immediate,
        TyKind::Tuple(items) if items.is_empty() => TransportClass::Immediate,
        TyKind::Enum(enum_ty) if is_nullary_enum_ty(db, *enum_ty) => TransportClass::Immediate,
        TyKind::Function { .. } => TransportClass::CapabilityHandle,
        TyKind::Inter(members) => boundary_intersection_transport_class(db, members),
        TyKind::ExactInt(_) | TyKind::Pointer { .. } | TyKind::ExternStruct(_) => {
            TransportClass::CanonicalValue
        }
        TyKind::String
        | TyKind::Array(_)
        | TyKind::Tuple(_)
        | TyKind::Record(_)
        | TyKind::Union(_)
        | TyKind::Rec(_, _)
        | TyKind::Struct(_)
        | TyKind::Enum(_)
        | TyKind::Unknown
        | TyKind::Var(_) => TransportClass::CanonicalValue,
    }
}

fn boundary_intersection_transport_class(
    db: &dyn salsa::Database,
    members: &[Ty<'_>],
) -> TransportClass {
    let Some(carrier_index) = choose_intersection_carrier_index(db, members) else {
        return TransportClass::CanonicalValue;
    };
    let carrier = members[carrier_index];
    let carrier_bits = ty_bits(carrier);
    let mut seen = std::collections::BTreeSet::new();
    let mut live_members = 0usize;

    for &member in members {
        let bits = ty_bits(member);
        if !seen.insert(bits) {
            continue;
        }
        if bits == carrier_bits {
            continue;
        }
        live_members += 1;
    }

    if live_members == 0 {
        boundary_transport_class(db, carrier)
    } else {
        TransportClass::CanonicalValue
    }
}

fn choose_intersection_carrier_index(
    db: &dyn salsa::Database,
    members: &[Ty<'_>],
) -> Option<usize> {
    members
        .iter()
        .enumerate()
        .min_by_key(|(_, ty)| (transport_carrier_priority(db, **ty), ty_bits(**ty)))
        .map(|(index, _)| index)
}

fn transport_carrier_priority(db: &dyn salsa::Database, ty: Ty<'_>) -> u8 {
    match ty.kind(db) {
        TyKind::Record(_) => 0,
        TyKind::ExternStruct(_) => 1,
        TyKind::Struct(_) => 2,
        TyKind::Tuple(_) => 3,
        TyKind::Enum(_) => 4,
        TyKind::Union(_) => 5,
        TyKind::Array(_) => 6,
        TyKind::String => 7,
        TyKind::Bool | TyKind::Float | TyKind::Int | TyKind::ExactInt(_) | TyKind::Char => 8,
        TyKind::Pointer { .. } => 9,
        TyKind::Function { .. } => 10,
        TyKind::Unknown | TyKind::Var(_) | TyKind::Rec(_, _) | TyKind::Inter(_) => 11,
    }
}

fn is_nullary_enum_ty<'db>(
    db: &'db dyn salsa::Database,
    enum_ty: mitki_hir::ty::EnumTy<'db>,
) -> bool {
    enum_variants(db, enum_ty).iter().all(|(_, fields)| fields.is_empty())
}

#[cfg(test)]
mod tests {
    use mitki_abi::{
        AbiTypeKind, REQUIRED_FEATURE_RECURSIVE_CANONICAL, REQUIRED_FEATURE_UNION_TRANSPORT,
        TransportClass,
    };
    use mitki_db::RootDatabase;
    use mitki_hir::ty::{Ty, TyKind};

    use super::{AbiGraphBuilder, finalize_intersections, finalize_transport_ref};

    #[test]
    fn builder_lowers_recursive_types_into_graph_edges() {
        let db = RootDatabase::default();
        let mut builder = AbiGraphBuilder::new(&db);
        let recursive =
            Ty::new(&db, TyKind::Rec(0, Ty::new(&db, TyKind::Array(Ty::new(&db, TyKind::Var(0))))));

        let type_id = builder.intern_type(recursive, None).expect("recursive type should lower");
        builder.graph.populate_recursive_groups();

        assert!(matches!(
            builder.graph.types[type_id.0 as usize].kind,
            AbiTypeKind::Array { elem } if elem == type_id
        ));
        assert_eq!(builder.graph.recursive_groups.len(), 1);
        assert_eq!(
            builder.graph.types[type_id.0 as usize].recursive_group,
            Some(mitki_abi::RecursiveGroupId(0))
        );
    }

    #[test]
    fn builder_marks_required_features_from_boundary_transports() {
        let db = RootDatabase::default();
        let mut builder = AbiGraphBuilder::new(&db);
        let recursive =
            Ty::new(&db, TyKind::Rec(0, Ty::new(&db, TyKind::Array(Ty::new(&db, TyKind::Var(0))))));
        let union = Ty::new(&db, TyKind::Union(vec![Ty::new(&db, TyKind::Int), recursive]));
        let function_type = builder
            .intern_function_type(
                &[union],
                Ty::new(&db, TyKind::Int),
                mitki_abi::ExecutionDomain::Runtime,
            )
            .expect("function type");
        let result = builder.transport_ref(Ty::new(&db, TyKind::Int)).expect("result transport");
        let param = builder.transport_ref(union).expect("transport should build");

        assert_eq!(param.transport_class, TransportClass::CanonicalValue);
        builder.graph.signatures.push(mitki_abi::FunctionSignature {
            id: mitki_abi::SigId(0),
            function_type,
            params: vec![param],
            result,
        });
        builder.graph.populate_recursive_groups();
        builder.graph.recompute_required_features();

        assert_eq!(
            builder.graph.required_features,
            REQUIRED_FEATURE_RECURSIVE_CANONICAL | REQUIRED_FEATURE_UNION_TRANSPORT
        );
    }

    #[test]
    fn intersections_choose_deterministic_carriers_and_facet_plans() {
        let db = RootDatabase::default();
        let mut builder = AbiGraphBuilder::new(&db);
        let intersection = Ty::new(
            &db,
            TyKind::Inter(vec![
                Ty::new(&db, TyKind::Record(vec![])),
                Ty::new(
                    &db,
                    TyKind::Function {
                        inputs: vec![Ty::new(&db, TyKind::Int)],
                        output: Ty::new(&db, TyKind::Int),
                    },
                ),
            ]),
        );

        let semantic_type = builder.intern_type(intersection, None).expect("intersection type");
        let mut transport = builder.transport_ref(intersection).expect("transport should build");
        builder.graph.populate_recursive_groups();
        builder.graph.populate_fingerprints().expect("fingerprints");
        builder.graph.normalize_commutative_members().expect("normalized members");
        builder.graph.populate_fingerprints().expect("fingerprints");
        finalize_intersections(&mut builder.graph).expect("intersection finalization");

        finalize_transport_ref(&builder.graph, &mut transport).expect("transport finalization");

        let AbiTypeKind::Intersection { carrier, facet_plan, .. } =
            &builder.graph.types[semantic_type.0 as usize].kind
        else {
            panic!("expected intersection type");
        };
        assert_eq!(transport.transport_class, TransportClass::CanonicalValue);
        assert_eq!(transport.transport_type, Some(*carrier));
        assert!(matches!(builder.graph.types[carrier.0 as usize].kind, AbiTypeKind::Record { .. }));
        let plan_id = facet_plan.expect("expected facet plan");
        let plan = &builder.graph.facet_plans[plan_id.0 as usize];
        assert_eq!(plan.entries.len(), 2);
        assert_eq!(
            plan.entries
                .iter()
                .filter(|entry| entry.kind == mitki_abi::FacetPlanEntryKind::Erased)
                .count(),
            1
        );
        assert_eq!(
            plan.entries
                .iter()
                .filter(|entry| entry.kind == mitki_abi::FacetPlanEntryKind::HandleFacet)
                .count(),
            1
        );
    }
}

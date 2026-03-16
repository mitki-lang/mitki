use anyhow::{anyhow, bail};

use crate::TransportClass;
use crate::codec::{BinaryReader, BinaryWriter};

pub const METADATA_V2_MAGIC: [u8; 8] = *b"MTKABI2\0";
pub const METADATA_ENCODING_VERSION: u16 = 2;
pub const ABI_SEMANTIC_MAJOR: u16 = 2;
pub const ABI_SEMANTIC_MINOR: u16 = 0;
pub const REQUIRED_FEATURE_HANDLES: u64 = 1 << 0;
pub const REQUIRED_FEATURE_RECURSIVE_CANONICAL: u64 = 1 << 1;
pub const REQUIRED_FEATURE_UNION_TRANSPORT: u64 = 1 << 2;
pub const REQUIRED_FEATURE_INTERSECTION_TRANSPORT: u64 = 1 << 3;

macro_rules! id_type {
    ($name:ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);
    };
}

id_type!(StringId);
id_type!(FieldNameId);
id_type!(VariantNameId);
id_type!(SymbolId);
id_type!(DebugNameId);
id_type!(RecursiveGroupId);
id_type!(TypeId);
id_type!(SigId);
id_type!(InstanceId);
id_type!(GenericOriginId);
id_type!(FacetPlanId);
id_type!(CapabilityId);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeFingerprint(pub [u8; 16]);

impl TypeFingerprint {
    pub const ZERO: Self = Self([0; 16]);

    pub fn to_hex(self) -> String {
        let mut output = String::with_capacity(32);
        for byte in self.0 {
            use std::fmt::Write as _;
            let _ = write!(&mut output, "{byte:02x}");
        }
        output
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ExecutionDomain {
    Runtime,
    Stage,
    Both,
}

impl ExecutionDomain {
    pub fn as_bits(self) -> u8 {
        match self {
            Self::Runtime => 1,
            Self::Stage => 2,
            Self::Both => 3,
        }
    }

    pub fn from_bits(bits: u8) -> anyhow::Result<Self> {
        match bits {
            1 => Ok(Self::Runtime),
            2 => Ok(Self::Stage),
            3 => Ok(Self::Both),
            value => bail!("unknown execution domain bits `{value}`"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecordField {
    pub name: FieldNameId,
    pub ty: TypeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: VariantNameId,
    pub fields: Vec<TypeId>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FacetPlanEntryKind {
    Erased,
    ValueFacet,
    HandleFacet,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FacetPlanEntry {
    pub member: TypeId,
    pub kind: FacetPlanEntryKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FacetPlan {
    pub id: FacetPlanId,
    pub entries: Vec<FacetPlanEntry>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbiTypeKind {
    Unit,
    Bool,
    Int { signed: bool, bits: u16 },
    Float { bits: u16 },
    Char,
    String,
    Array { elem: TypeId },
    Tuple { elems: Vec<TypeId> },
    Record { fields: Vec<RecordField> },
    Struct { nominal: SymbolId, fields: Vec<RecordField> },
    Enum { nominal: SymbolId, variants: Vec<EnumVariant> },
    Union { members: Vec<TypeId> },
    Intersection { members: Vec<TypeId>, carrier: TypeId, facet_plan: Option<FacetPlanId> },
    Function { params: Vec<TypeId>, result: TypeId, domain: ExecutionDomain },
    Opaque { capability_id: CapabilityId },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeNode {
    pub id: TypeId,
    pub fingerprint: TypeFingerprint,
    pub kind: AbiTypeKind,
    pub flags: u32,
    pub recursive_group: Option<RecursiveGroupId>,
    pub debug_name: Option<DebugNameId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RecursiveGroup {
    pub id: RecursiveGroupId,
    pub members: Vec<TypeId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TransportRef {
    pub semantic_type: TypeId,
    pub transport_class: TransportClass,
    pub transport_type: Option<TypeId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionSignature {
    pub id: SigId,
    pub function_type: TypeId,
    pub params: Vec<TransportRef>,
    pub result: TransportRef,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GenericOrigin {
    pub id: GenericOriginId,
    pub symbol: SymbolId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LinkageKind {
    WasmExport,
    WasmImport,
    StageEntry,
    RawImport,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionInstance {
    pub id: InstanceId,
    pub logical_symbol: SymbolId,
    pub generic_origin: Option<GenericOriginId>,
    pub type_args: Vec<TypeId>,
    pub signature: SigId,
    pub domain: ExecutionDomain,
    pub linkage: LinkageKind,
    pub wasm_module_name: Option<StringId>,
    pub wasm_field_name: Option<StringId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawImportRecord {
    pub module: StringId,
    pub field: StringId,
    pub symbol: Option<SymbolId>,
    pub signature: Option<SigId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RawExportRecord {
    pub name: StringId,
    pub symbol: Option<SymbolId>,
    pub signature: Option<SigId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BoundaryMemory {
    pub memory_index: u32,
    pub export_name: Option<StringId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SemanticTypeGraph {
    pub semantic_version_major: u16,
    pub semantic_version_minor: u16,
    pub encoding_version: u16,
    pub required_features: u64,
    pub transport_profile: StringId,
    pub profile_version_major: u16,
    pub profile_version_minor: u16,
    pub boundary_memory: BoundaryMemory,
    pub strings: Vec<String>,
    pub field_names: Vec<StringId>,
    pub variant_names: Vec<StringId>,
    pub nominal_symbols: Vec<StringId>,
    pub debug_names: Vec<StringId>,
    pub types: Vec<TypeNode>,
    pub recursive_groups: Vec<RecursiveGroup>,
    pub signatures: Vec<FunctionSignature>,
    pub generic_origins: Vec<GenericOrigin>,
    pub facet_plans: Vec<FacetPlan>,
    pub function_instances: Vec<FunctionInstance>,
    pub raw_imports: Vec<RawImportRecord>,
    pub raw_exports: Vec<RawExportRecord>,
}

impl Default for SemanticTypeGraph {
    fn default() -> Self {
        Self {
            semantic_version_major: ABI_SEMANTIC_MAJOR,
            semantic_version_minor: ABI_SEMANTIC_MINOR,
            encoding_version: METADATA_ENCODING_VERSION,
            required_features: 0,
            transport_profile: StringId(0),
            profile_version_major: 0,
            profile_version_minor: 0,
            boundary_memory: BoundaryMemory { memory_index: 0, export_name: None },
            strings: Vec::new(),
            field_names: Vec::new(),
            variant_names: Vec::new(),
            nominal_symbols: Vec::new(),
            debug_names: Vec::new(),
            types: Vec::new(),
            recursive_groups: Vec::new(),
            signatures: Vec::new(),
            generic_origins: Vec::new(),
            facet_plans: Vec::new(),
            function_instances: Vec::new(),
            raw_imports: Vec::new(),
            raw_exports: Vec::new(),
        }
    }
}

impl SemanticTypeGraph {
    pub fn insert_string(&mut self, value: impl Into<String>) -> StringId {
        let id = StringId(self.strings.len() as u32);
        self.strings.push(value.into());
        id
    }

    pub fn push_type(&mut self, kind: AbiTypeKind) -> TypeId {
        let id = TypeId(self.types.len() as u32);
        self.types.push(TypeNode {
            id,
            fingerprint: TypeFingerprint::ZERO,
            kind,
            flags: 0,
            recursive_group: None,
            debug_name: None,
        });
        id
    }

    pub fn type_node(&self, id: TypeId) -> anyhow::Result<&TypeNode> {
        self.types.get(id.0 as usize).ok_or_else(|| anyhow!("unknown type id `{}`", id.0))
    }

    pub fn populate_recursive_groups(&mut self) {
        let mut state = TarjanState::default();
        for index in 0..self.types.len() {
            if !state.indices.contains_key(&index) {
                self.visit_scc(index, &mut state);
            }
        }

        self.recursive_groups.clear();
        for node in &mut self.types {
            node.recursive_group = None;
        }

        for component in state.components {
            let is_recursive = component.len() > 1
                || self
                    .type_edges(TypeId(component[0] as u32))
                    .contains(&TypeId(component[0] as u32));
            if !is_recursive {
                continue;
            }
            let id = RecursiveGroupId(self.recursive_groups.len() as u32);
            let members =
                component.into_iter().map(|index| TypeId(index as u32)).collect::<Vec<_>>();
            for member in &members {
                if let Some(node) = self.types.get_mut(member.0 as usize) {
                    node.recursive_group = Some(id);
                }
            }
            self.recursive_groups.push(RecursiveGroup { id, members });
        }
    }

    pub fn populate_fingerprints(&mut self) -> anyhow::Result<()> {
        let mut memo = vec![None; self.types.len()];
        for index in 0..self.types.len() {
            let fingerprint = self.compute_fingerprint(TypeId(index as u32), &mut memo)?;
            self.types[index].fingerprint = fingerprint;
        }
        Ok(())
    }

    pub fn recompute_required_features(&mut self) {
        let mut features = 0;
        if self
            .signatures
            .iter()
            .flat_map(|signature| signature.params.iter().chain(std::iter::once(&signature.result)))
            .any(|transport| transport.transport_class == TransportClass::CapabilityHandle)
        {
            features |= REQUIRED_FEATURE_HANDLES;
        }
        if self.signatures.iter().any(|signature| {
            signature.params.iter().chain(std::iter::once(&signature.result)).any(|transport| {
                transport.transport_class == TransportClass::CanonicalValue
                    && self.transport_ref_uses_recursive_type(transport)
            })
        }) {
            features |= REQUIRED_FEATURE_RECURSIVE_CANONICAL;
        }
        if self.signatures.iter().any(|signature| {
            signature
                .params
                .iter()
                .chain(std::iter::once(&signature.result))
                .any(|transport| self.transport_ref_contains_union(transport))
        }) {
            features |= REQUIRED_FEATURE_UNION_TRANSPORT;
        }
        if self.signatures.iter().any(|signature| {
            signature
                .params
                .iter()
                .chain(std::iter::once(&signature.result))
                .any(|transport| self.transport_ref_contains_intersection(transport))
        }) {
            features |= REQUIRED_FEATURE_INTERSECTION_TRANSPORT;
        }
        self.required_features = features;
    }

    pub fn normalize_commutative_members(&mut self) -> anyhow::Result<()> {
        let mut memo = vec![None; self.types.len()];
        for index in 0..self.types.len() {
            let _ = self.compute_fingerprint(TypeId(index as u32), &mut memo)?;
        }

        for node in &mut self.types {
            match &mut node.kind {
                AbiTypeKind::Union { members } => {
                    members.sort_by_key(|member| {
                        (
                            memo.get(member.0 as usize)
                                .copied()
                                .flatten()
                                .unwrap_or(TypeFingerprint::ZERO),
                            *member,
                        )
                    });
                }
                AbiTypeKind::Intersection { members, .. } => {
                    members.sort_by_key(|member| {
                        (
                            memo.get(member.0 as usize)
                                .copied()
                                .flatten()
                                .unwrap_or(TypeFingerprint::ZERO),
                            *member,
                        )
                    });
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn visit_scc(&self, index: usize, state: &mut TarjanState) {
        state.indices.insert(index, state.next_index);
        state.lowlinks.insert(index, state.next_index);
        state.next_index += 1;
        state.stack.push(index);
        state.on_stack.insert(index);

        for edge in self.type_edges(TypeId(index as u32)) {
            let edge_index = edge.0 as usize;
            if !state.indices.contains_key(&edge_index) {
                self.visit_scc(edge_index, state);
                let lowlink = state.lowlinks[&index].min(state.lowlinks[&edge_index]);
                state.lowlinks.insert(index, lowlink);
            } else if state.on_stack.contains(&edge_index) {
                let lowlink = state.lowlinks[&index].min(state.indices[&edge_index]);
                state.lowlinks.insert(index, lowlink);
            }
        }

        if state.lowlinks[&index] == state.indices[&index] {
            let mut component = Vec::new();
            while let Some(member) = state.stack.pop() {
                state.on_stack.remove(&member);
                component.push(member);
                if member == index {
                    break;
                }
            }
            component.sort_unstable();
            state.components.push(component);
        }
    }

    fn compute_fingerprint(
        &self,
        id: TypeId,
        memo: &mut [Option<TypeFingerprint>],
    ) -> anyhow::Result<TypeFingerprint> {
        let index = id.0 as usize;
        if let Some(fingerprint) = memo.get(index).copied().flatten() {
            return Ok(fingerprint);
        }
        let node = self.type_node(id)?;
        if let Some(group_id) = node.recursive_group {
            self.compute_group_fingerprints(group_id, memo)?;
            return memo[index]
                .ok_or_else(|| anyhow!("missing recursive-group fingerprint for type `{}`", id.0));
        }

        let fingerprint = self.encode_type_for_hash(id, None, memo)?;
        memo[index] = Some(fingerprint);
        Ok(fingerprint)
    }

    fn compute_group_fingerprints(
        &self,
        group_id: RecursiveGroupId,
        memo: &mut [Option<TypeFingerprint>],
    ) -> anyhow::Result<()> {
        let group = self
            .recursive_groups
            .get(group_id.0 as usize)
            .ok_or_else(|| anyhow!("unknown recursive group `{}`", group_id.0))?;
        if group
            .members
            .iter()
            .all(|member| memo.get(member.0 as usize).copied().flatten().is_some())
        {
            return Ok(());
        }

        let positions = group
            .members
            .iter()
            .enumerate()
            .map(|(index, member)| (*member, index as u32))
            .collect::<std::collections::BTreeMap<_, _>>();
        let mut encodings = Vec::with_capacity(group.members.len());
        for &member in &group.members {
            encodings.push(self.encode_type_bytes(member, Some(&positions), memo)?);
        }

        for (index, member) in group.members.iter().enumerate() {
            let mut hasher = blake3::Hasher::new();
            hasher.update(b"mitki.abi.type.group.v1");
            hasher.update(&(group.members.len() as u32).to_le_bytes());
            for encoding in &encodings {
                hasher.update(&(encoding.len() as u32).to_le_bytes());
                hasher.update(encoding);
            }
            hasher.update(&(index as u32).to_le_bytes());
            let mut fingerprint = [0; 16];
            fingerprint.copy_from_slice(&hasher.finalize().as_bytes()[..16]);
            memo[member.0 as usize] = Some(TypeFingerprint(fingerprint));
        }
        Ok(())
    }

    fn transport_ref_uses_recursive_type(&self, transport: &TransportRef) -> bool {
        Self::transport_ref_roots(transport)
            .into_iter()
            .any(|root| self.type_uses_recursive_group(root))
    }

    fn transport_ref_contains_union(&self, transport: &TransportRef) -> bool {
        Self::transport_ref_roots(transport).into_iter().any(|root| self.type_contains_union(root))
    }

    fn transport_ref_contains_intersection(&self, transport: &TransportRef) -> bool {
        Self::transport_ref_roots(transport)
            .into_iter()
            .any(|root| self.type_contains_intersection(root))
    }

    fn transport_ref_roots(transport: &TransportRef) -> Vec<TypeId> {
        let mut roots = vec![transport.semantic_type];
        if let Some(transport_type) = transport.transport_type {
            roots.push(transport_type);
        }
        roots
    }

    fn type_uses_recursive_group(&self, root: TypeId) -> bool {
        let mut stack = vec![root];
        let mut seen = std::collections::BTreeSet::new();
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            if self.types.get(id.0 as usize).and_then(|node| node.recursive_group).is_some() {
                return true;
            }
            stack.extend(self.type_edges(id));
        }
        false
    }

    fn type_contains_union(&self, root: TypeId) -> bool {
        let mut stack = vec![root];
        let mut seen = std::collections::BTreeSet::new();
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            if matches!(
                self.types.get(id.0 as usize).map(|node| &node.kind),
                Some(AbiTypeKind::Union { .. })
            ) {
                return true;
            }
            stack.extend(self.type_edges(id));
        }
        false
    }

    fn type_contains_intersection(&self, root: TypeId) -> bool {
        let mut stack = vec![root];
        let mut seen = std::collections::BTreeSet::new();
        while let Some(id) = stack.pop() {
            if !seen.insert(id) {
                continue;
            }
            if matches!(
                self.types.get(id.0 as usize).map(|node| &node.kind),
                Some(AbiTypeKind::Intersection { .. })
            ) {
                return true;
            }
            stack.extend(self.type_edges(id));
        }
        false
    }
}

pub fn encode_semantic_type_graph(graph: &SemanticTypeGraph) -> anyhow::Result<Vec<u8>> {
    let mut writer = BinaryWriter::new();
    writer.bytes(&METADATA_V2_MAGIC);
    writer.u16(graph.semantic_version_major);
    writer.u16(graph.semantic_version_minor);
    writer.u16(graph.encoding_version);
    writer.u64(graph.required_features);
    encode_string_id(&mut writer, graph.transport_profile);
    writer.u16(graph.profile_version_major);
    writer.u16(graph.profile_version_minor);
    writer.u32(graph.boundary_memory.memory_index);
    writer.option(graph.boundary_memory.export_name, encode_string_id);

    encode_strings(&mut writer, &graph.strings);
    encode_ids(&mut writer, &graph.field_names, encode_string_id);
    encode_ids(&mut writer, &graph.variant_names, encode_string_id);
    encode_ids(&mut writer, &graph.nominal_symbols, encode_string_id);
    encode_ids(&mut writer, &graph.debug_names, encode_string_id);
    encode_type_nodes(&mut writer, &graph.types);
    encode_recursive_groups(&mut writer, &graph.recursive_groups);
    encode_signatures(&mut writer, &graph.signatures);
    encode_generic_origins(&mut writer, &graph.generic_origins);
    encode_facet_plans(&mut writer, &graph.facet_plans);
    encode_function_instances(&mut writer, &graph.function_instances);
    encode_raw_imports(&mut writer, &graph.raw_imports);
    encode_raw_exports(&mut writer, &graph.raw_exports);
    Ok(writer.into_bytes())
}

pub fn decode_semantic_type_graph(bytes: &[u8]) -> anyhow::Result<SemanticTypeGraph> {
    let mut reader = BinaryReader::new(bytes);
    let magic = reader.fixed::<8>()?;
    if magic != METADATA_V2_MAGIC {
        bail!("invalid mitki.abi.v2 magic header");
    }
    let semantic_version_major = reader.u16()?;
    let semantic_version_minor = reader.u16()?;
    let encoding_version = reader.u16()?;
    let required_features = reader.u64()?;
    let transport_profile = decode_string_id(&mut reader)?;
    let profile_version_major = reader.u16()?;
    let profile_version_minor = reader.u16()?;
    let memory_index = reader.u32()?;
    let export_name = reader.option(decode_string_id)?;

    let strings = decode_strings(&mut reader)?;
    let field_names = decode_ids(&mut reader, decode_string_id)?;
    let variant_names = decode_ids(&mut reader, decode_string_id)?;
    let nominal_symbols = decode_ids(&mut reader, decode_string_id)?;
    let debug_names = decode_ids(&mut reader, decode_string_id)?;
    let types = decode_type_nodes(&mut reader)?;
    let recursive_groups = decode_recursive_groups(&mut reader)?;
    let signatures = decode_signatures(&mut reader)?;
    let generic_origins = decode_generic_origins(&mut reader)?;
    let facet_plans = decode_facet_plans(&mut reader)?;
    let function_instances = decode_function_instances(&mut reader)?;
    let raw_imports = decode_raw_imports(&mut reader)?;
    let raw_exports = decode_raw_exports(&mut reader)?;
    reader.ensure_finished()?;

    Ok(SemanticTypeGraph {
        semantic_version_major,
        semantic_version_minor,
        encoding_version,
        required_features,
        transport_profile,
        profile_version_major,
        profile_version_minor,
        boundary_memory: BoundaryMemory { memory_index, export_name },
        strings,
        field_names,
        variant_names,
        nominal_symbols,
        debug_names,
        types,
        recursive_groups,
        signatures,
        generic_origins,
        facet_plans,
        function_instances,
        raw_imports,
        raw_exports,
    })
}

#[derive(Default)]
struct TarjanState {
    next_index: u32,
    indices: std::collections::BTreeMap<usize, u32>,
    lowlinks: std::collections::BTreeMap<usize, u32>,
    stack: Vec<usize>,
    on_stack: std::collections::BTreeSet<usize>,
    components: Vec<Vec<usize>>,
}

impl SemanticTypeGraph {
    fn type_edges(&self, id: TypeId) -> Vec<TypeId> {
        let node = &self.types[id.0 as usize];
        match &node.kind {
            AbiTypeKind::Unit
            | AbiTypeKind::Bool
            | AbiTypeKind::Int { .. }
            | AbiTypeKind::Float { .. }
            | AbiTypeKind::Char
            | AbiTypeKind::String
            | AbiTypeKind::Opaque { .. } => Vec::new(),
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
        }
    }

    fn encode_type_for_hash(
        &self,
        id: TypeId,
        local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
        memo: &mut [Option<TypeFingerprint>],
    ) -> anyhow::Result<TypeFingerprint> {
        let bytes = self.encode_type_bytes(id, local_positions, memo)?;
        let mut hasher = blake3::Hasher::new();
        hasher.update(b"mitki.abi.type.v1");
        hasher.update(&(bytes.len() as u32).to_le_bytes());
        hasher.update(&bytes);
        let mut fingerprint = [0; 16];
        fingerprint.copy_from_slice(&hasher.finalize().as_bytes()[..16]);
        Ok(TypeFingerprint(fingerprint))
    }

    fn encode_type_bytes(
        &self,
        id: TypeId,
        local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
        memo: &mut [Option<TypeFingerprint>],
    ) -> anyhow::Result<Vec<u8>> {
        let node = self.type_node(id)?;
        let mut writer = BinaryWriter::new();
        match &node.kind {
            AbiTypeKind::Unit => writer.u8(0),
            AbiTypeKind::Bool => writer.u8(1),
            AbiTypeKind::Int { signed, bits } => {
                writer.u8(2);
                writer.bool(*signed);
                writer.u16(*bits);
            }
            AbiTypeKind::Float { bits } => {
                writer.u8(3);
                writer.u16(*bits);
            }
            AbiTypeKind::Char => writer.u8(4),
            AbiTypeKind::String => writer.u8(5),
            AbiTypeKind::Array { elem } => {
                writer.u8(6);
                self.encode_type_ref(&mut writer, *elem, local_positions, memo)?;
            }
            AbiTypeKind::Tuple { elems } => {
                writer.u8(7);
                encode_type_refs(&mut writer, elems, local_positions, memo, self)?;
            }
            AbiTypeKind::Record { fields } => {
                writer.u8(8);
                encode_record_fields(&mut writer, fields, local_positions, memo, self)?;
            }
            AbiTypeKind::Struct { nominal, fields } => {
                writer.u8(9);
                encode_symbol_id(&mut writer, *nominal);
                encode_record_fields(&mut writer, fields, local_positions, memo, self)?;
            }
            AbiTypeKind::Enum { nominal, variants } => {
                writer.u8(10);
                encode_symbol_id(&mut writer, *nominal);
                writer.len(variants.len());
                for variant in variants {
                    encode_variant_name_id(&mut writer, variant.name);
                    encode_type_refs(&mut writer, &variant.fields, local_positions, memo, self)?;
                }
            }
            AbiTypeKind::Union { members } => {
                writer.u8(11);
                encode_normalized_members(&mut writer, members, local_positions, memo, self)?;
            }
            AbiTypeKind::Intersection { members, carrier, facet_plan } => {
                writer.u8(12);
                encode_normalized_members(&mut writer, members, local_positions, memo, self)?;
                self.encode_type_ref(&mut writer, *carrier, local_positions, memo)?;
                encode_facet_plan_fingerprint(
                    &mut writer,
                    *facet_plan,
                    local_positions,
                    memo,
                    self,
                )?;
            }
            AbiTypeKind::Function { params, result, domain } => {
                writer.u8(13);
                encode_type_refs(&mut writer, params, local_positions, memo, self)?;
                self.encode_type_ref(&mut writer, *result, local_positions, memo)?;
                writer.u8(domain.as_bits());
            }
            AbiTypeKind::Opaque { capability_id } => {
                writer.u8(14);
                encode_capability_id(&mut writer, *capability_id);
            }
        }
        Ok(writer.into_bytes())
    }

    fn encode_type_ref(
        &self,
        writer: &mut BinaryWriter,
        id: TypeId,
        local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
        memo: &mut [Option<TypeFingerprint>],
    ) -> anyhow::Result<()> {
        if let Some(position) = local_positions.and_then(|positions| positions.get(&id).copied()) {
            writer.u8(0);
            writer.u32(position);
            return Ok(());
        }
        writer.u8(1);
        let fingerprint = self.compute_fingerprint(id, memo)?;
        writer.bytes(&fingerprint.0);
        Ok(())
    }
}

fn encode_strings(writer: &mut BinaryWriter, values: &[String]) {
    writer.len(values.len());
    for value in values {
        writer.string(value);
    }
}

fn decode_strings(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<String>> {
    let len = reader.len()?;
    (0..len).map(|_| reader.string()).collect()
}

fn encode_ids<T: Copy>(
    writer: &mut BinaryWriter,
    values: &[T],
    encode: impl Fn(&mut BinaryWriter, T),
) {
    writer.len(values.len());
    for &value in values {
        encode(writer, value);
    }
}

fn decode_ids<T>(
    reader: &mut BinaryReader<'_>,
    decode: impl Fn(&mut BinaryReader<'_>) -> anyhow::Result<T>,
) -> anyhow::Result<Vec<T>> {
    let len = reader.len()?;
    (0..len).map(|_| decode(reader)).collect()
}

fn encode_type_nodes(writer: &mut BinaryWriter, values: &[TypeNode]) {
    writer.len(values.len());
    for value in values {
        encode_type_id(writer, value.id);
        writer.bytes(&value.fingerprint.0);
        writer.u32(value.flags);
        writer.option(value.recursive_group, encode_recursive_group_id);
        writer.option(value.debug_name, encode_debug_name_id);
        encode_type_kind(writer, &value.kind);
    }
}

fn decode_type_nodes(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<TypeNode>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(TypeNode {
            id: decode_type_id(reader)?,
            fingerprint: TypeFingerprint(reader.fixed::<16>()?),
            flags: reader.u32()?,
            recursive_group: reader.option(decode_recursive_group_id)?,
            debug_name: reader.option(decode_debug_name_id)?,
            kind: decode_type_kind(reader)?,
        });
    }
    Ok(values)
}

fn encode_recursive_groups(writer: &mut BinaryWriter, values: &[RecursiveGroup]) {
    writer.len(values.len());
    for value in values {
        encode_recursive_group_id(writer, value.id);
        encode_ids(writer, &value.members, encode_type_id);
    }
}

fn decode_recursive_groups(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<RecursiveGroup>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(RecursiveGroup {
            id: decode_recursive_group_id(reader)?,
            members: decode_ids(reader, decode_type_id)?,
        });
    }
    Ok(values)
}

fn encode_facet_plans(writer: &mut BinaryWriter, values: &[FacetPlan]) {
    writer.len(values.len());
    for value in values {
        encode_facet_plan_id(writer, value.id);
        writer.len(value.entries.len());
        for entry in &value.entries {
            encode_type_id(writer, entry.member);
            writer.u8(match entry.kind {
                FacetPlanEntryKind::Erased => 0,
                FacetPlanEntryKind::ValueFacet => 1,
                FacetPlanEntryKind::HandleFacet => 2,
            });
        }
    }
}

fn decode_facet_plans(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<FacetPlan>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        let id = decode_facet_plan_id(reader)?;
        let entry_len = reader.len()?;
        let mut entries = Vec::with_capacity(entry_len);
        for _ in 0..entry_len {
            entries.push(FacetPlanEntry {
                member: decode_type_id(reader)?,
                kind: decode_facet_plan_entry_kind(reader.u8()?)?,
            });
        }
        values.push(FacetPlan { id, entries });
    }
    Ok(values)
}

fn encode_signatures(writer: &mut BinaryWriter, values: &[FunctionSignature]) {
    writer.len(values.len());
    for value in values {
        encode_sig_id(writer, value.id);
        encode_type_id(writer, value.function_type);
        encode_transport_refs(writer, &value.params);
        encode_transport_ref(writer, &value.result);
    }
}

fn decode_signatures(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<FunctionSignature>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(FunctionSignature {
            id: decode_sig_id(reader)?,
            function_type: decode_type_id(reader)?,
            params: decode_transport_refs(reader)?,
            result: decode_transport_ref(reader)?,
        });
    }
    Ok(values)
}

fn encode_generic_origins(writer: &mut BinaryWriter, values: &[GenericOrigin]) {
    writer.len(values.len());
    for value in values {
        encode_generic_origin_id(writer, value.id);
        encode_symbol_id(writer, value.symbol);
    }
}

fn decode_generic_origins(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<GenericOrigin>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(GenericOrigin {
            id: decode_generic_origin_id(reader)?,
            symbol: decode_symbol_id(reader)?,
        });
    }
    Ok(values)
}

fn encode_function_instances(writer: &mut BinaryWriter, values: &[FunctionInstance]) {
    writer.len(values.len());
    for value in values {
        encode_instance_id(writer, value.id);
        encode_symbol_id(writer, value.logical_symbol);
        writer.option(value.generic_origin, encode_generic_origin_id);
        encode_ids(writer, &value.type_args, encode_type_id);
        encode_sig_id(writer, value.signature);
        writer.u8(value.domain.as_bits());
        writer.u8(match value.linkage {
            LinkageKind::WasmExport => 0,
            LinkageKind::WasmImport => 1,
            LinkageKind::StageEntry => 2,
            LinkageKind::RawImport => 3,
        });
        writer.option(value.wasm_module_name, encode_string_id);
        writer.option(value.wasm_field_name, encode_string_id);
    }
}

fn decode_function_instances(
    reader: &mut BinaryReader<'_>,
) -> anyhow::Result<Vec<FunctionInstance>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        let id = decode_instance_id(reader)?;
        let logical_symbol = decode_symbol_id(reader)?;
        let generic_origin = reader.option(decode_generic_origin_id)?;
        let type_args = decode_ids(reader, decode_type_id)?;
        let signature = decode_sig_id(reader)?;
        let domain = ExecutionDomain::from_bits(reader.u8()?)?;
        let linkage = match reader.u8()? {
            0 => LinkageKind::WasmExport,
            1 => LinkageKind::WasmImport,
            2 => LinkageKind::StageEntry,
            3 => LinkageKind::RawImport,
            value => bail!("unknown linkage tag `{value}`"),
        };
        values.push(FunctionInstance {
            id,
            logical_symbol,
            generic_origin,
            type_args,
            signature,
            domain,
            linkage,
            wasm_module_name: reader.option(decode_string_id)?,
            wasm_field_name: reader.option(decode_string_id)?,
        });
    }
    Ok(values)
}

fn encode_raw_imports(writer: &mut BinaryWriter, values: &[RawImportRecord]) {
    writer.len(values.len());
    for value in values {
        encode_string_id(writer, value.module);
        encode_string_id(writer, value.field);
        writer.option(value.symbol, encode_symbol_id);
        writer.option(value.signature, encode_sig_id);
    }
}

fn decode_raw_imports(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<RawImportRecord>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(RawImportRecord {
            module: decode_string_id(reader)?,
            field: decode_string_id(reader)?,
            symbol: reader.option(decode_symbol_id)?,
            signature: reader.option(decode_sig_id)?,
        });
    }
    Ok(values)
}

fn encode_raw_exports(writer: &mut BinaryWriter, values: &[RawExportRecord]) {
    writer.len(values.len());
    for value in values {
        encode_string_id(writer, value.name);
        writer.option(value.symbol, encode_symbol_id);
        writer.option(value.signature, encode_sig_id);
    }
}

fn decode_raw_exports(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<RawExportRecord>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values.push(RawExportRecord {
            name: decode_string_id(reader)?,
            symbol: reader.option(decode_symbol_id)?,
            signature: reader.option(decode_sig_id)?,
        });
    }
    Ok(values)
}

fn encode_transport_refs(writer: &mut BinaryWriter, values: &[TransportRef]) {
    writer.len(values.len());
    for value in values {
        encode_transport_ref(writer, value);
    }
}

fn decode_transport_refs(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<TransportRef>> {
    let len = reader.len()?;
    (0..len).map(|_| decode_transport_ref(reader)).collect()
}

fn encode_transport_ref(writer: &mut BinaryWriter, value: &TransportRef) {
    encode_type_id(writer, value.semantic_type);
    writer.u8(match value.transport_class {
        TransportClass::Immediate => 0,
        TransportClass::CanonicalValue => 1,
        TransportClass::CapabilityHandle => 2,
    });
    writer.option(value.transport_type, encode_type_id);
}

fn decode_transport_ref(reader: &mut BinaryReader<'_>) -> anyhow::Result<TransportRef> {
    let semantic_type = decode_type_id(reader)?;
    let transport_class = match reader.u8()? {
        0 => TransportClass::Immediate,
        1 => TransportClass::CanonicalValue,
        2 => TransportClass::CapabilityHandle,
        value => bail!("unknown transport class tag `{value}`"),
    };
    Ok(TransportRef {
        semantic_type,
        transport_class,
        transport_type: reader.option(decode_type_id)?,
    })
}

fn decode_facet_plan_entry_kind(tag: u8) -> anyhow::Result<FacetPlanEntryKind> {
    Ok(match tag {
        0 => FacetPlanEntryKind::Erased,
        1 => FacetPlanEntryKind::ValueFacet,
        2 => FacetPlanEntryKind::HandleFacet,
        value => bail!("unknown facet-plan entry tag `{value}`"),
    })
}

fn encode_type_kind(writer: &mut BinaryWriter, value: &AbiTypeKind) {
    match value {
        AbiTypeKind::Unit => writer.u8(0),
        AbiTypeKind::Bool => writer.u8(1),
        AbiTypeKind::Int { signed, bits } => {
            writer.u8(2);
            writer.bool(*signed);
            writer.u16(*bits);
        }
        AbiTypeKind::Float { bits } => {
            writer.u8(3);
            writer.u16(*bits);
        }
        AbiTypeKind::Char => writer.u8(4),
        AbiTypeKind::String => writer.u8(5),
        AbiTypeKind::Array { elem } => {
            writer.u8(6);
            encode_type_id(writer, *elem);
        }
        AbiTypeKind::Tuple { elems } => {
            writer.u8(7);
            encode_ids(writer, elems, encode_type_id);
        }
        AbiTypeKind::Record { fields } => {
            writer.u8(8);
            encode_field_list(writer, fields);
        }
        AbiTypeKind::Struct { nominal, fields } => {
            writer.u8(9);
            encode_symbol_id(writer, *nominal);
            encode_field_list(writer, fields);
        }
        AbiTypeKind::Enum { nominal, variants } => {
            writer.u8(10);
            encode_symbol_id(writer, *nominal);
            writer.len(variants.len());
            for variant in variants {
                encode_variant_name_id(writer, variant.name);
                encode_ids(writer, &variant.fields, encode_type_id);
            }
        }
        AbiTypeKind::Union { members } => {
            writer.u8(11);
            encode_ids(writer, members, encode_type_id);
        }
        AbiTypeKind::Intersection { members, carrier, facet_plan } => {
            writer.u8(12);
            encode_ids(writer, members, encode_type_id);
            encode_type_id(writer, *carrier);
            writer.option(*facet_plan, encode_facet_plan_id);
        }
        AbiTypeKind::Function { params, result, domain } => {
            writer.u8(13);
            encode_ids(writer, params, encode_type_id);
            encode_type_id(writer, *result);
            writer.u8(domain.as_bits());
        }
        AbiTypeKind::Opaque { capability_id } => {
            writer.u8(14);
            encode_capability_id(writer, *capability_id);
        }
    }
}

fn decode_type_kind(reader: &mut BinaryReader<'_>) -> anyhow::Result<AbiTypeKind> {
    Ok(match reader.u8()? {
        0 => AbiTypeKind::Unit,
        1 => AbiTypeKind::Bool,
        2 => AbiTypeKind::Int { signed: reader.bool()?, bits: reader.u16()? },
        3 => AbiTypeKind::Float { bits: reader.u16()? },
        4 => AbiTypeKind::Char,
        5 => AbiTypeKind::String,
        6 => AbiTypeKind::Array { elem: decode_type_id(reader)? },
        7 => AbiTypeKind::Tuple { elems: decode_ids(reader, decode_type_id)? },
        8 => AbiTypeKind::Record { fields: decode_field_list(reader)? },
        9 => AbiTypeKind::Struct {
            nominal: decode_symbol_id(reader)?,
            fields: decode_field_list(reader)?,
        },
        10 => {
            let nominal = decode_symbol_id(reader)?;
            let len = reader.len()?;
            let mut variants = Vec::with_capacity(len);
            for _ in 0..len {
                variants.push(EnumVariant {
                    name: decode_variant_name_id(reader)?,
                    fields: decode_ids(reader, decode_type_id)?,
                });
            }
            AbiTypeKind::Enum { nominal, variants }
        }
        11 => AbiTypeKind::Union { members: decode_ids(reader, decode_type_id)? },
        12 => AbiTypeKind::Intersection {
            members: decode_ids(reader, decode_type_id)?,
            carrier: decode_type_id(reader)?,
            facet_plan: reader.option(decode_facet_plan_id)?,
        },
        13 => AbiTypeKind::Function {
            params: decode_ids(reader, decode_type_id)?,
            result: decode_type_id(reader)?,
            domain: ExecutionDomain::from_bits(reader.u8()?)?,
        },
        14 => AbiTypeKind::Opaque { capability_id: decode_capability_id(reader)? },
        value => bail!("unknown abi type tag `{value}`"),
    })
}

fn encode_facet_plan_fingerprint(
    writer: &mut BinaryWriter,
    plan_id: Option<FacetPlanId>,
    local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
    memo: &mut [Option<TypeFingerprint>],
    graph: &SemanticTypeGraph,
) -> anyhow::Result<()> {
    match plan_id {
        Some(plan_id) => {
            writer.bool(true);
            let plan = graph
                .facet_plans
                .get(plan_id.0 as usize)
                .ok_or_else(|| anyhow!("unknown facet plan `{}`", plan_id.0))?;
            writer.len(plan.entries.len());
            for entry in &plan.entries {
                graph.encode_type_ref(writer, entry.member, local_positions, memo)?;
                writer.u8(match entry.kind {
                    FacetPlanEntryKind::Erased => 0,
                    FacetPlanEntryKind::ValueFacet => 1,
                    FacetPlanEntryKind::HandleFacet => 2,
                });
            }
        }
        None => writer.bool(false),
    }
    Ok(())
}

fn encode_field_list(writer: &mut BinaryWriter, values: &[RecordField]) {
    writer.len(values.len());
    for value in values {
        encode_field_name_id(writer, value.name);
        encode_type_id(writer, value.ty);
    }
}

fn decode_field_list(reader: &mut BinaryReader<'_>) -> anyhow::Result<Vec<RecordField>> {
    let len = reader.len()?;
    let mut values = Vec::with_capacity(len);
    for _ in 0..len {
        values
            .push(RecordField { name: decode_field_name_id(reader)?, ty: decode_type_id(reader)? });
    }
    Ok(values)
}

fn encode_record_fields(
    writer: &mut BinaryWriter,
    values: &[RecordField],
    local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
    memo: &mut [Option<TypeFingerprint>],
    graph: &SemanticTypeGraph,
) -> anyhow::Result<()> {
    writer.len(values.len());
    for value in values {
        encode_field_name_id(writer, value.name);
        graph.encode_type_ref(writer, value.ty, local_positions, memo)?;
    }
    Ok(())
}

fn encode_type_refs(
    writer: &mut BinaryWriter,
    values: &[TypeId],
    local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
    memo: &mut [Option<TypeFingerprint>],
    graph: &SemanticTypeGraph,
) -> anyhow::Result<()> {
    writer.len(values.len());
    for &value in values {
        graph.encode_type_ref(writer, value, local_positions, memo)?;
    }
    Ok(())
}

fn encode_normalized_members(
    writer: &mut BinaryWriter,
    members: &[TypeId],
    local_positions: Option<&std::collections::BTreeMap<TypeId, u32>>,
    memo: &mut [Option<TypeFingerprint>],
    graph: &SemanticTypeGraph,
) -> anyhow::Result<()> {
    let mut keyed = Vec::with_capacity(members.len());
    for &member in members {
        let key = if let Some(position) =
            local_positions.and_then(|positions| positions.get(&member).copied())
        {
            (TypeFingerprint::ZERO, position)
        } else {
            (graph.compute_fingerprint(member, memo)?, u32::MAX)
        };
        keyed.push((key, member));
    }
    keyed.sort_by_key(|entry| entry.0);
    writer.len(keyed.len());
    for (_, member) in keyed {
        graph.encode_type_ref(writer, member, local_positions, memo)?;
    }
    Ok(())
}

macro_rules! encode_decode_id {
    ($encode:ident, $decode:ident, $ty:ident) => {
        fn $encode(writer: &mut BinaryWriter, value: $ty) {
            writer.u32(value.0);
        }

        fn $decode(reader: &mut BinaryReader<'_>) -> anyhow::Result<$ty> {
            Ok($ty(reader.u32()?))
        }
    };
}

encode_decode_id!(encode_string_id, decode_string_id, StringId);
encode_decode_id!(encode_field_name_id, decode_field_name_id, FieldNameId);
encode_decode_id!(encode_variant_name_id, decode_variant_name_id, VariantNameId);
encode_decode_id!(encode_symbol_id, decode_symbol_id, SymbolId);
encode_decode_id!(encode_debug_name_id, decode_debug_name_id, DebugNameId);
encode_decode_id!(encode_recursive_group_id, decode_recursive_group_id, RecursiveGroupId);
encode_decode_id!(encode_type_id, decode_type_id, TypeId);
encode_decode_id!(encode_sig_id, decode_sig_id, SigId);
encode_decode_id!(encode_instance_id, decode_instance_id, InstanceId);
encode_decode_id!(encode_generic_origin_id, decode_generic_origin_id, GenericOriginId);
encode_decode_id!(encode_facet_plan_id, decode_facet_plan_id, FacetPlanId);
encode_decode_id!(encode_capability_id, decode_capability_id, CapabilityId);

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_graph() -> SemanticTypeGraph {
        let mut graph = SemanticTypeGraph {
            strings: vec![
                "wasm-core-v2/m32".to_owned(),
                "Point".to_owned(),
                "x".to_owned(),
                "y".to_owned(),
                "main".to_owned(),
                "memory".to_owned(),
            ],
            transport_profile: StringId(0),
            profile_version_major: 1,
            boundary_memory: BoundaryMemory { memory_index: 0, export_name: Some(StringId(5)) },
            field_names: vec![StringId(2), StringId(3)],
            nominal_symbols: vec![StringId(1), StringId(4)],
            ..SemanticTypeGraph::default()
        };

        let int = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let point = graph.push_type(AbiTypeKind::Struct {
            nominal: SymbolId(0),
            fields: vec![
                RecordField { name: FieldNameId(0), ty: int },
                RecordField { name: FieldNameId(1), ty: int },
            ],
        });
        let function = graph.push_type(AbiTypeKind::Function {
            params: vec![point],
            result: point,
            domain: ExecutionDomain::Runtime,
        });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type: function,
            params: vec![TransportRef {
                semantic_type: point,
                transport_class: TransportClass::CanonicalValue,
                transport_type: None,
            }],
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
            wasm_field_name: Some(StringId(4)),
        });
        graph.raw_exports.push(RawExportRecord {
            name: StringId(4),
            symbol: Some(SymbolId(1)),
            signature: Some(SigId(0)),
        });
        graph
    }

    #[test]
    fn semantic_graph_binary_round_trip() {
        let mut graph = sample_graph();
        graph.populate_recursive_groups();
        graph.recompute_required_features();
        graph.populate_fingerprints().expect("fingerprints should compute");
        let encoded = encode_semantic_type_graph(&graph).expect("graph should encode");
        let decoded = decode_semantic_type_graph(&encoded).expect("graph should decode");
        assert_eq!(decoded, graph);
    }

    #[test]
    fn facet_plan_binary_round_trip() {
        let mut graph = SemanticTypeGraph::default();
        let carrier = graph.push_type(AbiTypeKind::Bool);
        let handle = graph.push_type(AbiTypeKind::Opaque { capability_id: CapabilityId(0) });
        graph.facet_plans.push(FacetPlan {
            id: FacetPlanId(0),
            entries: vec![
                FacetPlanEntry { member: carrier, kind: FacetPlanEntryKind::Erased },
                FacetPlanEntry { member: handle, kind: FacetPlanEntryKind::HandleFacet },
            ],
        });
        let intersection = graph.push_type(AbiTypeKind::Intersection {
            members: vec![carrier, handle],
            carrier,
            facet_plan: Some(FacetPlanId(0)),
        });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type: intersection,
            params: vec![TransportRef {
                semantic_type: intersection,
                transport_class: TransportClass::CanonicalValue,
                transport_type: Some(carrier),
            }],
            result: TransportRef {
                semantic_type: intersection,
                transport_class: TransportClass::CanonicalValue,
                transport_type: Some(carrier),
            },
        });
        graph.populate_recursive_groups();
        graph.populate_fingerprints().expect("fingerprints should compute");

        let decoded = decode_semantic_type_graph(
            &encode_semantic_type_graph(&graph).expect("graph should encode"),
        )
        .expect("graph should decode");

        assert_eq!(decoded.facet_plans, graph.facet_plans);
        assert_eq!(decoded.types[intersection.0 as usize], graph.types[intersection.0 as usize]);
    }

    #[test]
    fn fingerprints_are_stable_for_commutative_unions() {
        let mut left = SemanticTypeGraph::default();
        let a = left.push_type(AbiTypeKind::Bool);
        let b = left.push_type(AbiTypeKind::String);
        let union = left.push_type(AbiTypeKind::Union { members: vec![a, b] });
        left.populate_recursive_groups();
        left.populate_fingerprints().expect("left fingerprints");

        let mut right = SemanticTypeGraph::default();
        let a = right.push_type(AbiTypeKind::Bool);
        let b = right.push_type(AbiTypeKind::String);
        let union_other = right.push_type(AbiTypeKind::Union { members: vec![b, a] });
        right.populate_recursive_groups();
        right.populate_fingerprints().expect("right fingerprints");

        assert_eq!(
            left.types[union.0 as usize].fingerprint,
            right.types[union_other.0 as usize].fingerprint
        );
    }

    #[test]
    fn fingerprints_ignore_module_local_type_ids() {
        let mut left = SemanticTypeGraph::default();
        let bool_ty = left.push_type(AbiTypeKind::Bool);
        let array = left.push_type(AbiTypeKind::Array { elem: bool_ty });
        left.populate_recursive_groups();
        left.populate_fingerprints().expect("left fingerprints");

        let mut right = SemanticTypeGraph::default();
        let _padding = right.push_type(AbiTypeKind::String);
        let bool_ty = right.push_type(AbiTypeKind::Bool);
        let shifted = right.push_type(AbiTypeKind::Array { elem: bool_ty });
        right.populate_recursive_groups();
        right.populate_fingerprints().expect("right fingerprints");

        assert_eq!(
            left.types[array.0 as usize].fingerprint,
            right.types[shifted.0 as usize].fingerprint
        );
    }

    #[test]
    fn recursive_groups_detect_self_cycles() {
        let mut graph = SemanticTypeGraph::default();
        let list = graph.push_type(AbiTypeKind::Array { elem: TypeId(0) });
        assert_eq!(list, TypeId(0));
        graph.populate_recursive_groups();
        assert_eq!(graph.recursive_groups.len(), 1);
        assert_eq!(graph.types[0].recursive_group, Some(RecursiveGroupId(0)));
    }

    #[test]
    fn required_features_follow_boundary_transports() {
        let mut graph = SemanticTypeGraph::default();
        let int = graph.push_type(AbiTypeKind::Int { signed: true, bits: 32 });
        let recursive = graph.push_type(AbiTypeKind::Array { elem: TypeId(1) });
        let union = graph.push_type(AbiTypeKind::Union { members: vec![int, recursive] });
        let intersection = graph.push_type(AbiTypeKind::Intersection {
            members: vec![int],
            carrier: int,
            facet_plan: None,
        });
        graph.signatures.push(FunctionSignature {
            id: SigId(0),
            function_type: int,
            params: vec![
                TransportRef {
                    semantic_type: int,
                    transport_class: TransportClass::CapabilityHandle,
                    transport_type: None,
                },
                TransportRef {
                    semantic_type: union,
                    transport_class: TransportClass::CanonicalValue,
                    transport_type: None,
                },
                TransportRef {
                    semantic_type: intersection,
                    transport_class: TransportClass::CanonicalValue,
                    transport_type: None,
                },
            ],
            result: TransportRef {
                semantic_type: int,
                transport_class: TransportClass::Immediate,
                transport_type: None,
            },
        });
        graph.populate_recursive_groups();
        graph.recompute_required_features();

        assert_eq!(
            graph.required_features,
            REQUIRED_FEATURE_HANDLES
                | REQUIRED_FEATURE_RECURSIVE_CANONICAL
                | REQUIRED_FEATURE_UNION_TRANSPORT
                | REQUIRED_FEATURE_INTERSECTION_TRANSPORT
        );
    }

    #[test]
    fn normalize_commutative_members_sorts_union_members_by_fingerprint() {
        let mut graph = SemanticTypeGraph::default();
        let string = graph.push_type(AbiTypeKind::String);
        let boolean = graph.push_type(AbiTypeKind::Bool);
        let union = graph.push_type(AbiTypeKind::Union { members: vec![string, boolean] });

        graph.populate_recursive_groups();
        graph.populate_fingerprints().expect("fingerprints should compute");
        graph.normalize_commutative_members().expect("union members should normalize");

        let AbiTypeKind::Union { members } = &graph.types[union.0 as usize].kind else {
            panic!("expected union type");
        };
        let fingerprints = members
            .iter()
            .map(|member| graph.types[member.0 as usize].fingerprint)
            .collect::<Vec<_>>();
        let mut sorted = fingerprints.clone();
        sorted.sort_unstable();
        assert_eq!(fingerprints, sorted);
    }

    #[test]
    fn facet_plan_fingerprints_use_plan_contents_not_plan_ids() {
        let mut left = SemanticTypeGraph::default();
        let carrier = left.push_type(AbiTypeKind::Bool);
        let handle = left.push_type(AbiTypeKind::Opaque { capability_id: CapabilityId(0) });
        left.facet_plans.push(FacetPlan {
            id: FacetPlanId(0),
            entries: vec![
                FacetPlanEntry { member: carrier, kind: FacetPlanEntryKind::Erased },
                FacetPlanEntry { member: handle, kind: FacetPlanEntryKind::HandleFacet },
            ],
        });
        let left_intersection = left.push_type(AbiTypeKind::Intersection {
            members: vec![carrier, handle],
            carrier,
            facet_plan: Some(FacetPlanId(0)),
        });
        left.populate_recursive_groups();
        left.populate_fingerprints().expect("left fingerprints");

        let mut right = SemanticTypeGraph::default();
        let _padding = right.push_type(AbiTypeKind::String);
        let carrier = right.push_type(AbiTypeKind::Bool);
        let handle = right.push_type(AbiTypeKind::Opaque { capability_id: CapabilityId(0) });
        for index in 0..8 {
            right.facet_plans.push(FacetPlan {
                id: FacetPlanId(index),
                entries: if index == 7 {
                    vec![
                        FacetPlanEntry { member: carrier, kind: FacetPlanEntryKind::Erased },
                        FacetPlanEntry { member: handle, kind: FacetPlanEntryKind::HandleFacet },
                    ]
                } else {
                    Vec::new()
                },
            });
        }
        let right_intersection = right.push_type(AbiTypeKind::Intersection {
            members: vec![carrier, handle],
            carrier,
            facet_plan: Some(FacetPlanId(7)),
        });
        right.populate_recursive_groups();
        right.populate_fingerprints().expect("right fingerprints");

        assert_eq!(
            left.types[left_intersection.0 as usize].fingerprint,
            right.types[right_intersection.0 as usize].fingerprint
        );
    }
}

use anyhow::{anyhow, bail};

use crate::codec::{BinaryReader, BinaryWriter};
use crate::metadata::{ABI_SEMANTIC_MAJOR, ABI_SEMANTIC_MINOR, TypeId};

pub const CANONICAL_BLOB_MAGIC: [u8; 8] = *b"MTKCV2\0\0";
pub const CANONICAL_BLOB_ENCODING_VERSION: u16 = 2;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TransportClass {
    Immediate,
    CanonicalValue,
    CapabilityHandle,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbiValue {
    Immediate(AbiScalar),
    Handle { type_id: TypeId, handle_id: u32 },
    Canonical { transport_type: TypeId, graph: CanonicalGraph },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AbiScalar {
    Unit,
    Bool(bool),
    Int { signed: bool, bits: u16, value: i64 },
    Float { bits: u16, raw_bits: u64 },
    Char { unicode_scalar: u32 },
    EnumTag { type_id: TypeId, variant_index: u32 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct NodeId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HandleSlotId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CanonicalGraph {
    pub root: ValueRef,
    pub nodes: Vec<CanonicalNode>,
    pub handles: Vec<HandleSlot>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HandleSlot {
    pub type_id: TypeId,
    pub handle_id: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueRef {
    InlineScalar(AbiScalar),
    NodeRef(NodeId),
    HandleRef(HandleSlotId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PackedScalarKind {
    Bool,
    I32,
    I64,
    F32,
    F64,
    Char,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ArrayElements {
    PackedScalars { kind: PackedScalarKind, len: u32, bytes: Vec<u8> },
    Values(Vec<ValueRef>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CanonicalNode {
    String { transport_type: TypeId, value: String },
    Array { transport_type: TypeId, elements: ArrayElements },
    Tuple { transport_type: TypeId, fields: Vec<ValueRef> },
    Record { transport_type: TypeId, fields: Vec<ValueRef> },
    Struct { transport_type: TypeId, fields: Vec<ValueRef> },
    Enum { transport_type: TypeId, variant_index: u32, fields: Vec<ValueRef> },
    Union { transport_type: TypeId, arm_index: u32, payload: ValueRef },
    Intersection { transport_type: TypeId, carrier: ValueRef, facets: Vec<ValueRef> },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CanonicalNodeKind {
    String,
    Array,
    Tuple,
    Record,
    Struct,
    Enum,
    Union,
    Intersection,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CanonicalBlobHeader {
    pub transport_type: TypeId,
    pub total_byte_len: u32,
    pub node_count: u32,
    pub handle_slot_count: u32,
    pub flags: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CanonicalNodeHeader {
    pub transport_type: TypeId,
    pub kind: CanonicalNodeKind,
    pub aux0: u32,
    pub aux1: u32,
    pub child_count: u32,
    pub payload_len: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CanonicalBlobView<'a> {
    bytes: &'a [u8],
    header: CanonicalBlobHeader,
    root: ValueRef,
    nodes: Vec<CanonicalNodeHeader>,
    handles: Vec<HandleSlot>,
    payload_ranges: Vec<std::ops::Range<usize>>,
}

impl CanonicalGraph {
    pub fn new(root: ValueRef) -> Self {
        Self { root, nodes: Vec::new(), handles: Vec::new() }
    }
}

impl CanonicalNode {
    pub fn kind(&self) -> CanonicalNodeKind {
        match self {
            Self::String { .. } => CanonicalNodeKind::String,
            Self::Array { .. } => CanonicalNodeKind::Array,
            Self::Tuple { .. } => CanonicalNodeKind::Tuple,
            Self::Record { .. } => CanonicalNodeKind::Record,
            Self::Struct { .. } => CanonicalNodeKind::Struct,
            Self::Enum { .. } => CanonicalNodeKind::Enum,
            Self::Union { .. } => CanonicalNodeKind::Union,
            Self::Intersection { .. } => CanonicalNodeKind::Intersection,
        }
    }

    pub fn transport_type(&self) -> TypeId {
        match self {
            Self::String { transport_type, .. }
            | Self::Array { transport_type, .. }
            | Self::Tuple { transport_type, .. }
            | Self::Record { transport_type, .. }
            | Self::Struct { transport_type, .. }
            | Self::Enum { transport_type, .. }
            | Self::Union { transport_type, .. }
            | Self::Intersection { transport_type, .. } => *transport_type,
        }
    }
}

pub fn encode_canonical_blob(value: &AbiValue) -> anyhow::Result<Vec<u8>> {
    let AbiValue::Canonical { transport_type, graph } = value else {
        bail!("canonical blob encoding expects AbiValue::Canonical");
    };

    let mut writer = BinaryWriter::new();
    writer.bytes(&CANONICAL_BLOB_MAGIC);
    writer.u16(ABI_SEMANTIC_MAJOR);
    writer.u16(ABI_SEMANTIC_MINOR);
    writer.u16(CANONICAL_BLOB_ENCODING_VERSION);
    writer.u32(0);
    writer.u32(transport_type.0);
    writer.u32(0);
    writer.u32(graph.nodes.len() as u32);
    writer.u32(graph.handles.len() as u32);
    encode_value_ref(&mut writer, &graph.root);
    for node in &graph.nodes {
        encode_node(&mut writer, node)?;
    }
    writer.len(graph.handles.len());
    for handle in &graph.handles {
        writer.u32(handle.type_id.0);
        writer.u32(handle.handle_id);
    }

    let mut bytes = writer.into_bytes();
    let total = u32::try_from(bytes.len())
        .map_err(|_error| anyhow!("canonical blob exceeded u32 length"))?;
    bytes[22..26].copy_from_slice(&total.to_le_bytes());
    Ok(bytes)
}

pub fn decode_canonical_blob(bytes: &[u8]) -> anyhow::Result<AbiValue> {
    let view = CanonicalBlobView::parse(bytes)?;
    Ok(AbiValue::Canonical { transport_type: view.header.transport_type, graph: view.decode()? })
}

impl<'a> CanonicalBlobView<'a> {
    pub fn parse(bytes: &'a [u8]) -> anyhow::Result<Self> {
        let mut reader = BinaryReader::new(bytes);
        if reader.fixed::<8>()? != CANONICAL_BLOB_MAGIC {
            bail!("invalid canonical blob magic");
        }
        let abi_major = reader.u16()?;
        let abi_minor = reader.u16()?;
        if abi_major != ABI_SEMANTIC_MAJOR || abi_minor != ABI_SEMANTIC_MINOR {
            bail!("unsupported canonical blob ABI version {abi_major}.{abi_minor}");
        }
        let encoding = reader.u16()?;
        if encoding != CANONICAL_BLOB_ENCODING_VERSION {
            bail!("unsupported canonical blob encoding version `{encoding}`");
        }
        let flags = reader.u32()?;
        let transport_type = TypeId(reader.u32()?);
        let total_byte_len = reader.u32()?;
        if total_byte_len as usize != bytes.len() {
            bail!("canonical blob length header did not match the actual buffer size");
        }
        let node_count = reader.u32()?;
        let handle_slot_count = reader.u32()?;
        let root = decode_value_ref(&mut reader)?;
        let mut nodes = Vec::with_capacity(node_count as usize);
        let mut payload_ranges = Vec::with_capacity(node_count as usize);
        for _ in 0..node_count {
            let node_start = reader.offset();
            let transport_type = TypeId(reader.u32()?);
            let kind = decode_node_kind(reader.u8()?)?;
            let aux0 = reader.u32()?;
            let aux1 = reader.u32()?;
            let child_count = reader.u32()?;
            let payload_len = reader.u32()?;
            for _ in 0..child_count {
                let _ = decode_value_ref(&mut reader)?;
            }
            let payload_start = reader.offset();
            let _payload = reader.bytes(payload_len as usize)?;
            let payload_end = reader.offset();
            let _ = node_start;
            nodes.push(CanonicalNodeHeader {
                transport_type,
                kind,
                aux0,
                aux1,
                child_count,
                payload_len,
            });
            payload_ranges.push(payload_start..payload_end);
        }
        let declared_handles = reader.len()? as u32;
        if declared_handles != handle_slot_count {
            bail!("canonical blob handle count header did not match encoded handle table");
        }
        let mut handles = Vec::with_capacity(handle_slot_count as usize);
        for _ in 0..handle_slot_count {
            handles.push(HandleSlot { type_id: TypeId(reader.u32()?), handle_id: reader.u32()? });
        }
        reader.ensure_finished()?;
        Ok(Self {
            bytes,
            header: CanonicalBlobHeader {
                transport_type,
                total_byte_len,
                node_count,
                handle_slot_count,
                flags,
            },
            root,
            nodes,
            handles,
            payload_ranges,
        })
    }

    pub fn header(&self) -> &CanonicalBlobHeader {
        &self.header
    }

    pub fn root(&self) -> &ValueRef {
        &self.root
    }

    pub fn nodes(&self) -> &[CanonicalNodeHeader] {
        &self.nodes
    }

    pub fn handles(&self) -> &[HandleSlot] {
        &self.handles
    }

    pub fn payload_bytes(&self, node: usize) -> Option<&'a [u8]> {
        self.payload_ranges.get(node).and_then(|range| self.bytes.get(range.clone()))
    }

    pub fn decode(&self) -> anyhow::Result<CanonicalGraph> {
        let mut reader = BinaryReader::new(self.bytes);
        let _ = reader.fixed::<8>()?;
        let _ = reader.u16()?;
        let _ = reader.u16()?;
        let _ = reader.u16()?;
        let _ = reader.u32()?;
        let _ = reader.u32()?;
        let _ = reader.u32()?;
        let node_count = reader.u32()?;
        let handle_count = reader.u32()?;
        let root = decode_value_ref(&mut reader)?;
        let mut nodes = Vec::with_capacity(node_count as usize);
        for _ in 0..node_count {
            nodes.push(decode_node(&mut reader)?);
        }
        let table_count = reader.len()? as u32;
        if table_count != handle_count {
            bail!("canonical blob handle table count changed during decode");
        }
        let mut handles = Vec::with_capacity(handle_count as usize);
        for _ in 0..handle_count {
            handles.push(HandleSlot { type_id: TypeId(reader.u32()?), handle_id: reader.u32()? });
        }
        Ok(CanonicalGraph { root, nodes, handles })
    }
}

fn encode_node(writer: &mut BinaryWriter, node: &CanonicalNode) -> anyhow::Result<()> {
    writer.u32(node.transport_type().0);
    writer.u8(match node.kind() {
        CanonicalNodeKind::String => 0,
        CanonicalNodeKind::Array => 1,
        CanonicalNodeKind::Tuple => 2,
        CanonicalNodeKind::Record => 3,
        CanonicalNodeKind::Struct => 4,
        CanonicalNodeKind::Enum => 5,
        CanonicalNodeKind::Union => 6,
        CanonicalNodeKind::Intersection => 7,
    });

    match node {
        CanonicalNode::String { value, .. } => {
            writer.u32(value.len() as u32);
            writer.u32(0);
            writer.u32(0);
            writer.u32(value.len() as u32);
            writer.bytes(value.as_bytes());
        }
        CanonicalNode::Array { elements, .. } => match elements {
            ArrayElements::PackedScalars { kind, len, bytes } => {
                writer.u32(*len);
                writer.u32(encode_packed_scalar_kind(*kind) as u32);
                writer.u32(0);
                writer.u32(bytes.len() as u32);
                writer.bytes(bytes);
            }
            ArrayElements::Values(values) => {
                writer.u32(values.len() as u32);
                writer.u32(u32::MAX);
                writer.u32(values.len() as u32);
                writer.u32(0);
                for value in values {
                    encode_value_ref(writer, value);
                }
            }
        },
        CanonicalNode::Tuple { fields, .. }
        | CanonicalNode::Record { fields, .. }
        | CanonicalNode::Struct { fields, .. } => {
            writer.u32(0);
            writer.u32(0);
            writer.u32(fields.len() as u32);
            writer.u32(0);
            for field in fields {
                encode_value_ref(writer, field);
            }
        }
        CanonicalNode::Enum { variant_index, fields, .. } => {
            writer.u32(*variant_index);
            writer.u32(0);
            writer.u32(fields.len() as u32);
            writer.u32(0);
            for field in fields {
                encode_value_ref(writer, field);
            }
        }
        CanonicalNode::Union { arm_index, payload, .. } => {
            writer.u32(*arm_index);
            writer.u32(0);
            writer.u32(1);
            writer.u32(0);
            encode_value_ref(writer, payload);
        }
        CanonicalNode::Intersection { carrier, facets, .. } => {
            writer.u32(facets.len() as u32);
            writer.u32(0);
            writer.u32(1 + facets.len() as u32);
            writer.u32(0);
            encode_value_ref(writer, carrier);
            for facet in facets {
                encode_value_ref(writer, facet);
            }
        }
    }
    Ok(())
}

fn decode_node(reader: &mut BinaryReader<'_>) -> anyhow::Result<CanonicalNode> {
    let transport_type = TypeId(reader.u32()?);
    let kind = decode_node_kind(reader.u8()?)?;
    let aux0 = reader.u32()?;
    let aux1 = reader.u32()?;
    let child_count = reader.u32()?;
    let payload_len = reader.u32()?;
    Ok(match kind {
        CanonicalNodeKind::String => {
            if child_count != 0 {
                bail!("string canonical node cannot have child refs");
            }
            let bytes = reader.bytes(payload_len as usize)?;
            CanonicalNode::String {
                transport_type,
                value: String::from_utf8(bytes.to_vec())
                    .map_err(|_error| anyhow!("canonical string payload was not valid UTF-8"))?,
            }
        }
        CanonicalNodeKind::Array => {
            if aux1 == u32::MAX {
                let mut values = Vec::with_capacity(child_count as usize);
                for _ in 0..child_count {
                    values.push(decode_value_ref(reader)?);
                }
                if payload_len != 0 {
                    let _ = reader.bytes(payload_len as usize)?;
                }
                CanonicalNode::Array { transport_type, elements: ArrayElements::Values(values) }
            } else {
                if child_count != 0 {
                    bail!("packed scalar array node cannot have child refs");
                }
                let bytes = reader.bytes(payload_len as usize)?.to_vec();
                CanonicalNode::Array {
                    transport_type,
                    elements: ArrayElements::PackedScalars {
                        kind: decode_packed_scalar_kind(aux1 as u8)?,
                        len: aux0,
                        bytes,
                    },
                }
            }
        }
        CanonicalNodeKind::Tuple => CanonicalNode::Tuple {
            transport_type,
            fields: decode_value_refs(reader, child_count as usize)?,
        },
        CanonicalNodeKind::Record => CanonicalNode::Record {
            transport_type,
            fields: decode_value_refs(reader, child_count as usize)?,
        },
        CanonicalNodeKind::Struct => CanonicalNode::Struct {
            transport_type,
            fields: decode_value_refs(reader, child_count as usize)?,
        },
        CanonicalNodeKind::Enum => CanonicalNode::Enum {
            transport_type,
            variant_index: aux0,
            fields: decode_value_refs(reader, child_count as usize)?,
        },
        CanonicalNodeKind::Union => {
            if child_count != 1 {
                bail!("canonical union node must encode exactly one payload ref");
            }
            CanonicalNode::Union {
                transport_type,
                arm_index: aux0,
                payload: decode_value_ref(reader)?,
            }
        }
        CanonicalNodeKind::Intersection => {
            if child_count == 0 {
                bail!("canonical intersection node must encode a carrier ref");
            }
            let carrier = decode_value_ref(reader)?;
            let facets = decode_value_refs(reader, child_count.saturating_sub(1) as usize)?;
            if aux0 != facets.len() as u32 {
                bail!("canonical intersection node facet count did not match aux data");
            }
            CanonicalNode::Intersection { transport_type, carrier, facets }
        }
    })
}

fn decode_value_refs(reader: &mut BinaryReader<'_>, len: usize) -> anyhow::Result<Vec<ValueRef>> {
    (0..len).map(|_| decode_value_ref(reader)).collect()
}

fn encode_value_ref(writer: &mut BinaryWriter, value: &ValueRef) {
    match value {
        ValueRef::InlineScalar(scalar) => {
            writer.u8(0);
            encode_scalar(writer, scalar);
        }
        ValueRef::NodeRef(id) => {
            writer.u8(1);
            writer.u32(id.0);
        }
        ValueRef::HandleRef(id) => {
            writer.u8(2);
            writer.u32(id.0);
        }
    }
}

fn decode_value_ref(reader: &mut BinaryReader<'_>) -> anyhow::Result<ValueRef> {
    Ok(match reader.u8()? {
        0 => ValueRef::InlineScalar(decode_scalar(reader)?),
        1 => ValueRef::NodeRef(NodeId(reader.u32()?)),
        2 => ValueRef::HandleRef(HandleSlotId(reader.u32()?)),
        value => bail!("unknown canonical value-ref tag `{value}`"),
    })
}

fn encode_scalar(writer: &mut BinaryWriter, scalar: &AbiScalar) {
    match scalar {
        AbiScalar::Unit => writer.u8(0),
        AbiScalar::Bool(value) => {
            writer.u8(1);
            writer.bool(*value);
        }
        AbiScalar::Int { signed, bits, value } => {
            writer.u8(2);
            writer.bool(*signed);
            writer.u16(*bits);
            writer.i64(*value);
        }
        AbiScalar::Float { bits, raw_bits } => {
            writer.u8(3);
            writer.u16(*bits);
            writer.u64(*raw_bits);
        }
        AbiScalar::Char { unicode_scalar } => {
            writer.u8(4);
            writer.u32(*unicode_scalar);
        }
        AbiScalar::EnumTag { type_id, variant_index } => {
            writer.u8(5);
            writer.u32(type_id.0);
            writer.u32(*variant_index);
        }
    }
}

fn decode_scalar(reader: &mut BinaryReader<'_>) -> anyhow::Result<AbiScalar> {
    Ok(match reader.u8()? {
        0 => AbiScalar::Unit,
        1 => AbiScalar::Bool(reader.bool()?),
        2 => AbiScalar::Int { signed: reader.bool()?, bits: reader.u16()?, value: reader.i64()? },
        3 => AbiScalar::Float { bits: reader.u16()?, raw_bits: reader.u64()? },
        4 => AbiScalar::Char { unicode_scalar: reader.u32()? },
        5 => AbiScalar::EnumTag { type_id: TypeId(reader.u32()?), variant_index: reader.u32()? },
        value => bail!("unknown abi scalar tag `{value}`"),
    })
}

fn encode_packed_scalar_kind(kind: PackedScalarKind) -> u8 {
    match kind {
        PackedScalarKind::Bool => 0,
        PackedScalarKind::I32 => 1,
        PackedScalarKind::I64 => 2,
        PackedScalarKind::F32 => 3,
        PackedScalarKind::F64 => 4,
        PackedScalarKind::Char => 5,
    }
}

fn decode_packed_scalar_kind(tag: u8) -> anyhow::Result<PackedScalarKind> {
    Ok(match tag {
        0 => PackedScalarKind::Bool,
        1 => PackedScalarKind::I32,
        2 => PackedScalarKind::I64,
        3 => PackedScalarKind::F32,
        4 => PackedScalarKind::F64,
        5 => PackedScalarKind::Char,
        value => bail!("unknown packed scalar kind `{value}`"),
    })
}

fn decode_node_kind(tag: u8) -> anyhow::Result<CanonicalNodeKind> {
    Ok(match tag {
        0 => CanonicalNodeKind::String,
        1 => CanonicalNodeKind::Array,
        2 => CanonicalNodeKind::Tuple,
        3 => CanonicalNodeKind::Record,
        4 => CanonicalNodeKind::Struct,
        5 => CanonicalNodeKind::Enum,
        6 => CanonicalNodeKind::Union,
        7 => CanonicalNodeKind::Intersection,
        value => bail!("unknown canonical node kind `{value}`"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_value() -> AbiValue {
        AbiValue::Canonical {
            transport_type: TypeId(7),
            graph: CanonicalGraph {
                root: ValueRef::NodeRef(NodeId(2)),
                nodes: vec![
                    CanonicalNode::String { transport_type: TypeId(1), value: "hello".to_owned() },
                    CanonicalNode::Array {
                        transport_type: TypeId(2),
                        elements: ArrayElements::PackedScalars {
                            kind: PackedScalarKind::I32,
                            len: 2,
                            bytes: [20i32.to_le_bytes(), 22i32.to_le_bytes()].concat(),
                        },
                    },
                    CanonicalNode::Struct {
                        transport_type: TypeId(7),
                        fields: vec![ValueRef::NodeRef(NodeId(0)), ValueRef::NodeRef(NodeId(1))],
                    },
                ],
                handles: Vec::new(),
            },
        }
    }

    #[test]
    fn canonical_blob_round_trips() {
        let value = sample_value();
        let bytes = encode_canonical_blob(&value).expect("canonical blob should encode");
        let decoded = decode_canonical_blob(&bytes).expect("canonical blob should decode");
        assert_eq!(decoded, value);
    }

    #[test]
    fn canonical_blob_round_trips_handle_slots() {
        let value = AbiValue::Canonical {
            transport_type: TypeId(9),
            graph: CanonicalGraph {
                root: ValueRef::HandleRef(HandleSlotId(0)),
                nodes: Vec::new(),
                handles: vec![HandleSlot { type_id: TypeId(3), handle_id: 77 }],
            },
        };

        let bytes = encode_canonical_blob(&value).expect("canonical blob with handles");
        let view = CanonicalBlobView::parse(&bytes).expect("view should parse");
        assert_eq!(view.handles(), &[HandleSlot { type_id: TypeId(3), handle_id: 77 }]);
        let decoded = decode_canonical_blob(&bytes).expect("canonical blob should decode");
        assert_eq!(decoded, value);
    }

    #[test]
    fn canonical_blob_view_exposes_payload_nodes() {
        let value = sample_value();
        let bytes = encode_canonical_blob(&value).expect("canonical blob should encode");
        let view = CanonicalBlobView::parse(&bytes).expect("view should parse");
        assert_eq!(view.nodes().len(), 3);
        assert_eq!(view.payload_bytes(0).expect("string payload"), b"hello");
    }

    #[test]
    fn packed_scalar_arrays_round_trip() {
        let value = AbiValue::Canonical {
            transport_type: TypeId(3),
            graph: CanonicalGraph {
                root: ValueRef::NodeRef(NodeId(0)),
                nodes: vec![CanonicalNode::Array {
                    transport_type: TypeId(3),
                    elements: ArrayElements::PackedScalars {
                        kind: PackedScalarKind::Bool,
                        len: 3,
                        bytes: vec![1, 0, 1],
                    },
                }],
                handles: Vec::new(),
            },
        };
        let decoded =
            decode_canonical_blob(&encode_canonical_blob(&value).expect("encode")).expect("decode");
        assert_eq!(decoded, value);
    }

    #[test]
    fn canonical_blob_round_trips_union_nodes() {
        let value = AbiValue::Canonical {
            transport_type: TypeId(11),
            graph: CanonicalGraph {
                root: ValueRef::NodeRef(NodeId(0)),
                nodes: vec![CanonicalNode::Union {
                    transport_type: TypeId(11),
                    arm_index: 1,
                    payload: ValueRef::InlineScalar(AbiScalar::Int {
                        signed: true,
                        bits: 32,
                        value: 42,
                    }),
                }],
                handles: Vec::new(),
            },
        };

        let decoded =
            decode_canonical_blob(&encode_canonical_blob(&value).expect("encode")).expect("decode");
        assert_eq!(decoded, value);
    }

    #[test]
    fn canonical_blob_round_trips_intersection_nodes() {
        let value = AbiValue::Canonical {
            transport_type: TypeId(12),
            graph: CanonicalGraph {
                root: ValueRef::NodeRef(NodeId(0)),
                nodes: vec![CanonicalNode::Intersection {
                    transport_type: TypeId(12),
                    carrier: ValueRef::InlineScalar(AbiScalar::Int {
                        signed: true,
                        bits: 32,
                        value: 7,
                    }),
                    facets: vec![ValueRef::HandleRef(HandleSlotId(0))],
                }],
                handles: vec![HandleSlot { type_id: TypeId(9), handle_id: 44 }],
            },
        };

        let decoded =
            decode_canonical_blob(&encode_canonical_blob(&value).expect("encode")).expect("decode");
        assert_eq!(decoded, value);
    }
}

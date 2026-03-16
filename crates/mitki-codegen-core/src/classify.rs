use mitki_hir::ty::{ExactInt, Ty, TyKind};
use mitki_lower::item::scope::{enum_variants, struct_fields};
use mitki_resolve::{RuntimeFunction, RuntimeTy};
use mitki_span::Symbol;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::plumbing::AsId as _;

use crate::layout::{
    ARC_HEADER_SIZE, ARRAY_HEADER_SIZE, AggregateKind, AggregateLayout, ArrayRuntimeLayout,
    EnumLayout, VariantLayout, abi_layout, align_to, layout_fields, symbol_bits,
};
use crate::stage_intrinsic::StageIntrinsic;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum WasmValType {
    I32,
    I64,
    F32,
    F64,
    V128,
    Ref,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum WasmBlockType {
    Empty,
    Result(WasmValType),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RefKind {
    String,
    Opaque,
    Array(u32),
    Nominal(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BackendTy {
    Int,
    I64,
    Bool,
    Float,
    Char,
    Ref(RefKind),
    Unit,
}

impl BackendTy {
    pub fn value_type(self) -> Option<WasmValType> {
        match self {
            BackendTy::Int | BackendTy::Bool | BackendTy::Char | BackendTy::Ref(_) => {
                Some(WasmValType::I32)
            }
            BackendTy::I64 => Some(WasmValType::I64),
            BackendTy::Float => Some(WasmValType::F64),
            BackendTy::Unit => None,
        }
    }

    pub fn block_type(self) -> WasmBlockType {
        self.value_type().map_or(WasmBlockType::Empty, WasmBlockType::Result)
    }

    pub fn is_heap_ref(self) -> bool {
        matches!(self, Self::Ref(_))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionSignature {
    pub params: Vec<AbiTy>,
    pub result: AbiTy,
}

impl FunctionSignature {
    #[allow(dead_code)]
    pub fn wasm_results(&self) -> Vec<WasmValType> {
        match self.result {
            AbiTy::Scalar(result) => result.value_type().into_iter().collect(),
            AbiTy::Aggregate(_) => Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn wasm_params_with_env(&self) -> Vec<WasmValType> {
        let mut params =
            Vec::with_capacity(self.params.len() + usize::from(self.result.is_aggregate()) + 1);
        params.push(WasmValType::I32);
        if self.result.is_aggregate() {
            params.push(WasmValType::I32);
        }

        for ty in &self.params {
            match ty {
                AbiTy::Scalar(ty) => {
                    if let Some(value_type) = ty.value_type() {
                        params.push(value_type);
                    }
                }
                AbiTy::Aggregate(_) => params.push(WasmValType::I32),
            }
        }

        params
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AbiTy {
    Scalar(BackendTy),
    Aggregate(Box<AggregateLayout>),
}

impl AbiTy {
    pub fn is_aggregate(&self) -> bool {
        matches!(self, Self::Aggregate(_))
    }

    pub fn aggregate(&self) -> Option<&AggregateLayout> {
        let Self::Aggregate(layout) = self else {
            return None;
        };
        Some(layout)
    }

    pub fn contains_heap_refs(&self) -> bool {
        match self {
            Self::Scalar(ty) => ty.is_heap_ref(),
            Self::Aggregate(layout) => layout.contains_heap_refs(),
        }
    }
}

pub fn exact_int_backend_ty(int_ty: ExactInt) -> BackendTy {
    if int_ty.bits() <= 32 { BackendTy::Int } else { BackendTy::I64 }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueShapeFailure<'db> {
    Unsupported(Ty<'db>),
    Recursive(Ty<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValueShapeField<'db> {
    pub name: Option<Symbol<'db>>,
    pub ty: Ty<'db>,
    pub shape: Box<ValueShape<'db>>,
    pub runtime_offset: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValueShapeVariant<'db> {
    pub name: Symbol<'db>,
    pub tag: i32,
    pub fields: Vec<ValueShapeField<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldsShape<'db> {
    pub fields: Vec<ValueShapeField<'db>>,
    pub runtime_layout: AggregateLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumShape<'db> {
    pub variants: Vec<ValueShapeVariant<'db>>,
    pub runtime_layout: AggregateLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UnionShape<'db> {
    pub members: Vec<ValueShape<'db>>,
    pub runtime_layout: AggregateLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArrayShape<'db> {
    pub item_ty: Ty<'db>,
    pub item: Box<ValueShape<'db>>,
    pub layout: ArrayRuntimeLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionValueShape<'db> {
    pub params: Vec<ValueShape<'db>>,
    pub result: Box<ValueShape<'db>>,
    pub signature: FunctionSignature,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueShapeKind<'db> {
    Unit,
    Int,
    Bool,
    Float,
    Char,
    String,
    Array(ArrayShape<'db>),
    Tuple(FieldsShape<'db>),
    Record(FieldsShape<'db>),
    Struct(FieldsShape<'db>),
    Enum(EnumShape<'db>),
    Union(UnionShape<'db>),
    Function(FunctionValueShape<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValueShape<'db> {
    pub ty: Ty<'db>,
    pub kind: ValueShapeKind<'db>,
    pub runtime_abi: AbiTy,
}

pub fn stage_intrinsic_signature(intrinsic: StageIntrinsic) -> FunctionSignature {
    match intrinsic {
        StageIntrinsic::TypeName => FunctionSignature {
            params: vec![AbiTy::Scalar(BackendTy::Int)],
            result: AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
        },
        StageIntrinsic::FieldCount
        | StageIntrinsic::VariantCount
        | StageIntrinsic::FunctionParamCount => FunctionSignature {
            params: vec![AbiTy::Scalar(BackendTy::Int)],
            result: AbiTy::Scalar(BackendTy::Int),
        },
        StageIntrinsic::FieldName
        | StageIntrinsic::VariantName
        | StageIntrinsic::FunctionParamTypeName => FunctionSignature {
            params: vec![AbiTy::Scalar(BackendTy::Int), AbiTy::Scalar(BackendTy::Int)],
            result: AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
        },
        StageIntrinsic::FunctionReturnTypeName => FunctionSignature {
            params: vec![AbiTy::Scalar(BackendTy::Int)],
            result: AbiTy::Scalar(BackendTy::Ref(RefKind::String)),
        },
    }
}

pub fn runtime_function_signature(function: RuntimeFunction) -> FunctionSignature {
    let params =
        function.params().iter().map(|ty| AbiTy::Scalar(runtime_ty_to_backend_ty(*ty))).collect();
    let result = AbiTy::Scalar(runtime_ty_to_backend_ty(function.result()));
    FunctionSignature { params, result }
}

pub fn runtime_ty_to_backend_ty(ty: RuntimeTy) -> BackendTy {
    match ty {
        RuntimeTy::Int => BackendTy::Int,
        RuntimeTy::Str => BackendTy::Ref(RefKind::String),
        RuntimeTy::Unit => BackendTy::Unit,
    }
}

#[allow(dead_code)]
pub fn classify_ty(db: &dyn salsa::Database, ty: Ty<'_>) -> Option<BackendTy> {
    match abi_ty(db, ty)? {
        AbiTy::Scalar(ty) => Some(ty),
        AbiTy::Aggregate(_) => None,
    }
}

pub fn ty_bits(ty: Ty<'_>) -> u32 {
    u32::try_from(ty.as_id().as_bits()).expect("type id should fit into u32")
}

pub fn nominal_ty_bits(ty: Ty<'_>) -> u32 {
    ty_bits(ty)
}

pub fn array_ty_bits(ty: Ty<'_>) -> u32 {
    ty_bits(ty)
}

pub fn value_shape<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Result<ValueShape<'db>, ValueShapeFailure<'db>> {
    build_value_shape(db, ty, &mut FxHashMap::default(), &mut FxHashSet::default())
}

pub fn supported_value_shape<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Option<ValueShape<'db>> {
    value_shape(db, ty).ok()
}

pub fn abi_ty(db: &dyn salsa::Database, ty: Ty<'_>) -> Option<AbiTy> {
    Some(supported_value_shape(db, ty)?.runtime_abi)
}

pub fn function_value_abi_ty() -> AbiTy {
    AbiTy::Aggregate(Box::new(function_value_layout()))
}

pub fn function_value_layout() -> AggregateLayout {
    AggregateLayout { size: 8, align: 4, kind: AggregateKind::FunctionValue }
}

fn build_value_shape<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    cache: &mut FxHashMap<u64, ValueShape<'db>>,
    active: &mut FxHashSet<u64>,
) -> Result<ValueShape<'db>, ValueShapeFailure<'db>> {
    let bits = ty.as_id().as_bits();
    if let Some(shape) = cache.get(&bits) {
        return Ok(shape.clone());
    }
    if !active.insert(bits) {
        return Err(ValueShapeFailure::Recursive(ty));
    }

    let shape = match ty.kind(db) {
        TyKind::Tuple(items) if items.is_empty() => {
            scalar_shape(ty, ValueShapeKind::Unit, BackendTy::Unit)
        }
        TyKind::Int => scalar_shape(ty, ValueShapeKind::Int, BackendTy::Int),
        TyKind::ExactInt(int_ty) => {
            scalar_shape(ty, ValueShapeKind::Int, exact_int_backend_ty(*int_ty))
        }
        TyKind::Bool => scalar_shape(ty, ValueShapeKind::Bool, BackendTy::Bool),
        TyKind::Float => scalar_shape(ty, ValueShapeKind::Float, BackendTy::Float),
        TyKind::Char => scalar_shape(ty, ValueShapeKind::Char, BackendTy::Char),
        TyKind::String => scalar_shape(ty, ValueShapeKind::String, BackendTy::Ref(RefKind::String)),
        TyKind::Pointer { .. } => scalar_shape(ty, ValueShapeKind::Int, BackendTy::Int),
        TyKind::Array(item_ty) => {
            let item = build_value_shape(db, *item_ty, cache, active)?;
            let layout = array_runtime_layout_from_item_abi(ty, item.runtime_abi.clone())
                .ok_or(ValueShapeFailure::Unsupported(ty))?;
            ValueShape {
                ty,
                runtime_abi: AbiTy::Scalar(BackendTy::Ref(RefKind::Array(array_ty_bits(ty)))),
                kind: ValueShapeKind::Array(ArrayShape {
                    item_ty: *item_ty,
                    item: Box::new(item),
                    layout,
                }),
            }
        }
        TyKind::Tuple(items) => {
            let fields = items.iter().map(|&item_ty| (None, item_ty)).collect::<Vec<_>>();
            let shape = build_fields_shape(db, ty, fields, cache, active)?;
            let runtime_abi = AbiTy::Aggregate(Box::new(shape.runtime_layout.clone()));
            ValueShape { ty, kind: ValueShapeKind::Tuple(shape), runtime_abi }
        }
        TyKind::Record(fields) => {
            let mut ordered = fields.clone();
            ordered.sort_by_key(|(name, _)| name.text(db).to_owned());
            let fields =
                ordered.into_iter().map(|(name, field_ty)| (Some(name), field_ty)).collect();
            let shape = build_fields_shape(db, ty, fields, cache, active)?;
            let runtime_abi = AbiTy::Aggregate(Box::new(shape.runtime_layout.clone()));
            ValueShape { ty, kind: ValueShapeKind::Record(shape), runtime_abi }
        }
        TyKind::ExternStruct(struct_ty) => {
            let fields = struct_fields(db, *struct_ty)
                .iter()
                .map(|(name, field_ty)| (Some(*name), *field_ty))
                .collect::<Vec<_>>();
            let shape = build_fields_shape(db, ty, fields, cache, active)?;
            let runtime_abi = AbiTy::Aggregate(Box::new(shape.runtime_layout.clone()));
            ValueShape { ty, kind: ValueShapeKind::Record(shape), runtime_abi }
        }
        TyKind::Struct(struct_ty) => {
            let fields = struct_fields(db, *struct_ty)
                .iter()
                .map(|(name, field_ty)| (Some(*name), *field_ty))
                .collect::<Vec<_>>();
            let shape = build_fields_shape(db, ty, fields, cache, active)?;
            ValueShape {
                ty,
                runtime_abi: AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(nominal_ty_bits(ty)))),
                kind: ValueShapeKind::Struct(shape),
            }
        }
        TyKind::Enum(enum_ty) => {
            let variants = enum_variants(db, *enum_ty)
                .iter()
                .enumerate()
                .map(|(tag, (name, fields))| (*name, tag as i32, fields.clone()))
                .collect::<Vec<_>>();
            let shape = build_enum_shape(db, ty, variants, cache, active)?;
            ValueShape {
                ty,
                runtime_abi: AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(nominal_ty_bits(ty)))),
                kind: ValueShapeKind::Enum(shape),
            }
        }
        TyKind::Union(items) => {
            let shape = build_union_shape(db, ty, items.clone(), cache, active)?;
            let runtime_abi = AbiTy::Aggregate(Box::new(shape.runtime_layout.clone()));
            ValueShape { ty, kind: ValueShapeKind::Union(shape), runtime_abi }
        }
        TyKind::Function { inputs, output } => {
            let params = inputs
                .iter()
                .map(|&input| build_value_shape(db, input, cache, active))
                .collect::<Result<Vec<_>, _>>()?;
            let result = Box::new(build_value_shape(db, *output, cache, active)?);
            let signature = FunctionSignature {
                params: params.iter().map(|shape| shape.runtime_abi.clone()).collect(),
                result: result.runtime_abi.clone(),
            };
            ValueShape {
                ty,
                runtime_abi: function_value_abi_ty(),
                kind: ValueShapeKind::Function(FunctionValueShape { params, result, signature }),
            }
        }
        TyKind::Inter(_) | TyKind::Unknown | TyKind::Var(_) => {
            return Err(ValueShapeFailure::Unsupported(ty));
        }
        TyKind::Rec(_, _) => return Err(ValueShapeFailure::Recursive(ty)),
    };

    active.remove(&bits);
    cache.insert(bits, shape.clone());
    Ok(shape)
}

fn scalar_shape<'db>(
    ty: Ty<'db>,
    kind: ValueShapeKind<'db>,
    backend_ty: BackendTy,
) -> ValueShape<'db> {
    let abi = AbiTy::Scalar(backend_ty);
    ValueShape { ty, kind, runtime_abi: abi }
}

fn build_fields_shape<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    declared_fields: Vec<(Option<Symbol<'db>>, Ty<'db>)>,
    cache: &mut FxHashMap<u64, ValueShape<'db>>,
    active: &mut FxHashSet<u64>,
) -> Result<FieldsShape<'db>, ValueShapeFailure<'db>> {
    let built = declared_fields
        .into_iter()
        .map(|(name, field_ty)| {
            build_value_shape(db, field_ty, cache, active).map(|shape| (name, field_ty, shape))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let runtime_layout = aggregate_layout_from_fields(
        built.iter().map(|(name, _, shape)| (name.map(symbol_bits), shape.runtime_abi.clone())),
    )
    .ok_or(ValueShapeFailure::Unsupported(ty))?;
    let runtime_fields = runtime_layout.fields().unwrap_or(&[]);
    let fields = built
        .into_iter()
        .zip(runtime_fields.iter())
        .map(|((name, field_ty, shape), runtime_field)| ValueShapeField {
            name,
            ty: field_ty,
            shape: Box::new(shape),
            runtime_offset: runtime_field.offset,
        })
        .collect();
    Ok(FieldsShape { fields, runtime_layout })
}

fn build_enum_shape<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    declared_variants: Vec<(Symbol<'db>, i32, Vec<Ty<'db>>)>,
    cache: &mut FxHashMap<u64, ValueShape<'db>>,
    active: &mut FxHashSet<u64>,
) -> Result<EnumShape<'db>, ValueShapeFailure<'db>> {
    let built_variants = declared_variants
        .into_iter()
        .map(|(name, tag, field_tys)| {
            let fields = field_tys
                .into_iter()
                .map(|field_ty| {
                    build_value_shape(db, field_ty, cache, active).map(|shape| (field_ty, shape))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok::<_, ValueShapeFailure<'db>>((name, tag, fields))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let runtime_layout =
        enum_layout_from_variants(built_variants.iter().map(|(name, tag, fields)| {
            (
                symbol_bits(*name),
                *tag,
                fields.iter().map(|(_, shape)| shape.runtime_abi.clone()).collect::<Vec<_>>(),
            )
        }))
        .ok_or(ValueShapeFailure::Unsupported(ty))?;
    let AggregateKind::Enum(runtime_enum_layout) = &runtime_layout.kind else {
        return Err(ValueShapeFailure::Unsupported(ty));
    };
    let variants = built_variants
        .into_iter()
        .zip(runtime_enum_layout.variants.iter())
        .map(|((name, tag, fields), runtime_variant)| ValueShapeVariant {
            name,
            tag,
            fields: fields
                .into_iter()
                .zip(runtime_variant.fields.iter())
                .map(|((field_ty, shape), runtime_field)| ValueShapeField {
                    name: None,
                    ty: field_ty,
                    shape: Box::new(shape),
                    runtime_offset: runtime_field.offset,
                })
                .collect(),
        })
        .collect();

    Ok(EnumShape { variants, runtime_layout })
}

fn build_union_shape<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
    members: Vec<Ty<'db>>,
    cache: &mut FxHashMap<u64, ValueShape<'db>>,
    active: &mut FxHashSet<u64>,
) -> Result<UnionShape<'db>, ValueShapeFailure<'db>> {
    let members = members
        .into_iter()
        .map(|member_ty| build_value_shape(db, member_ty, cache, active))
        .collect::<Result<Vec<_>, _>>()?;
    let runtime_layout =
        enum_layout_from_variants(members.iter().enumerate().map(|(index, shape)| {
            ((index as u64) + 1, index as i32, vec![shape.runtime_abi.clone()])
        }))
        .ok_or(ValueShapeFailure::Unsupported(ty))?;
    Ok(UnionShape { members, runtime_layout })
}

fn aggregate_layout_from_fields(
    fields: impl IntoIterator<Item = (Option<u64>, AbiTy)>,
) -> Option<AggregateLayout> {
    let layout = layout_fields(fields.into_iter().map(|(name_bits, abi)| (name_bits, Some(abi))))?;
    Some(AggregateLayout {
        size: layout.size,
        align: layout.align,
        kind: AggregateKind::Fields(layout.fields),
    })
}

fn enum_layout_from_variants(
    variants: impl IntoIterator<Item = (u64, i32, Vec<AbiTy>)>,
) -> Option<AggregateLayout> {
    let mut payload_size = 0u32;
    let mut payload_align = 1u32;
    let mut variant_layouts = Vec::new();

    for (name_bits, tag, fields) in variants {
        let payload = layout_fields(fields.into_iter().map(|ty| (None, Some(ty))))?;
        payload_size = payload_size.max(payload.size);
        payload_align = payload_align.max(payload.align);
        variant_layouts.push((name_bits, tag, payload));
    }

    let payload_offset = align_to(4, payload_align);
    let align = 4u32.max(payload_align);
    let variants = variant_layouts
        .into_iter()
        .map(|(name_bits, tag, payload)| VariantLayout {
            name_bits,
            tag,
            fields: payload
                .fields
                .into_iter()
                .map(|mut field| {
                    field.offset += payload_offset;
                    field
                })
                .collect(),
        })
        .collect();
    Some(AggregateLayout {
        size: align_to(payload_offset + payload_size, align),
        align,
        kind: AggregateKind::Enum(EnumLayout { payload_offset, variants }),
    })
}

fn array_runtime_layout_from_item_abi(ty: Ty<'_>, item_abi: AbiTy) -> Option<ArrayRuntimeLayout> {
    let item_layout = abi_layout(&item_abi)?;
    let item_align = item_layout.align.max(1);
    let item_size = item_layout.size;
    let item_stride = align_to(item_size, item_align);
    let data_offset = align_to(ARC_HEADER_SIZE + ARRAY_HEADER_SIZE, item_align) - ARC_HEADER_SIZE;
    Some(ArrayRuntimeLayout {
        type_bits: array_ty_bits(ty),
        item_abi,
        item_size,
        item_align,
        item_stride,
        data_offset,
    })
}

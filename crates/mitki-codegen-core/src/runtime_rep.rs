use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::{enum_variants, struct_fields};
use mitki_span::Symbol;
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::plumbing::AsId as _;

use crate::classify::{
    AbiTy, BackendTy, FunctionSignature, RefKind, array_ty_bits, exact_int_backend_ty,
    function_value_abi_ty, nominal_ty_bits, ty_bits,
};
use crate::layout::{
    ARC_HEADER_SIZE, ARRAY_HEADER_SIZE, AggregateKind, AggregateLayout, ArrayRuntimeLayout,
    EnumLayout, VariantLayout, abi_layout, align_to, layout_fields, symbol_bits,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RuntimeRepId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecursiveRuntimeRepGroupId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeRepField<'db> {
    pub name: Option<Symbol<'db>>,
    pub ty: Ty<'db>,
    pub rep: RuntimeRepId,
    pub runtime_offset: u32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeRepVariant<'db> {
    pub name: Symbol<'db>,
    pub tag: i32,
    pub fields: Vec<RuntimeRepField<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeFieldsRep<'db> {
    pub fields: Vec<RuntimeRepField<'db>>,
    pub runtime_layout: AggregateLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeEnumRep<'db> {
    pub variants: Vec<RuntimeRepVariant<'db>>,
    pub runtime_layout: AggregateLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeUnionRep {
    pub members: Vec<RuntimeRepId>,
    pub runtime_layout: AggregateLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeIntersectionRep {
    pub carrier: RuntimeRepId,
    pub members: Vec<RuntimeRepId>,
    pub runtime_layout: Option<AggregateLayout>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeArrayRep<'db> {
    pub item_ty: Ty<'db>,
    pub item: RuntimeRepId,
    pub layout: ArrayRuntimeLayout,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeFunctionRep {
    pub params: Vec<RuntimeRepId>,
    pub result: RuntimeRepId,
    pub signature: FunctionSignature,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RuntimeRepKind<'db> {
    Pending,
    Unit,
    Int,
    Bool,
    Float,
    Char,
    String,
    Array(RuntimeArrayRep<'db>),
    Tuple(RuntimeFieldsRep<'db>),
    Record(RuntimeFieldsRep<'db>),
    ExternStruct(RuntimeFieldsRep<'db>),
    Struct(RuntimeFieldsRep<'db>),
    Enum(RuntimeEnumRep<'db>),
    Union(RuntimeUnionRep),
    Intersection(RuntimeIntersectionRep),
    Function(RuntimeFunctionRep),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeRepNode<'db> {
    pub id: RuntimeRepId,
    pub ty: Ty<'db>,
    pub kind: RuntimeRepKind<'db>,
    pub runtime_abi: AbiTy,
    pub recursive_group: Option<RecursiveRuntimeRepGroupId>,
    pub contains_function: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeRepGraph<'db> {
    pub nodes: Vec<RuntimeRepNode<'db>>,
    pub recursive_groups: Vec<Vec<RuntimeRepId>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeRepDescriptor<'db> {
    graph: RuntimeRepGraph<'db>,
    root: RuntimeRepId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RuntimeRepFailure<'db> {
    Unsupported(Ty<'db>),
    Recursive(Ty<'db>),
}

impl<'db> RuntimeRepDescriptor<'db> {
    pub fn build(
        db: &'db dyn salsa::Database,
        ty: Ty<'db>,
    ) -> Result<Self, RuntimeRepFailure<'db>> {
        let mut builder = RuntimeRepBuilder::new(db);
        let root = builder.intern_type(ty, &mut FxHashMap::default())?;
        builder.populate_recursive_groups();
        builder.populate_contains_function()?;
        Ok(Self { graph: builder.graph, root })
    }

    pub fn root_node(&self) -> &RuntimeRepNode<'db> {
        self.node(self.root)
    }

    pub fn node(&self, id: RuntimeRepId) -> &RuntimeRepNode<'db> {
        &self.graph.nodes[id.0 as usize]
    }

    pub fn runtime_abi(&self) -> AbiTy {
        self.root_node().runtime_abi.clone()
    }

    pub fn array_layout(&self) -> Option<ArrayRuntimeLayout> {
        let RuntimeRepKind::Array(array) = &self.root_node().kind else {
            return None;
        };
        Some(array.layout.clone())
    }

    pub fn function_signature(&self) -> Option<FunctionSignature> {
        let RuntimeRepKind::Function(function) = &self.root_node().kind else {
            return None;
        };
        Some(function.signature.clone())
    }

    pub fn runtime_nominal_payload_layout(&self) -> Option<AggregateLayout> {
        match &self.root_node().kind {
            RuntimeRepKind::Struct(fields) => Some(fields.runtime_layout.clone()),
            RuntimeRepKind::Enum(shape) => Some(shape.runtime_layout.clone()),
            _ => None,
        }
    }
}

pub fn runtime_rep_descriptor<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Result<RuntimeRepDescriptor<'db>, RuntimeRepFailure<'db>> {
    RuntimeRepDescriptor::build(db, ty)
}

struct RuntimeRepBuilder<'db> {
    db: &'db dyn salsa::Database,
    graph: RuntimeRepGraph<'db>,
    cache: FxHashMap<u64, RuntimeRepId>,
    active: FxHashSet<u64>,
}

impl<'db> RuntimeRepBuilder<'db> {
    fn new(db: &'db dyn salsa::Database) -> Self {
        Self {
            db,
            graph: RuntimeRepGraph { nodes: Vec::new(), recursive_groups: Vec::new() },
            cache: FxHashMap::default(),
            active: FxHashSet::default(),
        }
    }

    fn intern_type(
        &mut self,
        ty: Ty<'db>,
        recursive_bindings: &mut FxHashMap<u32, RuntimeRepId>,
    ) -> Result<RuntimeRepId, RuntimeRepFailure<'db>> {
        if let TyKind::Var(id) = ty.kind(self.db) {
            return recursive_bindings.get(id).copied().ok_or(RuntimeRepFailure::Unsupported(ty));
        }

        let bits = ty.as_id().as_bits();
        if let Some(&id) = self.cache.get(&bits) {
            return Ok(id);
        }

        if let TyKind::Rec(id, body) = ty.kind(self.db) {
            let placeholder =
                self.placeholder_runtime_abi(*body).ok_or(RuntimeRepFailure::Recursive(ty))?;
            let rep_id = self.push_pending_node(ty, placeholder);
            self.cache.insert(bits, rep_id);
            let previous = recursive_bindings.insert(*id, rep_id);
            let (kind, runtime_abi) = self.build_kind(*body, recursive_bindings)?;
            if let Some(previous) = previous {
                recursive_bindings.insert(*id, previous);
            } else {
                recursive_bindings.remove(id);
            }
            let node = &mut self.graph.nodes[rep_id.0 as usize];
            node.kind = kind;
            node.runtime_abi = runtime_abi;
            return Ok(rep_id);
        }

        if self.active.contains(&bits) {
            return Err(RuntimeRepFailure::Recursive(ty));
        }

        if let Some(placeholder) = self.placeholder_runtime_abi(ty) {
            let rep_id = self.push_pending_node(ty, placeholder);
            self.cache.insert(bits, rep_id);
            self.active.insert(bits);
            let build = self.build_kind(ty, recursive_bindings);
            self.active.remove(&bits);
            let (kind, runtime_abi) = build?;
            let node = &mut self.graph.nodes[rep_id.0 as usize];
            node.kind = kind;
            node.runtime_abi = runtime_abi;
            return Ok(rep_id);
        }

        self.active.insert(bits);
        let build = self.build_kind(ty, recursive_bindings);
        self.active.remove(&bits);
        let (kind, runtime_abi) = build?;
        let rep_id = RuntimeRepId(self.graph.nodes.len() as u32);
        self.graph.nodes.push(RuntimeRepNode {
            id: rep_id,
            ty,
            kind,
            runtime_abi,
            recursive_group: None,
            contains_function: false,
        });
        self.cache.insert(bits, rep_id);
        Ok(rep_id)
    }

    fn push_pending_node(&mut self, ty: Ty<'db>, runtime_abi: AbiTy) -> RuntimeRepId {
        let rep_id = RuntimeRepId(self.graph.nodes.len() as u32);
        self.graph.nodes.push(RuntimeRepNode {
            id: rep_id,
            ty,
            kind: RuntimeRepKind::Pending,
            runtime_abi,
            recursive_group: None,
            contains_function: false,
        });
        rep_id
    }

    fn build_kind(
        &mut self,
        ty: Ty<'db>,
        recursive_bindings: &mut FxHashMap<u32, RuntimeRepId>,
    ) -> Result<(RuntimeRepKind<'db>, AbiTy), RuntimeRepFailure<'db>> {
        Ok(match ty.kind(self.db) {
            TyKind::Tuple(items) if items.is_empty() => {
                (RuntimeRepKind::Unit, AbiTy::Scalar(BackendTy::Unit))
            }
            TyKind::Int => (RuntimeRepKind::Int, AbiTy::Scalar(BackendTy::Int)),
            TyKind::ExactInt(int_ty) => {
                (RuntimeRepKind::Int, AbiTy::Scalar(exact_int_backend_ty(*int_ty)))
            }
            TyKind::Bool => (RuntimeRepKind::Bool, AbiTy::Scalar(BackendTy::Bool)),
            TyKind::Float => (RuntimeRepKind::Float, AbiTy::Scalar(BackendTy::Float)),
            TyKind::Char => (RuntimeRepKind::Char, AbiTy::Scalar(BackendTy::Char)),
            TyKind::String => {
                (RuntimeRepKind::String, AbiTy::Scalar(BackendTy::Ref(RefKind::String)))
            }
            TyKind::Pointer { .. } => (RuntimeRepKind::Int, AbiTy::Scalar(BackendTy::Int)),
            TyKind::Array(item_ty) => {
                let item = self.intern_type(*item_ty, recursive_bindings)?;
                let item_abi = self.graph.nodes[item.0 as usize].runtime_abi.clone();
                let layout = array_runtime_layout_from_item_abi(ty, item_abi.clone())
                    .ok_or(RuntimeRepFailure::Unsupported(ty))?;
                (
                    RuntimeRepKind::Array(RuntimeArrayRep { item_ty: *item_ty, item, layout }),
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Array(array_ty_bits(ty)))),
                )
            }
            TyKind::Tuple(items) => {
                let rep = self.build_fields_rep(
                    ty,
                    items.iter().copied().map(|field_ty| (None, field_ty)),
                    recursive_bindings,
                )?;
                let runtime_abi = AbiTy::Aggregate(Box::new(rep.runtime_layout.clone()));
                (RuntimeRepKind::Tuple(rep), runtime_abi)
            }
            TyKind::Record(fields) => {
                let mut ordered = fields.clone();
                ordered.sort_by_key(|(name, _)| name.text(self.db).to_owned());
                let rep = self.build_fields_rep(
                    ty,
                    ordered.into_iter().map(|(name, field_ty)| (Some(name), field_ty)),
                    recursive_bindings,
                )?;
                let runtime_abi = AbiTy::Aggregate(Box::new(rep.runtime_layout.clone()));
                (RuntimeRepKind::Record(rep), runtime_abi)
            }
            TyKind::ExternStruct(struct_ty) => {
                let rep = self.build_fields_rep(
                    ty,
                    struct_fields(self.db, *struct_ty)
                        .iter()
                        .map(|(name, field_ty)| (Some(*name), *field_ty)),
                    recursive_bindings,
                )?;
                let runtime_abi = AbiTy::Aggregate(Box::new(rep.runtime_layout.clone()));
                (RuntimeRepKind::ExternStruct(rep), runtime_abi)
            }
            TyKind::Struct(struct_ty) => {
                let rep = self.build_fields_rep(
                    ty,
                    struct_fields(self.db, *struct_ty)
                        .iter()
                        .map(|(name, field_ty)| (Some(*name), *field_ty)),
                    recursive_bindings,
                )?;
                (
                    RuntimeRepKind::Struct(rep),
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(nominal_ty_bits(ty)))),
                )
            }
            TyKind::Enum(enum_ty) => {
                let rep = self.build_enum_rep(
                    ty,
                    enum_variants(self.db, *enum_ty)
                        .iter()
                        .enumerate()
                        .map(|(tag, (name, fields))| (*name, tag as i32, fields.clone())),
                    recursive_bindings,
                )?;
                (
                    RuntimeRepKind::Enum(rep),
                    AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(nominal_ty_bits(ty)))),
                )
            }
            TyKind::Union(items) => {
                let members = items
                    .iter()
                    .map(|&member| self.intern_type(member, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                let runtime_layout = enum_layout_from_variants(members.iter().enumerate().map(
                    |(index, &member)| {
                        (
                            (index as u64) + 1,
                            index as i32,
                            vec![self.graph.nodes[member.0 as usize].runtime_abi.clone()],
                        )
                    },
                ))
                .ok_or(RuntimeRepFailure::Unsupported(ty))?;
                let runtime_abi = AbiTy::Aggregate(Box::new(runtime_layout.clone()));
                (RuntimeRepKind::Union(RuntimeUnionRep { members, runtime_layout }), runtime_abi)
            }
            TyKind::Inter(items) => {
                let members = items
                    .iter()
                    .map(|&member| self.intern_type(member, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                let carrier_index = choose_intersection_carrier_index(self.db, items.as_slice())
                    .ok_or(RuntimeRepFailure::Unsupported(ty))?;
                let carrier_rep = members[carrier_index];
                let carrier_ty = items[carrier_index];
                let carrier_bits = ty_bits(carrier_ty);
                let mut seen = FxHashSet::default();
                let mut runtime_members = Vec::new();
                runtime_members.push(carrier_rep);
                seen.insert(carrier_bits);
                for (&member_ty, &member_rep) in items.iter().zip(members.iter()) {
                    let bits = ty_bits(member_ty);
                    if !seen.insert(bits) {
                        continue;
                    }
                    runtime_members.push(member_rep);
                }
                let (runtime_layout, runtime_abi) = if runtime_members.len() == 1 {
                    (None, self.graph.nodes[carrier_rep.0 as usize].runtime_abi.clone())
                } else {
                    let layout =
                        aggregate_layout_from_fields(runtime_members.iter().map(|&member| {
                            (None, self.graph.nodes[member.0 as usize].runtime_abi.clone())
                        }))
                        .ok_or(RuntimeRepFailure::Unsupported(ty))?;
                    (Some(layout.clone()), AbiTy::Aggregate(Box::new(layout)))
                };
                (
                    RuntimeRepKind::Intersection(RuntimeIntersectionRep {
                        carrier: carrier_rep,
                        members: runtime_members,
                        runtime_layout,
                    }),
                    runtime_abi,
                )
            }
            TyKind::Function { inputs, output } => {
                let params = inputs
                    .iter()
                    .map(|&input| self.intern_type(input, recursive_bindings))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = self.intern_type(*output, recursive_bindings)?;
                let signature = FunctionSignature {
                    params: params
                        .iter()
                        .map(|&rep| self.graph.nodes[rep.0 as usize].runtime_abi.clone())
                        .collect(),
                    result: self.graph.nodes[result.0 as usize].runtime_abi.clone(),
                };
                (
                    RuntimeRepKind::Function(RuntimeFunctionRep { params, result, signature }),
                    function_value_abi_ty(),
                )
            }
            TyKind::Rec(_, _) => return Err(RuntimeRepFailure::Recursive(ty)),
            TyKind::Unknown | TyKind::Var(_) => return Err(RuntimeRepFailure::Unsupported(ty)),
        })
    }

    fn build_fields_rep(
        &mut self,
        ty: Ty<'db>,
        declared_fields: impl IntoIterator<Item = (Option<Symbol<'db>>, Ty<'db>)>,
        recursive_bindings: &mut FxHashMap<u32, RuntimeRepId>,
    ) -> Result<RuntimeFieldsRep<'db>, RuntimeRepFailure<'db>> {
        let built = declared_fields
            .into_iter()
            .map(|(name, field_ty)| {
                self.intern_type(field_ty, recursive_bindings).map(|rep| (name, field_ty, rep))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let runtime_layout = aggregate_layout_from_fields(built.iter().map(|(name, _, rep)| {
            (name.map(symbol_bits), self.graph.nodes[rep.0 as usize].runtime_abi.clone())
        }))
        .ok_or(RuntimeRepFailure::Unsupported(ty))?;
        let runtime_fields = runtime_layout.fields().unwrap_or(&[]);
        let fields = built
            .into_iter()
            .zip(runtime_fields.iter())
            .map(|((name, field_ty, rep), runtime_field)| RuntimeRepField {
                name,
                ty: field_ty,
                rep,
                runtime_offset: runtime_field.offset,
            })
            .collect();
        Ok(RuntimeFieldsRep { fields, runtime_layout })
    }

    fn build_enum_rep(
        &mut self,
        ty: Ty<'db>,
        declared_variants: impl IntoIterator<Item = (Symbol<'db>, i32, Vec<Ty<'db>>)>,
        recursive_bindings: &mut FxHashMap<u32, RuntimeRepId>,
    ) -> Result<RuntimeEnumRep<'db>, RuntimeRepFailure<'db>> {
        let built_variants = declared_variants
            .into_iter()
            .map(|(name, tag, field_tys)| {
                let fields = field_tys
                    .into_iter()
                    .map(|field_ty| {
                        self.intern_type(field_ty, recursive_bindings).map(|rep| (field_ty, rep))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok::<_, RuntimeRepFailure<'db>>((name, tag, fields))
            })
            .collect::<Result<Vec<_>, _>>()?;

        let runtime_layout =
            enum_layout_from_variants(built_variants.iter().map(|(name, tag, fields)| {
                (
                    symbol_bits(*name),
                    *tag,
                    fields
                        .iter()
                        .map(|(_, rep)| self.graph.nodes[rep.0 as usize].runtime_abi.clone())
                        .collect::<Vec<_>>(),
                )
            }))
            .ok_or(RuntimeRepFailure::Unsupported(ty))?;
        let AggregateKind::Enum(runtime_enum_layout) = &runtime_layout.kind else {
            return Err(RuntimeRepFailure::Unsupported(ty));
        };
        let variants = built_variants
            .into_iter()
            .zip(runtime_enum_layout.variants.iter())
            .map(|((name, tag, fields), runtime_variant)| RuntimeRepVariant {
                name,
                tag,
                fields: fields
                    .into_iter()
                    .zip(runtime_variant.fields.iter())
                    .map(|((field_ty, rep), runtime_field)| RuntimeRepField {
                        name: None,
                        ty: field_ty,
                        rep,
                        runtime_offset: runtime_field.offset,
                    })
                    .collect(),
            })
            .collect();

        Ok(RuntimeEnumRep { variants, runtime_layout })
    }

    fn placeholder_runtime_abi(&self, ty: Ty<'db>) -> Option<AbiTy> {
        match ty.kind(self.db) {
            TyKind::Tuple(items) if items.is_empty() => Some(AbiTy::Scalar(BackendTy::Unit)),
            TyKind::Int => Some(AbiTy::Scalar(BackendTy::Int)),
            TyKind::ExactInt(int_ty) => Some(AbiTy::Scalar(exact_int_backend_ty(*int_ty))),
            TyKind::Bool => Some(AbiTy::Scalar(BackendTy::Bool)),
            TyKind::Float => Some(AbiTy::Scalar(BackendTy::Float)),
            TyKind::Char => Some(AbiTy::Scalar(BackendTy::Char)),
            TyKind::String => Some(AbiTy::Scalar(BackendTy::Ref(RefKind::String))),
            TyKind::Pointer { .. } => Some(AbiTy::Scalar(BackendTy::Int)),
            TyKind::Array(_) => {
                Some(AbiTy::Scalar(BackendTy::Ref(RefKind::Array(array_ty_bits(ty)))))
            }
            TyKind::Struct(_) | TyKind::Enum(_) => {
                Some(AbiTy::Scalar(BackendTy::Ref(RefKind::Nominal(nominal_ty_bits(ty)))))
            }
            TyKind::Function { .. } => Some(function_value_abi_ty()),
            TyKind::Rec(_, body) => self.placeholder_runtime_abi(*body),
            TyKind::Tuple(_)
            | TyKind::ExternStruct(_)
            | TyKind::Record(_)
            | TyKind::Union(_)
            | TyKind::Inter(_)
            | TyKind::Unknown
            | TyKind::Var(_) => None,
        }
    }

    fn populate_recursive_groups(&mut self) {
        #[derive(Default)]
        struct TarjanState {
            next_index: usize,
            indices: FxHashMap<usize, usize>,
            lowlinks: FxHashMap<usize, usize>,
            stack: Vec<usize>,
            on_stack: FxHashSet<usize>,
            components: Vec<Vec<usize>>,
        }

        fn visit<'db>(graph: &RuntimeRepGraph<'db>, index: usize, state: &mut TarjanState) {
            state.indices.insert(index, state.next_index);
            state.lowlinks.insert(index, state.next_index);
            state.next_index += 1;
            state.stack.push(index);
            state.on_stack.insert(index);

            for edge in runtime_edges(graph, RuntimeRepId(index as u32)) {
                let edge_index = edge.0 as usize;
                if !state.indices.contains_key(&edge_index) {
                    visit(graph, edge_index, state);
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

        let mut state = TarjanState::default();
        for index in 0..self.graph.nodes.len() {
            if !state.indices.contains_key(&index) {
                visit(&self.graph, index, &mut state);
            }
        }

        self.graph.recursive_groups.clear();
        for node in &mut self.graph.nodes {
            node.recursive_group = None;
        }

        for component in state.components {
            let is_recursive = component.len() > 1
                || runtime_edges(&self.graph, RuntimeRepId(component[0] as u32))
                    .contains(&RuntimeRepId(component[0] as u32));
            if !is_recursive {
                continue;
            }
            let group_id = RecursiveRuntimeRepGroupId(self.graph.recursive_groups.len() as u32);
            let members =
                component.into_iter().map(|index| RuntimeRepId(index as u32)).collect::<Vec<_>>();
            for member in &members {
                self.graph.nodes[member.0 as usize].recursive_group = Some(group_id);
            }
            self.graph.recursive_groups.push(members);
        }
    }

    fn populate_contains_function(&mut self) -> Result<(), RuntimeRepFailure<'db>> {
        fn visit<'db>(
            graph: &RuntimeRepGraph<'db>,
            id: RuntimeRepId,
            memo: &mut [Option<bool>],
            active: &mut FxHashSet<RuntimeRepId>,
        ) -> Result<bool, RuntimeRepFailure<'db>> {
            if let Some(value) = memo[id.0 as usize] {
                return Ok(value);
            }
            if !active.insert(id) {
                return Ok(false);
            }
            let contains = match &graph.nodes[id.0 as usize].kind {
                RuntimeRepKind::Function(_) => true,
                RuntimeRepKind::Array(array) => visit(graph, array.item, memo, active)?,
                RuntimeRepKind::Tuple(fields)
                | RuntimeRepKind::Record(fields)
                | RuntimeRepKind::ExternStruct(fields)
                | RuntimeRepKind::Struct(fields) => fields
                    .fields
                    .iter()
                    .any(|field| visit(graph, field.rep, memo, active).unwrap_or(false)),
                RuntimeRepKind::Enum(shape) => shape.variants.iter().any(|variant| {
                    variant
                        .fields
                        .iter()
                        .any(|field| visit(graph, field.rep, memo, active).unwrap_or(false))
                }),
                RuntimeRepKind::Union(shape) => shape
                    .members
                    .iter()
                    .any(|&member| visit(graph, member, memo, active).unwrap_or(false)),
                RuntimeRepKind::Intersection(shape) => shape
                    .members
                    .iter()
                    .any(|&member| visit(graph, member, memo, active).unwrap_or(false)),
                RuntimeRepKind::Pending
                | RuntimeRepKind::Unit
                | RuntimeRepKind::Int
                | RuntimeRepKind::Bool
                | RuntimeRepKind::Float
                | RuntimeRepKind::Char
                | RuntimeRepKind::String => false,
            };
            active.remove(&id);
            memo[id.0 as usize] = Some(contains);
            Ok(contains)
        }

        let mut memo = vec![None; self.graph.nodes.len()];
        let mut active = FxHashSet::default();
        for index in 0..self.graph.nodes.len() {
            let contains = visit(&self.graph, RuntimeRepId(index as u32), &mut memo, &mut active)?;
            self.graph.nodes[index].contains_function = contains;
        }
        Ok(())
    }
}

fn runtime_edges<'db>(graph: &RuntimeRepGraph<'db>, id: RuntimeRepId) -> Vec<RuntimeRepId> {
    match &graph.nodes[id.0 as usize].kind {
        RuntimeRepKind::Array(array) => vec![array.item],
        RuntimeRepKind::Tuple(fields)
        | RuntimeRepKind::Record(fields)
        | RuntimeRepKind::ExternStruct(fields)
        | RuntimeRepKind::Struct(fields) => fields.fields.iter().map(|field| field.rep).collect(),
        RuntimeRepKind::Enum(shape) => shape
            .variants
            .iter()
            .flat_map(|variant| variant.fields.iter().map(|field| field.rep))
            .collect(),
        RuntimeRepKind::Union(shape) => shape.members.clone(),
        RuntimeRepKind::Intersection(shape) => shape.members.clone(),
        RuntimeRepKind::Function(function) => {
            let mut edges = function.params.clone();
            edges.push(function.result);
            edges
        }
        RuntimeRepKind::Pending
        | RuntimeRepKind::Unit
        | RuntimeRepKind::Int
        | RuntimeRepKind::Bool
        | RuntimeRepKind::Float
        | RuntimeRepKind::Char
        | RuntimeRepKind::String => Vec::new(),
    }
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

fn choose_intersection_carrier_index(
    db: &dyn salsa::Database,
    members: &[Ty<'_>],
) -> Option<usize> {
    members
        .iter()
        .enumerate()
        .min_by_key(|(_, ty)| (intersection_carrier_priority(db, **ty), ty_bits(**ty)))
        .map(|(index, _)| index)
}

fn intersection_carrier_priority(db: &dyn salsa::Database, ty: Ty<'_>) -> u8 {
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

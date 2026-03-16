use std::ops::Deref;

use mitki_hir::ty::Ty;

use crate::classify::{self, ValueShape, ValueShapeFailure, ty_bits};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeGraphNodeId(pub u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct RecursiveTypeGroupId(pub u32);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeRuntimeDescriptor<'db> {
    shape: ValueShape<'db>,
    graph_node: TypeGraphNodeId,
    recursive_group: Option<RecursiveTypeGroupId>,
}

pub type TypeRuntimeDescriptorFailure<'db> = ValueShapeFailure<'db>;

impl<'db> TypeRuntimeDescriptor<'db> {
    pub fn new(shape: ValueShape<'db>) -> Self {
        Self { graph_node: TypeGraphNodeId(ty_bits(shape.ty)), recursive_group: None, shape }
    }

    pub fn graph_node(&self) -> TypeGraphNodeId {
        self.graph_node
    }
}

impl<'db> Deref for TypeRuntimeDescriptor<'db> {
    type Target = ValueShape<'db>;

    fn deref(&self) -> &Self::Target {
        &self.shape
    }
}

pub fn type_runtime_descriptor<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Result<TypeRuntimeDescriptor<'db>, TypeRuntimeDescriptorFailure<'db>> {
    classify::value_shape(db, ty).map(TypeRuntimeDescriptor::new)
}

pub fn supported_type_runtime_descriptor<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Option<TypeRuntimeDescriptor<'db>> {
    type_runtime_descriptor(db, ty).ok()
}

use std::fmt;

use mitki_hir::hir::TyId;
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::{FunctionLocation, Signature, instantiate_nominal_type};
use mitki_span::Symbol;
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::Resolver;

pub struct SignatureTypeResolver<'db> {
    db: &'db dyn Database,
    location: FunctionLocation<'db>,
    signature: &'db Signature<'db>,
    type_params: FxHashMap<Symbol<'db>, u32>,
}

impl<'db> SignatureTypeResolver<'db> {
    pub fn new(
        db: &'db dyn Database,
        location: FunctionLocation<'db>,
        signature: &'db Signature<'db>,
    ) -> Self {
        let type_params = signature
            .type_params(db)
            .iter()
            .enumerate()
            .map(|(index, &name)| (name, index as u32))
            .collect();
        Self { db, location, signature, type_params }
    }

    pub fn resolve(&self, ty: TyId) -> Result<Ty<'db>, SignatureTypeResolutionError> {
        self.resolve_inner(ty)
    }

    fn resolve_inner(&self, ty: TyId) -> Result<Ty<'db>, SignatureTypeResolutionError> {
        if ty == TyId::ZERO {
            return Ok(Ty::new(self.db, TyKind::Tuple(Vec::new())));
        }

        let nodes = self.signature.nodes(self.db);

        if let Some(tuple_id) = nodes.as_type_tuple(ty) {
            let items = nodes
                .type_tuple(tuple_id)
                .iter()
                .map(|item| self.resolve_inner(item))
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Ty::new(self.db, TyKind::Tuple(items)));
        }

        if let Some(array_id) = nodes.as_type_array(ty) {
            let (item_ty, _) = nodes.type_array(array_id);
            let item = self.resolve_inner(item_ty)?;
            return Ok(Ty::new(self.db, TyKind::Array(item)));
        }

        if let Some(ptr_id) = nodes.as_type_ptr_const(ty) {
            let (item_ty, _) = nodes.type_ptr_const(ptr_id);
            let item = self.resolve_inner(item_ty)?;
            return Ok(Ty::new(self.db, TyKind::Pointer { mutable: false, pointee: item }));
        }

        if let Some(ptr_id) = nodes.as_type_ptr_mut(ty) {
            let (item_ty, _) = nodes.type_ptr_mut(ptr_id);
            let item = self.resolve_inner(item_ty)?;
            return Ok(Ty::new(self.db, TyKind::Pointer { mutable: true, pointee: item }));
        }

        if let Some(function_id) = nodes.as_type_function(ty) {
            let (inputs_ty, output_ty) = nodes.type_function(function_id);
            let inputs = if let Some(tuple_id) = nodes.as_type_tuple(inputs_ty) {
                nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|item| self.resolve_inner(item))
                    .collect::<Result<Vec<_>, _>>()?
            } else if inputs_ty == TyId::ZERO {
                Vec::new()
            } else {
                vec![self.resolve_inner(inputs_ty)?]
            };
            let output = self.resolve_inner(output_ty)?;
            return Ok(Ty::new(self.db, TyKind::Function { inputs, output }));
        }

        if let Some(union_id) = nodes.as_type_union(ty) {
            let (lhs_ty, rhs_ty) = nodes.type_union(union_id);
            let lhs = self.resolve_inner(lhs_ty)?;
            let rhs = self.resolve_inner(rhs_ty)?;
            return Ok(Ty::new(self.db, TyKind::Union(vec![lhs, rhs])));
        }

        if let Some(inter_id) = nodes.as_type_inter(ty) {
            let (lhs_ty, rhs_ty) = nodes.type_inter(inter_id);
            let lhs = self.resolve_inner(lhs_ty)?;
            let rhs = self.resolve_inner(rhs_ty)?;
            return Ok(Ty::new(self.db, TyKind::Inter(vec![lhs, rhs])));
        }

        if let Some(record_id) = nodes.as_type_record(ty) {
            let fields = nodes
                .type_record(record_id)
                .iter()
                .filter_map(|field_ty| {
                    let field_id = nodes.as_type_field(field_ty)?;
                    let (name_id, field_ty) = nodes.type_field(field_id);
                    Some((nodes.name(name_id), field_ty))
                })
                .map(|(name, field_ty)| {
                    self.resolve_inner(field_ty).map(|field_ty| (name, field_ty))
                })
                .collect::<Result<Vec<_>, _>>()?;
            return Ok(Ty::new(self.db, TyKind::Record(fields)));
        }

        if let Some(type_apply) = nodes.as_type_apply(ty) {
            let (path_ty, args_ty) = nodes.type_apply(type_apply);
            let base = self.resolve_inner(path_ty)?;
            let args = if let Some(tuple_id) = nodes.as_type_tuple(args_ty) {
                nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|arg| self.resolve_inner(arg))
                    .collect::<Result<Vec<_>, _>>()?
            } else {
                Vec::new()
            };

            return instantiate_nominal_type(self.db, base, args)
                .ok_or(SignatureTypeResolutionError::UnsupportedNode);
        }

        let Some(path_id) = nodes.as_type_path(ty) else {
            return Err(SignatureTypeResolutionError::UnsupportedNode);
        };
        let name = nodes.type_ref(path_id);

        if let Some(&id) = self.type_params.get(&name) {
            return Ok(Ty::new(self.db, TyKind::Var(id)));
        }

        let resolver = Resolver::new(self.db, self.location);
        resolver
            .resolve_type_binding(name)
            .and_then(|binding| resolver.ty_for_binding(binding))
            .ok_or_else(|| SignatureTypeResolutionError::UnresolvedPath(name.text(self.db).into()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SignatureTypeResolutionError {
    UnsupportedNode,
    UnresolvedPath(Box<str>),
}

impl fmt::Display for SignatureTypeResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnsupportedNode => write!(f, "unsupported type node in function signature"),
            Self::UnresolvedPath(path) => {
                write!(f, "could not resolve signature type `{path}`")
            }
        }
    }
}

use mitki_hir::ty::{Ty, TyKind};

use crate::classify::{AbiTy, FunctionSignature};
use crate::descriptor::{
    TypeRuntimeDescriptor,
    supported_type_runtime_descriptor as raw_supported_type_runtime_descriptor,
};
use crate::layout::{AggregateLayout, ArrayRuntimeLayout};
use crate::runtime_rep::runtime_rep_descriptor;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueSupportFailure<'db> {
    Unsupported(Ty<'db>),
    Recursive(Ty<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BoundaryCapabilityFailure<'db> {
    Type(ValueSupportFailure<'db>),
}

pub fn supported_value_abi(db: &dyn salsa::Database, ty: Ty<'_>) -> Option<AbiTy> {
    runtime_rep_descriptor(db, ty).ok().map(|descriptor| descriptor.runtime_abi())
}

pub fn supported_value_abi_or_message(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
    context: &str,
) -> Result<AbiTy, String> {
    supported_value_abi(db, ty).ok_or_else(|| {
        value_support_message(db, ensure_value_support(db, ty).unwrap_err(), context)
    })
}

pub fn supports_boundary(db: &dyn salsa::Database, ty: Ty<'_>) -> bool {
    ensure_boundary_capability(db, ty).is_ok()
}

pub fn supported_function_signature(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
) -> Option<FunctionSignature> {
    runtime_rep_descriptor(db, ty).ok()?.function_signature()
}

pub fn supported_function_signature_or_message(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
    context: &str,
) -> Result<FunctionSignature, String> {
    supported_function_signature(db, ty)
        .ok_or_else(|| unsupported_shape_message(db, ty, context, "a function value type"))
}

pub fn supported_array_runtime_layout(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
) -> Option<ArrayRuntimeLayout> {
    runtime_rep_descriptor(db, ty).ok()?.array_layout()
}

pub fn supported_array_runtime_layout_or_message(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
    context: &str,
) -> Result<ArrayRuntimeLayout, String> {
    supported_array_runtime_layout(db, ty)
        .ok_or_else(|| unsupported_shape_message(db, ty, context, "an array value type"))
}

pub fn supported_internal_nominal_payload_layout(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
) -> Option<AggregateLayout> {
    runtime_rep_descriptor(db, ty).ok()?.runtime_nominal_payload_layout()
}

pub fn supported_internal_nominal_payload_layout_or_message(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
    context: &str,
) -> Result<AggregateLayout, String> {
    supported_internal_nominal_payload_layout(db, ty)
        .ok_or_else(|| unsupported_shape_message(db, ty, context, "a nominal value type"))
}

fn ensure_value_support<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Result<(), ValueSupportFailure<'db>> {
    runtime_rep_descriptor(db, ty).map(|_shape| ()).map_err(map_runtime_rep_failure)
}

pub fn ensure_boundary_capability<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Result<(), BoundaryCapabilityFailure<'db>> {
    ensure_value_support(db, ty).map_err(BoundaryCapabilityFailure::Type)
}

pub fn supported_type_runtime_descriptor<'db>(
    db: &'db dyn salsa::Database,
    ty: Ty<'db>,
) -> Option<TypeRuntimeDescriptor<'db>> {
    raw_supported_type_runtime_descriptor(db, ty)
}

fn map_runtime_rep_failure<'db>(
    failure: crate::runtime_rep::RuntimeRepFailure<'db>,
) -> ValueSupportFailure<'db> {
    match failure {
        crate::runtime_rep::RuntimeRepFailure::Unsupported(ty) => {
            ValueSupportFailure::Unsupported(ty)
        }
        crate::runtime_rep::RuntimeRepFailure::Recursive(ty) => ValueSupportFailure::Recursive(ty),
    }
}

fn value_support_message(
    db: &dyn salsa::Database,
    failure: ValueSupportFailure<'_>,
    context: &str,
) -> String {
    format!("{context}; {}", value_support_detail(db, failure))
}

fn value_support_detail(db: &dyn salsa::Database, failure: ValueSupportFailure<'_>) -> String {
    let ty = match failure {
        ValueSupportFailure::Unsupported(ty) | ValueSupportFailure::Recursive(ty) => ty,
    };

    match failure {
        ValueSupportFailure::Recursive(_) => {
            format!("recursive types are not supported yet; found `{}`", ty.display(db))
        }
        ValueSupportFailure::Unsupported(_) => match ty.kind(db) {
            TyKind::Inter(_) => {
                format!("intersection types are not supported yet; found `{}`", ty.display(db))
            }
            TyKind::Rec(_, _) => {
                format!("recursive types are not supported yet; found `{}`", ty.display(db))
            }
            TyKind::Pointer { .. } => {
                format!("raw pointer type `{}` is not supported in this context", ty.display(db))
            }
            TyKind::ExternStruct(_) => {
                format!("extern struct type `{}` is not supported in this context", ty.display(db))
            }
            _ => format!("found `{}`", ty.display(db)),
        },
    }
}

fn unsupported_shape_message(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
    context: &str,
    expected: &str,
) -> String {
    match ensure_value_support(db, ty) {
        Err(failure) => value_support_message(db, failure, context),
        Ok(()) => format!("{context}; expected {expected}, found `{}`", ty.display(db)),
    }
}

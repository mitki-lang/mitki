use mitki_abi::TransportClass;
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::item::scope::enum_variants;
use salsa::plumbing::AsId as _;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BoundaryTransportPlan {
    pub transport_class: TransportClass,
}

pub fn boundary_transport_class(db: &dyn salsa::Database, ty: Ty<'_>) -> TransportClass {
    match ty.kind(db) {
        TyKind::Bool | TyKind::Float | TyKind::Int | TyKind::Char => TransportClass::Immediate,
        TyKind::Tuple(items) if items.is_empty() => TransportClass::Immediate,
        TyKind::Enum(enum_ty) if is_nullary_enum(db, *enum_ty) => TransportClass::Immediate,
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

fn is_nullary_enum(db: &dyn salsa::Database, enum_ty: mitki_hir::ty::EnumTy<'_>) -> bool {
    enum_variants(db, enum_ty).iter().all(|(_, fields)| fields.is_empty())
}

fn ty_bits(ty: Ty<'_>) -> u32 {
    u32::try_from(ty.as_id().as_bits()).expect("type id should fit into u32")
}

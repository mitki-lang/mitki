use mitki_hir::ty::{EnumTy, StructTy, Ty, TyKind};
use mitki_lower::item::scope::{
    HasVisibleItems as _, TypeDeclaration, enum_variants, struct_fields,
};
use rustc_hash::FxHashSet;
use salsa::plumbing::AsId as _;

#[derive(Debug, Clone, PartialEq, Eq, salsa::Update)]
pub struct TypeOwnership {
    copyable: bool,
    needs_drop: bool,
    explicit_destructor: bool,
}

impl TypeOwnership {
    pub fn is_copyable(&self) -> bool {
        self.copyable
    }

    pub fn needs_drop(&self) -> bool {
        self.needs_drop
    }

    pub fn has_explicit_destructor(&self) -> bool {
        self.explicit_destructor
    }
}

#[salsa::tracked(returns(ref))]
pub fn type_ownership<'db>(db: &'db dyn salsa::Database, ty: Ty<'db>) -> TypeOwnership {
    compute_type_ownership(db, ty, &mut FxHashSet::default())
}

pub fn is_copyable(db: &dyn salsa::Database, ty: Ty<'_>) -> bool {
    type_ownership(db, ty).is_copyable()
}

pub fn needs_drop(db: &dyn salsa::Database, ty: Ty<'_>) -> bool {
    type_ownership(db, ty).needs_drop()
}

pub fn has_explicit_destructor(db: &dyn salsa::Database, ty: Ty<'_>) -> bool {
    type_ownership(db, ty).has_explicit_destructor()
}

fn compute_type_ownership(
    db: &dyn salsa::Database,
    ty: Ty<'_>,
    seen: &mut FxHashSet<u64>,
) -> TypeOwnership {
    let bits = ty.as_id().as_bits();
    if !seen.insert(bits) {
        return TypeOwnership { copyable: false, needs_drop: false, explicit_destructor: false };
    }

    let ownership = match ty.kind(db) {
        TyKind::Bool
        | TyKind::Float
        | TyKind::Int
        | TyKind::ExactInt(_)
        | TyKind::String
        | TyKind::Char
        | TyKind::Pointer { .. }
        | TyKind::Function { .. }
        | TyKind::Unknown
        | TyKind::Var(_) => {
            TypeOwnership { copyable: true, needs_drop: false, explicit_destructor: false }
        }
        TyKind::Array(item) => inherited_ownership([compute_type_ownership(db, *item, seen)]),
        TyKind::Tuple(items) => {
            inherited_ownership(items.iter().map(|item| compute_type_ownership(db, *item, seen)))
        }
        TyKind::Record(fields) => inherited_ownership(
            fields.iter().map(|(_, field_ty)| compute_type_ownership(db, *field_ty, seen)),
        ),
        TyKind::Struct(struct_ty) => nominal_ownership(
            db,
            struct_ty.destructor(db).is_some(),
            struct_fields(db, *struct_ty)
                .iter()
                .map(|(_, field_ty)| compute_type_ownership(db, *field_ty, seen)),
        ),
        TyKind::Enum(enum_ty) => {
            let mut field_ownership = Vec::new();
            for (_, field_tys) in enum_variants(db, *enum_ty).iter() {
                for field_ty in field_tys {
                    field_ownership.push(compute_type_ownership(db, *field_ty, seen));
                }
            }
            nominal_ownership(db, enum_ty.destructor(db).is_some(), field_ownership)
        }
        TyKind::Union(items) | TyKind::Inter(items) => {
            inherited_ownership(items.iter().map(|item| compute_type_ownership(db, *item, seen)))
        }
        TyKind::ExternStruct(_) => {
            TypeOwnership { copyable: true, needs_drop: false, explicit_destructor: false }
        }
        TyKind::Rec(_, body) => compute_type_ownership(db, *body, seen),
    };

    seen.remove(&bits);
    ownership
}

fn inherited_ownership(items: impl IntoIterator<Item = TypeOwnership>) -> TypeOwnership {
    let mut copyable = true;
    let mut needs_drop = false;
    let mut explicit_destructor = false;

    for item in items {
        copyable &= item.copyable;
        needs_drop |= item.needs_drop;
        explicit_destructor |= item.explicit_destructor;
    }

    TypeOwnership { copyable, needs_drop, explicit_destructor }
}

fn nominal_ownership(
    _db: &dyn salsa::Database,
    explicit_destructor: bool,
    fields: impl IntoIterator<Item = TypeOwnership>,
) -> TypeOwnership {
    let inherited = inherited_ownership(fields);
    TypeOwnership {
        copyable: !explicit_destructor && inherited.copyable,
        needs_drop: explicit_destructor || inherited.needs_drop,
        explicit_destructor,
    }
}

pub trait NominalDestructorLookup<'db> {
    fn destructor(self, db: &'db dyn salsa::Database) -> Option<()>;
}

impl<'db> NominalDestructorLookup<'db> for StructTy<'db> {
    fn destructor(self, db: &'db dyn salsa::Database) -> Option<()> {
        let source = self.module(db).visible_items(db).get_type_declaration(&self.name(db))?;
        let TypeDeclaration::Struct(location) = source else {
            return None;
        };
        location.destructor(db).map(|_| ())
    }
}

impl<'db> NominalDestructorLookup<'db> for EnumTy<'db> {
    fn destructor(self, db: &'db dyn salsa::Database) -> Option<()> {
        let source = self.module(db).visible_items(db).get_type_declaration(&self.name(db))?;
        let TypeDeclaration::Enum(location) = source else {
            return None;
        };
        location.destructor(db).map(|_| ())
    }
}

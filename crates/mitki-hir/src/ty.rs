use std::marker::PhantomData;

use mitki_span::{Symbol, SymbolDatabase};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, facet::Facet)]
pub struct Ty<'db> {
    id: picante::InternId,
    _marker: PhantomData<&'db ()>,
}

#[repr(C)]
#[derive(Debug, Clone, Hash, PartialEq, Eq, facet::Facet)]
pub enum TyKind<'db> {
    Bool,
    Float,
    Int,
    String,
    Char,
    Tuple(Vec<Ty<'db>>),
    Record(Vec<(Symbol<'db>, Ty<'db>)>),
    Unknown,
    Function { inputs: Vec<Ty<'db>>, output: Ty<'db> },
    Var(u32),
    Union(Vec<Ty<'db>>),
    Inter(Vec<Ty<'db>>),
    Rec(u32, Ty<'db>),
    Struct { name: Symbol<'db>, fields: Vec<(Symbol<'db>, Ty<'db>)> },
    Enum { name: Symbol<'db>, variants: Vec<(Symbol<'db>, Vec<Ty<'db>>)> },
}

#[picante::interned]
pub struct TyData {
    pub kind: TyKind<'static>,
}

pub trait TypeDatabase: picante::HasRuntime + HasTyDataIngredient + SymbolDatabase {}

impl<T> TypeDatabase for T where T: picante::HasRuntime + HasTyDataIngredient + SymbolDatabase {}

impl<'db> Ty<'db> {
    pub fn new<DB>(db: &DB, kind: TyKind<'db>) -> Self
    where
        DB: TypeDatabase,
    {
        let data = TyData::new(db, erase_kind(kind)).expect("failed to intern type");
        Self { id: data.0, _marker: PhantomData }
    }

    pub fn kind<DB>(self, db: &DB) -> TyKind<'db>
    where
        DB: TypeDatabase,
    {
        let kind = TyData(self.id).kind(db).expect("failed to load type");
        unerase_kind(kind)
    }

    pub fn display<DB>(self, db: &'db DB) -> TyDisplay<'db, DB>
    where
        DB: TypeDatabase,
    {
        TyDisplay { db, ty_kind: self.kind(db) }
    }

    pub fn as_bits(self) -> u64 {
        (self.id.0 as u64) + 1
    }

    pub fn from_bits(bits: u64) -> Self {
        let id = bits.checked_sub(1).expect("invalid type bits");
        let id = u32::try_from(id).expect("type bits overflow");
        Self { id: picante::InternId(id), _marker: PhantomData }
    }
}

pub struct TyDisplay<'db, DB>
where
    DB: TypeDatabase,
{
    ty_kind: TyKind<'db>,
    db: &'db DB,
}

impl<DB> std::fmt::Display for TyDisplay<'_, DB>
where
    DB: TypeDatabase,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.ty_kind {
            TyKind::Bool => write!(f, "bool"),
            TyKind::Float => write!(f, "float"),
            TyKind::Int => write!(f, "int"),
            TyKind::String => write!(f, "str"),
            TyKind::Char => write!(f, "char"),
            TyKind::Tuple(items) => {
                write!(f, "(")?;
                write_joined(f, self.db, items.iter(), ", ")?;
                if items.len() == 1 {
                    write!(f, ",")?;
                }
                write!(f, ")")
            }
            TyKind::Record(fields) => {
                write!(f, "{{")?;
                for (idx, (name, ty)) in fields.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name.text(self.db).as_ref(), ty.display(self.db))?;
                }
                write!(f, "}}")
            }
            TyKind::Function { inputs, output } => {
                write!(f, "fun(")?;
                write_joined(f, self.db, inputs.iter(), ", ")?;
                write!(f, ") -> {}", output.display(self.db))
            }
            TyKind::Unknown => write!(f, "{{unknown}}"),
            TyKind::Var(id) => write_var(f, *id),
            TyKind::Union(items) => write_joined(f, self.db, items.iter(), " | "),
            TyKind::Inter(items) => write_joined(f, self.db, items.iter(), " & "),
            TyKind::Rec(id, body) => {
                write!(f, "mu")?;
                write_var(f, *id)?;
                write!(f, ". {}", body.display(self.db))
            }
            TyKind::Struct { name, .. } => write!(f, "{}", name.text(self.db).as_ref()),
            TyKind::Enum { name, .. } => write!(f, "{}", name.text(self.db).as_ref()),
        }
    }
}

fn write_var(f: &mut std::fmt::Formatter<'_>, id: u32) -> std::fmt::Result {
    let c = (b'A' + (id % 26) as u8) as char;
    if id >= 26 { write!(f, "{c}{}", id / 26) } else { write!(f, "{c}") }
}

fn write_joined<'db, DB>(
    f: &mut std::fmt::Formatter<'_>,
    db: &'db DB,
    iter: impl ExactSizeIterator<Item = &'db Ty<'db>>,
    sep: &str,
) -> std::fmt::Result
where
    DB: TypeDatabase,
{
    let mut first = true;
    for e in iter {
        if !first {
            write!(f, "{sep}")?;
        }
        first = false;
        write!(f, "{}", e.display(db))?;
    }
    Ok(())
}

fn erase_kind(kind: TyKind<'_>) -> TyKind<'static> {
    // SAFETY: TyKind is composed of copyable handles with phantom lifetimes only.
    unsafe { std::mem::transmute(kind) }
}

fn unerase_kind<'db>(kind: TyKind<'static>) -> TyKind<'db> {
    // SAFETY: TyKind stores no borrowed data and can be viewed at any database
    // lifetime.
    unsafe { std::mem::transmute(kind) }
}

use mitki_inputs::{File, ModuleId};
use mitki_span::Symbol;

#[salsa::interned(debug)]
pub struct StructTy<'db> {
    pub module: ModuleId<'db>,
    pub index: u32,
    pub name: Symbol<'db>,
    #[returns(ref)]
    pub args: Vec<Ty<'db>>,
}

#[salsa::interned(debug)]
pub struct EnumTy<'db> {
    pub module: ModuleId<'db>,
    pub index: u32,
    pub name: Symbol<'db>,
    #[returns(ref)]
    pub args: Vec<Ty<'db>>,
}

#[salsa::interned(debug)]
pub struct Ty<'db> {
    #[returns(ref)]
    pub kind: TyKind<'db>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, salsa::Update)]
pub enum ExactInt {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
}

impl ExactInt {
    pub fn signed(self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::I64)
    }

    pub fn bits(self) -> u8 {
        match self {
            Self::U8 | Self::I8 => 8,
            Self::U16 | Self::I16 => 16,
            Self::U32 | Self::I32 => 32,
            Self::U64 | Self::I64 => 64,
        }
    }

    pub fn display(self) -> &'static str {
        match self {
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
        }
    }
}

impl<'db> Ty<'db> {
    pub fn display(self, db: &'db dyn salsa::Database) -> TyDisplay<'db> {
        TyDisplay { db, ty_kind: self.kind(db) }
    }
}

impl<'db> StructTy<'db> {
    pub fn file(self, db: &'db dyn salsa::Database) -> File {
        self.module(db).file(db)
    }
}

impl<'db> EnumTy<'db> {
    pub fn file(self, db: &'db dyn salsa::Database) -> File {
        self.module(db).file(db)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, salsa::Update)]
pub enum TyKind<'db> {
    Bool,
    Float,
    Int,
    ExactInt(ExactInt),
    String,
    Char,
    Array(Ty<'db>),
    Tuple(Vec<Ty<'db>>),
    Record(Vec<(Symbol<'db>, Ty<'db>)>),
    Pointer { mutable: bool, pointee: Ty<'db> },
    Unknown,
    Function { inputs: Vec<Ty<'db>>, output: Ty<'db> },
    Var(u32),
    Union(Vec<Ty<'db>>),
    Inter(Vec<Ty<'db>>),
    Rec(u32, Ty<'db>),
    Struct(StructTy<'db>),
    ExternStruct(StructTy<'db>),
    Enum(EnumTy<'db>),
}

pub struct TyDisplay<'db> {
    ty_kind: &'db TyKind<'db>,
    db: &'db dyn salsa::Database,
}

impl<'db> std::fmt::Display for TyDisplay<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty_kind {
            TyKind::Bool => write!(f, "bool"),
            TyKind::Float => write!(f, "float"),
            TyKind::Int => write!(f, "int"),
            TyKind::ExactInt(int_ty) => write!(f, "{}", int_ty.display()),
            TyKind::String => write!(f, "str"),
            TyKind::Char => write!(f, "char"),
            TyKind::Array(item) => write!(f, "[{}]", item.display(self.db)),
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
                    write!(f, "{}: {}", name.text(self.db), ty.display(self.db))?;
                }
                write!(f, "}}")
            }
            TyKind::Pointer { mutable, pointee } => {
                let qualifier = if *mutable { "mut" } else { "const" };
                write!(f, "*{qualifier} {}", pointee.display(self.db))
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
                write!(f, "μ")?;
                write_var(f, *id)?;
                write!(f, ". {}", body.display(self.db))
            }
            TyKind::Struct(struct_ty) => {
                write_nominal(f, self.db, struct_ty.name(self.db), struct_ty.args(self.db))
            }
            TyKind::ExternStruct(struct_ty) => {
                write_nominal(f, self.db, struct_ty.name(self.db), struct_ty.args(self.db))
            }
            TyKind::Enum(enum_ty) => {
                write_nominal(f, self.db, enum_ty.name(self.db), enum_ty.args(self.db))
            }
        }
    }
}

fn write_var(f: &mut std::fmt::Formatter<'_>, id: u32) -> std::fmt::Result {
    let c = (b'A' + (id % 26) as u8) as char;
    if id >= 26 { write!(f, "{c}{}", id / 26) } else { write!(f, "{c}") }
}

fn write_joined<'db>(
    f: &mut std::fmt::Formatter<'_>,
    db: &'db dyn salsa::Database,
    iter: impl ExactSizeIterator<Item = &'db Ty<'db>>,
    sep: &str,
) -> std::fmt::Result {
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

fn write_nominal<'db>(
    f: &mut std::fmt::Formatter<'_>,
    db: &'db dyn salsa::Database,
    name: Symbol<'db>,
    args: &'db [Ty<'db>],
) -> std::fmt::Result {
    write!(f, "{}", name.text(db))?;
    if args.is_empty() {
        return Ok(());
    }

    write!(f, "[")?;
    write_joined(f, db, args.iter(), ", ")?;
    write!(f, "]")
}

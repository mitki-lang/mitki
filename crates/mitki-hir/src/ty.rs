use mitki_span::Symbol;

#[salsa::interned(debug)]
pub struct Ty<'db> {
    #[returns(ref)]
    pub kind: TyKind<'db>,
}

impl<'db> Ty<'db> {
    pub fn display(self, db: &'db dyn salsa::Database) -> TyDisplay<'db> {
        TyDisplay { db, ty_kind: self.kind(db) }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, salsa::Update)]
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
                    write!(f, "{}: {}", name.text(self.db), ty.display(self.db))?;
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
                write!(f, "μ")?;
                write_var(f, *id)?;
                write!(f, ". {}", body.display(self.db))
            }
            TyKind::Struct { name, .. } => write!(f, "{}", name.text(self.db)),
            TyKind::Enum { name, .. } => write!(f, "{}", name.text(self.db)),
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

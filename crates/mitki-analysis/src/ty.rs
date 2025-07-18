#[salsa::interned(debug)]
pub struct Ty<'db> {
    #[returns(ref)]
    pub kind: TyKind<'db>,
}

impl<'db> Ty<'db> {
    pub(crate) fn display(self, db: &'db dyn salsa::Database) -> TyDisplay<'db> {
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
    Unknown,
    Function { inputs: Vec<Ty<'db>>, output: Ty<'db> },
}

pub(crate) struct TyDisplay<'db> {
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
            TyKind::Function { inputs, output } => {
                write!(f, "fun(")?;
                write_joined(f, self.db, inputs.iter(), ", ")?;
                write!(f, ") -> {}", output.display(self.db))
            }
            TyKind::Unknown => write!(f, "{{unknown}}"),
        }
    }
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

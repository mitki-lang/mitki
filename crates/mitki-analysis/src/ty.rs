use std::fmt::Display;

#[salsa::interned(debug)]
pub(crate) struct Ty<'db> {
    #[return_ref]
    kind: TyKind<'db>,
}

impl<'db> Display for Ty<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        salsa::plumbing::with_attached_database(|db| {
            let str = match self.kind(db) {
                TyKind::Bool => "bool",
                TyKind::Float => "float",
                TyKind::Int => "int",
                TyKind::Tuple(items) => return Tuple(items).fmt(f),
                TyKind::Unknown => "{unknown}",
                TyKind::Function => "{function}",
            };

            f.write_str(str)
        })
        .unwrap()
    }
}

struct Tuple<'db>(&'db [Ty<'db>]);

impl<'db> Display for Tuple<'db> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(")?;
        let mut iter = self.0.iter();
        if let Some(first) = iter.next() {
            write!(f, "{first}")?;
            for item in iter {
                write!(f, ", {item}")?;
            }
        }
        if self.0.len() == 1 {
            write!(f, ",")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, salsa::Update)]
pub(crate) enum TyKind<'db> {
    Bool,
    Float,
    Int,
    Tuple(Vec<Ty<'db>>),
    Unknown,
    Function,
}

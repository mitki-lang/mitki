use rustc_hash::FxHashMap;
use salsa::Database;

use crate::hir::{Function, HasFunction as _, NodeId};
use crate::item::scope::FunctionLocation;
use crate::resolver::Resolver;
use crate::ty::Ty;

pub(crate) trait Inferable<'db> {
    fn infer(self, db: &'db dyn Database) -> &'db Inference<'db>;
}

#[salsa::tracked]
impl<'db> Inferable<'db> for FunctionLocation<'db> {
    #[salsa::tracked(return_ref, no_eq)]
    fn infer(self, db: &'db dyn Database) -> Inference<'db> {
        let builder = InferenceBuilder {
            db,
            resolver: Resolver::new(db, self),
            inference: Inference::default(),
            function: self.hir_function(db),
        };
        builder.build()
    }
}

#[derive(Debug, Default, salsa::Update)]
pub(crate) struct Inference<'db> {
    type_of_expr: FxHashMap<NodeId, Ty<'db>>,
}

#[expect(dead_code)]
pub(crate) struct InferenceBuilder<'db> {
    db: &'db dyn Database,
    resolver: Resolver<'db>,
    inference: Inference<'db>,
    function: &'db Function<'db>,
}

impl<'db> InferenceBuilder<'db> {
    fn build(self) -> Inference<'db> {
        self.inference
    }
}

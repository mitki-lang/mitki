use mitki_inputs::File;
use mitki_span::Symbol;
use salsa::Database;

use super::tree::{Function, Item, ItemTree};
use crate::item::tree::HasItemTree as _;

type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

pub(crate) trait HasItemScope {
    fn item_scope(self, db: &dyn Database) -> &ItemScope<'_>;
}

#[salsa::tracked]
impl HasItemScope for File {
    #[salsa::tracked(return_ref, no_eq)]
    fn item_scope(self, db: &dyn Database) -> ItemScope<'_> {
        ItemScopeBuilder { db, item_tree: self.item_tree(db), scope: ItemScope::default() }
            .build(self)
    }
}

#[salsa::tracked]
pub(crate) struct FunctionLocation<'db> {
    pub(crate) file: File,
    pub(crate) index: Function<'db>,
}

#[derive(Debug, Default)]
pub(crate) struct ItemScope<'db> {
    values: FxIndexMap<Symbol<'db>, FunctionLocation<'db>>,
    declarations: Vec<FunctionLocation<'db>>,
}

impl<'db> ItemScope<'db> {
    pub(crate) fn declarations(&self) -> impl ExactSizeIterator<Item = FunctionLocation<'db>> + '_ {
        self.declarations.iter().copied()
    }
}

struct ItemScopeBuilder<'db> {
    db: &'db dyn Database,
    item_tree: &'db ItemTree<'db>,
    scope: ItemScope<'db>,
}

impl<'db> ItemScopeBuilder<'db> {
    fn build(mut self, file: File) -> ItemScope<'db> {
        for item in self.item_tree.items() {
            match item {
                Item::Function(index) => {
                    let func = &self.item_tree[index];
                    let func_loc = FunctionLocation::new(self.db, file, index);

                    self.scope.declarations.push(func_loc);
                    self.scope.values.insert(func.name, func_loc);
                }
            }
        }

        self.scope
    }
}

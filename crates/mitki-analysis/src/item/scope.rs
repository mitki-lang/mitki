use mitki_inputs::File;
use mitki_span::Symbol;
use salsa::Database;

use super::tree::{Item, ItemTree};
use crate::item::tree::{Function, HasItemTree as _};

type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

pub(crate) trait HasItemScope {
    fn item_scope(self, db: &dyn Database) -> &ItemScope<'_>;
}

#[salsa::tracked]
impl HasItemScope for File {
    #[salsa::tracked(return_ref)]
    fn item_scope(self, db: &dyn Database) -> ItemScope<'_> {
        ItemScopeBuilder { db, item_tree: self.item_tree(db), scope: ItemScope::default() }
            .build(self)
    }
}

#[derive(salsa::Update, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Declaration<'db> {
    Function(FunctionLocation<'db>),
}

#[salsa::tracked(debug)]
pub struct FunctionLocation<'db> {
    pub file: File,
    pub index: Function<'db>,
}

impl<'db> FunctionLocation<'db> {
    pub fn source(&self, db: &'db dyn Database) -> mitki_yellow::ast::Function<'db> {
        use mitki_parse::FileParse as _;
        use mitki_yellow::ast::{self, Node as _};

        use crate::ast_map::HasAstMap as _;
        use crate::item::tree::HasItemTree as _;

        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let ptr = ast_map.find_node(item);

        let syntax = ptr.to_node(db, &file.parse(db).syntax_node());
        ast::Function::cast(db, syntax).unwrap()
    }
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct ItemScope<'db> {
    values: FxIndexMap<Symbol<'db>, FunctionLocation<'db>>,
    declarations: Vec<Declaration<'db>>,
}

impl<'db> ItemScope<'db> {
    pub(crate) fn get(&self, name: &Symbol<'db>) -> Option<FunctionLocation<'db>> {
        self.values.get(name).copied()
    }

    pub(crate) fn declarations(&self) -> &[Declaration<'db>] {
        &self.declarations
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

                    self.scope.declarations.push(Declaration::Function(func_loc));
                    self.scope.values.insert(func.name, func_loc);
                }
            }
        }

        self.scope
    }
}

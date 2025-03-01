use std::ops::Index;

use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_span::Symbol;
use mitki_yellow::RedNodePtr;
use mitki_yellow::ast::{HasName as _, Node};
use salsa::Database;

use crate::arena::{Arena, Key};
use crate::ast_map::HasAstMap as _;

pub type Function<'db> = Key<FunctionData<'db>>;

pub(crate) trait HasItemTree {
    fn item_tree(self, db: &dyn Database) -> &ItemTree<'_>;
}

#[salsa::tracked]
impl HasItemTree for File {
    #[salsa::tracked(return_ref, no_eq)]
    fn item_tree(self, db: &dyn Database) -> ItemTree<'_> {
        let mut item_tree = ItemTree::default();
        let ast_map = self.ast_map(db);

        for item in self.parse(db).tree(db).items(db) {
            let item = match item {
                mitki_yellow::ast::Item::Function(func) => {
                    let id = ast_map.find_id(db, func.syntax());
                    let Some(name) = func.name(db).map(|name| Symbol::new(db, name.as_str(db)))
                    else {
                        continue;
                    };

                    Item::Function(item_tree.functions.alloc(FunctionData { id, name }))
                }
            };

            item_tree.items.push(item);
        }

        item_tree
    }
}

#[derive(Debug, Default, PartialEq, salsa::Update)]
pub(crate) struct ItemTree<'db> {
    items: Vec<Item<'db>>,
    functions: Arena<FunctionData<'db>>,
}

impl<'db> Index<Function<'db>> for ItemTree<'db> {
    type Output = FunctionData<'db>;

    fn index(&self, index: Function<'db>) -> &Self::Output {
        &self.functions[index]
    }
}

impl<'db> ItemTree<'db> {
    pub(crate) fn items(&self) -> impl ExactSizeIterator<Item = Item<'db>> + '_ {
        self.items.iter().copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub(crate) enum Item<'db> {
    Function(Function<'db>),
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct FunctionData<'db> {
    pub(crate) id: Key<RedNodePtr>,
    pub(crate) name: Symbol<'db>,
}

use std::ops::Index;

use la_arena::{Arena, Idx};
use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_span::Symbol;
use mitki_yellow::RedNodePtr;
use mitki_yellow::ast::{HasName as _, Node};
use salsa::{Database, Update};

use crate::ast_map::HasAstMap as _;

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
                    Item::Function(Function(item_tree.functions.alloc(FunctionData { id, name })))
                }
            };

            item_tree.items.push(item);
        }

        item_tree
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Function<'db>(Idx<FunctionData<'db>>);

unsafe impl Update for Function<'_> {
    unsafe fn maybe_update(old_pointer: *mut Self, new_value: Self) -> bool {
        u32::maybe_update(old_pointer.cast(), new_value.0.into_raw().into_u32())
    }
}

#[derive(Debug, Default)]
pub(crate) struct ItemTree<'db> {
    items: Vec<Item<'db>>,
    functions: Arena<FunctionData<'db>>,
}

impl<'db> Index<Function<'db>> for ItemTree<'db> {
    type Output = FunctionData<'db>;

    fn index(&self, index: Function<'db>) -> &Self::Output {
        &self.functions[index.0]
    }
}

impl<'db> ItemTree<'db> {
    pub(crate) fn items(&self) -> impl ExactSizeIterator<Item = Item<'db>> + '_ {
        self.items.iter().copied()
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Item<'db> {
    Function(Function<'db>),
}

#[derive(Debug)]
pub(crate) struct FunctionData<'db> {
    pub(crate) id: Idx<RedNodePtr>,
    pub(crate) name: Symbol<'db>,
}

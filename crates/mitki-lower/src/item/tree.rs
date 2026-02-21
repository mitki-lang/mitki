use std::ops::Index;

use mitki_hir::arena::{Arena, Key};
use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::SyntaxNodePtr;
use mitki_yellow::ast::{HasName as _, Node as _};
use salsa::Database;

use crate::ast_map::HasAstMap as _;

pub type Function<'db> = Key<FunctionData<'db>>;
pub type Struct<'db> = Key<StructData<'db>>;
pub type Enum<'db> = Key<EnumData<'db>>;

pub trait HasItemTree {
    fn item_tree(self, db: &dyn Database) -> &ItemTree<'_>;
}

#[salsa::tracked]
impl HasItemTree for File {
    #[salsa::tracked(returns(ref), no_eq)]
    fn item_tree(self, db: &dyn Database) -> ItemTree<'_> {
        let mut item_tree = ItemTree::default();
        let ast_map = self.ast_map(db);

        for item in self.parse(db).tree().items() {
            let item = match item {
                mitki_yellow::ast::Item::Function(func) => {
                    let id = ast_map.find_id(func.syntax());
                    let Some(name) = func.name().map(|name| name.as_str().into_symbol(db)) else {
                        continue;
                    };

                    Item::Function(item_tree.functions.alloc(FunctionData { id, name }))
                }
                mitki_yellow::ast::Item::Struct(s) => {
                    let id = ast_map.find_id(s.syntax());
                    let Some(name) = s.name().map(|name| name.as_str().into_symbol(db)) else {
                        continue;
                    };

                    Item::Struct(item_tree.structs.alloc(StructData { id, name }))
                }
                mitki_yellow::ast::Item::Enum(e) => {
                    let id = ast_map.find_id(e.syntax());
                    let Some(name) = e.name().map(|name| name.as_str().into_symbol(db)) else {
                        continue;
                    };

                    Item::Enum(item_tree.enums.alloc(EnumData { id, name }))
                }
            };

            item_tree.items.push(item);
        }

        item_tree
    }
}

#[derive(Debug, Default, salsa::Update)]
pub struct ItemTree<'db> {
    items: Vec<Item<'db>>,
    functions: Arena<FunctionData<'db>>,
    structs: Arena<StructData<'db>>,
    enums: Arena<EnumData<'db>>,
}

impl<'db> Index<Function<'db>> for ItemTree<'db> {
    type Output = FunctionData<'db>;

    fn index(&self, index: Function<'db>) -> &Self::Output {
        &self.functions[index]
    }
}

impl<'db> Index<Struct<'db>> for ItemTree<'db> {
    type Output = StructData<'db>;

    fn index(&self, index: Struct<'db>) -> &Self::Output {
        &self.structs[index]
    }
}

impl<'db> Index<Enum<'db>> for ItemTree<'db> {
    type Output = EnumData<'db>;

    fn index(&self, index: Enum<'db>) -> &Self::Output {
        &self.enums[index]
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
    Struct(Struct<'db>),
    Enum(Enum<'db>),
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct FunctionData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct StructData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct EnumData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
}

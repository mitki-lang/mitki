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
pub type BoundaryInstance<'db> = Key<BoundaryInstanceData<'db>>;
pub type Module<'db> = Key<ModuleData<'db>>;
pub type Use<'db> = Key<UseData<'db>>;
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
                mitki_yellow::ast::Item::Instance(instance) => {
                    let id = ast_map.find_id(instance.syntax());
                    let Some(name) = instance.name().map(|name| name.as_str().into_symbol(db))
                    else {
                        continue;
                    };

                    Item::BoundaryInstance(
                        item_tree.boundary_instances.alloc(BoundaryInstanceData { id, name }),
                    )
                }
                mitki_yellow::ast::Item::Module(module) => {
                    let id = ast_map.find_id(module.syntax());
                    let Some(name) = module.name().map(|name| name.as_str().into_symbol(db)) else {
                        continue;
                    };

                    Item::Module(item_tree.modules.alloc(ModuleData {
                        id,
                        name,
                        public: module.is_public(),
                    }))
                }
                mitki_yellow::ast::Item::Use(use_item) => {
                    let id = ast_map.find_id(use_item.syntax());
                    let Some(path) =
                        use_item.path().map(|path| path.syntax().text_trimmed().into_symbol(db))
                    else {
                        continue;
                    };
                    let Some(local_name) = use_item
                        .alias()
                        .map(|name| name.as_str().into_symbol(db))
                        .or_else(|| last_path_segment(db, path))
                    else {
                        continue;
                    };

                    Item::Use(item_tree.uses.alloc(UseData { id, path, local_name }))
                }
                mitki_yellow::ast::Item::Struct(s) => {
                    let id = ast_map.find_id(s.syntax());
                    let Some(name) = s.name().map(|name| name.as_str().into_symbol(db)) else {
                        continue;
                    };
                    let destructor_id =
                        s.destructor().map(|destructor| SyntaxNodePtr::new(destructor.syntax()));

                    Item::Struct(item_tree.structs.alloc(StructData { id, name, destructor_id }))
                }
                mitki_yellow::ast::Item::Enum(e) => {
                    let id = ast_map.find_id(e.syntax());
                    let Some(name) = e.name().map(|name| name.as_str().into_symbol(db)) else {
                        continue;
                    };
                    let destructor_id =
                        e.destructor().map(|destructor| SyntaxNodePtr::new(destructor.syntax()));

                    Item::Enum(item_tree.enums.alloc(EnumData { id, name, destructor_id }))
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
    boundary_instances: Arena<BoundaryInstanceData<'db>>,
    modules: Arena<ModuleData<'db>>,
    uses: Arena<UseData<'db>>,
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

impl<'db> Index<BoundaryInstance<'db>> for ItemTree<'db> {
    type Output = BoundaryInstanceData<'db>;

    fn index(&self, index: BoundaryInstance<'db>) -> &Self::Output {
        &self.boundary_instances[index]
    }
}

impl<'db> Index<Module<'db>> for ItemTree<'db> {
    type Output = ModuleData<'db>;

    fn index(&self, index: Module<'db>) -> &Self::Output {
        &self.modules[index]
    }
}

impl<'db> Index<Use<'db>> for ItemTree<'db> {
    type Output = UseData<'db>;

    fn index(&self, index: Use<'db>) -> &Self::Output {
        &self.uses[index]
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
    BoundaryInstance(BoundaryInstance<'db>),
    Module(Module<'db>),
    Use(Use<'db>),
    Struct(Struct<'db>),
    Enum(Enum<'db>),
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct FunctionData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct BoundaryInstanceData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct ModuleData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
    pub public: bool,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct UseData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub path: Symbol<'db>,
    pub local_name: Symbol<'db>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct StructData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
    pub destructor_id: Option<SyntaxNodePtr>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct EnumData<'db> {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol<'db>,
    pub destructor_id: Option<SyntaxNodePtr>,
}

fn last_path_segment<'db>(db: &'db dyn Database, path: Symbol<'db>) -> Option<Symbol<'db>> {
    path.text(db).rsplit("::").next().map(|segment| segment.into_symbol(db))
}

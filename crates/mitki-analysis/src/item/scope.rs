use mitki_inputs::File;
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::ast::{self, Node as _};
use salsa::Database;

use super::tree::{Item, ItemTree};
use crate::hir::{NodeId, NodeStore};
use crate::item::tree::{Function, HasItemTree as _};

type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

pub(crate) trait HasItemScope {
    fn item_scope(self, db: &dyn Database) -> &ItemScope<'_>;
}

#[salsa::tracked]
impl HasItemScope for File {
    #[salsa::tracked(returns(ref))]
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

#[salsa::tracked]
impl<'db> FunctionLocation<'db> {
    #[salsa::tracked(returns(ref))]
    pub fn signature(self, db: &'db dyn Database) -> Signature<'db> {
        let mut node_store = NodeStore::default();
        let func = self.source(db);

        // Lower parameters
        let params = func.params(db).map_or_else(Vec::new, |param_list| {
            param_list
                .iter(db)
                .map(|param| {
                    let name = param.name(db).as_str(db).into_symbol(db);
                    let type_id = param
                        .ty(db)
                        .and_then(|ty| lower_type_ref(db, &mut node_store, ty))
                        .unwrap_or(NodeId::ZERO);
                    node_store.alloc_param(name, type_id)
                })
                .collect()
        });

        // Lower return type
        let ret_type = func
            .ret_type(db)
            .and_then(|r| r.ty(db))
            .and_then(|ty| lower_type_ref(db, &mut node_store, ty))
            .unwrap_or(NodeId::ZERO);

        Signature::new(db, params, ret_type, node_store)
    }
}

fn lower_type_ref<'db>(
    db: &'db dyn Database,
    node_store: &mut NodeStore<'db>,
    ty: ast::Type,
) -> Option<NodeId> {
    match ty {
        ast::Type::Path(path) => {
            let token = path
                .syntax()
                .children_with_tokens()
                .find_map(|child| {
                    let token = child.into_token()?;
                    if token.is_trivia() { None } else { Some(token) }
                })
                .expect("path should have at least one token");
            let sym = token.text_trimmed().into_symbol(db);
            Some(node_store.alloc_type_ref(sym))
        }
    }
}

impl<'db> FunctionLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::Function<'db> {
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

        let syntax = ptr.to_node(&file.parse(db).syntax_node());
        ast::Function::cast(db, syntax).unwrap()
    }
}

#[salsa::tracked]
pub struct Signature<'db> {
    #[tracked]
    #[returns(deref)]
    pub params: Vec<NodeId>,
    #[tracked]
    pub ret_type: NodeId,
    #[tracked]
    #[returns(ref)]
    pub nodes: NodeStore<'db>,
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

use std::future::Future;
use std::ops::Index;
use std::sync::Arc;

use mitki_hir::arena::{Arena, Key};
use mitki_inputs::File;
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::SyntaxNodePtr;
use mitki_yellow::ast::{HasName as _, Node as _};

use crate::ast_map::AstMap;

pub type Function = Key<FunctionData>;
pub type Struct = Key<StructData>;
pub type Enum = Key<EnumData>;

pub trait HasItemTree {
    fn item_tree<DB>(self, db: &DB) -> impl Future<Output = Arc<ItemTree>> + Send
    where
        DB: ItemTreeDb + Sync;
}

impl HasItemTree for File {
    #[allow(clippy::manual_async_fn)]
    fn item_tree<DB>(self, db: &DB) -> impl Future<Output = Arc<ItemTree>> + Send
    where
        DB: ItemTreeDb + Sync,
    {
        async move { item_tree(db, self).await.expect("failed to compute item tree") }
    }
}

pub trait ItemTreeDb: crate::ast_map::AstMapDb + HasItemTreeQuery {}

impl<T> ItemTreeDb for T where T: crate::ast_map::AstMapDb + HasItemTreeQuery {}

#[rustfmt::skip]
#[picante::tracked]
pub async fn item_tree<DB: mitki_span::SymbolDatabase + mitki_parse::HasParseQuery + crate::ast_map::HasAstMapQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<Arc<ItemTree>> {
    let parsed = mitki_parse::parse(db, file).await?;
    let ast_map = crate::ast_map::ast_map(db, file).await?;
    let item_tree = build_item_tree(db, &parsed, &ast_map);
    Ok(Arc::new(item_tree))
}

fn build_item_tree<DB>(db: &DB, parsed: &mitki_parse::Parsed, ast_map: &AstMap) -> ItemTree
where
    DB: mitki_span::SymbolDatabase,
{
    let mut item_tree = ItemTree::default();

    for item in parsed.tree().items() {
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

#[derive(Debug, Default, facet::Facet)]
pub struct ItemTree {
    #[facet(opaque)]
    items: Vec<Item>,
    #[facet(opaque)]
    functions: Arena<FunctionData>,
    #[facet(opaque)]
    structs: Arena<StructData>,
    #[facet(opaque)]
    enums: Arena<EnumData>,
}

impl Index<Function> for ItemTree {
    type Output = FunctionData;

    fn index(&self, index: Function) -> &Self::Output {
        &self.functions[index]
    }
}

impl Index<Struct> for ItemTree {
    type Output = StructData;

    fn index(&self, index: Struct) -> &Self::Output {
        &self.structs[index]
    }
}

impl Index<Enum> for ItemTree {
    type Output = EnumData;

    fn index(&self, index: Enum) -> &Self::Output {
        &self.enums[index]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Item {
    Function(Function),
    Struct(Struct),
    Enum(Enum),
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionData {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructData {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol,
}

#[derive(Debug, PartialEq, Eq)]
pub struct EnumData {
    pub id: Key<SyntaxNodePtr>,
    pub name: Symbol,
}

use mitki_hir::hir::{NodeStore, ParamId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_inputs::File;
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::SyntaxNodePtr;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use salsa::Database;

use super::tree::{Item, ItemTree};
use crate::ast_map::HasAstMap as _;
use crate::item::tree::{Enum, Function, HasItemTree as _, Struct};

type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

pub trait HasItemScope {
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
    Struct(StructLocation<'db>),
    Enum(EnumLocation<'db>),
}

#[salsa::tracked(debug)]
pub struct FunctionLocation<'db> {
    pub file: File,
    pub index: Function<'db>,
}

#[salsa::tracked(debug)]
pub struct StructLocation<'db> {
    pub file: File,
    pub index: Struct<'db>,
}

#[salsa::tracked(debug)]
pub struct EnumLocation<'db> {
    pub file: File,
    pub index: Enum<'db>,
}

#[salsa::tracked]
impl<'db> FunctionLocation<'db> {
    #[salsa::tracked(returns(ref))]
    pub fn signature(self, db: &'db dyn Database) -> Signature<'db> {
        let mut node_store = NodeStore::default();
        let func = self.source(db);

        // Lower type parameters
        let type_params: Vec<Symbol<'db>> =
            func.type_params().map(|tp| tp.as_str().into_symbol(db)).collect();

        // Lower parameters
        let params = func.params().map_or_else(Vec::new, |param_list| {
            param_list
                .iter()
                .map(|param| {
                    let name = param.name().as_str().into_symbol(db);
                    let type_id = param
                        .ty()
                        .and_then(|ty| lower_type_ref(db, &mut node_store, ty))
                        .unwrap_or(TyId::ZERO);
                    node_store.alloc_param(name, type_id)
                })
                .collect()
        });

        // Lower return type
        let ret_type = func
            .ret_type()
            .and_then(|r| r.ty())
            .and_then(|ty| lower_type_ref(db, &mut node_store, ty))
            .unwrap_or(TyId::ZERO);

        Signature::new(db, type_params, params, ret_type, node_store)
    }
}

fn lower_type_ref<'db>(
    db: &'db dyn Database,
    node_store: &mut NodeStore<'db>,
    ty: ast::Type,
) -> Option<TyId> {
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
            Some(node_store.alloc_type_ref(sym).into())
        }
        ast::Type::Tuple(tuple_type) => {
            let items: Vec<TyId> =
                tuple_type.types().filter_map(|t| lower_type_ref(db, node_store, t)).collect();
            Some(node_store.alloc_type_tuple(items).into())
        }
        ast::Type::Function(function_type) => {
            let inputs = function_type
                .inputs()
                .map(ast::Type::Tuple)
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            let output = function_type
                .output()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            Some(node_store.alloc_type_function(inputs, output).into())
        }
        ast::Type::Union(union_type) => {
            let lhs = union_type
                .lhs()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            let rhs = union_type
                .rhs()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            Some(node_store.alloc_type_union(lhs, rhs).into())
        }
        ast::Type::Inter(inter_type) => {
            let lhs = inter_type
                .lhs()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            let rhs = inter_type
                .rhs()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            Some(node_store.alloc_type_inter(lhs, rhs).into())
        }
        ast::Type::Record(record_type) => {
            let fields: Vec<TyId> = record_type
                .fields()
                .filter_map(|field| {
                    let name = field.name()?;
                    let ty = lower_type_ref(db, node_store, field.ty()?).unwrap_or(TyId::ZERO);
                    let name = name.as_str().into_symbol(db);
                    Some(node_store.alloc_type_field(name, ty).into())
                })
                .collect();
            Some(node_store.alloc_type_record(fields).into())
        }
    }
}

impl<'db> FunctionLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::Function<'db> {
        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let syntax = source_syntax(db, file, ast_map.find_node(item));
        ast::Function::cast(syntax).unwrap()
    }
}

impl<'db> StructLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::StructDef<'db> {
        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let syntax = source_syntax(db, file, ast_map.find_node(item));
        ast::StructDef::cast(syntax).unwrap()
    }
}

impl<'db> EnumLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::EnumDef<'db> {
        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let syntax = source_syntax(db, file, ast_map.find_node(item));
        ast::EnumDef::cast(syntax).unwrap()
    }
}

fn source_syntax<'db>(
    db: &'db dyn Database,
    file: File,
    ptr: &SyntaxNodePtr,
) -> mitki_yellow::SyntaxNode<'db> {
    use mitki_parse::FileParse as _;

    ptr.to_node(&file.parse(db).syntax_node())
}

#[salsa::tracked]
pub struct Signature<'db> {
    #[tracked]
    #[returns(deref)]
    pub type_params: Vec<Symbol<'db>>,
    #[tracked]
    #[returns(deref)]
    pub params: Vec<ParamId>,
    #[tracked]
    pub ret_type: TyId,
    #[tracked]
    #[returns(ref)]
    pub nodes: NodeStore<'db>,
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct ItemScope<'db> {
    values: FxIndexMap<Symbol<'db>, FunctionLocation<'db>>,
    types: FxIndexMap<Symbol<'db>, Ty<'db>>,
    declarations: Vec<Declaration<'db>>,
}

impl<'db> ItemScope<'db> {
    pub fn get(&self, name: &Symbol<'db>) -> Option<FunctionLocation<'db>> {
        self.values.get(name).copied()
    }

    pub fn get_type(&self, name: &Symbol<'db>) -> Option<Ty<'db>> {
        self.types.get(name).copied()
    }

    pub fn types(&self) -> impl Iterator<Item = (&Symbol<'db>, &Ty<'db>)> {
        self.types.iter()
    }

    pub fn declarations(&self) -> &[Declaration<'db>] {
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
                Item::Struct(index) => {
                    let data = &self.item_tree[index];
                    let loc = StructLocation::new(self.db, file, index);
                    self.scope.declarations.push(Declaration::Struct(loc));

                    let source = loc.source(self.db);
                    let fields: Vec<(Symbol<'db>, Ty<'db>)> = source
                        .field_list()
                        .map(|fl| {
                            fl.fields()
                                .filter_map(|f| {
                                    let name = f.name()?.as_str().into_symbol(self.db);
                                    let ty = self.resolve_ast_type(f.ty()?);
                                    Some((name, ty))
                                })
                                .collect()
                        })
                        .unwrap_or_default();

                    let ty = Ty::new(self.db, TyKind::Struct { name: data.name, fields });
                    self.scope.types.insert(data.name, ty);
                }
                Item::Enum(index) => {
                    let data = &self.item_tree[index];
                    let loc = EnumLocation::new(self.db, file, index);
                    self.scope.declarations.push(Declaration::Enum(loc));

                    let source = loc.source(self.db);
                    let variants: Vec<(Symbol<'db>, Vec<Ty<'db>>)> = source
                        .variant_list()
                        .map(|vl| {
                            vl.variants()
                                .filter_map(|v| {
                                    let name = v.name()?.as_str().into_symbol(self.db);
                                    let types = v
                                        .field_types()
                                        .map(|tt| {
                                            tt.types().map(|t| self.resolve_ast_type(t)).collect()
                                        })
                                        .unwrap_or_default();
                                    Some((name, types))
                                })
                                .collect()
                        })
                        .unwrap_or_default();

                    let ty = Ty::new(self.db, TyKind::Enum { name: data.name, variants });
                    self.scope.types.insert(data.name, ty);
                }
            }
        }

        self.scope
    }

    fn resolve_ast_type(&self, ty: ast::Type) -> Ty<'db> {
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
                let name = token.text_trimmed().into_symbol(self.db);

                // Check builtins
                match name.text(self.db) {
                    "bool" => Ty::new(self.db, TyKind::Bool),
                    "int" => Ty::new(self.db, TyKind::Int),
                    "float" => Ty::new(self.db, TyKind::Float),
                    "str" => Ty::new(self.db, TyKind::String),
                    "char" => Ty::new(self.db, TyKind::Char),
                    _ => self
                        .scope
                        .types
                        .get(&name)
                        .copied()
                        .unwrap_or(Ty::new(self.db, TyKind::Unknown)),
                }
            }
            ast::Type::Tuple(tuple_type) => {
                let items: Vec<Ty<'db>> =
                    tuple_type.types().map(|t| self.resolve_ast_type(t)).collect();
                Ty::new(self.db, TyKind::Tuple(items))
            }
            ast::Type::Function(function_type) => {
                let inputs = function_type
                    .inputs()
                    .map(|inputs| {
                        inputs.types().map(|t| self.resolve_ast_type(t)).collect::<Vec<_>>()
                    })
                    .unwrap_or_default();
                let output = function_type
                    .output()
                    .map_or(Ty::new(self.db, TyKind::Unknown), |t| self.resolve_ast_type(t));
                Ty::new(self.db, TyKind::Function { inputs, output })
            }
            ast::Type::Union(union_type) => {
                let lhs = union_type
                    .lhs()
                    .map_or(Ty::new(self.db, TyKind::Unknown), |t| self.resolve_ast_type(t));
                let rhs = union_type
                    .rhs()
                    .map_or(Ty::new(self.db, TyKind::Unknown), |t| self.resolve_ast_type(t));
                Ty::new(self.db, TyKind::Union(vec![lhs, rhs]))
            }
            ast::Type::Inter(inter_type) => {
                let lhs = inter_type
                    .lhs()
                    .map_or(Ty::new(self.db, TyKind::Unknown), |t| self.resolve_ast_type(t));
                let rhs = inter_type
                    .rhs()
                    .map_or(Ty::new(self.db, TyKind::Unknown), |t| self.resolve_ast_type(t));
                Ty::new(self.db, TyKind::Inter(vec![lhs, rhs]))
            }
            ast::Type::Record(record_type) => {
                let fields = record_type
                    .fields()
                    .filter_map(|field| {
                        let name = field.name()?;
                        let ty = field.ty().map(|t| self.resolve_ast_type(t))?;
                        Some((name.as_str().into_symbol(self.db), ty))
                    })
                    .collect();
                Ty::new(self.db, TyKind::Record(fields))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use mitki_inputs::File;
    use mitki_yellow::ast::HasName as _;

    use super::{Declaration, HasItemScope};

    #[salsa::db]
    #[derive(Default)]
    struct TestDb {
        storage: salsa::Storage<Self>,
    }

    #[salsa::db]
    impl salsa::Database for TestDb {}

    #[test]
    fn source_methods_resolve_each_declaration_kind() {
        let db = TestDb::default();
        let file = File::new(
            &db,
            "source_paths.mtk".into(),
            r#"
fun main() {}

struct Point {
    x: int,
}

enum Color {
    Red,
}
"#
            .to_owned(),
        );

        let scope = file.item_scope(&db);
        let mut saw_function = false;
        let mut saw_struct = false;
        let mut saw_enum = false;

        for declaration in scope.declarations() {
            match *declaration {
                Declaration::Function(location) => {
                    let source = location.source(&db);
                    assert_eq!(source.name().unwrap().as_str(), "main");
                    saw_function = true;
                }
                Declaration::Struct(location) => {
                    let source = location.source(&db);
                    assert_eq!(source.name().unwrap().as_str(), "Point");
                    saw_struct = true;
                }
                Declaration::Enum(location) => {
                    let source = location.source(&db);
                    assert_eq!(source.name().unwrap().as_str(), "Color");
                    saw_enum = true;
                }
            }
        }

        assert!(saw_function, "expected to resolve function source");
        assert!(saw_struct, "expected to resolve struct source");
        assert!(saw_enum, "expected to resolve enum source");
    }
}

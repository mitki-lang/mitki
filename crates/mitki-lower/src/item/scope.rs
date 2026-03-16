use mitki_hir::hir::{ExprId, NodeStore, ParamId, PatId, TyId};
use mitki_hir::ty::{EnumTy, ExactInt, StructTy, Ty, TyKind};
use mitki_inputs::{File, ModuleId, PackageId};
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::SyntaxNodePtr;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use salsa::Database;

use super::package::{HasPackage as _, child_module_named, package_modules, root_module};
use super::stdlib::stdlib_package;
use super::tree::{BoundaryInstance, Item, ItemTree, UseData};
use crate::ast_map::HasAstMap as _;
use crate::item::tree::{Enum, Function, HasItemTree as _, Struct};

type FxIndexMap<K, V> =
    indexmap::IndexMap<K, V, std::hash::BuildHasherDefault<rustc_hash::FxHasher>>;

pub trait HasItemDecls<'db> {
    fn item_decls(self, db: &'db dyn Database) -> &'db ItemDecls<'db>;
}

pub trait HasVisibleItems<'db> {
    fn visible_items(self, db: &'db dyn Database) -> &'db VisibleItems<'db>;
}

pub trait HasPackageDecls<'db> {
    fn package_decls(self, db: &'db dyn Database) -> &'db ItemDecls<'db>;
}

#[salsa::tracked]
impl<'db> HasItemDecls<'db> for File {
    #[salsa::tracked(returns(ref))]
    fn item_decls(self, db: &'db dyn Database) -> ItemDecls<'db> {
        ItemCollector::new(db, self).build_decls()
    }
}

#[salsa::tracked]
impl<'db> HasVisibleItems<'db> for File {
    #[salsa::tracked(returns(ref))]
    fn visible_items(self, db: &'db dyn Database) -> VisibleItems<'db> {
        ItemCollector::new(db, self).build_visible_items()
    }
}

#[salsa::tracked]
impl<'db> HasItemDecls<'db> for ModuleId<'db> {
    #[salsa::tracked(returns(ref))]
    fn item_decls(self, db: &'db dyn Database) -> ItemDecls<'db> {
        ItemCollector::for_module(db, self).build_decls()
    }
}

#[salsa::tracked]
impl<'db> HasVisibleItems<'db> for ModuleId<'db> {
    #[salsa::tracked(returns(ref))]
    fn visible_items(self, db: &'db dyn Database) -> VisibleItems<'db> {
        ItemCollector::for_module(db, self).build_visible_items()
    }
}

#[salsa::tracked]
impl<'db> HasPackageDecls<'db> for PackageId<'db> {
    #[salsa::tracked(returns(ref))]
    fn package_decls(self, db: &'db dyn Database) -> ItemDecls<'db> {
        let mut decls = ItemDecls::default();
        for &module in package_modules(db, self) {
            let module_decls = module.item_decls(db);
            decls.declarations.extend_from_slice(module_decls.declarations());
            for &variant in module_decls.enum_variants() {
                decls.enum_variants.push(variant);
                decls.enum_variants_by_name.entry(variant.name(db)).or_default().push(variant);
            }
        }
        decls
    }
}

#[derive(salsa::Update, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Declaration<'db> {
    Function(FunctionLocation<'db>),
    BoundaryInstance(BoundaryInstanceLocation<'db>),
    Struct(StructLocation<'db>),
    Enum(EnumLocation<'db>),
}

#[derive(salsa::Update, Debug, PartialEq, Eq, Clone, Copy)]
pub enum TypeDeclaration<'db> {
    Struct(StructLocation<'db>),
    Enum(EnumLocation<'db>),
}

#[salsa::tracked(debug)]
pub struct FunctionLocation<'db> {
    pub module: ModuleId<'db>,
    pub index: Function<'db>,
}

#[derive(salsa::Update, Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum BoundaryInstanceKind {
    Import,
    Export,
}

#[salsa::tracked(debug)]
pub struct BoundaryInstanceLocation<'db> {
    pub module: ModuleId<'db>,
    pub index: BoundaryInstance<'db>,
}

#[salsa::tracked(debug)]
pub struct StructLocation<'db> {
    pub module: ModuleId<'db>,
    pub index: Struct<'db>,
}

#[salsa::tracked(debug)]
pub struct EnumLocation<'db> {
    pub module: ModuleId<'db>,
    pub index: Enum<'db>,
}

#[salsa::tracked(debug)]
pub struct StructDestructorLocation<'db> {
    pub parent: StructLocation<'db>,
}

#[salsa::tracked(debug)]
pub struct EnumDestructorLocation<'db> {
    pub parent: EnumLocation<'db>,
}

#[salsa::tracked(debug)]
pub struct EnumVariantLocation<'db> {
    pub parent: EnumLocation<'db>,
    pub variant_index: u32,
}

#[salsa::tracked]
impl<'db> FunctionLocation<'db> {
    pub fn file(self, db: &'db dyn Database) -> File {
        self.module(db).file(db)
    }

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
                    let pattern = param
                        .pattern()
                        .map(|pattern| lower_pattern_ref(db, &mut node_store, pattern));
                    let type_id = param
                        .ty()
                        .and_then(|ty| lower_type_ref(db, &mut node_store, ty))
                        .unwrap_or(TyId::ZERO);
                    node_store.alloc_param(pattern.unwrap_or(PatId::ZERO), type_id)
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

#[salsa::tracked]
impl<'db> BoundaryInstanceLocation<'db> {
    pub fn file(self, db: &'db dyn Database) -> File {
        self.module(db).file(db)
    }

    #[salsa::tracked(returns(ref))]
    pub fn type_args(self, db: &'db dyn Database) -> Vec<Ty<'db>> {
        let visible_items = self.module(db).visible_items(db);
        self.source(db)
            .type_args()
            .into_iter()
            .map(|ty| resolve_ast_type_in_scope(db, self.module(db), visible_items, ty))
            .collect()
    }
}

impl<'db> BoundaryInstanceLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::InstanceItem<'db> {
        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let syntax = source_syntax(db, file, ast_map.find_node(item));
        ast::InstanceItem::cast(syntax).unwrap()
    }

    pub fn kind(self, db: &'db dyn Database) -> BoundaryInstanceKind {
        let source = self.source(db);
        if source.is_imported() {
            BoundaryInstanceKind::Import
        } else {
            BoundaryInstanceKind::Export
        }
    }

    pub fn origin(self, db: &'db dyn Database) -> Option<FunctionLocation<'db>> {
        let source = self.source(db);
        let name = source.name()?.as_str().into_symbol(db);
        self.module(db).visible_items(db).get_value(&name)
    }
}

fn lower_type_ref<'db>(
    db: &'db dyn Database,
    node_store: &mut NodeStore<'db>,
    ty: ast::Type,
) -> Option<TyId> {
    match ty {
        ast::Type::Path(path) => {
            let path_ref: TyId = node_store.alloc_type_ref(path.path_text().into_symbol(db)).into();
            let type_args = path
                .type_args()
                .into_iter()
                .filter_map(|arg| lower_type_ref(db, node_store, arg))
                .collect::<Vec<_>>();
            if type_args.is_empty() {
                Some(path_ref)
            } else {
                let args = node_store.alloc_type_tuple(type_args).into();
                Some(node_store.alloc_type_apply(path_ref, args).into())
            }
        }
        ast::Type::Array(array_type) => {
            let item = array_type
                .item()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            Some(node_store.alloc_type_array(item, TyId::ZERO).into())
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
        ast::Type::Pointer(pointer_type) => {
            let item = pointer_type
                .pointee()
                .and_then(|ty| lower_type_ref(db, node_store, ty))
                .unwrap_or(TyId::ZERO);
            if pointer_type.is_mut() {
                Some(node_store.alloc_type_ptr_mut(item, TyId::ZERO).into())
            } else {
                Some(node_store.alloc_type_ptr_const(item, TyId::ZERO).into())
            }
        }
    }
}

fn lower_pattern_ref<'db>(
    db: &'db dyn Database,
    node_store: &mut NodeStore<'db>,
    pattern: ast::Pattern<'db>,
) -> PatId {
    match pattern {
        ast::Pattern::Binding(binding) => {
            let name = binding.name().map_or("", |name| name.as_str()).into_symbol(db);
            node_store.alloc_pat_binding(name, PatId::ZERO).into()
        }
        ast::Pattern::Wildcard(_) => node_store.alloc_pat_wildcard().into(),
        ast::Pattern::Literal(literal) => match literal.kind() {
            ast::LiteralKind::Bool(true) => node_store.alloc_pat_true().into(),
            ast::LiteralKind::Bool(false) => node_store.alloc_pat_false().into(),
            ast::LiteralKind::Int(token) => {
                node_store.alloc_pat_int(Some(token.text_trimmed().into_symbol(db))).into()
            }
            ast::LiteralKind::Float(token) => {
                node_store.alloc_pat_float(Some(token.text_trimmed().into_symbol(db))).into()
            }
            ast::LiteralKind::String(token) => {
                node_store.alloc_pat_string(Some(token.text_trimmed().into_symbol(db))).into()
            }
            ast::LiteralKind::Char(token) => {
                node_store.alloc_pat_char(Some(token.text_trimmed().into_symbol(db))).into()
            }
        },
        ast::Pattern::Typed(typed) => {
            let inner = typed
                .pattern()
                .map_or(PatId::ZERO, |pattern| lower_pattern_ref(db, node_store, pattern));
            let ty =
                typed.ty().and_then(|ty| lower_type_ref(db, node_store, ty)).unwrap_or(TyId::ZERO);
            node_store.alloc_pat_typed(inner, ty).into()
        }
        ast::Pattern::Paren(paren) => {
            let inner = paren
                .pattern()
                .map_or(PatId::ZERO, |pattern| lower_pattern_ref(db, node_store, pattern));
            node_store.alloc_pat_paren(inner, PatId::ZERO).into()
        }
        ast::Pattern::Tuple(tuple) => {
            let items: Vec<_> = tuple
                .patterns()
                .map(|pattern| lower_pattern_ref(db, node_store, pattern))
                .collect();
            node_store.alloc_pat_tuple(items).into()
        }
        ast::Pattern::Variant(variant) => {
            let path = variant
                .path()
                .as_ref()
                .map_or(ExprId::ZERO, |path| lower_field_pattern_path(db, node_store, path));
            let args: Vec<_> = variant
                .patterns()
                .map(|pattern| lower_pattern_ref(db, node_store, pattern))
                .collect();
            node_store.alloc_pat_variant(path, args).into()
        }
        ast::Pattern::Struct(struct_pattern) => {
            let path = struct_pattern
                .path()
                .as_ref()
                .map_or(ExprId::ZERO, |path| lower_path_pattern_path(db, node_store, path));
            let fields = struct_pattern
                .fields()
                .map(|field| {
                    let name = field.name().map_or("", |name| name.as_str()).into_symbol(db);
                    let pat = field
                        .pattern()
                        .map_or(PatId::ZERO, |pattern| lower_pattern_ref(db, node_store, pattern));
                    node_store.alloc_pat_struct_field(name, pat)
                })
                .collect::<Vec<_>>();
            node_store.alloc_pat_struct(path, fields).into()
        }
    }
}

fn lower_path_pattern_path<'db>(
    db: &'db dyn Database,
    node_store: &mut NodeStore<'db>,
    path: &ast::PathPattern<'db>,
) -> ExprId {
    let name = path.name().map_or("", |name| name.as_str()).into_symbol(db);
    node_store.alloc_name(name).into()
}

fn lower_field_pattern_path<'db>(
    db: &'db dyn Database,
    node_store: &mut NodeStore<'db>,
    path: &ast::FieldPattern<'db>,
) -> ExprId {
    let base = path
        .base()
        .as_ref()
        .map_or(ExprId::ZERO, |base| lower_path_pattern_path(db, node_store, base));
    let name = path.name().map_or("", |name| name.as_str()).into_symbol(db);
    let field_name: ExprId = node_store.alloc_name(name).into();
    node_store.alloc_field(base, field_name).into()
}

impl<'db> FunctionLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::Function<'db> {
        let file = self.file(db);
        let ptr = file.ast_map(db).find_node(file.item_tree(db)[self.index(db)].id);
        let syntax = source_syntax(db, file, ptr);
        ast::Function::cast(syntax).unwrap()
    }
}

impl<'db> StructLocation<'db> {
    pub fn file(self, db: &'db dyn Database) -> File {
        self.module(db).file(db)
    }

    pub fn source(self, db: &'db dyn Database) -> ast::StructDef<'db> {
        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let syntax = source_syntax(db, file, ast_map.find_node(item));
        ast::StructDef::cast(syntax).unwrap()
    }

    pub fn destructor(self, db: &'db dyn Database) -> Option<StructDestructorLocation<'db>> {
        self.source(db).destructor().map(|_| StructDestructorLocation::new(db, self))
    }
}

impl<'db> EnumLocation<'db> {
    pub fn file(self, db: &'db dyn Database) -> File {
        self.module(db).file(db)
    }

    pub fn source(self, db: &'db dyn Database) -> ast::EnumDef<'db> {
        let file = self.file(db);
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);
        let index = self.index(db);

        let item = item_tree[index].id;
        let syntax = source_syntax(db, file, ast_map.find_node(item));
        ast::EnumDef::cast(syntax).unwrap()
    }

    pub fn destructor(self, db: &'db dyn Database) -> Option<EnumDestructorLocation<'db>> {
        self.source(db).destructor().map(|_| EnumDestructorLocation::new(db, self))
    }
}

impl<'db> StructDestructorLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::DestructorDef<'db> {
        self.parent(db).source(db).destructor().expect("struct destructor should exist")
    }

    pub fn file(self, db: &'db dyn Database) -> File {
        self.parent(db).file(db)
    }
}

impl<'db> EnumDestructorLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::DestructorDef<'db> {
        self.parent(db).source(db).destructor().expect("enum destructor should exist")
    }

    pub fn file(self, db: &'db dyn Database) -> File {
        self.parent(db).file(db)
    }
}

impl<'db> EnumVariantLocation<'db> {
    pub fn source(self, db: &'db dyn Database) -> ast::EnumVariant<'db> {
        let variants = self.parent(db).source(db).variant_list().expect("enum variant list");
        variants
            .variants()
            .nth(self.variant_index(db) as usize)
            .expect("enum variant index should be valid")
    }

    pub fn name(self, db: &'db dyn Database) -> Symbol<'db> {
        self.source(db).name().expect("enum variant should have a name").as_str().into_symbol(db)
    }
}

#[salsa::tracked(returns(ref))]
pub fn struct_fields<'db>(
    db: &'db dyn Database,
    nominal: StructTy<'db>,
) -> Vec<(Symbol<'db>, Ty<'db>)> {
    let source = struct_source(db, nominal);
    let visible_items = nominal.module(db).visible_items(db);
    let type_params = nominal_type_param_env(db, source.type_params());
    source
        .field_list()
        .map(|field_list| {
            field_list
                .fields()
                .filter_map(|field| {
                    let name = field.name()?.as_str().into_symbol(db);
                    let ty = resolve_ast_type_in_scope_with_type_params(
                        db,
                        nominal.module(db),
                        visible_items,
                        &type_params,
                        field.ty()?,
                    );
                    Some((name, substitute_type_params(db, ty, nominal.args(db))))
                })
                .collect()
        })
        .unwrap_or_default()
}

#[salsa::tracked(returns(ref))]
pub fn enum_variants<'db>(
    db: &'db dyn Database,
    nominal: EnumTy<'db>,
) -> Vec<(Symbol<'db>, Vec<Ty<'db>>)> {
    let source = enum_source(db, nominal);
    let visible_items = nominal.module(db).visible_items(db);
    let type_params = nominal_type_param_env(db, source.type_params());
    source
        .variant_list()
        .map(|variant_list| {
            variant_list
                .variants()
                .filter_map(|variant| {
                    let name = variant.name()?.as_str().into_symbol(db);
                    let fields = variant
                        .field_types()
                        .map(|types| {
                            types
                                .types()
                                .map(|ty| {
                                    substitute_type_params(
                                        db,
                                        resolve_ast_type_in_scope_with_type_params(
                                            db,
                                            nominal.module(db),
                                            visible_items,
                                            &type_params,
                                            ty,
                                        ),
                                        nominal.args(db),
                                    )
                                })
                                .collect()
                        })
                        .unwrap_or_default();
                    Some((name, fields))
                })
                .collect()
        })
        .unwrap_or_default()
}

fn source_syntax<'db>(
    db: &'db dyn Database,
    file: File,
    ptr: &SyntaxNodePtr,
) -> mitki_yellow::SyntaxNode<'db> {
    use mitki_parse::FileParse as _;

    ptr.to_node(&file.parse(db).syntax_node())
}

fn struct_source<'db>(db: &'db dyn Database, nominal: StructTy<'db>) -> ast::StructDef<'db> {
    let file = nominal.file(db);
    let item_tree = file.item_tree(db);
    let ast_map = file.ast_map(db);
    let index = Struct::new(nominal.index(db));
    let item = item_tree[index].id;
    let syntax = source_syntax(db, file, ast_map.find_node(item));
    ast::StructDef::cast(syntax).unwrap()
}

fn enum_source<'db>(db: &'db dyn Database, nominal: EnumTy<'db>) -> ast::EnumDef<'db> {
    let file = nominal.file(db);
    let item_tree = file.item_tree(db);
    let ast_map = file.ast_map(db);
    let index = Enum::new(nominal.index(db));
    let item = item_tree[index].id;
    let syntax = source_syntax(db, file, ast_map.find_node(item));
    ast::EnumDef::cast(syntax).unwrap()
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
pub struct ItemDecls<'db> {
    declarations: Vec<Declaration<'db>>,
    enum_variants: Vec<EnumVariantLocation<'db>>,
    enum_variants_by_name: FxIndexMap<Symbol<'db>, Vec<EnumVariantLocation<'db>>>,
}

impl<'db> ItemDecls<'db> {
    pub fn declarations(&self) -> &[Declaration<'db>] {
        &self.declarations
    }

    pub fn enum_variants(&self) -> &[EnumVariantLocation<'db>] {
        &self.enum_variants
    }

    pub fn enum_variants_by_name(&self, name: &Symbol<'db>) -> &[EnumVariantLocation<'db>] {
        self.enum_variants_by_name.get(name).map_or(&[], Vec::as_slice)
    }
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct VisibleItems<'db> {
    values: FxIndexMap<Symbol<'db>, FunctionLocation<'db>>,
    types: FxIndexMap<Symbol<'db>, Ty<'db>>,
    type_declarations: FxIndexMap<Symbol<'db>, TypeDeclaration<'db>>,
    modules: FxIndexMap<Symbol<'db>, ModuleBinding<'db>>,
    module_aliases: FxIndexMap<Symbol<'db>, ModuleBinding<'db>>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, salsa::Update)]
pub struct ModuleBinding<'db> {
    pub module: ModuleId<'db>,
    pub public: bool,
}

impl<'db> VisibleItems<'db> {
    pub fn get_value(&self, name: &Symbol<'db>) -> Option<FunctionLocation<'db>> {
        self.values.get(name).copied()
    }

    pub fn get_type(&self, name: &Symbol<'db>) -> Option<Ty<'db>> {
        self.types.get(name).copied()
    }

    pub fn get_type_declaration(&self, name: &Symbol<'db>) -> Option<TypeDeclaration<'db>> {
        self.type_declarations.get(name).copied()
    }

    pub fn get_module(&self, name: &Symbol<'db>) -> Option<ModuleBinding<'db>> {
        self.modules.get(name).copied()
    }

    pub fn get_module_alias(&self, name: &Symbol<'db>) -> Option<ModuleBinding<'db>> {
        self.module_aliases.get(name).copied()
    }

    pub fn types(&self) -> impl Iterator<Item = (&Symbol<'db>, &Ty<'db>)> {
        self.types.iter()
    }

    pub fn values(&self) -> impl Iterator<Item = (&Symbol<'db>, &FunctionLocation<'db>)> {
        self.values.iter()
    }

    pub fn modules(&self) -> impl Iterator<Item = (&Symbol<'db>, &ModuleBinding<'db>)> {
        self.modules.iter()
    }
}

struct ItemCollector<'db> {
    db: &'db dyn Database,
    module: ModuleId<'db>,
    item_tree: &'db ItemTree<'db>,
}

impl<'db> ItemCollector<'db> {
    fn new(db: &'db dyn Database, file: File) -> Self {
        let module = root_module(db, file.package(db));
        Self { db, module, item_tree: file.item_tree(db) }
    }

    fn for_module(db: &'db dyn Database, module: ModuleId<'db>) -> Self {
        let file = module.file(db);
        Self { db, module, item_tree: file.item_tree(db) }
    }

    fn build_decls(&self) -> ItemDecls<'db> {
        let mut decls = ItemDecls::default();

        for item in self.item_tree.items() {
            match item {
                Item::Function(index) => {
                    let func_loc = FunctionLocation::new(self.db, self.module, index);
                    decls.declarations.push(Declaration::Function(func_loc));
                }
                Item::BoundaryInstance(index) => {
                    let loc = BoundaryInstanceLocation::new(self.db, self.module, index);
                    decls.declarations.push(Declaration::BoundaryInstance(loc));
                }
                Item::Module(_) => {}
                Item::Use(_) => {}
                Item::Struct(index) => {
                    let loc = StructLocation::new(self.db, self.module, index);
                    decls.declarations.push(Declaration::Struct(loc));
                }
                Item::Enum(index) => {
                    let loc = EnumLocation::new(self.db, self.module, index);
                    decls.declarations.push(Declaration::Enum(loc));

                    let variants = loc
                        .source(self.db)
                        .variant_list()
                        .map(|variants| variants.variants().collect::<Vec<_>>())
                        .unwrap_or_default();
                    for (variant_index, variant) in variants.into_iter().enumerate() {
                        let Some(name) = variant.name() else {
                            continue;
                        };
                        let variant_loc =
                            EnumVariantLocation::new(self.db, loc, variant_index as u32);
                        decls.enum_variants.push(variant_loc);
                        decls
                            .enum_variants_by_name
                            .entry(name.as_str().into_symbol(self.db))
                            .or_default()
                            .push(variant_loc);
                    }
                }
            }
        }

        decls
    }

    fn build_visible_items(&self) -> VisibleItems<'db> {
        let mut visible_items = VisibleItems::default();

        for item in self.item_tree.items() {
            match item {
                Item::Function(index) => {
                    let func = &self.item_tree[index];
                    let func_loc = FunctionLocation::new(self.db, self.module, index);
                    visible_items.values.insert(func.name, func_loc);
                }
                Item::BoundaryInstance(index) => {
                    let _ = BoundaryInstanceLocation::new(self.db, self.module, index);
                }
                Item::Module(index) => {
                    let data = &self.item_tree[index];
                    if let Some(child) =
                        child_module_named(self.db, self.module, data.name.text(self.db).to_owned())
                    {
                        visible_items.modules.insert(
                            data.name,
                            ModuleBinding { module: child, public: data.public },
                        );
                    }
                }
                Item::Use(_) => {}
                Item::Struct(index) => {
                    let data = &self.item_tree[index];
                    let loc = StructLocation::new(self.db, self.module, index);
                    let nominal =
                        StructTy::new(self.db, self.module, index.index(), data.name, Vec::new());
                    let ty = if loc.source(self.db).is_extern() {
                        Ty::new(self.db, TyKind::ExternStruct(nominal))
                    } else {
                        Ty::new(self.db, TyKind::Struct(nominal))
                    };
                    visible_items.types.insert(data.name, ty);
                    visible_items.type_declarations.insert(data.name, TypeDeclaration::Struct(loc));
                }
                Item::Enum(index) => {
                    let data = &self.item_tree[index];
                    let loc = EnumLocation::new(self.db, self.module, index);
                    let nominal =
                        EnumTy::new(self.db, self.module, index.index(), data.name, Vec::new());
                    let ty = Ty::new(self.db, TyKind::Enum(nominal));
                    visible_items.types.insert(data.name, ty);
                    visible_items.type_declarations.insert(data.name, TypeDeclaration::Enum(loc));
                }
            }
        }

        for item in self.item_tree.items() {
            let Item::Use(index) = item else {
                continue;
            };
            self.import_use_item(&mut visible_items, &self.item_tree[index]);
        }

        visible_items
    }

    fn import_use_item(&self, visible_items: &mut VisibleItems<'db>, use_data: &UseData<'db>) {
        if let Some(function) =
            resolve_use_value(self.db, self.module, visible_items, use_data.path)
        {
            visible_items.values.insert(use_data.local_name, function);
        }

        if let Some((ty, declaration)) =
            resolve_use_type(self.db, self.module, visible_items, use_data.path)
        {
            visible_items.types.insert(use_data.local_name, ty);
            visible_items.type_declarations.insert(use_data.local_name, declaration);
        }

        if let Some(module) = resolve_use_module(self.db, self.module, visible_items, use_data.path)
        {
            visible_items.module_aliases.insert(use_data.local_name, module);
        }
    }
}

fn resolve_ast_type_in_scope<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    ty: ast::Type,
) -> Ty<'db> {
    resolve_ast_type_in_scope_with_type_params(
        db,
        current_module,
        visible_items,
        &FxIndexMap::default(),
        ty,
    )
}

fn resolve_ast_type_in_scope_with_type_params<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    type_params: &FxIndexMap<Symbol<'db>, Ty<'db>>,
    ty: ast::Type,
) -> Ty<'db> {
    match ty {
        ast::Type::Path(path) => {
            let name = path.path_text().into_symbol(db);
            let type_args = path
                .type_args()
                .into_iter()
                .map(|arg| {
                    resolve_ast_type_in_scope_with_type_params(
                        db,
                        current_module,
                        visible_items,
                        type_params,
                        arg,
                    )
                })
                .collect::<Vec<_>>();

            match resolve_type_name_in_scope(db, current_module, visible_items, type_params, name) {
                Some(base) if type_args.is_empty() => base,
                Some(base) => instantiate_nominal_type(db, base, type_args)
                    .unwrap_or_else(|| Ty::new(db, TyKind::Unknown)),
                None => Ty::new(db, TyKind::Unknown),
            }
        }
        ast::Type::Array(array_type) => {
            let item = array_type.item().map_or(Ty::new(db, TyKind::Unknown), |item| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    item,
                )
            });
            Ty::new(db, TyKind::Array(item))
        }
        ast::Type::Tuple(tuple_type) => {
            let items = tuple_type
                .types()
                .map(|item| {
                    resolve_ast_type_in_scope_with_type_params(
                        db,
                        current_module,
                        visible_items,
                        type_params,
                        item,
                    )
                })
                .collect();
            Ty::new(db, TyKind::Tuple(items))
        }
        ast::Type::Function(function_type) => {
            let inputs = function_type
                .inputs()
                .map(|inputs| {
                    inputs
                        .types()
                        .map(|item| {
                            resolve_ast_type_in_scope_with_type_params(
                                db,
                                current_module,
                                visible_items,
                                type_params,
                                item,
                            )
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let output = function_type.output().map_or(Ty::new(db, TyKind::Unknown), |output| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    output,
                )
            });
            Ty::new(db, TyKind::Function { inputs, output })
        }
        ast::Type::Union(union_type) => {
            let lhs = union_type.lhs().map_or(Ty::new(db, TyKind::Unknown), |lhs| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    lhs,
                )
            });
            let rhs = union_type.rhs().map_or(Ty::new(db, TyKind::Unknown), |rhs| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    rhs,
                )
            });
            Ty::new(db, TyKind::Union(vec![lhs, rhs]))
        }
        ast::Type::Inter(inter_type) => {
            let lhs = inter_type.lhs().map_or(Ty::new(db, TyKind::Unknown), |lhs| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    lhs,
                )
            });
            let rhs = inter_type.rhs().map_or(Ty::new(db, TyKind::Unknown), |rhs| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    rhs,
                )
            });
            Ty::new(db, TyKind::Inter(vec![lhs, rhs]))
        }
        ast::Type::Record(record_type) => {
            let fields = record_type
                .fields()
                .filter_map(|field| {
                    let name = field.name()?;
                    let ty = field.ty().map(|ty| {
                        resolve_ast_type_in_scope_with_type_params(
                            db,
                            current_module,
                            visible_items,
                            type_params,
                            ty,
                        )
                    })?;
                    Some((name.as_str().into_symbol(db), ty))
                })
                .collect();
            Ty::new(db, TyKind::Record(fields))
        }
        ast::Type::Pointer(pointer_type) => {
            let pointee = pointer_type.pointee().map_or(Ty::new(db, TyKind::Unknown), |item| {
                resolve_ast_type_in_scope_with_type_params(
                    db,
                    current_module,
                    visible_items,
                    type_params,
                    item,
                )
            });
            Ty::new(db, TyKind::Pointer { mutable: pointer_type.is_mut(), pointee })
        }
    }
}

fn resolve_type_name_in_scope<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    type_params: &FxIndexMap<Symbol<'db>, Ty<'db>>,
    name: Symbol<'db>,
) -> Option<Ty<'db>> {
    if let Some(&ty) = type_params.get(&name) {
        return Some(ty);
    }

    if let Some(ty) = builtin_type(name.text(db), db) {
        return Some(ty);
    }

    if let Some((module, last)) =
        resolve_segment_path_target(db, current_module, visible_items, name, AccessKind::Type)
    {
        return module.visible_items(db).get_type(&last);
    }

    visible_items.get_type(&name)
}

pub fn instantiate_nominal_type<'db>(
    db: &'db dyn Database,
    base: Ty<'db>,
    args: Vec<Ty<'db>>,
) -> Option<Ty<'db>> {
    match base.kind(db) {
        TyKind::Struct(struct_ty) => instantiate_struct_type(db, *struct_ty, args)
            .map(|nominal| Ty::new(db, TyKind::Struct(nominal))),
        TyKind::ExternStruct(struct_ty) => instantiate_struct_type(db, *struct_ty, args)
            .map(|nominal| Ty::new(db, TyKind::ExternStruct(nominal))),
        TyKind::Enum(enum_ty) => instantiate_enum_type(db, *enum_ty, args)
            .map(|nominal| Ty::new(db, TyKind::Enum(nominal))),
        _ => None,
    }
}

#[salsa::tracked(returns(ref))]
pub fn struct_type_params<'db>(db: &'db dyn Database, nominal: StructTy<'db>) -> Vec<Symbol<'db>> {
    struct_source(db, nominal).type_params().map(|tp| tp.as_str().into_symbol(db)).collect()
}

#[salsa::tracked(returns(ref))]
pub fn enum_type_params<'db>(db: &'db dyn Database, nominal: EnumTy<'db>) -> Vec<Symbol<'db>> {
    enum_source(db, nominal).type_params().map(|tp| tp.as_str().into_symbol(db)).collect()
}

fn instantiate_struct_type<'db>(
    db: &'db dyn Database,
    nominal: StructTy<'db>,
    args: Vec<Ty<'db>>,
) -> Option<StructTy<'db>> {
    (struct_type_params(db, nominal).len() == args.len())
        .then(|| StructTy::new(db, nominal.module(db), nominal.index(db), nominal.name(db), args))
}

fn instantiate_enum_type<'db>(
    db: &'db dyn Database,
    nominal: EnumTy<'db>,
    args: Vec<Ty<'db>>,
) -> Option<EnumTy<'db>> {
    (enum_type_params(db, nominal).len() == args.len())
        .then(|| EnumTy::new(db, nominal.module(db), nominal.index(db), nominal.name(db), args))
}

fn substitute_type_params<'db>(db: &'db dyn Database, ty: Ty<'db>, args: &[Ty<'db>]) -> Ty<'db> {
    match ty.kind(db) {
        TyKind::Var(id) => args.get(*id as usize).copied().unwrap_or(ty),
        TyKind::Array(item) => Ty::new(db, TyKind::Array(substitute_type_params(db, *item, args))),
        TyKind::Tuple(items) => Ty::new(
            db,
            TyKind::Tuple(
                items.iter().map(|&item| substitute_type_params(db, item, args)).collect(),
            ),
        ),
        TyKind::Record(fields) => Ty::new(
            db,
            TyKind::Record(
                fields
                    .iter()
                    .map(|(name, ty)| (*name, substitute_type_params(db, *ty, args)))
                    .collect(),
            ),
        ),
        TyKind::Pointer { mutable, pointee } => Ty::new(
            db,
            TyKind::Pointer {
                mutable: *mutable,
                pointee: substitute_type_params(db, *pointee, args),
            },
        ),
        TyKind::Function { inputs, output } => Ty::new(
            db,
            TyKind::Function {
                inputs: inputs
                    .iter()
                    .map(|&input| substitute_type_params(db, input, args))
                    .collect(),
                output: substitute_type_params(db, *output, args),
            },
        ),
        TyKind::Union(items) => Ty::new(
            db,
            TyKind::Union(
                items.iter().map(|&item| substitute_type_params(db, item, args)).collect(),
            ),
        ),
        TyKind::Inter(items) => Ty::new(
            db,
            TyKind::Inter(
                items.iter().map(|&item| substitute_type_params(db, item, args)).collect(),
            ),
        ),
        TyKind::Rec(id, body) => {
            Ty::new(db, TyKind::Rec(*id, substitute_type_params(db, *body, args)))
        }
        TyKind::Struct(struct_ty) => Ty::new(
            db,
            TyKind::Struct(StructTy::new(
                db,
                struct_ty.module(db),
                struct_ty.index(db),
                struct_ty.name(db),
                struct_ty
                    .args(db)
                    .iter()
                    .map(|&arg| substitute_type_params(db, arg, args))
                    .collect::<Vec<_>>(),
            )),
        ),
        TyKind::ExternStruct(struct_ty) => Ty::new(
            db,
            TyKind::ExternStruct(StructTy::new(
                db,
                struct_ty.module(db),
                struct_ty.index(db),
                struct_ty.name(db),
                struct_ty
                    .args(db)
                    .iter()
                    .map(|&arg| substitute_type_params(db, arg, args))
                    .collect::<Vec<_>>(),
            )),
        ),
        TyKind::Enum(enum_ty) => Ty::new(
            db,
            TyKind::Enum(EnumTy::new(
                db,
                enum_ty.module(db),
                enum_ty.index(db),
                enum_ty.name(db),
                enum_ty
                    .args(db)
                    .iter()
                    .map(|&arg| substitute_type_params(db, arg, args))
                    .collect::<Vec<_>>(),
            )),
        ),
        TyKind::Bool
        | TyKind::Float
        | TyKind::Int
        | TyKind::ExactInt(_)
        | TyKind::String
        | TyKind::Char
        | TyKind::Unknown => ty,
    }
}

fn nominal_type_param_env<'db>(
    db: &'db dyn Database,
    type_params: impl Iterator<Item = ast::TypeParam<'db>>,
) -> FxIndexMap<Symbol<'db>, Ty<'db>> {
    type_params
        .enumerate()
        .map(|(index, ty_param)| {
            let symbol = ty_param.as_str().into_symbol(db);
            (symbol, Ty::new(db, TyKind::Var(index as u32)))
        })
        .collect()
}

fn builtin_type<'db>(name: &str, db: &'db dyn Database) -> Option<Ty<'db>> {
    Some(match name {
        "bool" => Ty::new(db, TyKind::Bool),
        "u8" => Ty::new(db, TyKind::ExactInt(ExactInt::U8)),
        "u16" => Ty::new(db, TyKind::ExactInt(ExactInt::U16)),
        "u32" => Ty::new(db, TyKind::ExactInt(ExactInt::U32)),
        "u64" => Ty::new(db, TyKind::ExactInt(ExactInt::U64)),
        "i8" => Ty::new(db, TyKind::ExactInt(ExactInt::I8)),
        "i16" => Ty::new(db, TyKind::ExactInt(ExactInt::I16)),
        "i32" => Ty::new(db, TyKind::ExactInt(ExactInt::I32)),
        "i64" => Ty::new(db, TyKind::ExactInt(ExactInt::I64)),
        "int" => Ty::new(db, TyKind::Int),
        "float" => Ty::new(db, TyKind::Float),
        "str" => Ty::new(db, TyKind::String),
        "char" => Ty::new(db, TyKind::Char),
        _ => return None,
    })
}

fn resolve_use_value<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    path: Symbol<'db>,
) -> Option<FunctionLocation<'db>> {
    if let Some((module, last)) =
        resolve_segment_path_target(db, current_module, visible_items, path, AccessKind::Value)
    {
        return module.visible_items(db).get_value(&last);
    }

    visible_items.get_value(&path)
}

fn resolve_use_type<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    path: Symbol<'db>,
) -> Option<(Ty<'db>, TypeDeclaration<'db>)> {
    if let Some((module, last)) =
        resolve_segment_path_target(db, current_module, visible_items, path, AccessKind::Type)
    {
        let module_visible = module.visible_items(db);
        return Some((
            module_visible.get_type(&last)?,
            module_visible.get_type_declaration(&last)?,
        ));
    }

    Some((visible_items.get_type(&path)?, visible_items.get_type_declaration(&path)?))
}

fn resolve_use_module<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    path: Symbol<'db>,
) -> Option<ModuleBinding<'db>> {
    resolve_segment_module_path(db, current_module, visible_items, path)
}

#[derive(Clone, Copy)]
enum AccessKind {
    Value,
    Type,
}

fn resolve_segment_module_path<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    path: Symbol<'db>,
) -> Option<ModuleBinding<'db>> {
    let mut segments = path.text(db).split("::").filter(|segment| !segment.is_empty());
    let first = segments.next()?;

    let mut binding = match first {
        "crate" => {
            ModuleBinding { module: root_module(db, current_module.package(db)), public: true }
        }
        "std" => ModuleBinding { module: root_module(db, stdlib_package(db)), public: true },
        _ => visible_items
            .get_module_alias(&first.into_symbol(db))
            .or_else(|| visible_items.get_module(&first.into_symbol(db)))?,
    };

    for segment in segments {
        let child = binding.module.visible_items(db).get_module(&segment.into_symbol(db))?;
        binding = ModuleBinding { module: child.module, public: binding.public && child.public };
    }

    Some(binding)
}

fn resolve_segment_path_target<'db>(
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &VisibleItems<'db>,
    path: Symbol<'db>,
    _access: AccessKind,
) -> Option<(ModuleId<'db>, Symbol<'db>)> {
    let segments =
        path.text(db).split("::").filter(|segment| !segment.is_empty()).collect::<Vec<_>>();
    if segments.len() < 2 {
        return None;
    }

    let last = (*segments.last()?).into_symbol(db);
    let module_path = segments[..segments.len() - 1].join("::").into_symbol(db);
    let binding = resolve_segment_module_path(db, current_module, visible_items, module_path)?;

    let external = matches!(segments.first().copied(), Some("std"));
    if external && !binding.public {
        return None;
    }

    Some((binding.module, last))
}

#[cfg(test)]
mod tests {
    use mitki_inputs::File;
    use mitki_span::IntoSymbol as _;
    use mitki_yellow::ast::HasName as _;

    use super::{Declaration, HasItemDecls as _, HasVisibleItems as _};
    use crate::item::package::{child_module_named, root_module};
    use crate::item::stdlib::{stdlib_module_path, stdlib_package};

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
            "source_paths.mitki".into(),
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

        let scope = file.item_decls(&db);
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
                Declaration::BoundaryInstance(location) => {
                    let source = location.source(&db);
                    assert_eq!(source.name().unwrap().as_str(), "main");
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

    #[test]
    fn stdlib_modules_expose_module_local_items() {
        let db = TestDb::default();
        let std_root = root_module(&db, stdlib_package(&db));
        let io_module =
            child_module_named(&db, std_root, "io".to_owned()).expect("expected std::io module");

        let root_visible = std_root.visible_items(&db);
        let io_visible = io_module.visible_items(&db);
        let io_binding = root_visible
            .get_module(&"io".into_symbol(&db))
            .expect("expected std root to export io");
        let location = io_visible
            .get_value(&"print_int".into_symbol(&db))
            .expect("expected print_int to resolve inside std::io");

        assert!(io_binding.public);
        assert_eq!(io_binding.module, io_module);
        assert_eq!(location.file(&db).path(&db), stdlib_module_path("io.mitki"));
        assert_eq!(location.source(&db).name().unwrap().as_str(), "print_int");
    }
}

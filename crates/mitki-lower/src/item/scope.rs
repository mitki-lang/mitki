use std::future::Future;
use std::sync::Arc;

use mitki_hir::hir::{NodeStore, ParamId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_inputs::File;
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{SyntaxKind, SyntaxNodePtr};
use rustc_hash::FxHashMap;
use text_size::{TextRange, TextSize};

pub trait HasItemScope {
    fn item_scope<DB>(self, db: &DB) -> impl Future<Output = Arc<ItemScope>> + Send
    where
        DB: ItemScopeDb + Sync;
}

pub trait ItemScopeDb: mitki_parse::ParseDb + HasItemScopeQuery {}

impl<T> ItemScopeDb for T where T: mitki_parse::ParseDb + HasItemScopeQuery {}

pub trait SignatureDb: ItemScopeDb + HasSignatureQuery {}

impl<T> SignatureDb for T where T: ItemScopeDb + HasSignatureQuery {}

#[rustfmt::skip]
#[picante::tracked]
pub async fn item_scope<DB: mitki_hir::ty::TypeDatabase + mitki_parse::HasParseQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<Arc<ItemScope>> {
    let parsed = mitki_parse::parse(db, file).await?;
    Ok(Arc::new(build_item_scope(db, file, &parsed)))
}

impl HasItemScope for File {
    #[allow(clippy::manual_async_fn)]
    fn item_scope<DB>(self, db: &DB) -> impl Future<Output = Arc<ItemScope>> + Send
    where
        DB: ItemScopeDb + Sync,
    {
        async move { item_scope(db, self).await.expect("failed to compute item scope") }
    }
}

fn build_item_scope<DB>(db: &DB, file: File, parsed: &mitki_parse::Parsed) -> ItemScope
where
    DB: mitki_hir::ty::TypeDatabase,
{
    ItemScopeBuilder { db, scope: ItemScope::default() }.build(file, parsed)
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
struct SourcePtrData {
    kind: SyntaxKind,
    start: u32,
    end: u32,
}

impl SourcePtrData {
    fn from_ptr(ptr: SyntaxNodePtr) -> Self {
        Self { kind: ptr.kind, start: ptr.range.start().into(), end: ptr.range.end().into() }
    }

    fn to_ptr(self) -> SyntaxNodePtr {
        SyntaxNodePtr {
            kind: self.kind,
            range: TextRange::new(TextSize::from(self.start), TextSize::from(self.end)),
        }
    }
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, facet::Facet)]
pub enum Declaration {
    Function(FunctionLocationData),
    Struct(StructLocationData),
    Enum(EnumLocationData),
}

pub type FunctionLocation = FunctionLocationData;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
pub struct FunctionLocationData {
    file: File,
    source: SourcePtrData,
}

impl FunctionLocationData {
    pub fn new(file: File, source_ptr: SyntaxNodePtr) -> Self {
        Self { file, source: SourcePtrData::from_ptr(source_ptr) }
    }

    pub fn file<DB>(self, _: &DB) -> File
    where
        DB: picante::HasRuntime,
    {
        self.file
    }
}

pub type StructLocation = StructLocationData;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
pub struct StructLocationData {
    file: File,
    source: SourcePtrData,
}

impl StructLocationData {
    pub fn new(file: File, source_ptr: SyntaxNodePtr) -> Self {
        Self { file, source: SourcePtrData::from_ptr(source_ptr) }
    }

    pub fn file<DB>(self, _: &DB) -> File
    where
        DB: picante::HasRuntime,
    {
        self.file
    }
}

pub type EnumLocation = EnumLocationData;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
pub struct EnumLocationData {
    file: File,
    source: SourcePtrData,
}

impl EnumLocationData {
    pub fn new(file: File, source_ptr: SyntaxNodePtr) -> Self {
        Self { file, source: SourcePtrData::from_ptr(source_ptr) }
    }

    pub fn file<DB>(self, _: &DB) -> File
    where
        DB: picante::HasRuntime,
    {
        self.file
    }
}

impl FunctionLocation {
    pub async fn signature<DB>(self, db: &DB) -> Arc<Signature>
    where
        DB: SignatureDb,
    {
        signature(db, self).await.expect("failed to compute function signature")
    }
}

fn build_signature<DB>(
    db: &DB,
    location: FunctionLocation,
    parsed: &mitki_parse::Parsed,
) -> Signature
where
    DB: mitki_span::SymbolDatabase,
{
    let mut node_store = NodeStore::default();
    let syntax = location.source_ptr(db).to_node(&parsed.syntax_node());
    let func = ast::Function::cast(syntax).unwrap();

    let type_params: Vec<Symbol> =
        func.type_params().map(|tp| tp.as_str().into_symbol(db)).collect();

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

    let ret_type = func
        .ret_type()
        .and_then(|r| r.ty())
        .and_then(|ty| lower_type_ref(db, &mut node_store, ty))
        .unwrap_or(TyId::ZERO);

    Signature::new(type_params, params, ret_type, node_store)
}

pub type SignatureMap = FxHashMap<FunctionLocation, Arc<Signature>>;

#[rustfmt::skip]
#[picante::tracked]
pub async fn signature<DB: mitki_span::SymbolDatabase + mitki_parse::HasParseQuery>(
    db: &DB,
    location: FunctionLocation,
) -> picante::PicanteResult<Arc<Signature>> {
    let file = location.file(db);
    let parsed = mitki_parse::parse(db, file).await?;
    Ok(Arc::new(build_signature(db, location, &parsed)))
}

#[rustfmt::skip]
#[picante::tracked]
pub async fn signature_map<DB: HasItemScopeQuery + HasSignatureQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<Arc<SignatureMap>> {
    let item_scope = item_scope(db, file).await?;
    let mut signatures = FxHashMap::default();

    for declaration in item_scope.declarations() {
        if let Declaration::Function(location) = *declaration {
            let signature = signature(db, location).await?;
            signatures.insert(location, signature);
        }
    }

    Ok(Arc::new(signatures))
}

fn lower_type_ref(
    db: &impl mitki_span::SymbolDatabase,
    node_store: &mut NodeStore,
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

impl FunctionLocation {
    pub fn source_ptr<DB>(self, _: &DB) -> SyntaxNodePtr {
        self.source.to_ptr()
    }
}

impl StructLocation {
    pub fn source_ptr<DB>(self, _: &DB) -> SyntaxNodePtr {
        self.source.to_ptr()
    }
}

impl EnumLocation {
    pub fn source_ptr<DB>(self, _: &DB) -> SyntaxNodePtr {
        self.source.to_ptr()
    }
}

#[derive(Debug, PartialEq, Eq, facet::Facet)]
pub struct Signature {
    #[facet(opaque)]
    type_params: Vec<Symbol>,
    #[facet(opaque)]
    params: Vec<ParamId>,
    #[facet(opaque)]
    ret_type: TyId,
    #[facet(opaque)]
    nodes: NodeStore,
}

impl Signature {
    pub fn new(
        type_params: Vec<Symbol>,
        params: Vec<ParamId>,
        ret_type: TyId,
        nodes: NodeStore,
    ) -> Self {
        Self { type_params, params, ret_type, nodes }
    }

    pub fn type_params(&self) -> &[Symbol] {
        &self.type_params
    }

    pub fn params(&self) -> &[ParamId] {
        &self.params
    }

    pub fn ret_type(&self) -> TyId {
        self.ret_type
    }

    pub fn nodes(&self) -> &NodeStore {
        &self.nodes
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, facet::Facet)]
pub struct ItemScope {
    values: Vec<(Symbol, FunctionLocation)>,
    types: Vec<(Symbol, Ty)>,
    declarations: Vec<Declaration>,
}

impl ItemScope {
    pub fn get(&self, name: &Symbol) -> Option<FunctionLocation> {
        self.values.iter().find_map(|(key, value)| (key == name).then_some(*value))
    }

    pub fn get_type(&self, name: &Symbol) -> Option<Ty> {
        self.types.iter().find_map(|(key, value)| (key == name).then_some(*value))
    }

    pub fn types(&self) -> impl Iterator<Item = (&Symbol, &Ty)> {
        self.types.iter().map(|(name, ty)| (name, ty))
    }

    pub fn declarations(&self) -> &[Declaration] {
        &self.declarations
    }

    fn insert_value(&mut self, name: Symbol, location: FunctionLocation) {
        if let Some((_, value)) = self.values.iter_mut().find(|(key, _)| *key == name) {
            *value = location;
            return;
        }

        self.values.push((name, location));
    }

    fn insert_type(&mut self, name: Symbol, ty: Ty) {
        if let Some((_, value)) = self.types.iter_mut().find(|(key, _)| *key == name) {
            *value = ty;
            return;
        }

        self.types.push((name, ty));
    }
}

struct ItemScopeBuilder<'db, DB>
where
    DB: mitki_hir::ty::TypeDatabase,
{
    db: &'db DB,
    scope: ItemScope,
}

impl<'db, DB> ItemScopeBuilder<'db, DB>
where
    DB: mitki_hir::ty::TypeDatabase,
{
    fn build(mut self, file: File, parsed: &mitki_parse::Parsed) -> ItemScope {
        for item in parsed.tree().items() {
            match item {
                ast::Item::Function(function) => {
                    let Some(name) = function.name().map(|name| name.as_str().into_symbol(self.db))
                    else {
                        continue;
                    };
                    let loc = FunctionLocation::new(file, SyntaxNodePtr::new(function.syntax()));

                    self.scope.declarations.push(Declaration::Function(loc));
                    self.scope.insert_value(name, loc);
                }
                ast::Item::Struct(struct_def) => {
                    let Some(name) =
                        struct_def.name().map(|name| name.as_str().into_symbol(self.db))
                    else {
                        continue;
                    };

                    let loc = StructLocation::new(file, SyntaxNodePtr::new(struct_def.syntax()));
                    self.scope.declarations.push(Declaration::Struct(loc));
                    let fields: Vec<(Symbol, Ty)> = struct_def
                        .field_list()
                        .map(|fl| {
                            fl.fields()
                                .filter_map(|f| {
                                    let field_name = f.name()?.as_str().into_symbol(self.db);
                                    let ty = self.resolve_ast_type(f.ty()?);
                                    Some((field_name, ty))
                                })
                                .collect()
                        })
                        .unwrap_or_default();

                    let ty = Ty::new(self.db, TyKind::Struct { name, fields });
                    self.scope.insert_type(name, ty);
                }
                ast::Item::Enum(enum_def) => {
                    let Some(name) = enum_def.name().map(|name| name.as_str().into_symbol(self.db))
                    else {
                        continue;
                    };

                    let loc = EnumLocation::new(file, SyntaxNodePtr::new(enum_def.syntax()));
                    self.scope.declarations.push(Declaration::Enum(loc));
                    let variants: Vec<(Symbol, Vec<Ty>)> = enum_def
                        .variant_list()
                        .map(|vl| {
                            vl.variants()
                                .filter_map(|v| {
                                    let variant_name = v.name()?.as_str().into_symbol(self.db);
                                    let types = v
                                        .field_types()
                                        .map(|tt| {
                                            tt.types().map(|t| self.resolve_ast_type(t)).collect()
                                        })
                                        .unwrap_or_default();
                                    Some((variant_name, types))
                                })
                                .collect()
                        })
                        .unwrap_or_default();

                    let ty = Ty::new(self.db, TyKind::Enum { name, variants });
                    self.scope.insert_type(name, ty);
                }
            }
        }

        self.scope
    }

    fn resolve_ast_type(&self, ty: ast::Type) -> Ty {
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

                match name.text(self.db).as_ref() {
                    "bool" => Ty::new(self.db, TyKind::Bool),
                    "int" => Ty::new(self.db, TyKind::Int),
                    "float" => Ty::new(self.db, TyKind::Float),
                    "str" => Ty::new(self.db, TyKind::String),
                    "char" => Ty::new(self.db, TyKind::Char),
                    _ => self
                        .scope
                        .types
                        .iter()
                        .find_map(|(ty_name, ty)| (*ty_name == name).then_some(*ty))
                        .unwrap_or(Ty::new(self.db, TyKind::Unknown)),
                }
            }
            ast::Type::Tuple(tuple_type) => {
                let items: Vec<Ty> = tuple_type.types().map(|t| self.resolve_ast_type(t)).collect();
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
    use mitki_parse::FileParse as _;
    use mitki_yellow::ast::{self, HasName as _, Node as _};

    use super::{Declaration, HasItemScope as _};

    #[picante::db(
        inputs(mitki_inputs::SourceFile),
        interned(mitki_span::Symbol, mitki_hir::ty::TyData),
        tracked(mitki_parse::parse_file, mitki_parse::parse, super::item_scope),
        db_trait(TestDatabase)
    )]
    struct TestDb {}

    impl Default for TestDb {
        fn default() -> Self {
            Self::new()
        }
    }

    #[tokio::test]
    async fn source_methods_resolve_each_declaration_kind() {
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

        let scope = file.item_scope(&db).await;
        let mut saw_function = false;
        let mut saw_struct = false;
        let mut saw_enum = false;

        for declaration in scope.declarations() {
            match *declaration {
                Declaration::Function(location) => {
                    let parsed = file.parse(&db).await;
                    let syntax = location.source_ptr(&db).to_node(&parsed.syntax_node());
                    let source = ast::Function::cast(syntax).unwrap();
                    assert_eq!(source.name().unwrap().as_str(), "main");
                    saw_function = true;
                }
                Declaration::Struct(location) => {
                    let parsed = file.parse(&db).await;
                    let syntax = location.source_ptr(&db).to_node(&parsed.syntax_node());
                    let source = ast::StructDef::cast(syntax).unwrap();
                    assert_eq!(source.name().unwrap().as_str(), "Point");
                    saw_struct = true;
                }
                Declaration::Enum(location) => {
                    let parsed = file.parse(&db).await;
                    let syntax = location.source_ptr(&db).to_node(&parsed.syntax_node());
                    let source = ast::EnumDef::cast(syntax).unwrap();
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

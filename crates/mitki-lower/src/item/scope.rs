use std::marker::PhantomData;

use mitki_hir::hir::{NodeStore, ParamId, TyId};
use mitki_hir::ty::{Ty, TyKind};
use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_span::{IntoSymbol as _, Symbol};
use mitki_yellow::ast::{self, HasName as _, Node as _};
use mitki_yellow::{SyntaxKind, SyntaxNodePtr};
use text_size::{TextRange, TextSize};

pub trait HasItemScope {
    fn item_scope<DB>(self, db: &DB) -> ItemScope<'_>
    where
        DB: ItemScopeDb;
}

pub trait ItemScopeDb: mitki_parse::ParseDb + HasItemScopeCachedQuery {}

impl<T> ItemScopeDb for T where T: mitki_parse::ParseDb + HasItemScopeCachedQuery {}

#[rustfmt::skip]
#[picante::tracked]
pub async fn item_scope_cached<DB: mitki_inputs::FileDatabase + mitki_parse::HasParseFileQuery + mitki_hir::ty::TypeDatabase>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<ItemScope<'static>> {
    mitki_parse::parse_file(db, file).await?;
    let text = file.text(db);
    let parsed = mitki_parse::parse_text(text.as_ref());
    Ok(erase_item_scope(build_item_scope(db, file, &parsed)))
}

impl HasItemScope for File {
    fn item_scope<DB>(self, db: &DB) -> ItemScope<'_>
    where
        DB: ItemScopeDb,
    {
        let scope = <DB as mitki_parse::ParseExecutor>::block_on(db, item_scope_cached(db, self))
            .expect("failed to compute item scope");
        unerase_item_scope(scope)
    }
}

fn build_item_scope<'db, DB>(
    db: &'db DB,
    file: File,
    parsed: &mitki_parse::Parsed,
) -> ItemScope<'db>
where
    DB: mitki_hir::ty::TypeDatabase,
{
    ItemScopeBuilder { db, scope: ItemScope::default() }.build(file, parsed)
}

fn erase_item_scope(scope: ItemScope<'_>) -> ItemScope<'static> {
    // SAFETY: ItemScope stores copyable handle types with phantom lifetimes only.
    unsafe { std::mem::transmute(scope) }
}

fn unerase_item_scope<'db>(scope: ItemScope<'static>) -> ItemScope<'db> {
    // SAFETY: ItemScope stores no borrowed data and can be viewed at any db
    // lifetime.
    unsafe { std::mem::transmute(scope) }
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
pub enum Declaration<'db> {
    Function(FunctionLocation<'db>),
    Struct(StructLocation<'db>),
    Enum(EnumLocation<'db>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
pub struct FunctionLocation<'db> {
    file: File,
    source: SourcePtrData,
    _marker: PhantomData<&'db ()>,
}

impl<'db> FunctionLocation<'db> {
    pub fn new(file: File, source_ptr: SyntaxNodePtr) -> Self {
        Self { file, source: SourcePtrData::from_ptr(source_ptr), _marker: PhantomData }
    }

    pub fn file(self, _: &'db impl picante::HasRuntime) -> File {
        self.file
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
pub struct StructLocation<'db> {
    file: File,
    source: SourcePtrData,
    _marker: PhantomData<&'db ()>,
}

impl<'db> StructLocation<'db> {
    pub fn new(file: File, source_ptr: SyntaxNodePtr) -> Self {
        Self { file, source: SourcePtrData::from_ptr(source_ptr), _marker: PhantomData }
    }

    pub fn file(self, _: &'db impl picante::HasRuntime) -> File {
        self.file
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, facet::Facet)]
pub struct EnumLocation<'db> {
    file: File,
    source: SourcePtrData,
    _marker: PhantomData<&'db ()>,
}

impl<'db> EnumLocation<'db> {
    pub fn new(file: File, source_ptr: SyntaxNodePtr) -> Self {
        Self { file, source: SourcePtrData::from_ptr(source_ptr), _marker: PhantomData }
    }

    pub fn file(self, _: &'db impl picante::HasRuntime) -> File {
        self.file
    }
}

impl<'db> FunctionLocation<'db> {
    pub fn signature<DB>(self, db: &'db DB) -> Signature<'db>
    where
        DB: mitki_parse::ParseDb,
    {
        let mut node_store = NodeStore::default();
        let file = self.file(db);
        let parsed = file.parse(db);
        let syntax = self.source_ptr(db).to_node(&parsed.syntax_node());
        let func = ast::Function::cast(syntax).unwrap();

        let type_params: Vec<Symbol<'db>> =
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
}

fn lower_type_ref<'db>(
    db: &'db impl mitki_parse::ParseDb,
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
    pub fn source_ptr<DB>(self, _: &'db DB) -> SyntaxNodePtr
    where
        DB: mitki_parse::ParseDb,
    {
        self.source.to_ptr()
    }
}

impl<'db> StructLocation<'db> {
    pub fn source_ptr<DB>(self, _: &'db DB) -> SyntaxNodePtr
    where
        DB: mitki_parse::ParseDb,
    {
        self.source.to_ptr()
    }
}

impl<'db> EnumLocation<'db> {
    pub fn source_ptr<DB>(self, _: &'db DB) -> SyntaxNodePtr
    where
        DB: mitki_parse::ParseDb,
    {
        self.source.to_ptr()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Signature<'db> {
    type_params: Vec<Symbol<'db>>,
    params: Vec<ParamId>,
    ret_type: TyId,
    nodes: NodeStore<'db>,
}

impl<'db> Signature<'db> {
    pub fn new(
        type_params: Vec<Symbol<'db>>,
        params: Vec<ParamId>,
        ret_type: TyId,
        nodes: NodeStore<'db>,
    ) -> Self {
        Self { type_params, params, ret_type, nodes }
    }

    pub fn type_params(&self) -> &[Symbol<'db>] {
        &self.type_params
    }

    pub fn params(&self) -> &[ParamId] {
        &self.params
    }

    pub fn ret_type(&self) -> TyId {
        self.ret_type
    }

    pub fn nodes(&self) -> &NodeStore<'db> {
        &self.nodes
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone, facet::Facet)]
pub struct ItemScope<'db> {
    values: Vec<(Symbol<'db>, FunctionLocation<'db>)>,
    types: Vec<(Symbol<'db>, Ty<'db>)>,
    declarations: Vec<Declaration<'db>>,
}

impl<'db> ItemScope<'db> {
    pub fn get(&self, name: &Symbol<'db>) -> Option<FunctionLocation<'db>> {
        self.values.iter().find_map(|(key, value)| (key == name).then_some(*value))
    }

    pub fn get_type(&self, name: &Symbol<'db>) -> Option<Ty<'db>> {
        self.types.iter().find_map(|(key, value)| (key == name).then_some(*value))
    }

    pub fn types(&self) -> impl Iterator<Item = (&Symbol<'db>, &Ty<'db>)> {
        self.types.iter().map(|(name, ty)| (name, ty))
    }

    pub fn declarations(&self) -> &[Declaration<'db>] {
        &self.declarations
    }

    fn insert_value(&mut self, name: Symbol<'db>, location: FunctionLocation<'db>) {
        if let Some((_, value)) = self.values.iter_mut().find(|(key, _)| *key == name) {
            *value = location;
            return;
        }

        self.values.push((name, location));
    }

    fn insert_type(&mut self, name: Symbol<'db>, ty: Ty<'db>) {
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
    scope: ItemScope<'db>,
}

impl<'db, DB> ItemScopeBuilder<'db, DB>
where
    DB: mitki_hir::ty::TypeDatabase,
{
    fn build(mut self, file: File, parsed: &mitki_parse::Parsed) -> ItemScope<'db> {
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
                    let fields: Vec<(Symbol<'db>, Ty<'db>)> = struct_def
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
                    let variants: Vec<(Symbol<'db>, Vec<Ty<'db>>)> = enum_def
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
    use std::future::Future;
    use std::sync::LazyLock;

    use mitki_inputs::File;
    use mitki_parse::FileParse as _;
    use mitki_yellow::ast::{self, HasName as _, Node as _};

    use super::{Declaration, HasItemScope as _};

    #[picante::db(
        inputs(mitki_inputs::SourceFile),
        interned(mitki_span::SymbolData, mitki_hir::ty::TyData),
        tracked(mitki_parse::parse_file, super::item_scope_cached),
        db_trait(TestDatabase)
    )]
    struct TestDb {}

    impl Default for TestDb {
        fn default() -> Self {
            Self::new()
        }
    }

    static TEST_EXECUTOR: LazyLock<tokio::runtime::Runtime> = LazyLock::new(|| {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .expect("failed to build test parse runtime")
    });

    impl mitki_parse::ParseExecutor for TestDb {
        fn block_on<F>(&self, future: F) -> F::Output
        where
            F: Future,
        {
            TEST_EXECUTOR.block_on(future)
        }
    }

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
                    let parsed = file.parse(&db);
                    let syntax = location.source_ptr(&db).to_node(&parsed.syntax_node());
                    let source = ast::Function::cast(syntax).unwrap();
                    assert_eq!(source.name().unwrap().as_str(), "main");
                    saw_function = true;
                }
                Declaration::Struct(location) => {
                    let parsed = file.parse(&db);
                    let syntax = location.source_ptr(&db).to_node(&parsed.syntax_node());
                    let source = ast::StructDef::cast(syntax).unwrap();
                    assert_eq!(source.name().unwrap().as_str(), "Point");
                    saw_struct = true;
                }
                Declaration::Enum(location) => {
                    let parsed = file.parse(&db);
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

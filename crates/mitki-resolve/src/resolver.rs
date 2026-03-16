use mitki_hir::hir::{ExprId, NameId, TyId};
use mitki_hir::ty::{EnumTy, ExactInt, StructTy, Ty, TyKind};
use mitki_inputs::ModuleId;
use mitki_lower::item::package::root_module;
use mitki_lower::item::scope::{
    EnumLocation, EnumVariantLocation, FunctionLocation, HasItemDecls as _, HasVisibleItems as _,
    ItemDecls, ModuleBinding, StructLocation, TypeDeclaration, VisibleItems,
};
use mitki_lower::item::stdlib::stdlib_package;
use mitki_lower::item::tree::HasItemTree as _;
use mitki_span::{IntoSymbol as _, Symbol};
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::scope::{ExprScopes, HasExprScopes as _, Scope};
use crate::{
    CompilerIntrinsic, RuntimeFunction, SignatureTypeResolver, lookup_compiler_intrinsic,
    lookup_runtime_function,
};

#[salsa::tracked(returns(ref))]
fn builtin_scope(db: &dyn Database) -> FxHashMap<Symbol<'_>, Ty<'_>> {
    FxHashMap::from_iter([
        ("bool".into_symbol(db), Ty::new(db, TyKind::Bool)),
        ("char".into_symbol(db), Ty::new(db, TyKind::Char)),
        ("float".into_symbol(db), Ty::new(db, TyKind::Float)),
        ("u8".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::U8))),
        ("u16".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::U16))),
        ("u32".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::U32))),
        ("u64".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::U64))),
        ("i8".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::I8))),
        ("i16".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::I16))),
        ("i32".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::I32))),
        ("i64".into_symbol(db), Ty::new(db, TyKind::ExactInt(ExactInt::I64))),
        ("int".into_symbol(db), Ty::new(db, TyKind::Int)),
        ("str".into_symbol(db), Ty::new(db, TyKind::String)),
    ])
}

pub fn lookup_builtin_type<'db>(db: &'db dyn Database, name: Symbol<'db>) -> Option<Ty<'db>> {
    builtin_scope(db).get(&name).copied()
}

#[derive(Debug, Clone)]
pub struct MethodResolution<'db> {
    pub function: FunctionLocation<'db>,
    pub inputs: Vec<Ty<'db>>,
    pub output: Ty<'db>,
}

pub fn resolve_method_for_receiver<'db>(
    db: &'db dyn Database,
    receiver_ty: Ty<'db>,
    method_name: Symbol<'db>,
) -> Option<MethodResolution<'db>> {
    let method_module = match receiver_ty.kind(db) {
        TyKind::Struct(struct_ty) | TyKind::ExternStruct(struct_ty) => struct_ty.module(db),
        TyKind::Enum(enum_ty) => enum_ty.module(db),
        _ => return None,
    };

    let function = method_module.visible_items(db).get_value(&method_name)?;
    let signature = function.signature(db);
    let resolved = SignatureTypeResolver::new(db, function, signature);
    let params = signature.params(db);
    let (&receiver_param, method_params) = params.split_first()?;
    let (_, receiver_param_ty) = signature.nodes(db).param(receiver_param);
    let resolved_receiver = resolved.resolve(receiver_param_ty).ok()?;
    if resolved_receiver != receiver_ty {
        return None;
    }

    let inputs = method_params
        .iter()
        .map(|&param| {
            let (_, ty) = signature.nodes(db).param(param);
            resolved.resolve(ty).ok()
        })
        .collect::<Option<Vec<_>>>()?;
    let output = resolved.resolve(signature.ret_type(db)).ok()?;

    Some(MethodResolution { function, inputs, output })
}

#[derive(Clone)]
pub struct Resolver<'db> {
    db: &'db dyn Database,
    current_module: ModuleId<'db>,
    visible_items: &'db VisibleItems<'db>,
    item_decls: &'db ItemDecls<'db>,
    expr_scopes: &'db ExprScopes<'db>,
    scopes: Vec<Scope<'db>>,
    builtin_scope: &'db FxHashMap<Symbol<'db>, Ty<'db>>,
}

impl<'db> Resolver<'db> {
    pub fn new(db: &'db dyn Database, function: FunctionLocation<'db>) -> Self {
        let module = function.module(db);

        Self {
            db,
            current_module: module,
            visible_items: module.visible_items(db),
            item_decls: module.item_decls(db),
            expr_scopes: function.expr_scopes(db),
            scopes: Vec::new(),
            builtin_scope: builtin_scope(db),
        }
    }

    fn scopes(&self) -> impl ExactSizeIterator<Item = Scope<'db>> + '_ {
        self.scopes.iter().rev().copied()
    }

    pub fn scopes_for_node(&mut self, node: ExprId) -> Guard {
        let start = self.scopes.len();

        let innermost_scope = self.scopes().next();
        let scope_for_expr = self.expr_scopes.scope_for(node);

        let scopes = self.expr_scopes.chain(scope_for_expr);
        if let Some(scope) = innermost_scope {
            self.scopes.extend(scopes.take_while(|&it| it != scope));
        } else {
            self.scopes.extend(scopes);
        }

        self.scopes[start..].reverse();

        Guard(start)
    }

    pub fn scopes_for_type(&mut self, ty: TyId) -> Guard {
        let start = self.scopes.len();

        let innermost_scope = self.scopes().next();
        let scope_for_ty = self.expr_scopes.scope_for_ty(ty);

        let scopes = self.expr_scopes.chain(scope_for_ty);
        if let Some(scope) = innermost_scope {
            self.scopes.extend(scopes.take_while(|&it| it != scope));
        } else {
            self.scopes.extend(scopes);
        }

        self.scopes[start..].reverse();

        Guard(start)
    }

    pub fn reset(&mut self, Guard(start): Guard) {
        self.scopes.truncate(start);
    }

    pub fn resolve_name(&self, path: Symbol<'db>, namespace: Namespace) -> Resolution<'db> {
        match namespace {
            Namespace::Value => self.resolve_value(path),
            Namespace::Type => self.resolve_type(path),
        }
    }

    pub fn resolve_value_binding(&self, path: Symbol<'db>) -> Option<BindingId<'db>> {
        self.resolve_name(path, Namespace::Value).binding
    }

    pub fn resolve_type_binding(&self, path: Symbol<'db>) -> Option<BindingId<'db>> {
        self.resolve_name(path, Namespace::Type).binding
    }

    pub fn ty_for_binding(&self, binding: BindingId<'db>) -> Option<Ty<'db>> {
        match binding {
            BindingId::Struct(location) => {
                let file = location.file(self.db);
                let name = file.item_tree(self.db)[location.index(self.db)].name;
                let nominal = StructTy::new(
                    self.db,
                    location.module(self.db),
                    location.index(self.db).index(),
                    name,
                    Vec::new(),
                );
                Some(if location.source(self.db).is_extern() {
                    Ty::new(self.db, TyKind::ExternStruct(nominal))
                } else {
                    Ty::new(self.db, TyKind::Struct(nominal))
                })
            }
            BindingId::Enum(location) => {
                let file = location.file(self.db);
                let name = file.item_tree(self.db)[location.index(self.db)].name;
                let nominal = EnumTy::new(
                    self.db,
                    location.module(self.db),
                    location.index(self.db).index(),
                    name,
                    Vec::new(),
                );
                Some(Ty::new(self.db, TyKind::Enum(nominal)))
            }
            BindingId::BuiltinType(ty) => Some(ty),
            _ => None,
        }
    }

    fn resolve_value(&self, path: Symbol<'db>) -> Resolution<'db> {
        if let Some(binding) = self.resolve_segment_path(path, Namespace::Value) {
            return Resolution::resolved(binding, Namespace::Value);
        }
        if let Some(item) = self.visible_items.get_value(&path) {
            return Resolution::resolved(BindingId::Function(item), Namespace::Value);
        }
        if path.text(self.db).contains("::") {
            return Resolution::unresolved(Namespace::Value);
        }

        for scope in self.scopes() {
            if let Some(binding) = self.expr_scopes.lookup(scope, path) {
                return Resolution::resolved(binding, Namespace::Value);
            }
        }

        if let Some(runtime) = lookup_runtime_function(self.db, path) {
            return Resolution::resolved(BindingId::RuntimeFunction(runtime), Namespace::Value);
        }

        if let Some(intrinsic) = lookup_compiler_intrinsic(self.db, path) {
            return Resolution::resolved(BindingId::CompilerIntrinsic(intrinsic), Namespace::Value);
        }

        if let Some(item) = self.visible_items.get_value(&path) {
            return Resolution::resolved(BindingId::Function(item), Namespace::Value);
        }

        Resolution::unresolved(Namespace::Value)
    }

    fn resolve_type(&self, path: Symbol<'db>) -> Resolution<'db> {
        if let Some(binding) = self.resolve_segment_path(path, Namespace::Type) {
            return Resolution::resolved(binding, Namespace::Type);
        }
        if let Some(declaration) = self.visible_items.get_type_declaration(&path) {
            let binding = match declaration {
                TypeDeclaration::Struct(location) => BindingId::Struct(location),
                TypeDeclaration::Enum(location) => BindingId::Enum(location),
            };
            return Resolution::resolved(binding, Namespace::Type);
        }
        if path.text(self.db).contains("::") {
            return Resolution::unresolved(Namespace::Type);
        }

        if let Some(declaration) = self.visible_items.get_type_declaration(&path) {
            let binding = match declaration {
                TypeDeclaration::Struct(location) => BindingId::Struct(location),
                TypeDeclaration::Enum(location) => BindingId::Enum(location),
            };
            return Resolution::resolved(binding, Namespace::Type);
        }

        if let Some(&ty) = self.builtin_scope.get(&path) {
            return Resolution::resolved(BindingId::BuiltinType(ty), Namespace::Type);
        }

        Resolution::unresolved(Namespace::Type)
    }

    pub fn for_scope(
        db: &'db dyn Database,
        module: ModuleId<'db>,
        visible_items: &'db VisibleItems<'db>,
        item_decls: &'db ItemDecls<'db>,
        expr_scopes: &'db ExprScopes<'db>,
        scope: Option<Scope<'db>>,
    ) -> Self {
        let mut scopes: Vec<_> = expr_scopes.chain(scope).collect::<Vec<_>>().into_iter().collect();
        scopes.reverse();

        Resolver {
            db,
            current_module: module,
            visible_items,
            item_decls,
            scopes,
            expr_scopes,
            builtin_scope: builtin_scope(db),
        }
    }

    fn resolve_segment_path(
        &self,
        path: Symbol<'db>,
        namespace: Namespace,
    ) -> Option<BindingId<'db>> {
        let segments = path
            .text(self.db)
            .split("::")
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>();
        if segments.len() < 2 {
            return None;
        }

        let binding = self.resolve_module_path(&segments[..segments.len() - 1])?;
        if matches!(segments.first().copied(), Some("std")) && !binding.public {
            return None;
        }

        let last = segments.last()?.into_symbol(self.db);
        match namespace {
            Namespace::Value => binding
                .module
                .visible_items(self.db)
                .get_value(&last)
                .map(BindingId::Function)
                .or_else(|| {
                    if matches!(segments.first().copied(), Some("std")) {
                        lookup_runtime_function(self.db, path).map(BindingId::RuntimeFunction)
                    } else {
                        None
                    }
                }),
            Namespace::Type => {
                binding.module.visible_items(self.db).get_type_declaration(&last).map(
                    |declaration| match declaration {
                        TypeDeclaration::Struct(location) => BindingId::Struct(location),
                        TypeDeclaration::Enum(location) => BindingId::Enum(location),
                    },
                )
            }
        }
    }

    fn resolve_module_path(&self, segments: &[&str]) -> Option<ModuleBinding<'db>> {
        let (first, rest) = segments.split_first()?;
        let mut binding = match *first {
            "crate" => ModuleBinding {
                module: root_module(self.db, self.current_module.package(self.db)),
                public: true,
            },
            "std" => ModuleBinding {
                module: root_module(self.db, stdlib_package(self.db)),
                public: true,
            },
            _ => self
                .visible_items
                .get_module_alias(&first.into_symbol(self.db))
                .or_else(|| self.visible_items.get_module(&first.into_symbol(self.db)))?,
        };

        for segment in rest {
            let child =
                binding.module.visible_items(self.db).get_module(&segment.into_symbol(self.db))?;
            binding =
                ModuleBinding { module: child.module, public: binding.public && child.public };
        }

        Some(binding)
    }

    pub fn resolve_enum_variant_binding(&self, variant: Symbol<'db>) -> Resolution<'db> {
        let variants = self.item_decls.enum_variants_by_name(&variant);
        match variants {
            [] => Resolution::unresolved(Namespace::Value),
            [variant] => Resolution::resolved(BindingId::EnumVariant(*variant), Namespace::Value),
            _ => Resolution::ambiguous(Namespace::Value),
        }
    }

    pub fn resolve_enum_variant(&self, variant: Symbol<'db>) -> Option<Ty<'db>> {
        let BindingId::EnumVariant(variant) = self.resolve_enum_variant_binding(variant).binding?
        else {
            return None;
        };
        let enum_location = variant.parent(self.db);
        let file = enum_location.file(self.db);
        let enum_name = file.item_tree(self.db)[enum_location.index(self.db)].name;
        self.visible_items.get_type(&enum_name)
    }

    pub fn visible_bindings(&self) -> Vec<VisibleBinding<'db>> {
        let mut bindings = Vec::new();

        for scope in self.scopes() {
            for entry in self.expr_scopes.entries(scope) {
                bindings.push(VisibleBinding {
                    name: entry.name,
                    binding: entry.binding,
                    namespace: Namespace::Value,
                });
            }
        }

        bindings.extend(self.visible_items.values().map(|(name, location)| VisibleBinding {
            name: *name,
            binding: BindingId::Function(*location),
            namespace: Namespace::Value,
        }));

        bindings.extend(self.visible_items.types().filter_map(|(name, _)| {
            let binding = match self.visible_items.get_type_declaration(name)? {
                TypeDeclaration::Struct(location) => BindingId::Struct(location),
                TypeDeclaration::Enum(location) => BindingId::Enum(location),
            };
            Some(VisibleBinding { name: *name, binding, namespace: Namespace::Type })
        }));

        bindings.extend(self.builtin_scope.iter().map(|(name, ty)| VisibleBinding {
            name: *name,
            binding: BindingId::BuiltinType(*ty),
            namespace: Namespace::Type,
        }));

        bindings
    }
}

pub struct Guard(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum Namespace {
    Value,
    Type,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum ResolveStatus {
    Resolved,
    Unresolved,
    Ambiguous,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum BindingId<'db> {
    Local(NameId),
    Param(NameId),
    CompilerIntrinsic(CompilerIntrinsic),
    RuntimeFunction(RuntimeFunction),
    Function(FunctionLocation<'db>),
    Struct(StructLocation<'db>),
    Enum(EnumLocation<'db>),
    EnumVariant(EnumVariantLocation<'db>),
    BuiltinType(Ty<'db>),
}

impl<'db> BindingId<'db> {
    pub fn namespace(self) -> Namespace {
        match self {
            BindingId::Local(_)
            | BindingId::Param(_)
            | BindingId::CompilerIntrinsic(_)
            | BindingId::RuntimeFunction(_)
            | BindingId::Function(_)
            | BindingId::EnumVariant(_) => Namespace::Value,
            BindingId::Struct(_) | BindingId::Enum(_) | BindingId::BuiltinType(_) => {
                Namespace::Type
            }
        }
    }

    pub fn target(self) -> Option<TargetId<'db>> {
        match self {
            BindingId::Local(name) => Some(TargetId::Local(name)),
            BindingId::Param(name) => Some(TargetId::Param(name)),
            BindingId::Function(function) => Some(TargetId::Function(function)),
            BindingId::Struct(location) => Some(TargetId::Struct(location)),
            BindingId::Enum(location) => Some(TargetId::Enum(location)),
            BindingId::EnumVariant(location) => Some(TargetId::EnumVariant(location)),
            BindingId::CompilerIntrinsic(_)
            | BindingId::RuntimeFunction(_)
            | BindingId::BuiltinType(_) => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VisibleBinding<'db> {
    pub name: Symbol<'db>,
    pub binding: BindingId<'db>,
    pub namespace: Namespace,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, salsa::Update)]
pub enum TargetId<'db> {
    Local(NameId),
    Param(NameId),
    Function(FunctionLocation<'db>),
    Struct(StructLocation<'db>),
    Enum(EnumLocation<'db>),
    EnumVariant(EnumVariantLocation<'db>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, salsa::Update)]
pub struct Resolution<'db> {
    pub binding: Option<BindingId<'db>>,
    pub target: Option<TargetId<'db>>,
    pub namespace: Namespace,
    pub status: ResolveStatus,
}

impl<'db> Resolution<'db> {
    fn resolved(binding: BindingId<'db>, namespace: Namespace) -> Self {
        Self {
            target: binding.target(),
            binding: Some(binding),
            namespace,
            status: ResolveStatus::Resolved,
        }
    }

    fn unresolved(namespace: Namespace) -> Self {
        Self { binding: None, target: None, namespace, status: ResolveStatus::Unresolved }
    }

    fn ambiguous(namespace: Namespace) -> Self {
        Self { binding: None, target: None, namespace, status: ResolveStatus::Ambiguous }
    }
}

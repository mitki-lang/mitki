use mitki_inputs::File;
use mitki_lower::ast_map::HasAstMap as _;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::package::{HasPackage as _, root_module};
use mitki_lower::item::scope::{
    Declaration, EnumVariantLocation, FunctionLocation, HasItemDecls as _, HasVisibleItems as _,
    ItemDecls, VisibleItems,
};
use mitki_lower::item::tree::HasItemTree as _;
use mitki_resolve::scope::HasExprScopes as _;
use mitki_resolve::{
    BindingId, Namespace, Resolution, ResolveStatus, Resolver, TargetId, VisibleBinding,
    lookup_builtin_type, resolve_method_for_receiver,
};
use mitki_span::IntoSymbol as _;
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::ast::{HasName as _, Node as _};
use mitki_yellow::{SyntaxKind, SyntaxNode, SyntaxNodePtr, ast};
use rustc_hash::FxHashMap;
use salsa::Database;
use text_size::TextRange;

#[derive(Clone, Copy)]
pub enum ResolveIntent {
    Any,
    Value,
    Type,
    EnumVariant,
}

pub struct Semantics<'db> {
    file: File,
    source_map: SourceMap<'db>,
}

impl<'db> Semantics<'db> {
    pub fn new(db: &'db dyn Database, file: File) -> Self {
        let mut source_map = SourceMap { functions: FxHashMap::default() };
        let item_tree = file.item_tree(db);
        let ast_map = file.ast_map(db);

        for &declaration in file.item_decls(db).declarations() {
            match declaration {
                Declaration::Function(func) => {
                    let id = item_tree[func.index(db)].id;
                    let &ptr = ast_map.find_node(id);
                    source_map.functions.insert(ptr, func);
                }
                Declaration::BoundaryInstance(_)
                | Declaration::Struct(_)
                | Declaration::Enum(_) => {}
            }
        }

        Self { file, source_map }
    }

    pub fn function(&self, function: &SyntaxNode) -> FunctionLocation<'db> {
        self.source_map.functions[&SyntaxNodePtr::new(function)]
    }

    pub fn resolver(
        &self,
        db: &'db dyn Database,
        location: FunctionLocation<'db>,
        current_node: &SyntaxNode,
    ) -> Resolver<'db> {
        let source_map = location.hir_function(db).source_map(db);
        let scopes = location.expr_scopes(db);
        let scope = current_node
            .ancestors()
            .filter_map(ast::Expr::cast)
            .find_map(|expr| source_map.syntax_expr(expr.syntax()))
            .and_then(|expr| scopes.scope_by_node(expr.into()));

        Resolver::for_scope(
            db,
            location.module(db),
            location.module(db).visible_items(db),
            location.module(db).item_decls(db),
            scopes,
            scope,
        )
    }

    pub fn resolve_at(
        &self,
        db: &'db dyn Database,
        current_node: &SyntaxNode,
        intent: ResolveIntent,
    ) -> Resolution<'db> {
        if let Some(binding) = self.binding_for_declaration(db, current_node) {
            return Resolution {
                binding: Some(binding),
                target: binding.target(),
                namespace: binding.namespace(),
                status: ResolveStatus::Resolved,
            };
        }

        let symbol = Self::symbol_for_node(db, current_node);
        let root_module = root_module(db, self.file.package(db));
        if let Some(location) = self.enclosing_function(current_node) {
            let resolver = self.resolver(db, location, current_node);
            return self.resolve_with_resolver(
                db,
                &resolver,
                location,
                current_node,
                symbol,
                intent,
            );
        }

        Self::resolve_file_level(
            db,
            root_module.visible_items(db),
            root_module.item_decls(db),
            symbol,
            intent,
        )
    }

    pub fn binding_at(
        &self,
        db: &'db dyn Database,
        current_node: &SyntaxNode,
        intent: ResolveIntent,
    ) -> Option<BindingId<'db>> {
        self.resolve_at(db, current_node, intent).binding
    }

    pub fn declaration_target(
        &self,
        db: &'db dyn Database,
        binding: BindingId<'db>,
    ) -> Option<TextRange> {
        match binding {
            BindingId::Local(name) | BindingId::Param(name) => {
                let source_map = self.function_source_map_for_name(db, name)?;
                Some(source_map.node_syntax(name.into()).range)
            }
            BindingId::Function(location) => Some(location.source(db).name()?.text_range()),
            BindingId::Struct(location) => Some(location.source(db).name()?.text_range()),
            BindingId::Enum(location) => Some(location.source(db).name()?.text_range()),
            BindingId::EnumVariant(location) => Some(location.source(db).name()?.text_range()),
            BindingId::RuntimeFunction(_)
            | BindingId::CompilerIntrinsic(_)
            | BindingId::BuiltinType(_) => None,
        }
    }

    pub fn definition_target(
        &self,
        db: &'db dyn Database,
        target: TargetId<'db>,
    ) -> Option<TextRange> {
        match target {
            TargetId::Local(name) | TargetId::Param(name) => {
                let source_map = self.function_source_map_for_name(db, name)?;
                Some(source_map.node_syntax(name.into()).range)
            }
            TargetId::Function(location) => Some(location.source(db).name()?.text_range()),
            TargetId::Struct(location) => Some(location.source(db).name()?.text_range()),
            TargetId::Enum(location) => Some(location.source(db).name()?.text_range()),
            TargetId::EnumVariant(location) => Some(location.source(db).name()?.text_range()),
        }
    }

    pub fn definition_target_at(
        &self,
        db: &'db dyn Database,
        current_node: &SyntaxNode,
        target: TargetId<'db>,
    ) -> Option<TextRange> {
        match target {
            TargetId::Local(name) | TargetId::Param(name) => {
                let location = self.enclosing_function(current_node)?;
                let source_map = location.hir_function(db).source_map(db);
                Some(source_map.node_syntax(name.into()).range)
            }
            other => self.definition_target(db, other),
        }
    }

    pub fn visible_bindings_at(
        &self,
        db: &'db dyn Database,
        current_node: &SyntaxNode,
    ) -> Vec<VisibleBinding<'db>> {
        if let Some(location) = self.enclosing_function(current_node) {
            return self.resolver(db, location, current_node).visible_bindings();
        }

        let mut bindings = Vec::new();
        let visible_items = root_module(db, self.file.package(db)).visible_items(db);
        bindings.extend(visible_items.values().map(|(name, function)| VisibleBinding {
            name: *name,
            binding: BindingId::Function(*function),
            namespace: Namespace::Value,
        }));
        bindings.extend(visible_items.types().filter_map(|(name, _)| {
            let declaration = visible_items.get_type_declaration(name)?;
            let binding = match declaration {
                mitki_lower::item::scope::TypeDeclaration::Struct(location) => {
                    BindingId::Struct(location)
                }
                mitki_lower::item::scope::TypeDeclaration::Enum(location) => {
                    BindingId::Enum(location)
                }
            };
            Some(VisibleBinding { name: *name, binding, namespace: Namespace::Type })
        }));
        bindings
    }

    fn resolve_with_resolver(
        &self,
        db: &'db dyn Database,
        resolver: &Resolver<'db>,
        location: FunctionLocation<'db>,
        current_node: &SyntaxNode,
        symbol: mitki_span::Symbol<'db>,
        intent: ResolveIntent,
    ) -> Resolution<'db> {
        match intent {
            ResolveIntent::Value => {
                Self::resolve_method_call_at(db, location, current_node, symbol)
                    .unwrap_or_else(|| resolver.resolve_name(symbol, Namespace::Value))
            }
            ResolveIntent::Type => resolver.resolve_name(symbol, Namespace::Type),
            ResolveIntent::EnumVariant => {
                self.resolve_enum_variant_at(db, resolver, current_node, symbol)
            }
            ResolveIntent::Any => {
                if current_node.kind() == SyntaxKind::PATH_TYPE {
                    resolver.resolve_name(symbol, Namespace::Type)
                } else {
                    if Self::is_enum_variant_site(current_node) {
                        let enum_variant =
                            self.resolve_enum_variant_at(db, resolver, current_node, symbol);
                        if enum_variant.status != ResolveStatus::Unresolved {
                            return enum_variant;
                        }
                    }
                    if let Some(method) =
                        Self::resolve_method_call_at(db, location, current_node, symbol)
                    {
                        return method;
                    }
                    let value = resolver.resolve_name(symbol, Namespace::Value);
                    if value.status == ResolveStatus::Resolved {
                        value
                    } else {
                        resolver.resolve_name(symbol, Namespace::Type)
                    }
                }
            }
        }
    }

    fn resolve_enum_variant_at(
        &self,
        db: &'db dyn Database,
        resolver: &Resolver<'db>,
        current_node: &SyntaxNode,
        symbol: mitki_span::Symbol<'db>,
    ) -> Resolution<'db> {
        let Some(field_expr) = current_node.ancestors().find_map(ast::FieldExpr::cast) else {
            return resolver.resolve_enum_variant_binding(symbol);
        };
        let Some(field_name) = field_expr.name() else {
            return resolver.resolve_enum_variant_binding(symbol);
        };
        if field_name.syntax().text_range() != current_node.text_range() {
            return resolver.resolve_enum_variant_binding(symbol);
        }

        let matching = if let Some(base_expr) = field_expr.expr() {
            let ast::Expr::Path(path) = base_expr else {
                return Resolution {
                    binding: None,
                    target: None,
                    namespace: Namespace::Value,
                    status: ResolveStatus::Unresolved,
                };
            };
            let Some(base_name) = path.name() else {
                return Resolution {
                    binding: None,
                    target: None,
                    namespace: Namespace::Value,
                    status: ResolveStatus::Unresolved,
                };
            };
            let base_symbol = base_name.as_str().into_symbol(db);
            let enum_location = match self.file.visible_items(db).get_type_declaration(&base_symbol)
            {
                Some(mitki_lower::item::scope::TypeDeclaration::Enum(location)) => location,
                _ => match resolver.resolve_name(base_symbol, Namespace::Type).binding {
                    Some(BindingId::Enum(location)) => location,
                    _ => {
                        return Resolution {
                            binding: None,
                            target: None,
                            namespace: Namespace::Value,
                            status: ResolveStatus::Unresolved,
                        };
                    }
                },
            };
            self.file
                .item_decls(db)
                .enum_variants_by_name(&symbol)
                .iter()
                .copied()
                .filter(|variant| {
                    let parent = variant.parent(db);
                    parent.file(db) == enum_location.file(db)
                        && parent.index(db) == enum_location.index(db)
                })
                .collect::<Vec<_>>()
        } else {
            self.file.item_decls(db).enum_variants_by_name(&symbol).to_vec()
        };

        match matching.as_slice() {
            [] => Resolution {
                binding: None,
                target: None,
                namespace: Namespace::Value,
                status: ResolveStatus::Unresolved,
            },
            [variant] => Resolution {
                binding: Some(BindingId::EnumVariant(*variant)),
                target: Some(TargetId::EnumVariant(*variant)),
                namespace: Namespace::Value,
                status: ResolveStatus::Resolved,
            },
            _ => Resolution {
                binding: None,
                target: None,
                namespace: Namespace::Value,
                status: ResolveStatus::Ambiguous,
            },
        }
    }

    fn resolve_method_call_at(
        db: &'db dyn Database,
        location: FunctionLocation<'db>,
        current_node: &SyntaxNode,
        symbol: mitki_span::Symbol<'db>,
    ) -> Option<Resolution<'db>> {
        let field_expr = current_node.ancestors().find_map(ast::FieldExpr::cast)?;
        let field_name = field_expr.name()?;
        if field_name.syntax().text_range() != current_node.text_range() {
            return None;
        }

        let call_expr = current_node.ancestors().find_map(ast::CallExpr::cast)?;
        if call_expr.callee()?.syntax().text_range() != field_expr.syntax().text_range() {
            return None;
        }

        let receiver_expr = field_expr.expr()?;
        let source_map = location.hir_function(db).source_map(db);
        let receiver = source_map.syntax_expr(receiver_expr.syntax())?;
        let receiver_ty = location.infer(db).type_of_node(receiver)?;
        let method = resolve_method_for_receiver(db, receiver_ty, symbol)?;

        Some(Resolution {
            binding: Some(BindingId::Function(method.function)),
            target: Some(TargetId::Function(method.function)),
            namespace: Namespace::Value,
            status: ResolveStatus::Resolved,
        })
    }

    fn resolve_file_level(
        db: &'db dyn Database,
        visible_items: &VisibleItems<'db>,
        item_decls: &ItemDecls<'db>,
        symbol: mitki_span::Symbol<'db>,
        intent: ResolveIntent,
    ) -> Resolution<'db> {
        match intent {
            ResolveIntent::Value => visible_items.get_value(&symbol).map_or(
                Resolution {
                    binding: None,
                    target: None,
                    namespace: Namespace::Value,
                    status: ResolveStatus::Unresolved,
                },
                |function| Resolution {
                    binding: Some(BindingId::Function(function)),
                    target: Some(TargetId::Function(function)),
                    namespace: Namespace::Value,
                    status: ResolveStatus::Resolved,
                },
            ),
            ResolveIntent::Type | ResolveIntent::Any => {
                if let Some(declaration) = visible_items.get_type_declaration(&symbol) {
                    let (binding, target) = match declaration {
                        mitki_lower::item::scope::TypeDeclaration::Struct(location) => {
                            (BindingId::Struct(location), TargetId::Struct(location))
                        }
                        mitki_lower::item::scope::TypeDeclaration::Enum(location) => {
                            (BindingId::Enum(location), TargetId::Enum(location))
                        }
                    };
                    return Resolution {
                        binding: Some(binding),
                        target: Some(target),
                        namespace: Namespace::Type,
                        status: ResolveStatus::Resolved,
                    };
                }
                if let Some(ty) = lookup_builtin_type(db, symbol) {
                    return Resolution {
                        binding: Some(BindingId::BuiltinType(ty)),
                        target: None,
                        namespace: Namespace::Type,
                        status: ResolveStatus::Resolved,
                    };
                }
                Resolution {
                    binding: None,
                    target: None,
                    namespace: Namespace::Type,
                    status: ResolveStatus::Unresolved,
                }
            }
            ResolveIntent::EnumVariant => {
                let variants = item_decls.enum_variants_by_name(&symbol);
                match variants {
                    [] => Resolution {
                        binding: None,
                        target: None,
                        namespace: Namespace::Value,
                        status: ResolveStatus::Unresolved,
                    },
                    [variant] => Resolution {
                        binding: Some(BindingId::EnumVariant(*variant)),
                        target: Some(TargetId::EnumVariant(*variant)),
                        namespace: Namespace::Value,
                        status: ResolveStatus::Resolved,
                    },
                    _ => Resolution {
                        binding: None,
                        target: None,
                        namespace: Namespace::Value,
                        status: ResolveStatus::Ambiguous,
                    },
                }
            }
        }
    }

    fn binding_for_declaration(
        &self,
        db: &'db dyn Database,
        current_node: &SyntaxNode,
    ) -> Option<BindingId<'db>> {
        let symbol = Self::symbol_for_node(db, current_node);

        if let Some(function) = current_node.ancestors().find_map(ast::Function::cast)
            && function
                .name()
                .is_some_and(|name| name.syntax().text_range() == current_node.text_range())
        {
            return Some(BindingId::Function(self.function(function.syntax())));
        }

        if let Some(struct_def) = current_node.ancestors().find_map(ast::StructDef::cast)
            && struct_def
                .name()
                .is_some_and(|name| name.syntax().text_range() == current_node.text_range())
        {
            let declaration = self.file.visible_items(db).get_type_declaration(&symbol)?;
            return match declaration {
                mitki_lower::item::scope::TypeDeclaration::Struct(location) => {
                    Some(BindingId::Struct(location))
                }
                mitki_lower::item::scope::TypeDeclaration::Enum(_) => None,
            };
        }

        if let Some(enum_def) = current_node.ancestors().find_map(ast::EnumDef::cast)
            && enum_def
                .name()
                .is_some_and(|name| name.syntax().text_range() == current_node.text_range())
        {
            let declaration = self.file.visible_items(db).get_type_declaration(&symbol)?;
            return match declaration {
                mitki_lower::item::scope::TypeDeclaration::Enum(location) => {
                    Some(BindingId::Enum(location))
                }
                mitki_lower::item::scope::TypeDeclaration::Struct(_) => None,
            };
        }

        if let Some(variant) = current_node.ancestors().find_map(ast::EnumVariant::cast)
            && variant
                .name()
                .is_some_and(|name| name.syntax().text_range() == current_node.text_range())
        {
            return self
                .find_enum_variant_by_range(db, current_node.text_range())
                .map(BindingId::EnumVariant);
        }

        None
    }

    fn find_enum_variant_by_range(
        &self,
        db: &'db dyn Database,
        range: TextRange,
    ) -> Option<EnumVariantLocation<'db>> {
        self.file.item_decls(db).enum_variants().iter().copied().find(|variant| {
            variant.source(db).name().is_some_and(|name| name.text_range() == range)
        })
    }

    fn enclosing_function(&self, current_node: &SyntaxNode) -> Option<FunctionLocation<'db>> {
        current_node
            .ancestors()
            .find_map(ast::Function::cast)
            .map(|function| self.function(function.syntax()))
    }

    fn function_source_map_for_name(
        &self,
        db: &'db dyn Database,
        name: mitki_hir::hir::NameId,
    ) -> Option<&mitki_lower::hir::FunctionSourceMap> {
        self.source_map.functions.values().copied().find_map(|location| {
            let source_map = location.hir_function(db).source_map(db);
            source_map.try_node_syntax(name.into())?;
            Some(source_map)
        })
    }

    fn is_enum_variant_site(current_node: &SyntaxNode) -> bool {
        current_node.ancestors().find_map(ast::FieldExpr::cast).is_some()
    }

    fn symbol_for_node(
        db: &'db dyn Database,
        current_node: &SyntaxNode,
    ) -> mitki_span::Symbol<'db> {
        if current_node.kind() == SyntaxKind::PATH_EXPR {
            return current_node.text_trimmed().into_symbol(db);
        }

        if current_node.kind() == SyntaxKind::PATH_TYPE {
            return ast::PathType::cast(*current_node).map_or_else(
                || current_node.text_trimmed().into_symbol(db),
                |path| path.path_text().into_symbol(db),
            );
        }

        current_node
            .children_with_tokens()
            .find_map(|element| {
                let token = element.into_token()?;
                if token.is_trivia() { None } else { Some(token.text_trimmed().into_symbol(db)) }
            })
            .unwrap_or_else(|| current_node.text_trimmed().into_symbol(db))
    }
}

struct SourceMap<'db> {
    functions: FxHashMap<SyntaxNodePtr, FunctionLocation<'db>>,
}

#[cfg(test)]
mod tests {
    use mitki_db::RootDatabase;
    use mitki_inputs::File;
    use mitki_parse::FileParse as _;
    use mitki_resolve::{BindingId, ResolveStatus};
    use mitki_yellow::SyntaxKind;
    use mitki_yellow::ast::HasName as _;
    use text_size::{TextRange, TextSize};

    use super::{ResolveIntent, Semantics};

    const DEF_MARKER: &str = "$def$";

    fn extract_cursor_offset(text: &str) -> (TextSize, String) {
        let marker = "$0";
        let cursor = text.find(marker).expect("cursor marker");
        let mut new_text = String::with_capacity(text.len() - marker.len());
        new_text.push_str(&text[..cursor]);
        new_text.push_str(&text[cursor + marker.len()..]);
        (TextSize::from(cursor as u32), new_text)
    }

    fn extract_definition_range(text: &str) -> (TextRange, String) {
        let mut text = text.to_owned();
        let def = text.find(DEF_MARKER).expect("definition marker");
        text.replace_range(def..def + DEF_MARKER.len(), "");
        let len =
            text[def..].chars().take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_').count();
        (TextRange::at(TextSize::from(def as u32), TextSize::from(len as u32)), text)
    }

    fn name_node_at<'db>(
        db: &'db RootDatabase,
        file: File,
        offset: TextSize,
    ) -> mitki_yellow::SyntaxNode<'db> {
        let root = file.parse(db).syntax_node();
        let token = root
            .token_at_offset(offset)
            .filter(|token| !token.is_trivia())
            .max_by_key(|token| usize::from(token.kind() == SyntaxKind::NAME))
            .expect("name token");
        token.parent()
    }

    #[test]
    fn resolve_at_reports_unresolved_status() {
        let db = RootDatabase::default();
        let (offset, text) = extract_cursor_offset(
            r#"
fun main() {
    $0missing
}
"#,
        );
        let file = File::new(&db, "test.mitki".into(), text);
        let semantics = Semantics::new(&db, file);
        let node = name_node_at(&db, file, offset);

        let resolution = semantics.resolve_at(&db, &node, ResolveIntent::Any);
        assert_eq!(resolution.status, ResolveStatus::Unresolved);
        assert!(resolution.binding.is_none());
    }

    #[test]
    fn resolve_at_reports_ambiguous_enum_variant_status() {
        let db = RootDatabase::default();
        let (offset, text) = extract_cursor_offset(
            r#"
enum Color { Red }
enum Light { Red }

fun main() {
    .R$0ed
}
"#,
        );
        let file = File::new(&db, "test.mitki".into(), text);
        let semantics = Semantics::new(&db, file);
        let node = name_node_at(&db, file, offset);

        let resolution = semantics.resolve_at(&db, &node, ResolveIntent::Any);
        assert_eq!(resolution.status, ResolveStatus::Ambiguous);
        assert!(resolution.target.is_none());
    }

    #[test]
    fn declaration_and_definition_targets_match_for_locals() {
        let db = RootDatabase::default();
        let (expected, text) = extract_definition_range(
            r#"
fun main() {
    val $def$x = 1
    x
}
"#,
        );
        let (offset, text) = extract_cursor_offset(&text.replace("    x", "    $0x"));
        let file = File::new(&db, "test.mitki".into(), text);
        let semantics = Semantics::new(&db, file);
        let node = name_node_at(&db, file, offset);
        let resolution = semantics.resolve_at(&db, &node, ResolveIntent::Any);
        let binding = resolution.binding.expect("binding");
        let target = resolution.target.expect("target");

        assert_eq!(semantics.declaration_target(&db, binding), Some(expected));
        assert_eq!(semantics.definition_target(&db, target), Some(expected));
    }

    #[test]
    fn visible_bindings_are_ordered_from_inner_to_outer() {
        let db = RootDatabase::default();
        let (offset, text) = extract_cursor_offset(
            r#"
struct Point { x: int }

fun main(x: int) {
    val x = 1
    $0x
}
"#,
        );
        let file = File::new(&db, "test.mitki".into(), text);
        let semantics = Semantics::new(&db, file);
        let node = name_node_at(&db, file, offset);
        let visible = semantics.visible_bindings_at(&db, &node);

        assert!(matches!(
            visible.first().map(|binding| binding.binding),
            Some(BindingId::Local(_))
        ));
        assert!(visible.iter().any(|binding| matches!(binding.binding, BindingId::Param(_))));
        assert!(visible.iter().any(|binding| matches!(binding.binding, BindingId::Struct(_))));
    }

    #[test]
    fn resolve_at_maps_method_names_to_module_functions() {
        let db = RootDatabase::default();
        let (offset, text) = extract_cursor_offset(
            r#"
use std::vec::int as vec;

fun main() {
    val xs: vec::Vec = vec::new()
    xs.pu$0sh(1)
}
"#,
        );
        let file = File::new(&db, "test.mitki".into(), text);
        let semantics = Semantics::new(&db, file);
        let node = name_node_at(&db, file, offset);

        let resolution = semantics.resolve_at(&db, &node, ResolveIntent::Any);
        let BindingId::Function(function) = resolution.binding.expect("binding") else {
            panic!("expected method name to resolve to a function");
        };

        assert_eq!(resolution.status, ResolveStatus::Resolved);
        assert_eq!(function.source(&db).name().expect("function name").as_str(), "push");
    }
}

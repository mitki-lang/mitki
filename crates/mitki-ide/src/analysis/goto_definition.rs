use mitki_analysis::Semantics;
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{Declaration, HasItemScope as _, ItemScopeDb};
use mitki_parse::FileParse as _;
use mitki_resolve::{Resolution, ResolverDb};
use mitki_span::IntoSymbol as _;
use mitki_yellow::SyntaxKind;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use text_size::TextRange;

use crate::{FilePosition, find_name_at_offset};

impl super::Analysis {
    pub async fn goto_definition(
        &self,
        FilePosition { file, offset }: FilePosition,
    ) -> Option<(TextRange, TextRange)> {
        let db = self.db();
        let semantics = Semantics::new(db, file).await;
        let parsed = file.parse(db).await;
        let root = parsed.syntax_node();

        let name_at_offset = find_name_at_offset(root, offset, |kind| {
            kind == SyntaxKind::NAME_REF || kind == SyntaxKind::PATH_TYPE
        })?;
        let original_token = name_at_offset.token;
        let name_node = name_at_offset.node;

        let symbol = original_token.text_trimmed().into_symbol(db);
        let location = name_node
            .ancestors()
            .find_map(ast::Function::cast)
            .map(|function| semantics.function(function.syntax()));

        if let Some(location) = location {
            let mut resolver = semantics.resolver(db, location, &name_node).await;

            let mut type_guard = None;
            if name_node.kind() == SyntaxKind::PATH_TYPE {
                let hir = location.hir_function(db).await;
                let source_map = hir.source_map();
                if let Some(ty_id) = source_map.syntax_type(&name_node) {
                    type_guard = Some(resolver.scopes_for_type(ty_id));
                }
            }

            if let Some(target) = enum_variant_target(db, file, &resolver, &name_node, symbol).await
            {
                return Some((original_token.trimmed_range(), target));
            }

            let resolution = resolver.resolve_path(symbol);
            if let Some(guard) = type_guard {
                resolver.reset(guard);
            }

            match resolution? {
                Resolution::Local(path) => {
                    let hir = location.hir_function(db).await;
                    let source_map = hir.source_map();
                    let range = source_map.node_syntax(path.into()).range;

                    Some((original_token.trimmed_range(), range))
                }
                Resolution::Function(function_location) => {
                    let parsed = function_location.file(db).parse(db).await;
                    let syntax = function_location.source_ptr(db).to_node(&parsed.syntax_node());
                    let function = ast::Function::cast(syntax).unwrap();
                    let function_name_range = function.name().unwrap().text_range();

                    Some((original_token.trimmed_range(), function_name_range))
                }
                Resolution::Type(ty) => type_definition_range(db, file, ty)
                    .await
                    .map(|range| (original_token.trimmed_range(), range)),
            }
        } else {
            let scope = file.item_scope(db).await;
            let ty = scope.get_type(&symbol)?;
            let range = type_definition_range(db, file, ty).await?;
            Some((original_token.trimmed_range(), range))
        }
    }
}

async fn enum_variant_target<DB>(
    db: &DB,
    file: mitki_inputs::File,
    resolver: &mitki_resolve::Resolver<'_, DB>,
    name_ref: &mitki_yellow::SyntaxNode<'_>,
    variant_name: mitki_span::Symbol,
) -> Option<TextRange>
where
    DB: ResolverDb,
{
    let field_expr = name_ref.ancestors().find_map(ast::FieldExpr::cast)?;
    let field_name = field_expr.name()?;
    if field_name.syntax().text_range() != name_ref.text_range() {
        return None;
    }

    let enum_ty = if let Some(base_expr) = field_expr.expr() {
        let ast::Expr::Path(path) = base_expr else {
            return None;
        };
        let base_name = path.name()?.as_str().into_symbol(db);
        match resolver.resolve_path(base_name) {
            Some(Resolution::Type(ty)) => ty,
            _ => return None,
        }
    } else {
        resolver.resolve_enum_variant(variant_name)?
    };

    enum_variant_definition_range(db, file, enum_ty, variant_name).await
}

async fn enum_variant_definition_range<DB>(
    db: &DB,
    file: mitki_inputs::File,
    enum_ty: Ty,
    variant_name: mitki_span::Symbol,
) -> Option<TextRange>
where
    DB: ItemScopeDb,
{
    let TyKind::Enum { name, .. } = enum_ty.kind(db) else {
        return None;
    };

    let scope = file.item_scope(db).await;
    let mut enum_decl = None;
    for declaration in scope.declarations() {
        let Declaration::Enum(candidate) = *declaration else {
            continue;
        };
        let parsed = candidate.file(db).parse(db).await;
        let syntax = candidate.source_ptr(db).to_node(&parsed.syntax_node());
        let source = ast::EnumDef::cast(syntax).unwrap();
        if source.name().is_some_and(|enum_name| enum_name.as_str().into_symbol(db) == name) {
            enum_decl = Some(candidate);
            break;
        }
    }
    let enum_decl = enum_decl?;

    let parsed = enum_decl.file(db).parse(db).await;
    let syntax = enum_decl.source_ptr(db).to_node(&parsed.syntax_node());
    let source = ast::EnumDef::cast(syntax).unwrap();
    let variants = source.variant_list()?;
    for variant in variants.variants() {
        let Some(name_node) = variant.name() else {
            continue;
        };
        if name_node.as_str().into_symbol(db) == variant_name {
            return Some(name_node.text_range());
        }
    }

    None
}

async fn type_definition_range<DB>(db: &DB, file: mitki_inputs::File, ty: Ty) -> Option<TextRange>
where
    DB: ItemScopeDb,
{
    let ty_name = match ty.kind(db) {
        TyKind::Struct { name, .. } | TyKind::Enum { name, .. } => name,
        _ => return None,
    };

    let scope = file.item_scope(db).await;
    for declaration in scope.declarations() {
        match declaration {
            Declaration::Struct(struct_decl) => {
                let parsed = struct_decl.file(db).parse(db).await;
                let syntax = struct_decl.source_ptr(db).to_node(&parsed.syntax_node());
                let source = ast::StructDef::cast(syntax).unwrap();
                let Some(name_node) = source.name() else {
                    continue;
                };
                if name_node.as_str().into_symbol(db) == ty_name {
                    return Some(name_node.text_range());
                }
            }
            Declaration::Enum(enum_decl) => {
                let parsed = enum_decl.file(db).parse(db).await;
                let syntax = enum_decl.source_ptr(db).to_node(&parsed.syntax_node());
                let source = ast::EnumDef::cast(syntax).unwrap();
                let Some(name_node) = source.name() else {
                    continue;
                };
                if name_node.as_str().into_symbol(db) == ty_name {
                    return Some(name_node.text_range());
                }
            }
            Declaration::Function(_) => {}
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use mitki_inputs::File;
    use text_size::{TextRange, TextSize};

    use crate::{Analysis, FilePosition, extract_cursor_offset};

    const DEF_MARKER: &str = "$def$";

    fn extract_annotations(text: &str) -> Vec<TextRange> {
        let mut line_start_map = BTreeMap::new();
        let mut annotations = Vec::new();
        let mut line_start: TextSize = 0.into();

        for line in text.split_inclusive('\n') {
            let line_length = if let Some((prefix, suffix)) = line.split_once("//") {
                let ss_len = TextSize::of("//");
                let annotation_offset = TextSize::of(prefix) + ss_len;
                for mut annotation in extract_line_annotations(suffix.trim_end_matches('\n')) {
                    annotation += annotation_offset;
                    let line_start = line_start_map.range(annotation.end()..).next().unwrap();

                    annotations.push(annotation + line_start.1);
                }

                annotation_offset
            } else {
                TextSize::of(line)
            };

            line_start_map = line_start_map.split_off(&line_length);
            line_start_map.insert(line_length, line_start);
            line_start += TextSize::of(line);
        }

        annotations
    }

    fn extract_line_annotations(mut line: &str) -> Vec<TextRange> {
        let mut annotations = Vec::new();
        let mut offset: TextSize = 0.into();

        while let Some(idx) = line.find('^') {
            offset += TextSize::try_from(idx).unwrap();
            line = &line[idx..];

            let len = line.chars().take_while(|&it| it == '^').count();
            let range = TextRange::at(offset, len.try_into().unwrap());

            annotations.push(range);
            line = &line[len..];
        }

        annotations
    }

    async fn check(fixture: &str) {
        let analysis = Analysis::default();
        let (offset, fixture) = extract_cursor_offset(fixture);
        let annotations = extract_annotations(&fixture);
        let file = File::new(analysis.db(), "".into(), fixture.clone());
        let file_position = FilePosition { file, offset };

        assert_eq!(annotations.len(), 1);
        let expected = annotations.into_iter().next().unwrap();

        let (_, focus) =
            analysis.goto_definition(file_position).await.expect("no definition found");

        assert_eq!(focus, expected);
    }

    async fn check_none(fixture: &str) {
        let analysis = Analysis::default();
        let (offset, fixture) = extract_cursor_offset(fixture);
        let file = File::new(analysis.db(), "".into(), fixture);
        let file_position = FilePosition { file, offset };

        assert!(analysis.goto_definition(file_position).await.is_none());
    }

    fn extract_offset_and_expected_range(text: &str) -> (TextSize, TextRange, String) {
        let mut text = text.to_owned();
        let def_pos = text.find(DEF_MARKER).expect("Definition marker not found");
        text.replace_range(def_pos..def_pos + DEF_MARKER.len(), "");

        let ident_len = text[def_pos..]
            .chars()
            .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
            .count();
        let expected =
            TextRange::at(TextSize::from(def_pos as u32), TextSize::from(ident_len as u32));

        let (cursor_pos, text) = extract_cursor_offset(&text);

        (cursor_pos, expected, text)
    }

    async fn check_def_marker(fixture: &str) {
        let analysis = Analysis::default();
        let (offset, expected, fixture) = extract_offset_and_expected_range(fixture);
        let file = File::new(analysis.db(), "".into(), fixture);
        let file_position = FilePosition { file, offset };
        let (_, focus) =
            analysis.goto_definition(file_position).await.expect("no definition found");
        assert_eq!(focus, expected);
    }

    #[tokio::test]
    async fn variable() {
        check(
            r#"
fun main() {
    val x = 42
      //^
    $0x
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn function() {
        check(
            r#"
fun add() {}
  //^^^
fun main() {
    add$0();
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn parameter() {
        check(
            r#"
fun foo(x: i32) {
      //^
    $0x
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn if_block() {
        check(
            r#"
fun main() {
    val x = 42
      //^
    if true {
        $0x
    }
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn function_forward() {
        check(
            r#"
fun main() {
    add$0();
}

fun add() {}
  //^^^
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn closure_param() {
        check(
            r#"
fun main() {
    val f = { x in
            //^
        $0x
    }
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn closure_capture() {
        check(
            r#"
fun main() {
    val x = 0
      //^
    val f = {
        $0x
    }
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn shadow_inner() {
        check(
            r#"
fun main() {
    val x = 0
    {
        val x = 1
          //^
        $0x
    }
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn param_shadow() {
        check(
            r#"
fun foo(x: i32) {
    val x = 42
      //^
    $0x
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn nested_closure_capture() {
        check(
            r#"
fun main() {
    val f = { x in
            //^
        val g = {
            $0x
        }
    }
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn enum_variant_qualified() {
        check(
            r#"
enum Color {
    Red,
  //^^^
}

fun main() {
    Color.$0Red
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn enum_variant_without_prefix() {
        check(
            r#"
enum Color {
    Red,
  //^^^
}

fun main() {
    .R$0ed
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn enum_variant_without_prefix_ambiguous_no_definition() {
        check_none(
            r#"
enum Color {
    Red,
}

enum Light {
    Red,
}

fun main() {
    .R$0ed
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn struct_type_annotation() {
        check_def_marker(
            r#"
struct $def$Point {
    x: int,
    y: int,
}

fun main() {
    val p: P$0oint = Point { x: 1, y: 2 }
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn enum_type_annotation() {
        check_def_marker(
            r#"
enum $def$Color {
    Red,
}

fun paint(color: C$0olor) {}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn top_level_type_reference() {
        check_def_marker(
            r#"
struct $def$Point {
    x: int,
}

struct Wrapper {
    value: P$0oint,
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn type_used_as_value() {
        check_def_marker(
            r#"
struct $def$Point {
    x: int,
}

fun main() {
    P$0oint
}
"#,
        )
        .await;
    }

    #[tokio::test]
    async fn builtin_type_no_definition() {
        check_none(
            r#"
fun main() {
    val x: i$0nt = 1
}
"#,
        )
        .await;
    }
}

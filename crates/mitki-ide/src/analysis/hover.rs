use mitki_analysis::Semantics;
use mitki_hir::hir::{ExprId, TyId};
use mitki_lower::hir::HasFunction as _;
use mitki_parse::FileParse as _;
use mitki_resolve::Resolution;
use mitki_span::IntoSymbol as _;
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::SyntaxKind;
use mitki_yellow::ast::{self, HasName as _, Node as _};
use text_size::TextRange;

use crate::{FilePosition, find_name_at_offset};

pub struct HoverResult {
    pub range: TextRange,
    pub contents: String,
}

impl super::Analysis {
    pub fn hover(&self, FilePosition { file, offset }: FilePosition) -> Option<HoverResult> {
        let db = self.db();
        let semantics = Semantics::new(db, file);
        let root = file.parse(db).syntax_node();

        let name_at_offset = find_name_at_offset(root, offset, |kind| {
            kind == SyntaxKind::NAME_REF || kind == SyntaxKind::IDENT
        })?;
        let original_token = name_at_offset.token;
        let name_node = name_at_offset.node;

        let name = original_token.text_trimmed().into_symbol(db);
        let function_hover_contents = |function: ast::Function<'_>| {
            let fn_name = function.name().map(|n| n.as_str()).unwrap_or_default();
            let input_tys = function
                .params()
                .map(|params| {
                    params
                        .iter()
                        .map(|param| {
                            param.ty().map_or_else(
                                || "{unknown}".to_owned(),
                                |ty| ty.syntax().text_trimmed().to_owned(),
                            )
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let output_ty = function
                .ret_type()
                .and_then(|ret| ret.ty())
                .map_or_else(|| "()".to_owned(), |ty| ty.syntax().text_trimmed().to_owned());
            format!(
                "```mitki\nfun {}: fun({}) -> {}\n```",
                fn_name,
                input_tys.join(", "),
                output_ty
            )
        };

        if name_node.kind() == SyntaxKind::IDENT {
            if let Some(function_syntax) = name_node.ancestors().find_map(ast::Function::cast)
                && function_syntax.name().is_some_and(|decl_name| {
                    decl_name.syntax().text_range() == name_node.text_range()
                })
            {
                return Some(HoverResult {
                    range: original_token.trimmed_range(),
                    contents: function_hover_contents(function_syntax),
                });
            }

            if let Some(struct_syntax) = name_node.ancestors().find_map(ast::StructDef::cast)
                && struct_syntax.name().is_some_and(|decl_name| {
                    decl_name.syntax().text_range() == name_node.text_range()
                })
            {
                return Some(HoverResult {
                    range: original_token.trimmed_range(),
                    contents: format!("```mitki\ntype {}\n```", struct_syntax.name()?.as_str()),
                });
            }

            if let Some(enum_syntax) = name_node.ancestors().find_map(ast::EnumDef::cast)
                && enum_syntax.name().is_some_and(|decl_name| {
                    decl_name.syntax().text_range() == name_node.text_range()
                })
            {
                return Some(HoverResult {
                    range: original_token.trimmed_range(),
                    contents: format!("```mitki\ntype {}\n```", enum_syntax.name()?.as_str()),
                });
            }
        }

        let location = name_node
            .ancestors()
            .find_map(ast::Function::cast)
            .map(|function| semantics.function(function.syntax()))?;
        let inference = location.infer(db);
        let function = location.hir_function(db).function(db);
        let source_map = location.hir_function(db).source_map(db);
        let param_annotation_ty_text = |binding_expr: ExprId| {
            for &param in function.params() {
                let (param_name, param_ty) = function.node_store().param(param);
                if ExprId::from(param_name) != binding_expr || param_ty == TyId::ZERO {
                    continue;
                }
                let ty_syntax = source_map.try_type_syntax(param_ty)?.to_node(&root);
                return Some(ty_syntax.text_trimmed().to_owned());
            }
            None
        };

        if name_node.kind() == SyntaxKind::IDENT {
            if let Some(expr_id) = source_map.syntax_expr(&name_node) {
                let ty = inference.type_of_node(expr_id)?;
                let ty_text = param_annotation_ty_text(expr_id)
                    .unwrap_or_else(|| format!("{}", ty.display(db)));

                return Some(HoverResult {
                    range: original_token.trimmed_range(),
                    contents: format!("```mitki\nval {}: {ty_text}\n```", name.text(db)),
                });
            }

            return None;
        }

        let resolver = semantics.resolver(db, location, &name_node);
        let resolution = resolver.resolve_path(name)?;

        match resolution {
            Resolution::Local(binding) => {
                let ty = inference.type_of_node(binding.into())?;
                let ty_text = param_annotation_ty_text(binding.into())
                    .unwrap_or_else(|| format!("{}", ty.display(db)));
                Some(HoverResult {
                    range: original_token.trimmed_range(),
                    contents: format!("```mitki\nval {}: {ty_text}\n```", name.text(db)),
                })
            }
            Resolution::Function(func) => {
                let function_syntax = func.source(db);

                Some(HoverResult {
                    range: original_token.trimmed_range(),
                    contents: function_hover_contents(function_syntax),
                })
            }
            Resolution::Type(ty) => Some(HoverResult {
                range: original_token.trimmed_range(),
                contents: format!("```mitki\ntype {}\n```", ty.display(db)),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use mitki_inputs::File;

    use crate::{Analysis, FilePosition, extract_cursor_offset};

    #[track_caller]
    fn check(fixture: &str, expected: &str) {
        let analysis = Analysis::default();
        let (offset, fixture) = extract_cursor_offset(fixture);
        let file = File::new(analysis.db(), "".into(), fixture);
        let result = analysis.hover(FilePosition { file, offset }).expect("expected hover result");
        assert_eq!(result.contents, expected);
    }

    #[track_caller]
    fn check_no_hover(fixture: &str) {
        let analysis = Analysis::default();
        let (offset, fixture) = extract_cursor_offset(fixture);
        let file = File::new(analysis.db(), "".into(), fixture);
        assert!(analysis.hover(FilePosition { file, offset }).is_none());
    }

    #[test]
    fn local_variable() {
        check(
            r#"
fun main() {
    val x = 42
    $0x
}
"#,
            "```mitki\nval x: int\n```",
        );
    }

    #[test]
    fn parameter() {
        check(
            r#"
fun foo(x: int) {
    $0x
}
"#,
            "```mitki\nval x: int\n```",
        );
    }

    #[test]
    fn parameter_declaration() {
        check(
            r#"
fun foo($0x: int) {
    x
}
"#,
            "```mitki\nval x: int\n```",
        );
    }

    #[test]
    fn generic_parameter_reference() {
        check(
            r#"
fun hello[A](id: A): A {
    $0id
}
"#,
            "```mitki\nval id: A\n```",
        );
    }

    #[test]
    fn function_call() {
        check(
            r#"
fun add(x: int, y: int): int { x + y }

fun main() {
    $0add(1, 2);
}
"#,
            "```mitki\nfun add: fun(int, int) -> int\n```",
        );
    }

    #[test]
    fn generic_function_call_declared_signature() {
        check(
            r#"
enum Color {
    Red,
}

fun hello[A](id: A): A {
    id
}

fun main() {
    $0hello(Color.Red);
}
"#,
            "```mitki\nfun hello: fun(A) -> A\n```",
        );
    }

    #[test]
    fn function_declaration_name() {
        check(
            r#"
fun $0hello[A](id: A): A {
    id
}
"#,
            "```mitki\nfun hello: fun(A) -> A\n```",
        );
    }

    #[test]
    fn struct_declaration_name() {
        check(
            r#"
struct $0Point {
    x: int,
    y: int,
}
"#,
            "```mitki\ntype Point\n```",
        );
    }

    #[test]
    fn enum_declaration_name() {
        check(
            r#"
enum $0Color {
    Red,
}
"#,
            "```mitki\ntype Color\n```",
        );
    }

    #[test]
    fn function_no_params() {
        check(
            r#"
fun noop() {}

fun main() {
    $0noop();
}
"#,
            "```mitki\nfun noop: fun() -> ()\n```",
        );
    }

    #[test]
    fn builtin_type_in_annotation() {
        check_no_hover(
            r#"
fun main() {
    val x: $0int = 42
}
"#,
        );
    }

    #[test]
    fn type_used_as_value() {
        check(
            r#"
fun main() {
    $0int
}
"#,
            "```mitki\ntype int\n```",
        );
    }

    #[test]
    fn struct_type_used_as_value() {
        check(
            r#"
struct Point {
    x: int,
    y: int,
}

fun main() {
    $0Point
}
"#,
            "```mitki\ntype Point\n```",
        );
    }

    #[test]
    fn bool_variable() {
        check(
            r#"
fun main() {
    val flag = true
    $0flag
}
"#,
            "```mitki\nval flag: bool\n```",
        );
    }

    #[test]
    fn closure_param_unknown() {
        check(
            r#"
fun main() {
    val f = { x in
        $0x
    }
}
"#,
            "```mitki\nval x: {unknown}\n```",
        );
    }

    #[test]
    fn no_hover_on_keyword() {
        check_no_hover(
            r#"
$0fun main() {}
"#,
        );
    }

    #[test]
    fn no_hover_on_literal() {
        check_no_hover(
            r#"
fun main() {
    $042
}
"#,
        );
    }

    #[test]
    fn string_variable() {
        check(
            r#"
fun main() {
    val s = "hello"
    $0s
}
"#,
            "```mitki\nval s: str\n```",
        );
    }

    #[test]
    fn float_variable() {
        check(
            r#"
fun main() {
    val f = 3.14
    $0f
}
"#,
            "```mitki\nval f: float\n```",
        );
    }
}

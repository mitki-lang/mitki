use mitki_hir::hir::{ExprId, NodeKind, TyId};
use mitki_hir::ty::TyKind;
use mitki_inputs::File;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{Declaration, HasItemScope as _};
use mitki_typeck::infer::Inferable as _;
use text_size::{TextRange, TextSize};

pub struct InlayHint {
    pub offset: TextSize,
    pub label: String,
}

impl super::Analysis {
    pub fn inlay_hints(&self, file: File, range: TextRange) -> Vec<InlayHint> {
        let db = self.db();
        let mut hints = Vec::new();

        for declaration in file.item_scope(db).declarations() {
            match declaration {
                Declaration::Function(func) => {
                    let hir = func.hir_function(db);
                    let source_map = hir.source_map();
                    let function = hir.function();
                    let nodes = function.node_store();
                    let inference = func.infer(db);

                    // Hints for function parameters without type annotations.
                    for &param in function.params() {
                        let (name, ty_id) = nodes.param(param);
                        if ty_id != TyId::ZERO {
                            continue;
                        }
                        let Some(ty) = inference.type_of_node(name.into()) else {
                            continue;
                        };
                        if matches!(ty.kind(db), TyKind::Unknown)
                            || matches!(ty.kind(db), TyKind::Tuple(items) if items.is_empty())
                        {
                            continue;
                        }
                        let ptr = source_map.node_syntax(name.into());
                        if !range.contains_range(ptr.range) {
                            continue;
                        }
                        hints.push(InlayHint {
                            offset: ptr.range.end(),
                            label: format!(": {}", ty.display(db)),
                        });
                    }

                    // Hints for local variable bindings without type annotations.
                    if function.body() != ExprId::ZERO {
                        collect_binding_hints(
                            db,
                            nodes,
                            source_map,
                            &inference,
                            function.body(),
                            range,
                            &mut hints,
                        );
                    }
                }
                Declaration::Struct(_) | Declaration::Enum(_) => {}
            }
        }

        hints.sort_by_key(|h| h.offset);
        hints
    }
}

fn collect_binding_hints<'db, DB>(
    db: &'db DB,
    nodes: &mitki_hir::hir::NodeStore<'db>,
    source_map: &mitki_lower::hir::FunctionSourceMap,
    inference: &mitki_typeck::infer::Inference<'db>,
    expr: ExprId,
    range: TextRange,
    hints: &mut Vec<InlayHint>,
) where
    DB: mitki_parse::ParseDb,
{
    match nodes.node_kind(expr) {
        NodeKind::Block => {
            let Some(block_id) = nodes.as_block(expr) else { return };
            let (stmts, tail) = nodes.block_stmts(block_id);
            for stmt in stmts.iter() {
                if nodes.node_kind(stmt) == NodeKind::LocalVar {
                    let Some(var_id) = nodes.as_local_var(stmt) else { continue };
                    let var = nodes.local_var(var_id);
                    if var.ty == TyId::ZERO
                        && let Some(ty) = inference.type_of_node(var.name.into())
                        && !matches!(ty.kind(db), TyKind::Unknown)
                    {
                        let ptr = source_map.node_syntax(var.name.into());
                        if range.contains_range(ptr.range) {
                            hints.push(InlayHint {
                                offset: ptr.range.end(),
                                label: format!(": {}", ty.display(db)),
                            });
                        }
                    }
                    if var.initializer != ExprId::ZERO {
                        collect_binding_hints(
                            db,
                            nodes,
                            source_map,
                            inference,
                            var.initializer,
                            range,
                            hints,
                        );
                    }
                }
            }
            if tail != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, tail, range, hints);
            }
        }
        NodeKind::If => {
            let Some(if_id) = nodes.as_if(expr) else { return };
            let if_expr = nodes.if_expr(if_id);
            if if_expr.then_branch != ExprId::ZERO {
                collect_binding_hints(
                    db,
                    nodes,
                    source_map,
                    inference,
                    if_expr.then_branch,
                    range,
                    hints,
                );
            }
            if if_expr.else_branch != ExprId::ZERO {
                collect_binding_hints(
                    db,
                    nodes,
                    source_map,
                    inference,
                    if_expr.else_branch,
                    range,
                    hints,
                );
            }
        }
        NodeKind::Closure => {
            let Some(closure_id) = nodes.as_closure(expr) else { return };
            let (params, body) = nodes.closure_parts(closure_id);
            for param in params.iter() {
                let (name, ty_id) = nodes.param(param);
                if ty_id != TyId::ZERO {
                    continue;
                }
                if let Some(ty) = inference.type_of_node(name.into())
                    && !matches!(ty.kind(db), TyKind::Unknown)
                    && !matches!(ty.kind(db), TyKind::Tuple(items) if items.is_empty())
                {
                    let ptr = source_map.node_syntax(name.into());
                    if range.contains_range(ptr.range) {
                        hints.push(InlayHint {
                            offset: ptr.range.end(),
                            label: format!(": {}", ty.display(db)),
                        });
                    }
                }
            }
            if body != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, body, range, hints);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use mitki_inputs::File;
    use text_size::{TextRange, TextSize};

    use crate::Analysis;

    fn whole_file_range() -> TextRange {
        TextRange::new(TextSize::from(0), TextSize::from(u32::MAX))
    }

    #[track_caller]
    fn check(fixture: &str, expected: &[&str]) {
        let analysis = Analysis::default();
        let file = File::new(analysis.db(), "".into(), fixture.to_owned());
        let hints = analysis.inlay_hints(file, whole_file_range());
        let labels: Vec<&str> = hints.iter().map(|h| h.label.as_str()).collect();
        assert_eq!(labels, expected);
    }

    #[test]
    fn local_variable_int() {
        check(
            r#"
fun main() {
    val x = 42
}
"#,
            &[": int"],
        );
    }

    #[test]
    fn local_variable_string() {
        check(
            r#"
fun main() {
    val s = "hello"
}
"#,
            &[": str"],
        );
    }

    #[test]
    fn local_variable_float() {
        check(
            r#"
fun main() {
    val f = 3.14
}
"#,
            &[": float"],
        );
    }

    #[test]
    fn local_variable_bool() {
        check(
            r#"
fun main() {
    val b = true
}
"#,
            &[": bool"],
        );
    }

    #[test]
    fn no_hint_with_type_annotation() {
        check(
            r#"
fun main() {
    val x: int = 42
}
"#,
            &[],
        );
    }

    #[test]
    fn multiple_bindings() {
        check(
            r#"
fun main() {
    val x = 1
    val y = 2
    val z = 3
}
"#,
            &[": int", ": int", ": int"],
        );
    }

    #[test]
    fn mixed_annotated_and_inferred() {
        check(
            r#"
fun main() {
    val x: int = 1
    val y = "hello"
    val z: bool = true
}
"#,
            &[": str"],
        );
    }

    #[test]
    fn parameter_without_type() {
        check(
            r#"
fun foo(x) {
    x
}
"#,
            &[],
        );
    }

    #[test]
    fn parameter_with_type_no_hint() {
        check(
            r#"
fun foo(x: int) {
    x
}
"#,
            &[],
        );
    }

    #[test]
    fn empty_function() {
        check(
            r#"
fun main() {}
"#,
            &[],
        );
    }

    #[test]
    fn binding_in_if_branch() {
        check(
            r#"
fun main() {
    if true {
        val a = 1
    } else {
        val b = 2
    }
}
"#,
            &[": int", ": int"],
        );
    }

    #[test]
    fn nested_blocks() {
        check(
            r#"
fun main() {
    val x = 1
    {
        val y = 2
    }
}
"#,
            &[": int", ": int"],
        );
    }

    #[test]
    fn function_call_binding() {
        check(
            r#"
fun add(x: int, y: int): int { x + y }

fun main() {
    val result = add(1, 2)
}
"#,
            &[": int"],
        );
    }

    #[test]
    fn multiple_functions() {
        check(
            r#"
fun foo() {
    val a = 1
}

fun bar() {
    val b = "hello"
}
"#,
            &[": int", ": str"],
        );
    }

    #[test]
    fn hints_sorted_by_offset() {
        check(
            r#"
fun main() {
    val first = 1
    val second = "two"
    val third = true
}
"#,
            &[": int", ": str", ": bool"],
        );
    }

    #[test]
    fn range_filter() {
        let fixture = r#"
fun main() {
    val x = 1
    val y = 2
}
"#;
        let analysis = Analysis::default();
        let file = File::new(analysis.db(), "".into(), fixture.to_owned());
        // Use an empty range at the beginning — should produce no hints.
        let hints =
            analysis.inlay_hints(file, TextRange::new(TextSize::from(0), TextSize::from(0)));
        assert!(hints.is_empty());
    }

    #[test]
    fn struct_no_hints() {
        check(
            r#"
struct Point {
    x: int,
    y: int,
}
"#,
            &[],
        );
    }

    #[test]
    fn local_variable_enum_variant_without_type_prefix() {
        check(
            r#"
enum Color {
    Red,
}

fun main() {
    val c = .Red
}
"#,
            &[],
        );
    }

    #[test]
    fn local_variable_enum_variant_constructor_without_type_prefix() {
        check(
            r#"
enum Option {
    Some(int),
    None,
}

fun main() {
    val value = .Some(1)
}
"#,
            &[],
        );
    }

    #[test]
    fn local_variable_enum_variant_constructor_without_type_prefix_with_usage() {
        check(
            r#"
enum Option {
    Some(int),
    None,
}

fun takes_option(value: Option) {}

fun main() {
    val value = .Some(1)
    takes_option(value)
}
"#,
            &[": Option"],
        );
    }

    #[test]
    fn local_variable_if_unit_not_duplicated() {
        check(
            r#"
fun main() {
    val n = if true { } else { }
}
"#,
            &[": ()"],
        );
    }
}

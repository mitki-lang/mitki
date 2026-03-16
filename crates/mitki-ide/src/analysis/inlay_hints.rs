use mitki_hir::hir::{ExprId, NodeKind, StmtId, TyId};
use mitki_hir::ty::TyKind;
use mitki_inputs::File;
use mitki_lower::HasItemDecls as _;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::Declaration;
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

        for declaration in file.item_decls(db).declarations() {
            match declaration {
                Declaration::Function(func) => {
                    let source_map = func.hir_function(db).source_map(db);
                    let function = func.hir_function(db).function(db);
                    let nodes = function.node_store();
                    let inference = func.infer(db);

                    // Hints for function parameters without type annotations.
                    for &param in function.params() {
                        let (pattern, ty_id) = nodes.param(param);
                        if ty_id != TyId::ZERO {
                            continue;
                        }
                        for name in nodes.pattern_binding_names(pattern) {
                            push_binding_hint(
                                db,
                                source_map,
                                inference,
                                name.into(),
                                false,
                                range,
                                &mut hints,
                            );
                        }
                    }

                    // Hints for local variable bindings without type annotations.
                    if function.body() != ExprId::ZERO {
                        collect_binding_hints(
                            db,
                            nodes,
                            source_map,
                            inference,
                            function.body(),
                            range,
                            &mut hints,
                        );
                    }
                }
                Declaration::BoundaryInstance(_)
                | Declaration::Struct(_)
                | Declaration::Enum(_) => {}
            }
        }

        hints.sort_by_key(|h| h.offset);
        hints
    }
}

fn collect_binding_hints<'db>(
    db: &'db dyn salsa::Database,
    nodes: &mitki_hir::hir::NodeStore<'db>,
    source_map: &mitki_lower::hir::FunctionSourceMap,
    inference: &mitki_typeck::infer::Inference<'db>,
    expr: ExprId,
    range: TextRange,
    hints: &mut Vec<InlayHint>,
) {
    match nodes.node_kind(expr) {
        NodeKind::Tuple => {
            let Some(tuple_id) = nodes.as_tuple(expr) else { return };
            for item in nodes.tuple(tuple_id).iter() {
                collect_binding_hints(db, nodes, source_map, inference, item, range, hints);
            }
        }
        NodeKind::Block => {
            let Some(block_id) = nodes.as_block(expr) else { return };
            let (stmts, tail) = nodes.block_stmts(block_id);
            for stmt in stmts.iter() {
                if nodes.node_kind(stmt) == NodeKind::LocalVar {
                    let Some(var_id) = nodes.as_local_var(stmt) else { continue };
                    let var = nodes.local_var(var_id);
                    if var.ty == TyId::ZERO {
                        for name in nodes.pattern_binding_names(var.pattern) {
                            push_binding_hint(
                                db,
                                source_map,
                                inference,
                                name.into(),
                                true,
                                range,
                                hints,
                            );
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
                } else if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    collect_binding_hints(db, nodes, source_map, inference, expr, range, hints);
                }
            }
            if tail != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, tail, range, hints);
            }
        }
        NodeKind::Call => {
            let Some(call_id) = nodes.as_call(expr) else { return };
            let (callee, args) = nodes.call(call_id);
            if callee != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, callee, range, hints);
            }
            for arg in args.iter() {
                collect_binding_hints(db, nodes, source_map, inference, arg, range, hints);
            }
        }
        NodeKind::Binary => {
            let Some(binary_id) = nodes.as_binary(expr) else { return };
            let binary = nodes.binary(binary_id);
            collect_binding_hints(db, nodes, source_map, inference, binary.lhs, range, hints);
            collect_binding_hints(db, nodes, source_map, inference, binary.rhs, range, hints);
        }
        NodeKind::Postfix => {
            let Some(postfix_id) = nodes.as_postfix(expr) else { return };
            let postfix = nodes.postfix(postfix_id);
            collect_binding_hints(db, nodes, source_map, inference, postfix.expr, range, hints);
        }
        NodeKind::Prefix => {
            let Some(prefix_id) = nodes.as_prefix(expr) else { return };
            let prefix = nodes.prefix(prefix_id);
            collect_binding_hints(db, nodes, source_map, inference, prefix.expr, range, hints);
        }
        NodeKind::LoopExpr => {
            let Some(loop_id) = nodes.as_loop_expr(expr) else { return };
            let (body, _) = nodes.loop_expr(loop_id);
            if body != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, body, range, hints);
            }
        }
        NodeKind::If => {
            let Some(if_id) = nodes.as_if(expr) else { return };
            let if_expr = nodes.if_expr(if_id);
            collect_binding_hints(db, nodes, source_map, inference, if_expr.cond, range, hints);
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
        NodeKind::Match => {
            let Some(match_id) = nodes.as_match(expr) else { return };
            let (scrutinee, arms) = nodes.match_expr(match_id);
            collect_binding_hints(db, nodes, source_map, inference, scrutinee, range, hints);
            for arm in arms.iter() {
                let Some(arm_id) = nodes.as_match_arm(arm) else { continue };
                let (pattern, body) = nodes.match_arm(arm_id);
                for name in nodes.pattern_binding_names(pattern) {
                    push_binding_hint(db, source_map, inference, name.into(), false, range, hints);
                }
                collect_binding_hints(db, nodes, source_map, inference, body, range, hints);
            }
        }
        NodeKind::Closure => {
            let Some(closure_id) = nodes.as_closure(expr) else { return };
            let (params, body) = nodes.closure_parts(closure_id);
            for param in params.iter() {
                let (pattern, ty_id) = nodes.param(param);
                if ty_id != TyId::ZERO {
                    continue;
                }
                for name in nodes.pattern_binding_names(pattern) {
                    push_binding_hint(db, source_map, inference, name.into(), false, range, hints);
                }
            }
            if body != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, body, range, hints);
            }
        }
        NodeKind::Array => {
            let Some(array_id) = nodes.as_array(expr) else { return };
            for item in nodes.array(array_id).iter() {
                collect_binding_hints(db, nodes, source_map, inference, item, range, hints);
            }
        }
        NodeKind::ArrayRepeat => {
            let Some(array_repeat_id) = nodes.as_array_repeat(expr) else { return };
            let (value, len) = nodes.array_repeat(array_repeat_id);
            collect_binding_hints(db, nodes, source_map, inference, value, range, hints);
            collect_binding_hints(db, nodes, source_map, inference, len, range, hints);
        }
        NodeKind::Field => {
            let Some(field_id) = nodes.as_field(expr) else { return };
            let (base, _) = nodes.field(field_id);
            if base != ExprId::ZERO {
                collect_binding_hints(db, nodes, source_map, inference, base, range, hints);
            }
        }
        NodeKind::StructExpr => {
            let Some(struct_id) = nodes.as_struct_expr(expr) else { return };
            let items = nodes.struct_expr(struct_id);
            let has_struct_name = items.len() % 2 == 1;
            let mut index = if has_struct_name { 2 } else { 1 };
            while index < items.len() {
                collect_binding_hints(
                    db,
                    nodes,
                    source_map,
                    inference,
                    items.get(index).unwrap(),
                    range,
                    hints,
                );
                index += 2;
            }
        }
        _ => {}
    }
}

fn push_binding_hint<'db>(
    db: &'db dyn salsa::Database,
    source_map: &mitki_lower::hir::FunctionSourceMap,
    inference: &mitki_typeck::infer::Inference<'db>,
    binding: ExprId,
    allow_unit: bool,
    range: TextRange,
    hints: &mut Vec<InlayHint>,
) {
    let Some(ty) = inference.type_of_node(binding) else {
        return;
    };
    if matches!(ty.kind(db), TyKind::Unknown)
        || (!allow_unit && matches!(ty.kind(db), TyKind::Tuple(items) if items.is_empty()))
    {
        return;
    }
    let ptr = source_map.node_syntax(binding);
    if !range.contains_range(ptr.range) {
        return;
    }
    hints.push(InlayHint { offset: ptr.range.end(), label: format!(": {}", ty.display(db)) });
}

fn stmt_as_expr(nodes: &mitki_hir::hir::NodeStore<'_>, stmt: StmtId) -> Option<ExprId> {
    match nodes.node_kind(stmt) {
        NodeKind::Name => nodes.as_name(stmt).map(Into::into),
        NodeKind::True => nodes.as_true(stmt).map(Into::into),
        NodeKind::False => nodes.as_false(stmt).map(Into::into),
        NodeKind::Error => nodes.as_error(stmt).map(Into::into),
        NodeKind::Int => nodes.as_int(stmt).map(Into::into),
        NodeKind::Float => nodes.as_float(stmt).map(Into::into),
        NodeKind::String => nodes.as_string(stmt).map(Into::into),
        NodeKind::Char => nodes.as_char(stmt).map(Into::into),
        NodeKind::Tuple => nodes.as_tuple(stmt).map(Into::into),
        NodeKind::Array => nodes.as_array(stmt).map(Into::into),
        NodeKind::ArrayRepeat => nodes.as_array_repeat(stmt).map(Into::into),
        NodeKind::Call => nodes.as_call(stmt).map(Into::into),
        NodeKind::Field => nodes.as_field(stmt).map(Into::into),
        NodeKind::Binary => nodes.as_binary(stmt).map(Into::into),
        NodeKind::Postfix => nodes.as_postfix(stmt).map(Into::into),
        NodeKind::Prefix => nodes.as_prefix(stmt).map(Into::into),
        NodeKind::LoopExpr => nodes.as_loop_expr(stmt).map(Into::into),
        NodeKind::BreakExpr => nodes.as_break_expr(stmt).map(Into::into),
        NodeKind::ContinueExpr => nodes.as_continue_expr(stmt).map(Into::into),
        NodeKind::If => nodes.as_if(stmt).map(Into::into),
        NodeKind::Match => nodes.as_match(stmt).map(Into::into),
        NodeKind::Closure => nodes.as_closure(stmt).map(Into::into),
        NodeKind::Block => nodes.as_block(stmt).map(Into::into),
        NodeKind::StructExpr => nodes.as_struct_expr(stmt).map(Into::into),
        _ => None,
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

    #[test]
    fn incomplete_tuple_pattern_does_not_panic() {
        check(
            r#"
fun main() {
    val (x, ) = (1, 2)
}
"#,
            &[],
        );
    }
}

pub mod semantics;

use mitki_errors::Diagnostic;
use mitki_lower::item::scope::Declaration;
use mitki_parse::FileParse as _;
pub use semantics::Semantics;

#[salsa::tracked(returns(ref), no_eq)]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) -> Vec<Diagnostic> {
    use mitki_hir::ty::TyKind;
    use mitki_lower::hir::HasFunction as _;
    use mitki_lower::item::scope::HasItemScope as _;
    use mitki_typeck::infer;
    use mitki_typeck::infer::Inferable as _;
    use mitki_yellow::ast::Node as _;

    let mut diagnostics = file.parse(db).diagnostics().to_owned();

    for declaration in file.item_scope(db).declarations() {
        match declaration {
            Declaration::Function(func) => {
                let source_map = func.hir_function(db).source_map(db);
                let function = func.hir_function(db).function(db);
                let nodes = function.node_store();
                let fallback_range =
                    mitki_yellow::SyntaxNodePtr::new(func.source(db).syntax()).range;

                let node_range = |node_id| {
                    source_map.try_node_syntax(node_id).map_or(fallback_range, |ptr| ptr.range)
                };
                let type_range = |ty_id| {
                    source_map.try_type_syntax(ty_id).map_or(fallback_range, |ptr| ptr.range)
                };

                let context_label = |node_id| match nodes.node_kind(node_id) {
                    mitki_hir::hir::NodeKind::Call => "call expression",
                    mitki_hir::hir::NodeKind::Closure => "closure",
                    mitki_hir::hir::NodeKind::If => "if expression",
                    mitki_hir::hir::NodeKind::Tuple => "tuple expression",
                    mitki_hir::hir::NodeKind::StructExpr => "struct expression",
                    _ => "expression",
                };

                let inference = func.infer(db);

                for diagnostic in inference.diagnostics() {
                    let (message, range) =
                        salsa::plumbing::attach(db, || match diagnostic.kind() {
                            infer::DiagnosticKind::UnresolvedIdent(node_id) => {
                                ("Unresolved identifier".to_owned(), node_range(*node_id))
                            }
                            infer::DiagnosticKind::UnresolvedType(ty_id, name) => {
                                (format!("Unknown type `{}`", name.text(db)), type_range(*ty_id))
                            }
                            infer::DiagnosticKind::TypeMismatch(node_id, actual, expected) => (
                                format!(
                                    "expected `{expected}`, found `{actual}`",
                                    expected = expected.display(db),
                                    actual = actual.display(db)
                                ),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::UnknownType(node_id) => {
                                ("cannot infer type".to_owned(), node_range(*node_id))
                            }
                            infer::DiagnosticKind::ExpectedValueFoundType(node_id, ty) => (
                                format!("expected value, found type `{}`", ty.display(db)),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::CallArityMismatch(node_id, expected, actual) => {
                                (
                                    format!("expected {expected} argument(s), found {actual}"),
                                    node_range(*node_id),
                                )
                            }
                            infer::DiagnosticKind::CallNonFunction(node_id, ty) => (
                                format!("expected function, found `{}`", ty.display(db)),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::ClosureArityMismatch(
                                node_id,
                                actual,
                                expected,
                            ) => (
                                format!(
                                    "expected {expected} parameter(s), found {actual}",
                                    expected = expected,
                                    actual = actual
                                ),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::InvalidBinaryOp(node_id, op, lhs_ty, rhs_ty) => {
                                (
                                    format!(
                                        "cannot apply `{}` to `{}` and `{}`",
                                        op.text(db),
                                        lhs_ty.display(db),
                                        rhs_ty.display(db)
                                    ),
                                    node_range(*node_id),
                                )
                            }
                            infer::DiagnosticKind::InvalidPrefixOp(node_id, op, ty) => (
                                format!("cannot apply `{}` to `{}`", op.text(db), ty.display(db)),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::InvalidPostfixOp(node_id, op, ty) => (
                                format!(
                                    "cannot apply postfix `{}` to `{}`",
                                    op.text(db),
                                    ty.display(db)
                                ),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::MissingElseBranch(node_id) => {
                                ("missing `else` branch".to_owned(), node_range(*node_id))
                            }
                            infer::DiagnosticKind::MissingParameterType(node_id) => {
                                let message = inference.type_of_node(*node_id).map_or_else(
                                    || "Parameter type annotation is required.".to_owned(),
                                    |inferred| {
                                        let inferred_kind = inferred.kind(db);
                                        let is_unhelpful_hint =
                                            matches!(inferred_kind, TyKind::Unknown)
                                                || matches!(
                                                    inferred_kind,
                                                    TyKind::Tuple(items) if items.is_empty()
                                                );

                                        if is_unhelpful_hint {
                                            "Parameter type annotation is required.".to_owned()
                                        } else {
                                            format!(
                                                "Parameter type annotation is required. Inferred \
                                                 `{}`.",
                                                inferred.display(db)
                                            )
                                        }
                                    },
                                );
                                (message, node_range(*node_id))
                            }
                            infer::DiagnosticKind::MissingInitializer(node_id) => {
                                ("missing initializer".to_owned(), node_range(*node_id))
                            }
                            infer::DiagnosticKind::TupleArityMismatch(
                                node_id,
                                expected,
                                actual,
                            ) => (
                                format!("expected {expected} element(s), found {actual}"),
                                node_range(*node_id),
                            ),
                            infer::DiagnosticKind::MissingStructField(node_id, name) => {
                                (format!("missing field `{}`", name.text(db)), node_range(*node_id))
                            }
                            infer::DiagnosticKind::UnknownStructField(node_id, name) => {
                                (format!("unknown field `{}`", name.text(db)), node_range(*node_id))
                            }
                            infer::DiagnosticKind::NotAStruct(node_id, ty) => (
                                format!("`{}` is not a struct", ty.display(db)),
                                node_range(*node_id),
                            ),
                        });

                    let message = if let Some(context) = diagnostic.context() {
                        format!("In {}: {message}", context_label(context))
                    } else {
                        message
                    };

                    diagnostics.push(Diagnostic::error(message, range))
                }
            }
            Declaration::Struct(_) | Declaration::Enum(_) => {
                // No body to type-check for struct/enum declarations.
            }
        }
    }

    diagnostics
}

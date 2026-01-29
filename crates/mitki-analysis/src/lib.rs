pub mod semantics;

use mitki_errors::Diagnostic;
use mitki_lower::item::scope::Declaration;
use mitki_parse::FileParse as _;
pub use semantics::Semantics;

#[salsa::tracked(returns(ref), no_eq)]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) -> Vec<Diagnostic> {
    use mitki_lower::hir::HasFunction as _;
    use mitki_lower::item::scope::HasItemScope as _;
    use mitki_typeck::infer;
    use mitki_typeck::infer::Inferable as _;

    let mut diagnostics = file.parse(db).diagnostics().to_owned();

    for declaration in file.item_scope(db).declarations() {
        match declaration {
            Declaration::Function(func) => {
                let source_map = func.hir_function(db).source_map(db);
                let function = func.hir_function(db).function(db);
                let nodes = function.node_store();

                let context_label = |node_id| match nodes.node_kind(node_id) {
                    mitki_hir::hir::NodeKind::Call => "call expression",
                    mitki_hir::hir::NodeKind::Closure => "closure",
                    mitki_hir::hir::NodeKind::If => "if expression",
                    mitki_hir::hir::NodeKind::Tuple => "tuple expression",
                    _ => "expression",
                };

                for diagnostic in func.infer(db).diagnostics() {
                    let (message, range) =
                        salsa::plumbing::attach(db, || match diagnostic.kind() {
                            infer::DiagnosticKind::UnresolvedIdent(node_id) => (
                                "Unresolved identifier".to_owned(),
                                source_map.node_syntax(*node_id).range,
                            ),
                            infer::DiagnosticKind::UnresolvedType(ty_id, name) => (
                                format!("Unknown type `{}`", name.text(db)),
                                source_map.type_syntax(*ty_id).range,
                            ),
                            infer::DiagnosticKind::TypeMismatch(node_id, actual, expected) => (
                                format!(
                                    "expected `{expected}`, found `{actual}`",
                                    expected = expected.display(db),
                                    actual = actual.display(db)
                                ),
                                source_map.node_syntax(*node_id).range,
                            ),
                            infer::DiagnosticKind::ExpectedValueFoundType(node_id, ty) => (
                                format!("expected value, found type `{}`", ty.display(db)),
                                source_map.node_syntax(*node_id).range,
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
                                source_map.node_syntax(*node_id).range,
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
        }
    }

    diagnostics
}

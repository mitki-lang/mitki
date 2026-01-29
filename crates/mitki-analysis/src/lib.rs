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

                for diagnostic in func.infer(db).diagnostics() {
                    let (message, range) = salsa::plumbing::attach(db, || match diagnostic {
                        infer::Diagnostic::UnresolvedIdent(node_id) => (
                            "Unresolved identifier".to_owned(),
                            source_map.node_syntax(*node_id).range,
                        ),
                        infer::Diagnostic::TypeMismatch(node_id, actual, expected) => (
                            format!(
                                "expected `{expected}`, found `{actual}`",
                                expected = expected.display(db),
                                actual = actual.display(db)
                            ),
                            source_map.node_syntax(*node_id).range,
                        ),
                        infer::Diagnostic::ExpectedValueFoundType(node_id, ty) => (
                            format!("expected value, found type `{}`", ty.display(db)),
                            source_map.node_syntax(*node_id).range,
                        ),
                    });

                    diagnostics.push(Diagnostic::error(message, range))
                }
            }
        }
    }

    diagnostics
}

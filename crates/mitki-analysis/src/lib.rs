mod arena;
pub mod ast_map;
pub mod hir;
pub mod infer;
pub mod item;
pub mod resolver;
pub mod semantics;
pub mod ty;

use item::scope::Declaration;
use mitki_errors::Diagnostic;
use mitki_parse::FileParse as _;
pub use semantics::Semantics;

#[salsa::tracked(return_ref, no_eq)]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) -> Vec<Diagnostic> {
    use hir::HasFunction as _;
    use infer::Inferable as _;
    use item::scope::HasItemScope as _;

    let mut diagnostics = file.parse(db).diagnostics().to_owned();

    for declaration in file.item_scope(db).declarations() {
        match declaration {
            Declaration::Function(func) => {
                let source_map = func.hir_function(db).source_map(db);

                for diagnostic in func.infer(db).diagnostics() {
                    let (message, range) = salsa::plumbing::attach(db, || match diagnostic {
                        infer::Diagnostic::UnresolvedIdent(node_id) => (
                            "Unresolved identifier".to_owned(),
                            source_map.node_syntax(node_id).range,
                        ),
                        infer::Diagnostic::TypeMismatch(node_id, actual, expected) => (
                            format!("expected `{expected}`, found `{actual}`"),
                            source_map.node_syntax(node_id).range,
                        ),
                    });

                    diagnostics.push(Diagnostic::error(message, range))
                }
            }
        }
    }

    diagnostics
}

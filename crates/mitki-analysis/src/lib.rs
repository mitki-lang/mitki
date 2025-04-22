mod arena;
pub mod ast_map;
pub mod hir;
pub mod infer;
pub mod item;
pub mod resolver;
pub mod semantics;
pub mod ty;

use item::scope::Declaration;
pub use semantics::Semantics;

#[salsa::tracked]
pub fn check_file(db: &dyn salsa::Database, file: mitki_inputs::File) {
    use hir::HasFunction as _;
    use infer::Inferable as _;
    use item::scope::HasItemScope as _;
    use salsa::Accumulator as _;

    for declaration in file.item_scope(db).declarations() {
        match declaration {
            Declaration::Function(func) => {
                let source_map = func.hir_source_map(db);

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

                    mitki_errors::Diagnostic::error(message, range).accumulate(db);
                }
            }
        }
    }
}

pub use mitki_analysis::check_file;
pub use mitki_errors::{Diagnostic, Level};

#[picante::db(
    inputs(mitki_inputs::SourceFile),
    interned(mitki_span::Symbol, mitki_hir::ty::TyData),
    tracked(
        mitki_inputs::line_index,
        mitki_parse::parse_file,
        mitki_parse::parse,
        mitki_lower::ast_map::ast_map,
        mitki_lower::item::tree::item_tree,
        mitki_lower::item::scope::item_scope,
        mitki_lower::item::scope::signature,
        mitki_lower::item::scope::signature_map,
        mitki_lower::hir::hir_function,
        mitki_resolve::scope::expr_scopes,
        mitki_resolve::resolver::builtin_scope,
        mitki_typeck::infer::infer,
        mitki_analysis::check_file
    ),
    db_trait(Database)
)]
pub struct RootDatabase {}

impl Default for RootDatabase {
    fn default() -> Self {
        Self::new()
    }
}

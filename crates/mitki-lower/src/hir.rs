mod function;
pub use function::FunctionSourceMap;
use mitki_hir::hir::Function;

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionWithSourceMap<'db> {
    function: Function<'db>,
    source_map: FunctionSourceMap,
}

impl<'db> FunctionWithSourceMap<'db> {
    pub fn new(function: Function<'db>, source_map: FunctionSourceMap) -> Self {
        Self { function, source_map }
    }

    pub fn function(&self) -> &Function<'db> {
        &self.function
    }

    pub fn source_map(&self) -> &FunctionSourceMap {
        &self.source_map
    }
}

pub trait HasFunction<'db> {
    fn hir_function<DB>(self, db: &'db DB) -> FunctionWithSourceMap<'db>
    where
        DB: mitki_parse::ParseDb;
}

impl<'db> HasFunction<'db> for crate::item::scope::FunctionLocation<'db> {
    fn hir_function<DB>(self, db: &'db DB) -> FunctionWithSourceMap<'db>
    where
        DB: mitki_parse::ParseDb,
    {
        use mitki_parse::FileParse as _;
        use mitki_yellow::ast::{self, Node as _};

        let file = self.file(db);
        let parsed = file.parse(db);
        let syntax = self.source_ptr(db).to_node(&parsed.syntax_node());
        let function = ast::Function::cast(syntax).unwrap();

        function::FunctionBuilder::new(db).build(&function)
    }
}

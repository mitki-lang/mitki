use mitki_hir::ty::{Ty, TyKind};
use mitki_span::Symbol;
use salsa::Database;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuntimeTy {
    Int,
    Str,
    Unit,
}

impl RuntimeTy {
    pub fn display(self) -> &'static str {
        match self {
            Self::Int => "int",
            Self::Str => "str",
            Self::Unit => "()",
        }
    }

    pub fn as_ty<'db>(self, db: &'db dyn Database) -> Ty<'db> {
        match self {
            Self::Int => Ty::new(db, TyKind::Int),
            Self::Str => Ty::new(db, TyKind::String),
            Self::Unit => Ty::new(db, TyKind::Tuple(Vec::new())),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum RuntimeFunction {
    PrintI32,
    PrintStr,
    Alloc,
    Dealloc,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RuntimeFunctionInfo {
    pub function: RuntimeFunction,
    pub visible_in_source: bool,
    pub source_name: &'static str,
    pub import_module: &'static str,
    pub import_name: &'static str,
    pub params: &'static [RuntimeTy],
    pub result: RuntimeTy,
}

const PRINT_I32_PARAMS: [RuntimeTy; 1] = [RuntimeTy::Int];
const PRINT_STR_PARAMS: [RuntimeTy; 1] = [RuntimeTy::Str];
const ALLOC_PARAMS: [RuntimeTy; 2] = [RuntimeTy::Int, RuntimeTy::Int];
const DEALLOC_PARAMS: [RuntimeTy; 3] = [RuntimeTy::Int, RuntimeTy::Int, RuntimeTy::Int];

const RUNTIME_FUNCTIONS: [RuntimeFunctionInfo; 4] = [
    RuntimeFunctionInfo {
        function: RuntimeFunction::PrintI32,
        visible_in_source: false,
        source_name: "std::io::print_int",
        import_module: "mitki",
        import_name: "print_i32",
        params: &PRINT_I32_PARAMS,
        result: RuntimeTy::Unit,
    },
    RuntimeFunctionInfo {
        function: RuntimeFunction::PrintStr,
        visible_in_source: true,
        source_name: "std::io::print_str",
        import_module: "mitki",
        import_name: "print_str",
        params: &PRINT_STR_PARAMS,
        result: RuntimeTy::Unit,
    },
    RuntimeFunctionInfo {
        function: RuntimeFunction::Alloc,
        visible_in_source: false,
        source_name: "alloc",
        import_module: "mitki",
        import_name: "alloc",
        params: &ALLOC_PARAMS,
        result: RuntimeTy::Int,
    },
    RuntimeFunctionInfo {
        function: RuntimeFunction::Dealloc,
        visible_in_source: false,
        source_name: "dealloc",
        import_module: "mitki",
        import_name: "dealloc",
        params: &DEALLOC_PARAMS,
        result: RuntimeTy::Unit,
    },
];

impl RuntimeFunction {
    pub fn info(self) -> &'static RuntimeFunctionInfo {
        RUNTIME_FUNCTIONS
            .iter()
            .find(|info| info.function == self)
            .expect("runtime function metadata should exist")
    }

    pub fn source_name(self) -> &'static str {
        self.info().source_name
    }

    pub fn import_module(self) -> &'static str {
        self.info().import_module
    }

    pub fn import_name(self) -> &'static str {
        self.info().import_name
    }

    pub fn params(self) -> &'static [RuntimeTy] {
        self.info().params
    }

    pub fn result(self) -> RuntimeTy {
        self.info().result
    }

    pub fn function_ty<'db>(self, db: &'db dyn Database) -> Ty<'db> {
        let inputs = self.params().iter().map(|ty| ty.as_ty(db)).collect();
        let output = self.result().as_ty(db);
        Ty::new(db, TyKind::Function { inputs, output })
    }

    pub fn hover_text(self) -> String {
        let params = self.params().iter().map(|ty| ty.display()).collect::<Vec<_>>().join(", ");
        format!(
            "```mitki\nfun {}: fun({params}) -> {}\n```",
            self.source_name(),
            self.result().display()
        )
    }
}

pub fn runtime_functions() -> &'static [RuntimeFunctionInfo] {
    &RUNTIME_FUNCTIONS
}

pub fn lookup_runtime_function(db: &dyn Database, symbol: Symbol<'_>) -> Option<RuntimeFunction> {
    let text = symbol.text(db);
    runtime_functions()
        .iter()
        .find(|info| info.visible_in_source && info.source_name == text)
        .map(|info| info.function)
}

pub fn is_reserved_runtime_name(db: &dyn Database, symbol: Symbol<'_>) -> bool {
    lookup_runtime_function(db, symbol).is_some()
}

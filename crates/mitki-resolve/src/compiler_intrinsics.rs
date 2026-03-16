use mitki_span::Symbol;
use salsa::Database;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CompilerIntrinsic {
    Comptime,
    TypeName,
    FieldCount,
    FieldName,
    VariantCount,
    VariantName,
    FunctionParamCount,
    FunctionParamTypeName,
    FunctionReturnTypeName,
    StackAlloc,
    PtrRead,
    PtrWrite,
    PtrAdd,
    StrBytes,
    StrFromUtf8Unchecked,
    ArrayMutBytes,
}

const COMPILER_INTRINSICS: [(CompilerIntrinsic, &str); 16] = [
    (CompilerIntrinsic::Comptime, "comptime"),
    (CompilerIntrinsic::TypeName, "type_name"),
    (CompilerIntrinsic::FieldCount, "field_count"),
    (CompilerIntrinsic::FieldName, "field_name"),
    (CompilerIntrinsic::VariantCount, "variant_count"),
    (CompilerIntrinsic::VariantName, "variant_name"),
    (CompilerIntrinsic::FunctionParamCount, "function_param_count"),
    (CompilerIntrinsic::FunctionParamTypeName, "function_param_type_name"),
    (CompilerIntrinsic::FunctionReturnTypeName, "function_return_type_name"),
    (CompilerIntrinsic::StackAlloc, "stack_alloc"),
    (CompilerIntrinsic::PtrRead, "ptr_read"),
    (CompilerIntrinsic::PtrWrite, "ptr_write"),
    (CompilerIntrinsic::PtrAdd, "ptr_add"),
    (CompilerIntrinsic::StrBytes, "str_bytes"),
    (CompilerIntrinsic::StrFromUtf8Unchecked, "str_from_utf8_unchecked"),
    (CompilerIntrinsic::ArrayMutBytes, "array_mut_bytes"),
];

impl CompilerIntrinsic {
    pub fn source_name(self) -> &'static str {
        COMPILER_INTRINSICS
            .iter()
            .find(|(intrinsic, _)| *intrinsic == self)
            .map(|(_, name)| *name)
            .expect("compiler intrinsic metadata should exist")
    }

    pub fn is_reflection(self) -> bool {
        matches!(
            self,
            Self::TypeName
                | Self::FieldCount
                | Self::FieldName
                | Self::VariantCount
                | Self::VariantName
                | Self::FunctionParamCount
                | Self::FunctionParamTypeName
                | Self::FunctionReturnTypeName
        )
    }

    pub fn requires_unsafe(self) -> bool {
        matches!(
            self,
            Self::StackAlloc
                | Self::PtrRead
                | Self::PtrWrite
                | Self::PtrAdd
                | Self::StrFromUtf8Unchecked
                | Self::ArrayMutBytes
        )
    }
}

pub fn compiler_intrinsics() -> impl Iterator<Item = CompilerIntrinsic> {
    COMPILER_INTRINSICS.into_iter().map(|(intrinsic, _)| intrinsic)
}

pub fn lookup_compiler_intrinsic(
    db: &dyn Database,
    symbol: Symbol<'_>,
) -> Option<CompilerIntrinsic> {
    let text = symbol.text(db);
    COMPILER_INTRINSICS.iter().find(|(_, name)| *name == text).map(|(intrinsic, _)| *intrinsic)
}

pub fn is_reserved_compiler_name(db: &dyn Database, symbol: Symbol<'_>) -> bool {
    lookup_compiler_intrinsic(db, symbol).is_some()
}

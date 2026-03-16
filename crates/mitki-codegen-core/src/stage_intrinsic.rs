use mitki_resolve::CompilerIntrinsic;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum StageIntrinsic {
    TypeName,
    FieldCount,
    FieldName,
    VariantCount,
    VariantName,
    FunctionParamCount,
    FunctionParamTypeName,
    FunctionReturnTypeName,
}

impl StageIntrinsic {
    pub fn all() -> [Self; 8] {
        [
            Self::TypeName,
            Self::FieldCount,
            Self::FieldName,
            Self::VariantCount,
            Self::VariantName,
            Self::FunctionParamCount,
            Self::FunctionParamTypeName,
            Self::FunctionReturnTypeName,
        ]
    }

    pub fn module() -> &'static str {
        "mitki_stage"
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::TypeName => "type_name",
            Self::FieldCount => "field_count",
            Self::FieldName => "field_name",
            Self::VariantCount => "variant_count",
            Self::VariantName => "variant_name",
            Self::FunctionParamCount => "function_param_count",
            Self::FunctionParamTypeName => "function_param_type_name",
            Self::FunctionReturnTypeName => "function_return_type_name",
        }
    }

    pub fn from_compiler_intrinsic(intrinsic: CompilerIntrinsic) -> Option<Self> {
        match intrinsic {
            CompilerIntrinsic::Comptime => None,
            CompilerIntrinsic::TypeName => Some(Self::TypeName),
            CompilerIntrinsic::FieldCount => Some(Self::FieldCount),
            CompilerIntrinsic::FieldName => Some(Self::FieldName),
            CompilerIntrinsic::VariantCount => Some(Self::VariantCount),
            CompilerIntrinsic::VariantName => Some(Self::VariantName),
            CompilerIntrinsic::FunctionParamCount => Some(Self::FunctionParamCount),
            CompilerIntrinsic::FunctionParamTypeName => Some(Self::FunctionParamTypeName),
            CompilerIntrinsic::FunctionReturnTypeName => Some(Self::FunctionReturnTypeName),
            CompilerIntrinsic::StackAlloc
            | CompilerIntrinsic::PtrRead
            | CompilerIntrinsic::PtrWrite
            | CompilerIntrinsic::PtrAdd
            | CompilerIntrinsic::StrBytes
            | CompilerIntrinsic::StrFromUtf8Unchecked
            | CompilerIntrinsic::ArrayMutBytes => None,
        }
    }
}

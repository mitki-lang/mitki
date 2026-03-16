use mitki_abi::TransportClass;
use mitki_hir::hir::{ExprId, NameId, ParamId};
use mitki_hir::ty::Ty;
use mitki_lower::item::scope::FunctionLocation;
use mitki_resolve::RuntimeFunction;
use mitki_span::Symbol;

use super::{AbiTy, AggregateLayout, FieldLayout, FunctionSignature, StageIntrinsic};
use crate::layout::VariantLayout;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum HelperFunction {
    MemoryEq,
    StringEq,
    ArcRetain,
    ArcRelease,
}

impl HelperFunction {
    pub(super) fn all() -> [Self; 4] {
        [Self::MemoryEq, Self::StringEq, Self::ArcRetain, Self::ArcRelease]
    }

    pub(super) fn name(self) -> &'static str {
        match self {
            Self::MemoryEq => "memory_eq",
            Self::StringEq => "string_eq",
            Self::ArcRetain => "arc_retain",
            Self::ArcRelease => "arc_release",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) enum ValueOwnership {
    None,
    Borrowed,
    Owned,
}

impl ValueOwnership {
    pub(super) fn is_owned(self) -> bool {
        matches!(self, Self::Owned)
    }

    pub(super) fn is_borrowed(self) -> bool {
        matches!(self, Self::Borrowed)
    }
}

#[derive(Clone, Copy)]
pub(super) enum ExprPosition {
    Value,
}

#[derive(Clone, Debug)]
pub(super) struct BoundaryFunctionSignatures<'db> {
    pub(super) param_tys: Vec<Ty<'db>>,
    pub(super) result_ty: Ty<'db>,
    pub(super) param_transport_plans: Vec<BoundaryTransportPlan>,
    pub(super) result_transport_plan: BoundaryTransportPlan,
    pub(super) param_runtime_abis: Vec<AbiTy>,
    pub(super) result_runtime_abi: AbiTy,
}

impl BoundaryFunctionSignatures<'_> {
    pub(super) fn internal_signature(&self) -> FunctionSignature {
        FunctionSignature {
            params: self.param_runtime_abis.clone(),
            result: self.result_runtime_abi.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct BoundaryTransportPlan {
    pub(crate) transport_class: TransportClass,
}

#[derive(Clone, Debug)]
pub(super) enum BackendPattern {
    Binding(NameId),
    Wildcard,
    Literal(BackendPatternLiteral),
    Tuple(Vec<BackendPatternField>),
    Struct(Vec<BackendPatternField>),
    Variant { variant: VariantLayout, fields: Vec<BackendPatternField> },
}

impl BackendPattern {
    pub(super) fn binding_names(&self, names: &mut Vec<NameId>) {
        match self {
            Self::Binding(name) => names.push(*name),
            Self::Tuple(fields) | Self::Struct(fields) => {
                for field in fields {
                    field.pattern.binding_names(names);
                }
            }
            Self::Variant { fields, .. } => {
                for field in fields {
                    field.pattern.binding_names(names);
                }
            }
            Self::Wildcard | Self::Literal(_) => {}
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct BackendPatternField {
    pub(super) field: FieldLayout,
    pub(super) pattern: BackendPattern,
}

#[derive(Clone, Debug)]
pub(super) enum BackendPatternLiteral {
    Bool(bool),
    Int(i64),
    String(u32),
    Char(char),
}

#[derive(Clone, Debug)]
pub(super) struct BackendCallTarget<'db> {
    pub(super) callable: BackendCallable<'db>,
    pub(super) signature: FunctionSignature,
}

#[derive(Clone, Debug)]
pub(super) enum BackendCallable<'db> {
    Runtime(RuntimeFunction),
    StageIntrinsic(StageIntrinsic),
    Function(InstanceKey<'db>),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum FunctionValueTarget<'db> {
    Function(InstanceKey<'db>),
    Closure(ClosureInstanceKey<'db>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BackendBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    And,
    Or,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum BackendPrefixOp {
    Not,
    Neg,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct InstanceKey<'db> {
    pub(super) location: FunctionLocation<'db>,
    pub(super) type_args: Vec<Ty<'db>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct ClosureInstanceKey<'db> {
    pub(super) owner: FunctionLocation<'db>,
    pub(super) type_args: Vec<Ty<'db>>,
    pub(super) closure: ExprId,
}

impl<'db> ClosureInstanceKey<'db> {
    pub(super) fn owner_instance(&self) -> InstanceKey<'db> {
        InstanceKey { location: self.owner, type_args: self.type_args.clone() }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) enum ReachableInstance<'db> {
    Function(InstanceKey<'db>),
    Closure(ClosureInstanceKey<'db>),
}

impl<'db> ReachableInstance<'db> {
    pub(super) fn owner_location(&self) -> FunctionLocation<'db> {
        match self {
            Self::Function(instance) => instance.location,
            Self::Closure(instance) => instance.owner,
        }
    }

    pub(super) fn owner_instance(&self) -> InstanceKey<'db> {
        match self {
            Self::Function(instance) => instance.clone(),
            Self::Closure(instance) => instance.owner_instance(),
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct ClosureCapture<'db> {
    pub(super) binding: NameId,
    pub(super) field: FieldLayout,
    pub(super) _marker: std::marker::PhantomData<&'db ()>,
}

#[derive(Clone, Debug)]
pub(super) struct PendingClosureCapture {
    pub(super) binding: NameId,
    pub(super) ty: AbiTy,
}

#[derive(Clone, Debug)]
pub(super) struct ClosureInfo<'db> {
    pub(super) body: ExprId,
    pub(super) params: Vec<ParamId>,
    pub(super) captures: Vec<ClosureCapture<'db>>,
    pub(super) env_layout: AggregateLayout,
    pub(super) signature: FunctionSignature,
}

#[derive(Clone, Debug)]
pub(super) struct ReachableClosureInfo<'db> {
    pub(super) closure: ClosureInstanceKey<'db>,
    pub(super) info: ClosureInfo<'db>,
}

#[derive(Clone, Debug)]
pub(super) struct ExportedFunction<'db> {
    pub(super) instance: InstanceKey<'db>,
    pub(super) name: Symbol<'db>,
}

pub(super) struct SignaturePattern<'db> {
    pub(super) params: Vec<Ty<'db>>,
    pub(super) result: Ty<'db>,
}

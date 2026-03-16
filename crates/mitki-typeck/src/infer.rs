use mitki_hir::hir::{ExprId, Function, NameId, NodeKind, NodeStore, PatId, StmtId, TyId};
use mitki_hir::ty::{EnumTy, ExactInt, StructTy, Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{
    FunctionLocation, HasVisibleItems as _, TypeDeclaration, enum_variants,
    instantiate_nominal_type, struct_fields,
};
use mitki_resolve::{BindingId, CompilerIntrinsic, Resolver, resolve_method_for_receiver};
use mitki_span::{IntoSymbol as _, Symbol};
use rustc_hash::{FxHashMap, FxHashSet};
use salsa::Database;
use salsa::plumbing::{AsId as _, FromId as _};

pub trait Inferable<'db> {
    fn infer(self, db: &'db dyn Database) -> &'db Inference<'db>;
}

fn comptime_result_supported(db: &dyn Database, ty: Ty<'_>) -> bool {
    fn inner(db: &dyn Database, ty: Ty<'_>, seen: &mut FxHashSet<u64>) -> bool {
        let bits = ty.as_id().as_bits();
        if !seen.insert(bits) {
            return false;
        }

        let supported = match ty.kind(db) {
            TyKind::Bool
            | TyKind::Float
            | TyKind::Int
            | TyKind::ExactInt(_)
            | TyKind::String
            | TyKind::Char => true,
            TyKind::Array(item) => inner(db, *item, seen),
            TyKind::Tuple(items) => items.iter().all(|&item| inner(db, item, seen)),
            TyKind::Record(fields) => fields.iter().all(|(_, field_ty)| inner(db, *field_ty, seen)),
            TyKind::Struct(struct_ty) => {
                !struct_has_destructor(db, *struct_ty)
                    && struct_fields(db, *struct_ty)
                        .iter()
                        .all(|(_, field_ty)| inner(db, *field_ty, seen))
            }
            TyKind::Enum(enum_ty) => {
                !enum_has_destructor(db, *enum_ty)
                    && enum_variants(db, *enum_ty)
                        .iter()
                        .flat_map(|(_, fields)| fields.iter())
                        .all(|&field_ty| inner(db, field_ty, seen))
            }
            TyKind::Unknown
            | TyKind::Function { .. }
            | TyKind::Pointer { .. }
            | TyKind::Var(_)
            | TyKind::Union(_)
            | TyKind::Inter(_)
            | TyKind::ExternStruct(_)
            | TyKind::Rec(_, _) => false,
        };

        seen.remove(&bits);
        supported
    }

    inner(db, ty, &mut FxHashSet::default())
}

fn struct_has_destructor(db: &dyn Database, struct_ty: StructTy<'_>) -> bool {
    let Some(TypeDeclaration::Struct(location)) =
        struct_ty.module(db).visible_items(db).get_type_declaration(&struct_ty.name(db))
    else {
        return false;
    };
    location.destructor(db).is_some()
}

fn enum_has_destructor(db: &dyn Database, enum_ty: EnumTy<'_>) -> bool {
    let Some(TypeDeclaration::Enum(location)) =
        enum_ty.module(db).visible_items(db).get_type_declaration(&enum_ty.name(db))
    else {
        return false;
    };
    location.destructor(db).is_some()
}

#[salsa::tracked]
impl<'db> Inferable<'db> for FunctionLocation<'db> {
    #[salsa::tracked(returns(ref))]
    fn infer(self, db: &'db dyn Database) -> Inference<'db> {
        let hir_function = self.hir_function(db);
        let function = hir_function.function(db);
        let source_map = hir_function.source_map(db);
        let resolver = Resolver::new(db, self);
        Typer::new(db, function, source_map, resolver).build()
    }
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct Inference<'db> {
    type_of_node: FxHashMap<ExprId, Ty<'db>>,
    selected_union_members: FxHashMap<PatId, Ty<'db>>,
    matched_typed_patterns: FxHashMap<PatId, Ty<'db>>,
    diagnostics: Vec<Diagnostic<'db>>,
}

impl<'db> Inference<'db> {
    pub fn type_of_node(&self, node: ExprId) -> Option<Ty<'db>> {
        self.type_of_node.get(&node).copied()
    }

    pub fn selected_union_member(&self, pattern: PatId) -> Option<Ty<'db>> {
        self.selected_union_members.get(&pattern).copied()
    }

    pub fn matched_typed_pattern(&self, pattern: PatId) -> Option<Ty<'db>> {
        self.matched_typed_patterns.get(&pattern).copied()
    }

    pub fn diagnostics(&self) -> &[Diagnostic<'db>] {
        &self.diagnostics
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub struct Diagnostic<'db> {
    kind: DiagnosticKind<'db>,
    context: Option<ExprId>,
}

impl<'db> Diagnostic<'db> {
    fn new(kind: DiagnosticKind<'db>, context: Option<ExprId>) -> Self {
        Self { kind, context }
    }

    pub fn kind(&self) -> &DiagnosticKind<'db> {
        &self.kind
    }

    pub fn context(&self) -> Option<ExprId> {
        self.context
    }
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
pub enum DiagnosticKind<'db> {
    UnresolvedIdent(ExprId),
    UnresolvedType(TyId, Symbol<'db>),
    TypeMismatch(ExprId, Ty<'db>, Ty<'db>),
    PatternTypeMismatch(PatId, Ty<'db>, Ty<'db>),
    UnknownType(ExprId),
    ExpectedValueFoundType(ExprId, Ty<'db>),
    CallArityMismatch(ExprId, usize, usize),
    CallNonFunction(ExprId, Ty<'db>),
    ClosureArityMismatch(ExprId, usize, usize),
    InvalidBinaryOp(ExprId, Symbol<'db>, Ty<'db>, Ty<'db>),
    InvalidPrefixOp(ExprId, Symbol<'db>, Ty<'db>),
    InvalidPostfixOp(ExprId, Symbol<'db>, Ty<'db>),
    MissingElseBranch(ExprId),
    MissingParameterType(ExprId),
    MissingInitializer(ExprId),
    TupleArityMismatch(ExprId, usize, usize),
    MissingStructField(ExprId, Symbol<'db>),
    UnknownStructField(ExprId, Symbol<'db>),
    NotAStruct(ExprId, Ty<'db>),
    NotAnEnum(ExprId, Ty<'db>),
    DuplicatePatternBinding(ExprId),
    AmbiguousUnionPattern(PatId, Ty<'db>),
    RefutablePattern(ExprId),
    UnsupportedFloatPattern(ExprId),
    BreakOutsideLoop(ExprId),
    ContinueOutsideLoop(ExprId),
    CompilerIntrinsicMustBeCalled(ExprId, CompilerIntrinsic),
    InvalidComptimeCall(ExprId),
    ComptimeTargetMustBeZeroArg(ExprId),
    ComptimeTargetMustNotBeGeneric(ExprId),
    ComptimeTargetMustReturnSupportedType(ExprId, Ty<'db>),
    ReflectionOnlyInComptime(ExprId, CompilerIntrinsic),
    InvalidReflectionTarget(ExprId, CompilerIntrinsic, &'static str),
    UnsafeOperationRequiresUnsafeContext(ExprId),
    InvalidUnsafeIntrinsicArgument(ExprId, CompilerIntrinsic, &'static str),
    InvalidUnsafeIntrinsicResult(ExprId, CompilerIntrinsic, &'static str),
    InvalidAssignmentTarget(ExprId),
    AssignmentRequiresMutable(ExprId),
    MutableArgumentRequiresPlace(ExprId),
}

type VarId = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum InferTy {
    Var(VarId),
    Function(Vec<InferTy>, Box<InferTy>),
    Array(Box<InferTy>),
    Tuple(Vec<InferTy>),
    Record(Vec<(u64, InferTy)>),
    Union(Vec<InferTy>),
    Inter(Vec<InferTy>),
    /// A solved/interned type leaf, stored as raw salsa Id bits.
    Known(u64),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NumericKind {
    Int,
    ExactInt(ExactInt),
    Float,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum OrderableKind {
    Int,
    ExactInt(ExactInt),
    Float,
    Char,
}

#[derive(Debug, Clone)]
enum Scheme {
    Mono(InferTy),
    Poly { level: usize, body: InferTy },
}

#[derive(Debug, Clone)]
struct VarState {
    level: usize,
    lower_bounds: Vec<InferTy>,
    upper_bounds: Vec<InferTy>,
}

#[derive(Debug, Clone)]
struct VariantConstraint<'db> {
    enum_var: VarId,
    variant: Symbol<'db>,
    payload: Vec<(ExprId, InferTy)>,
    name_node: ExprId,
}

#[derive(Debug, Clone)]
struct DeferredCoercion {
    node: ExprId,
    actual: InferTy,
    expected: InferTy,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Polarity {
    Positive,
    Negative,
}

impl Polarity {
    fn flip(self) -> Self {
        match self {
            Polarity::Positive => Polarity::Negative,
            Polarity::Negative => Polarity::Positive,
        }
    }
}

#[derive(Clone, Copy)]
enum PatternBindingScheme {
    Mono,
    Poly { level: usize },
}

enum UnionPatternSelection {
    None,
    Unique(InferTy),
    Ambiguous,
}

enum PlaceResolution {
    Mutable(InferTy),
    Immutable,
    Invalid,
}

struct Typer<'func, 'db> {
    db: &'db dyn Database,
    function: &'func Function<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    resolver: Resolver<'db>,
    vars: Vec<VarState>,
    env: FxHashMap<ExprId, Scheme>,
    binding_names: FxHashSet<ExprId>,
    type_param_env: FxHashMap<Symbol<'db>, InferTy>,
    // Internal inference representation. Do not expose directly to users/LSP.
    node_types: FxHashMap<ExprId, InferTy>,
    selected_union_members: FxHashMap<PatId, InferTy>,
    matched_typed_patterns: FxHashMap<PatId, InferTy>,
    variant_constraints: Vec<VariantConstraint<'db>>,
    deferred_coercions: Vec<DeferredCoercion>,
    missing_param_nodes: FxHashSet<ExprId>,
    preserved_type_vars: FxHashSet<u32>,
    return_ty: Option<InferTy>,
    inference: Inference<'db>,
    context: Vec<ExprId>,
    loop_depth: usize,
    unsafe_depth: usize,
}

impl<'db> Typer<'_, 'db> {
    fn new<'func>(
        db: &'db dyn Database,
        function: &'func Function<'db>,
        source_map: &'db mitki_lower::hir::FunctionSourceMap,
        resolver: Resolver<'db>,
    ) -> Typer<'func, 'db> {
        Typer {
            db,
            function,
            source_map,
            resolver,
            vars: Vec::new(),
            env: FxHashMap::default(),
            binding_names: FxHashSet::default(),
            type_param_env: FxHashMap::default(),
            node_types: FxHashMap::default(),
            selected_union_members: FxHashMap::default(),
            matched_typed_patterns: FxHashMap::default(),
            variant_constraints: Vec::new(),
            deferred_coercions: Vec::new(),
            missing_param_nodes: FxHashSet::default(),
            preserved_type_vars: FxHashSet::default(),
            return_ty: None,
            inference: Inference::default(),
            context: Vec::new(),
            loop_depth: 0,
            unsafe_depth: usize::from(function.is_unsafe()),
        }
    }

    fn fresh_id(&mut self, level: usize) -> VarId {
        let id = self.vars.len();
        self.vars.push(VarState { level, lower_bounds: Vec::new(), upper_bounds: Vec::new() });
        id
    }

    fn fresh_var(&mut self, level: usize) -> InferTy {
        InferTy::Var(self.fresh_id(level))
    }

    fn instantiate(&mut self, scheme: &Scheme, lvl: usize) -> InferTy {
        match scheme {
            Scheme::Mono(t) => t.clone(),
            Scheme::Poly { level, body } => self.freshen(*level, body, lvl),
        }
    }

    fn level(&self, ty: &InferTy) -> usize {
        match ty {
            InferTy::Var(v) => self.vars[*v].level,
            InferTy::Known(_) | InferTy::Unknown => 0,
            InferTy::Function(inputs, output) => {
                let max_input = inputs.iter().map(|t| self.level(t)).max().unwrap_or(0);
                max_input.max(self.level(output))
            }
            InferTy::Array(item) => self.level(item),
            InferTy::Tuple(items) => items.iter().map(|t| self.level(t)).max().unwrap_or(0),
            InferTy::Record(fields) => {
                fields.iter().map(|(_, ty)| self.level(ty)).max().unwrap_or(0)
            }
            InferTy::Union(items) | InferTy::Inter(items) => {
                items.iter().map(|t| self.level(t)).max().unwrap_or(0)
            }
        }
    }

    fn symbol_to_bits(sym: Symbol<'db>) -> u64 {
        sym.as_id().as_bits()
    }

    fn symbol_from_bits(bits: u64) -> Symbol<'db> {
        Symbol::from_id(salsa::Id::from_bits(bits))
    }

    fn infer_ty_from_ty(ty: Ty<'db>) -> InferTy {
        InferTy::Known(ty.as_id().as_bits())
    }

    fn infer_ty_from_kind(&self, kind: TyKind<'db>) -> InferTy {
        Self::infer_ty_from_ty(Ty::new(self.db, kind))
    }

    fn known_ty(ty: &InferTy) -> Option<Ty<'db>> {
        match ty {
            InferTy::Known(bits) => Some(Ty::from_id(salsa::Id::from_bits(*bits))),
            _ => None,
        }
    }

    fn numeric_kind(&self, ty: &InferTy) -> Option<NumericKind> {
        let ty = Self::known_ty(ty)?;
        match ty.kind(self.db) {
            TyKind::Int => Some(NumericKind::Int),
            TyKind::ExactInt(int_ty) => Some(NumericKind::ExactInt(*int_ty)),
            TyKind::Float => Some(NumericKind::Float),
            _ => None,
        }
    }

    fn orderable_kind(&self, ty: &InferTy) -> Option<OrderableKind> {
        let ty = Self::known_ty(ty)?;
        match ty.kind(self.db) {
            TyKind::Int => Some(OrderableKind::Int),
            TyKind::ExactInt(int_ty) => Some(OrderableKind::ExactInt(*int_ty)),
            TyKind::Float => Some(OrderableKind::Float),
            TyKind::Char => Some(OrderableKind::Char),
            _ => None,
        }
    }

    fn bool_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::Bool)
    }

    fn int_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::Int)
    }

    fn float_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::Float)
    }

    fn exact_int_infer_ty(&self, int_ty: ExactInt) -> InferTy {
        self.infer_ty_from_kind(TyKind::ExactInt(int_ty))
    }

    fn numeric_infer_ty(&self, kind: NumericKind) -> InferTy {
        match kind {
            NumericKind::Int => self.int_infer_ty(),
            NumericKind::ExactInt(int_ty) => self.exact_int_infer_ty(int_ty),
            NumericKind::Float => self.float_infer_ty(),
        }
    }

    fn literal_can_coerce_to_numeric(kind: NumericKind) -> bool {
        !matches!(kind, NumericKind::Float)
    }

    fn orderable_infer_ty(&self, kind: OrderableKind) -> InferTy {
        match kind {
            OrderableKind::Int => self.int_infer_ty(),
            OrderableKind::ExactInt(int_ty) => self.exact_int_infer_ty(int_ty),
            OrderableKind::Float => self.float_infer_ty(),
            OrderableKind::Char => self.char_infer_ty(),
        }
    }

    fn literal_can_coerce_to_orderable(kind: OrderableKind) -> bool {
        matches!(kind, OrderableKind::Int | OrderableKind::ExactInt(_))
    }

    fn string_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::String)
    }

    fn char_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::Char)
    }

    fn unit_infer_ty() -> InferTy {
        InferTy::Tuple(Vec::new())
    }

    fn pointer_infer_ty(&self, mutable: bool, pointee: Ty<'db>) -> InferTy {
        self.infer_ty_from_kind(TyKind::Pointer { mutable, pointee })
    }

    fn exact_u8_ty(&self) -> Ty<'db> {
        Ty::new(self.db, TyKind::ExactInt(ExactInt::U8))
    }

    fn exact_u32_infer_ty(&self) -> InferTy {
        self.exact_int_infer_ty(ExactInt::U32)
    }

    fn array_u8_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::Array(self.exact_u8_ty()))
    }

    fn ptr_field_bits(&self) -> u64 {
        Self::symbol_to_bits("ptr".into_symbol(self.db))
    }

    fn len_field_bits(&self) -> u64 {
        Self::symbol_to_bits("len".into_symbol(self.db))
    }

    fn byte_view_infer_ty(&self, mutable: bool, expected: Option<&InferTy>) -> InferTy {
        if let Some(expected) = expected {
            if let Some(ty) = Self::known_ty(expected)
                && self.byte_view_ty_matches(ty, mutable)
            {
                return expected.clone();
            }
            if self.byte_view_record_matches(expected, mutable) {
                return expected.clone();
            }
        }

        let ptr = self.pointer_infer_ty(mutable, self.exact_u8_ty());
        InferTy::Record(vec![
            (self.ptr_field_bits(), ptr),
            (self.len_field_bits(), self.exact_u32_infer_ty()),
        ])
    }

    fn byte_view_record_matches(&self, ty: &InferTy, mutable: bool) -> bool {
        let InferTy::Record(fields) = ty else {
            return false;
        };

        let ptr = fields
            .iter()
            .find(|(name_bits, _)| *name_bits == self.ptr_field_bits())
            .map(|(_, ty)| ty);
        let len = fields
            .iter()
            .find(|(name_bits, _)| *name_bits == self.len_field_bits())
            .map(|(_, ty)| ty);

        matches!(
            (ptr, len),
            (Some(ptr), Some(len))
                if self.pointer_record_field_matches(ptr, mutable)
                    && matches!(Self::known_ty(len), Some(ty) if matches!(ty.kind(self.db), TyKind::ExactInt(ExactInt::U32)))
        )
    }

    fn pointer_record_field_matches(&self, ty: &InferTy, mutable: bool) -> bool {
        matches!(
            Self::known_ty(ty).map(|ty| ty.kind(self.db)),
            Some(TyKind::Pointer { mutable: actual_mutable, pointee })
                if *actual_mutable == mutable
                    && matches!(pointee.kind(self.db), TyKind::ExactInt(ExactInt::U8))
        )
    }

    fn byte_view_ty_matches(&self, ty: Ty<'db>, mutable: bool) -> bool {
        let TyKind::ExternStruct(struct_ty) = ty.kind(self.db) else {
            return false;
        };

        let fields = struct_fields(self.db, *struct_ty);
        let mut ptr_matches = false;
        let mut len_matches = false;
        for (name, field_ty) in fields {
            let field_bits = Self::symbol_to_bits(*name);
            if field_bits == self.ptr_field_bits() {
                ptr_matches = matches!(
                    field_ty.kind(self.db),
                    TyKind::Pointer { mutable: actual_mutable, pointee }
                        if *actual_mutable == mutable
                            && matches!(pointee.kind(self.db), TyKind::ExactInt(ExactInt::U8))
                );
            } else if field_bits == self.len_field_bits() {
                len_matches = matches!(field_ty.kind(self.db), TyKind::ExactInt(ExactInt::U32));
            }
        }
        ptr_matches && len_matches
    }

    fn in_unsafe_context(&self) -> bool {
        self.unsafe_depth > 0
    }

    fn with_unsafe_context<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.unsafe_depth += 1;
        let output = f(self);
        self.unsafe_depth -= 1;
        output
    }

    fn require_unsafe_context(&mut self, node: ExprId) {
        if !self.in_unsafe_context() {
            self.emit(DiagnosticKind::UnsafeOperationRequiresUnsafeContext(node));
        }
    }

    fn integer_literal_infer_ty(&self, expected: Option<&InferTy>) -> InferTy {
        let Some(ty) = expected.and_then(Self::known_ty) else {
            return self.int_infer_ty();
        };

        match ty.kind(self.db) {
            TyKind::Int => self.int_infer_ty(),
            TyKind::ExactInt(int_ty) => self.exact_int_infer_ty(*int_ty),
            _ => self.int_infer_ty(),
        }
    }

    fn integer_pattern_infer_ty(&self, expected: &InferTy) -> InferTy {
        let Some(ty) = Self::known_ty(expected) else {
            return self.int_infer_ty();
        };

        match ty.kind(self.db) {
            TyKind::Int => self.int_infer_ty(),
            TyKind::ExactInt(int_ty) => self.exact_int_infer_ty(*int_ty),
            _ => self.int_infer_ty(),
        }
    }

    fn flatten_union(ty: InferTy, out: &mut Vec<InferTy>) {
        match ty {
            InferTy::Union(items) => {
                for item in items {
                    Self::flatten_union(item, out);
                }
            }
            other => out.push(other),
        }
    }

    fn flatten_inter(ty: InferTy, out: &mut Vec<InferTy>) {
        match ty {
            InferTy::Inter(items) => {
                for item in items {
                    Self::flatten_inter(item, out);
                }
            }
            other => out.push(other),
        }
    }

    fn mk_union_many(items: impl IntoIterator<Item = InferTy>) -> InferTy {
        let mut flattened = Vec::new();
        for item in items {
            Self::flatten_union(item, &mut flattened);
        }

        let mut seen: FxHashSet<InferTy> = FxHashSet::default();
        let mut elems = Vec::new();
        for ty in flattened {
            if matches!(ty, InferTy::Unknown) {
                continue;
            }
            if seen.insert(ty.clone()) {
                elems.push(ty);
            }
        }

        match elems.len() {
            0 => InferTy::Unknown,
            1 => elems.pop().expect("single element"),
            _ => InferTy::Union(elems),
        }
    }

    fn mk_union(lhs: InferTy, rhs: InferTy) -> InferTy {
        Self::mk_union_many([lhs, rhs])
    }

    fn mk_inter_many(items: impl IntoIterator<Item = InferTy>) -> InferTy {
        let mut flattened = Vec::new();
        for item in items {
            Self::flatten_inter(item, &mut flattened);
        }

        let mut seen: FxHashSet<InferTy> = FxHashSet::default();
        let mut elems = Vec::new();
        for ty in flattened {
            if matches!(ty, InferTy::Unknown) {
                continue;
            }
            if seen.insert(ty.clone()) {
                elems.push(ty);
            }
        }

        match elems.len() {
            0 => InferTy::Unknown,
            1 => elems.pop().expect("single element"),
            _ => InferTy::Inter(elems),
        }
    }

    fn mk_inter(lhs: InferTy, rhs: InferTy) -> InferTy {
        Self::mk_inter_many([lhs, rhs])
    }

    fn emit(&mut self, kind: DiagnosticKind<'db>) {
        let context = self.context.last().copied();
        self.inference.diagnostics.push(Diagnostic::new(kind, context));
    }

    fn diagnostic_node(kind: &DiagnosticKind<'db>) -> Option<ExprId> {
        match kind {
            DiagnosticKind::UnresolvedIdent(node)
            | DiagnosticKind::TypeMismatch(node, _, _)
            | DiagnosticKind::UnknownType(node)
            | DiagnosticKind::ExpectedValueFoundType(node, _)
            | DiagnosticKind::CallArityMismatch(node, _, _)
            | DiagnosticKind::CallNonFunction(node, _)
            | DiagnosticKind::ClosureArityMismatch(node, _, _)
            | DiagnosticKind::InvalidBinaryOp(node, _, _, _)
            | DiagnosticKind::InvalidPrefixOp(node, _, _)
            | DiagnosticKind::InvalidPostfixOp(node, _, _)
            | DiagnosticKind::MissingElseBranch(node)
            | DiagnosticKind::MissingParameterType(node)
            | DiagnosticKind::MissingInitializer(node)
            | DiagnosticKind::TupleArityMismatch(node, _, _)
            | DiagnosticKind::MissingStructField(node, _)
            | DiagnosticKind::UnknownStructField(node, _)
            | DiagnosticKind::NotAStruct(node, _)
            | DiagnosticKind::NotAnEnum(node, _)
            | DiagnosticKind::DuplicatePatternBinding(node)
            | DiagnosticKind::RefutablePattern(node)
            | DiagnosticKind::UnsupportedFloatPattern(node)
            | DiagnosticKind::BreakOutsideLoop(node)
            | DiagnosticKind::ContinueOutsideLoop(node)
            | DiagnosticKind::CompilerIntrinsicMustBeCalled(node, _)
            | DiagnosticKind::InvalidComptimeCall(node)
            | DiagnosticKind::ComptimeTargetMustBeZeroArg(node)
            | DiagnosticKind::ComptimeTargetMustNotBeGeneric(node)
            | DiagnosticKind::ComptimeTargetMustReturnSupportedType(node, _)
            | DiagnosticKind::ReflectionOnlyInComptime(node, _)
            | DiagnosticKind::InvalidReflectionTarget(node, _, _)
            | DiagnosticKind::UnsafeOperationRequiresUnsafeContext(node)
            | DiagnosticKind::InvalidUnsafeIntrinsicArgument(node, _, _)
            | DiagnosticKind::InvalidUnsafeIntrinsicResult(node, _, _)
            | DiagnosticKind::InvalidAssignmentTarget(node)
            | DiagnosticKind::AssignmentRequiresMutable(node)
            | DiagnosticKind::MutableArgumentRequiresPlace(node) => Some(*node),
            DiagnosticKind::UnresolvedType(_, _)
            | DiagnosticKind::PatternTypeMismatch(_, _, _)
            | DiagnosticKind::AmbiguousUnionPattern(_, _) => None,
        }
    }

    fn has_diagnostic_at_node(&self, node: ExprId) -> bool {
        self.inference
            .diagnostics
            .iter()
            .any(|diag| Self::diagnostic_node(diag.kind()) == Some(node))
    }

    fn emit_unknown_type_errors(&mut self) {
        let nodes = self.function.node_store();
        let unknown_nodes: Vec<ExprId> = self
            .node_types
            .iter()
            .filter_map(|(node, ty)| {
                let kind = nodes.node_kind(*node);
                let eligible_kind = matches!(
                    kind,
                    NodeKind::Tuple
                        | NodeKind::If
                        | NodeKind::LoopExpr
                        | NodeKind::Closure
                        | NodeKind::Call
                        | NodeKind::Array
                        | NodeKind::ArrayRepeat
                        | NodeKind::Field
                        | NodeKind::Binary
                        | NodeKind::Postfix
                        | NodeKind::Prefix
                        | NodeKind::StructExpr
                );
                if matches!(ty, InferTy::Unknown)
                    && eligible_kind
                    && !self.binding_names.contains(node)
                    && !self.has_diagnostic_at_node(*node)
                {
                    Some(*node)
                } else {
                    None
                }
            })
            .collect();

        for node in unknown_nodes {
            self.emit(DiagnosticKind::UnknownType(node));
        }
    }

    fn with_context<T>(&mut self, node: ExprId, f: impl FnOnce(&mut Self) -> T) -> T {
        self.context.push(node);
        let out = f(self);
        self.context.pop();
        out
    }

    fn is_context_node(kind: NodeKind) -> bool {
        matches!(
            kind,
            NodeKind::Tuple
                | NodeKind::If
                | NodeKind::LoopExpr
                | NodeKind::Closure
                | NodeKind::Call
                | NodeKind::StructExpr
        )
    }

    fn with_loop_depth<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.loop_depth += 1;
        let out = f(self);
        self.loop_depth -= 1;
        out
    }

    fn diagnostic_ty(&self, ty: &InferTy) -> Ty<'db> {
        self.present_type(ty, Polarity::Positive)
    }

    fn ty_to_infer_ty(&self, ty: Ty<'db>) -> InferTy {
        match ty.kind(self.db) {
            TyKind::Unknown => InferTy::Unknown,
            TyKind::Array(item) => InferTy::Array(Box::new(self.ty_to_infer_ty(*item))),
            TyKind::Tuple(items) => {
                InferTy::Tuple(items.iter().map(|&t| self.ty_to_infer_ty(t)).collect())
            }
            TyKind::Record(fields) => InferTy::Record(
                fields
                    .iter()
                    .map(|(name, ty)| (Self::symbol_to_bits(*name), self.ty_to_infer_ty(*ty)))
                    .collect(),
            ),
            TyKind::Function { inputs, output } => InferTy::Function(
                inputs.iter().map(|&t| self.ty_to_infer_ty(t)).collect(),
                Box::new(self.ty_to_infer_ty(*output)),
            ),
            TyKind::Union(items) => {
                Self::mk_union_many(items.iter().map(|&t| self.ty_to_infer_ty(t)))
            }
            TyKind::Inter(items) => {
                Self::mk_inter_many(items.iter().map(|&t| self.ty_to_infer_ty(t)))
            }
            _ => Self::infer_ty_from_ty(ty),
        }
    }

    fn enum_variant_infer_ty(&self, enum_ty: Ty<'db>, variant: Symbol<'db>) -> Option<InferTy> {
        let TyKind::Enum(enum_ty_id) = enum_ty.kind(self.db) else {
            return None;
        };
        let variants = enum_variants(self.db, *enum_ty_id);

        variants.iter().find(|(name, _)| *name == variant).map(|(_, payload_tys)| {
            let enum_infer_ty = Self::infer_ty_from_ty(enum_ty);
            if payload_tys.is_empty() {
                enum_infer_ty
            } else {
                let inputs = payload_tys.iter().map(|&t| self.ty_to_infer_ty(t)).collect();
                InferTy::Function(inputs, Box::new(enum_infer_ty))
            }
        })
    }

    fn resolve_path_in_node_scope(
        &mut self,
        node: ExprId,
        path: Symbol<'db>,
    ) -> Option<BindingId<'db>> {
        let guard = self.resolver.scopes_for_node(node);
        let resolution = self.resolver.resolve_value_binding(path);
        self.resolver.reset(guard);
        resolution
    }

    fn resolve_type_in_node_scope(&mut self, node: ExprId, path: Symbol<'db>) -> Option<Ty<'db>> {
        let guard = self.resolver.scopes_for_node(node);
        let binding = self.resolver.resolve_type_binding(path);
        let ty = binding.and_then(|binding| self.resolver.ty_for_binding(binding));
        self.resolver.reset(guard);
        ty
    }

    fn resolve_place(&mut self, expr: ExprId, lvl: usize) -> PlaceResolution {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::Name => {
                let Some(name_id) = nodes.as_name(expr) else {
                    return PlaceResolution::Invalid;
                };
                let Some(binding) = self.resolve_path_in_node_scope(expr, nodes.name(name_id))
                else {
                    self.emit(DiagnosticKind::UnresolvedIdent(expr));
                    return PlaceResolution::Invalid;
                };
                match binding {
                    BindingId::Local(name) | BindingId::Param(name) => {
                        let ty = self.binding_infer_ty(name, lvl);
                        self.node_types.insert(expr, ty.clone());
                        if self.source_map.is_mutable_binding(name) {
                            PlaceResolution::Mutable(ty)
                        } else {
                            PlaceResolution::Immutable
                        }
                    }
                    _ => PlaceResolution::Invalid,
                }
            }
            NodeKind::Field => {
                let field_id = nodes.as_field(expr).expect("Field node mismatch");
                let (base, field_name_expr) = nodes.field(field_id);
                if base == ExprId::ZERO {
                    return PlaceResolution::Invalid;
                }
                let mutability = match self.resolve_place(base, lvl) {
                    PlaceResolution::Mutable(_) => true,
                    PlaceResolution::Immutable => false,
                    PlaceResolution::Invalid => return PlaceResolution::Invalid,
                };
                let field_ty = self.infer_field_expr(expr, base, field_name_expr, None, lvl);
                self.node_types.insert(expr, field_ty.clone());
                if mutability {
                    PlaceResolution::Mutable(field_ty)
                } else {
                    PlaceResolution::Immutable
                }
            }
            _ => PlaceResolution::Invalid,
        }
    }

    fn bind_pattern_root(
        &mut self,
        pattern: PatId,
        expected: &InferTy,
        lvl: usize,
        scheme: PatternBindingScheme,
        allow_refutable: bool,
        fallback: ExprId,
    ) {
        if pattern == PatId::ZERO {
            return;
        }

        let mut seen = FxHashSet::default();
        let irrefutable = self.bind_pattern(pattern, expected, lvl, scheme, fallback, &mut seen);
        if !allow_refutable && !irrefutable {
            self.emit(DiagnosticKind::RefutablePattern(
                self.pattern_anchor(pattern).unwrap_or(fallback),
            ));
        }
    }

    fn bind_pattern(
        &mut self,
        pattern: PatId,
        expected: &InferTy,
        lvl: usize,
        scheme: PatternBindingScheme,
        fallback: ExprId,
        seen: &mut FxHashSet<u64>,
    ) -> bool {
        let nodes = self.function.node_store();
        let anchor = self.pattern_anchor(pattern).unwrap_or(fallback);

        match self.select_union_member_for_pattern(pattern, expected) {
            UnionPatternSelection::Unique(selected) => {
                self.selected_union_members.insert(pattern, selected.clone());
                return self.bind_pattern(pattern, &selected, lvl, scheme, fallback, seen);
            }
            UnionPatternSelection::Ambiguous => {
                self.emit(DiagnosticKind::AmbiguousUnionPattern(
                    pattern,
                    self.diagnostic_ty(expected),
                ));
                return false;
            }
            UnionPatternSelection::None => {}
        }

        match nodes.node_kind(pattern) {
            NodeKind::PatBinding => {
                let (name, _) =
                    nodes.pat_binding(nodes.as_pat_binding(pattern).expect("PatBinding mismatch"));
                self.bind_pattern_name(name, expected.clone(), scheme, seen);
                true
            }
            NodeKind::PatWildcard => true,
            NodeKind::PatTyped => {
                let (inner, ty_id) =
                    nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped mismatch"));
                let annotation_resolved = self.resolve_type_to_infer_ty_silently(ty_id).is_some();
                let annotated = self.resolve_type_to_infer_ty(ty_id).unwrap_or(InferTy::Unknown);
                let matches_annotation = self.pattern_type_matches(expected, &annotated);
                if matches_annotation && annotation_resolved {
                    self.matched_typed_patterns.insert(pattern, expected.clone());
                }
                if !matches_annotation {
                    self.pattern_constrain(pattern, expected, &annotated);
                }
                let binding_ty = if matches_annotation { expected.clone() } else { annotated };
                self.bind_pattern(inner, &binding_ty, lvl, scheme, fallback, seen)
                    && matches_annotation
            }
            NodeKind::PatTrue | NodeKind::PatFalse => {
                self.pattern_constrain(pattern, expected, &self.bool_infer_ty());
                false
            }
            NodeKind::PatInt => {
                let int_ty = self.integer_pattern_infer_ty(expected);
                self.pattern_constrain(pattern, expected, &int_ty);
                false
            }
            NodeKind::PatFloat => {
                self.pattern_constrain(pattern, expected, &self.float_infer_ty());
                self.emit(DiagnosticKind::UnsupportedFloatPattern(anchor));
                false
            }
            NodeKind::PatString => {
                self.pattern_constrain(pattern, expected, &self.string_infer_ty());
                false
            }
            NodeKind::PatChar => {
                self.pattern_constrain(pattern, expected, &self.char_infer_ty());
                false
            }
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.bind_pattern(inner, expected, lvl, scheme, fallback, seen)
            }
            NodeKind::PatTuple => {
                let items = nodes.pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple"));
                let item_ids: Vec<_> = items.iter().collect();
                let item_tys = match expected {
                    InferTy::Tuple(expected_items) if expected_items.len() == item_ids.len() => {
                        expected_items.clone()
                    }
                    InferTy::Tuple(expected_items) => {
                        self.emit(DiagnosticKind::TupleArityMismatch(
                            anchor,
                            expected_items.len(),
                            item_ids.len(),
                        ));
                        item_ids.iter().map(|_| self.fresh_var(lvl)).collect()
                    }
                    _ => item_ids.iter().map(|_| self.fresh_var(lvl)).collect(),
                };
                let tuple_ty = InferTy::Tuple(item_tys.clone());
                self.pattern_constrain(pattern, expected, &tuple_ty);
                let mut irrefutable = true;
                for (item, item_ty) in item_ids.into_iter().zip(item_tys.iter()) {
                    irrefutable &= self.bind_pattern(item, item_ty, lvl, scheme, fallback, seen);
                }
                irrefutable
            }
            NodeKind::PatStruct => {
                let (path, fields) =
                    nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"));
                let Some(struct_ty) = self.resolve_struct_pattern_type(path) else {
                    for field in fields.iter() {
                        let (_, pat) = nodes.pat_struct_field(
                            nodes.as_pat_struct_field(field).expect("PatStructField"),
                        );
                        if pat != PatId::ZERO {
                            self.bind_pattern(pat, &InferTy::Unknown, lvl, scheme, fallback, seen);
                        }
                    }
                    return false;
                };

                let struct_infer = self.ty_to_infer_ty(struct_ty);
                self.pattern_constrain(pattern, expected, &struct_infer);

                let TyKind::Struct(struct_nominal) = struct_ty.kind(self.db) else {
                    self.emit(DiagnosticKind::NotAStruct(anchor, struct_ty));
                    return false;
                };
                let field_map: FxHashMap<_, _> = struct_fields(self.db, *struct_nominal)
                    .iter()
                    .map(|(name, ty)| (*name, self.ty_to_infer_ty(*ty)))
                    .collect();
                let mut seen_fields = FxHashSet::default();
                let mut irrefutable = true;

                for field in fields.iter() {
                    let (name, pat) = nodes.pat_struct_field(
                        nodes.as_pat_struct_field(field).expect("PatStructField"),
                    );
                    let field_sym = nodes.name(name);
                    if let Some(field_ty) = field_map.get(&field_sym) {
                        seen_fields.insert(field_sym);
                        if pat != PatId::ZERO {
                            irrefutable &=
                                self.bind_pattern(pat, field_ty, lvl, scheme, name.into(), seen);
                        } else {
                            self.bind_pattern_name(name, field_ty.clone(), scheme, seen);
                        }
                    } else {
                        self.emit(DiagnosticKind::UnknownStructField(name.into(), field_sym));
                        irrefutable = false;
                    }
                }

                for field_name in field_map.keys() {
                    if !seen_fields.contains(field_name) {
                        self.emit(DiagnosticKind::MissingStructField(anchor, *field_name));
                        irrefutable = false;
                    }
                }

                irrefutable
            }
            NodeKind::PatVariant => {
                let (path, args) =
                    nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant"));
                let Some((enum_ty, payload_tys, variant_name, single_variant)) =
                    self.resolve_variant_pattern(path, expected)
                else {
                    for arg in args.iter() {
                        self.bind_pattern(arg, &InferTy::Unknown, lvl, scheme, fallback, seen);
                    }
                    return false;
                };

                self.pattern_constrain(pattern, expected, &enum_ty);
                let arg_ids: Vec<_> = args.iter().collect();
                if payload_tys.len() != arg_ids.len() {
                    self.emit(DiagnosticKind::CallArityMismatch(
                        variant_name,
                        payload_tys.len(),
                        arg_ids.len(),
                    ));
                }

                let mut irrefutable = single_variant;
                for (index, arg) in arg_ids.into_iter().enumerate() {
                    let expected_ty = payload_tys.get(index).cloned().unwrap_or(InferTy::Unknown);
                    irrefutable &=
                        self.bind_pattern(arg, &expected_ty, lvl, scheme, variant_name, seen);
                }
                irrefutable
            }
            _ => true,
        }
    }

    fn select_union_member_for_pattern(
        &mut self,
        pattern: PatId,
        expected: &InferTy,
    ) -> UnionPatternSelection {
        if !self.is_member_specific_pattern(pattern) {
            return UnionPatternSelection::None;
        }

        let InferTy::Union(items) = expected else {
            return UnionPatternSelection::None;
        };

        let mut candidates = Vec::new();
        for item in items {
            Self::flatten_union(item.clone(), &mut candidates);
        }

        let compatible = candidates
            .into_iter()
            .filter(|candidate| self.pattern_matches_ty(pattern, candidate))
            .collect::<Vec<_>>();

        match compatible.as_slice() {
            [] => UnionPatternSelection::None,
            [selected] => UnionPatternSelection::Unique(selected.clone()),
            _ => UnionPatternSelection::Ambiguous,
        }
    }

    fn is_member_specific_pattern(&self, pattern: PatId) -> bool {
        if pattern == PatId::ZERO {
            return false;
        }

        let nodes = self.function.node_store();
        match nodes.node_kind(pattern) {
            NodeKind::PatBinding | NodeKind::PatWildcard => false,
            NodeKind::PatTyped => true,
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.is_member_specific_pattern(inner)
            }
            _ => true,
        }
    }

    fn pattern_matches_ty(&mut self, pattern: PatId, expected: &InferTy) -> bool {
        if pattern == PatId::ZERO {
            return true;
        }

        if let InferTy::Union(items) = expected
            && self.is_member_specific_pattern(pattern)
        {
            return items.iter().any(|item| self.pattern_matches_ty(pattern, item));
        }

        let nodes = self.function.node_store();
        match nodes.node_kind(pattern) {
            NodeKind::PatBinding | NodeKind::PatWildcard => true,
            NodeKind::PatTyped => {
                let (inner, ty_id) =
                    nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped mismatch"));
                let Some(annotated) = self.resolve_type_to_infer_ty_silently(ty_id) else {
                    return false;
                };
                self.pattern_type_matches(expected, &annotated)
                    && (inner == PatId::ZERO || self.pattern_matches_ty(inner, expected))
            }
            NodeKind::PatTrue | NodeKind::PatFalse => {
                Self::literal_pattern_matches_ty(expected, |ty| {
                    matches!(ty.kind(self.db), TyKind::Bool)
                })
            }
            NodeKind::PatInt => Self::literal_pattern_matches_ty(expected, |ty| {
                matches!(ty.kind(self.db), TyKind::Int | TyKind::ExactInt(_))
            }),
            NodeKind::PatFloat => Self::literal_pattern_matches_ty(expected, |ty| {
                matches!(ty.kind(self.db), TyKind::Float)
            }),
            NodeKind::PatString => Self::literal_pattern_matches_ty(expected, |ty| {
                matches!(ty.kind(self.db), TyKind::String)
            }),
            NodeKind::PatChar => Self::literal_pattern_matches_ty(expected, |ty| {
                matches!(ty.kind(self.db), TyKind::Char)
            }),
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.pattern_matches_ty(inner, expected)
            }
            NodeKind::PatTuple => {
                let items = nodes
                    .pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple"))
                    .iter()
                    .collect::<Vec<_>>();
                match expected {
                    InferTy::Tuple(expected_items) if expected_items.len() == items.len() => items
                        .iter()
                        .zip(expected_items.iter())
                        .all(|(&item, item_ty)| self.pattern_matches_ty(item, item_ty)),
                    InferTy::Var(_) | InferTy::Unknown => true,
                    _ => false,
                }
            }
            NodeKind::PatStruct => {
                let (path, fields) =
                    nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"));
                let resolved_path = self.resolve_pattern_type_silently(path);
                match expected {
                    InferTy::Known(bits) => {
                        let ty = Ty::from_id(salsa::Id::from_bits(*bits));
                        match ty.kind(self.db) {
                            TyKind::Struct(struct_ty) => {
                                if resolved_path.is_some_and(|path_ty| path_ty != ty) {
                                    return false;
                                }

                                let field_map: FxHashMap<_, _> = struct_fields(self.db, *struct_ty)
                                    .iter()
                                    .map(|(name, ty)| {
                                        (Self::symbol_to_bits(*name), self.ty_to_infer_ty(*ty))
                                    })
                                    .collect();
                                fields.iter().all(|field| {
                                    let (name, pat) = nodes.pat_struct_field(
                                        nodes.as_pat_struct_field(field).expect("PatStructField"),
                                    );
                                    let field_bits = Self::symbol_to_bits(nodes.name(name));
                                    let Some(field_ty) = field_map.get(&field_bits) else {
                                        return path != ExprId::ZERO;
                                    };
                                    pat == PatId::ZERO || self.pattern_matches_ty(pat, field_ty)
                                })
                            }
                            _ => false,
                        }
                    }
                    InferTy::Record(field_map) => {
                        if resolved_path.is_some() {
                            return false;
                        }

                        let field_map: FxHashMap<_, _> =
                            field_map.iter().map(|(name, ty)| (*name, ty.clone())).collect();
                        fields.iter().all(|field| {
                            let (name, pat) = nodes.pat_struct_field(
                                nodes.as_pat_struct_field(field).expect("PatStructField"),
                            );
                            let field_bits = Self::symbol_to_bits(nodes.name(name));
                            let Some(field_ty) = field_map.get(&field_bits) else {
                                return path == ExprId::ZERO;
                            };
                            pat == PatId::ZERO || self.pattern_matches_ty(pat, field_ty)
                        })
                    }
                    InferTy::Var(_) | InferTy::Unknown => {
                        resolved_path.is_some() || path == ExprId::ZERO
                    }
                    _ => false,
                }
            }
            NodeKind::PatVariant => {
                let (path, args) =
                    nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant"));
                let Some(field_id) = nodes.as_field(path) else {
                    return false;
                };
                let (base, variant_name_expr) = nodes.field(field_id);
                let Some(variant_name) = nodes.as_name(variant_name_expr) else {
                    return false;
                };
                let variant_sym = nodes.name(variant_name);
                let explicit_enum = self.resolve_pattern_type_silently(base);

                match expected {
                    InferTy::Known(bits) => {
                        let ty = Ty::from_id(salsa::Id::from_bits(*bits));
                        let TyKind::Enum(enum_ty) = ty.kind(self.db) else {
                            return false;
                        };

                        if explicit_enum.is_some_and(|enum_ty_path| enum_ty_path != ty) {
                            return false;
                        }

                        let Some((_, payload_tys)) = enum_variants(self.db, *enum_ty)
                            .iter()
                            .find(|(name, _)| *name == variant_sym)
                        else {
                            return false;
                        };

                        args.iter().zip(payload_tys.iter()).all(|(arg, payload_ty)| {
                            self.pattern_matches_ty(arg, &self.ty_to_infer_ty(*payload_ty))
                        })
                    }
                    InferTy::Var(_) | InferTy::Unknown => explicit_enum.is_some(),
                    _ => false,
                }
            }
            _ => true,
        }
    }

    fn literal_pattern_matches_ty(
        expected: &InferTy,
        matches_known: impl FnOnce(Ty<'db>) -> bool,
    ) -> bool {
        match expected {
            InferTy::Known(bits) => matches_known(Ty::from_id(salsa::Id::from_bits(*bits))),
            InferTy::Var(_) | InferTy::Unknown => true,
            _ => false,
        }
    }

    fn pattern_type_matches(&mut self, actual: &InferTy, annotated: &InferTy) -> bool {
        let vars_snapshot = self.vars.clone();
        let matches = self.constrain_top(actual, annotated).is_ok();
        self.vars = vars_snapshot;
        matches
    }

    fn resolve_type_to_infer_ty_silently(&mut self, ty: TyId) -> Option<InferTy> {
        let diagnostics_len = self.inference.diagnostics.len();
        let resolved = self.resolve_type_to_infer_ty(ty);
        let changed = self.inference.diagnostics.len() != diagnostics_len;
        self.inference.diagnostics.truncate(diagnostics_len);
        if changed { None } else { resolved }
    }

    fn resolve_pattern_type_silently(&mut self, path: ExprId) -> Option<Ty<'db>> {
        if path == ExprId::ZERO {
            return None;
        }

        let nodes = self.function.node_store();
        let name = nodes.as_name(path)?;
        let symbol = nodes.name(name);

        self.resolve_type_in_node_scope(path, symbol)
    }

    fn bind_pattern_name(
        &mut self,
        name: NameId,
        ty: InferTy,
        scheme: PatternBindingScheme,
        seen: &mut FxHashSet<u64>,
    ) {
        let symbol = self.function.node_store().name(name);
        let expr_id: ExprId = name.into();
        let bits = Self::symbol_to_bits(symbol);
        if !seen.insert(bits) {
            self.emit(DiagnosticKind::DuplicatePatternBinding(name.into()));
        }

        self.node_types.insert(expr_id, ty.clone());
        self.binding_names.insert(expr_id);
        match scheme {
            PatternBindingScheme::Mono => {
                self.env.insert(expr_id, Scheme::Mono(ty));
            }
            PatternBindingScheme::Poly { level } => {
                self.env.insert(expr_id, Scheme::Poly { level, body: ty });
            }
        }
    }

    fn pattern_constrain(&mut self, pattern: PatId, actual: &InferTy, expected: &InferTy) {
        if matches!(actual, InferTy::Unknown) || matches!(expected, InferTy::Unknown) {
            return;
        }
        if self.constrain_top(actual, expected).is_err() {
            self.emit(DiagnosticKind::PatternTypeMismatch(
                pattern,
                self.diagnostic_ty(actual),
                self.diagnostic_ty(expected),
            ));
        }
    }

    fn pattern_anchor(&self, pattern: PatId) -> Option<ExprId> {
        if pattern == PatId::ZERO {
            return None;
        }

        let nodes = self.function.node_store();
        match nodes.node_kind(pattern) {
            NodeKind::PatBinding => {
                let (name, _) =
                    nodes.pat_binding(nodes.as_pat_binding(pattern).expect("PatBinding"));
                Some(name.into())
            }
            NodeKind::PatTyped => {
                let (inner, _) = nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped"));
                self.pattern_anchor(inner)
            }
            NodeKind::PatStruct => {
                let (path, _) = nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"));
                (path != ExprId::ZERO).then_some(path).or_else(|| {
                    nodes
                        .pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"))
                        .1
                        .iter()
                        .find_map(|field| {
                            let (name, pat) = nodes.pat_struct_field(
                                nodes.as_pat_struct_field(field).expect("PatStructField"),
                            );
                            if pat != PatId::ZERO {
                                self.pattern_anchor(pat)
                            } else {
                                Some(name.into())
                            }
                        })
                })
            }
            NodeKind::PatVariant => {
                let (path, args) =
                    nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant"));
                (path != ExprId::ZERO)
                    .then_some(path)
                    .or_else(|| args.iter().find_map(|arg| self.pattern_anchor(arg)))
            }
            NodeKind::PatTuple => nodes
                .pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple"))
                .iter()
                .find_map(|item| self.pattern_anchor(item)),
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.pattern_anchor(inner)
            }
            _ => None,
        }
    }

    fn resolve_struct_pattern_type(&mut self, path: ExprId) -> Option<Ty<'db>> {
        if path == ExprId::ZERO {
            return None;
        }

        let nodes = self.function.node_store();
        let name = nodes.as_name(path)?;
        let symbol = nodes.name(name);
        let Some(ty) = self.resolve_type_in_node_scope(path, symbol) else {
            self.emit(DiagnosticKind::UnresolvedIdent(path));
            return None;
        };
        Some(ty)
    }

    fn resolve_variant_pattern(
        &mut self,
        path: ExprId,
        expected: &InferTy,
    ) -> Option<(InferTy, Vec<InferTy>, ExprId, bool)> {
        let nodes = self.function.node_store();
        let field_id = nodes.as_field(path)?;
        let (base, variant_name_expr) = nodes.field(field_id);
        let variant_name = nodes.as_name(variant_name_expr)?;
        let variant_sym = nodes.name(variant_name);

        let enum_ty = if base != ExprId::ZERO {
            let Some(base_name) = nodes.as_name(base) else {
                self.emit(DiagnosticKind::UnresolvedIdent(base));
                return None;
            };
            let base_sym = nodes.name(base_name);
            let Some(ty) = self.resolve_type_in_node_scope(base, base_sym) else {
                self.emit(DiagnosticKind::UnresolvedIdent(base));
                return None;
            };
            ty
        } else if let Some(ty) = Self::known_ty(expected) {
            ty
        } else if let Some(ty) = self.resolver.resolve_enum_variant(variant_sym) {
            ty
        } else {
            self.emit(DiagnosticKind::UnresolvedIdent(variant_name_expr));
            return None;
        };

        let TyKind::Enum(enum_ty_id) = enum_ty.kind(self.db) else {
            self.emit(DiagnosticKind::NotAnEnum(
                if base != ExprId::ZERO { base } else { variant_name_expr },
                enum_ty,
            ));
            return None;
        };

        let variants = enum_variants(self.db, *enum_ty_id);
        let Some((_, payload)) = variants.iter().find(|(name, _)| *name == variant_sym) else {
            self.emit(DiagnosticKind::UnresolvedIdent(variant_name_expr));
            return None;
        };

        Some((
            self.ty_to_infer_ty(enum_ty),
            payload.iter().map(|&ty| self.ty_to_infer_ty(ty)).collect(),
            variant_name_expr,
            variants.len() == 1,
        ))
    }

    fn infer_name_expr(&mut self, node: ExprId, lvl: usize) -> InferTy {
        let nodes = self.function.node_store();
        let name_id = nodes.as_name(node).expect("Name node mismatch");
        let path = nodes.name(name_id);

        let Some(resolution) = self.resolve_path_in_node_scope(node, path) else {
            if let Some(ty) = self.resolve_type_in_node_scope(node, path) {
                self.emit(DiagnosticKind::ExpectedValueFoundType(node, ty));
                return InferTy::Unknown;
            }
            self.emit(DiagnosticKind::UnresolvedIdent(node));
            return InferTy::Unknown;
        };

        match resolution {
            BindingId::Local(binding) | BindingId::Param(binding) => {
                self.binding_infer_ty(binding, lvl)
            }
            BindingId::RuntimeFunction(function) => {
                self.ty_to_infer_ty(function.function_ty(self.db))
            }
            BindingId::CompilerIntrinsic(intrinsic) => {
                self.emit(DiagnosticKind::CompilerIntrinsicMustBeCalled(node, intrinsic));
                InferTy::Unknown
            }
            BindingId::Function(function) => self.infer_function_binding_ty(function, lvl),
            BindingId::Struct(_) | BindingId::Enum(_) | BindingId::BuiltinType(_) => {
                let ty = self.resolver.ty_for_binding(resolution).expect("type binding");
                self.emit(DiagnosticKind::ExpectedValueFoundType(node, ty));
                InferTy::Unknown
            }
            BindingId::EnumVariant(_) => {
                self.emit(DiagnosticKind::UnresolvedIdent(node));
                InferTy::Unknown
            }
        }
    }

    fn binding_infer_ty(&mut self, binding: NameId, lvl: usize) -> InferTy {
        let key: ExprId = binding.into();
        match self.env.get(&key).cloned() {
            Some(scheme) => self.instantiate(&scheme, lvl),
            None => self.node_types.get(&key).cloned().unwrap_or_else(|| self.fresh_var(lvl)),
        }
    }

    fn infer_function_binding_ty(
        &mut self,
        function: FunctionLocation<'db>,
        lvl: usize,
    ) -> InferTy {
        let resolver = Resolver::new(self.db, function);
        let signature = function.signature(self.db);
        let sig_nodes = signature.nodes(self.db);
        let params = signature.params(self.db);
        let ret_type = signature.ret_type(self.db);
        let type_params = signature.type_params(self.db);

        let type_param_vars: FxHashMap<Symbol<'db>, InferTy> =
            type_params.iter().map(|&name| (name, self.fresh_var(lvl))).collect();

        let inputs: Vec<InferTy> = params
            .iter()
            .map(|&param| {
                let (_, ty) = sig_nodes.param(param);
                self.resolve_sig_type(ty, sig_nodes, &resolver, &type_param_vars, lvl)
            })
            .collect();

        let output = if ret_type == TyId::ZERO {
            InferTy::Tuple(Vec::new())
        } else {
            self.resolve_sig_type(ret_type, sig_nodes, &resolver, &type_param_vars, lvl)
        };

        InferTy::Function(inputs, Box::new(output))
    }

    fn compiler_intrinsic_call(
        &mut self,
        node: ExprId,
        callee: ExprId,
        args: &[ExprId],
        expected: Option<&InferTy>,
        lvl: usize,
    ) -> Option<InferTy> {
        let nodes = self.function.node_store();
        let name_id = nodes.as_name(callee)?;
        let symbol = nodes.name(name_id);
        let Some(BindingId::CompilerIntrinsic(intrinsic)) =
            self.resolve_path_in_node_scope(callee, symbol)
        else {
            return None;
        };

        Some(match intrinsic {
            CompilerIntrinsic::Comptime => self.typecheck_comptime_call(node, args, lvl),
            _ if intrinsic.is_reflection() => {
                self.typecheck_reflection_call(node, intrinsic, args, lvl)
            }
            _ => self.typecheck_unsafe_intrinsic_call(node, intrinsic, args, expected, lvl),
        })
    }

    fn typecheck_comptime_call(&mut self, node: ExprId, args: &[ExprId], lvl: usize) -> InferTy {
        if args.len() != 1 {
            for &arg in args {
                self.infer_expr(arg, lvl);
            }
            self.emit(DiagnosticKind::CallArityMismatch(node, 1, args.len()));
            return InferTy::Unknown;
        }

        let call_expr = args[0];
        let nodes = self.function.node_store();
        let Some(call_id) = nodes.as_call(call_expr) else {
            self.infer_expr(call_expr, lvl);
            self.emit(DiagnosticKind::InvalidComptimeCall(node));
            return InferTy::Unknown;
        };

        let (target_expr, call_args) = nodes.call(call_id);
        for arg in call_args.iter() {
            self.infer_expr(arg, lvl);
        }

        let Some(name_id) = nodes.as_name(target_expr) else {
            self.emit(DiagnosticKind::InvalidComptimeCall(node));
            return InferTy::Unknown;
        };
        let symbol = nodes.name(name_id);
        let Some(BindingId::Function(target)) =
            self.resolve_path_in_node_scope(target_expr, symbol)
        else {
            self.emit(DiagnosticKind::InvalidComptimeCall(node));
            return InferTy::Unknown;
        };

        let hir_function = target.hir_function(self.db).function(self.db);
        if !hir_function.is_comptime() {
            self.emit(DiagnosticKind::InvalidComptimeCall(node));
            return InferTy::Unknown;
        }

        if !call_args.is_empty() || !hir_function.params().is_empty() {
            self.emit(DiagnosticKind::ComptimeTargetMustBeZeroArg(node));
            return InferTy::Unknown;
        }

        if !hir_function.type_params().is_empty() {
            self.emit(DiagnosticKind::ComptimeTargetMustNotBeGeneric(node));
            return InferTy::Unknown;
        }

        let target_inference = target.infer(self.db);
        let return_ty = target_inference
            .type_of_node(hir_function.body())
            .unwrap_or_else(|| Ty::new(self.db, TyKind::Tuple(Vec::new())));
        if !comptime_result_supported(self.db, return_ty) {
            self.emit(DiagnosticKind::ComptimeTargetMustReturnSupportedType(node, return_ty));
            return InferTy::Unknown;
        }

        self.ty_to_infer_ty(return_ty)
    }

    fn typecheck_reflection_call(
        &mut self,
        node: ExprId,
        intrinsic: CompilerIntrinsic,
        args: &[ExprId],
        lvl: usize,
    ) -> InferTy {
        if !self.function.is_comptime() {
            self.emit(DiagnosticKind::ReflectionOnlyInComptime(node, intrinsic));
        }

        let result = match intrinsic {
            CompilerIntrinsic::TypeName
            | CompilerIntrinsic::FieldName
            | CompilerIntrinsic::VariantName
            | CompilerIntrinsic::FunctionParamTypeName
            | CompilerIntrinsic::FunctionReturnTypeName => self.string_infer_ty(),
            CompilerIntrinsic::FieldCount
            | CompilerIntrinsic::VariantCount
            | CompilerIntrinsic::FunctionParamCount => self.int_infer_ty(),
            CompilerIntrinsic::Comptime
            | CompilerIntrinsic::StackAlloc
            | CompilerIntrinsic::PtrRead
            | CompilerIntrinsic::PtrWrite
            | CompilerIntrinsic::PtrAdd
            | CompilerIntrinsic::StrBytes
            | CompilerIntrinsic::StrFromUtf8Unchecked
            | CompilerIntrinsic::ArrayMutBytes => unreachable!("handled above"),
        };

        let expected_arity = match intrinsic {
            CompilerIntrinsic::FieldName
            | CompilerIntrinsic::VariantName
            | CompilerIntrinsic::FunctionParamTypeName => 2,
            CompilerIntrinsic::TypeName
            | CompilerIntrinsic::FieldCount
            | CompilerIntrinsic::VariantCount
            | CompilerIntrinsic::FunctionParamCount
            | CompilerIntrinsic::FunctionReturnTypeName => 1,
            CompilerIntrinsic::Comptime
            | CompilerIntrinsic::StackAlloc
            | CompilerIntrinsic::PtrRead
            | CompilerIntrinsic::PtrWrite
            | CompilerIntrinsic::PtrAdd
            | CompilerIntrinsic::StrBytes
            | CompilerIntrinsic::StrFromUtf8Unchecked
            | CompilerIntrinsic::ArrayMutBytes => unreachable!("handled above"),
        };
        if args.len() != expected_arity {
            for &arg in args {
                self.infer_expr(arg, lvl);
            }
            self.emit(DiagnosticKind::CallArityMismatch(node, expected_arity, args.len()));
            return result;
        }

        match intrinsic {
            CompilerIntrinsic::TypeName => {
                let _ = self.resolve_reflection_type_arg(args[0], intrinsic, "a named type");
            }
            CompilerIntrinsic::FieldCount | CompilerIntrinsic::FieldName => {
                let Some(ty) =
                    self.resolve_reflection_type_arg(args[0], intrinsic, "a named struct type")
                else {
                    return result;
                };
                if !matches!(ty.kind(self.db), TyKind::Struct(_)) {
                    self.emit(DiagnosticKind::InvalidReflectionTarget(
                        args[0],
                        intrinsic,
                        "a named struct type",
                    ));
                }
                if args.len() > 1 {
                    let int_ty = self.int_infer_ty();
                    self.check_expr(args[1], &int_ty, lvl);
                }
            }
            CompilerIntrinsic::VariantCount | CompilerIntrinsic::VariantName => {
                let Some(ty) =
                    self.resolve_reflection_type_arg(args[0], intrinsic, "a named enum type")
                else {
                    return result;
                };
                if !matches!(ty.kind(self.db), TyKind::Enum(_)) {
                    self.emit(DiagnosticKind::InvalidReflectionTarget(
                        args[0],
                        intrinsic,
                        "a named enum type",
                    ));
                }
                if args.len() > 1 {
                    let int_ty = self.int_infer_ty();
                    self.check_expr(args[1], &int_ty, lvl);
                }
            }
            CompilerIntrinsic::FunctionParamCount
            | CompilerIntrinsic::FunctionParamTypeName
            | CompilerIntrinsic::FunctionReturnTypeName => {
                let _ = self.resolve_reflection_function_arg(
                    args[0],
                    intrinsic,
                    "a top-level function name",
                );
                if args.len() > 1 {
                    let int_ty = self.int_infer_ty();
                    self.check_expr(args[1], &int_ty, lvl);
                }
            }
            CompilerIntrinsic::Comptime
            | CompilerIntrinsic::StackAlloc
            | CompilerIntrinsic::PtrRead
            | CompilerIntrinsic::PtrWrite
            | CompilerIntrinsic::PtrAdd
            | CompilerIntrinsic::StrBytes
            | CompilerIntrinsic::StrFromUtf8Unchecked
            | CompilerIntrinsic::ArrayMutBytes => unreachable!("handled above"),
        }

        result
    }

    fn typecheck_unsafe_intrinsic_call(
        &mut self,
        node: ExprId,
        intrinsic: CompilerIntrinsic,
        args: &[ExprId],
        expected: Option<&InferTy>,
        lvl: usize,
    ) -> InferTy {
        if intrinsic.requires_unsafe() {
            self.require_unsafe_context(node);
        }

        match intrinsic {
            CompilerIntrinsic::StackAlloc => {
                if args.len() != 1 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 1, args.len()));
                    return InferTy::Unknown;
                }

                self.check_expr(args[0], &self.exact_u32_infer_ty(), lvl);
                match expected.and_then(Self::known_ty) {
                    Some(ty) if matches!(ty.kind(self.db), TyKind::Pointer { .. }) => {
                        self.ty_to_infer_ty(ty)
                    }
                    _ => {
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicResult(
                            node,
                            intrinsic,
                            "an expected raw pointer result type",
                        ));
                        InferTy::Unknown
                    }
                }
            }
            CompilerIntrinsic::PtrRead => {
                if args.len() != 1 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 1, args.len()));
                    return InferTy::Unknown;
                }

                let ptr_ty = self.infer_expr(args[0], lvl);
                match Self::known_ty(&ptr_ty).map(|ty| ty.kind(self.db)) {
                    Some(TyKind::Pointer { pointee, .. }) => self.ty_to_infer_ty(*pointee),
                    Some(_) => {
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicArgument(
                            args[0],
                            intrinsic,
                            "a raw pointer argument",
                        ));
                        InferTy::Unknown
                    }
                    None => InferTy::Unknown,
                }
            }
            CompilerIntrinsic::PtrWrite => {
                if args.len() != 2 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 2, args.len()));
                    return Self::unit_infer_ty();
                }

                let ptr_ty = self.infer_expr(args[0], lvl);
                match Self::known_ty(&ptr_ty).map(|ty| ty.kind(self.db)) {
                    Some(TyKind::Pointer { mutable: true, pointee }) => {
                        let pointee = self.ty_to_infer_ty(*pointee);
                        self.check_expr(args[1], &pointee, lvl);
                    }
                    Some(TyKind::Pointer { .. }) => {
                        self.infer_expr(args[1], lvl);
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicArgument(
                            args[0],
                            intrinsic,
                            "a mutable raw pointer as the first argument",
                        ));
                    }
                    Some(_) => {
                        self.infer_expr(args[1], lvl);
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicArgument(
                            args[0],
                            intrinsic,
                            "a raw pointer as the first argument",
                        ));
                    }
                    None => {
                        self.infer_expr(args[1], lvl);
                    }
                }
                Self::unit_infer_ty()
            }
            CompilerIntrinsic::PtrAdd => {
                if args.len() != 2 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 2, args.len()));
                    return InferTy::Unknown;
                }

                let ptr_ty = self.infer_expr(args[0], lvl);
                self.check_expr(args[1], &self.exact_u32_infer_ty(), lvl);
                match Self::known_ty(&ptr_ty).map(|ty| ty.kind(self.db)) {
                    Some(TyKind::Pointer { .. }) => ptr_ty,
                    Some(_) => {
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicArgument(
                            args[0],
                            intrinsic,
                            "a raw pointer as the first argument",
                        ));
                        InferTy::Unknown
                    }
                    None => InferTy::Unknown,
                }
            }
            CompilerIntrinsic::StrBytes => {
                if args.len() != 1 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 1, args.len()));
                    return InferTy::Unknown;
                }

                self.check_expr(args[0], &self.string_infer_ty(), lvl);
                self.byte_view_infer_ty(false, expected)
            }
            CompilerIntrinsic::StrFromUtf8Unchecked => {
                if args.len() != 2 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 2, args.len()));
                    return InferTy::Unknown;
                }

                let ptr_ty = self.infer_expr(args[0], lvl);
                match Self::known_ty(&ptr_ty).map(|ty| ty.kind(self.db)) {
                    Some(TyKind::Pointer { pointee, .. })
                        if matches!(pointee.kind(self.db), TyKind::ExactInt(ExactInt::U8)) => {}
                    Some(TyKind::Pointer { .. }) => {
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicArgument(
                            args[0],
                            intrinsic,
                            "a raw `*const u8` or `*mut u8` pointer as the first argument",
                        ));
                    }
                    Some(_) => {
                        self.emit(DiagnosticKind::InvalidUnsafeIntrinsicArgument(
                            args[0],
                            intrinsic,
                            "a raw `*const u8` or `*mut u8` pointer as the first argument",
                        ));
                    }
                    None => {}
                }

                self.check_expr(args[1], &self.exact_u32_infer_ty(), lvl);
                self.string_infer_ty()
            }
            CompilerIntrinsic::ArrayMutBytes => {
                if args.len() != 1 {
                    for &arg in args {
                        self.infer_expr(arg, lvl);
                    }
                    self.emit(DiagnosticKind::CallArityMismatch(node, 1, args.len()));
                    return InferTy::Unknown;
                }

                self.check_expr(args[0], &self.array_u8_infer_ty(), lvl);
                self.byte_view_infer_ty(true, expected)
            }
            CompilerIntrinsic::Comptime
            | CompilerIntrinsic::TypeName
            | CompilerIntrinsic::FieldCount
            | CompilerIntrinsic::FieldName
            | CompilerIntrinsic::VariantCount
            | CompilerIntrinsic::VariantName
            | CompilerIntrinsic::FunctionParamCount
            | CompilerIntrinsic::FunctionParamTypeName
            | CompilerIntrinsic::FunctionReturnTypeName => unreachable!("handled above"),
        }
    }

    fn resolve_reflection_type_arg(
        &mut self,
        arg: ExprId,
        intrinsic: CompilerIntrinsic,
        expected: &'static str,
    ) -> Option<Ty<'db>> {
        let nodes = self.function.node_store();
        let name_id = nodes
            .as_name(arg)
            .ok_or_else(|| {
                self.emit(DiagnosticKind::InvalidReflectionTarget(arg, intrinsic, expected));
            })
            .ok()?;
        let symbol = nodes.name(name_id);
        if let Some(ty) = self.resolve_type_in_node_scope(arg, symbol) {
            Some(ty)
        } else {
            match self.resolve_path_in_node_scope(arg, symbol) {
                Some(BindingId::Local(_))
                | Some(BindingId::Param(_))
                | Some(BindingId::CompilerIntrinsic(_))
                | Some(BindingId::RuntimeFunction(_))
                | Some(BindingId::Function(_))
                | Some(BindingId::EnumVariant(_))
                | Some(BindingId::Struct(_))
                | Some(BindingId::Enum(_))
                | Some(BindingId::BuiltinType(_)) => {
                    self.emit(DiagnosticKind::InvalidReflectionTarget(arg, intrinsic, expected));
                    None
                }
                None => {
                    self.emit(DiagnosticKind::UnresolvedIdent(arg));
                    None
                }
            }
        }
    }

    fn resolve_reflection_function_arg(
        &mut self,
        arg: ExprId,
        intrinsic: CompilerIntrinsic,
        expected: &'static str,
    ) -> Option<FunctionLocation<'db>> {
        let nodes = self.function.node_store();
        let name_id = nodes
            .as_name(arg)
            .ok_or_else(|| {
                self.emit(DiagnosticKind::InvalidReflectionTarget(arg, intrinsic, expected));
            })
            .ok()?;
        let symbol = nodes.name(name_id);
        match self.resolve_path_in_node_scope(arg, symbol) {
            Some(BindingId::Function(function)) => Some(function),
            Some(BindingId::Local(_))
            | Some(BindingId::Param(_))
            | Some(BindingId::CompilerIntrinsic(_))
            | Some(BindingId::RuntimeFunction(_))
            | Some(BindingId::EnumVariant(_))
            | Some(BindingId::Struct(_))
            | Some(BindingId::Enum(_))
            | Some(BindingId::BuiltinType(_)) => {
                self.emit(DiagnosticKind::InvalidReflectionTarget(arg, intrinsic, expected));
                None
            }
            None => {
                self.emit(DiagnosticKind::UnresolvedIdent(arg));
                None
            }
        }
    }

    fn infer_bare_enum_variant(
        &mut self,
        field_name: Symbol<'db>,
        field_name_expr: ExprId,
        lvl: usize,
    ) -> InferTy {
        let enum_ty = self.fresh_var(lvl.saturating_sub(1));
        if let InferTy::Var(enum_var) = enum_ty.clone() {
            self.variant_constraints.push(VariantConstraint {
                enum_var,
                variant: field_name,
                payload: Vec::new(),
                name_node: field_name_expr,
            });
        }
        enum_ty
    }

    fn infer_bare_variant_call(
        &mut self,
        callee: ExprId,
        args: &[ExprId],
        lvl: usize,
    ) -> Option<InferTy> {
        let nodes = self.function.node_store();
        if nodes.node_kind(callee) != NodeKind::Field {
            return None;
        }

        let field = nodes.field(nodes.as_field(callee).expect("Field node mismatch"));
        let (field_expr, field_name_expr) = field;
        if field_expr != ExprId::ZERO {
            return None;
        }

        let field_name_id = nodes.as_name(field_name_expr)?;
        let field_name = nodes.name(field_name_id);
        let payload: Vec<(ExprId, InferTy)> =
            args.iter().map(|&arg| (arg, self.infer_expr(arg, lvl))).collect();

        let enum_ty = self.fresh_var(lvl.saturating_sub(1));
        if let InferTy::Var(enum_var) = enum_ty.clone() {
            self.variant_constraints.push(VariantConstraint {
                enum_var,
                variant: field_name,
                payload,
                name_node: field_name_expr,
            });
            Some(enum_ty)
        } else {
            None
        }
    }

    fn infer_method_call(
        &mut self,
        node: ExprId,
        callee: ExprId,
        args: &[ExprId],
        lvl: usize,
    ) -> Option<InferTy> {
        let nodes = self.function.node_store();
        let field = nodes.as_field(callee)?;
        let (receiver_expr, field_name_expr) = nodes.field(field);
        if receiver_expr == ExprId::ZERO {
            return None;
        }

        if nodes.node_kind(receiver_expr) == NodeKind::Name
            && let Some(name_id) = nodes.as_name(receiver_expr)
            && self.resolve_type_in_node_scope(receiver_expr, nodes.name(name_id)).is_some()
        {
            return None;
        }

        let field_name_id = nodes.as_name(field_name_expr)?;
        let field_name = nodes.name(field_name_id);
        let receiver_ty = self.infer_expr(receiver_expr, lvl);
        let receiver_known = Self::known_ty(&receiver_ty)?;
        let method = resolve_method_for_receiver(self.db, receiver_known, field_name)?;
        let callee_ty = self.infer_function_binding_ty(method.function, lvl);
        self.node_types.insert(callee, callee_ty.clone());

        if method.function.hir_function(self.db).function(self.db).is_unsafe() {
            self.require_unsafe_context(node);
        }

        let InferTy::Function(inputs, output) = callee_ty else {
            return None;
        };
        let method_arity = inputs.len().saturating_sub(1);
        if method_arity != args.len() {
            for &arg in args {
                self.infer_expr(arg, lvl);
            }
            self.emit(DiagnosticKind::CallArityMismatch(node, method_arity, args.len()));
            return Some(*output);
        }

        if let Some(receiver_input) = inputs.first() {
            if self.function_param_is_mutable(method.function, 0) {
                self.check_mutable_argument(receiver_expr, receiver_input, lvl);
            } else {
                let _ = self.constrain_top(&receiver_ty, receiver_input);
            }
        }
        for (index, (&arg, input_ty)) in args.iter().zip(inputs.iter().skip(1)).enumerate() {
            if self.function_param_is_mutable(method.function, index + 1) {
                self.check_mutable_argument(arg, input_ty, lvl);
            } else {
                self.check_expr(arg, input_ty, lvl);
            }
        }

        Some(*output)
    }

    fn function_param_is_mutable(&self, function: FunctionLocation<'db>, index: usize) -> bool {
        function
            .source(self.db)
            .params()
            .and_then(|params| params.iter().nth(index))
            .is_some_and(|param| param.is_mutable())
    }

    fn check_mutable_argument(&mut self, arg: ExprId, input_ty: &InferTy, lvl: usize) {
        let _ = match self.resolve_place(arg, lvl) {
            PlaceResolution::Mutable(_) => self.check_expr(arg, input_ty, lvl),
            PlaceResolution::Immutable | PlaceResolution::Invalid => {
                self.emit(DiagnosticKind::MutableArgumentRequiresPlace(arg));
                self.check_expr(arg, input_ty, lvl)
            }
        };
    }

    fn infer_type_qualified_enum_variant(
        &mut self,
        field_expr: ExprId,
        field_name: Symbol<'db>,
        field_name_expr: ExprId,
    ) -> Option<InferTy> {
        let nodes = self.function.node_store();
        let base_name_id = nodes.as_name(field_expr).expect("field base should be Name");
        let base_name = nodes.name(base_name_id);
        let ty = self.resolve_type_in_node_scope(field_expr, base_name)?;

        if let Some(variant_ty) = self.enum_variant_infer_ty(ty, field_name) {
            return Some(variant_ty);
        }

        if matches!(ty.kind(self.db), TyKind::Enum(_)) {
            self.emit(DiagnosticKind::UnresolvedIdent(field_name_expr));
        } else {
            self.emit(DiagnosticKind::ExpectedValueFoundType(field_expr, ty));
        }

        Some(InferTy::Unknown)
    }

    fn infer_field_expr(
        &mut self,
        node: ExprId,
        field_expr: ExprId,
        field_name_expr: ExprId,
        _expected: Option<&InferTy>,
        lvl: usize,
    ) -> InferTy {
        let nodes = self.function.node_store();
        let Some(field_name_id) = nodes.as_name(field_name_expr) else {
            self.emit(DiagnosticKind::UnresolvedIdent(node));
            return InferTy::Unknown;
        };
        let field_name = nodes.name(field_name_id);

        if field_expr == ExprId::ZERO {
            return self.infer_bare_enum_variant(field_name, field_name_expr, lvl);
        }

        if nodes.node_kind(field_expr) == NodeKind::Name
            && let Some(variant_ty) =
                self.infer_type_qualified_enum_variant(field_expr, field_name, field_name_expr)
        {
            return variant_ty;
        }

        let base_ty = self.infer_expr(field_expr, lvl);
        match base_ty {
            InferTy::Unknown => InferTy::Unknown,
            InferTy::Record(fields) => fields
                .iter()
                .find(|(name_bits, _)| *name_bits == Self::symbol_to_bits(field_name))
                .map_or_else(
                    || {
                        self.emit(DiagnosticKind::UnknownStructField(node, field_name));
                        InferTy::Unknown
                    },
                    |(_, ty)| ty.clone(),
                ),
            InferTy::Known(bits) => {
                let ty = Ty::from_id(salsa::Id::from_bits(bits));
                if let TyKind::Struct(struct_ty) | TyKind::ExternStruct(struct_ty) =
                    ty.kind(self.db)
                {
                    let fields = struct_fields(self.db, *struct_ty);
                    if let Some((_, field_ty)) = fields.iter().find(|(name, _)| *name == field_name)
                    {
                        self.ty_to_infer_ty(*field_ty)
                    } else {
                        self.emit(DiagnosticKind::UnknownStructField(node, field_name));
                        InferTy::Unknown
                    }
                } else {
                    self.emit(DiagnosticKind::NotAStruct(node, ty));
                    InferTy::Unknown
                }
            }
            InferTy::Var(_) => {
                let result = self.fresh_var(lvl);
                let field_req =
                    InferTy::Record(vec![(Self::symbol_to_bits(field_name), result.clone())]);
                let _ = self.constrain_top(&base_ty, &field_req);
                result
            }
            other => {
                self.emit(DiagnosticKind::NotAStruct(node, self.diagnostic_ty(&other)));
                InferTy::Unknown
            }
        }
    }

    fn collect_enum_candidates(&self, enum_var: VarId) -> FxHashSet<u64> {
        let mut candidates: FxHashSet<u64> = FxHashSet::default();
        let mut seen_vars: FxHashSet<VarId> = FxHashSet::default();
        let mut stack: Vec<InferTy> = vec![InferTy::Var(enum_var)];

        while let Some(ty) = stack.pop() {
            match ty {
                InferTy::Var(var) => {
                    if !seen_vars.insert(var) {
                        continue;
                    }
                    stack.extend(self.vars[var].lower_bounds.iter().cloned());
                    stack.extend(self.vars[var].upper_bounds.iter().cloned());
                }
                InferTy::Known(bits) => {
                    let ty = Ty::from_id(salsa::Id::from_bits(bits));
                    if matches!(ty.kind(self.db), TyKind::Enum(_)) {
                        candidates.insert(bits);
                    }
                }
                InferTy::Function(inputs, output) => {
                    stack.extend(inputs);
                    stack.push(*output);
                }
                InferTy::Array(item) => stack.push(*item),
                InferTy::Tuple(items) => stack.extend(items),
                InferTy::Record(fields) => stack.extend(fields.into_iter().map(|(_, ty)| ty)),
                InferTy::Union(items) | InferTy::Inter(items) => stack.extend(items),
                InferTy::Unknown => {}
            }
        }

        candidates
    }

    fn solve_variant_constraints(&mut self, emit_unresolved: bool) -> bool {
        let constraints = std::mem::take(&mut self.variant_constraints);
        let total = constraints.len();
        let mut pending = Vec::new();

        for constraint in constraints {
            if !self.solve_variant_constraint(&constraint) {
                if emit_unresolved {
                    self.emit(DiagnosticKind::UnresolvedIdent(constraint.name_node));
                } else {
                    pending.push(constraint);
                }
            }
        }

        self.variant_constraints = pending;
        self.variant_constraints.len() != total
    }

    fn solve_variant_constraint(&mut self, constraint: &VariantConstraint<'db>) -> bool {
        let candidates = self.collect_enum_candidates(constraint.enum_var);
        let mut narrowed = Vec::new();

        for candidate_bits in &candidates {
            let enum_ty = Ty::from_id(salsa::Id::from_bits(*candidate_bits));
            let Some(variant_ty) = self.enum_variant_infer_ty(enum_ty, constraint.variant) else {
                continue;
            };

            let (inputs, output) = match variant_ty {
                InferTy::Known(_) => (Vec::new(), InferTy::Known(*candidate_bits)),
                InferTy::Function(inputs, output) => (inputs, *output),
                _ => continue,
            };

            if inputs.len() == constraint.payload.len() {
                narrowed.push((inputs, output));
            }
        }

        if narrowed.is_empty() {
            if candidates.len() == 1 {
                self.emit(DiagnosticKind::UnresolvedIdent(constraint.name_node));
                return true;
            }
            return false;
        }

        if narrowed.len() != 1 {
            return false;
        }

        let (inputs, output) = narrowed.pop().expect("one narrowed candidate expected");

        for ((arg_node, arg_ty), input_ty) in constraint.payload.iter().zip(inputs.iter()) {
            if self.constrain_top(arg_ty, input_ty).is_err() {
                let actual = self.diagnostic_ty(arg_ty);
                let expected = self.diagnostic_ty(input_ty);
                if actual != expected {
                    self.emit(DiagnosticKind::TypeMismatch(*arg_node, actual, expected));
                }
                return true;
            }
        }

        let enum_var_ty = InferTy::Var(constraint.enum_var);
        let _ = self.constrain_eq_top(&enum_var_ty, &output);
        true
    }

    fn coalesce_type_raw(&self, ty: &InferTy, polarity: Polarity) -> Ty<'db> {
        let mut recursive: FxHashMap<(VarId, Polarity), u32> = FxHashMap::default();
        let mut in_process: FxHashSet<(VarId, Polarity)> = FxHashSet::default();
        // Keep presentation-time recursive binders disjoint from inference vars.
        let mut next_present_var = self.vars.len() as u32;
        self.coalesce_raw(ty, polarity, &mut in_process, &mut recursive, &mut next_present_var)
    }

    // Presentation-only conversion: internal InferTy graph -> user-facing Ty plus
    // cleanup.
    fn present_type(&self, ty: &InferTy, polarity: Polarity) -> Ty<'db> {
        simplify(self.db, self.coalesce_type_raw(ty, polarity), &self.preserved_type_vars)
    }

    fn coalesce_type_for_missing_param(&self, ty: &InferTy) -> Ty<'db> {
        let positive = self.present_type(ty, Polarity::Positive);
        if !matches!(positive.kind(self.db), TyKind::Unknown) {
            return positive;
        }

        let negative = self.present_type(ty, Polarity::Negative);

        if matches!(negative.kind(self.db), TyKind::Unknown) { positive } else { negative }
    }

    // Final "presentation" phase: convert inferred InferTy graph into user-facing
    // Ty syntax. This is intentionally separate from constraint solving.
    fn finalize_user_types(&mut self) {
        for (node, infer_ty) in &self.node_types {
            let ty = if self.missing_param_nodes.contains(node) {
                self.coalesce_type_for_missing_param(infer_ty)
            } else {
                self.present_type(infer_ty, Polarity::Positive)
            };
            self.inference.type_of_node.insert(*node, ty);
        }
        for (pattern, infer_ty) in &self.selected_union_members {
            let ty = self.present_type(infer_ty, Polarity::Positive);
            self.inference.selected_union_members.insert(*pattern, ty);
        }
        for (pattern, infer_ty) in &self.matched_typed_patterns {
            let ty = self.present_type(infer_ty, Polarity::Positive);
            self.inference.matched_typed_patterns.insert(*pattern, ty);
        }
    }

    fn coalesce_raw(
        &self,
        ty: &InferTy,
        polarity: Polarity,
        in_process: &mut FxHashSet<(VarId, Polarity)>,
        recursive: &mut FxHashMap<(VarId, Polarity), u32>,
        next_present_var: &mut u32,
    ) -> Ty<'db> {
        match ty {
            InferTy::Known(bits) => Ty::from_id(salsa::Id::from_bits(*bits)),
            InferTy::Function(inputs, output) => {
                let inputs = inputs
                    .iter()
                    .map(|t| {
                        self.coalesce_raw(
                            t,
                            polarity.flip(),
                            in_process,
                            recursive,
                            next_present_var,
                        )
                    })
                    .collect();
                let output =
                    self.coalesce_raw(output, polarity, in_process, recursive, next_present_var);
                Ty::new(self.db, TyKind::Function { inputs, output })
            }
            InferTy::Array(item) => {
                let item =
                    self.coalesce_raw(item, polarity, in_process, recursive, next_present_var);
                Ty::new(self.db, TyKind::Array(item))
            }
            InferTy::Tuple(items) => {
                let items = items
                    .iter()
                    .map(|t| {
                        self.coalesce_raw(t, polarity, in_process, recursive, next_present_var)
                    })
                    .collect();
                Ty::new(self.db, TyKind::Tuple(items))
            }
            InferTy::Record(fields) => {
                let fields = fields
                    .iter()
                    .map(|(name_bits, ty)| {
                        (
                            Self::symbol_from_bits(*name_bits),
                            self.coalesce_raw(
                                ty,
                                polarity,
                                in_process,
                                recursive,
                                next_present_var,
                            ),
                        )
                    })
                    .collect();
                Ty::new(self.db, TyKind::Record(fields))
            }
            InferTy::Union(items) => {
                let items = items
                    .iter()
                    .map(|t| {
                        self.coalesce_raw(t, polarity, in_process, recursive, next_present_var)
                    })
                    .collect();
                Ty::new(self.db, TyKind::Union(items))
            }
            InferTy::Inter(items) => {
                let items = items
                    .iter()
                    .map(|t| {
                        self.coalesce_raw(t, polarity, in_process, recursive, next_present_var)
                    })
                    .collect();
                Ty::new(self.db, TyKind::Inter(items))
            }
            InferTy::Var(v) => {
                let v = *v;
                let key = (v, polarity);

                if in_process.contains(&key) {
                    let rec_var = *recursive.entry(key).or_insert_with(|| {
                        let id = *next_present_var;
                        *next_present_var += 1;
                        id
                    });
                    return Ty::new(self.db, TyKind::Var(rec_var));
                }

                let bounds = match polarity {
                    Polarity::Positive => self.vars[v].lower_bounds.clone(),
                    Polarity::Negative => self.vars[v].upper_bounds.clone(),
                };

                in_process.insert(key);
                let bound_types: Vec<Ty<'db>> = bounds
                    .iter()
                    .map(|b| {
                        self.coalesce_raw(b, polarity, in_process, recursive, next_present_var)
                    })
                    .collect();
                in_process.remove(&key);

                let var_ty = Ty::new(self.db, TyKind::Var(v as u32));

                let res = match polarity {
                    Polarity::Positive => {
                        if bound_types.is_empty() {
                            var_ty
                        } else {
                            let mut items = Vec::with_capacity(1 + bound_types.len());
                            items.push(var_ty);
                            items.extend(bound_types);
                            Ty::new(self.db, TyKind::Union(items))
                        }
                    }
                    Polarity::Negative => {
                        if bound_types.is_empty() {
                            var_ty
                        } else {
                            let mut items = Vec::with_capacity(1 + bound_types.len());
                            items.push(var_ty);
                            items.extend(bound_types);
                            Ty::new(self.db, TyKind::Inter(items))
                        }
                    }
                };

                if let Some(&rec_var) = recursive.get(&key) {
                    Ty::new(self.db, TyKind::Rec(rec_var, res))
                } else {
                    res
                }
            }
            InferTy::Unknown => Ty::new(self.db, TyKind::Unknown),
        }
    }

    /// Resolve a type annotation to an InferTy, checking type_param_env for
    /// type parameters.
    fn resolve_type_to_infer_ty(&mut self, ty: TyId) -> Option<InferTy> {
        if ty == TyId::ZERO {
            return None;
        }

        let nodes = self.function.node_store();

        if let Some(tuple_id) = nodes.as_type_tuple(ty) {
            let items: Vec<InferTy> = nodes
                .type_tuple(tuple_id)
                .iter()
                .map(|item| self.resolve_type_to_infer_ty(item).unwrap_or(InferTy::Unknown))
                .collect();
            return Some(InferTy::Tuple(items));
        }

        if let Some(array_id) = nodes.as_type_array(ty) {
            let (item_ty, _) = nodes.type_array(array_id);
            let item = self.resolve_type_to_infer_ty(item_ty).unwrap_or(InferTy::Unknown);
            return Some(InferTy::Array(Box::new(item)));
        }

        if let Some(ptr_id) = nodes.as_type_ptr_const(ty) {
            let (item_ty, _) = nodes.type_ptr_const(ptr_id);
            let item = self.resolve_type_to_infer_ty(item_ty).map(|item| self.diagnostic_ty(&item));
            return Some(self.infer_ty_from_kind(TyKind::Pointer {
                mutable: false,
                pointee: item.unwrap_or_else(|| Ty::new(self.db, TyKind::Unknown)),
            }));
        }

        if let Some(ptr_id) = nodes.as_type_ptr_mut(ty) {
            let (item_ty, _) = nodes.type_ptr_mut(ptr_id);
            let item = self.resolve_type_to_infer_ty(item_ty).map(|item| self.diagnostic_ty(&item));
            return Some(self.infer_ty_from_kind(TyKind::Pointer {
                mutable: true,
                pointee: item.unwrap_or_else(|| Ty::new(self.db, TyKind::Unknown)),
            }));
        }

        if let Some(function_id) = nodes.as_type_function(ty) {
            let (inputs_ty, output_ty) = nodes.type_function(function_id);
            let inputs = if let Some(tuple_id) = nodes.as_type_tuple(inputs_ty) {
                nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|item| self.resolve_type_to_infer_ty(item).unwrap_or(InferTy::Unknown))
                    .collect()
            } else if inputs_ty == TyId::ZERO {
                Vec::new()
            } else {
                vec![self.resolve_type_to_infer_ty(inputs_ty).unwrap_or(InferTy::Unknown)]
            };
            let output = self.resolve_type_to_infer_ty(output_ty).unwrap_or(InferTy::Unknown);
            return Some(InferTy::Function(inputs, Box::new(output)));
        }

        if let Some(union_id) = nodes.as_type_union(ty) {
            let (lhs_ty, rhs_ty) = nodes.type_union(union_id);
            let lhs = self.resolve_type_to_infer_ty(lhs_ty).unwrap_or(InferTy::Unknown);
            let rhs = self.resolve_type_to_infer_ty(rhs_ty).unwrap_or(InferTy::Unknown);
            return Some(Self::mk_union(lhs, rhs));
        }

        if let Some(inter_id) = nodes.as_type_inter(ty) {
            let (lhs_ty, rhs_ty) = nodes.type_inter(inter_id);
            let lhs = self.resolve_type_to_infer_ty(lhs_ty).unwrap_or(InferTy::Unknown);
            let rhs = self.resolve_type_to_infer_ty(rhs_ty).unwrap_or(InferTy::Unknown);
            return Some(Self::mk_inter(lhs, rhs));
        }

        if let Some(record_id) = nodes.as_type_record(ty) {
            let fields = nodes
                .type_record(record_id)
                .iter()
                .filter_map(|field_ty| {
                    let field_id = nodes.as_type_field(field_ty)?;
                    let (name_id, field_ty) = nodes.type_field(field_id);
                    let name = nodes.name(name_id);
                    let field_ty =
                        self.resolve_type_to_infer_ty(field_ty).unwrap_or(InferTy::Unknown);
                    Some((Self::symbol_to_bits(name), field_ty))
                })
                .collect();
            return Some(InferTy::Record(fields));
        }

        if let Some(type_apply) = nodes.as_type_apply(ty) {
            let (path_ty, args_ty) = nodes.type_apply(type_apply);
            let base = self.resolve_type_to_infer_ty(path_ty).unwrap_or(InferTy::Unknown);
            let args = if let Some(tuple_id) = nodes.as_type_tuple(args_ty) {
                nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|arg| {
                        self.resolve_type_to_infer_ty(arg).map_or_else(
                            || Ty::new(self.db, TyKind::Unknown),
                            |arg| self.diagnostic_ty(&arg),
                        )
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };

            return match Self::known_ty(&base)
                .and_then(|base| instantiate_nominal_type(self.db, base, args))
            {
                Some(ty) => Some(self.ty_to_infer_ty(ty)),
                None => Some(InferTy::Unknown),
            };
        }

        if let Some(type_path) = nodes.as_type_path(ty) {
            let name = nodes.type_ref(type_path);
            if let Some(var) = self.type_param_env.get(&name) {
                return Some(var.clone());
            }

            let guard = self.resolver.scopes_for_type(ty);
            let resolved = if let Some(ty) = self
                .resolver
                .resolve_type_binding(name)
                .and_then(|binding| self.resolver.ty_for_binding(binding))
            {
                Some(self.ty_to_infer_ty(ty))
            } else {
                self.emit(DiagnosticKind::UnresolvedType(ty, name));
                Some(InferTy::Unknown)
            };
            self.resolver.reset(guard);
            return resolved;
        }

        Some(InferTy::Unknown)
    }

    fn resolve_sig_type(
        &mut self,
        ty: TyId,
        sig_nodes: &NodeStore<'db>,
        resolver: &Resolver<'db>,
        type_param_vars: &FxHashMap<Symbol<'db>, InferTy>,
        lvl: usize,
    ) -> InferTy {
        if ty == TyId::ZERO {
            return self.fresh_var(lvl);
        }

        if let Some(tuple_id) = sig_nodes.as_type_tuple(ty) {
            let items: Vec<InferTy> = sig_nodes
                .type_tuple(tuple_id)
                .iter()
                .map(|item| self.resolve_sig_type(item, sig_nodes, resolver, type_param_vars, lvl))
                .collect();
            return InferTy::Tuple(items);
        }

        if let Some(array_id) = sig_nodes.as_type_array(ty) {
            let (item_ty, _) = sig_nodes.type_array(array_id);
            let item = self.resolve_sig_type(item_ty, sig_nodes, resolver, type_param_vars, lvl);
            return InferTy::Array(Box::new(item));
        }

        if let Some(ptr_id) = sig_nodes.as_type_ptr_const(ty) {
            let (item_ty, _) = sig_nodes.type_ptr_const(ptr_id);
            let item = self.resolve_sig_type(item_ty, sig_nodes, resolver, type_param_vars, lvl);
            return self.infer_ty_from_kind(TyKind::Pointer {
                mutable: false,
                pointee: self.diagnostic_ty(&item),
            });
        }

        if let Some(ptr_id) = sig_nodes.as_type_ptr_mut(ty) {
            let (item_ty, _) = sig_nodes.type_ptr_mut(ptr_id);
            let item = self.resolve_sig_type(item_ty, sig_nodes, resolver, type_param_vars, lvl);
            return self.infer_ty_from_kind(TyKind::Pointer {
                mutable: true,
                pointee: self.diagnostic_ty(&item),
            });
        }

        if let Some(function_id) = sig_nodes.as_type_function(ty) {
            let (inputs_ty, output_ty) = sig_nodes.type_function(function_id);
            let inputs = if let Some(tuple_id) = sig_nodes.as_type_tuple(inputs_ty) {
                sig_nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|item| {
                        self.resolve_sig_type(item, sig_nodes, resolver, type_param_vars, lvl)
                    })
                    .collect()
            } else if inputs_ty == TyId::ZERO {
                Vec::new()
            } else {
                vec![self.resolve_sig_type(inputs_ty, sig_nodes, resolver, type_param_vars, lvl)]
            };
            let output =
                self.resolve_sig_type(output_ty, sig_nodes, resolver, type_param_vars, lvl);
            return InferTy::Function(inputs, Box::new(output));
        }

        if let Some(union_id) = sig_nodes.as_type_union(ty) {
            let (lhs_ty, rhs_ty) = sig_nodes.type_union(union_id);
            let lhs = self.resolve_sig_type(lhs_ty, sig_nodes, resolver, type_param_vars, lvl);
            let rhs = self.resolve_sig_type(rhs_ty, sig_nodes, resolver, type_param_vars, lvl);
            return Self::mk_union(lhs, rhs);
        }

        if let Some(inter_id) = sig_nodes.as_type_inter(ty) {
            let (lhs_ty, rhs_ty) = sig_nodes.type_inter(inter_id);
            let lhs = self.resolve_sig_type(lhs_ty, sig_nodes, resolver, type_param_vars, lvl);
            let rhs = self.resolve_sig_type(rhs_ty, sig_nodes, resolver, type_param_vars, lvl);
            return Self::mk_inter(lhs, rhs);
        }

        if let Some(record_id) = sig_nodes.as_type_record(ty) {
            let fields = sig_nodes
                .type_record(record_id)
                .iter()
                .filter_map(|field_ty| {
                    let field_id = sig_nodes.as_type_field(field_ty)?;
                    let (name_id, field_ty) = sig_nodes.type_field(field_id);
                    let name = sig_nodes.name(name_id);
                    let field_ty =
                        self.resolve_sig_type(field_ty, sig_nodes, resolver, type_param_vars, lvl);
                    Some((Self::symbol_to_bits(name), field_ty))
                })
                .collect();
            return InferTy::Record(fields);
        }

        if let Some(type_apply) = sig_nodes.as_type_apply(ty) {
            let (path_ty, args_ty) = sig_nodes.type_apply(type_apply);
            let base = self.resolve_sig_type(path_ty, sig_nodes, resolver, type_param_vars, lvl);
            let args = if let Some(tuple_id) = sig_nodes.as_type_tuple(args_ty) {
                sig_nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|arg| {
                        let arg =
                            self.resolve_sig_type(arg, sig_nodes, resolver, type_param_vars, lvl);
                        self.diagnostic_ty(&arg)
                    })
                    .collect::<Vec<_>>()
            } else {
                Vec::new()
            };

            return match Self::known_ty(&base)
                .and_then(|base| instantiate_nominal_type(self.db, base, args))
            {
                Some(ty) => self.ty_to_infer_ty(ty),
                None => self.fresh_var(lvl),
            };
        }

        let Some(type_path) = sig_nodes.as_type_path(ty) else {
            return self.fresh_var(lvl);
        };
        let name = sig_nodes.type_ref(type_path);

        if let Some(var) = type_param_vars.get(&name) {
            return var.clone();
        }

        match resolver
            .resolve_type_binding(name)
            .and_then(|binding| resolver.ty_for_binding(binding))
        {
            Some(ty) => self.ty_to_infer_ty(ty),
            None => self.fresh_var(lvl),
        }
    }

    fn infer_expr(&mut self, node: ExprId, lvl: usize) -> InferTy {
        self.typecheck_expr(node, None, lvl)
    }

    fn check_expr(&mut self, node: ExprId, expected: &InferTy, lvl: usize) -> InferTy {
        self.typecheck_expr(node, Some(expected.clone()), lvl)
    }

    fn typecheck_expr(&mut self, node: ExprId, expected: Option<InferTy>, lvl: usize) -> InferTy {
        let nodes = self.function.node_store();
        let kind = nodes.node_kind(node);
        let result = if Self::is_context_node(kind) {
            self.with_context(node, |this| this.typecheck_inner(node, expected, lvl))
        } else {
            self.typecheck_inner(node, expected, lvl)
        };
        self.node_types.insert(node, result.clone());
        result
    }

    fn typecheck_inner(&mut self, node: ExprId, expected: Option<InferTy>, lvl: usize) -> InferTy {
        let nodes = self.function.node_store();
        let actual = match nodes.node_kind(node) {
            NodeKind::Int => self.integer_literal_infer_ty(expected.as_ref()),
            NodeKind::Float => self.float_infer_ty(),
            NodeKind::String => self.string_infer_ty(),
            NodeKind::Char => self.char_infer_ty(),
            NodeKind::True | NodeKind::False => self.bool_infer_ty(),
            NodeKind::Tuple => {
                let tuple = nodes.tuple(nodes.as_tuple(node).expect("Tuple node mismatch"));
                match &expected {
                    Some(InferTy::Tuple(expected_items)) if expected_items.len() == tuple.len() => {
                        let items: Vec<InferTy> = tuple
                            .iter()
                            .zip(expected_items.iter())
                            .map(|(item, exp)| self.check_expr(item, exp, lvl))
                            .collect();
                        return InferTy::Tuple(items);
                    }
                    Some(InferTy::Tuple(expected_items)) => {
                        self.emit(DiagnosticKind::TupleArityMismatch(
                            node,
                            expected_items.len(),
                            tuple.len(),
                        ));
                        let items = tuple.iter().map(|item| self.infer_expr(item, lvl)).collect();
                        return InferTy::Tuple(items);
                    }
                    _ => {
                        let items = tuple.iter().map(|item| self.infer_expr(item, lvl)).collect();
                        InferTy::Tuple(items)
                    }
                }
            }
            NodeKind::Array => {
                let array = nodes.array(nodes.as_array(node).expect("Array node mismatch"));
                let expected_item = match expected.as_ref() {
                    Some(InferTy::Array(item)) => Some((**item).clone()),
                    _ => None,
                };
                let item_ty = expected_item.unwrap_or_else(|| self.fresh_var(lvl));
                for item in array.iter() {
                    self.check_expr(item, &item_ty, lvl);
                }
                InferTy::Array(Box::new(item_ty))
            }
            NodeKind::ArrayRepeat => {
                let (value, len) =
                    nodes.array_repeat(nodes.as_array_repeat(node).expect("ArrayRepeat mismatch"));
                let expected_item = match expected.as_ref() {
                    Some(InferTy::Array(item)) => Some((**item).clone()),
                    _ => None,
                };
                let item_ty = expected_item.unwrap_or_else(|| self.fresh_var(lvl));
                self.check_expr(value, &item_ty, lvl);
                self.check_expr(len, &self.int_infer_ty(), lvl);
                InferTy::Array(Box::new(item_ty))
            }
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(node).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let ty = if var.initializer != ExprId::ZERO {
                    self.infer_expr(var.initializer, lvl)
                } else {
                    InferTy::Unknown
                };
                self.bind_pattern_root(
                    var.pattern,
                    &ty,
                    lvl,
                    PatternBindingScheme::Mono,
                    false,
                    node,
                );
                return InferTy::Tuple(Vec::new());
            }
            NodeKind::Name => self.infer_name_expr(node, lvl),
            NodeKind::Field => {
                let (field_expr, field_name_expr) =
                    nodes.field(nodes.as_field(node).expect("Field node mismatch"));
                self.infer_field_expr(node, field_expr, field_name_expr, expected.as_ref(), lvl)
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(node).expect("Block node mismatch"));
                let mut returned = false;

                for stmt in stmts.iter() {
                    if self.typecheck_stmt(stmt, lvl) {
                        returned = true;
                        break;
                    }
                }

                if returned {
                    return expected.unwrap_or_else(|| InferTy::Tuple(Vec::new()));
                }

                if tail != ExprId::ZERO {
                    return self.typecheck_expr(tail, expected, lvl);
                }
                return InferTy::Tuple(Vec::new());
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(node).expect("UnsafeBlock mismatch"));
                if body != ExprId::ZERO {
                    return self
                        .with_unsafe_context(|this| this.typecheck_expr(body, expected, lvl));
                }
                return Self::unit_infer_ty();
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(node).expect("Binary node mismatch"));
                let lhs_ty = self.infer_expr(binary.lhs, lvl);
                let rhs_ty = self.infer_expr(binary.rhs, lvl);

                if binary.op == ExprId::ZERO {
                    return InferTy::Unknown;
                }

                let op_sym = nodes.name(nodes.as_name(binary.op).expect("op should be Name"));
                let emit_invalid = |this: &mut Self| {
                    this.emit(DiagnosticKind::InvalidBinaryOp(
                        node,
                        op_sym,
                        this.diagnostic_ty(&lhs_ty),
                        this.diagnostic_ty(&rhs_ty),
                    ));
                    InferTy::Unknown
                };
                let constrain_numeric = |this: &mut Self, ty: &InferTy, kind: NumericKind| {
                    let expected = this.numeric_infer_ty(kind);
                    this.constrain_top(ty, &expected).is_ok()
                };
                let constrain_orderable = |this: &mut Self, ty: &InferTy, kind: OrderableKind| {
                    let expected = this.orderable_infer_ty(kind);
                    this.constrain_top(ty, &expected).is_ok()
                };

                match op_sym.text(self.db) {
                    "+" | "-" | "*" | "/" | "%" => {
                        if matches!(lhs_ty, InferTy::Unknown) || matches!(rhs_ty, InferTy::Unknown)
                        {
                            InferTy::Unknown
                        } else {
                            let expected_numeric =
                                expected.as_ref().and_then(|ty| self.numeric_kind(ty));

                            let target = expected_numeric
                                .or(self.numeric_kind(&lhs_ty))
                                .or(self.numeric_kind(&rhs_ty));

                            match target {
                                Some(target) => {
                                    let lhs_ty = if Self::literal_can_coerce_to_numeric(target)
                                        && nodes.node_kind(binary.lhs) == NodeKind::Int
                                    {
                                        self.check_expr(
                                            binary.lhs,
                                            &self.numeric_infer_ty(target),
                                            lvl,
                                        )
                                    } else {
                                        lhs_ty.clone()
                                    };
                                    let rhs_ty = if Self::literal_can_coerce_to_numeric(target)
                                        && nodes.node_kind(binary.rhs) == NodeKind::Int
                                    {
                                        self.check_expr(
                                            binary.rhs,
                                            &self.numeric_infer_ty(target),
                                            lvl,
                                        )
                                    } else {
                                        rhs_ty.clone()
                                    };
                                    if !constrain_numeric(self, &lhs_ty, target)
                                        || !constrain_numeric(self, &rhs_ty, target)
                                    {
                                        emit_invalid(self)
                                    } else {
                                        self.numeric_infer_ty(target)
                                    }
                                }
                                None => InferTy::Unknown,
                            }
                        }
                    }
                    "==" | "!=" => {
                        if matches!(lhs_ty, InferTy::Unknown) || matches!(rhs_ty, InferTy::Unknown)
                        {
                            InferTy::Unknown
                        } else {
                            let target = self.numeric_kind(&lhs_ty).or(self.numeric_kind(&rhs_ty));
                            let (lhs_ty, rhs_ty) = if let Some(target) = target {
                                let lhs_ty = if Self::literal_can_coerce_to_numeric(target)
                                    && nodes.node_kind(binary.lhs) == NodeKind::Int
                                {
                                    self.check_expr(binary.lhs, &self.numeric_infer_ty(target), lvl)
                                } else {
                                    lhs_ty.clone()
                                };
                                let rhs_ty = if Self::literal_can_coerce_to_numeric(target)
                                    && nodes.node_kind(binary.rhs) == NodeKind::Int
                                {
                                    self.check_expr(binary.rhs, &self.numeric_infer_ty(target), lvl)
                                } else {
                                    rhs_ty.clone()
                                };
                                (lhs_ty, rhs_ty)
                            } else {
                                (lhs_ty.clone(), rhs_ty.clone())
                            };
                            let same = self.constrain_eq_top(&lhs_ty, &rhs_ty).is_ok();
                            if same { self.bool_infer_ty() } else { emit_invalid(self) }
                        }
                    }
                    "<" | ">" | "<=" | ">=" => {
                        if matches!(lhs_ty, InferTy::Unknown) || matches!(rhs_ty, InferTy::Unknown)
                        {
                            InferTy::Unknown
                        } else {
                            let target =
                                self.orderable_kind(&lhs_ty).or(self.orderable_kind(&rhs_ty));
                            match target {
                                Some(target) => {
                                    let lhs_ty = if Self::literal_can_coerce_to_orderable(target)
                                        && nodes.node_kind(binary.lhs) == NodeKind::Int
                                    {
                                        self.check_expr(
                                            binary.lhs,
                                            &self.orderable_infer_ty(target),
                                            lvl,
                                        )
                                    } else {
                                        lhs_ty.clone()
                                    };
                                    let rhs_ty = if Self::literal_can_coerce_to_orderable(target)
                                        && nodes.node_kind(binary.rhs) == NodeKind::Int
                                    {
                                        self.check_expr(
                                            binary.rhs,
                                            &self.orderable_infer_ty(target),
                                            lvl,
                                        )
                                    } else {
                                        rhs_ty.clone()
                                    };
                                    if constrain_orderable(self, &lhs_ty, target)
                                        && constrain_orderable(self, &rhs_ty, target)
                                    {
                                        self.bool_infer_ty()
                                    } else {
                                        emit_invalid(self)
                                    }
                                }
                                None => emit_invalid(self),
                            }
                        }
                    }
                    "&&" | "||" => {
                        let bool_ty = self.bool_infer_ty();
                        if matches!(lhs_ty, InferTy::Unknown) || matches!(rhs_ty, InferTy::Unknown)
                        {
                            InferTy::Unknown
                        } else if self.constrain_top(&lhs_ty, &bool_ty).is_ok()
                            && self.constrain_top(&rhs_ty, &bool_ty).is_ok()
                        {
                            bool_ty
                        } else {
                            emit_invalid(self)
                        }
                    }
                    _ => emit_invalid(self),
                }
            }
            NodeKind::Postfix => {
                let postfix = nodes.postfix(nodes.as_postfix(node).expect("Postfix node mismatch"));
                let expr_ty = self.infer_expr(postfix.expr, lvl);

                if postfix.op == ExprId::ZERO {
                    return InferTy::Unknown;
                }

                let op_sym = nodes.name(nodes.as_name(postfix.op).expect("op should be Name"));

                match &expr_ty {
                    InferTy::Unknown | InferTy::Var(_) => InferTy::Unknown,
                    _ => {
                        self.emit(DiagnosticKind::InvalidPostfixOp(
                            node,
                            op_sym,
                            self.diagnostic_ty(&expr_ty),
                        ));
                        InferTy::Unknown
                    }
                }
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(node).expect("Prefix node mismatch"));
                let expr_ty = self.infer_expr(prefix.expr, lvl);

                if prefix.op == ExprId::ZERO {
                    return InferTy::Unknown;
                }

                let op_sym = nodes.name(nodes.as_name(prefix.op).expect("op should be Name"));
                let emit_invalid = |this: &mut Self| {
                    this.emit(DiagnosticKind::InvalidPrefixOp(
                        node,
                        op_sym,
                        this.diagnostic_ty(&expr_ty),
                    ));
                    InferTy::Unknown
                };

                if matches!(expr_ty, InferTy::Unknown) {
                    InferTy::Unknown
                } else {
                    match op_sym.text(self.db) {
                        "!" => {
                            let bool_ty = self.bool_infer_ty();
                            if self.constrain_top(&expr_ty, &bool_ty).is_ok() {
                                bool_ty
                            } else {
                                emit_invalid(self)
                            }
                        }
                        "-" => {
                            let expected_numeric =
                                expected.as_ref().and_then(|ty| self.numeric_kind(ty));
                            let target = expected_numeric.or(self.numeric_kind(&expr_ty));

                            match target {
                                Some(target) => {
                                    let target_ty = self.numeric_infer_ty(target);
                                    if self.constrain_top(&expr_ty, &target_ty).is_ok() {
                                        target_ty
                                    } else {
                                        emit_invalid(self)
                                    }
                                }
                                None => {
                                    if matches!(expr_ty, InferTy::Var(_)) {
                                        InferTy::Unknown
                                    } else {
                                        emit_invalid(self)
                                    }
                                }
                            }
                        }
                        _ => emit_invalid(self),
                    }
                }
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(node).expect("If node mismatch"));
                let bool_ty = self.bool_infer_ty();
                self.check_expr(if_expr.cond, &bool_ty, lvl);

                if let Some(expected_ty) = &expected {
                    if if_expr.then_branch != ExprId::ZERO {
                        self.check_expr(if_expr.then_branch, expected_ty, lvl);
                    }
                    if if_expr.else_branch != ExprId::ZERO {
                        self.check_expr(if_expr.else_branch, expected_ty, lvl);
                    } else if *expected_ty != InferTy::Tuple(Vec::new()) {
                        self.emit(DiagnosticKind::MissingElseBranch(node));
                    }
                    return expected_ty.clone();
                }

                let then_ty = if if_expr.then_branch != ExprId::ZERO {
                    self.infer_expr(if_expr.then_branch, lvl)
                } else {
                    InferTy::Tuple(Vec::new())
                };
                let else_ty = if if_expr.else_branch != ExprId::ZERO {
                    self.infer_expr(if_expr.else_branch, lvl)
                } else {
                    InferTy::Tuple(Vec::new())
                };

                // Try to unify branches via subtyping.
                let result = self.fresh_var(lvl);
                let _ = self.constrain_top(&then_ty, &result);
                let _ = self.constrain_top(&else_ty, &result);
                result
            }
            NodeKind::Match => {
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(node).expect("Match node mismatch"));
                let scrutinee_ty = self.infer_expr(scrutinee, lvl);

                if let Some(expected_ty) = &expected {
                    for arm in arms.iter() {
                        let (pattern, expr) =
                            nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm"));
                        let saved_env = self.env.clone();
                        let saved_binding_names = self.binding_names.clone();
                        self.bind_pattern_root(
                            pattern,
                            &scrutinee_ty,
                            lvl,
                            PatternBindingScheme::Mono,
                            true,
                            expr,
                        );
                        self.check_expr(expr, expected_ty, lvl);
                        self.env = saved_env;
                        self.binding_names = saved_binding_names;
                    }
                    return expected_ty.clone();
                }

                let result = self.fresh_var(lvl);
                for arm in arms.iter() {
                    let (pattern, expr) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm"));
                    let saved_env = self.env.clone();
                    let saved_binding_names = self.binding_names.clone();
                    self.bind_pattern_root(
                        pattern,
                        &scrutinee_ty,
                        lvl,
                        PatternBindingScheme::Mono,
                        true,
                        expr,
                    );
                    let arm_ty = self.infer_expr(expr, lvl);
                    let _ = self.constrain_top(&arm_ty, &result);
                    self.env = saved_env;
                    self.binding_names = saved_binding_names;
                }
                result
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(node).expect("LoopExpr node mismatch"));
                let unit_ty = InferTy::Tuple(Vec::new());
                if body != ExprId::ZERO {
                    self.with_loop_depth(|this| {
                        this.check_expr(body, &unit_ty, lvl);
                    });
                }
                unit_ty
            }
            NodeKind::BreakExpr => {
                if self.loop_depth == 0 {
                    self.emit(DiagnosticKind::BreakOutsideLoop(node));
                }
                InferTy::Tuple(Vec::new())
            }
            NodeKind::ContinueExpr => {
                if self.loop_depth == 0 {
                    self.emit(DiagnosticKind::ContinueOutsideLoop(node));
                }
                InferTy::Tuple(Vec::new())
            }
            NodeKind::Closure => {
                let (params, body) =
                    nodes.closure_parts(nodes.as_closure(node).expect("Closure node mismatch"));

                if let Some(InferTy::Function(exp_inputs, exp_output)) = &expected {
                    if exp_inputs.len() == params.len() {
                        for (param, exp_ty) in params.iter().zip(exp_inputs.iter()) {
                            let (pattern, ty_id) = nodes.param(param);
                            let anchor = self.pattern_anchor(pattern).unwrap_or(node);
                            let annotated = self.resolve_type_to_infer_ty(ty_id);
                            let param_ty = match annotated {
                                Some(annotated_infer_ty) => {
                                    if !matches!(&annotated_infer_ty, InferTy::Unknown)
                                        && !matches!(exp_ty, InferTy::Unknown)
                                        && self.constrain_top(&annotated_infer_ty, exp_ty).is_err()
                                    {
                                        self.emit(DiagnosticKind::TypeMismatch(
                                            anchor,
                                            self.diagnostic_ty(&annotated_infer_ty),
                                            self.diagnostic_ty(exp_ty),
                                        ));
                                    }
                                    annotated_infer_ty
                                }
                                None => exp_ty.clone(),
                            };
                            self.bind_pattern_root(
                                pattern,
                                &param_ty,
                                lvl,
                                PatternBindingScheme::Mono,
                                false,
                                anchor,
                            );
                        }
                        let output = if body != ExprId::ZERO {
                            self.check_expr(body, exp_output, lvl)
                        } else {
                            InferTy::Unknown
                        };
                        return InferTy::Function(exp_inputs.clone(), Box::new(output));
                    }
                    self.emit(DiagnosticKind::ClosureArityMismatch(
                        node,
                        params.len(),
                        exp_inputs.len(),
                    ));
                }

                let mut inputs = Vec::with_capacity(params.len());
                for param in params.iter() {
                    let (pattern, ty_id) = nodes.param(param);
                    let anchor = self.pattern_anchor(pattern).unwrap_or(node);
                    let param_ty = match self.resolve_type_to_infer_ty(ty_id) {
                        Some(ty) => ty,
                        None => self.fresh_var(lvl),
                    };
                    self.bind_pattern_root(
                        pattern,
                        &param_ty,
                        lvl,
                        PatternBindingScheme::Mono,
                        false,
                        anchor,
                    );
                    inputs.push(param_ty);
                }
                let output = if body != ExprId::ZERO {
                    self.infer_expr(body, lvl)
                } else {
                    InferTy::Unknown
                };
                InferTy::Function(inputs, Box::new(output))
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(node).expect("Call node mismatch"));
                let args: Vec<ExprId> = args.iter().collect();
                if let Some(intrinsic_result) =
                    self.compiler_intrinsic_call(node, callee, &args, expected.as_ref(), lvl)
                {
                    return intrinsic_result;
                }
                if let Some(variant_result) = self.infer_bare_variant_call(callee, &args, lvl) {
                    return variant_result;
                }
                if let Some(method_result) = self.infer_method_call(node, callee, &args, lvl) {
                    return method_result;
                }
                let direct_target = if nodes.node_kind(callee) == NodeKind::Name
                    && let Some(name_id) = nodes.as_name(callee)
                    && let Some(BindingId::Function(target)) =
                        self.resolve_path_in_node_scope(callee, nodes.name(name_id))
                {
                    Some(target)
                } else {
                    None
                };
                if nodes.node_kind(callee) == NodeKind::Name
                    && let Some(target) = direct_target
                    && target.hir_function(self.db).function(self.db).is_unsafe()
                {
                    self.require_unsafe_context(node);
                }
                let callee_ty = self.infer_expr(callee, lvl);

                match &callee_ty {
                    InferTy::Function(inputs, output) => {
                        if inputs.len() != args.len() {
                            for &arg in &args {
                                self.infer_expr(arg, lvl);
                            }
                            self.emit(DiagnosticKind::CallArityMismatch(
                                node,
                                inputs.len(),
                                args.len(),
                            ));
                        } else {
                            for (index, (&arg, input_ty)) in
                                args.iter().zip(inputs.iter()).enumerate()
                            {
                                if direct_target.is_some_and(|target| {
                                    self.function_param_is_mutable(target, index)
                                }) {
                                    self.check_mutable_argument(arg, input_ty, lvl);
                                } else {
                                    self.check_expr(arg, input_ty, lvl);
                                }
                            }
                        }
                        *output.clone()
                    }
                    InferTy::Unknown => {
                        for &arg in &args {
                            self.infer_expr(arg, lvl);
                        }
                        InferTy::Unknown
                    }
                    InferTy::Var(_) => {
                        // Apply function via constraint: callee <: (args) -> result
                        let arg_tys: Vec<InferTy> =
                            args.iter().map(|&arg| self.infer_expr(arg, lvl)).collect();
                        let result = self.fresh_var(lvl);
                        let fun_ty = InferTy::Function(arg_tys, Box::new(result.clone()));
                        let _ = self.constrain_top(&callee_ty, &fun_ty);
                        result
                    }
                    _ => {
                        for &arg in &args {
                            self.infer_expr(arg, lvl);
                        }
                        self.emit(DiagnosticKind::CallNonFunction(
                            node,
                            self.diagnostic_ty(&callee_ty),
                        ));
                        InferTy::Unknown
                    }
                }
            }
            NodeKind::StructExpr => {
                let struct_expr_id = nodes.as_struct_expr(node).expect("StructExpr node mismatch");
                let items = nodes.struct_expr(struct_expr_id);

                let has_struct_name = items.len() % 2 == 1;
                if !has_struct_name {
                    let mut fields: Vec<(u64, InferTy)> = Vec::new();
                    let mut i = 0;
                    while i + 1 < items.len() {
                        let field_name_id = items.get(i).unwrap();
                        let field_expr_id = items.get(i + 1).unwrap();
                        i += 2;

                        let field_sym = nodes
                            .name(nodes.as_name(field_name_id).expect("field name should be Name"));
                        let field_bits = Self::symbol_to_bits(field_sym);
                        let field_ty = self.infer_expr(field_expr_id, lvl);
                        if let Some((_, existing)) =
                            fields.iter_mut().find(|(name_bits, _)| *name_bits == field_bits)
                        {
                            *existing = field_ty;
                        } else {
                            fields.push((field_bits, field_ty));
                        }
                    }
                    InferTy::Record(fields)
                } else {
                    // items layout for named struct expr:
                    // [struct_name, field1_name, field1_expr, ...]
                    let name_id = items.get(0).unwrap();
                    let name_sym = nodes
                        .name(nodes.as_name(name_id).expect("struct expr name should be Name"));

                    let Some(ty) = self.resolve_type_in_node_scope(name_id, name_sym) else {
                        self.emit(DiagnosticKind::UnresolvedIdent(name_id));
                        // Still infer field exprs.
                        let mut i = 1;
                        while i + 1 < items.len() {
                            self.infer_expr(items.get(i + 1).unwrap(), lvl);
                            i += 2;
                        }
                        return InferTy::Unknown;
                    };

                    let ty = expected
                        .as_ref()
                        .and_then(Self::known_ty)
                        .and_then(|expected_ty| self.match_expected_nominal_type(ty, expected_ty))
                        .unwrap_or(ty);

                    let struct_ty = match ty.kind(self.db) {
                        TyKind::Struct(struct_ty) | TyKind::ExternStruct(struct_ty) => *struct_ty,
                        _ => {
                            // Still infer field exprs.
                            let mut i = 1;
                            while i + 1 < items.len() {
                                self.infer_expr(items.get(i + 1).unwrap(), lvl);
                                i += 2;
                            }
                            self.emit(DiagnosticKind::NotAStruct(node, ty));
                            return InferTy::Unknown;
                        }
                    };
                    let fields = struct_fields(self.db, struct_ty);

                    let field_map: FxHashMap<Symbol<'db>, Ty<'db>> =
                        fields.iter().map(|(name, ty)| (*name, *ty)).collect();
                    let mut seen_fields: FxHashSet<Symbol<'db>> = FxHashSet::default();

                    let mut i = 1;
                    while i + 1 < items.len() {
                        let field_name_id = items.get(i).unwrap();
                        let field_expr_id = items.get(i + 1).unwrap();
                        i += 2;

                        let field_sym = nodes
                            .name(nodes.as_name(field_name_id).expect("field name should be Name"));

                        if let Some(&expected_ty) = field_map.get(&field_sym) {
                            seen_fields.insert(field_sym);
                            let expected_infer_ty = self.ty_to_infer_ty(expected_ty);
                            self.check_expr(field_expr_id, &expected_infer_ty, lvl);
                        } else {
                            self.infer_expr(field_expr_id, lvl);
                            self.emit(DiagnosticKind::UnknownStructField(node, field_sym));
                        }
                    }

                    for (field_name, _) in fields.iter() {
                        if !seen_fields.contains(field_name) {
                            self.emit(DiagnosticKind::MissingStructField(node, *field_name));
                        }
                    }

                    self.node_types.insert(name_id, self.ty_to_infer_ty(ty));
                    InferTy::Known(ty.as_id().as_bits())
                }
            }
            _ => InferTy::Unknown,
        };

        if let Some(ref expected_ty) = expected {
            self.coerce(node, &actual, expected_ty);
            expected_ty.clone()
        } else {
            actual
        }
    }

    fn coerce(&mut self, node: ExprId, actual: &InferTy, expected: &InferTy) {
        let _ = self.solve_variant_constraints(false);
        if self.should_defer_coercion(actual, expected) {
            self.deferred_coercions.push(DeferredCoercion {
                node,
                actual: actual.clone(),
                expected: expected.clone(),
            });
            return;
        }
        self.coerce_now(node, actual, expected);
    }

    fn coerce_now(&mut self, node: ExprId, actual: &InferTy, expected: &InferTy) {
        if matches!(expected, InferTy::Unknown) || matches!(actual, InferTy::Unknown) {
            return;
        }
        if self.constrain_top(actual, expected).is_err() {
            let actual_ty = self.diagnostic_ty(actual);
            let expected_ty = self.diagnostic_ty(expected);
            if actual_ty != expected_ty {
                self.emit(DiagnosticKind::TypeMismatch(node, actual_ty, expected_ty));
            }
        } else {
            let _ = self.solve_variant_constraints(false);
        }
    }

    fn should_defer_coercion(&self, actual: &InferTy, expected: &InferTy) -> bool {
        self.has_pending_variant_dependency(actual) && !self.is_enum_nominal(expected)
    }

    fn is_enum_nominal(&self, ty: &InferTy) -> bool {
        let InferTy::Known(bits) = ty else {
            return false;
        };
        let nominal = Ty::from_id(salsa::Id::from_bits(*bits));
        matches!(nominal.kind(self.db), TyKind::Enum(_))
    }

    fn has_pending_variant_dependency(&self, ty: &InferTy) -> bool {
        if self.variant_constraints.is_empty() {
            return false;
        }

        let pending_vars: FxHashSet<VarId> =
            self.variant_constraints.iter().map(|c| c.enum_var).collect();
        let mut seen_vars: FxHashSet<VarId> = FxHashSet::default();
        let mut stack = vec![ty.clone()];

        while let Some(current) = stack.pop() {
            match current {
                InferTy::Var(v) => {
                    if pending_vars.contains(&v) {
                        return true;
                    }
                    if !seen_vars.insert(v) {
                        continue;
                    }
                    stack.extend(self.vars[v].lower_bounds.iter().cloned());
                    stack.extend(self.vars[v].upper_bounds.iter().cloned());
                }
                InferTy::Function(inputs, output) => {
                    stack.extend(inputs);
                    stack.push(*output);
                }
                InferTy::Array(item) => stack.push(*item),
                InferTy::Tuple(items) => stack.extend(items),
                InferTy::Record(fields) => stack.extend(fields.into_iter().map(|(_, ty)| ty)),
                InferTy::Union(items) | InferTy::Inter(items) => stack.extend(items),
                InferTy::Known(_) | InferTy::Unknown => {}
            }
        }

        false
    }

    fn process_deferred_coercions(&mut self) {
        let deferred = std::mem::take(&mut self.deferred_coercions);
        for coercion in deferred {
            self.coerce_now(coercion.node, &coercion.actual, &coercion.expected);
        }
    }

    fn typecheck_stmt(&mut self, stmt: StmtId, lvl: usize) -> bool {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(stmt).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let expected_infer_ty = self.resolve_type_to_infer_ty(var.ty);
                let anchor = self.pattern_anchor(var.pattern).unwrap_or(stmt.node_id());

                let binding_ty = if var.initializer != ExprId::ZERO {
                    match expected_infer_ty {
                        Some(expected) => {
                            // Let RHS is always checked at a deeper level so fresh vars can be
                            // generalized unless they escape through constraints.
                            self.check_expr(var.initializer, &expected, lvl + 1);
                            expected
                        }
                        None => self.infer_expr(var.initializer, lvl + 1),
                    }
                } else {
                    self.emit(DiagnosticKind::MissingInitializer(anchor));
                    expected_infer_ty.unwrap_or_else(|| self.fresh_var(lvl))
                };

                self.bind_pattern_root(
                    var.pattern,
                    &binding_ty,
                    lvl,
                    PatternBindingScheme::Poly { level: lvl },
                    false,
                    anchor,
                );
                false
            }
            NodeKind::AssignStmt => {
                let assign_id = nodes.as_assign_stmt(stmt).expect("AssignStmt node mismatch");
                let (target, value) = nodes.assign_stmt(assign_id);
                let _ = match self.resolve_place(target, lvl) {
                    PlaceResolution::Mutable(target_ty) => self.check_expr(value, &target_ty, lvl),
                    PlaceResolution::Immutable => {
                        self.emit(DiagnosticKind::AssignmentRequiresMutable(target));
                        self.infer_expr(value, lvl)
                    }
                    PlaceResolution::Invalid => {
                        self.emit(DiagnosticKind::InvalidAssignmentTarget(target));
                        self.infer_expr(value, lvl)
                    }
                };
                false
            }
            NodeKind::ReturnStmt => {
                let (value, _) =
                    nodes.return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
                if value != ExprId::ZERO {
                    if let Some(return_ty) = self.return_ty.clone() {
                        self.check_expr(value, &return_ty, lvl);
                    } else {
                        self.infer_expr(value, lvl);
                    }
                }
                true
            }
            _ => {
                if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    self.infer_expr(expr, lvl);
                }
                self.expr_guarantees_return(stmt_as_expr(nodes, stmt))
            }
        }
    }

    fn expr_guarantees_return(&self, expr: Option<ExprId>) -> bool {
        let Some(expr) = expr else {
            return false;
        };

        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block node mismatch"));
                stmts.iter().any(|stmt| self.stmt_guarantees_return(stmt))
                    || self.expr_guarantees_return((tail != ExprId::ZERO).then_some(tail))
            }
            NodeKind::UnsafeBlock => {
                let (body, _) =
                    nodes.unsafe_block(nodes.as_unsafe_block(expr).expect("UnsafeBlock mismatch"));
                self.expr_guarantees_return((body != ExprId::ZERO).then_some(body))
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                if_expr.then_branch != ExprId::ZERO
                    && if_expr.else_branch != ExprId::ZERO
                    && self.expr_guarantees_return(Some(if_expr.then_branch))
                    && self.expr_guarantees_return(Some(if_expr.else_branch))
            }
            _ => false,
        }
    }

    fn stmt_guarantees_return(&self, stmt: StmtId) -> bool {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::ReturnStmt => true,
            _ => self.expr_guarantees_return(stmt_as_expr(nodes, stmt)),
        }
    }

    fn build(mut self) -> Inference<'db> {
        // Create fresh type variables for each type parameter.
        for &tp in self.function.type_params() {
            let var = self.fresh_var(0);
            if let InferTy::Var(id) = var {
                self.preserved_type_vars.insert(id as u32);
            }
            self.type_param_env.insert(tp, var);
        }

        for &param in self.function.params() {
            let (pattern, ty_id) = self.function.node_store().param(param);
            let anchor = self.pattern_anchor(pattern).unwrap_or(ExprId::ZERO);
            let param_ty = if let Some(ty) = self.resolve_type_to_infer_ty(ty_id) {
                ty
            } else {
                self.emit(DiagnosticKind::MissingParameterType(anchor));
                for name in self.function.node_store().pattern_binding_names(pattern) {
                    self.missing_param_nodes.insert(name.into());
                }
                self.fresh_var(0)
            };
            self.bind_pattern_root(
                pattern,
                &param_ty,
                0,
                PatternBindingScheme::Mono,
                false,
                anchor,
            );
        }

        if self.function.body() == ExprId::ZERO {
            let ret_ty = if self.function.ret_type() == TyId::ZERO {
                Ty::new(self.db, TyKind::Tuple(Vec::new()))
            } else {
                self.resolve_type_to_infer_ty(self.function.ret_type()).map_or_else(
                    || Ty::new(self.db, TyKind::Unknown),
                    |ty| self.present_type(&ty, Polarity::Positive),
                )
            };

            for &param in self.function.params() {
                let (pattern, _) = self.function.node_store().param(param);
                for name in self.function.node_store().pattern_binding_names(pattern) {
                    let name_node = name.into();
                    if let Some(param_ty) =
                        self.env.get(&name.into()).and_then(|scheme| match scheme {
                            Scheme::Mono(ty) => {
                                Some(if self.missing_param_nodes.contains(&name_node) {
                                    self.coalesce_type_for_missing_param(ty)
                                } else {
                                    self.present_type(ty, Polarity::Positive)
                                })
                            }
                            Scheme::Poly { .. } => None,
                        })
                    {
                        self.inference.type_of_node.insert(name_node, param_ty);
                    }
                }
            }

            self.inference.type_of_node.insert(self.function.body(), ret_ty);
            for (pattern, infer_ty) in &self.selected_union_members {
                let ty = self.present_type(infer_ty, Polarity::Positive);
                self.inference.selected_union_members.insert(*pattern, ty);
            }
            for (pattern, infer_ty) in &self.matched_typed_patterns {
                let ty = self.present_type(infer_ty, Polarity::Positive);
                self.inference.matched_typed_patterns.insert(*pattern, ty);
            }
            return self.inference;
        }

        let ret_ty = if self.function.ret_type() == TyId::ZERO {
            InferTy::Tuple(Vec::new())
        } else {
            self.resolve_type_to_infer_ty(self.function.ret_type())
                .unwrap_or_else(|| self.fresh_var(0))
        };

        self.return_ty = Some(ret_ty.clone());
        self.check_expr(self.function.body(), &ret_ty, 0);

        while self.solve_variant_constraints(false) {}

        self.process_deferred_coercions();

        while self.solve_variant_constraints(false) {}

        let _ = self.solve_variant_constraints(true);
        self.emit_unknown_type_errors();

        self.finalize_user_types();

        self.inference
    }

    fn constrain_top(&mut self, lhs: &InferTy, rhs: &InferTy) -> Result<(), ()> {
        let mut cache: FxHashSet<(InferTy, InferTy)> = FxHashSet::default();
        self.constrain(lhs, rhs, &mut cache)
    }

    fn constrain_eq_top(&mut self, lhs: &InferTy, rhs: &InferTy) -> Result<(), ()> {
        let mut cache: FxHashSet<(InferTy, InferTy)> = FxHashSet::default();
        self.constrain(lhs, rhs, &mut cache)?;
        self.constrain(rhs, lhs, &mut cache)
    }

    fn constrain_with_snapshot(
        &mut self,
        lhs: &InferTy,
        rhs: &InferTy,
        cache: &mut FxHashSet<(InferTy, InferTy)>,
    ) -> Result<(), ()> {
        let vars_snapshot = self.vars.clone();
        let cache_snapshot = cache.clone();
        if let Ok(()) = self.constrain(lhs, rhs, cache) {
            Ok(())
        } else {
            self.vars = vars_snapshot;
            *cache = cache_snapshot;
            Err(())
        }
    }

    fn constrain(
        &mut self,
        lhs: &InferTy,
        rhs: &InferTy,
        cache: &mut FxHashSet<(InferTy, InferTy)>,
    ) -> Result<(), ()> {
        if lhs == rhs {
            return Ok(());
        }
        if matches!(lhs, InferTy::Unknown) || matches!(rhs, InferTy::Unknown) {
            return Ok(());
        }

        let pair = (lhs.clone(), rhs.clone());
        if matches!(lhs, InferTy::Var(_)) || matches!(rhs, InferTy::Var(_)) {
            if cache.contains(&pair) {
                return Ok(());
            }
            cache.insert(pair);
        }

        match (lhs, rhs) {
            (InferTy::Union(lhs_items), rhs) => {
                for lhs_item in lhs_items {
                    self.constrain(lhs_item, rhs, cache)?;
                }
                Ok(())
            }
            (lhs, InferTy::Inter(rhs_items)) => {
                for rhs_item in rhs_items {
                    self.constrain(lhs, rhs_item, cache)?;
                }
                Ok(())
            }
            (InferTy::Inter(lhs_items), rhs) => {
                let Some((last, rest)) = lhs_items.split_last() else {
                    return Err(());
                };
                for lhs_item in rest {
                    if self.constrain_with_snapshot(lhs_item, rhs, cache).is_ok() {
                        return Ok(());
                    }
                }
                self.constrain(last, rhs, cache)
            }
            (lhs, InferTy::Union(rhs_items)) => {
                let Some((last, rest)) = rhs_items.split_last() else {
                    return Err(());
                };
                for rhs_item in rest {
                    if self.constrain_with_snapshot(lhs, rhs_item, cache).is_ok() {
                        return Ok(());
                    }
                }
                self.constrain(lhs, last, cache)
            }
            (InferTy::Known(a), InferTy::Known(b)) if a == b => Ok(()),
            (InferTy::Known(a), InferTy::Known(b)) => {
                let lhs_ty = Ty::from_id(salsa::Id::from_bits(*a));
                let rhs_ty = Ty::from_id(salsa::Id::from_bits(*b));
                match (lhs_ty.kind(self.db), rhs_ty.kind(self.db)) {
                    (
                        TyKind::Pointer { mutable: true, pointee: lhs_pointee },
                        TyKind::Pointer { mutable: false, pointee: rhs_pointee },
                    ) if lhs_pointee == rhs_pointee => Ok(()),
                    _ => Err(()),
                }
            }
            (InferTy::Function(l_in, l_out), InferTy::Function(r_in, r_out))
                if l_in.len() == r_in.len() =>
            {
                for (l, r) in l_in.iter().zip(r_in.iter()) {
                    self.constrain(r, l, cache)?;
                }
                self.constrain(l_out, r_out, cache)
            }
            (InferTy::Array(l_item), InferTy::Array(r_item)) => {
                self.constrain(l_item, r_item, cache)
            }
            (InferTy::Tuple(l_items), InferTy::Tuple(r_items))
                if l_items.len() == r_items.len() =>
            {
                for (l, r) in l_items.iter().zip(r_items.iter()) {
                    self.constrain(l, r, cache)?;
                }
                Ok(())
            }
            (InferTy::Record(l_fields), InferTy::Record(r_fields)) => {
                for (r_name_bits, r_ty) in r_fields {
                    let Some((_, l_ty)) =
                        l_fields.iter().find(|(l_name_bits, _)| l_name_bits == r_name_bits)
                    else {
                        return Err(());
                    };
                    self.constrain(l_ty, r_ty, cache)?;
                }
                Ok(())
            }
            (InferTy::Known(bits), InferTy::Record(r_fields)) => {
                let nominal = Ty::from_id(salsa::Id::from_bits(*bits));
                let (TyKind::Struct(struct_ty) | TyKind::ExternStruct(struct_ty)) =
                    nominal.kind(self.db)
                else {
                    return Err(());
                };
                let fields = struct_fields(self.db, *struct_ty);

                for (r_name_bits, r_ty) in r_fields {
                    let Some((_, field_ty)) =
                        fields.iter().find(|(name, _)| Self::symbol_to_bits(*name) == *r_name_bits)
                    else {
                        return Err(());
                    };
                    let l_ty = self.ty_to_infer_ty(*field_ty);
                    self.constrain(&l_ty, r_ty, cache)?;
                }
                Ok(())
            }
            (InferTy::Record(l_fields), InferTy::Known(bits)) => {
                let nominal = Ty::from_id(salsa::Id::from_bits(*bits));
                let (TyKind::Struct(struct_ty) | TyKind::ExternStruct(struct_ty)) =
                    nominal.kind(self.db)
                else {
                    return Err(());
                };
                let fields = struct_fields(self.db, *struct_ty);

                if l_fields.len() != fields.len() {
                    return Err(());
                }

                for (field_name, field_ty) in fields {
                    let field_name_bits = Self::symbol_to_bits(*field_name);
                    let Some((_, l_ty)) =
                        l_fields.iter().find(|(name_bits, _)| *name_bits == field_name_bits)
                    else {
                        return Err(());
                    };
                    let r_ty = self.ty_to_infer_ty(*field_ty);
                    self.constrain(l_ty, &r_ty, cache)?;
                }
                Ok(())
            }
            (InferTy::Var(lhs_v), _) if self.level(rhs) <= self.vars[*lhs_v].level => {
                let lhs_v = *lhs_v;
                self.vars[lhs_v].upper_bounds.insert(0, rhs.clone());
                let lowers = self.vars[lhs_v].lower_bounds.clone();
                for l in lowers {
                    self.constrain(&l, rhs, cache)?;
                }
                Ok(())
            }
            (_, InferTy::Var(rhs_v)) if self.level(lhs) <= self.vars[*rhs_v].level => {
                let rhs_v = *rhs_v;
                self.vars[rhs_v].lower_bounds.insert(0, lhs.clone());
                let uppers = self.vars[rhs_v].upper_bounds.clone();
                for u in uppers {
                    self.constrain(lhs, &u, cache)?;
                }
                Ok(())
            }
            (InferTy::Var(lhs_v), _) => {
                let lhs_v = *lhs_v;
                let rhs_ex = self.extrude(
                    rhs,
                    Polarity::Negative,
                    self.vars[lhs_v].level,
                    &mut FxHashMap::default(),
                );
                self.constrain(&InferTy::Var(lhs_v), &rhs_ex, cache)
            }
            (_, InferTy::Var(rhs_v)) => {
                let rhs_v = *rhs_v;
                let lhs_ex = self.extrude(
                    lhs,
                    Polarity::Positive,
                    self.vars[rhs_v].level,
                    &mut FxHashMap::default(),
                );
                self.constrain(&lhs_ex, &InferTy::Var(rhs_v), cache)
            }
            _ => Err(()),
        }
    }

    fn extrude(
        &mut self,
        ty: &InferTy,
        pol: Polarity,
        lvl: usize,
        cache: &mut FxHashMap<(VarId, Polarity), VarId>,
    ) -> InferTy {
        if self.level(ty) <= lvl {
            return ty.clone();
        }

        match ty {
            InferTy::Known(_) | InferTy::Unknown => ty.clone(),
            InferTy::Function(inputs, output) => InferTy::Function(
                inputs.iter().map(|t| self.extrude(t, pol.flip(), lvl, cache)).collect(),
                Box::new(self.extrude(output, pol, lvl, cache)),
            ),
            InferTy::Array(item) => InferTy::Array(Box::new(self.extrude(item, pol, lvl, cache))),
            InferTy::Tuple(items) => {
                InferTy::Tuple(items.iter().map(|t| self.extrude(t, pol, lvl, cache)).collect())
            }
            InferTy::Record(fields) => InferTy::Record(
                fields
                    .iter()
                    .map(|(name, ty)| (*name, self.extrude(ty, pol, lvl, cache)))
                    .collect(),
            ),
            InferTy::Union(items) => {
                InferTy::Union(items.iter().map(|t| self.extrude(t, pol, lvl, cache)).collect())
            }
            InferTy::Inter(items) => {
                InferTy::Inter(items.iter().map(|t| self.extrude(t, pol, lvl, cache)).collect())
            }
            InferTy::Var(tv) => {
                let tv = *tv;
                if let Some(nv) = cache.get(&(tv, pol)) {
                    return InferTy::Var(*nv);
                }
                let nvs = self.fresh_id(lvl);
                cache.insert((tv, pol), nvs);

                match pol {
                    Polarity::Positive => {
                        self.vars[tv].upper_bounds.insert(0, InferTy::Var(nvs));
                        let old_lower = self.vars[tv].lower_bounds.clone();
                        let new_lower: Vec<InferTy> =
                            old_lower.iter().map(|b| self.extrude(b, pol, lvl, cache)).collect();
                        self.vars[nvs].lower_bounds = new_lower;
                    }
                    Polarity::Negative => {
                        self.vars[tv].lower_bounds.insert(0, InferTy::Var(nvs));
                        let old_upper = self.vars[tv].upper_bounds.clone();
                        let new_upper: Vec<InferTy> =
                            old_upper.iter().map(|b| self.extrude(b, pol, lvl, cache)).collect();
                        self.vars[nvs].upper_bounds = new_upper;
                    }
                }
                InferTy::Var(nvs)
            }
        }
    }

    fn freshen(&mut self, lim: usize, ty: &InferTy, lvl: usize) -> InferTy {
        let mut freshened: FxHashMap<VarId, VarId> = FxHashMap::default();
        self.freshen_inner(lim, ty, lvl, &mut freshened)
    }

    fn freshen_inner(
        &mut self,
        lim: usize,
        ty: &InferTy,
        lvl: usize,
        freshened: &mut FxHashMap<VarId, VarId>,
    ) -> InferTy {
        if self.level(ty) <= lim {
            return ty.clone();
        }

        match ty {
            InferTy::Known(_) | InferTy::Unknown => ty.clone(),
            InferTy::Function(inputs, output) => InferTy::Function(
                inputs.iter().map(|t| self.freshen_inner(lim, t, lvl, freshened)).collect(),
                Box::new(self.freshen_inner(lim, output, lvl, freshened)),
            ),
            InferTy::Array(item) => {
                InferTy::Array(Box::new(self.freshen_inner(lim, item, lvl, freshened)))
            }
            InferTy::Tuple(items) => InferTy::Tuple(
                items.iter().map(|t| self.freshen_inner(lim, t, lvl, freshened)).collect(),
            ),
            InferTy::Record(fields) => InferTy::Record(
                fields
                    .iter()
                    .map(|(name, ty)| (*name, self.freshen_inner(lim, ty, lvl, freshened)))
                    .collect(),
            ),
            InferTy::Union(items) => InferTy::Union(
                items.iter().map(|t| self.freshen_inner(lim, t, lvl, freshened)).collect(),
            ),
            InferTy::Inter(items) => InferTy::Inter(
                items.iter().map(|t| self.freshen_inner(lim, t, lvl, freshened)).collect(),
            ),
            InferTy::Var(tv) => {
                let tv = *tv;
                if self.vars[tv].level <= lim {
                    return ty.clone();
                }
                if let Some(v) = freshened.get(&tv) {
                    return InferTy::Var(*v);
                }
                let v = self.fresh_id(lvl);
                freshened.insert(tv, v);

                let old_lower = self.vars[tv].lower_bounds.clone();
                let old_upper = self.vars[tv].upper_bounds.clone();

                let mut new_lower = Vec::with_capacity(old_lower.len());
                for b in old_lower.iter().rev() {
                    new_lower.push(self.freshen_inner(lim, b, lvl, freshened));
                }
                new_lower.reverse();

                let mut new_upper = Vec::with_capacity(old_upper.len());
                for b in old_upper.iter().rev() {
                    new_upper.push(self.freshen_inner(lim, b, lvl, freshened));
                }
                new_upper.reverse();

                self.vars[v].lower_bounds = new_lower;
                self.vars[v].upper_bounds = new_upper;
                InferTy::Var(v)
            }
        }
    }

    fn match_expected_nominal_type(
        &self,
        resolved: Ty<'db>,
        expected_ty: Ty<'db>,
    ) -> Option<Ty<'db>> {
        match (resolved.kind(self.db), expected_ty.kind(self.db)) {
            (TyKind::Struct(resolved), TyKind::Struct(expected))
                if same_struct_declaration(self.db, *resolved, *expected) =>
            {
                Some(expected_ty)
            }
            (TyKind::ExternStruct(resolved), TyKind::ExternStruct(expected))
                if same_struct_declaration(self.db, *resolved, *expected) =>
            {
                Some(expected_ty)
            }
            (TyKind::Struct(resolved), TyKind::ExternStruct(expected))
                if same_struct_declaration(self.db, *resolved, *expected) =>
            {
                Some(expected_ty)
            }
            (TyKind::ExternStruct(resolved), TyKind::Struct(expected))
                if same_struct_declaration(self.db, *resolved, *expected) =>
            {
                Some(expected_ty)
            }
            _ => None,
        }
    }
}

fn same_struct_declaration<'db>(
    db: &'db dyn Database,
    lhs: StructTy<'db>,
    rhs: StructTy<'db>,
) -> bool {
    lhs.module(db) == rhs.module(db)
        && lhs.index(db) == rhs.index(db)
        && lhs.name(db) == rhs.name(db)
}

fn stmt_as_expr(nodes: &NodeStore<'_>, stmt: StmtId) -> Option<ExprId> {
    match nodes.node_kind(stmt) {
        NodeKind::Name => nodes.as_name(stmt).map(Into::into),
        NodeKind::True => nodes.as_true(stmt).map(Into::into),
        NodeKind::False => nodes.as_false(stmt).map(Into::into),
        NodeKind::Error => nodes.as_error(stmt).map(Into::into),
        NodeKind::Int => nodes.as_int(stmt).map(Into::into),
        NodeKind::Float => nodes.as_float(stmt).map(Into::into),
        NodeKind::String => nodes.as_string(stmt).map(Into::into),
        NodeKind::Char => nodes.as_char(stmt).map(Into::into),
        NodeKind::Tuple => nodes.as_tuple(stmt).map(Into::into),
        NodeKind::Array => nodes.as_array(stmt).map(Into::into),
        NodeKind::ArrayRepeat => nodes.as_array_repeat(stmt).map(Into::into),
        NodeKind::Call => nodes.as_call(stmt).map(Into::into),
        NodeKind::Field => nodes.as_field(stmt).map(Into::into),
        NodeKind::Binary => nodes.as_binary(stmt).map(Into::into),
        NodeKind::Postfix => nodes.as_postfix(stmt).map(Into::into),
        NodeKind::Prefix => nodes.as_prefix(stmt).map(Into::into),
        NodeKind::LoopExpr => nodes.as_loop_expr(stmt).map(Into::into),
        NodeKind::BreakExpr => nodes.as_break_expr(stmt).map(Into::into),
        NodeKind::ContinueExpr => nodes.as_continue_expr(stmt).map(Into::into),
        NodeKind::If => nodes.as_if(stmt).map(Into::into),
        NodeKind::Closure => nodes.as_closure(stmt).map(Into::into),
        NodeKind::Block => nodes.as_block(stmt).map(Into::into),
        NodeKind::UnsafeBlock => nodes.as_unsafe_block(stmt).map(Into::into),
        NodeKind::StructExpr => nodes.as_struct_expr(stmt).map(Into::into),
        _ => None,
    }
}

fn simplify<'db>(db: &'db dyn Database, ty: Ty<'db>, preserve: &FxHashSet<u32>) -> Ty<'db> {
    let mut polarities: FxHashMap<u32, (bool, bool)> = FxHashMap::default();
    let mut rec_vars: FxHashSet<u32> = FxHashSet::default();
    collect_polarities(
        db,
        ty,
        Polarity::Positive,
        &mut polarities,
        &mut rec_vars,
        &mut FxHashSet::default(),
    );

    let remove: FxHashSet<u32> = polarities
        .into_iter()
        .filter(|(id, (pos, neg))| {
            !(preserve.contains(id) || rec_vars.contains(id) || *pos && *neg)
        })
        .map(|(id, _)| id)
        .collect();

    remove_vars(db, ty, &remove)
}

fn collect_polarities(
    db: &dyn Database,
    ty: Ty<'_>,
    polarity: Polarity,
    polarities: &mut FxHashMap<u32, (bool, bool)>,
    rec_vars: &mut FxHashSet<u32>,
    seen: &mut FxHashSet<(u32, Polarity)>,
) {
    match ty.kind(db) {
        TyKind::Var(id) => {
            let entry = polarities.entry(*id).or_insert((false, false));
            match polarity {
                Polarity::Positive => entry.0 = true,
                Polarity::Negative => entry.1 = true,
            }
        }
        TyKind::Function { inputs, output } => {
            for input in inputs {
                collect_polarities(db, *input, polarity.flip(), polarities, rec_vars, seen);
            }
            collect_polarities(db, *output, polarity, polarities, rec_vars, seen);
        }
        TyKind::Array(item) => {
            collect_polarities(db, *item, polarity, polarities, rec_vars, seen);
        }
        TyKind::Tuple(items) => {
            for item in items {
                collect_polarities(db, *item, polarity, polarities, rec_vars, seen);
            }
        }
        TyKind::Record(fields) => {
            for (_, field_ty) in fields {
                collect_polarities(db, *field_ty, polarity, polarities, rec_vars, seen);
            }
        }
        TyKind::Union(items) | TyKind::Inter(items) => {
            for item in items {
                collect_polarities(db, *item, polarity, polarities, rec_vars, seen);
            }
        }
        TyKind::Rec(id, body) => {
            rec_vars.insert(*id);
            if !seen.insert((*id, polarity)) {
                return;
            }
            collect_polarities(db, *body, polarity, polarities, rec_vars, seen);
        }
        _ => {}
    }
}

fn remove_vars<'db>(db: &'db dyn Database, ty: Ty<'db>, remove: &FxHashSet<u32>) -> Ty<'db> {
    match ty.kind(db) {
        TyKind::Var(id) if remove.contains(id) => Ty::new(db, TyKind::Unknown),
        TyKind::Function { inputs, output } => {
            let inputs = inputs.iter().map(|&t| remove_vars(db, t, remove)).collect();
            let output = remove_vars(db, *output, remove);
            Ty::new(db, TyKind::Function { inputs, output })
        }
        TyKind::Array(item) => {
            let item = remove_vars(db, *item, remove);
            Ty::new(db, TyKind::Array(item))
        }
        TyKind::Tuple(items) => {
            let items = items.iter().map(|&t| remove_vars(db, t, remove)).collect();
            Ty::new(db, TyKind::Tuple(items))
        }
        TyKind::Record(fields) => {
            let fields =
                fields.iter().map(|(name, ty)| (*name, remove_vars(db, *ty, remove))).collect();
            Ty::new(db, TyKind::Record(fields))
        }
        TyKind::Union(items) => {
            let mut seen: FxHashSet<Ty<'db>> = FxHashSet::default();
            let mut reduced = Vec::new();
            for item in items {
                let reduced_item = remove_vars(db, *item, remove);
                if matches!(reduced_item.kind(db), TyKind::Unknown) {
                    continue;
                }
                if seen.insert(reduced_item) {
                    reduced.push(reduced_item);
                }
            }
            match reduced.len() {
                0 => Ty::new(db, TyKind::Unknown),
                1 => reduced.pop().expect("single element"),
                _ => Ty::new(db, TyKind::Union(reduced)),
            }
        }
        TyKind::Inter(items) => {
            let mut seen: FxHashSet<Ty<'db>> = FxHashSet::default();
            let mut reduced = Vec::new();
            for item in items {
                let reduced_item = remove_vars(db, *item, remove);
                if matches!(reduced_item.kind(db), TyKind::Unknown) {
                    continue;
                }
                if seen.insert(reduced_item) {
                    reduced.push(reduced_item);
                }
            }
            match reduced.len() {
                0 => Ty::new(db, TyKind::Unknown),
                1 => reduced.pop().expect("single element"),
                _ => Ty::new(db, TyKind::Inter(reduced)),
            }
        }
        TyKind::Rec(id, body) => {
            if remove.contains(id) {
                remove_vars(db, *body, remove)
            } else {
                let body = remove_vars(db, *body, remove);
                Ty::new(db, TyKind::Rec(*id, body))
            }
        }
        _ => ty,
    }
}

use std::future::Future;
use std::sync::Arc;

use mitki_hir::hir::{ExprId, Function, NodeKind, NodeStore, StmtId, TyId};
use mitki_hir::ty::{Ty, TyKind, TypeDatabase};
use mitki_lower::item::scope::{FunctionLocation, SignatureMap};
use mitki_resolve::{Resolution, Resolver};
use mitki_span::Symbol;
use rustc_hash::{FxHashMap, FxHashSet};

pub trait InferDb:
    mitki_resolve::ResolverDb + mitki_lower::item::scope::HasSignatureMapQuery + HasInferQuery
{
}

impl<T> InferDb for T where
    T: mitki_resolve::ResolverDb + mitki_lower::item::scope::HasSignatureMapQuery + HasInferQuery
{
}

#[rustfmt::skip]
#[picante::tracked]
pub async fn infer<DB: InferDb>(
    db: &DB,
    function: FunctionLocation,
) -> picante::PicanteResult<Arc<Inference>> {
    let hir = mitki_lower::hir::hir_function(db, function).await?;
    let file = function.file(db);
    let item_scope = mitki_lower::item::scope::item_scope(db, file).await?;
    let expr_scopes = mitki_resolve::scope::expr_scopes(db, function).await?;
    let builtin_scope = mitki_resolve::resolver::builtin_scope(db).await?;
    let signatures = mitki_lower::item::scope::signature_map(db, file).await?;

    let resolver =
        Resolver::for_scope(db, item_scope.clone(), expr_scopes, Arc::clone(&builtin_scope), None);
    let inference = Typer::new(db, hir.function(), resolver, item_scope, builtin_scope, signatures)
        .build();
    Ok(Arc::new(inference))
}

pub trait Inferable {
    fn infer<DB>(self, db: &DB) -> impl Future<Output = Arc<Inference>> + Send
    where
        DB: InferDb + Sync;
}

impl Inferable for FunctionLocation {
    #[allow(clippy::manual_async_fn)]
    fn infer<DB>(self, db: &DB) -> impl Future<Output = Arc<Inference>> + Send
    where
        DB: InferDb + Sync,
    {
        async move { infer(db, self).await.expect("failed to compute inference") }
    }
}

#[derive(Debug, Default, PartialEq, Eq, facet::Facet)]
pub struct Inference {
    #[facet(opaque)]
    type_of_node: FxHashMap<ExprId, Ty>,
    #[facet(opaque)]
    diagnostics: Vec<Diagnostic>,
}

impl Inference {
    pub fn type_of_node(&self, node: ExprId) -> Option<Ty> {
        self.type_of_node.get(&node).copied()
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Diagnostic {
    kind: DiagnosticKind,
    context: Option<ExprId>,
}

impl Diagnostic {
    fn new(kind: DiagnosticKind, context: Option<ExprId>) -> Self {
        Self { kind, context }
    }

    pub fn kind(&self) -> &DiagnosticKind {
        &self.kind
    }

    pub fn context(&self) -> Option<ExprId> {
        self.context
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum DiagnosticKind {
    UnresolvedIdent(ExprId),
    UnresolvedType(TyId, Symbol),
    TypeMismatch(ExprId, Ty, Ty),
    UnknownType(ExprId),
    ExpectedValueFoundType(ExprId, Ty),
    CallArityMismatch(ExprId, usize, usize),
    CallNonFunction(ExprId, Ty),
    ClosureArityMismatch(ExprId, usize, usize),
    InvalidBinaryOp(ExprId, Symbol, Ty, Ty),
    InvalidPrefixOp(ExprId, Symbol, Ty),
    InvalidPostfixOp(ExprId, Symbol, Ty),
    MissingElseBranch(ExprId),
    MissingParameterType(ExprId),
    MissingInitializer(ExprId),
    TupleArityMismatch(ExprId, usize, usize),
    MissingStructField(ExprId, Symbol),
    UnknownStructField(ExprId, Symbol),
    NotAStruct(ExprId, Ty),
}

type VarId = usize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum InferTy {
    Var(VarId),
    Function(Vec<InferTy>, Box<InferTy>),
    Tuple(Vec<InferTy>),
    Record(Vec<(u64, InferTy)>),
    Union(Vec<InferTy>),
    Inter(Vec<InferTy>),
    /// A solved/interned type leaf, stored as raw type bits.
    Known(u64),
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum NumericKind {
    Int,
    Float,
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
struct VariantConstraint {
    enum_var: VarId,
    variant: Symbol,
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

struct Typer<'func, 'db, DB>
where
    DB: InferDb,
{
    db: &'db DB,
    function: &'func Function,
    resolver: Resolver<'db, DB>,
    item_scope: Arc<mitki_lower::item::scope::ItemScope>,
    builtin_scope: Arc<FxHashMap<Symbol, Ty>>,
    function_signatures: Arc<SignatureMap>,
    vars: Vec<VarState>,
    env: FxHashMap<ExprId, Scheme>,
    binding_names: FxHashSet<ExprId>,
    type_param_env: FxHashMap<Symbol, InferTy>,
    // Internal inference representation. Do not expose directly to users/LSP.
    node_types: FxHashMap<ExprId, InferTy>,
    variant_constraints: Vec<VariantConstraint>,
    deferred_coercions: Vec<DeferredCoercion>,
    missing_param_nodes: FxHashSet<ExprId>,
    inference: Inference,
    context: Vec<ExprId>,
}

impl<'db, DB> Typer<'_, 'db, DB>
where
    DB: InferDb,
{
    fn new<'func>(
        db: &'db DB,
        function: &'func Function,
        resolver: Resolver<'db, DB>,
        item_scope: Arc<mitki_lower::item::scope::ItemScope>,
        builtin_scope: Arc<FxHashMap<Symbol, Ty>>,
        function_signatures: Arc<SignatureMap>,
    ) -> Typer<'func, 'db, DB> {
        Typer {
            db,
            function,
            resolver,
            item_scope,
            builtin_scope,
            function_signatures,
            vars: Vec::new(),
            env: FxHashMap::default(),
            binding_names: FxHashSet::default(),
            type_param_env: FxHashMap::default(),
            node_types: FxHashMap::default(),
            variant_constraints: Vec::new(),
            deferred_coercions: Vec::new(),
            missing_param_nodes: FxHashSet::default(),
            inference: Inference::default(),
            context: Vec::new(),
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
            InferTy::Tuple(items) => items.iter().map(|t| self.level(t)).max().unwrap_or(0),
            InferTy::Record(fields) => {
                fields.iter().map(|(_, ty)| self.level(ty)).max().unwrap_or(0)
            }
            InferTy::Union(items) | InferTy::Inter(items) => {
                items.iter().map(|t| self.level(t)).max().unwrap_or(0)
            }
        }
    }

    fn symbol_to_bits(sym: Symbol) -> u64 {
        sym.as_bits()
    }

    fn symbol_from_bits(bits: u64) -> Symbol {
        Symbol::from_bits(bits)
    }

    fn infer_ty_from_ty(ty: Ty) -> InferTy {
        InferTy::Known(ty.as_bits())
    }

    fn infer_ty_from_kind(&self, kind: TyKind) -> InferTy {
        Self::infer_ty_from_ty(Ty::new(self.db, kind))
    }

    fn known_ty(ty: &InferTy) -> Option<Ty> {
        match ty {
            InferTy::Known(bits) => Some(Ty::from_bits(*bits)),
            _ => None,
        }
    }

    fn numeric_kind(&self, ty: &InferTy) -> Option<NumericKind> {
        let ty = Self::known_ty(ty)?;
        match ty.kind(self.db) {
            TyKind::Int => Some(NumericKind::Int),
            TyKind::Float => Some(NumericKind::Float),
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

    fn string_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::String)
    }

    fn char_infer_ty(&self) -> InferTy {
        self.infer_ty_from_kind(TyKind::Char)
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

    fn emit(&mut self, kind: DiagnosticKind) {
        let context = self.context.last().copied();
        self.inference.diagnostics.push(Diagnostic::new(kind, context));
    }

    fn diagnostic_node(kind: &DiagnosticKind) -> Option<ExprId> {
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
            | DiagnosticKind::NotAStruct(node, _) => Some(*node),
            DiagnosticKind::UnresolvedType(_, _) => None,
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
                        | NodeKind::Closure
                        | NodeKind::Call
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
                | NodeKind::Closure
                | NodeKind::Call
                | NodeKind::StructExpr
        )
    }

    fn diagnostic_ty(&self, ty: &InferTy) -> Ty {
        self.present_type(ty, Polarity::Positive)
    }

    fn ty_to_infer_ty(&self, ty: Ty) -> InferTy {
        match ty.kind(self.db) {
            TyKind::Unknown => InferTy::Unknown,
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
                Box::new(self.ty_to_infer_ty(output)),
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

    fn enum_variant_infer_ty(&self, enum_ty: Ty, variant: Symbol) -> Option<InferTy> {
        let TyKind::Enum { variants, .. } = enum_ty.kind(self.db) else {
            return None;
        };

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

    fn resolve_path_in_node_scope(&mut self, node: ExprId, path: Symbol) -> Option<Resolution> {
        let guard = self.resolver.scopes_for_node(node);
        let resolution = self.resolver.resolve_path(path);
        self.resolver.reset(guard);
        resolution
    }

    fn infer_name_expr(&mut self, node: ExprId, lvl: usize) -> InferTy {
        let nodes = self.function.node_store();
        let name_id = nodes.as_name(node).expect("Name node mismatch");
        let path = nodes.name(name_id);

        let Some(resolution) = self.resolve_path_in_node_scope(node, path) else {
            self.emit(DiagnosticKind::UnresolvedIdent(node));
            return InferTy::Unknown;
        };

        match resolution {
            Resolution::Local(binding) => {
                let key: ExprId = binding.into();
                match self.env.get(&key).cloned() {
                    Some(scheme) => self.instantiate(&scheme, lvl),
                    None => {
                        self.node_types.get(&key).cloned().unwrap_or_else(|| self.fresh_var(lvl))
                    }
                }
            }
            Resolution::Function(function) => {
                let Some(signature) = self.function_signatures.get(&function).cloned() else {
                    return self.fresh_var(lvl);
                };
                let sig_nodes = signature.nodes();
                let params = signature.params();
                let ret_type = signature.ret_type();
                let type_params = signature.type_params();

                let type_param_vars: FxHashMap<Symbol, InferTy> =
                    type_params.iter().map(|&name| (name, self.fresh_var(lvl))).collect();

                let inputs: Vec<InferTy> = params
                    .iter()
                    .map(|&param| {
                        let (_, ty) = sig_nodes.param(param);
                        self.resolve_sig_type(ty, sig_nodes, &type_param_vars, lvl)
                    })
                    .collect();

                let output = if ret_type == TyId::ZERO {
                    InferTy::Tuple(Vec::new())
                } else {
                    self.resolve_sig_type(ret_type, sig_nodes, &type_param_vars, lvl)
                };

                InferTy::Function(inputs, Box::new(output))
            }
            Resolution::Type(ty) => {
                self.emit(DiagnosticKind::ExpectedValueFoundType(node, ty));
                InferTy::Unknown
            }
        }
    }

    fn infer_bare_enum_variant(
        &mut self,
        field_name: Symbol,
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

    fn infer_type_qualified_enum_variant(
        &mut self,
        field_expr: ExprId,
        field_name: Symbol,
        field_name_expr: ExprId,
    ) -> Option<InferTy> {
        let nodes = self.function.node_store();
        let base_name_id = nodes.as_name(field_expr).expect("field base should be Name");
        let base_name = nodes.name(base_name_id);
        let resolution = self.resolve_path_in_node_scope(field_expr, base_name);

        let Some(Resolution::Type(ty)) = resolution else {
            return None;
        };

        if let Some(variant_ty) = self.enum_variant_infer_ty(ty, field_name) {
            return Some(variant_ty);
        }

        if matches!(ty.kind(self.db), TyKind::Enum { .. }) {
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
                let ty = Ty::from_bits(bits);
                if let TyKind::Struct { fields, .. } = ty.kind(self.db) {
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
                    let ty = Ty::from_bits(bits);
                    if matches!(ty.kind(self.db), TyKind::Enum { .. }) {
                        candidates.insert(bits);
                    }
                }
                InferTy::Function(inputs, output) => {
                    stack.extend(inputs);
                    stack.push(*output);
                }
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

    fn solve_variant_constraint(&mut self, constraint: &VariantConstraint) -> bool {
        let candidates = self.collect_enum_candidates(constraint.enum_var);
        let mut narrowed = Vec::new();

        for candidate_bits in &candidates {
            let enum_ty = Ty::from_bits(*candidate_bits);
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

    fn coalesce_type_raw(&self, ty: &InferTy, polarity: Polarity) -> Ty {
        let mut recursive: FxHashMap<(VarId, Polarity), u32> = FxHashMap::default();
        let mut in_process: FxHashSet<(VarId, Polarity)> = FxHashSet::default();
        // Keep presentation-time recursive binders disjoint from inference vars.
        let mut next_present_var = self.vars.len() as u32;
        self.coalesce_raw(ty, polarity, &mut in_process, &mut recursive, &mut next_present_var)
    }

    // Presentation-only conversion: internal InferTy graph -> user-facing Ty plus
    // cleanup.
    fn present_type(&self, ty: &InferTy, polarity: Polarity) -> Ty {
        simplify(self.db, self.coalesce_type_raw(ty, polarity))
    }

    fn coalesce_type_for_missing_param(&self, ty: &InferTy) -> Ty {
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
    }

    fn coalesce_raw(
        &self,
        ty: &InferTy,
        polarity: Polarity,
        in_process: &mut FxHashSet<(VarId, Polarity)>,
        recursive: &mut FxHashMap<(VarId, Polarity), u32>,
        next_present_var: &mut u32,
    ) -> Ty {
        match ty {
            InferTy::Known(bits) => Ty::from_bits(*bits),
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
                let bound_types: Vec<Ty> = bounds
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

        if let Some(type_path) = nodes.as_type_path(ty) {
            let name = nodes.type_ref(type_path);
            if let Some(var) = self.type_param_env.get(&name) {
                return Some(var.clone());
            }

            let guard = self.resolver.scopes_for_type(ty);
            let resolved = if let Some(Resolution::Type(ty)) = self.resolver.resolve_path(name) {
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

    fn resolve_signature_type_name(&self, name: Symbol) -> Option<InferTy> {
        if let Some(ty) = self.item_scope.get_type(&name) {
            return Some(self.ty_to_infer_ty(ty));
        }

        self.builtin_scope.get(&name).map(|&ty| self.ty_to_infer_ty(ty))
    }

    fn resolve_sig_type(
        &mut self,
        ty: TyId,
        sig_nodes: &NodeStore,
        type_param_vars: &FxHashMap<Symbol, InferTy>,
        lvl: usize,
    ) -> InferTy {
        if ty == TyId::ZERO {
            return self.fresh_var(lvl);
        }

        if let Some(tuple_id) = sig_nodes.as_type_tuple(ty) {
            let items: Vec<InferTy> = sig_nodes
                .type_tuple(tuple_id)
                .iter()
                .map(|item| self.resolve_sig_type(item, sig_nodes, type_param_vars, lvl))
                .collect();
            return InferTy::Tuple(items);
        }

        if let Some(function_id) = sig_nodes.as_type_function(ty) {
            let (inputs_ty, output_ty) = sig_nodes.type_function(function_id);
            let inputs = if let Some(tuple_id) = sig_nodes.as_type_tuple(inputs_ty) {
                sig_nodes
                    .type_tuple(tuple_id)
                    .iter()
                    .map(|item| self.resolve_sig_type(item, sig_nodes, type_param_vars, lvl))
                    .collect()
            } else if inputs_ty == TyId::ZERO {
                Vec::new()
            } else {
                vec![self.resolve_sig_type(inputs_ty, sig_nodes, type_param_vars, lvl)]
            };
            let output = self.resolve_sig_type(output_ty, sig_nodes, type_param_vars, lvl);
            return InferTy::Function(inputs, Box::new(output));
        }

        if let Some(union_id) = sig_nodes.as_type_union(ty) {
            let (lhs_ty, rhs_ty) = sig_nodes.type_union(union_id);
            let lhs = self.resolve_sig_type(lhs_ty, sig_nodes, type_param_vars, lvl);
            let rhs = self.resolve_sig_type(rhs_ty, sig_nodes, type_param_vars, lvl);
            return Self::mk_union(lhs, rhs);
        }

        if let Some(inter_id) = sig_nodes.as_type_inter(ty) {
            let (lhs_ty, rhs_ty) = sig_nodes.type_inter(inter_id);
            let lhs = self.resolve_sig_type(lhs_ty, sig_nodes, type_param_vars, lvl);
            let rhs = self.resolve_sig_type(rhs_ty, sig_nodes, type_param_vars, lvl);
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
                    let field_ty = self.resolve_sig_type(field_ty, sig_nodes, type_param_vars, lvl);
                    Some((Self::symbol_to_bits(name), field_ty))
                })
                .collect();
            return InferTy::Record(fields);
        }

        let Some(type_path) = sig_nodes.as_type_path(ty) else {
            return self.fresh_var(lvl);
        };
        let name = sig_nodes.type_ref(type_path);

        if let Some(var) = type_param_vars.get(&name) {
            return var.clone();
        }

        self.resolve_signature_type_name(name).unwrap_or_else(|| self.fresh_var(lvl))
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
            NodeKind::Int => self.int_infer_ty(),
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
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(node).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let ty = self.infer_expr(var.initializer, lvl);
                self.node_types.insert(var.name.into(), ty.clone());
                self.binding_names.insert(var.name.into());
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

                for stmt in stmts.iter() {
                    self.typecheck_stmt(stmt, lvl);
                }

                if tail != ExprId::ZERO {
                    return self.typecheck_expr(tail, expected, lvl);
                }
                return InferTy::Tuple(Vec::new());
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
                    let expected = match kind {
                        NumericKind::Int => this.int_infer_ty(),
                        NumericKind::Float => this.float_infer_ty(),
                    };
                    this.constrain_top(ty, &expected).is_ok()
                };

                match op_sym.text(self.db).as_ref() {
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
                                    if !constrain_numeric(self, &lhs_ty, target)
                                        || !constrain_numeric(self, &rhs_ty, target)
                                    {
                                        emit_invalid(self)
                                    } else {
                                        match target {
                                            NumericKind::Int => self.int_infer_ty(),
                                            NumericKind::Float => self.float_infer_ty(),
                                        }
                                    }
                                }
                                None => InferTy::Unknown,
                            }
                        }
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        if matches!(lhs_ty, InferTy::Unknown) || matches!(rhs_ty, InferTy::Unknown)
                        {
                            InferTy::Unknown
                        } else {
                            let same = self.constrain_eq_top(&lhs_ty, &rhs_ty).is_ok();
                            if same { self.bool_infer_ty() } else { emit_invalid(self) }
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
                    match op_sym.text(self.db).as_ref() {
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
                                    let target_ty = match target {
                                        NumericKind::Int => self.int_infer_ty(),
                                        NumericKind::Float => self.float_infer_ty(),
                                    };
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
            NodeKind::Closure => {
                let (params, body) =
                    nodes.closure_parts(nodes.as_closure(node).expect("Closure node mismatch"));

                if let Some(InferTy::Function(exp_inputs, exp_output)) = &expected {
                    if exp_inputs.len() == params.len() {
                        for (param, exp_ty) in params.iter().zip(exp_inputs.iter()) {
                            let (name, ty_id) = nodes.param(param);
                            let annotated = self.resolve_type_to_infer_ty(ty_id);
                            let param_ty = match annotated {
                                Some(annotated_infer_ty) => {
                                    if !matches!(&annotated_infer_ty, InferTy::Unknown)
                                        && !matches!(exp_ty, InferTy::Unknown)
                                        && self.constrain_top(&annotated_infer_ty, exp_ty).is_err()
                                    {
                                        self.emit(DiagnosticKind::TypeMismatch(
                                            name.into(),
                                            self.diagnostic_ty(&annotated_infer_ty),
                                            self.diagnostic_ty(exp_ty),
                                        ));
                                    }
                                    annotated_infer_ty
                                }
                                None => exp_ty.clone(),
                            };
                            self.node_types.insert(name.into(), param_ty.clone());
                            self.env.insert(name.into(), Scheme::Mono(param_ty));
                            self.binding_names.insert(name.into());
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
                    let (name, ty_id) = nodes.param(param);
                    let param_ty = match self.resolve_type_to_infer_ty(ty_id) {
                        Some(ty) => ty,
                        None => self.fresh_var(lvl),
                    };
                    self.node_types.insert(name.into(), param_ty.clone());
                    self.env.insert(name.into(), Scheme::Mono(param_ty.clone()));
                    self.binding_names.insert(name.into());
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
                if let Some(variant_result) = self.infer_bare_variant_call(callee, &args, lvl) {
                    return variant_result;
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
                            for (&arg, input_ty) in args.iter().zip(inputs.iter()) {
                                self.check_expr(arg, input_ty, lvl);
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

                    let guard = self.resolver.scopes_for_node(name_id);
                    let resolution = self.resolver.resolve_path(name_sym);
                    self.resolver.reset(guard);

                    let Some(Resolution::Type(ty)) = resolution else {
                        self.emit(DiagnosticKind::UnresolvedIdent(name_id));
                        // Still infer field exprs.
                        let mut i = 1;
                        while i + 1 < items.len() {
                            self.infer_expr(items.get(i + 1).unwrap(), lvl);
                            i += 2;
                        }
                        return InferTy::Unknown;
                    };

                    let TyKind::Struct { fields, .. } = ty.kind(self.db) else {
                        // Still infer field exprs.
                        let mut i = 1;
                        while i + 1 < items.len() {
                            self.infer_expr(items.get(i + 1).unwrap(), lvl);
                            i += 2;
                        }
                        self.emit(DiagnosticKind::NotAStruct(node, ty));
                        return InferTy::Unknown;
                    };

                    let field_map: FxHashMap<Symbol, Ty> =
                        fields.iter().map(|(name, ty)| (*name, *ty)).collect();
                    let mut seen_fields: FxHashSet<Symbol> = FxHashSet::default();

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

                    for (field_name, _) in fields {
                        if !seen_fields.contains(&field_name) {
                            self.emit(DiagnosticKind::MissingStructField(node, field_name));
                        }
                    }

                    self.node_types.insert(name_id, self.ty_to_infer_ty(ty));
                    InferTy::Known(ty.as_bits())
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
        let nominal = Ty::from_bits(*bits);
        matches!(nominal.kind(self.db), TyKind::Enum { .. })
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

    fn typecheck_stmt(&mut self, stmt: StmtId, lvl: usize) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var_id = nodes.as_local_var(stmt).expect("LocalVar node mismatch");
                let var = nodes.local_var(var_id);
                let expected_infer_ty = self.resolve_type_to_infer_ty(var.ty);

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
                    self.emit(DiagnosticKind::MissingInitializer(var.name.into()));
                    expected_infer_ty.unwrap_or_else(|| self.fresh_var(lvl))
                };

                self.node_types.insert(var.name.into(), binding_ty.clone());
                self.env.insert(var.name.into(), Scheme::Poly { level: lvl, body: binding_ty });
                self.binding_names.insert(var.name.into());
            }
            _ => {
                if let Some(expr) = stmt_as_expr(nodes, stmt) {
                    self.infer_expr(expr, lvl);
                }
            }
        }
    }

    fn build(mut self) -> Inference {
        if self.function.body() == ExprId::ZERO {
            return self.inference;
        }

        // Create fresh type variables for each type parameter.
        for &tp in self.function.type_params() {
            let var = self.fresh_var(0);
            self.type_param_env.insert(tp, var);
        }

        for &param in self.function.params() {
            let (name, ty_id) = self.function.node_store().param(param);
            let param_ty = if let Some(ty) = self.resolve_type_to_infer_ty(ty_id) {
                ty
            } else {
                self.emit(DiagnosticKind::MissingParameterType(name.into()));
                self.missing_param_nodes.insert(name.into());
                self.fresh_var(0)
            };
            self.node_types.insert(name.into(), param_ty.clone());
            self.env.insert(name.into(), Scheme::Mono(param_ty));
            self.binding_names.insert(name.into());
        }

        let ret_ty = if self.function.ret_type() == TyId::ZERO {
            InferTy::Tuple(Vec::new())
        } else {
            self.resolve_type_to_infer_ty(self.function.ret_type())
                .unwrap_or_else(|| self.fresh_var(0))
        };

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
            (InferTy::Function(l_in, l_out), InferTy::Function(r_in, r_out))
                if l_in.len() == r_in.len() =>
            {
                for (l, r) in l_in.iter().zip(r_in.iter()) {
                    self.constrain(r, l, cache)?;
                }
                self.constrain(l_out, r_out, cache)
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
                let nominal = Ty::from_bits(*bits);
                let TyKind::Struct { fields, .. } = nominal.kind(self.db) else {
                    return Err(());
                };

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
                let nominal = Ty::from_bits(*bits);
                let TyKind::Struct { fields, .. } = nominal.kind(self.db) else {
                    return Err(());
                };

                if l_fields.len() != fields.len() {
                    return Err(());
                }

                for (field_name, field_ty) in fields {
                    let field_name_bits = Self::symbol_to_bits(field_name);
                    let Some((_, l_ty)) =
                        l_fields.iter().find(|(name_bits, _)| *name_bits == field_name_bits)
                    else {
                        return Err(());
                    };
                    let r_ty = self.ty_to_infer_ty(field_ty);
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
}

fn stmt_as_expr(nodes: &NodeStore, stmt: StmtId) -> Option<ExprId> {
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
        NodeKind::Call => nodes.as_call(stmt).map(Into::into),
        NodeKind::Field => nodes.as_field(stmt).map(Into::into),
        NodeKind::Binary => nodes.as_binary(stmt).map(Into::into),
        NodeKind::Postfix => nodes.as_postfix(stmt).map(Into::into),
        NodeKind::Prefix => nodes.as_prefix(stmt).map(Into::into),
        NodeKind::If => nodes.as_if(stmt).map(Into::into),
        NodeKind::Closure => nodes.as_closure(stmt).map(Into::into),
        NodeKind::Block => nodes.as_block(stmt).map(Into::into),
        NodeKind::StructExpr => nodes.as_struct_expr(stmt).map(Into::into),
        _ => None,
    }
}

fn simplify(db: &impl TypeDatabase, ty: Ty) -> Ty {
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
        .filter(|(id, (pos, neg))| !(rec_vars.contains(id) || *pos && *neg))
        .map(|(id, _)| id)
        .collect();

    remove_vars(db, ty, &remove)
}

fn collect_polarities(
    db: &impl TypeDatabase,
    ty: Ty,
    polarity: Polarity,
    polarities: &mut FxHashMap<u32, (bool, bool)>,
    rec_vars: &mut FxHashSet<u32>,
    seen: &mut FxHashSet<(u32, Polarity)>,
) {
    match ty.kind(db) {
        TyKind::Var(id) => {
            let entry = polarities.entry(id).or_insert((false, false));
            match polarity {
                Polarity::Positive => entry.0 = true,
                Polarity::Negative => entry.1 = true,
            }
        }
        TyKind::Function { inputs, output } => {
            for input in inputs {
                collect_polarities(db, input, polarity.flip(), polarities, rec_vars, seen);
            }
            collect_polarities(db, output, polarity, polarities, rec_vars, seen);
        }
        TyKind::Tuple(items) => {
            for item in items {
                collect_polarities(db, item, polarity, polarities, rec_vars, seen);
            }
        }
        TyKind::Record(fields) => {
            for (_, field_ty) in fields {
                collect_polarities(db, field_ty, polarity, polarities, rec_vars, seen);
            }
        }
        TyKind::Union(items) | TyKind::Inter(items) => {
            for item in items {
                collect_polarities(db, item, polarity, polarities, rec_vars, seen);
            }
        }
        TyKind::Rec(id, body) => {
            rec_vars.insert(id);
            if !seen.insert((id, polarity)) {
                return;
            }
            collect_polarities(db, body, polarity, polarities, rec_vars, seen);
        }
        _ => {}
    }
}

fn remove_vars(db: &impl TypeDatabase, ty: Ty, remove: &FxHashSet<u32>) -> Ty {
    match ty.kind(db) {
        TyKind::Var(id) if remove.contains(&id) => Ty::new(db, TyKind::Unknown),
        TyKind::Function { inputs, output } => {
            let inputs = inputs.into_iter().map(|t| remove_vars(db, t, remove)).collect();
            let output = remove_vars(db, output, remove);
            Ty::new(db, TyKind::Function { inputs, output })
        }
        TyKind::Tuple(items) => {
            let items = items.into_iter().map(|t| remove_vars(db, t, remove)).collect();
            Ty::new(db, TyKind::Tuple(items))
        }
        TyKind::Record(fields) => {
            let fields =
                fields.into_iter().map(|(name, ty)| (name, remove_vars(db, ty, remove))).collect();
            Ty::new(db, TyKind::Record(fields))
        }
        TyKind::Union(items) => {
            let mut seen: FxHashSet<Ty> = FxHashSet::default();
            let mut reduced = Vec::new();
            for item in items {
                let reduced_item = remove_vars(db, item, remove);
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
            let mut seen: FxHashSet<Ty> = FxHashSet::default();
            let mut reduced = Vec::new();
            for item in items {
                let reduced_item = remove_vars(db, item, remove);
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
            if remove.contains(&id) {
                remove_vars(db, body, remove)
            } else {
                let body = remove_vars(db, body, remove);
                Ty::new(db, TyKind::Rec(id, body))
            }
        }
        _ => ty,
    }
}

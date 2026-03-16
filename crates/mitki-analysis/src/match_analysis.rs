use std::fmt::Write as _;

use mitki_errors::{Diagnostic, TextRange};
use mitki_hir::hir::{ExprId, Function, NodeKind, NodeStore, PatId, StmtId};
use mitki_hir::ty::{StructTy, Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{FunctionLocation, enum_variants, struct_fields};
use mitki_span::Symbol;
use mitki_typeck::infer::{Inferable as _, Inference};
use patmat::{
    MatchArm, MatchInput, ReachabilityWarning, Space, SpaceContext, SpaceKind, check_match,
};
use salsa::plumbing::AsId as _;

pub(super) fn check_function_matches(
    db: &dyn salsa::Database,
    func: FunctionLocation<'_>,
) -> Vec<Diagnostic> {
    let hir_function = func.hir_function(db);
    let function = hir_function.function(db);
    if function.body() == ExprId::ZERO {
        return Vec::new();
    }

    let mut visitor = MatchVisitor {
        db,
        function,
        source_map: hir_function.source_map(db),
        inference: func.infer(db),
        diagnostics: Vec::new(),
    };
    visitor.expr(function.body());
    visitor.diagnostics
}

struct MatchVisitor<'db> {
    db: &'db dyn salsa::Database,
    function: &'db Function<'db>,
    source_map: &'db mitki_lower::hir::FunctionSourceMap,
    inference: &'db Inference<'db>,
    diagnostics: Vec<Diagnostic>,
}

impl<'db> MatchVisitor<'db> {
    fn expr(&mut self, expr: ExprId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(expr) {
            NodeKind::Tuple => {
                for item in nodes.tuple(nodes.as_tuple(expr).expect("Tuple node mismatch")).iter() {
                    self.expr(item);
                }
            }
            NodeKind::Array => {
                for item in nodes.array(nodes.as_array(expr).expect("Array node mismatch")).iter() {
                    self.expr(item);
                }
            }
            NodeKind::ArrayRepeat => {
                let (value, len) =
                    nodes.array_repeat(nodes.as_array_repeat(expr).expect("ArrayRepeat mismatch"));
                self.expr(value);
                self.expr(len);
            }
            NodeKind::Block => {
                let (stmts, tail) =
                    nodes.block_stmts(nodes.as_block(expr).expect("Block mismatch"));
                for stmt in stmts.iter() {
                    self.stmt(stmt);
                }
                if tail != ExprId::ZERO {
                    self.expr(tail);
                }
            }
            NodeKind::Call => {
                let (callee, args) = nodes.call(nodes.as_call(expr).expect("Call node mismatch"));
                if callee != ExprId::ZERO {
                    self.expr(callee);
                }
                for arg in args.iter() {
                    self.expr(arg);
                }
            }
            NodeKind::Field => {
                let (base, _) = nodes.field(nodes.as_field(expr).expect("Field node mismatch"));
                if base != ExprId::ZERO {
                    self.expr(base);
                }
            }
            NodeKind::Binary => {
                let binary = nodes.binary(nodes.as_binary(expr).expect("Binary node mismatch"));
                self.expr(binary.lhs);
                self.expr(binary.rhs);
            }
            NodeKind::Prefix => {
                let prefix = nodes.prefix(nodes.as_prefix(expr).expect("Prefix node mismatch"));
                self.expr(prefix.expr);
            }
            NodeKind::Postfix => {
                let postfix = nodes.postfix(nodes.as_postfix(expr).expect("Postfix mismatch"));
                self.expr(postfix.expr);
            }
            NodeKind::If => {
                let if_expr = nodes.if_expr(nodes.as_if(expr).expect("If node mismatch"));
                self.expr(if_expr.cond);
                if if_expr.then_branch != ExprId::ZERO {
                    self.expr(if_expr.then_branch);
                }
                if if_expr.else_branch != ExprId::ZERO {
                    self.expr(if_expr.else_branch);
                }
            }
            NodeKind::Match => {
                self.analyze_match(expr);
                let (scrutinee, arms) =
                    nodes.match_expr(nodes.as_match(expr).expect("Match node mismatch"));
                self.expr(scrutinee);
                for arm in arms.iter() {
                    let (_, arm_expr) =
                        nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
                    self.expr(arm_expr);
                }
            }
            NodeKind::LoopExpr => {
                let (body, _) =
                    nodes.loop_expr(nodes.as_loop_expr(expr).expect("LoopExpr node mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body);
                }
            }
            NodeKind::LocalVar => {
                let var = nodes.local_var(nodes.as_local_var(expr).expect("LocalVar mismatch"));
                if var.initializer != ExprId::ZERO {
                    self.expr(var.initializer);
                }
            }
            NodeKind::Closure => {
                let (_, body) =
                    nodes.closure_parts(nodes.as_closure(expr).expect("Closure mismatch"));
                if body != ExprId::ZERO {
                    self.expr(body);
                }
            }
            NodeKind::StructExpr => {
                let items = nodes.struct_expr(nodes.as_struct_expr(expr).expect("Struct mismatch"));
                let has_struct_name = items.len() % 2 == 1;
                let mut index = if has_struct_name { 2 } else { 1 };
                while index < items.len() {
                    self.expr(items.get(index).expect("struct field expr"));
                    index += 2;
                }
            }
            _ => {}
        }
    }

    fn stmt(&mut self, stmt: StmtId) {
        let nodes = self.function.node_store();
        match nodes.node_kind(stmt) {
            NodeKind::LocalVar => {
                let var = nodes.local_var(nodes.as_local_var(stmt).expect("LocalVar mismatch"));
                if var.initializer != ExprId::ZERO {
                    self.expr(var.initializer);
                }
            }
            NodeKind::ReturnStmt => {
                let (value, _) =
                    nodes.return_stmt(nodes.as_return_stmt(stmt).expect("ReturnStmt mismatch"));
                if value != ExprId::ZERO {
                    self.expr(value);
                }
            }
            _ => {}
        }
    }

    fn analyze_match(&mut self, match_expr: ExprId) {
        let nodes = self.function.node_store();
        let (scrutinee, arms) =
            nodes.match_expr(nodes.as_match(match_expr).expect("Match node mismatch"));
        let Some(scrutinee_ty) = self.inference.type_of_node(scrutinee) else {
            return;
        };

        let mut arm_patterns = Vec::with_capacity(arms.len());
        for arm in arms.iter() {
            let (pattern, _) = nodes.match_arm(nodes.as_match_arm(arm).expect("MatchArm mismatch"));
            arm_patterns.push(pattern);
        }

        let ops = MatchOperations {
            db: self.db,
            int_literals: collect_int_literals(self.db, nodes, &arm_patterns),
        };
        let mut context = SpaceContext::new();
        let scrutinee_space = context.of_type(SpaceTy::Base(scrutinee_ty));
        let lowerer =
            PatternLowerer { db: self.db, function: self.function, inference: self.inference };

        let mut match_arms = Vec::with_capacity(arm_patterns.len());
        for &pattern in &arm_patterns {
            let Some(pattern_space) =
                lowerer.lower_pattern_space(&mut context, pattern, scrutinee_ty)
            else {
                return;
            };
            let arm = if is_wildcard_pattern(nodes, pattern) {
                MatchArm::wildcard(pattern_space)
            } else {
                MatchArm::new(pattern_space)
            };
            match_arms.push(arm);
        }

        let analysis =
            check_match(&ops, &mut context, &MatchInput::new(scrutinee_space, match_arms));
        if let Some(example) = analysis.uncovered_spaces.first().copied() {
            self.diagnostics.push(Diagnostic::error(
                format!(
                    "non-exhaustive match; missing case for `{}`",
                    render_space(self.db, &context, example)
                ),
                self.node_range(match_expr),
            ));
        }

        for warning in analysis.reachability_warnings {
            let ReachabilityWarning::Unreachable { arm_index, .. } = warning else {
                continue;
            };
            let Some(&pattern) = arm_patterns.get(arm_index) else {
                continue;
            };
            self.diagnostics
                .push(Diagnostic::error("unreachable match arm", self.pat_range(pattern)));
        }
    }

    fn node_range(&self, expr: ExprId) -> TextRange {
        self.source_map.try_node_syntax(expr).map_or_else(|| self.body_range(), |ptr| ptr.range)
    }

    fn pat_range(&self, pattern: PatId) -> TextRange {
        self.source_map.try_pat_syntax(pattern).map_or_else(|| self.body_range(), |ptr| ptr.range)
    }

    fn body_range(&self) -> TextRange {
        self.source_map
            .try_node_syntax(self.function.body())
            .expect("function body should have syntax")
            .range
    }
}

fn is_wildcard_pattern(nodes: &NodeStore<'_>, pattern: PatId) -> bool {
    match nodes.node_kind(pattern) {
        NodeKind::PatWildcard => true,
        NodeKind::PatTyped => {
            let (inner, _) = nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped"));
            inner != PatId::ZERO && is_wildcard_pattern(nodes, inner)
        }
        NodeKind::PatParen => {
            let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
            inner != PatId::ZERO && is_wildcard_pattern(nodes, inner)
        }
        _ => false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum SpaceTy<'db> {
    Base(Ty<'db>),
    Bool(bool),
    IntLiteral(i32),
    IntRest(Vec<i32>),
    UnionMember { parent: Ty<'db>, index: usize, member: Ty<'db> },
    EnumVariant { parent: Ty<'db>, variant: Symbol<'db> },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum SpaceExtractor<'db> {
    Tuple(usize),
    StructFields { nominal: Option<StructTy<'db>>, fields: Vec<Symbol<'db>> },
    EnumVariant(Symbol<'db>),
    UnionMember { parent: Ty<'db>, index: usize, member: Ty<'db> },
}

fn collect_int_literals<'db>(
    db: &dyn salsa::Database,
    nodes: &NodeStore<'db>,
    patterns: &[PatId],
) -> Vec<i32> {
    let mut values = Vec::new();
    for &pattern in patterns {
        collect_pattern_int_literals(db, nodes, pattern, &mut values);
    }
    values.sort_unstable();
    values.dedup();
    values
}

fn collect_pattern_int_literals<'db>(
    db: &dyn salsa::Database,
    nodes: &NodeStore<'db>,
    pattern: PatId,
    values: &mut Vec<i32>,
) {
    if pattern == PatId::ZERO {
        return;
    }

    match nodes.node_kind(pattern) {
        NodeKind::PatInt => {
            if let Ok(value) =
                parse_int_literal(nodes.pat_int(nodes.as_pat_int(pattern).expect("PatInt")), db)
            {
                values.push(value);
            }
        }
        NodeKind::PatTyped => {
            let (inner, _) = nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped"));
            collect_pattern_int_literals(db, nodes, inner, values);
        }
        NodeKind::PatParen => {
            let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
            collect_pattern_int_literals(db, nodes, inner, values);
        }
        NodeKind::PatTuple => {
            for item in nodes.pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple")).iter() {
                collect_pattern_int_literals(db, nodes, item, values);
            }
        }
        NodeKind::PatVariant => {
            let (_, args) = nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant"));
            for arg in args.iter() {
                collect_pattern_int_literals(db, nodes, arg, values);
            }
        }
        NodeKind::PatStruct => {
            let (_, fields) = nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"));
            for field in fields.iter() {
                let (_, nested) = nodes
                    .pat_struct_field(nodes.as_pat_struct_field(field).expect("PatStructField"));
                collect_pattern_int_literals(db, nodes, nested, values);
            }
        }
        _ => {}
    }
}

struct PatternLowerer<'db> {
    db: &'db dyn salsa::Database,
    function: &'db Function<'db>,
    inference: &'db Inference<'db>,
}

impl<'db> PatternLowerer<'db> {
    fn lower_pattern_space(
        &self,
        context: &mut SpaceContext<SpaceTy<'db>, SpaceExtractor<'db>>,
        pattern: PatId,
        expected_ty: Ty<'db>,
    ) -> Option<Space<SpaceTy<'db>, SpaceExtractor<'db>>> {
        if let Some(space) = self.lower_selected_union_space(context, pattern, expected_ty) {
            return Some(space);
        }
        self.lower_pattern_space_inner(context, pattern, expected_ty)
    }

    fn lower_selected_union_space(
        &self,
        context: &mut SpaceContext<SpaceTy<'db>, SpaceExtractor<'db>>,
        pattern: PatId,
        expected_ty: Ty<'db>,
    ) -> Option<Space<SpaceTy<'db>, SpaceExtractor<'db>>> {
        let TyKind::Union(members) = expected_ty.kind(self.db) else {
            return None;
        };
        let selected_member = self.inference.selected_union_member(pattern)?;
        let arm_index = members.iter().position(|member| *member == selected_member)?;
        let inner = self.lower_pattern_space_inner(context, pattern, selected_member)?;
        Some(context.product(
            SpaceTy::Base(expected_ty),
            SpaceExtractor::UnionMember {
                parent: expected_ty,
                index: arm_index,
                member: selected_member,
            },
            vec![inner],
        ))
    }

    fn lower_pattern_space_inner(
        &self,
        context: &mut SpaceContext<SpaceTy<'db>, SpaceExtractor<'db>>,
        pattern: PatId,
        expected_ty: Ty<'db>,
    ) -> Option<Space<SpaceTy<'db>, SpaceExtractor<'db>>> {
        if pattern == PatId::ZERO {
            return Some(context.of_type(SpaceTy::Base(expected_ty)));
        }

        let nodes = self.function.node_store();
        match nodes.node_kind(pattern) {
            NodeKind::PatBinding | NodeKind::PatWildcard => {
                Some(context.of_type(SpaceTy::Base(expected_ty)))
            }
            NodeKind::PatTyped => {
                let (inner, _) = nodes.pat_typed(nodes.as_pat_typed(pattern).expect("PatTyped"));
                if self.inference.matched_typed_pattern(pattern) != Some(expected_ty) {
                    return None;
                }
                self.lower_pattern_space(context, inner, expected_ty)
            }
            NodeKind::PatTrue if matches!(expected_ty.kind(self.db), TyKind::Bool) => {
                Some(context.atomic_type(SpaceTy::Bool(true)))
            }
            NodeKind::PatFalse if matches!(expected_ty.kind(self.db), TyKind::Bool) => {
                Some(context.atomic_type(SpaceTy::Bool(false)))
            }
            NodeKind::PatTrue | NodeKind::PatFalse => None,
            NodeKind::PatInt => Some(context.atomic_type(SpaceTy::IntLiteral(
                if matches!(expected_ty.kind(self.db), TyKind::Int) {
                    parse_int_literal(
                        nodes.pat_int(nodes.as_pat_int(pattern).expect("PatInt")),
                        self.db,
                    )
                    .ok()?
                } else {
                    return None;
                },
            ))),
            NodeKind::PatParen => {
                let (inner, _) = nodes.pat_paren(nodes.as_pat_paren(pattern).expect("PatParen"));
                self.lower_pattern_space(context, inner, expected_ty)
            }
            NodeKind::PatTuple => {
                let TyKind::Tuple(item_tys) = expected_ty.kind(self.db) else {
                    return None;
                };
                let items = nodes
                    .pat_tuple(nodes.as_pat_tuple(pattern).expect("PatTuple"))
                    .iter()
                    .collect::<Vec<_>>();
                if items.len() != item_tys.len() {
                    return None;
                }
                let parameters = items
                    .iter()
                    .zip(item_tys.iter())
                    .map(|(&item, item_ty)| self.lower_pattern_space(context, item, *item_ty))
                    .collect::<Option<Vec<_>>>()?;
                Some(context.product(
                    SpaceTy::Base(expected_ty),
                    SpaceExtractor::Tuple(parameters.len()),
                    parameters,
                ))
            }
            NodeKind::PatStruct => self.lower_struct_pattern_space(context, pattern, expected_ty),
            NodeKind::PatVariant => self.lower_variant_pattern_space(context, pattern, expected_ty),
            _ => None,
        }
    }

    fn lower_struct_pattern_space(
        &self,
        context: &mut SpaceContext<SpaceTy<'db>, SpaceExtractor<'db>>,
        pattern: PatId,
        expected_ty: Ty<'db>,
    ) -> Option<Space<SpaceTy<'db>, SpaceExtractor<'db>>> {
        let nominal = match expected_ty.kind(self.db) {
            TyKind::Struct(struct_ty) => Some(*struct_ty),
            TyKind::Record(_) => None,
            _ => return None,
        };
        let nodes = self.function.node_store();
        let (_, fields) = nodes.pat_struct(nodes.as_pat_struct(pattern).expect("PatStruct"));
        let mut lowered_fields = fields
            .iter()
            .map(|field| {
                let (name, nested) = nodes
                    .pat_struct_field(nodes.as_pat_struct_field(field).expect("PatStructField"));
                let field_name = nodes.name(name);
                let field_ty = struct_or_record_field_ty(self.db, expected_ty, field_name)?;
                let parameter = if nested != PatId::ZERO {
                    self.lower_pattern_space(context, nested, field_ty)?
                } else {
                    context.of_type(SpaceTy::Base(field_ty))
                };
                Some((field_name, parameter))
            })
            .collect::<Option<Vec<_>>>()?;
        lowered_fields.sort_by_key(|(name, _)| name.as_id().as_bits());
        let field_names = lowered_fields.iter().map(|(name, _)| *name).collect::<Vec<_>>();
        let parameters = lowered_fields.into_iter().map(|(_, parameter)| parameter).collect();
        Some(context.product(
            SpaceTy::Base(expected_ty),
            SpaceExtractor::StructFields { nominal, fields: field_names },
            parameters,
        ))
    }

    fn lower_variant_pattern_space(
        &self,
        context: &mut SpaceContext<SpaceTy<'db>, SpaceExtractor<'db>>,
        pattern: PatId,
        expected_ty: Ty<'db>,
    ) -> Option<Space<SpaceTy<'db>, SpaceExtractor<'db>>> {
        let TyKind::Enum(enum_ty) = expected_ty.kind(self.db) else {
            return None;
        };
        let nodes = self.function.node_store();
        let (path, args) = nodes.pat_variant(nodes.as_pat_variant(pattern).expect("PatVariant"));
        let field_id = nodes.as_field(path)?;
        let (_, variant_name_expr) = nodes.field(field_id);
        let variant_name = nodes.as_name(variant_name_expr)?;
        let variant_sym = nodes.name(variant_name);
        let payload_tys = enum_variants(self.db, *enum_ty)
            .iter()
            .find(|(name, _)| *name == variant_sym)
            .map(|(_, payload)| payload.clone())?;
        let args = args.iter().collect::<Vec<_>>();
        if args.len() != payload_tys.len() {
            return None;
        }
        let parameters = args
            .iter()
            .zip(payload_tys.iter())
            .map(|(&arg, payload_ty)| self.lower_pattern_space(context, arg, *payload_ty))
            .collect::<Option<Vec<_>>>()?;
        Some(context.product(
            SpaceTy::Base(expected_ty),
            SpaceExtractor::EnumVariant(variant_sym),
            parameters,
        ))
    }
}

struct MatchOperations<'db> {
    db: &'db dyn salsa::Database,
    int_literals: Vec<i32>,
}

impl<'db> patmat::SpaceOperations for MatchOperations<'db> {
    type Type = SpaceTy<'db>;
    type Extractor = SpaceExtractor<'db>;

    fn decompose_type(&self, value_type: &Self::Type) -> patmat::Decomposition<Self::Type> {
        match value_type {
            SpaceTy::Base(ty) => match ty.kind(self.db) {
                TyKind::Bool => {
                    patmat::Decomposition::parts(vec![SpaceTy::Bool(false), SpaceTy::Bool(true)])
                }
                TyKind::Int => {
                    if self.int_literals.is_empty() {
                        patmat::Decomposition::NotDecomposable
                    } else {
                        let mut parts = self
                            .int_literals
                            .iter()
                            .copied()
                            .map(SpaceTy::IntLiteral)
                            .collect::<Vec<_>>();
                        parts.push(SpaceTy::IntRest(self.int_literals.clone()));
                        patmat::Decomposition::Parts(parts)
                    }
                }
                TyKind::Union(members) => patmat::Decomposition::parts(
                    members
                        .iter()
                        .enumerate()
                        .map(|(index, member)| SpaceTy::UnionMember {
                            parent: *ty,
                            index,
                            member: *member,
                        })
                        .collect(),
                ),
                TyKind::Enum(enum_ty) => patmat::Decomposition::parts(
                    enum_variants(self.db, *enum_ty)
                        .iter()
                        .map(|(variant, _)| SpaceTy::EnumVariant { parent: *ty, variant: *variant })
                        .collect(),
                ),
                _ => patmat::Decomposition::NotDecomposable,
            },
            _ => patmat::Decomposition::NotDecomposable,
        }
    }

    fn is_subtype(&self, left: &Self::Type, right: &Self::Type) -> bool {
        if left == right {
            return true;
        }

        match (left, right) {
            (SpaceTy::Bool(_), SpaceTy::Base(ty)) => matches!(ty.kind(self.db), TyKind::Bool),
            (SpaceTy::IntLiteral(_) | SpaceTy::IntRest(_), SpaceTy::Base(ty)) => {
                matches!(ty.kind(self.db), TyKind::Int)
            }
            (SpaceTy::UnionMember { parent, .. }, SpaceTy::Base(ty)) => *parent == *ty,
            (SpaceTy::EnumVariant { parent, .. }, SpaceTy::Base(ty)) => *parent == *ty,
            (SpaceTy::Base(left_ty), SpaceTy::Base(right_ty)) => {
                semantic_is_subtype(self.db, *left_ty, *right_ty)
            }
            _ => false,
        }
    }

    fn extractors_are_equivalent(&self, left: &Self::Extractor, right: &Self::Extractor) -> bool {
        left == right
    }

    fn extractor_parameter_types(
        &self,
        extractor: &Self::Extractor,
        scrutinee_type: &Self::Type,
        arity: usize,
    ) -> Vec<Self::Type> {
        let parameters = match extractor {
            SpaceExtractor::Tuple(expected_arity) => match scrutinee_type {
                SpaceTy::Base(ty) => match ty.kind(self.db) {
                    TyKind::Tuple(items) if items.len() == *expected_arity => {
                        items.iter().copied().map(SpaceTy::Base).collect()
                    }
                    _ => Vec::new(),
                },
                _ => Vec::new(),
            },
            SpaceExtractor::StructFields { fields, .. } => {
                struct_field_types(self.db, scrutinee_type, fields)
                    .into_iter()
                    .map(SpaceTy::Base)
                    .collect()
            }
            SpaceExtractor::EnumVariant(variant) => match scrutinee_type {
                SpaceTy::EnumVariant { parent, variant: actual } if actual == variant => {
                    enum_variant_payload_types(self.db, *parent, *variant)
                        .into_iter()
                        .map(SpaceTy::Base)
                        .collect()
                }
                _ => Vec::new(),
            },
            SpaceExtractor::UnionMember { member, .. } => match scrutinee_type {
                SpaceTy::UnionMember { member: actual, .. } if actual == member => {
                    vec![SpaceTy::Base(*member)]
                }
                _ => Vec::new(),
            },
        };
        debug_assert_eq!(parameters.len(), arity);
        parameters
    }

    fn extractor_covers_type(
        &self,
        extractor: &Self::Extractor,
        scrutinee_type: &Self::Type,
        arity: usize,
    ) -> bool {
        match extractor {
            SpaceExtractor::Tuple(expected_arity) => match scrutinee_type {
                SpaceTy::Base(ty) => {
                    matches!(ty.kind(self.db), TyKind::Tuple(items) if items.len() == *expected_arity && items.len() == arity)
                }
                _ => false,
            },
            SpaceExtractor::StructFields { nominal, fields } => match scrutinee_type {
                SpaceTy::Base(ty) => match (nominal, ty.kind(self.db)) {
                    (Some(expected), TyKind::Struct(actual)) if expected == actual => {
                        struct_field_types(self.db, scrutinee_type, fields).len() == arity
                    }
                    (None, TyKind::Record(_)) => {
                        struct_field_types(self.db, scrutinee_type, fields).len() == arity
                    }
                    _ => false,
                },
                _ => false,
            },
            SpaceExtractor::EnumVariant(variant) => match scrutinee_type {
                SpaceTy::EnumVariant { parent, variant: actual } if actual == variant => {
                    enum_variant_payload_types(self.db, *parent, *variant).len() == arity
                }
                _ => false,
            },
            SpaceExtractor::UnionMember { parent, index, member } => match scrutinee_type {
                SpaceTy::UnionMember {
                    parent: actual_parent,
                    index: actual_index,
                    member: actual_member,
                } => {
                    actual_parent == parent
                        && actual_index == index
                        && actual_member == member
                        && arity == 1
                }
                _ => false,
            },
        }
    }

    fn intersect_atomic_types(
        &self,
        left: &Self::Type,
        right: &Self::Type,
    ) -> patmat::AtomicIntersection<Self::Type> {
        let intersection = match (left, right) {
            (SpaceTy::IntLiteral(value), SpaceTy::IntRest(excluded))
            | (SpaceTy::IntRest(excluded), SpaceTy::IntLiteral(value)) => {
                (!excluded.contains(value)).then_some(SpaceTy::IntLiteral(*value))
            }
            (SpaceTy::Base(left_ty), SpaceTy::Base(right_ty)) => {
                intersect_semantic_types(self.db, *left_ty, *right_ty).map(SpaceTy::Base)
            }
            _ => None,
        };
        match intersection {
            Some(ty) => patmat::AtomicIntersection::Type(ty),
            None => patmat::AtomicIntersection::Empty,
        }
    }
}

fn semantic_is_subtype(db: &dyn salsa::Database, left: Ty<'_>, right: Ty<'_>) -> bool {
    if left == right {
        return true;
    }
    match right.kind(db) {
        TyKind::Union(members) => {
            members.iter().any(|member| semantic_is_subtype(db, left, *member))
        }
        _ => false,
    }
}

fn intersect_semantic_types<'db>(
    db: &'db dyn salsa::Database,
    left: Ty<'db>,
    right: Ty<'db>,
) -> Option<Ty<'db>> {
    if semantic_is_subtype(db, left, right) {
        Some(left)
    } else if semantic_is_subtype(db, right, left) {
        Some(right)
    } else {
        None
    }
}

fn struct_or_record_field_ty<'db>(
    db: &'db dyn salsa::Database,
    expected_ty: Ty<'db>,
    field_name: Symbol<'db>,
) -> Option<Ty<'db>> {
    match expected_ty.kind(db) {
        TyKind::Struct(struct_ty) => struct_fields(db, *struct_ty)
            .iter()
            .find(|(name, _)| *name == field_name)
            .map(|(_, ty)| *ty),
        TyKind::Record(fields) => {
            fields.iter().find(|(name, _)| *name == field_name).map(|(_, ty)| *ty)
        }
        _ => None,
    }
}

fn struct_field_types<'db>(
    db: &'db dyn salsa::Database,
    scrutinee_type: &SpaceTy<'db>,
    fields: &[Symbol<'db>],
) -> Vec<Ty<'db>> {
    let SpaceTy::Base(ty) = scrutinee_type else {
        return Vec::new();
    };
    fields.iter().filter_map(|field| struct_or_record_field_ty(db, *ty, *field)).collect()
}

fn enum_variant_payload_types<'db>(
    db: &'db dyn salsa::Database,
    enum_ty: Ty<'db>,
    variant: Symbol<'db>,
) -> Vec<Ty<'db>> {
    let TyKind::Enum(enum_ty_id) = enum_ty.kind(db) else {
        return Vec::new();
    };
    enum_variants(db, *enum_ty_id)
        .iter()
        .find(|(name, _)| *name == variant)
        .map(|(_, payload)| payload.clone())
        .unwrap_or_default()
}

fn render_space<'db>(
    db: &'db dyn salsa::Database,
    context: &SpaceContext<SpaceTy<'db>, SpaceExtractor<'db>>,
    space: Space<SpaceTy<'db>, SpaceExtractor<'db>>,
) -> String {
    match context.kind(space) {
        SpaceKind::Empty => "never".to_owned(),
        SpaceKind::Type(type_space) => match type_space.value_type {
            SpaceTy::Base(ty) => ty.display(db).to_string(),
            SpaceTy::Bool(value) => value.to_string(),
            SpaceTy::IntLiteral(value) => value.to_string(),
            SpaceTy::IntRest(_) => "int".to_owned(),
            SpaceTy::UnionMember { member, .. } => member.display(db).to_string(),
            SpaceTy::EnumVariant { variant, .. } => format!(".{}", variant.text(db)),
        },
        SpaceKind::Union(spaces) => spaces
            .iter()
            .map(|space| render_space(db, context, *space))
            .collect::<Vec<_>>()
            .join(" | "),
        SpaceKind::Product(product) => match product.extractor {
            SpaceExtractor::Tuple(_) => format!(
                "({})",
                product
                    .parameters
                    .iter()
                    .map(|space| render_space(db, context, *space))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            SpaceExtractor::StructFields { nominal, fields } => {
                let mut rendered = String::new();
                if let Some(nominal) = nominal {
                    let _ = write!(rendered, "{} ", nominal.name(db).text(db));
                }
                rendered.push('{');
                for (index, (field, parameter)) in
                    fields.iter().zip(product.parameters.iter()).enumerate()
                {
                    if index > 0 {
                        rendered.push_str(", ");
                    }
                    let _ = write!(
                        rendered,
                        "{}: {}",
                        field.text(db),
                        render_space(db, context, *parameter)
                    );
                }
                rendered.push('}');
                rendered
            }
            SpaceExtractor::EnumVariant(variant) => {
                if product.parameters.is_empty() {
                    format!(".{}", variant.text(db))
                } else {
                    format!(
                        ".{}({})",
                        variant.text(db),
                        product
                            .parameters
                            .iter()
                            .map(|space| render_space(db, context, *space))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }
            }
            SpaceExtractor::UnionMember { .. } => product
                .parameters
                .first()
                .map_or_else(|| "unknown".to_owned(), |space| render_space(db, context, *space)),
        },
    }
}

fn parse_int_literal(literal: Option<Symbol<'_>>, db: &dyn salsa::Database) -> Result<i32, String> {
    let Some(literal) = literal else {
        return Err("missing integer literal".to_owned());
    };

    let text = literal.text(db).replace('_', "");
    let (radix, digits) = if let Some(rest) = text.strip_prefix("0b") {
        (2, rest)
    } else if let Some(rest) = text.strip_prefix("0o") {
        (8, rest)
    } else if let Some(rest) = text.strip_prefix("0x") {
        (16, rest)
    } else {
        (10, text.as_str())
    };

    let value = i64::from_str_radix(digits, radix).map_err(|error| error.to_string())?;
    i32::try_from(value).map_err(|_overflow| "integer literal does not fit in i32".to_owned())
}

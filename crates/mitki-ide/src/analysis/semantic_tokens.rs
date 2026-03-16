use mitki_analysis::{ResolveIntent, Semantics};
use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_resolve::BindingId;
use mitki_yellow::SyntaxKind;
use text_size::TextRange;

#[derive(Clone, Copy)]
pub struct SemanticToken {
    pub range: TextRange,
    pub kind: SemanticTokenKind,
}

#[derive(Clone, Copy)]
pub enum SemanticTokenKind {
    Function,
    Parameter,
    Variable,
    Type,
    EnumMember,
    BuiltinType,
    BuiltinFunction,
}

impl super::Analysis {
    pub fn semantic_tokens(&self, file: File, range: Option<TextRange>) -> Vec<SemanticToken> {
        let db = self.db();
        let semantics = Semantics::new(db, file);
        let root = file.parse(db).syntax_node();
        let mut tokens = Vec::new();
        let mut nodes = vec![root];

        while let Some(node) = nodes.pop() {
            let children = node.children_with_tokens().collect::<Vec<_>>();
            for child in children.into_iter().rev() {
                match child {
                    mitki_yellow::SyntaxElement::Node(child_node) => nodes.push(child_node),
                    mitki_yellow::SyntaxElement::Token(token) => {
                        if token.kind() != SyntaxKind::NAME || token.is_trivia() {
                            continue;
                        }

                        let trimmed_range = token.trimmed_range();
                        if let Some(filter) = range
                            && (trimmed_range.end() <= filter.start()
                                || trimmed_range.start() >= filter.end())
                        {
                            continue;
                        }

                        let node = token.parent();
                        let resolved_node = node
                            .ancestors()
                            .find(|ancestor| {
                                matches!(
                                    ancestor.kind(),
                                    SyntaxKind::PATH_EXPR | SyntaxKind::PATH_TYPE
                                )
                            })
                            .unwrap_or(node);
                        if !matches!(
                            resolved_node.kind(),
                            SyntaxKind::IDENT
                                | SyntaxKind::NAME_REF
                                | SyntaxKind::PATH_EXPR
                                | SyntaxKind::PATH_TYPE
                        ) {
                            continue;
                        }

                        let resolution =
                            semantics.resolve_at(db, &resolved_node, ResolveIntent::Any);
                        let Some(binding) = resolution.binding else {
                            continue;
                        };

                        let kind = match binding {
                            BindingId::Function(_) => SemanticTokenKind::Function,
                            BindingId::Param(_) => SemanticTokenKind::Parameter,
                            BindingId::Local(_) => SemanticTokenKind::Variable,
                            BindingId::Struct(_) | BindingId::Enum(_) => SemanticTokenKind::Type,
                            BindingId::EnumVariant(_) => SemanticTokenKind::EnumMember,
                            BindingId::BuiltinType(_) => SemanticTokenKind::BuiltinType,
                            BindingId::RuntimeFunction(_) | BindingId::CompilerIntrinsic(_) => {
                                SemanticTokenKind::BuiltinFunction
                            }
                        };

                        tokens.push(SemanticToken { range: trimmed_range, kind });
                    }
                }
            }
        }

        tokens.sort_by_key(|token| token.range.start());
        tokens
    }
}

#[cfg(test)]
mod tests {
    use mitki_inputs::File;

    use super::SemanticTokenKind;
    use crate::Analysis;

    #[test]
    fn classifies_binding_kinds() {
        let analysis = Analysis::default();
        let file = File::new(
            analysis.db(),
            "semantic_tokens.mitki".into(),
            r#"
struct Point {
    x: int,
}

enum Color {
    Red,
}

fun paint(point: Point) {
    val local = point
    local
    Color.Red
    std::io::print_int(1)
}
"#
            .to_owned(),
        );

        let tokens = analysis.semantic_tokens(file, None);

        assert!(tokens.iter().any(|token| matches!(token.kind, SemanticTokenKind::Type)));
        assert!(tokens.iter().any(|token| matches!(token.kind, SemanticTokenKind::Function)));
        assert!(tokens.iter().any(|token| matches!(token.kind, SemanticTokenKind::Parameter)));
        assert!(tokens.iter().any(|token| matches!(token.kind, SemanticTokenKind::Variable)));
        assert!(tokens.iter().any(|token| matches!(token.kind, SemanticTokenKind::EnumMember)));
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum SyntaxKind {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_BRACE,
    RIGHT_BRACE,
    EQ,
    DOT,

    FUN_KW,
    IF_KW,
    LOOP_KW,
    VAL_KW,
    WHILE_KW,
    NAME,

    NUMBER,
    BINARY_OPERATOR,
    POSTFIX_OPERATOR,
    PREFIX_OPERATOR,

    UNKNOWN,
    EOF,

    MODULE,
    SEQUENCE,
    LITERAL,
    BINARY_EXPR,
    POSTFIX_EXPR,
    PREFIX_EXPR,
    PAREN_EXPR,
    ERROR,
    TOMBSTONE,
}

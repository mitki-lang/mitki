use std::fmt::Display;

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
    COMMA,
    COLON,
    SEMICOLON,
    FUN_KW,
    IF_KW,
    ELSE_KW,
    LOOP_KW,
    VAL_KW,
    WHILE_KW,
    RETURN_KW,
    BREAK_KW,
    NAME,

    INT_NUMBER,
    FLOAT_NUMBER,
    BINARY_OPERATOR,
    POSTFIX_OPERATOR,
    PREFIX_OPERATOR,

    UNKNOWN,
    EOF,

    BINARY_EXPR,
    ARG_LIST,
    LOOP_EXPR,
    IF_EXPR,
    ERROR,
    EXPR_STMT,
    FIELD_EXPR,
    FN,
    RETURN_TYPE,
    IDENT,
    PATH_EXPR,
    TYPE_PARAM,
    LITERAL,
    TUPLE_EXPR,
    ARRAY_EXPR,
    MODULE,
    PARAM,
    PARAM_LIST,
    GENERIC_PARAM_LIST,
    PAREN_EXPR,
    POSTFIX_EXPR,
    PATH_TYPE,
    PREFIX_EXPR,
    CALL_EXPR,
    STMT_LIST,
    VAL_STMT,
    RETURN_STMT,
    TOMBSTONE,
}

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::LEFT_PAREN => "`(`",
            Self::RIGHT_PAREN => "`)`",
            Self::LEFT_BRACKET => "`[`",
            Self::RIGHT_BRACKET => "`]`",
            Self::LEFT_BRACE => "`{`",
            Self::RIGHT_BRACE => "`}`",
            Self::EQ => "`=`",
            Self::DOT => "`.`",
            Self::COMMA => "`,`",
            Self::COLON => "`:`",
            Self::SEMICOLON => "`;`",
            Self::FUN_KW => "`fun`",
            Self::IF_KW => "`if`",
            Self::ELSE_KW => "`else`",
            Self::LOOP_KW => "`loop`",
            Self::VAL_KW => "`val`",
            Self::WHILE_KW => "`while`",
            Self::RETURN_KW => "`return`",
            Self::BREAK_KW => "`break`",
            Self::NAME => "identifier",
            Self::INT_NUMBER => "int",
            Self::FLOAT_NUMBER => "float",
            Self::BINARY_OPERATOR => "binary operator",
            Self::POSTFIX_OPERATOR => "postfix operator",
            Self::PREFIX_OPERATOR => "prefix operator",
            Self::UNKNOWN => "unknown",
            Self::EOF => "EOF",
            _ => "<syntax node>",
        })
    }
}

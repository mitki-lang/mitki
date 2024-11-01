use mitki_yellow::SyntaxKind::*;

use super::{delimited, name, types};
use crate::parser::{CompletedMarker, Parser};

pub(crate) fn stmt(p: &mut Parser) {
    match p.peek_kind() {
        SEMICOLON => p.advance(),
        VAL_KW => {
            let m = p.start();
            p.advance();
            name(p, &[VAL_KW, SEMICOLON]);
            if p.at(COLON) {
                types::ascription(p);
            }
            p.expect(EQ);
            expr(p);
            m.complete(p, VAL_STMT);
        }
        RETURN_KW => {
            let m = p.start();
            p.advance();
            expr(p);
            m.complete(p, RETURN_STMT);
        }
        BREAK_KW => {
            let m = p.start();
            p.advance();
            m.complete(p, BREAK_KW);
        }
        _ => _ = expr(p).map(|m| m.precede(p).complete(p, EXPR_STMT)),
    }
}

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    let mut lhs = unary_expr(p)?;

    while p.peek_kind() == BINARY_OPERATOR {
        let m = lhs.precede(p);
        p.advance();
        expr(p);
        lhs = m.complete(p, BINARY_EXPR);
    }

    lhs.into()
}

pub(crate) fn block(p: &mut Parser<'_>) {
    if p.peek_kind() != LEFT_BRACE {
        p.error("expected a block");
        return;
    }

    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        stmt(p);
    }

    p.expect(RIGHT_BRACE);
    m.complete(p, STMT_LIST);
}

fn unary_expr(p: &mut Parser) -> Option<CompletedMarker> {
    match p.peek_kind() {
        LOOP_KW => {
            let m = p.start();
            p.advance();
            block(p);
            m.complete(p, LOOP_EXPR).into()
        }
        IF_KW => if_(p),
        PREFIX_OPERATOR => {
            let m = p.start();
            p.advance();
            unary_expr(p);
            m.complete(p, PREFIX_EXPR).into()
        }
        BINARY_OPERATOR => {
            let m = p.start();
            p.error("unary operator cannot be separated from its operand");
            p.advance();
            unary_expr(p);
            m.complete(p, PREFIX_OPERATOR).into()
        }
        _ => postfix_expr(p),
    }
}

fn if_(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert_eq!(p.peek_kind(), IF_KW);

    let m = p.start();
    p.advance();
    expr(p);
    block(p);
    if p.at(ELSE_KW) {
        p.advance();
        if p.at(IF_KW) {
            if_(p);
        } else {
            block(p);
        }
    }
    m.complete(p, IF_EXPR).into()
}

fn postfix_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let mut head = primary_expr(p)?;

    loop {
        head = match p.peek_kind() {
            POSTFIX_OPERATOR => {
                p.advance();
                head.precede(p).complete(p, POSTFIX_EXPR)
            }
            LEFT_PAREN if p.next_token_on_same_line() => {
                let m = p.start();
                delimited(
                    p,
                    LEFT_PAREN,
                    RIGHT_PAREN,
                    COMMA,
                    "expected expression",
                    &[INT_NUMBER, FLOAT_NUMBER, LEFT_PAREN, PREFIX_OPERATOR],
                    |p| expr(p).is_some(),
                );
                m.complete(p, ARG_LIST);
                head.precede(p).complete(p, CALL_EXPR)
            }
            LEFT_BRACKET if p.next_token_on_same_line() => {
                p.advance();
                expr(p);
                p.expect(RIGHT_BRACKET);
                head.precede(p).complete(p, INDEX_EXPR)
            }
            _ => break,
        }
    }

    head.into()
}

fn primary_expr(p: &mut Parser) -> Option<CompletedMarker> {
    match p.peek_kind() {
        INT_NUMBER | FLOAT_NUMBER => {
            let m = p.start();
            p.advance();
            m.complete(p, LITERAL).into()
        }
        LEFT_PAREN => {
            let m = p.start();
            p.advance();

            let mut saw_comma = false;
            let mut saw_expr = false;

            if p.eat(COMMA) {
                p.error("expected expression");
                saw_comma = true;
            }

            while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
                saw_expr = true;

                if expr(p).is_none() {
                    break;
                }

                if !p.at(RIGHT_PAREN) {
                    saw_comma = true;
                    p.expect(COMMA);
                }
            }

            p.expect(RIGHT_PAREN);
            m.complete(p, if saw_expr && !saw_comma { PAREN_EXPR } else { TUPLE_EXPR }).into()
        }
        LEFT_BRACKET => {
            let m = p.start();
            p.advance();

            let mut n_exprs = 0u32;
            let mut has_semi = false;

            while !matches!(p.peek_kind(), RIGHT_BRACKET | EOF) {
                n_exprs += 1;

                if expr(p).is_none() {
                    break;
                }

                if n_exprs == 1 && p.eat(SEMICOLON) {
                    has_semi = true;
                    continue;
                }

                if has_semi || !p.at(RIGHT_BRACKET) && !p.expect(COMMA) {
                    break;
                }
            }

            p.expect(RIGHT_BRACKET);
            m.complete(p, ARRAY_EXPR).into()
        }
        NAME => {
            let m = p.start();
            name(p, &[]);
            m.complete(p, PATH_EXPR).into()
        }
        DOT => {
            let m = p.start();
            p.advance();
            p.expect(NAME);
            m.complete(p, FIELD_EXPR).into()
        }
        _ => {
            let m = p.start();
            p.error("expected expression");
            p.advance();
            m.complete(p, ERROR);
            None
        }
    }
}

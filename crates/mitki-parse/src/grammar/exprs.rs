use mitki_yellow::SyntaxKind::*;

use crate::parser::{CompletedMarker, Parser};

pub(crate) fn stmt(p: &mut Parser) {
    match p.peek_kind() {
        VAL_KW => {
            let m = p.start();
            p.advance();
            m.complete(p, VAL_STMT);
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
        p.advance();
        return;
    }

    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        stmt(p);
    }

    p.expect(RIGHT_BRACE, "expected '}'");
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
    let mut m = primary_expr(p)?;

    while let POSTFIX_OPERATOR = p.peek_kind() {
        p.advance();
        m = m.precede(p).complete(p, POSTFIX_EXPR)
    }

    m.into()
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
            if !p.at(RIGHT_PAREN) {
                expr(p);
            }
            p.expect(RIGHT_PAREN, "expected ')'");
            m.complete(p, PAREN_EXPR).into()
        }
        NAME => {
            let m = p.start();
            p.advance();
            m.complete(p, IDENT).into()
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

use mitki_yellow::SyntaxKind::*;

use crate::parser::{CompletedMarker, Parser};

pub(crate) fn module(p: &mut Parser) {
    let m = p.start();

    while p.peek_kind() != EOF {
        expr(p);
    }

    p.expect(EOF, "expected end of file");
    m.complete(p, MODULE);
}

fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    let mut lhs = unary_expr(p)?;

    while p.peek_kind() == BINARY_OPERATOR {
        let m = lhs.precede(p);
        p.advance();
        expr(p);
        lhs = m.complete(p, BINARY_EXPR);
    }

    lhs.into()
}

fn unary_expr(p: &mut Parser) -> Option<CompletedMarker> {
    match p.peek_kind() {
        PREFIX_OPERATOR => {
            let m = p.start();
            p.advance();
            unary_expr(p);
            m.complete(p, PREFIX_EXPR).into()
        }
        _ => postfix_expr(p),
    }
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
        NUMBER => {
            let m = p.start();
            p.advance();
            m.complete(p, LITERAL).into()
        }
        _ => {
            let m = p.start();
            p.error("expected a primary expression");
            p.advance();
            m.complete(p, ERROR);
            None
        }
    }
}

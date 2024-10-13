use mitki_yellow::SyntaxKind::*;

use super::{expr, name, types};
use crate::parser::Parser;

pub(crate) fn module(p: &mut Parser) {
    let m = p.start();

    while p.peek_kind() != EOF {
        item(p);
    }

    p.expect(EOF, "expected end of file");
    m.complete(p, MODULE);
}

fn item(p: &mut Parser) {
    match p.peek_kind() {
        FUN_KW => {
            let m = p.start();
            p.advance();

            name(p);

            if p.at(LEFT_PAREN) {
                param_list(p);
            } else {
                p.error("expected function parameters");
            }

            if p.at(LEFT_BRACE) {
                block(p);
            } else {
                p.error("expected function body");
            }

            m.complete(p, FN);
        }
        _ => {
            p.error("expected item");
            p.advance();
        }
    }
}

fn block(p: &mut Parser<'_>) {
    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        expr::stmt(p);
    }

    p.expect(RIGHT_BRACE, "expected '}'");
    m.complete(p, STMT_LIST);
}

fn param_list(p: &mut Parser) {
    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
        param(p);
        if p.at(COMMA) {
            p.advance();
        }
    }

    p.expect(RIGHT_PAREN, "expected ')'");
    m.complete(p, PARAM_LIST);
}

fn param(p: &mut Parser) {
    let m = p.start();
    p.advance();

    if p.at(COLON) {
        types::ascription(p);
    } else {
        p.error("missing type for function parameter");
    }

    p.advance();

    m.complete(p, PARAM);
}

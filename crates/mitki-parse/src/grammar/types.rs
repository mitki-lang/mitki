use mitki_yellow::SyntaxKind::*;

use crate::parser::Parser;

pub(crate) fn type_(p: &mut Parser) {
    match p.peek_kind() {
        NAME => {
            let m = p.start();
            p.advance();
            m.complete(p, PATH_TYPE);
        }
        _ => p.error("expected a type"),
    }
}

pub(crate) fn ascription(p: &mut Parser) {
    debug_assert_eq!(p.peek_kind(), COLON);
    p.advance();
    type_(p);
}

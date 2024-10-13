use mitki_yellow::SyntaxKind::*;

use crate::parser::Parser;

pub(crate) fn type_(_p: &mut Parser) {}

pub(crate) fn ascription(p: &mut Parser) {
    debug_assert_eq!(p.peek_kind(), COLON);
    p.advance();
    type_(p);
}

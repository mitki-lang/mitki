use mitki_yellow::SyntaxKind::{self, *};
use mitki_yellow::SyntaxSet;

use crate::parser::Parser;

mod exprs;
pub(crate) mod items;
mod types;

pub(crate) fn name(p: &mut Parser, recovery: &SyntaxSet) {
    match p.peek_kind() {
        NAME => {
            let m = p.start();
            p.advance();
            m.complete(p, IDENT);
        }
        _ => p.error_recover("expected identifier", recovery),
    }
}

pub(crate) fn delimited(
    p: &mut Parser<'_>,
    bra: SyntaxKind,
    ket: SyntaxKind,
    delim: SyntaxKind,
    unexpected_delim_message: &'static str,
    first_set: &SyntaxSet,
    mut parser: impl FnMut(&mut Parser<'_>) -> bool,
) {
    debug_assert_eq!(p.peek_kind(), bra);
    p.advance();

    while !p.at(ket) && !p.at(EOF) {
        if p.at(delim) {
            let m = p.start();
            p.error(unexpected_delim_message);
            p.advance();
            m.complete(p, ERROR);
            continue;
        }

        if !parser(p) {
            break;
        }

        if !p.eat(delim) {
            if first_set.contains(p.peek_kind()) {
                p.expect(delim);
            } else {
                break;
            }
        }
    }

    p.expect(ket);
}

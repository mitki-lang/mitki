use mitki_yellow::SyntaxKind::*;

use crate::parser::Parser;

mod expr;
pub(crate) mod items;
mod types;

pub(crate) fn name(p: &mut Parser) {
    match p.peek_kind() {
        NAME => {
            let m = p.start();
            p.advance();
            m.complete(p, IDENT);
        }
        _ => p.error("expected name"),
    }
}

use mitki_yellow::SyntaxKind::*;
use mitki_yellow::SyntaxSet;

use super::name;
use crate::parser::{CompletedMarker, Parser};

pub(crate) fn type_(p: &mut Parser) {
    if union_type(p).is_none() && !matches!(p.peek_kind(), RIGHT_PAREN | RIGHT_BRACE | EOF) {
        p.advance();
    }
}

pub(crate) fn ascription(p: &mut Parser) {
    debug_assert_eq!(p.peek_kind(), COLON);
    p.advance();
    type_(p);
}

fn union_type(p: &mut Parser) -> Option<CompletedMarker> {
    let mut head = inter_type(p)?;

    while p.at_binary_op("|") {
        let m = head.precede(p);
        p.advance();
        if inter_type(p).is_none() {
            head = m.complete(p, UNION_TYPE);
            break;
        }
        head = m.complete(p, UNION_TYPE);
    }

    Some(head)
}

fn inter_type(p: &mut Parser) -> Option<CompletedMarker> {
    let mut head = type_atom(p)?;

    while p.at_binary_op("&") {
        let m = head.precede(p);
        p.advance();
        if type_atom(p).is_none() {
            head = m.complete(p, INTER_TYPE);
            break;
        }
        head = m.complete(p, INTER_TYPE);
    }

    Some(head)
}

fn type_atom(p: &mut Parser) -> Option<CompletedMarker> {
    match p.peek_kind() {
        NAME => {
            let m = p.start();
            p.advance();
            Some(m.complete(p, PATH_TYPE))
        }
        LEFT_PAREN => tuple_type(p),
        LEFT_BRACE => record_type(p),
        FUN_KW => function_type(p),
        _ => {
            p.error("expected a type");
            None
        }
    }
}

fn tuple_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.peek_kind() != LEFT_PAREN {
        p.error("expected `(`");
        return None;
    }

    let m = p.start();
    p.advance();

    if p.eat(COMMA) {
        p.error("expected a type");
    }

    while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
        type_(p);

        if !p.eat(COMMA) {
            if p.at(RIGHT_PAREN) {
                break;
            }
            p.expect(COMMA);
            break;
        }
    }

    p.expect(RIGHT_PAREN);
    Some(m.complete(p, TUPLE_TYPE))
}

fn function_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.peek_kind() != FUN_KW {
        p.error("expected `fun`");
        return None;
    }

    let m = p.start();
    p.advance();

    let input_tuple = p.start();
    if p.at(LEFT_PAREN) {
        p.advance();
        while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
            type_(p);

            if !p.eat(COMMA) {
                if p.at(RIGHT_PAREN) {
                    break;
                }
                p.expect(COMMA);
                break;
            }
        }
        p.expect(RIGHT_PAREN);
    } else {
        p.error("expected `(`");
    }
    input_tuple.complete(p, TUPLE_TYPE);

    if p.at_binary_op("->") {
        p.advance();
    } else {
        p.error("expected `->`");
    }

    type_(p);

    Some(m.complete(p, FUNCTION_TYPE))
}

fn record_type(p: &mut Parser) -> Option<CompletedMarker> {
    if p.peek_kind() != LEFT_BRACE {
        p.error("expected `{`");
        return None;
    }

    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if p.at(COMMA) {
            p.error("expected field name");
            p.advance();
            continue;
        }

        let field = p.start();
        name(p, &SyntaxSet::new([COLON, COMMA, RIGHT_BRACE]));
        p.expect(COLON);
        type_(p);
        field.complete(p, STRUCT_FIELD);

        if !p.eat(COMMA) {
            if p.at(NAME) {
                p.expect(COMMA);
            } else {
                break;
            }
        }
    }

    p.expect(RIGHT_BRACE);
    Some(m.complete(p, RECORD_TYPE))
}

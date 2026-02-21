use mitki_yellow::SyntaxKind::*;
use mitki_yellow::SyntaxSet;

use super::{delimited, exprs, name, types};
use crate::parser::Parser;

pub(crate) fn module(p: &mut Parser) {
    let m = p.start();

    while p.peek_kind() != EOF {
        item(p);
    }

    p.expect(EOF);
    m.complete(p, MODULE);
}

fn item(p: &mut Parser) {
    match p.peek_kind() {
        FUN_KW => {
            let m = p.start();
            p.advance();

            name(p, &SyntaxSet::new([FUN_KW, SEMICOLON]));
            generic_param_list(p);

            if p.at(LEFT_PAREN) {
                param_list(p);
            } else {
                p.error("expected function parameters");
            }

            if p.at(COLON) {
                let m = p.start();
                types::ascription(p);
                m.complete(p, RETURN_TYPE);
            }

            exprs::block(p);

            m.complete(p, FN);
        }
        STRUCT_KW => {
            let m = p.start();
            p.advance();

            name(p, &SyntaxSet::new([LEFT_BRACE, SEMICOLON]));
            generic_param_list(p);

            if p.at(LEFT_BRACE) {
                struct_field_list(p);
            } else {
                p.error("expected `{`");
            }

            m.complete(p, STRUCT_DEF);
        }
        ENUM_KW => {
            let m = p.start();
            p.advance();

            name(p, &SyntaxSet::new([LEFT_BRACE, SEMICOLON]));
            generic_param_list(p);

            if p.at(LEFT_BRACE) {
                enum_variant_list(p);
            } else {
                p.error("expected `{`");
            }

            m.complete(p, ENUM_DEF);
        }
        SEMICOLON => p.error_and_bump("expected item, found `;`"),
        _ => {
            let m = p.start();
            p.error("expected an item");
            while !matches!(p.peek_kind(), EOF | FUN_KW | VAL_KW | STRUCT_KW | ENUM_KW) {
                p.advance();
            }
            m.complete(p, ERROR);
        }
    }
}

fn generic_param_list(p: &mut Parser) {
    if p.peek_kind() != LEFT_BRACKET {
        return;
    }

    delimited(
        p,
        LEFT_BRACKET,
        RIGHT_BRACKET,
        COMMA,
        "expected generic parameter",
        &SyntaxSet::new([NAME]),
        generic_param,
    );
}

fn generic_param(p: &mut Parser) -> bool {
    match p.peek_kind() {
        NAME => {
            let m = p.start();
            p.advance();
            m.complete(p, TYPE_PARAM);
            true
        }
        _ => false,
    }
}

fn param_list(p: &mut Parser) {
    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
        if p.peek_kind() != NAME {
            p.error("expected parameter name");
            if p.eat(COMMA) {
                continue;
            }
            break;
        }

        param(p);

        if !p.eat(COMMA) {
            if p.peek_kind() == NAME {
                p.expect(COMMA);
            } else {
                break;
            }
        }
    }

    p.expect(RIGHT_PAREN);
    m.complete(p, PARAM_LIST);
}

fn param(p: &mut Parser) {
    let m = p.start();
    name(p, &SyntaxSet::EMPTY);

    if p.at(COLON) {
        types::ascription(p);
    }

    m.complete(p, PARAM);
}

fn struct_field_list(p: &mut Parser) {
    let m = p.start();
    p.advance(); // eat LEFT_BRACE

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if p.peek_kind() != NAME {
            p.error("expected field name");
            if p.eat(COMMA) {
                continue;
            }
            break;
        }

        let field = p.start();
        name(p, &SyntaxSet::EMPTY);

        if p.at(COLON) {
            types::ascription(p);
        } else {
            p.error("missing type for struct field");
        }
        field.complete(p, STRUCT_FIELD);

        if !p.eat(COMMA) {
            if p.peek_kind() == NAME {
                p.expect(COMMA);
            } else {
                break;
            }
        }
    }

    p.expect(RIGHT_BRACE);
    m.complete(p, STRUCT_FIELD_LIST);
}

fn enum_variant_list(p: &mut Parser) {
    let m = p.start();
    p.advance(); // eat LEFT_BRACE

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if p.peek_kind() != NAME {
            p.error("expected variant name");
            if p.eat(COMMA) {
                continue;
            }
            break;
        }

        let variant = p.start();
        name(p, &SyntaxSet::EMPTY);

        if p.at(LEFT_PAREN) {
            let tl = p.start();
            p.advance();
            while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
                types::type_(p);
                if !p.at(RIGHT_PAREN) {
                    p.expect(COMMA);
                }
            }
            p.expect(RIGHT_PAREN);
            tl.complete(p, TUPLE_TYPE);
        }

        variant.complete(p, ENUM_VARIANT);

        if !p.eat(COMMA) {
            if p.peek_kind() == NAME {
                p.expect(COMMA);
            } else {
                break;
            }
        }
    }

    p.expect(RIGHT_BRACE);
    m.complete(p, ENUM_VARIANT_LIST);
}

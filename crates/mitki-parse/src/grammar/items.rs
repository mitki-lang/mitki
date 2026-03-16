use mitki_yellow::SyntaxKind::*;
use mitki_yellow::SyntaxSet;

use super::{delimited, exprs, name, patterns, types};
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
        NAME if matches!(p.peek_text(), "export" | "import")
            && p.nth_kind(1) == NAME
            && p.nth_text(1) == "instance" =>
        {
            instance_item(p);
        }
        NAME if looks_like_function_item_start(p) => function(p),
        NAME if p.peek_text() == "extern" && p.nth_kind(1) == STRUCT_KW => {
            struct_def(p);
        }
        FUN_KW => {
            function(p);
        }
        STRUCT_KW => {
            struct_def(p);
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
        MOD_KW => {
            module_item(p);
        }
        PUB_KW if p.nth_kind(1) == MOD_KW => {
            module_item(p);
        }
        USE_KW => {
            use_item(p);
        }
        SEMICOLON => p.error_and_bump("expected item, found `;`"),
        _ => {
            let m = p.start();
            p.error("expected an item");
            while !matches!(
                p.peek_kind(),
                EOF | FUN_KW | VAL_KW | STRUCT_KW | ENUM_KW | MOD_KW | PUB_KW | USE_KW
            ) {
                p.advance();
            }
            m.complete(p, ERROR);
        }
    }
}

fn looks_like_function_item_start(p: &mut Parser) -> bool {
    let mut index = 0usize;

    loop {
        match p.nth_kind(index) {
            NAME if matches!(p.nth_text(index), "comptime" | "export" | "unsafe") => {
                index += 1;
            }
            NAME if p.nth_text(index) == "import" => {
                index += 1;
                if p.nth_kind(index) != STRING {
                    return false;
                }
                index += 1;
            }
            FUN_KW => return true,
            _ => return false,
        }
    }
}

fn struct_def(p: &mut Parser) {
    let m = p.start();
    if p.peek_kind() == NAME && p.peek_text() == "extern" {
        p.advance();
    }
    p.expect(STRUCT_KW);

    name(p, &SyntaxSet::new([LEFT_BRACE, SEMICOLON]));
    generic_param_list(p);

    if p.at(LEFT_BRACE) {
        struct_field_list(p);
    } else {
        p.error("expected `{`");
    }

    m.complete(p, STRUCT_DEF);
}

fn module_item(p: &mut Parser) {
    let m = p.start();
    p.eat(PUB_KW);
    p.expect(MOD_KW);
    name(p, &SyntaxSet::new([SEMICOLON]));
    p.expect(SEMICOLON);
    m.complete(p, MOD_ITEM);
}

fn use_item(p: &mut Parser) {
    let m = p.start();
    p.expect(USE_KW);

    let path = p.start();
    exprs::path_segments(p);
    path.complete(p, PATH_EXPR);

    if p.eat(AS_KW) {
        name(p, &SyntaxSet::new([SEMICOLON]));
    }

    p.expect(SEMICOLON);
    m.complete(p, USE_ITEM);
}

fn instance_item(p: &mut Parser) {
    let m = p.start();
    if p.peek_kind() == NAME && matches!(p.peek_text(), "export" | "import") {
        p.advance();
    } else {
        p.error("expected `export` or `import`");
    }

    if p.peek_kind() == NAME && p.peek_text() == "instance" {
        p.advance();
    } else {
        p.error("expected `instance`");
    }

    name(p, &SyntaxSet::new([LEFT_BRACKET, SEMICOLON]));
    generic_arg_list(p);
    p.expect(SEMICOLON);
    m.complete(p, INSTANCE_ITEM);
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct FunctionModifiers {
    import: bool,
}

fn function(p: &mut Parser) {
    let m = p.start();
    let modifiers = function_modifiers(p);

    p.expect(FUN_KW);

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

    if modifiers.import {
        if p.at(LEFT_BRACE) {
            exprs::block(p);
        } else {
            p.expect(SEMICOLON);
        }
    } else if p.at(LEFT_BRACE) {
        exprs::block(p);
    } else if !p.eat(SEMICOLON) {
        p.error("expected function body");
    }

    m.complete(p, FN);
}

fn function_modifiers(p: &mut Parser) -> FunctionModifiers {
    let mut modifiers = FunctionModifiers::default();

    while p.peek_kind() == NAME {
        match p.peek_text() {
            "comptime" | "export" | "unsafe" => {
                p.advance();
            }
            "import" => {
                p.advance();
                if p.at(STRING) {
                    p.advance();
                    modifiers.import = true;
                } else {
                    p.error("expected import module string");
                }
            }
            _ => break,
        }
    }

    modifiers
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

fn generic_arg_list(p: &mut Parser) {
    if p.peek_kind() != LEFT_BRACKET {
        p.error("expected concrete type arguments");
        return;
    }

    let m = p.start();
    p.advance();

    if p.at(COMMA) {
        p.error("expected a type");
    }

    while !matches!(p.peek_kind(), RIGHT_BRACKET | EOF) {
        types::type_(p);

        if !p.eat(COMMA) {
            if p.at(RIGHT_BRACKET) {
                break;
            }
            p.expect(COMMA);
            break;
        }
    }

    p.expect(RIGHT_BRACKET);
    m.complete(p, GENERIC_ARG_LIST);
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
        if !matches!(
            p.peek_kind(),
            VAR_KW
                | NAME
                | DOT
                | LEFT_PAREN
                | INT_NUMBER
                | FLOAT_NUMBER
                | STRING
                | CHAR
                | TRUE_KW
                | FALSE_KW
        ) {
            p.error("expected parameter pattern");
            if p.eat(COMMA) {
                continue;
            }
            break;
        }

        param(p);

        if !p.eat(COMMA) {
            if matches!(
                p.peek_kind(),
                VAR_KW
                    | NAME
                    | DOT
                    | LEFT_PAREN
                    | INT_NUMBER
                    | FLOAT_NUMBER
                    | STRING
                    | CHAR
                    | TRUE_KW
                    | FALSE_KW
            ) {
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
    p.eat(VAR_KW);
    patterns::pattern(p, &SyntaxSet::new([COLON, COMMA, RIGHT_PAREN]));

    if p.at(COLON) {
        types::ascription(p);
    }

    m.complete(p, PARAM);
}

fn struct_field_list(p: &mut Parser) {
    let m = p.start();
    p.advance(); // eat LEFT_BRACE

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if looks_like_destructor_member(p) {
            destructor_def(p);
            if !p.eat(COMMA) {
                break;
            }
            continue;
        }

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
        if looks_like_destructor_member(p) {
            destructor_def(p);
            if !p.eat(COMMA) {
                break;
            }
            continue;
        }

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

fn destructor_def(p: &mut Parser) {
    let m = p.start();
    if p.peek_kind() == NAME && p.peek_text() == "drop" {
        p.advance();
    } else {
        p.error("expected `drop`");
    }

    if p.at(LEFT_PAREN) {
        param_list(p);
    } else {
        p.error("expected destructor parameters");
    }

    if p.at(LEFT_BRACE) {
        exprs::block(p);
    } else {
        p.error("expected destructor body");
    }

    m.complete(p, DESTRUCTOR_DEF);
}

fn looks_like_destructor_member(p: &mut Parser) -> bool {
    let mut result = false;
    p.try_parse(|lookahead| {
        if lookahead.peek_kind() != NAME || lookahead.peek_text() != "drop" {
            return false;
        }
        lookahead.advance();
        if lookahead.peek_kind() != LEFT_PAREN {
            return false;
        }

        let mut depth = 0usize;
        while !matches!(lookahead.peek_kind(), EOF) {
            match lookahead.peek_kind() {
                LEFT_PAREN => depth += 1,
                RIGHT_PAREN => {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                    if depth == 0 {
                        lookahead.advance();
                        break;
                    }
                }
                _ => {}
            }
            lookahead.advance();
        }

        result = depth == 0 && lookahead.peek_kind() == LEFT_BRACE;
        false
    });
    result
}

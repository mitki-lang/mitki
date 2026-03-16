use mitki_yellow::SyntaxKind::*;
use mitki_yellow::SyntaxSet;
use text_size::TextRange;

use super::{delimited, name, patterns, types};
use crate::parser::{CompletedMarker, Parser};

pub(crate) fn stmt(p: &mut Parser) -> bool {
    match p.peek_kind() {
        VAL_KW | VAR_KW => {
            let m = p.start();
            p.advance();
            patterns::pattern(p, &SyntaxSet::new([COLON, EQ, SEMICOLON]));
            if p.at(COLON) {
                types::ascription(p);
            }
            if p.eat(EQ) {
                expr(p);
            }
            m.complete(p, VAL_STMT);
        }
        RETURN_KW => {
            let m = p.start();
            p.advance();
            expr(p);
            m.complete(p, RETURN_STMT);
        }
        _ => {
            let parsed_expr = expr(p);
            if p.at(EQ) {
                let Some(lhs) = parsed_expr else {
                    p.error("expected assignment target");
                    return false;
                };
                let m = lhs.precede(p);
                p.advance();
                expr(p);
                m.complete(p, ASSIGN_STMT);
                return false;
            }
            let has_semi = p.eat(SEMICOLON);
            if has_semi {
                parsed_expr.map(|m| m.precede(p).complete(p, EXPR_STMT));
            }
            return has_semi;
        }
    };

    false
}

pub(crate) fn expr(p: &mut Parser) -> Option<CompletedMarker> {
    let head = unary_expr(p)?;

    if p.peek_kind() != BINARY_OPERATOR {
        return Some(head);
    }

    let m = head.precede(p);

    while p.peek_kind() == BINARY_OPERATOR {
        p.advance();
        if unary_expr(p).is_none() {
            break;
        }
    }

    Some(m.complete(p, BIN_OP_SEQ))
}

pub(crate) fn block(p: &mut Parser<'_>) {
    if p.peek_kind() != LEFT_BRACE {
        p.error("expected a block");
        return;
    }

    let m = p.start();
    p.advance();
    block_contents(p);
    p.expect(RIGHT_BRACE);
    m.complete(p, STMT_LIST);
}
fn block_contents(parser: &mut Parser) {
    let mut prev_had_semicolon = true;

    while !matches!(parser.peek_kind(), RIGHT_BRACE | EOF) {
        let consecutive_statements_on_same_line =
            !prev_had_semicolon && parser.next_token_on_same_line();

        let stmt_start = parser.peek_range().start();
        stmt(parser);

        if consecutive_statements_on_same_line {
            parser.error_with_range(
                "Consecutive statements on the same line must be separated by ';'",
                TextRange::new(stmt_start, parser.previous_range().end()),
            );
        }

        prev_had_semicolon = parser.eat(SEMICOLON);
    }
}

fn unary_expr(p: &mut Parser) -> Option<CompletedMarker> {
    match p.peek_kind() {
        NAME if p.peek_text() == "unsafe" && p.nth_kind(1) == LEFT_BRACE => {
            let m = p.start();
            p.advance();
            block(p);
            m.complete(p, UNSAFE_EXPR).into()
        }
        LOOP_KW => {
            let m = p.start();
            p.advance();
            block(p);
            m.complete(p, LOOP_EXPR).into()
        }
        BREAK_KW => {
            let m = p.start();
            p.advance();
            m.complete(p, BREAK_EXPR).into()
        }
        CONTINUE_KW => {
            let m = p.start();
            p.advance();
            m.complete(p, CONTINUE_EXPR).into()
        }
        MATCH_KW => match_(p),
        IF_KW => if_(p),
        PREFIX_OPERATOR => {
            let m = p.start();
            p.advance();
            unary_expr(p);
            m.complete(p, PREFIX_EXPR).into()
        }
        BINARY_OPERATOR => {
            let m = p.start();
            p.error("unary operator cannot be separated from its operand");
            p.advance();
            unary_expr(p);
            m.complete(p, PREFIX_OPERATOR).into()
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

fn match_(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    debug_assert_eq!(p.peek_kind(), MATCH_KW);

    let m = p.start();
    p.advance();
    expr(p);

    if !p.eat(LEFT_BRACE) {
        p.error("expected `{`");
        return Some(m.complete(p, MATCH_EXPR));
    }

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if p.at(COMMA) {
            let err = p.start();
            p.error("expected match arm");
            p.advance();
            err.complete(p, ERROR);
            continue;
        }

        let arm = p.start();
        patterns::match_pattern(p, &SyntaxSet::new([FAT_ARROW, COMMA, RIGHT_BRACE]));
        p.expect(FAT_ARROW);
        expr(p);
        arm.complete(p, MATCH_ARM);

        if !p.eat(COMMA) {
            if matches!(
                p.peek_kind(),
                LEFT_PAREN
                    | DOT
                    | NAME
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

    p.expect(RIGHT_BRACE);
    Some(m.complete(p, MATCH_EXPR))
}

fn postfix_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let mut head = primary_expr(p)?;

    loop {
        head = match p.peek_kind() {
            POSTFIX_OPERATOR => {
                p.advance();
                head.precede(p).complete(p, POSTFIX_EXPR)
            }
            LEFT_PAREN if p.next_token_on_same_line() => {
                let m = p.start();
                delimited(
                    p,
                    LEFT_PAREN,
                    RIGHT_PAREN,
                    COMMA,
                    "expected expression",
                    &SyntaxSet::new([
                        INT_NUMBER,
                        FLOAT_NUMBER,
                        DOT,
                        NAME,
                        IF_KW,
                        MATCH_KW,
                        LOOP_KW,
                        BREAK_KW,
                        CONTINUE_KW,
                        NAME,
                        LEFT_PAREN,
                        LEFT_BRACE,
                        LEFT_BRACKET,
                        PREFIX_OPERATOR,
                    ]),
                    |p| expr(p).is_some(),
                );
                m.complete(p, ARG_LIST);
                head.precede(p).complete(p, CALL_EXPR)
            }
            LEFT_BRACE
                if p.next_token_on_same_line()
                    && head.kind() == PATH_EXPR
                    && looks_like_struct_expr_field_list(p, true) =>
            {
                struct_expr_field_list(p);
                head.precede(p).complete(p, STRUCT_EXPR)
            }

            DOT if p.next_token_on_same_line() => {
                p.advance();
                let name = p.start();
                p.expect(NAME);
                name.complete(p, NAME_REF);
                head.precede(p).complete(p, FIELD_EXPR)
            }

            _ => break,
        }
    }

    head.into()
}

fn struct_expr_field_list(p: &mut Parser) {
    debug_assert_eq!(p.peek_kind(), LEFT_BRACE);
    let m = p.start();
    p.advance();

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if p.at(COMMA) {
            let err = p.start();
            p.error("expected field");
            p.advance();
            err.complete(p, ERROR);
            continue;
        }

        let field = p.start();
        name(p, &SyntaxSet::new([COLON, COMMA, RIGHT_BRACE]));
        if p.eat(COLON) {
            expr(p);
        }
        field.complete(p, STRUCT_EXPR_FIELD);

        if !p.eat(COMMA) {
            if p.at(NAME) {
                p.expect(COMMA);
            } else {
                break;
            }
        }
    }

    p.expect(RIGHT_BRACE);
    m.complete(p, STRUCT_EXPR_FIELD_LIST);
}

fn primary_expr(p: &mut Parser) -> Option<CompletedMarker> {
    match p.peek_kind() {
        INT_NUMBER | FLOAT_NUMBER | STRING | CHAR | TRUE_KW | FALSE_KW => {
            let m = p.start();
            p.advance();
            m.complete(p, LITERAL).into()
        }
        LEFT_PAREN => {
            let m = p.start();
            p.advance();

            let mut saw_comma = false;
            let mut saw_expr = false;

            if p.eat(COMMA) {
                p.error("expected expression");
                saw_comma = true;
            }

            while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
                saw_expr = true;

                if expr(p).is_none() {
                    break;
                }

                if !p.at(RIGHT_PAREN) {
                    saw_comma = true;
                    p.expect(COMMA);
                }
            }

            p.expect(RIGHT_PAREN);
            m.complete(p, if saw_expr && !saw_comma { PAREN_EXPR } else { TUPLE_EXPR }).into()
        }
        LEFT_BRACKET => {
            let m = p.start();
            p.advance();

            let mut n_exprs = 0u32;
            let mut has_semi = false;

            while !matches!(p.peek_kind(), RIGHT_BRACKET | EOF) {
                n_exprs += 1;

                if expr(p).is_none() {
                    break;
                }

                if n_exprs == 1 && p.eat(SEMICOLON) {
                    has_semi = true;
                    continue;
                }

                if has_semi || !p.at(RIGHT_BRACKET) && !p.expect(COMMA) {
                    break;
                }
            }

            p.expect(RIGHT_BRACKET);
            m.complete(p, ARRAY_EXPR).into()
        }
        NAME => {
            let m = p.start();
            path_segments(p);

            m.complete(p, PATH_EXPR).into()
        }
        DOT => {
            let m = p.start();
            p.advance();
            let name = p.start();
            p.expect(NAME);
            name.complete(p, NAME_REF);
            m.complete(p, FIELD_EXPR).into()
        }
        LEFT_BRACE => {
            if looks_like_struct_expr_field_list(p, false) {
                return anonymous_struct_expr(p);
            }

            let closure = p.start();
            p.advance();

            p.try_parse(|lookahead| {
                let m = lookahead.start();
                while matches!(
                    lookahead.peek_kind(),
                    LEFT_PAREN
                        | DOT
                        | NAME
                        | INT_NUMBER
                        | FLOAT_NUMBER
                        | STRING
                        | CHAR
                        | TRUE_KW
                        | FALSE_KW
                ) {
                    let m = lookahead.start();
                    patterns::pattern(lookahead, &SyntaxSet::new([COMMA, IN_KW, RIGHT_BRACE]));
                    m.complete(lookahead, PARAM);

                    if !lookahead.eat(COMMA) {
                        if matches!(
                            lookahead.peek_kind(),
                            LEFT_PAREN
                                | DOT
                                | NAME
                                | INT_NUMBER
                                | FLOAT_NUMBER
                                | STRING
                                | CHAR
                                | TRUE_KW
                                | FALSE_KW
                        ) {
                            lookahead.expect(COMMA);
                        } else {
                            break;
                        }
                    }
                }
                m.complete(lookahead, PARAM_LIST);
                lookahead.eat(IN_KW)
            });

            let stmts = p.start();
            if !p.at(RIGHT_BRACE) {
                block_contents(p);
            }
            stmts.complete(p, STMT_LIST);

            p.expect(RIGHT_BRACE);
            closure.complete(p, CLOSURE_EXPR).into()
        }
        _ => {
            p.error_and_bump("expected expression");
            None
        }
    }
}

pub(crate) fn path_segments(p: &mut Parser) {
    let name = p.start();
    if matches!(p.peek_kind(), NAME | VAR_KW) {
        p.advance();
    } else {
        p.error("expected identifier");
    }
    name.complete(p, NAME_REF);

    while p.at(DOUBLE_COLON) {
        p.advance();
        let name = p.start();
        if matches!(p.peek_kind(), NAME | VAR_KW) {
            p.advance();
        } else {
            p.error("expected identifier");
        }
        name.complete(p, NAME_REF);
    }
}

fn anonymous_struct_expr(p: &mut Parser) -> Option<CompletedMarker> {
    let m = p.start();
    struct_expr_field_list(p);
    Some(m.complete(p, STRUCT_EXPR))
}

fn looks_like_struct_expr_field_list(p: &mut Parser, allow_empty: bool) -> bool {
    let mut is_struct = false;
    p.try_parse(|lookahead| {
        is_struct = try_parse_struct_expr_field_list_lookahead(lookahead, allow_empty);
        false
    });
    is_struct
}

fn try_parse_struct_expr_field_list_lookahead(p: &mut Parser, allow_empty: bool) -> bool {
    if !p.eat(LEFT_BRACE) {
        return false;
    }
    if p.eat(RIGHT_BRACE) {
        return allow_empty;
    }
    if !p.at(NAME) {
        return false;
    }

    loop {
        if !p.eat(NAME) {
            return false;
        }
        if p.eat(COLON) {
            if expr(p).is_none() {
                return false;
            }
        } else if p.at(RIGHT_BRACE) {
            return false;
        }
        if p.eat(COMMA) {
            if p.at(RIGHT_BRACE) {
                break;
            }
            continue;
        }
        break;
    }

    p.eat(RIGHT_BRACE)
}

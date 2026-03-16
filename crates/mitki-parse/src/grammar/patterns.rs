use mitki_yellow::SyntaxKind::*;
use mitki_yellow::SyntaxSet;

use super::{name, types};
use crate::parser::{CompletedMarker, Parser};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum PatternMode {
    Default,
    Match,
}

pub(crate) fn pattern(p: &mut Parser<'_>, recovery: &SyntaxSet) -> bool {
    pattern_with_mode(p, recovery, PatternMode::Default)
}

pub(crate) fn match_pattern(p: &mut Parser<'_>, recovery: &SyntaxSet) -> bool {
    pattern_with_mode(p, recovery, PatternMode::Match)
}

fn pattern_with_mode(p: &mut Parser<'_>, recovery: &SyntaxSet, mode: PatternMode) -> bool {
    pattern_marker(p, recovery, mode).is_some()
}

fn pattern_marker(
    p: &mut Parser<'_>,
    recovery: &SyntaxSet,
    mode: PatternMode,
) -> Option<CompletedMarker> {
    let pattern = match p.peek_kind() {
        LEFT_PAREN => tuple_or_group_pattern(p, mode),
        DOT => variant_pattern(p, mode),
        NAME if p.peek_text() == "_" => {
            let m = p.start();
            p.advance();
            Some(m.complete(p, WILDCARD_PATTERN))
        }
        NAME if looks_like_variant_pattern(p) => variant_pattern(p, mode),
        NAME if looks_like_struct_pattern(p) => struct_pattern(p, mode),
        NAME => {
            let m = p.start();
            name(p, recovery);
            Some(m.complete(p, BINDING_PATTERN))
        }
        INT_NUMBER | FLOAT_NUMBER | STRING | CHAR | TRUE_KW | FALSE_KW => {
            let m = p.start();
            p.advance();
            Some(m.complete(p, LITERAL_PATTERN))
        }
        _ => {
            p.error_recover("expected pattern", recovery);
            None
        }
    }?;

    if mode == PatternMode::Match
        && p.at(COLON)
        && matches!(pattern.kind(), BINDING_PATTERN | WILDCARD_PATTERN)
    {
        let m = pattern.precede(p);
        types::ascription(p);
        return Some(m.complete(p, TYPED_PATTERN));
    }

    Some(pattern)
}

fn tuple_or_group_pattern(p: &mut Parser<'_>, mode: PatternMode) -> Option<CompletedMarker> {
    debug_assert_eq!(p.peek_kind(), LEFT_PAREN);

    let m = p.start();
    p.advance();

    let mut count = 0usize;
    let mut saw_comma = false;
    while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
        count += 1;
        if !pattern_with_mode(
            p,
            &SyntaxSet::new([COMMA, RIGHT_PAREN, FAT_ARROW, EQ, COLON, LEFT_BRACE]),
            mode,
        ) {
            break;
        }

        if p.eat(COMMA) {
            saw_comma = true;
            if p.at(RIGHT_PAREN) {
                break;
            }
            continue;
        }
        break;
    }

    p.expect(RIGHT_PAREN);

    let kind = if count == 1 && !saw_comma { PAREN_PATTERN } else { TUPLE_PATTERN };
    Some(m.complete(p, kind))
}

fn struct_pattern(p: &mut Parser<'_>, mode: PatternMode) -> Option<CompletedMarker> {
    debug_assert_eq!(p.peek_kind(), NAME);

    let m = p.start();
    path_pattern(p)?;
    p.expect(LEFT_BRACE);

    while !matches!(p.peek_kind(), RIGHT_BRACE | EOF) {
        if p.at(COMMA) {
            let err = p.start();
            p.error("expected struct pattern field");
            p.advance();
            err.complete(p, ERROR);
            continue;
        }

        struct_pattern_field(p, mode);

        if !p.eat(COMMA) {
            if p.at(NAME) {
                p.expect(COMMA);
            } else {
                break;
            }
        }
    }

    p.expect(RIGHT_BRACE);
    Some(m.complete(p, STRUCT_PATTERN))
}

fn struct_pattern_field(p: &mut Parser<'_>, mode: PatternMode) {
    let m = p.start();
    name(p, &SyntaxSet::new([COLON, COMMA, RIGHT_BRACE]));
    if p.eat(COLON) {
        pattern_with_mode(p, &SyntaxSet::new([COMMA, RIGHT_BRACE]), mode);
    }
    m.complete(p, STRUCT_PATTERN_FIELD);
}

fn variant_pattern(p: &mut Parser<'_>, mode: PatternMode) -> Option<CompletedMarker> {
    let head = path_head_pattern(p)?;
    let m = head.precede(p);

    if p.at(LEFT_PAREN) {
        p.advance();
        while !matches!(p.peek_kind(), RIGHT_PAREN | EOF) {
            if p.at(COMMA) {
                let err = p.start();
                p.error("expected pattern");
                p.advance();
                err.complete(p, ERROR);
                continue;
            }

            if !pattern_with_mode(p, &SyntaxSet::new([COMMA, RIGHT_PAREN]), mode) {
                break;
            }

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
        p.expect(RIGHT_PAREN);
    }

    Some(m.complete(p, VARIANT_PATTERN))
}

fn path_pattern(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    if p.peek_kind() != NAME {
        p.error("expected identifier");
        return None;
    }

    let m = p.start();
    name(p, &SyntaxSet::new([DOT, LEFT_BRACE]));
    Some(m.complete(p, PATH_PATTERN))
}

fn path_head_pattern(p: &mut Parser<'_>) -> Option<CompletedMarker> {
    match p.peek_kind() {
        DOT => {
            let m = p.start();
            p.advance();
            name(p, &SyntaxSet::new([LEFT_PAREN, FAT_ARROW, COMMA, RIGHT_BRACE]));
            Some(m.complete(p, FIELD_PATTERN))
        }
        NAME => {
            let head = path_pattern(p)?;
            if p.eat(DOT) {
                let m = head.precede(p);
                name(p, &SyntaxSet::new([LEFT_PAREN, FAT_ARROW, COMMA, RIGHT_BRACE]));
                Some(m.complete(p, FIELD_PATTERN))
            } else {
                Some(head)
            }
        }
        _ => {
            p.error("expected variant path");
            None
        }
    }
}

fn looks_like_variant_pattern(p: &mut Parser<'_>) -> bool {
    if p.peek_kind() == DOT {
        return true;
    }

    let mut result = false;
    p.try_parse(|lookahead| {
        if lookahead.peek_kind() != NAME {
            return false;
        }
        lookahead.advance();
        result = lookahead.eat(DOT) && lookahead.peek_kind() == NAME;
        false
    });
    result
}

fn looks_like_struct_pattern(p: &mut Parser<'_>) -> bool {
    let mut result = false;
    p.try_parse(|lookahead| {
        if lookahead.peek_kind() != NAME {
            return false;
        }
        lookahead.advance();
        result = lookahead.peek_kind() == LEFT_BRACE;
        false
    });
    result
}

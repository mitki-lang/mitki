mod cursor;

use std::marker::PhantomData;
use std::sync::Arc;

use ascii::AsciiChar;
use cursor::{Cursor, EOF_CHAR};
pub use mitki_yellow::SyntaxKind;
use mitki_yellow::SyntaxKind::*;
use mitki_yellow::{TriviaPiece, TriviaPieceKind};
use text_size::{TextRange, TextSize};

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: SyntaxKind,
    pub kind_range: TextRange,
    pub leading: TriviaRange,
    pub trailing: TriviaRange,
    leading_has_newline: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TriviaRange {
    pub start: u32,
    pub len: u32,
}

impl Token {
    pub fn on_same_line(&self) -> bool {
        !self.leading_has_newline
    }
}

impl TriviaRange {
    fn new(start: usize, len: usize) -> Self {
        let start = u32::try_from(start).expect("trivia start overflow");
        let len = u32::try_from(len).expect("trivia len overflow");
        Self { start, len }
    }

    fn range(self) -> std::ops::Range<usize> {
        let start = self.start as usize;
        let end = start + self.len as usize;
        start..end
    }
}

#[derive(Clone)]
pub enum Diagnostic {
    InconsistentWhitespaceAroundEqual(TextRange),
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum TriviaMode {
    Normal,
    NoNewlines,
}

pub type TokenIndex = u32;

#[derive(Clone)]
pub struct Tokenizer<'db> {
    kinds: Arc<[SyntaxKind]>,
    kind_ranges: Arc<[TextRange]>,
    leading_ranges: Arc<[TriviaRange]>,
    trailing_ranges: Arc<[TriviaRange]>,
    leading_has_newline: Arc<[bool]>,
    trivia: Arc<[TriviaPiece]>,
    diagnostics: Arc<[Diagnostic]>,
    position: TokenIndex,
    _marker: PhantomData<&'db str>,
}

impl<'db> Tokenizer<'db> {
    pub fn new(text: &'db str) -> Self {
        let mut lexer = Lexer::new(text);
        let mut kinds = Vec::new();
        let mut kind_ranges = Vec::new();
        let mut leading_ranges = Vec::new();
        let mut trailing_ranges = Vec::new();
        let mut leading_has_newline = Vec::new();

        loop {
            let token = lexer.next_token();
            let is_eof = token.kind == EOF;
            kinds.push(token.kind);
            kind_ranges.push(token.kind_range);
            leading_ranges.push(token.leading);
            trailing_ranges.push(token.trailing);
            leading_has_newline.push(token.leading_has_newline);
            if is_eof {
                break;
            }
        }

        let Lexer { diagnostics, trivia, .. } = lexer;

        Self {
            kinds: Arc::from(kinds),
            kind_ranges: Arc::from(kind_ranges),
            leading_ranges: Arc::from(leading_ranges),
            trailing_ranges: Arc::from(trailing_ranges),
            leading_has_newline: Arc::from(leading_has_newline),
            trivia: Arc::from(trivia),
            diagnostics: Arc::from(diagnostics),
            position: 0,
            _marker: PhantomData,
        }
    }

    pub fn peek(&self) -> Token {
        self.token_at(self.position)
    }

    pub fn next_token(&mut self) -> Token {
        let token = self.peek();
        if token.kind != EOF {
            self.position += 1;
        }
        token
    }

    pub fn next_token_index(&mut self) -> TokenIndex {
        let index = self.position;
        if self.kinds[index as usize] != EOF {
            self.position += 1;
        }
        index
    }

    pub fn token(&self, index: TokenIndex) -> Token {
        self.token_at(index)
    }

    pub fn leading_trivia(&self, index: TokenIndex) -> &[TriviaPiece] {
        let range = self.leading_ranges[index as usize];
        self.trivia_range(range)
    }

    pub fn trailing_trivia(&self, index: TokenIndex) -> &[TriviaPiece] {
        let range = self.trailing_ranges[index as usize];
        self.trivia_range(range)
    }

    pub fn diagnostics(&self) -> &[Diagnostic] {
        self.diagnostics.as_ref()
    }

    fn trivia_range(&self, range: TriviaRange) -> &[TriviaPiece] {
        let range = range.range();
        &self.trivia[range]
    }

    fn token_at(&self, index: TokenIndex) -> Token {
        let index = index as usize;
        Token {
            kind: self.kinds[index],
            kind_range: self.kind_ranges[index],
            leading: self.leading_ranges[index],
            trailing: self.trailing_ranges[index],
            leading_has_newline: self.leading_has_newline[index],
        }
    }
}

struct Lexer<'db> {
    text: &'db str,
    cursor: Cursor<'db>,
    trivia: Vec<TriviaPiece>,
    diagnostics: Vec<Diagnostic>,
}

impl<'db> Lexer<'db> {
    fn new(text: &'db str) -> Self {
        Self {
            text,
            cursor: Cursor::new(text),
            trivia: Vec::with_capacity(4),
            diagnostics: Vec::new(),
        }
    }

    fn next_token(&mut self) -> Token {
        let (leading, leading_has_newline) = self.trivia(TriviaMode::Normal);
        let (kind, kind_range) = self.syntax_kind();
        let (trailing, _) = self.trivia(TriviaMode::NoNewlines);

        Token { kind, kind_range, leading, trailing, leading_has_newline }
    }

    fn offset(&self) -> TextSize {
        TextSize::new(self.text.len() as u32) - self.cursor.len()
    }

    fn range(&self) -> TextRange {
        let end = self.offset();
        let len = self.cursor.pos_within_token();

        TextRange::at(end - len, len)
    }

    fn text(&self) -> &'db str {
        &self.text[self.range()]
    }

    fn trivia(&mut self, mode: TriviaMode) -> (TriviaRange, bool) {
        let start = self.trivia.len();
        let mut has_newline = false;
        loop {
            let kind = match self.cursor.peek() {
                '/' if self.cursor.second() == '/' => {
                    self.cursor.advance_until(AsciiChar::LineFeed);
                    TriviaPieceKind::SingleLineComment
                }
                '\n' | '\r' if mode == TriviaMode::Normal => {
                    self.cursor.advance_while(|ch| matches!(ch, '\n' | '\r'));
                    TriviaPieceKind::Newline
                }
                first_char => {
                    if matches!(first_char, ' ' | '\t') {
                        self.cursor.advance_while(|ch| ch.is_ascii_whitespace());
                        TriviaPieceKind::Whitespace
                    } else {
                        break;
                    }
                }
            };

            if kind == TriviaPieceKind::Newline {
                has_newline = true;
            }

            self.trivia.push(TriviaPiece::new(kind, self.cursor.pos_within_token()));
            self.cursor.reset_pos_within_token();
        }

        let len = self.trivia.len() - start;
        (TriviaRange::new(start, len), has_newline)
    }

    fn syntax_kind(&mut self) -> (SyntaxKind, TextRange) {
        let previous = self.cursor.previous();

        let kind = match self.cursor.advance() {
            '(' => LEFT_PAREN,
            ')' => RIGHT_PAREN,
            '[' => LEFT_BRACKET,
            ']' => RIGHT_BRACKET,
            '{' => LEFT_BRACE,
            '}' => RIGHT_BRACE,
            ':' => COLON,
            ',' => COMMA,
            ';' => SEMICOLON,
            '"' => self.string(),
            '\'' => self.char_literal(),
            first_char @ '0'..='9' => self.number(first_char),
            'A'..='Z' | 'a'..='z' | '_' => self.identifier_or_keyword(),
            EOF_CHAR => EOF,
            first_char => {
                if is_operator(first_char) {
                    self.operator(previous)
                } else {
                    UNKNOWN
                }
            }
        };

        let range = self.range();
        self.cursor.reset_pos_within_token();

        (kind, range)
    }

    fn identifier_or_keyword(&mut self) -> SyntaxKind {
        self.cursor.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');

        match self.text() {
            "fun" => FUN_KW,
            "if" => IF_KW,
            "else" => ELSE_KW,
            "loop" => LOOP_KW,
            "val" => VAL_KW,
            "while" => WHILE_KW,
            "return" => RETURN_KW,
            "break" => BREAK_KW,
            "true" => TRUE_KW,
            "false" => FALSE_KW,
            "in" => IN_KW,
            _ => NAME,
        }
    }

    fn operator(&mut self, previous: char) -> SyntaxKind {
        self.cursor.advance_while(is_operator);

        let left_bound = match previous {
            '(' | '[' | '{' | ',' | ':' | ';' => false,
            EOF_CHAR => false,
            prev => !prev.is_ascii_whitespace(),
        };

        let right_bound = match self.cursor.peek() {
            ')' | ']' | '}' | ',' | ':' | ';' => false,
            '.' => !left_bound,
            EOF_CHAR => false,
            peeked => !peeked.is_ascii_whitespace(),
        };

        match self.text() {
            "=" => {
                if left_bound != right_bound {
                    self.diagnostics
                        .push(Diagnostic::InconsistentWhitespaceAroundEqual(self.range()));
                }

                EQ
            }
            "." => DOT,
            _ => {
                if left_bound == right_bound {
                    BINARY_OPERATOR
                } else if left_bound {
                    POSTFIX_OPERATOR
                } else {
                    PREFIX_OPERATOR
                }
            }
        }
    }

    fn number(&mut self, c: char) -> SyntaxKind {
        if c == '0' {
            match self.cursor.peek() {
                'b' | 'o' => {
                    self.cursor.advance();
                    self.digits(false);
                }
                'x' => {
                    self.cursor.advance();
                    self.digits(true);
                }
                '0'..='9' | '_' | '.' | 'e' | 'E' => {
                    self.digits(false);
                }
                _ => return INT_NUMBER,
            }
        } else {
            self.digits(false);
        }

        if self.cursor.matches('.') && self.cursor.second() != '.' {
            self.cursor.advance();
            self.digits(false);
            self.float_exponent();
            return FLOAT_NUMBER;
        }

        if self.cursor.matches('e') || self.cursor.matches('E') {
            self.float_exponent();
            return FLOAT_NUMBER;
        }

        INT_NUMBER
    }

    fn digits(&mut self, allow_hex: bool) {
        loop {
            match self.cursor.peek() {
                '_' | '0'..='9' => {
                    self.cursor.advance();
                }
                'a'..='f' | 'A'..='F' if allow_hex => {
                    self.cursor.advance();
                }
                _ => return,
            }
        }
    }

    fn string(&mut self) -> SyntaxKind {
        while !self.cursor.is_eof() {
            match self.cursor.peek() {
                '"' => {
                    self.cursor.advance();
                    break;
                }
                '\\' => {
                    self.cursor.advance();
                    self.cursor.advance();
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }
        STRING
    }

    fn char_literal(&mut self) -> SyntaxKind {
        while !self.cursor.is_eof() {
            match self.cursor.peek() {
                '\'' => {
                    self.cursor.advance();
                    break;
                }
                '\\' => {
                    self.cursor.advance();
                    self.cursor.advance();
                }
                _ => {
                    self.cursor.advance();
                }
            }
        }
        CHAR
    }

    fn float_exponent(&mut self) {
        if self.cursor.matches('e') || self.cursor.matches('E') {
            self.cursor.advance();
            if self.cursor.matches('-') || self.cursor.matches('+') {
                self.cursor.advance();
            }
            self.digits(false);
        }
    }
}

fn is_operator(c: char) -> bool {
    matches!(
        c,
        '/' | '=' | '-' | '+' | '*' | '%' | '<' | '>' | '!' | '&' | '|' | '^' | '~' | '.' | '?'
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token_text<'a>(token: &Token, text: &'a str) -> &'a str {
        &text[token.kind_range]
    }

    #[test]
    fn test_integer_literals() {
        let inputs = vec![
            ("123", INT_NUMBER),
            ("0", INT_NUMBER),
            ("0b1010", INT_NUMBER),
            ("0o755", INT_NUMBER),
            ("0x1f", INT_NUMBER),
            ("123_456", INT_NUMBER),
        ];

        for (input, expected_kind) in inputs {
            let mut tokenizer = Tokenizer::new(input);
            let kind = tokenizer.next_token().kind;
            assert_eq!(kind, expected_kind, "Input: '{input}'");
            assert_eq!(
                tokenizer.peek().kind,
                EOF,
                "Tokenizer did not consume all input for '{input}'"
            );
        }
    }

    #[test]
    fn test_float_literals() {
        let inputs = vec![
            ("123.456", FLOAT_NUMBER),
            ("0.0", FLOAT_NUMBER),
            ("1e10", FLOAT_NUMBER),
            ("1.0e-5", FLOAT_NUMBER),
            ("123_456.789_012", FLOAT_NUMBER),
        ];

        for (input, expected_kind) in inputs {
            let mut tokenizer = Tokenizer::new(input);
            let kind = tokenizer.next_token().kind;
            assert_eq!(kind, expected_kind, "Input: '{input}'");
            assert_eq!(
                tokenizer.peek().kind,
                EOF,
                "Tokenizer did not consume all input for '{input}'"
            );
        }
    }

    #[test]
    fn test_string_literals() {
        let inputs = vec![("\"hello\"", STRING), ("\"\"", STRING), ("\"foo\\\"bar\"", STRING)];

        for (input, expected_kind) in inputs {
            let mut tokenizer = Tokenizer::new(input);
            let kind = tokenizer.next_token().kind;
            assert_eq!(kind, expected_kind, "Input: '{input}'");
            assert_eq!(
                tokenizer.peek().kind,
                EOF,
                "Tokenizer did not consume all input for '{input}'"
            );
        }
    }

    #[test]
    fn test_char_literals() {
        let inputs = vec![("'a'", CHAR), ("'\\n'", CHAR), ("'\\''", CHAR)];

        for (input, expected_kind) in inputs {
            let mut tokenizer = Tokenizer::new(input);
            let kind = tokenizer.next_token().kind;
            assert_eq!(kind, expected_kind, "Input: '{input}'");
            assert_eq!(
                tokenizer.peek().kind,
                EOF,
                "Tokenizer did not consume all input for '{input}'"
            );
        }
    }

    #[test]
    fn test_eq_operator() {
        let text = "x = y";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "x");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, EQ);
        assert_eq!(token_text(&token, text), "=");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "y");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_eq_operator_without_whitespace() {
        let text = "x=y";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "x");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, EQ);
        assert_eq!(token_text(&token, text), "=");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "y");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_chained_eq_operator() {
        let text = "x = y = z";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "x");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, EQ);
        assert_eq!(token_text(&token, text), "=");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "y");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, EQ);
        assert_eq!(token_text(&token, text), "=");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "z");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_double_eq_operator() {
        let text = "x == y";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "x");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "==");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "y");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_dot_operator() {
        let text = "object.property";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "object");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, DOT);
        assert_eq!(token_text(&token, text), ".");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "property");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_binary_operator() {
        let text = "a+b";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "+");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "b");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_prefix_operator() {
        let text = "-a";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_postfix_operator() {
        let text = "a++";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, POSTFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "++");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_binary_operator_with_whitespace() {
        let text = "a + b";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "+");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "b");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_binary_operator_with_whitespace_as_prefix() {
        let text = "- a";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_binary_operator_with_whitespace_as_postfix() {
        let text = "a ++";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "++");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_complex_operator_sequences() {
        let text = "-a * b++ / ++c - d--";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "*");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "b");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, POSTFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "++");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "/");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "++");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "c");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "d");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, POSTFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "--");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_operator_precedence() {
        let text = "!a && b || c";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "!");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "&&");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "b");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "||");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "c");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_operator_with_parentheses() {
        let text = "(-a) + (b++)";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, LEFT_PAREN);
        assert_eq!(token_text(&token, text), "(");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, RIGHT_PAREN);
        assert_eq!(token_text(&token, text), ")");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "+");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, LEFT_PAREN);
        assert_eq!(token_text(&token, text), "(");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "b");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, POSTFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "++");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, RIGHT_PAREN);
        assert_eq!(token_text(&token, text), ")");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }

    #[test]
    fn test_mixed_operator_contexts() {
        let text = "a * -b + c++ - --d";
        let mut tokenizer = Tokenizer::new(text);

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "a");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "*");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "b");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "+");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "c");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, POSTFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "++");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, BINARY_OPERATOR);
        assert_eq!(token_text(&token, text), "-");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, PREFIX_OPERATOR);
        assert_eq!(token_text(&token, text), "--");

        let token = tokenizer.next_token();
        assert_eq!(token.kind, NAME);
        assert_eq!(token_text(&token, text), "d");

        let eof_token = tokenizer.next_token();
        assert_eq!(eof_token.kind, EOF);
    }
}

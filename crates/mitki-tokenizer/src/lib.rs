mod cursor;

use ascii::AsciiChar;
use cursor::{Cursor, EOF_CHAR};
pub use mitki_yellow::SyntaxKind;
use mitki_yellow::SyntaxKind::*;
use mitki_yellow::{GreenTrivia, TriviaPiece, TriviaPieceKind};
use text_size::{TextRange, TextSize};

#[derive(Debug, Clone)]
pub struct Token {
    pub leading: GreenTrivia,
    pub kind: SyntaxKind,
    pub kind_range: TextRange,
    pub trailing: GreenTrivia,
}

impl Token {
    const EOF: Self = Self {
        kind: EOF,
        kind_range: TextRange::empty(TextSize::new(0)),
        leading: GreenTrivia::empty(),
        trailing: GreenTrivia::empty(),
    };

    pub fn on_same_line(&self) -> bool {
        self.leading.pieces().iter().all(|item| item.kind != TriviaPieceKind::Newline)
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

#[derive(Clone)]
pub struct Tokenizer<'db> {
    text: &'db str,
    cursor: Cursor<'db>,
    current: Token,
    trivia_pieces: Vec<TriviaPiece>,
    diagnostics: Vec<Diagnostic>,
    whitespace: GreenTrivia,
}

impl<'db> Tokenizer<'db> {
    pub fn new(text: &'db str) -> Self {
        let mut tokenizer = Self {
            text,
            cursor: Cursor::new(text),
            current: Token::EOF,
            trivia_pieces: Vec::with_capacity(4),
            diagnostics: Vec::new(),
            whitespace: GreenTrivia::whitespace(1),
        };

        tokenizer.next_token();
        tokenizer
    }

    pub fn peek(&self) -> &Token {
        &self.current
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

    pub fn next_token(&mut self) -> Token {
        self.trivia(TriviaMode::Normal);
        let trailing_start = self.trivia_pieces.len();
        let (kind, kind_range) = self.syntax_kind();
        self.trivia(TriviaMode::NoNewlines);

        let (leading, trailing) = self.trivia_pieces.split_at(trailing_start);
        let leading = self.green_trivia(leading);
        let trailing = self.green_trivia(trailing);

        self.trivia_pieces.clear();
        std::mem::replace(&mut self.current, Token { leading, kind, kind_range, trailing })
    }

    fn green_trivia(&self, pieces: &[TriviaPiece]) -> GreenTrivia {
        match pieces {
            [] => GreenTrivia::empty(),
            [TriviaPiece { kind: TriviaPieceKind::Whitespace, len }] if *len == 1.into() => {
                self.whitespace.clone()
            }
            _ => GreenTrivia::new(pieces),
        }
    }

    fn trivia(&mut self, mode: TriviaMode) {
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

            self.trivia_pieces.push(TriviaPiece::new(kind, self.cursor.pos_within_token()));
            self.cursor.reset_pos_within_token();
        }
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
            '"' => self.string_literal(),
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

    fn string_literal(&mut self) -> SyntaxKind {
        // The opening quote is consumed by `syntax_kind`
        loop {
            match self.cursor.peek() {
                '"' => {
                    self.cursor.advance(); // Consume closing quote
                    break;
                }
                '\\' => { // Backslash
                    self.cursor.advance(); // Consume backslash
                    match self.cursor.peek() {
                        '"' | '\\' | 'n' => { // Valid escape sequences
                            self.cursor.advance(); // Consume the character being escaped
                        }
                        // Optional: Handle invalid escape sequences, or let them pass through
                        // For now, we consume the backslash and whatever follows if it's not a known escape.
                        // A more robust lexer might flag unknown escape sequences as errors.
                        // According to the new guidance, do nothing for unknown escape sequences,
                        // effectively treating the backslash as a literal character in such cases,
                        // or letting a later stage handle it.
                        _ => {
                            // Only consume the character being escaped if it's a known one.
                            // If not, the backslash itself is consumed, and the next character
                            // will be processed in the next iteration of the loop.
                        }
                    }
                }
                EOF_CHAR => {
                    // Unterminated string
                    break;
                }
                _ => { // Any other character
                    self.cursor.advance();
                }
            }
        }
        STRING_LITERAL
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

    fn float_exponent(&mut self) {
        if self.cursor.matches('e') || self.cursor.matches('E') {
            self.cursor.advance();
            if self.cursor.matches('-') || self.cursor.matches('+') {
                self.cursor.advance();
            }
            self.digits(false);
        }
    }

    pub fn diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
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
            assert!(tokenizer.cursor.is_eof(), "Tokenizer did not consume all input for '{input}'");
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
            assert!(tokenizer.cursor.is_eof(), "Tokenizer did not consume all input for '{input}'");
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

    #[test]
    fn test_string_literals() {
        let inputs = vec![
            ("\"hello\"", STRING_LITERAL),
            ("\"\"", STRING_LITERAL),
            ("\"a \\\"quote\\\"\"", STRING_LITERAL),
            ("\"a \\\\ backslash\"", STRING_LITERAL),
            ("\"line1\\nline2\"", STRING_LITERAL),
            ("\"\\\\ \\\" \\n\"", STRING_LITERAL),
            ("\"abc", STRING_LITERAL), // Unterminated
            ("\"hello \\x world\"", STRING_LITERAL), // Invalid escape, passed through
        ];

        for (input_str, expected_kind) in inputs {
            let mut tokenizer = Tokenizer::new(input_str);
            let token = tokenizer.next_token();
            assert_eq!(token.kind, expected_kind, "Input: '{}'", input_str);

            let expected_range = TextRange::new(TextSize::from(0), TextSize::of(input_str));
            assert_eq!(token.kind_range, expected_range, "Input: '{}', Range expected {:?}, got {:?}", input_str, expected_range, token.kind_range);

            assert_eq!(tokenizer.next_token().kind, EOF, "Tokenizer did not consume all input for '{}'", input_str);
        }
    }
}

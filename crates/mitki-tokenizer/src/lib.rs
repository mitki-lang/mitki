mod cursor;

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
}

pub struct Tokenizer<'db> {
    text: &'db str,
    cursor: Cursor<'db>,
    current: Token,
    trivia_pieces: Vec<TriviaPiece>,
}

impl<'db> Tokenizer<'db> {
    pub fn new(text: &'db str) -> Self {
        let mut tokenizer = Self {
            text,
            cursor: Cursor::new(text),
            current: Token::EOF,
            trivia_pieces: Vec::with_capacity(4),
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
        self.trivia();
        let trailing_start = self.trivia_pieces.len();
        let (kind, kind_range) = self.syntax_kind();
        self.trivia();

        let (leading, trailing) = self.trivia_pieces.split_at(trailing_start);
        let leading = GreenTrivia::new(leading);
        let trailing = GreenTrivia::new(trailing);

        self.trivia_pieces.clear();
        std::mem::replace(&mut self.current, Token { leading, kind, kind_range, trailing })
    }

    fn trivia(&mut self) {
        loop {
            let kind = match self.cursor.peek() {
                '/' if self.cursor.second() == '/' => {
                    self.cursor.advance_while(|c| c != '\n');
                    TriviaPieceKind::SingleLineComment
                }
                first_char => {
                    if first_char.is_whitespace() {
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
            '0'..='9' => {
                self.cursor.advance_while(|c| c.is_ascii_digit() || c == '_');
                NUMBER
            }
            'A'..='Z' | 'a'..='z' | '_' => {
                self.cursor.advance_while(|c| c.is_ascii_alphanumeric() || c == '_');

                match self.text() {
                    "fun" => FUN_KW,
                    "if" => IF_KW,
                    "loop" => LOOP_KW,
                    "val" => VAL_KW,
                    "while" => WHILE_KW,
                    _ => NAME,
                }
            }
            EOF_CHAR => EOF,
            first_char => {
                if is_operator(first_char) {
                    self.cursor.advance_while(is_operator);

                    let left_bound = match previous {
                        '(' | '[' | '{' | ',' | ':' => false,
                        EOF_CHAR => false,
                        prev => !prev.is_ascii_whitespace(),
                    };

                    let right_bound = match self.cursor.peek() {
                        ')' | ']' | '}' | ',' | ':' => false,
                        '.' => !left_bound,
                        EOF_CHAR => false,
                        peeked => !peeked.is_ascii_whitespace(),
                    };

                    match self.text() {
                        "=" => EQ,
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
                } else {
                    UNKNOWN
                }
            }
        };

        let range = self.range();
        self.cursor.reset_pos_within_token();

        (kind, range)
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

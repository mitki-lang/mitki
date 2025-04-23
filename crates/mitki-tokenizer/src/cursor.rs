use std::str::Chars;

use text_size::{TextLen as _, TextSize};

pub(crate) const EOF_CHAR: char = '\0';

#[derive(Clone)]
pub(crate) struct Cursor<'text> {
    chars: Chars<'text>,
    len: TextSize,
    previous: char,
}

impl<'text> Cursor<'text> {
    pub(crate) fn new(text: &'text str) -> Self {
        Self { chars: text.chars(), len: text.text_len(), previous: '\0' }
    }

    fn as_str(&self) -> &'text str {
        self.chars.as_str()
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.chars.as_str().is_empty()
    }

    pub(crate) fn len(&self) -> TextSize {
        TextSize::new(self.chars.as_str().len() as u32)
    }

    pub(crate) fn previous(&self) -> char {
        self.previous
    }

    pub(crate) fn pos_within_token(&self) -> TextSize {
        self.len - self.len()
    }

    pub(crate) fn reset_pos_within_token(&mut self) {
        self.len = self.len();
    }

    pub(crate) fn matches(&self, ch: char) -> bool {
        self.peek() == ch
    }

    pub(crate) fn peek(&self) -> char {
        self.chars.clone().next().unwrap_or(EOF_CHAR)
    }

    pub(crate) fn second(&self) -> char {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next().unwrap_or(EOF_CHAR)
    }

    pub(crate) fn advance(&mut self) -> char {
        self.previous = self.chars.next().unwrap_or(EOF_CHAR);
        self.previous
    }

    pub(crate) fn advance_until(&mut self, byte: ascii::AsciiChar) {
        if let Some(index) = memchr::memchr(byte as u8, self.as_str().as_bytes()) {
            let (prefix, suffix) = self.chars.as_str().split_at(index);
            self.previous = prefix.chars().last().unwrap_or(EOF_CHAR);
            self.chars = suffix.chars();
        } else {
            let chars = std::mem::replace(&mut self.chars, "".chars());
            self.previous = chars.last().unwrap_or(EOF_CHAR);
        }
    }

    pub(crate) fn advance_while(&mut self, f: impl Fn(char) -> bool + Copy) {
        while !self.is_eof() && f(self.peek()) {
            self.advance();
        }
    }
}

#[cfg(test)]
mod tests {
    use ascii::AsciiChar;

    use super::*;

    #[test]
    fn test_previous_initial() {
        let cursor = Cursor::new("a");
        assert_eq!(cursor.previous(), '\0');
    }

    #[test]
    fn test_advance_and_previous() {
        let mut cursor = Cursor::new("ab");
        let first = cursor.advance();
        assert_eq!(first, 'a');
        assert_eq!(cursor.previous(), 'a');
        assert_eq!(cursor.peek(), 'b');

        let second = cursor.advance();
        assert_eq!(second, 'b');
        assert_eq!(cursor.previous(), 'b');
        assert_eq!(cursor.peek(), EOF_CHAR);
    }

    #[test]
    fn test_advance_on_empty_returns_eof() {
        let mut empty = Cursor::new("");
        let adv = empty.advance();
        assert_eq!(adv, EOF_CHAR);
        assert_eq!(empty.previous(), EOF_CHAR);
        assert!(empty.is_eof());
    }

    #[test]
    fn test_as_str_after_advance() {
        let mut cursor = Cursor::new("rustacean");
        cursor.advance();
        assert_eq!(cursor.as_str(), "ustacean");
    }

    #[test]
    fn test_peek_and_matches_and_second() {
        let cursor = Cursor::new("foo");
        assert_eq!(cursor.peek(), 'f');
        assert!(cursor.matches('f'));
        assert!(!cursor.matches('z'));
        assert_eq!(cursor.second(), 'o');

        let single = Cursor::new("x");
        assert_eq!(single.second(), EOF_CHAR);
    }

    #[test]
    fn test_second_does_not_consume() {
        let cursor = Cursor::new("abc");
        let sec = cursor.second();
        assert_eq!(sec, 'b');
        assert_eq!(cursor.peek(), 'a');
        assert_eq!(cursor.previous(), '\0');
    }

    #[test]
    fn test_advance_until_found_mid_string() {
        let mut cursor = Cursor::new("hello, world");
        cursor.advance_until(AsciiChar::Comma);
        assert_eq!(cursor.peek(), ',');
        assert_eq!(cursor.previous(), 'o');
        assert_eq!(cursor.as_str(), ", world");
    }

    #[test]
    fn test_advance_until_none_returns_last_char() {
        let mut cursor = Cursor::new("abc");
        cursor.advance_until(AsciiChar::z);
        assert!(cursor.is_eof());
        assert_eq!(cursor.previous(), 'c');
    }

    #[test]
    fn test_advance_until_multiple_occurrences() {
        let mut cursor = Cursor::new(",a,b");
        cursor.advance_until(AsciiChar::Comma);
        assert_eq!(cursor.peek(), ',');
        assert_eq!(cursor.previous(), EOF_CHAR);
        cursor.advance();
        cursor.advance_until(AsciiChar::Comma);
        assert_eq!(cursor.peek(), ',');
        assert_eq!(cursor.previous(), 'a');
    }

    #[test]
    fn test_advance_while() {
        let mut cursor = Cursor::new("abc123");
        cursor.advance_while(|c| c.is_alphabetic());
        assert_eq!(cursor.peek(), '1');
        assert_eq!(cursor.previous(), 'c');

        let mut empty = Cursor::new("");
        empty.advance_while(|c| c.is_ascii());
        assert!(empty.is_eof());
        assert_eq!(empty.previous(), EOF_CHAR);
    }

    #[test]
    fn test_is_eof_initial_and_after() {
        let cursor = Cursor::new("");
        assert!(cursor.is_eof());

        let mut cursor2 = Cursor::new("x");
        assert!(!cursor2.is_eof());
        cursor2.advance();
        assert!(cursor2.is_eof());
    }

    #[test]
    fn test_len_pos_within_token_and_reset() {
        let mut cursor = Cursor::new("hello");
        assert_eq!(cursor.len(), TextSize::new(5));
        assert_eq!(cursor.pos_within_token(), TextSize::new(0));

        cursor.advance();
        assert_eq!(cursor.len(), TextSize::new(4));
        assert_eq!(cursor.pos_within_token(), TextSize::new(1));

        cursor.reset_pos_within_token();
        assert_eq!(cursor.pos_within_token(), TextSize::new(0));
    }

    #[test]
    fn test_unicode_handling() {
        let s = "💖rust";
        let mut cursor = Cursor::new(s);
        assert_eq!(cursor.peek(), '💖');
        let first = cursor.advance();
        assert_eq!(first, '💖');
        assert_eq!(cursor.peek(), 'r');
        assert_eq!(cursor.len(), TextSize::new((s.len() - first.len_utf8()) as u32));
    }

    #[test]
    fn test_matches_false_on_empty() {
        let cursor = Cursor::new("");
        assert!(!cursor.matches('a'));
    }

    #[derive(Clone, Copy, Debug)]
    struct Char(AsciiChar);

    impl quickcheck::Arbitrary for Char {
        fn arbitrary(g: &mut quickcheck::Gen) -> Self {
            let byte: u8 = u8::arbitrary(g) % 128;
            Self(AsciiChar::from_ascii(byte).unwrap())
        }
    }

    quickcheck::quickcheck! {
        fn prop_len_plus_pos_equals_initial(text: String) -> bool {
            let mut cursor = Cursor::new(&text);
            let initial_len = cursor.len();
            cursor.advance_while(|c| c != ' ');
            let sum = cursor.len() + cursor.pos_within_token();
            sum == initial_len
        }

        fn prop_advance_until_preserves_invariants(text: String, c: Char) -> bool {
            let mut cursor = Cursor::new(&text);
            let initial = cursor.len();
            cursor.advance_until(c.0);
            let sum = cursor.len() + cursor.pos_within_token();
            sum == initial
        }

        fn prop_clone_does_not_affect_original(text: String) -> bool {
            let  a = Cursor::new(&text);
            let mut b = a.clone();
            let peek_before = a.peek();
            b.advance();
            a.peek() == peek_before
        }
    }
}

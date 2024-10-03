use std::str::Chars;

use text_size::{TextLen, TextSize};

pub(crate) const EOF_CHAR: char = '\0';

pub(crate) struct Cursor<'db> {
    chars: Chars<'db>,
    len: TextSize,
    previous: char,
}

impl<'db> Cursor<'db> {
    pub(crate) fn new(text: &'db str) -> Self {
        Self { chars: text.chars(), len: text.text_len(), previous: '\0' }
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

    pub(crate) fn advance_while(&mut self, f: impl Fn(char) -> bool + Copy) {
        while self.peek() != EOF_CHAR && f(self.peek()) {
            self.advance();
        }
    }
}

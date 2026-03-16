#[cfg(test)]
use std::fmt::Write as _;

use drop_bomb::DropBomb;
use mitki_errors::Diagnostic;
use mitki_tokenizer::{TokenIndex, Tokenizer};
use mitki_yellow::SyntaxKind::{self, *};
use mitki_yellow::{Builder, SyntaxSet, SyntaxTree};
use text_size::TextRange;

pub(crate) struct Parser<'text> {
    text: &'text str,
    tokenizer: Tokenizer,
    events: Vec<Event>,
    diagnostics: Vec<Diagnostic>,
    previous_range: TextRange,
}

impl<'text> Parser<'text> {
    pub(crate) fn new(text: &'text str) -> Self {
        Self {
            text,
            tokenizer: Tokenizer::new(text),
            events: Vec::new(),
            diagnostics: Vec::new(),
            previous_range: TextRange::default(),
        }
    }

    pub(crate) fn try_parse(&mut self, parser: impl FnOnce(&mut Self) -> bool) {
        let prev_events = self.events.len();
        let prev_diagnostics = self.diagnostics.len();
        let prev_tokenizer = self.tokenizer.clone();
        let prev_range = self.previous_range;

        if !parser(self) {
            self.events.truncate(prev_events);
            self.diagnostics.truncate(prev_diagnostics);
            self.tokenizer = prev_tokenizer;
            self.previous_range = prev_range;
        }
    }

    pub(crate) fn peek_kind(&self) -> SyntaxKind {
        self.tokenizer.peek().kind
    }

    pub(crate) fn peek_range(&self) -> TextRange {
        self.tokenizer.peek().kind_range
    }

    pub(crate) fn next_token_on_same_line(&self) -> bool {
        self.tokenizer.peek().on_same_line()
    }

    pub(crate) fn peek_text(&self) -> &'text str {
        &self.text[self.peek_range()]
    }

    pub(crate) fn nth_kind(&self, n: usize) -> SyntaxKind {
        let mut tokenizer = self.tokenizer.clone();
        for _ in 0..n {
            let _ = tokenizer.next_token();
        }
        tokenizer.peek().kind
    }

    pub(crate) fn nth_text(&self, n: usize) -> &'text str {
        let mut tokenizer = self.tokenizer.clone();
        for _ in 0..n {
            let _ = tokenizer.next_token();
        }
        let range = tokenizer.peek().kind_range;
        &self.text[range]
    }

    pub(crate) fn at_binary_op(&self, op: &str) -> bool {
        self.peek_kind() == BINARY_OPERATOR && self.peek_text() == op
    }

    pub(crate) fn advance(&mut self) {
        if self.peek_kind() == EOF {
            return;
        }

        let token_index = self.tokenizer.next_token_index();
        let token = self.tokenizer.token(token_index);
        self.previous_range = token.kind_range;
        self.events.push(Event::Token(token_index));
    }

    pub(crate) fn at(&mut self, kind: SyntaxKind) -> bool {
        self.peek_kind() == kind
    }

    pub(crate) fn eat(&mut self, kind: SyntaxKind) -> bool {
        let is_present = self.at(kind);

        if is_present {
            self.advance();
        }

        is_present
    }

    pub(crate) fn expect(&mut self, kind: SyntaxKind) -> bool {
        let is_present = self.at(kind);

        if self.at(kind) {
            self.advance();
        } else {
            self.error(&format!("expected {kind}"));
        }

        is_present
    }

    pub(crate) fn error_and_bump(&mut self, message: &str) {
        self.error_recover(message, &SyntaxSet::EMPTY);
    }

    pub(crate) fn error_recover(&mut self, message: &str, recovery: &SyntaxSet) {
        if recovery.contains(self.peek_kind()) {
            self.error(message);
            return;
        }

        let m = self.start();
        self.error(message);
        self.advance();
        m.complete(self, ERROR);
    }

    pub(crate) fn start(&mut self) -> Marker {
        let pos = self.events.len() as u32;
        self.events.push(Event::Start { kind: TOMBSTONE, forward_parent: None });
        Marker::new(pos)
    }

    pub(crate) fn error_with_range(&mut self, message: &str, range: TextRange) {
        if self.diagnostics.last().is_some_and(|last| last.range().start() == range.start()) {
            return;
        }

        self.diagnostics.push(Diagnostic::error(message, range));
    }

    pub(crate) fn error(&mut self, message: &str) {
        let range = self.tokenizer.peek().kind_range;
        self.error_with_range(message, range);
    }

    pub(crate) fn build_tree(self) -> (SyntaxTree, Vec<Diagnostic>) {
        let Parser { text, tokenizer, mut events, mut diagnostics, .. } = self;
        let mut builder = Builder::new(text);
        let mut forward_parents = Vec::new();

        for i in 0..events.len() {
            match std::mem::replace(&mut events[i], Event::TOMBSTONE) {
                Event::Start { kind, forward_parent } => {
                    if kind == TOMBSTONE {
                        continue;
                    }

                    collect_forward_parents(
                        &mut events,
                        i,
                        kind,
                        forward_parent,
                        &mut forward_parents,
                    );

                    for kind in forward_parents.drain(..).rev() {
                        builder.start_node(kind);
                    }
                }
                Event::Finish => builder.finish_node(),
                Event::Token(token_index) => {
                    let token = tokenizer.token(token_index);
                    let leading = tokenizer.leading_trivia(token_index);
                    let trailing = tokenizer.trailing_trivia(token_index);
                    builder.token(
                        leading.iter().copied(),
                        token.kind,
                        token.kind_range.len(),
                        trailing.iter().copied(),
                    );
                }
            }
        }

        extend_with_tokenizer_diagnostics(&mut diagnostics, &tokenizer);

        (builder.finish(), diagnostics)
    }

    pub(crate) fn previous_range(&self) -> TextRange {
        self.previous_range
    }

    #[cfg(test)]
    pub(crate) fn debug_tree(self) -> (String, Vec<Diagnostic>) {
        let Parser { text, tokenizer, mut events, mut diagnostics, .. } = self;
        let mut output = String::new();
        let mut indent = 0usize;
        let mut forward_parents = Vec::new();

        for i in 0..events.len() {
            match std::mem::replace(&mut events[i], Event::TOMBSTONE) {
                Event::Start { kind, forward_parent } => {
                    if kind == TOMBSTONE {
                        continue;
                    }

                    collect_forward_parents(
                        &mut events,
                        i,
                        kind,
                        forward_parent,
                        &mut forward_parents,
                    );

                    for kind in forward_parents.drain(..).rev() {
                        let indent_str = "  ".repeat(indent);
                        let _ = writeln!(output, "{indent_str}{kind:?}");
                        indent += 1;
                    }
                }
                Event::Finish => {
                    indent -= 1;
                }
                Event::Token(token_index) => {
                    let token = tokenizer.token(token_index);
                    if token.kind.is_trivia() {
                        continue;
                    }
                    let token_text = &text[token.kind_range];
                    let indent_str = "  ".repeat(indent);
                    let _ = writeln!(output, "{indent_str}{:?}: {:?}", token.kind, token_text);
                }
            }
        }

        extend_with_tokenizer_diagnostics(&mut diagnostics, &tokenizer);

        (output, diagnostics)
    }
}

fn collect_forward_parents(
    events: &mut [Event],
    start_index: usize,
    kind: SyntaxKind,
    forward_parent: Option<u32>,
    forward_parents: &mut Vec<SyntaxKind>,
) {
    forward_parents.push(kind);
    let mut idx = start_index;
    let mut fp = forward_parent;

    while let Some(fwd) = fp {
        idx += fwd as usize;

        fp = match std::mem::replace(&mut events[idx], Event::TOMBSTONE) {
            Event::Start { kind, forward_parent, .. } => {
                if kind != TOMBSTONE {
                    forward_parents.push(kind);
                }
                forward_parent
            }
            _ => unreachable!(),
        };
    }
}

fn extend_with_tokenizer_diagnostics(diagnostics: &mut Vec<Diagnostic>, tokenizer: &Tokenizer) {
    diagnostics.extend(tokenizer.diagnostics().iter().map(|diagnostic| match diagnostic {
        mitki_tokenizer::Diagnostic::InconsistentWhitespaceAroundEqual(range) => {
            Diagnostic::error("Consistent whitespace required around '='", *range)
        }
    }));
}

enum Event {
    Start { kind: SyntaxKind, forward_parent: Option<u32> },
    Token(TokenIndex),
    Finish,
}

impl Event {
    const TOMBSTONE: Self = Self::Start { kind: TOMBSTONE, forward_parent: None };
}

pub(crate) struct Marker {
    position: u32,
    bomb: DropBomb,
}

impl Marker {
    fn new(pos: u32) -> Self {
        Self { position: pos, bomb: DropBomb::new("Marker must be either completed or abandoned") }
    }

    pub(crate) fn complete(mut self, p: &mut Parser<'_>, kind: SyntaxKind) -> CompletedMarker {
        self.bomb.defuse();

        match &mut p.events[self.position as usize] {
            Event::Start { kind: slot, .. } => {
                *slot = kind;
            }
            _ => unreachable!(),
        }

        p.events.push(Event::Finish);
        CompletedMarker::new(self.position, kind)
    }
}

pub(crate) struct CompletedMarker {
    pos: u32,
    kind: SyntaxKind,
}

impl CompletedMarker {
    fn new(pos: u32, kind: SyntaxKind) -> Self {
        Self { pos, kind }
    }

    pub(crate) fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub(crate) fn precede(self, p: &mut Parser<'_>) -> Marker {
        let new_pos = p.start();

        match &mut p.events[self.pos as usize] {
            Event::Start { forward_parent, .. } => {
                *forward_parent = Some(new_pos.position - self.pos);
            }
            _ => unreachable!(),
        }

        new_pos
    }
}

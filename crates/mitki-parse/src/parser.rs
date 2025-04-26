use drop_bomb::DropBomb;
use mitki_errors::Diagnostic;
use mitki_tokenizer::{Token, Tokenizer};
use mitki_yellow::SyntaxKind::{self, *};
use mitki_yellow::{Builder, GreenNode, SyntaxSet};
use salsa::Database;
use text_size::TextRange;

pub(crate) struct Parser<'db> {
    db: &'db dyn Database,
    text: &'db str,
    tokenizer: Tokenizer<'db>,
    events: Vec<Event>,
    diagnostics: Vec<Diagnostic>,
    previous_range: TextRange,
}

impl<'db> Parser<'db> {
    pub(crate) fn new(db: &'db dyn Database, text: &'db str) -> Self {
        Self {
            db,
            text,
            tokenizer: Tokenizer::new(text),
            events: Vec::new(),
            diagnostics: Vec::new(),
            previous_range: TextRange::default(),
        }
    }

    pub(crate) fn try_parse(&mut self, parser: fn(&mut Self) -> bool) {
        let mut snapshot = self.fork();
        if parser(&mut snapshot) {
            self.merge(snapshot);
        }
    }

    fn fork(&self) -> Self {
        Self {
            db: self.db,
            text: self.text,
            tokenizer: self.tokenizer.clone(),
            events: Vec::new(),
            diagnostics: Vec::new(),
            previous_range: TextRange::default(),
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

    pub(crate) fn advance(&mut self) {
        if self.peek_kind() == EOF {
            return;
        }

        let token = self.tokenizer.next_token();
        self.previous_range = token.kind_range;
        self.events.push(Event::Token(token));
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
        self.events.push(Event::TOMBSTONE);
        Marker::new(pos)
    }

    pub(crate) fn merge(&mut self, other: Self) {
        self.tokenizer = other.tokenizer;
        self.events.extend(other.events);
        self.diagnostics.extend(other.diagnostics);
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

    pub(crate) fn build_tree(self) -> GreenNode<'db> {
        use salsa::Accumulator as _;

        let Parser { db, text, tokenizer, mut events, diagnostics, .. } = self;
        let mut builder = Builder::new(db, text);
        let mut forward_parents = Vec::new();

        for i in 0..events.len() {
            match std::mem::replace(&mut events[i], Event::TOMBSTONE) {
                Event::Start { kind, forward_parent } => {
                    if kind == TOMBSTONE {
                        continue;
                    }

                    forward_parents.push(kind);
                    let mut idx = i;
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

                    for kind in forward_parents.drain(..).rev() {
                        builder.start_node(kind);
                    }
                }
                Event::Finish => builder.finish_node(),
                Event::Token(Token { leading, kind, kind_range, trailing }) => {
                    builder.token(leading, kind, kind_range, trailing);
                }
            }
        }

        for diagnostic in diagnostics {
            diagnostic.accumulate(db);
        }

        for diagnostic in tokenizer.diagnostics() {
            match diagnostic {
                mitki_tokenizer::Diagnostic::InconsistentWhitespaceAroundEqual(range) => {
                    Diagnostic::error("Consistent whitespace required around '='", range)
                        .accumulate(db);
                }
            }
        }

        builder.finish()
    }

    pub(crate) fn previous_range(&self) -> TextRange {
        self.previous_range
    }
}

enum Event {
    Start { kind: SyntaxKind, forward_parent: Option<u32> },
    Token(Token),
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
        CompletedMarker::new(self.position)
    }
}

pub(crate) struct CompletedMarker {
    pos: u32,
}

impl CompletedMarker {
    fn new(pos: u32) -> Self {
        Self { pos }
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

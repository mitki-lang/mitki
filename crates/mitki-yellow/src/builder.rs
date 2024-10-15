use salsa::Database;
use text_size::TextRange;

use crate::green::{GreenNode, GreenToken};
use crate::{Green, GreenTrivia, SyntaxKind};

pub struct Builder<'db> {
    db: &'db dyn Database,
    text: &'db str,
    parents: Vec<(SyntaxKind, usize)>,
    children: Vec<Green<'db>>,
}

impl<'db> Builder<'db> {
    pub fn new(db: &'db dyn Database, text: &'db str) -> Self {
        Self { db, text, parents: Vec::new(), children: Vec::new() }
    }

    pub fn start_node(&mut self, kind: SyntaxKind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }

    pub fn token(
        &mut self,
        leading: GreenTrivia,
        kind: SyntaxKind,
        kind_range: TextRange,
        trailing: GreenTrivia,
    ) {
        let range =
            TextRange::new(kind_range.start() - leading.len(), kind_range.end() + trailing.len());

        let text = &self.text[range];
        let token = GreenToken::new(self.db, leading, kind, text, trailing);

        self.children.push(Green::Token(token));
    }

    pub fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let children: Vec<_> = self.children.drain(first_child..).collect();

        let node = GreenNode::new(self.db, kind, children);
        self.children.push(Green::Node(node));
    }

    pub fn finish(mut self) -> GreenNode<'db> {
        assert_eq!(self.children.len(), 1);

        match self.children.pop().unwrap() {
            Green::Node(node) => node,
            Green::Token(_) => panic!(),
        }
    }
}

use salsa::Database;

use crate::{NodeOrToken, Red, RedNode};

pub enum Direction {
    Next,
    Prev,
}

pub struct Preorder<'db> {
    db: &'db dyn Database,
    start: RedNode<'db>,
    next: Option<WalkEvent<RedNode<'db>>>,
    skip_subtree: bool,
}

impl<'db> Preorder<'db> {
    pub fn new(db: &'db dyn Database, start: RedNode<'db>) -> Self {
        let next = Some(WalkEvent::Enter(start.clone()));
        Self { db, start, next, skip_subtree: false }
    }

    pub fn skip_subtree(&mut self) {
        self.skip_subtree = true;
    }
}

impl<'db> Iterator for Preorder<'db> {
    type Item = WalkEvent<RedNode<'db>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.skip_subtree {
            self.next = self.next.take().map(|next| match next {
                WalkEvent::Enter(first_child) => {
                    WalkEvent::Leave(first_child.parent().unwrap().clone())
                }
                WalkEvent::Leave(parent) => WalkEvent::Leave(parent),
            });
            self.skip_subtree = false;
        }

        let next = self.next.take();

        self.next = next.as_ref().and_then(|event| {
            Some(match event {
                WalkEvent::Enter(node) => match node.first_child(self.db) {
                    Some(child) => WalkEvent::Enter(child),
                    None => WalkEvent::Leave(node.clone()),
                },
                WalkEvent::Leave(node) => {
                    if node == &self.start {
                        return None;
                    }

                    match node.next_sibling(self.db) {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(node.parent()?.clone()),
                    }
                }
            })
        });

        next
    }
}

pub struct PreorderWithTokens<'db> {
    db: &'db dyn Database,
    start: Red<'db>,
    next: Option<WalkEvent<Red<'db>>>,
    skip_subtree: bool,
    direction: Direction,
}

impl<'db> PreorderWithTokens<'db> {
    pub fn new(db: &'db dyn Database, start: RedNode<'db>, direction: Direction) -> Self {
        let start = Red::Node(start);
        let next = Some(WalkEvent::Enter(start.clone()));

        Self { db, start, next, skip_subtree: false, direction }
    }
}

impl<'db> Iterator for PreorderWithTokens<'db> {
    type Item = WalkEvent<Red<'db>>;

    fn next(&mut self) -> Option<WalkEvent<Red<'db>>> {
        if self.skip_subtree {
            self.next = self.next.take().map(|next| match next {
                WalkEvent::Enter(first_child) => {
                    WalkEvent::Leave(Red::Node(first_child.parent().unwrap().clone()))
                }
                WalkEvent::Leave(parent) => WalkEvent::Leave(parent),
            });
            self.skip_subtree = false;
        }

        let next = self.next.take();
        self.next = next.as_ref().and_then(|next| {
            Some(match next {
                WalkEvent::Enter(el) => match el {
                    NodeOrToken::Node(node) => {
                        let next = match self.direction {
                            Direction::Next => node.first_child_or_token(self.db),
                            Direction::Prev => node.last_child_or_token(self.db),
                        };
                        match next {
                            Some(child) => WalkEvent::Enter(child),
                            None => WalkEvent::Leave(Red::Node(node.clone())),
                        }
                    }
                    NodeOrToken::Token(token) => WalkEvent::Leave(Red::Token(token.clone())),
                },
                WalkEvent::Leave(el) if el == &self.start => return None,
                WalkEvent::Leave(el) => {
                    let next = match self.direction {
                        Direction::Next => el.next_sibling_or_token(self.db),
                        Direction::Prev => el.prev_sibling_or_token(self.db),
                    };

                    match next {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(Red::Node(el.parent()?.clone())),
                    }
                }
            })
        });
        next
    }
}

#[derive(Debug)]
pub enum WalkEvent<T> {
    Enter(T),
    Leave(T),
}

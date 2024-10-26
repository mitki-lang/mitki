use salsa::Database;

use crate::RedNode;

pub struct Preorder<'db> {
    db: &'db dyn Database,
    start: RedNode<'db>,
    next: Option<WalkEvent<'db>>,
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
    type Item = WalkEvent<'db>;

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

#[derive(Debug)]
pub enum WalkEvent<'db> {
    Enter(RedNode<'db>),
    Leave(RedNode<'db>),
}

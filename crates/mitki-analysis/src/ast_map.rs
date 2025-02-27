use mitki_inputs::File;
use mitki_parse::FileParse;
use mitki_yellow::{RedNode, RedNodePtr, SyntaxKind};
use rustc_hash::FxHashMap;
use salsa::Database;

use crate::arena::{Arena, Idx};

pub(crate) trait HasAstMap {
    fn ast_map(self, db: &dyn Database) -> &AstMap;
}

#[salsa::tracked]
impl HasAstMap for File {
    #[salsa::tracked(return_ref, no_eq)]
    fn ast_map(self, db: &dyn Database) -> AstMap {
        AstMap::from_root(db, &self.parse(db).syntax_node())
    }
}

#[derive(Debug, salsa::Update)]
pub(crate) struct AstMap {
    arena: Arena<RedNodePtr>,
    map: FxHashMap<RedNodePtr, Idx<RedNodePtr>>,
}

impl AstMap {
    pub(crate) fn from_root(db: &dyn Database, root: &RedNode<'_>) -> Self {
        let mut arena = Arena::new();

        root.children(db).for_each(|node| {
            if let SyntaxKind::FN = node.kind(db) {
                arena.alloc(RedNodePtr::new(db, &node));
            }
        });

        let mut map = FxHashMap::with_capacity_and_hasher(arena.len(), Default::default());
        for (index, node) in arena.iter() {
            map.insert(*node, index);
        }

        Self { arena, map }
    }

    pub(crate) fn find_id(&self, db: &dyn Database, node: &RedNode<'_>) -> Idx<RedNodePtr> {
        self.map[&RedNodePtr::new(db, node)]
    }

    pub(crate) fn find_node(&self, index: Idx<RedNodePtr>) -> &RedNodePtr {
        &self.arena[index]
    }
}

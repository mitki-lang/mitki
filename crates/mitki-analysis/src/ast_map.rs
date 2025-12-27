use hashbrown::HashTable;
use mitki_inputs::File;
use mitki_parse::FileParse as _;
use mitki_yellow::{RedNode, RedNodePtr, SyntaxKind};
use salsa::Database;

use crate::arena::{Arena, Key};

pub(crate) trait HasAstMap {
    fn ast_map(self, db: &dyn Database) -> &AstMap;
}

#[salsa::tracked]
impl HasAstMap for File {
    #[salsa::tracked(returns(ref), no_eq)]
    fn ast_map(self, db: &dyn Database) -> AstMap {
        AstMap::from_root(&self.parse(db).syntax_node())
    }
}

#[derive(Debug)]
pub(crate) struct AstMap {
    arena: Arena<RedNodePtr>,
    map: HashTable<Key<RedNodePtr>>,
}

impl PartialEq for AstMap {
    fn eq(&self, other: &Self) -> bool {
        self.arena == other.arena
    }
}

impl Eq for AstMap {}

impl AstMap {
    pub(crate) fn from_root(root: &RedNode<'_>) -> Self {
        let mut arena = Arena::new();
        let mut map = HashTable::default();

        root.children().for_each(|node| {
            if let SyntaxKind::FN = node.kind() {
                arena.alloc(RedNodePtr::new(&node));
            }
        });

        for (key, value) in arena.iter_enumerated() {
            let hash = hash_one(&value);
            map.insert_unique(hash, key, |&key| hash_one(&arena[key]));
        }

        Self { arena, map }
    }

    pub(crate) fn find_id(&self, node: &RedNode<'_>) -> Key<RedNodePtr> {
        let ptr = RedNodePtr::new(node);
        *self.map.find(hash_one(&ptr), |&key| self.arena[key] == ptr).unwrap()
    }

    pub(crate) fn find_node(&self, index: Key<RedNodePtr>) -> &RedNodePtr {
        &self.arena[index]
    }
}

fn hash_one<T: std::hash::Hash>(t: &T) -> u64 {
    use std::hash::BuildHasher as _;

    std::hash::BuildHasherDefault::<rustc_hash::FxHasher>::default().hash_one(t)
}

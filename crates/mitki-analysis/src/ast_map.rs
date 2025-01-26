use hashbrown::HashMap;
use la_arena::{Arena, Idx};
use mitki_inputs::File;
use mitki_parse::FileParse;
use mitki_yellow::{RedNode, RedNodePtr, SyntaxKind};
use salsa::Database;

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

#[derive(Debug)]
pub(crate) struct AstMap {
    arena: Arena<RedNodePtr>,
    map: HashMap<Idx<RedNodePtr>, (), ()>,
}

impl AstMap {
    pub(crate) fn from_root(db: &dyn Database, root: &RedNode<'_>) -> Self {
        let mut arena = Arena::new();

        root.children(db).for_each(|node| {
            if let SyntaxKind::FN = node.kind(db) {
                arena.alloc(RedNodePtr::new(db, &node));
            }
        });

        let mut map = HashMap::with_capacity_and_hasher(arena.len(), ());
        for (index, node) in arena.iter() {
            let hash = hash_one(node);

            match map.raw_entry_mut().from_hash(hash, |&found| found == index) {
                hashbrown::hash_map::RawEntryMut::Vacant(entry) => {
                    entry.insert_with_hasher(hash, index, (), |&idx| hash_one(&arena[idx]));
                }
                hashbrown::hash_map::RawEntryMut::Occupied(_) => {
                    unreachable!("duplicate node");
                }
            }
        }

        Self { arena, map }
    }

    pub(crate) fn find_id(&self, db: &dyn Database, node: &RedNode<'_>) -> Idx<RedNodePtr> {
        let ptr = RedNodePtr::new(db, node);
        let hash = hash_one(&ptr);

        match self.map.raw_entry().from_hash(hash, |&index| self.arena[index] == ptr) {
            Some((&index, &())) => index,
            None => panic!(
                "Can't find {:?} in AstMap:\n{:?}",
                RedNodePtr::new(db, node),
                self.arena.iter().map(|(_id, i)| i).collect::<Vec<_>>(),
            ),
        }
    }

    pub(crate) fn find_node(&self, index: Idx<RedNodePtr>) -> &RedNodePtr {
        &self.arena[index]
    }
}

fn hash_one<T: std::hash::Hash>(t: &T) -> u64 {
    use std::hash::BuildHasher as _;

    std::hash::BuildHasherDefault::<rustc_hash::FxHasher>::default().hash_one(t)
}

use std::future::Future;
use std::sync::Arc;

use hashbrown::HashTable;
use mitki_hir::arena::{Arena, Key};
use mitki_inputs::File;
use mitki_yellow::{SyntaxKind, SyntaxNode, SyntaxNodePtr};

pub trait HasAstMap {
    fn ast_map<DB>(self, db: &DB) -> impl Future<Output = Arc<AstMap>> + Send
    where
        DB: AstMapDb + Sync;
}

impl HasAstMap for File {
    #[allow(clippy::manual_async_fn)]
    fn ast_map<DB>(self, db: &DB) -> impl Future<Output = Arc<AstMap>> + Send
    where
        DB: AstMapDb + Sync,
    {
        async move { ast_map(db, self).await.expect("failed to compute ast map") }
    }
}

pub trait AstMapDb: mitki_parse::ParseDb + HasAstMapQuery {}

impl<T> AstMapDb for T where T: mitki_parse::ParseDb + HasAstMapQuery {}

#[picante::tracked]
pub async fn ast_map<DB: mitki_parse::HasParseQuery>(
    db: &DB,
    file: File,
) -> picante::PicanteResult<Arc<AstMap>> {
    let parsed = mitki_parse::parse(db, file).await?;
    Ok(Arc::new(AstMap::from_root(&parsed.syntax_node())))
}

#[derive(Debug, facet::Facet)]
pub struct AstMap {
    #[facet(opaque)]
    arena: Arena<SyntaxNodePtr>,
    #[facet(opaque)]
    map: HashTable<Key<SyntaxNodePtr>>,
}

impl PartialEq for AstMap {
    fn eq(&self, other: &Self) -> bool {
        self.arena == other.arena
    }
}

impl Eq for AstMap {}

impl AstMap {
    pub fn from_root(root: &SyntaxNode<'_>) -> Self {
        let mut arena = Arena::new();
        let mut map = HashTable::default();

        root.children().for_each(|node| {
            if matches!(node.kind(), SyntaxKind::FN | SyntaxKind::STRUCT_DEF | SyntaxKind::ENUM_DEF)
            {
                arena.alloc(SyntaxNodePtr::new(&node));
            }
        });

        for (key, value) in arena.iter_enumerated() {
            let hash = hash_one(&value);
            map.insert_unique(hash, key, |&key| hash_one(&arena[key]));
        }

        Self { arena, map }
    }

    pub fn find_id(&self, node: &SyntaxNode<'_>) -> Key<SyntaxNodePtr> {
        let ptr = SyntaxNodePtr::new(node);
        *self.map.find(hash_one(&ptr), |&key| self.arena[key] == ptr).unwrap()
    }

    pub fn find_node(&self, index: Key<SyntaxNodePtr>) -> &SyntaxNodePtr {
        &self.arena[index]
    }
}

fn hash_one<T: std::hash::Hash>(t: &T) -> u64 {
    use std::hash::BuildHasher as _;

    std::hash::BuildHasherDefault::<rustc_hash::FxHasher>::default().hash_one(t)
}

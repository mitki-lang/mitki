mod goto_definition;

use mitki_db::RootDatabase;

#[derive(Default)]
pub struct Analysis {
    db: RootDatabase,
}

impl Analysis {
    pub fn db(&self) -> &RootDatabase {
        &self.db
    }

    pub fn db_mut(&mut self) -> &mut RootDatabase {
        &mut self.db
    }
}

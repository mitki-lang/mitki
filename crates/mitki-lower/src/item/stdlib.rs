use camino::{Utf8Path, Utf8PathBuf};
use mitki_inputs::{File, PackageId};
use salsa::Database;

fn stdlib_src_dir() -> Utf8PathBuf {
    Utf8Path::new(env!("CARGO_MANIFEST_DIR")).join("../../stdlib/src")
}

pub fn stdlib_module_path(relative: &str) -> Utf8PathBuf {
    stdlib_src_dir().join(relative)
}

fn load_stdlib_file(db: &dyn Database, path: Utf8PathBuf) -> File {
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|err| panic!("failed to read stdlib file {}: {err}", path));
    File::new(db, path, text)
}

#[salsa::tracked]
pub fn stdlib_root_file(db: &dyn Database) -> File {
    load_stdlib_file(db, stdlib_module_path("lib.mitki"))
}

#[salsa::tracked]
pub fn stdlib_package(db: &dyn Database) -> PackageId<'_> {
    PackageId::new(db, stdlib_root_file(db))
}

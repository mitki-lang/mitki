use mitki_inputs::{File, ModuleId, PackageId};
use mitki_parse::FileParse as _;
use mitki_yellow::ast::HasName as _;
use salsa::Database;

use super::stdlib::stdlib_package;

pub trait HasPackage<'db> {
    fn package(self, db: &'db dyn Database) -> PackageId<'db>;
}

#[salsa::tracked]
impl<'db> HasPackage<'db> for File {
    #[salsa::tracked]
    fn package(self, db: &'db dyn Database) -> PackageId<'db> {
        PackageId::new(db, self)
    }
}

#[derive(Debug, Default, PartialEq, Eq, salsa::Update)]
pub struct PackageGraph<'db> {
    root: Option<ModuleId<'db>>,
    modules: Vec<ModuleId<'db>>,
    entries: Vec<ModuleGraphEntry<'db>>,
}

#[derive(Debug, PartialEq, Eq, salsa::Update)]
struct ModuleGraphEntry<'db> {
    module: ModuleId<'db>,
    parent: Option<ModuleId<'db>>,
    name: String,
    crate_path: String,
    public: bool,
    children: Vec<ModuleId<'db>>,
}

impl<'db> PackageGraph<'db> {
    fn entry(&self, module: ModuleId<'db>) -> Option<&ModuleGraphEntry<'db>> {
        self.entries.iter().find(|entry| entry.module == module)
    }
}

#[salsa::tracked(returns(ref), no_eq)]
pub fn package_graph<'db>(db: &'db dyn Database, package: PackageId<'db>) -> PackageGraph<'db> {
    let root_file = package.root_file(db);
    let root = ModuleId::new(db, package, root_file);
    let mut graph = PackageGraph { root: Some(root), ..PackageGraph::default() };
    let root_name = package_root_name(db, package);
    ModuleLoader { db, package, graph: &mut graph }.load_module_graph(
        root,
        None,
        root_name.as_str(),
        ModuleFlags { is_root: true, is_public: true },
    );
    graph
}

#[salsa::tracked]
pub fn package_root_name<'db>(db: &'db dyn Database, package: PackageId<'db>) -> String {
    if package == stdlib_package(db) { "std".to_owned() } else { "crate".to_owned() }
}

#[salsa::tracked]
pub fn root_module<'db>(db: &'db dyn Database, package: PackageId<'db>) -> ModuleId<'db> {
    package_graph(db, package).root.expect("package graph should always have a root module")
}

#[salsa::tracked(returns(ref), no_eq)]
pub fn package_modules<'db>(db: &'db dyn Database, package: PackageId<'db>) -> Vec<ModuleId<'db>> {
    package_graph(db, package).modules.clone()
}

#[salsa::tracked(returns(ref), no_eq)]
pub fn module_children<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> Vec<ModuleId<'db>> {
    let graph = package_graph(db, module.package(db));
    graph.entry(module).map(|entry| entry.children.clone()).unwrap_or_default()
}

#[salsa::tracked]
pub fn module_name<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> String {
    let graph = package_graph(db, module.package(db));
    graph.entry(module).map(|entry| entry.name.clone()).unwrap_or_default()
}

#[salsa::tracked]
pub fn module_crate_path<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> String {
    let graph = package_graph(db, module.package(db));
    graph.entry(module).map_or_else(|| "crate".to_owned(), |entry| entry.crate_path.clone())
}

#[salsa::tracked]
#[allow(clippy::needless_pass_by_value)]
pub fn child_module_named<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    name: String,
) -> Option<ModuleId<'db>> {
    let graph = package_graph(db, module.package(db));
    graph.entry(module).and_then(|entry| {
        entry.children.iter().copied().find(|child| module_name(db, *child) == name)
    })
}

#[salsa::tracked]
pub fn module_is_public<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> bool {
    let graph = package_graph(db, module.package(db));
    graph.entry(module).is_some_and(|entry| entry.public)
}

#[salsa::tracked]
#[allow(clippy::needless_pass_by_value)]
pub fn exported_child_module_named<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    name: String,
) -> Option<ModuleId<'db>> {
    let graph = package_graph(db, module.package(db));
    graph.entry(module).and_then(|entry| {
        entry
            .children
            .iter()
            .copied()
            .find(|child| module_name(db, *child) == name && module_is_public(db, *child))
    })
}

#[derive(Clone, Copy)]
struct ModuleFlags {
    is_root: bool,
    is_public: bool,
}

struct ModuleLoader<'a, 'db> {
    db: &'db dyn Database,
    package: PackageId<'db>,
    graph: &'a mut PackageGraph<'db>,
}

impl<'db> ModuleLoader<'_, 'db> {
    fn load_module_graph(
        &mut self,
        module: ModuleId<'db>,
        parent: Option<ModuleId<'db>>,
        crate_path: &str,
        flags: ModuleFlags,
    ) {
        if self.graph.modules.contains(&module) {
            return;
        }

        let db = self.db;
        let file = module.file(db);
        self.graph.modules.push(module);
        self.graph.entries.push(ModuleGraphEntry {
            module,
            parent,
            name: if flags.is_root {
                crate_path.to_owned()
            } else if file.path(db).file_name() == Some("mod.mitki") {
                file.path(db)
                    .parent()
                    .and_then(|parent| parent.file_name())
                    .unwrap_or_default()
                    .to_owned()
            } else {
                file.path(db).file_stem().unwrap_or_default().to_owned()
            },
            crate_path: crate_path.to_owned(),
            public: flags.is_public,
            children: Vec::new(),
        });

        let declared_children = file
            .parse(db)
            .tree()
            .items()
            .filter_map(|item| match item {
                mitki_yellow::ast::Item::Module(module_item) => module_item
                    .name()
                    .map(|name| (name.as_str().to_owned(), module_item.is_public())),
                _ => None,
            })
            .collect::<Vec<_>>();

        let mut child_ids = Vec::new();
        for (child_name, child_public) in declared_children {
            let Some(child_file) = load_child_file(db, file, &child_name, flags.is_root) else {
                continue;
            };
            let child_module = ModuleId::new(db, self.package, child_file);
            let child_path = format!("{crate_path}::{child_name}");
            self.load_module_graph(
                child_module,
                Some(module),
                child_path.as_str(),
                ModuleFlags { is_root: false, is_public: child_public },
            );
            child_ids.push(child_module);
        }

        if let Some(entry) = self.graph.entries.iter_mut().find(|entry| entry.module == module) {
            entry.children = child_ids;
        }
    }
}

fn load_child_file(
    db: &dyn Database,
    current_file: File,
    child_name: &str,
    is_root: bool,
) -> Option<File> {
    let path = current_file.path(db);
    let parent = path.parent()?;
    let module_dir = if is_root || path.file_name() == Some("mod.mitki") {
        parent.to_owned()
    } else {
        parent.join(path.file_stem()?)
    };

    let direct = module_dir.join(format!("{child_name}.mitki"));
    if let Some(file) = read_file(db, &direct) {
        return Some(file);
    }

    let nested = module_dir.join(child_name).join("mod.mitki");
    read_file(db, &nested)
}

fn read_file(db: &dyn Database, path: &camino::Utf8Path) -> Option<File> {
    let text = std::fs::read_to_string(path).ok()?;
    Some(File::new(db, path.to_owned(), text))
}

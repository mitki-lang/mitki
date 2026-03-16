use mitki_abi::{
    AbiScalar, AbiValue, ArrayElements, CanonicalGraph, CanonicalNode, PackedScalarKind, ValueRef,
};
use mitki_hir::hir::WasmLinkage;
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::HasItemDecls as _;
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{Declaration, FunctionLocation};
use mitki_resolve::SignatureTypeResolver;
use mitki_typeck::infer::Inferable as _;
use mitki_yellow::ast::HasName as _;

#[cfg(not(miri))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

#[derive(clap::Parser)]
enum Options {
    #[command(alias = "r")]
    Run {
        path: camino::Utf8PathBuf,
    },
    Build {
        #[arg(long, value_enum)]
        target: BuildTarget,
        #[arg(short = 'o', long)]
        output: Option<camino::Utf8PathBuf>,
        path: camino::Utf8PathBuf,
    },
    #[command(name = "run-wasm")]
    RunWasm {
        path: camino::Utf8PathBuf,
    },
    Lsp,
}

#[derive(Clone, Copy, clap::ValueEnum)]
enum BuildTarget {
    Wasm32,
}

fn main() -> anyhow::Result<()> {
    use clap::Parser as _;

    match Options::parse() {
        Options::Run { path } => {
            let db = mitki_db::RootDatabase::default();
            let file = load_file(&db, path)?;
            render_diagnostics(&db, file, mitki_db::check_file(&db, file))?;
            Ok(())
        }
        Options::Build { target, output, path } => {
            let db = mitki_db::RootDatabase::default();
            let file = load_file(&db, path.clone())?;

            match target {
                BuildTarget::Wasm32 => match mitki_comptime_wasm::compile_file_to_wasm(&db, file) {
                    Ok(bytes) => {
                        let output = output.unwrap_or_else(|| path.with_extension("wasm"));
                        std::fs::write(&output, bytes).map_err(|error| {
                            anyhow::anyhow!("failed to write `{output}`: {error}")
                        })?;
                    }
                    Err(diagnostics) => render_diagnostics(&db, file, &diagnostics)?,
                },
            }

            Ok(())
        }
        Options::RunWasm { path } => {
            let db = mitki_db::RootDatabase::default();
            let file = load_file(&db, path)?;

            match mitki_comptime_wasm::compile_file_to_wasm(&db, file) {
                Ok(bytes) => run_wasm(&db, file, &bytes)?,
                Err(diagnostics) => render_diagnostics(&db, file, &diagnostics)?,
            }

            Ok(())
        }
        Options::Lsp => mitki_lsp_server::Server::new()?.run(),
    }
}

fn load_file(
    db: &mitki_db::RootDatabase,
    path: camino::Utf8PathBuf,
) -> anyhow::Result<mitki_inputs::File> {
    use anyhow::Context as _;

    let text =
        std::fs::read_to_string(&path).with_context(|| format!("failed to read `{path}`"))?;
    Ok(mitki_inputs::File::new(db, path, text))
}

fn render_diagnostics(
    db: &mitki_db::RootDatabase,
    file: mitki_inputs::File,
    diagnostics: &[mitki_errors::Diagnostic],
) -> anyhow::Result<()> {
    use std::io::Write as _;

    let default_path = file.path(db).as_str();
    let default_text = file.text(db);
    let renderer = mitki_errors::Renderer::styled();
    let mut stderr = std::io::stderr().lock();

    for diagnostic in diagnostics {
        let (path, text) = if let Some(path) = diagnostic.file() {
            match std::fs::read_to_string(path) {
                Ok(text) => (path.to_owned(), text),
                Err(_) => (default_path.to_owned(), default_text.to_owned()),
            }
        } else {
            (default_path.to_owned(), default_text.to_owned())
        };

        let range = diagnostic.range();
        let start = u32::from(range.start());
        let end = u32::from(range.end());
        let text_len = text.len() as u32;

        if start <= end && end <= text_len {
            writeln!(stderr, "{}", diagnostic.render(&renderer, &path, &text))?;
        } else {
            writeln!(
                stderr,
                "{:?}: {} [{}..{}] (outside `{}`; likely from another file/module)",
                diagnostic.level(),
                diagnostic.message(),
                start,
                end,
                path,
            )?;
        }
    }

    Ok(())
}

fn resolve_run_wasm_main(
    db: &mitki_db::RootDatabase,
    file: mitki_inputs::File,
) -> anyhow::Result<Ty<'_>> {
    for declaration in file.item_decls(db).declarations().iter().rev() {
        let Declaration::Function(function) = *declaration else {
            continue;
        };
        let source = function.source(db);
        if source.name().is_none_or(|name| name.as_str() != "main") {
            continue;
        }

        let lowered = function.hir_function(db).function(db);
        if matches!(lowered.linkage(), WasmLinkage::Import { .. } | WasmLinkage::RawImport { .. }) {
            continue;
        }
        if !lowered.params().is_empty() {
            anyhow::bail!("run-wasm requires `main` to have no parameters");
        }

        return classify_main_return(db, function);
    }

    anyhow::bail!("run-wasm requires an exported top-level `main` function")
}

fn classify_main_return<'db>(
    db: &'db mitki_db::RootDatabase,
    location: FunctionLocation<'db>,
) -> anyhow::Result<Ty<'db>> {
    let signature = location.signature(db);
    let return_ty = if signature.ret_type(db).is_zero() {
        let hir_function = location.hir_function(db);
        let function = hir_function.function(db);
        let inference = location.infer(db);
        inference
            .type_of_node(function.body())
            .unwrap_or_else(|| Ty::new(db, TyKind::Tuple(Vec::new())))
    } else {
        SignatureTypeResolver::new(db, location, signature)
            .resolve(signature.ret_type(db))
            .unwrap_or_else(|_error| Ty::new(db, TyKind::Unknown))
    };
    if ty_has_unresolved_parts(db, return_ty) {
        anyhow::bail!(
            "run-wasm requires `main` to return a printable runtime value, found `{}`",
            return_ty.display(db)
        )
    }
    Ok(return_ty)
}

fn ty_has_unresolved_parts(db: &mitki_db::RootDatabase, ty: Ty<'_>) -> bool {
    match ty.kind(db) {
        TyKind::Unknown | TyKind::Var(_) => true,
        TyKind::Array(item) => ty_has_unresolved_parts(db, *item),
        TyKind::Pointer { pointee, .. } => ty_has_unresolved_parts(db, *pointee),
        TyKind::Tuple(items) | TyKind::Union(items) | TyKind::Inter(items) => {
            items.iter().any(|&item| ty_has_unresolved_parts(db, item))
        }
        TyKind::Record(fields) => {
            fields.iter().any(|(_, field_ty)| ty_has_unresolved_parts(db, *field_ty))
        }
        TyKind::Function { inputs, output } => {
            inputs.iter().any(|&input| ty_has_unresolved_parts(db, input))
                || ty_has_unresolved_parts(db, *output)
        }
        TyKind::Rec(_, body) => ty_has_unresolved_parts(db, *body),
        TyKind::Bool
        | TyKind::Int
        | TyKind::ExactInt(_)
        | TyKind::Float
        | TyKind::String
        | TyKind::Char
        | TyKind::Struct(_)
        | TyKind::ExternStruct(_)
        | TyKind::Enum(_) => false,
    }
}

fn run_wasm(
    db: &mitki_db::RootDatabase,
    file: mitki_inputs::File,
    bytes: &[u8],
) -> anyhow::Result<()> {
    let abi = mitki_wasm_runtime::describe_module_abi(bytes)?;
    let start_export = abi.exports.iter().any(|export| export.name == "_start");
    let output = if start_export {
        mitki_wasm_runtime::invoke_raw_export_with_config(
            bytes,
            "_start",
            mitki_wasm_runtime::RunConfig::wasi_default(),
        )?
    } else {
        mitki_wasm_runtime::invoke_export_with_config(
            bytes,
            "main",
            &[],
            mitki_wasm_runtime::RunConfig::wasi_default(),
        )?
    };
    print!("{}", output.stdout);

    if !start_export && let Some(result) = output.result {
        let _main = resolve_run_wasm_main(db, file)?;
        println!("{}", format_abi_value(abi.metadata.as_ref(), &result));
    }

    Ok(())
}

fn format_abi_value(graph: Option<&mitki_abi::SemanticTypeGraph>, value: &AbiValue) -> String {
    match value {
        AbiValue::Immediate(scalar) => format_abi_scalar(graph, scalar),
        AbiValue::Handle { type_id, handle_id } => format!("<handle {}:{}>", type_id.0, handle_id),
        AbiValue::Canonical { transport_type, graph: canonical } => {
            format_value_ref(graph, canonical, *transport_type, &canonical.root)
        }
    }
}

fn format_abi_scalar(graph: Option<&mitki_abi::SemanticTypeGraph>, scalar: &AbiScalar) -> String {
    match scalar {
        AbiScalar::Unit => "()".to_owned(),
        AbiScalar::Bool(value) => value.to_string(),
        AbiScalar::Int { value, .. } => value.to_string(),
        AbiScalar::Float { raw_bits, .. } => f64::from_bits(*raw_bits).to_string(),
        AbiScalar::Char { unicode_scalar } => char::from_u32(*unicode_scalar)
            .map_or_else(|| format!("char({unicode_scalar})"), |value| value.to_string()),
        AbiScalar::EnumTag { type_id, variant_index } => {
            if let Some(graph) = graph
                && let Some(node) = graph.types.get(type_id.0 as usize)
                && let mitki_abi::AbiTypeKind::Enum { variants, .. } = &node.kind
                && let Some(variant) = variants.get(*variant_index as usize)
                && let Some(name_id) = graph.variant_names.get(variant.name.0 as usize)
                && let Some(name) = graph.strings.get(name_id.0 as usize)
            {
                name.clone()
            } else {
                format!("enum_tag({}, {})", type_id.0, variant_index)
            }
        }
    }
}

fn format_value_ref(
    graph: Option<&mitki_abi::SemanticTypeGraph>,
    canonical: &CanonicalGraph,
    transport_type: mitki_abi::TypeId,
    value: &ValueRef,
) -> String {
    match value {
        ValueRef::InlineScalar(scalar) => format_abi_scalar(graph, scalar),
        ValueRef::HandleRef(slot) => canonical.handles.get(slot.0 as usize).map_or_else(
            || format!("<handle-slot {}>", slot.0),
            |handle| format!("<handle {}:{}>", handle.type_id.0, handle.handle_id),
        ),
        ValueRef::NodeRef(node_id) => canonical.nodes.get(node_id.0 as usize).map_or_else(
            || format!("<node {}>", node_id.0),
            |node| format_node(graph, canonical, transport_type, node),
        ),
    }
}

fn format_node(
    graph: Option<&mitki_abi::SemanticTypeGraph>,
    canonical: &CanonicalGraph,
    transport_type: mitki_abi::TypeId,
    node: &CanonicalNode,
) -> String {
    match node {
        CanonicalNode::String { value, .. } => value.clone(),
        CanonicalNode::Array { elements, .. } => match elements {
            ArrayElements::Values(values) => format!(
                "[{}]",
                values
                    .iter()
                    .map(|value| format_value_ref(graph, canonical, transport_type, value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            ArrayElements::PackedScalars { kind, len, bytes } => {
                format!("[{}]", format_packed_scalars(*kind, *len, bytes).join(", "))
            }
        },
        CanonicalNode::Tuple { fields, .. } => format!(
            "({})",
            fields
                .iter()
                .map(|value| format_value_ref(graph, canonical, transport_type, value))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        CanonicalNode::Record { fields, .. } => format!(
            "{{ {} }}",
            fields
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    let name = field_name(graph, transport_type, index)
                        .unwrap_or_else(|| format!("field{index}"));
                    format!("{name}: {}", format_value_ref(graph, canonical, transport_type, value))
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
        CanonicalNode::Struct { fields, .. } => format!(
            "{{ {} }}",
            fields
                .iter()
                .enumerate()
                .map(|(index, value)| {
                    let name = field_name(graph, transport_type, index)
                        .unwrap_or_else(|| format!("field{index}"));
                    format!("{name}: {}", format_value_ref(graph, canonical, transport_type, value))
                })
                .collect::<Vec<_>>()
                .join(", ")
        ),
        CanonicalNode::Enum { variant_index, fields, .. } => {
            let variant_name = enum_variant_name(graph, transport_type, *variant_index)
                .unwrap_or_else(|| format!("variant{variant_index}"));
            if fields.is_empty() {
                variant_name
            } else {
                format!(
                    "{variant_name}({})",
                    fields
                        .iter()
                        .map(|value| format_value_ref(graph, canonical, transport_type, value))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        CanonicalNode::Union { arm_index, payload, .. } => format!(
            "union#{arm_index}({})",
            format_value_ref(graph, canonical, transport_type, payload)
        ),
        CanonicalNode::Intersection { carrier, facets, .. } => {
            let carrier_text = format_value_ref(graph, canonical, transport_type, carrier);
            if facets.is_empty() {
                format!("intersection({carrier_text})")
            } else {
                format!(
                    "intersection({}, [{}])",
                    carrier_text,
                    facets
                        .iter()
                        .map(|facet| format_value_ref(graph, canonical, transport_type, facet))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

fn format_packed_scalars(kind: PackedScalarKind, len: u32, bytes: &[u8]) -> Vec<String> {
    let len = usize::try_from(len).expect("packed scalar array length should fit in usize");
    match kind {
        PackedScalarKind::Bool => bytes
            .iter()
            .take(len)
            .map(|byte| if *byte == 0 { "false".to_owned() } else { "true".to_owned() })
            .collect(),
        PackedScalarKind::I32 => bytes
            .chunks_exact(4)
            .take(len)
            .map(|chunk| {
                i32::from_le_bytes(chunk.try_into().expect("packed i32 chunk")).to_string()
            })
            .collect(),
        PackedScalarKind::I64 => bytes
            .chunks_exact(8)
            .take(len)
            .map(|chunk| {
                i64::from_le_bytes(chunk.try_into().expect("packed i64 chunk")).to_string()
            })
            .collect(),
        PackedScalarKind::F32 => bytes
            .chunks_exact(4)
            .take(len)
            .map(|chunk| {
                f32::from_bits(u32::from_le_bytes(chunk.try_into().expect("packed f32 chunk")))
                    .to_string()
            })
            .collect(),
        PackedScalarKind::F64 => bytes
            .chunks_exact(8)
            .take(len)
            .map(|chunk| {
                f64::from_bits(u64::from_le_bytes(chunk.try_into().expect("packed f64 chunk")))
                    .to_string()
            })
            .collect(),
        PackedScalarKind::Char => bytes
            .chunks_exact(4)
            .take(len)
            .map(|chunk| {
                let scalar = u32::from_le_bytes(chunk.try_into().expect("packed char chunk"));
                char::from_u32(scalar)
                    .map_or_else(|| format!("\\u{{{scalar:x}}}"), |value| value.to_string())
            })
            .collect(),
    }
}

fn field_name(
    graph: Option<&mitki_abi::SemanticTypeGraph>,
    transport_type: mitki_abi::TypeId,
    index: usize,
) -> Option<String> {
    let graph = graph?;
    let node = graph.types.get(transport_type.0 as usize)?;
    let field = match &node.kind {
        mitki_abi::AbiTypeKind::Record { fields }
        | mitki_abi::AbiTypeKind::Struct { fields, .. } => fields.get(index)?,
        _ => return None,
    };
    let string_id = *graph.field_names.get(field.name.0 as usize)?;
    graph.strings.get(string_id.0 as usize).cloned()
}

fn enum_variant_name(
    graph: Option<&mitki_abi::SemanticTypeGraph>,
    transport_type: mitki_abi::TypeId,
    variant_index: u32,
) -> Option<String> {
    let graph = graph?;
    let node = graph.types.get(transport_type.0 as usize)?;
    let variant = match &node.kind {
        mitki_abi::AbiTypeKind::Enum { variants, .. } => variants.get(variant_index as usize)?,
        _ => return None,
    };
    let string_id = *graph.variant_names.get(variant.name.0 as usize)?;
    graph.strings.get(string_id.0 as usize).cloned()
}

#[cfg(test)]
mod tests {
    use mitki_hir::ty::TyKind;
    use mitki_inputs::File;

    use super::resolve_run_wasm_main;

    #[test]
    fn run_wasm_main_prefers_declared_return_type() {
        let db = mitki_db::RootDatabase::default();
        let file = File::new(
            &db,
            "declared_main_return.mitki".into(),
            r#"
fun main(): int | bool {
    42
}
"#
            .to_owned(),
        );

        let ty = resolve_run_wasm_main(&db, file).expect("main should resolve");
        let TyKind::Union(items) = ty.kind(&db) else {
            panic!("expected declared return type to stay a union, found `{}`", ty.display(&db));
        };
        assert!(items.iter().any(|item| matches!(item.kind(&db), TyKind::Int)));
        assert!(items.iter().any(|item| matches!(item.kind(&db), TyKind::Bool)));
    }

    #[test]
    fn run_wasm_main_preserves_unannotated_unit_return_type() {
        let db = mitki_db::RootDatabase::default();
        let file = File::new(
            &db,
            "unit_main_return.mitki".into(),
            r#"
fun main() {
}
"#
            .to_owned(),
        );

        let ty = resolve_run_wasm_main(&db, file).expect("main should resolve");
        assert!(matches!(ty.kind(&db), TyKind::Tuple(items) if items.is_empty()));
    }
}

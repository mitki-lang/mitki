#![allow(dead_code, unused_imports)]

use std::borrow::Cow;
use std::sync::{Arc, Mutex};

use mitki_abi::{
    AbiScalar, AbiValue, ArrayElements, CanonicalGraph, CanonicalNode, ExecutionDomain,
    LinkageKind, METADATA_ENCODING_VERSION, REQUIRED_FEATURE_HANDLES, SemanticTypeGraph, ValueRef,
    decode_semantic_type_graph, encode_semantic_type_graph, validate_wasm_module_contract,
};
use mitki_backend_wasm::CompileOptions;
use mitki_comptime_wasm::{compile_file_to_wasm, compile_file_to_wasm_with_options};
use mitki_db::RootDatabase;
use mitki_inputs::File;
use mitki_wasm_runtime::{
    RunConfig, WasmRuntime, collect_unsupported_imports_with_config, describe_module_abi,
};

pub(crate) fn compile_ok(fixture: &str) -> Vec<u8> {
    let db = RootDatabase::default();
    let file = File::new(&db, "wasm_backend_ok.mitki".into(), fixture.to_owned());
    let bytes = compile_file_to_wasm(&db, file).unwrap_or_else(|diagnostics| {
        let messages = diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>();
        panic!("expected success, got diagnostics: {messages:#?}");
    });
    wasmparser::Validator::new().validate_all(&bytes).expect("emitted Wasm should validate");
    bytes
}

pub(crate) fn compile_ok_with_options(fixture: &str, options: CompileOptions) -> Vec<u8> {
    let db = RootDatabase::default();
    let file = File::new(&db, "wasm_backend_ok_opts.mitki".into(), fixture.to_owned());
    let bytes =
        compile_file_to_wasm_with_options(&db, file, options).unwrap_or_else(|diagnostics| {
            let messages =
                diagnostics.iter().map(|diag| diag.message().to_owned()).collect::<Vec<_>>();
            panic!("expected success, got diagnostics: {messages:#?}");
        });
    wasmparser::Validator::new().validate_all(&bytes).expect("emitted Wasm should validate");
    bytes
}

pub(crate) fn compile_err(fixture: &str) -> Vec<String> {
    let db = RootDatabase::default();
    let file = File::new(&db, "wasm_backend_err.mitki".into(), fixture.to_owned());
    let diagnostics = compile_file_to_wasm(&db, file).expect_err("expected compilation failure");
    diagnostics.iter().map(|diag| diag.message().to_owned()).collect()
}

pub(crate) fn normalized_wat(bytes: &[u8]) -> String {
    wasmprinter::print_bytes(bytes)
        .expect("WAT should print")
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

pub(crate) fn bump_module_abi_version(bytes: &[u8], version: u32) -> Vec<u8> {
    let mut module = wasm_encoder::Module::new();
    let mut rewritten = false;

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload.expect("valid wasm payload");
        match payload {
            wasmparser::Payload::CustomSection(reader) if reader.name() == "mitki.abi.v2" => {
                let mut graph =
                    decode_semantic_type_graph(reader.data()).expect("valid ABI metadata");
                graph.encoding_version =
                    u16::try_from(version).expect("test metadata version should fit in u16");
                let encoded = encode_semantic_type_graph(&graph).expect("encoded ABI metadata");
                module.section(&wasm_encoder::CustomSection {
                    name: reader.name().into(),
                    data: Cow::Owned(encoded),
                });
                rewritten = true;
            }
            other => {
                if let Some((id, range)) = other.as_section() {
                    module.section(&wasm_encoder::RawSection { id, data: &bytes[range] });
                }
            }
        }
    }

    assert!(rewritten, "expected ABI metadata section to exist");
    module.finish()
}

pub(crate) fn string_value(graph: &SemanticTypeGraph, id: mitki_abi::StringId) -> &str {
    graph.strings.get(id.0 as usize).map(String::as_str).expect("valid string id")
}

pub(crate) fn symbol_name(graph: &SemanticTypeGraph, id: mitki_abi::SymbolId) -> &str {
    let string_id = graph.nominal_symbols.get(id.0 as usize).copied().expect("valid symbol id");
    string_value(graph, string_id)
}

pub(crate) fn export_instance<'a>(
    graph: &'a SemanticTypeGraph,
    logical_name: &str,
) -> &'a mitki_abi::FunctionInstance {
    graph
        .function_instances
        .iter()
        .find(|instance| {
            instance.linkage == LinkageKind::WasmExport
                && symbol_name(graph, instance.logical_symbol) == logical_name
        })
        .expect("expected export instance")
}

pub(crate) fn import_instance<'a>(
    graph: &'a SemanticTypeGraph,
    module_name: &str,
    logical_name: &str,
) -> &'a mitki_abi::FunctionInstance {
    graph
        .function_instances
        .iter()
        .find(|instance| {
            instance.linkage == LinkageKind::WasmImport
                && instance
                    .wasm_module_name
                    .is_some_and(|id| string_value(graph, id) == module_name)
                && symbol_name(graph, instance.logical_symbol) == logical_name
        })
        .expect("expected import instance")
}

pub(crate) fn expect_diagnostic(messages: &[String], needle: &str) {
    assert!(
        messages.iter().any(|message| message.contains(needle)),
        "expected diagnostic containing `{needle}`, got {messages:#?}"
    );
}

pub(crate) fn assert_func_signature(
    extern_ty: &wasmtime::ExternType,
    params: &[wasmtime::ValType],
    results: &[wasmtime::ValType],
) {
    let wasmtime::ExternType::Func(func_ty) = extern_ty else {
        panic!("expected function extern type, got {extern_ty:?}");
    };
    let actual_params = func_ty.params().map(|ty| val_type_name(&ty)).collect::<Vec<_>>();
    let expected_params = params.iter().map(val_type_name).collect::<Vec<_>>();
    let actual_results = func_ty.results().map(|ty| val_type_name(&ty)).collect::<Vec<_>>();
    let expected_results = results.iter().map(val_type_name).collect::<Vec<_>>();
    assert_eq!(actual_params, expected_params);
    assert_eq!(actual_results, expected_results);
}

fn val_type_name(ty: &wasmtime::ValType) -> &'static str {
    match ty {
        wasmtime::ValType::I32 => "i32",
        wasmtime::ValType::I64 => "i64",
        wasmtime::ValType::F32 => "f32",
        wasmtime::ValType::F64 => "f64",
        wasmtime::ValType::V128 => "v128",
        wasmtime::ValType::Ref(_) => "ref",
    }
}

pub(crate) fn expect_immediate_i32(value: &AbiValue, expected: i64) {
    assert_eq!(
        value,
        &AbiValue::Immediate(AbiScalar::Int { signed: true, bits: 32, value: expected })
    );
}

pub(crate) fn decode_string_array(value: &AbiValue) -> Vec<String> {
    let AbiValue::Canonical { graph, .. } = value else {
        panic!("expected canonical value, got {value:?}");
    };
    decode_string_array_ref(graph, &graph.root)
}

pub(crate) fn decode_string(value: &AbiValue) -> String {
    let AbiValue::Canonical { graph, .. } = value else {
        panic!("expected canonical value, got {value:?}");
    };
    let ValueRef::NodeRef(root) = graph.root else {
        panic!("expected node root, got {:?}", graph.root);
    };
    let CanonicalNode::String { value, .. } =
        graph.nodes.get(root.0 as usize).expect("string root node")
    else {
        panic!("expected canonical string");
    };
    value.clone()
}

pub(crate) fn decode_string_array_ref(graph: &CanonicalGraph, root: &ValueRef) -> Vec<String> {
    assert!(graph.handles.is_empty(), "baseline canonical array should not carry handles");
    let ValueRef::NodeRef(root) = root else {
        panic!("expected node root, got {root:?}");
    };
    let CanonicalNode::Array { elements: ArrayElements::Values(items), .. } =
        graph.nodes.get(root.0 as usize).expect("array root node")
    else {
        panic!("expected canonical string array");
    };

    items
        .iter()
        .map(|value_ref| {
            let ValueRef::NodeRef(node_id) = value_ref else {
                panic!("expected string node ref, got {value_ref:?}");
            };
            let CanonicalNode::String { value, .. } =
                graph.nodes.get(node_id.0 as usize).expect("string node")
            else {
                panic!("expected canonical string node");
            };
            value.clone()
        })
        .collect()
}

pub(crate) fn decode_i32_array(value: &AbiValue) -> Vec<i32> {
    let AbiValue::Canonical { graph, .. } = value else {
        panic!("expected canonical value, got {value:?}");
    };
    assert!(graph.handles.is_empty(), "baseline canonical array should not carry handles");
    let ValueRef::NodeRef(root) = graph.root else {
        panic!("expected node root, got {:?}", graph.root);
    };
    let CanonicalNode::Array { elements, .. } =
        graph.nodes.get(root.0 as usize).expect("array root node")
    else {
        panic!("expected canonical array");
    };
    match elements {
        ArrayElements::PackedScalars { bytes, len, .. } => {
            let expected_len = usize::try_from(*len).expect("len fits usize");
            bytes
                .chunks_exact(4)
                .map(|chunk| i32::from_le_bytes(chunk.try_into().expect("packed i32 chunk")))
                .take(expected_len)
                .collect()
        }
        ArrayElements::Values(values) => values
            .iter()
            .map(|value| match value {
                ValueRef::InlineScalar(AbiScalar::Int { value, .. }) => *value as i32,
                other => panic!("expected inline i32 scalar, got {other:?}"),
            })
            .collect(),
    }
}

pub(crate) fn decode_i32_pair(value: &AbiValue) -> [i32; 2] {
    let AbiValue::Canonical { graph, .. } = value else {
        panic!("expected canonical value, got {value:?}");
    };
    let ValueRef::NodeRef(root) = graph.root else {
        panic!("expected node root, got {:?}", graph.root);
    };
    let CanonicalNode::Tuple { fields, .. } =
        graph.nodes.get(root.0 as usize).expect("tuple root node")
    else {
        panic!("expected canonical tuple");
    };
    assert_eq!(fields.len(), 2, "expected 2-tuple");
    let mut values = [0; 2];
    for (slot, field) in values.iter_mut().zip(fields.iter()) {
        *slot = match field {
            ValueRef::InlineScalar(AbiScalar::Int { value, .. }) => *value as i32,
            other => panic!("expected inline i32 scalar, got {other:?}"),
        };
    }
    values
}

pub(crate) fn decode_some_string_array(value: &AbiValue) -> Vec<String> {
    let AbiValue::Canonical { graph, .. } = value else {
        panic!("expected canonical value, got {value:?}");
    };
    let ValueRef::NodeRef(root) = graph.root else {
        panic!("expected node root, got {:?}", graph.root);
    };
    let CanonicalNode::Enum { variant_index, fields, .. } =
        graph.nodes.get(root.0 as usize).expect("enum root node")
    else {
        panic!("expected canonical enum");
    };
    assert_eq!(*variant_index, 0, "expected first enum variant");
    assert_eq!(fields.len(), 1, "expected single enum payload");
    decode_string_array_ref(graph, &fields[0])
}

pub(crate) fn compile_runtime_with_host_callback(
    bytes: &[u8],
    seen: &Arc<Mutex<Option<AbiValue>>>,
) -> WasmRuntime {
    let seen_clone = Arc::clone(seen);
    WasmRuntime::builder()
        .host_function("env", "round_trip", move |args| {
            assert_eq!(args.len(), 1, "expected one canonical array argument");
            *seen_clone.lock().expect("host arg lock") = Some(args[0].clone());
            Ok(Some(args[0].clone()))
        })
        .instantiate(bytes)
        .expect("runtime should instantiate with typed host callback")
}

pub(crate) fn assert_shared_abi_contract(bytes: &[u8]) {
    let abi = describe_module_abi(bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    validate_wasm_module_contract(bytes, metadata)
        .expect("emitted Wasm should agree with the shared ABI contract");
}

pub(crate) fn assert_metadata_requires_handles(bytes: &[u8]) {
    let abi = describe_module_abi(bytes).expect("ABI metadata should decode");
    let metadata = abi.metadata.as_ref().expect("expected ABI v2 metadata");
    assert_ne!(metadata.required_features & REQUIRED_FEATURE_HANDLES, 0);
}

pub(crate) fn assert_metadata_version_bump_rejected(bytes: &[u8]) {
    let bumped = bump_module_abi_version(bytes, u32::from(METADATA_ENCODING_VERSION) + 1);
    let error = describe_module_abi(&bumped).expect_err("newer metadata encoding should reject");
    assert!(error.to_string().contains("metadata encoding version"));
}

pub(crate) fn unsupported_imports(bytes: &[u8]) -> Vec<String> {
    collect_unsupported_imports_with_config(bytes, RunConfig::without_wasi())
        .expect("unsupported import collection should succeed")
}

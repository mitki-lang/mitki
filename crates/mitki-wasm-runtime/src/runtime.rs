use std::collections::BTreeMap;

use anyhow::{Context as _, anyhow, bail};
use mitki_abi::{
    AbiValue, FunctionSignature as AbiFunctionSignature, InstanceId, LinkageKind,
    SemanticTypeGraph, TransportClass, decode_canonical_blob, encode_canonical_blob,
    find_import_instance_by_linkage, string_value as graph_string_value,
    symbol_name as graph_symbol_name, transport_carrier_type, transport_has_wasm_lane,
};
use wasmtime::{Caller, Extern, Linker};
use wasmtime_wasi::{DirPerms, FilePerms, WasiCtxBuilder, p1};

const WASM_PAGE_SIZE: u64 = 65_536;

#[cfg(test)]
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum WasmValue {
    Unit,
    I32(i32),
    Bool(bool),
    F64(f64),
    Char(char),
    String(String),
    Array(Vec<WasmValue>),
    Tuple(Vec<WasmValue>),
    Record(Vec<(String, WasmValue)>),
    Struct(Vec<(String, WasmValue)>),
    Enum { variant: String, fields: Vec<WasmValue> },
    Union { arm_index: u32, value: Box<WasmValue> },
    Intersection { carrier: Box<WasmValue>, facets: Vec<WasmValue> },
    Handle { type_id: u32, handle_id: u32 },
}

type HostImportHandler = Box<dyn FnMut(&[AbiValue]) -> anyhow::Result<Option<AbiValue>> + Send>;

#[derive(Default)]
struct HostHandlers {
    named: BTreeMap<String, BTreeMap<String, HostImportHandler>>,
    by_instance: BTreeMap<u32, HostImportHandler>,
}

enum HostHandlerKey {
    Instance(u32),
    Named { module: String, name: String },
}

impl HostHandlers {
    fn is_empty(&self) -> bool {
        self.named.is_empty() && self.by_instance.is_empty()
    }

    fn insert_named(&mut self, module: String, name: String, handler: HostImportHandler) {
        self.named.entry(module).or_default().insert(name, handler);
    }

    fn insert_instance(&mut self, instance_id: InstanceId, handler: HostImportHandler) {
        self.by_instance.insert(instance_id.0, handler);
    }

    fn has_named(&self, module: &str, name: &str) -> bool {
        self.named.get(module).is_some_and(|module_handlers| module_handlers.contains_key(name))
    }

    fn has_instance(&self, instance_id: InstanceId) -> bool {
        self.by_instance.contains_key(&instance_id.0)
    }

    fn get_mut(&mut self, key: &HostHandlerKey) -> Option<&mut HostImportHandler> {
        match key {
            HostHandlerKey::Instance(id) => self.by_instance.get_mut(id),
            HostHandlerKey::Named { module, name } => {
                self.named.get_mut(module).and_then(|module_handlers| module_handlers.get_mut(name))
            }
        }
    }
}

/// The captured stdout and optional decoded result from a Wasm export
/// invocation.
#[derive(Clone, Debug, PartialEq)]
pub struct WasmRunOutput {
    pub stdout: String,
    pub result: Option<AbiValue>,
}

/// Configuration for builtin imports wired by the Wasm runtime harness.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RunConfig {
    pub enable_wasi: bool,
    pub enable_mitki_runtime: bool,
}

impl RunConfig {
    pub fn wasi_default() -> Self {
        Self { enable_wasi: true, enable_mitki_runtime: true }
    }

    pub fn without_wasi() -> Self {
        Self { enable_wasi: false, enable_mitki_runtime: true }
    }
}

impl Default for RunConfig {
    fn default() -> Self {
        Self::wasi_default()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct ActiveAllocation {
    requested_size: u64,
    reserved_size: u64,
    align: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct FreeRegion {
    start: u64,
    len: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AllocSource {
    Fresh { end: u64 },
    Free { index: usize, region: FreeRegion, alloc_start: u64, alloc_end: u64 },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct AllocationPlan {
    ptr: u64,
    requested_size: u64,
    reserved_size: u64,
    align: u64,
    source: AllocSource,
}

impl AllocationPlan {
    fn required_end(self) -> u64 {
        match self.source {
            AllocSource::Fresh { end } => end,
            AllocSource::Free { alloc_end, .. } => alloc_end,
        }
    }
}

#[derive(Default)]
struct GuestAllocator {
    frontier: Option<u64>,
    active: BTreeMap<u64, ActiveAllocation>,
    free_regions: Vec<FreeRegion>,
}

impl GuestAllocator {
    fn plan_alloc(
        &self,
        current_limit: u64,
        size: u64,
        align: u64,
    ) -> anyhow::Result<AllocationPlan> {
        if align == 0 || !align.is_power_of_two() {
            anyhow::bail!("runtime alloc requires a positive power-of-two alignment");
        }

        let reserved_size = size.max(1);
        for (index, region) in self.free_regions.iter().copied().enumerate() {
            let ptr = align_to(region.start, align);
            let end = ptr
                .checked_add(reserved_size)
                .ok_or_else(|| anyhow!("runtime alloc overflowed the address space"))?;
            let region_end = region
                .start
                .checked_add(region.len)
                .ok_or_else(|| anyhow!("runtime free-list metadata overflowed"))?;
            if end <= region_end {
                return Ok(AllocationPlan {
                    ptr,
                    requested_size: size,
                    reserved_size,
                    align,
                    source: AllocSource::Free { index, region, alloc_start: ptr, alloc_end: end },
                });
            }
        }

        let frontier = self.frontier.unwrap_or(current_limit);
        let ptr = align_to(frontier, align);
        let end =
            ptr.checked_add(reserved_size).ok_or_else(|| anyhow!("runtime alloc overflowed"))?;
        Ok(AllocationPlan {
            ptr,
            requested_size: size,
            reserved_size,
            align,
            source: AllocSource::Fresh { end },
        })
    }

    fn commit_alloc(&mut self, plan: AllocationPlan) -> anyhow::Result<i32> {
        match plan.source {
            AllocSource::Fresh { end } => {
                self.frontier = Some(end);
            }
            AllocSource::Free { index, region, alloc_start, alloc_end } => {
                let existing = self.free_regions.get(index).copied().ok_or_else(|| {
                    anyhow!("runtime allocator plan referenced a missing free slot")
                })?;
                if existing != region {
                    anyhow::bail!("runtime allocator plan was invalidated before commit");
                }

                self.free_regions.remove(index);
                if alloc_start > region.start {
                    self.free_regions.insert(
                        index,
                        FreeRegion { start: region.start, len: alloc_start - region.start },
                    );
                }
                if alloc_end < region.start + region.len {
                    let suffix =
                        FreeRegion { start: alloc_end, len: region.start + region.len - alloc_end };
                    let insert_at = if alloc_start > region.start { index + 1 } else { index };
                    self.free_regions.insert(insert_at, suffix);
                }
            }
        }

        if self
            .active
            .insert(
                plan.ptr,
                ActiveAllocation {
                    requested_size: plan.requested_size,
                    reserved_size: plan.reserved_size,
                    align: plan.align,
                },
            )
            .is_some()
        {
            anyhow::bail!("runtime allocator produced a duplicate live pointer");
        }

        i32::try_from(plan.ptr)
            .map_err(|_error| anyhow!("runtime alloc exceeded i32 address space"))
    }

    fn dealloc(&mut self, ptr: i32, size: i32, align: i32) -> anyhow::Result<()> {
        let ptr = u64::try_from(ptr)
            .map_err(|_error| anyhow!("runtime dealloc requires a non-negative pointer"))?;
        let requested_size = u64::try_from(size)
            .map_err(|_error| anyhow!("runtime dealloc requires a non-negative size"))?;
        let align = u64::try_from(align).map_err(|_error| {
            anyhow!("runtime dealloc requires a positive power-of-two alignment")
        })?;
        if align == 0 || !align.is_power_of_two() {
            anyhow::bail!("runtime dealloc requires a positive power-of-two alignment");
        }

        let allocation = self
            .active
            .get(&ptr)
            .copied()
            .ok_or_else(|| anyhow!("runtime dealloc received an unknown pointer"))?;
        if allocation.requested_size != requested_size || allocation.align != align {
            anyhow::bail!("runtime dealloc did not match the original allocation");
        }
        self.active.remove(&ptr);
        self.insert_free_region(ptr, allocation.reserved_size)
    }

    fn insert_free_region(&mut self, start: u64, len: u64) -> anyhow::Result<()> {
        if len == 0 {
            return Ok(());
        }

        let mut index = self.free_regions.partition_point(|region| region.start < start);
        let mut merged_start = start;
        let mut merged_end =
            start.checked_add(len).ok_or_else(|| anyhow!("runtime free region overflowed"))?;

        if index > 0 {
            let prev = self.free_regions[index - 1];
            let prev_end = prev
                .start
                .checked_add(prev.len)
                .ok_or_else(|| anyhow!("runtime free-list metadata overflowed"))?;
            if prev_end > merged_start {
                anyhow::bail!("runtime allocator detected overlapping free regions");
            }
            if prev_end == merged_start {
                merged_start = prev.start;
                self.free_regions.remove(index - 1);
                index -= 1;
            }
        }

        while index < self.free_regions.len() {
            let next = self.free_regions[index];
            if next.start < merged_end {
                anyhow::bail!("runtime allocator detected overlapping free regions");
            }
            if next.start != merged_end {
                break;
            }
            merged_end = next
                .start
                .checked_add(next.len)
                .ok_or_else(|| anyhow!("runtime free-list metadata overflowed"))?;
            self.free_regions.remove(index);
        }

        self.free_regions
            .insert(index, FreeRegion { start: merged_start, len: merged_end - merged_start });
        Ok(())
    }
}

/// A raw Wasm function type exposed by a module import or export.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WasmFunctionAbi {
    pub params: Vec<String>,
    pub results: Vec<String>,
}

/// The kind of Wasm extern exported from or imported into a module.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum WasmExternAbiKind {
    Function(WasmFunctionAbi),
    Table,
    Memory,
    Global,
    Tag,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WasmImportAbi {
    pub module: String,
    pub name: String,
    pub kind: WasmExternAbiKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WasmExportAbi {
    pub name: String,
    pub kind: WasmExternAbiKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WasmModuleAbi {
    pub metadata: Option<SemanticTypeGraph>,
    pub imports: Vec<WasmImportAbi>,
    pub exports: Vec<WasmExportAbi>,
}

/// Builder for instantiating a module with optional typed custom host imports.
#[derive(Default)]
pub struct WasmRuntimeBuilder {
    config: RunConfig,
    host_handlers: HostHandlers,
}

/// A reusable Wasm runtime instance that can inspect ABI metadata and invoke
/// typed exports.
///
/// ```ignore
/// use mitki_abi::{AbiScalar, AbiValue};
/// use mitki_codegen_wasm::{WasmRuntime, compile_file_to_wasm, describe_module_abi};
/// use mitki_db::RootDatabase;
/// use mitki_inputs::File;
///
/// # fn main() -> anyhow::Result<()> {
/// let db = RootDatabase::default();
/// let file = File::new(
///     &db,
///     "example.mitki".into(),
///     r#"
/// struct Point {
///     x: int,
///     y: int,
/// }
///
/// import "env" fun host_point(): Point;
///
/// export fun sum_host_point(): int {
///     val point = host_point();
///     point.x + point.y
/// }
/// "#
///     .to_owned(),
/// );
///
/// let bytes = compile_file_to_wasm(&db, file).expect("expected Wasm compilation to succeed");
/// let abi = describe_module_abi(&bytes)?;
/// assert!(abi.metadata.is_some());
/// assert!(abi.imports.iter().any(|import| import.module == "env" && import.name == "host_point"));
///
/// let mut runtime = WasmRuntime::builder()
///     .host_function("env", "host_point", |_args| {
///         Ok(Some(AbiValue::Canonical {
///             transport_type: mitki_abi::TypeId(1),
///             graph: mitki_abi::CanonicalGraph {
///                 root: mitki_abi::ValueRef::NodeRef(mitki_abi::NodeId(0)),
///                 nodes: vec![mitki_abi::CanonicalNode::Struct {
///                     transport_type: mitki_abi::TypeId(1),
///                     fields: vec![
///                         mitki_abi::ValueRef::InlineScalar(AbiScalar::Int { signed: true, bits: 32, value: 20 }),
///                         mitki_abi::ValueRef::InlineScalar(AbiScalar::Int { signed: true, bits: 32, value: 22 }),
///                     ],
///                 }],
///                 handles: Vec::new(),
///             },
///         }))
///     })
///     .instantiate(&bytes)?;
/// let output = runtime.invoke_export("sum_host_point", &[])?;
/// assert_eq!(
///     output.result,
///     Some(AbiValue::Immediate(AbiScalar::Int { signed: true, bits: 32, value: 42 }))
/// );
/// # Ok(())
/// # }
/// ```
pub struct WasmRuntime {
    store: wasmtime::Store<RuntimeStore>,
    instance: wasmtime::Instance,
    memory: Option<wasmtime::Memory>,
    abi: WasmModuleAbi,
    pub(crate) abi_v2: crate::runtime_v2::MitkiModuleAbiV2,
}

impl WasmRuntimeBuilder {
    /// Overrides the builtin runtime configuration used during instantiation.
    pub fn config(mut self, config: RunConfig) -> Self {
        self.config = config;
        self
    }

    /// Registers a typed host handler for a Mitki `import "module" fun
    /// name(...)`.
    pub fn host_function<F>(
        mut self,
        module: impl Into<String>,
        name: impl Into<String>,
        handler: F,
    ) -> Self
    where
        F: FnMut(&[AbiValue]) -> anyhow::Result<Option<AbiValue>> + Send + 'static,
    {
        insert_host_handler(&mut self.host_handlers, module.into(), name.into(), Box::new(handler));
        self
    }

    /// Registers a typed host handler for one concrete ABI v2 import instance.
    pub fn host_function_instance<F>(mut self, instance_id: InstanceId, handler: F) -> Self
    where
        F: FnMut(&[AbiValue]) -> anyhow::Result<Option<AbiValue>> + Send + 'static,
    {
        insert_host_instance_handler(&mut self.host_handlers, instance_id, Box::new(handler));
        self
    }

    /// Instantiates a Wasm runtime using the configured builtin and typed host
    /// imports.
    pub fn instantiate(self, bytes: &[u8]) -> anyhow::Result<WasmRuntime> {
        WasmRuntime::instantiate(bytes, self.config, self.host_handlers)
    }
}

impl WasmRuntime {
    /// Creates a runtime builder for modules that need typed custom host
    /// imports.
    pub fn builder() -> WasmRuntimeBuilder {
        WasmRuntimeBuilder::default()
    }

    /// Instantiates a module using only builtin `mitki` imports and optional
    /// WASI support.
    pub fn new(bytes: &[u8], config: RunConfig) -> anyhow::Result<Self> {
        Self::instantiate(bytes, config, HostHandlers::default())
    }

    fn instantiate(
        bytes: &[u8],
        config: RunConfig,
        host_handlers: HostHandlers,
    ) -> anyhow::Result<Self> {
        let abi = describe_module_abi(bytes)?;
        ensure_supported_imports(&abi, config, &host_handlers)?;
        let engine = wasmtime::Engine::default();
        let module = wasmtime::Module::from_binary(&engine, bytes)?;
        let mut linker = Linker::new(&engine);
        if config.enable_wasi {
            p1::add_to_linker_sync(&mut linker, |state: &mut RuntimeStore| &mut state.wasi)?;
        }
        if config.enable_mitki_runtime {
            add_runtime_imports(&mut linker)?;
        }
        add_builtin_typed_import_aliases(&mut linker, &module, abi.metadata.as_ref(), config)?;
        add_typed_host_imports(&mut linker, &module, abi.metadata.as_ref(), &host_handlers)?;

        let mut store = wasmtime::Store::new(
            &engine,
            RuntimeStore {
                wasi: build_wasi_context(config)?,
                stdout: String::new(),
                allocator: GuestAllocator::default(),
                host_handlers,
            },
        );
        let instance = linker.instantiate(&mut store, &module)?;
        let memory = instance.get_memory(&mut store, "memory");
        let abi_v2 = crate::runtime_v2::MitkiModuleAbiV2 { metadata: abi.metadata.clone() };
        Ok(Self { store, instance, memory, abi, abi_v2 })
    }

    /// Returns the parsed Wasm import/export surface for this runtime's module.
    pub fn abi(&self) -> &WasmModuleAbi {
        &self.abi
    }

    pub fn invoke_export(
        &mut self,
        export: &str,
        args: &[AbiValue],
    ) -> anyhow::Result<WasmRunOutput> {
        let output = self.invoke_export_v2(export, args)?;
        Ok(WasmRunOutput { stdout: output.stdout, result: output.result })
    }

    /// Invokes one concrete ABI v2 export instance by its metadata instance id.
    pub fn invoke_export_instance(
        &mut self,
        instance_id: InstanceId,
        args: &[AbiValue],
    ) -> anyhow::Result<WasmRunOutput> {
        let output = self.invoke_export_instance_v2(instance_id, args)?;
        Ok(WasmRunOutput { stdout: output.stdout, result: output.result })
    }

    /// Invokes a raw exported function with no parameters and no typed result.
    pub fn invoke_raw_export(&mut self, export: &str) -> anyhow::Result<WasmRunOutput> {
        self.clear_stdout();
        self.call_dynamic_export(export, &[], &mut [])?;
        Ok(WasmRunOutput { stdout: self.take_stdout(), result: None })
    }

    pub(crate) fn clear_stdout(&mut self) {
        self.store.data_mut().stdout.clear();
    }

    pub(crate) fn take_stdout(&mut self) -> String {
        std::mem::take(&mut self.store.data_mut().stdout)
    }

    pub(crate) fn exported_memory(&self) -> anyhow::Result<wasmtime::Memory> {
        self.memory
            .ok_or_else(|| anyhow!("typed invocation requires an exported memory named `memory`"))
    }

    pub(crate) fn call_dynamic_export(
        &mut self,
        export: &str,
        params: &[wasmtime::Val],
        results: &mut [wasmtime::Val],
    ) -> anyhow::Result<()> {
        let func = self
            .instance
            .get_func(&mut self.store, export)
            .ok_or_else(|| anyhow!("missing export `{export}`"))?;
        func.call(&mut self.store, params, results)?;
        Ok(())
    }

    pub(crate) fn call_abi_v2_alloc(&mut self, size: i32, align: i32) -> anyhow::Result<i32> {
        let alloc = self
            .instance
            .get_typed_func::<(i32, i32), i32>(&mut self.store, "mitki:abi/2/alloc")?;
        Ok(alloc.call(&mut self.store, (size, align))?)
    }

    pub(crate) fn call_abi_v2_blob_release(&mut self, ptr: i32) -> anyhow::Result<()> {
        let release =
            self.instance.get_typed_func::<i32, ()>(&mut self.store, "mitki:abi/2/blob_release")?;
        Ok(release.call(&mut self.store, ptr)?)
    }

    pub(crate) fn call_abi_v2_handle_retain(&mut self, handle: i32) -> anyhow::Result<i32> {
        let retain = self
            .instance
            .get_typed_func::<i32, i32>(&mut self.store, "mitki:abi/2/handle_retain")?;
        Ok(retain.call(&mut self.store, handle)?)
    }

    pub(crate) fn call_abi_v2_handle_release(&mut self, handle: i32) -> anyhow::Result<()> {
        let release = self
            .instance
            .get_typed_func::<i32, ()>(&mut self.store, "mitki:abi/2/handle_release")?;
        Ok(release.call(&mut self.store, handle)?)
    }

    pub(crate) fn retain_abi_value_handles(
        &mut self,
        value: &AbiValue,
    ) -> anyhow::Result<Vec<u32>> {
        let mut retained = Vec::new();
        for handle_id in abi_value_handle_ids(value) {
            self.call_abi_v2_handle_retain(handle_to_i32(handle_id)?)?;
            retained.push(handle_id);
        }
        Ok(retained)
    }

    pub(crate) fn release_handle_ids(&mut self, handles: &[u32]) -> anyhow::Result<()> {
        for &handle_id in handles.iter().rev() {
            self.call_abi_v2_handle_release(handle_to_i32(handle_id)?)?;
        }
        Ok(())
    }

    pub(crate) fn write_memory(&mut self, offset: usize, bytes: &[u8]) -> anyhow::Result<()> {
        let memory = self.exported_memory()?;
        memory.write(&mut self.store, offset, bytes)?;
        Ok(())
    }

    pub(crate) fn read_canonical_blob(&mut self, ptr: i32) -> anyhow::Result<Vec<u8>> {
        let memory = self.exported_memory()?;
        let start = usize::try_from(ptr)
            .map_err(|_error| anyhow!("canonical blob pointer was negative"))?;
        let bytes = memory.data(&self.store);
        let header = bytes
            .get(start..start + 26)
            .ok_or_else(|| anyhow!("canonical blob header was truncated in guest memory"))?;
        let total = u32::from_le_bytes(header[22..26].try_into().expect("total len bytes"));
        let end = start
            .checked_add(total as usize)
            .ok_or_else(|| anyhow!("canonical blob length overflowed"))?;
        let blob = bytes
            .get(start..end)
            .ok_or_else(|| anyhow!("canonical blob was truncated in guest memory"))?;
        Ok(blob.to_vec())
    }
}

pub fn invoke_export(
    bytes: &[u8],
    export: &str,
    args: &[AbiValue],
) -> anyhow::Result<WasmRunOutput> {
    invoke_export_with_config(bytes, export, args, RunConfig::default())
}

pub fn invoke_export_instance(
    bytes: &[u8],
    instance_id: InstanceId,
    args: &[AbiValue],
) -> anyhow::Result<WasmRunOutput> {
    invoke_export_instance_with_config(bytes, instance_id, args, RunConfig::default())
}

pub fn invoke_export_with_config(
    bytes: &[u8],
    export: &str,
    args: &[AbiValue],
    config: RunConfig,
) -> anyhow::Result<WasmRunOutput> {
    let mut runtime = WasmRuntime::new(bytes, config)?;
    runtime.invoke_export(export, args)
}

pub fn invoke_raw_export_with_config(
    bytes: &[u8],
    export: &str,
    config: RunConfig,
) -> anyhow::Result<WasmRunOutput> {
    let mut runtime = WasmRuntime::new(bytes, config)?;
    runtime.invoke_raw_export(export)
}

pub fn invoke_export_instance_with_config(
    bytes: &[u8],
    instance_id: InstanceId,
    args: &[AbiValue],
    config: RunConfig,
) -> anyhow::Result<WasmRunOutput> {
    let mut runtime = WasmRuntime::new(bytes, config)?;
    runtime.invoke_export_instance(instance_id, args)
}

pub fn collect_unsupported_imports(bytes: &[u8]) -> anyhow::Result<Vec<String>> {
    collect_unsupported_imports_with_config(bytes, RunConfig::default())
}

pub fn collect_unsupported_imports_with_config(
    bytes: &[u8],
    config: RunConfig,
) -> anyhow::Result<Vec<String>> {
    let mut unsupported = Vec::new();
    let metadata = crate::runtime_v2::describe_module_abi_v2(bytes)?.metadata;

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload?;
        let wasmparser::Payload::ImportSection(reader) = payload else {
            continue;
        };

        for import in reader.into_imports() {
            let import = import?;
            let runtime_import =
                config.enable_mitki_runtime && runtime_builtin_matches(import.module, import.name);
            let wasi_import = config.enable_wasi && import.module == "wasi_snapshot_preview1";
            if !runtime_import && !wasi_import {
                if let Some(graph) = metadata.as_ref()
                    && let Some(instance) =
                        find_import_instance_by_linkage(graph, import.module, import.name)
                    && let Ok(symbol) = graph_symbol_name(graph, instance.logical_symbol)
                {
                    unsupported.push(format!("{}::{}", import.module, symbol));
                } else {
                    unsupported.push(format!("{}::{}", import.module, import.name));
                }
            }
        }
    }

    Ok(unsupported)
}

/// Describes the raw Wasm import/export surface together with decoded ABI v2
/// metadata, when present.
pub fn describe_module_abi(bytes: &[u8]) -> anyhow::Result<WasmModuleAbi> {
    let mut function_types = Vec::new();
    let mut function_type_indices = Vec::new();
    let mut imports = Vec::new();
    let mut raw_exports = Vec::new();
    let metadata = crate::runtime_v2::describe_module_abi_v2(bytes)?.metadata;

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload = payload?;
        match payload {
            wasmparser::Payload::TypeSection(reader) => {
                for ty in reader.into_iter_err_on_gc_types() {
                    let ty = ty?;
                    function_types.push(WasmFunctionAbi {
                        params: ty.params().iter().map(|ty| ty.to_string()).collect(),
                        results: ty.results().iter().map(|ty| ty.to_string()).collect(),
                    });
                }
            }
            wasmparser::Payload::ImportSection(reader) => {
                for import in reader.into_imports() {
                    let import = import?;
                    let kind = match import.ty {
                        wasmparser::TypeRef::Func(type_index) => {
                            function_type_indices.push(type_index);
                            WasmExternAbiKind::Function(
                                function_types.get(type_index as usize).cloned().ok_or_else(
                                    || anyhow!("missing function type `{type_index}`"),
                                )?,
                            )
                        }
                        wasmparser::TypeRef::Table(_) => WasmExternAbiKind::Table,
                        wasmparser::TypeRef::Memory(_) => WasmExternAbiKind::Memory,
                        wasmparser::TypeRef::Global(_) => WasmExternAbiKind::Global,
                        wasmparser::TypeRef::Tag(_) => WasmExternAbiKind::Tag,
                        wasmparser::TypeRef::FuncExact(type_index) => WasmExternAbiKind::Function(
                            function_types.get(type_index as usize).cloned().ok_or_else(|| {
                                anyhow!("missing exact function type `{type_index}`")
                            })?,
                        ),
                    };
                    imports.push(WasmImportAbi {
                        module: import.module.to_owned(),
                        name: import.name.to_owned(),
                        kind,
                    });
                }
            }
            wasmparser::Payload::FunctionSection(reader) => {
                for function in reader {
                    function_type_indices.push(function?);
                }
            }
            wasmparser::Payload::ExportSection(reader) => {
                for export in reader {
                    raw_exports.push(export?);
                }
            }
            _ => {}
        }
    }

    let exports = raw_exports
        .into_iter()
        .map(|export| {
            let kind = match export.kind {
                wasmparser::ExternalKind::Func | wasmparser::ExternalKind::FuncExact => {
                    let type_index = *function_type_indices
                        .get(export.index as usize)
                        .ok_or_else(|| anyhow!("missing function index `{}`", export.index))?;
                    WasmExternAbiKind::Function(
                        function_types
                            .get(type_index as usize)
                            .cloned()
                            .ok_or_else(|| anyhow!("missing function type `{type_index}`"))?,
                    )
                }
                wasmparser::ExternalKind::Table => WasmExternAbiKind::Table,
                wasmparser::ExternalKind::Memory => WasmExternAbiKind::Memory,
                wasmparser::ExternalKind::Global => WasmExternAbiKind::Global,
                wasmparser::ExternalKind::Tag => WasmExternAbiKind::Tag,
            };
            let name = export.name.to_owned();
            Ok(WasmExportAbi { name, kind })
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    Ok(WasmModuleAbi { metadata, imports, exports })
}

fn builtin_import_supported(module: &str, name: &str, config: RunConfig) -> bool {
    let runtime_import = config.enable_mitki_runtime && runtime_builtin_matches(module, name);
    let wasi_import = config.enable_wasi && module == "wasi_snapshot_preview1";
    runtime_import || wasi_import
}

fn import_display_name(metadata: Option<&SemanticTypeGraph>, module: &str, name: &str) -> String {
    if let Some(graph) = metadata
        && let Some(instance) = find_import_instance_by_linkage(graph, module, name)
        && let Ok(symbol) = graph_symbol_name(graph, instance.logical_symbol)
    {
        return format!("{module}::{symbol}");
    }
    format!("{module}::{name}")
}

fn import_instance_named_matches(
    graph: &SemanticTypeGraph,
    instance: &mitki_abi::FunctionInstance,
    module: &str,
    name: &str,
) -> bool {
    instance.linkage == LinkageKind::WasmImport
        && instance
            .wasm_module_name
            .and_then(|id| graph_string_value(graph, id).ok())
            .is_some_and(|candidate| candidate == module)
        && graph_symbol_name(graph, instance.logical_symbol).is_ok_and(|symbol| symbol == name)
}

fn resolve_host_handler_key(
    graph: &SemanticTypeGraph,
    instance: &mitki_abi::FunctionInstance,
    host_handlers: &HostHandlers,
) -> anyhow::Result<Option<HostHandlerKey>> {
    if host_handlers.has_instance(instance.id) {
        return Ok(Some(HostHandlerKey::Instance(instance.id.0)));
    }

    let Some(module_id) = instance.wasm_module_name else {
        return Ok(None);
    };
    let module = graph_string_value(graph, module_id)?;
    let logical_name = graph_symbol_name(graph, instance.logical_symbol)?;
    if !host_handlers.has_named(module, logical_name) {
        return Ok(None);
    }

    let mut matches = graph
        .function_instances
        .iter()
        .filter(|candidate| import_instance_named_matches(graph, candidate, module, logical_name));
    let Some(_first) = matches.next() else {
        return Ok(None);
    };
    if matches.next().is_some() {
        bail!(
            "typed host handler `{module}::{logical_name}` is ambiguous; register a concrete \
             instance handler instead"
        );
    }
    Ok(Some(HostHandlerKey::Named { module: module.to_owned(), name: logical_name.to_owned() }))
}

fn ensure_supported_imports(
    abi: &WasmModuleAbi,
    config: RunConfig,
    host_handlers: &HostHandlers,
) -> anyhow::Result<()> {
    let unsupported_imports = abi
        .imports
        .iter()
        .filter(|import| !supports_import(import, abi.metadata.as_ref(), config, host_handlers))
        .map(|import| import_display_name(abi.metadata.as_ref(), &import.module, &import.name))
        .collect::<Vec<_>>();
    if unsupported_imports.is_empty() {
        return Ok(());
    }

    let imports = unsupported_imports.join(", ");
    if host_handlers.is_empty() {
        anyhow::bail!(
            "run-wasm supports only WASI and `mitki` runtime imports; unresolved imports: \
             {imports}"
        );
    }
    anyhow::bail!(
        "runtime supports only configured typed host imports, WASI, and `mitki` runtime imports; \
         unresolved imports: {imports}"
    );
}

fn insert_host_handler(
    handlers: &mut HostHandlers,
    module: String,
    name: String,
    handler: HostImportHandler,
) {
    handlers.insert_named(module, name, handler);
}

fn insert_host_instance_handler(
    handlers: &mut HostHandlers,
    instance_id: InstanceId,
    handler: HostImportHandler,
) {
    handlers.insert_instance(instance_id, handler);
}

fn supports_import(
    import: &WasmImportAbi,
    metadata: Option<&SemanticTypeGraph>,
    config: RunConfig,
    host_handlers: &HostHandlers,
) -> bool {
    let builtin_import = builtin_import_supported(&import.module, &import.name, config)
        || metadata
            .and_then(|graph| find_import_instance_by_linkage(graph, &import.module, &import.name))
            .is_some_and(|instance| {
                graph_symbol_name(metadata.expect("graph should exist"), instance.logical_symbol)
                    .is_ok_and(|name| builtin_import_supported(&import.module, name, config))
            });
    let host_import = metadata
        .and_then(|graph| find_import_instance_by_linkage(graph, &import.module, &import.name))
        .is_some_and(|instance| {
            metadata
                .and_then(|graph| resolve_host_handler_key(graph, instance, host_handlers).ok())
                .flatten()
                .is_some()
        });
    builtin_import || host_import
}

fn add_builtin_typed_import_aliases(
    linker: &mut Linker<RuntimeStore>,
    module: &wasmtime::Module,
    metadata: Option<&SemanticTypeGraph>,
    config: RunConfig,
) -> anyhow::Result<()> {
    let Some(graph) = metadata else {
        return Ok(());
    };

    for import in module.imports() {
        let Some(instance) = find_import_instance_by_linkage(graph, import.module(), import.name())
        else {
            continue;
        };
        let logical_name = graph_symbol_name(graph, instance.logical_symbol)?;
        if import.name() == logical_name
            || !builtin_import_supported(import.module(), logical_name, config)
        {
            continue;
        }
        linker.alias(import.module(), logical_name, import.module(), import.name())?;
    }

    Ok(())
}

fn add_typed_host_imports(
    linker: &mut Linker<RuntimeStore>,
    module: &wasmtime::Module,
    metadata: Option<&SemanticTypeGraph>,
    host_handlers: &HostHandlers,
) -> anyhow::Result<()> {
    let Some(graph) = metadata else {
        return Ok(());
    };

    for import in module.imports() {
        let Some(instance) = find_import_instance_by_linkage(graph, import.module(), import.name())
        else {
            continue;
        };
        let Some(handler_key) = resolve_host_handler_key(graph, instance, host_handlers)? else {
            continue;
        };

        let extern_ty = import.ty();
        let wasmtime::ExternType::Func(func_ty) = extern_ty else {
            anyhow::bail!(
                "typed host handlers require function imports, found `{}::{}`",
                import.module(),
                import.name()
            );
        };
        let function_signature = graph
            .signatures
            .get(instance.signature.0 as usize)
            .cloned()
            .ok_or_else(|| anyhow!("missing ABI v2 signature `{}`", instance.signature.0))?;

        register_typed_host_import(
            linker,
            import.module(),
            import.name(),
            func_ty,
            handler_key,
            graph.clone(),
            function_signature,
        )?;
    }

    Ok(())
}

fn register_typed_host_import(
    linker: &mut Linker<RuntimeStore>,
    module: &str,
    name: &str,
    ty: wasmtime::FuncType,
    handler_key: HostHandlerKey,
    graph: SemanticTypeGraph,
    function_signature: AbiFunctionSignature,
) -> anyhow::Result<()> {
    linker.func_new(
        module,
        name,
        ty,
        move |mut caller, params, results| -> wasmtime::Result<()> {
            invoke_typed_host_import(
                &mut caller,
                &handler_key,
                &graph,
                &function_signature,
                params,
                results,
            )
            .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    Ok(())
}

fn build_wasi_context(config: RunConfig) -> anyhow::Result<p1::WasiP1Ctx> {
    if config.enable_wasi {
        let mut builder = WasiCtxBuilder::new();
        builder.inherit_stdio().inherit_args().inherit_env();
        let cwd = std::env::current_dir().context("failed to resolve current working directory")?;
        builder.preopened_dir(cwd, ".", DirPerms::all(), FilePerms::all()).map_err(|error| {
            anyhow!("failed to preopen the current working directory for WASI: {error}")
        })?;
        Ok(builder.build_p1())
    } else {
        Ok(WasiCtxBuilder::new().build_p1())
    }
}

fn invoke_typed_host_import(
    caller: &mut Caller<'_, RuntimeStore>,
    handler_key: &HostHandlerKey,
    graph: &SemanticTypeGraph,
    function_signature: &AbiFunctionSignature,
    params: &[wasmtime::Val],
    results: &mut [wasmtime::Val],
) -> anyhow::Result<()> {
    let memory = guest_memory(caller)?;
    let bytes = memory.data(&*caller).to_vec();
    let mut value_index = 0usize;
    let abi_args = function_signature
        .params
        .iter()
        .map(|transport| match transport.transport_class {
            TransportClass::Immediate => {
                let needs_lane = transport_has_wasm_lane(graph, transport)?;
                let raw = needs_lane
                    .then(|| {
                        params
                            .get(value_index)
                            .ok_or_else(|| anyhow!("missing immediate typed host import argument"))
                    })
                    .transpose()?;
                if needs_lane {
                    value_index += 1;
                    Ok(AbiValue::Immediate(crate::runtime_v2::read_immediate_result(
                        graph,
                        transport_carrier_type(transport),
                        std::slice::from_ref(raw.expect("immediate raw value")),
                    )?))
                } else {
                    Ok(AbiValue::Immediate(crate::runtime_v2::read_immediate_result(
                        graph,
                        transport_carrier_type(transport),
                        &[],
                    )?))
                }
            }
            TransportClass::CanonicalValue => {
                let ptr = params
                    .get(value_index)
                    .and_then(wasmtime::Val::i32)
                    .ok_or_else(|| anyhow!("missing canonical typed host import argument"))?;
                value_index += 1;
                let blob = read_canonical_blob_from_bytes(&bytes, ptr)?;
                decode_canonical_blob(&blob)
            }
            TransportClass::CapabilityHandle => {
                let ptr = params
                    .get(value_index)
                    .and_then(wasmtime::Val::i32)
                    .ok_or_else(|| anyhow!("missing capability typed host import argument"))?;
                value_index += 1;
                Ok(AbiValue::Handle {
                    type_id: transport_carrier_type(transport),
                    handle_id: i32_to_handle(ptr)?,
                })
            }
        })
        .collect::<anyhow::Result<Vec<_>>>()?;
    if value_index != params.len() {
        anyhow::bail!(
            "expected {value_index} raw argument value(s) for typed host import, got {}",
            params.len()
        );
    }
    let result = {
        let handler = caller
            .data_mut()
            .host_handlers
            .get_mut(handler_key)
            .ok_or_else(|| anyhow!("missing host handler for typed import"))?;
        handler(&abi_args)
    };
    let result = result?;
    match function_signature.result.transport_class {
        TransportClass::Immediate => {
            let unit = AbiValue::Immediate(mitki_abi::AbiScalar::Unit);
            let value = result.as_ref().unwrap_or(&unit);
            if let Some(raw) = crate::runtime_v2::abi_immediate_to_val(
                graph,
                transport_carrier_type(&function_signature.result),
                value,
            )? {
                let slot = results.first_mut().ok_or_else(|| {
                    anyhow!("missing immediate result slot for typed host import")
                })?;
                *slot = raw;
            }
            Ok(())
        }
        TransportClass::CanonicalValue => {
            let value = result
                .as_ref()
                .ok_or_else(|| anyhow!("typed host import expected a canonical result"))?;
            let retained_handles = retain_guest_abi_value_handles(caller, value)?;
            let blob = match encode_canonical_blob(value) {
                Ok(blob) => blob,
                Err(error) => {
                    release_guest_handle_ids(caller, &retained_handles)?;
                    return Err(error);
                }
            };
            let ptr = match alloc_store_region(
                caller,
                &memory,
                u32::try_from(blob.len())
                    .map_err(|_error| anyhow!("canonical host import result blob was too large"))?,
                4,
            ) {
                Ok(ptr) => ptr,
                Err(error) => {
                    release_guest_handle_ids(caller, &retained_handles)?;
                    return Err(error);
                }
            };
            let offset = usize::try_from(ptr)
                .map_err(|_error| anyhow!("canonical host import result pointer was negative"))?;
            if let Err(error) = memory.write(&mut *caller, offset, &blob) {
                release_guest_handle_ids(caller, &retained_handles)?;
                return Err(error.into());
            }
            let slot = results
                .first_mut()
                .ok_or_else(|| anyhow!("missing pointer result slot for typed host import"))?;
            *slot = wasmtime::Val::I32(ptr);
            Ok(())
        }
        TransportClass::CapabilityHandle => {
            let value = result
                .as_ref()
                .ok_or_else(|| anyhow!("typed host import expected a handle result"))?;
            let AbiValue::Handle { type_id, handle_id } = value else {
                bail!("typed host import expected a handle result");
            };
            let carrier = transport_carrier_type(&function_signature.result);
            if *type_id != carrier {
                bail!(
                    "typed host import returned handle type `{}` but signature expected `{}`",
                    type_id.0,
                    carrier.0
                );
            }
            let slot = results
                .first_mut()
                .ok_or_else(|| anyhow!("missing handle result slot for typed host import"))?;
            *slot = wasmtime::Val::I32(handle_to_i32(*handle_id)?);
            Ok(())
        }
    }
}

fn read_canonical_blob_from_bytes(bytes: &[u8], ptr: i32) -> anyhow::Result<Vec<u8>> {
    let start =
        usize::try_from(ptr).map_err(|_error| anyhow!("canonical blob pointer was negative"))?;
    let header = bytes
        .get(start..start + 26)
        .ok_or_else(|| anyhow!("canonical blob header was truncated in guest memory"))?;
    let total = u32::from_le_bytes(header[22..26].try_into().expect("total len bytes"));
    let end = start
        .checked_add(total as usize)
        .ok_or_else(|| anyhow!("canonical blob length overflowed"))?;
    let blob = bytes
        .get(start..end)
        .ok_or_else(|| anyhow!("canonical blob was truncated in guest memory"))?;
    Ok(blob.to_vec())
}

#[derive(Clone, Copy)]
enum RuntimeBuiltinBinding {
    PrintI32,
    PrintStr,
    Alloc,
    Dealloc,
    CopyNonOverlapping,
}

#[derive(Clone, Copy)]
struct RuntimeBuiltinDescriptor {
    import_module: &'static str,
    import_name: &'static str,
    binding: RuntimeBuiltinBinding,
}

const RUNTIME_BUILTINS: [RuntimeBuiltinDescriptor; 7] = [
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "print_i32",
        binding: RuntimeBuiltinBinding::PrintI32,
    },
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "_print_i32",
        binding: RuntimeBuiltinBinding::PrintI32,
    },
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "print_str",
        binding: RuntimeBuiltinBinding::PrintStr,
    },
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "_print_str",
        binding: RuntimeBuiltinBinding::PrintStr,
    },
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "alloc",
        binding: RuntimeBuiltinBinding::Alloc,
    },
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "dealloc",
        binding: RuntimeBuiltinBinding::Dealloc,
    },
    RuntimeBuiltinDescriptor {
        import_module: "mitki",
        import_name: "_copy_nonoverlapping",
        binding: RuntimeBuiltinBinding::CopyNonOverlapping,
    },
];

fn runtime_builtin_matches(module: &str, name: &str) -> bool {
    RUNTIME_BUILTINS
        .iter()
        .any(|builtin| builtin.import_module == module && builtin.import_name == name)
}

fn add_runtime_imports(linker: &mut Linker<RuntimeStore>) -> anyhow::Result<()> {
    for descriptor in RUNTIME_BUILTINS {
        register_runtime_builtin(linker, descriptor)?;
    }
    Ok(())
}

fn register_runtime_builtin(
    linker: &mut Linker<RuntimeStore>,
    descriptor: RuntimeBuiltinDescriptor,
) -> anyhow::Result<()> {
    match descriptor.binding {
        RuntimeBuiltinBinding::PrintI32 => {
            linker.func_wrap(
                descriptor.import_module,
                descriptor.import_name,
                |mut caller: Caller<'_, RuntimeStore>, value: i32| {
                    caller.data_mut().stdout.push_str(&value.to_string());
                },
            )?;
        }
        RuntimeBuiltinBinding::PrintStr => {
            linker.func_wrap(
                descriptor.import_module,
                descriptor.import_name,
                |mut caller: Caller<'_, RuntimeStore>, ptr: i32| -> wasmtime::Result<()> {
                    let string = read_guest_string(&mut caller, ptr)
                        .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
                    caller.data_mut().stdout.push_str(&string);
                    Ok(())
                },
            )?;
        }
        RuntimeBuiltinBinding::Alloc => register_alloc_runtime_builtin(
            linker,
            descriptor.import_module,
            descriptor.import_name,
        )?,
        RuntimeBuiltinBinding::Dealloc => register_dealloc_runtime_builtin(
            linker,
            descriptor.import_module,
            descriptor.import_name,
        )?,
        RuntimeBuiltinBinding::CopyNonOverlapping => register_copy_non_overlapping_runtime_builtin(
            linker,
            descriptor.import_module,
            descriptor.import_name,
        )?,
    }
    Ok(())
}

fn register_alloc_runtime_builtin<T: AllocatingStore + 'static>(
    linker: &mut Linker<T>,
    module: &str,
    name: &str,
) -> anyhow::Result<()> {
    linker.func_wrap(
        module,
        name,
        |mut caller: Caller<'_, T>, size: i32, align: i32| -> wasmtime::Result<i32> {
            let size = u64::try_from(size).map_err(|_error| {
                wasmtime::Error::msg("runtime alloc requires a non-negative size")
            })?;
            let align = u64::try_from(align).map_err(|_error| {
                wasmtime::Error::msg("runtime alloc requires a positive power-of-two alignment")
            })?;
            alloc_caller_region(&mut caller, size, align)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    Ok(())
}

fn register_dealloc_runtime_builtin<T: AllocatingStore + 'static>(
    linker: &mut Linker<T>,
    module: &str,
    name: &str,
) -> anyhow::Result<()> {
    linker.func_wrap(
        module,
        name,
        |mut caller: Caller<'_, T>, ptr: i32, size: i32, align: i32| -> wasmtime::Result<()> {
            caller.data_mut().allocator_mut().dealloc(ptr, size, align).map_err(|error| {
                wasmtime::Error::msg(format!(
                    "runtime dealloc failed for ptr={ptr} size={size} align={align}: {error}"
                ))
            })
        },
    )?;
    Ok(())
}

fn register_copy_non_overlapping_runtime_builtin<T: 'static>(
    linker: &mut Linker<T>,
    module: &str,
    name: &str,
) -> anyhow::Result<()> {
    linker.func_wrap(
        module,
        name,
        |mut caller: Caller<'_, T>, dst: i32, src: i32, count: i32| -> wasmtime::Result<()> {
            let memory = guest_memory(&mut caller).map_err(|error| {
                wasmtime::Error::msg(format!(
                    "runtime copy_nonoverlapping could not access guest memory: {error}"
                ))
            })?;
            let dst = usize::try_from(dst).map_err(|_error| {
                wasmtime::Error::msg("runtime copy_nonoverlapping requires a non-negative dst")
            })?;
            let src = usize::try_from(src).map_err(|_error| {
                wasmtime::Error::msg("runtime copy_nonoverlapping requires a non-negative src")
            })?;
            let count = usize::try_from(count).map_err(|_error| {
                wasmtime::Error::msg("runtime copy_nonoverlapping requires a non-negative count")
            })?;
            let mut bytes = vec![0; count];
            memory.read(&mut caller, src, &mut bytes).map_err(|error| {
                wasmtime::Error::msg(format!(
                    "runtime copy_nonoverlapping could not read guest memory: {error}"
                ))
            })?;
            memory.write(&mut caller, dst, &bytes).map_err(|error| {
                wasmtime::Error::msg(format!(
                    "runtime copy_nonoverlapping could not write guest memory: {error}"
                ))
            })?;
            Ok(())
        },
    )?;
    Ok(())
}

fn abi_value_handle_ids(value: &AbiValue) -> Vec<u32> {
    let mut handles = std::collections::BTreeSet::new();
    match value {
        AbiValue::Immediate(_) => {}
        AbiValue::Handle { handle_id, .. } => {
            handles.insert(*handle_id);
        }
        AbiValue::Canonical { graph, .. } => {
            handles.extend(graph.handles.iter().map(|slot| slot.handle_id));
        }
    }
    handles.into_iter().collect()
}

fn handle_to_i32(handle_id: u32) -> anyhow::Result<i32> {
    i32::try_from(handle_id).map_err(|_error| anyhow!("handle id exceeded i32 range"))
}

fn i32_to_handle(raw: i32) -> anyhow::Result<u32> {
    u32::try_from(raw).map_err(|_error| anyhow!("handle id was negative"))
}

fn retain_guest_abi_value_handles(
    caller: &mut Caller<'_, RuntimeStore>,
    value: &AbiValue,
) -> anyhow::Result<Vec<u32>> {
    let mut retained = Vec::new();
    for handle_id in abi_value_handle_ids(value) {
        caller_handle_retain(caller, handle_to_i32(handle_id)?)?;
        retained.push(handle_id);
    }
    Ok(retained)
}

fn release_guest_handle_ids(
    caller: &mut Caller<'_, RuntimeStore>,
    handles: &[u32],
) -> anyhow::Result<()> {
    for &handle_id in handles.iter().rev() {
        caller_handle_release(caller, handle_to_i32(handle_id)?)?;
    }
    Ok(())
}

fn caller_handle_retain(caller: &mut Caller<'_, RuntimeStore>, handle: i32) -> anyhow::Result<i32> {
    let func = caller
        .get_export("mitki:abi/2/handle_retain")
        .and_then(Extern::into_func)
        .ok_or_else(|| anyhow!("missing export `mitki:abi/2/handle_retain`"))?;
    let typed = func.typed::<i32, i32>(&mut *caller)?;
    Ok(typed.call(&mut *caller, handle)?)
}

fn caller_handle_release(caller: &mut Caller<'_, RuntimeStore>, handle: i32) -> anyhow::Result<()> {
    let func = caller
        .get_export("mitki:abi/2/handle_release")
        .and_then(Extern::into_func)
        .ok_or_else(|| anyhow!("missing export `mitki:abi/2/handle_release`"))?;
    let typed = func.typed::<i32, ()>(&mut *caller)?;
    Ok(typed.call(&mut *caller, handle)?)
}

fn read_guest_string<T>(caller: &mut Caller<'_, T>, ptr: i32) -> anyhow::Result<String> {
    let memory = guest_memory(caller)?;
    read_memory_string(&memory, caller, ptr)
}

fn read_memory_string<T>(
    memory: &wasmtime::Memory,
    mut store: impl wasmtime::AsContextMut<Data = T>,
    ptr: i32,
) -> anyhow::Result<String> {
    let ptr = usize::try_from(ptr)
        .map_err(|_error| anyhow!("runtime print_str requires a non-negative pointer"))?;
    let mut len_bytes = [0; 4];
    memory
        .read(&mut store, ptr, &mut len_bytes)
        .context("runtime print_str could not read string length")?;
    let len = u32::from_le_bytes(len_bytes) as usize;
    let mut bytes = vec![0; len];
    memory
        .read(&mut store, ptr + 4, &mut bytes)
        .context("runtime print_str could not read string bytes")?;
    String::from_utf8(bytes).context("runtime print_str encountered invalid UTF-8")
}

fn guest_memory<T>(caller: &mut Caller<'_, T>) -> anyhow::Result<wasmtime::Memory> {
    caller
        .get_export("memory")
        .and_then(Extern::into_memory)
        .ok_or_else(|| anyhow!("runtime import requires an exported memory named `memory`"))
}

fn ensure_memory<T>(
    caller: &mut Caller<'_, T>,
    memory: &wasmtime::Memory,
    end: u64,
) -> anyhow::Result<()> {
    let current = memory.data_size(&mut *caller) as u64;
    if end <= current {
        return Ok(());
    }

    let additional = end - current;
    let pages = additional.div_ceil(WASM_PAGE_SIZE);
    memory
        .grow(&mut *caller, pages)
        .map(|_previous| ())
        .map_err(|error| anyhow!("runtime alloc failed to grow memory: {error}"))
}

fn ensure_store_memory<T: 'static, S: wasmtime::AsContextMut<Data = T>>(
    store: &mut S,
    memory: &wasmtime::Memory,
    end: u64,
) -> anyhow::Result<()> {
    let mut store = store.as_context_mut();
    let current = memory.data_size(&mut store) as u64;
    if end <= current {
        return Ok(());
    }

    let additional = end - current;
    let pages = additional.div_ceil(WASM_PAGE_SIZE);
    memory
        .grow(&mut store, pages)
        .map(|_previous| ())
        .map_err(|error| anyhow!("runtime alloc failed to grow memory: {error}"))
}

fn align_to(offset: u64, align: u64) -> u64 {
    if align <= 1 {
        offset
    } else {
        let mask = align - 1;
        (offset + mask) & !mask
    }
}

fn alloc_caller_region<T: AllocatingStore>(
    caller: &mut Caller<'_, T>,
    size: u64,
    align: u64,
) -> anyhow::Result<i32> {
    let memory = guest_memory(caller)?;
    let current_limit = memory.data_size(&*caller) as u64;
    let plan = caller.data().allocator().plan_alloc(current_limit, size, align)?;
    ensure_memory(caller, &memory, plan.required_end())?;
    caller.data_mut().allocator_mut().commit_alloc(plan)
}

fn alloc_store_region<T: AllocatingStore + 'static, S: wasmtime::AsContextMut<Data = T>>(
    store: &mut S,
    memory: &wasmtime::Memory,
    size: u32,
    align: u32,
) -> anyhow::Result<i32> {
    let size = u64::from(size);
    let align = u64::from(align.max(1));
    let mut store_context = store.as_context_mut();
    let current_limit = memory.data_size(&mut store_context) as u64;
    let plan = store_context.data().allocator().plan_alloc(current_limit, size, align)?;
    ensure_store_memory(&mut store_context, memory, plan.required_end())?;
    store_context.data_mut().allocator_mut().commit_alloc(plan)
}

trait AllocatingStore {
    fn allocator(&self) -> &GuestAllocator;
    fn allocator_mut(&mut self) -> &mut GuestAllocator;
}

impl AllocatingStore for RuntimeStore {
    fn allocator(&self) -> &GuestAllocator {
        &self.allocator
    }

    fn allocator_mut(&mut self) -> &mut GuestAllocator {
        &mut self.allocator
    }
}

struct RuntimeStore {
    wasi: p1::WasiP1Ctx,
    stdout: String,
    allocator: GuestAllocator,
    host_handlers: HostHandlers,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guest_allocator_splits_and_coalesces_free_regions() {
        let mut allocator = GuestAllocator::default();
        let first = allocator
            .commit_alloc(allocator.plan_alloc(64, 8, 4).expect("first allocation plan"))
            .expect("first allocation");
        let second = allocator
            .commit_alloc(allocator.plan_alloc(64, 8, 4).expect("second allocation plan"))
            .expect("second allocation");
        assert_eq!(allocator.frontier, Some(80));

        allocator.dealloc(first, 8, 4).expect("free first region");
        let split_plan = allocator.plan_alloc(64, 4, 4).expect("split allocation plan");
        assert_eq!(split_plan.ptr, u64::try_from(first).expect("non-negative first pointer"));
        let split = allocator.commit_alloc(split_plan).expect("split allocation");
        assert_eq!(allocator.frontier, Some(80));

        allocator.dealloc(split, 4, 4).expect("free split region");
        allocator.dealloc(second, 8, 4).expect("free second region");
        let coalesced = allocator.plan_alloc(64, 16, 4).expect("coalesced allocation plan");
        assert_eq!(coalesced.ptr, u64::try_from(first).expect("non-negative first pointer"));
        assert_eq!(allocator.frontier, Some(80));
    }

    #[test]
    fn guest_allocator_rejects_invalid_deallocs() {
        let mut allocator = GuestAllocator::default();
        let ptr = allocator
            .commit_alloc(allocator.plan_alloc(64, 8, 4).expect("allocation plan"))
            .expect("allocation");

        let mismatch = allocator.dealloc(ptr, 4, 4).expect_err("mismatched dealloc should fail");
        assert!(mismatch.to_string().contains("did not match the original allocation"));

        let unknown =
            allocator.dealloc(ptr + 16, 8, 4).expect_err("unknown pointer dealloc should fail");
        assert!(unknown.to_string().contains("unknown pointer"));

        allocator.dealloc(ptr, 8, 4).expect("original allocation can still be freed");
    }
}

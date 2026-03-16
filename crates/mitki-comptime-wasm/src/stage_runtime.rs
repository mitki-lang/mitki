use anyhow::{Context as _, anyhow};
use mitki_abi::{
    AbiValue, TransportClass, decode_canonical_blob, export_wasm_name, find_export_instance,
    signature, transport_carrier_type,
};
use mitki_hir::ty::{Ty, TyKind};
use mitki_lower::hir::HasFunction as _;
use mitki_lower::item::scope::{FunctionLocation, enum_variants, struct_fields};
use mitki_typeck::infer::Inferable as _;
use salsa::plumbing::FromId as _;
use wasmtime::{Caller, Extern, Linker};

const ARC_ALIGN: u32 = 8;
const WASM_PAGE_SIZE: u64 = 65_536;

pub(crate) fn run_stage_export(
    db: &dyn salsa::Database,
    bytes: &[u8],
    export: &str,
) -> anyhow::Result<AbiValue> {
    let engine = wasmtime::Engine::default();
    let module = wasmtime::Module::from_binary(&engine, bytes)?;
    let abi_v2 = mitki_wasm_runtime::describe_module_abi_v2(bytes)?;
    let metadata = abi_v2
        .metadata
        .clone()
        .ok_or_else(|| anyhow!("staged execution requires `mitki.abi.v2` metadata"))?;
    let instance_metadata = find_export_instance(&metadata, export)?;
    let signature = signature(&metadata, instance_metadata.signature)?;
    let typed_export_name = export_wasm_name(&metadata, instance_metadata)?.to_owned();
    let mut linker = Linker::new(&engine);
    add_alloc_runtime_imports(&mut linker)?;
    add_stage_imports(&mut linker)?;
    let mut store = wasmtime::Store::new(
        &engine,
        StageRuntimeStore {
            db: db as *const dyn salsa::Database,
            allocator: GuestAllocator::default(),
        },
    );
    let instance = linker.instantiate(&mut store, &module)?;
    let memory = instance
        .get_memory(&mut store, "memory")
        .ok_or_else(|| anyhow!("staged execution requires an exported memory named `memory`"))?;
    let func = instance
        .get_func(&mut store, &typed_export_name)
        .ok_or_else(|| anyhow!("missing stage export `{typed_export_name}`"))?;
    let mut results = mitki_wasm_runtime::typed_result_slots(&metadata, &signature.result)?;
    func.call(&mut store, &[], &mut results)?;

    match signature.result.transport_class {
        TransportClass::Immediate => {
            Ok(AbiValue::Immediate(mitki_wasm_runtime::read_immediate_result(
                &metadata,
                signature.result.semantic_type,
                &results,
            )?))
        }
        TransportClass::CanonicalValue => {
            let ptr = results
                .first()
                .and_then(wasmtime::Val::i32)
                .ok_or_else(|| anyhow!("expected canonical ABI v2 pointer stage result"))?;
            let bytes = read_canonical_blob_from_store(&memory, &store, ptr)?;
            let value = decode_canonical_blob(&bytes)?;
            let release =
                instance.get_typed_func::<i32, ()>(&mut store, "mitki:abi/2/blob_release")?;
            release.call(&mut store, ptr)?;
            Ok(value)
        }
        TransportClass::CapabilityHandle => {
            let raw = results
                .first()
                .and_then(wasmtime::Val::i32)
                .ok_or_else(|| anyhow!("expected handle ABI v2 stage result"))?;
            Ok(AbiValue::Handle {
                type_id: transport_carrier_type(&signature.result),
                handle_id: i32_to_handle(raw)?,
            })
        }
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
    active: std::collections::BTreeMap<u64, ActiveAllocation>,
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

struct StageRuntimeStore {
    db: *const dyn salsa::Database,
    allocator: GuestAllocator,
}

impl StageRuntimeStore {
    fn db(&self) -> &dyn salsa::Database {
        unsafe { &*self.db }
    }
}

trait AllocatingStore {
    fn allocator(&self) -> &GuestAllocator;
    fn allocator_mut(&mut self) -> &mut GuestAllocator;
}

impl AllocatingStore for StageRuntimeStore {
    fn allocator(&self) -> &GuestAllocator {
        &self.allocator
    }

    fn allocator_mut(&mut self) -> &mut GuestAllocator {
        &mut self.allocator
    }
}

fn add_alloc_runtime_imports<T: AllocatingStore + 'static>(
    linker: &mut Linker<T>,
) -> anyhow::Result<()> {
    register_alloc_runtime_builtin(linker, "mitki", "alloc")?;
    register_dealloc_runtime_builtin(linker, "mitki", "dealloc")?;
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

fn add_stage_imports(linker: &mut Linker<StageRuntimeStore>) -> anyhow::Result<()> {
    linker.func_wrap(
        "mitki_stage",
        "type_name",
        |mut caller: Caller<'_, StageRuntimeStore>, ty_bits: i32| -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let ty = decode_type_id(db, ty_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let text = ty.display(db).to_string();
            write_guest_string(&mut caller, &text)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "field_count",
        |caller: Caller<'_, StageRuntimeStore>, ty_bits: i32| -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let ty = decode_type_id(db, ty_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let TyKind::Struct(struct_ty) = ty.kind(db) else {
                return Err(wasmtime::Error::msg("field_count requires a struct type"));
            };
            Ok(struct_fields(db, *struct_ty).len() as i32)
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "field_name",
        |mut caller: Caller<'_, StageRuntimeStore>,
         ty_bits: i32,
         index: i32|
         -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let ty = decode_type_id(db, ty_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let TyKind::Struct(struct_ty) = ty.kind(db) else {
                return Err(wasmtime::Error::msg("field_name requires a struct type"));
            };
            let index = usize::try_from(index).map_err(|_error| {
                wasmtime::Error::msg("field_name requires a non-negative index")
            })?;
            let fields = struct_fields(db, *struct_ty);
            let (name, _) = fields
                .get(index)
                .ok_or_else(|| wasmtime::Error::msg("field_name index out of range"))?;
            let text = name.text(db).to_owned();
            write_guest_string(&mut caller, &text)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "variant_count",
        |caller: Caller<'_, StageRuntimeStore>, ty_bits: i32| -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let ty = decode_type_id(db, ty_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let TyKind::Enum(enum_ty) = ty.kind(db) else {
                return Err(wasmtime::Error::msg("variant_count requires an enum type"));
            };
            Ok(enum_variants(db, *enum_ty).len() as i32)
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "variant_name",
        |mut caller: Caller<'_, StageRuntimeStore>,
         ty_bits: i32,
         index: i32|
         -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let ty = decode_type_id(db, ty_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let TyKind::Enum(enum_ty) = ty.kind(db) else {
                return Err(wasmtime::Error::msg("variant_name requires an enum type"));
            };
            let index = usize::try_from(index).map_err(|_error| {
                wasmtime::Error::msg("variant_name requires a non-negative index")
            })?;
            let variants = enum_variants(db, *enum_ty);
            let (name, _) = variants
                .get(index)
                .ok_or_else(|| wasmtime::Error::msg("variant_name index out of range"))?;
            let text = name.text(db).to_owned();
            write_guest_string(&mut caller, &text)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "function_param_count",
        |caller: Caller<'_, StageRuntimeStore>, function_bits: i32| -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let function = decode_function_id(db, function_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            Ok(function.hir_function(db).function(db).params().len() as i32)
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "function_param_type_name",
        |mut caller: Caller<'_, StageRuntimeStore>,
         function_bits: i32,
         index: i32|
         -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let function = decode_function_id(db, function_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let index = usize::try_from(index).map_err(|_error| {
                wasmtime::Error::msg("function_param_type_name requires a non-negative index")
            })?;
            let hir_function = function.hir_function(db).function(db);
            let param = *hir_function.params().get(index).ok_or_else(|| {
                wasmtime::Error::msg("function_param_type_name index out of range")
            })?;
            let nodes = hir_function.node_store();
            let inference = function.infer(db);
            let (pattern, _) = nodes.param(param);
            let ty = nodes
                .pattern_binding_names(pattern)
                .into_iter()
                .find_map(|name| inference.type_of_node(name.into()))
                .unwrap_or_else(|| Ty::new(db, TyKind::Unknown));
            let text = ty.display(db).to_string();
            write_guest_string(&mut caller, &text)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    linker.func_wrap(
        "mitki_stage",
        "function_return_type_name",
        |mut caller: Caller<'_, StageRuntimeStore>, function_bits: i32| -> wasmtime::Result<i32> {
            let db = caller.data().db();
            let function = decode_function_id(db, function_bits)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))?;
            let hir_function = function.hir_function(db).function(db);
            let inference = function.infer(db);
            let ty = inference
                .type_of_node(hir_function.body())
                .unwrap_or_else(|| Ty::new(db, TyKind::Tuple(Vec::new())));
            let text = ty.display(db).to_string();
            write_guest_string(&mut caller, &text)
                .map_err(|error| wasmtime::Error::msg(error.to_string()))
        },
    )?;
    Ok(())
}

fn read_canonical_blob_from_store<T>(
    memory: &wasmtime::Memory,
    store: &wasmtime::Store<T>,
    ptr: i32,
) -> anyhow::Result<Vec<u8>> {
    let start =
        usize::try_from(ptr).map_err(|_error| anyhow!("canonical blob pointer was negative"))?;
    let bytes = memory.data(store);
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

fn decode_type_id<'db>(db: &'db dyn salsa::Database, bits: i32) -> anyhow::Result<Ty<'db>> {
    let _ = db;
    let bits = u32::try_from(bits).map_err(|_error| anyhow!("type id must be non-negative"))?;
    Ok(Ty::from_id(salsa::Id::from_bits(u64::from(bits))))
}

fn decode_function_id<'db>(
    db: &'db dyn salsa::Database,
    bits: i32,
) -> anyhow::Result<FunctionLocation<'db>> {
    let _ = db;
    let bits = u32::try_from(bits).map_err(|_error| anyhow!("function id must be non-negative"))?;
    Ok(FunctionLocation::from_id(salsa::Id::from_bits(u64::from(bits))))
}

fn write_guest_string<T: AllocatingStore>(
    caller: &mut Caller<'_, T>,
    value: &str,
) -> anyhow::Result<i32> {
    let bytes = value.as_bytes();
    let size = 4u64
        .checked_add(u64::try_from(bytes.len()).expect("string length should fit in u64"))
        .ok_or_else(|| anyhow!("string write overflowed the address space"))?;
    let ptr = alloc_caller_region(caller, size, ARC_ALIGN as u64)?;
    let memory = guest_memory(caller)?;
    let ptr_usize =
        usize::try_from(ptr).map_err(|_error| anyhow!("string write exceeded usize memory"))?;
    let len = u32::try_from(bytes.len()).map_err(|_error| anyhow!("string result is too large"))?;
    memory
        .write(&mut *caller, ptr_usize, &len.to_le_bytes())
        .context("failed to write string length")?;
    memory.write(&mut *caller, ptr_usize + 4, bytes).context("failed to write string bytes")?;
    Ok(ptr)
}

fn i32_to_handle(raw: i32) -> anyhow::Result<u32> {
    u32::try_from(raw).map_err(|_error| anyhow!("handle id was negative"))
}

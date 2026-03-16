pub(super) mod function_codegen_ir;
pub(super) mod function_kernel;
pub(super) mod function_legalize;
pub(super) mod function_ownership;
pub(super) mod function_wasm_ir;
pub(super) mod wrapper_mir;

use super::{boundary, planning as plan, *};

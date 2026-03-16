pub(super) mod boundary;
pub(super) mod function;
pub(super) mod module;

use mitki_errors::Diagnostic;
use wasm_encoder::{BlockType, Function as WasmFunction, Instruction, ValType};

pub(super) use self::function as backend_ir;
use super::boundary::BoundarySig;
use super::lowering::wrapper_mir;
use super::{planning as plan, registry, validation as validate, *};

//! IR:
//!   - Stack-based IR.
//!   - External scope for locals.
//!     - May start with data (function arguments).
//!     - Should always end empty (linear types).
//!   - Generic instructions (do not store the data types).
//!     - Collapsed when validating and/or code generation.

pub use self::{instr::Instruction, scope::LocalScope};

mod instr;
mod scope;

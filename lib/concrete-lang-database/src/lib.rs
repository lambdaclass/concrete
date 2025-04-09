//! Database:
//!   - SQL tables
//!     - Items
//!       - Types
//!       - Traits (TODO: trait items)
//!       - Structs (TODO: fields)
//!       - Enums (TODO: variants)
//!       - Unions (TODO: fields/variants)
//!       - Functions (TODO: arguments)
//!       - Ffi declarations (TODO: arguments)
//!     - Impl blocks
//!   - IR as a BLOB
//!   - Generics:
//!     - Separate table, a row per generic?
//!   - Constraints:
//!     - Separate table, a row per constraint.
//!     - Can be checked using SQL queries.
//!   - Keys are item paths.
//!
//! Queries:
//!   - Find (global) symbol by name:
//!     - Supported: Requires reverse path index.
//!   - Find all symbols in (global) scope:
//!     - Supported: Forward path index.
//!   - Find traits meeting a criterion:
//!     - ???
//!
//!
//!
//! Scope checks: IR-checked
//! Borrow checker: IR-checked
//! Linearity checks: IR-checked
//!
//! Trait solver: Database
//!   - Generic impls (`impl<T> X for T`).
//!   - Concrete impls (`impl X for T`).
//!
//! Type checker:
//!   - IR: Actual code.
//!   - DB: Traits, cycles...

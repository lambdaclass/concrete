# Concrete Language Implementation Plan

This document outlines the implementation plan for adding features described in the [Concrete Programming Language: Systems Programming for Formal Reasoning](https://federicocarrone.com/series/concrete/the-concrete-programming-language-systems-programming-for-formal-reasoning/) article.

## Overview of Features

### Type System
| Feature | Description | Current Status |
|---------|-------------|----------------|
| **Strict linearity** | Values must be used *exactly* once (not "at most" like Rust) | Partial - `linearity_check` exists but disabled |
| **Copy marker** | Explicit `Copy` types that can't contain linear fields | Not implemented |
| **Capabilities** | `with(Alloc)`, `with(Std)`, `with(Clock)` effect tracking | Not implemented |
| **Pure by default** | Functions without capabilities are pure | Not implemented |

### Memory Management
| Feature | Description |
|---------|-------------|
| **`destroy(x)`** | Explicit destruction for linear types |
| **`defer`** | LIFO cleanup scheduling |
| **Explicit allocators** | `Alloc` capability with allocator binding |
| **Region-based borrowing** | `borrow x as xref in R { ... }` syntax |

### Error Handling
| Feature | Description |
|---------|-------------|
| **`Result<T, E>`** | With `?` operator propagation |
| **No panic** | Only `abort()` for catastrophic failures |

### Compilation
| Feature | Description |
|---------|-------------|
| **LL(1) grammar** | Simplify parser for formal reasoning |
| **Kernel IR + Lean** | Verified core type system |
| **C backend** | For portability |
| **WebAssembly backend** | For browser/edge |

---

## Difficulty Ranking (1-10 scale)

| Feature | Difficulty | LOC Estimate | Dependencies | Rationale |
|---------|-----------|--------------|--------------|-----------|
| **`defer` statement** | 2/10 | ~300 | None | Simple statement, LIFO cleanup in codegen |
| **`destroy(x)` function** | 3/10 | ~400 | Linearity check | Explicit consumption marker |
| **Copy type marker** | 4/10 | ~600 | Type system | Add attribute, skip linearity for Copy types |
| **Complete linearity check** | 5/10 | ~800 | Copy marker | Return validation, match exprs, enable module |
| **`Result<T,E>` + `?` operator** | 5/10 | ~1000 | Enums working | Sugar syntax, error propagation transform |
| **Capability system (`with()`)** | 6/10 | ~1500 | None | New AST nodes, inference, checking pass |
| **Explicit allocators** | 6/10 | ~1200 | Capabilities | `Alloc` capability + allocator parameter |
| **Region-based borrowing** | 7/10 | ~2000 | Linearity | Lexical regions, borrow checker rewrite |
| **LL(1) grammar rewrite** | 7/10 | ~2500 | None | Full parser replacement (recursive descent) |
| **WebAssembly backend** | 7/10 | ~2000 | None | MLIR WASM dialect, memory model |
| **C backend** | 8/10 | ~3000 | None | Full IR→C translation, external toolchain |
| **Kernel IR + Lean proofs** | 10/10 | ~5000+ | Everything | Formal semantics, proof engineering |

---

## Detailed Implementation Plan

### Phase 1: Foundation

#### 1.1 `defer` Statement
**Difficulty: 2/10 | ~300 LOC**

```
Files to modify:
├── src/parser/tokens.rs        (+5)   Add KeywordDefer token
├── src/parser/lexer.rs         (+3)   Add "defer" keyword
├── src/grammar.lalrpop         (+15)  DeferStmt rule
├── src/ast/statements.rs       (+12)  DeferStmt struct
├── src/ir/mod.rs               (+20)  IR defer representation
├── src/ir/lowering/statements.rs (+50) Lower defer to IR
├── src/codegen/compiler.rs     (+150) Generate cleanup blocks (LIFO)
└── tests/                      (+50)  Test cases
```

**Implementation details:**
- Store deferred expressions in function-local stack
- At function exit/return, emit cleanup in reverse order
- Codegen: create cleanup basic blocks, branch through them before return

---

#### 1.2 `destroy(x)` Function
**Difficulty: 3/10 | ~400 LOC**

```
Files to modify:
├── src/parser/tokens.rs        (+5)   Add KeywordDestroy token
├── src/parser/lexer.rs         (+3)   Add "destroy" keyword
├── src/grammar.lalrpop         (+15)  DestroyExpr rule
├── src/ast/expressions.rs      (+10)  DestroyExpr struct
├── src/ir/mod.rs               (+15)  IR destroy statement
├── src/ir/lowering/expressions.rs (+80) Lower destroy, mark consumed
├── src/check/linearity_check.rs (+100) Treat destroy as consumption
├── src/codegen/compiler.rs     (+100) Call destructor if exists, else drop
└── tests/                      (+70)  Test cases
```

**Implementation details:**
- `destroy(x)` marks variable as consumed in linearity checker
- Codegen: if type has destructor, call it; otherwise no-op (just drops)
- Error if destroying already-consumed variable

---

#### 1.3 Copy Type Marker
**Difficulty: 4/10 | ~600 LOC**

```
Files to modify:
├── src/ast/structs.rs          (+15)  Add `is_copy: bool` to StructDecl
├── src/grammar.lalrpop         (+20)  Parse `copy struct Foo {}`
├── src/ir/mod.rs               (+25)  AdtBody.is_copy field
├── src/ir/lowering/structs.rs  (+60)  Validate Copy (no linear fields)
├── src/ir/lowering/errors.rs   (+20)  CopyContainsLinear error
├── src/check/linearity_check.rs (+200) Skip linearity for Copy types
├── src/check/mod.rs            (+30)  Type → is_copy lookup
└── tests/                      (+80)  Test cases
```

**Implementation details:**
- Syntax: `copy struct Point { x: i32, y: i32 }`
- Builtin Copy: all numeric primitives, bool, char
- Validation: Copy struct cannot contain non-Copy fields
- Linearity checker: exempt Copy types from consumption tracking

---

### Phase 2: Core Safety

#### 2.1 Complete Linearity Checker
**Difficulty: 5/10 | ~800 LOC**

```
Files to modify:
├── src/check/mod.rs            (+10)  Uncomment/enable module
├── src/check/linearity_check.rs:
│   ├── check_return()          (+80)  Validate all linears consumed
│   ├── check_match()           (+150) Match arm linearity
│   ├── check_bindings()        (+100) Destructuring patterns
│   ├── type integration        (+150) Query IR for is_copy
│   └── borrow states           (+200) Enable _Borrowed/_BorrowedMut
├── src/ir/lowering/lower.rs    (+30)  Call linearity check
├── src/driver/mod.rs           (+20)  Wire into pipeline
└── tests/invalid_programs/     (+100) Negative test cases
```

**Key fixes needed:**
1. `src/check/linearity_check.rs:723-750` - Uncomment return validation
2. `src/check/linearity_check.rs:799` - Replace `"Linear"` string check with type lookup
3. `src/check/linearity_check.rs:562-570` - Implement binding checks
4. `src/check/linearity_check.rs:759-761` - Implement match statement

---

#### 2.2 `Result<T,E>` with `?` Operator
**Difficulty: 5/10 | ~1000 LOC**

```
Files to modify:
├── stdlib/result.con           (+50)  Result enum definition
├── src/parser/tokens.rs        (+5)   QuestionMark token
├── src/grammar.lalrpop         (+25)  TryExpr (expr?)
├── src/ast/expressions.rs      (+15)  TryExpr struct
├── src/ir/mod.rs               (+40)  IR try operation
├── src/ir/lowering/expressions.rs (+200) Desugar ? to match
├── src/ir/lowering/prelude.rs  (+100) Load stdlib Result
├── src/codegen/compiler.rs     (+150) Generate branch on Result tag
└── tests/                      (+100) Test cases
```

**Desugaring:**
```rust
// expr? becomes:
match expr {
    Ok(v) => v,
    Err(e) => return Err(e),
}
```

**Prerequisites:** Enums and match must work (they partially do)

---

### Phase 3: Capabilities

#### 3.1 Capability System
**Difficulty: 6/10 | ~1500 LOC**

```
Files to create:
├── src/capabilities/mod.rs     (+200) Capability definitions
├── src/capabilities/check.rs   (+400) Capability inference & checking

Files to modify:
├── src/parser/tokens.rs        (+10)  KeywordWith token
├── src/grammar.lalrpop         (+40)  with(Cap) syntax, fn! shorthand
├── src/ast/functions.rs        (+25)  capabilities: Vec<Capability>
├── src/ast/expressions.rs      (+20)  WithExpr block
├── src/ir/mod.rs               (+50)  Function.required_capabilities
├── src/ir/lowering/functions.rs (+150) Propagate capabilities
├── src/ir/lowering/expressions.rs (+100) Check capability at call site
├── src/codegen/compiler.rs     (+50)  No codegen changes (compile-time only)
└── tests/                      (+150) Test cases
```

**Capability set:**
```rust
enum Capability {
    Std,      // General I/O (includes Alloc, Print, Clock)
    Alloc,    // Heap allocation
    Print,    // Console output
    Clock,    // Time access
    Fs,       // File system
    Net,      // Network
}
```

**Rules:**
- Functions without `with()` are pure (no capabilities)
- `fn main!()` is sugar for `fn main() with(Std)`
- Calling function with caps requires caller to have those caps
- Capabilities are static, not runtime values

---

#### 3.2 Explicit Allocators
**Difficulty: 6/10 | ~1200 LOC**

```
Files to create:
├── stdlib/alloc/mod.con        (+100) Allocator trait
├── stdlib/alloc/general.con    (+80)  GeneralPurposeAllocator
├── stdlib/alloc/arena.con      (+80)  Arena allocator
├── stdlib/alloc/fixed.con      (+80)  FixedBufferAllocator

Files to modify:
├── src/grammar.lalrpop         (+30)  Allocator binding syntax
├── src/ast/expressions.rs      (+25)  AllocExpr with allocator param
├── src/ir/mod.rs               (+40)  Allocation with allocator index
├── src/ir/lowering/expressions.rs (+200) Resolve allocator, check Alloc cap
├── src/codegen/compiler.rs     (+300) Generate allocator calls
└── tests/                      (+100) Test cases
```

**Syntax:**
```rust
fn create_list() with(Alloc) -> List<i32> {
    let alloc = GeneralPurposeAllocator#new();
    let list = List#new(alloc);  // Allocator passed explicitly
    list
}
```

---

### Phase 4: Advanced Memory

#### 4.1 Region-Based Borrowing
**Difficulty: 7/10 | ~2000 LOC**

```
Files to create:
├── src/regions/mod.rs          (+150) Region definitions
├── src/regions/inference.rs    (+400) Region inference algorithm
├── src/regions/check.rs        (+500) Region constraint solving

Files to modify:
├── src/parser/tokens.rs        (+10)  KeywordBorrow, KeywordIn
├── src/grammar.lalrpop         (+60)  borrow x as y in R {} syntax
├── src/ast/types.rs            (+40)  Region parameters in types
├── src/ast/expressions.rs      (+30)  BorrowExpr struct
├── src/ir/mod.rs               (+80)  Region tracking in IR
├── src/ir/lowering/expressions.rs (+300) Lower borrow expressions
├── src/check/linearity_check.rs (+300) Integrate with region checker
├── src/codegen/compiler.rs     (+100) Regions erased at codegen
└── tests/                      (+200) Test cases
```

**Key concepts:**
- Regions are lexical scopes, not lifetime parameters
- `borrow x as xref in R { ... }` creates reference valid in region R
- Anonymous borrows: `length(&x)` - region is the expression
- Multiple immutable borrows OK; mutable borrows exclusive

---

### Phase 5: Alternative Backends

#### 5.1 WebAssembly Backend
**Difficulty: 7/10 | ~2000 LOC**

```
Files to create:
├── src/codegen/wasm/mod.rs     (+200) WASM backend entry
├── src/codegen/wasm/types.rs   (+300) Type mapping to WASM
├── src/codegen/wasm/memory.rs  (+400) Linear memory management
├── src/codegen/wasm/functions.rs (+600) Function compilation
├── src/codegen/wasm/exports.rs (+150) Export section generation

Files to modify:
├── src/codegen/mod.rs          (+50)  Backend dispatch
├── src/driver/config.rs        (+20)  WASM target option
├── src/driver/mod.rs           (+80)  WASM compilation flow
├── Cargo.toml                  (+5)   wasm dependencies
└── tests/                      (+200) WASM test harness
```

**Approach:** Use MLIR's WASM dialect, or direct WASM emission via `wasm-encoder` crate

---

#### 5.2 C Backend
**Difficulty: 8/10 | ~3000 LOC**

```
Files to create:
├── src/codegen/c/mod.rs        (+200) C backend entry
├── src/codegen/c/types.rs      (+400) IR types → C types
├── src/codegen/c/expressions.rs (+600) Expression emission
├── src/codegen/c/statements.rs (+500) Statement emission
├── src/codegen/c/functions.rs  (+500) Function generation
├── src/codegen/c/headers.rs    (+200) Header generation
├── src/codegen/c/driver.rs     (+300) Invoke C compiler

Files to modify:
├── src/codegen/mod.rs          (+50)  Backend dispatch
├── src/driver/config.rs        (+30)  C target options
├── src/driver/mod.rs           (+100) C compilation flow
└── tests/                      (+200) C backend tests
```

**Challenges:**
- Must preserve exact memory layout
- Control flow translation (IR terminators → C if/goto)
- Struct field ordering and alignment

---

### Phase 6: Parser Rewrite (Optional)

#### 6.1 LL(1) Grammar
**Difficulty: 7/10 | ~2500 LOC**

```
Files to replace:
├── src/parser/mod.rs           (rewrite) Recursive descent parser
├── src/parser/lexer.rs         (~same)   Keep Logos lexer
├── src/grammar.lalrpop         (delete)  Remove LALRPOP

Files to create:
├── src/parser/recursive.rs     (+1500) Recursive descent implementation
├── src/parser/pratt.rs         (+400)  Pratt parser for expressions
├── src/parser/grammar_spec.md  (+200)  LL(1) grammar documentation
```

**Rationale:** Required for formal verification (predictable parsing)

---

### Phase 7: Formal Verification (Long-term)

#### 7.1 Kernel IR + Lean Proofs
**Difficulty: 10/10 | ~5000+ LOC**

```
New components:
├── kernel/                     Lean 4 project
│   ├── Kernel/IR.lean          (+500)  IR definitions in Lean
│   ├── Kernel/Types.lean       (+400)  Type system formalization
│   ├── Kernel/Linearity.lean   (+600)  Linearity proofs
│   ├── Kernel/Capabilities.lean (+500) Capability soundness
│   └── Kernel/Theorems.lean    (+1000) Safety theorems

├── src/kernel_ir/mod.rs        (+800)  Kernel IR extraction
├── src/kernel_ir/check.rs      (+600)  Call Lean checker
```

**This is a multi-year research project** - requires:
- Formal semantics definition
- Type soundness proofs
- Linearity preservation proofs
- Capability safety proofs

---

## Recommended Implementation Order

```
Phase 1: Foundation
  1. defer           (2/10)  ─┐
  2. destroy         (3/10)  ─┼─ Can be done in parallel
  3. Copy marker     (4/10)  ─┘

Phase 2: Core Safety
  4. Complete linearity (5/10) ← Depends on 2, 3
  5. Result + ?      (5/10)     ← Can parallelize with 4

Phase 3: Capabilities
  6. Capability system (6/10) ← Independent
  7. Explicit allocators (6/10) ← Depends on 6

Phase 4: Advanced Memory
  8. Region borrowing (7/10) ← Depends on 4

Phase 5: Backends (can parallelize)
  9. WebAssembly    (7/10) ─┐
  10. C backend     (8/10) ─┴─ Independent of each other

Phase 6: Parser (optional)
  11. LL(1) rewrite (7/10) ← Can be done anytime

Phase 7: Verification (long-term)
  12. Kernel IR + Lean (10/10) ← Depends on stable language design
```

---

## Dependency Graph

```
                    ┌─────────────┐
                    │   defer     │
                    │   (2/10)    │
                    └─────────────┘
                           │
                           ▼
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   destroy   │───▶│  Complete   │◀───│    Copy     │
│   (3/10)    │    │  Linearity  │    │   marker    │
└─────────────┘    │   (5/10)    │    │   (4/10)    │
                   └─────────────┘    └─────────────┘
                          │
                          ▼
                   ┌─────────────┐
                   │   Region    │
                   │  Borrowing  │
                   │   (7/10)    │
                   └─────────────┘

┌─────────────┐    ┌─────────────┐
│ Capability  │───▶│  Explicit   │
│   System    │    │ Allocators  │
│   (6/10)    │    │   (6/10)    │
└─────────────┘    └─────────────┘

┌─────────────┐    ┌─────────────┐
│    WASM     │    │  C Backend  │
│  Backend    │    │   (8/10)    │
│   (7/10)    │    └─────────────┘
└─────────────┘

┌─────────────┐    ┌─────────────┐
│ Result + ?  │    │ LL(1) Parser│
│   (5/10)    │    │   (7/10)    │
└─────────────┘    └─────────────┘

                   ┌─────────────┐
                   │ Kernel IR + │
                   │    Lean     │
                   │  (10/10)    │
                   └─────────────┘
```

---

## Current Codebase Status

### Linearity Checker (`src/check/linearity_check.rs`)
- **Status:** Partially implemented but **disabled**
- **Working:** Variable state tracking, decision table for consumption
- **Missing:** Return validation, match expressions, Copy type integration, borrow states
- **Key issue:** Only checks types literally named "Linear" (line 799)

### Type System (`src/ast/types.rs`, `src/ir/mod.rs`)
- **Status:** Functional with generics
- **Working:** Monomorphization, basic types, structs, enums
- **Missing:** Capabilities, Result/Option sugar, trait bounds

### Parser (`src/grammar.lalrpop`)
- **Status:** LR(1) via LALRPOP (not LL(1))
- **Working:** Full expression hierarchy, 6 precedence levels
- **Missing:** `defer`, `destroy`, `with()`, `borrow` syntax

### Codegen (`src/codegen/`)
- **Status:** MLIR/LLVM backend working
- **Working:** Functions, control flow, memory operations
- **Missing:** Alternative backends, defer cleanup blocks

---

## Notes

- LOC estimates include tests and documentation
- Difficulty ratings assume familiarity with the codebase
- Phases can overlap; parallel work is possible on independent features
- The Kernel IR + Lean phase is a research project, not typical engineering work

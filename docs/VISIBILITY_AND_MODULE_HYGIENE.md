# Visibility and Module Hygiene

Status: stable reference (Phase 3, items 73 and 78)

This document defines Concrete's visibility model and the module hygiene guarantees that must hold before stdlib freeze and package management. It covers the current state, the target rules, what the compiler must enforce, and how to validate that modules expose clean, stable interfaces.

For the language identity, see [IDENTITY.md](IDENTITY.md).
For anti-features, see [ANTI_FEATURES.md](ANTI_FEATURES.md).
For the stdlib design principles, see [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md).
For the core/hosted boundary, see [HOSTED_STDLIB_SPLIT.md](HOSTED_STDLIB_SPLIT.md).
For the stdlib API audit, see [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md).

---

## Part 1: Visibility Rules (Item 73)

### 1. Current State

The compiler already has a working visibility system with two levels:

**`pub` (public)**. Applied to top-level declarations: `pub fn`, `pub struct`, `pub enum`, `pub trait`, `pub const`, `pub type`, `pub cap`, `pub newtype`, `pub extern fn`. Inside `impl` and `impl Trait for` blocks, individual methods can be marked `pub`. The parser (`Parser.lean` lines 1712-1819) reads the `pub` keyword and sets `isPublic := true` on the declaration's AST node.

**No modifier (private)**. The default. Declarations without `pub` are module-private. They do not appear in `FileSummary.publicNames` and cannot be imported by other modules.

**Enforcement is real.** Three passes enforce visibility:

1. **Resolve** (`Resolve.lean`): builds `fullExportTable` from `publicNames` per module. When an import names a symbol, the resolver checks `pubNames.contains sym.name`. If it fails, the pass emits `E0111: 'X' is not public in module 'Y'`.
2. **Check** (`Check.lean`): independently validates imports against the summary table. Emits `E0285: 'X' is not public in module 'Y'`.
3. **Elab** (`Elab.lean`): validates imports a third time during elaboration. Emits `E0419: 'X' is not public in module 'Y'`.

The `FileSummary` (`FileSummary.lean`) computes `publicNames` from all declaration kinds filtered by `isPublic`. Cross-module import resolution (`resolveImports`) only exposes public functions, structs, enums, type aliases, and public impl methods to importers.

The existing test `adversarial_neg_private_fn_call.con` confirms that importing a non-pub function produces a compile error.

**What is NOT enforced today:**

- There is no `pub(crate)` or `internal` visibility. Every declaration is either fully public or fully private.
- Struct fields have no visibility modifier. `StructField` in `AST.lean` has only `name` and `ty` -- no `isPublic` flag. All fields of a public struct are accessible to any code that can see the struct.
- Enum variant fields have no visibility modifier. `EnumVariant` has `name` and `fields` with no visibility control.
- Trait methods have no per-method visibility. All methods in a `pub trait` are implicitly public.
- Submodule declarations (`mod X;`) have no visibility modifier. All submodules are visible to the parent module's scope.

### 2. Target Visibility Model

Concrete's visibility model has three levels:

| Modifier | Meaning | Scope |
|----------|---------|-------|
| *(none)* | Private | Visible only within the declaring module |
| `pub` | Public | Visible to all importers, including external packages |
| `pub(pkg)` | Package-internal | Visible within the same package but not to external importers |

**Why `pub(pkg)` instead of `pub(crate)`:** Concrete does not have crates. Concrete's compilation unit is a *package* defined by `Concrete.toml`. The modifier name should match Concrete's terminology. `pub(pkg)` is shorter than `pub(package)` and unambiguous.

**Why not `internal`:** A keyword-based modifier (`internal fn helper()`) requires a new reserved word and creates asymmetry with `pub`. The parenthesized form `pub(pkg)` makes it visually clear that this is a restricted form of `pub`, not a third category.

**Why three levels, not two:** Two levels (`pub`/private) are sufficient for single-package programs. But once packages can depend on other packages, a module often needs helpers that are visible across modules within the package but not exported to dependents. Without `pub(pkg)`, authors must choose between making helpers fully public (polluting the package interface) or duplicating code across modules (violating DRY). Rust's ecosystem confirms that `pub(crate)` is heavily used in practice.

**When `pub(pkg)` is needed:** Not until package management lands. The first release can ship with `pub` and private only, and add `pub(pkg)` when the package model is implemented. The design is documented here so that the `pub` keyword's semantics do not need to change later -- `pub` always means "public to all importers," and `pub(pkg)` is a strict restriction of `pub`.

### 3. Default Visibility: Private

All declarations are private by default. The `pub` modifier must be written explicitly to make anything visible outside its module.

**Rationale:**
- Matches Rust (private by default) and Zig (pub is opt-in). Avoids Go's implicit-export-via-casing, which Concrete already rejected by not using case-based semantics.
- Private-by-default means the public API is always intentional. A missing `pub` cannot accidentally export an internal helper.
- Aligns with the stdlib design principle "no hidden magic" (Principle 5). If something is public, the keyword says so.

**This is already implemented.** The parser sets `isPublic := false` as the default for every declaration kind. No change needed.

### 4. Struct Field Visibility

**Current state:** Struct fields have no visibility modifier. All fields of a `pub struct` are fully accessible to any module that imports the struct. This includes:
- Direct field access: `p.x`
- Struct literal construction: `Pair { x: 1, y: 2 }`
- Pattern matching on struct fields in `match` arms

**Target: fields are public when the struct is public.**

For the first release, Concrete does not add per-field visibility. The reasons:

1. **Concrete has no constructors.** Structs are constructed with struct literal syntax (`Pair { x: 1, y: 2 }`). If fields were private, external code could not construct the struct at all without a factory function. This is the right design for encapsulated types, but it is a pattern that requires `impl` methods like `new()` on every struct -- which the stdlib already provides for complex types (`Vec::new()`, `String::new()`).

2. **The stdlib already uses the right pattern.** Types that need encapsulation (`Vec`, `String`, `HashMap`) have raw-pointer fields (`ptr: *mut T`) that should never be accessed directly. These types are inside `trusted impl` blocks, and all access goes through methods. The encapsulation is enforced by convention and by the `trusted` boundary, not by field visibility.

3. **Copy structs are value types.** `pub struct Copy Pair { x: Int, y: Int }` is a plain data type. Making its fields private would be counterproductive -- the whole point of a Copy struct is transparent data.

4. **Pattern matching depends on field access.** `match` on struct fields (e.g., `match p { Pair { x, y } => ... }`) requires field visibility. Private fields would make destructuring impossible without visibility escape hatches.

**Future direction:** When package management arrives and the ecosystem grows, per-field visibility may become necessary for types like `String` where the raw pointer fields should be inaccessible to external code. The design would be:

```
pub struct String {
    ptr: *mut u8,      // private (no modifier) -- only visible within this module
    len: u64,          // private
    pub cap: u64,      // hypothetical: public field
}
```

This is deferred. The first release ships with all-fields-visible for public structs. The `trusted impl` pattern provides sufficient encapsulation today.

### 5. Enum Variants

**Target: if the enum is public, all its variants are public.**

Rationale:
- Concrete enums are closed (no trait objects, no open dispatch). The caller must handle all variants in `match`. If some variants were hidden, exhaustiveness checking would be impossible or would require wildcard arms that defeat the purpose.
- Rust makes all variants of a `pub enum` public. This is the correct default for closed sum types.
- OCaml similarly exports all constructors when a type is in the module interface.

**Enum variant fields follow the same rule as struct fields:** all fields within a variant are accessible. This is necessary for pattern matching:

```
match result {
    Result::Ok { value } => { ... },
    Result::Err { error } => { ... },
}
```

If variant fields were private, destructuring would be impossible.

**No per-variant visibility.** This is a permanent decision for Concrete. The language's core commitment to exhaustive matching and static knowledge of all code paths requires that all variants of a visible enum are visible.

### 6. Trait Methods

**Target: all methods of a public trait are public.**

Rationale:
- A trait defines a behavioral contract. If method `M` is in trait `T`, then every `impl T for X` must implement `M`. Hiding `M` from callers while requiring implementors to provide it is incoherent.
- Rust makes all trait methods public. Zig does not have traits. Go makes all interface methods public. No mainstream language has per-method visibility on interface/trait types.
- A "partially public" trait would create confusion: can you call `x.hidden_method()` if you have the concrete type but not the trait in scope? This kind of question has no good answer.

**Methods in `impl` blocks (not trait impls) can be individually `pub` or private.** This is already implemented -- the parser reads `pub fn` inside `impl` blocks, and `buildFileSummary` filters impl methods by `isPublic`. A private impl method is usable only within the same module.

### 7. Required Compiler Changes

The following changes are needed to complete the visibility model for the first release. None are blocking -- the existing enforcement is functional and correct for the two-level (`pub`/private) system.

#### 7.1 Changes for first release (required)

**None.** The current system is complete for a two-level visibility model:
- `pub` and private-by-default are implemented and enforced across three passes.
- Struct fields are all-visible, which is the intended first-release behavior.
- Enum variants are all-visible, which is the permanent behavior.
- Trait methods are all-visible, which is the permanent behavior.
- The `--report interface` command already shows the public API of each module.

#### 7.2 Changes for package management (deferred)

When package management is implemented, the following changes complete the visibility model:

1. **Add `pub(pkg)` to the parser.** Recognize `pub(pkg)` as a visibility modifier. This requires:
   - A new `Visibility` enum in `AST.lean`: `private | public | packageInternal`
   - Replacing `isPublic : Bool` with `visibility : Visibility` on all declaration types
   - Parser changes to recognize `pub(pkg)` after `pub` + `(` + `pkg` + `)`
   - `FileSummary` must track two export lists: `publicNames` (visible to all) and `packageNames` (visible within the package)
   - `resolveImports` must distinguish same-package and cross-package imports

2. **Add per-field visibility to `StructField`.** When the ecosystem needs it:
   - Add `isPublic : Bool` to `StructField` in `AST.lean`
   - Parser reads `pub` before field declarations in struct bodies
   - Check/Elab reject field access and struct literal construction for private fields from outside the declaring module
   - Pattern matching on private fields from outside the module produces an error

3. **Track package identity.** Each module must know which package it belongs to, so that `pub(pkg)` can be checked. This requires the package model (item from Phase J).

---

## Part 2: Module Hygiene (Item 78)

### 1. Interface Drift Detection

**Problem:** A module's public API can change silently. Adding `pub` to a helper function, removing a public method, or changing a function signature all modify the interface without any explicit review gate. In a language that values auditability, the public interface should be trackable and diffable.

**Current state:** The `--report interface` command (implemented in `Report.lean` lines 502-571) already produces a human-readable summary of every module's public API:

```
=== Interface Summary ===

module std (38 exports):
  public API:
    fn compute(a: Int, b: Int) -> Int  [caps: (pure)]
    struct Pair { x: Int, y: Int }
    enum Option<T> { Some, None }
    ...

Totals: 5 modules, 42 public exports
```

This report lists public functions with parameter types and return types, public structs with their fields, public enums with their variants, public traits with their methods, public constants, type aliases, and newtypes.

**Target: machine-readable export snapshots for CI.**

The `--report interface` output is already deterministic (same input produces same output). To enable CI-based drift detection:

1. **`--report exports`** -- a new report mode that emits one line per public symbol in a machine-parseable format:

   ```
   pub fn std.vec.Vec_new<T>() -> Vec<T>  [caps: (pure)]
   pub fn std.vec.Vec_push<T>(&mut self, value: T)  [caps: Alloc]
   pub struct std.vec.Vec<T> { ptr: *mut T, len: u64, cap: u64 }
   pub enum Option<T> { Some { value: T }, None }
   pub trait std.hash.Hash { hash }
   ```

   Each line is: `pub <kind> <qualified-name><generics>(<signature>)  [caps: <capset>]`

2. **Snapshot workflow:**
   - `concrete build --report exports > exports.snapshot` generates the current snapshot.
   - CI compares `exports.snapshot` against the checked-in version.
   - Any diff triggers a review: the author must acknowledge the interface change explicitly.
   - This is the same pattern as Rust's `cargo public-api` or Go's `apidiff` tool.

3. **No language-level interface files.** Concrete does not adopt OCaml's `.mli` approach of separate interface files. The interface is derived from the source (declarations marked `pub`), not maintained separately. Separate interface files create synchronization burden and drift of their own. The snapshot-and-diff approach provides the same protection without a second source of truth.

### 2. Namespace Pollution

**Problem:** When module A imports from module B, does it get everything in B or only the `pub` items?

**Current state: only `pub` items are importable.** This is already enforced.

The import system is explicit and selective:

```
import std.vec.{ Vec };
import std.option.{ Option };
```

There is no wildcard import (`import std.vec.*`). Every imported symbol is named. The resolver checks each named symbol against `publicNames` and rejects non-public symbols with `E0111`.

A module's non-public items (private functions, internal helpers, unexported structs) are invisible to importers. They do not appear in `FileSummary.publicNames`, are not included in `resolveImports`, and cannot be named in import lists.

**What this means in practice:**

```
mod Secret {
    fn hidden() -> Int { return 42; }      // private
    pub fn visible() -> Int { return 1; }  // public
}

mod Main {
    import Secret.{ hidden };  // ERROR: E0111 'hidden' is not public in module 'Secret'
    import Secret.{ visible }; // OK
}
```

**No glob imports.** Concrete requires every imported symbol to be named. This is a permanent design decision:
- Glob imports (`use module::*` in Rust, `from module import *` in Python) make it impossible to determine where a name comes from without checking every imported module.
- Named imports create a manifest at the top of each module listing every external dependency.
- This aligns with "every dependency is visible" (Principle 8 from STDLIB_DESIGN_PRINCIPLES.md).

**Submodule visibility:** When a parent module declares `mod child;`, the parent can access the child's `pub` items via qualified syntax (`child::fn_name()` parsed as `child_fn_name`). The child's non-public items remain inaccessible. This is enforced by `buildGlobalScopeFromSummary` in `Resolve.lean`, which filters submodule items through `subS.publicNames.contains name`.

### 3. Capability Exposure Across Module Boundaries

**Problem:** If module B internally uses `with(File)` and module A calls a function from B, does A need `with(File)`?

**Current state: capabilities propagate honestly.** This is already enforced by the capability checker.

The rule is simple: a function can only call functions whose capabilities are a subset of its own. If `B.write_data()` has `with(File)`, then any caller must also have `with(File)` (or a superset). The capability requirement is visible in the function signature and checked at every call site.

**The containment pattern:** Hosted stdlib modules use `trusted` wrappers to contain `Unsafe`:

```
// In std.io
trusted impl TextFile {
    pub fn write(&self, data: &String) with(Unsafe) { ... }
}
```

The `Unsafe` is visible in the signature. A caller that does not have `with(Unsafe)` cannot call `write` directly. Higher-level wrappers expose domain capabilities:

```
pub fn println(s: &String) with(Console) { ... }
```

Here `Console` is the public-facing capability. The internal `Unsafe` calls to libc `write()` are contained inside the `trusted` implementation.

**What modules must NOT do:**

1. **Smuggle capabilities.** A module must not hide a `with(File)` call behind a `with(Alloc)` signature. The capability checker prevents this: if `f` calls `g` and `g` requires `File`, then `f` must also declare `File`.

2. **Force unnecessary capabilities.** A public function that does not need `File` should not declare `with(File)` just because a private helper in the same module uses it. Each function declares exactly the capabilities it needs.

**Audit tools:**
- `--report caps` shows the capability set of every function.
- `--report authority` shows transitive capability propagation.
- `--report interface` shows capabilities on every public function signature.
- Policy enforcement (`[policy] deny = ["Unsafe"]` in `Concrete.toml`) can reject packages that use specific capabilities.

### 4. Validation Example: Parser Core + IO Shell Split

The following example demonstrates that Concrete's module system enforces clean boundaries between a pure computation core and an I/O shell. It proves that internal helpers stay internal, only the public API is importable, and capability requirements are explicit at the boundary.

#### Design

```
// File: src/main.con

// Pure parsing core -- no capabilities, no I/O
mod ParserCore {
    import std.option.{ Option };

    pub struct Copy Token {
        kind: i32,     // 1 = number, 2 = plus, 3 = minus
        value: i32,
    }

    pub struct Copy ParseResult {
        ok: bool,
        value: i32,
    }

    // INTERNAL: tokenize a single character -- not pub, not importable
    fn classify(ch: char) -> Token {
        if ch == '+' {
            return Token { kind: 2, value: 0 };
        }
        if ch == '-' {
            return Token { kind: 3, value: 0 };
        }
        // Assume digit
        let digit: i32 = (ch as i32) - 48;
        return Token { kind: 1, value: digit };
    }

    // INTERNAL: evaluate a token sequence -- not pub, not importable
    fn eval_tokens(a: Token, op: Token, b: Token) -> i32 {
        if op.kind == 2 {
            return a.value + b.value;
        }
        return a.value - b.value;
    }

    // PUBLIC: the only entry point for external modules
    pub fn parse_expr(a: char, op: char, b: char) -> ParseResult {
        let ta: Token = classify(a);
        let top: Token = classify(op);
        let tb: Token = classify(b);
        let result: i32 = eval_tokens(ta, top, tb);
        return ParseResult { ok: true, value: result };
    }
}

// I/O shell -- uses Console capability
mod IOShell {
    import ParserCore.{ ParseResult, parse_expr };
    // import ParserCore.{ classify };     -- would fail: E0111, not public
    // import ParserCore.{ eval_tokens };  -- would fail: E0111, not public

    pub fn run() with(Console) {
        let result: ParseResult = parse_expr('3', '+', '4');
        if result.ok {
            println(result.value);
        }
    }
}

// Entry point -- only needs Console because IOShell::run needs Console
// Does NOT need Unsafe, File, Network, etc.
mod Main {
    import IOShell.{ run };

    pub fn main() with(Console) {
        run();
    }
}
```

#### What this proves

1. **Internal helpers stay internal.** `classify` and `eval_tokens` are private to `ParserCore`. Uncommenting the import lines would produce compile errors (`E0111`). The `--report interface` output for `ParserCore` would show only `parse_expr`, `Token`, and `ParseResult` -- not the helper functions.

2. **Only the public API is importable.** `IOShell` can import `parse_expr`, `ParseResult`, and `Token` because they are `pub`. It cannot import anything else from `ParserCore`.

3. **Capability requirements are explicit at the boundary.**
   - `ParserCore` has no capabilities -- all functions are pure. `--report caps` would show `caps: (pure)` for `parse_expr`.
   - `IOShell.run` needs `Console` because it calls `println`. This appears in its signature.
   - `Main.main` needs `Console` because it calls `IOShell.run`. The capability propagates honestly.
   - No function in this program needs `File`, `Network`, `Unsafe`, or `Alloc`. The absence of these capabilities is verifiable via `--report authority`.

4. **The module boundary is the encapsulation boundary.** `ParserCore` encapsulates its tokenization strategy. If `classify` is refactored to return different token kinds, `IOShell` is unaffected because it never sees `classify`. The only contract is the `parse_expr` signature and the `ParseResult` / `Token` struct definitions.

### 5. Pre-Package Hygiene Checklist

The following conditions must hold before package management can work correctly. Each is either already true or has a clear implementation path.

#### Already enforced

| Condition | How it is enforced |
|-----------|-------------------|
| Private declarations are invisible to importers | Resolve, Check, Elab all check `publicNames` |
| Imports are explicit and selective | No glob imports; every symbol is named |
| Capability requirements propagate honestly | Capability subset checking at every call site |
| Public API is machine-reportable | `--report interface` produces deterministic output |
| Non-public impl methods are excluded from imports | `buildFileSummary` filters methods by `isPublic` |
| Cross-module struct access requires pub struct | Only `pub struct` appears in `publicNames` |
| Cross-module function calls require pub fn | Only `pub fn` appears in `publicNames` |
| Import of non-public symbol produces a clear error | Three error codes: E0111, E0285, E0419 |

#### Required before package management

| Condition | Current state | Work needed |
|-----------|--------------|-------------|
| `pub(pkg)` visibility modifier | Not implemented | Parser, AST, FileSummary, resolve/check/elab changes |
| Package identity tracked per module | Not implemented | Package model from Phase J |
| Cross-package imports resolve `pub` only (not `pub(pkg)`) | N/A (single-package today) | Import resolver must distinguish same-package vs cross-package |
| `--report exports` machine-readable snapshot | `--report interface` exists but is human-oriented | Add a machine-parseable export format |
| Internal stdlib modules (`libc`, `ptr`) are unexportable | Currently importable by user code | Mark as `pub(pkg)` or equivalent once the modifier exists |
| Stable export snapshot checked in CI | Not implemented | CI workflow: generate snapshot, diff against committed version |

#### Recommended before stdlib freeze

| Condition | Why |
|-----------|-----|
| Audit every `pub` in `std/src/*.con` | Ensure no internal helper is accidentally public |
| `--report interface` output reviewed for each stdlib module | Confirm the public surface matches STDLIB_TARGET.md |
| `libc` and `ptr` marked non-public | Per STDLIB_API_REVIEW.md recommendation 20 |
| No new `pub` items added to stdlib without review | The interface snapshot workflow catches this |

---

## Comparison With Other Languages

### Rust

**Visibility:** `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`, and private (no modifier). Private is the default. Items in a module are visible to all ancestors (parent modules can access child private items).

**Concrete's differences:**
- Concrete does not allow parent modules to access child private items. Privacy is strict: private means "this module only."
- Concrete does not have `pub(super)` or `pub(in path)`. The three-level model (`pub`/`pub(pkg)`/private) is deliberately simpler. `pub(super)` addresses a narrow use case (helper visible to the parent but not the grandparent) that Concrete handles by restructuring modules instead.
- Rust's struct fields are private by default and individually markable as `pub`. Concrete's struct fields are all-visible (first release), with per-field visibility deferred.
- Rust's enum variants are all public if the enum is public. Concrete matches this.
- Rust's trait methods are all public. Concrete matches this.

### Go

**Visibility:** Uppercase first letter = exported, lowercase = unexported. Package-scoped: all files in a package can see all symbols.

**Concrete's differences:**
- Concrete uses a keyword (`pub`), not naming convention. This avoids coupling visibility to identifier names and makes visibility greppable.
- Go has no sub-package visibility. Everything is either package-public or package-private. Concrete's `pub(pkg)` achieves a similar effect to Go's package-level visibility.
- Go has no cross-package internal visibility (`internal/` directory convention is a tooling hack, not a language feature). Concrete's `pub(pkg)` is a language-level mechanism.

### Zig

**Visibility:** `pub` for public, no modifier for file-private. No intermediate visibility level.

**Concrete's differences:**
- Zig's `pub` is equivalent to Concrete's `pub`.
- Zig has no `pub(crate)`-equivalent. When Zig projects need internal-but-not-exported symbols, they use file-level organization.
- Zig's struct fields are all public. Concrete matches this for the first release.
- Zig does not have traits, so the trait method visibility question does not arise.

### OCaml

**Visibility:** `.mli` interface files define what is exported. If no `.mli` file exists, everything is exported. The interface file is a separate source of truth.

**Concrete's differences:**
- Concrete does not use interface files. The public interface is derived from `pub` annotations in the source. This eliminates the synchronization problem where `.mli` and `.ml` files diverge.
- OCaml's approach is more powerful (you can hide a type's representation by omitting it from `.mli`), but the cost is maintaining two files per module.
- Concrete's `--report exports` snapshot achieves a similar "interface specification" role without a separate file.

### Summary Table

| Feature | Rust | Go | Zig | OCaml | Concrete |
|---------|------|-----|-----|-------|----------|
| Public modifier | `pub` | Uppercase | `pub` | `.mli` export | `pub` |
| Private (default) | No modifier | Lowercase | No modifier | Omit from `.mli` | No modifier |
| Package-internal | `pub(crate)` | Package scope | None | None | `pub(pkg)` (deferred) |
| Sub-module scope | `pub(super)`, `pub(in path)` | None | None | None | None |
| Struct fields | Private by default | Uppercase/lowercase | All public | `.mli` controlled | All public (first release) |
| Enum variants | All public | Uppercase/lowercase | All public | `.mli` controlled | All public (permanent) |
| Trait methods | All public | All public (interface) | N/A | `.mli` controlled | All public (permanent) |
| Glob imports | `use mod::*` | N/A (package scope) | None | `open Module` | None (permanent) |
| Interface files | None | None | None | `.mli` | None (`--report exports` snapshot) |

---

## Design Decisions Log

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Default visibility | Private | Explicit public surface; no accidental exports |
| Struct field visibility (first release) | All visible | No constructors; Copy structs need transparent access; pattern matching requires field access |
| Struct field visibility (future) | Per-field `pub` deferred | May be needed for encapsulated types when package ecosystem grows |
| Enum variant visibility | All visible (permanent) | Exhaustive matching requires all variants visible |
| Trait method visibility | All visible (permanent) | Contract coherence: implementors implement all, callers see all |
| `pub(pkg)` | Deferred until package management | Two levels sufficient for single-package programs |
| Glob imports | Never (permanent) | Every dependency is named; aligns with auditability principle |
| Interface files | Never (permanent) | Single source of truth; `--report exports` provides interface snapshots |
| Parent access to child private items | No (permanent) | Strict module privacy; unlike Rust where parent sees child internals |
| `pub(super)` / `pub(in path)` | Never | Restructure modules instead; three levels is enough |

---

## Implementation Priority

For the first release, the visibility model is **complete as-is**. No compiler changes are required. The two-level system (`pub`/private) is implemented, enforced, tested, and reported.

The work for the first release is organizational, not mechanical:

1. Audit every `pub` in `std/src/*.con` to ensure the public surface matches intent.
2. Add `--report exports` as a machine-readable export format (enhancement to existing `--report interface`).
3. Mark `libc` and `ptr` as internal (remove `pub` from their items, or if they have no `pub` items, document that they are implementation-only modules not intended for user import).
4. Establish the snapshot-and-diff workflow for CI.

For package management, implement `pub(pkg)` as described in section 7.2. This is a medium-sized change touching the parser, AST, FileSummary, and three resolution passes, but the design is straightforward and backward-compatible.

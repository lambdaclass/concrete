# Formatting and Text-Output Ergonomics

Status: historical stable reference plus Phase 7 update (Phase 3 item 62 closed;
Phase 7 items 14a/14b supersede the sink/rendering direction)

This document defines the direction for formatting and text output in Concrete before the stdlib freeze. It exists to settle the question posed by ROADMAP item 62:

> string-heavy programs such as `policy_engine`, `grep`, and parser error reporters should have explicit, buffer-oriented formatting paths that feel idiomatic enough without depending on interpolation magic, hidden allocation, or ad hoc builtin-shaped helpers.

For encoding rules, see [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md).
For stdlib direction, see [STDLIB.md](stdlib/STDLIB.md). For the current
roadmap contract, see Phase 7 items 14a/14b in [ROADMAP.md](../ROADMAP.md):
formatting targets a `Writer`, and user-defined rendering is an explicit
static `write_to` / `format_into` convention, not a trait-object `Display`
surface.

---

## 1. Goals and Non-Goals

**Goals.**
- One idiomatic path for writing mixed-type output to the console.
- One idiomatic path for building a `String` from mixed-type pieces.
- No hidden allocation. Every allocation is visible in a capability (`with(Alloc)`).
- No interpolation magic, no parser-level format strings, no hidden runtime.
- Text output API shape is separate from string-construction API shape — even if the two share primitives underneath.

**Non-goals (deliberately deferred).**
- Trait-object or universal formatting (`Display`/`Debug` equivalents).
  Concrete does not auto-render arbitrary structs. See section 6 for the Phase
  7 replacement: a static `write_to` / `format_into` convention over `Writer`.
- Format strings / positional arguments / `{}`-style placeholders.
- Locale, collation, or Unicode-aware width/padding.
- Runtime-pluggable writers or trait-object sink dispatch. Phase 7 uses a
  concrete `std.io.Writer` contract as the primary sink, but calls remain
  statically known and capability-visible.
- Floating-point formatting sophistication (rounding modes, precision control beyond what `float_to_string` already provides).

---

## 2. Public Surface (stable direction)

Two variadic forms, parallel in shape, distinct in destination:

### 2.1 `print(args...)` / `println(args...)` — console output
Already shipped. Desugars at elaboration into typed `print_string` / `print_int` / `print_char` / `print_bool` calls against the `Console` capability. `println` appends a newline.

Accepted argument types: `String`, `&String`, `&mut String`, `Int`/`Uint`/`i8..i32`/`u8..u32`, `bool`, `char`.

Unsupported types currently emit the placeholder `"<unprintable>"`. This is a
temporary fallback, not a contract. Phase 7 removes the ambiguity by routing
formatting through the `Writer` contract: built-ins render directly, and a
user-defined type renders only if its statically-known type exposes the explicit
`write_to(&self, w: &mut Writer) -> Result<usize, IoError>` (or `format_into`)
convention. There is no runtime type dispatch and no universal auto-rendering.

### 2.2 `append(&mut buf, args...)` — buffer append
Shipped (2026-04-20). Variadic buffer append. First argument must be a `&mut String`. Subsequent arguments dispatch by type into the existing typed append builtins (`string_append`, `string_append_int`, `string_append_bool`, `string_push_char`). Requires `with(Alloc)`. Reference test: `tests/programs/variadic_append.con`.

Result type: `unit`. No chaining, no return value. Callers hold the buffer explicitly.

Accepted argument types: same as `print`, minus console-only fallback. Unsupported types are a compile error, not a placeholder — `append` is for data construction and must not silently lossy-print.

This is a compiler elaboration step, not a user-visible free function. It guards on the absence of a user-defined `append` at file scope, mirroring the `print` guard.

### 2.3 Method-form alternatives remain available
- `String::append(&mut self, other: &String)` — existing stdlib method.
- `String::append_int(&mut self, n: Int)` — existing stdlib method.
- `String::push_char(&mut self, c: char)` — existing stdlib method.

The variadic `append` is a convenience; it does not replace method calls. Programs that only append one kind of thing can continue using method syntax without penalty.

---

## 3. Why Not Format Strings

A format-string-based API (`format!("hello {}", name)`) is superficially more ergonomic but imports several hidden properties that Concrete's stable subset rejects:

1. **Hidden allocation.** `format!` allocates a backing buffer whose size depends on runtime values. Concrete requires every allocation to be visible at the call site; a variadic typed-dispatch builtin preserves this by making the buffer argument explicit.
2. **Parser complexity.** A format-string mini-grammar inside string literals requires either a macro system or a parser extension. Concrete's syntax freeze commits to LL(1) and rejects macros.
3. **Trait-coupling.** Format strings typically drive a `Display`/`Debug` trait dispatch. Concrete's Phase 7 direction avoids that coupling: the sink is `Writer`, and user-defined rendering is an explicit static method convention, not a trait-object display surface.
4. **Drift of conventions.** `{}`/`{:?}`/`{:x}` specifiers proliferate, and each one implies a stdlib-wide convention. Deferring lets the convention emerge from evidence (item 67) rather than be imposed up front.

---

## 4. Why Not Method Chaining

`s.append(&a).append_int(n).append(&b)` would read better than the current 3-line form. We do not pursue it now because:

1. Method chaining on `&mut self` requires returning `&mut Self`. This complicates borrow-checking (the returned borrow aliases the buffer for the duration of the chain) and can mask subtle reborrow bugs.
2. The variadic `append(&mut buf, ...)` form gives the same ergonomic win without changing borrow semantics.
3. If chaining proves desirable later, it can be added as a pure stdlib change; no compiler work is required.

---

## 5. Buffer-Oriented Error Reporting

Parser/diagnostic error reporters should write into a caller-provided buffer, not return owned strings. Canonical shape:

```concrete pseudocode
fn format_error(msg: &mut String, err: &ParseError) with(Alloc) {
    append(msg, "error at line ", err.line, ", col ", err.col, ": ", &err.kind_str());
}
```

Rationale: reporters can be composed, buffers can be reused across many errors, and no hidden allocation leaks into the caller. Callers who want a fresh `String` call `String::new()` first.

---

## 6. Phase 7 Writer/Rendering Contract

Phase 3 shipped the variadic `print`/`println`/`append` surface. Phase 7's
stdlib-facing contract refines the underlying direction:

- `std.io.Writer` is the primary sink for formatting, diagnostics, logs,
  `std.test` output, file/console output, and progress output.
- `Writer` is not a `dyn Writer` trait object or hidden vtable. The v1
  representation is a concrete handle with explicit function pointers
  (callable-values / manual-vtable style) or a closed enum of sinks; capability
  and allocation behavior remain visible in signatures and reports.
- Built-in scalar/text values render through the shared `Writer` path.
- User-defined values are not auto-rendered. A type may opt in by exposing a
  statically-known method such as:

```concrete pseudocode
fn write_to(&self, w: &mut Writer) -> Result<usize, IoError>
```

  or the equivalent `format_into` spelling chosen by the stdlib.
- `std.fmt` may dispatch to that convention only when the concrete type is
  statically known. No trait-object `Display`, no runtime formatter registry,
  and no reflection-based fallback.
- String-producing helpers (`to_string`-style) are secondary wrappers over the
  `Writer` path and carry `with(Alloc)`. Fixed-buffer or caller-provided
  writers must be usable without `Alloc`.
- The evidence/report surface names the capability and allocation behavior of a
  formatting call, especially when the writer targets console, file, or network
  authority.

The Phase 7 gate should prove one built-in and one user struct render through
the same `Writer` path, a fixed-buffer writer does not require `Alloc`, and a
string-producing wrapper does.

## 7. Reconsideration Triggers

The Phase 3 surface in section 2 and the Phase 7 contract in section 6 are
reconsidered only if workload evidence produces any of the following findings:

- **Dispatch explosion.** A real workload requires appending many user-defined types, and the static `write_to` convention becomes too repetitive without a more general mechanism.
- **Width/padding pressure.** Aligned tabular output is unavoidable in a workload (e.g., `ls`-style columns) and cannot be built from primitives without excessive verbosity.
- **Float precision.** A workload needs fine-grained float formatting (e.g., scientific notation, fixed precision) that `float_to_string` cannot deliver.

Any such finding is recorded in [STDLIB.md](stdlib/STDLIB.md) gap ledger and
triggers a scoped design revision. The `print`/`println`/`append` triad is the
shipped Phase 3 surface; Phase 7's `Writer` path is the stdlib contract that new
formatting work should target.

---

## 8. Summary Table

| Intent | Call site | Capability | Allocates |
|---|---|---|---|
| Print mixed values to console | `println(a, b, c)` | `Console` | no |
| Print a formed string | `print_string(&s)` | `Console` | no |
| Append mixed values to a buffer | `append(&mut buf, a, b, c)` | `Alloc` | yes (buffer growth) |
| Append one `&String` | `buf.append(&other)` | `Alloc` | yes |
| Append one integer | `buf.append_int(n)` | `Alloc` | yes |
| Build an owned `String` from one int | `let s = int_to_string(n);` | `Alloc` | yes |

No other public entry points are part of the stable formatting surface at freeze.

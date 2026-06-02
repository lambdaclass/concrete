# Formatting and Text-Output Ergonomics

Status: stable reference (Phase 3 item 62 closed)

This document defines the direction for formatting and text output in Concrete before the stdlib freeze. It exists to settle the question posed by ROADMAP item 62:

> string-heavy programs such as `policy_engine`, `grep`, and parser error reporters should have explicit, buffer-oriented formatting paths that feel idiomatic enough without depending on interpolation magic, hidden allocation, or ad hoc builtin-shaped helpers.

For encoding rules, see [STRING_TEXT_CONTRACT.md](STRING_TEXT_CONTRACT.md).
For stdlib direction, see [STDLIB.md](STDLIB.md).

---

## 1. Goals and Non-Goals

**Goals.**
- One idiomatic path for writing mixed-type output to the console.
- One idiomatic path for building a `String` from mixed-type pieces.
- No hidden allocation. Every allocation is visible in a capability (`with(Alloc)`).
- No interpolation magic, no parser-level format strings, no hidden runtime.
- Text output API shape is separate from string-construction API shape — even if the two share primitives underneath.

**Non-goals (deliberately deferred).**
- Trait-based formatting (`Display`/`Debug` equivalents). Deferred until medium-workload evidence forces it. See section 6.
- Format strings / positional arguments / `{}`-style placeholders.
- Locale, collation, or Unicode-aware width/padding.
- Runtime-pluggable writers (`io.Writer`-style traits). A future addition; not required before freeze.
- Floating-point formatting sophistication (rounding modes, precision control beyond what `float_to_string` already provides).

---

## 2. Public Surface (stable direction)

Two variadic forms, parallel in shape, distinct in destination:

### 2.1 `print(args...)` / `println(args...)` — console output
Already shipped. Desugars at elaboration into typed `print_string` / `print_int` / `print_char` / `print_bool` calls against the `Console` capability. `println` appends a newline.

Accepted argument types: `String`, `&String`, `&mut String`, `Int`/`Uint`/`i8..i32`/`u8..u32`, `bool`, `char`.

Unsupported types currently emit the placeholder `"<unprintable>"`. This is a temporary fallback, not a contract. Future extensions (floats, user types via a small trait) will replace the placeholder, not carry it forward.

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
3. **Trait-coupling.** Format strings typically drive a `Display`/`Debug` trait dispatch. Introducing those traits before the stable subset is frozen would couple formatting to the trait system's final shape and risk drift.
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

```concrete
fn format_error(msg: &mut String, err: &ParseError) with(Alloc) {
    append(msg, "error at line ", err.line, ", col ", err.col, ": ", &err.kind_str());
}
```

Rationale: reporters can be composed, buffers can be reused across many errors, and no hidden allocation leaks into the caller. Callers who want a fresh `String` call `String::new()` first.

---

## 6. Reconsideration Triggers

The design in section 2 is the frozen first-release target. It is reconsidered only if post-freeze workload evidence produces any of the following findings:

- **Dispatch explosion.** A real workload requires appending many user-defined types, and the `append` variadic cannot dispatch them without a trait.
- **Width/padding pressure.** Aligned tabular output is unavoidable in a workload (e.g., `ls`-style columns) and cannot be built from primitives without excessive verbosity.
- **Float precision.** A workload needs fine-grained float formatting (e.g., scientific notation, fixed precision) that `float_to_string` cannot deliver.

Any such finding is recorded in [STDLIB.md](STDLIB.md) gap ledger and triggers a scoped design revision. The `print`/`println`/`append` triad is the frozen surface and all three are shipped as of 2026-04-20.

---

## 7. Summary Table

| Intent | Call site | Capability | Allocates |
|---|---|---|---|
| Print mixed values to console | `println(a, b, c)` | `Console` | no |
| Print a formed string | `print_string(&s)` | `Console` | no |
| Append mixed values to a buffer | `append(&mut buf, a, b, c)` | `Alloc` | yes (buffer growth) |
| Append one `&String` | `buf.append(&other)` | `Alloc` | yes |
| Append one integer | `buf.append_int(n)` | `Alloc` | yes |
| Build an owned `String` from one int | `let s = int_to_string(n);` | `Alloc` | yes |

No other public entry points are part of the stable formatting surface at freeze.

# Language Gaps Discovered During Phase H

Gaps found while writing the first real programs (`examples/policy_engine/` and `examples/mal/`). Each claim has been fact-checked against the codebase and corrected where wrong.

## True Blockers

### ~~1. Enum fields in structs panic the layout engine (Bug 005)~~ — FIXED

**Status:** Fixed. Enum fields in structs now work correctly, including in `Vec<Rule>` and similar containers. The layout engine handles enum alignment properly.

### ~~2. Standalone programs lack an always-available print path (Bug 007)~~ — FIXED

**Status:** Fixed. Added `print_string(&String)`, `print_int(Int)`, `print_char(Int)` as compiler builtins with `Console` capability. User-defined functions with the same names take precedence.

### 3. No string formatting or interpolation

Building `"[ALLOW] admin read source_code"` requires 7 chained `string_concat` calls. No `format(pattern, ...)` or string interpolation exists.

**Effect:** String-heavy code is verbose and error-prone. Every intermediate string is a potential leak if cleanup is missed.

### ~~4. No substring extraction path (Bug 010)~~ — FIXED

**Status:** Fixed. `string_slice(s, start, end)` already existed; added `string_substr` as an alias. Intrinsic name canonicalization ensures both resolve to the same LLVM function.

## Real Ergonomic Pain (Not Blockers)

### ~~5. If-else is a statement, not an expression (Bug 008)~~ — FIXED

**Status:** Fixed. If-else expressions now work:

```con
let label: String = if v == 1 { "ALLOW" } else { "DENY" };
```

Added `ifExpr` to `AST.Expr`, `Core.CExpr`, parser (`parseExprBlock`), elaboration, and lowering (alloca+condBr+store+load pattern with proper type casts).

### ~~6. Linear string building is awkward inside loops (Bug 011)~~ — FIXED

**Status:** Fixed. Added `string_push_char(&mut String, Int)` and `string_append(&mut String, &String)` builtins. These mutate in-place via `&mut`, working naturally with loop-carried mutable variables.

### 7. No qualified name access across modules

When two modules export functions with the same name (e.g., `from_tag`), there's no way to disambiguate except renaming one. `Module.function()` syntax does not exist. Confirmed: call expressions take a plain `String` name, not a qualified path.

### ~~8. Const declarations are parsed but broken at SSA lowering (Bug 009)~~ — FIXED

**Status:** Fixed. Constants now inline correctly during lowering. Added `constants` field to `LowerState` and constant lookup in the `.ident` case of `lowerExpr`.

### 9. No destructuring let

`let (a, b) = ...;` is not supported. `parseLet` only expects a single identifier.

### 10. Interpreter/runtime data-structure ergonomics are still thin

MAL exposed an important distinction:

- the first environment design was an interpreter implementation problem (a flat global binding pool with backwards scans over the full history)
- but Concrete still lacks some of the runtime-oriented collection ergonomics that would make the better design more natural

The right MAL fix is a frame-bounded environment design, not a language workaround. But the language/stdlib still makes this class of runtime somewhat harder than it should be:

- no hashmap/dictionary yet
- no obvious nested collection patterns for runtime structures
- string-heavy runtime code is already under pressure from Bugs 010 and 011

**Effect:** Concrete can support better interpreter designs than the first MAL attempt, but the supporting runtime/data-structure toolbox is still thinner than ideal for this workload class.

### ~~11. Standalone benchmark programs lack an easy timing path (Bug 012)~~ — FIXED

**Status:** Fixed. Added `clock_monotonic_ns() -> Int` builtin with `Clock` capability. Returns nanoseconds from monotonic clock via `clock_gettime`.

## Not Actually Missing (Previously Claimed Incorrectly)

- **print/println** — Exists in stdlib (`std.io`). The gap is standalone access, not absence.
- **Constants** — `const` is in the parser and grammar. The gap is a lowering bug (009), not a missing feature.
- **`&&` / `||`** — Work correctly. Used throughout tests (e.g., `lean_tests/integration_text_processing.con:31`).
- **Enums as values** — Work generally. The gap is specifically enum fields inside structs (Bug 005).
- **MAL's first slow environment design** — primarily an interpreter design problem, not proof that Concrete cannot support a better environment model.
- **No clock/timing support at all** — false. `std.time` exists; the gap is standalone accessibility, not absence.

## Summary

### Fixed (since initial discovery)
- **Bug 005** — Enum-in-struct layout: fixed
- **Bug 007** — Standalone print: fixed (`print_string`, `print_int`, `print_char` builtins)
- **Bug 008** — If-expression: fixed (was statement-only, now works as expression)
- **Bug 009** — Const lowering: fixed (constants inline during lowering)
- **Bug 010** — Substring extraction: fixed (`string_slice(start, end)` + `string_substr(start, len)` with correct semantics)
- **Bug 011** — Linear string building: fixed (`string_push_char`, `string_append` with `&mut`)
- **Bug 012** — Standalone timing: fixed (`clock_monotonic_ns` builtin)

### Remaining findings:
1. **Add string formatting** — cuts string-building verbosity by 5-7x
2. **No qualified name access across modules** — no `Module.function()` syntax
3. **No destructuring let** — `let (a, b) = ...` not supported

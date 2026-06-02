# Syntax Freeze Review

Status: freeze close-out review (Phase 3, items 76 and 77)

This document covers two items before syntax freeze:

- **Item 76**: Syntax friction review -- LL(1)-preserving changes only.
- **Item 77**: Syntax/ergonomics kill list -- every known pain point with a disposition.

All proposals in this document must preserve LL(1). Anything requiring backtracking, context-sensitivity, or lookahead > 1 is explicitly rejected.

For the LL(1) commitment, see [research/compiler/ll1-grammar.md](../research/compiler/ll1-grammar.md).
For what is permanently excluded, see [ANTI_FEATURES.md](ANTI_FEATURES.md).
For error handling ergonomics, see [ERROR_HANDLING_DESIGN.md](ERROR_HANDLING_DESIGN.md).
For stdlib API friction, see [STDLIB_API_REVIEW.md](STDLIB_API_REVIEW.md).

---

## Part 1: Syntax Friction Review (Item 66)

### 1.1 `Type::Variant` as the Canonical Enum Qualifier

**Current state.** Concrete now uses:

| Separator | Meaning | Example |
|-----------|---------|---------|
| `.` | Field access and method call | `s.len()`, `header.version` |
| `::` | Module qualification, generic instantiation, enum/static qualification | `std::fs::read_to_string`, `Vec::<i32>::new()`, `Option::<i32>::Some { value: 42 }`, `ParseError::TooShort` |

The old `#` separator is gone from the shipped language surface.

**Decision.** `Type::Variant` / `Type::method(...)` is the canonical qualification form.

**Why this fits the language.**

1. **It removes a language-specific oddity without hiding semantics.** The type name stays explicit at every use site; only the separator changed.
2. **It keeps the surface smaller.** One qualification syntax is easier to teach and review than separate `#` and `::` rules.
3. **It stays LL(1).** After `Name::`, the parser can still decide with one token of lookahead plus the existing base-name casing rule:
   - `<` continues the turbofish
   - lowercase `ident` under a lowercase base name is a module-qualified path
   - uppercase-type base names followed by `ident` produce enum/static qualification

**What is intentionally not allowed.**

- Bare variants such as `Ok` / `Err`
- Keeping `#` as a permanent second spelling
- Reintroducing `Enum.Variant` as a public alternate surface

`::` solved the qualification problem. The remaining syntax work is now the narrower ergonomics set: field punning, ignore/rest patterns, and destructuring forms.

---

### 1.2 Declaration Modifier Ordering

**Current state.** The parser expects modifiers in a specific order:

```
[pub] [trusted] [struct|enum|fn|impl] [Copy] Name
```

For structs and enums, `Copy` appears after the keyword:

```
struct Copy Header { ... }
enum Copy ParseError { ... }
pub struct Copy Header { ... }
```

For functions, `trusted` appears before `fn`:

```
trusted fn read_byte(buf: *const u8, offset: u64) -> u8 { ... }
pub trusted fn read_header_bytes(...) with(File) -> u64 { ... }
```

**Friction.** The `struct Copy` ordering is unusual. In most languages, modifiers precede the keyword entirely. A newcomer might try `Copy struct Header` or `pub Copy struct Header`. The current grammar puts a semantic modifier (`Copy`) in a syntactic position between the keyword and the name.

**Real code.** From `examples/parse_validate/src/main.con`:

```
enum Copy ParseError { ... }
struct Copy Header { ... }
enum Copy ParseResult { ... }
```

From `tests/programs/pressure_fixcap_ring_buffer.con`:

```
struct Copy IntRing { ... }
struct Copy IntPopResult { ... }
struct Copy Msg { ... }
struct Copy MsgRing { ... }
struct Copy MsgPopResult { ... }
```

The pattern is pervasive and consistent. Every Copy type in every file uses `struct Copy Name` or `enum Copy Name`.

**Alternative: `pub Copy struct Header`?** This reads better as English ("a public, copyable struct called Header") but requires the parser to accept `Copy` as a modifier before `struct`/`enum`, which means `Copy` joins `pub` and `trusted` as a pre-keyword modifier. The LL(1) impact is small: after `pub`, check for `Copy`/`trusted`/`struct`/`enum`/`fn`/`impl`. After `Copy`, expect `struct` or `enum`.

**LL(1) impact.** Either ordering is LL(1)-safe. The current ordering works because the parser consumes `struct`/`enum` first, then checks if the next identifier is "Copy" before consuming the type name. The alternative ordering would check for "Copy" in the modifier position before the keyword.

**Recommendation: FREEZE AS-IS.** The `struct Copy Name` ordering is established across all existing code. Changing it would require touching every Copy type declaration in the codebase. The current ordering is parseable, learnable, and consistent. It follows a "keyword first, then modifiers, then name" pattern that is not standard but is internally consistent. Teaching it costs one sentence in documentation.

---

### 1.3 Generic Construction Verbosity

**Current state.** Constructing a generic enum variant requires fully specifying all type parameters:

```
return Result::<File, FsError>::Err { error: FsError::OpenFailed };
return Option::<i32>::Some { value: 42 };
return Option::<&V>::None;
```

**The friction.** When the return type of the function is `Result<File, FsError>`, the type parameters on the construction are fully redundant:

```
fn open(path: &String) with(File) -> Result<File, FsError> {
    // The return type already tells us T=File, E=FsError
    return Result::<File, FsError>::Ok { value: f };  // redundant
}
```

In `std/src/map.con`, the `HashMap` implementation has 23 lines that construct `Option::<V>::None`, `Option::<V>::Some`, `Option::<&V>::Some`, etc. Each repeats the type parameter that the method signature already determines.

From `std/src/vec.con`:

```
pub fn pop(&mut self) -> Option<T> {
    if self.len == 0 {
        return Option::<T>::None;  // T is known from the method signature
    }
    self.len = self.len - 1;
    let target_ptr: *mut T = self.ptr + self.len;
    return Option::<T>::Some { value: *target_ptr };  // same
}
```

**What would help.** Elaboration-time type inference on enum variant construction. If the expected type is known (from return type, let-binding annotation, or function argument position), the type parameters could be omitted:

```
return Result::Ok { value: f };     // infer <File, FsError> from return type
return Option::None;                // infer <T> from return type
let o: Option<i32> = Option::Some { value: 42 };  // infer <i32> from annotation
```

**LL(1) impact.** None. This is purely an elaboration/type-checking change. The parser already produces `enumLit` nodes with a possibly-empty type argument list. When the turbofish is absent (`Option::Some` without `::<T>`), the type argument list is empty and elaboration fills it in. No grammar change is needed.

**Constraints.** This does not violate the "no Hindley-Milner inference" rule in ANTI_FEATURES.md. It is local inference: the expected type is immediately available from the enclosing let-binding or return statement. No global constraint solving is needed. This is comparable to Rust's ability to write `Ok(value)` when the return type is known, or Go's `return nil` when the return type is `error`.

**Recommendation: CHANGE (elaboration-time).** This is the single highest-impact ergonomic improvement available. It would eliminate redundant type annotations on every `Result` and `Option` construction throughout the entire codebase and stdlib. The change is purely in the type checker/elaboration pass, not in the parser. The type parameters remain available for cases where the context is ambiguous. The parser grammar is unchanged.

**Implementation sketch:** In the type checker, when processing an enum literal with an empty type argument list, check if the expected type (from return position, let-binding annotation, or function argument) provides the missing type parameters. If so, fill them in. If not, emit the existing "missing type arguments" error.

---

### 1.4 Enum Variant Construction Verbosity

**Current state.** Beyond the generic parameter repetition (1.3), enum variant construction requires repeating the enum type name even for non-generic enums:

```
return ParseResult::Err { error: ParseError::TooShort };
return ParseResult::Err { error: ParseError::BadVersion };
return ParseResult::Err { error: ParseError::BadType };
return ParseResult::Err { error: ParseError::PayloadTooBig };
return ParseResult::Err { error: ParseError::Truncated };
return ParseResult::Err { error: ParseError::BadChecksum };
```

From `examples/service_errors/src/main.con`:

```
return ValidateResult::Err { error: ValidateError::BadUserId };
return ValidateResult::Err { error: ValidateError::BadAction };
return ValidateResult::Err { error: ValidateError::PayloadTooLarge };
```

**Analysis.** The outer enum type (`ParseResult`, `ValidateResult`) must always be specified because the parser does not know the expected type. The inner error enum (`ParseError`) must be specified because Concrete has no way to infer which enum type a bare variant belongs to.

If 1.3 is implemented (elaboration-time inference for generic types), then `Result` replaces `ParseResult` and the construction becomes:

```
return Result::Err { error: ParseError::TooShort };
```

This is a significant improvement. The inner `ParseError::TooShort` cannot be shortened further without either import-based variant flattening (bringing `TooShort` into scope) or contextual variant resolution (knowing that the `error:` field expects a `ParseError`). Both would require semantic information during parsing or elaboration changes.

**Recommendation: DEFER.** The remaining verbosity after 1.3 is acceptable. Each variant construction names the type explicitly, which is Concrete's design intent ("every type is visible where it matters"). If sustained real-program pressure demonstrates this is too painful, contextual variant inference for fields with known enum types could be considered post-freeze. This would be an elaboration change, not a syntax change.

---

### 1.5 Match Arm Syntax

**Current state.** Match arms use `=>` (fat arrow) with block or expression bodies:

```
match result {
    Option::Some { value } => {
        if *value != 10 {
            v.drop();
            return 1;
        }
    },
    Option::None => {
        v.drop();
        return 1;
    }
}
```

**Friction points observed.**

1. **Comma between arms.** The trailing comma after `},` is optional but conventional. Forgetting it produces a parse error. This is a minor stumbling point but not a significant issue.

2. **Both `=>` and `->` are accepted.** The parser accepts either `=>` or `->` in match arms (Parser.lean lines 1088-1090). This dual acceptance is undocumented and may cause confusion if different files use different styles.

3. **Match-as-expression works but is limited.** The parser supports bare expression bodies for match arms (`=> expr,`) and block expression bodies (`=> { expr }`). This is sufficient for current use.

4. **No nested pattern matching.** Match arms destructure only one level: `EnumName::Variant { field }`. There is no nested destructuring like `Result::Ok { value: Option::Some { inner } }`. This is consistent with Concrete's explicit philosophy but means nested enums require multiple match statements.

**Real code showing clean usage.** From `examples/grep/src/main.con`:

```
match result {
    Result::Ok { value } => {
        let content_len: Int = string_length(&value);
        // ...
        value.drop();
    },
    Result::Err { error } => {
        // ...
    }
}
```

This is clear, explicit, and matches the language's philosophy.

**LL(1) impact.** No changes proposed.

**Recommendation: FREEZE AS-IS.** Match syntax works well. The only action is to document that both `=>` and `->` are accepted, and decide whether to deprecate one. Recommendation: keep `=>` as the canonical form and emit a warning for `->` in match arms (since `->` is also the return type arrow, using it in match arms is visually confusing).

---

### 1.6 Import Syntax

**Current state.** Imports use dotted paths with a braced symbol list:

```
import std.fs.{read_to_string, FsError};
import std.hash.{hash_string, eq_string};
import std.vec.{Vec};
```

**Friction points observed.**

1. **Single-symbol imports are verbose.** Builtin `Result` / `Option` no longer need imports, but ordinary one-symbol imports like `import std.vec.{Vec};` still require braces. A non-braced form (`import std.vec.Vec;`) would save characters but adds an alternative production.

2. **No glob imports.** There is no `import std.fs.*` or `import std.fs.{*}`. Every symbol must be named. This is intentional (visibility, auditability) and consistent with ANTI_FEATURES.md ("every import is visible").

3. **No re-exports.** A module cannot re-export symbols from its imports. This is a design gap, not a syntax gap.

4. **Import blocks in examples are long.** The `integrity` example has 10 import lines. The `kvstore` example has 8. These are proportional to the number of stdlib modules used, which is appropriate for explicit imports.

**Real code.** From `examples/integrity/src/main.con`:

```
import std.fs.{read_file, read_to_string, write_file, FsError};
import std.bytes.{Bytes};
import std.args.{count, get};
import std.map.{HashMap};
import std.set.{HashSet};
import std.hash.{hash_string, eq_string};
import std.vec.{Vec};
import std.sha256.{hash_raw};
import std.hex.{encode_u32_vec};
import std.string.{String};
```

**LL(1) impact of allowing non-braced single import.** After the last path segment, the parser would need to distinguish `.{` (braced symbol list) from `;` (single-symbol import where the last segment is the symbol name). This is LL(1)-safe: after the final identifier, if the next token is `;`, the last segment is the imported symbol; if `.{`, parse the symbol list. However, this creates ambiguity about whether the last segment is a module name or a symbol name.

**Recommendation: FREEZE AS-IS.** The braced syntax is explicit, consistent, and works. The verbosity is minimal (two extra characters per import). No change.

---

### 1.7 Borrow Syntax

**Current state.** Borrow blocks use explicit syntax:

```
borrow x as ref in R { ... }
borrow mut x as ref in R { ... }
```

**Friction.** Five tokens before the block body is verbose. However, each token carries meaning:

- `borrow` -- keyword, introduces the block
- `[mut]` -- mutable or shared
- `x` -- the variable being borrowed
- `as ref` -- the name of the reference binding
- `in R` -- the region name

**Real usage.** From test programs:

```
borrow d as r1 in R1 {
    if *r1 != 42 { return 1; }
}
borrow mut d as r2 in R2 {
    *r2 = 100;
}
```

**Analysis.** The region name (`R1`, `R2`) is currently unused by the compiler for any semantic purpose beyond scoping. If regions become more important (e.g., for proof purposes), the explicit name is valuable. If regions remain purely syntactic, the `in R` could be optional.

The `as ref` provides an explicit binding name for the reference, which is consistent with Concrete's philosophy of "every binding is visible." Making it optional would require the parser to use the original variable name, which creates confusion about whether `x` inside the block is the owned value or the borrowed reference.

**LL(1) impact.** No change proposed.

**Recommendation: FREEZE AS-IS.** The syntax is verbose but every component carries meaning. Removing any piece would either lose information or create ambiguity. The region name might become optional post-freeze if region-polymorphism does not materialize, but that is a future simplification, not a freeze-time change.

---

### 1.8 Method Call Syntax

**Current state.** Method calls use `.method()` syntax:

```
v.push(10);
s.append(&" world");
m.insert(1, 10);
v.drop();
```

Field access, method calls, and turbofish methods are all postfix with `.`:

```
self.len                          // field access
self.grow()                       // method call
v.fold::<i32>(0, add_i32)        // generic method call
```

**Friction.** Two observed points:

1. **`self.field` vs `(*self).field` for ref methods.** Methods that take `&self` access fields via `self.field` transparently. The auto-deref is implicit -- `self` is `&Vec<T>`, but `self.len` accesses the `len` field without explicit dereferencing. This is a deliberate ergonomic choice that is already implemented in the compiler. No friction here.

2. **Calling free functions vs methods.** Some stdlib code mixes free-function style and method style:

   ```
   // Method style (modern):
   v.push(42);
   s.drop();
   
   // Free function style (legacy):
   vec_push::<i32>(&mut v, 42);
   vec_free::<i32>(v);
   string_length(&s);
   ```

   This inconsistency is a stdlib migration issue (documented in STDLIB_API_REVIEW.md), not a syntax issue. The free-function style predates the `impl` block support.

**Recommendation: FREEZE AS-IS.** Method call syntax works well and is LL(1)-safe. The legacy free-function style should be migrated to method style in the stdlib (separate from syntax freeze).

---

### 1.9 Additional Friction: Deeply Nested `if-else` Chains

**Observed in code.** From `tests/programs/pressure_parse_json_subset.con`, the tokenizer uses deeply nested if-else chains because there is no `else if` with pattern matching and no `match` on primitive values at the statement level:

```
if b == 123 {
    // ...
} else {
    if b == 125 {
        // ...
    } else {
        if b == 58 {
            // ...
        } else {
            if b == 44 {
                // ...
            } else {
                if b == 34 {
                    // ...
                } else {
                    if b == 116 || b == 102 {
                        // ...
                    } else {
                        if is_digit(b) == 1 || b == 45 {
                            // ...
                        } else {
                            break;
                        }
                    }
                }
            }
        }
    }
}
```

This is 7 levels of nesting. Match on integer values would flatten this:

```
match b {
    123 => { ... },
    125 => { ... },
    58 => { ... },
    44 => { ... },
    34 => { ... },
    _ => {
        if b == 116 || b == 102 { ... }
        else if is_digit(b) == 1 || b == 45 { ... }
        else { break; }
    }
}
```

**Current state.** The parser already supports integer literal patterns in match arms (Parser.lean lines 1085-1094). Match on integer values already works. The pressure test was likely written before this feature was available or the author was unaware.

**Recommendation: FREEZE AS-IS.** The syntax already supports integer match. Document the pattern in examples.

---

### 1.10 Additional Friction: String Building Without Interpolation

**Observed in code.** From `examples/grep/src/main.con`:

```
let mut out: String = "";
if show_filename {
    out.append(filename);
    string_push_char(&mut out, 58);  // ':'
}
if show_numbers {
    out.append_int(line_num);
    string_push_char(&mut out, 58);  // ':'
}
out.append(&line);
println(out);
out.drop();
```

Building a string requires multiple append calls. There is no string interpolation or `format!` macro (permanently excluded per ANTI_FEATURES.md: no macros). The `Writer` type in `std.writer` provides an allocation-free alternative but is not commonly used.

**Recommendation: FREEZE AS-IS.** String interpolation would require either macros (permanently excluded) or a special parser form. The explicit `append`/`append_int` pattern is verbose but consistent with Concrete's "no hidden allocation" principle. The `Writer` type exists as the allocation-free alternative. Document both patterns.

---

## Part 2: Syntax/Ergonomics Kill List (Item 67)

### Priority Table

| # | Pain Point | Relief Type | Status | Notes |
|---|-----------|------------|--------|-------|
| 1 | **Generic enum construction verbosity** (`Result::<T, E>::Ok`) | Elaboration-time inference | **CHANGE** | Highest-impact single improvement. No parser change. |
| 2 | **Verbose error propagation (multi-step)** | Library: `map_err` + `?` | Freeze as-is | `?` exists. `map_err` is a library addition (ERROR_HANDLING_DESIGN.md). |
| 3 | **Ad hoc result types instead of `Result<T, E>`** | Library: `unwrap_or` on Result/Option | Freeze as-is | Stdlib addition, not syntax. See ERROR_HANDLING_DESIGN.md items. |
| 4 | **`Vec::<T>::new()` type repetition on let-binding** | Elaboration-time inference | **CHANGE** | Same mechanism as #1. `let mut v: Vec<i32> = Vec::new();` with type inferred from LHS. |
| 5 | **`Option::<T>::None` / `Option::<T>::Some` everywhere** | Elaboration-time inference | **CHANGE** | Same mechanism as #1. |
| 6 | **Custom result enums duplicating `Result<T, E>`** | Library: Result helpers | Freeze as-is | Once `unwrap_or` and `?` are usable, custom result enums become unnecessary. |
| 7 | **Deeply nested if-else for byte dispatch** | Already supported: match on integer | Freeze as-is | Document integer match patterns. |
| 8 | **Fixed-array construction boilerplate** | Library helpers | **Deferred** | `[0; 256]` already works. More complex initializations need either comptime (deferred) or helper functions. |
| 9 | **String building without interpolation** | Library: `Writer` type | Freeze as-is | `Writer` exists. Document the pattern. No macro/interpolation syntax. |
| 10 | **Three namespace separators (`.`, `::`, `#`)** | Syntax: unify `#` with `::` | **Deferred** | Possible post-freeze if user feedback demands it. |
| 11 | **`struct Copy` modifier ordering** | Syntax | Freeze as-is | Established convention. Changing would touch all existing code. |
| 12 | **Borrow block verbosity** | Syntax | Freeze as-is | Every component carries meaning. |
| 13 | **Import braces for single symbols** | Syntax | Freeze as-is | Two extra characters. Consistent pattern. |
| 14 | **`->` vs `=>` in match arms** | Documentation | Freeze as-is | Standardize on `=>`. Emit warning for `->`. |
| 15 | **Legacy free-function API style** | Stdlib migration | Freeze as-is | `vec_push` -> `v.push()`. Migration tracked in STDLIB_API_REVIEW.md. |
| 16 | **No `else if` match/switch** | Already supported | Freeze as-is | `else if` already works in the parser. |
| 17 | **Repetitive enum error ceremony** | Library: `map_err` | Freeze as-is | See ERROR_HANDLING_DESIGN.md Wave 2 helpers. |
| 18 | **No `for x in collection` syntax** | Syntax | **Deferred** | Requires iterator protocol. Current `for (init; cond; step)` is C-style. |
| 19 | **Integer literal as char** | Syntax/stdlib | Freeze as-is | `58` for `:` is painful but explicit. `':'` (char literal) with cast to `u8`/`i32` is available. |
| 20 | **Missing `+=`, `-=`, `*=` compound assignment** | Syntax | **Deferred** | Would reduce `x = x + 1` to `x += 1`. LL(1)-safe. Low priority. |

### Detailed Notes on Key Entries

#### #1: Generic enum construction inference (CHANGE)

**What it is.** When the expected type is known from context, allow omitting type parameters on enum variant construction.

**Before:**

```
fn open(path: &String) with(File) -> Result<File, FsError> {
    if fp == (0 as *const u8) {
        return Result::<File, FsError>::Err { error: FsError::OpenFailed };
    }
    return Result::<File, FsError>::Ok { value: f };
}
```

**After:**

```
fn open(path: &String) with(File) -> Result<File, FsError> {
    if fp == (0 as *const u8) {
        return Result::Err { error: FsError::OpenFailed };
    }
    return Result::Ok { value: f };
}
```

**Scope of impact.** Every `Result::<T, E>::Variant` and `Option::<T>::Variant` construction in the stdlib and examples -- conservatively 100+ sites. Also applies to user-defined generic enums.

**What must be true.** The expected type must be unambiguously available from one of:
- Return type of the enclosing function (for `return` statements)
- Type annotation on a `let` binding (for `let x: T = ...`)
- Parameter type of a function call (for passing enum values as arguments)

If the expected type is not available, the existing turbofish syntax remains required and the existing error message applies.

**LL(1) impact.** Zero. The parser already handles `EnumName::Variant` with no type arguments (the type argument list is empty). The change is in the type checker, which fills in the missing type arguments from context.

---

#### #4: Let-binding type inference for constructors (CHANGE)

**What it is.** When a let-binding has an explicit type annotation on the left-hand side, the constructor on the right-hand side can omit redundant type parameters.

**Before:**

```
let mut v: Vec<i32> = Vec::<i32>::new();
let mut m: HashMap<String, String> = HashMap::<String, String>::new(hash_string, eq_string);
let mut keys: Vec<String> = Vec::<String>::new();
```

**After:**

```
let mut v: Vec<i32> = Vec::new();
let mut m: HashMap<String, String> = HashMap::new(hash_string, eq_string);
let mut keys: Vec<String> = Vec::new();
```

The type annotation on the left provides the type parameters. The constructor call on the right does not need to repeat them.

**Scope of impact.** The `examples/lox/src/main.con` alone has 20+ lines of `Vec::<T>::new()` where the type is repeated from the left-hand side. The `examples/integrity/src/main.con` has similar patterns for `HashMap`, `HashSet`, and `Vec`.

**What must be true.** Same mechanism as #1: the expected type from the let-binding annotation provides the type parameters for the right-hand side expression.

**LL(1) impact.** Zero. Same reasoning as #1.

---

#### #8: Fixed-array construction boilerplate (DEFERRED)

**What it is.** Complex array initialization requires explicit element-by-element assignment.

**Real code from `pressure_parse_json_subset.con`:**

```
trusted fn build_test_json() -> Cursor {
    let mut buf: [u8; 256] = [0; 256];
    buf[0] = 123;  // {
    buf[1] = 34;   // "
    buf[2] = 97;   // a
    buf[3] = 103;  // g
    buf[4] = 101;  // e
    buf[5] = 34;   // "
    buf[6] = 58;   // :
    // ... 16 more lines
    return cursor_new(buf, 23);
}
```

**What would help.** Byte-string literals (`b"hello"`) or array-from-string-literal syntax. This is a common pattern in parser code.

**Why deferred.** Byte-string literals would require a new literal type and parser production. The design space includes: `b"string"` producing `[u8; N]`, `"string".as_bytes()` producing `&[u8]`, or a `Bytes::from_literal("string")` library function. The right design depends on how the byte/string boundary evolves (see STRING_TEXT_CONTRACT.md). Deferring until post-freeze when byte/string semantics are more settled.

**What would need to be true to reconsider.** A clear byte-string literal design that does not require hidden allocation, is compatible with fixed-size arrays, and does not create a second string type.

---

#### #10: Three namespace separators (DEFERRED)

**What it is.** `.` for modules/fields, `::` for generics/statics, `#` for variants.

**Why deferred.** The three separators work. Unifying `#` into `::` is possible and LL(1)-safe, but would require touching every enum construction and match arm in the codebase. The benefit is reduced cognitive load for newcomers. The cost is a large mechanical migration and loss of the visual distinction between variant construction and static method calls.

**What would need to be true to reconsider.** Post-release user feedback indicating that the `#` separator is a significant learning barrier or source of confusion. If feedback is positive ("the `#` makes variants visually obvious"), freeze permanently.

---

#### #18: `for x in collection` syntax (DEFERRED)

**What it is.** Currently, iterating over a collection requires:

```
let i: u64 = 0;
while i < v.len() {
    let elem: &i32 = v.get_unchecked(i);
    // use elem
    i = i + 1;
}
```

Or with `fold`/`for_each` and function pointers:

```
v.for_each(print_elem);
```

A `for x in collection { ... }` syntax would be more ergonomic for the common case.

**Why deferred.** Requires an iterator protocol (trait, method convention, or compiler-recognized interface). Concrete does not have closures, so the loop body cannot be passed as a callback. A `for-in` loop would need compiler support to desugar into index-based or pointer-based iteration. The design depends on whether Concrete adopts an `Iterator` trait or a simpler `Iterable` convention.

**What would need to be true to reconsider.** An iterator protocol design that is compatible with:
- No closures (the loop body is inline, not a callback)
- No dynamic dispatch (the iterator type is known at compile time)
- Linear ownership (iterating over a borrowed collection does not consume it)
- Predictable execution (iteration count is bounded if the collection is bounded)

---

#### #20: Compound assignment operators (DEFERRED)

**What it is.** `x += 1` instead of `x = x + 1`.

**Current code pattern (very common):**

```
i = i + 1;
count = count + 1;
acc = acc + value;
pos = pos + 1;
```

**LL(1) impact.** `+=` would be a new token. After an identifier, the parser would need to distinguish `=` (assignment) from `+=` (compound assignment). This is LL(1)-safe: the lexer produces `+=` as a single token, distinct from `=`.

**Why deferred.** Low priority. The verbosity is minor (3 extra characters). Compound assignment operators are syntactic sugar that the optimizer can already handle (`x = x + 1` produces identical code to what `x += 1` would produce). The benefit is readability in tight loops, not performance.

**What would need to be true to reconsider.** Post-release feedback indicating that the verbosity is a sustained pain point. Or: if proof extraction needs to distinguish compound assignment from general assignment for loop invariant analysis.

---

### Summary of Changes Before Freeze

| Item | Action | Type | Affects |
|------|--------|------|---------|
| Generic enum inference (#1) | Implement elaboration-time type parameter inference | Type checker | All `Result::<T, E>::Variant` and `Option::<T>::Variant` sites |
| Constructor inference (#4) | Same mechanism as #1 | Type checker | All `Type::<T>::new()` sites where LHS type is annotated |
| Match arm arrow (#14) | Document `=>` as canonical; consider warning for `->` | Documentation | Style consistency |

### Summary of Explicit Deferrals

| Item | Condition for Reconsideration |
|------|------------------------------|
| Unify `#` with `::` (#10) | Post-release user feedback |
| Byte-string literals (#8) | Clear byte/string boundary design |
| `for x in collection` (#18) | Iterator protocol design |
| Compound assignment (#20) | Sustained post-release feedback |
| `struct Copy` vs `Copy struct` (#11) | Never (frozen) |

### Summary of Freeze-as-is Decisions

| Item | Rationale |
|------|-----------|
| `?` operator | Well-tested, sufficient. Cross-type conversion via `map_err`. |
| Import syntax | Explicit, consistent, minimal overhead. |
| Borrow block syntax | Every component carries meaning. |
| Match arm syntax | Works well. Both `#` and `.` accepted in patterns. |
| String building | `append`/`append_int` is explicit. `Writer` exists for allocation-free path. |
| Declaration modifier ordering | Established convention across all existing code. |
| Method call syntax | Works well. Auto-deref on `&self` is implemented. |

---

## Constraints Recap

Every proposal in this document was evaluated against:

1. **LL(1) preservation.** No proposal requires backtracking, save/restore, or lookahead > 1.
2. **Phase separation.** Proposed changes affect elaboration/type-checking, not the parser grammar.
3. **Explicitness.** No proposal hides type information. Inferred type parameters are always determinable from the immediate context.
4. **ANTI_FEATURES.md compliance.** No proposal introduces closures, macros, implicit trait resolution, or global type inference.
5. **Practical impact.** Each "CHANGE" item was justified with real code from existing examples, stdlib, or pressure tests.

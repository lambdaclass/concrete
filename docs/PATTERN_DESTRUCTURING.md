# Pattern Destructuring: LL(1)-Preserving `let` Destructuring and `let...else`

Status: design document (Phase 3, item 62)

This document designs pattern destructuring for Concrete. The goal is to reduce match verbosity for common enum-extraction patterns without introducing name resolution magic, inference, or parser backtracking. Every form described here desugars to an existing `match` expression and preserves the LL(1) grammar invariant.

Note: some examples below use older schematic names such as `ParseResult` or `ServiceResult` to isolate the destructuring shape. The shipped canonical examples now use builtin `Result<T, E>` directly.

For the LL(1) grammar commitment, see `research/compiler/ll1-grammar.md`.
For what Concrete permanently excludes, see [ANTI_FEATURES.md](ANTI_FEATURES.md).
For the error handling context that motivates this, see [ERROR_HANDLING_DESIGN.md](ERROR_HANDLING_DESIGN.md).
For the language gap that first identified this need, see [LANGUAGE_GAPS.md](LANGUAGE_GAPS.md) (item 9: "No destructuring let").

---

## 1. The Problem

Match statements are the only way to extract fields from enum variants. For simple "get the Ok value or bail" patterns, this requires 5-10 lines where 1-2 lines would suffice.

### Real example: extracting the success case and returning on error

From `examples/service_errors/src/main.con`, lines 208-219:

```
fn handle_request(req: Request) -> ServiceResult {
    match validate(req) {
        ValidateResult::Err { error } => {
            return ServiceResult::Err {
                error: ServiceError::Validation { code: validate_error_code(error) }
            };
        },
        ValidateResult::Ok { val } => {
            return handle_validated(req);
        },
    }
}
```

The programmer wants: "if validate succeeds, continue with the value; if it fails, convert the error and return." The match block is 10 lines. The essential logic is 2 lines.

### Real example: extracting a value and checking it

From `examples/parse_validate/src/main.con`, lines 162-170:

```
match parse_header(good, 5) {
    ParseResult::Ok { header } => {
        if header.version != 1 { return 11; }
        if header.msg_type != 2 { return 12; }
        if header.payload_len != 0 { return 13; }
        if header.checksum != 3 { return 14; }
    },
    ParseResult::Err { error } => { return 10; },
}
```

The programmer wants: "extract the header or return 10." The match block is 8 lines. The interesting code is 4 lines of field checks. The Err arm is pure boilerplate.

### Real example: cascaded match in tests

From `examples/service_errors/src/main.con`, lines 240-245:

```
match handle_request(bad_user) {
    ServiceResult::Ok { response } => { return 20; },
    ServiceResult::Err { error } => {
        if service_error_code(error) != 101 { return 21; }
    },
}
```

This pattern repeats 9 times in the same function. Each instance is 5-6 lines. The essential code is always one line.

### Scale of the problem

In `examples/service_errors/src/main.con`: 9 match blocks for tests, 3 match blocks for pipeline functions. In `examples/parse_validate/src/main.con`: 8 match blocks for tests, 1 for error classification. The match blocks account for roughly 60% of the line count in both files. With `let...else`, each test match block becomes 2 lines.

---

## 2. `let` Destructuring (Irrefutable Patterns)

### Syntax

```
let Type::Variant { field1, field2 } = expr;
```

### When this is safe

Irrefutable destructuring is safe when the pattern covers the only possible variant. In practice this applies to:

- Single-variant enums (wrapper types)
- Structs (always a single "variant")

For multi-variant enums, irrefutable `let` destructuring is a compile-time error. The compiler must reject this because the pattern does not cover all variants. Use `let...else` (section 3) for refutable patterns.

### Examples

```
// Single-variant enum (wrapper)
enum Token {
    Ident { name: String },
}

let Token::Ident { name } = next_token();
// `name` is now bound
```

```
// Struct destructuring (structs have exactly one shape)
struct Point { x: i32, y: i32 }

let Point { x, y } = make_point();
// `x` and `y` are now bound
```

Note: struct destructuring uses `let Type { field1, field2 } = expr;` (no `::Variant` because structs have no variants). The parser distinguishes this from enum destructuring by the absence of `::Variant`.

### LL(1) parse rule

The current `parseLet` production is:

```
LetStmt ::= 'let' ['mut'] IDENT [':' Type] '=' Expr ';'
```

The new production adds an alternative path after `let`:

```
LetStmt ::= 'let' ['mut'] LetTarget [':' Type] '=' Expr ';'

LetTarget ::= IDENT                                     -- plain binding (existing)
            | IDENT '::' IDENT '{' BindingList '}'      -- enum destructure
            | IDENT '{' BindingList '}'                 -- struct destructure

BindingList ::= IDENT (',' IDENT)* [',']
```

**LL(1) analysis:** After `let` (and optional `mut`), the parser reads an IDENT. It then peeks at the next token:

- `::` -- enum destructuring. Committed. Read variant name, expect `{`, read bindings, expect `}`.
- `{` -- struct destructuring (only if IDENT starts with uppercase, same convention as struct literals). Committed. Read bindings, expect `}`.
- `:` or `=` -- plain let binding (existing path).

This is decided with one token of lookahead after consuming the first IDENT. No backtracking. The uppercase check for struct destructuring is the same convention already used in `parsePrimary` (line 494 of `Parser.lean`) to distinguish struct literals from brace blocks.

### Desugaring

```
let Option::Some { value } = expr;
```

Desugars to:

```
match expr {
    Option::Some { value } => {},
}
```

If the enum has other variants, the match is non-exhaustive and the checker rejects it. This is the same exhaustiveness check that already exists for match statements.

For struct destructuring:

```
let Point { x, y } = expr;
```

Desugars to two let bindings that access the fields:

```
let __tmp = expr;
let x = __tmp.x;
let y = __tmp.y;
```

Or equivalently, to a single-arm match where the struct is the sole "variant."

---

## 3. `let...else` for Refutable Patterns

### Syntax

```
let Type::Variant { field1, field2 } = expr else {
    // diverging body: must return, break, continue, or abort
};
```

### Semantics

If `expr` matches the pattern, the bindings are introduced into the surrounding scope. If `expr` does not match, the else block executes. The else block must diverge -- it must contain a `return`, `break`, `continue`, or call to an abort function on every code path. The compiler rejects an else block that falls through.

### Examples

**Extract Ok or return the error:**

```
let Result::Ok { value } = validate(req) else {
    return ServiceResult::Err {
        error: ServiceError::Validation { code: validate_error_code(error) }
    };
};
// `value` is now bound, continue with the happy path
```

Wait -- there is a subtlety here. In the else block, we need access to the non-matching variant's fields. This requires binding the else case. Two options:

**Option A: The else block receives no bindings.** The programmer can only use values already in scope. This is simpler but limits the else block to fixed error responses.

**Option B: The else block receives a catch-all binding.** The original expression is available in the else block via a variable. The programmer must match it again or discard it.

**Decision: Option A for the first release.** The else block receives no bindings from the failed pattern. This is simpler to implement, simpler to understand, and covers the most common case (return a fixed error, break, or continue). If the programmer needs access to the non-matching value, they should use a full match statement. This keeps the feature small and avoids introducing a second destructuring site inside the else block.

For cases where the error value is needed, a full match remains the right tool:

```
// When you need the error value, use match -- this is the right tool
match validate(req) {
    ValidateResult::Ok { val } => {
        // continue
    },
    ValidateResult::Err { error } => {
        return ServiceResult::Err {
            error: ServiceError::Validation { code: validate_error_code(error) }
        };
    },
}
```

**Revised examples for Option A:**

```
// Extract the header or return a fixed error code
let ParseResult::Ok { header } = parse_header(good, 5) else {
    return 10;
};
if header.version != 1 { return 11; }
if header.msg_type != 2 { return 12; }
```

```
// Extract a value or use a default via return-from-block
let Option::Some { value } = lookup(key) else {
    return default_value;
};
```

```
// In a loop: skip non-matching items
let PacketType::Data { payload } = classify(raw) else {
    continue;
};
process(payload);
```

### LL(1) parse rule

```
LetStmt ::= 'let' ['mut'] LetTarget [':' Type] '=' Expr ElseClause? ';'

ElseClause ::= 'else' Block
```

**LL(1) analysis:** After parsing the `= Expr` part of a let statement, the parser peeks at the next token:

- `else` -- enter the else clause. Committed. Parse a block.
- `;` -- no else clause. Plain let or irrefutable destructure (existing path).

This is one-token lookahead on `else` vs `;`. No ambiguity. The `else` keyword is already reserved and tokenized.

Note: a plain `let x = expr else { ... };` (without a destructuring pattern) is grammatically valid but semantically meaningless -- it always matches. The checker should warn or error on this, not the parser. Keeping the grammar uniform simplifies both the parser and the mental model.

### Desugaring

```
let Result::Ok { value } = expr else {
    return err;
};
// ... rest of function using `value` ...
```

Desugars to:

```
match expr {
    Result::Ok { value } => {
        // ... rest of function using `value` ...
    },
    _ => {
        return err;
    },
}
```

The "rest of function" is everything after the `let...else` statement in the current block. The desugaring nests the continuation inside the match arm. This is the same transformation Rust uses for `let...else`.

Implementation note: the simplest implementation is to desugar during elaboration (the Elab pass), not in the parser. The parser produces a new AST node (`Stmt.letDestructure` or similar), and elaboration rewrites it to the existing `Expr.match_` form. This keeps the parser simple and puts the structural transformation in the phase designed for it.

### Divergence requirement

The else block must diverge. "Diverge" means: every control-flow path through the block must execute one of:

- `return` (exit the function)
- `break` (exit a loop)
- `continue` (skip to next loop iteration)
- A call to a function with return type `!` (never type, e.g., `abort()`)

The checker already tracks control flow for linearity analysis. Extending it to verify divergence in `let...else` blocks is a localized change: after desugaring, the wildcard match arm's body must not fall through.

---

## 4. What This Does NOT Include

These boundaries are deliberate. Each exclusion avoids a specific source of complexity.

### No bare variant names

```
// NOT supported:
let Some(x) = expr;

// MUST write:
let Option::Some { value } = expr;
```

Bare variant names (`Some`, `Ok`, `Err`) require the parser or resolver to know which enum a variant belongs to. This couples parsing to name resolution -- a phase separation violation. Concrete's `Type::Variant` syntax makes the enum explicit at every use site. This is more verbose but eliminates an entire category of ambiguity.

### No nested patterns

```
// NOT supported:
let Result::Ok { value: Option::Some { inner } } = expr;

// Instead, destructure in stages:
let Result::Ok { value } = expr else { return err; };
let Option::Some { inner } = value else { return err2; };
```

Nested patterns require recursive pattern parsing, complicate error messages, and make the desugaring non-trivial (multiple match levels). Staged destructuring is more explicit and produces better error locations.

### No `if let`

```
// NOT supported:
if let Option::Some { value } = expr {
    // use value
}
```

`if let` is syntactic sugar over `let...else` with a negated condition. It can be added later without breaking existing code. It is deferred because `let...else` covers the more common "extract or bail" pattern, and adding both at once doubles the parser/checker surface for one design cycle.

### No `while let`

```
// NOT supported:
while let Option::Some { item } = iter.next() {
    // process item
}
```

Same reasoning as `if let`. Deferred. When Concrete gains iterator patterns, `while let` becomes more valuable. Adding it now would be premature.

### No pattern matching in function parameters

```
// NOT supported:
fn process(Result::Ok { value }: Result<i32, Error>) -> i32 { ... }
```

Function parameter patterns require the parser to handle patterns in parameter position, complicating `parseParam`. They also make function signatures harder to read. Concrete requires explicit parameter names and types.

### No guard clauses

```
// NOT supported:
match expr {
    Option::Some { value } if value > 0 => { ... },
    _ => { ... },
}
```

Guard clauses add a conditional inside a match arm. They require `if` to be parsed in a new context (after a pattern, before `=>`), creating a grammar ambiguity with `if` as a statement. Deferred. Use `if` inside the match arm body.

### No tuple destructuring

```
// NOT supported:
let (a, b) = expr;
```

Concrete does not have tuple types. Structs serve the same role with named fields. Struct destructuring (section 2) handles this case.

### No `_` wildcard in patterns

```
// NOT supported:
let Result::Ok { value: _ } = expr;
```

For the first release, every binding name in a destructuring pattern binds a variable. There is no wildcard. If a field is not needed, bind it and let the unused-variable checker flag it (or, for Copy types, simply ignore it). Wildcards can be added later without grammar changes -- `_` is already a valid identifier character, but a standalone `_` would need to be special-cased.

---

## 5. Desugaring Rules

### Rule 1: Irrefutable enum destructuring

Source:
```
let EnumType::Variant { a, b, c } = expr;
CONTINUATION
```

Desugars to:
```
match expr {
    EnumType::Variant { a, b, c } => {
        CONTINUATION
    },
}
```

Where CONTINUATION is all remaining statements in the enclosing block. The checker verifies exhaustiveness: if `EnumType` has other variants, the match is non-exhaustive and compilation fails with an error telling the programmer to use `let...else` or a full match.

### Rule 2: Irrefutable struct destructuring

Source:
```
let StructType { x, y } = expr;
CONTINUATION
```

Desugars to:
```
let __tmp = expr;
let x = __tmp.x;
let y = __tmp.y;
CONTINUATION
```

Structs always have exactly one shape, so this is always irrefutable. The temporary `__tmp` is consumed by the field accesses. If `StructType` is Copy, the field accesses are copies. If linear, `__tmp` is consumed by extracting all its fields.

Note on linearity: if the struct is linear, all fields must be bound. Binding a subset of fields would leave the struct partially consumed, which violates linear ownership. The checker must verify that the binding list covers all fields of a linear struct. For Copy structs, partial binding is allowed (the un-bound fields are simply dropped as copies).

### Rule 3: Refutable `let...else`

Source:
```
let EnumType::Variant { a, b } = expr else {
    DIVERGING_BODY
};
CONTINUATION
```

Desugars to:
```
match expr {
    EnumType::Variant { a, b } => {
        CONTINUATION
    },
    _ => {
        DIVERGING_BODY
    },
}
```

The wildcard arm handles all non-matching variants. The checker verifies that `DIVERGING_BODY` diverges on all paths.

### Rule 4: `let...else` with `mut` bindings

Source:
```
let mut EnumType::Variant { a } = expr else {
    return err;
};
a = a + 1;
```

The `mut` applies to the bound variables. Desugars with mutable bindings:

```
match expr {
    EnumType::Variant { a } => {
        let mut a = a;  // rebind as mutable
        a = a + 1;
    },
    _ => {
        return err;
    },
}
```

---

## 6. Parser Changes Required

### New AST node

Add to `Stmt`:

```lean
| letDestructure (span : Span) (enumName : String) (variant : String)
    (bindings : List String) (mutable : Bool) (value : Expr)
    (elseBody : Option (List Stmt))
| letStructDestructure (span : Span) (structName : String)
    (bindings : List String) (mutable : Bool) (value : Expr)
```

Alternatively, a single node with a flag distinguishing enum vs struct destructuring.

### Modified `parseLet`

Current `parseLet` (Parser.lean lines 915-932):

```lean
partial def parseLet : ParseM Stmt := do
  let sp ← peekSpan
  expect .«let»
  let tk ← peek
  let isMut := tk == .mut
  if isMut then advance
  let name ← expectIdent
  -- DECISION POINT: peek at next token
  let tk ← peek
  -- ... existing: ':' or '=' ...
```

Modified logic after reading the first IDENT:

```
let name ← expectIdent
let tk ← peek
if tk == .doubleColon then
  -- Enum destructuring: name is the enum type name
  advance
  let variant ← expectIdent
  expect .lbrace
  let bindings ← parseBindingList
  expect .rbrace
  expect .assign
  let value ← parseExpr
  let elseBody ← parseOptionalElse
  expect .semicolon
  return .letDestructure sp name variant bindings isMut value elseBody
else if tk == .lbrace && name starts with uppercase then
  -- Struct destructuring: name is the struct type name
  advance
  let bindings ← parseBindingList
  expect .rbrace
  expect .assign
  let value ← parseExpr
  expect .semicolon
  return .letStructDestructure sp name bindings isMut value
else
  -- Existing path: plain let binding
  ...
```

### New helper: `parseBindingList`

```
parseBindingList ::= IDENT (',' IDENT)* [',']
```

This is the same shape as struct literal field lists but without the `: Expr` part. Simple loop: read identifiers separated by commas, allow trailing comma, stop at `}`.

### New helper: `parseOptionalElse`

```
parseOptionalElse := do
  let tk ← peek
  if tk == .else_ then
    advance
    let body ← parseBlock
    return some body
  else
    return none
```

### LL(1) verification

Every new decision point uses one-token lookahead:

| After consuming | Peek token | Decision |
|----------------|------------|----------|
| `let [mut] IDENT` | `::` | Enum destructuring |
| `let [mut] IDENT` | `{` (+ uppercase check) | Struct destructuring |
| `let [mut] IDENT` | `:` or `=` | Plain let binding |
| `... = Expr` | `else` | Has else clause |
| `... = Expr` | `;` | No else clause |

No save/restore. No backtracking. No speculative parse. The uppercase check is the same convention used in expression parsing for struct literals vs brace blocks (Parser.lean line 494).

---

## 7. Interaction with Linear Types

Destructuring must respect Concrete's linear ownership model.

### Consuming the scrutinee

When `let Type::Variant { a, b } = expr`, the expression `expr` is consumed. Its value is moved into the match. If the pattern matches, the variant's fields become new bindings. The original value no longer exists.

For Copy types, this is trivially correct -- the value is copied into the match, and the bindings are copies of the fields.

For linear types, the match consumes the value. The bindings `a` and `b` are the only way to access the fields. The original value is gone. This is the same as a regular match statement.

### All fields of linear types must be bound

If a linear enum variant has three fields and the destructuring only binds two, the third field is leaked. The checker must enforce that destructuring of a linear type binds all fields. This is the same rule that applies to match arms today.

```
// Error: linear type Result<File, IoError> -- all fields must be bound
let Result::Ok { value } = open(path) else { return err; };
// This is fine IF Result<File, IoError>::Ok has exactly one field named `value`
```

### `let...else` and the wildcard arm

In the desugared form, the wildcard arm (`_ => { DIVERGING_BODY }`) must handle all non-matching variants. If the enum contains linear values in other variants, the wildcard arm must consume them.

Since the else block diverges (returns, breaks, etc.), the non-matching value is consumed by the function exit. The match wildcard binding does not need to name the value -- the diverging body abandons the entire scope, and the compiler's existing match linearity rules handle cleanup. This is identical to how a wildcard match arm works today: the matched value is consumed by the arm even if the arm does not bind it.

### Copy enums: partial binding is allowed

For Copy enum variants, binding a subset of fields is allowed. Un-bound fields are simply unused copies. The unused-variable checker may warn, but there is no ownership violation.

---

## 8. Before/After Examples

### Example 1: Extract header or return error code

**Before** (8 lines):
```
match parse_header(good, 5) {
    ParseResult::Ok { header } => {
        if header.version != 1 { return 11; }
        if header.msg_type != 2 { return 12; }
    },
    ParseResult::Err { error } => { return 10; },
}
```

**After** (5 lines):
```
let ParseResult::Ok { header } = parse_header(good, 5) else {
    return 10;
};
if header.version != 1 { return 11; }
if header.msg_type != 2 { return 12; }
```

The control flow is linear instead of nested. The happy path is at the top indentation level.

### Example 2: Pipeline stage with error conversion

**Before** (10 lines):
```
fn handle_request(req: Request) -> ServiceResult {
    match validate(req) {
        ValidateResult::Err { error } => {
            return ServiceResult::Err {
                error: ServiceError::Validation { code: validate_error_code(error) }
            };
        },
        ValidateResult::Ok { val } => {
            return handle_validated(req);
        },
    }
}
```

**After** -- this case still needs match because the else block needs the error value:
```
fn handle_request(req: Request) -> ServiceResult {
    match validate(req) {
        ValidateResult::Err { error } => {
            return ServiceResult::Err {
                error: ServiceError::Validation { code: validate_error_code(error) }
            };
        },
        ValidateResult::Ok { val } => {
            return handle_validated(req);
        },
    }
}
```

This is an important boundary: `let...else` does not help when the else block needs to inspect the non-matching variant. The design deliberately does not try to solve this case. Error conversion is addressed by `map_err`-style helpers (see [ERROR_HANDLING_DESIGN.md](ERROR_HANDLING_DESIGN.md)).

### Example 3: Test assertions with expected errors

**Before** (5 lines, repeated 9 times in service_errors):
```
match handle_request(bad_user) {
    ServiceResult::Ok { response } => { return 20; },
    ServiceResult::Err { error } => {
        if service_error_code(error) != 101 { return 21; }
    },
}
```

**After** (3 lines):
```
let ServiceResult::Err { error } = handle_request(bad_user) else {
    return 20;
};
if service_error_code(error) != 101 { return 21; }
```

The pattern is inverted: destructure the expected variant (Err), bail if it is unexpectedly Ok. This reads naturally for negative tests.

### Example 4: Struct destructuring

**Before** (3 lines):
```
let resp: Response = process_action(req.action, req.payload);
let status: i32 = resp.status;
let result_val: i32 = resp.result_val;
```

**After** (1 line):
```
let Response { status, result_val } = process_action(req.action, req.payload);
```

---

## 9. Comparison with Other Languages

### Rust: `let...else` (stabilized in Rust 1.65)

```rust
let Ok(value) = validate(req) else {
    return Err(ServiceError::Validation);
};
```

Rust uses bare variant names (`Ok`, `Err`). This works because Rust's name resolution knows which enum a variant belongs to. Concrete cannot do this without coupling the parser to name resolution (violates phase separation). Concrete requires `Type::Variant` instead.

Rust allows nested patterns (`let Ok(Some(x)) = ...`). Concrete does not, by design, to keep the first release simple.

Rust's `let...else` else block must diverge. Same in Concrete.

### Swift: `guard let`

```swift
guard let value = validate(req) else {
    return .validationError
}
```

Swift's `guard let` is conceptually identical to `let...else` with the syntax inverted: the `guard` keyword introduces the construct, and the else clause is mandatory. Swift uses optional binding (implicit unwrapping of `Optional<T>`), which requires type-level knowledge of optionality. Concrete's form is more explicit: the programmer writes the full enum type and variant.

### Kotlin: `val...?:` (Elvis operator)

```kotlin
val value = validate(req) ?: return ServiceError.Validation
```

Kotlin's elvis operator `?:` provides a default when the left side is null. It is limited to nullable types and cannot destructure arbitrary enum variants. Concrete's `let...else` is more general.

### Zig: `if` capture and `orelse`

```zig
const value = validate(req) orelse |err| return err;
// or
if (validate(req)) |value| {
    // use value
} else |err| {
    return err;
}
```

Zig's `orelse` provides a fallback when an optional/error union is null/error. The `if` capture syntax binds the payload. Both forms give access to the error value in the else branch. Concrete's `let...else` deliberately does not provide error access in the else block (Option A, section 3) -- this is a simplicity tradeoff.

### Summary table

| Feature | Concrete | Rust | Swift | Kotlin | Zig |
|---------|----------|------|-------|--------|-----|
| Keyword | `let...else` | `let...else` | `guard let...else` | `val...?:` | `orelse` / `if` capture |
| Requires full type | Yes (`Type::Variant`) | No (bare variant) | No (implicit optional) | No (nullable) | No (error union) |
| Nested patterns | No | Yes | No | N/A | No |
| Else block access to error | No | No | No | N/A | Yes |
| Else must diverge | Yes | Yes | Yes | No (Elvis returns value) | No (orelse returns value) |
| LL(1) parseable | Yes | Yes (keywords) | Yes (guard keyword) | N/A | Yes |

---

## 10. Implementation Sequence

1. **AST**: Add `Stmt.letDestructure` and `Stmt.letStructDestructure` nodes.
2. **Parser**: Extend `parseLet` with the decision logic described in section 6. Add `parseBindingList` and `parseOptionalElse` helpers.
3. **Elaboration**: In the Elab pass, desugar the new AST nodes to existing `Expr.match_` forms. This is where the structural transformation happens.
4. **Checker**: Verify exhaustiveness for irrefutable patterns (existing match exhaustiveness). Verify divergence of else blocks (extend existing control-flow analysis). Verify all fields of linear types are bound.
5. **Tests**: Add parser tests for the new syntax. Add checker tests for irrefutable vs refutable errors, divergence enforcement, and linear field coverage. Add integration tests showing before/after equivalence.
6. **Free variable analysis**: Update `collectFreeVarsStmts` in AST.lean to handle the new statement nodes (extract bindings as new bound variables, recurse into value expression and else body).

No changes are needed to Core IR, SSA, LLVM emission, or the proof pipeline. The desugaring happens before those phases see the code.

---

## 11. Future Extensions

These are not part of this design but would be natural additions later:

- **`if let`**: `if let Type::Variant { x } = expr { ... }` -- syntactic sugar for a match with two arms (one matching, one fallthrough). Adds convenience for "do something if it matches" without the divergence requirement.
- **`while let`**: `while let Type::Variant { x } = expr { ... }` -- loop while a pattern matches. Useful with iterators.
- **`_` wildcard in binding lists**: Allow `_` to discard a field without binding it. Requires special-casing `_` in the binding list parser. Useful for Copy types where not all fields are needed.
- **Nested patterns**: `let Result::Ok { value: Option::Some { inner } } = expr else { ... }` -- multi-level destructuring in a single statement. Requires recursive pattern parsing.
- **Else block with error access**: `let Result::Ok { value } = expr else |other| { ... }` -- the else block receives the non-matching value. Requires a binding syntax in the else clause.

Each of these can be added incrementally without breaking existing code.

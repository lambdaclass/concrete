# Error Handling Design: Result/Error-Flow Ergonomics and Helper APIs

Status: design/freeze reference (Phase 3, items 63 and 71)

This document covers two tightly coupled items:

- **Item 63**: Make Result/error-flow ergonomics a first-class stdlib quality target.
- **Item 71**: Define the library helper floor before considering more syntax.

Both items share a single constraint: Concrete's error handling must remain explicit, auditable, and free of hidden control flow. The improvements proposed here are library-only. No new syntax is introduced beyond the `?` operator that already exists.

For the failure model, see [FAILURE_STRATEGY.md](FAILURE_STRATEGY.md).
For the predictable failure discipline, see [PREDICTABLE_FAILURE_DISCIPLINE.md](PREDICTABLE_FAILURE_DISCIPLINE.md).
For what is permanently excluded, see [ANTI_FEATURES.md](ANTI_FEATURES.md).
For stdlib design principles, see [STDLIB_DESIGN_PRINCIPLES.md](STDLIB_DESIGN_PRINCIPLES.md).
For the stdlib module inventory, see [STDLIB_TARGET.md](STDLIB_TARGET.md).

---

## Part 1: Error Flow Ergonomics (Item 58)

### Current State

Today, `std.result` and `std.option` provide:

- `Result<T, E>` with variants `Ok { value: T }` and `Err { error: E }`
- `Option<T>` with variants `Some { value: T }` and `None`
- `is_ok`, `is_err` on Result
- `is_some`, `is_none` on Option
- The `?` operator (parsed, type-checked, lowered to SSA)

Nothing else. No `unwrap_or`, no `map_err`, no conversion helpers. Every operation beyond querying the variant requires a full `match`.

### Pain Points from Existing Code

Three classes of real code expose the cost of this minimal surface.

#### Pain point 1: Ad hoc result types instead of Result

Pressure programs cannot use `Result<T, E>` because there are no helpers to extract values without a match block. Instead, they define custom result structs with an `ok: i32` field:

```
// From pressure_parse_json_subset.con
struct Copy IntResult {
    cursor: Cursor,
    value: i32,
    ok: i32,
}
```

This pattern appears in `pressure_parse_json_subset.con` (IntResult, BoolResult, StringResult), `pressure_parse_http_request.con` (MethodResult, PathResult, VersionResult), `pressure_fixcap_ring_buffer.con` (IntPopResult), and `pressure_parse_dns_packet.con`. Each program defines 2-4 ad hoc result types. The `ok: i32` field is a sentinel that the caller must remember to check. There is no compiler enforcement.

With `Result<T, E>` plus `unwrap_or` or `?`, these ad hoc types are unnecessary and the error discipline is compiler-enforced.

#### Pain point 2: Verbose match cascades in multi-stage pipelines

The shipped `service_errors` example already uses builtin `Result<T, E>` throughout, but it still has to spell out a full match block each time one stage-specific error enum is converted into the unified `ServiceError`:

```
// From examples/service_errors/src/main.con
fn handle_request(req: Request) -> Result<Response, ServiceError> {
    match validate(req) {
        Result::Err { error } => {
            return Result::<Response, ServiceError>::Err {
                error: ServiceError::Validation { code: validate_error_code(error) }
            };
        },
        Result::Ok { value } => {
            return handle_validated(req);
        },
    }
}
```

This is 9 lines for what is conceptually "propagate the error with a type conversion." With `map_err` or a `with_context`-style helper, this would be 1-2 lines. With `?` (already supported), the propagation itself is one character, but the error type conversion still requires a manual wrapping step before `?` can apply.

#### Pain point 3: Builtin Result still needs better library relief

The `parse_validate` example no longer defines a custom `ParseResult`; it uses builtin `Result<Header, ParseError>` directly. That cleaned up the public surface and made `?` possible in principle, but the remaining ergonomic pressure did not disappear. Without helpers like `map_err`, `and_then`, and the later destructuring forms, fallible code still expands into explicit match blocks quickly.

That is the right tradeoff for now: one canonical `Result` is much better than two competing surfaces. The remaining job is library relief and narrowly-scoped syntax relief, not inventing more result types.

### Target Ergonomics

Idiomatic error handling in Concrete should look like this:

**Single-function error propagation with `?`:**

```
fn parse_header(data: [i32; 8], len: i32) -> Result<Header, ParseError> {
    validate_length(len, 5)?;
    let version: i32 = validate_version(data[0])?;
    let msg_type: i32 = validate_msg_type(data[1])?;
    let payload_len: i32 = validate_payload_len(data[2], 240)?;
    validate_total_len(len, 4 + payload_len)?;
    validate_checksum(data[4], compute_checksum(data, 4))?;
    return Result::<Header, ParseError>::Ok { value: Header {
        version: version, msg_type: msg_type,
        payload_len: payload_len, checksum: data[4],
    }};
}
```

**Multi-stage pipeline with error conversion:**

```
fn handle_request(req: Request) -> Result<Response, ServiceError> {
    validate(req).map_err_with::<ServiceError>(to_service_validation)?;
    authorize(req).map_err_with::<ServiceError>(to_service_auth)?;
    check_rate_limit(req.user_id, req.payload)
        .map_err_with::<ServiceError>(to_service_rate)?;
    let resp: Response = process_action(req.action, req.payload);
    return Result::<Response, ServiceError>::Ok { value: resp };
}
```

**Value extraction with fallback:**

```
let count: i32 = opt_count.unwrap_or(0);
let name: i32 = lookup_user(id).unwrap_or(default_name);
```

### Parser/Validate Pattern

A parser/validator function typically:

1. Receives raw input (byte buffer, field array, cursor)
2. Performs a sequence of checks, each of which can fail with a different error variant
3. Returns a structured result on success

The error enum is flat with one variant per failure category (TooShort, BadVersion, BadType, etc.). All variants are Copy. Error propagation is linear: each check either succeeds (continue) or fails (return Err immediately).

**What works today:** `?` already handles this pattern when the function returns `Result<T, E>`. Each validation step returns `Result<ValidatedValue, ParseError>` and `?` propagates the error.

**What is missing:** validation functions that return `Result<(), ParseError>` (success with no interesting value) need a way to discard the unit value. Today `?` extracts the value from the Ok variant. For `Result<(), ParseError>`, the caller would write `validate_version(data[0])?;` and discard the `()`. This works if the Ok variant's field type is unit — this should be tested and documented.

### Service/Multi-Stage Pattern

A service pipeline typically:

1. Runs multiple stages (validate, authorize, rate-limit, process)
2. Each stage has its own error enum
3. A unified error enum wraps all stage errors
4. Each stage's error must be converted to the unified type before propagation

**What works today:** match-and-convert works but is verbose. `?` propagates within a single error type but cannot convert between error types.

**What is missing:** a way to convert error types during propagation. In Rust, `From` trait implementations let `?` convert automatically. Concrete has no implicit trait resolution (ANTI_FEATURES.md: permanent exclusion). The Concrete answer is an explicit conversion function passed to a `map_err`-style helper. Since closures are excluded (ANTI_FEATURES.md: permanent exclusion), this helper must accept a function pointer.

### Linear Types and Result

Concrete's linear type system requires that every non-Copy value is consumed exactly once. Result interacts with this in two ways:

1. **Result itself is Copy when both T and E are Copy.** This is the common case for error enums, integers, and small structs. All examples and pressure programs today use Copy payloads. No linear consumption issue arises.

2. **Result containing a linear value must be consumed.** If `Result<File, IoError>` is returned, the caller must match it and either use the `File` (consuming it) or handle the error. The `?` operator handles this correctly: on the Ok path, the value is extracted and bound; on the Err path, the entire Result is returned to the caller (who must consume it). Helpers like `unwrap_or` would consume the Result by extracting the value or the default.

**The rule:** helpers on `Result<T, E>` where T is linear must consume the Result. They take `self` (not `&self`). Methods that only query (is_ok, is_err) take `&self` and do not consume.

### No Hidden Control Flow

Concrete's error handling guarantee is: **every exit from a function is visible in the source**.

- `return` is an explicit keyword
- `?` is sugar for an early return with `Err` — the `?` is visible at the call site
- `match` arms are exhaustive — every variant must be handled
- There are no exceptions, no panics-as-control-flow, no longjmp, no hidden unwinding
- `defer` runs on all exit paths (normal return, `?`-triggered return, break/continue)

The helpers proposed in this document preserve this guarantee. None of them introduce hidden control flow:

- `map`, `map_err`, `and_then`, `or_else` are pure transformations
- `unwrap_or`, `unwrap_or_else` are pure extractions with fallback
- `with_context` wraps an error value — no control flow change
- `ok()`, `err()` are type conversions — no control flow change

The only operator that affects control flow is `?`, and it is already implemented, tested, and documented as visible sugar for early return.

---

## Part 2: Result/Option Helper APIs (Item 63)

### Constraint: No Closures

Concrete permanently excludes closures (ANTI_FEATURES.md). This directly affects the API design for `map`, `and_then`, `or_else`, and similar higher-order helpers that in Rust take a closure.

**The alternatives for Concrete are:**

1. **Function pointer variants:** `map_with(fn_ptr)` where the function pointer is a named function. This works today — Concrete supports function pointers with C-compatible calling convention. The limitation is that function pointers cannot capture state.

2. **Deferred until closures or generic fn-ptr ergonomics exist:** mark these APIs as future work and provide match-based patterns in examples.

3. **Monomorphized generic methods with function pointer parameters:** `map<U>(f: fn(T) -> U) -> Result<U, E>`. This requires generics on methods (which exist today in the compiler) and function pointer types in generic positions.

**Decision for first release:** implement the subset of helpers that do not require function arguments first. Defer the function-pointer-taking helpers (map, and_then, or_else, map_err as generic) until function-pointer-in-generic ergonomics are validated. Provide concrete-typed workaround variants (like `map_err_with`) that take explicit function pointers for the most critical use case (error type conversion).

### Result<T, E> API Surface

Listed in priority order. Each API notes its compiler prerequisites and whether it ships in the first release.

#### Tier 1: Ship in First Release (no function pointer needed)

**`is_ok(&self) -> bool`**
Already exists. Returns true if the Result is Ok.

**`is_err(&self) -> bool`**
Already exists. Returns true if the Result is Err.

**`unwrap_or(self, default: T) -> T`**
Returns the Ok value, or `default` if Err. Consumes the Result.

```
impl<T, E> Result<T, E> {
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Result::Ok { value } => { return value; },
            Result::Err { error } => { return default; },
        }
    }
}
```

**Prerequisite:** generic method that takes `self` by value. The compiler already supports `impl<T, E> Result<T, E>` with `&self` methods. Consuming `self` methods need validation.

**`ok(self) -> Option<T>`**
Converts `Result<T, E>` to `Option<T>`, discarding the error.

```
impl<T, E> Result<T, E> {
    pub fn ok(self) -> Option<T> {
        match self {
            Result::Ok { value } => { return Option::<T>::Some { value: value }; },
            Result::Err { error } => { return Option::<T>::None; },
        }
    }
}
```

**Prerequisite:** generic method returning a different generic type. Requires `Option<T>` to be in scope (it is — both are builtins).

**`err(self) -> Option<E>`**
Converts `Result<T, E>` to `Option<E>`, discarding the success value.

```
impl<T, E> Result<T, E> {
    pub fn err(self) -> Option<E> {
        match self {
            Result::Ok { value } => { return Option::<E>::None; },
            Result::Err { error } => { return Option::<E>::Some { value: error }; },
        }
    }
}
```

#### Tier 2: Ship If Function-Pointer-in-Generic Works

These helpers require passing a function pointer as an argument to a generic method. The compiler supports function pointer types (`fn(T) -> U`) and generics, but the combination in method signatures needs validation.

**`map<U>(self, f: fn(T) -> U) -> Result<U, E>`**
Transforms the Ok value, leaving Err untouched.

```
impl<T, E> Result<T, E> {
    pub fn map<U>(self, f: fn(T) -> U) -> Result<U, E> {
        match self {
            Result::Ok { value } => {
                return Result::<U, E>::Ok { value: f(value) };
            },
            Result::Err { error } => {
                return Result::<U, E>::Err { error: error };
            },
        }
    }
}
```

**`map_err<F>(self, f: fn(E) -> F) -> Result<T, F>`**
Transforms the Err value, leaving Ok untouched. This is the critical helper for service-pipeline error conversion.

```
impl<T, E> Result<T, E> {
    pub fn map_err<F>(self, f: fn(E) -> F) -> Result<T, F> {
        match self {
            Result::Ok { value } => {
                return Result::<T, F>::Ok { value: value };
            },
            Result::Err { error } => {
                return Result::<T, F>::Err { error: f(error) };
            },
        }
    }
}
```

**Usage with the service_errors pattern:**

```
fn to_service_error(e: ValidateError) -> ServiceError {
    return ServiceError::Validation { code: validate_error_code(e) };
}

fn handle_request(req: Request) -> Result<Response, ServiceError> {
    validate(req).map_err::<ServiceError>(to_service_error)?;
    // ...
}
```

**`and_then<U>(self, f: fn(T) -> Result<U, E>) -> Result<U, E>`**
Chains a fallible operation on the Ok value.

```
impl<T, E> Result<T, E> {
    pub fn and_then<U>(self, f: fn(T) -> Result<U, E>) -> Result<U, E> {
        match self {
            Result::Ok { value } => { return f(value); },
            Result::Err { error } => { return Result::<U, E>::Err { error: error }; },
        }
    }
}
```

**`or_else<F>(self, f: fn(E) -> Result<T, F>) -> Result<T, F>`**
Chains a fallible recovery on the Err value.

```
impl<T, E> Result<T, E> {
    pub fn or_else<F>(self, f: fn(E) -> Result<T, F>) -> Result<T, F> {
        match self {
            Result::Ok { value } => { return Result::<T, F>::Ok { value: value }; },
            Result::Err { error } => { return f(error); },
        }
    }
}
```

**`unwrap_or_else(self, f: fn(E) -> T) -> T`**
Returns the Ok value, or computes a fallback from the error.

```
impl<T, E> Result<T, E> {
    pub fn unwrap_or_else(self, f: fn(E) -> T) -> T {
        match self {
            Result::Ok { value } => { return value; },
            Result::Err { error } => { return f(error); },
        }
    }
}
```

#### Tier 3: Deferred (Needs Design Work)

**`with_context(self, ctx: C) -> Result<T, ContextError<C, E>>`**
Wraps the error with additional context. This requires a generic `ContextError` wrapper type or a convention for context-carrying error enums. The design depends on how error enums compose in practice.

**Deferred because:** the right design depends on real error-wrapping patterns emerging from more examples. The service_errors example uses flat enum wrapping (ServiceError::Validation { code }), not nested context. Until we see code that genuinely needs nested context (not just error conversion), this is premature.

**Workaround:** use `map_err` with a function that wraps the error in a larger enum variant. This is what the service_errors example already does, and it is explicit and auditable.

### Option<T> API Surface

#### Tier 1: Ship in First Release

**`is_some(&self) -> bool`**
Already exists.

**`is_none(&self) -> bool`**
Already exists.

**`unwrap_or(self, default: T) -> T`**
Returns the Some value, or `default` if None.

```
impl<T> Option<T> {
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Option::Some { value } => { return value; },
            Option::None => { return default; },
        }
    }
}
```

**`ok_or<E>(self, err: E) -> Result<T, E>`**
Converts `Option<T>` to `Result<T, E>`, using the provided error if None.

```
impl<T> Option<T> {
    pub fn ok_or<E>(self, err: E) -> Result<T, E> {
        match self {
            Option::Some { value } => {
                return Result::<T, E>::Ok { value: value };
            },
            Option::None => {
                return Result::<T, E>::Err { error: err };
            },
        }
    }
}
```

This is the bridge between Option and Result. It enables patterns like:

```
let user: User = lookup(id).ok_or::<LookupError>(LookupError::NotFound)?;
```

#### Tier 2: Ship If Function-Pointer-in-Generic Works

**`map<U>(self, f: fn(T) -> U) -> Option<U>`**
Transforms the Some value.

```
impl<T> Option<T> {
    pub fn map<U>(self, f: fn(T) -> U) -> Option<U> {
        match self {
            Option::Some { value } => {
                return Option::<U>::Some { value: f(value) };
            },
            Option::None => { return Option::<U>::None; },
        }
    }
}
```

**`and_then<U>(self, f: fn(T) -> Option<U>) -> Option<U>`**
Chains a fallible operation.

**`or_else(self, f: fn() -> Option<T>) -> Option<T>`**
Provides a fallback computation.

**`unwrap_or_default(self) -> T`**
Deferred. Requires a Default trait or convention, which Concrete does not have.

#### Tier 3: Deferred

**`unwrap_or_default(self) -> T`**
Requires a trait or convention for default values. Not possible without either a Default trait (which would need implicit trait resolution, permanently excluded) or a separate `unwrap_or_default_int`, `unwrap_or_default_bool` style API (ugly, ad hoc). Deferred.

### The `?` Operator: Current State and Future

#### What exists today

The `?` operator is fully implemented:

- **Lexer:** `?` is tokenized as `Token.question` (`Lexer.lean:300`)
- **Parser:** postfix `?` on expressions creates `Expr.try_` AST node (`Parser.lean:652-654`, `Parser.lean:713-715`)
- **Type checker:** validates that the expression type is an enum with `Ok` and `Err` variants, and that the enclosing function returns the same Result type (`Check.lean:1695-1722`)
- **Lowering:** generates SSA that loads the tag, branches on Ok/Err, runs deferred calls on the Err path, and extracts the Ok payload on the Ok path (`Lower.lean:955-990`)

The `?` operator:

- Works with both non-generic `Result { Ok, Err }` and generic `Result<T, E>`
- Runs all deferred cleanup on the Err path (defer + `?` interaction is tested in `defer_try.con` and `test_defer_try_nested.con`)
- Chains correctly (`complex_error_chain.con` chains three `?` operations)
- Works with user-defined enums that have `Ok` and `Err` variants (not just the builtin Result)

#### What `?` desugars to

`let v: T = expr?;` desugars to approximately:

```
let __tmp: Result<T, E> = expr;
match __tmp {
    Result::Ok { value } => { /* v = value; continue */ },
    Result::Err { error } => {
        /* run all deferred calls */
        return __tmp;
    },
}
```

The Err path returns the entire Result value (not just the error), preserving the type for the caller.

#### Limitation: no cross-type error conversion

Today, `?` requires the expression's Result type to exactly match the enclosing function's return type. This means:

```
fn handle(req: Request) -> Result<Response, ServiceError> {
    // ERROR: validate returns Result<i32, ValidateError>,
    // but this function returns Result<Response, ServiceError>
    validate(req)?;
}
```

In Rust, `From<ValidateError> for ServiceError` would make this work via implicit conversion. Concrete has no implicit trait resolution (permanent exclusion). The workaround is:

```
fn handle(req: Request) -> Result<Response, ServiceError> {
    validate(req).map_err::<ServiceError>(to_service_error)?;
}
```

This is more explicit than Rust's implicit `From` conversion but less verbose than a full match block. It is the recommended Concrete pattern.

#### Should `?` be frozen or extended?

**Recommendation: freeze `?` at its current semantics for first release.**

The current `?` is well-defined, well-tested, and sufficient. It handles the common case (same error type throughout a function) cleanly. The uncommon case (cross-type error conversion) is handled by `map_err` + `?`.

Extending `?` to support automatic error conversion would require either:
- Implicit trait resolution (permanently excluded)
- An explicit `into` annotation on the `?` site (new syntax, against item 71's mandate)
- A special-case conversion lookup (ad hoc, fragile)

None of these are worth the complexity. `map_err` + `?` is explicit, auditable, and sufficient.

### Before/After Examples

#### Example 1: Parse result extraction

**Before (today's code, from pressure_parse_json_subset.con):**

```
struct Copy IntResult {
    cursor: Cursor,
    value: i32,
    ok: i32,
}

fn parse_int(c: Cursor) -> IntResult {
    // ...
    if digits == 0 {
        return IntResult { cursor: c, value: 0, ok: 0 };
    }
    return IntResult { cursor: cur, value: val * sign, ok: 1 };
}

// Caller:
let ir: IntResult = parse_int(cur);
if ir.ok != 1 { break; }
// use ir.value and ir.cursor
```

**After (with Result + unwrap_or or ?):**

```
enum Copy ParseError {
    NoDigits,
    // ...
}

fn parse_int(c: Cursor) -> Result<IntValue, ParseError> {
    // ...
    if digits == 0 {
        return Result::<IntValue, ParseError>::Err { error: ParseError::NoDigits };
    }
    return Result::<IntValue, ParseError>::Ok { value: IntValue {
        cursor: cur, value: val * sign
    }};
}

// Caller (with ?):
let iv: IntValue = parse_int(cur)?;
// use iv.value and iv.cursor
```

The ad hoc `ok: i32` field is gone. Error handling is compiler-enforced. The `?` propagates the error to the caller.

#### Example 2: Multi-stage service pipeline

**Before (today's code, from examples/service_errors):**

```
// 3 custom result enums (ValidateResult, AuthResult, RateResult)
// 1 unified result enum (ServiceResult)
// Each pipeline function: 9-line match block per stage

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

**After (with Result + map_err + ?):**

```
fn to_validate_svc(e: ValidateError) -> ServiceError {
    return ServiceError::Validation { code: validate_error_code(e) };
}

fn to_auth_svc(e: AuthError) -> ServiceError {
    return ServiceError::Auth { code: auth_error_code(e) };
}

fn to_rate_svc(e: RateLimitError) -> ServiceError {
    return ServiceError::RateLimit { code: rate_error_code(e) };
}

fn handle_request(req: Request) -> Result<Response, ServiceError> {
    validate(req).map_err::<ServiceError>(to_validate_svc)?;
    authorize(req).map_err::<ServiceError>(to_auth_svc)?;
    check_rate_limit(req.user_id, req.payload)
        .map_err::<ServiceError>(to_rate_svc)?;
    let resp: Response = process_action(req.action, req.payload);
    return Result::<Response, ServiceError>::Ok { value: resp };
}
```

The three custom result enums (ValidateResult, AuthResult, RateResult) are eliminated. Each stage function returns `Result<i32, StageError>`. The unified ServiceResult is replaced by `Result<Response, ServiceError>`. The match-and-convert blocks become one-line `map_err` + `?` chains.

#### Example 3: Optional value extraction

**Before (ad hoc pattern from pressure_fixcap_ring_buffer.con):**

```
struct Copy IntPopResult {
    value: i32,
    ring: IntRing,
    ok: i32,
}

let pr: IntPopResult = int_ring_pop(ring);
if pr.ok == 0 {
    // use default
    result = 0;
} else {
    result = pr.value;
}
```

**After (with Option + unwrap_or):**

```
fn int_ring_pop(r: IntRing) -> Option<PopValue> {
    if r.count == 0 {
        return Option::<PopValue>::None;
    }
    // ...
    return Option::<PopValue>::Some { value: PopValue { val: val, ring: new_ring } };
}

let pop: Option<PopValue> = int_ring_pop(ring);
// If we only care about the value:
let val: i32 = pop.map::<i32>(get_pop_val).unwrap_or(0);
```

### Implementation Priority

The helpers break into three implementation waves based on compiler prerequisites:

**Wave 1 (no new compiler work needed):**
- `Result.unwrap_or(self, default: T) -> T`
- `Option.unwrap_or(self, default: T) -> T`
- `Option.ok_or<E>(self, err: E) -> Result<T, E>`
- `Result.ok(self) -> Option<T>`
- `Result.err(self) -> Option<E>`

These require only consuming-self generic methods and cross-generic-type returns, both of which the existing `impl<T, E>` mechanism should support.

**Wave 2 (requires function-pointer-in-generic-method validation):**
- `Result.map<U>(self, f: fn(T) -> U) -> Result<U, E>`
- `Result.map_err<F>(self, f: fn(E) -> F) -> Result<T, F>`
- `Result.and_then<U>(self, f: fn(T) -> Result<U, E>) -> Result<U, E>`
- `Result.or_else<F>(self, f: fn(E) -> Result<T, F>) -> Result<T, F>`
- `Result.unwrap_or_else(self, f: fn(E) -> T) -> T`
- `Option.map<U>(self, f: fn(T) -> U) -> Option<U>`
- `Option.and_then<U>(self, f: fn(T) -> Option<U>) -> Option<U>`
- `Option.or_else(self, f: fn() -> Option<T>) -> Option<T>`

These require the compiler to accept `fn(T) -> U` in a generic method signature where T and U are type parameters. If this works today, these move to Wave 1. If it requires compiler work, that work is bounded (function pointer types already exist; generics already exist; the combination is the gap).

**Wave 3 (deferred, needs design evidence):**
- `with_context` / `context` for nested error wrapping
- `unwrap_or_default` (needs Default convention)
- Any new syntax or `?` extensions

### Validation Criteria

This design is validated when:

1. `examples/parse_validate/` is rewritten to use `Result<Header, ParseError>` with `?` instead of the custom `ParseResult` enum, and compiles and runs correctly.
2. `examples/service_errors/` is rewritten to use `Result<Response, ServiceError>` with `map_err` + `?` instead of three custom result enums, and compiles and runs correctly.
3. At least one pressure program (`pressure_parse_json_subset.con` or `pressure_parse_http_request.con`) is rewritten to use `Result<T, E>` instead of ad hoc `ok: i32` result structs.
4. `unwrap_or` works for both `Result` and `Option` in a compiled test.
5. If Wave 2 lands: `map_err` + `?` works in a compiled test that converts between error types.

### Summary Table

| API | Type | Tier | Needs fn ptr | First release? |
|-----|------|------|-------------|----------------|
| `is_ok` | Result | 1 | No | Yes (exists) |
| `is_err` | Result | 1 | No | Yes (exists) |
| `unwrap_or` | Result | 1 | No | Yes |
| `ok` | Result | 1 | No | Yes |
| `err` | Result | 1 | No | Yes |
| `map` | Result | 2 | Yes | If fn-ptr-in-generic works |
| `map_err` | Result | 2 | Yes | If fn-ptr-in-generic works |
| `and_then` | Result | 2 | Yes | If fn-ptr-in-generic works |
| `or_else` | Result | 2 | Yes | If fn-ptr-in-generic works |
| `unwrap_or_else` | Result | 2 | Yes | If fn-ptr-in-generic works |
| `with_context` | Result | 3 | Depends | Deferred |
| `is_some` | Option | 1 | No | Yes (exists) |
| `is_none` | Option | 1 | No | Yes (exists) |
| `unwrap_or` | Option | 1 | No | Yes |
| `ok_or` | Option | 1 | No | Yes |
| `map` | Option | 2 | Yes | If fn-ptr-in-generic works |
| `and_then` | Option | 2 | Yes | If fn-ptr-in-generic works |
| `or_else` | Option | 2 | Yes | If fn-ptr-in-generic works |
| `unwrap_or_default` | Option | 3 | No | Deferred (needs Default) |
| `?` operator | Syntax | -- | No | Yes (exists, freeze) |

### Constraints Recap

From Wave 1 documents and permanent exclusions:

- **No exceptions** (ANTI_FEATURES.md) — Result is the only error mechanism. This design reinforces that.
- **No closures** (ANTI_FEATURES.md) — function-pointer-taking helpers are the ceiling. No captured-state lambdas.
- **No implicit trait resolution** (ANTI_FEATURES.md) — `?` cannot auto-convert error types. `map_err` is the explicit alternative.
- **Abort-only for unrecoverable failures** (FAILURE_STRATEGY.md) — helpers are for recoverable errors only. OOM and hardware traps still abort.
- **Explicit is better than implicit** (STDLIB_DESIGN_PRINCIPLES.md, principle 8) — every error conversion is a visible function call.
- **Uniform error vocabulary** (STDLIB_DESIGN_PRINCIPLES.md, principle 10) — all fallible stdlib operations use `Result<T, ModuleError>`. The helpers make this pattern ergonomic.
- **Every API must justify its existence** (STDLIB_DESIGN_PRINCIPLES.md, principle 7) — every helper listed here points to real code (pressure programs or canonical examples) that would be shorter, clearer, or more correct with it.

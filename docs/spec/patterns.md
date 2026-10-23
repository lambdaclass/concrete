# Patterns

A pattern destructures one level: a variant with its fields, a struct with its fields, a literal,
an integer range, a binding or `_`. Patterns appear in `match` arms, `if let`, `while let`, and
destructuring `let` (with an optional `else`). How a pattern binds through a reference scrutinee
is REF-9 (pending), and which values `_` may ignore is OWN-5.

Two things are deliberately not in the language. **Nested patterns** (`E::Wrap { P { x, y } }`)
are deferred until a workload needs them; one level of destructuring plus a field access or an
inner `match` expresses the same programs ([NESTED_PATTERNS.md](../language/NESTED_PATTERNS.md),
`check_nested_patterns.sh`). **Tuples** do not exist; the named struct is the one product type
([TUPLES.md](../language/TUPLES.md), `check_no_tuples.sh`). The design records are
[PATTERN_ERGONOMICS.md](../language/PATTERN_ERGONOMICS.md) and
[PATTERN_DESTRUCTURING.md](../language/PATTERN_DESTRUCTURING.md).

## Rules

<a id="PAT-1"></a>
### PAT-1. A pattern field may be bound under another name

**PAT-1@1.** A struct or enum pattern in `match`, `if let`, `while let` or a destructuring `let` may bind a field under another name with `field: binder`, and the formatter keeps the label.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseBindingList
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** SCOPE-1 forbids a binder that reuses a visible name. Without this rule, a nested
`Option` or `Result` match could not be written, because both payload fields are named
`value`. If any entry of a pattern is labeled, every entry names its field, each field once,
and every field is listed (`field: _` ignores a Copy field). Breaking that is E0304, which
migrates here with the rest of the patterns chapter. This was gap G4 of MEMORY_SEMANTICS §15.

```con accept name=nested-option exit=42
fn nested(o: Option<Option<Int>>) -> Int {
    match o {
        Option::Some { value: outer } => {
            match outer {
                Option::Some { value: inner } => { return inner; },
                Option::None => { return 0; }
            }
        },
        Option::None => { return 0; }
    }
}

fn main() -> Int {
    return nested(Option::Some { value: Option::Some { value: 42 } });
}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_renamed_pattern_binder.con` | `run:42` | `-` | `binary,interp,fmt` |

<a id="PAT-2"></a>
### PAT-2. A match is exhaustive

**PAT-2@1.** A `match` covers every value of its scrutinee: over an enum, every variant has an unguarded arm or an unguarded catch-all (`_` or a binding) follows, and over any other type an unguarded catch-all follows the literal and range arms, a guarded arm never counting as coverage.

- Kind: refusal
- Uses: none
- Codes: E0530 (core-check), E0534 (core-check)
- Status: enforced
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never falls off its last arm (R-0507)
- Evidence: tested

**Rationale.** A range is not a catch-all, even one that spans the whole type, and a guard can
fail, so neither proves coverage. An `if let` and a `while let` supply their own catch-all (PAT-6).

```con reject name=missing-variant code=E0530 stage=core-check pair=every-variant
enum Copy E { A, B, C }

fn f(e: E) -> Int {
    match e {
        E::A => { return 1; },
        E::B => { return 2; }
    }
}

fn main() -> Int {
    return f(E::A);
}
```

```con reject name=no-default-arm code=E0534 stage=core-check pair=every-variant
fn main() -> Int {
    let k: Int = 3;
    match k {
        1 => { return 1; },
        2..=9 => { return 42; }
    }
}
```

```con accept name=every-variant exit=42
enum Copy E { A, B, C }

fn f(e: E) -> Int {
    match e {
        E::A => { return 40; },
        E::B => { return 1; },
        E::C => { return 0; }
    }
}

fn main() -> Int {
    let k: Int = 3;
    let mut out: Int = f(E::A);
    match k {
        1 => { out = out + 0; },
        2..=9 => { out = out + 2; },
        _ => {}
    }
    return out;
}
```

<a id="PAT-3"></a>
### PAT-3. Every arm fits its scrutinee

**PAT-3@1.** A variant pattern names a variant of the scrutinee's enum and binds or ignores every one of its fields, a literal or range pattern appears only over a scrutinee of its type, and no unguarded arm repeats a variant an earlier unguarded arm covers.

- Kind: refusal
- Uses: none
- Codes: E0304 (check), E0531 (core-check), E0532 (core-check), E0533 (core-check)
- Status: partial R-0499
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never inspects a value its pattern does not describe (R-0507)
- Evidence: tested

**Rationale.** A pattern describes the shape the scrutinee has. Every field is listed, so a field
holding a linear value cannot be skipped silently (`field: _` ignores a Copy one, OWN-5), and an
arm that can never be reached is a mistake. Two gaps fail closed at a later stage instead of at
the checker: a variant of another enum makes the compiler panic (`Option.get!`) before elaboration
reports `unknown variant` (bug 196), and a range arm over an enum reaches the SSA verifier
natively while `--interp` runs it (bug 195).

```con reject name=duplicate-arm code=E0532 stage=core-check pair=arms-fit
enum Copy E { A, B }

fn main() -> Int {
    let e: E = E::A;
    match e {
        E::A => { return 1; },
        E::A => { return 2; },
        E::B => { return 0; }
    }
}
```

```con reject name=missing-payload-field code=E0533 stage=core-check pair=arms-fit
enum Copy E { A { x: Int, y: Int }, B }

fn main() -> Int {
    let e: E = E::A { x: 1, y: 2 };
    match e {
        E::A { x } => { return x; },
        E::B => { return 0; }
    }
}
```

```con reject name=other-enum-variant code=new stage=check pair=arms-fit pending=R-0499 now=fail:Option.get!
enum Copy E { A, B }
enum Copy F { X, Y }

fn main() -> Int {
    let e: E = E::A;
    match e {
        F::X => { return 1; },
        _ => { return 0; }
    }
}
```

```con reject name=range-over-enum code=new stage=check pair=arms-fit pending=R-0499 now=reject:E0715:ssa-verify now-interp=accept:42
enum Copy E { A, B }

fn main() -> Int {
    let e: E = E::B;
    match e {
        0..=9 => { return 1; },
        _ => { return 42; }
    }
}
```

```con accept name=arms-fit exit=42
enum Copy E { A { x: Int, y: Int }, B }

fn main() -> Int {
    let e: E = E::A { x: 40, y: 2 };
    match e {
        E::A { x, y } => { return x + y; },
        E::B => { return 0; }
    }
}
```

<a id="PAT-4"></a>
### PAT-4. A guard is read-only

**PAT-4@2.** A match guard is a `bool` condition evaluated after its arm's pattern matched and before the arm commits, and it moves no non-Copy value, neither an outer binding nor one of its arm's binders.

- Kind: refusal
- Uses: OWN-D1
- Codes: E0300 (check)
- Status: enforced
- Enforced by: Concrete.checkGuard
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** A false guard falls through to the next arm, which may move the same value again
(bug 090). A guard reads and borrows; it moves nothing. Its type is TYPE-5's (bug 192).

```con reject name=guard-moves code=E0300 stage=check pair=guard-reads
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let k: Int = 1;
    match k {
        1 if take(r) > 0 => { return 1; },
        _ => { return take(r); }
    }
}
```

```con accept name=guard-reads exit=42
struct R { v: Int }

fn take(r: R) -> Int {
    let R { v } = r;
    return v;
}

fn peek(r: &R) -> Int {
    return r.v;
}

fn main() -> Int {
    let r: R = R { v: 42 };
    let k: Int = 1;
    match k {
        1 if peek(&r) > 100 => { return take(r) * 0; },
        _ => { return take(r); }
    }
}
```

<a id="PAT-5"></a>
### PAT-5. A destructuring `let` binds every field

**PAT-5@1.** A destructuring `let` consumes its scrutinee and binds every field of its pattern, a pattern that may fail to match takes an `else` block, and that block diverges on every path (`return`, `break`, `continue` or a call that never returns).

- Kind: refusal
- Uses: none
- Codes: E0252 (check)
- Status: partial R-0499
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** A struct always matches its own pattern; a multi-variant enum may not, so its
pattern needs an `else`. Falling out of the `else` would continue with the bindings unbound. That
divergence requirement has no enforcing site today: an `else` block that falls through reaches
the backend, which fails in `llvm-as`, while `--interp` runs it (bug 194). Over a non-Copy enum the
`else` arm would discard the scrutinee, so `let ... else` there is refused (OWN-5); write a
`match`.

```con reject name=struct-field-missing code=E0252 stage=check pair=destructure-all
struct P { a: Int, b: Int }

fn main() -> Int {
    let p: P = P { a: 1, b: 2 };
    let P { a } = p;
    return a;
}
```

```con reject name=else-falls-through code=new stage=check pair=destructure-all pending=R-0499 now=fail:doesn't_match_function_result_type now-interp=accept:42
fn main() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    let Option::Some { value } = o else {
        let z: Int = 0;
    };
    return value;
}
```

```con accept name=destructure-all exit=42
struct P { a: Int, b: Int }

fn get(o: Option<Int>) -> Int {
    let Option::Some { value } = o else {
        return 0;
    };
    return value;
}

fn main() -> Int {
    let p: P = P { a: 1, b: 2 };
    let P { a, b } = p;
    return get(Option::Some { value: 39 }) + a + b;
}
```

<a id="PAT-6"></a>
### PAT-6. `if let` and `while let` are matches with a catch-all

**PAT-6@1.** `if let P = e { A } else { B }` means `match e { P => { A }, _ => { B } }` (an empty `B` when there is no `else`), and `while let P = e { A }` re-evaluates `e` before each iteration and ends the loop at the first value that does not match `P`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** Both forms are desugared before checking, so exhaustiveness, binding, linearity and
lowering are exactly those of the `match`. In particular the catch-all is a `_`, so over a non-Copy
enum both forms are refused (OWN-5).

```con accept name=if-let-and-while-let exit=42
fn main() -> Int {
    let mut total: Int = 0;
    let mut k: Int = 3;
    let mut cur: Option<Int> = Option::Some { value: k };
    while let Option::Some { value: step } = cur {
        total = total + step;
        k = k - 1;
        if k > 0 {
            cur = Option::Some { value: k };
        } else {
            cur = Option::None;
        }
    }
    let o: Option<Int> = Option::Some { value: 36 };
    if let Option::Some { value } = o {
        total = total + value;
    } else {
        total = 0;
    }
    return total;
}
```

<a id="PAT-7"></a>
### PAT-7. Literal, range and alternative patterns

**PAT-7@1.** An integer range pattern `lo..=hi` matches `lo <= x <= hi` and `lo..hi` matches `lo <= x < hi`, compared in the scrutinee's type and signedness, and an arm `P1 | P2 if g => body` is the arms `P1 if g => body` and `P2 if g => body` in that order.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.parseStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: range and guard patterns are modelled in the proof path (they are reported unsupported today)
- Evidence: tested

**Rationale.** Range bounds are integer literals, negative ones included, elaborated at the
scrutinee's type, so a `u8` scrutinee compares unsigned and `200..=255` works as written. The
alternatives of an OR pattern are desugared into one arm each, so every alternative must bind the
names the body uses. A binding or `_` pattern on a field ignores or names that field; a `_` field
is not in scope and its value is not loaded.

```con accept name=ranges-and-alternatives exit=42
fn classify(b: u8) -> Int {
    let mut out: Int = 0;
    match b {
        0..200 => { out = 1; },
        200..=254 | 255 => { out = 40; },
        _ => { out = 3; }
    }
    return out;
}

fn main() -> Int {
    let k: Int = 7;
    let mut extra: Int = 0;
    match k {
        1 | 7 if k > 5 => { extra = 2; },
        _ => {}
    }
    return classify(200) + extra;
}
```


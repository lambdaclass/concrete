# Patterns

A pattern destructures one level: a variant with its fields, a struct with its fields, a literal,
an integer range, a binding or `_`. Patterns appear in `match` arms, `if let`, `while let`, and
destructuring `let` (with an optional `else`). How a pattern binds through a reference scrutinee
is REF-9 (pending), and which values `_` may ignore is OWN-5.

Two things are deliberately not in the language. **Nested patterns** (`E::Wrap { P { x, y } }`)
are deferred until a workload needs them; one level of destructuring plus a field access or an
inner `match` expresses the same programs ([No nested patterns](#no-nested-patterns),
`check_nested_patterns.sh`). **Tuples** do not exist; the named struct is the one product type
([types.md, "No anonymous tuples"](types.md#no-anonymous-tuples), `check_no_tuples.sh`). The design records are
[PATTERN_ERGONOMICS.md](../archive/design-records/PATTERN_ERGONOMICS.md) and
[PATTERN_DESTRUCTURING.md](../archive/design-records/PATTERN_DESTRUCTURING.md) (absorbed into PAT-10
and PAT-11 on 2026-10-30).

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

fn example_value() -> Int {
    return nested(Option::Some { value: Option::Some { value: 42 } });
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

**Conformance fixtures.**

| fixture | expected | pair | modes |
|---|---|---|---|
| `tests/programs/regress_2i_renamed_pattern_binder.con` | `run:42` | `-` | `binary,interp,fmt` |

<a id="PAT-2"></a>
### PAT-2. A match is exhaustive

**PAT-2@2.** A `match` covers every value of its scrutinee: over an enum, every variant has an unguarded arm or the match has an unguarded catch-all (`_` or a binding), over `bool` both `true` and `false` have unguarded arms or the match has an unguarded catch-all, and over any other type the match has an unguarded catch-all, a guarded arm never counting as coverage.

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

fn example_value() -> Int {
    return f(E::A);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=no-default-arm code=E0534 stage=core-check pair=every-variant
fn main() -> u8 {
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

fn example_value() -> Int {
    let k: Int = 3;
    let mut out: Int = f(E::A);
    match k {
        1 => { out = out + 0; },
        2..=9 => { out = out + 2; },
        _ => {}
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-3"></a>
### PAT-3. Every arm fits its scrutinee

**PAT-3@3.** A variant pattern names a variant of the scrutinee's enum and lists every one of its fields, ignoring one only explicitly (`field: _`, or `Variant { .. }` for all), a bare variant name being legal only for a fieldless variant, a literal or range pattern appears only over a scrutinee of its type, and no unguarded arm repeats a variant an earlier unguarded arm covers.

- Kind: refusal
- Uses: none
- Codes: E0304 (check), E0309 (check), E0531 (core-check), E0532 (core-check), E0533 (core-check)
- Status: partial bug-321
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never inspects a value its pattern does not describe (R-0507)
- Evidence: tested

**Rationale.** A pattern describes the shape the scrutinee has. Every field is listed, so a field
holding a linear value cannot be skipped silently (`field: _` ignores a Copy one, OWN-5), and an
arm that can never be reached is a mistake. Before bug 196 a pattern that did not fit the scrutinee
was not checked: a variant of another enum made the compiler panic, one sharing a name with the
scrutinee's own was silently taken as it, and a range arm over an enum reached the SSA verifier.
Check now refuses each with E0309. A bare variant name over a variant with a non-Copy field
silently dropped the payload until bug 197; that refusal is OWN-5's (E0288).

The owner decided on 2026-10-29 (OD25) that ignoring a payload is always written,
`Variant { .. }` or `field: _`, and a bare variant name matches only a fieldless variant. The current compiler still
lets a bare name ignore a Copy payload (bug 321), pinned by the pending example below; the
`Variant { .. }` form is not parsed yet.


```con reject name=duplicate-arm code=E0532 stage=core-check pair=arms-fit
enum Copy E { A, B }

fn main() -> u8 {
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

fn example_value() -> Int {
    let e: E = E::A { x: 1, y: 2 };
    match e {
        E::A { x } => { return x; },
        E::B => { return 0; }
    }
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=other-enum-variant code=E0309 stage=check pair=arms-fit
enum Copy E { A, B }
enum Copy F { X, Y }

fn main() -> u8 {
    let e: E = E::A;
    match e {
        F::X => { return 1; },
        _ => { return 0; }
    }
}
```

```con reject name=range-over-enum code=E0309 stage=check pair=arms-fit
enum Copy E { A, B }

fn main() -> u8 {
    let e: E = E::B;
    match e {
        0..=9 => { return 1; },
        _ => { return 42; }
    }
}
```

```con reject name=same-name-other-enum code=E0309 stage=check pair=arms-fit
enum Copy E { A, B }
enum Copy F { A, X }

fn main() -> u8 {
    let e: E = E::B;
    match e {
        F::A => { return 1; },
        _ => { return 42; }
    }
}
```

```con accept name=arms-fit exit=42
enum Copy E { A { x: Int, y: Int }, B }

fn example_value() -> Int {
    let e: E = E::A { x: 40, y: 2 };
    match e {
        E::A { x, y } => { return x + y; },
        E::B => { return 0; }
    }
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=bare-variant-with-payload code=new stage=check pair=variant-field-ignored pending=bug-321 now=accept:42
enum Copy E { A { v: Int }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A => { return 42; },
        E::B => { return 0; },
    }
}
```

```con accept name=variant-field-ignored exit=42
enum Copy E { A { v: Int }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A { v: _ } => { return 42; },
        E::B => { return 0; },
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

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let k: Int = 1;
    match k {
        1 if take(r) > 0 => { return 1; },
        _ => { return take(r); }
    }
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

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

fn example_value() -> Int {
    let r: R = R { v: 42 };
    let k: Int = 1;
    match k {
        1 if peek(&r) > 100 => { return take(r) * 0; },
        _ => { return take(r); }
    }
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-5"></a>
### PAT-5. A destructuring `let` binds every field

**PAT-5@3.** A destructuring `let` consumes its scrutinee and names every field of its pattern, ignoring one only explicitly with `field: _`, a pattern that may fail to match takes an `else` block, and that block diverges on every path (`return`, `break`, `continue` or a call that never returns).

- Kind: refusal
- Uses: none
- Codes: E0252 (check), E0308 (check)
- Status: partial bug-321
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** A struct always matches its own pattern; a multi-variant enum may not, so its
pattern needs an `else`. Falling out of the `else` would continue with the bindings unbound. That
divergence requirement had no enforcing site until bug 195: an `else` block that fell through
skipped the rest of the function, or reached the backend, which failed in `llvm-as` while
`--interp` ran it. Check now refuses it with E0308. A function whose end is reachable without a
`return` is STMT-9. Over a non-Copy enum the
`else` arm would discard the scrutinee, so `let ... else` there is refused (OWN-5); write a
`match`.

Three entries cover the destructuring `let`. This one states what it must write: every field, an `else` where the pattern may fail, and an `else` that diverges. PAT-10 states what an enum destructuring `let` means, and PAT-8 where one may appear. In the current compiler, an enum `let` with no `else` over a pattern that may fail (`let E::A { v } = e;` with a variant `B` left out) is reported as E0530 at core-check under PAT-2, after the desugaring PAT-10 states, while a missing field and an `else` that falls through are reported under this entry as E0252 and E0308 (the examples below). They stay separate entries because their kinds differ (PAT-10 is a guarantee, PAT-8 and this entry are refusals) and PAT-8 retires with the C-style `for` (OD47).

The owner decided on 2026-10-29 (OD25) that a struct `let` names every field, a
Copy struct included. The current compiler still lets a pattern over a Copy struct omit fields (bug 321).

A field binding written `_` in a variant or destructuring pattern holds its field's position, so
the other binders still bind the right fields, but it adds no name (reading `_` is E0100) and its
field is not loaded; over an owned scrutinee it may ignore only a Copy field (OWN-5), while through a
borrowed scrutinee it ignores a borrowed payload, which owns nothing. The pattern forms of ROADMAP old
Phase 6 #5 (ranges, `if let` and `while let`, guards, alternatives, matching through a reference,
functional update, `_` binders) are gated by `scripts/tests/check_pattern_ergonomics.sh`; their
implementation history is the archived
[PATTERN_ERGONOMICS.md](../archive/design-records/PATTERN_ERGONOMICS.md).

```con reject name=struct-field-missing code=E0252 stage=check pair=destructure-all
struct P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 1, b: 2 };
    let P { a } = p;
    return a;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=else-falls-through code=E0308 stage=check pair=destructure-all
fn example_value() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    let Option::Some { value } = o else {
        let z: Int = 0;
    };
    return value;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
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

fn example_value() -> Int {
    let p: P = P { a: 1, b: 2 };
    let P { a, b } = p;
    return get(Option::Some { value: 39 }) + a + b;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=struct-let-omits-field code=E0252 stage=check pair=struct-let-ignores-field pending=bug-321 now=accept:42
struct Copy P { a: Int, b: Int }
fn example_value() -> Int {
    let p: P = P { a: 42, b: 1 };
    let P { a } = p;
    return a;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=struct-let-ignores-field exit=42
struct Copy P { a: Int, b: Int }
fn example_value() -> Int {
    let p: P = P { a: 42, b: 1 };
    let P { a, b: _ } = p;
    return a;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PAT-6"></a>
### PAT-6. `if let` and `while let` are matches with a catch-all

**PAT-6@2.** For an enum variant pattern `P`, `if let P = e { A } else { B }` means `match e { P => { A }, _ => { B } }` (an empty `B` when there is no `else`), and `while let P = e { A }` re-evaluates `e` before each iteration and ends the loop at the first value that does not match `P`.

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
fn example_value() -> Int {
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
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
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

fn example_value() -> Int {
    let k: Int = 7;
    let mut extra: Int = 0;
    match k {
        1 | 7 if k > 5 => { extra = 2; },
        _ => {}
    }
    return classify(200) + extra;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-8"></a>
### PAT-8. An enum destructuring `let` is a block statement

**PAT-8@1.** A `let` that destructures an enum variant appears as a statement of a block, not as the initializer of a `for` header.

- Kind: refusal
- Uses: none
- Codes: E0318 (check)
- Status: enforced
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** A `let` over an enum variant is lowered to a `match` over the rest of its block
(PAT-10); a `for` header has no block of its own to become that match, so the form is refused rather than skipped. PAT-5 states what a destructuring `let` must write wherever it appears. A struct destructuring needs no match (it becomes field reads) and is
accepted there. Destructure the enum before the loop. OD47 removes the C-style `for` and its header (bug 331), and with it the form this entry
refuses; the entry retires when that lands.

```con reject name=destructure-in-for-header code=E0318 stage=check pair=destructure-before-loop
enum Copy E { A { v: Int } }

fn example_value() -> Int {
    let e: E = E::A { v: 0 };
    let mut out: Int = 0;
    for (let E::A { v } = e; v < 1; v = v + 1) {
        out = 42;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=destructure-before-loop exit=42
enum Copy E { A { v: Int } }

fn example_value() -> Int {
    let e: E = E::A { v: 0 };
    let E::A { v } = e;
    let mut out: Int = 0;
    for (let mut i: Int = v; i < 1; i = i + 1) {
        out = 42;
    }
    return out;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-9"></a>
### PAT-9. The first matching arm wins

**PAT-9@1.** A `match` selects the first arm in source order whose pattern matches and whose guard holds, and an arm that no value can reach because earlier unguarded arms cover every value it matches is refused.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0500
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD18). Whether the refusal applies to the fallback
arm the parser adds for `if let`, `let ... else` and `while let` is OPEN-40, and whether a catch-all
after ranges that span the whole type is unreachable is OPEN-41. Source order is the order a reader sees,
so the arm that runs is the first one written that matches. This differs from the current compiler: CoreCanonicalize
moves unguarded catch-all arms after the others (bug 213's provisional repair), so a specific arm
written after `_` is reached and the program below returns 42 on both consumers; under this rule the
catch-all wins and the later arm is unreachable, a refusal. Repeating a variant an earlier
unguarded arm covers is already PAT-3's refusal. Two readings are open for the owner: whether the fallback arm of a desugared
`if let`, `let ... else` or `while let` is judged (OPEN-40), and whether a catch-all after integer
ranges that span the scrutinee's type is unreachable (OPEN-41).

```con reject name=arm-after-catch-all code=new stage=check pair=catch-all-last pending=R-0500 now=accept:42
fn main() -> u8 {
    let x: Int = 1;
    match x {
        _ => {
            return 1;
        },
        1 => {
            return 42;
        },
    }
}
```

```con accept name=catch-all-last exit=42
fn main() -> u8 {
    let x: Int = 1;
    match x {
        1 => {
            return 42;
        },
        _ => {
            return 1;
        },
    }
}
```

<a id="PAT-10"></a>
### PAT-10. An enum destructuring `let` is a `match` over the rest of its block

**PAT-10@1.** For an enum variant pattern `P` and the statements `R` that follow in the same block, `let P = e else { B };` means `match e { P => { R }, _ => { B } }` and `let P = e;` means `match e { P => { R } }`, so the binders of `P` are in scope in `R` alone and `B` sees none of them.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.desugarStmts
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** Both forms are desugared before checking, so exhaustiveness (PAT-2), binding,
linearity and lowering are the `match`'s, as for `if let` (PAT-6). Without `else` the `match` has one
arm, so a pattern that may fail is refused as non-exhaustive, and with it `B` must diverge (PAT-5).
The `else` block gets no binding for the value that failed to match; a program that needs it writes
a `match`; a binder of `P` used in `B` is an undeclared name (MOD-1's
`let-else-binder-out-of-scope`). A struct destructuring `let` is not a `match`: it always matches and reads each field
(PAT-5, SYN-3). A plain `let x = e else { B };` is a parse error. Whether `let ... else` stays is
OPEN-52.

```con accept name=let-else-and-irrefutable exit=42
enum Copy One { A { v: Int } }

fn get(o: Option<Int>) -> Int {
    let Option::Some { value } = o else {
        return 1;
    };
    return value;
}

fn example_value() -> Int {
    let one: One = One::A { v: 1 };
    let One::A { v } = one;
    return get(Option::Some { value: 39 }) + get(Option::None) + v + 1;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PAT-11"></a>
### PAT-11. `let mut` makes every binder of a pattern mutable

**PAT-11@1.** A destructuring `let mut P = e`, with or without `else`, declares every binder of `P` mutable.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore, Concrete.desugarStmts
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One `mut` covers the pattern, as one `let` does; a binder cannot be marked on its own.
What a mutable binding allows is OWN-10. The parser does not keep the `mut`: the checker declares
every binder of a destructuring `let` mutable (a struct pattern in `checkStmt`, a variant pattern in
the `match` arm `desugarStmts` makes of it), so in the current compiler the binders of a `let` without `mut` are
assignable too. That is bug 336, which violates OWN-10@2; the examples here show that `let mut`
binders are assignable, not that the others are not.

```con accept name=let-mut-pattern exit=42
struct Copy P { a: Int, b: Int }

fn example_value() -> Int {
    let p: P = P { a: 40, b: 1 };
    let mut P { a, b } = p;
    a = a + 1;
    b = b + 0;
    let o: Option<Int> = Option::Some { value: 0 };
    let mut Option::Some { value } = o else {
        return 0;
    };
    value = value + 1;
    return a + b + value - 1;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=let-mut-variant-without-else exit=42
enum Copy One { A { v: Int } }

fn example_value() -> Int {
    let o: One = One::A { v: 41 };
    let mut One::A { v } = o;
    v = v + 1;
    return v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

## Absent by decision

This section is explanatory: it records pattern forms the language does not have, why, and what
would reopen them. The grammar (SYN-1) is what refuses them.

### No nested patterns

Deferred, not in V1 (decided 2026-06-22, ROADMAP old Phase 6 #5, the last open pattern item),
pinned by `scripts/tests/check_nested_patterns.sh`. A pattern destructures exactly one level: an
arm binds each field of a variant or struct to one name, and the value at that field is bound
whole. `E::Wrap { P { x, y } }` and `Some(Some(n))` are parse errors. A deeper value is reached in a
second step, by a field access or an inner `match`:

```con pseudocode
match e { E::Wrap { p } => use(p.x, p.y) }

match outer {
    Option::Some { value } => match value {
        Option::Some { value: inner } => use(inner),
        Option::None => fallback(),
    },
    Option::None => fallback(),
}
```

This is more verbose and exactly as expressive: no program written with nested patterns lacks a
one-level equivalent. The feature is deferred because it is a large cross-cutting change for sugar
(the flat binder list would become a recursive pattern type, threaded through parsing, resolution,
checking, elaboration, lowering with recursive field extraction, and the proof path, the heaviest
change of the pattern-ergonomics work), and because it is workload-driven, not symmetry-driven: no
example or workload is blocked by the one-level limit, since decoders, parsers and interpreters are
served by ranges, guards, alternatives, `if let` and `while let`, matching on `&T`, functional
update and one-level destructuring, all of which landed. The same call was made for const generics
and tuples ([types.md](types.md#no-anonymous-tuples)). Nested patterns are built when a real
workload shows the one-level form untenable (for example deep protocol syntax trees where the
inner matches dominate the code), as a recursive pattern type with recursive destructuring in
check, elaboration and lowering.

### No bare variant names and no parameter patterns

A variant is always written with its enum, `Option::Some { value }`, never `Some(x)`: a bare name
would make the parser depend on name resolution to know which enum it belongs to. A function
parameter is a name with a type, never a pattern; the body destructures it.

## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- PAT-3@3 records OD25 (2026-10-29): ignoring a payload is always written.
- PAT-5@3 records OD25 (2026-10-29): a struct `let` names every field.

# Patterns

Patterns destructure one level in explicit `match` arms and irrefutable destructuring `let`:
variants, structs, literals, bindings, `_` and alternatives. Guards, ranges, `if let`, `while let`
and `let ... else` are refused (OD52, OD55, OD56). Borrowed scrutinees follow REF-9 and ignored
fields follow OWN-5, including its Result/Option restrictions.

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

**PAT-1@2.** A struct or enum pattern in a `match` or an irrefutable destructuring `let` may bind a field under another name with `field: binder`, and the formatter keeps the label.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.parseBindingList
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Renaming avoids shadowing under SCOPE-1; OD52 removes conditional and refutable destructuring sugar without changing field labels.

```con accept name=nested-option exit=42
fn nested(o: Option<Option<i64>>) -> i64 {
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

fn example_value() -> i64 {
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

**PAT-2@3.** A `match` covers every value of its scrutinee: an enum covers every variant or has a catch-all, `bool` covers both literals or has a catch-all, and every other type requires a catch-all even when every integer literal is listed, with guards and range patterns refused.

- Kind: refusal
- Uses: none
- Codes: E0530 (core-check), E0534 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never falls off its last arm (R-0507)
- Evidence: tested

**Rationale.** OD55 and OD56 remove guards and ranges; exhaustive enum and boolean coverage remains structural, and an integer match always needs a catch-all.

```con reject name=missing-variant-decided code=E0530 stage=core-check pair=every-variant pending=R-0555 now=reject:E0530:core-check
enum Copy E { A, B, C }

fn f(e: E) -> i64 {
    match e {
        E::A => { return 1; },
        E::B => { return 2; }
    }
}

fn example_value() -> i64 {
    return f(E::A);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=no-default-arm-decided code=E0534 stage=core-check pair=every-variant pending=R-0555 now=reject:E0534:core-check
fn main() -> u8 { let k: u8 = 3; match k { 1 => { return 1; }, 3 => { return 42; } } }
```

```con accept name=every-variant exit=42
enum Copy E { A, B, C }
fn f(e: E) -> u8 {
    match e { E::A => { return 42; }, E::B => { return 1; }, E::C => { return 0; } }
}
fn main() -> u8 { return f(E::A); }
```

<a id="PAT-3"></a>
### PAT-3. Every arm fits its scrutinee

**PAT-3@4.** A variant pattern names a variant of the scrutinee’s enum and lists every field, ignoring one only explicitly with an eligible wildcard or `Variant { .. }`, a bare variant name is legal only for a fieldless variant, a literal pattern has the scrutinee’s type and no arm repeats a covered variant, while guards and ranges are refused.

- Kind: refusal
- Uses: none
- Codes: E0304 (check), E0309 (check), E0531 (core-check), E0532 (core-check), E0533 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore, Concrete.ccCheckExpr
- Semantics: none
- Obligation: a checked match never inspects a value its pattern does not describe (R-0507)
- Evidence: tested

**Rationale.** OD25 requires explicit ignored fields; OD55 and OD56 remove guards and ranges. Only eligible values may be ignored (OWN-5).

```con reject name=duplicate-arm-decided code=E0532 stage=core-check pair=arms-fit pending=R-0555 now=reject:E0532:core-check
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

```con reject name=missing-payload-field-decided code=E0533 stage=core-check pair=arms-fit pending=R-0555 now=reject:E0533:core-check
enum Copy E { A { x: i64, y: i64 }, B }

fn example_value() -> i64 {
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

```con reject name=other-enum-variant-decided code=E0309 stage=check pair=arms-fit pending=R-0555 now=reject:E0309:check
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



```con reject name=same-name-other-enum-decided code=E0309 stage=check pair=arms-fit pending=R-0555 now=reject:E0309:check
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
enum Copy E { A { x: i64, y: i64 }, B }

fn example_value() -> i64 {
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
enum Copy E { A { v: i64 }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A => { return 42; },
        E::B => { return 0; },
    }
}
```

```con accept name=variant-field-ignored exit=42
enum Copy E { A { v: i64 }, B }
fn main() -> u8 {
    let e: E = E::A { v: 1 };
    match e {
        E::A { v: _ } => { return 42; },
        E::B => { return 0; },
    }
}
```

<a id="PAT-4"></a>
### PAT-4. Match guards are refused

**PAT-4@3.** Match guards are refused; a conditional test is written inside the matching arm or in an `if` chain.

- Kind: refusal
- Uses: OWN-D1
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.checkGuard
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** OD55 removes the guard form entirely, so there is no guard commitment or move rule.



```con accept name=guard-reads exit=42
fn peek(r: &i64) -> bool { return *r > 100; }
fn main() -> u8 {
 let k: i64 = 1;
 match k { 1 => { if peek(&k) { return 1; } return 42; }, _ => { return 0; } }
}
```

```con reject name=guard-removed code=new stage=parse pair=test-inside-arm pending=R-0555 now=accept:42
fn main() -> u8 { let n: u8 = 1; match n { 1 if true => { return 42; }, _ => { return 0; } } }
```

```con accept name=test-inside-arm exit=42
fn main() -> u8 { let n: u8 = 1; match n { 1 => { if true { return 42; } return 0; }, _ => { return 0; } } }
```

<a id="PAT-5"></a>
### PAT-5. A destructuring `let` binds every field

**PAT-5@4.** A destructuring `let` consumes its scrutinee, names every field with ignored fields explicitly marked by eligible wildcards, and has an irrefutable pattern, while refutable destructuring and `let ... else` are refused and use `match` instead.

- Kind: refusal
- Uses: none
- Codes: new (parse), E0252 (check), E0308 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore
- Semantics: none
- Obligation: linear conservation (R-0507)
- Evidence: tested

**Rationale.** OD52 and OD85 require an explicit match for refutable destructuring; OD25 requires all fields, with eligible ignored fields marked explicitly.

```con reject name=struct-field-missing-decided code=E0252 stage=check pair=destructure-all pending=R-0555 now=reject:E0252:check
struct P { a: i64, b: i64 }

fn example_value() -> i64 {
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



```con accept name=destructure-all exit=42
struct P { a: i64, b: i64 }
fn main() -> u8 { let p: P = P { a: 40, b: 2 }; let P { a, b } = p; if a + b == 42 { return 42; } return 1; }
```

```con reject name=struct-let-omits-field code=E0252 stage=check pair=struct-let-ignores-field pending=bug-321 now=accept:42
struct Copy P { a: i64, b: i64 }
fn example_value() -> i64 {
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
struct Copy P { a: i64, b: i64 }
fn example_value() -> i64 {
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

```con reject name=refutable-let-else-removed code=new stage=parse pair=destructure-all pending=R-0555 now=accept:42
fn main() -> u8 { let o: Option<u8> = Option::Some { value: 42 }; let Option::Some { value } = o else { return 0; }; return value; }
```

<a id="PAT-6"></a>
### PAT-6. Conditional destructuring sugar is refused

**PAT-6@3.** `if let` and `while let` are refused; conditional destructuring uses an explicit `match` and repetition uses `while`.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.parseStmt
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** OD52 removes both forms; explicit matches expose every branch and its ownership obligations.


```con reject name=if-let-removed code=new stage=parse pair=explicit-match pending=R-0555 now=accept:42
fn main() -> u8 { let o: Option<u8> = Option::Some { value: 42 }; if let Option::Some { value } = o { return value; } return 0; }
```

```con accept name=explicit-match exit=42
fn main() -> u8 { let o: Option<u8> = Option::Some { value: 42 }; match o { Option::Some { value } => { return value; }, Option::None => { return 0; } } }
```

<a id="PAT-7"></a>
### PAT-7. Alternatives remain; ranges and guards are refused

**PAT-7@2.** Alternative patterns `P1 | P2 => body` mean consecutive arms `P1 => body` and `P2 => body` in that order, while integer range patterns and match guards are refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.parseStmt, Concrete.ccCheckExpr
- Semantics: none
- Obligation: range and guard patterns are modelled in the proof path (they are reported unsupported today)
- Evidence: tested

**Rationale.** OD55 and OD56 remove guards and integer intervals; alternatives remain separate consecutive arms and must bind the names their body uses.

```con accept name=ranges-and-alternatives exit=42
fn main() -> u8 { let k: u8 = 7; match k { 1 | 7 => { return 42; }, _ => { return 1; } } }
```

<a id="PAT-8"></a>
### PAT-8. C-style loop headers are refused

**PAT-8@2.** C-style `for` headers, including destructuring initializers, are refused; a counted loop binds its immutable index only in its body.

- Kind: refusal
- Uses: none
- Codes: new (parse)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD47 and OD61 remove the entire C-style header, rather than retaining a special rule about its initializer.



```con accept name=destructure-before-loop exit=42
enum Copy E { A { v: i64 } }
fn main() -> u8 { let e: E = E::A { v: 42 }; let E::A { v } = e; if v == 42 { return 42; } return 1; }
```

```con reject name=c-header-removed code=new stage=parse pair=while-replacement pending=R-0555 now=accept:42
fn main() -> u8 { for (let mut i: u8 = 0; i < 1; i = i + 1) { if i == 0 { return 42; } } return 0; }
```

```con accept name=while-replacement exit=42
fn main() -> u8 { let mut i: u8 = 0; while i < 1 { if i == 0 { return 42; } i = i + 1; } return 0; }
```

<a id="PAT-9"></a>
### PAT-9. The first matching arm wins

**PAT-9@2.** A `match` selects the first matching arm in source order and refuses an arm covered by earlier arms, while every integer match requires a catch-all that listing integer literals never makes unreachable.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0555
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** OD18 fixes source order; OD40 and OD41 no longer need desugaring or interval exceptions because OD52 and OD56 removed those forms.

```con reject name=arm-after-catch-all code=new stage=check pair=catch-all-last pending=R-0500 now=accept:42
fn main() -> u8 {
    let x: i64 = 1;
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
    let x: i64 = 1;
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
### PAT-10. Irrefutable enum destructuring matches the rest of its block

**PAT-10@2.** An irrefutable enum destructuring `let P = e;` means a one-arm `match` over the rest of its block, with binders visible only there, while a refutable pattern or `let ... else` is refused.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.desugarStmts
- Semantics: none
- Obligation: the desugaring is part of the elaboration relation (R-0496)
- Evidence: tested

**Rationale.** OD52 removes the fallback form; an irrefutable one-arm match preserves binder scope and explicit consumption.

```con accept name=let-else-and-irrefutable exit=42
enum Copy One { A { v: u8 } }
fn main() -> u8 { let one: One = One::A { v: 42 }; let One::A { v } = one; return v; }
```

<a id="PAT-11"></a>
### PAT-11. `let mut` makes every binder of a pattern mutable

**PAT-11@2.** An irrefutable destructuring `let mut P = e` declares every binder of `P` mutable, and no destructuring `let` has an `else` block.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.checkExprCore, Concrete.desugarStmts
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** One `mut` applies to each binder; OD52 removes the refutable else form, and immutable binders remain protected by OWN-10.

```con accept name=let-mut-pattern exit=42
struct Copy P { a: u8, b: u8 }
fn main() -> u8 { let p: P = P { a: 40, b: 1 }; let mut P { a, b } = p; a = a + 1; b = b + 0; return a + b; }
```

```con accept name=let-mut-variant-without-else exit=42
enum Copy One { A { v: i64 } }

fn example_value() -> i64 {
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
served by explicit matches, comparisons, alternatives, matching on `&T` and one-level destructuring. The same call was made for const generics
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

- PAT-1@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-1@1 is superseded by version.
- PAT-2@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-2@2 is superseded by version.
- PAT-3@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-3@3 is superseded by version.
- PAT-4@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-4@2 is superseded by version.
- PAT-5@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-5@3 is superseded by version.
- PAT-6@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-6@2 is superseded by version.
- PAT-7@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-7@1 is superseded by version.
- PAT-8@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-8@1 is superseded by version.
- PAT-9@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-9@1 is superseded by version.
- PAT-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-10@1 is superseded by version.
- PAT-11@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; PAT-11@1 is superseded by version.

# Pipeline Preservation

The compiler is a chain of stages (PIPELINE_CONTRACTS.md). Each stage's preservation entry
states the relation between the stage's input and its output, and the evidence for it. Under
VERIFIED_PIPELINE the target form of each statement is
`WF Γ p → pass p = .ok q → WF Γ q ∧ Refines Γ p q`. Until that is proved, the evidence is
`tested`: the examples are accepted programs, and every consumer must give each one the stated
result. A stage-boundary entry also has reject examples: a program that breaks an earlier
stage's contract must be refused by the first responsible stage (`stage=` pins its pass tag), and
no lower layer (`llvm-as`, the linker, a panic) may report it instead. The reject example's code
belongs to the chapter of the rule it enforces. This chapter replaces the boundary contracts of
[PIPELINE_CONTRACTS.md](../compiler/PIPELINE_CONTRACTS.md), which is now their design record, and
`check_spec.sh` runs their fixtures, which used to belong to `check_pipeline_contracts.sh`.

The stages are, in order: Parse, Resolve, Check, Elab (into Core), CoreCheck, Mono, Lower (to SSA,
then SSAVerify), Emit (LLVM IR) and link. PASS-5 to PASS-11 state each boundary. PASS-1 and
PASS-2 are about specialization, and PASS-3 and PASS-4 are about the end-to-end profiles.

## Rules

<a id="PASS-1"></a>
### PASS-1. Specialization preserves meaning

**PASS-1@1.** Monomorphization refines its input: a call of a generic function at a type argument behaves as the generic body behaves at that type, on every consumer.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.monoProgram
- Semantics: none
- Obligation: the substitution and dispatch theorem of S8a (R-0510)
- Evidence: tested

**Rationale.** A generic body is checked once, at its opaque type parameters. Every property
the checker established there must survive substitution, and the code that runs is the
specialization. A rule keyed on a property that substitution changes, such as whether a type is
Copy, breaks this (bug 105's class, and bug 181, PASS-2). Today this is tested only. S8a
(R-0510) replaces Mono with a specialization search plus a total validator and proves the
substitution example.

```con accept name=generic-at-int exit=42
fn twice<T: Copy>(x: T, f: fn(T) -> Int) -> Int {
    return f(x) + f(x);
}

fn half(n: Int) -> Int {
    return n / 2;
}

fn example_value() -> Int {
    return twice::<Int>(42, half);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```


<a id="PASS-2"></a>
### PASS-2. Binder modes are fixed before specialization

**PASS-2@2.** Every pattern binder's mode (by reference or by value) is decided once by the checker and carried explicitly in Core as the binder's type, and it is the same in every specialization, except that a whole-value binder over an owned scrutinee instantiated at a reference type may change mode where both modes bind the same value.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.payloadBinderTy, Concrete.wholeBinderTy
- Semantics: none
- Obligation: CoreCheck re-checks the carried mode after Mono (R-0496, R-0510)
- Evidence: tested

**Rationale.** Bug 181: Check gave a payload binder through a reference the type `&T`, Elab
gave it `T`, and after specialization at `T := Int` the binary dereferenced the payload as an
address. The decision was made in one place and recomputed in another, and nothing compared
the two. REF-9 removes the Copy dependence from the rule. This entry requires the mode to be
data in Core rather than recomputed: since 2026-10-23 a binder's Core type is its mode (a
reference type is a borrowed binder), decided once from the scrutinee's type, and Lower and the
interpreter read it from there. The re-check after Mono is the open obligation. Version 2 (the
first enforced one) names the one case where the carried type changes under substitution: a
whole-value binder `n` over a scrutinee of type `T` at `T := &Int` gets a reference type, and
Lower and the interpreter bind the scrutinee itself in both modes.

```con accept name=binder-mode-survives-mono exit=42
fn get<T>(o: &Option<T>, k: fn(&T) -> Int) -> Int {
    match o {
        Option::Some { value } => { return k(value); },
        Option::None => { return 0; }
    }
}

fn read(p: &Int) -> Int {
    return *p;
}

fn example_value() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    return get::<Int>(&o, read);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-3"></a>
### PASS-3. The native profile is a forward simulation from checked Core to AArch64

**PASS-3@1.** For the native profile, every behaviour of the compiled AArch64 image is a behaviour of the checked Core program it was compiled from, proved as a forward simulation through each stage under the stated premises of the trusted base.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0508
- Semantics: none
- Obligation: the composed compiler-correctness theorem (R-0508), with the native image checked by R-0505
- Evidence: tested

**Rationale.** This is the theorem the verified pipeline exists for (VERIFIED_PIPELINE, Part 2).
Forward simulation is turned into backward refinement only after its receptiveness and
determinacy hypotheses are proved. Each stage's own preservation entry in this chapter is one
link of the chain.

<a id="PASS-4"></a>
### PASS-4. The LLVM profile is trusted and differential-tested

**PASS-4@1.** The LLVM profile is outside PASS-3: its binaries are labelled unverified, its BIR-to-LLVM printer is trusted, and its results are differential-tested against the interpreter and the native profile.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0503
- Semantics: none
- Obligation: none (trusted; TRUSTED_COMPUTING_BASE section 4)
- Evidence: tested

**Rationale.** LLVM is the fast profile, not the verified one. Unverified does not mean
unspecified: the printer adds no poison- or UB-generating flags without their preconditions, and
an unsupported mapping fails explicitly (VERIFIED_PIPELINE, "IR chain").

<a id="PASS-5"></a>
### PASS-5. After Check: linearity resolved, operand types agree

**PASS-5@1.** A program that Check accepts has every owned linear local, parameter and branch or arm local consumed on every normal exit and every arithmetic, bitwise or comparison operation on numeric operands at one numeric type, so no later stage receives either violation.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.check
- Semantics: none
- Obligation: Check's judgment is the Core checker's (R-0499); width agreement comes from the one `IntArith` authority (R-0493)
- Evidence: tested

**Rationale.** Front end and back end share one width and signedness truth, so "Check says ok,
SSA-verify says no" cannot happen. A leak caught by Check is never left for a later pass to
notice.

```con reject name=unconsumed-linear-local code=E0208 stage=check pair=linear-local-consumed
mod m {
    struct R { x: i32 }
    fn f() { let r: R = R { x: 1 }; }
    fn main() -> u8 { return 0; }
}
```

```con reject name=mixed-width-binop code=E0228 stage=check pair=linear-local-consumed
mod m {
    fn example_value() -> Int {
        let a: i32 = 1;
        let b: i64 = 2;
        return (a + b) as Int;
    }
    fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con accept name=linear-local-consumed exit=42
mod m {
    struct R { x: i32 }
    fn f() -> i32 {
        let r: R = R { x: 41 };
        let R { x } = r;
        return x;
    }
    fn example_value() -> Int {
        let a: i64 = 1;
        let b: i64 = f() as i64;
        return (a + b) as Int;
    }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="PASS-6"></a>
### PASS-6. After Elab: desugaring adds nothing

**PASS-6@1.** Elaboration into Core introduces no ill-typed term, no mixed-width operation and no capability call hidden from the capability check, and CoreCheck refuses any Core in which it would have, so desugaring never smuggles an effect past the check.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.elaborate, Concrete.Pipeline.coreCheck
- Semantics: none
- Obligation: elaboration is proved against the declarative elaboration relation (VERIFIED_PIPELINE, "Passes"; R-0496)
- Evidence: tested

**Rationale.** Elaboration rewrites loops, patterns, method calls and borrow sugar into Core.
CoreCheck re-checks the result, so a desugaring that introduced a capability call or a
mixed-width operation is refused there, not compiled.

```con accept name=sugar-elaborates exit=42
struct Copy P { a: Int, b: Int }

fn get(p: &P) -> Int {
    return p.a + p.b;
}

fn example_value() -> Int {
    let base: P = P { a: 20, b: 2 };
    let q: P = P { a: 40, ..base };
    let mut total: Int = 0;
    for (let mut i: Int = 0; i < 1; i = i + 1) {
        total = total + get(&q);
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

<a id="PASS-7"></a>
### PASS-7. After Mono: no type variable survives

**PASS-7@1.** After monomorphization no type variable and no unresolved generic declaration remains in the Core that later stages compile, and every Copy type's fields are Copy in each specialization.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.monomorphize, Concrete.verifyPostMono
- Semantics: none
- Obligation: specialization validated by S8a's total validator (R-0510)
- Evidence: tested

**Rationale.** Lowering assumes every type is concrete. `verifyPostMono` is a hard wall: a
specialization that left a type variable, or a Copy specialization with a non-Copy field, stops
the pipeline instead of reaching the backend.

```con accept name=generic-specialized exit=42
struct Copy Pair<T> { a: T, b: T }

fn sum<T: Copy>(p: Pair<T>, f: fn(T) -> Int) -> Int {
    return f(p.a) + f(p.b);
}

fn id(x: Int) -> Int {
    return x;
}

fn example_value() -> Int {
    let p: Pair<Int> = Pair::<Int> { a: 40, b: 2 };
    return sum::<Int>(p, id);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-8"></a>
### PASS-8. After CoreCheck: no invalid Core proceeds

**PASS-8@1.** No Core program proceeds past CoreCheck unless its binary operations are width-compatible, every capability it uses is declared, every Copy type has Copy fields, no type has infinite size, and every unsafe or trusted operation carries its fact.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.coreCheckProgram
- Semantics: none
- Obligation: CoreCheck becomes the authoritative checker on Core (R-0499)
- Evidence: tested

**Rationale.** CoreCheck is the wall between the front end and code generation. A recursive type
used to reach `llvm-as` (bug 024). Each reject example pins the `core-check` boundary, so a
regression that moves a check later fails here even when the program is still refused.

```con reject name=undeclared-capability code=E0520 stage=core-check pair=valid-core
mod m {
    fn needs() with(Network) -> Int { return 0; }
    fn example_value() -> Int { return needs(); }
    fn main() -> u8 {
        if example_value() == 0 {
            return 0;
        }
        return 1;
    }
}
```

```con reject name=copy-struct-non-copy-field code=E0571 stage=core-check pair=valid-core
mod m {
    struct S { x: i32 }
    struct Copy T { s: S }
    fn main() -> u8 { return 0; }
}
```

```con reject name=infinite-size-type code=E0583 stage=core-check pair=valid-core
mod m {
    struct S { x: S }
    fn main() -> u8 { return 0; }
}
```

```con accept name=valid-core exit=42
mod m {
    fn needs() with(Network) -> Int { return 42; }
    struct Copy S { x: i32 }
    struct Copy T { s: S }
    fn example_value() with(Network) -> Int {
        let t: T = T { s: S { x: 0 } };
        return needs() + (t.s.x as Int);
    }
    fn main() with(Network) -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="PASS-9"></a>
### PASS-9. After Lower: verified SSA with every runtime check

**PASS-9@1.** Lowering produces SSA that passes the SSA verifier, with a runtime trap for every check the semantics requires (array bounds, integer overflow, division by zero and shift range) and with every defer and scope cleanup represented as edges of the control-flow graph.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower, Concrete.ssaVerifyProgram
- Semantics: none
- Obligation: Core to Place MIR to BIR as proved simulations or validated passes (R-0501, R-0503, R-0504)
- Evidence: tested

**Rationale.** Bounds, overflow and division are semantics, not static checks: they are trapping
operations in Core, and a pass may remove a trap only with a discharged-VC certificate naming
that operation (VERIFIED_PIPELINE, "Where each check lives"). The verifier runs before and after
SSA cleanup.

```con accept name=checked-operations-pass exit=42
fn example_value() -> Int {
    let a: [Int; 4] = [10, 20, 5, 7];
    let mut total: Int = 0;
    let mut i: Int = 0;
    while i < 4 {
        total = total + a[i];
        i = i + 1;
    }
    return total / 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

<a id="PASS-10"></a>
### PASS-10. After Emit: checked operations stay checked, executables have an entry point

**PASS-10@1.** Every checked operation is emitted through a named helper or a named proven elision and never as a raw wrapping or undefined LLVM operation, and an executable build without an entry point is refused with a `link` diagnostic before the system linker runs.

- Kind: preservation
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.emit, compileSSA
- Semantics: none
- Obligation: none (the LLVM profile is trusted, PASS-4)
- Evidence: tested

**Rationale.** Overflow checks lower to `@__cc_*` helpers built on LLVM's overflow intrinsics,
not to plain `add` with undefined wrap. A missing `main` used to leak from the system linker as
"Undefined symbols: _main" (bug 025). The `link` diagnostic has no code yet, and the
interpreter reports a missing `main` its own way.

```con reject name=no-entry-point code=none stage=link pair=entry-point interp-exempt=no-link-stage-in-the-interpreter
mod m {
    fn helper() -> Int { return 1; }
}
```

```con accept name=entry-point exit=42
mod m {
    fn helper() -> i32 { return 41; }
    fn example_value() -> Int { return (helper() + 1) as Int; }
    fn main() -> u8 {
        if example_value() == 42 {
            return 42;
        }
        return 1;
    }
}
```

<a id="PASS-11"></a>
### PASS-11. BackendIR keeps backend intent structured

**PASS-11@1.** Runtime checks, trap and source spans, layout and ABI facts, helper calls, target constants and capability and trust labels stay structured through BackendIR, and `BackendIRVerify` refuses malformed backend intent before LLVM text is emitted.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0503
- Semantics: none
- Obligation: BIR is the only input to LLVM and to the native adapter (R-0503)
- Evidence: tested

**Rationale.** Reports, audits and code generation must not reconstruct facts from LLVM text.
The boundary is useful even while LLVM is the only emitter.

<a id="PASS-12"></a>
### PASS-12. An accepted program is compiled

**PASS-12@1.** A program that every rule of this specification accepts is compiled: no stage after Check refuses it, crashes on it or emits LLVM IR that fails validation.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial bug-054
- Enforced by: Concrete.Pipeline.lower, Concrete.Pipeline.emit
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The front end is where a program is judged; the later stages only translate it. A
generic function that calls itself at a growing type argument does not finish monomorphizing in the current compiler;
whether Check refuses it, and how, is OPEN-132. A refusal,
a crash or invalid IR after Check means the program met no rule and still did not compile, so the
user gets no diagnostic that names a rule. Bugs 005, 009, 014, 016, 029, 032, 035, 044, 055, 077,
108 and 116 were such failures. A refusal that a later stage needs (such as E0809, when a generated
specialization's name is already declared) is a gap in a rule, not a licence: the current compiler still refuses
the program below at monomorphization (bug 054, whose mangling is forgeable).

```con accept name=generic-beside-user-types exit=42
struct Copy Box<T> { v: T }
struct Copy Pair { a: Int, b: Int }

fn example_value() -> Int {
    let a: Box<Int> = Box::<Int> { v: 40 };
    let p: Pair = Pair { a: 1, b: 1 };
    return a.v + p.a + p.b;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=user-type-spells-a-specialization exit=42 pending=bug-054 now=reject:E0809:mono
struct Copy Box<T> { v: T }
struct Copy Box_Int { v: Int }

fn example_value() -> Int {
    let a: Box<Int> = Box::<Int> { v: 40 };
    let b: Box_Int = Box_Int { v: 2 };
    return a.v + b.v;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="PASS-13"></a>
### PASS-13. Every consumer gives the checked Core behaviour

**PASS-13@1.** The interpreter and every compiled profile give an accepted program the behaviour of its checked Core program: the same result, the same output and the same traps.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Pipeline.lower, Concrete.Pipeline.emit, Concrete.Interp.interpret
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** PASS-3 states this for the native profile as a proof obligation (pending, R-0508) and
PASS-4 tests the LLVM profile against the interpreter; this entry states the agreement every consumer owes, which the differential gates
(native against `--interp`) check on every example of this specification. Bugs 004, 006, 019,
038, 102, 184 and 254 were disagreements between consumers.

```con accept name=consumers-agree exit=42
fn example_value() -> Int {
    let mut s: Int = 0;
    let mut i: Int = 0;
    while i < 7 {
        s = s + i;
        i = i + 1;
    }
    return s + 21;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```


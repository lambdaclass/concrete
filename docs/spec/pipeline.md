# Pipeline Preservation

The compiler is a chain of stages (PIPELINE_CONTRACTS.md). Each stage's preservation entry
states the relation between the stage's input and its output, and the evidence for it. Under
VERIFIED_PIPELINE the target form of each statement is
`WF Γ p → pass p = .ok q → WF Γ q ∧ Refines Γ p q`. Until that is proved, the evidence is
`tested`: the examples are accepted programs, and every consumer must give each one the stated
result. This chapter is a stub. Generic substitution and binder modes come first, and the other
boundaries of [PIPELINE_CONTRACTS.md](../compiler/PIPELINE_CONTRACTS.md) migrate here
(README §8, step 0).

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

fn main() -> Int {
    return twice::<Int>(42, half);
}
```


<a id="PASS-2"></a>
### PASS-2. Binder modes are fixed before specialization

**PASS-2@1.** Every pattern binder's mode (by reference or by value) is decided once by the checker, carried explicitly in Core, and is the same in every specialization.

- Kind: preservation
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: CoreCheck re-checks the carried mode after Mono (R-0496, R-0510)
- Evidence: tested

**Rationale.** Bug 181: Check gave a payload binder through a reference the type `&T`, Elab
gave it `T`, and after specialization at `T := Int` the binary dereferenced the payload as an
address. The decision was made in one place and recomputed in another, and nothing compared
the two. REF-9 removes the Copy dependence from the rule. This entry requires the mode to be
data in Core rather than recomputed.

```con accept name=binder-mode-survives-mono exit=42 pending=R-0496 now=fail:signal_11 now-interp=fail:deref_of_non-ref_value
fn get<T>(o: &Option<T>, k: fn(&T) -> Int) -> Int {
    match o {
        Option::Some { value } => { return k(value); },
        Option::None => { return 0; }
    }
}

fn read(p: &Int) -> Int {
    return *p;
}

fn main() -> Int {
    let o: Option<Int> = Option::Some { value: 42 };
    return get::<Int>(&o, read);
}
```

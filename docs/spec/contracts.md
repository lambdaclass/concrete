# Contracts

A contract is a claim, not a check the program runs: the compiler turns each claim into
obligations with stable identities and attaches evidence to each (proved by a Lean theorem or a
kernel decision procedure, assumed, or still open). Contracts and ghost code are erased before code
generation and never change what the program does. This chapter states what a contract means
(CON-1, CON-2), what a contract expression may contain (CON-3, pending its specification
judgment), how ghost code is kept apart from runtime code (CON-4), specification functions (CON-5),
`assert` and `assume` (CON-6), the scope of `result` (CON-7) and stacked clauses (CON-8). How obligations are
discharged and reported is the verification documentation:
[CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) (now a usage guide),
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md) and
[EVIDENCE_CLASSES.md](../verification/EVIDENCE_CLASSES.md).

## Rules

<a id="CON-1"></a>
### CON-1. Claims are partial correctness

**CON-1@1.** A contract claim states partial correctness, holding for every execution that terminates, and a function is claimed to terminate only when `decreasing` evidence for its loops and recursion is checked.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0473
- Semantics: none
- Obligation: claims are generated against Core semantics as partial-correctness statements, with total correctness only from checked `decreasing` measures (R-0473, R-0495)
- Evidence: tested

**Rationale.** Partial correctness is what a proof about a terminating execution can establish
without a termination argument. Termination is a separate, explicit claim, so a proof never
implies termination by accident. Divergence remains one of the three outcomes (STMT-1).

<a id="CON-2"></a>
### CON-2. What `requires`, `ensures`, `invariant` and `variant` claim

**CON-2@1.** `#[requires(p)]` claims that `p` holds at every call (an obligation of each caller, assumed on entry), `#[ensures(q)]` claims that `q` holds at every return with `result` naming the returned value, and a loop's `#[invariant(i)]` and `#[variant(v)]` claim that `i` holds before the loop and after every iteration and that `v` is non-negative and decreases on every iteration.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Report.callSiteObligations, Concrete.Report.loopVCGoals
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** A claim is generated into obligations (a call site's precondition, a loop's
initialization, preservation, exit-to-postcondition, variant non-negativity and decrease), each
with its own evidence; nothing is reported green without it. Contracts never execute, so a program
with false contracts runs exactly as one without them; whether a claim is proved is a separate
question that `--report contracts` answers.

```con accept name=contracts-are-erased exit=42
#[requires(n >= 0)]
#[ensures(result == n)]
fn count(n: Int) -> Int {
    let mut i: Int = 0;
    #[invariant(0 <= i && i <= n)]
    #[variant(n - i)]
    while i < n {
        i = i + 1;
    }
    return i;
}

fn example_value() -> Int {
    return count(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-3"></a>
### CON-3. A contract expression is a pure specification

**CON-3@1.** A contract expression is checked by its own specification judgment: it binds no name, moves and mutably borrows nothing, calls only specification functions, reads parameters and `result`, and its integer arithmetic is mathematical unless it names a machine operation.

- Kind: refusal
- Uses: none
- Codes: E0320 (check), E0321 (check)
- Status: partial R-0473
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the specification judgment of contract expressions (R-0473)
- Evidence: tested

**Rationale.** Decided 2026-10-23 (TIGHTENING_DECISIONS, T-H27). Today a contract expression is
checked like runtime code, so machine types leak into a claim (`#[requires(x < 9999999999)]` on an
`i32`), and an ill-typed contract such as `(x > 0) + 1 > 0` is accepted (KNOWN_HOLES H27). The
binder-free part is already enforced as SCOPE-1 (E0305). Two more parts are enforced: a clause or
spec function body of the wrong sort (an integer `requires`, a boolean `variant`) is E0320, and a
call that the clause may not make is E0321: from a spec function body, a call to anything but a
spec function; from a contract clause, a call to a function that requires a capability (a
capability-free runtime function is still callable there until the full judgment lands).
Under the full judgment, operand sorts must agree inside an expression too and literals are
mathematical.

Decided by the owner on 2026-10-29 (OD12): a contract is a pure, non-consuming specification judgment, with machine and mathematical arithmetic kept distinct.

```con reject name=ill-typed-contract code=new stage=check pair=well-typed-contract pending=R-0473 now=accept:42
#[requires((x > 0) + 1 > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> Int {
    return f(42) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con reject name=integer-requires code=E0320 stage=check pair=well-typed-contract
#[requires(x + 1)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> Int {
    return f(42) as Int;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con reject name=spec-fn-calls-runtime-code code=E0321 stage=check pair=well-typed-contract
fn twice(x: i32) -> i32 {
    return x + x;
}

spec fn big(x: i32) -> bool = twice(x) > 10;

fn main() -> u8 {
    return 42;
}
```

```con accept name=well-typed-contract exit=42
#[requires(x > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn example_value() -> Int {
    return f(42) as Int;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-4"></a>
### CON-4. Ghost code stays out of runtime code

**CON-4@1.** A `ghost let` binding is erased before code generation and may be read only by contracts and other ghost code, never by runtime code.

- Kind: refusal
- Uses: none
- Codes: E0420 (elab)
- Status: partial R-0499
- Enforced by: Concrete.throwElab
- Semantics: none
- Obligation: ghost erasure preserves runtime behaviour (R-0496)
- Evidence: tested

**Rationale.** A ghost value names something a proof needs and the program does not keep. Proof
code cannot change runtime ownership in the other direction either (OWN-11). Ghost destructuring
bindings are not supported. A ghost initializer is elaborated as runtime code today, so it cannot
read another ghost binding yet (the pending example below). E0420 names only this refusal; the
`append()` argument refusal that shared it is E0421 (TYPE-10, bug 203).

```con reject name=ghost-read-at-runtime code=E0420 stage=elab pair=ghost-in-contract
fn f() -> Int {
    ghost let bound: Int = 8;
    return bound;
}

fn example_value() -> Int {
    return f();
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=ghost-in-contract exit=28
fn f() -> Int {
    ghost let bound: Int = 8;
    let mut acc: Int = 0;
    #[invariant(0 <= i && i <= bound)]
    #[variant(bound - i)]
    for (let mut i: Int = 0; i < 8; i = i + 1) {
        acc = acc + i;
    }
    return acc;
}

fn example_value() -> Int {
    return f();
}

fn main() -> u8 {

    if example_value() == 28 {

        return 28;

    }

    return 1;

}
```

```con accept name=ghost-reads-ghost exit=28 pending=R-0499 now=reject:E0420:elab
fn f() -> Int {
    ghost let bound: Int = 8;
    ghost let limit: Int = bound;
    let mut acc: Int = 0;
    #[invariant(0 <= i && i <= limit)]
    #[variant(limit - i)]
    for (let mut i: Int = 0; i < 8; i = i + 1) {
        acc = acc + i;
    }
    return acc;
}

fn example_value() -> Int {
    return f();
}

fn main() -> u8 {

    if example_value() == 28 {

        return 28;

    }

    return 1;

}
```

<a id="CON-5"></a>
### CON-5. A specification function is erased

**CON-5@1.** A `spec fn` is erased: no code is generated for it, and it is named only by contract expressions and the bodies of other `spec fn` declarations, never by runtime code.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.elabModule, Concrete.checkModule
- Semantics: none
- Obligation: ghost erasure preserves runtime behaviour (R-0496)
- Evidence: tested

**Rationale.** A specification function is a mathematical description that a claim names, as
`rotr_spec` names the function `rotr` must compute. Runtime code that names one is refused as an
unknown name (E0101, MOD-1), since only the contract tables resolve it; a `spec fn` body that calls
runtime code is E0321 (CON-3). Whether a `spec fn` may be declared without a body, and whether it
must be total, is OPEN-115: the current compiler accepts both a body-less declaration and a recursive body.
Absorbed from [CONTRACTS_GUIDE.md](../verification/CONTRACTS_GUIDE.md) and
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=spec-fn-in-contract exit=42
spec fn big(x: Int) -> bool = x > 10;

#[requires(big(x))]
fn f(x: Int) -> Int {
    return x;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-6"></a>
### CON-6. What `assert` and `assume` claim

**CON-6@1.** `assert e` claims that `e` holds where it is written, an obligation of the function, and `assume e` makes `e` a hypothesis of the obligations after it whose own evidence is `assumed`, never `proved`, and neither statement executes.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.elabStmtEv, Concrete.Report.renderAssertAssume
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** `assume` is a proof-only trapdoor: a claim that leans on it is reported as depending
on an assumption, never as a clean proof, and a project policy may forbid it. An `assert` that is
never discharged stays `unproven`, and one whose condition is constant-false is reported as a
violation; neither stops the program, since both are erased (OWN-11 keeps them from changing
ownership). Whether the grammar has the two statements is OPEN-79 and OPEN-80. Absorbed from
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=assert-and-assume-are-erased exit=42
fn f(x: Int) -> Int {
    assume x > 0;
    assert x > 0;
    return x;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-7"></a>
### CON-7. `result` is bound only in a postcondition

**CON-7@1.** `result` names the returned value in an `#[ensures]` clause and is bound in no other contract clause, so a `requires`, `invariant` or `variant` that names it names an unknown variable.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Only a postcondition is evaluated after a return. A precondition that names `result`
is refused as an undeclared variable (E0201, MOD-1). Outside contracts `result` is an ordinary
identifier. Absorbed from [KNOWN_HOLES.md](../verification/KNOWN_HOLES.md) H27.

```con accept name=result-in-ensures exit=42
#[requires(x > 0)]
#[ensures(result == x)]
fn f(x: Int) -> Int {
    let result: Int = x;
    return result;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="CON-8"></a>
### CON-8. Stacked clauses are a conjunction

**CON-8@1.** Several `#[requires]` clauses on one function, or several `#[invariant]` clauses on one loop, claim their conjunction, each clause becoming an obligation of its own.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.Report.callSiteObligations, Concrete.Report.loopVCGoals
- Semantics: none
- Obligation: claims are generated against Core semantics with exact contract identity (R-0473, R-0495)
- Evidence: tested

**Rationale.** CON-2 states one clause; writing two is the same claim as writing their `&&`, with
separate identities in the report (`R1`, `R2`). Absorbed from
[CONTRACTS_AND_VCS.md](../verification/CONTRACTS_AND_VCS.md).

```con accept name=stacked-requires exit=42
#[requires(x > 0)]
#[requires(x < 100)]
fn f(x: Int) -> Int {
    return x;
}

fn example_value() -> Int {
    return f(42);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

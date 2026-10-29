# Contracts

A contract is a claim, not a check the program runs: the compiler turns each claim into
obligations with stable identities and attaches evidence to each (proved by a Lean theorem or a
kernel decision procedure, assumed, or still open). Contracts and ghost code are erased before code
generation and never change what the program does. This chapter states what a contract means
(CON-1, CON-2), what a contract expression may contain (CON-3, pending its specification
judgment) and how ghost code is kept apart from runtime code (CON-4). How obligations are
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

fn main() -> Int {
    return count(42);
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

```con reject name=ill-typed-contract code=new stage=check pair=well-typed-contract pending=R-0473 now=accept:42
#[requires((x > 0) + 1 > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn main() -> Int {
    return f(42) as Int;
}
```

```con reject name=integer-requires code=E0320 stage=check pair=well-typed-contract
#[requires(x + 1)]
fn f(x: i32) -> i32 {
    return x;
}

fn main() -> Int {
    return f(42) as Int;
}
```

```con reject name=spec-fn-calls-runtime-code code=E0321 stage=check pair=well-typed-contract
fn twice(x: i32) -> i32 {
    return x + x;
}

spec fn big(x: i32) -> bool = twice(x) > 10;

fn main() -> Int {
    return 42;
}
```

```con accept name=well-typed-contract exit=42
#[requires(x > 0)]
fn f(x: i32) -> i32 {
    return x;
}

fn main() -> Int {
    return f(42) as Int;
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

fn main() -> Int {
    return f();
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

fn main() -> Int {
    return f();
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

fn main() -> Int {
    return f();
}
```


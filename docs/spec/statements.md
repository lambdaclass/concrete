# Statements, Expressions and Execution

Statements execute in source order. A block's trailing expression without `;` is its value, and
`e;` is a statement whose value is discarded (OWN-5 to OWN-7 say which values may be). This
chapter covers how executions end (STMT-1 to STMT-3), evaluation order (STMT-4), where exits may
appear (STMT-5), loops (STMT-6), `main` (STMT-7), value blocks (STMT-8) and returning a value on
every path (STMT-9). The design records are
[STATEMENT_EXPRESSION_MODEL.md](../language/STATEMENT_EXPRESSION_MODEL.md),
[LOOP_CONTROL.md](../language/LOOP_CONTROL.md) and
[MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md). [EXECUTION_MODEL.md](../language/EXECUTION_MODEL.md)
keeps the runtime and platform description (hosted runtime, startup, allocation, targets).

## Rules

<a id="STMT-1"></a>
### STMT-1. Exit, abort and divergence are distinct behaviours

**STMT-1@1.** Every execution that does not get stuck has exactly one behaviour, a trace of external events that ends in exit with a status, abort at a defined trap, silent divergence or infinite external interaction, and no behaviour is observed as another, while stuck execution (a violated premise of a trusted operation) is a distinct state that safe code never reaches.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the behaviour definition of Core semantics (R-0496)
- Evidence: tested

**Rationale.** Correctness theorems quantify over behaviours. If a trap could look like a normal
exit, or a divergence like an exit, a refinement proof would equate different programs. The Core
semantics records the external-event trace together with its outcome (VERIFIED_PIPELINE,
"Semantic contract"). Clock exhaustion in the evaluator is inconclusive, not divergence.

<a id="STMT-2"></a>
### STMT-2. Failure is abort only

**STMT-2@1.** A failure (a trap, running out of memory, or an explicit abort) ends the process at once, with no unwinding and no deferred calls, destructors or loan endings run.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: abort is a behaviour of Core semantics and every "exactly once" cleanup claim is qualified by "on every path that does not abort" (R-0496)
- Evidence: tested

**Rationale.** One failure mode keeps the semantics and the linearity argument small. Cleanup is
guaranteed on normal and structured exits, and abort promises none
([FAILURE_STRATEGY.md](../language/FAILURE_STRATEGY.md); TIGHTENING_DECISIONS §4.5, T-ARITH).
Recoverable errors are values (`Result`), not failures.

<a id="STMT-3"></a>
### STMT-3. Programs are single-threaded

**STMT-3@1.** A Concrete program has one thread of execution, so no memory is mutated concurrently with it.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: the first verified profile is sequential (R-0496)
- Evidence: tested

**Rationale.** Concurrency needs a memory model, ownership-transfer rules and new refinement
obligations, and none exists yet. Foreign threads, asynchronous shared-memory mutation, signals
and reentrant callbacks are outside the semantics unless the environment model covers them
(VERIFIED_PIPELINE, "Scope of the first verified profile"). Child processes are separate
programs, not threads.

<a id="STMT-4"></a>
### STMT-4. Evaluation is left to right, assignment evaluates its value first

**STMT-4@1.** Subexpressions are evaluated left to right in source order, each completely before the next begins (a call's receiver, then its arguments; a binary operator's left operand, then its right, with `&&` and `||` short-circuiting; literal elements and fields in the order written; a base before its index; a functional update's explicit fields, then its base exactly once), except that an assignment evaluates its right-hand side before its destination's subexpressions.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: partial R-0500
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: V1a's evaluation-order obligation; the verified pipeline may not model a looser order than the compiler implements (R-0500, R-0496)
- Evidence: tested

**Rationale.** A fixed order makes effects predictable and lets every consumer agree. A trap ends
evaluation: no later operand is evaluated. Three cases are violated today, each a silent defect
with its own record: an enum literal evaluates its fields in declaration order (bug 137), a
functional update reads its base once per copied field (bug 136), and an assignment through a
dereference evaluates its destination first natively (bug 183). Struct literals follow source
order since bug 127.

```con accept name=source-order exit=42
struct Copy P { a: Int, b: Int }

fn tick(acc: &mut Int, k: Int) -> Int {
    *acc = *acc * 10 + k;
    return k;
}

fn main() -> Int {
    let mut acc: Int = 0;
    let p: P = P { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    let s: Int = tick(&mut acc, 1) + tick(&mut acc, 2);
    return acc - 2112 + p.a + p.b + s * 0 + 39;
}
```

```con accept name=enum-literal-source-order exit=21 pending=bug-137 now=accept:12
enum Copy E { V { a: Int, b: Int }, W }

fn tick(acc: &mut Int, k: Int) -> Int {
    *acc = *acc * 10 + k;
    return k;
}

fn main() -> Int {
    let mut acc: Int = 0;
    let e: E = E::V { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    return acc;
}
```

<a id="STMT-5"></a>
### STMT-5. No exit inside an operand

**STMT-5@1.** `return`, `break` and `continue` do not occur inside a call argument or receiver, a struct, enum or array literal element (a functional-update base included), or an operator operand, unless a `break` or `continue` belongs to a loop entered inside that operand.

- Kind: refusal
- Uses: none
- Codes: E0278 (check)
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: an operand already evaluated for a pending construct is a live owned temporary until it completes (R-0499)
- Evidence: tested

**Rationale.** An exit inside an operand leaves a call or literal whose earlier operands were
already evaluated and moved into it; the construct never completes and, with no implicit
destruction, nothing destroys them (bug 142). An exit stays legal in statement position, in a
tail, and in a `let` or assignment initializer's branch chain (the `let ... else` idiom written as a
`match`).

```con reject name=return-in-argument code=E0278 stage=check pair=exit-in-branch
fn add(a: Int, b: Int) -> Int {
    return a + b;
}

fn f(c: bool) -> Int {
    return add(1, if c { return 42; } else { 2 });
}

fn main() -> Int {
    return f(true);
}
```

```con accept name=exit-in-branch exit=42
fn f(o: Option<Int>) -> Int {
    let v: Int = match o {
        Option::Some { value } => value,
        Option::None => { return 42; }
    };
    return v;
}

fn main() -> Int {
    return f(Option::None);
}
```

<a id="STMT-6"></a>
### STMT-6. Loops are statements; `break` and `continue` name a loop

**STMT-6@1.** `while` and `for` are statements, and `break` and `continue` act on the innermost enclosing loop or, with a label, on the labeled loop, and appear only inside a loop.

- Kind: refusal
- Uses: none
- Codes: E0216 (check), E0280 (check), E0281 (check), E0542 (core-check), E0543 (core-check)
- Status: enforced
- Enforced by: Concrete.checkStmt, Concrete.ccCheckStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** There is no bare `loop`; `while true` is the infinite loop. A loop produces no
value (the value `while … else` form was removed in Phase 6D); a result is a mutable local
assigned before `break`. In a `for (init; cond; step)` loop, `continue` still runs `step`. A
`break` or `continue` leaves the loop body's scopes like any exit (OWN-2).

```con reject name=break-outside-loop code=E0280 stage=check pair=labeled-break
fn main() -> Int {
    break;
    return 0;
}
```

```con accept name=labeled-break exit=42
fn main() -> Int {
    let mut out: Int = 0;
    'outer: for (let mut i: Int = 0; i < 10; i = i + 1) {
        for (let mut j: Int = 0; j < 10; j = j + 1) {
            if j == 2 {
                continue 'outer;
            }
            if i == 7 {
                break 'outer;
            }
            out = out + 3;
        }
    }
    return out;
}
```

<a id="STMT-7"></a>
### STMT-7. `main` takes no parameters and its result is the exit status

**STMT-7@1.** The entry point is a module-level `fn main()` with no parameters, returning `Int`, a narrower integer or `Unit`, and its result masked to 0–255 is the process exit status (`Unit` exits 0), with nothing written by the runtime.

- Kind: refusal
- Uses: none
- Codes: E0310 (check)
- Status: enforced
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the exit status is the `exit` behaviour of STMT-1 (R-0496)
- Evidence: tested

**Rationale.** Command-line arguments come from `std.args` (`with(Env)`); a parameterized `main`
used to read an undefined register (bug 111). There is no `std.process.exit`: an early exit would
skip cleanup the linear rules promise, so status flows back through `main`. Narrowing the result
to `u8` (so masking is unrepresentable) is a recorded preference, not the rule
(MAIN_EXIT_MODEL). `--interp` prints the result instead of exiting with it.

```con reject name=main-with-parameter code=E0310 stage=check pair=main-exit-status
fn main(argc: Int) -> Int {
    return 0;
}
```

```con accept name=main-exit-status exit=42
fn main() -> Int {
    return 42;
}
```

<a id="STMT-8"></a>
### STMT-8. A trailing expression is the block's value

**STMT-8@1.** In a block that produces a value (an `if` expression's branch, a value `match` arm or a `let` initializer's branch), the last expression written without `;` is the block's value, while a function body produces its result only through `return`.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: enforced
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The `;` decides whether an expression is a value or a discarded statement, so a
statement `match` whose arms happen to end in expressions is not a value (the fixed E0225 class).
A trailing `if` or `match` inside a value block is itself value-bearing when its branches end in
values. A braced block as an arbitrary expression (`let x = { ...; v };`) and an implicit trailing
return in a function body are not in the language.

```con accept name=trailing-values exit=42
fn pick(c: bool, k: Int) -> Int {
    let v: Int = if c {
        let t: Int = k * 2;
        match k {
            0 => 1,
            _ => t
        }
    } else {
        0
    };
    return v;
}

fn main() -> Int {
    return pick(true, 21);
}
```

<a id="STMT-9"></a>
### STMT-9. A function with a result type returns on every path

**STMT-9@1.** A function whose result type is not `Unit` cannot reach the end of its body: every path ends in a `return`, an abort or a loop that never exits.

- Kind: refusal
- Uses: none
- Codes: new (check)
- Status: pending R-0499
- Semantics: none
- Obligation: every reachable exit returns a value of the result type (R-0499)
- Evidence: tested

**Rationale.** Function bodies have no trailing value expression (STMT-8), so a reachable end has
no value to return. Nothing checked this: the backend failed in `llvm-as` (a `ret void` in a
non-void function) while `--interp` ran the program (bug 194, fixed on `wip/bug194` with E0307,
using a reachability analysis over every statement of a block).

```con reject name=one-branch-returns code=new stage=check pair=every-path-returns pending=R-0499 now=fail:doesn't_match_function_result_type now-interp=accept:42
fn f(c: bool) -> Int {
    if c {
        return 1;
    }
}

fn main() -> Int {
    return f(true) + 41;
}
```

```con accept name=every-path-returns exit=42
fn f(c: bool) -> Int {
    if c {
        return 1;
    }
    return 0;
}

fn main() -> Int {
    return f(true) + 41;
}
```


# Statements, Expressions and Execution

Statements execute in source order. A block's trailing expression without `;` is its value, and
`e;` is a statement whose value is discarded (OWN-5 to OWN-7 say which values may be). This
chapter covers how executions end (STMT-1 to STMT-3), evaluation order (STMT-4), where exits may
appear (STMT-5), loops (STMT-6), `main` (STMT-7), value blocks (STMT-8), returning a value on
every path (STMT-9), test functions (STMT-10), the value flow of statements (STMT-11), the counted
loop's measure (STMT-12) and the capability `abort()` needs (STMT-13). The design records are
[STATEMENT_EXPRESSION_MODEL.md](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md)
and [MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md). [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md)
keeps the runtime and platform description (hosted runtime, startup, allocation, targets).
The section "Program execution" at the end follows a program from its source files to a running
process and records what `--interp` does differently today.

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
### STMT-2. Allocation errors conserve; traps abort

**STMT-2@2.** A trap or explicit abort ends the process immediately without unwinding, deferred calls, destructors or loan endings, while program allocation failure is an ordinary error that conserves inputs and ownership obligations.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: abort is a behaviour of Core semantics and every "exactly once" cleanup claim is qualified by "on every path that does not abort" (R-0496)
- Evidence: tested

**Rationale.** The Allocation decision distinguishes ordinary conserving allocation errors from terminal traps. Stack, runtime and environment exhaustion remain separately stated premises of VERIFIED_PIPELINE, not program OOM aborts.

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

**STMT-4@2.** Subexpressions are evaluated left to right in source order, each completely before the next begins (a call's receiver, then its arguments; a binary operator's left operand, then its right, with `&&` and `||` short-circuiting; literal elements and fields in the order written; a base before its index), except that an assignment evaluates its right-hand side before its destination's subexpressions.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: V1a's evaluation-order obligation; the verified pipeline may not model a looser order than the compiler implements (R-0500, R-0496)
- Evidence: tested

**Rationale.** Evaluation remains left to right except that assignment computes its value before its destination; OD53 removes the functional-update evaluation case.

```con accept name=source-order exit=42
struct Copy P { a: i64, b: i64 }

fn tick(acc: &mut i64, k: i64) -> i64 {
    *acc = *acc * 10 + k;
    return k;
}

fn example_value() -> i64 {
    let mut acc: i64 = 0;
    let p: P = P { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    let s: i64 = tick(&mut acc, 1) + tick(&mut acc, 2);
    return acc - 2112 + p.a + p.b + s * 0 + 39;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=enum-literal-source-order exit=21
enum Copy E { V { a: i64, b: i64 }, W }

fn tick(acc: &mut i64, k: i64) -> i64 {
    *acc = *acc * 10 + k;
    return k;
}

fn example_value() -> i64 {
    let mut acc: i64 = 0;
    let e: E = E::V { b: tick(&mut acc, 2), a: tick(&mut acc, 1) };
    discard(e);
    return acc;
}

fn main() -> u8 {

    if example_value() == 21 {

        return 21;

    }

    return 1;

}
```

<a id="STMT-5"></a>
### STMT-5. No exit inside an operand

**STMT-5@2.** `return`, `break` and `continue` do not occur inside a call argument or receiver, a struct, enum or array literal element, or an operator operand, unless a `break` or `continue` belongs to a loop entered inside that operand.

- Kind: refusal
- Uses: none
- Codes: E0278 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkExprCore
- Semantics: none
- Obligation: an operand already evaluated for a pending construct is a live owned temporary until it completes (R-0499)
- Evidence: tested

**Rationale.** Operand exits cannot bypass the surrounding operation; OD53 removes functional update and its former base-operand case.

```con reject name=return-in-argument-decided code=E0278 stage=check pair=exit-in-branch pending=R-0555 now=reject:E0278:check
fn add(a: i64, b: i64) -> i64 {
    return a + b;
}

fn f(c: bool) -> i64 {
    return add(1, if c { return 42; } else { 2 });
}

fn example_value() -> i64 {
    return f(true);
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=exit-in-branch exit=42
fn f(o: Option<i64>) -> i64 {
    let v: i64 = match o {
        Option::Some { value } => value,
        Option::None => { return 42; }
    };
    return v;
}

fn example_value() -> i64 {
    return f(Option::None);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-6"></a>
### STMT-6. Loops are statements; `break` and `continue` name a loop

**STMT-6@4.** The loops are `while cond { ... }` and the counted loop `for i from a until b { ... }`, both statements, where the counted loop evaluates `a` and then `b` once, both of one integer type, which the index `i` takes, before its first iteration, compares `i` with `b` before each iteration and advances it only after a comparison that succeeded, so it never advances `i` past `b`, and runs its body with `i`, immutable and scoped to the body, bound to each value from `a` up to but not including `b`, none when `a >= b`; `break` carries no value, and `break` and `continue` act on the innermost enclosing loop or, with a label, on the labeled loop and appear only inside a loop, `continue` running the body's exit sequence and then advancing only the loop it names, while `break` and `return` advance nothing.

- Kind: refusal
- Uses: none
- Codes: E0216 (check), E0245 (check), E0280 (check), E0281 (check), E0542 (core-check), E0543 (core-check)
- Status: pending R-0555
- Enforced by: Concrete.checkStmt, Concrete.ccCheckStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** There is no bare `loop`; `while true` is the infinite loop. A loop produces no
value (the value `while … else` form was removed in Phase 6D); a result is a mutable local
assigned before `break`. In the current compiler's `for (init; cond; step)` loop, `continue` still runs `step`, and
E0245 refuses a `break` or `continue` in the step that targets its own loop (bug 330); E0245 belongs to
this entry only while the current compiler parses the step form, which this entry removes (OD47), and retires with it. A
`break` or `continue` leaves the loop body's scopes like any exit (OWN-2).

The current compiler's loop forms are `while cond { ... }` and the C-style forms that this entry removes (OD47, bug 331):
`for (init; cond; step) { ... }`, whose `init` is a `let`,
whose `cond` is the continuation test and whose `step` is an expression or an assignment
(`for (i = 0; ...)` is a parse error), `for (cond) { ... }` and the unconditional `for { ... }`
(`regress_176_loop_shadow_restores_outer.con`). An unlabeled `break` or
`continue` acts on the innermost enclosing loop; a label (`'outer: while ...`) is the only way to
act on an outer loop, and `break 'outer;` or `continue 'outer;` exits or continues every loop in
between. A `break` or `continue` that would leave a linear value live is refused (E0210, E0211;
OWN-2), while consuming the value before the jump is fine: the rule concerns values live at the
jump, as for an early `return`. A `break` or `continue` does not disable runtime-safety
obligations (an index in a loop with a `break` still gets its bounds obligation), and loop
contracts (`#[invariant]`, `#[variant]`) attach to the loop and are checked as usual.

Deliberately absent: a bare `loop { }` (write `while true`) and a `break` or `continue` out of a
non-loop block. `break <expr>;` is still parsed; its value is evaluated and discarded, and a
non-Copy value is refused (bug 149), since no loop produces a value. History: before Phase 6D a
`while` in value position took its value from `break <expr>` or from its `else` block's trailing
expression, with E0222 for a disagreement between the two (`while-expression break type 'T' does
not match else type 'U'`); a bug once stored that value at the default `i64` width into a narrower
slot, fixed by coercing it to the result type (`tests/programs/loop_control/while_expr_*_i32.con`).
Phase 6D item 2 removed the form: a loop result is a mutable local assigned before `break;`
(`tests/programs/loop_break_result_*.con`). The surface is pinned by
`scripts/tests/check_loop_control.sh` (fixtures in `tests/programs/loop_control/`; ROADMAP old
Phase 6 #4, 2026-06-21).

The owner decided on 2026-10-29 (OD27) that `break <expr>` is refused at parse.
The current compiler still parses a value after `break` inside a `while` or `for` statement and drops it (bug 323).


The owner decided on 2026-10-29 (OD47) that the loops are `while` and a counted
range loop, and the C-style `for` is removed in every form (`for (init; cond; step)`,
`for (cond)` and the unconditional `for`). The counted loop is half-open, `for i from a until b`
running `a`, `a + 1`, ..., `b - 1` and nothing when `a >= b`; its index cannot be assigned in the
body, and its scope is the body; both bounds must have one integer type, which the index takes.
`from` and `until` are reserved (OD50). The grammar is LL(1): `for` IDENT `from` expr `until` expr block, the upper bound
being a restricted head expression so that `{` ends the head (SYN-4, OD16). In Core the counted loop
desugars to the one `while` form, so proofs keep a single loop construct, and the counted form
carries its termination measure, `b - i`. Why: a counted loop terminates without a proof and gives
a bounds check over its index something to eliminate; the C-style step had its own control flow,
which bugs 306 and 330 got wrong; and the old header was an LL(1) conflict. Austral and Ada have a
counted loop (inclusive), and Swift removed the C-style loop (SE-0007); `until` is half-open to avoid
off-by-one errors and because `to` reads as exclusive to some and inclusive to others. The current compiler still has
only the C-style forms (bug 331 implements the decision and migrates the corpus under exit 2q). The
two-untyped-literal bounds are refused (OD48), the termination measure is mathematical (OD49), and reverse, stepped and collection iteration remain absent (OD51).

The reviews of 2026-10-29 (Codex, Kimi and Grok) fixed the details this version states: the bounds
are evaluated once, left to right, before the first iteration, so a side effect in `b` happens
once; the comparison comes before the increment, so a loop whose `b` is its type's maximum never
increments past it and cannot overflow; `continue` runs the body's exit sequence (STMT-11) and then
advances, a labeled `continue` advancing only the loop it names, while `break` and `return` leave
without advancing; and the index cannot be assigned and is not visible after the loop. STMT-12
gives the counted loop its termination measure. The examples of this specification that use the C-style form migrate with the
corpus.

```con reject name=break-outside-loop-decided code=E0280 stage=check pair=labeled-break pending=R-0555 now=reject:E0280:check
fn main() -> u8 {
    break;
    return 0;
}
```

```con accept name=labeled-break exit=42
fn example_value() -> i64 {
    let mut out: i64 = 0;
    let mut i: i64 = 0;
    'outer: while i < 10 {
        i = i + 1;
        let mut j: i64 = 0;
        while j < 10 {
            j = j + 1;
            if j == 3 {
                continue 'outer;
            }
            if i == 8 {
                break 'outer;
            }
            out = out + 3;
        }
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

```con reject name=break-with-value code=new stage=parse pair=break-without-value pending=bug-323 now=accept:1
fn example_value() -> i64 {
    let mut i: i64 = 0;
    while i < 3 {
        i = i + 1;
        break 42;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 1 {
        return 1;
    }
    return 2;
}
```

```con accept name=break-without-value exit=1
fn example_value() -> i64 {
    let mut i: i64 = 0;
    while i < 3 {
        i = i + 1;
        break;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 1 {
        return 1;
    }
    return 2;
}
```

```con reject name=c-style-for code=new stage=parse pair=counted-loop pending=bug-331 now=accept:42
fn example_value() -> i64 {
    let mut s: i64 = 0;
    for (let mut i: i64 = 0; i < 7; i = i + 1) {
        s = s + i;
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

```con reject name=for-condition-only code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn example_value() -> i64 {
    let mut i: i64 = 0;
    for (i < 42) {
        i = i + 1;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=unconditional-for code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn example_value() -> i64 {
    let mut i: i64 = 0;
    for {
        i = i + 1;
        if i == 42 { break; }
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=while-loop exit=42
fn example_value() -> i64 {
    let mut i: i64 = 0;
    while i < 42 {
        i = i + 1;
    }
    return i;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 7;
    let mut s: i64 = 0;
    for i from lo until hi {
        s = s + i;
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

```con accept name=counted-loop-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 5;
    let hi: i64 = 5;
    let mut s: i64 = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-bounds-once exit=42 pending=bug-331 now=reject:E0001:parse
fn bump(c: &mut i64) -> i64 {
    *c = *c + 1;
    return 3;
}

fn example_value() -> i64 {
    let mut calls: i64 = 0;
    let lo: i64 = 0;
    let mut s: i64 = 0;
    for i from lo until bump(&mut calls) {
        s = s + 10;
    }
    return s + calls + 11;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

```con accept name=counted-loop-at-type-max exit=5 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: u8 = 250;
    let hi: u8 = 255;
    let mut n: i64 = 0;
    for i from lo until hi {
        n = n + 1;
    }
    return n;
}
fn main() -> u8 {
    if example_value() == 5 {
        return 5;
    }
    return 1;
}
```

```con accept name=counted-loop-reversed-bounds-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 9;
    let hi: i64 = 3;
    let mut s: i64 = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-continue-and-break exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 100;
    let mut s: i64 = 0;
    for i from lo until hi {
        if i == 2 {
            continue;
        }
        if i == 10 {
            break;
        }
        s = s + i;
    }
    return s - 1;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con accept name=counted-loop-labeled-continue exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let zero: i64 = 0;
    let three: i64 = 3;
    let mut s: i64 = 0;
    'outer: for i from zero until three {
        for j from zero until three {
            if j == 1 {
                continue 'outer;
            }
            s = s + 14;
        }
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 42 {
        return 42;
    }
    return 1;
}
```

```con reject name=counted-loop-index-is-immutable code=new stage=check pair=counted-loop pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 3;
    let mut s: i64 = 0;
    for i from lo until hi {
        i = 5;
        s = s + i;
    }
    return s;
}
fn main() -> u8 {
    if example_value() == 0 {
        return 0;
    }
    return 1;
}
```

<a id="STMT-7"></a>
### STMT-7. `main` declares target grants and returns u8

**STMT-7@4.** The entry point is one module-level `main` returning only `u8`, whose parameters declare a subset of the target grants and whose `with(...)` declares permitted effects, every declared grant being explicitly consumed before normal return and the returned `u8` being the process exit status.

- Kind: refusal
- Uses: none
- Codes: E0310 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the exit status is the `exit` behaviour of STMT-1 (R-0496)
- Evidence: tested

**Rationale.** The main-result decision fixes `u8` only. Authority rule 12 and the audit permit only declared target grants and require their explicit consumption; undeclared grants are never minted. Target grant schemas and lifecycle APIs are [undecided: see the target Core proposal](decisions.md#core-grants).

```con reject name=main-with-parameter-decided code=E0310 stage=check pair=main-exit-status pending=R-0555 now=reject:E0310:check
fn main(argc: i64) -> u8 {
    return 0;
}
```

```con accept name=main-exit-status exit=7
fn main() -> u8 {
    return 7;
}
```

```con reject name=main-returns-int code=new stage=check pair=main-returns-u8 pending=bug-320 now=accept:42
fn main() -> i64 {
    if 42 == 0 { return 0; } return 1;
}
```

```con accept name=main-returns-u8 exit=42
fn main() -> u8 {
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

The parser records the distinction on the statement itself (`Stmt.expr` carries `isValue`, true
only for a value block's trailing expression with no `;`, set in `parseExprBlock` and for a bare
`=> expr` arm), and every stage computes a block's type and value the same way: a trailing value
expression gives its type and value, a final `return` or `break` diverges, and anything else is
`Unit`. Because every construction site must set the flag, no site can miss it, and Lower's
`lastExprVal` reads the value only from a value expression, which removed a class of `store void`
and wrong-slot hazards (ROADMAP #36, 2026-06-20; `regress_stmt_match_arm_unit.con`). The formatter
prints a value expression without `;` and a statement with one.

```con accept name=trailing-values exit=42
fn pick(c: bool, k: i64) -> i64 {
    let v: i64 = if c {
        let t: i64 = k * 2;
        match k {
            0 => 1,
            _ => t
        }
    } else {
        0
    };
    return v;
}

fn example_value() -> i64 {
    return pick(true, 21);
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-9"></a>
### STMT-9. A function with a result type returns on every path

**STMT-9@1.** A function whose result type is not `Unit` cannot reach the end of its body: every path ends in a `return`, an abort or a loop that never exits.

- Kind: refusal
- Uses: none
- Codes: E0307 (check)
- Status: enforced
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: every reachable exit returns a value of the result type (R-0499)
- Evidence: tested

**Rationale.** Function bodies have no trailing value expression (STMT-8), so a reachable end has
no value to return. Until bug 194 nothing checked this: the backend failed in `llvm-as` (a `ret
void` in a non-void function) while `--interp` ran the program. Check now refuses it with E0307,
using a reachability analysis over every statement of a block.

```con reject name=one-branch-returns code=E0307 stage=check pair=every-path-returns
fn f(c: bool) -> i64 {
    if c {
        return 1;
    }
}

fn example_value() -> i64 {
    return f(true) + 41;
}

fn main() -> u8 {

    if example_value() == 0 {

        return 0;

    }

    return 1;

}
```

```con accept name=every-path-returns exit=42
fn f(c: bool) -> i64 {
    if c {
        return 1;
    }
    return 0;
}

fn example_value() -> i64 {
    return f(true) + 41;
}

fn main() -> u8 {

    if example_value() == 42 {

        return 42;

    }

    return 1;

}
```

<a id="STMT-10"></a>
### STMT-10. A test function is an entry point

**STMT-10@2.** A test function marked `#[test]` returns only `u8`, with zero denoting success and a nonzero result denoting failure.

- Kind: refusal
- Uses: none
- Codes: E0319 (check)
- Status: pending R-0555
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** Revised OD146 follows `main`: tests return only `u8`.

```con reject name=test-with-parameter-decided code=E0319 stage=check pair=test-shape-decided pending=R-0555 now=reject:E0319:check
#[test]
fn t(x: u8) -> u8 {
    return x;
}

fn main() -> u8 {
    return 42;
}
```

```con accept name=test-shape-decided exit=42 pending=R-0555 now=reject:E0319:check
#[test]
fn t() -> u8 {
    return 0;
}

fn main() -> u8 {
    return 42;
}
```

<a id="STMT-11"></a>
### STMT-11. One exit sequence

**STMT-11@3.** Every non-abort scope exit first evaluates its exit value, then runs deferred calls in reverse registration order from innermost scope outward, then ends the loans and tears down local storage without invoking an implicit `Destroy`, while a trapping deferred call stops the sequence.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: STMT-1 (R-0496)
- Evidence: tested

**Rationale.** OD2 fixes the exit order, OD68 stops cleanup after a trapping deferred call, and OD69 makes the final step storage teardown rather than an implicit Destroy; all linear obligations were explicitly discharged already.

<a id="STMT-12"></a>
### STMT-12. A counted loop carries its own measure

**STMT-12@2.** In Core a counted loop is the one `while` form, carrying the proof-only mathematical-integer termination measure `b - i` that the loop gives itself, refusing an explicit `#[variant]` and checking its invariant before the loop and after each iteration.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Semantics: none
- Obligation: the counted loop's measure decreases on every iteration (R-0496)
- Evidence: tested

**Rationale.** Decided with OD47 on 2026-10-29: proofs keep a single loop construct. The measure
proves only that the number of iterations is finite; the loop terminates when its bounds, its body
and its cleanup do, and the counted loop's proof rule (zero iterations, `break`, a labelled
`continue`) is stated over this desugared loop. OD104 refuses an explicit variant; OD105 checks invariants before the loop and after every iteration. Removing a bounds check needs an in-range certificate at the access (the bounds against the array's length), not only a length that cannot change. OD49 makes the measure a mathematical integer in the proof only, never computed at runtime.

```con accept name=counted-loop-own-measure exit=42 pending=bug-331 now=reject:E0001:parse
fn example_value() -> i64 {
    let lo: i64 = 0;
    let hi: i64 = 7;
    let mut s: i64 = 0;
    for i from lo until hi {
        s = s + i;
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

<a id="STMT-13"></a>
### STMT-13. `abort()` needs `Process`

**STMT-13@2.** The builtin `abort()` and checked traps need neither a resource value nor a permission, while target diagnostics and output flushing are recorded in traces and target assumptions.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0555
- Enforced by: Concrete.IntrinsicId.capability
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The 2026-11-02 audit clarifies that pure computation and terminal traps are outside the resource-operation requirement; target-side observable effects remain accounted for.

```con accept name=abort-without-permission exit=42 pending=R-0555 now=reject:E0520:core-check
fn main() -> u8 {
    if 1 == 2 {
        abort();
    }
    return 42;
}
```

## Recoverable failure: explicit `Result` flow

This section is explanatory; it records a design decision and the shape of programs under it. A
recoverable failure is a `Result<T, E>` value. There are no exceptions, no unwinding, no implicit
error conversion and no propagation operator: every propagation edge is an exhaustive `match` whose
error arm contains the `return` that leaves the function. A fatal failure is an abort (STMT-2).

**Postfix `?` was removed (2026-09-08).** It compressed a match and an early return into
punctuation at an expression site, which saved characters but made a reviewer reconstruct which
call can exit the function, which error type leaves it, whether a conversion happens, which
deferred cleanup runs and whether a refactor moved or duplicated a linear value. For an
audit-first language that trade is not worth it: generated explicit arms are cheap, review is the
scarce resource. The lexer still recognizes `?`, only so the parser can refuse it with a migration
diagnostic, and the retired diagnostics stay reserved.

Same-error propagation:

```con pseudocode
fn parse_header(data: [i32; 8]) -> Result<Header, ParseError> {
    let version: i32 = match validate_version(data[0]) {
        Result::Ok { value } => value,
        Result::Err { error } => {
            return Result::<Header, ParseError>::Err { error: error };
        },
    };
    return Result::<Header, ParseError>::Ok { value: Header { version: version } };
}
```

Cross-error propagation converts visibly in the error arm
(`return Result::<Response, ServiceError>::Err { error: to_service_validation(error) };`). `map_err`
is an ordinary named function: it creates no hidden return, and its caller still matches the
mapped value.

An error arm's `return` is a normal return: the deferred calls of the scopes it leaves run in the
same order as for any return (OWN-12), while an abort runs none. Calling a fallible function uses
the capabilities it declares; propagating its result needs no capability, allocation or
conversion. A `Result` obeys the ordinary conservation rules: the match consumes it, the selected
payload becomes a new binding, neither arm may drop the other's linear payload, and returning a
payload moves it once; the checker and Core have no propagation case.

The library's helper floor is ordinary functions, not control flow: `is_ok`, `is_err` (borrowed
receiver), `unwrap_or_else`, `map`, `map_err`, `and_then` (explicit function pointers, since there
are no closures, implicit trait resolution or `From` conversion), and `unwrap_or`, `ok`, `err`
(Copy bounds where the other arm is discarded) on `Result`; `is_some`, `is_none`,
`unwrap_or_else`, `map`, `and_then` on `Option`. A helper that returns a `Result` never propagates it
out of its caller. Parsers keep a flat named error enum, with each failure next to its check;
services convert stage errors in a visible arm at the stage boundary. A long run of similar arms is
not evidence that syntax is needed: first prefer a narrower named validation function, a combinator
where it improves the data flow, one match around a multi-step function, or reviewed source
generated outside the language.

Rejected: postfix `?` (permanently), implicit error conversion (hidden conversion lookup),
exceptions and unwinding (excluded by STMT-2), try/catch blocks (a second failure-control model),
result-specific statement syntax (duplicates `match`), closure-taking combinators (closures are
excluded, TYPE-1). These hold only while: `?` is refused with its removal message; neither the
surface AST nor Core has a propagation node; no stage (Check, Elab, Mono, lowering, the
interpreter, proof extraction, evidence identity) has a propagation path; same-type and
cross-type explicit propagation compile and run; explicit error returns run `defer` cleanup in
nested scopes; ignored fallible results stay refused (OWN-6); and the documentation, the book, the
site, tests and examples show the exhaustive match and the explicit return.

## Abort and failure

STMT-2 distinguishes ordinary error returns from terminal traps. There is no exception unwinding.

| situation | response | deferred calls run? |
|---|---|---|
| normal return or explicit error return | ordinary control flow, preserving every ownership obligation | yes |
| break, continue or end of a block | exit the scopes left | yes |
| program allocation failure | ordinary conserving error; allocator valid and inputs returned | on subsequent structured exit |
| checked trap or explicit abort | terminal, no cleanup | no |
| stack, runtime or environment exhaustion | separately stated target premises | no language cleanup promise |

Every non-abort exit follows STMT-11: evaluate the exit value, run deferred calls innermost first
and last registered first, end loans, then tear down storage without implicit Destroy (OD69).
A trapping deferred call stops the sequence (OD68). A write through `&mut` already changes its
place, so there is no write-back step (REF-13). `abort()` needs no permission (STMT-13).
The legacy runtime's null-allocation abort is a disagreement with the Allocation decision, not
an alternative language rule or a workload-gated recovery feature.

**Traps in the current compiler.** These are non-normative observations of the run-time checks that
ended a program, on the native binary and under `--interp`, on 2026-10-31. In each probe the operands
reached the operation through a function parameter, so the compiler could not evaluate them. Every
native run below exited with status 134 (SIGABRT on Linux), which agrees with the paragraph above:
the trap helpers call libc `abort()`. The interpreter stops with a run-time error whose first line is
shown, without its `interp: ` prefix; the `trap=` attribute of an abort example (§2.2 of
[MAINTAINERS.md](MAINTAINERS.md)) is matched against that line.

| operation probed | operands | related rule | native | `--interp` stops with |
|---|---|---|---|---|
| `+` on `i32` | 2147483647 and 1 | ARITH-3 | status 134 | `arithmetic overflow (checked +)` |
| `*` on `u8` | 16 and 16 | ARITH-3 | status 134 | `arithmetic overflow (checked *)` |
| `-` on `i32` | 0 and the minimum, -2147483648 | ARITH-3 | status 134 | `arithmetic overflow (checked -)` |
| `/` on `i32` | 7 and 0 | ARITH-3 | status 134 | `division by zero` |
| `/` on `i32` | -2147483648 and -1 | ARITH-3 | status 134 | `arithmetic overflow (checked /)` |
| `%` on `i32` | 7 and 0 | ARITH-3 | status 134 | `modulo by zero` |
| `<<` on `i32` | 1 and -1 | ARITH-3 | status 134 | `shift amount out of range` |
| `<<` on `u8` | 1 and 8 | ARITH-3 | status 134 | `shift amount out of range` |
| `f64` `as i32` | NaN (0.0 / 0.0) | ARITH-5 | status 134 | `float-to-integer conversion of an infinity or NaN (the binary aborts)` |
| `f64` `as i32` | 100000000000.0 | ARITH-5 | status 134 | `float-to-integer conversion out of range for 'i32' (the binary aborts)` |
| `a[i]` on `[u8; 3]` | `i` = 5 | MEM-6 | status 134 | `array index 5 out of bounds (size 3)` |
| `abort()` in a `main` declared `with(Process)` | none | STMT-2 | status 134 | `undefined function 'abort'` |

The recorded answers in [OD97](decisions.md#od97) govern this subject; the frozen compiler’s earlier behavior is not an alternative rule.

**Recoverable failures** use values and exhaustive matching. Allocation failure conserves its
input, failed growth preserves the container and returns the rejected element, and release retains
every surviving obligation. Exact failure payloads are
[undecided: see the target Core proposal](decisions.md#core-allocation-contracts).

**Hardware traps** are outside the language model: a null dereference (only in trusted code) and a
stack overflow raise SIGSEGV, and an illegal instruction (SIGILL) should not come from correct code
generation. Checked arithmetic and safe indexing are not among them: the compiler inserts a
language-defined abort before an invalid operation. **Undefined behaviour** needs trusted or
`Unsafe` code or a dishonest foreign boundary (TRUST-2): a trusted unchecked pointer or index access
can read or write arbitrary memory, a dishonest `extern` signature can corrupt the ABI or memory, and
an invalid raw-pointer lifetime or alias causes use after free; the mitigation is a named, audited
boundary.

On every normal path (including explicit error returns) all deferred calls run, every linear value
is consumed (OWN-2), each scope's loans end and its local storage is torn down without implicit Destroy in STMT-11's order, and the frame is reclaimed; predictable
code has no heap, so the deferred call is its only cleanup. On an abort, the resources of the
aborting function and of its callers get no language cleanup.

At the foreign boundary, a linear value passed by value to an `extern fn` is consumed, a reference
argument is borrowed for the call, and a return value has its declared type, trusted and not
verified. Nothing checks that a foreign function follows its declared contract, nothing cleans up
when it calls `longjmp` or `exit` or raises a signal, raw pointers it returns are not tracked, and it
is not assumed deterministic or pure. If it aborts or raises a signal, no deferred call runs and the
linear values in scope are leaked to the OS; `longjmp` across Concrete frames is undefined (no
setjmp-safe code is emitted). A proved function calls no `extern fn` (the proof-eligibility FFI
gate), so foreign failure is outside the proof model; what proved code may and may not assume about
failure is in [PROOF_SEMANTICS_BOUNDARY.md](../verification/PROOF_SEMANTICS_BOUNDARY.md).

## Program execution

This section is explanatory and states no rule. It follows a program from its source files to a
running process and names, for each step, the entries that govern it; what those entries say is in
the entries, not here. Where this section reports what the current compiler does, it says so and
names the probe. The runtime and platform side (the generated C entry, libc, targets) is described
in [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md).

| step | governing entries and what this section records | entries |
|---|---|---|
| source | MOD-4 covers how `mod` declarations load module files, SYN-5 the encoding of source text and SYN-1 the grammar a program is a sentence of. | MOD-4, SYN-5, SYN-1 |
| names | MOD-1 and MOD-3 cover name resolution and the names a module binds. The [builtin catalog](types.md#builtin-functions) lists the current compiler's builtin functions, with the sources and probes of its observations; every probe there calls builtins with no import, and this section records no other probe of name resolution. | MOD-1, MOD-3 |
| compilation | [pipeline.md](pipeline.md) describes the stages: Parse, Resolve, Check, Elab, CoreCheck, Mono, Lower, Emit and link. The entries for the program after a stage are PASS-5 (Check), PASS-6 (Elab), PASS-7 (Mono), PASS-8 (CoreCheck), PASS-9 (Lower) and PASS-10 (Emit); PASS-1 and PASS-2 cover specialization and PASS-11 the backend IR. PASS-12 (partial) covers the compiling of a program every rule accepts. The current compiler can fail after Check on a checked program: the builtin catalog records such failures for `unwrap`, `abort` and `get_args` (bugs 345 to 347). | pipeline.md, PASS-1, PASS-2, PASS-5 to PASS-11, PASS-12 (partial) |
| entry point | STMT-7 (partial) covers `main` and its exit status, and PASS-10 an executable's entry point. | STMT-7 (partial), PASS-10 |
| start | TRUST-8 (pending) covers module-level state and TYPE-22 the evaluation of constants. [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md) describes the generated C entry. This section records no probe of start-up, of the generated C entry or of constant evaluation. | TRUST-8 (pending), TYPE-22 |
| authority | TRUST-4 covers the capabilities of `main` on the same terms as every other function. TRUST-9 and TRUST-10 are pending, and authority rule 12 governs the target grants `main` declares and consumes; no Root type exists. STMT-7 (partial) states the entry-point signature. Every probe below uses a `main` with no parameters; this section records no probe of a `main` that declares parameters. | TRUST-4, TRUST-9 (pending), TRUST-10 (pending), OPEN-38, STMT-7 (partial) |
| running | STMT-3 and STMT-4 cover threads and evaluation order, and STMT-1 (pending) what a behaviour is. | STMT-3, STMT-4, STMT-1 (pending) |
| normal exit | STMT-11 (pending) covers the exit sequence of a scope, and STMT-7 (partial) the exit status. The paragraph below records the status the current compiler produces; the order of the exit sequence was not probed for this section. | STMT-11 (pending), STMT-7 (partial) |
| failure | STMT-2 (pending) covers failure, and STMT-13 `abort()`. "Abort and failure" above records the current compiler's traps and abort paths, and the table below what `--interp` does instead. | STMT-2 (pending), STMT-13 |
| tests | STMT-10 covers `#[test]` functions under `concrete test`. This section records no probe of `concrete test`. | STMT-10 |

**The exit status today.** STMT-7 is partial (bug 320): its pending example records that the
current compiler still accepts result types other than those STMT-7 names. Probes of the native
binary on Linux x86-64, at f17645a13 on 2026-10-31 and again at 904c7cc87: a `main` declared
`-> u8` that returns 7 exits with status 7; a `main` with no result type exits with status 0; a
`main` declared `-> i64` that returns 300 exits with status 44; a `main` declared `-> bool` that
returns `true` exits with status 1.

**What `--interp` does differently today.** PASS-13 is the entry for what every consumer gives a
checked program. The rows below are probes of single-file programs, run on 2026-10-31 at f17645a13
and again at 904c7cc87. "Status" is the process exit status as the shell
reported it on Linux x86-64, where a process ended by SIGABRT is reported as 134.

| situation | native binary | `--interp` |
|---|---|---|
| `main` returns a `u8` (7, after printing `hello`) | prints `hello`, status 7 | prints `hello`, then `7` as the last line, status 0; STMT-7 states that the exit status is that `u8`, with nothing written by the runtime, and PASS-13 that every consumer gives the same result, output and traps; no bug record covers the `u8` case, and bug 320 records the same printing for an `i64` result and names `Interp.interpret` as its site |
| `main` returns `i64` 300 | status 44 (bug 320) | prints `300`, status 0 (bug 320) |
| `main` returns `bool` `true` | status 1 (bug 320) | prints nothing (as bug 320's record reports for a `bool` result), status 0 |
| a division by zero at `i32`: `main` calls `fn d(a: i32, b: i32) -> i32 { return a / b; }` as `d(7, 0)` | ended by SIGABRT, status 134 | prints `interp: division by zero`, status 1 |
| `abort();` as a statement in a `main` declared `with(Process)` | ended by SIGABRT, status 134 | prints `interp: undefined function 'abort'`, status 1 (bug 344) |
| `clock_monotonic_ns()` in a `main` declared `with(Time)` | runs, status 42 | prints `interp: clock_monotonic_ns reads the system clock, a value the interpreter cannot know; run the compiled binary`, status 1 |
| `float_to_string(1.5)` | runs, status 42 | prints `interp: undefined function 'float_to_string'`, status 1 (bug 348) |

Whether `--interp` runs a program of several files or a project with dependencies was not probed
for this section. The traps the current compiler reports are listed under "Traps in the current
compiler" above.

## Version history

This appendix is not normative. It records what earlier versions of this chapter's entries said
or why they changed, so that each rationale explains the current version only. The registry,
[ids.tsv](ids.tsv), keeps every version's state and sentence hash.

- STMT-6@2 records OD27 (2026-10-29): `break <expr>` is refused at parse. STMT-6@3 records OD47 (2026-10-29): the loops are `while` and the counted loop, and the C-style `for` is removed.
- STMT-7@2 masked an integer result to 0–255 and mapped `bool` (stage 1 of MAIN_EXIT_MODEL). STMT-7@3 records OD24 (2026-10-29): `main` returns `u8` or `Unit`.
- STMT-11@1 (retired before it was enforced, as the registry records) said of the C-style `for` step, which OD47 removes, what STMT-11@2 says of the counted loop of OD47.

- STMT-2@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-2@1 is superseded by version.
- STMT-4@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-4@1 is superseded by version.
- STMT-5@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-5@1 is superseded by version.
- STMT-7@4 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-7@3 is superseded by version.
- STMT-10@2 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-10@1 is superseded by version.
- STMT-11@3 (2026-11-03) brings the chapter into line with the recorded owner decisions; STMT-11@2 is superseded by version.

- STMT-13@2 (2026-11-03) supersedes STMT-13@1 to state the recorded decision.

- STMT-6@4 (2026-11-03) supersedes STMT-6@3; pending legacy conformance is explicit.

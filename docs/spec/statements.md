# Statements, Expressions and Execution

Statements execute in source order. A block's trailing expression without `;` is its value, and
`e;` is a statement whose value is discarded (OWN-5 to OWN-7 say which values may be). This
chapter covers how executions end (STMT-1 to STMT-3), evaluation order (STMT-4), where exits may
appear (STMT-5), loops (STMT-6), `main` (STMT-7), value blocks (STMT-8) and returning a value on
every path (STMT-9). The design records are
[STATEMENT_EXPRESSION_MODEL.md](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md)
and [MAIN_EXIT_MODEL.md](../platform/MAIN_EXIT_MODEL.md). [EXECUTION_MODEL.md](../platform/EXECUTION_MODEL.md)
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
("Abort and failure" below; TIGHTENING_DECISIONS §4.5, T-ARITH).
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
- Status: enforced
- Enforced by: Concrete.Pipeline.lower
- Semantics: none
- Obligation: V1a's evaluation-order obligation; the verified pipeline may not model a looser order than the compiler implements (R-0500, R-0496)
- Evidence: tested

**Rationale.** A fixed order makes effects predictable and lets every consumer agree. A trap ends
evaluation: no later operand is evaluated. Struct literals follow source order since bug 127.
Three silent violations were fixed on 2026-10-23 (landing train 6), each with its own record: an
enum literal evaluated its fields in declaration order (bug 137), a functional update read its
base once per copied field (bug 136), and an assignment through a dereference evaluated its
destination first natively (bug 183).

Statements run in source order, and an operation is performed only after all its operands are
evaluated, each completely (its calls, effects and traps included). Struct and enum literal fields
follow the order they are written, not the order the type declares (`P { b: f(), a: g() }` calls
`f` before `g`); a functional update evaluates its explicit initializers, then its base once, and
takes every omitted field from that one value. In an assignment the right-hand side comes first,
then the destination's subexpressions left to right (`arr[i()] = j()` and `*(&mut a[i()]) = j()`
both call `j` before `i`; Rust's order for simple assignment), and there are no compound assignment
operators. An arithmetic trap is checked after both operands are evaluated (ARITH-3). The rule was
decided 2026-10-21 (exit 2j, R-0500); it discharges V1a's evaluation-order obligation, since the
verified pipeline may not model a looser order than the compiler implements, and the permanent
differential coverage of literal-field order, functional update and assignment order is exit 2c's
(R-0491). The per-rule status tables of that decision are kept in the archived design record
([STATEMENT_EXPRESSION_MODEL.md](../archive/design-records/STATEMENT_EXPRESSION_MODEL.md)).

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

```con accept name=enum-literal-source-order exit=21
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

**STMT-6@3.** The loops are `while cond { ... }` and the counted loop `for i from a until b { ... }`, both statements, where the counted loop evaluates `a` and then `b` once, both of one integer type, which the index `i` takes, before its first iteration, compares `i` with `b` before each iteration and advances it only after a comparison that succeeded, so it never advances `i` past `b`, and runs its body with `i`, immutable and scoped to the body, bound to each value from `a` up to but not including `b`, none when `a >= b`; `break` carries no value, and `break` and `continue` act on the innermost enclosing loop or, with a label, on the labeled loop and appear only inside a loop, `continue` running the body's exit sequence and then advancing only the loop it names, while `break` and `return` advance nothing.

- Kind: refusal
- Uses: none
- Codes: E0216 (check), E0245 (check), E0280 (check), E0281 (check), E0542 (core-check), E0543 (core-check)
- Status: partial bug-331
- Enforced by: Concrete.checkStmt, Concrete.ccCheckStmt
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** There is no bare `loop`; `while true` is the infinite loop. A loop produces no
value (the value `while … else` form was removed in Phase 6D); a result is a mutable local
assigned before `break`. In main's `for (init; cond; step)` loop, `continue` still runs `step`, and
E0245 refuses a `break` or `continue` in the step that targets its own loop (bug 330); E0245 belongs to
this entry only while main parses the step form, which version 3 removes, and retires with it. A
`break` or `continue` leaves the loop body's scopes like any exit (OWN-2).

Main still has the earlier loop forms, which version 3 removes (bug 331): `while cond { ... }`,
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

Version 2 records the owner's decision of 2026-10-29 (OD27): `break <expr>` is refused at parse.
Main still parses a value after `break` inside a `while` or `for` statement and drops it (bug 323).


Version 3 records the owner's decision of 2026-10-29 (OD47): the loops are `while` and a counted
range loop, and the C-style `for` is removed in every form (`for (init; cond; step)`,
`for (cond)` and the unconditional `for`). The counted loop is half-open, `for i from a until b`
running `a`, `a + 1`, ..., `b - 1` and nothing when `a >= b`; its index cannot be assigned in the
body, and its scope is the body; both bounds must have one integer type, which the index takes.
Whether `from` and `until` are reserved words or contextual keywords is OPEN-50. The grammar is LL(1): `for` IDENT `from` expr `until` expr block, the upper bound
being a restricted head expression so that `{` ends the head (SYN-4, OD16). In Core the counted loop
desugars to the one `while` form, so proofs keep a single loop construct, and the counted form
carries its termination measure, `b - i`. Why: a counted loop terminates without a proof and gives
a bounds check over its index something to eliminate; the C-style step had its own control flow,
which bugs 306 and 330 got wrong; and the old header was an LL(1) conflict. Austral and Ada have a
counted loop (inclusive), and Swift removed the C-style loop (SE-0007); `until` is half-open to avoid
off-by-one errors and because `to` reads as exclusive to some and inclusive to others. Main still has
only the C-style forms (bug 331 implements the decision and migrates the corpus under exit 2q). The
index type when both bounds are untyped literals is OPEN-48, the arithmetic of the termination
measure is OPEN-49, and whether reverse, stepped or collection iteration stays out of the language
is OPEN-51.

The reviews of 2026-10-29 (Codex, Kimi and Grok) fixed the details this version states: the bounds
are evaluated once, left to right, before the first iteration, so a side effect in `b` happens
once; the comparison comes before the increment, so a loop whose `b` is its type's maximum never
increments past it and cannot overflow; `continue` runs the body's exit sequence (STMT-11) and then
advances, a labeled `continue` advancing only the loop it names, while `break` and `return` leave
without advancing; and the index cannot be assigned and is not visible after the loop. STMT-12
gives the counted loop its termination measure. The examples of this specification that use the C-style form migrate with the
corpus.

```con reject name=break-outside-loop code=E0280 stage=check pair=labeled-break
fn main() -> Int {
    break;
    return 0;
}
```

```con accept name=labeled-break exit=42
fn main() -> Int {
    let mut out: Int = 0;
    let mut i: Int = 0;
    'outer: while i < 10 {
        i = i + 1;
        let mut j: Int = 0;
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
```

```con reject name=break-with-value code=new stage=parse pair=break-without-value pending=bug-323 now=accept:1
fn main() -> Int {
    let mut i: Int = 0;
    while i < 3 {
        i = i + 1;
        break 42;
    }
    return i;
}
```

```con accept name=break-without-value exit=1
fn main() -> Int {
    let mut i: Int = 0;
    while i < 3 {
        i = i + 1;
        break;
    }
    return i;
}
```

```con reject name=c-style-for code=new stage=parse pair=counted-loop pending=bug-331 now=accept:42
fn main() -> Int {
    let mut s: Int = 0;
    for (let mut i: Int = 0; i < 7; i = i + 1) {
        s = s + i;
    }
    return s + 21;
}
```

```con reject name=for-condition-only code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn main() -> Int {
    let mut i: Int = 0;
    for (i < 42) {
        i = i + 1;
    }
    return i;
}
```

```con reject name=unconditional-for code=new stage=parse pair=while-loop pending=bug-331 now=accept:42
fn main() -> Int {
    let mut i: Int = 0;
    for {
        i = i + 1;
        if i == 42 { break; }
    }
    return i;
}
```

```con accept name=while-loop exit=42
fn main() -> Int {
    let mut i: Int = 0;
    while i < 42 {
        i = i + 1;
    }
    return i;
}
```

```con accept name=counted-loop exit=42 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: Int = 0;
    let hi: Int = 7;
    let mut s: Int = 0;
    for i from lo until hi {
        s = s + i;
    }
    return s + 21;
}
```

```con accept name=counted-loop-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: Int = 5;
    let hi: Int = 5;
    let mut s: Int = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
```

```con accept name=counted-loop-bounds-once exit=42 pending=bug-331 now=reject:E0001:parse
fn bump(c: &mut Int) -> Int {
    *c = *c + 1;
    return 3;
}

fn main() -> Int {
    let mut calls: Int = 0;
    let lo: Int = 0;
    let mut s: Int = 0;
    for i from lo until bump(&mut calls) {
        s = s + 10;
    }
    return s + calls + 11;
}
```

```con accept name=counted-loop-at-type-max exit=5 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: u8 = 250;
    let hi: u8 = 255;
    let mut n: Int = 0;
    for i from lo until hi {
        n = n + 1;
    }
    return n;
}
```

```con accept name=counted-loop-reversed-bounds-empty exit=42 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: Int = 9;
    let hi: Int = 3;
    let mut s: Int = 42;
    for i from lo until hi {
        s = 0;
    }
    return s;
}
```

```con accept name=counted-loop-continue-and-break exit=42 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: Int = 0;
    let hi: Int = 100;
    let mut s: Int = 0;
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
```

```con accept name=counted-loop-labeled-continue exit=42 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let zero: Int = 0;
    let three: Int = 3;
    let mut s: Int = 0;
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
```

```con reject name=counted-loop-index-is-immutable code=new stage=check pair=counted-loop pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: Int = 0;
    let hi: Int = 3;
    let mut s: Int = 0;
    for i from lo until hi {
        i = 5;
        s = s + i;
    }
    return s;
}
```

<a id="STMT-7"></a>
### STMT-7. `main` takes no parameters and its result is the exit status

**STMT-7@3.** The entry point is a module-level `fn main()` with no parameters whose result type is `u8` or `Unit`, and the process exit status is that `u8`, or 0 for `Unit`, with nothing written by the runtime.

- Kind: refusal
- Uses: none
- Codes: E0310 (check)
- Status: partial bug-320
- Enforced by: Concrete.checkModule
- Semantics: none
- Obligation: the exit status is the `exit` behaviour of STMT-1 (R-0496)
- Evidence: tested

**Rationale.** Command-line arguments come from `std.args` (`with(Env)`); a parameterized `main`
used to read an undefined register (bug 111). There is no `std.process.exit`: an early exit would
skip cleanup the linear rules promise, so status flows back through `main`. Version 2 masked an integer result to
0–255 and mapped `bool` (stage 1 of MAIN_EXIT_MODEL); OD24 narrows the result to `u8` or `Unit`, so
masking is unrepresentable. `--interp` prints the result instead of exiting with it.
The owner decided OD8 on 2026-10-29: `main` will receive the root resource value and split it
(TRUST-9); this entry changes with R-0557, which lands that parameter, and until then `main` takes
none.

Version 3 records the owner's decision of 2026-10-29 (OD24): `main` returns `u8` or `Unit`; there
is no masking, no `bool` mapping and no other result type. Main still accepts any result type, masks
an integer natively and prints it under `--interp` (bug 320); the examples of this specification
still declare `fn main() -> Int` and migrate with the rest of the corpus.


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

```con reject name=main-returns-int code=new stage=check pair=main-returns-u8 pending=bug-320 now=accept:42
fn main() -> Int {
    return 42;
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

<a id="STMT-10"></a>
### STMT-10. A test function is an entry point

**STMT-10@1.** A `#[test]` function takes no parameters, has no type parameters and returns `i32`, its exit status under `concrete test`.

- Kind: refusal
- Uses: none
- Codes: E0319 (check)
- Status: enforced
- Enforced by: Concrete.checkFn
- Semantics: none
- Obligation: none
- Evidence: tested

**Rationale.** The test runner calls each test as it calls `main` (STMT-7): with nothing to pass and
a status to read, so a test has the same shape.

```con reject name=test-with-parameter code=E0319 stage=check pair=test-shape
#[test]
fn t(x: i32) -> i32 {
    return x;
}

fn main() -> Int {
    return 42;
}
```

```con accept name=test-shape exit=42
#[test]
fn t() -> i32 {
    return 0;
}

fn main() -> Int {
    return 42;
}
```

<a id="STMT-11"></a>
### STMT-11. One exit sequence

**STMT-11@2.** Every exit of a scope that does not abort runs the same sequence: the exit value is computed, then the scope's deferred calls run, last registered first and innermost scope first, then the scope's loans end, and then its remaining locals are destroyed, so a counted loop advances its index only after its body scope's exit sequence.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending R-0496
- Semantics: none
- Obligation: STMT-1 (R-0496)
- Evidence: tested

**Rationale.** Decided by the owner on 2026-10-29 (OD2, part 5). One order on every normal exit
(falling off the end, `return`, `break`, `continue`) is what lets a deferred call see the exit value
already computed and the loans it was given still live (OWN-16), and lets the destruction of the
remaining locals come last. An abort runs none of it (STMT-2; OD10). A counted loop's body is its own scope and advancing the index belongs to the loop,
outside it, so the body's deferred calls, loan ends and destructions run, on a fall-through or a
`continue`, before the index advances. Version 2 restates for the counted loop of OD47 what version 1
said of the C-style `for` step, which OD47 removes. Bugs 305 and 306 brought the borrow block's exits and the interpreter's `for`
step into this order; the entry stays pending until
R-0496's Core semantics defines the sequence for every exit form.

<a id="STMT-12"></a>
### STMT-12. A counted loop carries its own measure

**STMT-12@1.** In Core a counted loop is the one `while` form, carrying the termination measure `b - i` that the loop gives itself.

- Kind: guarantee
- Uses: none
- Codes: none
- Status: pending bug-331
- Semantics: none
- Obligation: the counted loop's measure decreases on every iteration (R-0496)
- Evidence: tested

**Rationale.** Decided with OD47 on 2026-10-29: proofs keep a single loop construct. The measure
proves only that the number of iterations is finite; the loop terminates when its bounds, its body
and its cleanup do, and the counted loop's proof rule (zero iterations, `break`, a labelled
`continue`) is stated over this desugared loop. Whether a `#[variant]` written on a counted loop is
refused, checked or ignored is OPEN-92, and where its `#[invariant]` is checked is OPEN-93 (CON-2
checks a loop invariant before the loop and after every iteration). Removing a bounds check over
the index is an optimization, not a rule of the language: it needs an in-range certificate at the
access (the bounds against the array's length), not only a length that cannot change. How the
measure is represented when `b - a` does not fit the bounds' type is OPEN-49.

```con accept name=counted-loop-own-measure exit=42 pending=bug-331 now=reject:E0001:parse
fn main() -> Int {
    let lo: Int = 0;
    let hi: Int = 7;
    let mut s: Int = 0;
    for i from lo until hi {
        s = s + i;
    }
    return s + 21;
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

This section is explanatory: it states the failure model that STMT-1 and STMT-2 (pending, R-0496)
formalize. Failure is abort only: there is no panic, no stack unwinding, no catch and no exception
mechanism, permanently. Unwinding needs hidden control flow the predictable profile cannot reason
about and runtime support (personality functions, LSDA tables) that freestanding targets lack, makes
every call site a potential exit for proofs, and the model stays small enough to state on one page.

| situation | response | deferred calls run? |
|---|---|---|
| a normal `return`, or the end of the body | the function returns | yes, last registered first |
| an explicit error return (a `Result::Err` arm's `return`) | ordinary control flow, not unwinding | yes, in each returning frame |
| `break`, `continue`, the end of a block (an `if` branch, a loop body, a borrow block) | control flow | yes, for the scopes left |
| out of memory | terminal abort | no |
| stack overflow | the OS guard page, SIGSEGV | no |
| a hardware signal | the OS ends the process | no |
| `abort()` | terminal abort | no |

Every non-abort exit runs one sequence (STMT-11): the exit value, then the scope's deferred calls,
then its loans end, then the remaining locals are destroyed. There is no write-back step: a `&mut`
refers to the place it borrows, so a write through it is already the write to that place (REF-13).

**Terminal failures** abort with no cleanup after the trap. A trap inside cleanup is terminal too,
and a value is destroyed at most once, so a destructor never runs again after a trap interrupted it
(the trap helpers and `__concrete_check_oom` call libc `abort()`). The sources are: out of memory
(`malloc` or `realloc` returning null, through `__concrete_check_oom`); the `abort()` intrinsic
(user code, needing `Process`); precondition violations in `std.alloc`'s wrappers (needing `Alloc`);
and the compiler's checked operations: integer overflow, a zero divisor, an invalid shift count and
`MIN / -1` (ARITH-3), a float-to-integer conversion out of range (ARITH-5), and an index out of
bounds (MEM-6). The first three cannot be reached from predictable code (no allocation, no
`Process`); the checked operations can, unless statically discharged. Hosted execution calls libc
`abort()`, which usually raises SIGABRT (status 134 on POSIX); a freestanding profile calls a declared
non-returning handler and may not assume an OS that reclaims resources. No deferred call, `Destroy`
implementation or generated drop glue runs on an abort, and there is no implicit scope-end
destructor the compiler could have inserted (OWN-13); in a hosted program the OS reclaims the
process's memory and descriptors, which is an environmental fact, not a language guarantee.

**Recoverable failures** are values (`Result`, `Option`; the recoverable-failure section above and
the std conventions of [ERROR_CONVENTIONS.md](../stdlib/ERROR_CONVENTIONS.md)). Operations an
application can reasonably handle get a fallible API, and a consuming fallible API states what
happens to the resource on failure. Fallible allocation is the missing piece: std has no
`try_alloc`, `try_reserve` or `try_push`, and `__concrete_check_oom` aborts on null, so allocation
failure is terminal today; it is a pull-gated backlog item with no owner yet, pulled by a workload
that needs recovery (RocksDB slice 1 does not), with `research/stdlib/allocator-as-value.md` as design
background. Lossy numeric `as` conversions are to become compile errors, not traps (ARITH-5).

**Hardware traps** are outside the language model: a null dereference (only in trusted code) and a
stack overflow raise SIGSEGV, and an illegal instruction (SIGILL) should not come from correct code
generation. Checked arithmetic and safe indexing are not among them: the compiler inserts a
language-defined abort before an invalid operation. **Undefined behaviour** needs trusted or
`Unsafe` code or a dishonest foreign boundary (TRUST-2): a trusted unchecked pointer or index access
can read or write arbitrary memory, a dishonest `extern` signature can corrupt the ABI or memory, and
an invalid raw-pointer lifetime or alias causes use after free; the mitigation is a named, audited
boundary.

On every normal path (including explicit error returns) all deferred calls run, every linear value
is consumed (OWN-2), each scope's loans end and its remaining locals are destroyed in STMT-11's order, and the frame is reclaimed; predictable
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


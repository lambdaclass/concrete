# Bug 176 — a loop body's shadowing `let` outlived the loop in the checker

<!-- bug-ledger
summary: a `let` in a `while`/`for` body (or a `for` header) that shadowed an outer name stayed visible to the checker after the loop, so the outer name took the inner binding's type and mutability: valid code was refused (E0217, E0233), a `u8` binding held 300 natively and in `--interp`, and a checked program failed SSA verification (silent wrong code). Check's loops restored only their depth and labels; every block now closes through one helper (`closeBlockScope`/`inBlockScope`). Found by the fuzz farm (`refusal-eea6c54fe796`), 2026-10-22. Fixtures `regress_176_*`, `error_176_*`. Class 2; joins exit 0b.
fixtures: regress_176_loop_shadow_restores_outer.con error_176_while_shadow_type.con error_176_while_shadow_ssa.con error_176_while_shadow_mut.con error_176_for_header_type.con error_176_loop_body_linear_leak.con error_176_loop_body_leak_while_let.con error_176_loop_body_leak_labelled_break.con error_176_loop_body_leak_labelled_break_mid.con
class: 2 — a loop body's shadowing `let` outlived the loop in the checker) (and 068's binder half
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/bug176`)
**Found:** 2026-10-22 by the fuzz farm (differential generator, finding `refusal-eea6c54fe796`,
compiler `34f19e97`), reduced by hand.
**Severity:** silent wrong code (a `u8` binding holding 300), a valid program refused, and a
program the checker accepted that then failed SSA verification. The checker typed and
mutability-checked the rest of the function against a binding that had gone out of scope; Elab,
the interpreter and the binary used the right one.

## Observed

A `let` inside a `while` body that shadows an outer name stayed visible to the checker after the
loop, with the inner binding's type and mutability.

1. Valid code refused (the fuzz-farm shape; E0233 for `&mut v0` after the loop likewise):

   ```
   let mut v0: i32 = 0; let mut k: i32 = 0;
   while k < 2 { let v0: i32 = 7; k = k + 1; }
   v0 = 1;              // E0217 cannot assign to immutable variable 'v0'
   ```

2. Type confusion. Native and `--interp` both printed 300 from a `u8`:

   ```
   let mut v0: i32 = 300; let mut k: i32 = 0;
   while k < 1 { let mut v0: u8 = 7; v0 = v0 + 1; k = k + 1; }
   let y: u8 = v0;      // accepted; without the loop this is E0220
   println(y);          // 300
   ```

3. Adding `let z: u8 = y + 1;` to (2) passed the checker and failed SSA verification (E0715, i32
   vs u8 operands) while `--interp` printed 301.

The same held for every loop form, not only `while`: a C-style `for` body, the `for` header's
`let` (after `for (let mut i: i32 = 0; …)` an outer `i: u8` read as i32), `for (cond)` and the
infinite `for { }`. An immutable outer `v0` with a `let mut v0` in the body was the unsound
mirror of (1): `v0 = 1;` after the loop was accepted. A non-shadowing name declared in a loop did
not leak, because Resolve scopes loop bodies and refuses it with E0100.

## Expected

The loop body (and a `for` header) is a lexical block: after it closes, the outer binding is the
visible one, with its own type and mutability. (1) compiles and prints 1; (2) and (3) are E0220 at
`let y`, as without the loop.

## Structural cause

Check's binding environment is a list of `(name, VarInfo)` searched by name, so a block must drop
its own bindings when it closes or they keep answering for the name. Each block construct did this
by hand, in three different ways:

- `if`/`else` (statement and value form) and borrow blocks filtered the environment to the
  bindings present before the block (by uid);
- `if` without `else` and `match` arms restored or rebuilt from the pre-block snapshot;
- `while` and `for` restored only the fields they had changed, `loopDepth` and `loopLabels`.

So `if` and `match` were lexical and every loop was not. The `for` step was also checked with
the body's bindings still visible (`for (let mut i: i32 = 0; …; i = i + 1) { let i: u8 = 9; }`
checked the step against the u8). Elab, the interpreter and Lower were right: bug 087 made Elab
close every block (`enterBlock`/`exitBlock`) and alpha-rename shadowing binders, so the Core
program read the outer `v0`. Only the source checker disagreed. The type confusion reached the
binary because CoreCheck does not compare a `let`'s declared type with its value's type, so
nothing after Check caught `let y: u8 = <i32>`.

A second consequence of the leak: a loop body's linear locals were checked for consumption only at
function end, which a non-terminating function skips (`while true { let r = Res {…}; }` dropped
one value per iteration and was accepted).

## Fix

`Concrete/Check/CheckHelpers.lean` has one scope discipline for Check's environment:

- `closeBlockScope outer` drops every binding whose uid is not in `outer`. Outer bindings keep
  the states the block left them in.
- `inBlockScope body span k` runs `k` as a block, applies H9 (`checkBlockLocalsConsumed`, a linear
  local must be consumed before a terminating block ends), and closes the block.

`while` bodies, `for` headers and `for` bodies use `inBlockScope`. The `for` scopes nest as in
Elab: the header (init, condition, step) is scoped to the loop and the body is a block inside it,
so the step sees the header and never the body. `if`/`else` and value-`if` merges and borrow
blocks now close through `closeBlockScope` instead of their own filters. `if` without `else` and
`match` still restore from the snapshot, which is lexical and also discards the branch's effects
on outer bindings, as their merge rules require.

### Codex review, round 1

Closing loop bodies made H9 depend on the divergence predicate (`blockNonTerminating`) at every
loop body's end. Before, these locals reached the function-end check. The predicate's break
detection (`blockHasBreak`) saw only direct `break`s and `if` branches, so:

- a body ending in `while let`, whose `break` sits in the desugared `match`'s fallback arm, read
  as infinite, and its unconsumed locals were exempted and then dropped (a per-iteration leak
  that the function-end check used to refuse);
- a labelled `break 'outer` inside a loop nested in an inner `while true` was ignored the same
  way;
- `for (true) { }` was not recognized as infinite at all, so a body ending in it was falsely
  refused.

`blockHasBreak` now finds every exit from the loop. That covers an unlabelled `break` outside
nested loops, and a labelled `break`/`continue` whose label is not a nested loop's (a `continue`
to the loop's own label stays in it). It looks through `match` arms, value-`if` branches, borrow
blocks, `let`-`else` and statement expressions. `stmtDiverges` and `stmtNonTerminating` treat
`for (true)` like `while true`. Over-counting exits is the safe direction: it can only make a
block count as terminating. Fixtures: `error_176_loop_body_leak_while_let.con`,
`error_176_loop_body_leak_labelled_break.con` (E0208 with this fix alone; E0210 once merged with
safety-eval2, whose break-skips-linear rule refuses `break 'outer` first),
`error_176_loop_body_leak_labelled_break_mid.con` (E0208: the same nesting with a `break 'mid`
that stays in the scope of `r`), and `spin_holding` in the regress fixture (accepted).

Round 2 found that the one predicate answered two questions. H9 asks whether a body's scope can
close; any exit from the loop, including a labelled `break`/`continue` to an enclosing loop,
closes it. The if/match merge asks whether control can reach the statement after the loop; only a
`break` that targets this loop does that. Counting `continue 'outer` as a fall-through refused a
valid branch that consumed `r` and then left through it. `blockHasBreak` now takes `anyExit`:
`stmtNonTerminating` (H9) passes true and `stmtDiverges` (merges) false. The regress fixture's
`branch_exits_outer` covers this case.

**Known limit, not changed here.** When every arm of a `match` diverges, the checker continues
with the first arm's state. So a loop body ending in `match c { true => { abort(); }, false => {
consume(r); continue; } }` still reports `r` as unconsumed. Before this fix, the function-end
check refused the same body unless the function never terminates, so the only new refusal is in a
non-terminating function. The precise rule is class 15's owner (R-0499, ownership along
control-flow paths).

## Structural owner

[Structural class](STRUCTURAL_CLASSES.md) 2, binder identity by name, not a `LocalId` (with 087,
093, 094): the checker's environment finds bindings by spelled name, so scoping depends on every
construct remembering to remove its own. The class owner is R-0492 (S5 `LocalId`), with
R-0499/R-0514 (row 4) moving the checker's environments onto it. With `LocalId`s a reference
after the loop names the outer binder and cannot resolve to the inner one, whatever the
environment still holds. Until then `closeBlockScope` is the one place a block closes.

Follow-up for defence in depth: CoreCheck could refuse a Core `let` whose value type differs from
its declared type, which would have turned (2) into a refusal after Check.

## Containment

- The fix above closes the defect on main.
- The language rule against shadowing (E0301, LANGUAGE_INVARIANTS §4, R-0435, exit 2i) refuses
  the trigger itself: a `let` that shadows a visible name. That is defence in depth, not the fix.
  The checker must stay lexical for anything the rule admits (sibling scopes reusing a dead name,
  match binders), and the H9 leak (a linear body local never consumed) is not reached by the rule
  at all. Merged with exit 2i (train 1b), the four shadowing error fixtures are refused with
  E0301 first, and the regress fixture exercises the scoping through later siblings that reuse
  the body's names at another type or mutability.

## Regression tests

- `tests/programs/regress_176_loop_shadow_restores_outer.con` (`run_ok 42`, and `run_tests.sh`
  requires `--interp` to print 42 as well): a binding after the loop reusing a body's name at
  another type or mutability, after `while` (assignment, `&mut` borrow, use at its own type),
  `while let`, nested loops, labelled `break` and `continue`, C-style `for` (body and header),
  `for (cond)` and `for { }`. (Before exit 2i it shadowed a visible outer binding instead;
  previous compiler: E0217.)
- `tests/programs/error_176_while_shadow_type.con` (E0220 with the fix alone, E0301 since exit
  2i; previously printed 300),
  `error_176_while_shadow_ssa.con` (E0220, now E0301; previously E0715 compiled, 301 interpreted),
  `error_176_while_shadow_mut.con` (E0217, now E0301; previously accepted, assigning an
  immutable binding),
  `error_176_for_header_type.con` (E0220, now E0301; previously accepted),
  `error_176_loop_body_linear_leak.con` (E0208; previously accepted, a per-iteration leak in a
  non-terminating function).

The language has no `loop` keyword; `for { }` is the infinite loop, and `while let` desugars to
`while true` with a `match`.

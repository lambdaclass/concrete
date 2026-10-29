# Bug 285 — Lower's address-taken walk skipped enum literal payloads

<!-- bug-ledger
summary: the walk that tells Lower's branch and loop sites which locals have their address taken (`cexprAddrTaken`) had no case for an enum literal and fell to a wildcard, so `&x` inside an enum payload was invisible: a pre-branch local was promoted lazily inside the branch and the other path loaded a never-stored slot, and a loop variable was phi-carried and promoted mid-body (the loop never ended)
fixtures: regress_285_enum_payload_addr_taken.con
class: 8 — the address-taken walk had a wildcard case
silent: yes
exit: 0b
ruling: Violates: MEM-3@1
-->

**Status:** FIXED (branch `wip/bug-285`)
**Found:** 2026-10-26, designing the fix of the nested-branch lowering quadratic (R-0550 scaling
gate, `nested-ifs`): that fix relies on the walk seeing every address-take, and the walk's
wildcard case was read against `CExpr`'s constructors.
**Severity:** silent wrong code. The native binary reads an uninitialized stack slot, or never
terminates; `--interp` is right.

## Reproducer

```
enum E { A { v: i64 }, B }
fn get(p: &i64) -> i64 { return *p; }
fn pick(c: bool) -> i64 {
    let mut x: i64 = 40;
    let mut r: i64 = 0;
    if c {
        x = 7;
        let e: E = E::A { v: get(&x) };
        match e { E::A { v } => { r = v; }, E::B => { r = 1; }, }
    }
    return x + r;
}
fn main() -> Int { return (pick(false) + 2) as Int; }
```

Observed on main cc7a4084: the binary exits 9, `--interp` prints 42. `--emit-ssa` of `pick`: the
slot `%addr.2` is allocated in the entry block, stored only in the `then` block, and loaded after
the merge on both paths. The loop form
(`while i < 6 { s = s + payload(E::A { v: get(&i) }); i = i + 1; }`) compiles to a binary that never
terminates (killed at 5 s); `--interp` prints the sum.

`regress_285_enum_payload_addr_taken.con` exercises every site that asks the walk. On main's
compiler, each called alone: the statement `if` returns 70 (want 4), the value `if` 70 (want 3),
the `match` 71 (want 6), the `&&` right operand (a value `if` in it assigns `x` and takes its
address in a payload) 70 (want 6), and the loop does not terminate (want 15). With the fix all agree
with `--interp`.

## Expected

The binary computes what `--interp` computes: an address taken in code that does not run on a path
does not change the value a local holds on that path (STATEMENT_EXPRESSION_MODEL, evaluation
order; the interpreter is the reference).

## Cause

`IR/Lower.lean`, `cexprAddrTaken`: the walk behind bug 031's pre-promotion (`prePromoteAddrTaken`,
asked by the statement `if`, the value `if`, `match` and the `&&`/`||` right operand) and the loop's
slot-or-phi choice (C9) listed the compound expressions it descends into and ended in `| _ => acc`.
`enumLit` was not listed, so its payload expressions were not walked, although Lower evaluates
them (`lowerExpr`'s `enumLit` case). A local whose only address-take in a branch is in a payload
is then promoted when that `&x` is lowered, inside the branch: the slot's initializing store is
there, the promotion is registered for the whole function, and the other path loads the slot. In a
loop the variable gets a header phi, and the mid-body promotion sends later writes to the slot,
which the back edge does not read (Codex review, 2026-10-26, traced both).

## Fix

`cexprAddrTaken` walks an enum literal's payload as it walks a struct literal's fields, and lists
every leaf constructor instead of a wildcard, so an expression form added to Core is a compile
error in the walk rather than a skipped subtree. Codex found no other compound `CExpr`, `CStmt` or
`CMatchArm` form the walk skipped, and no second walk with the same purpose in Lower.

## Structural cause

Class 8: storage decided ad hoc in Lower. Whether a local lives in a slot is decided per site by
a syntactic walk that must agree with what lowering evaluates, and the walk's wildcard made a
missing case silent (the absence of a case read as "no address taken", class 11's shape).

## Structural owner

R-0501 (S10 Place MIR): the storage of every local decided once per function from the places it
uses, not by per-site walks. Until then the walk has no wildcard.

Codex review 2026-10-26: CONFIRMED — Missing traversal of `enumLit` payloads defeats shared pre-promotion, allowing uninitialized branch-path loads and divergent loop phi/storage state.

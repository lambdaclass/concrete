# Bug 301 — Lower's address-taken walk did not see the root local of a field or element borrow

<!-- bug-ledger
summary: Lower's address-taken walk (`cexprAddrTaken`) recorded a local only for a bare `&x` / `&mut x`, while `placeAddr` lowers a field or element borrow (`&s.x`, `&q.p.y`, `&w.a[1]`, a method call on a field) by promoting the place's root local; when the root was not already slot-backed (an immutable struct local or a struct parameter), a branch or `&&` site did not pre-promote it, the root was promoted inside the branch that borrows it, and the other path read a never-stored slot (silent wrong code)
fixtures: regress_301_place_borrow_root.con
class: 8 — the address-taken walk and `placeAddr` decided separately which local a borrow promotes
silent: yes
exit: 0b
-->

**Status:** FIXED (branch `wip/bug-301`)
**Found:** 2026-10-26, hunting silent wrong code in Lower's handling of borrows and address-taken
locals after bugs 285 and 287 (hand probes of each `CExpr` constructor against the walk).
**Severity:** silent wrong code. The native binary reads an uninitialized stack slot on the path
that skips the borrow; `--interp` is right.

## Reproducer

```
struct Copy P { x: i64, y: i64 }
fn get(p: &i64) -> i64 { return *p; }
fn f5(k: i64, s: P) -> i64 {
    let mut r: i64 = 0;
    if k > 5 { r = get(&s.y); }
    return s.x + r;
}
fn main() with(Console) { println(f5(1, P { x: 9, y: 4 })); println(f5(9, P { x: 9, y: 4 })); }
```

## Observed

On main ea21a2e8 the binary prints `-1` and `13`; `--interp` prints `9` and `13`. `--emit-ssa` of
`f5`: `store %P %s, %addr.1` is in the `then` block only, and `load i64, %addr.1` is in the merge
block, reached by both paths. The same shape with an immutable local
(`let s: P = P { x: k, y: 2 }; if c { r = get(&s.x); }`) has the same unstored load on the `else`
path; clang happened to fold it in the probe. `regress_301_place_borrow_root.con` (a parameter, an
immutable local, a nested field, an element of an array field, a method call on a field, a value
`if`, a `match` arm and the `&&` right operand, each called on the path that skips the borrow):
main's binary exits 249, `--interp` 42.

## Expected

The binary computes what `--interp` computes: a borrow in code that does not run on a path does
not change the value a local holds on that path (STATEMENT_EXPRESSION_MODEL, evaluation order; the
interpreter is the reference).

## Cause

`IR/Lower.lean`. `placeAddr` lowers `&s.x` by recursing through each field access or element index
whose base is held by value down to the root identifier, and `addrOfLocal` promotes that root to a
slot (alloca in the entry block, initializing store where the borrow is lowered, promotion
registered for the rest of the function). The walk behind bug 031's pre-promotion
(`cexprAddrTaken`, asked by the statement `if`, the value `if`, `match`, the `&&`/`||` right
operand and the `defer` registration of bug 287) and the loop's slot-or-phi choice added a name only
when the borrowed operand was itself an identifier. A `let mut` struct is promoted at its
declaration and a loop promotes every aggregate, which hid the gap except for immutable struct
locals and struct parameters borrowed in one branch (Codex confirmation, 2026-10-26).

## Fix

`placeRootLocal?` names the local a borrow of a place promotes, following `placeAddr`'s recursion:
through a field access or element index whose base is held by value, to the root identifier; a
base that is a reference, raw pointer or heap cell is not the root's storage. Both decide that with
one predicate, `placeBaseIsPointer`, which `placeAddr` now uses for its two recursive cases, so the
walk and the lowering cannot disagree about which base is recursed into. The walk's `.borrow` and
`.borrowMut` cases add `placeRootLocal?` of the operand.

## Structural cause

Class 8: storage decided ad hoc in Lower. Which local a borrow makes slot-backed was decided twice,
by the lowering (`placeAddr`) and by a separate syntactic walk that must predict it; the walk
predicted only the bare-identifier spelling (bugs 031, 034, 285 and 287 are the same prediction
missing other spellings and sites).

## Structural owner

R-0501 (S10 Place MIR): explicit places, and the storage of every local decided once per function
from the places it uses, not predicted per site. Until then the walk and `placeAddr` share
`placeBaseIsPointer`.

Codex confirmation 2026-10-26: CONFIRMED — Projected borrows omit their local storage root from pre-promotion, allowing branch-local initialization of a globally registered slot and uninitialized reads on other paths.

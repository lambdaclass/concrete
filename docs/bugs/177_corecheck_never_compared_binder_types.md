# Bug 177 — CoreCheck never compared a binder's declared type with its value's

<!-- bug-ledger
summary: CoreCheck, the independent re-check of elaborated Core, never compared a binder's declared type with its value's (`let`, assignment, call arguments, fields, stores) and compared returns width-blind, so bug 176's checker leak compiled `let y: u8 = <i32>` into a binary that printed 300. One binder-agreement relation (`bindTyAgrees`, E0510; E0560 for returns) now checks every binder exactly on scalars and shapes, deferring only named/generic/type-variable types; CoreCheck's own blocks close their scope. Found fixing bug 176, 2026-10-22. Covered by `pipeline-test` `corecheck/binder-agreement` (planted Core) and `check_corecheck_boundary.sh` class 7. Class 13 (secondary 12); owner R-0499.
fixtures: regress_177_alias_value_hints.con
class: 12 — secondary: CoreCheck kept a width-blind copy of type equality
class: 13 — CoreCheck documented "arguments match parameters" and never compared any binder's type with its value's
silent: yes
exit: 0b
ruling: Violates: unrecorded
-->

**Status:** FIXED (branch `wip/bug177`)
**Found:** 2026-10-22, while fixing bug 176 (its record's "follow-up for defence in depth").
**Severity:** a front-end checker defect became silent wrong code instead of a refusal. CoreCheck,
the independent re-check of elaborated Core, accepted a Core program that bound an `i32` value to
a `u8` binder, so a `u8` held 300 in the binary.

## Observed

On main at `34f19e97`, bug 176's shape compiled and printed 300 from a `u8`:

```
fn main() with(Std) -> Int {
    let mut v0: i32 = 300; let mut k: i32 = 0;
    while k < 1 { let mut v0: u8 = 7; v0 = v0 + 1; k = k + 1; }
    let y: u8 = v0;      // v0 is the outer i32: Check's leaked loop scope typed it u8
    println(y);          // 300
    return 0;
}
```

Elab alpha-renames the shadowing binder (bug 087), so its Core reads
`let y: u8 = (v0 : i32)`. CoreCheck walked it and said nothing. With `let z: u8 = y + 1;` added,
the program reached SSA verification as E0715 (i32 vs u8 operands).

The same held for every place Core binds a value to a typed slot. Planting an `i32` value in Core
(bypassing Check) was accepted at a `let`, an assignment, a direct or indirect call argument, a
struct-literal or variant field, a field store, a store through `&mut`/`*mut`, an array element
store, an array literal's element, a value `if`/`match` branch, and a `return` from a `-> u8`
function. A fn value with `with(Console)` was accepted in a binder of capability-free fn type,
after which a call through the binder was checked against the binder's empty set. A `for` loop's
step statements were not checked at all, and a borrow block's reference was never bound, so a
store to it was not checked either.

## Expected

CoreCheck refuses a Core value whose type differs from its binder's, whatever the front end
decided: E0510 at a `let`, assignment, argument, field or store, E0560 at a `return`.

## Cause

`Concrete/Check/CoreCheck.lean`, `ccCheckStmt`:

- `.letDecl name _ ty value` checked `value` and then recorded `name : ty`. The declared type was
  trusted as the binding's type and never compared with `value.ty`. The case has had this shape
  since CoreCheck was written (`2fbe6437`, 2026-03-08).
- `.assign`, `.fieldAssign`, `.derefAssign` and `.arrayIndexAssign` checked their operands and
  never the stored value against the slot.
- `.call` compared argument COUNT with the callee's parameters, never argument types, although
  the module header lists "arguments match parameters". Struct and variant literals compared
  nothing.
- `.return_` compared with `typesCompatible`, which holds for every pair of numeric types, and
  skipped any type containing a named, generic or unit part. `return <i32>` from a `-> u8`
  function passed.
- `.while_` ignored its `step` list.

The one type-equality helper CoreCheck had, `typesCompatible`, collapses all numeric widths, so
even its existing checks (E0500 on identifiers, E0560 on returns) could not see a width confusion.
That helper predates exact-width integer typing (E0228, the flexible-literal work); the source
checker became exact, and CoreCheck's comparisons did not follow.

CoreCheck's own environment also leaked scope: `if`, `while`, `borrowIn`, literal and range match
arms did not drop their bindings when they closed (the bug 176 defect, in CoreCheck). With Elab's
alpha-renaming this only affected which binding an out-of-scope name resolved to, never a valid
program, but it is the same per-construct scope discipline.

## Fix

`bindTyAgrees declared got` is the binder-agreement relation. It is exact on everything Core has
decided before Mono: scalars (width and signedness), `bool`, `char`, `String`, `()`, and the shape
of references, pointers, arrays, heap shells and fn types, and on fn types the binder's capability
set must cover the value's. It defers (accepts) where CoreCheck does
not hold the facts to decide: `.named` and mismatched `.generic` heads (aliases, newtypes and
nominal types, which Check compares against tables CoreCheck does not re-derive), `.typeVar`
(decided per instance after Mono), `.placeholder`, and a `never` value. `&mut T` fits `&T`, and
`*mut T`/`*const T` pair as before. A `.generic` pair with the same head and arity is compared
argument-wise. A fn binder must declare at least the value's capabilities, because a later call
through the binding is capability-checked against the binder's recorded type (Codex round 1).

`checkBinding site declared value` applies it at every binder: `let`, assignment (against the
variable's recorded type), direct-call arguments (against the callee's signature), indirect-call
arguments (against the fn pointer's parameter types), struct-literal fields, variant fields, field
stores, stores through `&mut`/`*mut`, array element stores, array literal elements (against the
literal's element type), and the trailing value of each branch of a non-unit value `if` or
`match` (against the expression's type). Each refusal is E0510
(`bindingTypeMismatch`, "type mismatch at let 'y': declared u8, value has type i32"). `return`
uses the same relation and keeps E0560. The `while` step is now checked, and `if`/`else`,
value-`if`, `while` bodies, `for` steps, borrow blocks and literal/range match arms close their
scope through `inCoreBlock`. A borrow block binds its reference inside that scope.

### A disagreement the new check found in Elab

The full gate set refused `check_type_alias.sh`'s alias program (`type Id = i32; struct S { a: Id,
.. }` with `S { a: 1, .. }`): E0510, field `a` declared `i32`, value `Int`. Elab elaborated a
struct-literal field's value with the field's DECLARED type as the hint, aliases unexpanded, so
`.named "Id"` was no hint for the literal, which defaulted to `Int` while Core's field type is the
expanded `i32`. Enum-literal payloads and field stores (`s.a = 1`) used the same unexpanded hint.
Match-arm payload binders already expanded aliases (bug 096). All three sites now expand aliases
through one helper (`expandFieldAliases`, the type's own parameters shadowing aliases), so the
literal is an `i32`. Codex round 4 found the same unexpanded hint at two more sites: a
function's return type (`fn f() -> Id { return 1; }`, the hint for every `return`) and a
constant's declared type (`const X: Id = 1`). Both now resolve aliases with `resolveTypeE`, as
parameter types and `let` hints already did. Regressions: `check_type_alias.sh` section 2, which
compiles the program for its layout and fingerprint reports, and
`tests/programs/regress_177_alias_value_hints.con` (exit 42), a literal at each of the five sites. This is the defence-in-depth check working: an
Elab inconsistency that the backend tolerated is now a refusal, and it is fixed at its source.

## Structural cause

CoreCheck was built as a consistency pass that trusted the front end's binder types: it recorded
each declared type as a fact about the binding instead of a claim to check against the value. Its
one type-equality helper was the front end's old, width-blind `typesCompatible`. So the
"independent" re-check repeated no typing judgement the source checker made, and any source-checker
defect that let a wrong value reach a binder (bug 176's leaked loop scope) passed through it.

Class 13 of the [structural classes](STRUCTURAL_CLASSES.md), documented rules with no enforcing
site (with 073, 075, 091, 092, 113): CoreCheck's header documented "arguments match parameters"
and its role as the post-elaboration semantic authority, and nothing performed either for binder
types. Secondary class 12, one fact kept in two places: the typing rule lives in Check, and CoreCheck
held a weaker copy (`typesCompatible`) that had diverged from it.

A second enumeration of binder positions exists: Elab's bug 096 walk (`tyAgreeStmts`,
`checkCoreTypeAgreement`) lists call arguments, results, literal fields, field reads, field
stores, `let` initializers and returns, and compares only the respelled type names in them, and only when a
program has same-named types. `bindTyAgrees` and that walk are two producers of "the positions where
Core asserts a type", with different coverage (the 096 walk has no variable assignments, stores
through references, array element stores, value branches or array literal elements; CoreCheck
has no field reads or call results). Merging them into one
position enumerator with two relations is the follow-up, under the same owner.

## Structural owner

R-0499 (verified-pipeline S8, queue row 4): one authoritative checker over Core implements the
typing and ownership rules, so binder agreement is a rule of that checker rather than a re-check
that may or may not mirror Check. Until it lands, `bindTyAgrees` is the one binder-agreement
relation in CoreCheck, and its deferrals are the remaining gap, each still Check's alone to catch
before Mono:

- `.named`, unequal `.generic` heads and `.typeVar`: a confusion between two nominal types, or
  inside a generic body;
- fn-type capability sets that mention a capability parameter or variable. A `<cap C>`
  parameter reaches Core inside a fn type spelled as the concrete name `C` (Codex round 2), so
  CoreCheck compares only sets of built-in capability names (`capsDecidable`); the indirect-call
  check uses the binder's set, so a cap-variable requirement held in a narrower binder is not
  refused here;
- a value `if`/`match` typed `never`: Elab types an unhinted `if` by its first branch, which may
  diverge (`if c { abort() } else { 1 }` is `never`), so its branches are not compared. R-0492's `LocalId` (class 2, bug 176's owner) removes the trigger.

## Regression tests

- `tests/programs/regress_177_alias_value_hints.con` (`run_ok 42`): alias-typed struct field,
  enum payload, field store, return value and constant, each written as a literal.

- `pipeline-test` `corecheck/binder-agreement` (`Concrete/Pipeline/PipelineTest.lean`): the front
  end accepts a small program; each case then plants an `i32` value in its Core at a `let`, an
  assignment, a call argument, a struct field and a `return` of a `-> u8` function, and requires
  `coreCheckProgram` to refuse it with E0510 (E0560 for the return) naming the binder. Codex
  round 1 added a value-`if` branch, an array literal element and a `with(Console)` fn value in a
  capability-free fn binder. Round 2's corpus run showed that comparing every fn-type capability
  set refused `std.io`'s `Writer` and every cap-polymorphic callback (`with(C)` spelled as a
  concrete name), hence `capsDecidable`. It feeds
  CoreCheck directly, so it stays meaningful after bug 176's fix makes Check refuse the source
  shape first. On the previous compiler all eight cases were accepted.
- `check_corecheck_boundary.sh` class 7: bug 176's program must not compile. On main it compiled and
  printed 300; with this fix CoreCheck refuses it (E0510), and once bug 176 lands Check refuses it
  first (E0220).

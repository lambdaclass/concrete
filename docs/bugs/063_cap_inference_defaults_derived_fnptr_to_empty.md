# Bug 063: cap-variable inference reads "unknown" as "no capabilities", so a stored or derived fn pointer cannot reach a `cap C` parameter

<!-- bug-ledger
summary: cap-variable inference records "unknown argument type" as "no capabilities", so a fn pointer read from a struct field, call result or array element cannot reach a `cap C` parameter; misreported as E0220 against a `with()` the program never wrote (R-0470, 2026-07-27)
coverage: cap-variable inference recorded "unknown argument type" as "no capabilities", so a fn pointer read from a struct field, call result or array element could not reach a cap C parameter; misreported as E0220 against a with() the program never wrote. The authority check already PASSED on the fabricated empty capset and only expectTy stopped it, so relaxing fn-type comparison to subsetting would have turned a rejected-valid-program into an authority hole. FIX: both inference sites yield Option CapSet and contribute nothing when the type is unknown, letting resolveCaps name the variable. TWO SITES -- Check.lean (function calls) and CheckHelpers.lean (methods); the first fix took only one, and std's cap-polymorphic combinators (Set::fold, for_each, with_value) are METHODS, so real callers reach the one that was missed. Gated by check_cap_inference.sh (10/0), which asserts E0242 and the ABSENCE of E0220 on each path -- a gate that only checked "does it fail" would pass either way; see docs/bugs/063 (both sites 2026-09-19/2026-09-20 (R-0470))
class: 6
class: 11
-->

**Status:** Fixed
**Discovered:** 2026-07-27, while filing R-0005. Reproduced on four derived
argument forms.
**Fixed:** 2026-09-19 (function-call path), 2026-09-20 (method-call path).
**Regression test:** `scripts/tests/check_cap_inference.sh` (10/0), fixtures under
`tests/regressions/cap_inference/`.

**TWO SITES, AND THE FIRST FIX TOOK ONLY ONE.** `Check.lean`'s function-call path
and `CheckHelpers.lean`'s method-call path carried byte-identical copies of the
defect. The 2026-09-19 fix closed the first; the method path was still converting
an unrecognized argument type into `CapSet.empty` and still reporting E0220. It
was found by the sweep that became
[ABSENCE_IS_NOT_A_FACT.md](../project/ABSENCE_IS_NOT_A_FACT.md) — the shape
recurs, so fixing one instance is not fixing the defect.

The method path is the one that matters: the capability-polymorphic combinators
that actually exist in `std` — `Set::fold`, `for_each`, `with_value` — are
METHODS, so real callers reach it and not the other one. Both sites are now
asserted, each with its own derived-forms fixture and its own generic-return
E0242 control, so neither can regress alone.

## Symptom

A capability-polymorphic combinator accepts a function pointer only when the
argument is written as a bare name or as an annotated local. Any *derived*
fn-pointer expression carrying capabilities is rejected as a type mismatch:

```concrete
fn shout(x: Int) with(Console) -> Int { return x * 2; }
fn apply<cap C>(f: fn(Int) with(C) -> Int, x: Int) with(C) -> Int { return f(x); }

struct Ops { op: fn(Int) with(Console) -> Int }

fn main() with(Console) -> Int {
  let ops: Ops = Ops { op: shout };
  return apply(ops.op, 4);          // <-- rejected
}
```

```
error[check]: (E0220) type mismatch in argument 'f' of 'apply':
  expected fn(i64) with() -> i64, got fn(i64) with(Console) -> i64
```

`with()` is not something the program wrote. The compiler invented it, then
blamed the program for not matching it.

Accepted / rejected, measured:

| argument form                                   | result |
| ----------------------------------------------- | ------ |
| bare fn name — `apply(shout, 4)`                 | ok     |
| annotated local — `let g: fn(Int) with(Console) -> Int = shout; apply(g, 4)` | ok |
| call result — `apply(pick(), 4)`                 | E0220  |
| struct field — `apply(ops.op, 4)`                | E0220  |
| parenthesised field — `apply((ops.op), 4)`       | E0220  |
| array element — `apply(fns[0], 4)`               | E0220  |
| **any derived form whose fn type needs NO caps** | **ok** |

The last row is the diagnostic one. `apply(pick(), 4)` where `pick` returns
`fn(Int) -> Int` compiles — not because inference worked, but because the
invented answer happened to be correct. The feature appears to work until a
capability is involved.

## Root cause

`Concrete/Check/Check.lean:637-655`, inferring cap-variable bindings from
fn-typed arguments:

```lean
let argCapSet ← do
  let argTy ← peekExprType arg
  match argTy with
  | .fn_ _ cs _ => pure cs
  | _ =>
    match arg with
    | .ident _ varName =>
      match ← lookupFn varName with
      | some argSig => pure argSig.capSet
      | none => pure CapSet.empty
    | _ => pure CapSet.empty          -- <-- here
```

`peekExprType` (`Concrete/Check/CheckHelpers.lean:566-611`) is a deliberately
cheap syntactic peek: it handles literals, identifiers, `fnRef`, `paren`,
`binOp`, borrows and deref, and answers `.placeholder` for everything else. It
has no case for field access, call, or index — so every derived form lands on
`.placeholder`.

The defect is not the missing peek cases. It is that the consumer maps
`.placeholder` — *I do not know this expression's type* — onto
`CapSet.empty` — *this callback requires no authority*. Absence of information
is recorded as a positive fact.

The right answer was already available and is never reached. `resolveCaps`
takes a *list* of bindings and returns `.error cv` when a cap variable has
none, surfacing as `cannotInferCapVariable`. Supplying `empty` means a
variable that could not be inferred is reported as successfully inferred, so
that error path is dead for this shape. Contributing nothing would have named
the actual problem at the actual place.

The E0220 that the user sees is then a *second* check catching the first
check's fabrication: `expectTy` compares the parameter type after cap
substitution (`with()`) against the argument's real type (`with(Console)`).
The message describes the collision, not the cause, and points at the
argument.

## Severity

Rejected-valid-program, not wrong code — today. `expectTy` is what stops the
empty binding from going anywhere, and it is unrelated to capabilities; it
catches this as an ordinary type difference. Capability *satisfaction* runs
earlier, against the fabricated `resolvedCapSet`, and passes: with `C := {}`,
`missingCaps` finds nothing missing. So the authority check is already being
asked the wrong question and answering "fine"; the program survives on a
type-equality check that happens to sit downstream. Any future relaxation of
fn-type cap comparison — subset instead of equality, which is the natural
direction for passing a low-authority callback where a high-authority one is
expected — removes the only thing currently rejecting these programs, and
turns this into an authority hole. Fix it before that relaxation, not after.

## Collisions

- **DECISIONS.md:27,43** and **ANTI_FEATURES.md:192** recommend a struct of
  function pointers as *the* answer for pluggable interfaces and manual
  vtables — Concrete's stated replacement for dynamic dispatch. Reading a
  method out of that struct and handing it to a capability-polymorphic
  combinator is the intended use, and it is exactly the rejected form.
- **R-0016** (`Vec::with_slice<R, cap C>(&self, f: fn(&Slice<T>) with(C) -> R)`)
  is specified entirely in `cap C` combinators. Callers passing anything but a
  bare fn name hit this.
- The documented `for_each_with` / `with_owned` / `modify` family in
  CALLABLE_VALUES_AND_CAPABILITIES.md has the same shape.

## Fix as applied (2026-09-19)

Both halves of the candidate fix below, in that order.

**Distinguish "no binding" from "empty binding".** The consumer now contributes nothing
when the argument's capability set is unknown, so `resolveCaps` throws the variable it
could not infer. The misleading `E0220` became `E0242 cannot infer capability variable
'C'`, naming the actual problem at the actual place.

**Then widen what is known**, so the accurate diagnostic is rarely reached.
`peekExprType` gained a field-access case (auto-dereferencing through borrows, because
`ops.op` and `(&ops).op` name the same field), a call case, and an index case. All four
derived forms in the table above now compile AND dispatch correctly — the fixture runs
six calls through one `cap C` combinator and returns 12.

Generic call returns deliberately still answer `.placeholder`: substituting a callee's
type arguments is `unifyTypes`' job, and doing it inside a cheap syntactic peek would
make this a second producer of that answer. That case is the one the gate uses to check
the honest-refusal path.

**The gate asserts the ERROR CODE, not merely that something fails.** `E0242` means the
capability leg refused; `E0220` would mean the fallback is back and type equality is
doing the work. Per the severity note above, a gate that only checked "does it fail"
would pass either way — and would go on passing after fn-type comparison is relaxed to
subsetting, which is exactly when this becomes an authority hole rather than a rejected
valid program.

## Candidate fix (as filed)

Distinguish "no binding" from "empty binding". Contribute a binding only when
the argument's capset is actually known; let `resolveCaps` report
`cannotInferCapVariable` otherwise. That alone converts a misleading E0220
into an accurate diagnostic pointing at the cap variable.

Then widen what is known, so the accurate diagnostic is rarely needed: give
the peek a field-access case (struct field types are available), a call case
(the callee's return type is available), and an index case. Whether that
belongs in `peekExprType` — whose contract is to stay cheap and
conservative — or in a cap-specific query is a design call; the important part
is that whichever answers, it must be able to say "unknown" and be believed.

Regression to gate: each derived form (field, call, index, paren) reaching a
`cap C` parameter with a capability-carrying fn pointer; the genuinely
uninferable case producing `cannotInferCapVariable` and naming the variable,
not E0220; the empty-capset derived form still accepted; and a mutation
restoring the `pure CapSet.empty` fallback, which must be killed by the
capability leg rather than by the type-equality leg — otherwise the gate is
testing `expectTy` and would go on passing after the subset relaxation
described above.

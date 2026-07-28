# Bug 061: the proof model spells a parameter application and a global call the same way

**Status:** Fixed (2026-07-28, R-0442).
**Discovered:** 2026-07-25, auditing R-0002/R-0003 against `docs/PRINCIPLES.md`.

## Symptom

There is none yet — that is the point of filing it.

`Concrete/Proof/Proof.lean` models every application with one constructor:

```lean
| call (fn : String) (args : List PExpr)
```

Extraction produces that node for both a call of a global definition and a call
through a fn-typed parameter:

```lean
| .call (.direct fn) _ args _   => some (.call fn pargs)
| .call (.indirect binding) _ args _ => some (.call binding pargs)
```

So in the extracted model, `f(x)` where `f` is a parameter is indistinguishable
from `f(x)` where `f` is a top-level function. The two are different entities:
one is bound by the enclosing function and quantified over by the theorem, the
other names a definition with its own body and its own proof obligations.

## Why it is not reachable today

Within a single body the two cannot both occur under one name: Elab resolves a
call to a fn-typed local before it consults global signatures, so if a parameter
`f` is in scope, every `f(...)` in that body is the parameter. A conflation needs
a global `f` and a body where some occurrences bind to it and others to a
parameter of the same name — no std function does this, and `Option::map`,
`Result::map` and `Result::map_err` (the proved functions that apply a callback)
have no same-named global.

The FINGERPRINT already distinguishes them — `(call f …)` versus
`(callptr f …)` — so a program that changed one into the other would be caught as
stale. It is only the extracted PExpr that flattens the distinction.

## Provenance

Pre-existing and not a regression: before R-0002 there was no distinction to lose
at all, since Core itself spelled both as `.call name`. R-0002 introduced the
distinction in Core and in the fingerprint; extraction was briefly made to REFUSE
indirect callees, which was too strong — it blocked three real std proofs whose
statements hold for any `f` precisely because `f` is opaque — so extraction now
applies the binding as an uninterpreted function, restoring the previous
(conflating) spelling.

## Candidate fix

Give the model a distinct application form for an opaque, locally-bound callee —
`PExpr.applyVar (binding : String) (args)` or equivalent — so a theorem over
`Option::map` visibly quantifies over its callback instead of naming something
that could also denote a definition. This is the proof-layer instance of
principle 12: the parameter is an entity, and its identity should not be a string
that another entity can also spell.

Regression when fixed: a program with a global `f` and a function taking a
parameter `f` extracts two distinguishable nodes; existing proofs over
`Option::map`/`Result::map`/`Result::map_err` still verify.

## The witness, found while fixing it

Filed as latent with "no reachable witness in std today". That was wrong in one
respect worth recording: the witness was already in the repository, in the
proofs themselves.

`pureCoreFns` bound the representative callback `f` in the **global** function
table, because the HOF specs applied their fn-pointer parameter as `.call "f"`.
So `option_map_correct`, `result_map_correct` and `result_map_err_correct` were
discharged by resolving a *parameter* application against a *definition* — the
conflation, load-bearing, in three shipped theorems. The theorems are still
true (the callback really is that function, and their scope was always recorded
as `proof_coverage(representative)`), but the mechanism by which Lean accepted
them was the defect.

A source-level witness is now trivial to write, and is the gate's first case:

```concrete
fn f(x: Int) -> Int { return x + 100; }
#[spec] fn global_f(y: Int) -> Int { return f(y); }
#[spec] fn param_f(f: fn(Int) -> Int, y: Int) -> Int { return f(y); }
```

Before: both bodies extract to `.call "f" [.var "y"]`. After: `global_f` gives
`.call "f"`, `param_f` gives `.applyVar "f"`.

## Fix as shipped

Two identities, and two namespaces to resolve them in:

- `PExpr.call fn args` — `fn` names a definition. Answered by
  `FnTable.globals` only.
- `PExpr.applyVar binding args` — `binding` is a local. Answered by
  `FnTable.callables` only.

`FnTable` became a structure carrying both. It stayed *named* `FnTable` so the
~96 `(fns : FnTable)` annotations and ~166 `eval fns …` call sites kept their
meaning; only the three places that actually **apply** a table had to choose a
namespace, which is where the choice belongs. There is deliberately no
`CoeFun FnTable`: an implicit application would resolve to `globals`, silently
reinstating the conflation.

Carried through, per the task's list:

- **extraction** — `cExprToPExprImpl` maps `.call (.indirect b)` to `.applyVar`
  at both of its sites. It does not refuse them; refusing cost three real std
  proofs and was reverted earlier for that reason.
- **evaluation** — two `eval` arms over two namespaces.
- **fingerprints** — already `call` vs `callptr`; now verified by gate rather
  than by inspection.
- **preservation statements** — `eval_call_reduces` takes a `globals`
  hypothesis; new `eval_apply_var_reduces` takes a `callables` one; new
  `apply_var_ignores_globals` proves an application of a local is *stuck* under
  any global table, which is the formal refutation of this bug.
- **reports** — `renderPExpr` prints `&binding(...)`, `renderPExprAsLean` emits
  `.applyVar`, and both scaffold generators emit a two-namespace table with the
  callable slot documented. A report that printed the two forms identically
  would let a reader upgrade a representative-scoped theorem by eye.
- **proof dependencies** — `collectCallsExpr` takes only `directName?`, so an
  applied parameter contributes no edge; gated by asserting `param_f` depends on
  nothing.
- **table completeness** — `pexprCalls`/`fnTableComplete` for globals,
  new `pexprApplies`/`callableTableComplete` for locals. Two collectors, not one
  tagged list: a single list is what let one predicate check a parameter against
  the global namespace. Kernel-checked `example`s for the three HOF specs assert
  the callable namespace is complete AND that their global namespace is empty.

## Regression

`scripts/tests/check_proofcore_callable_identity.sh` (29 checks): same-spelling
extraction to different nodes; distinct fingerprints; no dependency edge; both
collectors in both directions; a global `f` failing to answer `.applyVar f`
while answering `.call f`, and a callable `f` the reverse; the three theorems
still present and now callable-scoped; the specs' global namespace empty; the
report rendering distinct; no proof presenting its callback as arbitrary; and
structural checks that both identities exist with no coercion between them.

Mutations #31-#33: extraction collapsing back to `.call` (killed by the gate),
`eval` resolving a local through `globals`, and the representative callback
rebound as a global. The last two are killed by the Lean **kernel** rather than
the gate — the three map theorems reduce to `⊢ False`, because `.applyVar f` is
stuck when `f` is bound in the wrong namespace. The proofs are therefore
themselves evidence for the separation, and the gate's structural assertions are
an independent second line rather than the only one.

# Bug 061: the proof model spells a parameter application and a global call the same way

**Status:** Open — latent. No reachable witness in std today; filed because the
proof model is where soundness claims live, and principle 12 asks for this class
to be visible before it bites rather than after.
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

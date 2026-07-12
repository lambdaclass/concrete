# The CoreCheck / pre-Lower boundary

Phase 6.5 #4. This note is the **audit** of the compiler's internal trust
boundary: what the pipeline guarantees about the IR it hands to `Lower`, and
where each guarantee is enforced. The recent judgment unifications (`TypeJudgment`,
`CopyJudgment`, `Shared.unifyTypes`) made the *inputs* to Core single-sourced;
this boundary is what turns that into a hard phase transition — `Lower` and the
backend may assume their input is free of frontend / mono / type-policy residue,
rather than defensively re-deriving it.

## Pipeline stages and validation points

```
ParsedProgram
  → Resolve → Check            (front-end: types, linearity, borrows, capabilities)
  → Elab   → ElaboratedProgram (Core CExpr, every node carries ty : Ty)
  → coreCheck → ValidatedCore  (CoreCheck: cast legality, unsafe-op capability, …)
  → monomorphize → MonomorphizedProgram   (verifyPostMono: no type vars, Copy fields)
  → lower → SSAProgram         (lowerModule + ssaVerify)
```

`ValidatedCore` and `MonomorphizedProgram` are **constructor-guarded tokens**
(`Concrete/Pipeline/Pipeline.lean`): the only way to obtain one is to pass the
corresponding validator, so a downstream stage cannot fabricate "this was
checked". `verifyPostMono = verifyNoTypeVars ++ verifyCopyFieldsPostMono`.

## Residue classes and where each is rejected

Every class below is **rejected before a binary is produced** — proven
end-to-end by `scripts/tests/check_corecheck_boundary.sh` (each residue program
must make `concrete -o` fail). The *stage* that rejects it:

| # | Residue class | Enforced at | Re-asserted post-mono? |
|---|---|---|---|
| 1 | Unresolved `Ty.typeVar` after mono | `verifyPostMono` (`verifyNoTypeVars`) | ✅ boundary |
| 2 | Illegal Copy specialization (Copy aggregate, non-Copy field) | `verifyPostMono` (`verifyCopyFieldsPostMono`) | ✅ boundary |
| 3 | Mixed-width binop (`i8 + i32`, E0228/E0715) | Check (E0228) **and** CoreCheck (E0502) | ✅ Core boundary |
| 4 | Capability misuse (call needs a cap the caller lacks) | CoreCheck | ✅ Core boundary |
| 5 | Unsafe op without capability (raw-ptr deref in a safe fn) | CoreCheck | ✅ Core boundary |
| 6 | Returning a second-class reference | Check | ❌ front-end only |

Classes 1, 2, 3, 4, 5 are enforced *at a boundary token* (`ValidatedCore` or
`MonomorphizedProgram`). Only class 6 is caught once, in Check.

Class 3 is worth a note: it is enforced BOTH in Check (E0228, exact-type) and,
independently, at the CoreCheck boundary (E0502, `binaryOperandMismatch`) — so
even a `Check` hole is caught before Lower. This was confirmed by mutation
testing: disabling Check's E0228 leaves `i8 + i32` rejected by CoreCheck E0502. A
speculative post-mono mixed-width verifier was prototyped and then **removed** as
redundant — CoreCheck already owns this boundary. (Note the width axis: E0715 is a
*bit-width* mismatch — `Uint`/`Int` are both 64-bit and lower to one SSA op, so a
same-width/different-signedness pair produced by a monomorphized generic is NOT a
width residue; it reaches Lower and lowers fine.)

## Defense-in-depth follow-ups (staged)

Class 6 is the one residue class caught only in Check:

- **Class 6 — safe returned-reference verifier.** Long-term policy is now the
  simple second-class-reference rule from `docs/VALUE_MODEL.md`: a safe callable
  may not return `&T` / `&mut T`, directly or nested in an aggregate/alias/generic
  instantiation. Former `&self -> &T` accessor shapes migrate to scoped access
  (`with_value`, `with_value_mut` / `modify`), value returns for `Copy` data,
  owned views, or explicit trusted/raw-pointer boundaries. Therefore the
  post-mono re-assertion should be structural over the fully substituted return
  type plus the function trust/safety class: safe Core must not expose a
  reference-return type. No lifetime/provenance verifier is required unless the
  deferred `from(param)` escape valve is deliberately admitted later; if that
  happens, it needs its own evidence-gated provenance design and must not weaken
  this default rule.
- **Class 4 capability erasure.** Confirm whether `CapSet.var` can legitimately
  survive to `Lower` (capabilities are largely erased at codegen). If it cannot,
  add `verifyNoCapVars` mirroring `verifyNoTypeVars`; if it can, document that
  cap vars are erased and require no boundary check.

These are additive, low-blast-radius verifiers in the `verifyPostMono` style;
each should land with a `check_corecheck_boundary.sh` row and, where it re-asserts
a Check rule, a mutation test (disable the Check rule, confirm the boundary now
catches it).

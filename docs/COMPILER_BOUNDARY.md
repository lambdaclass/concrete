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
| 6 | Returning a second-class reference | Check (E-ret) **and** `verifyPostMono` (`verifyNoReturnedRefs`, E0236) | ✅ boundary |

Classes 1, 2, 3, 4, 5, 6 are all enforced *at a boundary token* (`ValidatedCore`
or `MonomorphizedProgram`). There is no longer a residue class caught only in
Check.

Class 3 is worth a note: it is enforced BOTH in Check (E0228, exact-type) and,
independently, at the CoreCheck boundary (E0502, `binaryOperandMismatch`) — so
even a `Check` hole is caught before Lower. This was confirmed by mutation
testing: disabling Check's E0228 leaves `i8 + i32` rejected by CoreCheck E0502. A
speculative post-mono mixed-width verifier was prototyped and then **removed** as
redundant — CoreCheck already owns this boundary. (Note the width axis: E0715 is a
*bit-width* mismatch — `Uint`/`Int` are both 64-bit and lower to one SSA op, so a
same-width/different-signedness pair produced by a monomorphized generic is NOT a
width residue; it reaches Lower and lowers fine.)

## Class 6 — returned-reference boundary (landed)

- **Class 6 — returned-reference verifier (`verifyNoReturnedRefs`, E0236).** The
  long-term policy is **Option A**, the strictest form of the second-class-reference
  rule from `docs/VALUE_MODEL.md`: **no function may return `&T` / `&mut T`** —
  not safe, and not trusted — directly or nested in any aggregate/alias/generic
  instantiation. Former `&self -> &T` accessor shapes migrate to scoped access
  (`with_value`, `with_value_mut` / `modify`), value returns for `Copy` data, or
  owned views; trusted low-level code that must hand back a borrow uses a raw
  pointer (`*const T` / `*mut T`), which is the sole escape and is not a reference
  type. This is now re-asserted structurally at the post-mono boundary:
  `verifyNoReturnedRefs` (in `Concrete/Check/Verify.lean`) walks the fully
  substituted return type of every `CModule` function — via `tyExposesRef`, which
  descends through `.ref`/`.refMut`, `.generic` args, and `.array` elements — and
  rejects any reference-return with E0236, with **no trust/safety exemption**. It
  is wired into `verifyPostMono` alongside `verifyNoTypeVars` and
  `verifyCopyFieldsPostMono`, gated by `check_corecheck_boundary.sh`, and confirmed
  load-bearing by mutation testing. No lifetime/provenance verifier is required
  unless the deferred `from(param)` escape valve is deliberately admitted later; if
  that happens, it needs its own evidence-gated provenance design and must not
  weaken this default rule.

## Defense-in-depth follow-ups (staged)

- **Class 4 capability erasure.** Confirm whether `CapSet.var` can legitimately
  survive to `Lower` (capabilities are largely erased at codegen). If it cannot,
  add `verifyNoCapVars` mirroring `verifyNoTypeVars`; if it can, document that
  cap vars are erased and require no boundary check.

These are additive, low-blast-radius verifiers in the `verifyPostMono` style;
each should land with a `check_corecheck_boundary.sh` row and, where it re-asserts
a Check rule, a mutation test (disable the Check rule, confirm the boundary now
catches it).

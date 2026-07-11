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
| 3 | Mixed-width binop (`i8 + i32`, E0228/E0715) | Check | ❌ front-end only |
| 4 | Capability misuse (call needs a cap the caller lacks) | CoreCheck | ✅ Core boundary |
| 5 | Unsafe op without capability (raw-ptr deref in a safe fn) | CoreCheck | ✅ Core boundary |
| 6 | Returning a second-class reference | Check | ❌ front-end only |

Classes 1, 2, 4, 5 are enforced *at a boundary token* (`ValidatedCore` or
`MonomorphizedProgram`). Classes 3 and 6 are caught once, in Check — the observable
guarantee holds (nothing compiles), but they are not re-asserted after
monomorphization, so a future Check hole or a pass that *introduces* a mixed-width
binop post-Check would not be caught until SSA-verify (historically E0715, an
internal-error class).

## Defense-in-depth follow-ups (staged)

Turning "Check caught it" into "the boundary re-asserts it" for classes 3 and 6:

- **Class 3 — post-mono mixed-width verifier.** Walk every Core `CExpr.binOp` and
  assert operand widths agree, via `Shared.binOpOperandsAgree`. NOTE the
  exemptions that must NOT be flagged: shifts (`a << b` — the shift amount may be
  a different width than the value), pointer arithmetic (`ptr + int`), and the
  boolean short-circuit ops. Getting these wrong would false-positive *in the
  trust-critical boundary itself*, so the exact legal/illegal rule must mirror
  Check's line-122 rule (`isNumeric ∧ isNumeric ∧ ≠`, minus the exemptions)
  before this lands.
- **Class 6 — reference-in-return verifier.** Assert no monomorphized function's
  return type is `.ref`/`.refMut` (the second-class-reference invariant).
- **Class 4 capability erasure.** Confirm whether `CapSet.var` can legitimately
  survive to `Lower` (capabilities are largely erased at codegen). If it cannot,
  add `verifyNoCapVars` mirroring `verifyNoTypeVars`; if it can, document that
  cap vars are erased and require no boundary check.

These are additive, low-blast-radius verifiers in the `verifyPostMono` style;
each should land with a `check_corecheck_boundary.sh` row and, where it re-asserts
a Check rule, a mutation test (disable the Check rule, confirm the boundary now
catches it).

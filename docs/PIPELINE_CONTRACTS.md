# Pipeline stage contracts

ROADMAP Phase 6.5 #3. The compiler is a sequence of stages, each of which
consumes the previous stage's output and **promises** something to the next.
This note states those promises explicitly, so the pipeline is *reviewable*:
every later pass can point to the earlier contract that makes a case
impossible, and every user-triggerable violation is caught at the **first
responsible boundary** — never leaked to Lower, LLVM, the linker, or a panic.

`scripts/tests/check_pipeline_contracts.sh` enforces the "caught at the right
boundary, no leak" property with one violating fixture per contract. This is the
gate-based V1 of the deliverable; a `concrete inspect --pipeline-contracts
--json` surface can be layered on later without changing the contracts.

## The stages

```
source ──▶ Parse ──▶ Resolve ──▶ Check ──▶ Elab/Core ──▶ Mono ──▶ CoreCheck ──▶ Lower ──▶ SSA ──▶ SSAVerify ──▶ Emit ──▶ link
```

## Contracts (what each boundary guarantees)

### after Check
- No unconsumed owned linear locals, parameters, or branch/arm locals
  (linearity is fully resolved). *Violation → `error[check]` (E0208 class).*
- Operand types agree: no mixed-width/mixed-signedness binop survives to a
  later stage. Front-end and back-end share one width/signedness truth
  (`IntArith`, Phase 6.5 #1), so "Check says ok, SSA-verify says no" cannot
  happen. *Violation → `error[check]` (E0228 class).*

### after Elab / Core
- No impossible type hints, no illegal mixed-width binops, and no hidden
  capability calls introduced by lowering sugar (desugaring cannot smuggle an
  effect past the capability check).

### after Mono
- No unresolved generic declarations or `Ty.typeVar` leakage in emitted Core,
  except explicitly allowed generic metadata. Enforced as a hard wall:
  `Pipeline.monomorphize` runs `verifyPostMono` (`verifyNoTypeVars ++
  verifyCopyFieldsPostMono`) and refuses to proceed on any violation.

### after CoreCheck
- Binops are width-compatible; capabilities are checked; Copy constraints hold
  (a `Copy` type's fields are all Copy; no infinite-size recursive type);
  unsafe/trusted operations carry their fact. CoreCheck is the hard
  "no invalid Core proceeds" wall. *Violations → `error[core-check]`
  (capability E0521, Copy-field E0571, recursive-type E0583, …).*

### after Lower
- The SSA verifier passes (`ssaVerifyProgram`); runtime traps are present for
  every required check (bounds, overflow, div/mod-zero, shift range — the
  checked-arithmetic and array-bounds contracts); defer/scope cleanup
  boundaries are represented in the CFG.

### after Emit
- Checked operations lower through named helpers (`@__cc_*`) or named proven
  elisions — never silently to raw LLVM wrap/UB. An executable build has an
  entry point. *Violation → `error[link]`, not an `ld` "Undefined symbols"
  leak.*

## Enforcement discipline

The gate is deliberately a *boundary* assertion, not just a "does it error"
check: each fixture pins the **pass tag** of the diagnostic, so a regression
that moves a check later (e.g. a recursive type once again reaching `llvm-as`,
bug 024) fails the gate even though the program is still ultimately rejected.
When adding a new user-triggerable failure mode, add its fixture here at the
boundary that should own it — that is where "this can't happen downstream"
becomes a checked fact rather than a hope.

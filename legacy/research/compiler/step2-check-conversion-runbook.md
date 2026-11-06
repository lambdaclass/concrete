# Step 2 runbook — shared `TypeJudgment` for literals (Phase 6.5 #9)

Date: 2026-07-10
Status: **not started.** The old Check/Elab → `IdExpr` conversion plan is
superseded for the type axis. Core `CExpr` is already typed (`ty : Ty` on every
expression node), so the first type-axis slice is not a new typed source tree.

## Goal

Make literal/defaulting type decisions come from one implementation:

```text
Concrete/Semantics/TypeJudgment.lean
        ├─ Check uses it for type-dependent checks
        └─ Elab uses it to stamp CExpr.ty
```

This is the type-axis sibling of `IntArith`: one reference meaning, both
consumers route through it, and a gate proves they cannot diverge.

## Why This Replaces The IdExpr Conversion

`Concrete/Elab/Core.lean` already defines typed Core:

```lean
inductive CExpr where
  | intLit (val : Int) (ty : Ty)
  | binOp (op : BinOp) (lhs rhs : CExpr) (ty : Ty)
  -- ...
```

The bug class was not "there is no typed carrier." The bug class was that Check
and Elab had separate source type judgments and sometimes disagreed. Therefore
the first fix is shared judgment, not another typed source IR.

Future source identity and `CompilerDB` remain useful for relations, evidence,
provenance, and DB-owned facts. They are not the primary type-drift fix.

## First Slice: Literals

Target the E0228/defaulting family first:

1. Extract literal type/defaulting logic into
   `Concrete/Semantics/TypeJudgment.lean`.
2. Route Check's literal type-dependent checks through it.
3. Route Elab's literal `CExpr.ty` stamping through it.
4. Delete or bypass no private Elab fallback for migrated literal cases; remove
   the duplicate inference path once the slice is green.
5. Add a red-team gate proving Check and Elab cannot disagree on literal type
   selection.

For literals, the primary type truth is `CExpr.ty`, stamped from
`TypeJudgment`. `CompilerDB` may record evidence/provenance for the decision,
but must not store a second independent literal type answer.

## Suggested Verification

- Build.
- Main suite.
- Existing numeric literal / mixed-width / cast gates.
- New type-agreement gate seeded with the historical E0228 shape:
  Check's hint/defaulting path and Elab's sibling/defaulting path must produce
  the same type for the same source literal.

## Expansion Order

After literals:

1. binops and mixed-width agreement;
2. casts;
3. calls and type arguments;
4. aggregates;
5. match/if expressions;
6. patterns and places.

Each family follows the same rule: Check and Elab call `TypeJudgment`; Elab
stamps typed Core; downstream stages read `CExpr.ty`.

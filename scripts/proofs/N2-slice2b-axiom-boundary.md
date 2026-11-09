# N2 slice 2b: unavailable axiom transfer boundary

Owner: R-0219. Status: blocked design point, not a completed solver slice.
For independent review by the coordinator: do the best long-term thing rather
than cut corners. No review tooling was run by this writer.

## Exact required positive counterexample

The source term is the premise witness of `finite-collection-simp-positive`
(`vacuity_controls_round2.py:239–248`), instantiated at the existing case's `n = 2`:

```lean
theorem singletonCertificate : ∀ x : Nat, x ∈ [2] → x = 2 := by simp
```

Its elaborated body is:

```lean
of_eq_true
  (Eq.trans
    (forall_congr fun x =>
      Eq.trans
        (implies_congr
          (Eq.trans List.mem_cons._simp_1
            (Eq.trans (congrArg (Or (x = 2)) List.not_mem_nil._simp_1)
              (or_false (x = 2))))
          (Eq.refl (x = 2)))
        imp_self._simp_1)
    (implies_true Nat))
```

There is no planted impossible axiom or inconsistent local assumption. The current
`falseDischarge` returns `false`. Applying `validateCertificate` to this exact body
and its exact type, with empty guards and permitted assumptions, produces exactly:

```
INCOMPLETE analysis-budget-or-error Boundary.instance stage=certificate-dependencies law=Boundary.law case=finite-collection-simp-positive unavailable-certificate-transfer propext
```

The probe also checks that `propext` is an `axiomInfo` whose body is unavailable,
even with `allowOpaque := true`. This is an unavailable primitive, not a pending
recursive lookup, supplied False, exhausted budget, or unavailable theorem body.
The output is checked for the exact diagnostic, including Instance/law/case and
supplier declaration; an arbitrary failure does not satisfy the probe.

## Specific gap in the accepted note

[N2-discharge-solver.md:127–137](N2-discharge-solver.md#L127) requires expression
and type dependency closure, makes unavailable required transfers unresolved, and
permits generic recursor/constructor metadata rather than a trusted proof list.
The A2 trust-boundary sweep at lines 192–203 applies this requirement to helper
summaries and safety-bearing transports too. `propext` has neither an exposed body
nor recursor/constructor metadata. Its transfer through this transport therefore
needs a rule beyond the body/metadata rules specified there.

At lines 366–374 the note requires the existing positive controls to complete,
including all safety-bearing dependencies; this witness belongs to one of those
controls. Lines 159–161 forbid discarding an unfinished dependency check and then
reporting clean. The current validator's prescribed unresolved result therefore
cannot simply be swallowed to meet the required positive outcome. The same
primitive also occurs in the simp-generated exclusion for
`unreachable-closed-supplier-positive` (line 370).

Lines 213–215 explicitly require stopping rather than inventing a trust rule when
the convention does not settle a transfer.

This identifies a missing axiom-transfer rule in the design, not a proof that no
stronger solver can handle the term. Continuing needs an explicit account of how
that transfer closes while preserving the A2 boundary. Kernel validity alone,
accepting all non-refutable opaque results as clean, trusting `propext` by name,
or bypassing validation of simp transports is not that account.

## Long-term resolution proposed for acceptance

Extend the owning R-0219 design with a generic typed relation for primitive axiom
applications, distinct from unavailable implementations: retain the exact supplied
result origin and all actual input origins, and propagate later impossible demands
and transport constraints back through that relation. Specify the finite evidence
that closes a safety-bearing transport; a non-refutable result type alone must not
close it. Where that evidence cannot be established, retain the named INCOMPLETE.
Require both this positive and supplied contradictory-equality/transport negatives
to validate the rule. This is a proposal for the coordinator's design acceptance,
not a new trust rule implemented here. No OD choice or new roadmap identifier is
made; R-0219 remains the owner.

The attempted interpreter was removed rather than retaining unvalidated safety
rules. Production code and the consumer remain unchanged. No slice-2b solver entry
point or 33-control completion is claimed. After the amendment is accepted, finish
the application transfer/worklist/SCC solver and run all 33 exact controls before
integrating the consumer in slice 3.

## Reproduce

```sh
bash scripts/checks/lib/bounded.sh 90 python3 -u scripts/proofs/vacuity_solver_boundary.py
```

`PASS boundary reproduced` means the specific required-positive/unfinished-transfer
boundary was reproduced; it does not mean the new solver or positive gate passed.

Focused results on the unchanged production implementation at
`9a05487dd2cdc4f3a949fcb80d2d5917f476d162`:

- Boundary reproducer: exact unavailable-`propext` diagnostic reproduced; existing
  `falseDischarge=false` and primitive-axiom/body-unavailable assertions pass.
- Existing direct certificate controls: 5 PASS, 0 FAIL, including exact supplier
  `impossible`, non-circular rejection and shared-budget diagnostic.
- No completed 33-case new-entry run; no heavy gates or review tooling run.

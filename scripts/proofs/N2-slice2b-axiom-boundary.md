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

[N2-discharge-solver.md:127–137](N2-discharge-solver.md#a2-amendment-dependency-validation-and-non-circular-admission) requires expression
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

## Design amendment 2 disposition

The owning [design amendment 2](N2-discharge-solver.md#design-amendment-2-primitive-axiom-applications--owner-decision-required)
retains the typed `PrimitiveApp` representation contract without admitting a
transfer. Its round-2 bounded Lean trace at
`22f30020360906ad705f6c7a62041606eed0b747` visited 111 declarations, following
expression constants, types and all available bodies, under 2,000,000 heartbeats
and a 20,000-step cap. The full nonstructural bodyless closure of
`singletonCertificate` is `propext`, `Quot.sound` (`axiomInfo`) and `Quot`,
`Quot.mk`, `Quot.lift` (`quotInfo`). `forall_congr` unfolds through `funext` to
the quotient construction. The first `propext` diagnostic masks these later
dependencies. Classify quotient primitives by kernel `quotInfo` metadata alongside
`inductInfo`, `ctorInfo` and `recInfo` (including the general `Quot.ind` rule,
although it is not in this certificate's closure). `Quot.lift` reduces on
`Quot.mk`, but lifting across equality supplied by `Quot.sound` still needs the
selected axiom-dependent provenance rule; metadata does not settle that demand.

The amendment corrects the earlier grouping of two situations above.
`refutableContext` generates the gate's own certificate using `simpAll`, whereas
`finite-collection-simp-positive` supplies an authored Instance premise proof.
The same bounded Lean check established that
`hn (of_decide_eq_true h) : False` for `h : decide p = true` and `hn : ¬p`
has no axiom dependencies. Constructive synthesis for this generated exclusion
is an N2 design choice, not an owner question. It should complete
`unreachable-closed-supplier-positive` without axioms; nothing is implemented here.

Only authored axiom-dependent transports need the owner's policy answer. The
amendment presents three options with explicit soundness, generality and effort
costs: (1) checked constructive simulation of extensionality transports, with high
simulation/provenance proof effort and limited observations; (2) explicit
provenance-preserving contracts for exactly `STANDARD_AXIOMS` (`propext`,
`Quot.sound`, `Classical.choice`), with a named trust premise and substantial
contract work but broader standard-proof coverage; (3) strictly constructive N2,
with no added axiom-transfer trust, lower solver complexity and narrower authored
proof coverage. Option 3 keeps authored `propext` transports INCOMPLETE and requires
an explicit revision of the affected control expectation. Option 1 necessarily
inspects standard-operation identity and application/type shape: a bodyless axiom
cannot be simulated while both are forbidden. Under every option, user-declared
axioms are unproved supplied assumptions, never standard axioms merely because
their types match. The three-axiom permission in docs/decisions.md:462 and
scripts/proofs/lib.sh does not itself establish a provenance contract.

The recommendation is option 2, explicitly not a choice. No positive completion,
new control count or primitive transport admission is claimed. The interim exact
INCOMPLETE, retained `PrimitiveApp` contract and fixed phase-B exit criterion
remain unchanged. R-0219 owns the finite-evidence and backward-propagation design;
resume the authored-transfer policy design after the owner's answer, then obtain
the coordinator's independent acceptance before implementation.

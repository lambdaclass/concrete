# C5 round 4 (Codex writes): finish item 6 of ROUND3.md (your question is answered)

HEAD is f8d9a0a9 (round 3: items 1–5 done; not yet reviewed). All rules of _review/BRIEF.md and ROUND3.md apply (commit each piece after `nix develop -c timeout 1200 lake build ConcreteSpec` passes; do not push; do not edit scripts/proofs/axioms.golden).

Your question is the decided reading; the brief's wording "every outcome kind" was imprecise. The owner's decision (2026-11-08, Q3) is:
- **Unconditional forward simulation**: whenever the instrumented C1 machine takes a step, the erased machine takes the matching step with the same event and the same concrete store; whenever the instrumented machine reaches a final outcome that is not `stuck` (returned, trapped, badAnswer, outOfFuel), the erased machine reaches the same outcome with the same events and store. Lift it to multi-step runs (`Steps`/`runFrom`). Instrumented `stuck` is NOT required to correspond: instrumentation may refuse where the erased program continues (your moved-owner counter-example, erased returns 7, is the planted refutation of the unconditional converse; keep it).
- **Converse only for checker-accepted programs**: for a program the checker accepts, the instrumented machine is stuck exactly when the erased one is (equivalently, instrumentation never refuses on an accepted program). State the checker-acceptance judgment as an explicit named interface (a `Prop` over programs with the property the proof needs, documented as the C5/C9 checker obligation, `/-- Obligation (discharged by the ownership checker, slice N, R-0571). -/`), not as an assumption that makes the theorem trivial; give a concrete accepted program where the converse is exercised and the moved-owner program as one the judgment must reject.
Cover eval, test, life, allows, primitive answers and event order in the simulation. Remove the "outstanding" wording in README O3 for what you prove; keep it for what remains, with the roadmap row.

## Best long-term design (owner rule, binding)
Judge and fix everything by the best long-term design. Never cut corners for speed, a smaller diff or fewer rounds. Implement the structural fix, not a containment patch. If staging is truly unavoidable, record the long-term design and its roadmap row in your final message.

## Vacuity rule and evidence tags
Every law you add or change needs a non-trivial instance and a planted counter-example (an `example` with a negation that falsifies its conclusion), tagged `/-- Instance (law `Fully.Qualified.name`). -/` and `/-- Planted (law `Fully.Qualified.name`). -/`.

### Phase A reordered after the design review (2026-11-03)

An independent review of the design on main
([research/process/design-review-2026-11-03.md](research/process/design-review-2026-11-03.md)) found it
ready for the target Core proposal to be drafted, not approved. Row A1 now also drafts the semantic
contract's objects, the theorem's signature, the trusted-base inventory and slice 1's manifest, so Core
is proposed against them; A2's approval requires representative design proofs in Lean before Core is
frozen; A3 formalizes the approved result; totality is required as each phase B pass lands and audited at
B3. The nine questions for the Core session are listed in the roadmap.

`check_roadmap_coverage.sh`'s negative controls now fail when their edit matches nothing: two of them
had matched the old row A1 by its literal owner list and would have passed silently once the row
changed. They now find row A1 by its structure.

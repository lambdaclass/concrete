# Units of Measure / Dimensional Annotations — Decision

Status: evaluated, deferred (no units in v1) — ROADMAP Phase 6 #15.

Units-of-measure annotations (bytes vs bits, ms vs s, block-count vs byte-offset,
protocol-length, memory-size) would catch a real class of systems mistakes. This
records the decision to **not ship them in v1**.

## Decision

**No units-of-measure in v1.** It is genuinely useful, but it is a *type-system
feature* with proof and report implications, not a localized addition:

- it interacts with the proof model (a unit-tagged integer must extract to the
  same `PExpr` as its untagged form, or units become a proof axis);
- it interacts with reports/obligations (unit erasure must be an audit fact, not
  silent — "no proof status may depend on unit erasure unless a contract/VC names
  the conversion explicitly");
- done poorly it adds ceremony for marginal near-term value.

Cost/benefit does not favor building it now, ahead of the stdlib and tooling work
that Phase 6/7 actually need.

## What we do instead, today

The current discipline carries most of the benefit at zero language cost:
**encode the unit in the name** — `len_bytes`, `timeout_ms`, `off_in_block`,
`size_bits` — per [STYLE.md](STYLE.md). And checked arithmetic already removes the
silent-corruption failure mode that unit mix-ups often hide behind.

## If it ever lands

Keep it **research / workload-gated**. The first step would be a *report-only*
prototype (as the roadmap originally sketched): `examples/units_probe/` covering
the mistake classes above, `docs`-tracked, with annotations optional and **erased
only after audit records the conversion** — no implicit unit conversion, no
dependent numeric refinements, and no proof status depending on unit erasure
unless a contract/VC names the conversion explicitly. Only a real workload that
keeps hitting unit bugs should pull this in.

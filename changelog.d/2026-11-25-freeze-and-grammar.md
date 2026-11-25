### Phase T restarts, grammar approval, slice scheduling and executable evidence

Record the [November 25 owner decisions](docs/decisions.md#decided-2026-11-25-freeze-and-grammar).
Agents can restart paused Phase T work when a recorded bug on main shows the
item is needed. Each restart cites the defect and stays within its scope. The owner approved
the Slice 0 productions shown to them, subject to Codex and agy review. Each later slice
requires grammar approval before parser work. The combined 0.1 grammar goes to the owner
before Slice 3's parser and freezes at Slice 11. Slice 1 starts alongside the rest of
`B0a` under the standing two-slice limit. Each slice still requires end-to-end proof
before closure. Codex and agy review the grammar, with Gemini Neon when available.
[R-0729](ROADMAP.md#r-0729) tracks binding each executable by digest to its separately
kept proof evidence. The compiler architecture plan will settle the format before
implementation.

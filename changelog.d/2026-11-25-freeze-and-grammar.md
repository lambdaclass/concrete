### Phase T restarts, grammar approval, slice scheduling and executable evidence

Record the [November 25 owner decisions](docs/decisions.md#decided-2026-11-25-freeze-and-grammar).
Agents can restart paused Phase T work when a recorded bug on main shows the
item is needed. Each restart cites the defect and stays within its scope. The owner approved
the Slice 0 productions shown to them, subject to Codex and agy review, with Gemini Neon
for agy when available. Each later slice requires grammar approval before parser work.
For Slices 0 and 1 only, matters that would otherwise need the owner’s approval, including
grammar approval, can be decided by agreement among Claude, Codex and agy. Record their
verdicts and report the decision to the owner. From Slice 2 onward, the owner approves the grammar.
The coordinator prepares the combined Slices 0–11 grammar to expose cross-feature clashes
“before slice 3.” It freezes at Slice 11. Slice 1 starts alongside the rest of `B0a`.
Later starts retain the sequential rule. The owner set a standing cap, “but never do more
than two stages in parallel,” followed by “remember this.” Each slice still requires
end-to-end proof before closure.
For [R-0729](ROADMAP.md#r-0729), the owner preferred hash binding in the proposal to embed
facts and a hash and ship the full proof alongside the executable. The Slice 0 implementation
design remains open and will be settled under the delegation and recorded in a later entry.

### Std coverage audit against Rust, Zig, Go and C/POSIX (2026-11-08)

`docs/proposals/STD_COVERAGE.md` compares the workload-driven std plan with the standard libraries
of Rust, Zig, Go and C/POSIX, category by category, and classifies each category as covered,
deliberately out (citing the decision), a real gap for an adopted showcase stage, a conditional
need, or design validation only. It names six definite gaps (among them that OD13's replace/take
through `&mut T` is not scheduled in safe code before `OrderedMap` at 23, line framing over partial
reads for grep at 16, and the operation registry for already-decided filesystem operations), small
design probes at slices 15, 20–22 and 18–27, and owner questions S1–S5. It is PROPOSED, reviewed by
Codex, Kimi and agy, and decides nothing.

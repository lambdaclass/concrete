### Wide Matches Verify in Near-Linear Time (bug 178 follow-up, 2026-10-26)

The SSA verifier computed dominators with an algorithm that, at the merge after a `match` of many
arms, walked a chain as long as the match once per arm, so a 16000-arm match spent most of a
second there. It now uses the Lengauer–Tarjan algorithm, which gives the same dominators in
near-linear time; the same match lowers in under 0.2 s
([bug 178](docs/bugs/178_superlinear_match_lowering.md)).

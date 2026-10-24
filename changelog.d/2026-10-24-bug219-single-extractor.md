### The Proof Theorems Are About The Extractor The Compiler Runs (bug 219 follow-up, 2026-10-24)

`cExprToPExpr` and `cStmtsToPExprK` no longer repeat the extraction arms of `cExprToPExprImpl` and
`cStmtsToPExprKImpl`; each is a bare delegation. The copies dated from when the implementation was a
`partial def` and the ProofSoundness theorems needed reducible arms. The implementation is an
ordinary `def` now, so the preservation theorems unfold its equations, and every extraction rule
exists once. `check_proofcore_callable_identity.sh` fails if a copy reappears (two controls), and
mutation 31 is killed against the single copy.

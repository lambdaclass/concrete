### Mutation 31 Mutates The Extractor The Compiler Runs (bug 219, 2026-10-23)

Mutation family 31 restores bug 061 (a parameter application extracted as a definition call). Its
anchor matched the `.applyVar` arm of `cExprToPExpr`, the non-partial wrapper that only the
ProofSoundness theorems read, so the mutant changed nothing a gate executes and survived. The
anchor now names the arm of `cExprToPExprImpl`, and `check_proofcore_callable_identity.sh` kills it.

### The LCB joins slice 0, policies in Concrete.toml, ideas-sweep and pipeline-review tasks (2026-11-06)

`docs/decisions.md` records two owner decisions:
- **The LCB joins slice 0.** Slice 0 builds every example through two unverified backends, LLVM and the Lean Compiler Backend, and CI compares their full observations on a GitHub-hosted AArch64 macOS runner. The BIR → LCB-IR adapter is the first backend proof target, and the LP0 pilot composes it with the LCB's theorems. The proof-infrastructure ("Backend order") and long-term ("Bound the LLVM hole") rows are revised in place. New tasks R-0578 and R-0579; R-0572 and the LP0 row are revised.
- **Policies and declared assumptions live in `Concrete.toml`.** The sections are typed, versioned data; a module may tighten a policy, never relax it; dependencies are checked against the consumer's policy, transitively and fail-closed. The spellings wait for the owner. New task R-0589.

ROADMAP gains these tasks from the pipeline review:
- R-0580: a Memory CFG stage.
- R-0581: BIR as a backend contract.
- R-0582: per-pass certificates.
- R-0583: the per-pass proof-or-validator table.
- R-0584: a minimal direct-syscall launcher.
- R-0505: final-image validation.

It also gains these from the ideas sweep:
- R-0585: vacuity checks and contract-strength labels.
- R-0586: the Lean evaluator's I/O driver.
- R-0587: debug info on the LLVM path.
- R-0588: fact preservation across indirect calls, specialization and packages.
- R-0575: gains source locations, plus frames and dependency-flow fields.
- R-0201: now covers only the audit commands over the query.
- R-0353: separates historical validity from current acceptance.
- R-0491: refuses expectations recorded from the compiler.
- R-0137: ships paired rejected-violation examples.

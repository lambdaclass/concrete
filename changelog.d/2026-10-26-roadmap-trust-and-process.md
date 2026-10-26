### Roadmap: Per-Compilation Certificates, The Trusted-Base Inventory, An Estimated Plan, Provisional Behaviour Under Owner Decisions; Agent Process (2026-10-26)

New task R-0552, beside R-0517: release builds emit a certificate, checked by the Lean kernel or an
independent kernel (nanoda, con-leche), that each output preserves the source semantics, so Lean's
code generator, the C compiler and the Lean runtime leave the trusted base for checked artifacts.
A separately compiled checker would not close that gap; self-hosting is recorded as an optional,
much later alternative, not planned. R-0509 now lists the trusted base entry by entry, each with
its mitigation and owning task. [Phases and releases](ROADMAP.md#estimated-plan-to-a-proven-compiler)
gains an estimated plan (V1a about 3 months, V1a – V4 about 9 – 18 months, the whole with the native
backend a further 1 – 2 years), counted from the date the owner answers OD1 – OD3 and OD10 – OD11.

[Owner decisions](ROADMAP.md#owner-decisions) now opens with the rule that no agent, orchestrator or
reviewer chooses, encodes, implements or provisionally ships any side of OD1 – OD22, bug fixes
included. OD22 records bug 189's conflict with TRUST-5@1 (`trusted impl`) with both options. The
landed fixes of bugs 189, 198, 212 and 213 are recorded as conflicts, not decisions: what main does
today, provisional until the owner answers under OD22, OD13, OD16 and OD18.

Agent process ([AGENTS.md](docs/project/AGENTS.md)): the owner-decision rule, with the Codex review
(`scripts/review/review.sh`) asking whether a patch picks a side on an OD; one bug or one rule per
branch now; a rebase onto `origin/main` after every landing; impact-selected gates locally and CI
dispatched on the branch for the full set. `scripts/land/train.sh` reads the train's push to `ci/<name>`
and both landing pushes back with `ls-remote` and retries one that did not move the ref (GitHub drops the connection during
long hooks), and `land` cancels main's duplicate push run of CI for the SHA its train run validated
(`--keep-main-ci` keeps it); `check_landing_script.sh` gains legs for both.

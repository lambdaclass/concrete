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

More owner decisions of 2026-11-06, in the same change:
- **Diagnostics are checker outputs** (R-0592, R-0593, R-0479).
- **Heap facts and memory policies** (R-0592, R-0594, R-0595).
- **Secrets, noninterference and constant time** (R-0592, R-0596). This revises Q2.2: `Secret<T>` is `Copy` only for scalar `T`.

  The three proposals behind these decisions are archived in `docs/archive/crucial-2026-11-06/`.
- **The new compiler pipeline** is recorded as a decision row. It supersedes the legacy VERIFIED_PIPELINE.md.
- **`legacy/` is kept way later.** B3 deletes nothing. LD (R-0590) deletes `legacy/` only after the ideas sweeps, once every RETENTION row is resolved and with the owner's approval. The deletion point was chosen later the same day (see "When `legacy/` is deleted"). Phases C, D and E each start with a legacy ideas sweep (R-0591, R-0597, R-0598).
- **OS-level sandboxing** is outside the compiler's scope.

The documents now point readers to the decisions and the open work:
- `docs/decisions.md` opens with "Waiting for the owner" and "Recent decisions".
- `docs/proposals/` holds the legacy ideas sweep and the pipeline review.
- README gains a "Where to read" section.
- The slice-0 surface: `concrete build` and `concrete facts`; a distinct "not supported in this slice" refusal; leading-zero integer literals refused; the reported trap site is a named premise until proved.
- **Thin vertical slices, integers first, capabilities early:** each slice is fully proven before the next, and LP0 lands before the language grows large. The slice order will come from the agreed slice plan. The former slice 1's ownership obligations arrive with the ownership slice.
- **From the SPARK review** (internal): R-0575 derives writes and output dependencies when `&mut` lands; R-0599 adds replayed counterexamples without SMT; R-0585 marks discharges in infeasible contexts as vacuous; R-0600 adds a derived "always aborts" fact. The review itself is in `docs/proposals/SPARK.md`, and its owner items are pending.
- **Owner approval of 2026-11-06 ("ok do everything").** These follow the unanimous choice of Claude, Codex, agy and Kimi.
  - **SPARK items 1, 2, 3, 6, 7, 9 and 10:**
    - functions with safety preconditions are refused as function values until function-type contracts exist (R-0601);
    - struct invariants;
    - `assert(e) by thm(...)`;
    - an observable-event kind (R-0602);
    - unused permissions as facts;
    - lexicographic measures on demand (R-0605);
    - named assurance properties (R-0606).
  - **Runtime and spec:**
    - the trap line;
    - the default struct layout;
    - rule revisions in place of the spec version in rule IDs;
    - one `spec_version` per output (R-0603).
  - **The slice order** is filled in ROADMAP:
    - slice 1 stays Q8.2, with an explicit `abort()`;
    - LP0 comes after slice 1;
    - integers are complete at slice 2;
    - file-copy and 0.1 come at slice 18, with R-0595 moved to slice 19;
    - RocksDB and ethlambda come at 25 and 27, after an audit (R-0604).
  - **`Concrete.toml`** uses `[policy]` and `[assumptions]` tables.
  - **`#[overflow_checked]` (ARITH-9)** is retired.

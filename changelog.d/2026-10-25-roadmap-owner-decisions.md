### Roadmap: Owner Decisions, Exit 0b From The Records, A Per-Class Discovery Signal, Compile-Time Scaling (2026-10-25)

The queue now says plainly that nineteen language decisions (OD21, a parent's rights over its
child's imported declarations, from bug 262), the PROVABLE_V1 scope and bug 226's
std visibility question are decided only by the project owner. They are listed as OD1 – OD21 under
[owner decisions](ROADMAP.md#owner-decisions), each with the exits, deliverables and tasks it gates,
and held by the new Phase 1 exit 2n, which blocks RocksDB slice 1, V1a, V1b and V1c. Agents may
analyse, recommend and ask; they do not choose, encode or implement an answer. The 2026-10-23
tightening record is kept as written; the owner's new answer governs, and decides whether a rule
already on main stays.

Exit 0b's generated bug list (from the bug records' `exit: 0b` lines) now holds every silent bug
the discovery record lists, except the false proofs exit 0a owns, plus the safety repairs the exit
names: 25 records gained the line, among them the silent bugs from 185 to 221. Exit 2m is measured per structural
class: each class needs three quiet counting batches, and a batch that probes a migration's own new
code is tagged `class N: ` and counts toward that class only (a silent bug still counts against its
own class). `check_docs_drift.sh` gains controls for it.

New tasks: R-0550, a compile-time scaling gate (exit 2o, owned with R-0422, reproducers bugs 027,
248 – 250 and the long-function slowdown), and R-0551, continuous leanprof profiling of landed main
commits on concrete-ci-2. The development-throughput section records what is in use, with evidence,
and what remains (R-0546's caches, R-0547's time-to-land measurement).

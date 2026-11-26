# Slice 0–1 fuzzing and parity

Run `LEAN_NUM_THREADS=2 nix develop -c bash scripts/checks/compiler/fuzz_parity.sh [seed [cases]]`
from the repository root. The default seed is 1 and the default campaign is eight cases per
mode per slice. The runner bounds seed, case count, individual subprocesses and reduction
attempts. `CONCRETE_BIR_FUEL`, `CONCRETE_LCB_FUEL` and `CONCRETE_SLICE1_FUEL` select
independent campaign budgets (defaults 32, 512 and 4). Controls use fixed budgets.
Evaluator exhaustion and bounded native timeouts exit 3.
Use `--require-ready` for the pre-slice-2 readiness check. It reports status 3 until production
slice-1 parity is available.

| Comparison | Independent expectation | Actual |
|---|---|---|
| Admitted addition | Sum in 0–255, empty stdout and stderr | Production admission, Lean oracle, BIR and LCB evaluators, LLVM and LCB executables |
| Unchecked addition | Overflow kind, plus-token site and exact trap line, status 134, empty stdout | Admission refusal followed by unchecked lowering, Lean oracle, BIR and LCB evaluators, LLVM and LCB executables |
| Refused addition | Current checker refusal status, code and payload | Production admission pipeline |
| Slice-1 spec only | Destructured moves admit; consumed abort has its exact process observation; unused linear binding at abort refuses | Executable Slice1 elaboration, checker and evaluator on generated ASTs, alongside the checked source witnesses in `ConcreteSpec/Proofs/Surface1.lean` |

A failing slice-0 comparison saves its input, independent expectation, observed outputs, seed,
case index, target and budgets. Mismatch reduction tries at most six smaller operand pairs that
preserve the mode. It retains a smaller input only when the same comparison boundary fails again.
The bundle is provisional until A15/B4 supplies the typed fixture format. The current
checker refusal carries a string and has no rule revision or structured primary and related
origins. Those fields and the Core trap/site cutover belong to A15 and step 4.

## Frozen legacy oracle compatibility

The only eligible legacy identity is the artifact verified against
`scripts/oracle/ORACLE.lock` (`legacy-compiler-final`,
`066c9d9b8836a2b176aa37f6c8313195a4b4ee29`). This campaign uses the Lean oracle. No legacy
observations count toward qualification. A future legacy leg needs positive and negative
qualification against this table and must recheck compatibility if the lock changes.

| Input or observation | Legacy comparison policy |
|---|---|
| Slice-0 closed scalar addition known to stay in u8 range | Candidate overlap for successful exit and empty streams |
| `as` conversion | Excluded. It is removed from the current language. |
| Named linear `move t`, destructuring and empty linear structs | Excluded from legacy comparison. The current slice-1 grammar and ownership model govern. |
| Strict checked arithmetic, including static overflow refusal and unchecked overflow traps | Excluded from legacy comparison. Use current Lean semantics and current trap/refusal observations. |
| Exact abort sites and stderr, refusal revisions and locations | Excluded until an equivalence of the observations is qualified. |
| Unsupported host/profile, evaluator exhaustion or OS timeout | Incomplete. These supply no comparable program result. |

B5 readiness still needs slice-1 production compilation and the A15/B4 typed refusal and
trap interface. The slice-2 arithmetic corpus is due after those rules land, including the
pending E4 shift-count decision. Full parity through slice 4 remains a B0b closure obligation.

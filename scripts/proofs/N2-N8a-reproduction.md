# N8a slice-2a repair and measured certificate controls

Owner: R-0219. For the coordinator’s independent reviewers: **do the best
long-term thing rather than cut corners**. No review tooling was run here.

This record updates the `22f300203` baseline reproduction with the completed
slice-2a structural repair in `scripts/proofs/vacuity.lean` (`namespace Discharge`)
and non-vacuous controls in `scripts/proofs/vacuity_certificates.py`, following the
accepted solver design ([N2-discharge-solver.md](N2-discharge-solver.md)).

## Structural repairs (F1 – F5)

1. **Single ownership of traversal (F4):** `Graph` and `Work` are the sole owner of
   certificate dependency traversal. Closed declaration templates (`compileTemplate`)
   and transfer nodes (`compileNode`) are compiled once over closed terms with de Bruijn
   indices retained. `evalNode`, `evalCall`, `applyValue`, and `openMinorFields`
   evaluate directly over compiled `Transfer` nodes with formal-to-actual `Input`
   and `Origin` substitution across `.call`, `.branch`, `.constructor`, `.application`,
   `.letBind`, `.projection`, `.binder`, and `.forallBinder`. The parallel
   `certificateDependency` walker, `DependencyState`, and `Dependencies` have been
   deleted completely.
2. **`.forallE` and `.lam` distinction and unconditional body validation (F1, F2):**
   `compileNode` compiles `.lam` to `Transfer.binder` and `.forallE` to
   `Transfer.forallBinder`. Binder bodies and codomains are always validated (using
   canonical binder `FVarId`s in `withCanonicalBinder` when unapplied, and actual
   argument `Input`s when applied); the empty-inductive binder shortcut that skipped
   bodies is gone. `structuralExclusions` is recorded only at an actual 0-constructor
   recursor elimination (`.recInfo r` with `r.rules.isEmpty`, such as `False.rec`,
   `Empty.rec`, `PEmpty.rec`) whose major premise has a direct permitted hypothetical
   origin (`isDirectHypotheticalOrigin`).
3. **Genuine `Origin` transfer (F2, F3):**
   - Constant applications construct `Origin.substitution` combining the callee body's
     resulting origin with all actual argument origins (and wrap supplier origins in
     `Origin.substitution` along the call spine on rejection).
   - Closures capture the origins of free de Bruijn indices and permitted hypotheticals
     via `Origin.substitution`.
   - Structure projections and recursor constructor rule matches construct
     `Origin.field parent idx` and propagate field values from `Value.ctor`.
   - Non-constant application heads (`let`, projection, higher-order closure, partial
     call) retain and pass their actual argument `Input`s through `applyValue` rather
     than dropping arguments and opening binders as fresh hypotheticals.
4. **Bodyless axioms (`axiomInfo`) boundary and hypothetical-only `propext` finding (F3, Amendment 2):**
   - The named `INCOMPLETE` recorded in Design Amendment 2
     (`unavailable-certificate-transfer propext`) is preserved unchanged for bodyless
     axioms (` vacuity_solver_boundary.py` passes on
     `singletonCertificate : ∀ x : Nat, x ∈ [2] → x = 2 := by simp`). Neither
     pending trust-policy option is implemented or provisionally admitted.
   - **Precise finding for the coordinator to relay to the owner:** with genuine
     `Origin` transfer implemented, when `propext` is applied to arguments constructed
     exclusively from permitted hypothetical assumptions (`hasProofHyp && allProofsHyp && !anySupplied`,
     as in `eq_true h` for hypothetical `h : False`, `eq_false hn` for hypothetical
     `hn : ¬ p`, and `decide_eq_true_eq hd` in `hypothetical-propext`, `joint-simp`,
     and `f3b-simp-all`) and no safety-bearing demand is made of `propext`'s internal
     proof content, the resulting equality is purely hypothetical-derived (`hasHypotheticalOrigin = true`,
     `hasSuppliedOrigin = false`). Origin transfer alone establishes that this result
     cannot introduce a supplied contradiction without needing axiom-content evidence,
     and `evalCall` never blames `propext` as `rejected-supplied-contradiction supplier=propext`.
     In accordance with instruction (4), we do not admit `propext` before the owner's
     decision; `evalCall` continues to report the exact Amendment 2
     `unavailable-certificate-transfer propext` `INCOMPLETE` on these cases.

## Repaired N8a observation measurements (`vacuity_n8a_repro.py`)

All seven N8a reproducer observations run under the unchanged 20,000-work budget:

| Observation | Baseline (`22f300203`) | Repaired outcome | Work | Terms | Nodes | Templates | Queries | Components |
| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `bad-chain` (`bad1 → bad0`, leaf `fun _ => impossible`) | `admitted` (defect) | `rejected supplier=impossible` | 22 | 8 | 7 | 4 | 4 | 0 |
| `useImpossibleLet` (applied to permitted hypothetical `h : Empty`) | `admitted` (defect) | `rejected supplier=impossible` | 87 | 40 | 36 | 8 | 9 | 0 |
| `directApp` (`suppliedByBool true`) | `rejected supplier=suppliedByBool` | `rejected supplier=suppliedByBool` | 19 | 10 | 9 | 3 | 3 | 0 |
| `letApp` (`(let _ : Nat := 0; fun b => suppliedByBool b) true`) | `unavailable-certificate-transfer suppliedByBool` (defect) | `rejected supplier=suppliedByBool` | 92 | 45 | 39 | 9 | 10 | 0 |
| `cert01` (`¬ (0 = 1) := by decide`) | `false-discharge-budget` (20,000) | `admitted` | 6326 | 1295 | 772 | 62 | 235 | 0 |
| `joint-simp` (permitted `p`, `hp : p`, `hn : ¬ p`) | `unavailable-certificate-transfer propext` | `unavailable-certificate-transfer propext` | 132 | 61 | 49 | 6 | 8 | 0 |
| `F3b-simp` (permitted `y`, `decide (y = 0) = true`, `¬ (y = 0)`) | `false-discharge-budget` (20,000) | `unavailable-certificate-transfer propext` | 1764 | 740 | 539 | 50 | 116 | 0 |

## Non-vacuous certificate controls (`vacuity_certificates.py`)

All 13 certificate controls check their exact planted diagnostic and verify origin
transfer, rank/exclusion counts, and the unchanged 20,000-work budget:

| Control | Expected & actual diagnostic | Work | Terms | Nodes | Templates | Queries | Components |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `origin` (`of_eq_true injectedFalse._simp_1`, entry-arg transfer checked) | `rejected-supplied-contradiction supplier=impossible` | 25 | 9 | 9 | 4 | 4 | 0 |
| `unapplied-empty-binder` (`fun _ : Empty => impossible`) | `rejected-supplied-contradiction supplier=impossible` | 18 | 8 | 7 | 3 | 3 | 0 |
| `use-impossible-let` (`useImpossibleLet h` on `h : Empty`) | `rejected-supplied-contradiction supplier=impossible` | 87 | 40 | 36 | 8 | 9 | 0 |
| `let-app-supplied` (`(let _ : Nat := 0; fun b => suppliedByBool b) true`) | `rejected-supplied-contradiction supplier=suppliedByBool` | 92 | 45 | 39 | 9 | 10 | 0 |
| `non-circular` (`cyclicCaller` with pending `cyclicHelper`) | `INCOMPLETE ... case=non-circular cyclic-certificate-dependency` | 8 | 3 | 3 | 1 | 1 | 0 |
| `dependency-chain-poisoned` (`badDependency80`, leaf `fun _ => impossible`) | `rejected-supplied-contradiction supplier=impossible` | 338 | 87 | 86 | 83 | 83 | 0 |
| `structural-empty-elim` (`Empty.elim h` on `h : Empty`) | `admitted rank=1 exclusions=1` | 40 | 22 | 19 | 3 | 3 | 0 |
| `hypothetical-propext` (`of_eq_true (eq_true h)` on `h : False`) | `INCOMPLETE ... case=hypothetical-propext unavailable-certificate-transfer propext` | 71 | 43 | 34 | 5 | 6 | 0 |
| `library-decide-matcher` (`¬ (0 = 1) := by decide`) | `admitted rank=0 exclusions=0` | 6326 | 1295 | 772 | 62 | 235 | 0 |
| `f3b-decide-matcher` (`hn (of_decide_eq_true hd)`) | `admitted rank=0 exclusions=0` | 3517 | 1050 | 693 | 61 | 191 | 0 |
| `f3b-simp-all` (`simpAll` on `y`, `hd : decide (y = 0) = true`, `hn : ¬ (y = 0)`) | `INCOMPLETE ... case=f3b-simp-all unavailable-certificate-transfer propext` | 1764 | 740 | 539 | 50 | 116 | 0 |
| `dependency-chain-baseline` (`dependency80`, leaf `fun h => h`) | `admitted rank=0 exclusions=0` | 339 | 88 | 86 | 82 | 82 | 0 |
| `shared-budget` (`dependency80` with `spent := 19970`) | `INCOMPLETE ... case=shared-budget false-discharge-budget` | 20000 | 9 | 9 | 6 | 7 | 0 |

### Mutation verification of control non-vacuity

- `python3 -u scripts/proofs/vacuity_n8a_repro.py --remove-cycle-check`: **FAIL** (exit code 1)
  at `non-circular` (`expected ... cyclic-certificate-dependency, got rejected-supplied-contradiction supplier=impossible`).
- `python3 -u scripts/proofs/vacuity_n8a_repro.py --poison-leaf`: **FAIL** (exit code 1)
  at `dependency-chain-baseline` (`expected admitted rank=0 exclusions=0, got rejected-supplied-contradiction supplier=impossible`).

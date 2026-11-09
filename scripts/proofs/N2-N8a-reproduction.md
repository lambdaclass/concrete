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
5. **Round-2 dependency-validation repairs (declaration types, symbolic invariant calls, and minor-premise/closure obligations):**
   - **Instantiated declaration-type validation (`evalDeclaredType` / `evalTypeInstantiate`):**
     Every constant application in `evalCall` validates the callee's instantiated
     `declaredType` via `evalDeclaredType` under the shared 20,000-work budget and
     `pendingQueries` / `completedQueries`, retaining exact argument origins in
     `Origin.substitution` and caching only completed invariant node evaluations
     (`validatedTypes`).
   - **Removal of unchecked symbolic `.invariant`-mode skips:** `skipSymbolicNonProof`
     is removed from `evalCall` (both `hasVal == true` and `.recInfo`), and the
     `.invariant`-mode non-proof shortcut is removed from `.quotInfo`. Symbolic type
     and helper calls in binder domains/codomains are evaluated through the same
     application-sensitive transfer and recursor evaluator.
   - **Same-class dependency paths attempted in Round 2 and correction of the Round-2 constructor/cache claim:**
     In Round 2 (`ccfbfb5f5`), `openMinorFields` opened `ruleMinorArity` and evaluated
     the minor body in `.body` mode; `applyValue` forced direct unapplied
     `Value.partialCall` and `Value.closure` values; and `validateCertificate`
     checked and evaluated each assumption's `input.type`. **Correction (Round 3):**
     the Round-2 text here previously claimed that "unprojected `.ctor` fields" were
     validated. That claim was false: in Round 2 (`ccfbfb5f5`), `.ctorInfo` recorded
     `Value.ctor` and returned `.ok callInput` without completing the recursive
     dependencies of its arguments, and `isUnappliedValue` returned `true` only for
     direct `.closure` and `.partialCall` values (`false` for `.ctor`). Unselected
     constructor fields were checked only when an outer `Expr.proj` directly visited
     that `.ctor` and only when the unselected field was itself a direct `.closure`
     or `.partialCall`. As Codex Round 2 demonstrated, any constructor container that
     was never projected (`boxedType`, `boxedBody`), any nested constructor container
     (`nestedBoxType`), and any closure returning a constructor container
     (`closureReturningBox`) hid unvalidated function bodies and was unsoundly
     admitted. In addition, Round 2 inserted `cKey` into `validatedClosures` *before*
     evaluating the closure or partial call (so a rejected or `INCOMPLETE` validation
     remained cached as completed and was unsoundly admitted on a same-state retry,
     as in `retryFn`), cached only `Input` in `completedQueries` / `validatedTypes`
     without replaying `transfers` and `structuralExclusions`, and overwrote
     unselected minor premises with `{ term := 0, type := 0, origin := 0 }` on
     concrete recursor matches (erasing base minor premises referenced by recursive
     step rules, as in `natRecSuccBasePoison`, and colliding with `TermId = 0`). The
     complete dependency-obligation model and cache protocol that close this class are
     specified in [N2-discharge-solver.md](N2-discharge-solver.md#design-amendment-3-slice-2a-round-3-complete-dependency-obligation-model-and-cache-protocol-of-the-certificate-evaluator).

## Repaired N8a observation measurements (`vacuity_n8a_repro.py`)

All seven N8a reproducer observations run under the unchanged 20,000-work budget:

| Observation | Baseline (`22f300203`) | Repaired outcome | Work | Terms | Nodes | Templates | Queries | Components |
| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `bad-chain` (`bad1 → bad0`, leaf `fun _ => impossible`) | `admitted` (defect) | `rejected supplier=impossible` | 33 | 9 | 7 | 4 | 8 | 0 |
| `useImpossibleLet` (applied to permitted hypothetical `h : Empty`) | `admitted` (defect) | `rejected supplier=impossible` | 127 | 41 | 37 | 9 | 19 | 0 |
| `directApp` (`suppliedByBool true`) | `rejected supplier=suppliedByBool` | `rejected supplier=suppliedByBool` | 267 | 82 | 70 | 15 | 31 | 0 |
| `letApp` (`(let _ : Nat := 0; fun b => suppliedByBool b) true`) | `unavailable-certificate-transfer suppliedByBool` (defect) | `rejected supplier=suppliedByBool` | 358 | 113 | 97 | 20 | 42 | 0 |
| `cert01` (`¬ (0 = 1) := by decide`) | `false-discharge-budget` (20,000) | `admitted` | 9157 | 1538 | 776 | 63 | 595 | 0 |
| `joint-simp` (permitted `p`, `hp : p`, `hn : ¬ p`) | `unavailable-certificate-transfer propext` | `unavailable-certificate-transfer propext` | 182 | 67 | 56 | 8 | 18 | 0 |
| `F3b-simp` (permitted `y`, `decide (y = 0) = true`, `¬ (y = 0)`) | `false-discharge-budget` (20,000) | `unavailable-certificate-transfer propext` | 6441 | 1036 | 602 | 56 | 529 | 0 |

## Non-vacuous certificate controls (`vacuity_certificates.py`)

All 16 certificate controls check their exact planted diagnostic and verify origin
transfer, rank/exclusion counts, and the unchanged 20,000-work budget:

| Control | Expected & actual diagnostic | Work | Terms | Nodes | Templates | Queries | Components |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `origin` (`of_eq_true injectedFalse._simp_1`, entry-arg transfer checked) | `rejected-supplied-contradiction supplier=impossible` | 61 | 17 | 16 | 6 | 13 | 0 |
| `unapplied-empty-binder` (`fun _ : Empty => impossible`) | `rejected-supplied-contradiction supplier=impossible` | 33 | 10 | 8 | 4 | 8 | 0 |
| `use-impossible-let` (`useImpossibleLet h` on `h : Empty`) | `rejected-supplied-contradiction supplier=impossible` | 127 | 41 | 37 | 9 | 19 | 0 |
| `let-app-supplied` (`(let _ : Nat := 0; fun b => suppliedByBool b) true`) | `rejected-supplied-contradiction supplier=impossible/suppliedByBool` (`supplier=suppliedByBool`) | 358 | 113 | 97 | 20 | 42 | 0 |
| `hidden-declaration-type` (`usesHiddenType` calling `hiddenType : (let _ : False := impossible; True)`) | `rejected-supplied-contradiction supplier=impossible` | 35 | 9 | 8 | 5 | 10 | 0 |
| `symbolic-type-poison` (`typePoison : (h : False) → badType False h → True`) | `rejected-supplied-contradiction supplier=impossible` | 49 | 20 | 17 | 4 | 8 | 0 |
| `non-circular` (`cyclicCaller` with pending `cyclicHelper`) | `INCOMPLETE ... case=non-circular cyclic-certificate-dependency` | 17 | 5 | 4 | 2 | 4 | 0 |
| `dependency-chain-poisoned` (`badDependency80`, leaf `fun _ => impossible`) | `rejected-supplied-contradiction supplier=impossible` | 586 | 88 | 86 | 83 | 166 | 0 |
| `structural-empty-elim` (`Empty.elim h` on `h : Empty`) | `admitted rank=1 exclusions=1` | 56 | 23 | 19 | 3 | 6 | 0 |
| `symbolic-type-honest` (`typeHonest : (h : False) → goodType False h → True`) | `admitted rank=0 exclusions=0` | 70 | 26 | 20 | 6 | 12 | 0 |
| `hypothetical-propext` (`of_eq_true (eq_true h)` on `h : False`) | `INCOMPLETE ... case=hypothetical-propext unavailable-certificate-transfer propext` | 133 | 53 | 42 | 7 | 16 | 0 |
| `library-decide-matcher` (`¬ (0 = 1) := by decide`) | `admitted rank=0 exclusions=0` | 9157 | 1538 | 776 | 63 | 595 | 0 |
| `f3b-decide-matcher` (`hn (of_decide_eq_true hd)`) | `admitted rank=0 exclusions=0` | 8279 | 1322 | 755 | 66 | 652 | 0 |
| `f3b-simp-all` (`simpAll` on `y`, `hd : decide (y = 0) = true`, `hn : ¬ (y = 0)`) | `INCOMPLETE ... case=f3b-simp-all unavailable-certificate-transfer propext` | 6441 | 1036 | 602 | 56 | 529 | 0 |
| `dependency-chain-baseline` (`dependency80`, leaf `fun h => h`) | `admitted rank=0 exclusions=0` | 583 | 88 | 86 | 82 | 164 | 0 |
| `shared-budget` (`dependency80` with `spent := 19970`) | `INCOMPLETE ... case=shared-budget false-discharge-budget` | 20000 | 9 | 7 | 4 | 8 | 0 |

### Mutation verification of control non-vacuity

- `python3 -u scripts/proofs/vacuity_n8a_repro.py --remove-cycle-check`: **FAIL** (exit code 1)
  at `non-circular` (`expected ... cyclic-certificate-dependency, got rejected-supplied-contradiction supplier=impossible`).
- `python3 -u scripts/proofs/vacuity_n8a_repro.py --poison-leaf`: **FAIL** (exit code 1)
  at `dependency-chain-baseline` (`expected admitted rank=0 exclusions=0, got rejected-supplied-contradiction supplier=impossible`).

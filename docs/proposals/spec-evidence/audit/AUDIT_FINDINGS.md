# Vacuity audit of accepted contracts at 6157005eb (Codex, Kimi, agy). Raw texts below.

## Codex findings table
**Findings**

Severity concerns acceptance as an independent foundation law; it does not imply the definitions are logically inconsistent.

| Location | Severity | Finding and concrete fix |
|---|---|---|
| `ConcreteSpec/Proofs/Exec.lean:374` — `cleans_iff_runs_plan` | **Medium — WEAK** | The advertised “exactly the plan … and nothing else” characterizes abstract cleanup certificates. Add an inversion theorem over actual completed unwinding executions, with an explicit boundary/first-arrival condition, deriving the planned calls and drops. Retain this equivalence as a helper. |
| `ConcreteSpec/Proofs/Exec.lean:486` — `step_silent_deterministic` | **Low — VACUOUS** | Generic function congruence survives every mutation of `step`. Reclassify as a helper; validate intended transitions through independent rule specifications or mutation-sensitive operational laws. |
| `ConcreteSpec/Proofs/Exec.lean:494` — `step_deterministic` | **Medium — VACUOUS** | The determinism envelope follows from the representation of `Step`. Add correspondence with an independently stated declarative transition relation, then establish its determinism up to fixed environment answers. |
| `ConcreteSpec/Proofs/Exec.lean:635` — `destroy_not_copy` | **Low — VACUOUS** | Restates `D.notCopy`. Treat it as an interface projection; discharge the property for concrete destruction admission/checking without accepting the same proposition as an input field. |
| `ConcreteSpec/Proofs/Scalar.lean:255` — `machine_operands` | **Low — VACUOUS** | Extracts `operandsOk`’s successful guard. Keep operand refusal checks, but classify this as guard characterization; prove admitted runtime operands have the required ranges from typing/evaluation evidence independently of that guard. |
| `ConcreteSpec/Proofs/Scalar.lean:340` — `checkedNeg_inRange` | **Low — VACUOUS** | Output range is explicitly checked by `inRangeOr`. Add the arithmetic characterization: for valid input, success occurs exactly when mathematical `-a` is representable, returns `-a`, and otherwise produces overflow. |
| `ConcreteSpec/Proofs/Const.lean:205` — `admitted_copy` | **Medium — VACUOUS** | Extracts an explicit declaration-evaluation guard. Derive Copy from successful independent static typing and declared/value type agreement; keep this evaluation-guard lemma as a helper. |
| `ConcreteSpec/Proofs/Const.lean:220` — `ofC_copy` | **Low — VACUOUS** | Directly restates `U.ofC_copy`. Classify as an interface projection and require concrete type-universe instances to establish scalar Copy classification independently. |
| `ConcreteSpec/Proofs/Memory.lean:460` — `access_some` | **Low — VACUOUS** | Expands the successful access branch’s guards. Retain as an inversion helper; separately prove success from independently established access obligations, including nonempty concrete witnesses and refusal cases. |
| `ConcreteSpec/Proofs/Memory.lean:483` — `access_valid` | **Low — VACUOUS** | Target validity is explicitly tested in both access branches. Classify as guard extraction; establish validity of pointers produced by admitted pointer operations independently of access’s validation guard. |
| `ConcreteSpec/Proofs/Memory.lean:502` — `decodePtr_valid` | **Low — VACUOUS** | Decoder tests precisely the returned pointer’s validity. Keep the defensive check; derive validity from independently well-formed stored-pointer fragments, rather than crediting this theorem as that derivation. |
| `ConcreteSpec/Proofs/Observe2.lean:29` — `Responds.allows` | **Low — VACUOUS** | Constructor premise is the conclusion. Classify as relation inversion; use concrete adapter/oracle validation and `runFrom_validTrace` for behavioral evidence. |
| `ConcreteSpec/Proofs/Observe2.lean:36` — `Responds.matches` | **Low — VACUOUS** | Constructor premise is the conclusion. Classify as relation inversion; establish matching from concrete response handling or typed adapter construction. |
| `ConcreteSpec/Proofs/Observe2.lean:515` — `report_noAnswer_mayBlock` | **Low — VACUOUS** | Instantiates the definition of its `BlockingPermitted` assumption. Keep as a helper; obtain permission through the independently justified run/adapter theorem, as `runFrom_blockingPermitted` is intended to do after the merge fix. |

VERDICT: FINDINGS
## Kimi
  - **Low — `ConcreteSpec/Proofs/Exec.lean:486` — `step_silent_deterministic` is VACUOUS.** Its proof is just rewriting `h₁` into `h₂` and injecting `some`; it holds for every function `step`, including one with arbitrarily wrong execution rules. **Fix:** remove it as a standalone contract law and rely on `step_deterministic`, or replace it with a rule-constraining characterization such as `Step P c .silent c' ↔ step P c = some (.next c')` and keep the `some`-injectivity result only as an unadvertised helper.

  No other VACUOUS or WEAK contract theorem or registered example was found.

  VERDICT: FINDINGS

To resume this session: kimi -r session_bd2dd1f4-72cd-4c82-8b89-3c44782e5b63
rc=0

## agy C1
### Findings (VACUOUS and WEAK)

1. **`continuation_refused` (VACUOUS) and `prim_bind_refused` / `refusingOps` examples (WEAK)**
   - **Location**: `ConcreteSpec/Proofs/Exec.lean:581-598`, `ConcreteSpec/Spec/Exec.lean:483-498`
   - **Severity**: High
   - **Problem**: `continuation_refused` claims "A refused continuation admits no successful response transition for that answer", and `Spec/Exec.lean` states "`Step.prim` requires success, and `runFrom` reports the requesting configuration as stuck without accepting the response event when continuation fails." Yet `continuation_refused` only proves `cont a = none → ¬ ∃ c', cont a = some c'` on a bare function `cont` without referencing `Step` or `runFrom`. Furthermore, `prim_bind_refused` states in its docstring that error answers bypass binding and remain traps and that the oracle's answer kind is unchanged, but proves neither; and the `refusingOps` examples in `Spec/Exec.lean` never test primitive result-binding refusal in `runFrom`.
   - **Concrete fix**:
     1. Replace `continuation_refused` with a theorem over `Step` and `runFrom`:
        ```lean
        theorem step_prim_continuation_refused {P : Program O} {c : Config O}
            {p : O.Prim} {vs : List O.Val} {site : SiteId} {st : O.Store}
            {cont : Answer O.Val O.Event O.Store → Option (Config O)}
            (hreq : step P c = some (.request p vs site st cont)) :
            (∀ a, O.allows p vs st a = true → cont a = none) →
            ∀ l c', ¬ Step P c l c' := by
          intro href l c' hs
          cases hs with
          | silent hs => rw [hreq] at hs; cases hs
          | prim hs hall hk =>
            rw [hreq] at hs; cases hs
            rw [href _ hall] at hk; cases hk

        theorem runFrom_continuation_refused (P : Program O) (ρ : Oracle O)
            (n : Nat) (hist : List O.Event) (c : Config O)
            (p : O.Prim) (vs : List O.Val) (site : SiteId) (st : O.Store)
            (cont : Answer O.Val O.Event O.Store → Option (Config O))
            (a : Answer O.Val O.Event O.Store)
            (hfin : c.final? = none)
            (hreq : step P c = some (.request p vs site st cont))
            (horacle : ρ hist p vs site st = some a)
            (hallows : O.allows p vs site_or_st ... = true) -- O.allows p vs st a = true
            (href : cont a = none) :
            runFrom P ρ (n + 1) hist c = ⟨hist, .stuck c⟩ := by
          simp [runFrom, hfin, hreq, horacle, hallows, href]
        ```
     2. Add the error-bypass conjunct to `prim_bind_refused`:
        ```lean
        ∀ kd, a.result = .error kd → cont a = some ⟨.trapped kd site, L, a.store, k⟩
        ```
     3. Add a `refusingOps` example in `Spec/Exec.lean` where `.bind x` is refused on `.prim (some x) .log [.lit 1] s` under `env`, verifying `run` returns `⟨[], .stuck _⟩` (not `.badAnswer` and without appending `1` to `events`).

2. **`destroy_not_copy` (VACUOUS)**
   - **Location**: `ConcreteSpec/Proofs/Exec.lean:614-616`
   - **Severity**: Medium
   - **Problem**: `destroy_not_copy` (`:= D.notCopy T f h`) merely projects the `notCopy` proof field out of the `DestroyProtocol` structure, restating its definition. Moreover, no `DestroyProtocol` instance is exhibited in `Demo` to witness that the protocol axioms are jointly satisfiable.
   - **Concrete fix**: Either define a decidable validity predicate `DestroyProtocol.wf` checked on a concrete function table and prove `destroy_not_copy` from it, or at minimum instantiate `DestroyProtocol Demo.prog` for `.tok` (`impl | .tok => some eatF | _ => none`) in `Demo` and remove or demote the trivial field-projection theorem.

3. **`enter_refused` (VACUOUS) and `assign_refused`, `call_refused`, `test_drop_refused` (WEAK)**
   - **Location**: `ConcreteSpec/Proofs/Exec.lean:520-568`
   - **Severity**: Low
   - **Problem**:
     - `enter_refused` merely unfolds `Option.map` on `none` in `enter`.
     - `assign_refused`, `call_refused`, and `test_drop_refused` omit the predecessor guards (`AList.lookup L x = some v₀`, `fn.params.length = vs.length`, and `O.test v = some b`), so they hold even in states where `step` already returns `none` due to an unbound variable, wrong argument count, or non-boolean test value before `O.life` is ever called.
   - **Concrete fix**: Add the predecessor success hypotheses (`hx : AList.lookup L x = some v₀` in `assign_refused`, `hlen : fn.params.length = vs.length` in `call_refused`, `ht : O.test v = some b` in `test_drop_refused`) and show that with `O.life ... = some st''` the step succeeds (`step P ... = some (.next ...)`), whereas with `O.life ... = none` it returns `none` (an `iff` characterization: `step P c = none ↔ O.life ... = none` under the predecessor premises).

VERDICT: FINDINGS
rc=0

## agy C2
### Findings (VACUOUS and WEAK)

1. **[`ConcreteSpec/Proofs/Const.lean:199`](file:///home/unbalancedparen/ConcreteSpec/Proofs/Const.lean#L199) (`ofC_copy`) — VACUOUS (Severity: Medium)**
   - **Why:** [`ConstTypes`](file:///home/unbalancedparen/ConcreteSpec/Spec/Const.lean#L117-L123) bakes in `ofC_copy : ∀ c, isCopy (ofC c) = true` as a structure field, and `theorem ofC_copy` in `Proofs/Const.lean` is the one-line restatement `U.ofC_copy v.ty`.
   - **Concrete fix:** Instead of assuming `ofC_copy` as an uninterpreted axiom on `ConstTypes`, define `isCopy` on `CTy` (`def CTy.isCopy : CTy → Bool | .int _ | .bool | .unit => true`) or require `ConstTypes.isCopy` to agree with `CTy.isCopy` and prove by cases on `v : CVal` (or `c : CTy`) that every scalar type in `CTy` is `Copy`.

2. **[`ConcreteSpec/Proofs/Const.lean:185-196`](file:///home/unbalancedparen/ConcreteSpec/Proofs/Const.lean#L185-L196) (`admitted_copy`) — VACUOUS (Severity: Medium)**
   - **Why:** [`evalConsts`](file:///home/unbalancedparen/ConcreteSpec/Spec/Const.lean#L141-L149) checks the explicit runtime guard `if U.isCopy d.ty = true ∧ U.ofC v.ty = d.ty ∧ ...`, and `admitted_copy` merely projects `hc.1` from that guard. Moreover, because `U.ofC_copy` forces every `U.ofC c` to satisfy `isCopy = true`, the second conjunct `U.ofC v.ty = d.ty` already makes `U.isCopy d.ty = true` hold automatically.
   - **Concrete fix:** Either (a) drop the redundant `U.isCopy d.ty = true` guard from `evalConsts` (and `typeConsts`) and prove `admitted_copy` from `U.ofC v.ty = d.ty` and scalar `Copy`-ness (`rw [← hc.1]; exact ...`), or (b) if `ConstTypes` is meant to test admission against non-`Copy` types in `CTy` when widened, do not hardcode `ofC_copy : ∀ c, isCopy (ofC c) = true` on `ConstTypes` so the `U.isCopy d.ty = true` check in `typeConsts`/`evalConsts` does real work.

3. **[`ConcreteSpec/Spec/Const.lean:161-162`](file:///home/unbalancedparen/ConcreteSpec/Spec/Const.lean#L161-L162) and [`ConcreteSpec/Spec/Const.lean:198-199`](file:///home/unbalancedparen/ConcreteSpec/Spec/Const.lean#L198-L199) (`ConstsAdmitted` and its non-`Copy` Refused example) — WEAK (Severity: Low)**
   - **Why:**
     1. The specification text ([`Spec/Const.lean:38-41`](file:///home/unbalancedparen/ConcreteSpec/Spec/Const.lean#L38-L41)) states that static admission is decided by typing (`typeConsts`) without evaluating, and that evaluation of an admitted list therefore cannot fail; defining `ConstsAdmitted U ds` as `(evalConsts U ds []).isSome = true` inverts this so that "admitted implies evaluation succeeds" is the definition rather than the theorem.
     2. The Refused example `¬ ConstsAdmitted U [⟨cid "S", .text, .unit⟩]` ([`Spec/Const.lean:199`](file:///home/unbalancedparen/ConcreteSpec/Spec/Const.lean#L199)) purports to test refusal of a non-`Copy` type `.text`, but its initializer `.unit` has type `.c .unit ≠ .text`, so it is refused by the type-equality check `U.ofC T = d.ty` even if `isCopy .text` is changed to `true`.
   - **Concrete fix:** Define `ConstsAdmitted U ds := (typeConsts U ds []).isSome = true` so that `typed_evaluates` / `evalConsts_sound` proves that statically admitted declarations always evaluate at compile time without failure; and if `U.isCopy d.ty = true` is meant to be independently testable by a Refused example, allow an uninterpreted constant/type in `ConstTypes` whose `isCopy` is `false` while matching `d.ty`, or derive `admitted_copy` from `U.ofC T = d.ty` without claiming the `.text` / `.unit` example isolates `isCopy`.

VERDICT: FINDINGS
rc=0

## agy C4
- Accepted/Refused examples in [Spec/Memory.lean](file:///home/unbalancedparen/ConcreteSpec/Spec/Memory.lean#L262-L333) and [Spec/Hybrid.lean](file:///home/unbalancedparen/ConcreteSpec/Spec/Hybrid.lean#L178-L214): **NON-VACUOUS** — All execute concrete traces decided by kernel evaluation, except the `decodePtr` sub-case in [Spec/Memory.lean](file:///home/unbalancedparen/ConcreteSpec/Spec/Memory.lean#L266) which relies on `decodePtr`'s `p.Valid` guard (covered in the findings below).

---

### Findings (VACUOUS and WEAK Laws)

1. **`decodePtr_encodePtr` and `decodePtr_valid` hold by runtime guards in `decodePtr`**
   - **Location**: [Spec/Memory.lean:248-258](file:///home/unbalancedparen/ConcreteSpec/Spec/Memory.lean#L248-L258), [Proofs/Memory.lean:102-115](file:///home/unbalancedparen/ConcreteSpec/Proofs/Memory.lean#L102-L115), [Proofs/Memory.lean:494-505](file:///home/unbalancedparen/ConcreteSpec/Proofs/Memory.lean#L494-L505)
   - **Verdict / Severity**: **VACUOUS** (High)
   - **Why**: `encodePtr` takes an unused `(_valid : p.Valid tgt)` premise, and `decodePtr` does not decode the octets at all—it pulls `p` out of the first fragment `.init _ (some (p, 0))` and runs `if hp : p.Valid tgt then if bs = encodePtr tgt p hp then some p else none else none`. Thus `decodePtr_valid` holds trivially by extracting the guard `hp`, and `decodePtr_encodePtr` holds by reflexivity of `bs = encodePtr tgt p hp` without ever using `leValue_leBytes` or `ordered_ordered` (if `encodePtr` wrote all-zero octets, both theorems would still compile unchanged).
   - **Concrete fix**: Remove `_valid : p.Valid tgt` from `encodePtr`. Define `decodePtr` to check `bs.length = tgt.ptrSize`, extract `p` from fragment `0`, check that fragments `0 .. tgt.ptrSize - 1` all equal `some (p, i)`, check `p.space ∈ tgt.spaces`, and check `leValue (ordered tgt.byteOrder os) = p.addr` (where `octets bs = some os`). Then prove `decodePtr_valid` under `ht : tgt.WellFormed` using `leValue_lt` and `8 * tgt.ptrSize = tgt.addrBits`, and prove `decodePtr_encodePtr` using `ordered_ordered` and `leValue_leBytes`.

2. **`alloc_failure` and `access_zero` restate definitions verbatim**
   - **Location**: [Proofs/Memory.lean:228-234](file:///home/unbalancedparen/ConcreteSpec/Proofs/Memory.lean#L228-L234)
   - **Verdict / Severity**: **VACUOUS** (Medium)
   - **Why**: `alloc_failure` assumes `¬ m.Fits tgt (candidate s base size align)`—the exact negation of `Memory.alloc`'s `if`-guard—and restates its `else` branch. `access_zero` restates the `n = 0` branch of `Memory.access` word-for-word (`rfl`).
   - **Concrete fix**: Replace `alloc_failure` with concrete failure laws required by the contract prose (e.g., if a live block `x` in `m` overlaps `candidate s base size align`, or `base % align ≠ 0`, or `base + size > 2 ^ tgt.addrBits - 1`, then `m.alloc tgt s base size align = .error .unavailable`), and replace `access_zero` with laws relating `m.load tgt p 0 a = some []` / `m.store tgt p [] a = some m` to `p.addr % a = 0 ∧ p.addr ≠ tgt.null ∧ p.Valid tgt`.

3. **`access_valid` holds in the nonempty case via a redundant `p.Valid tgt` guard in `Memory.access`**
   - **Location**: [Spec/Memory.lean:176-178](file:///home/unbalancedparen/ConcreteSpec/Spec/Memory.lean#L176-L178), [Proofs/Memory.lean:476-490](file:///home/unbalancedparen/ConcreteSpec/Proofs/Memory.lean#L476-L490)
   - **Verdict / Severity**: **WEAK** (Medium)
   - **Why**: In a `WellFormed` memory `m`, any nonempty in-bounds access to a block `x` (`x.base ≤ p.addr`, `p.addr + n ≤ x.base + x.size` with `n ≠ 0`, `x.space = p.space`) already satisfies `p.Valid tgt` by `hw.fits` (`x.base + x.size ≤ 2 ^ tgt.addrBits - 1`) and `hw.spaces` (`x.space ∈ tgt.spaces`). Because `Memory.access` appends `∧ p.Valid tgt` to the `n ≠ 0` branch anyway, `access_valid` does not take `hw : m.WellFormed tgt` and merely projects the runtime guard.
   - **Concrete fix**: Remove `∧ p.Valid tgt` from the `n ≠ 0` branch of `Memory.access`, add `(hw : m.WellFormed tgt)` to `access_valid`, and prove `p.Valid tgt` in the `n ≠ 0` case from `hw.fits`, `hw.spaces`, and the bounds inequalities `x.base ≤ p.addr` and `p.addr + n ≤ x.base + x.size`.

4. **`typed_store_frame` does not reference `State.write` or `State`**
   - **Location**: [Proofs/Hybrid.lean:91-100](file:///home/unbalancedparen/ConcreteSpec/Proofs/Hybrid.lean#L91-L100)
   - **Verdict / Severity**: **WEAK** (High)
   - **Why**: The contract and docstring state that a typed write (`State.write`) preserves all padding bytes outside `sizeof c.ty`. However, `typed_store_frame` takes `h : m.store tgt r.ptr (encode tgt v) r.align = some m'` on raw `Memory` and is just a thin alias for `store_same_block_frame` with `bs := encode tgt v`. If `State.write` were mutated to clobber trailing padding in `r`, `typed_store_frame` would still hold.
   - **Concrete fix**: Restate `typed_store_frame` over `State.write`: given `hw : st.WellFormed tgt`, `hc : st.places p = some ⟨t, .materialized r⟩`, `h : st.write tgt p v = some st'`, and `i` in the padding range (`sizeof t ≤ i`), prove that for the block `b` of `r`, `(st'.memory.block b).map (fun y => y.bytes[i]?) = (st.memory.block b).map (fun x => x.bytes[i]?)`.

VERDICT: FINDINGS
rc=0

## agy C6
### Findings (VACUOUS and WEAK)

1. **`ConcreteSpec/Proofs/Observe2.lean:27` & `34` — `Responds.allows` and `Responds.matches` (VACUOUS, Low)**
   * **Why:** Both theorems merely project the arguments stored directly in the single `.response` constructor of `Responds`.
   * **Concrete fix:** Either demote them to unlabeled projection helpers or replace them with a theorem relating `Responds` to `ValidTrace` extension: given `CompleteTrace C input hist` and `Responds C input (hist ++ [.request q]) q (.response r)`, prove `CompleteTrace C input (hist ++ [.request q, .response r])`.

2. **`ConcreteSpec/Proofs/Observe2.lean:319` — `trap_line_uses_byte_columns` (VACUOUS, Medium)**
   * **Why:** `Span.HasByteColumn sp before` is defined as `sp.col = byteColumn before` (`Spec/Observe.lean:105`), and the theorem just rewrites `sp.col` to `byteColumn before` inside the unfolded definition of `trapLine`.
   * **Concrete fix:** Define source-text span construction from a line prefix `before : String` (computing the column via UTF-8 bytes) and prove that for multi-byte UTF-8 characters (or tabs) where `before.utf8ByteSize ≠ before.length`, the decoded column from `decodeTrapLine (trapLine kind site sp)` equals `before.utf8ByteSize + 1` and differs from the scalar character column `before.length + 1`.

3. **`ConcreteSpec/Proofs/Observe2.lean:367` & `420` — `runFrom_validTrace` and `runFrom_noAnswer_pending_valid` (WEAK, High)**
   * **Why:**
     1. Does not account for C1's partial continuation (`k` returning `Option (Exec.Config O)` / `badAnswer` when `k` refuses the answer).
     2. In `runFrom_validTrace` and `runFrom_noAnswer_pending_valid`, the fact that `ending = .noAnswer c'` implies `∃ p vs site store k, Exec.step P c' = some (.request p vs site store k) ∧ ρ events p vs site store = none` is not proved; instead `Exec.step P c' = some (.request ...)` is assumed as an external hypothesis, so if `runFrom` ever produced `.noAnswer c'` at a non-request state, the theorems would hold vacuously!
   * **Concrete fix:** Update `k` for C1's partial continuation (handling the `k a = none` branch in `runFrom`), strengthen `ExecAdapter.WellFormed` so every C1 request where `ρ` returns `none` has `∃ q, A.request p vs site store = some q`, and prove in `runFrom_validTrace` that `(Exec.runFrom P ρ fuel hist c).ending = .noAnswer c'` *implies* `∃ p vs site store k q, Exec.step P c' = some (.request p vs site store k) ∧ A.request p vs site store = some q ∧ C.mayBlock ...` (existentially producing the witness rather than assuming it in a universal implication).

4. **`ConcreteSpec/Proofs/Observe2.lean:450` — `output_before_trap_kept` (VACUOUS, High)**
   * **Why:** In `Spec/Observe.lean:245`, `stepsObservation` ignores its `_steps : Exec.Steps P c events c'` argument completely and just projects `events`. Thus `output_before_trap_kept` holds by `simp` unfolding `stepsObservation` even if `events` had nothing to do with `steps` or if a multi-step prefix emitted `events₁` before trapping with `events₂`.
   * **Concrete fix:** State the theorem over sequential composition of `Exec.Steps`: given `s₁ : Exec.Steps P c₁ events₁ c₂` and `s₂ : Exec.Steps P c₂ events₂ c₃` where `c₃` traps (`c₃.final? = some (.trapped kind site)`) and `site` resolves to `sp`, prove `stepsObservation A (s₁.trans s₂) = some ⟨A.project events₁ ++ A.project events₂, .trapped kind site sp (trapLine kind site sp)⟩`, showing that events from earlier steps `s₁` are preserved as a prefix before the trap in `s₂`.

5. **`ConcreteSpec/Proofs/Observe2.lean:459`, `470`, `482` — `report_noAnswer_mayBlock`, `report_noAnswer_blocked`, and `report_outOfFuel_none` (VACUOUS, High)**
   * **Why:** `reportObservation` (`Spec/Observe.lean:261`) takes an unused proof argument `(_permitted : ExecAdapter.BlockingPermitted P A C input r)`. `report_noAnswer_mayBlock` literally just applies `permitted rfl pending typed` (restating the definition of `BlockingPermitted`), and `report_noAnswer_blocked` / `report_outOfFuel_none` just unfold the `match r.ending` branches of `reportObservation` on synthetic reports `⟨events, .noAnswer c⟩` and `⟨events, .outOfFuel c⟩`.
   * **Concrete fix:** Connect `reportObservation` to actual executions (`Exec.runFrom P ρ fuel hist c`) under `wellFormed : ExecAdapter.WellFormed A C input ρ` and `valid : CompleteTrace C input (A.project hist)`: prove that when `reportObservation P A C input (Exec.runFrom P ρ fuel hist c) (runFrom_blockingPermitted ...)` returns `some (.terminal obs)`, `CompleteTrace C input obs.events` holds; when it returns `some (.blocked events q)`, `ValidTrace C input (some q) events ∧ C.mayBlock input events q` holds; and when `runFrom` ends in `.outOfFuel`, `.stuck`, or `.badAnswer`, `reportObservation` returns `none`.

VERDICT: FINDINGS
rc=0

## Added from the review of the integration fix (Codex, 2026-11-08, on 07c16bb5)
Findings:

- **ConcreteSpec/Proofs/Observe2.lean:518 — major, REAL DEFECT (vacuity).** `report_noAnswer_mayBlock` merely specializes the definition of its `BlockingPermitted` hypothesis: the proof is `permitted rfl pending typed`. The new positive example at line 708 reduces its conclusion to `True`; the negative example changes that definition to `False`. These examples do not establish an independent behavioral property. **Concrete fix:** retain the required statement, but add a substantive companion law deriving permission from a reachable evaluator run and independently specified environment policy, with a planted oracle that blocks a forbidden request and fails well-formedness. Identify this existing theorem as a projection lemma rather than independent validation.

- **ConcreteSpec/Spec/Observe.lean:446 — major, REAL DEFECT (vacuity).** `refusedReportPermitted` holds exclusively because its `noAnswer` antecedent cannot hold. The proof explicitly eliminates that impossible antecedent. This is legitimate bookkeeping for calling `reportObservation`, but it fails the owner’s stated criterion if counted as a law. **Concrete fix:** make the substantive refusal law expose the actual request, allowed answer, failed continuation, exact `.stuck refusedRequest` ending, and unchanged history together. Keep the vacuous permission evidence as local bookkeeping, and add a paired successful-continuation example that records the event.

- **ConcreteSpec/Proofs/Observe2.lean:530 — major, REAL DEFECT under the binding definition-restatement rule.** `report_noAnswer_blocked` unfolds exactly the corresponding branch of `reportObservation`; `blockedObservation` at line 720 and its repeated example inherit that limitation. Comparing the result with an unrelated terminal constructor demonstrates constructor discrimination, not independent correctness of the adapter projection. **Concrete fix:** preserve this required computation lemma and supplement it with a reachable-run observation law using a nonempty prior history. Plant a dropped/reordered-history or wrong-pending-request result and prove it fails.

Vacuity audit of the changed and newly introduced named laws:


## C6 full-access Codex re-review (738500fb vs 5bc0748b; replaces the read-only review.sh acceptance) — VERDICT: CHANGES

VERDICT: CHANGES is warranted. The stuttering and trap-line injectivity proofs are substantive, but C6 still has contract gaps and vacuous laws under the owner’s binding rule.

I reviewed the full `REVIEW.diff` and surrounding definitions, C1 execution machinery, cited decision rows, and `_review/FOUNDATION_PROPOSAL.md` §C6. The requested `docs/proposals/FOUNDATION.md` is absent from this checkout. No files were modified; no network, builds, gates, or writing Git commands were used. Witnesses and planted faults below were checked by source reasoning, not executed.

**Findings**

1. **ConcreteSpec/Spec/Observe.lean:161 — major — REAL DEFECT: final external state is omitted.**

   C6 explicitly requires preservation of “externally visible final state.” `Observation` contains only events and a terminal action. `Execution`, `SameObservation`, and the C1 adapter likewise have no final external-state projection or preservation obligation.

   Concrete counterexample: two executions perform the same recorded operation and exit with the same status, but leave different externally visible file contents. An operation contract can currently allow both responses without constraining the resulting external state. C6 observations identify those executions.

   **Fix:** add an abstract external-state observation and its relationship to operation effects and terminal execution state. Preserve it in observation equivalence and adapter laws. Alternatively, formally require that the event history determines every externally visible final-state component, and prove that requirement for adapters; the current arbitrary `events` mapping supplies no such guarantee.

2. **ConcreteSpec/Proofs/Observe2.lean:370 — major — REAL DEFECT: byte-column correctness is assumed.**

   Confirmed known finding. `Span.HasByteColumn sp before` is exactly `sp.col = byteColumn before`. The theorem substitutes that assumed equality into a renderer that already prints `sp.col`.

   An elaborator producing scalar column `4` for `"a\té"` escapes this law: the hypothesis becomes false. The existing numeric example establishes that `byteColumn` returns `5`; it does not establish that a produced span uses it.

   **Fix:** define source-position construction from the relevant source prefix and prove its rendered column independently. Include `"a\té"` as a positive witness and a planted scalar-counting implementation as a rejection control. Keep source-to-span correctness explicitly open until that connection exists.

3. **ConcreteSpec/Proofs/Observe2.lean:29 — major — REAL DEFECT: environment laws extract their conclusions from admission premises.**

   Confirmed for both `Responds.allows` and `Responds.matches`. The `Responds.response` constructor requires precisely `C.allows` and `RequestResponse`; these theorems only recover those constructor arguments.

   A bad answer cannot challenge either law because it cannot satisfy the admission hypothesis. The doubling examples demonstrate admission and rejection by the definition, but do not make these two laws independent correctness evidence.

   The same definition-only issue affects `RequestResponse.kind` at line 23: the constructor has already required a common kind.

   **Fix:** distinguish admission facts from correctness laws. Establish matching and contract compliance for a separately defined response producer or concrete adapter, with accepted responses and planted mismatched/disallowed responses. Constructor projections may remain useful helpers, but cannot count as the required non-vacuous laws.

4. **ConcreteSpec/Proofs/Observe2.lean:506 — major — REAL DEFECT: output preservation is built into the observation definition.**

   `stepsObservation` ignores its execution proof and directly installs `A.project events` in the result. `output_before_trap_kept` unfolds that definition. It establishes neither that the adapter preserves a C1 output nor that an independently defined execution observation retains it. In particular, `A.events := fun _ => []` erases every output and still satisfies this theorem.

   **Fix:** give externally relevant C1 output an independent correspondence requirement, provide a realizable output-then-trap adapter instance, and prove its operational observation retains the expected output. A planted adapter that drops that output must fail the obligation.

5. **ConcreteSpec/Proofs/Observe2.lean:515 — major — REAL DEFECT: the report laws do not independently establish their claims.**

   Confirmed known findings:

   - `report_noAnswer_mayBlock` directly applies `BlockingPermitted`, whose definition already states the conclusion.
   - `report_noAnswer_blocked` unfolds the `.noAnswer` projection.
   - `report_outOfFuel_none` is the literal `.outOfFuel` branch of `reportObservation`.

   `BlockingPermitted` is realizable for contracts allowing blocking; therefore these are not *universally impossible-hypothesis* defects. Nevertheless, for a concrete nonblocking request, permission cannot be supplied, so that case cannot challenge `report_noAnswer_mayBlock`.

   **Fix:** prove end-to-end results for reports actually produced by `runFrom`, starting with independently specified environment behavior. Supply a blocking instance, a nonblocking missing-answer rejection case, and a fuel-exhaustion case. Treat the current equations as projection helpers rather than contract evidence.

   `refusedReportPermitted` is absent from this head; that named finding cannot be reproduced as a declaration here.

6. **ConcreteSpec/Proofs/Observe2.lean:266 — major — REAL DEFECT under the binding vacuity rule: divergence exclusion is definition-only.**

   `infinite_not_termination` and its silent specialization unfold the mixed-constructor branch of `SameObservation`, which is literally `False`. The demo at `Spec/Observe.lean:395` repeats this.

   This is a useful representation distinction, but the theorem cannot detect an adapter or transformation that incorrectly classifies an infinite computation as finite: it assumes the classifications as its inputs.

   **Fix:** retain the constructor distinction, but establish classification preservation for an independently defined execution transformation, including a silently divergent witness and a planted transformation that replaces it with successful termination. C8 adequacy can remain separate; this theorem should not count as operational classification evidence.

7. **ConcreteSpec/Spec/Observe.lean:286 — major — REAL DEFECT: adapter well-formedness does not require valid trap paths.**

   `SiteTable.Valid` is defined but never required by `ExecAdapter.WellFormed`, `terminal?`, or either observation function. A well-formed adapter can resolve a trap to `"/tmp/main.con"` or `"src\\main.con"` and produce a terminal observation violating C6’s project-relative, forward-slash requirement.

   **Fix:** include site-table validity in the adapter’s semantic premises and establish path validity for reachable trap observations. Keep the stronger source-position/elaboration obligation under O1.

**Known finding reassessment**

`runFrom_validTrace` at `Observe2.lean:419` is **not wholly vacuous**. Its trace component follows the actual evaluator recursively, appends projected answers, and constructs valid request/response traces. Dropping its recursive history extension or reversing request/response order would break that argument. Its blocking component propagates the permission already required by `WellFormed.blocked`; it does not independently validate that premise.

**Every-law vacuity audit**

`P` means substantive; `V` means vacuous under the supplied rule. Witnesses describe nontrivial instances. “Fault” identifies a planted change that should fail the property. For `V`, the stated guard or definition prevents an independent counterexample; that is the defect. All lines below are in `ConcreteSpec/Proofs/Observe2.lean`.

| Line / law | Verdict | Witness and planted fault |
|---|---|---|
| 23 `RequestResponse.kind` | V | Same-kind doubling response is inhabited; a cross-kind response is excluded by the constructor. Finding 3. |
| 29 `Responds.allows` | V | `3 → 6` admitted; `3 → 7` cannot satisfy admission. Finding 3. |
| 36 `Responds.matches` | V | Matching response admitted; cross-kind response cannot satisfy admission. Finding 3. |
| 43 `SilentInsertion.visible` | P | Insert silence between two distinct outputs. Fault: insert an output instead. |
| 52 `silent_internal_steps` | P | Insert leading, middle and trailing silence around two events. Fault: reorder the events. |
| 57 `blocked_silent_internal_steps` | P | Stutter an output prefix before pending request `q`. Fault: add visible output or change `q`. |
| 63 `PrefixEventually.refl` | P | An execution with both silent and emitting steps, including repeated events. Fault: discard a visible prefix in the compared stream. |
| 68 `PrefixEventually.trans` | P | Compose two streams with successively delayed matching emissions. Fault: lose or reorder a required prefix in the final stream. |
| 75 `SameInfiniteObservation.refl` | P | Alternating silence and output; relies on prefix reflexivity. Fault: corrupt visible-prefix extraction. |
| 78 `SameInfiniteObservation.symm` | V | The definition is explicitly the conjunction of both directions; this only swaps its fields. Retain as a packaging helper, not contract evidence. |
| 81 `SameInfiniteObservation.trans` | P | Compose two distinct stutterings. Fault: replace a required event in the third stream. |
| 86 `visiblePrefix_succ` | P | Extend an emitting prefix, then a silent prefix. Fault: omit or duplicate the last event. |
| 92 `visiblePrefix_prefix` | P | Compare prefixes crossing multiple silent/emitting steps. Fault: reverse the accumulated event order. |
| 103 `StepIndexIncreasing.monotone` | P | `f n = 2*n+3`, separated indices. Fault: replace the derived monotonicity bound with a strict bound at equal indices. |
| 113 `StepIndexIncreasing.strict` | P | Same map, nonadjacent indices. Fault: reverse the claimed inequality. |
| 120 `StepIndexIncreasing.apply_le` | P | Offset and stretched increasing maps. Fault: strengthen the conclusion to `n < f n`; identity rejects it. |
| 128 `InfiniteStuttering.prefix_indices` | P | Retain old indices at `2*n+1`. Fault: emit at an unretained index or introduce an off-by-one prefix boundary. |
| 169 `InfiniteStuttering.visiblePrefix_at` | P | Leading silence plus separated retained emissions. Fault: use `f n + 1`, including the next emitted event. |
| 191 `infinite_stuttering` | P | Infinitely emitting and eventually silent streams under `2*n+1`. Fault: insert an extra visible event. |
| 204 `SameObservation.refl` | P | Nonempty finite, blocked and infinite executions. Fault: make the infinite relation require a strict unmatched extension. |
| 211 `SameObservation.symm` | V for independent-law credit | Finite equalities reverse; the infinite branch uses the definitionally symmetric conjunction. It adds no independent semantic obligation beyond those component definitions. |
| 216 `SameObservation.trans` | P | Three differently stuttered executions with the same ending. Fault: break eventual-prefix composition. |
| 226 `full_observation_preservation` | P, limited | Transport a two-event finite divergence through delayed emissions; also blocked and finite observations. Fault: truncate the projected history or change the pending request. It proves relational transport, not final-state preservation or execution adequacy. |
| 266 `infinite_not_termination` | V | Mixed constructors reduce to `False`. Finding 6. |
| 272 `infinite_silent_not_termination` | V | Specialization of the same definitional exclusion. Finding 6. |
| 283 private `stripPrefix_append` | P | Nonempty prefix and suffix containing repeated delimiters. Fault: consume an extra character. |
| 295 private `parseRevNat_digits` | P | Multi-digit number, nondigit delimiter and remaining suffix. Fault: omit digit reversal or consume the delimiter. |
| 314 private `decodeKind_render` | P | Every trap kind with a nonempty path. Fault: collide or alter a kind spelling. |
| 340 private `trapLineChars_prefix` | V | Reassociates the renderer’s literal definition. Keep as an unfolding helper; it is not independent rendering evidence. |
| 346 private `decodeWithKind_render` | P | Multi-digit site/positions and path containing colons/parentheses. Fault: parse the wrong delimiter or numeric field. |
| 354 private `decodeTrapLine_render` | P | Kind plus punctuated path and numeric fields. Fault: mismatch renderer and decoder kind/site fields. |
| 361 `trapLine_injective` | P | Distinct sites or spans, including punctuated paths. Fault: omit site or column from rendering; distinct inputs then collide. |
| 370 `trap_line_uses_byte_columns` | V | Assumes the desired column equality. Finding 2. |
| 379 `byteColumn_tab` | P | Prefix containing multibyte UTF-8 followed by a tab. Fault: tab-stop expansion instead of one byte. |
| 388 `ValidTrace.outputs` | P | Append two outputs after a completed exchange. Fault: change the pending state or insert a request. |
| 397 `ValidTrace.answer_projection` | P | Outputs before and after `3 → 6`, with history-sensitive allowance. Fault: reverse request/response or put output between them. |
| 412 `ExecAdapter.project_append` | P | Two C1 events, each projecting to multiple C6 events. Fault: reverse or truncate one side of concatenation. |
| 419 `runFrom_validTrace` | P, qualified | A two-answer run with nonempty projections and history-sensitive contracts. Fault: lose the accumulated history or append response before request. Blocking permission is inherited. |
| 478 `runFrom_noAnswer_pending_valid` | P, qualified | Valid completed exchange followed by permitted pending request. Fault: omit or duplicate the appended pending request. Permission component is inherited. |
| 495 `runFrom_blockingPermitted` | P as a derived bridge | Same blocked run; derives the report premise from evaluator induction. Fault: use the wrong report history. It adds no new blocking justification. |
| 506 `output_before_trap_kept` | V | Observation directly installs the projected history; an output-erasing adapter still passes. Finding 4. |
| 515 `report_noAnswer_mayBlock` | V | Direct application of the permission assumption. Finding 5. |
| 527 `report_noAnswer_blocked` | V | Literal report-projection equation. Finding 5. |
| 539 `report_outOfFuel_none` | V | Literal `none` branch. Finding 5. |

The additional definition-only laws identified in this table are **REAL DEFECTS when credited as non-vacuous contract laws**. Their concrete repair is to retain them as implementation helpers and supply independent semantic obligations with realizable witnesses and rejection controls.

**Changed conformance examples**

These are all in `ConcreteSpec/Spec/Observe.lean`:

| Line | Audit |
|---|---|
| 362 complete doubling trace | Inhabited positive witness; response `7` is the negative control. |
| 366 rejected response `7` | Meaningful admission rejection; response `6` is the positive control. Does not repair `Responds.allows` vacuity. |
| 372 rejected blocking | Meaningful rejection for the nonblocking instance; an otherwise identical blocking-permitted contract supplies the opposite case. |
| 378 accepted response `6` | Positive admission witness; response `7` is rejected. |
| 382 infinite trace | Type inhabitant only, not a preservation law or rejection control. |
| 385 finite stuttering | Meaningful concrete observation equality; replacing/reordering the event rejects it. |
| 390 retained silent steps | Meaningful `SilentInsertion` witness; inserting a visible event rejects it. |
| 395 infinite versus finite | Vacuous definitional exclusion; Finding 6. |
| 404 exact trap text | Meaningful literal conformance witness; altered delimiter, field or spelling rejects it. |
| 407 trap status | Concrete mapping check; planted status `125` rejects it. |
| 408 refused launch | Definition-only branch equation; does not establish grant checking or startup execution ordering. |
| 410 normal exit 134 versus trap | Concrete constructor distinction; useful representation check, not operational classification evidence. |
| 419 UTF-8 byte column | Meaningful independent numeric expectation; scalar counting or tab expansion rejects it. |
| 421 `HasByteColumn` | Definition-only equality witness; does not connect source positions to produced spans. |

No new owner decision is needed to address the substantive contract gaps: preserving final external state, valid trap paths, and byte columns are already specified. Concrete grant instances and C8 adequacy remain later work, but cannot be replaced by crediting the vacuous laws above.

VERDICT: CHANGES
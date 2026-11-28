import ConcreteSpec.Proofs.TableVerifierBounds
import ConcreteSpec.Spec.LL1
import ConcreteSpec.Spec.TableVerifier
import ConcreteSpec.Spec.VerifyMachine

namespace ConcreteSpec.GrammarVerification
open ConcreteSpec.StageGrammar

theorem rankedFacts_exact {rs : List Clause} {height : Nat → Nat}
    (closed : Closed rs height) (justified : Justified rs height) (a : Nat) :
    0 < height a ↔ Provable rs a := by
  constructor
  · intro h
    generalize eqn : height a = k
    induction k using Nat.strongRecOn generalizing a with
    | ind k ih =>
      obtain ⟨r, hr, rfl, smaller⟩ := justified a h
      exact .step r hr fun p hp => ih _ (eqn ▸ (smaller p hp).2) p (smaller p hp).1 rfl
  · intro h
    induction h with
    | step r hr _ ih => exact closed r hr ih

private theorem code_length (t : Test) (s pc yes no : Nat) :
    (t.code s pc yes no).length = t.width := by
  induction t generalizing pc yes no <;> simp [Test.code, Test.width, *]

private def LocatedCode (program : Array VerifyInstruction) (pc : Nat)
    (code : List VerifyInstruction) : Prop :=
  ∀ i op, code[i]? = some op → program[pc+i]? = some op

private theorem LocatedCode.left {program pc} {a b : List VerifyInstruction}
    (h : LocatedCode program pc (a ++ b)) : LocatedCode program pc a := by
  intro i op hi
  apply h
  have lt : i < a.length := (List.getElem?_eq_some_iff.mp hi).1
  rwa [List.getElem?_append_left lt]

private theorem LocatedCode.right {program pc} {a b : List VerifyInstruction}
    (h : LocatedCode program pc (a ++ b)) : LocatedCode program (pc+a.length) b := by
  intro i op hi
  rw [Nat.add_assoc]
  apply h
  rw [List.getElem?_append_right (by omega)]
  simpa using hi

private inductive Steps (program : Array VerifyInstruction) :
    VerifyState → VerifyState → Nat → Prop
  | done (s) : Steps program s s 0
  | step {s next last k} : (program[s.pc]?.bind s.step) = some (.ok next) →
      Steps program next last k → Steps program s last (k+1)

private theorem Steps.trans {prog s m e i j} (h : Steps prog s m i) (h' : Steps prog m e j) :
    Steps prog s e (i+j) := by
  induction h with
  | done => simpa using h'
  | step step _ ih => simpa [Nat.succ_add] using Steps.step step (ih h')

private theorem Steps.run {prog s t k} (h : Steps prog s t k) (fuel : Nat) :
    verifyRun prog s (fuel+k) = ((verifyRun prog t fuel).1+k, (verifyRun prog t fuel).2) := by
  induction h with
  | done => simp
  | step hop _ ih =>
    rw [Nat.add_succ, verifyRun, hop]
    dsimp only
    rw [ih]
    rfl

private def Agrees (s : Nat) (before after : Array UInt64) : Prop :=
  after.size = before.size ∧ ∀ i < s, after[i]! = before[i]!

private theorem Test.eval_agrees {scratch : Nat} {a b : Array UInt64} (t : Test)
    (h : t.inBounds scratch = true) (same : Agrees scratch a b) :
    t.eval (fun i => b[i]!) = t.eval (fun i => a[i]!) := by
  induction t <;> simp_all [Test.inBounds, Test.eval, same.2]

private theorem compileTest_correct (t : Test) (mem : Array UInt64) (scratch pc yes no : Nat)
    (prog : Array VerifyInstruction) (located : LocatedCode prog pc (t.code scratch pc yes no))
    (bound : t.inBounds scratch = true) (size : scratch < mem.size) :
    ∃ next cost, Agrees scratch mem next ∧ 0 < cost ∧ cost ≤ t.width ∧
      Steps prog ⟨pc, mem⟩ ⟨if t.eval (fun i => mem[i]!) then yes else no, next⟩ cost := by
  induction t generalizing pc yes no mem with
  | literal b =>
    have op := located 0 _ (by rfl)
    simp only [Nat.add_zero] at op
    refine ⟨mem, 1, ⟨rfl, fun _ _ => rfl⟩, by decide, Nat.le_refl _, .step ?_ (.done _)⟩
    simp [op, VerifyState.step, Array.getElem?_eq_getElem size, Test.eval] <;> rfl
  | word addr val =>
    have ha : addr < scratch := of_decide_eq_true bound
    have inMem := Nat.lt_trans ha size
    have op := located 0 _ (by rfl)
    have op' := located 1 _ (by rfl)
    simp only [Nat.add_zero] at op
    let next := mem.set! scratch val
    have same : Agrees scratch mem next := by
      refine ⟨by simp [next], fun i hi => ?_⟩
      have lt := Nat.lt_trans hi size
      simp [next, getElem!_pos, lt, Nat.ne_of_gt hi]
    refine ⟨next, 2, same, by decide, Nat.le_refl _, .step (next := ⟨pc+1,next⟩) ?_ (.step ?_ (.done _))⟩
    · simp [op, VerifyState.step, VerifyState.advance, size, next]
    · simp [op', VerifyState.step, Test.eval, next, size, Nat.ne_of_gt ha, getElem!_pos, inMem]
  | less a b =>
    have hb : a < scratch ∧ b < scratch := by simpa [Test.inBounds] using bound
    have op := located 0 _ (by rfl)
    simp only [Nat.add_zero] at op
    refine ⟨mem, 1, ⟨rfl, fun _ _ => rfl⟩, by decide, Nat.le_refl _, .step ?_ (.done _)⟩
    simp [op, VerifyState.step, Test.eval, getElem!_pos, Nat.lt_trans hb.1 size, Nat.lt_trans hb.2 size]
  | neg p ih =>
    have path := ih mem pc no yes located bound size
    cases hp : p.eval (fun i => mem[i]!) <;> simpa [Test.eval, hp, Test.width] using path
  | both p q ihp ihq | either p q ihp ihq =>
    all_goals
      have bounds : p.inBounds scratch ∧ q.inBounds scratch := by simpa [Test.inBounds] using bound
      have left := located.left
      have right := located.right
      simp only [code_length] at right
      obtain ⟨m, i, same,positive, hi, path⟩ := ihp mem _ _ _ left bounds.1 size
      cases hp : p.eval (fun i => mem[i]!) <;> simp only [Test.eval, hp, Bool.false_and, Bool.true_and, Bool.false_or, Bool.true_or,
        Bool.false_eq_true, ↓reduceIte] at path ⊢
    all_goals first
      | exact ⟨m, i, same,positive, by simp only [Test.width]; omega, path⟩
      | obtain ⟨out, j, same', _, hj, tail⟩ := ihq m _ _ _ right bounds.2 (same.1 ▸ size)
        rw [q.eval_agrees bounds.2 same] at tail
        exact ⟨out, i+j, ⟨same'.1.trans same.1, fun x hx => (same'.2 x hx).trans (same.2 x hx)⟩,
          by omega, by simp only [Test.width]; omega, path.trans tail⟩

private def block (scratch reject pc : Nat) : List Test → List VerifyInstruction
  | [] => []
  | p :: ps => p.code scratch pc (pc+p.width) reject ++ block scratch reject (pc+p.width) ps

private theorem emit_eq (scratch reject : Nat) (code : Array VerifyInstruction) (ps : List Test) :
    (emitChecks scratch reject code ps).toList = code.toList ++ block scratch reject code.size ps := by
  induction ps generalizing code with
  | nil => simp [emitChecks, block]
  | cons p ps ih =>
    change (emitChecks _ _ _ ps).toList = _
    rw [ih]
    simp [block, code_length]

private theorem block_length (scratch reject pc : Nat) (ps : List Test) :
    (block scratch reject pc ps).length = totalWidth ps := by
  induction ps generalizing pc with
  | nil => rfl
  | cons p ps ih => simp [block, totalWidth, code_length, ih]

private theorem block_correct (ps : List Test) (mem : Array UInt64) (scratch pc reject : Nat)
    (prog : Array VerifyInstruction) (located : LocatedCode prog pc (block scratch reject pc ps))
    (bound : ps.all (Test.inBounds scratch) = true) (size : scratch < mem.size) :
    ∃ out cost, cost ≤ totalWidth ps ∧
      (ps.all (Test.eval fun i => mem[i]!) = true → ps.length ≤ cost) ∧ Steps prog ⟨pc,mem⟩
      ⟨if ps.all (Test.eval fun i => mem[i]!) then pc+totalWidth ps else reject, out⟩ cost := by
  induction ps generalizing pc mem with
  | nil => exact ⟨mem, 0, Nat.le_refl _, by simp, .done _⟩
  | cons p ps ih =>
    simp only [List.all_cons, Bool.and_eq_true] at bound
    obtain ⟨m,i,same,positive,hi,path⟩ := compileTest_correct p mem scratch pc _ _ prog located.left bound.1 size
    have tail := located.right
    rw [code_length] at tail
    have width : totalWidth (p :: ps) = p.width + totalWidth ps := rfl
    cases hp : p.eval (fun i => mem[i]!) with
    | false =>
      simp [hp] at path
      exact ⟨m,i, by omega, by simp [hp], by simpa [hp] using path⟩
    | true =>
      simp [hp] at path
      obtain ⟨out,j,hj,measured,rest⟩ := ih m _ tail bound.2 (same.1 ▸ size)
      have hreads : ps.all (Test.eval fun i => m[i]!) = ps.all (Test.eval fun i => mem[i]!) :=
        by
          apply Bool.eq_iff_iff.mpr
          simp only [List.all_eq_true]
          constructor <;> intro h t ht
          · rw [← t.eval_agrees (List.all_eq_true.mp bound.2 t ht) same]
            exact h t ht
          · rw [t.eval_agrees (List.all_eq_true.mp bound.2 t ht) same]
            exact h t ht
      rw [hreads] at rest measured
      refine ⟨out,i+j,by omega,?_,by simpa [hp, width, Nat.add_assoc] using path.trans rest⟩
      simp only [List.all_cons,hp,Bool.true_and,List.length_cons]
      intro good
      have := measured good
      omega

/-- Instruction-level reflection. All comparison paths end at the appropriate
halt within the instruction budget, for any initial scratch word. -/
private theorem compileTests_measured (ts : Array Test) (scratch : Nat) (mem : Array UInt64) (fuel : Nat)
    (bounds : ts.all (Test.inBounds scratch) = true) (size : scratch < mem.size)
    (enough : totalWidth ts.toList + 1 ≤ fuel) :
    (verifyRun (compileTests scratch ts) ⟨0,mem⟩ fuel).2 =
      some (ts.toList.all (Test.eval fun i => mem[i]!)) ∧
    (ts.toList.all (Test.eval fun i => mem[i]!) → ts.size ≤ (verifyRun (compileTests scratch ts) ⟨0,mem⟩ fuel).1) := by
  let w := totalWidth ts.toList
  let prog := compileTests scratch ts
  have shape : prog.toList = block scratch (w+1) 0 ts.toList ++ [.halt true,.halt false] := by
    simp [prog, compileTests, emit_eq, w]
  have located : LocatedCode prog 0 (block scratch (w+1) 0 ts.toList) := by
    intro i op hi
    exact LocatedCode.left (program := prog) (pc := 0) (b := [.halt true,.halt false])
      (by intro j ins hj; simpa only [Nat.zero_add, ← Array.getElem?_toList, shape] using hj) i op hi
  obtain ⟨out,k,hk,measured,path⟩ := block_correct ts.toList mem scratch 0 (w+1) prog located
    (by simpa using bounds) size
  let accept := ts.toList.all (Test.eval fun i => mem[i]!)
  let dest := if accept then w else w+1
  have atHalt : prog[dest]? = some (.halt accept) := by
    rw [← Array.getElem?_toList, shape]
    have len := block_length scratch (w+1) 0 ts.toList
    cases ha : accept <;> simp [dest, ha, len, w]
  have halt (left : Nat) : (verifyRun prog ⟨dest,out⟩ (left+1)).2 = some accept := by
    simp [verifyRun, atHalt, VerifyState.step]
  have pos : fuel - k = (fuel-k-1)+1 := by omega
  have run := path.run (fuel-k)
  rw [Nat.sub_add_cancel (by omega)] at run
  change (verifyRun prog ⟨0,mem⟩ fuel).2 = some accept ∧
    (accept → ts.size ≤ (verifyRun prog ⟨0,mem⟩ fuel).1)
  rw [run]
  simp only [Nat.zero_add]
  constructor
  · change (verifyRun prog ⟨dest,out⟩ (fuel-k)).2 = some accept
    rw [pos, halt]
  · intro hp
    have := measured hp
    simp only [Array.length_toList] at this
    omega

theorem compileTests_run (ts : Array Test) (scratch : Nat) (mem : Array UInt64) (fuel : Nat)
    (bounds : ts.all (Test.inBounds scratch) = true) (size : scratch < mem.size)
    (enough : totalWidth ts.toList + 1 ≤ fuel) :
    (verifyRun (compileTests scratch ts) ⟨0,mem⟩ fuel).2 =
      some (ts.toList.all (Test.eval fun i => mem[i]!)) :=
  (compileTests_measured ts scratch mem fuel bounds size enough).1

theorem compileTableVerifier_spaceBound (g : RawGrammar) :
    (compileTableVerifier g).words ≤ g.budget := by
  unfold compileTableVerifier
  split
  · rename_i h
    exact scratch_budget (valid_of_representable h)
  · exact Nat.zero_le _

theorem compileTableVerifier_instructionBound (g : RawGrammar) :
    (compileTableVerifier g).program.size ≤ g.budget := by
  unfold compileTableVerifier
  split
  · rename_i h
    have width := checks_width_budget (valid_of_representable h)
    have len := congrArg List.length (emit_eq g.layout.scratch (totalWidth (checks g).toList + 1)
      #[] (checks g).toList)
    simpa [compileTests, block_length, Nat.add_comm] using
      (show (emitChecks g.layout.scratch (totalWidth (checks g).toList+1) #[] (checks g).toList).size
          + 2 ≤ g.budget from (by simp [block_length] at len; omega))
  · change 1 ≤ 256 * (g.positions+1) * (g.terminals+1)
    exact Nat.mul_pos (Nat.mul_pos (by decide) (Nat.zero_lt_succ _)) (Nat.zero_lt_succ _)

/-- Executing the actual machine reflects the conjunction of the local
certificate obligations on the original input. Length is checked at the entry. -/
theorem compileTableVerifier_accepts_iff (g : RawGrammar) (mem : Array UInt64) :
    (compileTableVerifier g).accepts mem g.budget = true ↔ representable g = true ∧
      mem.size = g.layout.scratch + 1 ∧
      ∀ t ∈ (checks g).toList, t.eval (fun i => mem[i]!) = true := by
  by_cases h : representable g = true
  · have bounds := checks_inBounds (valid_of_representable h)
    have width := checks_width_budget (valid_of_representable h)
    simp only [compileTableVerifier, h, ↓reduceIte, Compiled.accepts, Bool.and_eq_true,
      beq_iff_eq, true_and]
    constructor
    · rintro ⟨size, accepted⟩
      rw [compileTests_run _ _ _ _ bounds (by omega) (by omega)] at accepted
      exact ⟨size, List.all_eq_true.mp (Option.some.inj accepted)⟩
    · rintro ⟨size, valid⟩
      rw [compileTests_run _ _ _ _ bounds (by omega) (by omega)]
      exact ⟨size, congrArg some (List.all_eq_true.mpr valid)⟩
  · simp only [compileTableVerifier, h, Compiled.accepts]
    cases budget : g.budget <;> simp [verifyRun, VerifyState.step]

theorem compileTableVerifier_completes (g : RawGrammar) (mem : Array UInt64)
    (shape : mem.size = (compileTableVerifier g).words) :
    (verifyRun (compileTableVerifier g).program ⟨0,mem⟩ g.budget).2.isSome := by
  by_cases h : representable g = true
  · have bounds := checks_inBounds (valid_of_representable h)
    have width := checks_width_budget (valid_of_representable h)
    simp only [compileTableVerifier, h, ↓reduceIte] at shape ⊢
    rw [compileTests_run _ _ _ _ bounds (by omega) (by omega)]
    rfl
  · simp only [compileTableVerifier, h]
    cases hb : g.budget with
    | zero => simp [RawGrammar.budget, RawGrammar.columns] at hb
    | succ => rfl

theorem accepted_work_covers_grammar {g : RawGrammar} {mem : Array UInt64}
    (accepted : (compileTableVerifier g).accepts mem g.budget = true) :
    g.positions ≤ (verifyRun (compileTableVerifier g).program ⟨0,mem⟩ g.budget).1 := by
  obtain ⟨rep,shape,all⟩ := (compileTableVerifier_accepts_iff ..).1 accepted
  have bounds := checks_inBounds (valid_of_representable rep)
  have width := checks_width_budget (valid_of_representable rep)
  have work := (compileTests_measured (checks g) g.layout.scratch mem g.budget bounds
    (by omega) (by omega)).2 (List.all_eq_true.mpr all)
  simp only [compileTableVerifier, rep, ↓reduceIte]
  apply Nat.le_trans _ work
  simp [checks, factChecks, Layout.factAddresses, Layout.scratch, Layout.suffixFirst,
    Layout.suffixProductive, Layout.suffixNull, Layout.prefixNull, Layout.follow,
    Layout.firstNt, Layout.productive, Layout.nullNt, Layout.ntBase, Layout.rank, RawGrammar.layout]
  omega

theorem decodeCells_dimensions (l : Layout) (mem : Array UInt64) {cells : Array (Option Nat)}
    (h : decodeCells l mem = some cells) : cells.size = l.n * l.c := by
  have bounds : l.n*l.c ≤ l.scratch := by
    simp only [Layout.scratch, Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
      Layout.prefixNull, Layout.follow, Layout.firstNt, Layout.productive, Layout.nullNt,
      Layout.ntBase, Layout.rank]
    omega
  by_cases hs : mem.size = l.scratch + 1
  · have hbeq : (mem.size == l.scratch + 1) = true := beq_iff_eq.mpr hs
    simp only [decodeCells, guard, hbeq, ↓reduceIte, pure, Bind.bind, Option.bind_some,
      Option.some.injEq] at h
    subst cells
    rw [Array.size_map, Array.size_extract, Nat.sub_zero,
      Nat.min_eq_left (by omega : l.n * l.c ≤ mem.size)]
  · have hbeq : (mem.size == l.scratch + 1) = false := beq_false_of_ne hs
    simp only [decodeCells, guard, hbeq, Bool.false_eq_true, ↓reduceIte,
      failure, Alternative.failure, Bind.bind, Option.bind_none] at h
    contradiction

private theorem eval_all (read : Nat → UInt64) (ts : List Test) :
    (Test.all ts).eval read = true ↔ ∀ t ∈ ts, t.eval read = true := by
  have aux (acc : Test) (xs : List Test) :
      (xs.foldl Test.both acc).eval read = true ↔
        acc.eval read = true ∧ ∀ t ∈ xs, t.eval read = true := by
    induction xs generalizing acc with
    | nil => simp
    | cons t ts ih => simp [ih, Test.eval, and_assoc]
  cases ts <;> simp [Test.all, Test.eval, aux]

private theorem eval_any (read : Nat → UInt64) (ts : List Test) :
    (Test.any ts).eval read = true ↔ ∃ t ∈ ts, t.eval read = true := by
  have aux (acc : Test) (xs : List Test) :
      (xs.foldl Test.either acc).eval read = true ↔
        acc.eval read = true ∨ ∃ t ∈ xs, t.eval read = true := by
    induction xs generalizing acc with
    | nil => simp
    | cons t ts ih => simp [ih, Test.eval, or_assoc]
  cases ts <;> simp [Test.any, Test.eval, aux]

private theorem eval_positive (read : Nat → UInt64) (a : Nat) :
    (Test.positive a).eval read = true ↔ 0 < (read a).toNat := by
  simp [Test.positive, Test.eval, Nat.pos_iff_ne_zero]
  exact not_congr (UInt64.toNat_inj (a := read a) (b := 0)).symm

private theorem eval_implies (read : Nat → UInt64) (p q : Test) :
    (p.implies q).eval read = true ↔ (p.eval read = true → q.eval read = true) := by
  cases hp : p.eval read <;> simp [Test.implies, Test.eval, hp]

private theorem eval_closed (read : Nat → UInt64) (r : Clause) :
    r.closedTest.eval read = true ↔
    ((∀ a ∈ r.premises, 0 < (read a).toNat) → 0 < (read r.head).toNat) := by
  simp [Clause.closedTest, eval_implies, eval_all, eval_positive]

private theorem eval_justification (read : Nat → UInt64) (r : Clause) :
    r.justification.eval read = true ↔
    ∀ a ∈ r.premises, 0 < (read a).toNat ∧ (read a).toNat < (read r.head).toNat := by
  simp only [Clause.justification, Test.eval, Bool.and_eq_true, eval_all, List.forall_mem_map,
    eval_positive, decide_eq_true_eq, UInt64.lt_iff_toNat_lt]
  exact ⟨fun ⟨h1, h2⟩ a ha => ⟨h1 a ha, h2 a ha⟩,
    fun h => ⟨fun a ha => (h a ha).1, fun a ha => (h a ha).2⟩⟩

private theorem mem_factAddresses (l : Layout) (a : Nat) :
    a ∈ l.factAddresses ↔ l.isFact a = true := by
  simp only [Layout.factAddresses, Layout.isFact, List.mem_map, List.mem_range,
    Bool.and_eq_true, decide_eq_true_eq]
  constructor
  · rintro ⟨k, hk, rfl⟩
    constructor <;> omega
  · rintro ⟨lo, hi⟩
    exact ⟨a-l.ntBase, by omega, by omega⟩

private theorem eval_explanations (rs : List Clause) (read : Nat → UInt64) :
    (Test.any (rs.map Clause.justification)).eval read = true ↔
    ∃ r ∈ rs, ∀ a ∈ r.premises, 0 < (read a).toNat ∧ (read a).toNat < (read r.head).toNat := by
  rw [eval_any]
  constructor
  · rintro ⟨t, ht, ok⟩
    obtain ⟨r,hr,rfl⟩ := List.mem_map.mp ht
    exact ⟨r,hr,(eval_justification ..).1 ok⟩
  · rintro ⟨r,hr,ok⟩
    exact ⟨_, List.mem_map_of_mem hr, (eval_justification ..).2 ok⟩

/-- Reflection of the local rank and closure constraints into an independently
inductive least model. Words outside the fact region are not facts. -/
theorem factChecks_iff (l : Layout) (rs : Array Clause) (read : Nat → UInt64)
    (wf : ∀ r ∈ rs.toList, l.isFact r.head ∧ ∀ p ∈ r.premises, l.isFact p) :
    let height := fun a => if l.isFact a then (read a).toNat else 0
    (∀ t ∈ (factChecks l rs).toList, t.eval read = true) ↔
      Closed rs.toList height ∧ Justified rs.toList height := by
  dsimp only
  simp only [factChecks, Array.toList_append, Array.toList_map,
    List.forall_mem_append, List.forall_mem_map, eval_closed, eval_implies, eval_positive,
    eval_explanations, mem_factAddresses]
  constructor
  · rintro ⟨closure, supported⟩
    refine ⟨fun r hr hp => ?_, fun a ha => ?_⟩
    · simp only [(wf r hr).1, ↓reduceIte]
      apply closure r hr
      intro p hm
      simpa [(wf r hr).2 p hm] using hp p hm
    · dsimp only at ha
      split at ha
      next h =>
        obtain ⟨r, hr, premises⟩ := supported a h ha
        have lt : a < l.scratch := (by simpa [Layout.isFact] using h : l.ntBase ≤ a ∧ a < l.scratch).2
        obtain ⟨member, eqn⟩ := (group_mem Rule.head _ _ _ lt r).1 hr
        subst a
        exact ⟨r,member,rfl,fun p hp => by simpa [(wf r member).2 p hp, (wf r member).1] using premises p hp⟩
      next => omega
  · rintro ⟨closed, justified⟩
    refine ⟨fun r hr h => ?_, fun a ha pos => ?_⟩
    · have claim := closed r hr (fun p hp => by simpa [(wf r hr).2 p hp] using h p hp)
      simpa [(wf r hr).1] using claim
    · obtain ⟨r, hr, rfl, hp⟩ := justified a (by simpa [ha] using pos)
      have lt : r.head < l.scratch := (by simpa [Layout.isFact] using ha : l.ntBase ≤ r.head ∧ r.head < l.scratch).2
      exact ⟨r,(group_mem Rule.head _ _ _ lt r).2 ⟨hr,rfl⟩,
        fun p hm => by simpa [(wf r hr).2 p hm, ha] using hp p hm⟩

theorem accepted_facts_exact (g : RawGrammar) (mem : Array UInt64)
    (accepted : (compileTableVerifier g).accepts mem g.budget = true)
    (a : Nat) (fact : g.layout.isFact a) :
    0 < mem[a]!.toNat ↔ Provable (grammarClauses g).toList a := by
  obtain ⟨h,_,valid⟩ := (compileTableVerifier_accepts_iff ..).1 accepted
  have wf := grammarClauses_valid (valid_of_representable h)
  have model := (factChecks_iff g.layout (grammarClauses g) (fun i => mem[i]!)
    wf).1 (fun t ht => valid t (by simp [checks, ht]))
  simpa [fact] using rankedFacts_exact model.1 model.2 a

private theorem eval_prediction (l : Layout) (read : Nat → UInt64) (r : Row) (c : Nat) :
    (l.prediction r c).eval read = true ↔ l.PredictsAt read r c := by
  simp [Layout.prediction, Layout.PredictsAt, Test.eval, eval_positive]

private theorem eval_any_map {α : Type} (f : α → Test) (xs : List α) (read : Nat → UInt64) :
    (Test.any (xs.map f)).eval read = true ↔ ∃ x ∈ xs, (f x).eval read = true := by
  rw [eval_any]
  constructor
  · rintro ⟨t, ht, ok⟩
    obtain ⟨x,hx,rfl⟩ := List.mem_map.mp ht
    exact ⟨x,hx,ok⟩
  · rintro ⟨x,hx,ok⟩
    exact ⟨_,List.mem_map_of_mem hx,ok⟩

private theorem cellChecks_iff (l : Layout) (rows : List Row) (read : Nat → UInt64) :
    (∀ t ∈ l.cellChecks rows, t.eval read = true) ↔
    ∀ n < l.n, ∀ c < l.c,
      ((0 < (read (l.cell n c)).toNat → ∃ r ∈ rows, r.lhs = n ∧ l.PredictsAt read r c) ∧
       ∀ r ∈ rows, r.lhs = n → l.PredictsAt read r c → read (l.cell n c) = (r.index+1).toUInt64) := by
  simp only [Layout.cellChecks, List.forall_mem_flatMap, List.mem_range, List.forall_mem_cons,
    eval_implies, eval_positive, eval_any_map, List.forall_mem_map, eval_prediction]
  apply forall_congr'; intro n
  apply forall_congr'; intro hn
  apply forall_congr'; intro c
  apply forall_congr'; intro _
  simp only [group_mem Row.lhs _ _ n hn, Test.eval, beq_iff_eq, and_imp]
  constructor
  · rintro ⟨h1, h2⟩
    refine ⟨fun hpos => ?_, fun r hr hlhs hpred => h2 r hr hlhs hpred⟩
    obtain ⟨r, ⟨hr, hlhs⟩, hpred⟩ := h1 hpos
    exact ⟨r, hr, hlhs, hpred⟩
  · rintro ⟨h1, h2⟩
    refine ⟨fun hpos => ?_, fun r hr hlhs hpred => h2 r hr hlhs hpred⟩
    obtain ⟨r, hr, hlhs, hpred⟩ := h1 hpos
    exact ⟨r, ⟨hr, hlhs⟩, hpred⟩

private theorem descentChecks_iff (l : Layout) (r : Row) (read : Nat → UInt64) :
    (∀ t ∈ l.descentChecks r, t.eval read = true) ↔
    ∀ child i, (Concrete.LL1.Sym.n child, i) ∈ r.rhs.zipIdx →
      0 < (read (l.prefixNull (r.base+i))).toNat → read (l.rank child) < read (l.rank r.lhs) := by
  simp only [Layout.descentChecks, List.mem_filterMap]
  constructor
  · intro h child i member
    have := h _ ⟨(_,i),member,rfl⟩
    simpa [eval_implies, eval_positive, Test.eval] using this
  · rintro h t ⟨⟨sym,i⟩,member,eqn⟩
    cases sym with
    | t => contradiction
    | n child =>
      cases eqn
      simpa [eval_implies, eval_positive, Test.eval] using h child i member

theorem compileTableVerifier_correct (g : RawGrammar) (mem : Array UInt64) :
    (compileTableVerifier g).accepts mem g.budget = true ↔
      representable g = true ∧ mem.size = g.layout.scratch+1 ∧
      ValidCertificate g (fun a => mem[a]!) := by
  rw [compileTableVerifier_accepts_iff]
  by_cases h : representable g = true
  · simp only [h, true_and, and_congr_right_iff]
    intro _
    have wf := grammarClauses_valid (valid_of_representable h)
    simp only [checks, predictionChecks, Array.toList_append,
      List.forall_mem_append, List.forall_mem_flatMap, List.forall_mem_map, List.mem_range,
      factChecks_iff _ _ _ wf, cellChecks_iff, descentChecks_iff, eval_positive, ValidCertificate,
      and_assoc]
    rfl
  · simp [h]

theorem compileTableVerifier_complete {g : RawGrammar} {mem : Array UInt64}
    (representable : representable g) (dimensions : mem.size = g.layout.scratch+1)
    (valid : ValidCertificate g (fun a => mem[a]!)) :
    (compileTableVerifier g).accepts mem g.budget = true :=
  (compileTableVerifier_correct ..).2 ⟨representable, dimensions, valid⟩

theorem compileTableVerifier_sound {g : RawGrammar} {mem : Array UInt64}
    (accepted : (compileTableVerifier g).accepts mem g.budget = true) :
    ValidCertificate g (fun a => mem[a]!) :=
  ((compileTableVerifier_correct ..).1 accepted).2.2

private theorem grid_unique {a b c i j : Nat} (hi : i < c) (hj : j < c)
    (eqn : a*c+i = b*c+j) : a = b ∧ i = j := by
  have same : i = j := by
    simpa [Nat.mod_eq_of_lt hi, Nat.mod_eq_of_lt hj] using congrArg (· % c) eqn
  subst j
  exact ⟨Nat.eq_of_mul_eq_mul_right (by omega) (Nat.add_right_cancel eqn), rfl⟩

private theorem address_injective {l : Layout} {a b : Fact} (ha : a.valid l)
    (hb : b.valid l) (eqn : a.address l = b.address l) : a = b := by
  cases a <;> cases b <;> simp only [Fact.valid, decide_eq_true_eq, Bool.and_eq_true] at ha hb
  all_goals try obtain ⟨ha,ca⟩ := ha
  all_goals try obtain ⟨hb,cb⟩ := hb
  all_goals try have ba := grid_bound ha ca
  all_goals try have bb := grid_bound hb cb
  all_goals simp only [Fact.address, Layout.suffixFirst, Layout.suffixProductive, Layout.suffixNull,
    Layout.prefixNull, Layout.follow, Layout.firstNt, Layout.productive, Layout.nullNt,
    Layout.ntBase, Layout.rank] at eqn
  all_goals first
    | congr 1 <;> omega
    | rename_i a i b j
      obtain ⟨same,other⟩ := grid_unique (a := a) (b := b) ca cb (by omega)
      cases same
      cases other
      rfl

private theorem provable_map {α β : Type} {rs : List (Rule α)} (f : α → β) {a : α}
    (h : Provable rs a) : Provable (rs.map (Rule.map f)) (f a) := by
  induction h with
  | step r hr _ ih =>
    apply Provable.step (r.map f) (List.mem_map_of_mem hr)
    intro p hp
    obtain ⟨x,hx,rfl⟩ := List.mem_map.mp hp
    exact ih x hx

private theorem provable_unmap {α β : Type} {rs : List (Rule α)} (f : α → β)
    (valid : α → Prop) (within : ∀ r ∈ rs, valid r.head ∧ ∀ p ∈ r.premises, valid p)
    (unique : ∀ {a b}, valid a → valid b → f a = f b → a = b) {b : β}
    (h : Provable (rs.map (Rule.map f)) b) : ∀ a, valid a → f a = b → Provable rs a := by
  induction h with
  | step r hr _ ih =>
    obtain ⟨rule, member, rfl⟩ := List.mem_map.mp hr
    intro a ha eqn
    have same := unique ha (within rule member).1 eqn
    subst a
    exact .step rule member (fun p hp => ih (f p) (List.mem_map_of_mem hp) p ((within rule member).2 p hp) rfl)

theorem accepted_symbolic_exact {g : RawGrammar} {mem : Array UInt64}
    (h : (compileTableVerifier g).accepts mem g.budget = true) {f : Fact}
    (hf : f.valid g.layout) :
    0 < mem[f.address g.layout]!.toNat ↔ Provable (grammarRules g) f := by
  rw [accepted_facts_exact g mem h _ (valid_fact_address hf)]
  simp only [grammarClauses, List.toList_toArray]
  have rep := ((compileTableVerifier_correct ..).1 h).1
  have valid := grammarRules_valid (valid_of_representable rep)
  exact ⟨fun p => provable_unmap _ (fun s => s.valid g.layout = true) valid
      (fun ha hb eqn => address_injective ha hb eqn) p f hf rfl,
    provable_map _⟩

/-- The engine rank of a nonterminal comes from the accepted initial memory. -/
theorem decodeRank_at (l : Layout) (mem : Array UInt64) (n : Fin l.n)
    (size : mem.size = l.scratch + 1) :
    decodeRank l mem n = some mem[l.rank n.val]!.toNat := by
  have bound : l.rank n.val < mem.size := by
    rw [size]
    have lt := n.isLt
    simp [Layout.scratch,Layout.suffixFirst,Layout.suffixProductive,Layout.suffixNull,
      Layout.prefixNull,Layout.follow,Layout.firstNt,Layout.productive,Layout.nullNt,
      Layout.ntBase,Layout.rank]
    omega
  rw [getElem!_pos mem (l.rank n.val) bound]
  have hbeq : (mem.size == l.scratch + 1) = true := beq_iff_eq.mpr size
  simp only [decodeRank, guard, hbeq, ↓reduceIte, pure, Bind.bind, Option.bind_some,
    Array.getElem?_eq_getElem bound]

end ConcreteSpec.GrammarVerification

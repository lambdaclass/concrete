import ConcreteSpec.StageGrammarCore
import ConcreteSpec.Spec.SliceGrammar
import ConcreteSpec.Proofs.TableVerifier

/-! Closed grammar and parser selectors at the stage boundary.
Until F5b connects the production proofs in `ConcreteSpec/Proofs/SliceGrammar.lean`,
the six selectors remain backed by the single-production bootstrap grammar compiled
through `compileTableVerifier`. -/

namespace ConcreteSpec.StageGrammar

open ConcreteSpec.StageInterface
open ConcreteSpec.GrammarVerification
open Concrete.LL1

private def early := slice0
private def later : Slice := ⟨⟨2, by decide⟩⟩

private def bootstrapGrammar (production : Production (fun (_ : Fin 1) => SyntaxTree ⊕ Bool)) : SharedGrammar 1 :=
  { terminalSlices := Array.replicate Surface0.Terminal.count early
    terminalSliceCoverage := Surface0.Terminal.index_lt_count
    terminalSliceExact := fun i =>
      ⟨Surface0.Terminal.ofNat i.val, Surface0.Terminal.index_ofNat ⟨i.val, i.isLt⟩⟩
    integerSlices := Array.replicate 9 early ++ Array.replicate 27 later
    integerSliceCoverage := by
      intro radix suffix
      cases radix <;> cases suffix <;> first | (rename_i ty; cases ty <;> decide) | decide
    nonDecimalLater := by
      intro radix suffix nonDecimal
      cases radix <;> cases suffix <;> first
        | contradiction
        | (rename_i ty; cases ty <;> decide)
        | decide
    start := 0
    value := fun _ => SyntaxTree ⊕ Bool
    extraAst := Bool
    startShape := rfl
    productions := #[production] }

private def returnRhs : List (Sym Terminal (Fin 1)) :=
  [.t .fn, .t .ident, .t .lparen, .t .rparen, .t .arrow, .t .u8,
    .t .lbrace, .t .return_, .t .integer, .t .semi, .t .rbrace]

private def returnProduction : Production (fun (_ : Fin 1) => SyntaxTree ⊕ Bool) :=
  let afterFn := Action.right .input
  let name := Action.left afterFn
  let afterName := Action.right afterFn
  let afterOpen := Action.right afterName
  let afterClose := Action.right afterOpen
  let afterArrow := Action.right afterClose
  let afterType := Action.right afterArrow
  let afterBrace := Action.right afterType
  let ret := Action.left afterBrace
  let afterReturn := Action.right afterBrace
  let value := Action.left afterReturn
  let semi := Action.left (.right afterReturn)
  let body := Action.node .return_ (.cons (.leaf ret) (.cons (.leaf value) (.cons (.leaf semi) (.constant []))))
  ⟨0, returnRhs, early, .inl (.node .program (.cons (.leaf name)
    (.cons (.node .body (.cons body (.constant []))) (.constant [])))), .program⟩

abbrev sliceGrammarCount (_slice : Slice) : Nat := 1

def sliceASTExtra (_slice : Slice) : Type := Bool

def sliceGrammar (slice : Slice) : SharedGrammar (sliceGrammarCount slice) :=
  bootstrapGrammar returnProduction

theorem sliceGrammarStartShape (slice : Slice) :
    (sliceGrammar slice).value (sliceGrammar slice).start = SurfaceAST sliceASTExtra slice := rfl

private def returnCells : Array (Option Nat) :=
  (Array.replicate (sliceGrammar slice0).terminalColumns none).set! (terminalColumn (some .fn)) (some 0)

private def bootstrapTable : CheckedTable (sliceGrammar slice0) where
  cells := returnCells
  dimensions := rfl
  predicts := by
    intro n look i
    have hn : n = 0 := Subsingleton.elim _ _
    subst n
    have entry : tableLookup (sliceGrammar slice0).terminalColumns returnCells (0 : Fin 1) look =
        if look = some Surface0.Terminal.fn then some 0 else none := by
      cases look with
      | none => rfl
      | some t => cases t <;> rfl
    rw [entry]
    change _ ↔ Predicts (sliceGrammar slice0).grammar i look ∧
      ∃ rhs, [((0 : Fin 1), returnRhs)][i]? = some (0, rhs)
    cases i with
    | zero =>
      simp only [List.getElem?_cons_zero, Option.some.injEq, Prod.mk.injEq, true_and, exists_eq', and_true]
      constructor
      · intro hit
        have h : look = some Surface0.Terminal.fn := by
          split at hit
          · assumption
          · contradiction
        exact ⟨0, returnRhs, rfl, .inl ⟨.fn, h, [], returnRhs.drop 1,
          ⟨[], rfl, by simp⟩, .inl rfl⟩⟩
      · rintro ⟨n, rhs, row, first | ⟨nullable, _⟩⟩
        · change some ((0 : Fin 1), returnRhs) = some (n, rhs) at row
          have hr := (Prod.mk.inj (Option.some.inj row)).2
          subst rhs
          obtain ⟨a, rfl, pre, post, ⟨xs, rfl, _⟩, same | ⟨_, same, _⟩⟩ := first
          · cases xs <;> simp_all [returnRhs]
          · cases xs <;> simp_all [returnRhs]
        · change some ((0 : Fin 1), returnRhs) = some (n, rhs) at row
          have hr := (Prod.mk.inj (Option.some.inj row)).2
          subst rhs
          obtain ⟨xs, eq, _⟩ := nullable
          cases xs <;> simp [returnRhs] at eq
    | succ i => split <;> simp
  rank _ := 0
  acyclic := by
    intro p member before child after rhs _
    have eq : p = returnProduction := List.mem_singleton.mp member
    subst p
    have childMem : Sym.n child ∈ returnRhs := by
      rw [show returnRhs = before ++ .n child :: after from rhs]
      exact List.mem_append_right _ (List.mem_cons_self ..)
    simp [returnRhs] at childMem

private def bootstrapRaw : RawGrammar := (sliceGrammar slice0).rawGrammar

private theorem bootstrapRaw_representable : representable bootstrapRaw = true := by
  decide

private theorem bootstrapRaw_layout_eq :
    bootstrapRaw.layout = ⟨1, Surface0.Terminal.count + 1, 12⟩ := rfl

private theorem bootstrapRaw_scratch : bootstrapRaw.layout.scratch = 1254 := by
  decide

private def bootstrapDummySpan : FileSpan := ⟨⟨0⟩, 0, 0, 1, 1⟩

private def bootstrapFixedToken (k : Terminal) (h : Terminal.HasPayload k = false := by decide) :
    SurfaceToken :=
  ⟨bootstrapDummySpan, .fixed k h⟩

private theorem bootstrapProductive : (sliceGrammar slice0).Productive := by
  intro p hp
  have heq : p = returnProduction := List.mem_singleton.mp hp
  subst heq
  let t0 := bootstrapFixedToken .fn
  let t1 : SurfaceToken := ⟨bootstrapDummySpan, .name "main"⟩
  let t2 := bootstrapFixedToken .lparen
  let t3 := bootstrapFixedToken .rparen
  let t4 := bootstrapFixedToken .arrow
  let t5 := bootstrapFixedToken .u8
  let t6 := bootstrapFixedToken .lbrace
  let t7 := bootstrapFixedToken .return_
  let t8 : SurfaceToken := ⟨bootstrapDummySpan, .integer 0 .decimal none⟩
  let t9 := bootstrapFixedToken .semi
  let t10 := bootstrapFixedToken .rbrace
  exact ⟨[t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10],
    (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, ()), 11,
    .token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
    (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
    (.token rfl (by decide) (.token rfl (by decide) (.token rfl (by decide)
    (.token rfl (by decide) (.token rfl (by decide) .nil))))))))))⟩

private theorem bootstrap_no_suffixNull0 :
    ∀ r ∈ grammarRules bootstrapRaw, r.head ≠ Fact.suffixNull 0 := by
  have h : (grammarRules bootstrapRaw).all (fun r => r.head != Fact.suffixNull 0) = true := by
    decide
  intro r hr
  simpa using List.all_eq_true.mp h r hr

private theorem bootstrap_fn_suffixFirst0 :
    ⟨Fact.suffixFirst 0 (Surface0.Terminal.fn.ctorIdx + 1), []⟩ ∈ grammarRules bootstrapRaw := by
  have h : (grammarRules bootstrapRaw).any (fun r =>
      r.head == Fact.suffixFirst 0 (Surface0.Terminal.fn.ctorIdx + 1) && r.premises.isEmpty) = true := by
    decide
  obtain ⟨r, hr, hmatch⟩ := List.any_eq_true.mp h
  simp only [Bool.and_eq_true, beq_iff_eq, List.isEmpty_iff] at hmatch
  rcases r with ⟨hd, pre⟩
  obtain ⟨h1, h2⟩ := hmatch
  dsimp only at h1 h2
  subst h1
  subst h2
  exact hr

private theorem bootstrap_other_suffixFirst0 :
    ∀ r ∈ grammarRules bootstrapRaw, ∀ col : Nat,
      r.head = Fact.suffixFirst 0 col → col = Surface0.Terminal.fn.ctorIdx + 1 := by
  have h : (grammarRules bootstrapRaw).all (fun r =>
      match r.head with
      | .suffixFirst 0 col => col == Surface0.Terminal.fn.ctorIdx + 1
      | _ => true) = true := by
    decide
  intro r hr col hhead
  have hr' := List.all_eq_true.mp h r hr
  rw [hhead] at hr'
  exact eq_of_beq hr'

private theorem not_provable_of_no_head {α : Type} {rules : List (Rule α)} {a : α}
    (h : ∀ r ∈ rules, r.head ≠ a) : ¬ Provable rules a := by
  intro hp
  cases hp with
  | step r hr _ => exact h r hr rfl

private theorem terminalCount_eq : Surface0.Terminal.count = 80 := rfl

private def bootstrapVerifier : TableVerifier (sliceGrammar slice0) where
  program := (compileTableVerifier bootstrapRaw).program
  words := (compileTableVerifier bootstrapRaw).words
  spaceBound := by
    rw [← (sliceGrammar slice0).rawGrammar_budget]
    exact compileTableVerifier_spaceBound bootstrapRaw
  completes := by
    intro memory hsize
    rw [← (sliceGrammar slice0).rawGrammar_budget]
    exact compileTableVerifier_completes bootstrapRaw memory hsize
  sound := by
    intro memory hsize
    dsimp only
    intro hacc
    have haccept : (compileTableVerifier bootstrapRaw).accepts memory bootstrapRaw.budget = true := by
      simp only [Compiled.accepts, Bool.and_eq_true, beq_iff_eq]
      refine ⟨hsize, ?_⟩
      change (verifyRun (compileTableVerifier bootstrapRaw).program ⟨0, memory⟩ (sliceGrammar slice0).verificationBudget).2 = some true at hacc
      rw [← (sliceGrammar slice0).rawGrammar_budget] at hacc
      exact hacc
    refine ⟨bootstrapTable, bootstrapProductive, ?_, ?_⟩
    · rw [← (sliceGrammar slice0).rawGrammar_positions, ← (sliceGrammar slice0).rawGrammar_budget]
      exact accepted_work_covers_grammar haccept
    · intro i
      have hdim : bootstrapTable.cells.size = 81 := by decide
      have hi : i.val < 81 := hdim ▸ i.isLt
      have hmemSize : memory.size = 1255 := by
        simpa [compileTableVerifier, bootstrapRaw_representable, bootstrapRaw_scratch] using hsize
      have hiMem : i.val < memory.size := by omega
      have hvalid := compileTableVerifier_sound haccept
      have hcellRules := hvalid.2.2.1 0 (by decide) i.val (by simpa [bootstrapRaw_layout_eq, terminalCount_eq] using hi)
      have hcellAddr : bootstrapRaw.layout.cell 0 i.val = i.val := by
        simp [Layout.cell]
      have hmemAt : memory[i.val]! = memory[i.val]'(hiMem) := getElem!_pos memory i.val hiMem
      have hreturnAt : returnCells[i.val]? =
          some (if terminalColumn (some .fn) = i.val then some 0 else none) := by
        have fits : terminalColumn (some .fn) < (sliceGrammar slice0).terminalColumns := by decide
        have hiCol : i.val < (sliceGrammar slice0).terminalColumns := by
          change i.val < Surface0.Terminal.count + 1; rw [terminalCount_eq]; exact hi
        simp only [returnCells, Array.set!_eq_setIfInBounds, Array.getElem?_setIfInBounds,
          Array.getElem?_replicate, Array.size_replicate, fits, hiCol, ↓reduceIte, apply_ite some]
      refine ⟨memory[i.val]'(hiMem), Array.getElem?_eq_getElem hiMem, ?_⟩
      change returnCells[i.val]? = _
      rw [hreturnAt]
      by_cases hfn : i.val = Surface0.Terminal.fn.ctorIdx + 1
      · have hprov : Provable (grammarRules bootstrapRaw) (.suffixFirst 0 (Surface0.Terminal.fn.ctorIdx + 1)) :=
          .step _ bootstrap_fn_suffixFirst0 (by simp)
        have hpos : 0 < memory[(Fact.suffixFirst 0 (Surface0.Terminal.fn.ctorIdx + 1)).address bootstrapRaw.layout]!.toNat :=
          (accepted_symbolic_exact haccept (by decide)).2 hprov
        have hbase0 : (bootstrapRaw.rows[0]'(by decide)).base = 0 := rfl
        have hidx0 : (bootstrapRaw.rows[0]'(by decide)).index = 0 := rfl
        have hpred : bootstrapRaw.layout.PredictsAt (fun a => memory[a]!)
            (bootstrapRaw.rows[0]'(by decide)) i.val := by
          left
          rw [hbase0, hfn]
          exact hpos
        have hrowMem : bootstrapRaw.rows[0]'(by decide) ∈ bootstrapRaw.rows := List.getElem_mem _
        have hword := hcellRules.2 _ hrowMem rfl hpred
        dsimp only at hword
        rw [hcellAddr, hmemAt, hidx0] at hword
        have htc : terminalColumn (some .fn) = i.val := hfn.symm
        simp only [htc, ↓reduceIte, hword]
        rfl
      · have hnotNull : ¬ (0 < memory[(Fact.suffixNull 0).address bootstrapRaw.layout]!.toNat) := by
          intro hpos
          exact not_provable_of_no_head bootstrap_no_suffixNull0
            ((accepted_symbolic_exact haccept (by decide)).1 hpos)
        have hnotFirst : ¬ (0 < memory[(Fact.suffixFirst 0 i.val).address bootstrapRaw.layout]!.toNat) := by
          intro hpos
          have hfvalid : (Fact.suffixFirst 0 i.val).valid bootstrapRaw.layout = true := by
            simp only [Fact.valid, bootstrapRaw_layout_eq, terminalCount_eq, Bool.and_eq_true]
            exact ⟨by decide, decide_eq_true hi⟩
          have hno : ∀ r ∈ grammarRules bootstrapRaw, r.head ≠ Fact.suffixFirst 0 i.val :=
            fun r hr hEq => hfn (bootstrap_other_suffixFirst0 r hr i.val hEq)
          exact not_provable_of_no_head hno
            ((accepted_symbolic_exact haccept hfvalid).1 hpos)
        have hzero : ¬ (0 < memory[i.val]!.toNat) := by
          intro hpos
          rw [← hcellAddr] at hpos
          obtain ⟨r, hr, _, hpred⟩ := hcellRules.1 hpos
          have hr0 : r = bootstrapRaw.rows[0]'(by decide) := by
            change r ∈ [bootstrapRaw.rows[0]'(by decide)] at hr
            exact List.mem_singleton.mp hr
          subst hr0
          rcases hpred with hf | ⟨hn, _⟩
          · exact hnotFirst hf
          · exact hnotNull hn
        rw [hmemAt] at hzero
        have hw0 : memory[i.val]'(hiMem) = 0 := by
          have hnat : (memory[i.val]'(hiMem)).toNat = 0 := by omega
          exact UInt64.toNat_inj.mp hnat
        have htc : ¬ (terminalColumn (some .fn) = i.val) := fun h => hfn h.symm
        simp only [htc, ↓reduceIte, hw0]
        rfl

private def bootstrapLayout : GrammarVerification.Layout := ⟨1, 81, 12⟩

private def bootstrapEntries : List (Nat × UInt64) :=
  let l := bootstrapLayout
  [(l.cell 0 0 + 1, 1),
   (l.rank 0, 1),
   (l.productive 0, 13),
   (l.firstNt 0 1, 2),
   (l.follow 0 0, 1),
   (l.prefixNull 0, 1),
   (l.suffixNull 11, 1),
   (l.suffixProductive 0, 12),
   (l.suffixProductive 1, 11),
   (l.suffixProductive 2, 10),
   (l.suffixProductive 3, 9),
   (l.suffixProductive 4, 8),
   (l.suffixProductive 5, 7),
   (l.suffixProductive 6, 6),
   (l.suffixProductive 7, 5),
   (l.suffixProductive 8, 4),
   (l.suffixProductive 9, 3),
   (l.suffixProductive 10, 2),
   (l.suffixProductive 11, 1),
   (l.suffixFirst 0 (Surface0.Terminal.fn.ctorIdx + 1), 1),
   (l.suffixFirst 1 (Surface0.Terminal.ident.ctorIdx + 1), 1),
   (l.suffixFirst 2 (Surface0.Terminal.lparen.ctorIdx + 1), 1),
   (l.suffixFirst 3 (Surface0.Terminal.rparen.ctorIdx + 1), 1),
   (l.suffixFirst 4 (Surface0.Terminal.arrow.ctorIdx + 1), 1),
   (l.suffixFirst 5 (Surface0.Terminal.u8.ctorIdx + 1), 1),
   (l.suffixFirst 6 (Surface0.Terminal.lbrace.ctorIdx + 1), 1),
   (l.suffixFirst 7 (Surface0.Terminal.return_.ctorIdx + 1), 1),
   (l.suffixFirst 8 (Surface0.Terminal.integer.ctorIdx + 1), 1),
   (l.suffixFirst 9 (Surface0.Terminal.semi.ctorIdx + 1), 1),
   (l.suffixFirst 10 (Surface0.Terminal.rbrace.ctorIdx + 1), 1)]

private def bootstrapWord (a : Nat) : UInt64 :=
  (bootstrapEntries.lookup a).getD 0

private def bootstrapProposal : Array UInt64 :=
  Array.ofFn fun i : Fin 1255 => bootstrapWord i.val

private theorem lookup_mem_fst {α : Type} :
    ∀ (xs : List (Nat × α)) (a : Nat) (v : α), xs.lookup a = some v → a ∈ xs.map Prod.fst
  | [], _, _, h => by cases h
  | (k, w) :: rest, a, v, h => by
    simp only [List.lookup, List.map_cons, List.mem_cons] at h ⊢
    cases hbeq : (a == k) with
    | true => exact Or.inl (eq_of_beq hbeq)
    | false =>
      rw [hbeq] at h
      exact Or.inr (lookup_mem_fst rest a v h)

private theorem bootstrapWord_pos_mem {a : Nat} (h : 0 < (bootstrapWord a).toNat) :
    a ∈ bootstrapEntries.map Prod.fst := by
  unfold bootstrapWord at h
  cases hlook : bootstrapEntries.lookup a with
  | none => simp [hlook] at h
  | some v => exact lookup_mem_fst bootstrapEntries a v hlook

private theorem bootstrapEntries_lt :
    bootstrapEntries.all (fun e => decide (e.1 < 1255)) = true := by
  decide

private theorem bootstrapWord_outside (a : Nat) (h : 1255 ≤ a) : bootstrapWord a = 0 := by
  by_cases hpos : 0 < (bootstrapWord a).toNat
  · have hmem := bootstrapWord_pos_mem hpos
    obtain ⟨e, he, rfl⟩ := List.mem_map.mp hmem
    have hlt : e.1 < 1255 := of_decide_eq_true (List.all_eq_true.mp bootstrapEntries_lt e he)
    omega
  · have h0 : (bootstrapWord a).toNat = (0 : UInt64).toNat := by
      have hz : (0 : UInt64).toNat = 0 := rfl
      omega
    exact UInt64.toNat_inj.mp h0

private theorem bootstrapProposal_size : bootstrapProposal.size = 1255 := by
  rw [bootstrapProposal, Array.size_ofFn]

private theorem bootstrapProposal_getElem (i : Nat) (hi : i < 1255) :
    bootstrapProposal[i]'(bootstrapProposal_size ▸ hi) = bootstrapWord i := by
  simp only [bootstrapProposal, Array.getElem_ofFn]

private theorem getElemBang_ofFn_u64 {n : Nat} (f : Nat → UInt64) (a : Nat)
    (hout : ∀ x, n ≤ x → f x = 0) :
    (Array.ofFn (fun i : Fin n => f i.val))[a]! = f a := by
  by_cases ha : a < n
  · have hlt : a < (Array.ofFn (fun i : Fin n => f i.val)).size := by
      rw [Array.size_ofFn]; exact ha
    rw [getElem!_pos _ a hlt, Array.getElem_ofFn]
  · have hge : ¬ a < (Array.ofFn (fun i : Fin n => f i.val)).size := by
      rw [Array.size_ofFn]; exact ha
    rw [getElem!_neg _ a hge]
    exact (hout a (Nat.le_of_not_lt ha)).symm

private theorem bootstrapProposal_getElemBang (a : Nat) :
    bootstrapProposal[a]! = bootstrapWord a :=
  getElemBang_ofFn_u64 bootstrapWord a bootstrapWord_outside

private theorem bootstrapWord_closed :
    Closed (grammarClauses bootstrapRaw).toList (bootstrapRaw.layout.height bootstrapWord) := by
  have h : (grammarClauses bootstrapRaw).toList.all (fun r =>
      !(r.premises.all fun p => decide (0 < bootstrapRaw.layout.height bootstrapWord p)) ||
      decide (0 < bootstrapRaw.layout.height bootstrapWord r.head)) = true := by
    decide
  intro r hr hpre
  have hr' := List.all_eq_true.mp h r hr
  simp only [Bool.or_eq_true, Bool.not_eq_true', decide_eq_true_eq] at hr'
  rcases hr' with hfalse | hhead
  · exfalso
    have htrue : (r.premises.all fun p => decide (0 < bootstrapRaw.layout.height bootstrapWord p)) = true :=
      List.all_eq_true.mpr fun p hp => decide_eq_true (hpre p hp)
    rw [htrue] at hfalse
    cases hfalse
  · exact hhead

private theorem bootstrapWord_justified :
    Justified (grammarClauses bootstrapRaw).toList (bootstrapRaw.layout.height bootstrapWord) := by
  have h : (bootstrapEntries.map Prod.fst).all (fun a =>
      !decide (0 < bootstrapRaw.layout.height bootstrapWord a) ||
      (grammarClauses bootstrapRaw).toList.any (fun r =>
        decide (r.head = a) &&
        r.premises.all (fun p =>
          decide (0 < bootstrapRaw.layout.height bootstrapWord p) &&
          decide (bootstrapRaw.layout.height bootstrapWord p < bootstrapRaw.layout.height bootstrapWord a)))) = true := by
    decide
  intro a ha
  have hposWord : 0 < (bootstrapWord a).toNat := by
    unfold Layout.height at ha
    split at ha
    · exact ha
    · omega
  have hmem := bootstrapWord_pos_mem hposWord
  have ha' := List.all_eq_true.mp h a hmem
  simp only [Bool.or_eq_true, Bool.not_eq_true', decide_eq_false_iff_not,
    List.any_eq_true, Bool.and_eq_true, decide_eq_true_eq, List.all_eq_true] at ha'
  rcases ha' with hnot | ⟨r, hr, hhead, hpre⟩
  · exact False.elim (hnot ha)
  · exact ⟨r, hr, hhead, hpre⟩

private theorem bootstrapWord_cells :
    ∀ n < bootstrapRaw.layout.n, ∀ col < bootstrapRaw.layout.c,
      ((0 < (bootstrapWord (bootstrapRaw.layout.cell n col)).toNat →
          ∃ r ∈ bootstrapRaw.rows, r.lhs = n ∧ bootstrapRaw.layout.PredictsAt bootstrapWord r col) ∧
       ∀ r ∈ bootstrapRaw.rows, r.lhs = n →
          bootstrapRaw.layout.PredictsAt bootstrapWord r col →
          bootstrapWord (bootstrapRaw.layout.cell n col) = (r.index + 1).toUInt64) := by
  have h : (List.finRange 81).all (fun col =>
      (!decide (0 < (bootstrapWord (bootstrapRaw.layout.cell 0 col.val)).toNat) ||
        bootstrapRaw.rows.any (fun r =>
          decide (r.lhs = 0) &&
          (decide (0 < (bootstrapWord (bootstrapRaw.layout.suffixFirst r.base col.val)).toNat) ||
           (decide (0 < (bootstrapWord (bootstrapRaw.layout.suffixNull r.base)).toNat) &&
            decide (0 < (bootstrapWord (bootstrapRaw.layout.follow r.lhs col.val)).toNat))))) &&
      bootstrapRaw.rows.all (fun r =>
        !(decide (r.lhs = 0) &&
          (decide (0 < (bootstrapWord (bootstrapRaw.layout.suffixFirst r.base col.val)).toNat) ||
           (decide (0 < (bootstrapWord (bootstrapRaw.layout.suffixNull r.base)).toNat) &&
            decide (0 < (bootstrapWord (bootstrapRaw.layout.follow r.lhs col.val)).toNat)))) ||
        decide (bootstrapWord (bootstrapRaw.layout.cell 0 col.val) = (r.index + 1).toUInt64))) = true := by
    decide
  intro n hn col hcol
  have hn0 : n = 0 := by
    change n < 1 at hn; omega
  subst hn0
  have hcol81 : col < 81 := by
    change col < Surface0.Terminal.count + 1 at hcol
    rw [terminalCount_eq] at hcol
    exact hcol
  have hc := List.all_eq_true.mp h ⟨col, hcol81⟩ (List.mem_finRange _)
  simp only [Bool.and_eq_true, Bool.or_eq_true, Bool.not_eq_true', decide_eq_false_iff_not,
    decide_eq_true_eq, List.any_eq_true, List.all_eq_true] at hc
  constructor
  · intro hpos
    rcases hc.1 with hnot | ⟨r, hr, hlhs, hpred⟩
    · exact False.elim (hnot hpos)
    · exact ⟨r, hr, hlhs, hpred⟩
  · intro r hr hlhs hpred
    have h2 := hc.2 r hr
    rcases h2 with hnot | heq
    · exfalso
      simp only [Bool.and_eq_false_iff, Bool.or_eq_false_iff, decide_eq_false_iff_not] at hnot
      rcases hnot with hnlhs | ⟨hnf, hnn⟩
      · exact hnlhs hlhs
      · rcases hpred with hf | ⟨hn1, hn2⟩
        · exact hnf hf
        · rcases hnn with hnn1 | hnn2
          · exact hnn1 hn1
          · exact hnn2 hn2
    · exact heq

private theorem bootstrapWord_valid : ValidCertificate bootstrapRaw bootstrapWord := by
  refine ⟨bootstrapWord_closed, bootstrapWord_justified, bootstrapWord_cells, ?_, ?_⟩
  · intro r hr child i hmem
    have hr0 : r = bootstrapRaw.rows[0]'(by decide) := by
      change r ∈ [bootstrapRaw.rows[0]'(by decide)] at hr
      exact List.mem_singleton.mp hr
    subst hr0
    have hno : ∀ p ∈ (bootstrapRaw.rows[0]'(by decide)).rhs.zipIdx, ∀ c, p.1 ≠ Sym.n c := by
      intro p hp c
      have hmem' := List.mem_zipIdx_iff_getElem?.mp hp
      have hall : (bootstrapRaw.rows[0]'(by decide)).rhs.all (fun | .t _ => true | .n _ => false) = true := by
        decide
      have hp' := List.all_eq_true.mp hall p.1 (List.mem_of_getElem? hmem')
      cases h1 : p.1 with
      | t _ => simp
      | n _ => simp [h1] at hp'
    exact False.elim (hno _ hmem child rfl)
  · intro n hn
    have hn0 : n = 0 := by change n < 1 at hn; omega
    subst hn0
    decide

private theorem bootstrapProposal_accepted :
    (bootstrapVerifier.run bootstrapProposal).2 = some true := by
  have hvalid : ValidCertificate bootstrapRaw (fun a => bootstrapProposal[a]!) := by
    have hfun : (fun a => bootstrapProposal[a]!) = bootstrapWord :=
      funext bootstrapProposal_getElemBang
    rw [hfun]
    exact bootstrapWord_valid
  have hdims : bootstrapProposal.size = bootstrapRaw.layout.scratch + 1 := by
    rw [bootstrapProposal_size, bootstrapRaw_scratch]
  have hacc := compileTableVerifier_complete bootstrapRaw_representable hdims hvalid
  simp only [Compiled.accepts, Bool.and_eq_true, beq_iff_eq] at hacc
  change (verifyRun (compileTableVerifier bootstrapRaw).program ⟨0, bootstrapProposal⟩
    (sliceGrammar slice0).verificationBudget).2 = some true
  rw [← (sliceGrammar slice0).rawGrammar_budget]
  exact hacc.2

private def bootstrapVerification : VerificationInputs (sliceGrammar slice0) where
  verifier := bootstrapVerifier
  verifierCompiled := ⟨rfl, rfl⟩
  proposal := bootstrapProposal
  proposalSize := by
    rw [bootstrapProposal_size]
    simp [bootstrapVerifier, compileTableVerifier, bootstrapRaw_representable, bootstrapRaw_scratch]
  accepted := bootstrapProposal_accepted
  engine := ⟨bootstrapTable, 16, earlyMalformedRule, earlyUnsupportedProductionRule, earlyUnsupportedLiteralRule⟩
  engineTableEq := by
    apply Array.ext
    · simp only [bootstrapTable, returnCells, acceptedCells, Array.size_map, Array.size_extract,
        Array.set!_eq_setIfInBounds, Array.size_setIfInBounds, Array.size_replicate,
        bootstrapProposal_size, sliceGrammar, bootstrapGrammar, SharedGrammar.terminalColumns,
        terminalCount_eq]
      decide
    · intro i hi1 hi2
      have hi : i < 81 := by
        simpa [bootstrapTable, returnCells, Array.set!_eq_setIfInBounds, sliceGrammar,
          bootstrapGrammar, SharedGrammar.terminalColumns, terminalCount_eq] using hi1
      have h1 : bootstrapTable.cells[i]'(by simpa [bootstrapTable, returnCells, Array.set!_eq_setIfInBounds,
          sliceGrammar, bootstrapGrammar, SharedGrammar.terminalColumns, terminalCount_eq] using hi) =
          if terminalColumn (some .fn) = i then some 0 else none := by
        have hiCol : i < (sliceGrammar slice0).terminalColumns := by
          change i < Surface0.Terminal.count + 1; rw [terminalCount_eq]; exact hi
        simp [bootstrapTable, returnCells, Array.set!_eq_setIfInBounds, Array.getElem_setIfInBounds, hiCol]
      have h2 : (acceptedCells bootstrapProposal (sliceGrammar slice0))[i]'(by
          simp only [acceptedCells, Array.size_map, Array.size_extract, bootstrapProposal_size,
            sliceGrammar, sliceGrammarCount, bootstrapGrammar, SharedGrammar.terminalColumns,
            Array.size_replicate, terminalCount_eq]
          omega) =
          if bootstrapWord i == 0 then none else some ((bootstrapWord i).toNat - 1) := by
        have hi1255 : i < 1255 := by omega
        simp only [acceptedCells, Array.getElem_map, Array.getElem_extract, Nat.zero_add]
        rw [bootstrapProposal_getElem i hi1255]
      dsimp only
      rw [h1, h2]
      have hcheck : (List.finRange 81).all (fun j =>
          decide ((if terminalColumn (some .fn) = j.val then some 0 else none) =
            (if bootstrapWord j.val == 0 then none else some ((bootstrapWord j.val).toNat - 1)))) = true := by
        decide
      exact of_decide_eq_true (List.all_eq_true.mp hcheck ⟨i, hi⟩ (List.mem_finRange _))

def sliceVerifierInputs (slice : Slice) : VerificationInputs (sliceGrammar slice) :=
  bootstrapVerification

def sliceParser (slice : Slice) : ParserEngine (sliceGrammar slice) :=
  (sliceVerifierInputs slice).engine

end ConcreteSpec.StageGrammar

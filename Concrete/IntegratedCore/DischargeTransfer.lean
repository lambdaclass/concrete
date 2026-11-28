import ConcreteSpec.Admission.Early

/-! Discharge transfer and admission soundness for the straight-line Core
fragments shared by Slices 0 and 1. -/
namespace Concrete.IntegratedCore.DischargeTransfer
open ConcreteSpec ConcreteSpec.Core ConcreteSpec.StageInterface ConcreteSpec.Admission

/-! ### Constructive list, `Option`, and `Except` helpers (avoiding `Classical.choice`) -/

private theorem isSublist_refl {α : Type} [BEq α] [LawfulBEq α] :
    ∀ (xs : List α), xs.isSublist xs = true
  | [] => rfl
  | x :: xs => by simp only [List.isSublist, beq_self_eq_true, ↓reduceIte]; exact isSublist_refl xs

private theorem isSublist_nil {α : Type} [BEq α] : ∀ (ys : List α), ([] : List α).isSublist ys = true
  | [] => rfl
  | _ :: _ => rfl

private theorem sublist_of_isSublist {α : Type} [BEq α] [LawfulBEq α] :
    ∀ {xs ys : List α}, xs.isSublist ys = true → xs.Sublist ys
  | [], [], _ => .slnil
  | [], y :: ys, _ => .cons y (sublist_of_isSublist (isSublist_nil ys))
  | _ :: _, [], h => by cases h
  | x :: xs, y :: ys, h => by
    simp only [List.isSublist] at h
    cases hxy : (x == y) with
    | true => rw [hxy] at h; obtain rfl := eq_of_beq hxy; exact .cons_cons x (sublist_of_isSublist h)
    | false => rw [hxy] at h; exact .cons y (sublist_of_isSublist h)

private theorem sublist_subset {α : Type} {xs ys : List α} (h : xs.Sublist ys) :
    ∀ {x : α}, x ∈ xs → x ∈ ys := by
  induction h with
  | slnil => exact fun hx => hx
  | cons _ _ ih => exact fun hx => .tail _ (ih hx)
  | cons_cons _ _ ih =>
    intro _ hx
    cases hx with
    | head => exact .head _
    | tail _ hx => exact .tail _ (ih hx)

private theorem sublist_map {α β : Type} (f : α → β) {xs ys : List α} (h : xs.Sublist ys) :
    (xs.map f).Sublist (ys.map f) := by
  induction h with
  | slnil => exact .slnil
  | cons a _ ih => exact .cons (f a) ih
  | cons_cons a _ ih => exact .cons_cons (f a) ih

private theorem nodup_of_sublist {α : Type} {xs ys : List α} (h : xs.Sublist ys) :
    ys.Nodup → xs.Nodup := by
  induction h with
  | slnil => exact id
  | cons _ _ ih => exact fun hn => ih (List.nodup_cons.mp hn).2
  | cons_cons _ sub ih =>
    intro hn
    obtain ⟨hnotin, htail⟩ := List.nodup_cons.mp hn
    exact List.nodup_cons.mpr ⟨fun hx => hnotin (sublist_subset sub hx), ih htail⟩

private theorem nodup_map_of_inj {α β : Type} {f : α → β} (hinj : ∀ a b, f a = f b → a = b)
    {xs : List α} (hn : xs.Nodup) : (xs.map f).Nodup := by
  induction xs with
  | nil => exact List.nodup_nil
  | cons x rest ih =>
    obtain ⟨hnotin, htail⟩ := List.nodup_cons.mp hn
    refine List.nodup_cons.mpr ⟨fun hmem => ?_, ih htail⟩
    obtain ⟨y, hy, heq⟩ := List.mem_map.mp hmem
    exact hnotin (hinj y x heq ▸ hy)

private theorem find?_isSome_of_mem {α : Type} {p : α → Bool} {xs : List α}
    (hex : ∃ x ∈ xs, p x = true) : ∃ y, xs.find? p = some y := by
  induction xs with
  | nil => obtain ⟨_, hmem, _⟩ := hex; cases hmem
  | cons x rest ih =>
    cases hp : p x with
    | true => exact ⟨x, by simp [List.find?, hp]⟩
    | false =>
      obtain ⟨y, hy, hpy⟩ := hex
      cases hy with
      | head => rw [hp] at hpy; cases hpy
      | tail _ htail =>
        obtain ⟨z, hz⟩ := ih ⟨y, htail, hpy⟩
        exact ⟨z, by simp [List.find?, hp, hz]⟩

private inductive ListRel {α β : Type} (P : α → β → Prop) : List α → List β → Prop
  | nil : ListRel P [] []
  | cons {x y xs ys} : P x y → ListRel P xs ys → ListRel P (x :: xs) (y :: ys)

private theorem mapM_option_rel {α β : Type} {f : α → Option β} {P : α → β → Prop}
    {xs : List α} {ys : List β} (h : xs.mapM f = some ys) (hp : ∀ x y, f x = some y → P x y) :
    ListRel P xs ys := by
  induction xs generalizing ys with
  | nil => rw [List.mapM_nil] at h; cases h; exact .nil
  | cons x rest ih =>
    rw [List.mapM_cons] at h
    cases hfx : f x with
    | none => simp [hfx] at h
    | some y0 =>
      cases hrest : rest.mapM f with
      | none => simp [hfx, hrest] at h
      | some ys' =>
        simp [hfx, hrest] at h; subst ys
        exact .cons (hp x y0 hfx) (ih hrest)

private theorem mapM_option_isSome {α β : Type} {f : α → Option β} {xs : List α}
    (hall : ∀ x ∈ xs, ∃ y, f x = some y) : ∃ ys, xs.mapM f = some ys := by
  induction xs with
  | nil => exact ⟨[], List.mapM_nil⟩
  | cons x rest ih =>
    obtain ⟨y, hy⟩ := hall x (.head _)
    obtain ⟨ys, hys⟩ := ih (fun a ha => hall a (.tail _ ha))
    exact ⟨y :: ys, by rw [List.mapM_cons]; simp [hy, hys]⟩

private theorem listRel_map_eq {α β γ : Type} {P : α → β → Prop} {g : β → γ} {k : α → γ}
    {xs : List α} {ys : List β} (h : ListRel P xs ys) (heq : ∀ x y, P x y → g y = k x) :
    ys.map g = xs.map k := by
  induction h with
  | nil => rfl
  | cons hp _ ih => simp only [List.map_cons, heq _ _ hp, ih]

private theorem listRel_mem_right {α β : Type} {P : α → β → Prop} {xs : List α} {ys : List β}
    (h : ListRel P xs ys) {y : β} (hy : y ∈ ys) : ∃ x ∈ xs, P x y := by
  induction h with
  | nil => cases hy
  | cons hp _ ih =>
    cases hy with
    | head => exact ⟨_, .head _, hp⟩
    | tail _ htail => obtain ⟨x, hx, hpx⟩ := ih htail; exact ⟨x, .tail _ hx, hpx⟩

private theorem getElem?_some_of_lt {α : Type} :
    ∀ (xs : List α) {i : Nat}, i < xs.length → ∃ x, xs[i]? = some x
  | [], _, hi => (Nat.not_lt_zero _ hi).elim
  | x :: _, 0, _ => ⟨x, rfl⟩
  | _ :: rest, _ + 1, hi => getElem?_some_of_lt rest (Nat.lt_of_succ_lt_succ hi)

private theorem lt_of_getElem?_eq_some {α : Type} :
    ∀ {xs : List α} {i : Nat} {x : α}, xs[i]? = some x → i < xs.length
  | [], _, _, h => by cases h
  | _ :: _, 0, _, _ => Nat.zero_lt_succ _
  | _ :: rest, i + 1, _, h => Nat.succ_lt_succ (lt_of_getElem?_eq_some h)

private theorem getElem?_zipIdx_eq {α : Type} :
    ∀ (xs : List α) (start i : Nat),
      (xs.zipIdx start)[i]? = (xs[i]?).map (fun x => (x, start + i))
  | [], _, _ => rfl
  | _ :: _, _, 0 => by simp [List.zipIdx]
  | _ :: rest, start, i + 1 => by
    simp only [List.zipIdx, List.getElem?_cons_succ]
    rw [getElem?_zipIdx_eq rest (start + 1) i, Nat.add_right_comm, Nat.add_assoc]

private theorem bind_error_iff {ε α β : Type} {r : Except ε α} {f : α → Except ε β} {e : ε} :
    (r >>= f) = .error e ↔ r = .error e ∨ ∃ a, r = .ok a ∧ f a = .error e := by
  cases r <;> simp [Bind.bind, Except.bind]

private theorem bind_ok_iff {ε α β : Type} {r : Except ε α} {f : α → Except ε β} {b : β} :
    (r >>= f) = .ok b ↔ ∃ a, r = .ok a ∧ f a = .ok b := by
  cases r <;> simp [Bind.bind, Except.bind]

private theorem mapError_error_iff {ε₁ ε₂ α : Type} {f : ε₁ → ε₂} {r : Except ε₁ α} {e : ε₂} :
    r.mapError f = .error e ↔ ∃ e₁, r = .error e₁ ∧ f e₁ = e := by
  cases r <;> simp [Except.mapError]

private theorem option_bind_some {α β : Type} (a : α) (f : α → Option β) :
    (some a >>= f) = f a := rfl

private theorem option_bind_eq_some {α β : Type} {x : Option α} {f : α → Option β} {y : β}
    (h : (x >>= f) = some y) : ∃ a, x = some a ∧ f a = some y := by
  cases x with
  | none => cases h
  | some a => exact ⟨a, rfl, h⟩

/-! ### `AdmissionView` origins, operations, and discharge encoding totality -/

theorem view_program_eq (v : AdmissionView) :
    v.program = ⟨List.map Prod.snd v.located.decls,
      v.located.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement),
      v.located.ending.toCore v.located.endingOrigin.1⟩ := by
  cases v with
  | zero p =>
    simp [AdmissionView.program, Slice0.toCore, EarlyProgram.eraseLocations,
      AdmissionView.located, StraightProgram.decls, StraightEnd.toCore,
      StraightBinding.toCore, Function.comp_def]
  | one p =>
    cases h : p.ending <;>
      simp [AdmissionView.program, AdmissionView.located, h,
        StraightEnd.toCore, StraightProgram.endingOrigin] <;> rfl

private theorem mapSiblingKeys_getElem?_some {α β : Type} (kind : ConstructKind)
    (name : α → ConstructName) (emit : α → FunctionRelativeKey → β) :
    ∀ (preceding : List KeyConstruct) (xs : List α) (i : Nat) {x : α},
      xs[i]? = some x →
      ∃ rel, (KeyConstruct.mapSiblingKeys kind name emit preceding xs)[i]? = some (emit x rel)
  | _, [], _, _, h => by cases h
  | _, _ :: _, 0, _, h => by cases h; exact ⟨_, rfl⟩
  | preceding, x₀ :: rest, i + 1, _, h =>
    mapSiblingKeys_getElem?_some kind name emit (preceding ++ [⟨kind, name x₀, []⟩]) rest i h

private theorem mapSiblingKeys_getElem?_eq {α β : Type} (kind : ConstructKind)
    (name : α → ConstructName) (emit : α → FunctionRelativeKey → β) :
    ∀ (preceding : List KeyConstruct) (xs : List α) (i : Nat) {y : β},
      (KeyConstruct.mapSiblingKeys kind name emit preceding xs)[i]? = some y →
      ∃ x rel, xs[i]? = some x ∧ y = emit x rel
  | _, [], _, _, h => by cases h
  | _, x₀ :: _, 0, _, h => by cases h; exact ⟨x₀, _, rfl, rfl⟩
  | preceding, x₀ :: rest, i + 1, _, h =>
    mapSiblingKeys_getElem?_eq kind name emit (preceding ++ [⟨kind, name x₀, []⟩]) rest i h

private def emitBindingOrigin (pair : LocatedStraightBinding × Nat) (rel : FunctionRelativeKey) :
    SiteId × Origin :=
  (⟨pair.2⟩, ⟨⟨pair.2⟩,
    ⟨pair.1.operationSpan.fileId, pair.1.operationSpan.start, pair.1.operationSpan.stop,
      pair.1.operationSpan.line, pair.1.operationSpan.column⟩,
    ⟨.function earlyEntry, rel⟩⟩)

private theorem bindingOrigins_getElem?_some (p : StraightProgram) {pos : Nat}
    (hlt : pos < p.bindings.length) :
    ∃ origin, p.bindingOrigins[pos]? = some (⟨pos⟩, origin) ∧ origin.operation = ⟨pos⟩ := by
  obtain ⟨row, hrow⟩ := getElem?_some_of_lt p.bindings hlt
  have hzip : (p.bindings.zipIdx 0)[pos]? = some (row, pos) := by
    rw [getElem?_zipIdx_eq, hrow, Nat.zero_add]; rfl
  obtain ⟨rel, hrel⟩ := mapSiblingKeys_getElem?_some .binding (fun r => r.1.name)
    emitBindingOrigin [] (p.bindings.zipIdx 0) pos hzip
  exact ⟨(emitBindingOrigin (row, pos) rel).2, hrel, rfl⟩

private theorem bindingOrigins_getElem?_eq (p : StraightProgram) {pos : Nat}
    {entry : SiteId × Origin} (h : p.bindingOrigins[pos]? = some entry) :
    entry.1 = ⟨pos⟩ ∧ entry.2.operation = ⟨pos⟩ := by
  have h' : (KeyConstruct.mapSiblingKeys .binding (fun r : LocatedStraightBinding × Nat => r.1.name)
      emitBindingOrigin [] (p.bindings.zipIdx 0))[pos]? = some entry := h
  obtain ⟨pair, rel, hzip, heq⟩ := mapSiblingKeys_getElem?_eq .binding
    (fun r : LocatedStraightBinding × Nat => r.1.name)
    emitBindingOrigin [] (p.bindings.zipIdx 0) pos h'
  rw [getElem?_zipIdx_eq, Nat.zero_add] at hzip
  cases hrow : p.bindings[pos]? with
  | none => rw [hrow] at hzip; cases hzip
  | some row => rw [hrow] at hzip; cases hzip; rw [heq]; exact ⟨rfl, rfl⟩

theorem originAt_spec (v : AdmissionView) {pos : Nat} {origin : Origin}
    (h : v.originAt pos = some origin) :
    origin.operation = ⟨pos⟩ ∧ (⟨pos⟩, origin) ∈ v.execution.origins := by
  unfold AdmissionView.originAt at h
  by_cases heq : pos = v.located.bindings.length
  · rw [ite_eq_left heq] at h; cases h; subst heq
    exact ⟨rfl, List.mem_append_right _ (List.Mem.head _)⟩
  · rw [ite_eq_right heq] at h
    cases hget : v.located.bindingOrigins[pos]? with
    | none => rw [hget] at h; cases h
    | some entry =>
      rw [hget] at h; cases h
      obtain ⟨h1, h2⟩ := bindingOrigins_getElem?_eq v.located hget
      refine ⟨h2, List.mem_append_left _ ?_⟩
      have hmem : entry ∈ v.located.bindingOrigins := List.mem_of_getElem? hget
      cases entry; simp only at h1; subst h1; exact hmem

theorem originAt_isSome (v : AdmissionView) {pos : Nat}
    (hle : pos ≤ v.located.bindings.length) :
    ∃ origin, v.originAt pos = some origin := by
  unfold AdmissionView.originAt
  by_cases heq : pos = v.located.bindings.length
  · rw [ite_eq_left heq]; exact ⟨v.located.endingOrigin.2, rfl⟩
  · rw [ite_eq_right heq]
    obtain ⟨origin, hget, _⟩ := bindingOrigins_getElem?_some v.located (Nat.lt_of_le_of_ne hle heq)
    exact ⟨origin, by simp [hget]⟩

private theorem machineBody_stmtOperations_lookup {slice owners decls fresh ending}
    (rows : List LocatedStraightBinding) (start pos : Nat) {row : LocatedStraightBinding}
    (atPos : rows[pos]? = some row) :
    AList.lookup (stmtOperations (StraightProgram.machineBody (slice := slice)
      (owners := owners) (decls := decls) fresh ending rows start)) ⟨start + pos⟩ =
      some (CoreOperation.expression (O := (sliceLanguage slice).instrumented owners decls)
        (Sum.inl row.statement.expression)) := by
  induction rows generalizing start pos with
  | nil => cases atPos
  | cons b rest ih =>
    cases pos with
    | zero => cases atPos; simp [StraightProgram.machineBody, stmtOperations, AList.lookup]
    | succ pos' =>
      have hne : (⟨start⟩ : SiteId) ≠ ⟨start + (pos' + 1)⟩ := fun h =>
        by have h' : start = start + (pos' + 1) := congrArg SiteId.id h; omega
      change AList.lookup ((⟨start⟩,
        CoreOperation.expression (O := (sliceLanguage slice).instrumented owners decls)
          (Sum.inl b.statement.expression)) :: _) (⟨start + (pos' + 1)⟩ : SiteId) = _
      rw [AList.lookup_cons, ite_eq_right hne, show start + (pos' + 1) = (start + 1) + pos' by omega]
      exact ih (start + 1) pos' atPos

private theorem lookup_map_fst {κ α β : Type} [DecidableEq κ] (f : α → β) :
    ∀ (l : AList κ α) (k : κ),
      AList.lookup (List.map (fun (site, op) => (site, f op)) l) k = (AList.lookup l k).map f
  | [], _ => rfl
  | (k', a) :: rest, k => by
    simp only [List.map_cons, AList.lookup_cons]
    by_cases hk : k' = k
    · rw [ite_eq_left hk, ite_eq_left hk]; rfl
    · rw [ite_eq_right hk, ite_eq_right hk]; exact lookup_map_fst f rest k

private def eraseOp {language : CoreLanguage} {owners : AList Var Own.Descriptor}
    {decls : Declarations} :
    CoreOperation (language.instrumented owners decls) → CoreOperation language.operands
  | .expression e => .expression e
  | .call fn args => .call fn args
  | .primitive p args => .primitive p args
  | .abort => .abort

private theorem execution_operations_lookup (v : AdmissionView) {pos : Nat}
    {row : LocatedStraightBinding} (atPos : v.located.bindings[pos]? = some row) :
    AList.lookup v.execution.operations ⟨pos⟩ =
      some (CoreOperation.expression (O := (sliceLanguage v.slice).operands) (Sum.inl row.statement.expression)) := by
  have h := machineBody_stmtOperations_lookup (slice := v.slice)
    (owners := v.execution.ownership) (decls := v.execution.declarations) (fresh := v.located.freshBase)
    (ending := v.located.ending.machine (slice := v.slice)
      (owners := v.execution.ownership) (decls := v.execution.declarations) v.located.endingOrigin.1)
    v.located.bindings 0 pos atPos
  rw [Nat.zero_add] at h
  have hflat : v.execution.operations =
      (stmtOperations (StraightProgram.machineBody (slice := v.slice)
        (owners := v.execution.ownership) (decls := v.execution.declarations) v.located.freshBase
        (v.located.ending.machine (slice := v.slice)
          (owners := v.execution.ownership) (decls := v.execution.declarations) v.located.endingOrigin.1)
        v.located.bindings 0)).map (fun (site, op) => (site, eraseOp op)) := by
    unfold MachineProgram.operations AdmissionView.execution
    simp only [List.flatMap, List.map_cons, List.map_nil, List.flatten_cons, List.flatten_nil,
      List.append_nil]
    apply List.map_congr_left
    rintro ⟨_, op⟩ _; cases op <;> rfl
  rw [hflat]
  exact (lookup_map_fst eraseOp _ _).trans (congrArg (Option.map eraseOp) h)

private theorem checkCoreBody_discharge_at {Δ : Decls} {Γ : Ctx} {ss : List Stmt}
    {ending : Terminator} {start : Nat} {ds : List ConcreteSpec.Admission.Discharge}
    (h : checkCoreBody Δ Γ start ss ending = .ok ds)
    {d : ConcreteSpec.Admission.Discharge} (hd : d ∈ ds) :
    start ≤ d.position ∧
    ∃ binder, ss[d.position - start]? =
      some (.let_ binder (.scalar (.int d.type)) (.add d.left d.right)) := by
  induction ss generalizing Γ start ds with
  | nil =>
    simp only [checkCoreBody, checkCoreBodyWith] at h
    cases he : checkEnd Δ Γ ending <;> (rw [he] at h; cases h)
    cases hd
  | cons s rest ih =>
    simp only [checkCoreBody, checkCoreBodyWith, ArithmeticPolicy.discharge] at h
    cases hs : checkStmtWith ArithmeticPolicy.static.permits Δ Γ s with
    | error _ => simp only [hs] at h; cases h
    | ok Γ₁ =>
      simp only [hs] at h
      cases ht : checkCoreBodyWith .static Δ Γ₁ (start + 1) rest ending with
      | error _ => simp only [ht] at h; cases h
      | ok tail =>
        simp only [ht] at h; cases h
        rcases List.mem_append.mp hd with hhead | htail
        · simp only [Option.mem_toList] at hhead
          unfold stmtDischarge at hhead
          cases s with
          | let_ binder ty expr =>
            cases ty with
            | scalar st => cases st with
              | int _ => cases expr with
                | add _ _ => cases hhead; exact ⟨Nat.le_refl _, binder, by simp⟩
                | _ => cases hhead
              | bool => cases hhead
            | struct _ => cases hhead
          | letStruct _ _ _ => cases hhead
        · obtain ⟨hle, binder, hget⟩ := ih ht htail
          exact ⟨Nat.le_trans (Nat.le_succ _) hle, binder,
            by rw [show d.position - start = (d.position - (start + 1)) + 1 by omega]; exact hget⟩

private theorem straightBinding_add_expression {b : StraightBinding} {binder : Binder}
    {t : ConcreteSpec.IntTy} {l r : ConcreteSpec.Atom}
    (h : b.toCore = .let_ binder (.scalar (.int t)) (.add l r)) :
    ∃ a₁ a₂ : Slice0.Atom, b.expression = .add .u8 (earlyOperand a₁) (earlyOperand a₂) := by
  cases b with
  | slice0 s => cases s with | mk _ e => cases e with | atom _ => cases h | add a₁ a₂ => exact ⟨a₁, a₂, rfl⟩
  | bind _ ty rhs =>
    cases ty with
    | nominal _ => cases h
    | u8 => cases rhs with
      | scalar e => cases e with | atom _ => cases h | add a₁ a₂ => exact ⟨a₁, a₂, rfl⟩
      | _ => cases h
  | destructure _ _ => cases h

private theorem earlyAdd_safety_singleton (slice : Slice) (a₁ a₂ : Slice0.Atom) :
    ∃ cond, (sliceLanguage slice).safety (.expression (.inl (.add .u8 (earlyOperand a₁) (earlyOperand a₂)))) = [cond] ∧
      cond.trap = .overflow := by
  cases a₁ <;> cases a₂ <;> exact ⟨_, rfl, rfl⟩

/-- Every validated subset of an admitted early-fragment batch encodes cleanly. -/
theorem encode_validated_isSome {language : CoreLanguage} {input : CoreInput language}
    (batch : InputBatch input) {selected : List ConcreteSpec.Admission.Discharge}
    (hval : batch.batch.validates selected = true) :
    ∃ out, batch.encode selected = some out := by
  rcases input with ⟨project, sl, payload⟩
  rcases batch with ⟨v, ⟨hs, eq, hp⟩, b⟩
  dsimp only at hs hp; cases hs; cases eq; cases hp
  have hsub : selected.Sublist b.discharges := sublist_of_isSublist hval
  have hbody : checkCoreBody v.program.decls [] 0 v.program.body v.program.ending = .ok b.discharges := by
    have hi := b.issued
    unfold checkProgram checkProgramWith at hi
    cases hd : checkDecls v.program.decls <;> (rw [hd] at hi; first | exact hi | cases hi)
  unfold InputBatch.encode
  rw [hval]
  simp only [Bool.not_true, Bool.false_eq_true, ↓reduceIte]
  apply mapM_option_isSome
  intro d hd
  obtain ⟨_, binder, hget⟩ := checkCoreBody_discharge_at hbody (sublist_subset hsub hd)
  rw [Nat.sub_zero, view_program_eq] at hget
  simp only [List.getElem?_map, Function.comp_def] at hget
  cases hrow : v.located.bindings[d.position]? with
  | none => rw [hrow] at hget; cases hget
  | some row =>
    rw [hrow] at hget
    have hcore : row.statement.toCore = .let_ binder (.scalar (.int d.type)) (.add d.left d.right) :=
      Option.some.inj hget
    obtain ⟨origin, horigin⟩ := originAt_isSome v (Nat.le_of_lt (lt_of_getElem?_eq_some hrow))
    obtain ⟨hop, _⟩ := originAt_spec v horigin
    have hlookup : AList.lookup v.execution.operations origin.operation =
        some (.expression (.inl row.statement.expression)) := by
      rw [hop]; exact execution_operations_lookup v hrow
    obtain ⟨a₁, a₂, hexpr⟩ := straightBinding_add_expression hcore
    obtain ⟨cond, hsafety, htrap⟩ := earlyAdd_safety_singleton v.slice a₁ a₂
    rw [hexpr] at hlookup
    refine ⟨(origin.operation, [⟨origin.operation, origin.evidenceKey,
      ⟨origin.operation, .safety, cond.discriminator⟩, cond.trap, cond.operands⟩]), ?_⟩
    dsimp only [CoreInput.machineProgram, Payload.execution]
    rw [horigin, option_bind_some, hlookup, option_bind_some]
    simp only [hsafety, htrap, bne_self_eq_false, Bool.false_eq_true, ↓reduceIte]
    rfl

theorem encode_issued_isSome {language : CoreLanguage} {input : CoreInput language}
    (batch : InputBatch input) :
    ∃ out, batch.encode batch.batch.discharges = some out :=
  encode_validated_isSome batch (isSublist_refl _)

/-! ### Checker refusal rendering totality -/

private theorem checkDeclsFrom_error {seen : List Name} {start : Nat} {ds : Decls}
    {r : ConcreteSpec.Admission.Refusal} (h : checkDeclsFrom seen start ds = .error r) :
    ∃ index, start ≤ index ∧ index < start + ds.length ∧
      ((∃ n, r = .duplicateStructName index n) ∨
       (∃ n, r = .reservedStructName index n) ∨
       (∃ s f, r = .duplicateDeclaredField index s f)) := by
  induction ds generalizing seen start with
  | nil => cases h
  | cons d rest ih =>
    simp only [checkDeclsFrom] at h
    by_cases hdup : d.name ∈ seen
    · rw [ite_eq_left hdup] at h; cases h
      exact ⟨start, Nat.le_refl _, by simp, .inl ⟨d.name, rfl⟩⟩
    · rw [ite_eq_right hdup] at h
      by_cases hres : d.name = ⟨"main"⟩
      · rw [ite_eq_left hres] at h; cases h
        exact ⟨start, Nat.le_refl _, by simp, .inr (.inl ⟨d.name, rfl⟩)⟩
      · rw [ite_eq_right hres] at h
        rcases bind_error_iff.mp h with hrep | ⟨_, _, htail⟩
        · cases hf : firstRepeated? (d.fields.map Prod.fst) with
          | none => rw [hf] at hrep; cases hrep
          | some f => rw [hf] at hrep; cases hrep; exact ⟨start, Nat.le_refl _, by simp, .inr (.inr ⟨d.name, f, rfl⟩)⟩
        · obtain ⟨idx, hge, hlt, hshape⟩ := ih htail
          exact ⟨idx, Nat.le_trans (Nat.le_succ _) hge, by simp at hlt ⊢; omega, hshape⟩

private def IsEndReason : Reason → Prop
  | .unusedBinding _ | .liveLinearBinding _ => True
  | _ => False

private def IsReadReason : Reason → Prop
  | .unknownBinding _ | .movedBinding _ | .copyOfLinear _ | .moveOfCopy _ => True
  | _ => False

private def IsOutOfRangeReason (ty : ConcreteSpec.IntTy) (n : Int) (r : Reason) : Prop :=
  r = .literalOutOfRange ty n ∨
    ∃ op, r = .additionOperandType op (.atomFailure (.outOfRange ty n))

private theorem rejectFound_error_of {α : Type} {found : Option α} {mk : α → Reason} {r : Reason}
    (h : rejectFound found mk = .error r) : ∃ a, r = mk a := by
  cases found with | none => cases h | some a => cases h; exact ⟨a, rfl⟩

private theorem useAtom_error_read {Δ : Decls} {Γ : Ctx} {a : ConcreteSpec.Atom} {r : Reason}
    (h : useAtom Δ Γ a = .error r) : IsReadReason r := by
  cases a with
  | int _ _ | bool _ => cases h
  | copy x | move x =>
    simp only [useAtom] at h
    cases hl : AList.lookup Γ x with
    | none => rw [hl] at h; cases h; trivial
    | some e =>
      rw [hl] at h
      cases hlv : e.live <;> cases hcp : e.ty.isCopy Δ <;> simp [hlv, hcp] at h
      all_goals (subst r; trivial)

private theorem readAtoms_error_read {Δ : Decls} {Γ : Ctx} {as : List ConcreteSpec.Atom}
    {r : Reason} (h : readAtoms Δ Γ as = .error r) : IsReadReason r := by
  induction as generalizing Γ with
  | nil => cases h
  | cons a rest ih =>
    simp only [readAtoms] at h
    cases hu : useAtom Δ Γ a with
    | error _ => rw [hu] at h; cases h; exact useAtom_error_read hu
    | ok _ => rw [hu] at h; exact ih h

private theorem readReason_not_end {r : Reason} (h : IsReadReason r) : ¬ IsEndReason r := by
  intro he; cases r <;> first | exact False.elim h | exact False.elim he

private theorem readReason_not_outOfRange {ty : ConcreteSpec.IntTy} {n : Int} {r : Reason}
    (h : IsReadReason r) : ¬ IsOutOfRangeReason ty n r := by
  intro hl; rcases hl with rfl | ⟨_, rfl⟩ <;> cases h

private theorem inferAddOperand_error_not_end {Γ : TyCtx} {a : ConcreteSpec.Atom} {r : Reason}
    (h : inferAddOperand Γ a = .error r) : ¬ IsEndReason r := by
  simp only [inferAddOperand] at h
  cases hi : inferAtomType Γ a with
  | error _ => rw [hi] at h; cases h; exact nofun
  | ok ty =>
    rw [hi] at h
    cases ty with
    | struct _ => cases h; exact nofun
    | scalar st => cases st with | int _ => cases h | bool => cases h; exact nofun

private theorem checkFieldTypes_error_not_end {Γ : TyCtx} {d : StructDecl}
    {fs : List (Name × ConcreteSpec.Atom)} {r : Reason}
    (h : checkFieldTypes Γ d fs = .error r) : ¬ IsEndReason r := by
  induction fs with
  | nil => cases h
  | cons pair rest ih =>
    obtain ⟨field, atom⟩ := pair
    simp only [checkFieldTypes] at h
    cases hf : AList.lookup d.fields field with
    | none => rw [hf] at h; cases h; exact nofun
    | some expected =>
      rw [hf] at h
      rcases bind_error_iff.mp h with ha | ⟨actual, _, hrest⟩
      · obtain ⟨_, _, rfl⟩ := mapError_error_iff.mp ha; exact nofun
      · by_cases heq : actual = .scalar expected
        · rw [ite_eq_left heq] at hrest; exact ih hrest
        · rw [ite_eq_right heq] at hrest; cases hrest; exact nofun

private theorem inferExprType_error_not_end {Δ : Decls} {Γ : TyCtx} {e : Expr} {r : Reason}
    (h : inferExprType Δ Γ e = .error r) : ¬ IsEndReason r := by
  cases e with
  | atom _ =>
    simp only [inferExprType] at h
    obtain ⟨c, _, rfl⟩ := mapError_error_iff.mp h
    cases c <;> exact nofun
  | add _ _ =>
    simp only [inferExprType] at h
    rcases bind_error_iff.mp h with hl | ⟨lt, _, hr⟩
    · exact inferAddOperand_error_not_end hl
    · rcases bind_error_iff.mp hr with hr' | ⟨rt, _, hcmp⟩
      · exact inferAddOperand_error_not_end hr'
      · by_cases heq : lt = rt
        · rw [ite_eq_left heq] at hcmp; cases hcmp
        · rw [ite_eq_right heq] at hcmp; cases hcmp; exact nofun
  | mk s _ =>
    simp only [inferExprType] at h
    cases hd : findDecl Δ s with
    | none => rw [hd] at h; cases h; exact nofun
    | some d =>
      rw [hd] at h
      rcases bind_error_iff.mp h with hcf | ⟨_, _, hret⟩
      · simp only [checkFields] at hcf
        rcases bind_error_iff.mp hcf with hr1 | ⟨_, _, hcf2⟩
        · obtain ⟨_, rfl⟩ := rejectFound_error_of hr1; exact nofun
        · rcases bind_error_iff.mp hcf2 with hft | ⟨_, _, hr2⟩
          · exact checkFieldTypes_error_not_end hft
          · obtain ⟨_, rfl⟩ := rejectFound_error_of hr2; exact nofun
      · cases hret

private theorem requireExprType_error_not_end {Δ : Decls} {Γ : TyCtx} {expected : Ty} {e : Expr}
    {r : Reason} (h : requireExprType Δ Γ expected e = .error r) : ¬ IsEndReason r := by
  simp only [requireExprType] at h
  rcases bind_error_iff.mp h with hi | ⟨actual, _, hcmp⟩
  · exact inferExprType_error_not_end hi
  · by_cases heq : actual = expected
    · rw [ite_eq_left heq] at hcmp; cases hcmp
    · rw [ite_eq_right heq] at hcmp; cases hcmp; exact nofun

private theorem checkStmtExprWith_error_not_end {ev : Ctx → Expr → Bool} {Δ : Decls} {Γ : Ctx}
    {t : Ty} {e : Expr} {r : Reason} (h : checkStmtExprWith ev Δ Γ t e = .error r) :
    ¬ IsEndReason r := by
  simp only [checkStmtExprWith] at h
  rcases bind_error_iff.mp h with hreq | ⟨_, _, h1⟩
  · exact requireExprType_error_not_end hreq
  · rcases bind_error_iff.mp h1 with hread | ⟨_, _, h2⟩
    · exact readReason_not_end (readAtoms_error_read hread)
    · cases hev : ev Γ e with
      | true => rw [hev] at h2; cases h2
      | false => rw [hev] at h2; cases h2; exact nofun

private theorem checkStmtWith_error_not_end {ev : Ctx → Expr → Bool} {Δ : Decls} {Γ : Ctx}
    {s : Stmt} {r : Reason} (h : checkStmtWith ev Δ Γ s = .error r) : ¬ IsEndReason r := by
  cases s with
  | let_ binder t _ =>
    cases binder with
    | name x =>
      simp only [checkStmtWith] at h
      by_cases hf : Fresh Γ x
      · rw [ite_eq_left hf] at h
        rcases bind_error_iff.mp h with hse | ⟨_, _, hok⟩
        · exact checkStmtExprWith_error_not_end hse
        · cases hok
      · rw [ite_eq_right hf] at h; cases h; exact nofun
    | wild =>
      simp only [checkStmtWith] at h
      rcases bind_error_iff.mp h with hse | ⟨_, _, hcp⟩
      · exact checkStmtExprWith_error_not_end hse
      · cases hc : t.isCopy Δ with
        | true => rw [hc] at hcp; cases hcp
        | false => rw [hc] at hcp; cases hcp; exact nofun
  | letStruct name _ _ =>
    simp only [checkStmtWith] at h
    rcases bind_error_iff.mp h with hse | ⟨_, _, h1⟩
    · exact checkStmtExprWith_error_not_end hse
    · cases hd : findDecl Δ name with
      | none => rw [hd] at h1; cases h1; exact nofun
      | some d =>
        rw [hd] at h1
        rcases bind_error_iff.mp h1 with hp | ⟨_, _, h2⟩
        · simp only [checkPattern] at hp
          rcases bind_error_iff.mp hp with r1 | ⟨_, _, hp2⟩
          · obtain ⟨_, rfl⟩ := rejectFound_error_of r1; exact nofun
          · rcases bind_error_iff.mp hp2 with r2 | ⟨_, _, r3⟩
            · obtain ⟨_, rfl⟩ := rejectFound_error_of r2; exact nofun
            · obtain ⟨_, rfl⟩ := rejectFound_error_of r3; exact nofun
        · rcases bind_error_iff.mp h2 with hpb | ⟨_, _, hok⟩
          · simp only [checkPatternBinders] at hpb
            rcases bind_error_iff.mp hpb with r1 | ⟨_, _, r2⟩
            · obtain ⟨_, rfl⟩ := rejectFound_error_of r1; exact nofun
            · obtain ⟨_, rfl⟩ := rejectFound_error_of r2; exact nofun
          · cases hok

private theorem finishChecks_error_spec {Δ : Decls} {atReturn : Bool} {Γ : Ctx} {r : Reason}
    (h : finishChecks Δ atReturn Γ = .error r) :
    ∃ x, x ∈ List.map Prod.fst Γ ∧ (r = .liveLinearBinding x ∨ r = .unusedBinding x) := by
  induction Γ with
  | nil => cases h
  | cons entry rest ih =>
    obtain ⟨x, e⟩ := entry
    simp only [finishChecks] at h
    cases h1 : (atReturn && !e.ty.isCopy Δ && e.live) with
    | true => rw [h1] at h; simp at h; subst r; exact ⟨x, .head _, .inl rfl⟩
    | false =>
      rw [h1] at h; simp only [Bool.false_eq_true, ↓reduceIte] at h
      cases hu : e.used with
      | false => rw [hu] at h; simp at h; subst r; exact ⟨x, .head _, .inr rfl⟩
      | true =>
        rw [hu] at h; simp only [↓reduceIte] at h
        obtain ⟨y, hy, hshape⟩ := ih h
        exact ⟨y, .tail _ hy, hshape⟩

private theorem update_keys {κ α : Type} [DecidableEq κ] (l : AList κ α) (k : κ) (f : α → α) :
    List.map Prod.fst (AList.update l k f) = List.map Prod.fst l := by
  induction l with
  | nil => rfl
  | cons pair rest ih =>
    obtain ⟨k', _⟩ := pair
    rw [AList.update_cons]
    by_cases h : k' = k
    · rw [ite_eq_left h]; rfl
    · rw [ite_eq_right h]; simp [ih]

private theorem useAtom_keys {Δ : Decls} {Γ Γ' : Ctx} {a : ConcreteSpec.Atom}
    (h : useAtom Δ Γ a = .ok Γ') : List.map Prod.fst Γ' = List.map Prod.fst Γ := by
  cases a with
  | int _ _ | bool _ => cases h; rfl
  | copy x | move x =>
    simp only [useAtom] at h
    cases hl : AList.lookup Γ x with
    | none => rw [hl] at h; cases h
    | some e =>
      rw [hl] at h
      cases hlv : e.live <;> cases hcp : e.ty.isCopy Δ <;> simp [hlv, hcp] at h
      subst Γ'; exact update_keys Γ x _

private theorem readAtoms_keys {Δ : Decls} {Γ Γ' : Ctx} {as : List ConcreteSpec.Atom}
    (h : readAtoms Δ Γ as = .ok Γ') : List.map Prod.fst Γ' = List.map Prod.fst Γ := by
  induction as generalizing Γ with
  | nil => cases h; rfl
  | cons a _ ih =>
    simp only [readAtoms] at h
    cases hu : useAtom Δ Γ a with
    | error _ => rw [hu] at h; cases h
    | ok _ => rw [hu] at h; exact (ih h).trans (useAtom_keys hu)

private theorem checkStmtExprWith_keys {ev : Ctx → Expr → Bool} {Δ : Decls} {Γ Γ' : Ctx}
    {t : Ty} {e : Expr} (h : checkStmtExprWith ev Δ Γ t e = .ok Γ') :
    List.map Prod.fst Γ' = List.map Prod.fst Γ := by
  simp only [checkStmtExprWith] at h
  obtain ⟨_, _, h1⟩ := bind_ok_iff.mp h
  obtain ⟨_, hread, h2⟩ := bind_ok_iff.mp h1
  cases hev : ev Γ e with
  | false => rw [hev] at h2; cases h2
  | true => rw [hev] at h2; cases h2; exact readAtoms_keys hread

private theorem checkStmtWith_straight_keys {ev : Ctx → Expr → Bool} {Δ : Decls} {Γ Γ' : Ctx}
    {b : StraightBinding} (h : checkStmtWith ev Δ Γ b.toCore = .ok Γ') :
    ∀ x ∈ List.map Prod.fst Γ', x ∈ List.map Prod.fst Γ ∨ b.destination = some x := by
  cases b with
  | slice0 s =>
    simp only [StraightBinding.toCore, Slice0.Stmt.toCore, checkStmtWith] at h
    by_cases hf : Fresh Γ s.x
    · rw [ite_eq_left hf] at h
      obtain ⟨_, hse, hok⟩ := bind_ok_iff.mp h; cases hok
      intro x hx
      cases hx with
      | head => exact .inr rfl
      | tail _ htail => exact .inl (checkStmtExprWith_keys hse ▸ htail)
    · rw [ite_eq_right hf] at h; cases h
  | bind binder ty _ =>
    cases binder with
    | name y =>
      simp only [StraightBinding.toCore, checkStmtWith] at h
      by_cases hf : Fresh Γ y
      · rw [ite_eq_left hf] at h
        obtain ⟨_, hse, hok⟩ := bind_ok_iff.mp h; cases hok
        intro x hx
        cases hx with
        | head => exact .inr rfl
        | tail _ htail => exact .inl (checkStmtExprWith_keys hse ▸ htail)
      · rw [ite_eq_right hf] at h; cases h
    | wild =>
      simp only [StraightBinding.toCore, checkStmtWith] at h
      obtain ⟨_, hse, hcp⟩ := bind_ok_iff.mp h
      cases hc : ty.toCore.isCopy Δ with
      | false => rw [hc] at hcp; cases hcp
      | true => rw [hc] at hcp; cases hcp; exact fun x hx => .inl (checkStmtExprWith_keys hse ▸ hx)
  | destructure name _ =>
    simp only [StraightBinding.toCore, checkStmtWith] at h
    obtain ⟨_, hse, h1⟩ := bind_ok_iff.mp h
    cases hd : findDecl Δ name with
    | none => rw [hd] at h1; cases h1
    | some _ =>
      rw [hd] at h1
      obtain ⟨_, _, h2⟩ := bind_ok_iff.mp h1
      obtain ⟨_, _, hok⟩ := bind_ok_iff.mp h2
      simp only [bindPat] at hok; cases hok
      exact fun x hx => .inl (checkStmtExprWith_keys hse ▸ hx)

private theorem checkEnd_error_spec {Δ : Decls} {Γ : Ctx} {ending : Terminator} {r : Reason}
    (h : checkEnd Δ Γ ending = .error r) :
    (∀ x, r ≠ .duplicateBinding x) ∧
    (∀ x, (r = .unusedBinding x ∨ r = .liveLinearBinding x) → x ∈ List.map Prod.fst Γ) := by
  cases ending with
  | abort _ =>
    obtain ⟨x, hx, hshape⟩ := finishChecks_error_spec h
    refine ⟨fun _ => by rcases hshape with rfl | rfl <;> exact nofun,
      fun _ hy => by rcases hshape with rfl | rfl <;> rcases hy with hy | hy <;> cases hy <;> exact hx⟩
  | ret _ =>
    simp only [checkEnd] at h
    rcases bind_error_iff.mp h with hreq | ⟨_, _, h1⟩
    · refine ⟨fun x heq => ?_, fun _ hx => by
        have hne := requireExprType_error_not_end hreq
        rcases hx with rfl | rfl <;> exact (hne trivial).elim⟩
      subst heq
      simp only [requireExprType] at hreq
      rcases bind_error_iff.mp hreq with hi | ⟨actual, _, hcmp⟩
      · simp only [inferExprType] at hi
        obtain ⟨c, _, hc⟩ := mapError_error_iff.mp hi; cases c <;> cases hc
      · by_cases hty : actual = .scalar (.int .u8)
        · rw [ite_eq_left hty] at hcmp; cases hcmp
        · rw [ite_eq_right hty] at hcmp; cases hcmp
    · rcases bind_error_iff.mp h1 with hu | ⟨_, hu, hfin⟩
      · have hread := useAtom_error_read hu
        exact ⟨fun _ heq => (by subst heq; cases hread),
          fun _ hx => (by rcases hx with rfl | rfl <;> cases hread)⟩
      · obtain ⟨x, hx, hshape⟩ := finishChecks_error_spec hfin
        rw [useAtom_keys hu] at hx
        exact ⟨fun _ => by rcases hshape with rfl | rfl <;> exact nofun,
          fun _ hy => by rcases hshape with rfl | rfl <;> rcases hy with hy | hy <;> cases hy <;> exact hx⟩

private theorem inferAtomType_outOfRange {Γ : TyCtx} {a : Slice0.Atom}
    {ty : ConcreteSpec.IntTy} {n : Int}
    (h : inferAtomType Γ a.toCore = .error (.outOfRange ty n)) :
    a.toCore = .int ty n := by
  cases a with
  | lit m =>
    simp only [Slice0.Atom.toCore, inferAtomType] at h
    by_cases hin : ConcreteSpec.IntTy.u8.inRange (m : Int) = true
    · rw [ite_eq_left hin] at h; cases h
    · rw [ite_eq_right hin] at h; cases h; rfl
  | var x =>
    simp only [Slice0.Atom.toCore, inferAtomType] at h
    cases hl : AList.lookup Γ x <;> rw [hl] at h <;> cases h

private theorem inferAddOperand_slice0_outOfRange {Γ : TyCtx} {a : Slice0.Atom}
    {r : Reason} {ty : ConcreteSpec.IntTy} {n : Int}
    (h : inferAddOperand Γ a.toCore = .error r) (hl : IsOutOfRangeReason ty n r) :
    a.toCore = .int ty n := by
  simp only [inferAddOperand] at h
  cases ha : inferAtomType Γ a.toCore with
  | ok ty' =>
    rw [ha] at h
    cases ty' with
    | struct _ => cases h; rcases hl with h | ⟨_, h⟩ <;> cases h
    | scalar st => cases st with
      | int _ => cases h
      | bool => cases h; rcases hl with h | ⟨_, h⟩ <;> cases h
  | error _ =>
    rw [ha] at h; cases h
    rcases hl with h | ⟨_, h⟩
    · cases h
    · cases h; exact inferAtomType_outOfRange ha

private theorem checkStmtWith_slice0_outOfRange {ev : Ctx → Expr → Bool} {Δ : Decls} {Γ : Ctx}
    {s : Slice0.Stmt} {r : Reason} {ty : ConcreteSpec.IntTy} {n : Int}
    (h : checkStmtWith ev Δ Γ s.toCore = .error r) (hlit : IsOutOfRangeReason ty n r) :
    match s.e with
    | .atom a => a.toCore = .int ty n
    | .add l r' => l.toCore = .int ty n ∨ r'.toCore = .int ty n := by
  simp only [Slice0.Stmt.toCore, checkStmtWith] at h
  by_cases hf : Fresh Γ s.x
  · rw [ite_eq_left hf] at h
    rcases bind_error_iff.mp h with hse | ⟨_, _, hok⟩
    · simp only [checkStmtExprWith] at hse
      rcases bind_error_iff.mp hse with hreq | ⟨_, _, h1⟩
      · simp only [requireExprType] at hreq
        rcases bind_error_iff.mp hreq with hi | ⟨actual, _, hcmp⟩
        · cases s with | mk _ e => cases e with
          | atom _ =>
            simp only [Slice0.Expr.toCore, inferExprType] at hi
            obtain ⟨c, ha, hc⟩ := mapError_error_iff.mp hi
            cases c with
            | unbound _ => subst hc; rcases hlit with h | ⟨_, h⟩ <;> cases h
            | outOfRange _ _ => subst hc; rcases hlit with h | ⟨_, h⟩ <;> cases h; exact inferAtomType_outOfRange ha
          | add _ _ =>
            simp only [Slice0.Expr.toCore, inferExprType] at hi
            rcases bind_error_iff.mp hi with hl | ⟨lt, _, h2⟩
            · exact .inl (inferAddOperand_slice0_outOfRange hl hlit)
            · rcases bind_error_iff.mp h2 with hr | ⟨rt, _, h3⟩
              · exact .inr (inferAddOperand_slice0_outOfRange hr hlit)
              · by_cases heq : lt = rt
                · rw [ite_eq_left heq] at h3; cases h3
                · rw [ite_eq_right heq] at h3; cases h3; rcases hlit with h | ⟨_, h⟩ <;> cases h
        · by_cases heq : actual = .scalar (.int .u8)
          · rw [ite_eq_left heq] at hcmp; cases hcmp
          · rw [ite_eq_right heq] at hcmp; cases hcmp; rcases hlit with h | ⟨_, h⟩ <;> cases h
      · rcases bind_error_iff.mp h1 with hread | ⟨_, _, h2⟩
        · exact (readReason_not_outOfRange (readAtoms_error_read hread) hlit).elim
        · cases hev : ev Γ s.e.toCore with
          | true => rw [hev] at h2; cases h2
          | false => rw [hev] at h2; cases h2; rcases hlit with h | ⟨_, h⟩ <;> cases h
    · cases hok
  · rw [ite_eq_right hf] at h; cases h; rcases hlit with h | ⟨_, h⟩ <;> cases h

private theorem checkEnd_slice0_outOfRange {Δ : Decls} {Γ : Ctx} {a : Slice0.Atom}
    {r : Reason} {ty : ConcreteSpec.IntTy} {n : Int}
    (h : checkEnd Δ Γ (.ret a.toCore) = .error r) (hlit : IsOutOfRangeReason ty n r) :
    a.toCore = .int ty n := by
  simp only [checkEnd] at h
  rcases bind_error_iff.mp h with hreq | ⟨_, _, h1⟩
  · simp only [requireExprType] at hreq
    rcases bind_error_iff.mp hreq with hi | ⟨actual, _, hcmp⟩
    · simp only [inferExprType] at hi
      obtain ⟨c, ha, hc⟩ := mapError_error_iff.mp hi
      cases c with
      | unbound _ => subst hc; rcases hlit with h | ⟨_, h⟩ <;> cases h
      | outOfRange _ _ => subst hc; rcases hlit with h | ⟨_, h⟩ <;> cases h; exact inferAtomType_outOfRange ha
    · by_cases heq : actual = .scalar (.int .u8)
      · rw [ite_eq_left heq] at hcmp; cases hcmp
      · rw [ite_eq_right heq] at hcmp; cases hcmp; rcases hlit with h | ⟨_, h⟩ <;> cases h
  · rcases bind_error_iff.mp h1 with hu | ⟨_, _, hfin⟩
    · exact (readReason_not_outOfRange (useAtom_error_read hu) hlit).elim
    · obtain ⟨_, _, hshape⟩ := finishChecks_error_spec hfin
      rcases hshape with rfl | rfl <;> rcases hlit with h | ⟨_, h⟩ <;> cases h

private theorem checkCoreBodyWith_straight_error {policy : ArithmeticPolicy} {Δ : Decls}
    {allBindings : List LocatedStraightBinding} {ending : Terminator}
    {rows : List LocatedStraightBinding} {Γ : Ctx} {start : Nat}
    {r : ConcreteSpec.Admission.Refusal}
    (hsub : ∀ b ∈ rows, b ∈ allBindings)
    (hinv : ∀ x ∈ List.map Prod.fst Γ, ∃ b ∈ allBindings, b.statement.destination = some x)
    (herr : checkCoreBodyWith policy Δ Γ start
      (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement)) ending = .error r) :
    ∃ pos reason, r = .atPosition pos reason ∧
      ((∃ idx row Γ', pos = start + idx ∧ rows[idx]? = some row ∧
          checkStmtWith policy.permits Δ Γ' row.statement.toCore = .error reason) ∨
       (∃ Γ', pos = start + rows.length ∧ checkEnd Δ Γ' ending = .error reason ∧
          ∀ x ∈ List.map Prod.fst Γ', ∃ b ∈ allBindings, b.statement.destination = some x)) := by
  induction rows generalizing Γ start with
  | nil =>
    simp only [List.map_nil, checkCoreBodyWith] at herr
    cases he : checkEnd Δ Γ ending with
    | ok _ => rw [he] at herr; cases herr
    | error reason => rw [he] at herr; cases herr; exact ⟨start, reason, rfl, .inr ⟨Γ, by simp, he, hinv⟩⟩
  | cons b rest ih =>
    simp only [List.map_cons, Function.comp_def, checkCoreBodyWith] at herr
    cases hs : checkStmtWith policy.permits Δ Γ b.statement.toCore with
    | error reason => simp only [hs] at herr; cases herr; exact ⟨start, reason, rfl, .inl ⟨0, b, Γ, by simp, rfl, hs⟩⟩
    | ok Γ₁ =>
      simp only [hs] at herr
      cases ht : checkCoreBodyWith policy Δ Γ₁ (start + 1)
          (rest.map (fun x => x.statement.toCore)) ending with
      | ok _ => simp only [ht] at herr; cases herr
      | error _ =>
        simp only [ht] at herr; cases herr
        have hinv₁ : ∀ x ∈ List.map Prod.fst Γ₁,
            ∃ b' ∈ allBindings, b'.statement.destination = some x := fun x hx =>
          (checkStmtWith_straight_keys hs x hx).elim (hinv x) (fun hd => ⟨b, hsub b (.head _), hd⟩)
        obtain ⟨pos, reason, rfl, hcase⟩ := ih (fun b' hb' => hsub b' (.tail _ hb')) hinv₁ ht
        refine ⟨pos, reason, rfl, ?_⟩
        rcases hcase with ⟨idx, row, Γ', hpos, hrow, hst⟩ | ⟨Γ', hpos, hend, hinv'⟩
        · exact .inl ⟨idx + 1, row, Γ', by omega, hrow, hst⟩
        · exact .inr ⟨Γ', by simp; omega, hend, hinv'⟩

private theorem find?_pair_fst_some {α β : Type} [BEq α] [LawfulBEq α]
    {xs : List (α × β)} {a : α} {b : β} (hmem : (a, b) ∈ xs) :
    ∃ p, xs.find? (fun pair => pair.1 == a) = some p :=
  find?_isSome_of_mem ⟨(a, b), hmem, beq_self_eq_true a⟩

private theorem zero_readsAt_find?_isSome (p : EarlyProgram) {pos : Nat}
    {ty : ConcreteSpec.IntTy} {n : Int}
    (h : (pos = p.bindings.length ∧ p.ret.toCore = .int ty n) ∨
      (pos < p.bindings.length ∧ ∃ lb, p.bindings[pos]? = some lb ∧
        match lb.statement.e with
        | .atom a => a.toCore = .int ty n
        | .add l r => l.toCore = .int ty n ∨ r.toCore = .int ty n)) :
    ∃ pair, (if pos = p.bindings.length then [(p.ret.toCore, p.resultSpan)]
      else (p.bindings[pos]?.map fun b =>
        match b with
        | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
        | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]).getD []).find?
      (fun read => read.1 == ConcreteSpec.Atom.int ty n) = some pair := by
  rcases h with ⟨hpos, hret⟩ | ⟨hlt, lb, hlb, hcase⟩
  · rw [ite_eq_left hpos, hret]; exact find?_pair_fst_some (List.Mem.head _)
  · rw [ite_eq_right (Nat.ne_of_lt hlt), hlb]; simp only [Option.map_some, Option.getD_some]
    cases lb with | mk stmt _ _ opSpans _ => cases stmt with | mk _ e => cases e with
      | atom _ => dsimp only at hcase ⊢; rw [hcase]; exact find?_pair_fst_some (List.Mem.head _)
      | add _ _ =>
        cases opSpans with | mk _ _ =>
          dsimp only at hcase ⊢
          rcases hcase with hl | hr
          · rw [hl]; exact find?_pair_fst_some (List.Mem.head _)
          · rw [hr]; exact find?_pair_fst_some (.tail _ (List.Mem.head _))

private theorem render_atPosition_some (v : AdmissionView) (pos : Nat) (reason : Reason)
    (hpos : pos ≤ v.located.bindings.length)
    (hdup : ∀ x, reason = .duplicateBinding x → pos < v.located.bindings.length)
    (hlit : ∀ (p : EarlyProgram) (ty : ConcreteSpec.IntTy) (n : Int),
      v = .zero p → IsOutOfRangeReason ty n reason →
      (pos = p.bindings.length ∧ p.ret.toCore = .int ty n) ∨
      (pos < p.bindings.length ∧ ∃ lb, p.bindings[pos]? = some lb ∧
        match lb.statement.e with
        | .atom a => a.toCore = .int ty n
        | .add l r => l.toCore = .int ty n ∨ r.toCore = .int ty n))
    (hend : ∀ x, (reason = .unusedBinding x ∨ reason = .liveLinearBinding x) →
      ∃ b ∈ v.located.bindings, b.statement.destination = some x) :
    ∃ diagnostic, v.render (.atPosition pos reason) = some diagnostic := by
  obtain ⟨origin, horigin⟩ := originAt_isSome v hpos
  dsimp only [AdmissionView.render]
  rw [horigin, option_bind_some]
  cases reason with
  | duplicateBinding x =>
    obtain ⟨b, hb⟩ := getElem?_some_of_lt v.located.bindings (hdup x rfl)
    dsimp only; rw [hb, option_bind_some]; exact ⟨_, rfl⟩
  | unusedBinding x =>
    obtain ⟨b, hb, hdest⟩ := hend x (.inl rfl)
    obtain ⟨b', hb'⟩ := find?_isSome_of_mem
      (p := fun b : LocatedStraightBinding => b.statement.destination == some x)
      ⟨b, hb, by rw [hdest]; exact beq_self_eq_true (some x)⟩
    cases v with
    | zero _ =>
      refine ⟨RefusalCause.core .slice0 origin.lexical
        [(Name.mk "binding", surfaceFileSpan b'.declarationSpan)] (.atPosition pos (.unusedBinding x)), ?_⟩
      change (Option.bind (AdmissionView.bindingAt _ x) fun span => some _) = _
      dsimp only [AdmissionView.bindingAt]; rw [hb', Option.map_some, Option.bind_some]; rfl
    | one p =>
      refine ⟨RefusalCause.core (match p.ending with | .ret _ => .returning | .abort => .aborting)
        origin.lexical [(Name.mk "binding", surfaceFileSpan b'.declarationSpan)] (.atPosition pos (.unusedBinding x)), ?_⟩
      change (Option.bind (AdmissionView.bindingAt _ x) fun span => some _) = _
      dsimp only [AdmissionView.bindingAt]; rw [hb', Option.map_some, Option.bind_some]; rfl
  | liveLinearBinding x =>
    obtain ⟨b, hb, hdest⟩ := hend x (.inr rfl)
    obtain ⟨b', hb'⟩ := find?_isSome_of_mem
      (p := fun b : LocatedStraightBinding => b.statement.destination == some x)
      ⟨b, hb, by rw [hdest]; exact beq_self_eq_true (some x)⟩
    cases v with
    | zero _ =>
      refine ⟨RefusalCause.core .slice0 origin.lexical
        [(Name.mk "binding", surfaceFileSpan b'.declarationSpan)] (.atPosition pos (.liveLinearBinding x)), ?_⟩
      change (Option.bind (AdmissionView.bindingAt _ x) fun span => some _) = _
      dsimp only [AdmissionView.bindingAt]; rw [hb', Option.map_some, Option.bind_some]; rfl
    | one p =>
      refine ⟨RefusalCause.core (match p.ending with | .ret _ => .returning | .abort => .aborting)
        origin.lexical [(Name.mk "binding", surfaceFileSpan b'.declarationSpan)] (.atPosition pos (.liveLinearBinding x)), ?_⟩
      change (Option.bind (AdmissionView.bindingAt _ x) fun span => some _) = _
      dsimp only [AdmissionView.bindingAt]; rw [hb', Option.map_some, Option.bind_some]; rfl
  | literalOutOfRange ty n =>
    cases v with
    | one _ => exact ⟨_, rfl⟩
    | zero p =>
      obtain ⟨⟨_, span⟩, hq⟩ := zero_readsAt_find?_isSome p (hlit p ty n rfl (.inl rfl))
      refine ⟨RefusalCause.core .slice0 (surfaceFileSpan span) [] (.atPosition pos (.literalOutOfRange ty n)), ?_⟩
      change (Option.bind (List.find? (fun read => read.1 == ConcreteSpec.Atom.int ty n)
        (if pos = p.bindings.length then [(p.ret.toCore, p.resultSpan)]
         else (p.bindings[pos]?.map fun b => match b with
           | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
           | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]).getD [])) _) = _
      rw [hq, Option.bind_some]; rfl
  | additionOperandType op cause =>
    cases cause with
    | wrongType _ => cases v <;> exact ⟨_, rfl⟩
    | atomFailure af =>
      cases af with
      | unbound _ => cases v <;> exact ⟨_, rfl⟩
      | outOfRange ty n =>
        cases v with
        | one _ => exact ⟨_, rfl⟩
        | zero p =>
          obtain ⟨⟨_, span⟩, hq⟩ := zero_readsAt_find?_isSome p (hlit p ty n rfl (.inr ⟨op, rfl⟩))
          refine ⟨RefusalCause.core .slice0 (surfaceFileSpan span) []
            (.atPosition pos (.additionOperandType op (.atomFailure (.outOfRange ty n)))), ?_⟩
          change (Option.bind (List.find? (fun read => read.1 == ConcreteSpec.Atom.int ty n)
            (if pos = p.bindings.length then [(p.ret.toCore, p.resultSpan)]
             else (p.bindings[pos]?.map fun b => match b with
               | ⟨⟨_, .atom a⟩, span, _, _, _⟩ => [(a.toCore, span)]
               | ⟨⟨_, .add l r⟩, _, _, (ls, rs), _⟩ => [(l.toCore, ls), (r.toCore, rs)]).getD [])) _) = _
          rw [hq, Option.bind_some]; rfl
  | _ => cases v <;> exact ⟨_, rfl⟩

/-- Every checker refusal of an early-fragment program renders to a located
diagnostic; `AdmissionView.render` never loses a location on a checker error. -/
theorem render_of_checkProgramWith_error (policy : ArithmeticPolicy) (v : AdmissionView)
    {r : ConcreteSpec.Admission.Refusal} (herr : checkProgramWith policy v.program = .error r) :
    ∃ diagnostic, v.render r = some diagnostic := by
  unfold checkProgramWith at herr
  cases hd : checkDecls v.program.decls with
  | error _ =>
    rw [hd] at herr; cases herr
    obtain ⟨idx, _, hlt, hshape⟩ := checkDeclsFrom_error hd
    have hlen : v.program.decls.length = v.located.declarations.length := by
      rw [view_program_eq]; simp [StraightProgram.decls]
    rw [Nat.zero_add, hlen] at hlt
    obtain ⟨⟨_, span⟩, hget⟩ := getElem?_some_of_lt v.located.declarations hlt
    rcases hshape with ⟨_, rfl⟩ | ⟨_, rfl⟩ | ⟨_, _, rfl⟩
    all_goals exact ⟨_, by simp [AdmissionView.render, hget]; rfl⟩
  | ok _ =>
    rw [hd] at herr; rw [view_program_eq] at herr
    obtain ⟨pos, reason, rfl, hcase⟩ := checkCoreBodyWith_straight_error
      (allBindings := v.located.bindings) (fun _ h => h) (fun _ h => by cases h) herr
    rcases hcase with ⟨idx, row, Γ', hpos, hrow, hst⟩ | ⟨Γ', hpos, hend, hinv⟩
    · rw [Nat.zero_add] at hpos; subst pos
      have hlt : idx < v.located.bindings.length := lt_of_getElem?_eq_some hrow
      refine render_atPosition_some v idx reason (Nat.le_of_lt hlt) (fun _ _ => hlt) ?_ ?_
      · intro p ty n hv hlit; subst hv
        simp only [AdmissionView.located, List.getElem?_map] at hrow
        cases hlb : p.bindings[idx]? with
        | none => rw [hlb] at hrow; cases hrow
        | some lb =>
          rw [hlb] at hrow; cases hrow
          exact .inr ⟨lt_of_getElem?_eq_some hlb, lb, rfl, checkStmtWith_slice0_outOfRange hst hlit⟩
      · intro _ hx
        have hne := checkStmtWith_error_not_end hst
        rcases hx with rfl | rfl <;> exact (hne trivial).elim
    · rw [Nat.zero_add] at hpos; subst pos
      obtain ⟨hndup, hendMem⟩ := checkEnd_error_spec hend
      refine render_atPosition_some v v.located.bindings.length reason (Nat.le_refl _)
        (fun x heq => (hndup x heq).elim)
        (fun p _ _ hv hlit => by subst hv; exact .inl ⟨List.length_map _, checkEnd_slice0_outOfRange hend hlit⟩)
        (fun x hx => hinv x (hendMem x hx))

theorem render_of_checkProgram_error (v : AdmissionView) {r : ConcreteSpec.Admission.Refusal}
    (herr : checkProgram v.program = .error r) :
    ∃ diagnostic, v.render r = some diagnostic :=
  render_of_checkProgramWith_error .static v herr

/-! ### Deterministic `Exec.Steps` non-trapping transfer -/

private theorem step_det {O : Exec.Operands} (noPrim : O.Prim → False) {P : Exec.Program O}
    {c c₁ c₂ : Exec.Config O} {l₁ l₂ : Exec.Label O}
    (h₁ : Exec.Step P c l₁ c₁) (h₂ : Exec.Step P c l₂ c₂) : l₁ = l₂ ∧ c₁ = c₂ := by
  match h₁ with
  | @Exec.Step.prim _ _ _ _ p _ _ _ _ _ _ _ _ => exact (noPrim p).elim
  | .silent s₁ =>
    match h₂ with
    | @Exec.Step.prim _ _ _ _ p _ _ _ _ _ _ _ _ => exact (noPrim p).elim
    | .silent s₂ => rw [s₁] at s₂; cases s₂; exact ⟨rfl, rfl⟩

private theorem step_none_of_final {O : Exec.Operands} {P : Exec.Program O} {c : Exec.Config O}
    {f : Exec.Final O.Val} (hfinal : c.final? = some f) : Exec.step P c = none := by
  cases c with | mk ctrl _ _ frames => cases ctrl with
    | exec _ | refused => cases hfinal
    | done => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | exiting e => cases e with
      | brk _ | cont _ => cases hfinal
      | ret _ => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | trapped _ _ => rfl

private theorem activeOperation_none_of_final {language : CoreLanguage}
    {payload : MachineProgram language} {c : Exec.Config payload.machine}
    {f : Exec.Final language.operands.Val} (hfinal : c.final? = some f) :
    payload.activeOperation c = none := by
  cases c with | mk ctrl _ _ frames => cases ctrl with
    | exec _ | refused => cases hfinal
    | done => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | exiting e => cases e with
      | brk _ | cont _ => cases hfinal
      | ret _ => cases frames with | nil => rfl | cons _ _ => cases hfinal
    | trapped _ _ => rfl

private theorem no_step_of_final {O : Exec.Operands} {P : Exec.Program O} {c c' : Exec.Config O}
    {l : Exec.Label O} {f : Exec.Final O.Val}
    (hfinal : c.final? = some f) (hstep : Exec.Step P c l c') : False := by
  have hnone := step_none_of_final (P := P) hfinal
  cases hstep with | silent h => rw [hnone] at h; cases h | prim h => rw [hnone] at h; cases h

private theorem reachable_prefix_of_det {O : Exec.Operands} (noPrim : O.Prim → False)
    {P : Exec.Program O} {a q : Exec.Config O} {f : Exec.Final O.Val} {ev₁ : List O.Event}
    (toFinal : Exec.Steps P a ev₁ q) (hfinal : q.final? = some f) :
    ∀ {ev₂ state}, Exec.Steps P a ev₂ state →
      state = q ∨ ∃ label next ev', Exec.Step P state label next ∧ Exec.Steps P next ev' q := by
  induction toFinal with
  | refl _ =>
    intro _ _ reach
    cases reach with
    | refl => exact .inl rfl
    | step tick _ => exact (no_step_of_final hfinal tick).elim
  | step tick rest ih =>
    intro _ _ reach
    cases reach with
    | refl => exact .inr ⟨_, _, _, tick, rest⟩
    | step tick₂ rest₂ => obtain ⟨_, rfl⟩ := step_det noPrim tick tick₂; exact ih hfinal rest₂

private theorem trapped_reaches_self {O : Exec.Operands} {P : Exec.Program O}
    {c q : Exec.Config O} {ev : List O.Event} {k : TrapKind} {site : SiteId}
    (htrap : c.ctrl = .trapped k site) (reach : Exec.Steps P c ev q) : q = c := by
  have hfinal : c.final? = some (.trapped k site) := by cases c; simp only at htrap; subst htrap; rfl
  cases reach with | refl => rfl | step tick _ => exact (no_step_of_final hfinal tick).elim

private theorem nonTrappingActive_of_final {language : CoreLanguage}
    (noPrim : language.operands.Prim → False) {payload : MachineProgram language}
    {q : Exec.Config payload.machine} {f : Exec.Final language.operands.Val}
    {ev₁ : List language.operands.Event} {trapKind : TrapKind}
    (toFinal : Exec.Steps payload.body payload.initial ev₁ q) (hfinal : q.final? = some f)
    (noTrap : ∀ s, f ≠ .trapped trapKind s)
    {state events site op}
    (reach : Exec.Steps payload.body payload.initial events state)
    (active : payload.activeOperation state = some (site, op)) :
    (∃ label next, Exec.Step payload.body state label next) ∧
    (∀ label next, Exec.Step payload.body state label next → next.ctrl ≠ .trapped trapKind site) := by
  rcases reachable_prefix_of_det (O := payload.machine) noPrim toFinal hfinal reach with
    rfl | ⟨label₀, next₀, _, tick₀, rest⟩
  · rw [activeOperation_none_of_final hfinal] at active; cases active
  · refine ⟨⟨label₀, next₀, tick₀⟩, fun _ _ tick trapped => ?_⟩
    obtain ⟨_, rfl⟩ := step_det (O := payload.machine) noPrim tick₀ tick
    have hq := trapped_reaches_self trapped rest
    have hf : Exec.Config.final? next₀ = some (Exec.Final.trapped trapKind site) := by
      cases next₀; simp only at trapped; subst trapped; rfl
    rw [hq, hf] at hfinal; cases hfinal; exact noTrap site rfl

private def EncodedEntry (v : AdmissionView) (d : ConcreteSpec.Admission.Discharge)
    (entry : SiteId × List (StageInterface.Discharge (sliceLanguage v.slice))) : Prop :=
  ∃ origin op cond,
    v.originAt d.position = some origin ∧
    AList.lookup v.execution.operations origin.operation = some op ∧
    (sliceLanguage v.slice).safety op = [cond] ∧
    cond.trap = .overflow ∧
    entry = (origin.operation, [⟨origin.operation, origin.evidenceKey,
      ⟨origin.operation, .safety, cond.discriminator⟩, cond.trap, cond.operands⟩])

/-- Given that every checked straight-line view terminates without an overflow
trap in `Exec.Steps`, every encoded discharge subset satisfies `ValidDischarges`. -/
theorem validDischarges_of_terminates
    (terminates : ∀ v : AdmissionView, Checked v.program →
      ∃ q f, Exec.Steps v.execution.body v.execution.initial [] q ∧
        q.final? = some f ∧ ∀ s, f ≠ .trapped .overflow s)
    {language : CoreLanguage} (input : CoreInput language)
    (batch : InputBatch input) (selected : List ConcreteSpec.Admission.Discharge)
    (out : StageInterface.Discharges language)
    (encoded : batch.encode selected = some out) :
    ValidDischarges input out := by
  rcases input with ⟨project, sl, payload⟩
  rcases batch with ⟨v, ⟨hs, eq, hp⟩, b⟩
  dsimp only at hs hp; cases hs; cases eq; cases hp
  unfold InputBatch.encode at encoded
  cases hval : b.validates selected with
  | false => simp [hval] at encoded
  | true =>
    simp only [hval, Bool.not_true, Bool.false_eq_true, ↓reduceIte] at encoded
    dsimp only [CoreInput.machineProgram, Payload.execution] at encoded
    have hall : ListRel (EncodedEntry v) selected out :=
      mapM_option_rel encoded fun d entry hen => by
        obtain ⟨origin, ho, hen⟩ := option_bind_eq_some hen
        obtain ⟨op, hl, hen⟩ := option_bind_eq_some hen
        cases hs : (sliceLanguage v.slice).safety op with
        | nil => rw [hs] at hen; cases hen
        | cons cond rest =>
          cases rest with
          | cons _ _ => rw [hs] at hen; cases hen
          | nil =>
            rw [hs] at hen
            cases htr : (cond.trap != .overflow) with
            | true => simp [htr] at hen
            | false =>
              simp only [htr, Bool.false_eq_true, ↓reduceIte] at hen
              have htrap : cond.trap = .overflow := by
                cases hc : cond.trap with | overflow => rfl | _ => rw [hc] at htr; cases htr
              exact ⟨origin, op, cond, ho, hl, hs, htrap, (Option.some.inj hen).symm⟩
    have hnodup : (List.map Prod.fst out).Nodup := by
      have hkeys : List.map Prod.fst out =
          (selected.map ConcreteSpec.Admission.Discharge.position).map SiteId.mk := by
        rw [List.map_map]
        exact listRel_map_eq hall fun _ _ ⟨_, _, _, ho, _, _, _, heq⟩ => by
          rw [heq]; exact (originAt_spec v ho).1
      rw [hkeys]
      exact nodup_map_of_inj (fun _ _ h => congrArg SiteId.id h)
        (nodup_of_sublist (sublist_map _ (sublist_of_isSublist hval)) b.uniquePositions)
    obtain ⟨q, f, hsteps, hfinal, hnoov⟩ := terminates v ((checkProgram_iff v.program).mp ⟨_, b.issued⟩)
    refine ⟨rfl, hnodup, ?_, ?_⟩
    · intro site group hmem
      obtain ⟨_, _, _, _, _, _, _, _, _, heq⟩ := listRel_mem_right hall hmem
      cases heq
      exact ⟨List.cons_ne_nil _ _, List.nodup_cons.mpr ⟨List.not_mem_nil, List.nodup_nil⟩⟩
    · intro site d ⟨group, hmem, hd⟩
      obtain ⟨_, _, origin, op, cond, ho, hl, hs, htrap, heq⟩ := listRel_mem_right hall hmem
      cases heq
      have hd_eq := List.mem_singleton.mp hd; subst hd_eq
      obtain ⟨hop_eq, horig_mem⟩ := originAt_spec v ho
      refine ⟨rfl, rfl, rfl,
        ⟨(sliceLanguage v.slice).opcode op, [cond],
          ⟨op, AList.lookup_mem hl, ?_, rfl, hs.symm⟩, List.Mem.head _⟩,
        ⟨origin, hop_eq ▸ horig_mem, rfl⟩, fun _ _ _ reach active => by
          rw [htrap]; exact nonTrappingActive_of_final (fun p => by cases p) hsteps hfinal hnoov reach active⟩
      cases op with
      | abort => change [⟨0, TrapKind.abort, []⟩] = [cond] at hs; cases hs; cases htrap
      | expression _ | call _ _ | primitive _ _ => trivial

/-- Any discharge transfer theorem for `InputBatch.encode` yields full
`AdmissionSound` for `ConcreteSpec.Admission.checkInput`. -/
theorem admissionSound_of_transfer
    (transfer : ∀ {language : CoreLanguage} (input : CoreInput language)
      (batch : InputBatch input) (selected : List ConcreteSpec.Admission.Discharge)
      (out : StageInterface.Discharges language),
      input.WF → batch.encode selected = some out → ValidDischarges input out)
    {language : CoreLanguage} :
    AdmissionSound (language := language) ConcreteSpec.Admission.checkInput := by
  intro input wf ⟨v, hv⟩
  have hdec : input.decode = .ok v := (input.decode_iff v).mpr hv
  cases hchk : checkProgram v.program with
  | ok ds =>
    let b : Batch v.program := ⟨ds, hchk⟩
    let ib : InputBatch input := ⟨v, hv, b⟩
    have hadmit : admitInput input = .ok ib := by
      unfold admitInput
      split
      · rename_i _ heq; rw [hdec] at heq; cases heq
      · rename_i _ heq; rw [hdec] at heq; cases heq
        change (match admitView v with | .ok batch => Except.ok (⟨v, _, batch⟩ : InputBatch input) | .error _ => _) = _
        have hav : admitView v = .ok b := by
          unfold admitView admitProgram
          split
          · rename_i _ h'; rw [hchk] at h'; cases h'; rfl
          · rename_i _ h'; rw [hchk] at h'; cases h'
        rw [hav]
    obtain ⟨out, henc⟩ := encode_issued_isSome ib
    have hcheck : checkInput input = .ok out := by
      simp [checkInput, hadmit, henc, Bind.bind, Except.bind, Pure.pure, Except.pure]
    have hacc : input.Accepted := ib.accepted wf
    refine ⟨⟨fun _ => ⟨out, hcheck⟩, fun _ => hacc⟩, ?_, ?_⟩
    · intro _ hok; rw [hcheck] at hok; cases hok; exact ⟨hacc.1, transfer input ib ds out wf henc⟩
    · intro _ herr; rw [hcheck] at herr; cases herr
  | error reason =>
    obtain ⟨diagnostic, hren⟩ := render_of_checkProgram_error v hchk
    have hadmit : admitInput input = .error (.refused diagnostic) := by
      unfold admitInput
      split
      · rename_i _ heq; rw [hdec] at heq; cases heq
      · rename_i _ heq; rw [hdec] at heq; cases heq
        change (match admitView v with
          | .ok batch => Except.ok (⟨v, _, batch⟩ : InputBatch input)
          | .error reason' => match v.render reason' with
            | some d => .error (AdmissionFailure.refused d)
            | none => .error (AdmissionFailure.missingLocation reason')) = _
        have hav : admitView v = .error reason := by
          unfold admitView admitProgram
          split
          · rename_i _ h'; rw [hchk] at h'; cases h'
          · rename_i _ h'; rw [hchk] at h'; cases h'; rfl
        simp only [hav, hren]
    have hcheck : checkInput input = .error (.refused diagnostic) := by
      simp [checkInput, hadmit, Bind.bind, Except.bind]
    have hnotAcc : ¬ input.Accepted := by
      rintro ⟨_, w, hw, hcw⟩
      cases v.unique hv hw
      obtain ⟨_, hok⟩ := (checkProgram_iff v.program).mpr hcw
      rw [hchk] at hok; cases hok
    refine ⟨⟨fun hacc => (hnotAcc hacc).elim, fun ⟨_, hok⟩ => by rw [hcheck] at hok; cases hok⟩, ?_, ?_⟩
    · intro _ hok; rw [hcheck] at hok; cases hok
    · intro _ herr; rw [hcheck] at herr; cases herr; exact ⟨diagnostic, rfl, admitInput_refused hadmit⟩

end Concrete.IntegratedCore.DischargeTransfer

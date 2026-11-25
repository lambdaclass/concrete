import Std.Data.HashMap.Lemmas
import Concrete.Elab

namespace Concrete.Admission

/-- The statically known result of an addition, indexed by the Core binder for that operation. -/
structure Token where
  binding : ConcreteSpec.Var
  value : Int
  deriving DecidableEq

structure Output where
  private mk ::
  core : Elab.Output
  tokens : List Token

open ConcreteSpec (Ctx Entry Var)
open ConcreteSpec.Core (AList)
open ConcreteSpec.Slice0 (Atom Expr Stmt Program)

private abbrev Entries := Std.HashMap Nat Entry

private def find (m : Entries) (x : Var) : Option Entry := m[x.id]?

private def Agrees (m : Entries) (Γ : Ctx) : Prop :=
  ∀ x, find m x = AList.lookup Γ x

private theorem agrees_empty : Agrees ∅ [] := by
  intro x
  simp [find, AList.lookup]

private theorem id_inj {x y : Var} : x.id = y.id ↔ x = y := by
  cases x; cases y; simp

private theorem agrees_insert {m : Entries} {Γ : Ctx} (h : Agrees m Γ)
    (x : Var) (e : Entry) : Agrees (m.insert x.id e) ((x, e) :: Γ) := by
  intro y
  simp [find, Std.HashMap.getElem?_insert, AList.lookup, id_inj, ← h y]

private def mark (m : Entries) (x : Var) : Entries :=
  m.modify x.id (Entry.read false)

private theorem agrees_mark {m : Entries} {Γ : Ctx} (h : Agrees m Γ) (x : Var) :
    Agrees (mark m x) (ConcreteSpec.ctxMark Γ x false) := by
  intro y
  by_cases he : y = x
  · subst y
    simp only [find, mark, Std.HashMap.getElem?_modify_self, ConcreteSpec.ctxMark,
      AList.lookup_update]
    exact congrArg (Option.map (Entry.read false)) (h x)
  · have hid : x.id ≠ y.id := fun eq => he (id_inj.mp eq.symm)
    simp [find, mark, Std.HashMap.getElem?_modify, ConcreteSpec.ctxMark, AList.lookup_update,
      beq_eq_false_iff_ne.mpr hid, he, ← h y]

private def Atom.refs : Atom → List Var
  | .lit _ => []
  | .var x => [x]

private def Expr.refs : Expr → List Var
  | .atom a => Atom.refs a
  | .add l r => Atom.refs l ++ Atom.refs r

/-- The specification's local queries only need the entries named by the current expression. -/
private def project (m : Entries) (xs : List Var) : Ctx :=
  xs.filterMap fun x => (find m x).map (x, ·)

private theorem lookup_project (m : Entries) (xs : List Var) (x : Var) (hx : x ∈ xs) :
    AList.lookup (project m xs) x = find m x := by
  induction xs with
  | nil => simp at hx
  | cons y ys ih =>
    by_cases h : y = x
    · subst y
      cases hf : find m x with
      | some e => simp [project, hf, AList.lookup]
      | none =>
        have missing : ∀ zs : List Var, AList.lookup (project m zs) x = none := by
          intro zs
          induction zs with
          | nil => rfl
          | cons z zs iz =>
            by_cases hz : z = x
            · subst z; simpa [project, hf] using iz
            · cases he : find m z <;> simpa [project, he, AList.lookup, hz] using iz
        simpa [project, hf] using missing ys
    · have hm : x ∈ ys := by simpa [Ne.symm h] using hx
      cases hf : find m y <;> simpa [project, hf, AList.lookup, h] using ih hm

private theorem project_agrees {m : Entries} {Γ : Ctx} (h : Agrees m Γ)
    {xs : List Var} {x : Var} (hx : x ∈ xs) :
    AList.lookup (project m xs) x = AList.lookup Γ x :=
  (lookup_project m xs x hx).trans (h x)

private theorem atomTy_frame {Γ Γ' : Ctx} (a : Atom)
    (h : ∀ x ∈ Atom.refs a, AList.lookup Γ x = AList.lookup Γ' x) :
    ConcreteSpec.atomTy Γ.tys a.toCore = ConcreteSpec.atomTy Γ'.tys a.toCore ∧
    ConcreteSpec.tyOf [] Γ.tys (.atom a.toCore) = ConcreteSpec.tyOf [] Γ'.tys (.atom a.toCore) ∧
    ConcreteSpec.atomBounds Γ a.toCore = ConcreteSpec.atomBounds Γ' a.toCore ∧
    ConcreteSpec.knownAtom Γ a.toCore = ConcreteSpec.knownAtom Γ' a.toCore ∧
    ConcreteSpec.knownFieldsAtom Γ a.toCore = ConcreteSpec.knownFieldsAtom Γ' a.toCore := by
  cases a with
  | lit n => exact ⟨rfl, rfl, rfl, rfl, rfl⟩
  | var x =>
    have hx := h x (by simp [Atom.refs])
    simp [Atom.toCore, ConcreteSpec.atomTy, ConcreteSpec.tyOf, ConcreteSpec.Ctx.tys,
      AList.lookup_map, ConcreteSpec.atomBounds, ConcreteSpec.knownAtom,
      ConcreteSpec.knownFieldsAtom, hx]

private theorem expr_frame {Γ Γ' : Ctx} (e : Expr)
    (h : ∀ x ∈ Expr.refs e, AList.lookup Γ x = AList.lookup Γ' x) :
    ConcreteSpec.tyOf [] Γ.tys e.toCore = ConcreteSpec.tyOf [] Γ'.tys e.toCore ∧
    ConcreteSpec.admitted Γ e.toCore = ConcreteSpec.admitted Γ' e.toCore ∧
    ConcreteSpec.knownExpr Γ e.toCore = ConcreteSpec.knownExpr Γ' e.toCore ∧
    ConcreteSpec.knownFields Γ e.toCore = ConcreteSpec.knownFields Γ' e.toCore := by
  cases e with
  | atom a =>
    obtain ⟨_, ht, _, hk, hf⟩ := atomTy_frame a h
    exact ⟨ht, rfl, hk, hf⟩
  | add l r =>
    have hl := atomTy_frame l (fun x hx => h x (List.mem_append_left _ hx))
    have hr := atomTy_frame r (fun x hx => h x (List.mem_append_right _ hx))
    simp [Expr.toCore, ConcreteSpec.tyOf, ConcreteSpec.admitted, ConcreteSpec.knownExpr,
      ConcreteSpec.knownFields, hl.1, hr.1, hl.2.2.1, hr.2.2.1, hl.2.2.2.1, hr.2.2.2.1]

private def readAtom (m : Entries) : Atom → Option Entries
  | .lit _ => some m
  | .var x => do
    let e ← find m x
    if e.live ∧ e.ty.isCopy [] = true then some (mark m x) else none

private def readExpr (m : Entries) : Expr → Option Entries
  | .atom a => readAtom m a
  | .add l r => (readAtom m l).bind (readAtom · r)

private def Related (m : Option Entries) (Γ : Option Ctx) : Prop :=
  match m, Γ with
  | some m, some Γ => Agrees m Γ
  | none, none => True
  | _, _ => False

private theorem readAtom_agrees {m : Entries} {Γ : Ctx} (h : Agrees m Γ) (a : Atom) :
    Related (readAtom m a) (ConcreteSpec.useAtom [] Γ a.toCore) := by
  cases a with
  | lit n => exact h
  | var x =>
    simp only [readAtom, Atom.toCore, ConcreteSpec.useAtom, h x]
    cases AList.lookup Γ x with
    | none => trivial
    | some e => by_cases he : e.live ∧ e.ty.isCopy [] = true
                · simpa [Option.bind, he, Related] using agrees_mark h x
                · simp [Option.bind, he, Related]

private theorem readExpr_agrees {m : Entries} {Γ : Ctx} (h : Agrees m Γ) (e : Expr) :
    Related (readExpr m e) (ConcreteSpec.uses [] Γ e.toCore) := by
  cases e with
  | atom a => exact readAtom_agrees h a
  | add l r =>
    have hl := readAtom_agrees h l
    simp only [readExpr, Expr.toCore, ConcreteSpec.uses]
    cases hm : readAtom m l <;> cases hg : ConcreteSpec.useAtom [] Γ l.toCore <;>
      simp only [hm, hg, Related] at hl
    · trivial
    · exact readAtom_agrees hl r

private def u8 : ConcreteSpec.Ty := .scalar (.int .u8)

private def step (m : Entries) (s : Stmt) : Option Entries :=
  let localCtx := project m (Expr.refs s.e)
  let e := s.e.toCore
  if ConcreteSpec.tyOf [] localCtx.tys e = some u8 ∧ ConcreteSpec.admitted localCtx e = true then do
    let next ← readExpr m s.e
    if (find next s.x).isNone then
      some (next.insert s.x.id {
        ty := u8
        live := true
        used := false
        known := ConcreteSpec.knownExpr localCtx e
        kfields := ConcreteSpec.knownFields localCtx e })
    else none
  else none

private theorem step_agrees {m : Entries} {Γ : Ctx} (h : Agrees m Γ) (s : Stmt) :
    Related (step m s) (ConcreteSpec.stmtOut [] Γ s.toCore) := by
  obtain ⟨ht, ha, hk, hf⟩ := expr_frame s.e (fun _ hx => project_agrees h hx)
  simp only [step, Stmt.toCore, ConcreteSpec.stmtOut, u8, ht, ha, hk, hf]
  split
  · have hr := readExpr_agrees h s.e
    cases hm : readExpr m s.e <;> cases hg : ConcreteSpec.uses [] Γ s.e.toCore <;>
      simp only [hm, hg, Related] at hr
    · trivial
    · rename_i m' Γ'
      dsimp only [bind, Option.bind, ConcreteSpec.Fresh]
      rw [hr s.x]
      by_cases fresh : AList.lookup Γ' s.x = none
      · simpa [fresh, Related, u8] using agrees_insert hr s.x
          {
            ty := u8
            live := true
            used := false
            known := ConcreteSpec.knownExpr Γ s.e.toCore
            kfields := ConcreteSpec.knownFields Γ s.e.toCore }
      · simp [fresh, Related]
  · trivial

private theorem update_keys (Γ : Ctx) (x : Var) (f : Entry → Entry) :
    List.map Prod.fst (AList.update Γ x f) = List.map Prod.fst Γ := by
  induction Γ with
  | nil => rfl
  | cons pair Γ ih =>
    rcases pair with ⟨y, e⟩
    by_cases h : y = x <;> simp [AList.update, h, ih]

private theorem useAtom_keys {Γ Γ' : Ctx} {a : Atom}
    (h : ConcreteSpec.useAtom [] Γ a.toCore = some Γ') :
    List.map Prod.fst Γ' = List.map Prod.fst Γ := by
  cases a with
  | lit n => cases h; rfl
  | var x =>
    simp only [Atom.toCore, ConcreteSpec.useAtom] at h
    split at h <;> try contradiction
    split at h <;> try contradiction
    cases h
    exact update_keys Γ x _

private theorem uses_keys {Γ Γ' : Ctx} {e : Expr}
    (h : ConcreteSpec.uses [] Γ e.toCore = some Γ') :
    List.map Prod.fst Γ' = List.map Prod.fst Γ := by
  cases e with
  | atom a => exact useAtom_keys h
  | add l r =>
    simp only [Expr.toCore, ConcreteSpec.uses] at h
    split at h <;> try contradiction
    rename_i Γ₁ hl
    exact (useAtom_keys (a := r) h).trans (useAtom_keys (a := l) hl)

private theorem stmt_keys {Γ Γ' : Ctx} {s : Stmt}
    (h : ConcreteSpec.stmtOut [] Γ s.toCore = some Γ') :
    List.map Prod.fst Γ' = s.x :: List.map Prod.fst Γ ∧ s.x ∉ List.map Prod.fst Γ := by
  simp only [Stmt.toCore, ConcreteSpec.stmtOut] at h
  split at h <;> try contradiction
  split at h <;> try contradiction
  rename_i Γ₁ hu
  split at h <;> try contradiction
  rename_i fresh
  cases h
  have keys := uses_keys (e := s.e) hu
  refine ⟨by simp [keys], ?_⟩
  rw [← keys]
  intro hm
  obtain ⟨⟨x, e⟩, he, hx⟩ := List.mem_map.mp hm
  change x = s.x at hx
  subst x
  obtain ⟨_, hf⟩ := AList.lookup_isSome_of_mem he
  rw [fresh] at hf
  contradiction

private theorem lookup_of_mem {Γ : Ctx} (hn : (List.map Prod.fst Γ).Nodup)
    {x : Var} {e : Entry} (he : (x, e) ∈ Γ) : AList.lookup Γ x = some e := by
  induction Γ with
  | nil => simp at he
  | cons pair Γ ih =>
    rcases pair with ⟨y, v⟩
    simp only [List.map_cons, List.nodup_cons] at hn
    rcases List.mem_cons.mp he with hp | hm
    · cases hp; simp [AList.lookup]
    · have hne : y ≠ x := fun eq => hn.1 (eq ▸ List.mem_map_of_mem hm)
      simp [AList.lookup, hne, ih hn.2 hm]

private def discharged (m : Entries) (keys : List Var) : Bool :=
  keys.all fun x => match find m x with
    | some e => (e.ty.isCopy [] || !e.live) && e.used
    | none => false

private theorem discharged_eq {m : Entries} {Γ : Ctx} (h : Agrees m Γ)
    (hn : (List.map Prod.fst Γ).Nodup) :
    discharged m (List.map Prod.fst Γ) = decide (ConcreteSpec.Discharged [] Γ) := by
  apply Bool.eq_iff_iff.mpr
  simp only [discharged, List.all_eq_true, decide_eq_true_eq, ConcreteSpec.Discharged]
  constructor
  · intro hall pair hp
    rcases pair with ⟨x, e⟩
    simpa [h x, lookup_of_mem hn hp] using hall x (List.mem_map_of_mem hp)
  · intro hall x hx
    obtain ⟨⟨y, e⟩, he, rfl⟩ := List.mem_map.mp hx
    simpa [h y, lookup_of_mem hn he] using hall (y, e) he

private def checkBody (m : Entries) (keys : List Var) (ret : Atom) : List Stmt → Bool
  | [] =>
    ConcreteSpec.tyOf [] (project m (Atom.refs ret)).tys (.atom ret.toCore) == some u8 &&
      match readAtom m ret with
      | some next => discharged next keys
      | none => false
  | s :: rest =>
    match step m s with
    | some next => checkBody next (s.x :: keys) ret rest
    | none => false

private theorem checkBody_eq {m : Entries} {Γ : Ctx} (h : Agrees m Γ)
    (hn : (List.map Prod.fst Γ).Nodup) (ret : Atom) (body : List Stmt) :
    checkBody m (List.map Prod.fst Γ) ret body =
      ConcreteSpec.bodyOkB [] Γ (body.map Stmt.toCore) ret.toCore := by
  induction body generalizing m Γ with
  | nil =>
    have ht := (atomTy_frame ret (fun _ hx => project_agrees h hx)).2.1
    have hr := readAtom_agrees h ret
    simp only [checkBody, List.map_nil, ConcreteSpec.bodyOkB, ht, u8]
    change (_ && _) = (_ && (match ConcreteSpec.useAtom [] Γ ret.toCore with
      | some next => decide (ConcreteSpec.Discharged [] next)
      | none => false))
    cases hm : readAtom m ret <;> cases hg : ConcreteSpec.useAtom [] Γ ret.toCore <;>
      simp only [hm, hg, Related] at hr
    · rfl
    · rename_i m' Γ'
      change (_ && discharged m' (List.map Prod.fst Γ)) =
        (_ && decide (ConcreteSpec.Discharged [] Γ'))
      have keys := useAtom_keys hg
      rw [← keys, discharged_eq hr (keys.symm ▸ hn)]
  | cons s rest ih =>
    have hs := step_agrees h s
    simp only [checkBody, List.map_cons, ConcreteSpec.bodyOkB]
    cases hm : step m s <;> cases hg : ConcreteSpec.stmtOut [] Γ s.toCore <;>
      simp only [hm, hg, Related] at hs
    · rfl
    · rename_i m' Γ'
      change checkBody m' (s.x :: List.map Prod.fst Γ) ret rest =
        ConcreteSpec.bodyOkB [] Γ' (rest.map Stmt.toCore) ret.toCore
      obtain ⟨keys, fresh⟩ := stmt_keys hg
      rw [← keys]
      exact ih hs (keys.symm ▸ List.nodup_cons.mpr ⟨fresh, hn⟩)

def checkFast (p : Program) : Bool := checkBody ∅ [] p.ret p.body

theorem checkFast_eq (p : Program) : checkFast p = Core.check (Core.toCore p) := by
  simpa [checkFast, ConcreteSpec.check, ConcreteSpec.Slice0.toCore, ConcreteSpec.DeclsOk] using
    checkBody_eq agrees_empty List.nodup_nil p.ret p.body

private def record (acc : ConcreteSpec.Ctx × List Token) (stmt : Core.Stmt) :
    ConcreteSpec.Ctx × List Token :=
  let (ctx, tokens) := acc
  let known := ConcreteSpec.knownExpr ctx stmt.e.toCore
  let tokens := match stmt.e, known with
    | .add .., some (.int .u8 value) => ⟨stmt.x, value⟩ :: tokens
    | _, _ => tokens
  ((stmt.x, { ty := .scalar (.int .u8), live := true, used := false, known }) :: ctx, tokens)

def evidence (p : Core.Program) : List Token := (p.body.foldl record ([], [])).2.reverse

private def recordFast (acc : Entries × List Token) (stmt : Stmt) : Entries × List Token :=
  let (m, tokens) := acc
  let known := ConcreteSpec.knownExpr (project m (Expr.refs stmt.e)) stmt.e.toCore
  let tokens := match stmt.e, known with
    | .add .., some (.int .u8 value) => ⟨stmt.x, value⟩ :: tokens
    | _, _ => tokens
  (m.insert stmt.x.id { ty := u8, live := true, used := false, known := known }, tokens)

private theorem recordFast_agrees {m : Entries} {Γ : Ctx} (h : Agrees m Γ)
    (tokens : List Token) (s : Stmt) :
    Agrees (recordFast (m, tokens) s).1 (record (Γ, tokens) s).1 ∧
    (recordFast (m, tokens) s).2 = (record (Γ, tokens) s).2 := by
  have hk := (expr_frame s.e (fun _ hx => project_agrees h hx)).2.2.1
  simp only [recordFast, record, hk]
  exact ⟨agrees_insert h _ _, trivial⟩

private theorem fold_recordFast {m : Entries} {Γ : Ctx} (h : Agrees m Γ)
    (tokens : List Token) (body : List Stmt) :
    (body.foldl recordFast (m, tokens)).2 = (body.foldl record (Γ, tokens)).2 := by
  induction body generalizing m Γ tokens with
  | nil => rfl
  | cons s rest ih =>
    obtain ⟨ha, ht⟩ := recordFast_agrees h tokens s
    simp only [List.foldl_cons]
    calc
      _ = (rest.foldl recordFast ((recordFast (m, tokens) s).1, (recordFast (m, tokens) s).2)).2 := rfl
      _ = (rest.foldl recordFast ((recordFast (m, tokens) s).1, (record (Γ, tokens) s).2)).2 := by rw [ht]
      _ = _ := ih ha _

private def evidenceFast (p : Program) : List Token :=
  (p.body.foldl recordFast (∅, [])).2.reverse

theorem evidenceFast_eq (p : Program) : evidenceFast p = evidence p := by
  exact congrArg List.reverse (fold_recordFast agrees_empty [] p.body)

def check (out : Elab.Output) : Except Refusal Output :=
  if checkFast out.program then .ok ⟨out, evidenceFast out.program⟩
  else .error (.checker "Core check rejected the program")

theorem admit_checked {src : Elab.Output} {out : Output} (h : check src = .ok out) :
    Core.Checked (Core.toCore out.core.program) := by
  unfold check at h
  split at h
  · cases h
    exact (Core.check_iff _).mp (checkFast_eq _ ▸ ‹checkFast src.program = true›)
  · contradiction

theorem check_evidence {src : Elab.Output} {out : Output} (h : check src = .ok out) :
    out.core = src ∧ out.tokens = evidence src.program := by
  unfold check at h
  split at h
  · cases h
    exact ⟨rfl, evidenceFast_eq _⟩
  · contradiction

theorem admit_never_traps {src : Elab.Output} {out : Output} (h : check src = .ok out)
    (fuel : Nat) (trap : ConcreteSpec.Trap) :
    Core.run (Core.toCore out.core.program) fuel ≠ .trap trap :=
  Core.slice0_never_traps (admit_checked h) fuel trap

end Concrete.Admission

import Concrete.Interfaces.Contracts
import ConcreteSpec.Admission.Early
import Concrete.IntegratedCore.DischargeTransfer

/-! Execution correspondence for the two located, straight-line Core fragments. -/
namespace Concrete.IntegratedCore.Agreement
open ConcreteSpec ConcreteSpec.Core ConcreteSpec.StageInterface
open Concrete.Interfaces ConcreteSpec.Core.SourcePathSpan

private abbrev nominal (n : ConcreteSpec.Name) : DeclId := ⟨earlyEntry.module, n⟩

/-- Exactly the values representable in a located straight-line program. -/
private inductive RepresentsValue : Ty → EarlyValue → Prop where
  | byte {n : Int} : IntTy.u8.inRange n = true →
      RepresentsValue (.scalar (.int .u8)) (.scalar (.int .u8 n))
  | token (name : ConcreteSpec.Name) : RepresentsValue (.struct name) (.other (nominal name))

/-- Source environments read this projection only for a cell with a represented
byte or nominal. Synthetic machine slots have no entry in that environment. -/
private def specValue : EarlyValue → Val
  | .scalar (.int _ n) => .scalar (.int .u8 n)
  | .other id => .struct id.name []
  | _ => .scalar (.bool false)

private structure Cell where
  key : Var
  value : EarlyValue
  desc : Own.Descriptor
  entry : Option Entry

private def Cell.status (c : Cell) : Own.Status :=
  if c.entry.any (fun e => !e.live) then .moved else .owned

private def Cell.slot (c : Cell) (index : Nat) : Own.Slot :=
  ⟨⟨index⟩, 0, c.desc, c.status⟩

private abbrev Cells := List Cell

private def context : Cells → Ctx
  | [] => []
  | c :: rest => match c.entry with
    | some e => (c.key, e) :: context rest
    | none => context rest

private def environment : Cells → Env
  | [] => []
  | c :: rest => match c.entry with
    | some e => (c.key, if e.live then .val (specValue c.value) else .moved) :: environment rest
    | none => environment rest

private def locals (cs : Cells) : AList Var EarlyValue := cs.map fun c => (c.key, c.value)
private def roots : Cells → AList Var Hybrid.Place
  | [] => []
  | c :: cs => (c.key, ⟨cs.length⟩) :: roots cs

private def slots : Cells → List Own.Slot
  | [] => []
  | c :: cs => c.slot cs.length :: slots cs

private def own (cs : Cells) : Own.State :=
  { frames := [⟨0, roots cs, []⟩], slots := slots cs, nextPlace := cs.length, nextActivation := 1 }

private def store (cs : Cells) : Own.Store Hybrid.State := ⟨⟨[], Mem.Memory.empty⟩, own cs⟩

private def Cell.Valid (Δ : Decls) (base : Nat) (c : Cell) : Prop :=
  match c.entry with
  | some e => c.key.id < base ∧ RepresentsValue e.ty c.value ∧ c.desc.copy = e.ty.isCopy Δ
  | none => base ≤ c.key.id ∧ c.desc.copy = true

private def Valid (Δ : Decls) (base : Nat) (cs : Cells) : Prop :=
  ∀ c ∈ cs, c.Valid Δ base

private theorem slot_lt {cs : Cells} {s : Own.Slot} (h : s ∈ slots cs) : s.place.id < cs.length := by
  induction cs with
  | nil => cases h
  | cons c cs ih =>
    rcases List.mem_cons.mp h with rfl | h
    · exact Nat.lt_succ_self _
    · exact Nat.lt_succ_of_lt (ih h)

private def mark (cs : Cells) (x : Var) (mv : Bool) : Cells :=
  match cs with
  | [] => []
  | c :: tail => if c.key = x then {c with entry := c.entry.map (Entry.read mv)} :: tail
    else c :: mark tail x mv

private theorem mark_shape (cs : Cells) (x : Var) (mv : Bool) :
    (mark cs x mv).map Cell.key = cs.map Cell.key ∧ locals (mark cs x mv) = locals cs := by
  induction cs with
  | nil => exact ⟨rfl, rfl⟩
  | cons c cs ih => by_cases h : c.key = x <;> simp_all [mark, locals]

private theorem mark_length (cs : Cells) (x : Var) (mv : Bool) : (mark cs x mv).length = cs.length := by
  simpa using congrArg List.length (mark_shape cs x mv).1

private theorem mark_roots (cs : Cells) (x : Var) (mv : Bool) : roots (mark cs x mv) = roots cs := by
  induction cs with
  | nil => rfl
  | cons c cs ih => by_cases h : c.key = x <;> simp [mark, h, roots, mark_length, ih]

private theorem mark_valid {cs : Cells} {Δ base} (h : Valid Δ base cs) (x : Var) (mv : Bool) :
    Valid Δ base (mark cs x mv) := by
  induction cs with
  | nil => exact h
  | cons c cs ih =>
    have hc := h c (.head _)
    have ht := fun k hk => h k (.tail _ hk)
    by_cases eq : c.key = x
    · simp only [mark, eq, ↓reduceIte]
      intro k hk
      rcases List.mem_cons.mp hk with rfl | hk
      · cases he : c.entry <;> simpa [Cell.Valid, he, Entry.read, eq] using hc
      · exact ht _ hk
    · simp only [mark, eq, ↓reduceIte]
      intro k hk
      rcases List.mem_cons.mp hk with rfl | hk
      · exact hc
      · exact ih ht _ hk

private theorem slot_at {cs : Cells} {s : Own.Slot} (h : s ∈ slots cs) :
    Own.slot (own cs) s.place = some s := by
  induction cs with
  | nil => cases h
  | cons c cs ih =>
    rcases List.mem_cons.mp h with rfl | h
    · simp [Own.slot, own, slots]
    · have lt := slot_lt h
      have ne : (c.slot cs.length).place ≠ s.place := fun eq => (Nat.ne_of_lt lt) (congrArg Hybrid.Place.id eq).symm
      simpa [Own.slot, own, slots, ne] using ih h

private def movedAt (p : Hybrid.Place) (s : Own.Slot) : Own.Slot :=
  if s.place = p then {s with status := .moved} else s

/-- A successful source read selects the same local and the same live owner
place in the instrumented frame. The straight-line environment records moves
explicitly while the machine retains the value and marks its owner place. -/
private theorem read_local {Δ base cs x e} (valid : Valid Δ base cs)
    (lookup : AList.lookup (context cs) x = some e) (live : e.live = true) (mv : Bool) :
    ∃ (c : Cell) (p : Hybrid.Place), RepresentsValue e.ty c.value ∧ c.desc.copy = e.ty.isCopy Δ ∧
      AList.lookup (locals cs) x = some c.value ∧
      AList.lookup (environment cs) x = some (.val (specValue c.value)) ∧
      AList.lookup (roots cs) x = some p ∧
      (⟨p, 0, c.desc, .owned⟩ : Own.Slot) ∈ slots cs ∧
      context (mark cs x mv) = ctxMark (context cs) x mv ∧
      environment (mark cs x mv) =
        (if mv then AList.update (environment cs) x (fun _ => .moved) else environment cs) ∧
      slots (mark cs x mv) = (if mv then (slots cs).map (movedAt p) else slots cs) := by
  induction cs with
  | nil => cases lookup
  | cons c cs ih =>
    have vc := valid c (.head _)
    have vt : Valid Δ base cs := fun k hk => valid k (.tail _ hk)
    cases he : c.entry with
    | some entry =>
      simp only [context, he, AList.lookup_cons] at lookup
      by_cases eq : c.key = x
      · simp only [eq, ↓reduceIte, Option.some.injEq] at lookup
        subst entry
        have vv : c.key.id < base ∧ RepresentsValue e.ty c.value ∧ c.desc.copy = e.ty.isCopy Δ := by
          simpa [Cell.Valid, he] using vc
        have unchanged : (slots cs).map (movedAt ⟨cs.length⟩) = slots cs := by
          change (slots cs).map _ = _
          conv => rhs; rw [← List.map_id (l := slots cs)]
          apply List.map_congr_left
          intro s hs
          have ne : s.place ≠ ⟨cs.length⟩ := fun eq => Nat.ne_of_lt (slot_lt hs) (congrArg Hybrid.Place.id eq)
          simp [movedAt, ne]
        refine ⟨c, ⟨cs.length⟩, vv.2.1, vv.2.2, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩
        all_goals cases mv <;> simp [locals, environment, roots, slots, mark, context, ctxMark,
          AList.lookup, AList.update, eq, he, live, Cell.slot, Cell.status, Entry.read, movedAt, unchanged]
      · simp only [eq, ↓reduceIte] at lookup
        obtain ⟨d, p, hv, hd, hl, henv, hr, hs, hc, hev, hslots⟩ := ih vt lookup
        have ne : (c.slot cs.length).place ≠ p := by
          have lt := slot_lt hs
          exact fun same => Nat.ne_of_lt lt (congrArg Hybrid.Place.id same).symm
        refine ⟨d, p, hv, hd, ?_, ?_, ?_, .tail _ hs, ?_, ?_, ?_⟩
        all_goals cases mv <;> simp_all [locals, environment, roots, slots, mark, context, ctxMark,
          AList.lookup_cons, AList.update_cons, movedAt, mark_length]
    | none =>
      simp only [context, he] at lookup
      obtain ⟨d, p, hv, hd, hl, henv, hr, hs, hc, hev, hslots⟩ := ih vt lookup
      have named_lt {tail : Cells} (v : Valid Δ base tail) (found : AList.lookup (context tail) x = some e) :
          x.id < base := by
        induction tail with
        | nil => cases found
        | cons k tail it =>
          have vk := v k (.head _)
          have vt' := fun a ha => v a (.tail _ ha)
          cases h : k.entry with
          | none => exact it vt' (by simpa [context, h] using found)
          | some en =>
            by_cases hk : k.key = x
            · simpa [hk] using (show k.key.id < base from
                (show k.key.id < base ∧ _ from (by simpa [Cell.Valid, h] using vk)).1)
            · exact it vt' (by simpa [context, h, AList.lookup_cons, hk] using found)
      have lo := named_lt vt lookup
      have hi : base ≤ c.key.id := (show base ≤ c.key.id ∧ c.desc.copy = true from
        (by simpa [Cell.Valid, he] using vc)).1
      have eq : c.key ≠ x := fun same => Nat.not_lt_of_ge (same ▸ hi) lo
      have ne : (c.slot cs.length).place ≠ p := fun same =>
        Nat.ne_of_lt (slot_lt hs) (congrArg Hybrid.Place.id same).symm
      refine ⟨d, p, hv, hd, ?_, ?_, ?_, .tail _ hs, ?_, ?_, ?_⟩
      all_goals cases mv <;> simp_all [locals, environment, roots, slots, mark, context, ctxMark,
        AList.lookup_cons, movedAt, mark_length]

private structure DeclModel (Δ : Decls) (ds : Declarations) : Prop where
  linear (name : ConcreteSpec.Name) : (Ty.struct name).isCopy Δ = false
  declared {name d} : findDecl Δ name = some d → ds.emptyLinear (nominal name) = true

private theorem decl_model (p : StraightProgram) (ok : DeclsOk (List.map Prod.snd p.decls)) :
    DeclModel (List.map Prod.snd p.decls) p.decls := by
  let decl (n : ConcreteSpec.Name) : StructDecl := ⟨n, false, []⟩
  have names (n : ConcreteSpec.Name) : findDecl (List.map Prod.snd p.decls) n = some (decl n) ∨
      findDecl (List.map Prod.snd p.decls) n = none := by
    unfold findDecl
    cases h : (List.map Prod.snd p.decls).find? _ with
    | none => exact .inr rfl
    | some d =>
      have mem := List.mem_of_find?_eq_some h
      have same := List.find?_some h
      simp [StraightProgram.decls] at mem
      obtain ⟨name, _, _, rfl⟩ := mem
      simp at same
      cases same
      exact .inl rfl
  constructor
  · intro n
    rcases names n with h | h <;> simp [Ty.isCopy, h, decl]
  · intro n d hd
    have member := List.mem_of_find?_eq_some hd
    have same := List.find?_some hd
    have hn : (p.declarations.map Prod.fst).Nodup := by simpa [StraightProgram.decls, Function.comp_def] using ok.1
    have present : n ∈ p.declarations.map Prod.fst := by
      have key := List.mem_map_of_mem (f := StructDecl.name) member
      rw [beq_iff_eq.mp same] at key
      simpa [StraightProgram.decls, Function.comp_def] using key
    unfold Declarations.emptyLinear StraightProgram.decls
    have filt (xs : List (ConcreteSpec.Name × Surface0.Span)) (unique : (xs.map Prod.fst).Nodup) :
        ((xs.map fun (name, _) => (nominal name, decl name)).filter (fun e => e.1 == nominal n)) =
          (if n ∈ xs.map Prod.fst then [(nominal n, decl n)] else []) := by
      induction xs with
      | nil => simp
      | cons a xs ih =>
        obtain ⟨key, span⟩ := a
        have ⟨apart, rest⟩ := List.nodup_cons.mp unique
        by_cases eq : key = n
        · subst key
          simp [ih rest, apart]
        · have ne : nominal key ≠ nominal n := fun h => eq (congrArg DeclId.name h)
          have absent : (n ∈ List.map Prod.fst ((key, span) :: xs)) ↔ n ∈ List.map Prod.fst xs := by
            change n ∈ key :: _ ↔ _
            simp [Ne.symm eq]
          simp [ih rest, ne]
          split <;> split <;> simp_all
    change (match (p.declarations.map fun (key, _) => (nominal key, decl key)).filter (fun e => e.1 == nominal n) with | [(_, d)] => d.name == n && !d.copy && d.fields.isEmpty | _ => false) = true
    rw [filt p.declarations hn, ite_eq_left present]
    simp [decl]

private theorem copy_byte {Δ ds t v} (decl : DeclModel Δ ds) (h : RepresentsValue t v)
    (copy : t.isCopy Δ = true) :
    ∃ n, t = .scalar (.int .u8) ∧ v = .scalar (.int .u8 n) ∧ IntTy.u8.inRange n = true := by
  cases h with
  | token name => rw [decl.linear] at copy; cases copy
  | byte range => exact ⟨_, rfl, rfl, range⟩

private theorem move_token {Δ t v} (h : RepresentsValue t v) (linear : t.isCopy Δ = false) :
    ∃ n, t = .struct n ∧ v = .other (nominal n) := by
  cases h with
  | token n => exact ⟨_, rfl, rfl⟩
  | byte => cases linear

private structure ReadResult (Δ : Decls) (base : Nat) (cs : Cells) (Γ' : Ctx)
    (next : Cells) : Prop where
  valid : Valid Δ base next
  contextEq : context next = Γ'
  keys : next.map Cell.key = cs.map Cell.key
  localsEq : locals next = locals cs
  rootsEq : roots next = roots cs

private theorem read_byte {Δ ds base cs a Γ' t} (decl : DeclModel Δ ds) (valid : Valid Δ base cs)
    (ht : HasType Δ (context cs).tys (.atom (Slice0.Atom.toCore a)) t)
    (use : UseAtom Δ (context cs) a.toCore Γ') :
    ∃ n next, t = .scalar (.int .u8) ∧ IntTy.u8.inRange n = true ∧
      ReadResult Δ base cs Γ' next ∧
      earlyRead (locals cs) (earlyOperand a) = some (.scalar (.int .u8 n)) ∧
      earlyPermitRead (own cs) (earlyOperand a) = some (own next) ∧
      evalExpr Δ (environment cs) (.atom a.toCore) = .ok (.scalar (.int .u8 n), environment next) ∧
      evalAtom Δ (environment cs) a.toCore = .ok (.int .u8 n, environment next) := by
  cases a with
  | lit n =>
    cases ht with | int range =>
      cases use
      exact ⟨_, _, rfl, range, ⟨valid, rfl, rfl, rfl, rfl⟩, rfl, rfl, rfl, rfl⟩
  | var x =>
    cases ht with | copy typeFound =>
      cases use with | copy found live copy =>
        rw [tys_lookup, found] at typeFound
        cases typeFound
        obtain ⟨c, p, val, desc, localEq, envEq, rootEq, slotMem, ctx, env, slotEq⟩ :=
          read_local valid found live false
        obtain ⟨n, ty, cv, range⟩ := copy_byte decl val copy
        rw [cv] at localEq envEq
        rw [ty]
        · have hs := slot_at slotMem
          have same : own (mark cs x false) = own cs := by simp [own, mark_roots, mark_length, slotEq]
          refine ⟨_, _, rfl, range, ⟨mark_valid valid _ _, ctx, (mark_shape ..).1,
            (mark_shape ..).2, mark_roots ..⟩, ?_, ?_, ?_, ?_⟩
          · simp [earlyOperand, earlyRead, localEq]
          · rw [same]
            simp [earlyOperand, earlyPermitRead, own, rootEq, Own.accessOwner, Own.ownerAccess, own] at hs ⊢
            simp [hs]
          · simp [Slice0.Atom.toCore, evalExpr, readVar, envEq, specValue, Val.isCopy, env]
          · simp [Slice0.Atom.toCore, evalAtom, readVar, envEq, specValue, Val.isCopy, env]

private inductive EvalResult (Δ : Decls) (ds : Declarations) (base : Nat) (cs : Cells)
    (e : StraightExpr) (t : Ty) (Γ' : Ctx) : Prop where
  | ok (v : EarlyValue) (next : Cells) : RepresentsValue t v → ReadResult Δ base cs Γ' next →
      earlyEval ds (locals cs) e.integrated = .ok v →
      earlyPermit (own cs) e.integrated = some (own next) →
      evalExpr Δ (environment cs) e.toCore = .ok (specValue v, environment next) →
      EvalResult Δ ds base cs e t Γ'
  | overflow (next : Own.State) : earlyEval ds (locals cs) e.integrated = .trap .overflow →
      earlyPermit (own cs) e.integrated = some next →
      evalExpr Δ (environment cs) e.toCore = .trap .arithmeticOverflow →
      EvalResult Δ ds base cs e t Γ'

private theorem atom_hasType {Δ Γ a st} (ht : AtomTy Γ a st) : HasType Δ Γ (.atom a) (.scalar st) := by
  cases ht <;> constructor <;> assumption

private theorem expr_agrees {Δ ds base cs Γ' e t} (decl : DeclModel Δ ds) (valid : Valid Δ base cs)
    (typed : HasType Δ (context cs).tys (StraightExpr.toCore e) t)
    (used : Uses Δ (context cs) e.toCore Γ') : EvalResult Δ ds base cs e t Γ' := by
  cases e with
  | scalar rhs =>
    cases rhs with
    | atom a =>
      cases used with | atom use =>
        obtain ⟨n, next, rfl, range, frame, read, permit, eval, _⟩ := read_byte decl valid typed use
        exact .ok _ next (.byte range) frame (by simp [StraightExpr.integrated, earlyExpression, earlyEval, read])
          permit eval
    | add l r =>
      cases typed with | add tl tr =>
        cases used with | add ul ur =>
          obtain ⟨a, mid, typeEq, _, fm, rl, pl, _, sl⟩ := read_byte decl valid (atom_hasType tl) ul
          cases typeEq
          obtain ⟨b, last, _, _, fl, rr, pr, _, sr⟩ := read_byte decl fm.valid
            (atom_hasType (by rw [fm.contextEq, useAtom_tys ul]; exact tr)) (fm.contextEq ▸ ur)
          rw [fm.localsEq] at rr
          have permit : earlyPermit (own cs) (earlyExpression (.add l r)) = some (own last) := by
            simp [earlyExpression, earlyPermit, pl, pr]
          by_cases range : Core.IntTy.u8.InRange (a+b)
          · have fits : IntTy.u8.inRange (a+b) = true := decide_eq_true range
            exact .ok _ last (.byte fits) ⟨fl.valid, fl.contextEq, fl.keys.trans fm.keys,
              fl.localsEq.trans fm.localsEq, fl.rootsEq.trans fm.rootsEq⟩
              (by simp [StraightExpr.integrated, earlyExpression, earlyEval, rl, rr, plain, checked,
                        inRangeOr, range, Res.map, Res.bind]) permit
              (by simp [StraightExpr.toCore, Slice0.Expr.toCore, evalExpr, sl, sr, checkedAdd, fits, specValue])
          · have fits : IntTy.u8.inRange (a+b) = false := decide_eq_false range
            exact .overflow _ (by simp [StraightExpr.integrated, earlyExpression, earlyEval, rl, rr,
              plain, checked, inRangeOr, range, Res.map, Res.bind, ArithmeticError.trapKind]) permit
              (by simp [StraightExpr.toCore, Slice0.Expr.toCore, evalExpr, sl, sr, checkedAdd, fits])
  | move x =>
    cases typed with | move ht =>
      cases used with | atom use =>
        cases use with | move found live linear =>
          rw [tys_lookup, found] at ht
          cases ht
          obtain ⟨c, p, value, desc, localEq, envEq, rootEq, slotMem, ctx, env, slotEq⟩ :=
            read_local valid found live true
          have slot := slot_at slotMem
          obtain ⟨name, ty, cv⟩ := move_token value linear
          rw [cv] at localEq envEq
          rw [ty] at linear ⊢
          · refine .ok _ _ (.token name) ⟨mark_valid valid _ _, ctx, (mark_shape ..).1,
              (mark_shape ..).2, mark_roots ..⟩ ?_ ?_ ?_
            · simp [StraightExpr.integrated, earlyEval, earlyRead, localEq]
            · simp [StraightExpr.integrated, earlyPermit, earlyPermitRead, Own.move, own, rootEq,
                own] at slot ⊢
              simp [slot, desc, ty, linear, mark_roots, mark_length, slotEq, movedAt]
            · simp [StraightExpr.toCore, evalExpr, readVar, envEq, specValue, env, Val.isCopy] <;> exact linear
  | construct name =>
    cases typed with | mk found _ =>
      cases used with | mk use =>
        cases use
        exact .ok _ _ (.token name) ⟨valid, rfl, rfl, rfl, rfl⟩
          (by simp [StraightExpr.integrated, earlyEval, decl.declared found]) rfl rfl

private theorem lookup_fresh {Δ base cs x} (valid : Valid Δ base cs) (small : x.id < base)
    (fresh : Fresh (context cs) x) : AList.lookup (roots cs) x = none := by
  induction cs with
  | nil => rfl
  | cons c cs ih =>
    have vc := valid c (.head _)
    have vt : Valid Δ base cs := fun k hk => valid k (.tail _ hk)
    cases he : c.entry with
    | none =>
      have big : base ≤ c.key.id := (show base ≤ c.key.id ∧ c.desc.copy = true from
        (by simpa [Cell.Valid, he] using vc)).1
      have ne : c.key ≠ x := fun h => Nat.not_lt_of_ge (h ▸ big) small
      simp [roots, AList.lookup_cons, ne, ih vt (by simpa [context, he, Fresh] using fresh)]
    | some e =>
      simp only [Fresh, context, he, AList.lookup_cons] at fresh
      split at fresh
      · cases fresh
      · rename_i ne
        simp [roots, AList.lookup_cons, ne, ih vt fresh]

private theorem roots_keys (cs : Cells) : List.map Prod.fst (roots cs) = cs.map Cell.key := by
  induction cs with
  | nil => rfl
  | cons c cs ih => simp [roots, ih]

private def cellFor (b : StraightBinding) (base pos : Nat) (v : EarlyValue) (entry : Option Entry) : Cell :=
  ⟨b.target base pos, v, b.descriptor, entry⟩

private inductive RowResult (Δ : Decls) (ds : Declarations) (base pos : Nat)
    (cs : Cells) (b : StraightBinding) (Γ' : Ctx) : Prop where
  | ok (v : EarlyValue) (entry : Option Entry) (mid : Cells) :
      (cellFor b base pos v entry).status = .owned →
      Valid Δ base (cellFor b base pos v entry :: mid) → context (cellFor b base pos v entry :: mid) = Γ' →
      mid.map Cell.key = cs.map Cell.key → roots mid = roots cs → locals mid = locals cs →
      AList.lookup (roots mid) (b.target base pos) = none →
      earlyEval ds (locals cs) b.expression = .ok v →
      earlyPermit (own cs) b.expression = some (own mid) →
      execStmt Δ (environment cs) b.toCore = .ok (environment (cellFor b base pos v entry :: mid)) →
      RowResult Δ ds base pos cs b Γ'
  | overflow (permission : Own.State) : earlyEval ds (locals cs) b.expression = .trap .overflow →
      earlyPermit (own cs) b.expression = some permission →
      execStmt Δ (environment cs) b.toCore = .trap .arithmeticOverflow →
      RowResult Δ ds base pos cs b Γ'

private theorem valid_cons {Δ base c cs} (hc : Cell.Valid Δ base c) (ht : Valid Δ base cs) :
    Valid Δ base (c :: cs) := by
  intro k hk
  rcases List.mem_cons.mp hk with rfl | hk
  · exact hc
  · exact ht _ hk

private theorem row_agrees {Δ ds base pos cs Γ'} (decl : DeclModel Δ ds) (valid : Valid Δ base cs)
    (older : ∀ x ∈ cs.map Cell.key, x.id < base + pos) (b : StraightBinding)
    (bounded : b.bound ≤ base) (ok : StmtOkWith ArithmeticPolicy.runtime.permits Δ (context cs) b.toCore Γ') :
    RowResult Δ ds base pos cs b Γ' := by
  have synthetic {mid : Cells} (keys : mid.map Cell.key = cs.map Cell.key) :
      AList.lookup (roots mid) ⟨base + pos⟩ = none := by
    cases h : AList.lookup (roots mid) ⟨base + pos⟩ with
    | none => rfl
    | some p =>
      have mem := List.mem_map_of_mem (f := Prod.fst) (AList.lookup_mem h)
      rw [roots_keys, keys] at mem
      exact (Nat.lt_irrefl _ (older _ mem)).elim
  have bind_case (binder : Binder) (t : StraightType) (rhs : StraightExpr)
      (bound : (StraightBinding.bind binder t rhs).bound ≤ base)
      (hs : StmtOkWith ArithmeticPolicy.runtime.permits Δ (context cs) (.let_ binder t.toCore rhs.toCore) Γ') :
      RowResult Δ ds base pos cs (.bind binder t rhs) Γ' := by
    have descriptor : (StraightBinding.bind binder t rhs).descriptor.copy = t.toCore.isCopy Δ := by
      cases t with
      | u8 => rfl
      | nominal n => exact (decl.linear n).symm
    cases binder with
    | name x =>
      have small : x.id < base := Nat.lt_of_succ_le (Nat.le_trans (Nat.le_max_left ..) bound)
      cases hs with | let_ ht _ hu fresh =>
        cases expr_agrees decl valid ht hu with
        | overflow permission trap perm spec =>
          exact .overflow permission trap perm (by simp [StraightBinding.toCore, execStmt, spec])
        | ok v mid value h eval permit spec =>
          refine .ok v (some ⟨t.toCore, true, false, knownExpr (context cs) rhs.toCore,
            knownFields (context cs) rhs.toCore⟩) mid rfl (valid_cons ?_ h.valid) ?_ h.keys h.rootsEq h.localsEq ?_ eval permit ?_
          · exact ⟨small, value, descriptor⟩
          · simp [cellFor, context, h.contextEq, StraightBinding.target, StraightBinding.destination]
          · exact lookup_fresh h.valid small (h.contextEq ▸ fresh)
          · simp [cellFor, StraightBinding.toCore, execStmt, spec, environment, StraightBinding.target,
              StraightBinding.destination]
    | wild =>
      cases hs with | letWild ht _ copy hu =>
        cases expr_agrees decl valid ht hu with
        | overflow permission trap perm spec =>
          exact .overflow permission trap perm (by simp [StraightBinding.toCore, execStmt, spec])
        | ok v mid value h eval permit spec =>
          exact .ok v none mid rfl (valid_cons ⟨Nat.le_add_right _ _, descriptor.trans copy⟩ h.valid)
            h.contextEq h.keys h.rootsEq h.localsEq (synthetic h.keys) eval permit
            (by simp [StraightBinding.toCore, execStmt, spec, environment, cellFor])
  cases b with
  | bind binder t rhs => exact bind_case binder t rhs bounded ok
  | slice0 s =>
    obtain ⟨x, e⟩ := s
    have h := bind_case (.name x) .u8 (.scalar e) bounded ok
    cases h with
    | ok v en mid alive hv hc hk hr hl hf he hp hs => exact .ok v en mid alive hv hc hk hr hl hf he hp hs
    | overflow perm he hp hs => exact .overflow perm he hp hs
  | destructure n rhs =>
    cases ok with | letStruct ht _ found _ hu _ =>
      cases expr_agrees decl valid ht hu with
      | overflow permission trap perm spec =>
        exact .overflow permission (by simp [StraightBinding.expression, earlyEval, trap]) perm
          (by simp [StraightBinding.toCore, execStmt, spec])
      | ok v mid value h eval permit spec =>
        cases value
        exact .ok (.scalar .unit) none mid rfl (valid_cons ⟨Nat.le_add_right _ _, rfl⟩ h.valid)
          h.contextEq h.keys h.rootsEq h.localsEq (synthetic h.keys)
          (by simp [StraightBinding.expression, earlyEval, eval, decl.declared found, nominal]) permit
          (by simp [StraightBinding.toCore, execStmt, spec, specValue, environment, cellFor, destructure])

private theorem discharge_slots {cs : Cells} {Δ base} (valid : Valid Δ base cs)
    (ends : Discharged Δ (context cs)) :
    ∀ s ∈ slots cs, s.desc.copy = true ∨ s.status = .moved := by
  induction cs with
  | nil => exact nofun
  | cons c cs ih =>
    have vc := valid c (.head _)
    have vt : Valid Δ base cs := fun a ha => valid a (.tail _ ha)
    intro s h
    cases he : c.entry with
    | none =>
      rcases List.mem_cons.mp h with rfl | h
      · exact .inl (show base ≤ c.key.id ∧ c.desc.copy = true from by simpa [Cell.Valid, he] using vc).2
      · exact ih vt (by simpa [context, he] using ends) _ h
    | some e =>
      have ec : Discharged Δ ((c.key, e) :: context cs) := by simpa [context, he] using ends
      rcases List.mem_cons.mp h with rfl | h
      · have d := (ec _ (.head _)).1
        have v : c.key.id < base ∧ RepresentsValue e.ty c.value ∧ c.desc.copy = e.ty.isCopy Δ := by
          simpa [Cell.Valid, he] using vc
        rcases d with copy | dead
        · exact .inl (v.2.2.trans copy)
        · exact .inr (by simp [Cell.slot, Cell.status, he] <;> exact dead)
      · exact ih vt (fun a ha => ec a (.tail _ ha)) _ h

private theorem cleanup {cs : Cells} (xs : List Var)
    (free : ∀ s ∈ slots cs, s.desc.copy = true ∨ s.status = .moved) :
    ∃ left final, Own.drop xs (own cs) = some left ∧ Own.leave left = some final := by
  have can (s : Own.State) (loans : s.loans = []) (ok : ∀ k ∈ s.slots, k.desc.copy = true ∨ k.status = .moved)
      (ps : List Hybrid.Place) : Own.canDropPlaces s ps = true := by
    simp only [Own.canDropPlaces, loans, List.all_nil, Bool.and_true, List.all_eq_true]
    intro k hk
    rcases ok k hk with h | h <;> simp [h]
  let places := List.map Prod.snd ((roots cs).filter fun e => decide (e.1 ∈ xs))
  let left : Own.State := { own cs with
    slots := (slots cs).filter fun k => decide (k.place ∉ places)
    frames := [⟨0, (roots cs).filter fun e => decide (e.2 ∉ places), []⟩] }
  have hdrop : Own.drop xs (own cs) = some left := by
    unfold Own.drop
    change (if Own.canDropPlaces (own cs) places then _ else none) = _
    rw [ite_eq_left (can _ rfl free _)]
    rfl
  have retained : ∀ k ∈ left.slots, k.desc.copy = true ∨ k.status = .moved :=
    fun k hk => free k (List.mem_filter.mp hk).1
  let keys := List.map Prod.fst ((roots cs).filter fun e => decide (e.2 ∉ places))
  have release : ∃ after, Own.drop keys left = some after := by
    unfold Own.drop
    change ∃ _, (if Own.canDropPlaces left _ then _ else none) = _
    rw [ite_eq_left (can _ rfl retained _)]
    exact ⟨_, rfl⟩
  obtain ⟨after, h⟩ := release
  refine ⟨left, {after with frames := after.frames.tail}, hdrop, ?_⟩
  change (do let s ← Own.drop keys left; pure {s with frames := s.frames.tail}) = _
  rw [h]; rfl

private abbrev Machine (slice : Slice) (owners : AList Var Own.Descriptor) (ds : Declarations) :=
  (sliceLanguage slice).instrumented owners ds

private abbrev Next {O : Exec.Operands} (P : Exec.Program O) (c q : Exec.Config O) :=
  Exec.step P c = some (.next q)

/-- Locate the first trapping statement using the straight-line judgment's own
statement step and environment. A stuck prefix never reaches a later site. -/
def firstTrapSite (decls : Decls) (env : Env) (position : Nat) : List ConcreteSpec.Stmt → Option SiteId
  | [] => none
  | stmt :: rest => match execStmt decls env stmt with
    | .ok next => firstTrapSite decls next (position + 1) rest
    | .trap _ => some ⟨position⟩
    | .stuck _ => none

inductive Matches (first : Option SiteId) : Outcome → Exec.Final EarlyValue → Prop where
  | returned {n : Int} : IntTy.u8.inRange n = true →
      Matches first (.exit n.toNat) (.returned (.scalar (.int .u8 n)))
  | overflow {site : SiteId} : first = some site →
      Matches first (.trap .arithmeticOverflow) (.trapped .overflow site)
  | abort (site : SiteId) : Matches first (.aborted site) (.trapped .abort site)

private theorem eval_ok {slice owners ds cs e v mid}
    (value : earlyEval ds (locals cs) e = .ok v)
    (permit : earlyPermit (own cs) e = some (own mid)) :
    (Machine slice owners ds).eval (locals cs) (store cs) (.inl e) = .ok (v, store mid) := by
  change (match earlyPermit (own cs) e with | none => _ | some _ => _) = _
  rw [permit]
  simp [sliceLanguage, earlyEvaluate, value, store, Res.map, Res.bind]

private theorem eval_overflow {slice owners ds cs e permission}
    (trap : earlyEval ds (locals cs) e = .trap .overflow)
    (permit : earlyPermit (own cs) e = some permission) :
    (Machine slice owners ds).eval (locals cs) (store cs) (.inl e) = .trap .overflow := by
  change (match earlyPermit (own cs) e with | none => _ | some _ => _) = _
  rw [permit]
  simp [sliceLanguage, earlyEvaluate, trap, store, Res.map, Res.bind]

private theorem life_ok {slice owners ds} (action : Exec.Life) {s t : Own.State}
    (h : Own.life (AList.lookup owners) action s = some t) :
    (Machine slice owners ds).life action ⟨⟨[], Mem.Memory.empty⟩, s⟩ =
      some ⟨⟨[], Mem.Memory.empty⟩, t⟩ := by
  change (do let next ← Own.life (AList.lookup owners) action s; pure (⟨⟨[], Mem.Memory.empty⟩, next⟩ : Own.Store Hybrid.State)) = _
  rw [h]
  rfl

private abbrev frames {O : Exec.Operands} : List (Exec.Frame O) := [.scope ⟨0⟩ [] [], .functionEnd]

private theorem body_agrees {slice owners ds Δ base} (P : Exec.Program (Machine slice owners ds))
    (decl : DeclModel Δ ds) (ending : StraightEnd) (site : SiteId)
    (rows : List LocatedStraightBinding) (pos : Nat) (cs : Cells)
    (valid : Valid Δ base cs)
    (bounded : ∀ b ∈ rows, b.statement.bound ≤ base)
    (older : ∀ x ∈ cs.map Cell.key, x.id < base + pos)
    (table : ∀ x, (roots cs).lookup x = none →
      AList.lookup owners x = AList.lookup (StraightProgram.owners base rows pos) x)
    (checked : BodyOkWith ArithmeticPolicy.runtime.permits Δ (context cs) (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
      (ending.toCore site)) :
    ∃ q f o, Concrete.Contracts.Steps (Next P)
      ⟨.exec (StraightProgram.machineBody base (ending.machine site) rows pos), locals cs, store cs, frames⟩ q ∧
      q.final? = some f ∧ Matches (firstTrapSite Δ (environment cs) pos
        (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))) o f ∧
      ConcreteSpec.Exec Δ (environment cs) (rows.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement))
        (ending.toCore site) o := by
  induction rows generalizing pos cs with
  | nil =>
    cases ending with
    | abort =>
      exact ⟨_, _, _, .cons rfl (.nil _), rfl, .abort site, .finish⟩
    | ret a =>
      cases checked with | ret ht uses endCtx =>
        cases uses with | atom use =>
          obtain ⟨n, mid, _, range, h, read, perm, _, result⟩ := read_byte decl valid ht use
          have eval : earlyEval ds (locals cs) (.read (earlyOperand a)) = .ok (.scalar (.int .u8 n)) := by
            simp [earlyEval, read]
          have close := discharge_slots h.valid (h.contextEq ▸ endCtx)
          obtain ⟨left, last, drop, leave⟩ := cleanup (Exec.dead [] (locals cs)) close
          have tick := eval_ok (slice := slice) (owners := owners) eval perm
          have pop := life_ok (slice := slice) (owners := owners) (ds := ds) (.drop _) drop
          have exit := life_ok (slice := slice) (owners := owners) (ds := ds) .leave leave
          let v : EarlyValue := .scalar (.int .u8 n)
          let deadRoots := Exec.restrict [] (locals cs)
          let doneState : Exec.Config (Machine slice owners ds) :=
            ⟨.exiting (.ret v), deadRoots, ⟨⟨[], Mem.Memory.empty⟩, last⟩, []⟩
          refine ⟨doneState, _, _, .cons (b := ⟨.exiting (.ret v), locals cs, store mid, frames⟩) ?_
            (.cons (b := ⟨.exiting (.ret v), deadRoots, ⟨⟨[], Mem.Memory.empty⟩, left⟩, [.functionEnd]⟩) ?_
              (.cons ?_ (.nil _))), rfl, .returned range, ?_⟩
          · change Exec.onRes _ _ _ _ _ _ = _
            rw [tick]; rfl
          · change (Exec.dropOut (Machine slice owners ds) [] (locals cs) (store mid)).map _ = _
            unfold Exec.dropOut
            exact congrArg (Option.map (fun s => Exec.Out.next (O := Machine slice owners ds)
              ⟨.exiting (.ret v), deadRoots, s, [.functionEnd]⟩)) pop
          · change ((Machine slice owners ds).life .leave _).map _ = _
            rw [exit]; rfl
          · have he : finish Δ (environment cs) (.ret a.toCore) = .exit n.toNat := by
              simp [finish, retOutcome, result]
            exact he ▸ ConcreteSpec.Exec.finish
  | cons b rows ih =>
    cases checked with | cons hs tail =>
      let continuation := StraightProgram.machineBody base (ending.machine (slice := slice)
        (owners := owners) (decls := ds) site) rows (pos + 1)
      have top := bounded b (.head _)
      have small := fun k hk => bounded k (.tail _ hk)
      cases row_agrees decl valid older b.statement top hs with
      | overflow state trap permit spec =>
        have eval := eval_overflow (slice := slice) (owners := owners) trap permit
        refine ⟨⟨.trapped .overflow ⟨pos⟩, locals cs, store cs, .seq continuation :: frames⟩, _, _, .cons rfl (.cons ?_ (.nil _)), rfl, .overflow (by simp [firstTrapSite, spec]), .trap spec⟩
        change Exec.onRes _ _ _ _ _ _ = _
        rw [eval]; rfl
      | ok v entry mid alive good ctx keys rootsEq localsEq fresh eval permit spec =>
        let x := b.statement.target base pos
        let c := cellFor b.statement base pos v entry
        have old : (roots cs).lookup x = none := rootsEq ▸ fresh
        have desc : AList.lookup owners x = some b.statement.descriptor := by
          rw [table _ old]; simp [StraightProgram.owners, AList.lookup_cons, x]
        have bind : Own.bind (AList.lookup owners) x (own mid) = some (own (c :: mid)) := by
          simp [Own.bind, own, roots, slots, Cell.slot, c, cellFor, x, fresh, desc] at *
          exact alive.symm
        have newer : ∀ y ∈ (c :: mid).map Cell.key, y.id < base + (pos + 1) := by
          intro y hy
          rcases List.mem_cons.mp hy with rfl | hy
          · change (b.statement.target base pos).id < _
            unfold StraightBinding.target
            cases hd : b.statement.destination with
            | none => simp
            | some z =>
              have bound := Nat.le_trans (Nat.le_max_left _ _) top
              simp [hd] at bound
              simp; omega
          · rw [keys] at hy
            exact Nat.lt_trans (older _ hy) (by omega)
        have nextTable : ∀ y, (roots (c :: mid)).lookup y = none →
            AList.lookup owners y = AList.lookup (StraightProgram.owners base rows (pos + 1)) y := by
          intro y hy
          change (if x = y then some _ else (roots mid).lookup y) = none at hy
          split at hy
          · cases hy
          · rename_i ne
            rw [table y (rootsEq ▸ hy)]
            change (if x = y then _ else _) = _
            rw [ite_eq_right ne]
        obtain ⟨q, f, o, path, final, obs, run⟩ := ih _ _ good small newer nextTable (ctx ▸ tail)
        have tick := eval_ok (slice := slice) (owners := owners) eval permit
        have life := life_ok (slice := slice) (owners := owners) (ds := ds) (.bind x) bind
        refine ⟨q, f, o, .cons rfl (.cons (b := ⟨.done, (x, v) :: locals cs, store (c :: mid), .seq continuation :: frames⟩)
          ?_ (.cons ?_ path)), final, by simpa [firstTrapSite, spec] using obs, .step spec run⟩
        · change Exec.onRes _ _ _ _ _ _ = _
          rw [tick]
          change ((Machine slice owners ds).life (.bind x) (store mid)).map _ = _
          exact congrArg (Option.map (fun s => Exec.Out.next (O := Machine slice owners ds)
            ⟨.done, (x, v) :: locals cs, s, .seq continuation :: frames⟩)) life
        · exact congrArg (fun vs => some (Exec.Out.next (O := Machine slice owners ds)
            ⟨.exec continuation, (x, v) :: vs, store (c :: mid), frames⟩)) localsEq.symm

private theorem view_program (v : AdmissionView) :
    v.program = ⟨List.map Prod.snd v.located.decls,
      v.located.bindings.map (StraightBinding.toCore ∘ LocatedStraightBinding.statement),
      v.located.ending.toCore v.located.endingOrigin.1⟩ := by
  cases v with
  | zero p => simp [AdmissionView.program, Slice0.toCore, EarlyProgram.eraseLocations, AdmissionView.located,
      StraightProgram.decls, StraightEnd.toCore, StraightBinding.toCore, Function.comp_def]
  | one p => cases h : p.ending <;> simp [AdmissionView.program, AdmissionView.located, h,
      StraightEnd.toCore, StraightProgram.endingOrigin] <;> rfl

private theorem view_agrees (v : AdmissionView) (decls : DeclsOk v.program.decls)
    (body : BodyOkWith ArithmeticPolicy.runtime.permits v.program.decls [] v.program.body v.program.ending) :
    ∃ q f, Concrete.Contracts.Steps (Next v.execution.body) v.execution.initial q ∧ q.final? = some f ∧
      Matches (firstTrapSite v.program.decls [] 0 v.program.body) (ConcreteSpec.run v.program (v.program.body.length + 1)) f := by
  let p := v.located
  have bound {rows : List LocatedStraightBinding} {n} {b} (member : b ∈ rows) :
      b.statement.bound ≤ rows.foldr (fun k m => max k.statement.bound m) n := by
    induction rows with
    | nil => cases member
    | cons k ks ih =>
      rcases List.mem_cons.mp member with rfl | hm
      · exact Nat.le_max_left ..
      · exact Nat.le_trans (ih hm) (Nat.le_max_right ..)
  rw [view_program] at body decls
  obtain ⟨q, f, o, path, final, matchValue, spec⟩ := body_agrees v.execution.body (decl_model p decls)
    p.ending p.endingOrigin.1 p.bindings 0 [] (by intro _ h; cases h) (fun _ h => bound h)
    (by intro _ h; cases h) (fun _ _ => rfl) body
  refine ⟨q, f, path, final, ?_⟩
  rw [view_program]
  have h := exec_complete spec
  rw [← h] at matchValue
  exact matchValue

/-- A straight-line outcome as a process observation. Overflow acquires its
actual Core operation's source location; explicit abort already has a site. -/
def Observable {language : CoreLanguage} (i : CoreInput language) (program : ConcreteSpec.Program) :
    Outcome → Concrete.Contracts.Observation → Prop
  | .exit n, o => o = .returned (UInt8.ofNat n) ByteArray.empty
  | .trap .arithmeticOverflow, o => (firstTrapSite program.decls [] 0 program.body).bind
      (fun site => CoreInput.observeHosted i (.trapped 134 .overflow site)) = some o
  | .aborted site, o => CoreInput.observeHosted i (.trapped 134 .abort site) = some o
  | _, _ => False

private theorem located_agreement (v : AdmissionView) (project : Project)
    (i := (⟨project, v.slice, .straightLine v rfl⟩ : CoreInput (sliceLanguage v.slice)))
    (same : i = ⟨project, v.slice, .straightLine v rfl⟩)
    (wf : i.WF) (decls : DeclsOk v.program.decls)
    (body : BodyOkWith ArithmeticPolicy.runtime.permits v.program.decls [] v.program.body v.program.ending) :
    ProgramSafe CoreInput.Execution i ∧ ∃ o,
      Observable i v.program (ConcreteSpec.run v.program (v.program.body.length + 1)) o ∧
      ∀ result, CoreInput.Execution.observes i result ↔ result = o := by
  subst i
  let core : CoreInput (sliceLanguage v.slice) := ⟨project, v.slice, .straightLine v rfl⟩
  obtain ⟨q, f, path, final, matched⟩ := view_agrees v decls body
  have toExec {a b} (steps : Concrete.Contracts.Steps (Next v.execution.body) a b) :
      Core.Exec.Steps v.execution.body a [] b := by
    induction steps with
    | nil => exact .refl _
    | cons tick _ ih => exact .step (.silent tick) ih
  have reached := toExec path
  have trap {k site} (trapped : q.final? = some (.trapped k site)) :
      ∃ o, CoreInput.observeHosted core (.trapped 134 k site) = some o := by
    obtain ⟨org, found⟩ := wf.1.2.2.2.2.1 q [] k site reached trapped
    obtain ⟨_, span, file, fileAt, _⟩ := wf.1.2.2.2.2.2 site org (AList.lookup_mem found)
    change AList.lookup core.machineProgram.origins site = some org at found
    change core.project.files[org.lexical.file.index]? = some file at fileAt
    exact ⟨.trapped 134 k ⟨⟨site.id⟩, ⟨file.path.render.text, org.lexical.line, org.lexical.col⟩⟩
      (coreTrapDiagnostic core k site), by simp [CoreInput.observeHosted, found, fileAt]⟩
  have finish {f : Core.Exec.Final EarlyValue} {o : Outcome} (m : Matches (firstTrapSite v.program.decls [] 0 v.program.body) o f) (fin : q.final? = some f) :
      ProgramSafe CoreInput.Execution core ∧ ∃ obs, Observable core v.program o obs ∧
        ∀ result, CoreInput.Execution.observes core result ↔ result = obs := by
    cases m with
    | returned range =>
      rename_i n
      have bounds : 0 ≤ n ∧ n < 256 := by
        have h : 0 ≤ n ∧ n ≤ 255 := of_decide_eq_true range
        omega
      have hosted : (sliceLanguage v.slice).hostedResult (.returned (.scalar (.int .u8 n))) =
          some (.returned (UInt8.ofNat n.toNat)) := by
        simp [CoreLanguage.hostedResult, bounds, Observe.Observation.status]
      obtain ⟨safe, observed⟩ := CoreInput.safe_of_silent_path (core := core) path fin hosted rfl
      exact ⟨safe, _, rfl, observed⟩
    | overflow located =>
      obtain ⟨obs, render⟩ := trap fin
      obtain ⟨safe, observed⟩ := CoreInput.safe_of_silent_path (core := core) path fin rfl render
      exact ⟨safe, obs, by simpa [Observable, located] using render, observed⟩
    | abort site =>
      obtain ⟨obs, render⟩ := trap fin
      obtain ⟨safe, observed⟩ := CoreInput.safe_of_silent_path (core := core) path fin rfl render
      exact ⟨safe, obs, render, observed⟩
  exact finish matched final

/-- Every eligible Core has a unique hosted observation, agrees with the
straight-line evaluator at sufficient fuel, and has no reachable stuck state. -/
theorem unchecked_agreement {language : CoreLanguage} {i : CoreInput language} (eligible : i.UncheckedWF)
    (v : AdmissionView) (represents : v.Represents i) :
    ProgramSafe CoreInput.Execution i ∧ ∃ o,
      Observable i v.program (ConcreteSpec.run v.program (v.program.body.length + 1)) o ∧
      ∀ result, CoreInput.Execution.observes i result ↔ result = o := by
  obtain ⟨wf, ⟨t, ht, decls, _⟩, w, hw, body⟩ := eligible
  cases t.unique ht represents
  cases w.unique hw represents
  obtain ⟨project, slice, payload⟩ := i
  obtain ⟨rfl, rfl, rfl⟩ := represents
  exact located_agreement _ project _ rfl wf decls body

theorem unchecked_programSafe {language : CoreLanguage} {i : CoreInput language} (eligible : i.UncheckedWF) :
    ProgramSafe CoreInput.Execution i :=
  let ⟨v, represented, _⟩ := eligible.2.2
  (unchecked_agreement eligible v represented).1

/-- Unchecked lowering gives target safety and observable-result preservation
without a separate source-safety hypothesis. -/
theorem lowered_observations {language : CoreLanguage} {lang : LoweringLanguage language.operands.Ty}
    {profile : Profile} {i : CoreInput language} {mir : MIR lang profile} (lowered : LowersUnchecked i mir) :
    ProgramSafe (MIR.Execution (language := language)) mir ∧
      ∀ o, (MIR.Execution (language := language)).observes mir o → CoreInput.Execution.observes i o := by
  obtain ⟨eligible, _, _, _, _, _, preserves⟩ := lowered
  obtain ⟨finite, _, safe⟩ := preserves (unchecked_programSafe eligible)
  refine ⟨safe, ?_⟩
  rintro o ⟨q, reached, result⟩
  obtain ⟨p, path, same, _⟩ := finite q o reached result
  exact ⟨p, path, same⟩

private theorem checked_terminates_no_overflow (v : AdmissionView) (checked : Checked v.program) :
    ∃ q f, Exec.Steps v.execution.body v.execution.initial [] q ∧
      q.final? = some f ∧ ∀ s, f ≠ .trapped .overflow s := by
  obtain ⟨q, f, path, final, matched⟩ := view_agrees v checked.1 checked.2.runtime
  have toExec {a b} (steps : Concrete.Contracts.Steps (Next v.execution.body) a b) :
      Core.Exec.Steps v.execution.body a [] b := by
    induction steps with
    | nil => exact .refl _
    | cons tick _ ih => exact .step (.silent tick) ih
  refine ⟨q, f, toExec path, final, ?_⟩
  intro s heq
  subst heq
  generalize ho : ConcreteSpec.run v.program (v.program.body.length + 1) = o at matched
  cases matched with
  | overflow _ =>
    exact checked_no_implicit_traps checked (v.program.body.length + 1) .arithmeticOverflow ho

/-- Every encoded discharge subset of an admitted early-fragment Core input
satisfies `ValidDischarges`, for any `CoreLanguage` index. -/
theorem validDischarges_encoded {language : CoreLanguage} (input : CoreInput language)
    (batch : ConcreteSpec.Admission.InputBatch input)
    (selected : List ConcreteSpec.Admission.Discharge)
    (out : StageInterface.Discharges language)
    (_wf : input.WF) (encoded : batch.encode selected = some out) :
    ValidDischarges input out :=
  DischargeTransfer.validDischarges_of_terminates checked_terminates_no_overflow
    input batch selected out encoded

/-- Fix (c)/(e) discharge transfer: every encoded discharge subset issued by
straight-line admission holds at every matching reachable integrated machine state. -/
theorem dischargeTransfer : ConcreteSpec.Admission.DischargeTransfer :=
  fun input batch selected out wf encoded =>
    validDischarges_encoded input batch selected out wf encoded

/-- `ConcreteSpec.Admission.checkInput` is sound on every well-located
early-fragment Core input: acceptance matches success, issued discharges
transfer to `ValidDischarges`, and every refusal is a rendered `CheckerRefuses`. -/
theorem admissionSound {language : CoreLanguage} :
    AdmissionSound (language := language) ConcreteSpec.Admission.checkInput :=
  DischargeTransfer.admissionSound_of_transfer (fun input batch selected out wf encoded =>
    validDischarges_encoded input batch selected out wf encoded)

end Concrete.IntegratedCore.Agreement

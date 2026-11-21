import ConcreteSpec.Proofs.Own
import ConcreteSpec.Proofs.OwnProtocols
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Own
import ConcreteSpec.Spec.Scalar

/-! # Exact ownership transitions

Authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal, #od15, #od23,
#od60 and #od69. The relations describe successful ledger changes. The Laws identify
both the full domain and the exact successor of the executable operations.
-/

namespace ConcreteSpec.Core.Own
namespace LegalStep

def Reinitialize (s : State) (p : Hybrid.Place) (t : State) : Prop :=
  ∃ a, slot s p = some a ∧
    (a.status = .moved ∨ (a.desc.copy = true ∧ a.desc.mutable = true)) ∧
    (∀ l ∈ s.loans, l.root ≠ p) ∧
    t = { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .owned } else x }

def Bind (types : Var → Option Descriptor) (s : State) (x : Var) (t : State) : Prop :=
  ∃ f, s.frames.head? = some f ∧
    match AList.lookup f.locals x with
    | some p => Reinitialize s p t
    | none =>
      ∃ d, types x = some d ∧ f.id < s.nextActivation ∧
        t = { s with
          nextPlace := s.nextPlace + 1
          slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots
          frames := { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } :: s.frames.tail }

def Drop (s : State) (xs : List Var) (t : State) : Prop :=
  ∃ f, AdmittedDrop s xs f ∧
    t = { s with
      slots := s.slots.filter fun x => decide (x.place ∉ departingPlaces f xs)
      frames := { f with locals := f.locals.filter (fun e =>
        decide (e.2 ∉ departingPlaces f xs)) } :: s.frames.tail }

def AccessOwner (s : State) (p : Hybrid.Place) (m : Mode) (t : State) : Prop :=
  OwnerPermission s p m ∧ t = s

def AccessLoan (s : State) (i : LoanId) (m : Mode) (t : State) : Prop :=
  LoanPermission s i m ∧ t = s

/-- Law (authority: docs/decisions.md#od15). Owner access is a permitted identity step. -/
theorem accessOwner_iff (s : State) (p : Hybrid.Place) (m : Mode) (t : State) :
    accessOwner s p m = some t ↔ AccessOwner s p m t := by
  simp [accessOwner, AccessOwner, ownerAccess_iff, eq_comm (a := t)]

/-- Law (authority: docs/decisions.md#od15). Loan access is a permitted identity step. -/
theorem accessLoan_iff (s : State) (i : LoanId) (m : Mode) (t : State) :
    accessLoan s i m = some t ↔ AccessLoan s i m t := by
  simp [accessLoan, AccessLoan, loanAccess_iff, eq_comm (a := t)]

def Move (s : State) (p : Hybrid.Place) (t : State) : Prop :=
  AdmittedMove s p ∧ t = { s with slots := s.slots.map fun x =>
    if x.place = p then { x with status := .moved } else x }

/-- Law (authority: docs/decisions.md#od69). Reinitialization has an exact legal successor. -/
theorem reinitialize_iff (s : State) (p : Hybrid.Place) (t : State) :
    reinitialize s p = some t ↔ Reinitialize s p t := by
  cases h : slot s p <;>
    simp [reinitialize, Reinitialize, h, Bool.or_eq_true, Bool.and_eq_true,
      List.all_eq_true, bne_iff_ne, eq_comm (a := t), and_assoc]

/-- Law (authority: docs/decisions.md#od69). Binding uses checked descriptors and fresh places. -/
theorem bind_iff (types : Var → Option Descriptor) (s : State) (x : Var) (t : State) :
    bind types x s = some t ↔ Bind types s x t := by
  cases hf : s.frames.head? with
  | none => simp [bind, Bind, hf]
  | some f =>
    cases hp : AList.lookup f.locals x with
    | some p => simpa [bind, Bind, hf, hp] using reinitialize_iff s p t
    | none =>
      cases hd : types x <;> simp [bind, Bind, hf, hp, hd, eq_comm (a := t)]

/-- Law (authority: docs/decisions.md#od69). Drop removes precisely the discharged storage. -/
theorem drop_iff (s : State) (xs : List Var) (t : State) :
    drop xs s = some t ↔ Drop s xs t := by
  cases h : s.frames.head? with
  | none => simp [drop, Drop, AdmittedDrop, h]
  | some f =>
    have discharge (a : Slot) :
        (a.place ∉ departingPlaces f xs ∨ a.desc.copy = true ∨ a.status = .moved) ↔
        (a.place ∈ departingPlaces f xs → a.desc.copy = true ∨ a.status = .moved) := by
      by_cases ha : a.place ∈ departingPlaces f xs <;> simp [ha]
    have guard :
        (s.slots.all (fun a => decide (a.place ∉ departingPlaces f xs) ||
            a.desc.copy || a.status == .moved) &&
          s.loans.all (fun l => decide (l.root ∉ departingPlaces f xs))) = true ↔
        (∀ a ∈ s.slots, a.place ∈ departingPlaces f xs →
            a.desc.copy = true ∨ a.status = .moved) ∧
          (∀ l ∈ s.loans, l.root ∉ departingPlaces f xs) := by
      simp only [Bool.and_eq_true, List.all_eq_true, Bool.or_eq_true, decide_eq_true_eq,
        beq_iff_eq, or_assoc, discharge]
    simp only [drop, Drop, AdmittedDrop, h, Option.some.injEq]
    change (if _ then _ else _) = some t ↔ _
    unfold departingPlaces at guard ⊢
    split <;> rename_i condition
    · have admitted := guard.mp condition
      constructor
      · intro h
        exact ⟨f, ⟨rfl, admitted.1, admitted.2⟩, (Option.some.inj h).symm⟩
      · rintro ⟨_, ⟨rfl, _, _⟩, rfl⟩
        rfl
    · have refused := mt guard.mpr condition
      constructor
      · intro h
        cases h
      · rintro ⟨_, ⟨rfl, discharged, clear⟩, _⟩
        exact (refused ⟨discharged, clear⟩).elim

/-- Law (authority: docs/decisions.md#od60; #od62). Move consumes only an unborrowed linear owner. -/
theorem move_iff (s : State) (p : Hybrid.Place) (t : State) :
    move s p = some t ↔ Move s p t := by
  cases h : slot s p <;>
    simp [move, Move, AdmittedMove, h, Bool.or_eq_true, List.any_eq_true, and_assoc,
      eq_comm (a := t)]

/-- A sequence records each intermediate ledger in evaluation order. -/
inductive Trace {α β : Type} (rule : α → β → α → Prop) : α → List β → α → Prop
  | nil (s) : Trace rule s [] s
  | cons {s u t : α} {e : β} {es : List β} : rule s e u → Trace rule u es t → Trace rule s (e :: es) t

/-- Helper (induction). A fold implements exactly the relational composition of its steps. -/
theorem fold_iff {α β : Type} {rule : α → β → α → Prop} (step : α → β → Option α)
    (law : ∀ s e t, step s e = some t ↔ rule s e t)
    (s : α) (es : List β) (t : α) :
    es.foldlM step s = some t ↔ Trace rule s es t := by
  induction es generalizing s with
  | nil =>
    constructor
    · intro h
      cases h
      exact .nil _
    · intro h
      cases h
      rfl
  | cons e es ih =>
    change (Option.bind (step s e) (fun u => es.foldlM step u)) = some t ↔ _
    simp only [Option.bind_eq_some_iff, law, ih]
    constructor
    · rintro ⟨u, hu, ht⟩
      exact .cons hu ht
    · intro h
      cases h with
      | cons hu ht => exact ⟨_, hu, ht⟩

def LoanSequence := Trace LoanStep

/-- Law (authority: docs/decisions.md#od15). Every successful sequence is a legal trace. -/
theorem runLoanSequence_iff (s : State) (es : List LoanAction) (t : State) :
    runLoanSequence s es = some t ↔ LoanSequence s es t :=
  fold_iff loanAction loanAction_iff s es t

def pushFrame (s : State) : State :=
  { s with nextActivation := s.nextActivation + 1
           frames := ⟨s.nextActivation, [], []⟩ :: s.frames }

def Enter (types : Var → Option Descriptor) (s : State) (xs : List Var) (t : State) : Prop :=
  Trace (Bind types) (pushFrame s) xs t

def EndLocals (s : State) (activation : Nat) (t : State) : Prop :=
  Trace (fun s l => EndLoan s l.id) s
    (s.loans.filter fun l => decide (l.activation = activation)) t

/-- Law (authority: docs/decisions.md#od23). Local ending processes loans newest first. -/
theorem endLocals_iff (s : State) (activation : Nat) (t : State) :
    endLocals activation s = some t ↔ EndLocals s activation t := by
  simpa [EndLocals, endLocals, Bool.beq_eq_decide_eq] using
    fold_iff (fun s (l : Loan) => endLoan s l.id) (fun s l t => endLoan_iff s l.id t)
      s (s.loans.filter (·.activation == activation)) t

def Leave (s t : State) : Prop :=
  ∃ f u v, s.frames.head? = some f ∧ EndLocals s f.id u ∧
    Drop u (List.map Prod.fst f.locals) v ∧ t = { v with frames := v.frames.tail }

def Life (types : Var → Option Descriptor) (s : State) (e : Exec.Life) (t : State) : Prop :=
  match e with
  | .bind x => Bind types s x t
  | .enter xs => Enter types s xs t
  | .leave => Leave s t
  | .drop xs => Drop s xs t

/-- Law (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal). Entry types each binding. -/
theorem enter_iff (types : Var → Option Descriptor) (s : State) (xs : List Var) (t : State) :
    enter types xs s = some t ↔ Enter types s xs t :=
  fold_iff (fun s x => bind types x s) (bind_iff types) (pushFrame s) xs t

/-- Law (authority: docs/decisions.md#od69). Exit ends local loans and discharges local storage. -/
theorem leave_iff (s t : State) : leave s = some t ↔ Leave s t := by
  cases h : s.frames.head? with
  | none => simp [leave, Leave, h]
  | some f =>
    simp [leave, Leave, h, Option.bind_eq_some_iff, endLocals_iff, drop_iff,
      eq_comm (a := t), exists_and_left]

/-- Law (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal). Lifecycle exactness. -/
theorem life_iff (types : Var → Option Descriptor) (s : State) (e : Exec.Life) (t : State) :
    life types e s = some t ↔ Life types s e t := by
  cases e <;> first | apply bind_iff | apply enter_iff | apply leave_iff | apply drop_iff

def LifeStore {σ : Type} (types : Var → Option Descriptor) (s : Store σ) (e : Exec.Life)
    (t : Store σ) : Prop :=
  ∃ u, Life types s.own e u ∧ t = ⟨s.concrete, u⟩

/-- Law (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal). Store lifecycle exactness. -/
theorem lifeStore_iff {σ : Type} (types : Var → Option Descriptor) (s : Store σ)
    (e : Exec.Life) (t : Store σ) : lifeStore types e s = some t ↔ LifeStore types s e t := by
  simp [lifeStore, LifeStore, Option.map_eq_some_iff, life_iff, eq_comm (a := t)]

def InstrumentLife {σ : Type} (concrete : Exec.Life → σ → σ → Prop)
    (types : Var → Option Descriptor) (s : Store σ) (e : Exec.Life) (t : Store σ) : Prop :=
  Life types s.own e t.own ∧ concrete e s.concrete t.concrete

/-- Law (authority: docs/decisions.md#decided-2026-11-13-lifecycle-refusal).
An instrumented lifecycle realizes the ownership and concrete rules together. -/
theorem instrumentLife_iff (O : Exec.Operands)
    (concrete : Exec.Life → O.Store → O.Store → Prop)
    (concreteLaw : ∀ e s t, O.life e s = some t ↔ concrete e s t)
    (types : Var → Option Descriptor) (s : Store O.Store) (e : Exec.Life) (t : Store O.Store) :
    instrumentLife O types e s = some t ↔ InstrumentLife concrete types s e t := by
  cases t
  simp [instrumentLife, InstrumentLife, Option.bind_eq_some_iff, life_iff, concreteLaw]
  constructor
  · rintro ⟨_, ownershipStep, _, concreteStep, rfl, rfl⟩
    exact ⟨ownershipStep, concreteStep⟩
  · rintro ⟨ownershipStep, concreteStep⟩
    exact ⟨_, ownershipStep, _, concreteStep, rfl, rfl⟩

def AdoptDown (s : State) (args : List (Var × LoanId)) (t : State) : Prop :=
  ∃ f, s.frames.head? = some f ∧ f.id < s.nextActivation ∧
    (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
    (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
    (∀ e ∈ args, AList.lookup f.locals e.1 = none) ∧
    t = { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }

/-- Law (authority: docs/decisions.md#od23). Adoption transfers exactly the pending grants. -/
theorem adoptDown_iff (s : State) (args : List (Var × LoanId)) (t : State) :
    adoptDown s args = some t ↔ AdoptDown s args t := by
  cases h : s.frames.head? <;>
    simp [adoptDown, AdoptDown, h, and_assoc, eq_comm (a := t)]

def adaptLife (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life)
    (t : CallState) : Prop :=
  s.pending = [] ∧ ∃ u, Life types s.own e u ∧ t = { s with own := u }

def CallLife (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life)
    (t : CallState) : Prop :=
  match e with
  | .enter xs =>
    (∀ e ∈ s.pending, e.1 ∈ xs) ∧ ∃ u v,
      Enter types s.own (xs.filter fun x => decide (x ∉ s.pending.map Prod.fst)) u ∧
      AdoptDown u s.pending v ∧ t = ⟨v, []⟩
  | .bind x => adaptLife types s (.bind x) t
  | .drop xs => adaptLife types s (.drop xs) t
  | .leave => adaptLife types s .leave t

/-- Law (authority: docs/decisions.md#od23). Pending arguments cross only matching entry. -/
theorem callLife_iff (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life)
    (t : CallState) : callLife types e s = some t ↔ CallLife types s e t := by
  cases e with
  | enter xs =>
    simp [callLife, CallLife, List.all_eq_true, Option.bind_eq_some_iff, enter_iff, adoptDown_iff,
      eq_comm (a := t)]
  | bind x =>
    simp [callLife, CallLife, adaptLife, Option.map_eq_some_iff, life_iff, eq_comm (a := t)]
  | drop xs =>
    simp [callLife, CallLife, adaptLife, Option.map_eq_some_iff, life_iff, eq_comm (a := t)]
  | leave =>
    simp [callLife, CallLife, adaptLife, Option.map_eq_some_iff, life_iff, eq_comm (a := t)]

def CallLifeStore {σ : Type} (types : Var → Option Descriptor) (s : σ × CallState)
    (e : Exec.Life) (t : σ × CallState) : Prop :=
  ∃ u, CallLife types s.2 e u ∧ t = (s.1, u)

/-- Law (authority: docs/decisions.md#od23). The call adapter preserves the concrete store. -/
theorem callLifeStore_iff {σ : Type} (types : Var → Option Descriptor) (s : σ × CallState)
    (e : Exec.Life) (t : σ × CallState) :
    callLifeStore types e s = some t ↔ CallLifeStore types s e t := by
  simp [callLifeStore, CallLifeStore, Option.map_eq_some_iff, callLife_iff, eq_comm (a := t)]

def DownwardParameter (s : State) (e : Var × LoanId) (t : State) : Prop :=
  ∃ a f u, loan s e.2 = some a ∧ s.frames.head? = some f ∧
    ReborrowTo s e.2 [] a.mode f.id u ∧
    t = { u with frames := { f with views := (e.1, ⟨s.nextLoan⟩) :: f.views } :: s.frames.tail }

def EnterDown (s : State) (args : List (Var × LoanId)) (t : State) : Prop :=
  Trace DownwardParameter (pushFrame s) args t

/-- Law (authority: docs/decisions.md#od23). Each downward grant sees the ledger of earlier grants. -/
theorem enterDown_iff (s : State) (args : List (Var × LoanId)) (t : State) :
    enterDown s args = some t ↔ EnterDown s args t := by
  change args.foldlM _ (pushFrame s) = some t ↔ _
  apply fold_iff
  intro v e u
  simp [DownwardParameter, Option.bind_eq_some_iff, reborrowTo_iff, eq_comm (a := u),
    exists_and_left]

def PassDown (s : CallState) (i : LoanId) (x : Var) (t : CallState) : Prop :=
  x ∉ s.pending.map Prod.fst ∧ ∃ a f,
    loan s.own i = some a ∧ LoanPermission s.own i a.mode ∧
    s.own.frames.head? = some f ∧ f.id < s.own.nextActivation ∧
    t = ⟨{ s.own with
      nextLoan := s.own.nextLoan + 1
      loans := ⟨⟨s.own.nextLoan⟩, a.root, a.path, a.mode, some i, i :: a.ancestors,
        f.id, true, false⟩ :: s.own.loans },
      s.pending ++ [(x, ⟨s.own.nextLoan⟩)]⟩

/-- Law (authority: docs/decisions.md#od23). A view argument grants a fresh pending child. -/
theorem passDown_iff (s : CallState) (i : LoanId) (x : Var) (t : CallState) :
    passDown s i x = some t ↔ PassDown s i x t := by
  cases h : s.own.frames.head? <;> cases ha : loan s.own i <;>
    simp [passDown, PassDown, h, ha, Bool.and_eq_true, loanAccess_iff,
      eq_comm (a := t), and_assoc]

def PassOwner (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (x : Var) (t : CallState) : Prop :=
  x ∉ s.pending.map Prod.fst ∧ OwnerPermission s.own p m ∧
    ∃ f, s.own.frames.head? = some f ∧ f.id < s.own.nextActivation ∧
      t = ⟨{ s.own with
        nextLoan := s.own.nextLoan + 1
        loans := ⟨⟨s.own.nextLoan⟩, p, path, m, none, [], f.id, true, true⟩ :: s.own.loans },
        s.pending ++ [(x, ⟨s.own.nextLoan⟩)]⟩

/-- Law (authority: docs/decisions.md#od23). The owner argument opens a pending whole-root grant. -/
theorem passOwner_iff (s : CallState) (p : Hybrid.Place) (path : List Projection) (m : Mode)
    (x : Var) (t : CallState) : passOwner s p path m x = some t ↔ PassOwner s p path m x t := by
  cases h : s.own.frames.head? <;>
    simp [passOwner, PassOwner, h, Bool.and_eq_true, ownerAccess_iff, and_assoc, eq_comm (a := t)]

def Issuance (types : Var → Option Descriptor) (s : CallState) (e : IssuanceAction)
    (t : CallState) : Prop :=
  match e with
  | .ordinary op => OrdinaryIssuanceAllowed s op ∧ ∃ u, LoanStep s.own op u ∧ t = { s with own := u }
  | .owner p path m x => PassOwner s p path m x t
  | .view i x => PassDown s i x t
  | .entry args => ∃ u, EnterDown s.own args u ∧ t = { s with own := u }
  | .lifecycle e => CallLife types s e t

/-- Law (authority: docs/decisions.md#od15; #od23). Issuance realizes each ledger rule. -/
theorem issuanceStep_iff (types : Var → Option Descriptor) (s : CallState)
    (e : IssuanceAction) (t : CallState) : issuanceStep types s e = some t ↔ Issuance types s e t := by
  cases e with
  | ordinary op =>
    by_cases h : OrdinaryIssuanceAllowed s op <;>
      simp [issuanceStep, Issuance, h, Option.map_eq_some_iff, loanAction_iff, eq_comm (a := t)]
  | owner | view | entry | lifecycle =>
    simp [issuanceStep, Issuance, passOwner_iff, passDown_iff, callLife_iff, Option.map_eq_some_iff,
      enterDown_iff, eq_comm (a := t)]

/-- Law (authority: docs/decisions.md#od15; #od23). Issuance traces compose legal steps. -/
theorem issuanceTrace_iff (types : Var → Option Descriptor) (s : CallState)
    (es : List IssuanceAction) (t : CallState) :
    es.foldlM (issuanceStep types) s = some t ↔ Trace (Issuance types) s es t :=
  fold_iff (issuanceStep types) (issuanceStep_iff types) s es t

private def UniqueStep {α β : Type} (rule : α → β → α → Prop) : Prop :=
  ∀ ⦃s e u v⦄, rule s e u → rule s e v → u = v

private theorem trace_unique {α β : Type} {rule : α → β → α → Prop}
    (unique : UniqueStep rule) : UniqueStep (Trace rule) := by
  intro s es u v hu
  induction hu generalizing v with
  | nil => intro hv; cases hv; rfl
  | cons step tail ih =>
    intro hv
    cases hv with
    | cons step' tail' =>
      have same := unique step step'
      cases same
      exact ih tail'

private theorem reborrowTo_unique {s : State} {i : LoanId} {path : List Projection}
    {m : Mode} {a : Nat} {u v : State}
    (hu : ReborrowTo s i path m a u) (hv : ReborrowTo s i path m a v) : u = v := by
  unfold ReborrowTo at hu hv
  grind

private theorem loanStep_unique : UniqueStep LoanStep := by
  intro s e u v hu hv
  cases e with
  | reborrow => exact reborrowTo_unique hu hv
  | borrow => unfold LoanStep Borrow at hu hv; grind
  | endLoan => unfold LoanStep EndLoan at hu hv; grind

private theorem bind_unique (types : Var → Option Descriptor) : UniqueStep (Bind types) := by
  intro s x u v hu hv
  obtain ⟨f, hf, hu⟩ := hu
  obtain ⟨g, hg, hv⟩ := hv
  have same := Option.some.inj (hf.symm.trans hg)
  subst g
  cases hp : AList.lookup f.locals x with
  | none => simp only [hp] at hu hv; grind
  | some p => simp only [hp, Reinitialize] at hu hv; grind

private theorem drop_unique {s : State} {xs : List Var} {u v : State}
    (hu : Drop s xs u) (hv : Drop s xs v) : u = v := by
  unfold Drop AdmittedDrop at hu hv
  grind

private theorem life_unique (types : Var → Option Descriptor) : UniqueStep (Life types) := by
  intro s e u v hu hv
  cases e with
  | bind => exact bind_unique types hu hv
  | drop => exact drop_unique hu hv
  | enter => exact trace_unique (bind_unique types) hu hv
  | leave =>
    obtain ⟨f, a, b, hf, ha, hb, rfl⟩ := hu
    obtain ⟨g, c, d, hg, hc, hd, rfl⟩ := hv
    have same := Option.some.inj (hf.symm.trans hg)
    subst g
    have endUnique : UniqueStep (fun s (l : Loan) t => EndLoan s l.id t) := by
      intro s l u v hu hv
      exact loanStep_unique (e := .endLoan l.id) hu hv
    have same := trace_unique endUnique ha hc
    subst c
    rw [drop_unique hb hd]

private theorem callLife_unique (types : Var → Option Descriptor) : UniqueStep (CallLife types) := by
  intro s e u v hu hv
  cases e with
  | enter =>
    obtain ⟨_, a, b, ha, hb, rfl⟩ := hu
    obtain ⟨_, c, d, hc, hd, rfl⟩ := hv
    have same := trace_unique (bind_unique types) ha hc
    subst c
    unfold AdoptDown at hb hd
    grind
  | bind | leave | drop =>
    obtain ⟨_, a, ha, rfl⟩ := hu
    obtain ⟨_, b, hb, rfl⟩ := hv
    rw [life_unique types ha hb]

private theorem downwardParameter_unique : UniqueStep DownwardParameter := by
  intro s e u v hu hv
  obtain ⟨a, f, b, ha, hf, hb, rfl⟩ := hu
  obtain ⟨c, g, d, hc, hg, hd, rfl⟩ := hv
  have same := Option.some.inj (ha.symm.trans hc)
  subst c
  have same := Option.some.inj (hf.symm.trans hg)
  subst g
  rw [reborrowTo_unique hb hd]

/-- Law (authority: docs/decisions.md#od15; #od23). The issuance clauses determine at most one
successor by inversion, including the intermediate ledgers of downward entry. -/
theorem issuance_deterministic (types : Var → Option Descriptor) {s u v : CallState}
    {e : IssuanceAction} (hu : Issuance types s e u) (hv : Issuance types s e v) : u = v := by
  cases e with
  | ordinary =>
    obtain ⟨_, a, ha, rfl⟩ := hu
    obtain ⟨_, b, hb, rfl⟩ := hv
    rw [loanStep_unique ha hb]
  | owner => unfold Issuance PassOwner at hu hv; grind
  | view => unfold Issuance PassDown at hu hv; grind
  | entry =>
    obtain ⟨a, ha, rfl⟩ := hu
    obtain ⟨b, hb, rfl⟩ := hv
    rw [trace_unique downwardParameter_unique ha hb]
  | lifecycle => exact callLife_unique types hu hv

/-- Law (authority: docs/decisions.md#od15; #od23). A legal issuance trace has at most one successor. -/
theorem issuanceTrace_deterministic (types : Var → Option Descriptor) :
    UniqueStep (Trace (Issuance types)) :=
  trace_unique (by intro s e u v hu hv; exact issuance_deterministic types hu hv)

/-- Admission is existence of a legal successor. -/
def Admitted {α β : Type} (step : α → β → α → Prop) (s : α) (e : β) : Prop :=
  ∃ t, step s e t

/-- Helper (enumeration). Every legal loan step is represented in the older history ledger. -/
theorem loanLedger_complete (s : State) (e : LoanAction) (t : State)
    (h : LoanStep s e t) : LoanLedger s e t := by
  cases e with
  | borrow p path m =>
    obtain ⟨allowed, f, frame, issued, rfl⟩ := h
    exact .borrow ⟨f, frame, issued, allowed⟩ frame
  | reborrow i path m a =>
    obtain ⟨allowed, issued, current, parent, found, rfl⟩ := h
    exact .reborrow ⟨parent, found, issued, current, allowed⟩ found
  | endLoan i =>
    obtain ⟨⟨a, ha⟩, clear, rfl⟩ := h
    exact .endLoan ⟨a, ha, clear⟩

/-- Helper (constructor enumeration). Every event names exactly its relational rule. -/
theorem admission_cases (types : Var → Option Descriptor) (s : CallState) :
    (∀ e, Admitted (Life types) s.own e ↔ match e with
      | .bind x => ∃ t, Bind types s.own x t
      | .enter xs => ∃ t, Enter types s.own xs t
      | .leave => ∃ t, Leave s.own t
      | .drop xs => ∃ t, Drop s.own xs t) ∧
    (∀ e, Admitted (CallLife types) s e ↔ match e with
      | .bind x => ∃ t, adaptLife types s (.bind x) t
      | .enter xs => (∀ e ∈ s.pending, e.1 ∈ xs) ∧ ∃ u v,
          Enter types s.own (xs.filter fun x => decide (x ∉ s.pending.map Prod.fst)) u ∧
          AdoptDown u s.pending v
      | .leave => ∃ t, adaptLife types s .leave t
      | .drop xs => ∃ t, adaptLife types s (.drop xs) t) ∧
    (∀ e, Admitted LoanStep s.own e ↔ match e with
      | .borrow p path m => ∃ t, Borrow s.own p path m t
      | .reborrow i path m a => ∃ t, ReborrowTo s.own i path m a t
      | .endLoan i => ∃ t, EndLoan s.own i t) ∧
    (∀ e, Admitted (Issuance types) s e ↔ match e with
      | .ordinary op => OrdinaryIssuanceAllowed s op ∧ Admitted LoanStep s.own op
      | .owner p path m x => ∃ t, PassOwner s p path m x t
      | .view i x => ∃ t, PassDown s i x t
      | .entry args => Admitted EnterDown s.own args
      | .lifecycle e => Admitted (CallLife types) s e) ∧
    (∀ e t, LoanStep s.own e t → LoanLedger s.own e t) := by
  refine ⟨?_, ?_, ?_, ?_, loanLedger_complete s.own⟩
  · intro e
    cases e <;> rfl
  · intro e
    cases e with
    | bind | leave | drop => rfl
    | enter xs =>
      constructor
      · rintro ⟨_, supplied, u, v, hu, hv, _⟩
        exact ⟨supplied, u, v, hu, hv⟩
      · rintro ⟨supplied, u, v, hu, hv⟩
        exact ⟨⟨v, []⟩, supplied, u, v, hu, hv, rfl⟩
  · intro e
    cases e <;> rfl
  · intro e
    cases e with
    | owner | view | lifecycle => rfl
    | ordinary =>
      constructor
      · rintro ⟨_, allowed, u, hu, _⟩
        exact ⟨allowed, u, hu⟩
      · rintro ⟨allowed, u, hu⟩
        exact ⟨{ s with own := u }, allowed, u, hu, rfl⟩
    | entry =>
      constructor
      · rintro ⟨_, u, hu, _⟩
        exact ⟨u, hu⟩
      · rintro ⟨u, hu⟩
        exact ⟨{ s with own := u }, u, hu, rfl⟩

/-- Helper (preservation). Legal lifecycle transitions preserve well-formed ownership. -/
theorem life_wellFormed {types : Var → Option Descriptor} {s t : State} {e : Exec.Life}
    (h : Life types s e t) (hs : WellFormed s) : WellFormed t := by
  have safe := life_preserves types e s hs
  rwa [(life_iff types s e t).mpr h] at safe

/-- Helper (preservation). Legal call lifecycle transitions preserve the call ledger. -/
theorem callLife_wellFormed {types : Var → Option Descriptor} {s t : CallState} {e : Exec.Life}
    (h : CallLife types s e t) (hs : s.WellFormed) : t.WellFormed := by
  have safe := callLife_call_preserves types e s hs
  rwa [(callLife_iff types s e t).mpr h] at safe

private def PendingConsistent (pending : List (Var × LoanId)) (s : State) : Prop :=
  (∀ e ∈ pending, ∃ l ∈ s.loans, l.id = e.2) ∧
  (∀ l ∈ s.loans, l.pendingArgument = true ↔ l.id ∈ pending.map Prod.snd)

private theorem pending_cons {pending : List (Var × LoanId)} {s t : State} {l : Loan}
    (consistent : PendingConsistent pending s) (loans : t.loans = l :: s.loans)
    (unflagged : l.pendingArgument = false) (fresh : l.id ∉ pending.map Prod.snd) :
    PendingConsistent pending t := by
  constructor
  · intro e he
    obtain ⟨b, hb, eq⟩ := consistent.1 e he
    exact ⟨b, loans ▸ List.mem_cons_of_mem l hb, eq⟩
  · intro b hb
    rw [loans] at hb
    rcases List.mem_cons.mp hb with rfl | hb
    · simp [unflagged, fresh]
    · exact consistent.2 b hb

private theorem nextLoan_not_pending {pending : List (Var × LoanId)} {s : State}
    (consistent : PendingConsistent pending s) (wf : WellFormed s) :
    LoanId.mk s.nextLoan ∉ pending.map Prod.snd := by
  intro h
  obtain ⟨e, he, eq⟩ := List.mem_map.mp h
  obtain ⟨l, hl, id⟩ := consistent.1 e he
  have older := (wf.2.2.2.1 l hl).1
  rw [id, eq] at older
  exact Nat.lt_irrefl _ older

private theorem ordinary_pending {s : CallState} {e : LoanAction} {t : State}
    (allowed : OrdinaryIssuanceAllowed s e) (step : LoanStep s.own e t)
    (wf : s.WellFormed) : PendingConsistent s.pending t := by
  have consistent : PendingConsistent s.pending s.own := wf.2.2.2
  have fresh := nextLoan_not_pending consistent wf.1
  cases e with
  | borrow =>
    obtain ⟨_, _, _, _, rfl⟩ := step
    exact pending_cons consistent rfl rfl fresh
  | reborrow =>
    obtain ⟨_, _, _, _, _, rfl⟩ := step
    exact pending_cons consistent rfl rfl fresh
  | endLoan i =>
    change i ∉ s.pending.map Prod.snd at allowed
    obtain ⟨_, _, rfl⟩ := step
    constructor
    · intro e he
      obtain ⟨l, hl, eq⟩ := consistent.1 e he
      refine ⟨l, List.mem_filter.mpr ⟨hl, ?_⟩, eq⟩
      simp only [bne_iff_ne]
      intro same
      exact allowed (List.mem_map.mpr ⟨e, he, eq.symm.trans same⟩)
    · intro l hl
      exact consistent.2 l (List.mem_filter.mp hl).1

private theorem downwardTrace_pending {pending : List (Var × LoanId)} {s t : State}
    {args : List (Var × LoanId)} (step : Trace DownwardParameter s args t)
    (initial : WellFormed s) (keep : PendingConsistent pending s) :
    PendingConsistent pending t := by
  induction step with
  | nil => exact keep
  | @cons s u₀ t e es grant _ ih =>
    obtain ⟨a, f, u, _, frame, reborrow, rfl⟩ := grant
    have preserve := loanStep_wellFormed (e := .reborrow _ [] a.mode f.id) reborrow initial
    have frames : u.frames = s.frames := by
      obtain ⟨_, _, _, _, _, rfl⟩ := reborrow
      rfl
    have next := set_views_preserves u f ((e.1, ⟨s.nextLoan⟩) :: f.views) preserve (by simpa [frames] using frame)
    have fresh := nextLoan_not_pending keep initial
    obtain ⟨_, _, _, _, _, rfl⟩ := reborrow
    exact ih next (pending_cons keep rfl rfl fresh)

/-- Law (authority: docs/decisions.md#od15; #od23). Each legal issuance step preserves the
ownership ledger and its pending call buffer together. -/
theorem issuance_wellFormed (types : Var → Option Descriptor) {s t : CallState}
    {e : IssuanceAction} (step : Issuance types s e t) (wf : s.WellFormed) : t.WellFormed := by
  cases e with
  | ordinary op =>
    obtain ⟨allowed, u, hu, rfl⟩ := step
    exact ⟨loanStep_wellFormed hu wf.1, wf.2.1, wf.2.2.1, ordinary_pending (s := s) allowed hu wf⟩
  | entry args =>
    obtain ⟨u, hu, rfl⟩ := step
    have safe := enterDown_preserves s.own args wf.1
    rw [(enterDown_iff ..).mpr hu] at safe
    have initial : WellFormed (pushFrame s.own) := enter_preserves (fun _ => none) s.own [] wf.1
    exact ⟨safe, wf.2.1, wf.2.2.1, downwardTrace_pending hu initial wf.2.2.2⟩
  | owner p path m x =>
    have safe := passOwner_call_preserves s p path m x wf
    rwa [(passOwner_iff ..).mpr step] at safe
  | view i x =>
    have safe := passDown_call_preserves s i x wf
    rwa [(passDown_iff ..).mpr step] at safe
  | lifecycle => exact callLife_wellFormed step wf

/-- Helper (preservation). The place and loan retirement corollaries have independent premises. -/
theorem callLife_retirement {types : Var → Option Descriptor} {s t : CallState} {e : Exec.Life}
    (h : CallLife types s e t) :
    (∀ p, RetiredPlace s.own p → RetiredPlace t.own p) ∧
    (∀ i, RetiredLoan s.own i → RetiredLoan t.own i) := by
  have safe := (callLife_dispatch types s e).2
  rw [(callLife_iff types s e t).mpr h] at safe
  exact safe.2.2

/-- Helper (induction). Every ledger in a legal loan sequence is well formed. -/
theorem loanSequence_wellFormed {s t : State} {es : List LoanAction}
    (h : LoanSequence s es t) (hs : WellFormed s) : WellFormed t := by
  induction h with
  | nil => exact hs
  | cons head _ tail => exact tail (loanStep_wellFormed head hs)

/-- Helper (inversion). A failure of any substituted dispatcher falsifies exact realization. -/
theorem rejects {α : Type} {rule : α → Prop} {actual mutant : Option α}
    (law : ∀ t, actual = some t ↔ rule t) (different : mutant ≠ actual) :
    ¬ (∀ t, mutant = some t ↔ rule t) :=
  fun realizes => different (Option.ext fun t => (realizes t).trans (law t).symm)

namespace Controls
open Examples

def keepOnLifeRefusal (e : Exec.Life) (s : State) : Option State :=
  (life types e s).or (some s)

def keepOnSequenceRefusal (s : State) (es : List LoanAction) : Option State :=
  (runLoanSequence s es).or (some s)

/-- Planted (law `life_iff`, mutant `keepOnLifeRefusal`). A live linear owner blocks exit. -/
example : ¬ (∀ t, keepOnLifeRefusal .leave initial = some t ↔ Life types initial .leave t) :=
  rejects (life_iff types initial .leave) (by decide)

/-- Planted (law `runLoanSequence_iff`, mutant `keepOnSequenceRefusal`). A live child blocks end. -/
example : ¬ (∀ t, keepOnSequenceRefusal child [.endLoan ⟨0⟩] = some t ↔
    LoanSequence child [.endLoan ⟨0⟩] t) :=
  rejects (runLoanSequence_iff child [.endLoan ⟨0⟩]) (by decide)

def lifeWithoutDropDischarge (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .drop xs => DispatchEvidence.dropWithoutDischarge s xs
  | .bind _ | .enter _ | .leave => life types e s

/-- Planted (law `life_iff`, mutant `lifeWithoutDropDischarge`). An unborrowed owner is still linear. -/
example : ¬ (∀ t, lifeWithoutDropDischarge (.drop [x]) initial = some t ↔
    Life types initial (.drop [x]) t) :=
  rejects (life_iff types initial (.drop [x])) (by decide)

def untyped (_ : Var) : Option Descriptor := none

def defaultDescriptor (checked : Var → Option Descriptor) (x : Var) : Option Descriptor :=
  (checked x).or (some ⟨.int .i32, false, true⟩)

def lifeWithDefaultBinding (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .bind x => bind (defaultDescriptor untyped) x s
  | .enter _ | .leave | .drop _ => life untyped e s

def lifeWithDefaultEntry (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .enter xs => enter (defaultDescriptor untyped) xs s
  | .bind _ | .leave | .drop _ => life untyped e s

/-- Planted (law `life_iff`, mutant `lifeWithDefaultBinding`). Allocation still advances the supply. -/
example : ¬ (∀ t, lifeWithDefaultBinding (.bind y) initial = some t ↔
    Life untyped initial (.bind y) t) :=
  rejects (life_iff untyped initial (.bind y)) (by decide)

/-- Planted (law `life_iff`, mutant `lifeWithDefaultEntry`). Entry still allocates its activation. -/
example : ¬ (∀ t, lifeWithDefaultEntry (.enter [y]) initial = some t ↔
    Life untyped initial (.enter [y]) t) :=
  rejects (life_iff untyped initial (.enter [y])) (by decide)

def placeOnlyRetired : CallState := ⟨RetirementEvidence.retired, []⟩

def rewindPlace (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .drop [] => (callLife types e s).map fun t => { t with own := { t.own with nextPlace := 0 } }
  | .drop (_ :: _) | .enter _ | .leave | .bind _ => callLife types e s

/-- Planted (law `callLife_iff`, mutant `rewindPlace`). Only the place supply changes. -/
example : RetiredPlace placeOnlyRetired.own ⟨0⟩ ∧ placeOnlyRetired.own.nextLoan = 0 ∧
    ¬ (∀ t, rewindPlace (.drop []) placeOnlyRetired = some t ↔
      CallLife types placeOnlyRetired (.drop []) t) :=
  ⟨by decide, rfl, rejects (callLife_iff types placeOnlyRetired (.drop [])) (by decide)⟩

def refuseProjected (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (a : Nat) : Option State :=
  if path.isEmpty then reborrowTo s i path m a else none

/-- Planted (law `reborrowTo_iff`, mutant `refuseProjected`). Refusal is at the lending primitive. -/
example : ¬ (∀ t, refuseProjected LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ↔
    ReborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 t) :=
  rejects (reborrowTo_iff LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1) (by decide)

/-- Planted (law `runLoanSequence_iff`, mutant `DispatchEvidence.refuseRunLoanSequence`). -/
example : ¬ (∀ t, DispatchEvidence.refuseRunLoanSequence initial [] = some t ↔
    LoanSequence initial [] t) :=
  rejects (runLoanSequence_iff initial []) (by decide)

/-- Planted (law `issuanceStep_iff`, mutant `DispatchEvidence.refuseIssuanceStep`). -/
example : ¬ (∀ t, DispatchEvidence.refuseIssuanceStep ⟨initial, []⟩ (.entry []) = some t ↔
    Issuance types ⟨initial, []⟩ (.entry []) t) :=
  rejects (issuanceStep_iff types ⟨initial, []⟩ (.entry [])) (by decide)

def ignorePendingOnDrop (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .drop _ => (life types e s.own).map fun own => { s with own := own }
  | .enter _ | .leave | .bind _ => callLife types e s

/-- Planted (law `callLife_iff`, mutant `ignorePendingOnDrop`). Pending arguments block teardown. -/
example : ¬ (∀ t, ignorePendingOnDrop (.drop []) PendingEvidence.down = some t ↔
    CallLife types PendingEvidence.down (.drop []) t) :=
  rejects (callLife_iff types PendingEvidence.down (.drop [])) (by decide)

open DispatchEvidence

/-- Planted (law `life_iff`, mutant `acceptLiveReturn`). Exit must discharge each local. -/
example : ¬ (∀ t, acceptLiveReturn .leave initial = some t ↔ Life types initial .leave t) :=
  rejects (life_iff types initial .leave) (by decide)

/-- Planted (law `life_iff`, mutant `acceptBorrowedDrop`). Copy storage still requires loan exclusion. -/
example : ¬ (∀ t, acceptBorrowedDrop (.drop [x]) borrowedCopy = some t ↔
    Life types borrowedCopy (.drop [x]) t) :=
  rejects (life_iff types borrowedCopy (.drop [x])) (by decide)

/-- Planted (laws `loanAction_iff`, `runLoanSequence_iff` and `issuanceStep_iff`,
mutant `acceptMissingRoot`). The same borrow rule is checked through all three interfaces. -/
example :
    let e := LoanAction.borrow ⟨9⟩ [] .shared
    let s : CallState := ⟨initial, []⟩
    ¬ RetiringLoanAction acceptMissingRoot initial e ∧
    (¬ (∀ t, [e].foldlM acceptMissingRoot initial = some t ↔ LoanSequence initial [e] t)) ∧
    (¬ (∀ t, (acceptMissingRoot initial e).map (fun own => { s with own := own }) = some t ↔
      Issuance types s (.ordinary e) t)) :=
  ⟨fun h => rejects (fun t => loanAction_iff _ _ t) (by decide) h.2.2.1,
   rejects (fun t => runLoanSequence_iff _ _ t) (by decide),
   rejects (fun t => issuanceStep_iff _ _ _ t) (by decide)⟩

/-- Planted (laws `loanAction_iff`, `runLoanSequence_iff` and `issuanceStep_iff`,
mutant `acceptUnissuedDestination`). The current frame must also have been issued. -/
example :
    let e := LoanAction.reborrow ⟨0⟩ [] .mutable 0
    let s : CallState := ⟨unissuedCurrent, []⟩
    ¬ RetiringLoanAction acceptUnissuedDestination s.own e ∧
    (¬ (∀ t, [e].foldlM acceptUnissuedDestination s.own = some t ↔ LoanSequence s.own [e] t)) ∧
    (¬ (∀ t, (acceptUnissuedDestination s.own e).map (fun own => { s with own := own }) = some t ↔
      Issuance types s (.ordinary e) t)) :=
  ⟨fun h => rejects (fun t => loanAction_iff _ _ t) (by decide) h.2.2.1,
   rejects (fun t => runLoanSequence_iff _ _ t) (by decide),
   rejects (fun t => issuanceStep_iff _ _ _ t) (by decide)⟩

/-- Planted (laws `loanAction_iff`, `runLoanSequence_iff` and `issuanceStep_iff`,
mutant `acceptParentEnd`). The removal of an existing loan requires it to be a leaf. -/
example :
    let e := LoanAction.endLoan ⟨0⟩
    let s : CallState := ⟨child, []⟩
    ¬ RetiringLoanAction acceptParentEnd child e ∧
    (¬ (∀ t, [e].foldlM acceptParentEnd child = some t ↔ LoanSequence child [e] t)) ∧
    (¬ (∀ t, (acceptParentEnd child e).map (fun own => { s with own := own }) = some t ↔
      Issuance types s (.ordinary e) t)) :=
  ⟨fun h => rejects (fun t => loanAction_iff _ _ t) (by decide) h.2.2.1,
   rejects (fun t => runLoanSequence_iff _ _ t) (by decide),
   rejects (fun t => issuanceStep_iff _ _ _ t) (by decide)⟩

/-- Planted (law `passOwner_iff`, mutant `acceptFrozenOwner`). Owner arguments require exclusion. -/
example : ¬ (∀ t, acceptFrozenOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y = some t ↔
    PassOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y t) :=
  rejects (fun t => passOwner_iff _ _ _ _ _ t) (by decide)

/-- Planted (law `passDown_iff`, mutant `acceptFrozenView`). An existing child can block lending. -/
example : ¬ (∀ t, acceptFrozenView ⟨child, []⟩ ⟨0⟩ y = some t ↔ PassDown ⟨child, []⟩ ⟨0⟩ y t) :=
  rejects (fun t => passDown_iff _ _ _ t) (by decide)

/-- Planted (law `enterDown_iff`, mutant `acceptFrozenDownward`). The first grant freezes the source. -/
example : ¬ (∀ t, acceptFrozenDownward exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] = some t ↔
    EnterDown exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] t) :=
  rejects (fun t => enterDown_iff _ _ t) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseCallBinding`). -/
example : ¬ (∀ t, refuseCallBinding (.bind DirectArgumentWitnesses.z) DirectArgumentWitnesses.caller = some t ↔
    CallLife types DirectArgumentWitnesses.caller (.bind DirectArgumentWitnesses.z) t) :=
  rejects (fun t => callLife_iff _ _ _ t) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseMixedCallEntry`). -/
example : ¬ (∀ t, refuseMixedCallEntry (.enter [x, y, DirectArgumentWitnesses.z])
      Round5Evidence.twoWaiting = some t ↔
    CallLife types Round5Evidence.twoWaiting (.enter [x, y, DirectArgumentWitnesses.z]) t) :=
  rejects (fun t => callLife_iff _ _ _ t) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseNonemptyCallReturn`). Each admissible return succeeds. -/
example :
    (¬ (∀ t, refuseNonemptyCallReturn .leave localReturn = some t ↔ CallLife types localReturn .leave t)) ∧
    (¬ (∀ t, refuseMultipleLoanReturn .leave ⟨Round4Evidence.localChild, []⟩ = some t ↔
      CallLife types ⟨Round4Evidence.localChild, []⟩ .leave t)) :=
  ⟨rejects (fun t => callLife_iff _ _ _ t) (by decide), rejects (fun t => callLife_iff _ _ _ t) (by decide)⟩

/-- Planted (law `life_iff`, mutants `refuseLifeBind` and `refuseLifeEnter`). -/
example :
    (¬ (∀ t, refuseLifeBind (.bind y) initial = some t ↔ Life types initial (.bind y) t)) ∧
    (¬ (∀ t, refuseLifeEnter (.enter []) initial = some t ↔ Life types initial (.enter []) t)) :=
  ⟨rejects (fun t => life_iff _ _ _ t) (by decide), rejects (fun t => life_iff _ _ _ t) (by decide)⟩

def refuseStoreDrop (e : Exec.Life) (s : Store Nat) : Option (Store Nat) :=
  match e with
  | .drop [] => if s.concrete = 7 then none else lifeStore types e s
  | .drop (_ :: _) | .enter _ | .leave | .bind _ => lifeStore types e s

/-- Planted (law `lifeStore_iff`, mutant `refuseStoreDrop`). Concrete payload cannot veto teardown. -/
example : ¬ (∀ t, refuseStoreDrop (.drop []) ⟨7, initial⟩ = some t ↔
    LifeStore types (⟨7, initial⟩ : Store Nat) (.drop []) t) :=
  rejects (lifeStore_iff types ⟨7, initial⟩ (.drop [])) (by decide)

/-- Planted (law `callLifeStore_iff`, mutant `resetStore`). Concrete payload must survive entry. -/
example : ¬ (∀ t, resetStore (.enter []) (7, nestedCaller) = some t ↔
    CallLifeStore types (7, nestedCaller) (.enter []) t) :=
  rejects (callLifeStore_iff types (7, nestedCaller) (.enter [])) (by decide)

/-- Planted (law `enterDown_iff`, mutant `refuseStandaloneEntry`). -/
example : ¬ (∀ t, refuseStandaloneEntry exclusive singletonEntry = some t ↔
    EnterDown exclusive singletonEntry t) :=
  rejects (enterDown_iff exclusive singletonEntry) (by decide)

/-- Planted (law `adoptDown_iff`, mutant `refuseMultiAdopt`). -/
example : ¬ (∀ t, refuseMultiAdopt Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending = some t ↔
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t) :=
  rejects (adoptDown_iff Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending) (by decide)

/-- Planted (law `move_iff`, mutant `refuseNonzeroMove`). -/
example : ¬ (∀ t, refuseNonzeroMove MoveLifecycleEvidence.source ⟨1⟩ = some t ↔
    Move MoveLifecycleEvidence.source ⟨1⟩ t) :=
  rejects (move_iff MoveLifecycleEvidence.source ⟨1⟩) (by decide)

/-- Planted (law `passDown_iff`, mutant `refuseSecondPendingView`). -/
example : ¬ (∀ t, refuseSecondPendingView PendingEvidence.down ⟨0⟩ y = some t ↔
    PassDown PendingEvidence.down ⟨0⟩ y t) :=
  rejects (passDown_iff PendingEvidence.down ⟨0⟩ y) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseEmptyCallEntry`). -/
example : ¬ (∀ t, refuseEmptyCallEntry (.enter []) nestedCaller = some t ↔
    CallLife types nestedCaller (.enter []) t) :=
  rejects (callLife_iff types nestedCaller (.enter [])) (by decide)

/-- Planted (law `move_iff`, mutant `acceptBorrowedMove`). -/
example : ¬ (∀ t, acceptBorrowedMove exclusive ⟨0⟩ = some t ↔ Move exclusive ⟨0⟩ t) :=
  rejects (move_iff exclusive ⟨0⟩) (by decide)

/-- Planted (law `adoptDown_iff`, mutant `PreservationEvidence.adoptDownUnissuedActivation`). -/
example : ¬ (∀ t, PreservationEvidence.adoptDownUnissuedActivation Round5Evidence.twoBefore
    Round5Evidence.twoWaiting.pending = some t ↔
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t) :=
  rejects (adoptDown_iff Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending) (by decide)

/-- Planted (law `callLife_iff`, mutant `PreservationEvidence.callLifeKeepPending`). -/
example : ¬ (∀ t, PreservationEvidence.callLifeKeepPending types (.enter [y, DirectArgumentWitnesses.z])
      Round5Evidence.twoWaiting = some t ↔
    CallLife types Round5Evidence.twoWaiting (.enter [y, DirectArgumentWitnesses.z]) t) :=
  rejects (callLife_iff types Round5Evidence.twoWaiting (.enter [y, DirectArgumentWitnesses.z])) (by decide)

/-- Planted (law `life_iff`, mutant `refuseReinitBind`). -/
example : ¬ (∀ t, refuseReinitBind (.bind DirectArgumentWitnesses.z) localReturn.own = some t ↔
    Life types localReturn.own (.bind DirectArgumentWitnesses.z) t) :=
  rejects (life_iff types localReturn.own (.bind DirectArgumentWitnesses.z)) (by decide)

/-- Planted (law `runLoanSequence_iff`, mutant `LoanHistoryEvidence.recycle`). -/
example : ¬ (∀ t, LoanHistoryEvidence.recycle LoanHistoryEvidence.endedChild LoanHistoryEvidence.actions =
      some t ↔ LoanSequence LoanHistoryEvidence.endedChild LoanHistoryEvidence.actions t) :=
  rejects (runLoanSequence_iff LoanHistoryEvidence.endedChild LoanHistoryEvidence.actions) (by decide)


private theorem admits {α : Type} {actual : Option α} {rule : α → Prop}
    (law : ∀ t, actual = some t ↔ rule t) (present : actual.isSome = true) :
    ∃ t, actual = some t ∧ rule t := by
  cases actual with
  | none => contradiction
  | some t => exact ⟨t, rfl, (law t).mp rfl⟩

private theorem rejectsPermission {actual mutant : Bool} {rule : Prop}
    (law : actual = true ↔ rule) (different : mutant ≠ actual) :
    ¬ (mutant = true ↔ rule) := by
  cases actual <;> cases mutant <;> simp_all

def baseCall : CallState := ⟨initial, []⟩
def baseStore : Store Nat := ⟨7, initial⟩

abbrev controlOperands : Exec.Operands where
  Val := Unit
  Expr := Unit
  Ty := Unit
  Store := Nat
  Prim := Unit
  Event := Unit
  unitVal := ()
  unitTy := ()
  isCopy := fun _ => true
  eval := fun _ s _ => .ok ((), s)
  life := fun _ s => some s
  test := fun _ => none
  allows := fun _ _ _ _ => false

def concreteIdentity (_ : Exec.Life) (s t : Nat) : Prop := s = t

private theorem concreteIdentity_iff (e : Exec.Life) (s t : Nat) :
    controlOperands.life e s = some t ↔ concreteIdentity e s t :=
  by simp [controlOperands, concreteIdentity]

def splitActivation : State :=
  { child with loans := child.loans.map fun l =>
      if l.id = ⟨1⟩ then { l with activation := 1 } else l }

/-- Mutant omitting the requirement that every transferred argument already exists. -/
def adoptAbsent (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

def pendingEnd : IssuanceAction := .ordinary (.endLoan ⟨1⟩)

/-- Mutant omitting only the protection of buffered arguments on an ordinary end. -/
def endPending (s : CallState) : IssuanceAction → Option CallState
  | .ordinary (.endLoan i) => (endLoan s.own i).map fun own => { s with own := own }
  | e => issuanceStep types s e


/-- Mutant dropping only the whole-root exclusion test. -/
def ownerWithoutExclusion (s : State) (p : Hybrid.Place) (m : Mode) : Bool :=
  match slot s p with
  | none => false
  | some a => a.status == .owned && (m == .shared || a.desc.mutable)

def loanWithoutExclusion (s : State) (i : LoanId) (m : Mode) : Bool :=
  match loan s i with
  | none => false
  | some a => !a.pendingArgument && (m == .shared || a.mode == .mutable)

def refuseMutableOwner (s : State) (p : Hybrid.Place) (m : Mode) : Bool :=
  m == .shared && ownerAccess s p m

def refuseMutableLoan (s : State) (i : LoanId) (m : Mode) : Bool :=
  m == .shared && loanAccess s i m

def accessWith (allowed : Bool) (s : State) : Option State :=
  if allowed then some s else none

/-- Mutant allowing overwrite of a live linear owner, with the other checks intact. -/
def overwriteLinear (s : State) (p : Hybrid.Place) : Option State := do
  let a ← slot s p
  if (a.status == .moved || a.desc.mutable) && s.loans.all (·.root != p) then
    some { s with slots := s.slots.map fun x =>
      if x.place = p then { x with status := .owned } else x }
  else none

def refuseMovedSlot (s : State) (p : Hybrid.Place) : Option State :=
  if (slot s p).any (fun a => a.status == .moved) then none else reinitialize s p

def refuseFreshSlot (types : Var → Option Descriptor) (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  let _ ← AList.lookup f.locals x
  bind types x s

def refuseNonemptyDrop (xs : List Var) (s : State) : Option State :=
  if xs.isEmpty then drop xs s else none

def refuseMutableBorrow (s : State) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) : Option State :=
  if m == .mutable then none else borrow s p path m

def refuseMutableReborrow (s : State) (i : LoanId) (path : List Projection)
    (m : Mode) : Option State :=
  if m == .mutable then none else reborrow s i path m

def refuseLeafEnd (s : State) (i : LoanId) : Option State :=
  if (loan s i).any (fun l => l.parent.isSome) then none else endLoan s i

def refuseChildEnd (s : State) : LoanAction → Option State
  | .endLoan i => refuseLeafEnd s i
  | e => loanAction s e

def refusePopulatedEnding (activation : Nat) (s : State) : Option State :=
  if s.loans.any (·.activation == activation) then none else endLocals activation s

def refuseTypedEntry (types : Var → Option Descriptor) (xs : List Var) (s : State) : Option State :=
  if xs.isEmpty then enter types xs s else none

def refusePopulatedExit (s : State) : Option State := do
  let f ← s.frames.head?
  if f.locals.isEmpty then leave s else none

def refuseMutableArgument (s : CallState) (p : Hybrid.Place) (path : List Projection)
    (m : Mode) (x : Var) : Option CallState :=
  if m == .mutable then none else passOwner s p path m x

def instrumentWithPermissiveDrop (e : Exec.Life) (s : Store Nat) : Option (Store Nat) := do
  let own ← match e with
    | .drop xs => dropWithoutDischarge s.own xs
    | .bind _ | .enter _ | .leave => life types e s.own
  let concrete ← controlOperands.life e s.concrete
  some ⟨concrete, own⟩

def instrumentRefusingEmptyDrop (e : Exec.Life) (s : Store Nat) : Option (Store Nat) :=
  match e with
  | .drop [] => none
  | .drop (_ :: _) | .bind _ | .enter _ | .leave => instrumentLife controlOperands types e s

private theorem rejectsAcceptance {α : Type} {rule : α → Prop} {actual mutant : Option α}
    (law : ∀ t, actual = some t ↔ rule t)
    (refused : actual = none) (accepted : mutant.isSome = true) :
    ¬ (∀ t, mutant = some t ↔ rule t) := by
  apply rejects law
  intro same
  simp [same, refused] at accepted

private theorem rejectsRefusal {α : Type} {rule : α → Prop} {actual mutant : Option α}
    (law : ∀ t, actual = some t ↔ rule t)
    (accepted : actual.isSome = true) (refused : mutant = none) :
    ¬ (∀ t, mutant = some t ↔ rule t) := by
  apply rejects law
  intro same
  simp [← same, refused] at accepted

/-- Instance (law `ownerAccess_iff`, case `initial access`). -/
example : ownerAccess initial ⟨0⟩ .mutable = true ∧ OwnerPermission initial ⟨0⟩ .mutable := ⟨rfl, (ownerAccess_iff _ _ _).mp (show ownerAccess initial ⟨0⟩ .mutable = true from rfl)⟩

/-- Planted (law `ownerAccess_iff`, direction over-accepting). Omit exclusion. -/
example : ¬ (ownerWithoutExclusion exclusive ⟨0⟩ .mutable = true ↔ OwnerPermission exclusive ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (ownerAccess exclusive ⟨0⟩ .mutable)) (ownerAccess_iff _ _ _) (by decide)

/-- Planted (law `ownerAccess_iff`, direction over-refusing). Deny permitted access. -/
example : ¬ (refuseMutableOwner initial ⟨0⟩ .mutable = true ↔ OwnerPermission initial ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (ownerAccess initial ⟨0⟩ .mutable)) (ownerAccess_iff _ _ _) (by decide)

/-- Instance (law `loanAccess_iff`, case `initial access`). -/
example : loanAccess exclusive ⟨0⟩ .mutable = true ∧ LoanPermission exclusive ⟨0⟩ .mutable := ⟨rfl, (loanAccess_iff _ _ _).mp (show loanAccess exclusive ⟨0⟩ .mutable = true from rfl)⟩

/-- Planted (law `loanAccess_iff`, direction over-accepting). Omit exclusion. -/
example : ¬ (loanWithoutExclusion child ⟨0⟩ .mutable = true ↔ LoanPermission child ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (loanAccess child ⟨0⟩ .mutable)) (loanAccess_iff _ _ _) (by decide)

/-- Planted (law `loanAccess_iff`, direction over-refusing). Deny permitted access. -/
example : ¬ (refuseMutableLoan exclusive ⟨0⟩ .mutable = true ↔ LoanPermission exclusive ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (loanAccess exclusive ⟨0⟩ .mutable)) (loanAccess_iff _ _ _) (by decide)

/-- Instance (law `accessOwner_iff`, case `admitted operation`). -/
example : ∃ t, accessOwner initial ⟨0⟩ .mutable = some t ∧ AccessOwner initial ⟨0⟩ .mutable t :=
  admits (fun t => accessOwner_iff _ _ _ t) (by decide)

/-- Planted (law `accessOwner_iff`, direction over-accepting). -/
example : ¬ (∀ t, accessWith (ownerWithoutExclusion exclusive ⟨0⟩ .mutable) exclusive = some t ↔
    AccessOwner exclusive ⟨0⟩ .mutable t) :=
  rejectsAcceptance (actual := (accessOwner exclusive ⟨0⟩ .mutable))
    (fun t => accessOwner_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `accessOwner_iff`, direction over-refusing). -/
example : ¬ (∀ t, accessWith (refuseMutableOwner initial ⟨0⟩ .mutable) initial = some t ↔
    AccessOwner initial ⟨0⟩ .mutable t) :=
  rejectsRefusal (actual := (accessOwner initial ⟨0⟩ .mutable))
    (fun t => accessOwner_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `accessLoan_iff`, case `admitted operation`). -/
example : ∃ t, accessLoan exclusive ⟨0⟩ .mutable = some t ∧ AccessLoan exclusive ⟨0⟩ .mutable t :=
  admits (fun t => accessLoan_iff _ _ _ t) (by decide)

/-- Planted (law `accessLoan_iff`, direction over-accepting). -/
example : ¬ (∀ t, accessWith (loanWithoutExclusion child ⟨0⟩ .mutable) child = some t ↔
    AccessLoan child ⟨0⟩ .mutable t) :=
  rejectsAcceptance (actual := (accessLoan child ⟨0⟩ .mutable))
    (fun t => accessLoan_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `accessLoan_iff`, direction over-refusing). -/
example : ¬ (∀ t, accessWith (refuseMutableLoan exclusive ⟨0⟩ .mutable) exclusive = some t ↔
    AccessLoan exclusive ⟨0⟩ .mutable t) :=
  rejectsRefusal (actual := (accessLoan exclusive ⟨0⟩ .mutable))
    (fun t => accessLoan_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `reinitialize_iff`, case `admitted operation`). -/
example : ∃ t, reinitialize LifecycleWitnesses.moved ⟨0⟩ = some t ∧
    Reinitialize LifecycleWitnesses.moved ⟨0⟩ t :=
  admits (fun t => reinitialize_iff _ _ t) (by decide)

/-- Planted (law `reinitialize_iff`, direction over-accepting). -/
example : ¬ (∀ t, overwriteLinear initial ⟨0⟩ = some t ↔ Reinitialize initial ⟨0⟩ t) :=
  rejectsAcceptance (actual := (reinitialize initial ⟨0⟩))
    (fun t => reinitialize_iff _ _ t) (by decide) (by decide)

/-- Planted (law `reinitialize_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseMovedSlot LifecycleWitnesses.moved ⟨0⟩ = some t ↔
    Reinitialize LifecycleWitnesses.moved ⟨0⟩ t) :=
  rejectsRefusal (actual := (reinitialize LifecycleWitnesses.moved ⟨0⟩))
    (fun t => reinitialize_iff _ _ t) (by decide) (by decide)

/-- Instance (law `bind_iff`, case `admitted operation`). -/
example : ∃ t, bind types y initial = some t ∧ Bind types initial y t :=
  admits (fun t => bind_iff _ _ _ t) (by decide)

/-- Planted (law `bind_iff`, direction over-accepting). -/
example : ¬ (∀ t, bind (defaultDescriptor untyped) y initial = some t ↔ Bind untyped initial y t) :=
  rejectsAcceptance (actual := (bind untyped y initial)) (fun t => bind_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `bind_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseFreshSlot types y initial = some t ↔ Bind types initial y t) :=
  rejectsRefusal (actual := (bind types y initial)) (fun t => bind_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `drop_iff`, case `admitted operation`). -/
example : ∃ t, drop [x] LifecycleWitnesses.moved = some t ∧ Drop LifecycleWitnesses.moved [x] t :=
  admits (fun t => drop_iff _ _ t) (by decide)

/-- Planted (law `drop_iff`, direction over-accepting). -/
example : ¬ (∀ t, dropWithoutDischarge initial [x] = some t ↔ Drop initial [x] t) :=
  rejectsAcceptance (actual := (drop [x] initial)) (fun t => drop_iff _ _ t) (by decide) (by decide)

/-- Planted (law `drop_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseNonemptyDrop [x] LifecycleWitnesses.moved = some t ↔
    Drop LifecycleWitnesses.moved [x] t) :=
  rejectsRefusal (actual := (drop [x] LifecycleWitnesses.moved))
    (fun t => drop_iff _ _ t) (by decide) (by decide)

/-- Instance (law `move_iff`, case `admitted operation`). -/
example : ∃ t, move MoveLifecycleEvidence.source ⟨1⟩ = some t ∧ Move MoveLifecycleEvidence.source ⟨1⟩ t :=
  admits (fun t => move_iff _ _ t) (by decide)

/-- Planted (law `move_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptBorrowedMove exclusive ⟨0⟩ = some t ↔ Move exclusive ⟨0⟩ t) :=
  rejectsAcceptance (actual := (move exclusive ⟨0⟩)) (fun t => move_iff _ _ t) (by decide) (by decide)

/-- Planted (law `move_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseNonzeroMove MoveLifecycleEvidence.source ⟨1⟩ = some t ↔
    Move MoveLifecycleEvidence.source ⟨1⟩ t) :=
  rejectsRefusal (actual := (move MoveLifecycleEvidence.source ⟨1⟩))
    (fun t => move_iff _ _ t) (by decide) (by decide)

/-- Instance (law `borrow_iff`, case `admitted operation`). -/
example : ∃ t, borrow initial ⟨0⟩ [] .mutable = some t ∧ Borrow initial ⟨0⟩ [] .mutable t :=
  admits (fun t => borrow_iff _ _ _ _ t) (by decide)

/-- Planted (law `borrow_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptMissingRoot initial (.borrow ⟨9⟩ [] .shared) = some t ↔
    Borrow initial ⟨9⟩ [] .shared t) :=
  rejectsAcceptance (actual := (borrow initial ⟨9⟩ [] .shared))
    (fun t => borrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `borrow_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseMutableBorrow initial ⟨0⟩ [] .mutable = some t ↔ Borrow initial ⟨0⟩ [] .mutable t) :=
  rejectsRefusal (actual := (borrow initial ⟨0⟩ [] .mutable))
    (fun t => borrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `reborrowTo_iff`, case `admitted operation`). -/
example : ∃ t, reborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ∧
    ReborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 t :=
  admits (fun t => reborrowTo_iff _ _ _ _ _ t) (by decide)

/-- Planted (law `reborrowTo_iff`, direction over-accepting). -/
example : ¬ (∀ t, reborrowIgnoringConflict child ⟨0⟩ [] .mutable 0 = some t ↔
    ReborrowTo child ⟨0⟩ [] .mutable 0 t) :=
  rejectsAcceptance (actual := (reborrowTo child ⟨0⟩ [] .mutable 0))
    (fun t => reborrowTo_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `reborrowTo_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseProjected LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ↔
    ReborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 t) :=
  rejectsRefusal (actual := (reborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1))
    (fun t => reborrowTo_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `reborrow_iff`, case `admitted operation`). -/
example : ∃ t, reborrow exclusive ⟨0⟩ [] .mutable = some t ∧ Reborrow exclusive ⟨0⟩ [] .mutable t :=
  admits (fun t => reborrow_iff _ _ _ _ t) (by decide)

/-- Planted (law `reborrow_iff`, direction over-accepting). -/
example : ¬ (∀ t, reborrowIgnoringConflict child ⟨0⟩ [] .mutable 0 = some t ↔
    Reborrow child ⟨0⟩ [] .mutable t) :=
  rejectsAcceptance (actual := (reborrow child ⟨0⟩ [] .mutable))
    (fun t => reborrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `reborrow_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseMutableReborrow exclusive ⟨0⟩ [] .mutable = some t ↔
    Reborrow exclusive ⟨0⟩ [] .mutable t) :=
  rejectsRefusal (actual := (reborrow exclusive ⟨0⟩ [] .mutable))
    (fun t => reborrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `endLoan_iff`, case `admitted operation`). -/
example : ∃ t, endLoan child ⟨1⟩ = some t ∧ EndLoan child ⟨1⟩ t :=
  admits (fun t => endLoan_iff _ _ t) (by decide)

/-- Planted (law `endLoan_iff`, direction over-accepting). -/
example : ¬ (∀ t, PreservationEvidence.endLoanAllowChildren child ⟨0⟩ = some t ↔ EndLoan child ⟨0⟩ t) :=
  rejectsAcceptance (actual := (endLoan child ⟨0⟩)) (fun t => endLoan_iff _ _ t) (by decide) (by decide)

/-- Planted (law `endLoan_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseLeafEnd child ⟨1⟩ = some t ↔ EndLoan child ⟨1⟩ t) :=
  rejectsRefusal (actual := (endLoan child ⟨1⟩)) (fun t => endLoan_iff _ _ t) (by decide) (by decide)

/-- Instance (law `loanAction_iff`, case `admitted operation`). -/
example : ∃ t, loanAction child (.endLoan ⟨1⟩) = some t ∧ LoanStep child (.endLoan ⟨1⟩) t :=
  admits (fun t => loanAction_iff _ _ t) (by decide)

/-- Planted (law `loanAction_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptParentEnd child (.endLoan ⟨0⟩) = some t ↔ LoanStep child (.endLoan ⟨0⟩) t) :=
  rejectsAcceptance (actual := (loanAction child (.endLoan ⟨0⟩)))
    (fun t => loanAction_iff _ _ t) (by decide) (by decide)

/-- Planted (law `loanAction_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseChildEnd child (.endLoan ⟨1⟩) = some t ↔ LoanStep child (.endLoan ⟨1⟩) t) :=
  rejectsRefusal (actual := (loanAction child (.endLoan ⟨1⟩)))
    (fun t => loanAction_iff _ _ t) (by decide) (by decide)

/-- Instance (law `runLoanSequence_iff`, case `admitted operation`). -/
example : ∃ t, runLoanSequence child [] = some t ∧ LoanSequence child [] t :=
  admits (fun t => runLoanSequence_iff _ _ t) (by decide)

/-- Planted (law `runLoanSequence_iff`, direction over-accepting). -/
example : ¬ (∀ t, keepOnSequenceRefusal child [.endLoan ⟨0⟩] = some t ↔
    LoanSequence child [.endLoan ⟨0⟩] t) :=
  rejectsAcceptance (actual := (runLoanSequence child [.endLoan ⟨0⟩]))
    (fun t => runLoanSequence_iff _ _ t) (by decide) (by decide)

/-- Planted (law `runLoanSequence_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseRunLoanSequence child [] = some t ↔ LoanSequence child [] t) :=
  rejectsRefusal (actual := (runLoanSequence child []))
    (fun t => runLoanSequence_iff _ _ t) (by decide) (by decide)

/-- Instance (law `endLocals_iff`, case `admitted operation`). -/
example : ∃ t, endLocals 0 child = some t ∧ EndLocals child 0 t :=
  admits (fun t => endLocals_iff _ _ t) (by decide)

/-- Planted (law `endLocals_iff`, direction over-accepting). -/
example : ¬ (∀ t, PreservationEvidence.endLocalsAllowChildren 0 splitActivation = some t ↔
    EndLocals splitActivation 0 t) :=
  rejectsAcceptance (actual := (endLocals 0 splitActivation))
    (fun t => endLocals_iff _ _ t) (by decide) (by decide)

/-- Planted (law `endLocals_iff`, direction over-refusing). -/
example : ¬ (∀ t, refusePopulatedEnding 0 child = some t ↔ EndLocals child 0 t) :=
  rejectsRefusal (actual := (endLocals 0 child)) (fun t => endLocals_iff _ _ t) (by decide) (by decide)

/-- Instance (law `enter_iff`, case `admitted operation`). -/
example : ∃ t, enter types [y] initial = some t ∧ Enter types initial [y] t :=
  admits (fun t => enter_iff _ _ _ t) (by decide)

/-- Planted (law `enter_iff`, direction over-accepting). -/
example : ¬ (∀ t, enter (defaultDescriptor untyped) [y] initial = some t ↔ Enter untyped initial [y] t) :=
  rejectsAcceptance (actual := (enter untyped [y] initial))
    (fun t => enter_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `enter_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseTypedEntry types [y] initial = some t ↔ Enter types initial [y] t) :=
  rejectsRefusal (actual := (enter types [y] initial)) (fun t => enter_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `leave_iff`, case `admitted operation`). -/
example : ∃ t, leave LifecycleWitnesses.moved = some t ∧ Leave LifecycleWitnesses.moved t :=
  admits (fun t => leave_iff _ t) (by decide)

/-- Planted (law `leave_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptLiveReturn .leave initial = some t ↔ Leave initial t) :=
  rejectsAcceptance (actual := (leave initial)) (fun t => leave_iff _ t) (by decide) (by decide)

/-- Planted (law `leave_iff`, direction over-refusing). -/
example : ¬ (∀ t, refusePopulatedExit LifecycleWitnesses.moved = some t ↔ Leave LifecycleWitnesses.moved t) :=
  rejectsRefusal (actual := (leave LifecycleWitnesses.moved)) (fun t => leave_iff _ t) (by decide) (by decide)

/-- Instance (law `life_iff`, case `admitted operation`). -/
example : ∃ t, life types (.bind y) initial = some t ∧ Life types initial (.bind y) t :=
  admits (fun t => life_iff _ _ _ t) (by decide)

/-- Planted (law `life_iff`, direction over-accepting). -/
example : ¬ (∀ t, lifeWithoutDropDischarge (.drop [x]) initial = some t ↔ Life types initial (.drop [x]) t) :=
  rejectsAcceptance (actual := (life types (.drop [x]) initial))
    (fun t => life_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `life_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseLifeBind (.bind y) initial = some t ↔ Life types initial (.bind y) t) :=
  rejectsRefusal (actual := (life types (.bind y) initial))
    (fun t => life_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `lifeStore_iff`, case `admitted operation`). -/
example : ∃ t, lifeStore types (.drop []) baseStore = some t ∧ LifeStore types baseStore (.drop []) t :=
  admits (fun t => lifeStore_iff _ _ _ t) (by decide)

/-- Planted (law `lifeStore_iff`, direction over-accepting). -/
example : ¬ (∀ t, (dropWithoutDischarge initial [x]).map (fun own => { baseStore with own := own }) = some t ↔
    LifeStore types baseStore (.drop [x]) t) :=
  rejectsAcceptance (actual := (lifeStore types (.drop [x]) baseStore))
    (fun t => lifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `lifeStore_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseStoreDrop (.drop []) baseStore = some t ↔ LifeStore types baseStore (.drop []) t) :=
  rejectsRefusal (actual := (lifeStore types (.drop []) baseStore))
    (fun t => lifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `instrumentLife_iff`, case `admitted operation`). -/
example : ∃ t, instrumentLife controlOperands types (.drop []) baseStore = some t ∧
    InstrumentLife concreteIdentity types baseStore (.drop []) t :=
  admits (fun t => instrumentLife_iff controlOperands concreteIdentity concreteIdentity_iff _ _ _ t) (by decide)

/-- Planted (law `instrumentLife_iff`, direction over-accepting). -/
example : ¬ (∀ t, instrumentWithPermissiveDrop (.drop [x]) baseStore = some t ↔
    InstrumentLife concreteIdentity types baseStore (.drop [x]) t) :=
  rejectsAcceptance (actual := (instrumentLife controlOperands types (.drop [x]) baseStore))
    (fun t => instrumentLife_iff controlOperands concreteIdentity concreteIdentity_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `instrumentLife_iff`, direction over-refusing). -/
example : ¬ (∀ t, instrumentRefusingEmptyDrop (.drop []) baseStore = some t ↔
    InstrumentLife concreteIdentity types baseStore (.drop []) t) :=
  rejectsRefusal (actual := (instrumentLife controlOperands types (.drop []) baseStore))
    (fun t => instrumentLife_iff controlOperands concreteIdentity concreteIdentity_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `adoptDown_iff`, case `admitted operation`). -/
example : ∃ t, adoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending = some t ∧
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t :=
  admits (fun t => adoptDown_iff _ _ t) (by decide)

/-- Planted (law `adoptDown_iff`, direction over-accepting). -/
example : ¬ (∀ t, adoptAbsent initial [(y, ⟨9⟩)] = some t ↔ AdoptDown initial [(y, ⟨9⟩)] t) :=
  rejectsAcceptance (actual := (adoptDown initial [(y, ⟨9⟩)]))
    (fun t => adoptDown_iff _ _ t) (by decide) (by decide)

/-- Planted (law `adoptDown_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseMultiAdopt Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending = some t ↔
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t) :=
  rejectsRefusal (actual := (adoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending))
    (fun t => adoptDown_iff _ _ t) (by decide) (by decide)

/-- Instance (law `callLife_iff`, case `admitted operation`). -/
example : ∃ t, callLife types (.enter []) nestedCaller = some t ∧ CallLife types nestedCaller (.enter []) t :=
  admits (fun t => callLife_iff _ _ _ t) (by decide)

/-- Planted (law `callLife_iff`, direction over-accepting). -/
example : ¬ (∀ t, ignorePendingOnDrop (.drop []) PendingEvidence.down = some t ↔
    CallLife types PendingEvidence.down (.drop []) t) :=
  rejectsAcceptance (actual := (callLife types (.drop []) PendingEvidence.down))
    (fun t => callLife_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `callLife_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseEmptyCallEntry (.enter []) nestedCaller = some t ↔
    CallLife types nestedCaller (.enter []) t) :=
  rejectsRefusal (actual := (callLife types (.enter []) nestedCaller))
    (fun t => callLife_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `callLifeStore_iff`, case `admitted operation`). -/
example : ∃ t, callLifeStore types (.enter []) ((7, nestedCaller) : Nat × CallState) = some t ∧
    CallLifeStore types ((7, nestedCaller) : Nat × CallState) (.enter []) t :=
  admits (fun t => callLifeStore_iff _ _ _ t) (by decide)

/-- Planted (law `callLifeStore_iff`, direction over-accepting). -/
example : ¬ (∀ t, (ignorePendingOnDrop (.drop []) PendingEvidence.down).map (fun u => (7, u)) = some t ↔
    CallLifeStore types ((7, PendingEvidence.down) : Nat × CallState) (.drop []) t) :=
  rejectsAcceptance (actual := (callLifeStore types (.drop []) ((7, PendingEvidence.down) : Nat × CallState)))
    (fun t => callLifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `callLifeStore_iff`, direction over-refusing). -/
example : ¬ (∀ t, (refuseEmptyCallEntry (.enter []) nestedCaller).map (fun u => (7, u)) = some t ↔
    CallLifeStore types ((7, nestedCaller) : Nat × CallState) (.enter []) t) :=
  rejectsRefusal (actual := (callLifeStore types (.enter []) ((7, nestedCaller) : Nat × CallState)))
    (fun t => callLifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `enterDown_iff`, case `admitted operation`). -/
example : ∃ t, enterDown exclusive singletonEntry = some t ∧ EnterDown exclusive singletonEntry t :=
  admits (fun t => enterDown_iff _ _ t) (by decide)

/-- Planted (law `enterDown_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptFrozenDownward exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] = some t ↔
    EnterDown exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] t) :=
  rejectsAcceptance (actual := (enterDown exclusive [(x, ⟨0⟩), (y, ⟨0⟩)]))
    (fun t => enterDown_iff _ _ t) (by decide) (by decide)

/-- Planted (law `enterDown_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseStandaloneEntry exclusive singletonEntry = some t ↔
    EnterDown exclusive singletonEntry t) :=
  rejectsRefusal (actual := (enterDown exclusive singletonEntry))
    (fun t => enterDown_iff _ _ t) (by decide) (by decide)

/-- Instance (law `passDown_iff`, case `admitted operation`). -/
example : ∃ t, passDown PendingEvidence.down ⟨0⟩ y = some t ∧ PassDown PendingEvidence.down ⟨0⟩ y t :=
  admits (fun t => passDown_iff _ _ _ t) (by decide)

/-- Planted (law `passDown_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptFrozenView ⟨child, []⟩ ⟨0⟩ y = some t ↔ PassDown ⟨child, []⟩ ⟨0⟩ y t) :=
  rejectsAcceptance (actual := (passDown ⟨child, []⟩ ⟨0⟩ y))
    (fun t => passDown_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `passDown_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseSecondPendingView PendingEvidence.down ⟨0⟩ y = some t ↔
    PassDown PendingEvidence.down ⟨0⟩ y t) :=
  rejectsRefusal (actual := (passDown PendingEvidence.down ⟨0⟩ y))
    (fun t => passDown_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `passOwner_iff`, case `admitted operation`). -/
example : ∃ t, passOwner baseCall ⟨0⟩ [] .mutable y = some t ∧ PassOwner baseCall ⟨0⟩ [] .mutable y t :=
  admits (fun t => passOwner_iff _ _ _ _ _ t) (by decide)

/-- Planted (law `passOwner_iff`, direction over-accepting). -/
example : ¬ (∀ t, acceptFrozenOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y = some t ↔
    PassOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y t) :=
  rejectsAcceptance (actual := (passOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y))
    (fun t => passOwner_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `passOwner_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseMutableArgument baseCall ⟨0⟩ [] .mutable y = some t ↔
    PassOwner baseCall ⟨0⟩ [] .mutable y t) :=
  rejectsRefusal (actual := (passOwner baseCall ⟨0⟩ [] .mutable y))
    (fun t => passOwner_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `issuanceStep_iff`, case `admitted operation`). -/
example : ∃ t, issuanceStep types baseCall (.entry []) = some t ∧ Issuance types baseCall (.entry []) t :=
  admits (fun t => issuanceStep_iff _ _ _ t) (by decide)

/-- Planted (law `issuanceStep_iff`, direction over-accepting). -/
example : ¬ (∀ t, endPending PendingEvidence.down pendingEnd = some t ↔
    Issuance types PendingEvidence.down pendingEnd t) :=
  rejectsAcceptance (actual := (issuanceStep types PendingEvidence.down pendingEnd))
    (fun t => issuanceStep_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `issuanceStep_iff`, direction over-refusing). -/
example : ¬ (∀ t, refuseIssuanceStep baseCall (.entry []) = some t ↔ Issuance types baseCall (.entry []) t) :=
  rejectsRefusal (actual := (issuanceStep types baseCall (.entry [])))
    (fun t => issuanceStep_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `issuanceTrace_iff`, case `admitted operation`). -/
example : ∃ t, [IssuanceAction.entry []].foldlM (issuanceStep types) baseCall = some t ∧
    Trace (Issuance types) baseCall [.entry []] t :=
  admits (fun t => issuanceTrace_iff _ _ _ t) (by decide)

/-- Planted (law `issuanceTrace_iff`, direction over-accepting). -/
example : ¬ (∀ t, [pendingEnd].foldlM endPending PendingEvidence.down = some t ↔
    Trace (Issuance types) PendingEvidence.down [pendingEnd] t) :=
  rejectsAcceptance (actual := ([pendingEnd].foldlM (issuanceStep types) PendingEvidence.down))
    (fun t => issuanceTrace_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `issuanceTrace_iff`, direction over-refusing). -/
example : ¬ (∀ t, [IssuanceAction.entry []].foldlM refuseIssuanceStep baseCall = some t ↔
    Trace (Issuance types) baseCall [.entry []] t) :=
  rejectsRefusal (actual := ([IssuanceAction.entry []].foldlM (issuanceStep types) baseCall))
    (fun t => issuanceTrace_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `issuance_wellFormed`, case `PendingEvidence.down`). -/
example : ∃ t, Issuance types PendingEvidence.down (.view ⟨0⟩ y) t ∧ t.WellFormed := by
  obtain ⟨t, _, legal⟩ :=
    admits (issuanceStep_iff types PendingEvidence.down (.view ⟨0⟩ y)) (by decide)
  exact ⟨t, legal, issuance_wellFormed types legal (by decide)⟩

/-- The same omitted guard in the declarative relation, with every other clause retained. -/
def issuanceWithoutPendingGuard (s : CallState) (e : IssuanceAction) (t : CallState) : Prop :=
  match e with
  | .ordinary (.endLoan i) => ∃ u, EndLoan s.own i u ∧ t = { s with own := u }
  | _ => Issuance types s e t

/-- Planted (law `issuance_wellFormed`, mutant `issuanceWithoutPendingGuard`).
An ordinary end of the buffered child leaves a dead ID in the pending buffer. -/
example : ¬ (∀ t, issuanceWithoutPendingGuard PendingEvidence.down pendingEnd t →
    PendingEvidence.down.WellFormed → t.WellFormed) := by
  let t := (endPending PendingEvidence.down pendingEnd).getD PendingEvidence.down
  have legal : issuanceWithoutPendingGuard PendingEvidence.down pendingEnd t := by
    exact ⟨t.own, (endLoan_iff ..).mp (by decide), rfl⟩
  have bad : ¬ t.WellFormed := by decide
  exact fun preserve => bad (preserve t legal (by decide))

/-- The mixed dispatcher can still end a live ordinary leaf when it has no pending obligation. -/
example : ∃ t, Issuance types ⟨child, []⟩ (.ordinary (.endLoan ⟨1⟩)) t ∧ t.WellFormed := by
  obtain ⟨t, _, legal⟩ :=
    admits (issuanceStep_iff types ⟨child, []⟩ (.ordinary (.endLoan ⟨1⟩))) (by decide)
  exact ⟨t, legal, issuance_wellFormed types legal (by decide)⟩

/-- Re-lending changes only the new pending child even if a malformed input reuses the supply. -/
example :
    let s : CallState := ⟨{ exclusive with nextLoan := 0 }, []⟩
    ∃ t, passDown s ⟨0⟩ y = some t ∧ t.own.loans.tail = s.own.loans :=
  ⟨(passDown ⟨{ exclusive with nextLoan := 0 }, []⟩ ⟨0⟩ y).getD baseCall, rfl, rfl⟩

/-- Mutant allowing an empty standalone entry to reuse the caller's activation. -/
def entryMayReuse (s : CallState) (e : IssuanceAction) (t : CallState) : Prop :=
  Issuance types s e t ∨ (e = .entry [] ∧ t = s)

/-- Instance (law `issuance_deterministic`, case `empty standalone entry`). -/
example : Issuance types baseCall (.entry []) { baseCall with own := pushFrame initial } ∧
    (∀ t, Issuance types baseCall (.entry []) t → t = { baseCall with own := pushFrame initial }) := by
  have step : Issuance types baseCall (.entry []) { baseCall with own := pushFrame initial } :=
    ⟨pushFrame initial, .nil _, rfl⟩
  exact ⟨step, fun _ other => issuance_deterministic types other step⟩

/-- Planted (law `issuance_deterministic`, mutant `entryMayReuse`). -/
example : ¬ UniqueStep entryMayReuse := by
  intro unique
  have original : entryMayReuse baseCall (.entry []) { baseCall with own := pushFrame initial } :=
    Or.inl ⟨pushFrame initial, .nil _, rfl⟩
  have alternative : entryMayReuse baseCall (.entry []) baseCall := Or.inr ⟨rfl, rfl⟩
  exact (by decide : { baseCall with own := pushFrame initial } ≠ baseCall)
    (unique original alternative)

/-- Instance (law `issuanceTrace_deterministic`, case `standalone entry trace`). -/
example : Trace (Issuance types) baseCall [.entry []] { baseCall with own := pushFrame initial } ∧
    (∀ t, Trace (Issuance types) baseCall [.entry []] t →
      t = { baseCall with own := pushFrame initial }) := by
  have trace : Trace (Issuance types) baseCall [.entry []] { baseCall with own := pushFrame initial } :=
    .cons ⟨pushFrame initial, .nil _, rfl⟩ (.nil _)
  exact ⟨trace, fun _ other => issuanceTrace_deterministic types other trace⟩

/-- Planted (law `issuanceTrace_deterministic`, mutant `entryMayReuse`). -/
example : ¬ UniqueStep (Trace entryMayReuse) := by
  intro unique
  have original : Trace entryMayReuse baseCall [.entry []] { baseCall with own := pushFrame initial } :=
    .cons (Or.inl ⟨pushFrame initial, .nil _, rfl⟩) (.nil _)
  have alternative : Trace entryMayReuse baseCall [.entry []] baseCall :=
    .cons (Or.inr ⟨rfl, rfl⟩) (.nil _)
  exact (by decide : { baseCall with own := pushFrame initial } ≠ baseCall)
    (unique original alternative)

end Controls

end LegalStep
end ConcreteSpec.Core.Own

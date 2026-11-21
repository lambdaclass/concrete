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
  ∃ f, s.frames.head? = some f ∧ ParameterAvailable f.views x ∧
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

/-- Law (authority: docs/decisions.md#od67; #od69). Binding uses checked descriptors and fresh places. -/
theorem bind_iff (types : Var → Option Descriptor) (s : State) (x : Var) (t : State) :
    bind types x s = some t ↔ Bind types s x t := by
  cases hf : s.frames.head? with
  | none => simp [bind, Bind, hf]
  | some f =>
    by_cases available : x ∈ List.map Prod.fst f.views
    · have blocked : ¬ ParameterAvailable f.views x := fun h => h available
      simp [bind, Bind, hf, available, blocked]
    have free : ParameterAvailable f.views x := available
    cases hp : AList.lookup f.locals x with
    | some p => simpa [bind, Bind, hf, hp, free, available] using reinitialize_iff s p t
    | none =>
      cases hd : types x <;> simp [bind, Bind, hf, hp, hd, free, available, eq_comm (a := t)]

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
  xs.Nodup ∧ Trace (Bind types) (pushFrame s) xs t

def EndLocals (s : State) (activation : Nat) (t : State) : Prop :=
  Trace (fun s l => EndLeafAtBoundary s l.id) s
    (s.loans.filter fun l => decide (l.activation = activation)) t

/-- Law (authority: docs/decisions.md#od23). Local ending processes loans newest first. -/
theorem endLocals_iff (s : State) (activation : Nat) (t : State) :
    endLocals activation s = some t ↔ EndLocals s activation t := by
  simpa [EndLocals, endLocals, Bool.beq_eq_decide_eq] using
    fold_iff (fun s (l : Loan) => endLeafAtBoundary s l.id) (fun s l t => endLeafAtBoundary_iff s l.id t)
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

/-- Law (authority: docs/decisions.md#od67; #decided-2026-11-13-lifecycle-refusal). Entry types each distinct binding. -/
theorem enter_iff (types : Var → Option Descriptor) (s : State) (xs : List Var) (t : State) :
    enter types xs s = some t ↔ Enter types s xs t := by
  by_cases h : xs.Nodup <;> simp [enter, Enter, h, pushFrame]
  exact fold_iff (fun s x => bind types x s) (bind_iff types) (pushFrame s) xs t

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
    (∀ e ∈ args, AList.lookup f.locals e.1 = none ∧ ParameterAvailable f.views e.1) ∧
    t = { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }

/-- Law (authority: docs/decisions.md#od23; #od67). Adoption transfers exactly the pending grants to free identities. -/
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
    (xs.Nodup ∧ ∀ e ∈ s.pending, e.1 ∈ xs) ∧ ∃ u v,
      Enter types s.own (xs.filter fun x => decide (x ∉ s.pending.map Prod.fst)) u ∧
      AdoptDown u s.pending v ∧ t = ⟨v, []⟩
  | .bind x => adaptLife types s (.bind x) t
  | .drop xs => adaptLife types s (.drop xs) t
  | .leave => adaptLife types s .leave t

/-- Law (authority: docs/decisions.md#od23; #od67). Pending arguments cross only matching entry. -/
theorem callLife_iff (types : Var → Option Descriptor) (s : CallState) (e : Exec.Life)
    (t : CallState) : callLife types e s = some t ↔ CallLife types s e t := by
  cases e with
  | enter xs =>
    simp [callLife, CallLife, Option.bind_eq_some_iff, enter_iff, adoptDown_iff,
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
    (AList.lookup f.locals e.1 = none ∧ ParameterAvailable f.views e.1) ∧ ReborrowTo s e.2 [] a.mode f.id u ∧
    t = { u with frames := { f with views := (e.1, ⟨s.nextLoan⟩) :: f.views } :: s.frames.tail }

def EnterDown (s : State) (args : List (Var × LoanId)) (t : State) : Prop :=
  CallerSources s args ∧ Trace DownwardParameter (pushFrame s) args t

/-- Law (authority: docs/decisions.md#od23). Each downward grant sees the ledger of earlier grants. -/
theorem enterDown_iff (s : State) (args : List (Var × LoanId)) (t : State) :
    enterDown s args = some t ↔ EnterDown s args t := by
  simp [enterDown, EnterDown, enter, pushFrame]
  intro _
  change args.foldlM enterDownParameter (pushFrame s) = some t ↔ _
  apply fold_iff
  intro v e u
  cases e with
  | mk x i =>
    cases ha : loan v i <;> cases hf : v.frames.head? <;>
      simp [DownwardParameter, enterDownParameter, ha, hf, reborrowTo_iff,
        Option.bind_eq_some_iff, eq_comm (a := u), exists_and_left]

def PassDown (s : CallState) (i : LoanId) (x : Var) (t : CallState) : Prop :=
  ParameterAvailable s.pending x ∧ ∃ a f,
    loan s.own i = some a ∧ LoanPermission s.own i a.mode ∧
    s.own.frames.head? = some f ∧ f.id < s.own.nextActivation ∧
    ChildGrant s.own t.own a i [] a.mode f.id true ∧
    t.pending = s.pending ++ [(x, ⟨s.own.nextLoan⟩)]

/-- Law (authority: docs/decisions.md#od23). A view argument grants a fresh pending child. -/
theorem passDown_iff (s : CallState) (i : LoanId) (x : Var) (t : CallState) :
    passDown s i x = some t ↔ PassDown s i x t := by
  cases t with
  | mk own pending =>
    cases h : s.own.frames.head? <;> cases ha : loan s.own i <;>
      simp [passDown, PassDown, ParameterAvailable, ChildGrant, h, ha, Bool.and_eq_true,
        loanAccess_iff, and_assoc, eq_comm (a := own), eq_comm (a := pending)]

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
  | .entry args => s.pending = [] ∧ ∃ u, EnterDown s.own args u ∧ t = { s with own := u }
  | .lifecycle e => CallLife types s e t

/-- Law (authority: docs/decisions.md#od15; #od23). Issuance realizes each ledger rule. -/
theorem issuanceStep_iff (types : Var → Option Descriptor) (s : CallState)
    (e : IssuanceAction) (t : CallState) : issuanceStep types s e = some t ↔ Issuance types s e t := by
  cases e with
  | ordinary op =>
    by_cases h : OrdinaryIssuanceAllowed s op <;>
      simp [issuanceStep, Issuance, h, Option.map_eq_some_iff, loanAction_iff, eq_comm (a := t)]
  | entry args =>
    by_cases h : s.pending = [] <;>
      simp [issuanceStep, Issuance, h, Option.map_eq_some_iff, enterDown_iff, eq_comm (a := t)]
  | owner | view | lifecycle =>
    simp [issuanceStep, Issuance, passOwner_iff, passDown_iff, callLife_iff]

/-- Law (authority: docs/decisions.md#od15; #od23). Issuance traces compose legal steps. -/
theorem issuanceTrace_iff (types : Var → Option Descriptor) (s : CallState)
    (es : List IssuanceAction) (t : CallState) :
    es.foldlM (issuanceStep types) s = some t ↔ Trace (Issuance types) s es t :=
  fold_iff (issuanceStep types) (issuanceStep_iff types) s es t

def UniqueStep {α β : Type} (rule : α → β → α → Prop) : Prop :=
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
  unfold ReborrowTo ChildGrant at hu hv
  grind

private theorem loanStep_unique : UniqueStep LoanStep := by
  intro s e u v hu hv
  cases e with
  | reborrow => exact reborrowTo_unique hu hv
  | borrow => unfold LoanStep Borrow at hu hv; grind
  | endLoan => unfold LoanStep EndLoan EndLeafAtBoundary at hu hv; grind

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
  | enter => exact trace_unique (bind_unique types) hu.2 hv.2
  | leave =>
    obtain ⟨f, a, b, hf, ha, hb, rfl⟩ := hu
    obtain ⟨g, c, d, hg, hc, hd, rfl⟩ := hv
    have same := Option.some.inj (hf.symm.trans hg)
    subst g
    have endUnique : UniqueStep (fun s (l : Loan) t => EndLeafAtBoundary s l.id t) := by
      intro s l u v hu hv
      exact hu.2.2.trans hv.2.2.symm
    have same := trace_unique endUnique ha hc
    subst c
    rw [drop_unique hb hd]

private theorem callLife_unique (types : Var → Option Descriptor) : UniqueStep (CallLife types) := by
  intro s e u v hu hv
  cases e with
  | enter =>
    obtain ⟨_, a, b, ha, hb, rfl⟩ := hu
    obtain ⟨_, c, d, hc, hd, rfl⟩ := hv
    have same := trace_unique (bind_unique types) ha.2 hc.2
    subst c
    unfold AdoptDown at hb hd
    grind
  | bind | leave | drop =>
    obtain ⟨_, a, ha, rfl⟩ := hu
    obtain ⟨_, b, hb, rfl⟩ := hv
    rw [life_unique types ha hb]

private theorem downwardParameter_unique : UniqueStep DownwardParameter := by
  intro s e u v hu hv
  obtain ⟨a, f, b, ha, hf, _, hb, rfl⟩ := hu
  obtain ⟨c, g, d, hc, hg, _, hd, rfl⟩ := hv
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
  | view =>
    cases u; cases v
    unfold Issuance PassDown ChildGrant at hu hv
    grind
  | entry =>
    obtain ⟨_, a, ha, rfl⟩ := hu
    obtain ⟨_, b, hb, rfl⟩ := hv
    rw [trace_unique downwardParameter_unique ha.2 hb.2]
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
    obtain ⟨allowed, ⟨a, ha⟩, clear, rfl⟩ := h
    exact .endLoan ⟨a, ha, allowed, clear⟩

/-- Helper (constructor enumeration). Every event names exactly its relational rule. -/
theorem admission_cases (types : Var → Option Descriptor) (s : CallState) :
    (∀ e, Admitted (Life types) s.own e ↔ match e with
      | .bind x => ∃ t, Bind types s.own x t
      | .enter xs => ∃ t, Enter types s.own xs t
      | .leave => ∃ t, Leave s.own t
      | .drop xs => ∃ t, Drop s.own xs t) ∧
    (∀ e, Admitted (CallLife types) s e ↔ match e with
      | .bind x => ∃ t, adaptLife types s (.bind x) t
      | .enter xs => (xs.Nodup ∧ ∀ e ∈ s.pending, e.1 ∈ xs) ∧ ∃ u v,
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
      | .entry args => s.pending = [] ∧ Admitted EnterDown s.own args
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
      · rintro ⟨_, empty, u, hu, _⟩
        exact ⟨empty, u, hu⟩
      · rintro ⟨empty, u, hu⟩
        exact ⟨{ s with own := u }, empty, u, hu, rfl⟩

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
  have consistent : PendingConsistent s.pending s.own := ⟨wf.2.2.2.1, wf.2.2.2.2.1⟩
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
    obtain ⟨_, _, _, rfl⟩ := step
    constructor
    · intro e he
      obtain ⟨l, hl, eq⟩ := consistent.1 e he
      refine ⟨l, List.mem_filter.mpr ⟨hl, ?_⟩, eq⟩
      simp only [bne_iff_ne]
      intro same
      exact allowed (List.mem_map.mpr ⟨e, he, eq.symm.trans same⟩)
    · intro l hl
      exact consistent.2 l (List.mem_filter.mp hl).1

private theorem loanStep_liveViews {s t : State} {e : LoanAction}
    (step : LoanStep s e t) (live : LiveViews s) : LiveViews t := by
  cases e with
  | borrow =>
    obtain ⟨_, _, _, _, rfl⟩ := step
    exact live.retain rfl (fun l hl => List.mem_cons_of_mem _ hl)
  | reborrow =>
    obtain ⟨_, _, _, _, _, rfl⟩ := step
    exact live.retain rfl (fun l hl => List.mem_cons_of_mem _ hl)
  | endLoan i =>
    obtain ⟨allowed, _, _, rfl⟩ := step
    intro frame hf binding hb
    obtain ⟨l, hl, id, rest⟩ := live frame hf binding hb
    obtain ⟨f, hm, rfl⟩ := List.mem_map.mp hf
    refine ⟨l, List.mem_filter.mpr ⟨hl, ?_⟩, id, rest⟩
    simp only [bne_iff_ne]
    intro eq
    exact allowed.2 f hm (List.mem_map.mpr ⟨binding, hb, id.symm.trans eq⟩)

private theorem loanStep_liveDestinations {s t : State} {e : LoanAction}
    (step : LoanStep s e t) (live : LiveDestinations s) : LiveDestinations t := by
  cases e with
  | borrow =>
    obtain ⟨_, f, frame, _, rfl⟩ := step
    intro l hl
    rcases List.mem_cons.mp hl with rfl | old
    · exact ⟨f, List.mem_of_head? frame, rfl⟩
    · exact live l old
  | reborrow i path m a =>
    have safe := reborrowTo_live s i path m a live
    rwa [(reborrowTo_iff ..).mpr step] at safe
  | endLoan =>
    obtain ⟨_, _, _, rfl⟩ := step
    exact fun l hl => live l (List.mem_filter.mp hl).1

private theorem loanStep_frames {s t : State} {e : LoanAction}
    (step : LoanStep s e t) : t.frames = s.frames := by
  cases e with
  | borrow => obtain ⟨_, _, _, _, rfl⟩ := step; rfl
  | reborrow => obtain ⟨_, _, _, _, _, rfl⟩ := step; rfl
  | endLoan => obtain ⟨_, _, _, rfl⟩ := step; rfl

private theorem downwardTrace_preserves {pending : List (Var × LoanId)} {s t : State}
    {args : List (Var × LoanId)} (step : Trace DownwardParameter s args t)
    (initial : WellFormed s) (keep : PendingConsistent pending s) (live : LiveViews s)
    (destinations : LiveDestinations s) (unique : UniqueFrameBindings s) :
    PendingConsistent pending t ∧ LiveViews t ∧ LiveDestinations t ∧ UniqueFrameBindings t := by
  induction step with
  | nil => exact ⟨keep, live, destinations, unique⟩
  | @cons s u₀ t e es grant _ ih =>
    obtain ⟨a, f, u, _, frame, available, reborrow, rfl⟩ := grant
    have preserve := loanStep_wellFormed (e := .reborrow _ [] a.mode f.id) reborrow initial
    have frames : u.frames = s.frames := by
      obtain ⟨_, _, _, _, _, rfl⟩ := reborrow
      rfl
    have next := set_views_preserves u f ((e.1, ⟨s.nextLoan⟩) :: f.views) preserve
      (by simpa [frames] using frame)
    have fresh := nextLoan_not_pending keep initial
    have liveGrant := loanStep_liveDestinations (e := .reborrow _ [] a.mode f.id)
      reborrow destinations
    have liveNext : LiveDestinations
        { u with frames := { f with views := (e.1, ⟨s.nextLoan⟩) :: f.views } :: s.frames.tail } := by
      intro l hl
      obtain ⟨g, hg, id⟩ := liveGrant l hl
      rw [frames] at hg
      cases stack : s.frames with
      | nil => simp [stack] at frame
      | cons head tail =>
        simp [stack] at frame
        subst head
        rw [stack] at hg
        rcases List.mem_cons.mp hg with rfl | old
        · exact ⟨_, List.mem_cons_self, id⟩
        · exact ⟨g, List.mem_cons_of_mem _ old, id⟩
    have uniqueNext : UniqueFrameBindings
        { u with frames := { f with views := (e.1, ⟨s.nextLoan⟩) :: f.views } :: s.frames.tail } := by
      apply uniqueFrameBindings_replaceHead unique frame
      have old := List.nodup_append.mp (unique f (List.mem_of_head? frame))
      simp only [Activation.UniqueBindings, List.map_cons, List.nodup_append, List.nodup_cons]
      refine ⟨old.1, ⟨available.2, old.2.1⟩, ?_⟩
      intro x hx y hy eq
      rcases List.mem_cons.mp hy with rfl | earlier
      · exact localIdentityAbsent _ _ available.1 (eq ▸ hx)
      · exact old.2.2 x hx y earlier eq
    obtain ⟨_, _, _, _, _, rfl⟩ := reborrow
    refine ih next (pending_cons keep rfl rfl fresh) ?_ liveNext uniqueNext
    intro g hg binding hb
    change g ∈ (f.id, (e.1, ⟨s.nextLoan⟩) :: f.views) ::
      s.frames.tail.map (fun g => (g.id, g.views)) at hg
    rcases List.mem_cons.mp hg with rfl | old
    · rcases List.mem_cons.mp hb with rfl | old
      · exact ⟨_, List.mem_cons_self, rfl, rfl, rfl⟩
      · obtain ⟨l, hl, id, rest⟩ := live (f.id, f.views)
          (List.mem_map.mpr ⟨f, List.mem_of_head? frame, rfl⟩) binding old
        exact ⟨l, List.mem_cons_of_mem _ hl, id, rest⟩
    · obtain ⟨g, gTail, rfl⟩ := List.mem_map.mp old
      obtain ⟨l, hl, id, rest⟩ := live (g.id, g.views)
        (List.mem_map.mpr ⟨g, List.mem_of_mem_tail gTail, rfl⟩) binding hb
      exact ⟨l, List.mem_cons_of_mem _ hl, id, rest⟩

/-- Law (authority: docs/decisions.md#od15; #od23). Each legal issuance step preserves the
ownership ledger and its pending call buffer together. -/
theorem issuance_wellFormed (types : Var → Option Descriptor) {s t : CallState}
    {e : IssuanceAction} (step : Issuance types s e t) (wf : s.WellFormed) : t.WellFormed := by
  cases e with
  | ordinary op =>
    obtain ⟨allowed, u, hu, rfl⟩ := step
    have pending := ordinary_pending (s := s) allowed hu wf
    exact ⟨loanStep_wellFormed hu wf.1, wf.2.1, wf.2.2.1, pending.1, pending.2,
      loanStep_liveViews hu wf.2.2.2.2.2.1, loanStep_liveDestinations hu wf.2.2.2.2.2.2.1,
      by simpa only [UniqueFrameBindings, loanStep_frames hu] using wf.2.2.2.2.2.2.2⟩
  | entry args =>
    obtain ⟨_, u, hu, rfl⟩ := step
    have safe := enterDown_preserves s.own args wf.1
    rw [(enterDown_iff ..).mpr hu] at safe
    have initial : WellFormed (pushFrame s.own) := enter_preserves (fun _ => none) s.own [] wf.1
    have views : LiveViews (pushFrame s.own) := by
      intro f hf binding hb
      change f ∈ (s.own.nextActivation, []) :: ViewIndex s.own at hf
      rcases List.mem_cons.mp hf with rfl | old
      · simp at hb
      · exact wf.2.2.2.2.2.1 f old binding hb
    have destinations : LiveDestinations (pushFrame s.own) := by
      intro l hl
      obtain ⟨f, hf, id⟩ := wf.2.2.2.2.2.2.1 l hl
      exact ⟨f, List.mem_cons_of_mem _ hf, id⟩
    have unique : UniqueFrameBindings (pushFrame s.own) := by
      intro f hf
      rcases List.mem_cons.mp hf with rfl | old
      · exact List.nodup_nil
      · exact wf.2.2.2.2.2.2.2 f old
    have keep := downwardTrace_preserves hu.2 initial
      ⟨wf.2.2.2.1, wf.2.2.2.2.1⟩ views destinations unique
    exact ⟨safe, wf.2.1, wf.2.2.1, keep.1.1, keep.1.2, keep.2⟩
  | owner p path m x =>
    have safe := passOwner_call_preserves s p path m x wf
    rwa [(passOwner_iff ..).mpr step] at safe
  | view i x =>
    have safe := passDown_call_preserves s i x wf
    rwa [(passDown_iff ..).mpr step] at safe
  | lifecycle => exact callLife_wellFormed step wf

/-- Events that can occur while evaluating a call or executing its body. -/
inductive LedgerEvent where
  | issue (event : IssuanceAction)
  | move (place : Hybrid.Place)
  | accessOwner (place : Hybrid.Place) (mode : Mode)
  | accessLoan (loan : LoanId) (mode : Mode)

def LedgerStep (types : Var → Option Descriptor) (s : CallState)
    (event : LedgerEvent) (t : CallState) : Prop :=
  match event with
  | .issue e => Issuance types s e t
  | .move p => Move s.own p t.own ∧ t.pending = s.pending
  | .accessOwner p m => AccessOwner s.own p m t.own ∧ t.pending = s.pending
  | .accessLoan i m => LiveDestinations s.own ∧ AccessLoan s.own i m t.own ∧ t.pending = s.pending

/-- Law (authority: docs/decisions.md#decided-2026-11-08-recovered-d3).
Loan access requires live destinations as well as ordinary loan permission. -/
theorem ledgerAccessLoan_iff (types : Var → Option Descriptor) (s t : CallState)
    (i : LoanId) (m : Mode) :
    LedgerStep types s (.accessLoan i m) t ↔
      LiveDestinations s.own ∧ accessLoan s.own i m = some t.own ∧ t.pending = s.pending := by
  simp only [LedgerStep, accessLoan_iff]

/-- Helper (preservation). Every ledger event preserves the call metadata. -/
theorem ledgerStep_wellFormed (types : Var → Option Descriptor) {s t : CallState}
    {e : LedgerEvent} (step : LedgerStep types s e t) (wf : s.WellFormed) : t.WellFormed := by
  cases s with
  | mk own pending =>
    cases t with
    | mk next nextPending =>
      cases e with
      | issue e => exact issuance_wellFormed types step wf
      | move p =>
        obtain ⟨legal, pendingEq⟩ := step
        change nextPending = pending at pendingEq
        subst nextPending
        have safe := move_preserves own p wf.1
        rw [(move_iff own p next).mpr legal] at safe
        obtain ⟨_, rfl⟩ := legal
        exact ⟨safe, wf.2⟩
      | accessOwner p m =>
        obtain ⟨⟨_, rfl⟩, rfl⟩ := step
        exact wf
      | accessLoan i m =>
        obtain ⟨_, ⟨_, rfl⟩, rfl⟩ := step
        exact wf

/-- Law (authority: docs/decisions.md#od15; #od23). Legal ledger histories preserve
ownership and call metadata through access, move and issuance. -/
theorem ledgerTrace_wellFormed (types : Var → Option Descriptor) {s t : CallState}
    {events : List LedgerEvent} (trace : Trace (LedgerStep types) s events t)
    (wf : s.WellFormed) : t.WellFormed := by
  induction trace with
  | nil => exact wf
  | cons step _ ih => exact ih (ledgerStep_wellFormed types step wf)

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
theorem life_iff_planted_1 : ¬ (∀ t, keepOnLifeRefusal .leave initial = some t ↔ Life types initial .leave t) :=
  rejects (life_iff types initial .leave) (by decide)

/-- Planted (law `runLoanSequence_iff`, mutant `keepOnSequenceRefusal`). A live child blocks end. -/
theorem runLoanSequence_iff_planted_1 : ¬ (∀ t, keepOnSequenceRefusal child [.endLoan ⟨0⟩] = some t ↔
    LoanSequence child [.endLoan ⟨0⟩] t) :=
  rejects (runLoanSequence_iff child [.endLoan ⟨0⟩]) (by decide)

def lifeWithoutDropDischarge (e : Exec.Life) (s : State) : Option State :=
  match e with
  | .drop xs => DispatchEvidence.dropWithoutDischarge s xs
  | .bind _ | .enter _ | .leave => life types e s

/-- Planted (law `life_iff`, mutant `lifeWithoutDropDischarge`). An unborrowed owner is still linear. -/
theorem life_iff_planted_2 : ¬ (∀ t, lifeWithoutDropDischarge (.drop [x]) initial = some t ↔
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
theorem life_iff_planted_3 : ¬ (∀ t, lifeWithDefaultBinding (.bind y) initial = some t ↔
    Life untyped initial (.bind y) t) :=
  rejects (life_iff untyped initial (.bind y)) (by decide)

/-- Planted (law `life_iff`, mutant `lifeWithDefaultEntry`). Entry still allocates its activation. -/
theorem life_iff_planted_4 : ¬ (∀ t, lifeWithDefaultEntry (.enter [y]) initial = some t ↔
    Life untyped initial (.enter [y]) t) :=
  rejects (life_iff untyped initial (.enter [y])) (by decide)

def placeOnlyRetired : CallState := ⟨RetirementEvidence.retired, []⟩

def rewindPlace (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .drop [] => (callLife types e s).map fun t => { t with own := { t.own with nextPlace := 0 } }
  | .drop (_ :: _) | .enter _ | .leave | .bind _ => callLife types e s

/-- Planted (law `callLife_iff`, mutant `rewindPlace`). Only the place supply changes. -/
theorem callLife_iff_planted_1 : RetiredPlace placeOnlyRetired.own ⟨0⟩ ∧ placeOnlyRetired.own.nextLoan = 0 ∧
    ¬ (∀ t, rewindPlace (.drop []) placeOnlyRetired = some t ↔
      CallLife types placeOnlyRetired (.drop []) t) :=
  ⟨by decide, rfl, rejects (callLife_iff types placeOnlyRetired (.drop [])) (by decide)⟩

def refuseProjected (s : State) (i : LoanId) (path : List Projection) (m : Mode)
    (a : Nat) : Option State :=
  if path.isEmpty then reborrowTo s i path m a else none

/-- Planted (law `reborrowTo_iff`, mutant `refuseProjected`). Refusal is at the lending primitive. -/
theorem reborrowTo_iff_planted_1 : ¬ (∀ t, refuseProjected LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ↔
    ReborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 t) :=
  rejects (reborrowTo_iff LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1) (by decide)

/-- Planted (law `runLoanSequence_iff`, mutant `DispatchEvidence.refuseRunLoanSequence`). -/
theorem runLoanSequence_iff_planted_2 : ¬ (∀ t, DispatchEvidence.refuseRunLoanSequence initial [] = some t ↔
    LoanSequence initial [] t) :=
  rejects (runLoanSequence_iff initial []) (by decide)

/-- Planted (law `issuanceStep_iff`, mutant `DispatchEvidence.refuseIssuanceStep`). -/
theorem issuanceStep_iff_planted_1 : ¬ (∀ t, DispatchEvidence.refuseIssuanceStep ⟨initial, []⟩ (.entry []) = some t ↔
    Issuance types ⟨initial, []⟩ (.entry []) t) :=
  rejects (issuanceStep_iff types ⟨initial, []⟩ (.entry [])) (by decide)

def ignorePendingOnDrop (e : Exec.Life) (s : CallState) : Option CallState :=
  match e with
  | .drop _ => (life types e s.own).map fun own => { s with own := own }
  | .enter _ | .leave | .bind _ => callLife types e s

/-- Planted (law `callLife_iff`, mutant `ignorePendingOnDrop`). Pending arguments block teardown. -/
theorem callLife_iff_planted_2 : ¬ (∀ t, ignorePendingOnDrop (.drop []) PendingEvidence.down = some t ↔
    CallLife types PendingEvidence.down (.drop []) t) :=
  rejects (callLife_iff types PendingEvidence.down (.drop [])) (by decide)

open DispatchEvidence

/-- Planted (law `life_iff`, mutant `acceptLiveReturn`). Exit must discharge each local. -/
theorem life_iff_planted_5 : ¬ (∀ t, acceptLiveReturn .leave initial = some t ↔ Life types initial .leave t) :=
  rejects (life_iff types initial .leave) (by decide)

/-- Planted (law `life_iff`, mutant `acceptBorrowedDrop`). Copy storage still requires loan exclusion. -/
theorem life_iff_planted_6 : ¬ (∀ t, acceptBorrowedDrop (.drop [x]) borrowedCopy = some t ↔
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
theorem passOwner_iff_planted_1 : ¬ (∀ t, acceptFrozenOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y = some t ↔
    PassOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y t) :=
  rejects (fun t => passOwner_iff _ _ _ _ _ t) (by decide)

/-- Planted (law `passDown_iff`, mutant `acceptFrozenView`). An existing child can block lending. -/
theorem passDown_iff_planted_1 : ¬ (∀ t, acceptFrozenView ⟨child, []⟩ ⟨0⟩ y = some t ↔ PassDown ⟨child, []⟩ ⟨0⟩ y t) :=
  rejects (fun t => passDown_iff _ _ _ t) (by decide)

/-- Planted (law `enterDown_iff`, mutant `acceptFrozenDownward`). The first grant freezes the source. -/
theorem enterDown_iff_planted_1 : ¬ (∀ t, acceptFrozenDownward exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] = some t ↔
    EnterDown exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] t) :=
  rejects (fun t => enterDown_iff _ _ t) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseCallBinding`). -/
theorem callLife_iff_planted_3 : ¬ (∀ t, refuseCallBinding (.bind DirectArgumentWitnesses.z) DirectArgumentWitnesses.caller = some t ↔
    CallLife types DirectArgumentWitnesses.caller (.bind DirectArgumentWitnesses.z) t) :=
  rejects (fun t => callLife_iff _ _ _ t) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseMixedCallEntry`). -/
theorem callLife_iff_planted_4 : ¬ (∀ t, refuseMixedCallEntry (.enter [x, y, DirectArgumentWitnesses.z])
      Round5Evidence.twoWaiting = some t ↔
    CallLife types Round5Evidence.twoWaiting (.enter [x, y, DirectArgumentWitnesses.z]) t) :=
  rejects (fun t => callLife_iff _ _ _ t) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseNonemptyCallReturn`). Each admissible return succeeds. -/
theorem callLife_iff_planted_5 :
    (¬ (∀ t, refuseNonemptyCallReturn .leave localReturn = some t ↔ CallLife types localReturn .leave t)) ∧
    (¬ (∀ t, refuseMultipleLoanReturn .leave ⟨Round4Evidence.localChild, []⟩ = some t ↔
      CallLife types ⟨Round4Evidence.localChild, []⟩ .leave t)) :=
  ⟨rejects (fun t => callLife_iff _ _ _ t) (by decide), rejects (fun t => callLife_iff _ _ _ t) (by decide)⟩

/-- Planted (law `life_iff`, mutants `refuseLifeBind` and `refuseLifeEnter`). -/
theorem life_iff_planted_7 :
    (¬ (∀ t, refuseLifeBind (.bind y) initial = some t ↔ Life types initial (.bind y) t)) ∧
    (¬ (∀ t, refuseLifeEnter (.enter []) initial = some t ↔ Life types initial (.enter []) t)) :=
  ⟨rejects (fun t => life_iff _ _ _ t) (by decide), rejects (fun t => life_iff _ _ _ t) (by decide)⟩

def refuseStoreDrop (e : Exec.Life) (s : Store Nat) : Option (Store Nat) :=
  match e with
  | .drop [] => if s.concrete = 7 then none else lifeStore types e s
  | .drop (_ :: _) | .enter _ | .leave | .bind _ => lifeStore types e s

/-- Planted (law `lifeStore_iff`, mutant `refuseStoreDrop`). Concrete payload cannot veto teardown. -/
theorem lifeStore_iff_planted_1 : ¬ (∀ t, refuseStoreDrop (.drop []) ⟨7, initial⟩ = some t ↔
    LifeStore types (⟨7, initial⟩ : Store Nat) (.drop []) t) :=
  rejects (lifeStore_iff types ⟨7, initial⟩ (.drop [])) (by decide)

/-- Planted (law `callLifeStore_iff`, mutant `resetStore`). Concrete payload must survive entry. -/
theorem callLifeStore_iff_planted_1 : ¬ (∀ t, resetStore (.enter []) (7, nestedCaller) = some t ↔
    CallLifeStore types (7, nestedCaller) (.enter []) t) :=
  rejects (callLifeStore_iff types (7, nestedCaller) (.enter [])) (by decide)

/-- Planted (law `enterDown_iff`, mutant `refuseStandaloneEntry`). -/
theorem enterDown_iff_planted_2 : ¬ (∀ t, refuseStandaloneEntry exclusive singletonEntry = some t ↔
    EnterDown exclusive singletonEntry t) :=
  rejects (enterDown_iff exclusive singletonEntry) (by decide)

/-- Planted (law `adoptDown_iff`, mutant `refuseMultiAdopt`). -/
theorem adoptDown_iff_planted_1 : ¬ (∀ t, refuseMultiAdopt Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending = some t ↔
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t) :=
  rejects (adoptDown_iff Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending) (by decide)

/-- Planted (law `move_iff`, mutant `refuseNonzeroMove`). -/
theorem move_iff_planted_1 : ¬ (∀ t, refuseNonzeroMove MoveLifecycleEvidence.source ⟨1⟩ = some t ↔
    Move MoveLifecycleEvidence.source ⟨1⟩ t) :=
  rejects (move_iff MoveLifecycleEvidence.source ⟨1⟩) (by decide)

/-- Planted (law `passDown_iff`, mutant `refuseSecondPendingView`). -/
theorem passDown_iff_planted_2 : ¬ (∀ t, refuseSecondPendingView PendingEvidence.down ⟨0⟩ y = some t ↔
    PassDown PendingEvidence.down ⟨0⟩ y t) :=
  rejects (passDown_iff PendingEvidence.down ⟨0⟩ y) (by decide)

/-- Planted (law `callLife_iff`, mutant `refuseEmptyCallEntry`). -/
theorem callLife_iff_planted_6 : ¬ (∀ t, refuseEmptyCallEntry (.enter []) nestedCaller = some t ↔
    CallLife types nestedCaller (.enter []) t) :=
  rejects (callLife_iff types nestedCaller (.enter [])) (by decide)

/-- Planted (law `move_iff`, mutant `acceptBorrowedMove`). -/
theorem move_iff_planted_2 : ¬ (∀ t, acceptBorrowedMove exclusive ⟨0⟩ = some t ↔ Move exclusive ⟨0⟩ t) :=
  rejects (move_iff exclusive ⟨0⟩) (by decide)

/-- Planted (law `adoptDown_iff`, mutant `PreservationEvidence.adoptDownUnissuedActivation`). -/
theorem adoptDown_iff_planted_2 : ¬ (∀ t, PreservationEvidence.adoptDownUnissuedActivation Round5Evidence.twoBefore
    Round5Evidence.twoWaiting.pending = some t ↔
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t) :=
  rejects (adoptDown_iff Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending) (by decide)

/-- Planted (law `callLife_iff`, mutant `PreservationEvidence.callLifeKeepPending`). -/
theorem callLife_iff_planted_7 : ¬ (∀ t, PreservationEvidence.callLifeKeepPending types (.enter [y, DirectArgumentWitnesses.z])
      Round5Evidence.twoWaiting = some t ↔
    CallLife types Round5Evidence.twoWaiting (.enter [y, DirectArgumentWitnesses.z]) t) :=
  rejects (callLife_iff types Round5Evidence.twoWaiting (.enter [y, DirectArgumentWitnesses.z])) (by decide)

/-- Planted (law `life_iff`, mutant `refuseReinitBind`). -/
theorem life_iff_planted_8 : ¬ (∀ t, refuseReinitBind (.bind DirectArgumentWitnesses.z) localReturn.own = some t ↔
    Life types localReturn.own (.bind DirectArgumentWitnesses.z) t) :=
  rejects (life_iff types localReturn.own (.bind DirectArgumentWitnesses.z)) (by decide)

/-- Planted (law `runLoanSequence_iff`, mutant `LoanHistoryEvidence.recycle`). -/
theorem runLoanSequence_iff_planted_3 : ¬ (∀ t, LoanHistoryEvidence.recycle LoanHistoryEvidence.endedChild LoanHistoryEvidence.actions =
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

def adoptedEnd : IssuanceAction := .ordinary (.endLoan ⟨1⟩)

/-- Retain the pending-loan check, but omit protection for an adopted parameter binding. -/
def endWithoutBindingGuard (s : State) (i : LoanId) : Option State :=
  if ∀ a ∈ s.loans, a.id = i → a.pendingArgument = false then
    endLeafAtBoundary s i
  else none

/-- Substitute the mutant through the actual ordinary issuance branch. -/
def endAdopted (s : CallState) : IssuanceAction → Option CallState
  | .ordinary (.endLoan i) =>
    if OrdinaryIssuanceAllowed s (.endLoan i) then
      (endWithoutBindingGuard s.own i).map fun own => { s with own := own }
    else none
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
theorem ownerAccess_iff_instance_1 : ownerAccess initial ⟨0⟩ .mutable = true ∧ OwnerPermission initial ⟨0⟩ .mutable := ⟨rfl, (ownerAccess_iff _ _ _).mp (show ownerAccess initial ⟨0⟩ .mutable = true from rfl)⟩

/-- Planted (law `ownerAccess_iff`, direction over-accepting). Omit exclusion. -/
theorem ownerAccess_iff_planted_1 : ¬ (ownerWithoutExclusion exclusive ⟨0⟩ .mutable = true ↔ OwnerPermission exclusive ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (ownerAccess exclusive ⟨0⟩ .mutable)) (ownerAccess_iff _ _ _) (by decide)

/-- Planted (law `ownerAccess_iff`, direction over-refusing). Deny permitted access. -/
theorem ownerAccess_iff_planted_2 : ¬ (refuseMutableOwner initial ⟨0⟩ .mutable = true ↔ OwnerPermission initial ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (ownerAccess initial ⟨0⟩ .mutable)) (ownerAccess_iff _ _ _) (by decide)

/-- Instance (law `loanAccess_iff`, case `initial access`). -/
theorem loanAccess_iff_instance_1 : loanAccess exclusive ⟨0⟩ .mutable = true ∧ LoanPermission exclusive ⟨0⟩ .mutable := ⟨rfl, (loanAccess_iff _ _ _).mp (show loanAccess exclusive ⟨0⟩ .mutable = true from rfl)⟩

/-- Planted (law `loanAccess_iff`, direction over-accepting). Omit exclusion. -/
theorem loanAccess_iff_planted_1 : ¬ (loanWithoutExclusion child ⟨0⟩ .mutable = true ↔ LoanPermission child ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (loanAccess child ⟨0⟩ .mutable)) (loanAccess_iff _ _ _) (by decide)

/-- Planted (law `loanAccess_iff`, direction over-refusing). Deny permitted access. -/
theorem loanAccess_iff_planted_2 : ¬ (refuseMutableLoan exclusive ⟨0⟩ .mutable = true ↔ LoanPermission exclusive ⟨0⟩ .mutable) :=
  rejectsPermission (actual := (loanAccess exclusive ⟨0⟩ .mutable)) (loanAccess_iff _ _ _) (by decide)

/-- Instance (law `accessOwner_iff`, case `admitted operation`). -/
theorem accessOwner_iff_instance_1 : ∃ t, accessOwner initial ⟨0⟩ .mutable = some t ∧ AccessOwner initial ⟨0⟩ .mutable t :=
  admits (fun t => accessOwner_iff _ _ _ t) (by decide)

/-- Planted (law `accessOwner_iff`, direction over-accepting). -/
theorem accessOwner_iff_planted_1 : ¬ (∀ t, accessWith (ownerWithoutExclusion exclusive ⟨0⟩ .mutable) exclusive = some t ↔
    AccessOwner exclusive ⟨0⟩ .mutable t) :=
  rejectsAcceptance (actual := (accessOwner exclusive ⟨0⟩ .mutable))
    (fun t => accessOwner_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `accessOwner_iff`, direction over-refusing). -/
theorem accessOwner_iff_planted_2 : ¬ (∀ t, accessWith (refuseMutableOwner initial ⟨0⟩ .mutable) initial = some t ↔
    AccessOwner initial ⟨0⟩ .mutable t) :=
  rejectsRefusal (actual := (accessOwner initial ⟨0⟩ .mutable))
    (fun t => accessOwner_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `accessLoan_iff`, case `admitted operation`). -/
theorem accessLoan_iff_instance_1 : ∃ t, accessLoan exclusive ⟨0⟩ .mutable = some t ∧ AccessLoan exclusive ⟨0⟩ .mutable t :=
  admits (fun t => accessLoan_iff _ _ _ t) (by decide)

/-- Planted (law `accessLoan_iff`, direction over-accepting). -/
theorem accessLoan_iff_planted_1 : ¬ (∀ t, accessWith (loanWithoutExclusion child ⟨0⟩ .mutable) child = some t ↔
    AccessLoan child ⟨0⟩ .mutable t) :=
  rejectsAcceptance (actual := (accessLoan child ⟨0⟩ .mutable))
    (fun t => accessLoan_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `accessLoan_iff`, direction over-refusing). -/
theorem accessLoan_iff_planted_2 : ¬ (∀ t, accessWith (refuseMutableLoan exclusive ⟨0⟩ .mutable) exclusive = some t ↔
    AccessLoan exclusive ⟨0⟩ .mutable t) :=
  rejectsRefusal (actual := (accessLoan exclusive ⟨0⟩ .mutable))
    (fun t => accessLoan_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `reinitialize_iff`, case `admitted operation`). -/
theorem reinitialize_iff_instance_1 : ∃ t, reinitialize LifecycleWitnesses.moved ⟨0⟩ = some t ∧
    Reinitialize LifecycleWitnesses.moved ⟨0⟩ t :=
  admits (fun t => reinitialize_iff _ _ t) (by decide)

/-- Planted (law `reinitialize_iff`, direction over-accepting). -/
theorem reinitialize_iff_planted_1 : ¬ (∀ t, overwriteLinear initial ⟨0⟩ = some t ↔ Reinitialize initial ⟨0⟩ t) :=
  rejectsAcceptance (actual := (reinitialize initial ⟨0⟩))
    (fun t => reinitialize_iff _ _ t) (by decide) (by decide)

/-- Planted (law `reinitialize_iff`, direction over-refusing). -/
theorem reinitialize_iff_planted_2 : ¬ (∀ t, refuseMovedSlot LifecycleWitnesses.moved ⟨0⟩ = some t ↔
    Reinitialize LifecycleWitnesses.moved ⟨0⟩ t) :=
  rejectsRefusal (actual := (reinitialize LifecycleWitnesses.moved ⟨0⟩))
    (fun t => reinitialize_iff _ _ t) (by decide) (by decide)

/-- Instance (law `bind_iff`, case `admitted operation`). -/
theorem bind_iff_instance_1 : ∃ t, bind types y initial = some t ∧ Bind types initial y t :=
  admits (fun t => bind_iff _ _ _ t) (by decide)

/-- Planted (law `bind_iff`, direction over-accepting). -/
theorem bind_iff_planted_1 : ¬ (∀ t, bind (defaultDescriptor untyped) y initial = some t ↔ Bind untyped initial y t) :=
  rejectsAcceptance (actual := (bind untyped y initial)) (fun t => bind_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `bind_iff`, direction over-refusing). -/
theorem bind_iff_planted_2 : ¬ (∀ t, refuseFreshSlot types y initial = some t ↔ Bind types initial y t) :=
  rejectsRefusal (actual := (bind types y initial)) (fun t => bind_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `drop_iff`, case `admitted operation`). -/
theorem drop_iff_instance_1 : ∃ t, drop [x] LifecycleWitnesses.moved = some t ∧ Drop LifecycleWitnesses.moved [x] t :=
  admits (fun t => drop_iff _ _ t) (by decide)

/-- Planted (law `drop_iff`, direction over-accepting). -/
theorem drop_iff_planted_1 : ¬ (∀ t, dropWithoutDischarge initial [x] = some t ↔ Drop initial [x] t) :=
  rejectsAcceptance (actual := (drop [x] initial)) (fun t => drop_iff _ _ t) (by decide) (by decide)

/-- Planted (law `drop_iff`, direction over-refusing). -/
theorem drop_iff_planted_2 : ¬ (∀ t, refuseNonemptyDrop [x] LifecycleWitnesses.moved = some t ↔
    Drop LifecycleWitnesses.moved [x] t) :=
  rejectsRefusal (actual := (drop [x] LifecycleWitnesses.moved))
    (fun t => drop_iff _ _ t) (by decide) (by decide)

/-- Instance (law `move_iff`, case `admitted operation`). -/
theorem move_iff_instance_1 : ∃ t, move MoveLifecycleEvidence.source ⟨1⟩ = some t ∧ Move MoveLifecycleEvidence.source ⟨1⟩ t :=
  admits (fun t => move_iff _ _ t) (by decide)

/-- Planted (law `move_iff`, direction over-accepting). -/
theorem move_iff_planted_3 : ¬ (∀ t, acceptBorrowedMove exclusive ⟨0⟩ = some t ↔ Move exclusive ⟨0⟩ t) :=
  rejectsAcceptance (actual := (move exclusive ⟨0⟩)) (fun t => move_iff _ _ t) (by decide) (by decide)

/-- Planted (law `move_iff`, direction over-refusing). -/
theorem move_iff_planted_4 : ¬ (∀ t, refuseNonzeroMove MoveLifecycleEvidence.source ⟨1⟩ = some t ↔
    Move MoveLifecycleEvidence.source ⟨1⟩ t) :=
  rejectsRefusal (actual := (move MoveLifecycleEvidence.source ⟨1⟩))
    (fun t => move_iff _ _ t) (by decide) (by decide)

/-- Instance (law `borrow_iff`, case `admitted operation`). -/
theorem borrow_iff_instance_1 : ∃ t, borrow initial ⟨0⟩ [] .mutable = some t ∧ Borrow initial ⟨0⟩ [] .mutable t :=
  admits (fun t => borrow_iff _ _ _ _ t) (by decide)

/-- Planted (law `borrow_iff`, direction over-accepting). -/
theorem borrow_iff_planted_1 : ¬ (∀ t, acceptMissingRoot initial (.borrow ⟨9⟩ [] .shared) = some t ↔
    Borrow initial ⟨9⟩ [] .shared t) :=
  rejectsAcceptance (actual := (borrow initial ⟨9⟩ [] .shared))
    (fun t => borrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `borrow_iff`, direction over-refusing). -/
theorem borrow_iff_planted_2 : ¬ (∀ t, refuseMutableBorrow initial ⟨0⟩ [] .mutable = some t ↔ Borrow initial ⟨0⟩ [] .mutable t) :=
  rejectsRefusal (actual := (borrow initial ⟨0⟩ [] .mutable))
    (fun t => borrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `reborrowTo_iff`, case `admitted operation`). -/
theorem reborrowTo_iff_instance_1 : ∃ t, reborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ∧
    ReborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 t :=
  admits (fun t => reborrowTo_iff _ _ _ _ _ t) (by decide)

/-- Planted (law `reborrowTo_iff`, direction over-accepting). -/
theorem reborrowTo_iff_planted_2 : ¬ (∀ t, reborrowIgnoringConflict child ⟨0⟩ [] .mutable 0 = some t ↔
    ReborrowTo child ⟨0⟩ [] .mutable 0 t) :=
  rejectsAcceptance (actual := (reborrowTo child ⟨0⟩ [] .mutable 0))
    (fun t => reborrowTo_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `reborrowTo_iff`, direction over-refusing). -/
theorem reborrowTo_iff_planted_3 : ¬ (∀ t, refuseProjected LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 = some t ↔
    ReborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1 t) :=
  rejectsRefusal (actual := (reborrowTo LifecycleWitnesses.called ⟨1⟩ [.index 2] .mutable 1))
    (fun t => reborrowTo_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `reborrow_iff`, case `admitted operation`). -/
theorem reborrow_iff_instance_1 : ∃ t, reborrow exclusive ⟨0⟩ [] .mutable = some t ∧ Reborrow exclusive ⟨0⟩ [] .mutable t :=
  admits (fun t => reborrow_iff _ _ _ _ t) (by decide)

/-- Keep the current-frame destination when isolating the conflict check. -/
def reborrowCurrentIgnoringConflict (s : State) (i : LoanId)
    (path : List Projection) (mode : Mode) : Option State := do
  let frame ← s.frames.head?
  reborrowIgnoringConflict s i path mode frame.id

/-- Planted (law `reborrow_iff`, direction over-accepting). -/
theorem reborrow_iff_planted_1 : ¬ (∀ t, reborrowCurrentIgnoringConflict child ⟨0⟩ [] .mutable = some t ↔
    Reborrow child ⟨0⟩ [] .mutable t) :=
  rejectsAcceptance (actual := (reborrow child ⟨0⟩ [] .mutable))
    (fun t => reborrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `reborrow_iff`, direction over-refusing). -/
theorem reborrow_iff_planted_2 : ¬ (∀ t, refuseMutableReborrow exclusive ⟨0⟩ [] .mutable = some t ↔
    Reborrow exclusive ⟨0⟩ [] .mutable t) :=
  rejectsRefusal (actual := (reborrow exclusive ⟨0⟩ [] .mutable))
    (fun t => reborrow_iff _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `endLoan_iff`, case `admitted operation`). -/
theorem endLoan_iff_instance_1 : ∃ t, endLoan child ⟨1⟩ = some t ∧ EndLoan child ⟨1⟩ t :=
  admits (fun t => endLoan_iff _ _ t) (by decide)

/-- Planted (law `endLoan_iff`, direction over-accepting). -/
theorem endLoan_iff_planted_1 : ¬ (∀ t, PreservationEvidence.endLoanAllowChildren child ⟨0⟩ = some t ↔ EndLoan child ⟨0⟩ t) :=
  rejectsAcceptance (actual := (endLoan child ⟨0⟩)) (fun t => endLoan_iff _ _ t) (by decide) (by decide)

/-- Planted (law `endLoan_iff`, direction over-refusing). -/
theorem endLoan_iff_planted_2 : ¬ (∀ t, refuseLeafEnd child ⟨1⟩ = some t ↔ EndLoan child ⟨1⟩ t) :=
  rejectsRefusal (actual := (endLoan child ⟨1⟩)) (fun t => endLoan_iff _ _ t) (by decide) (by decide)

/-- Instance (law `loanAction_iff`, case `admitted operation`). -/
theorem loanAction_iff_instance_1 : ∃ t, loanAction child (.endLoan ⟨1⟩) = some t ∧ LoanStep child (.endLoan ⟨1⟩) t :=
  admits (fun t => loanAction_iff _ _ t) (by decide)

/-- Planted (law `loanAction_iff`, direction over-accepting). -/
theorem loanAction_iff_planted_1 : ¬ (∀ t, acceptParentEnd child (.endLoan ⟨0⟩) = some t ↔ LoanStep child (.endLoan ⟨0⟩) t) :=
  rejectsAcceptance (actual := (loanAction child (.endLoan ⟨0⟩)))
    (fun t => loanAction_iff _ _ t) (by decide) (by decide)

/-- Planted (law `loanAction_iff`, direction over-refusing). -/
theorem loanAction_iff_planted_2 : ¬ (∀ t, refuseChildEnd child (.endLoan ⟨1⟩) = some t ↔ LoanStep child (.endLoan ⟨1⟩) t) :=
  rejectsRefusal (actual := (loanAction child (.endLoan ⟨1⟩)))
    (fun t => loanAction_iff _ _ t) (by decide) (by decide)

/-- Instance (law `runLoanSequence_iff`, case `admitted operation`). -/
theorem runLoanSequence_iff_instance_1 : ∃ t, runLoanSequence child [] = some t ∧ LoanSequence child [] t :=
  admits (fun t => runLoanSequence_iff _ _ t) (by decide)

/-- Planted (law `runLoanSequence_iff`, direction over-accepting). -/
theorem runLoanSequence_iff_planted_4 : ¬ (∀ t, keepOnSequenceRefusal child [.endLoan ⟨0⟩] = some t ↔
    LoanSequence child [.endLoan ⟨0⟩] t) :=
  rejectsAcceptance (actual := (runLoanSequence child [.endLoan ⟨0⟩]))
    (fun t => runLoanSequence_iff _ _ t) (by decide) (by decide)

/-- Planted (law `runLoanSequence_iff`, direction over-refusing). -/
theorem runLoanSequence_iff_planted_5 : ¬ (∀ t, refuseRunLoanSequence child [] = some t ↔ LoanSequence child [] t) :=
  rejectsRefusal (actual := (runLoanSequence child []))
    (fun t => runLoanSequence_iff _ _ t) (by decide) (by decide)

/-- Instance (law `endLocals_iff`, case `admitted operation`). -/
theorem endLocals_iff_instance_1 : ∃ t, endLocals 0 child = some t ∧ EndLocals child 0 t :=
  admits (fun t => endLocals_iff _ _ t) (by decide)

/-- Planted (law `endLocals_iff`, direction over-accepting). -/
theorem endLocals_iff_planted_1 : ¬ (∀ t, PreservationEvidence.endLocalsAllowChildren 0 splitActivation = some t ↔
    EndLocals splitActivation 0 t) :=
  rejectsAcceptance (actual := (endLocals 0 splitActivation))
    (fun t => endLocals_iff _ _ t) (by decide) (by decide)

/-- Planted (law `endLocals_iff`, direction over-refusing). -/
theorem endLocals_iff_planted_2 : ¬ (∀ t, refusePopulatedEnding 0 child = some t ↔ EndLocals child 0 t) :=
  rejectsRefusal (actual := (endLocals 0 child)) (fun t => endLocals_iff _ _ t) (by decide) (by decide)

/-- Instance (law `enter_iff`, case `admitted operation`). -/
theorem enter_iff_instance_1 : ∃ t, enter types [y] initial = some t ∧ Enter types initial [y] t :=
  admits (fun t => enter_iff _ _ _ t) (by decide)

/-- Planted (law `enter_iff`, direction over-accepting). -/
theorem enter_iff_planted_1 : ¬ (∀ t, enter (defaultDescriptor untyped) [y] initial = some t ↔ Enter untyped initial [y] t) :=
  rejectsAcceptance (actual := (enter untyped [y] initial))
    (fun t => enter_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `enter_iff`, direction over-refusing). -/
theorem enter_iff_planted_2 : ¬ (∀ t, refuseTypedEntry types [y] initial = some t ↔ Enter types initial [y] t) :=
  rejectsRefusal (actual := (enter types [y] initial)) (fun t => enter_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `leave_iff`, case `admitted operation`). -/
theorem leave_iff_instance_1 : ∃ t, leave LifecycleWitnesses.moved = some t ∧ Leave LifecycleWitnesses.moved t :=
  admits (fun t => leave_iff _ t) (by decide)

/-- Planted (law `leave_iff`, direction over-accepting). -/
theorem leave_iff_planted_1 : ¬ (∀ t, acceptLiveReturn .leave initial = some t ↔ Leave initial t) :=
  rejectsAcceptance (actual := (leave initial)) (fun t => leave_iff _ t) (by decide) (by decide)

/-- Planted (law `leave_iff`, direction over-refusing). -/
theorem leave_iff_planted_2 : ¬ (∀ t, refusePopulatedExit LifecycleWitnesses.moved = some t ↔ Leave LifecycleWitnesses.moved t) :=
  rejectsRefusal (actual := (leave LifecycleWitnesses.moved)) (fun t => leave_iff _ t) (by decide) (by decide)

/-- Instance (law `life_iff`, case `admitted operation`). -/
theorem life_iff_instance_1 : ∃ t, life types (.bind y) initial = some t ∧ Life types initial (.bind y) t :=
  admits (fun t => life_iff _ _ _ t) (by decide)

/-- Planted (law `life_iff`, direction over-accepting). -/
theorem life_iff_planted_9 : ¬ (∀ t, lifeWithoutDropDischarge (.drop [x]) initial = some t ↔ Life types initial (.drop [x]) t) :=
  rejectsAcceptance (actual := (life types (.drop [x]) initial))
    (fun t => life_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `life_iff`, direction over-refusing). -/
theorem life_iff_planted_10 : ¬ (∀ t, refuseLifeBind (.bind y) initial = some t ↔ Life types initial (.bind y) t) :=
  rejectsRefusal (actual := (life types (.bind y) initial))
    (fun t => life_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `lifeStore_iff`, case `admitted operation`). -/
theorem lifeStore_iff_instance_1 : ∃ t, lifeStore types (.drop []) baseStore = some t ∧ LifeStore types baseStore (.drop []) t :=
  admits (fun t => lifeStore_iff _ _ _ t) (by decide)

/-- Planted (law `lifeStore_iff`, direction over-accepting). -/
theorem lifeStore_iff_planted_2 : ¬ (∀ t, (dropWithoutDischarge initial [x]).map (fun own => { baseStore with own := own }) = some t ↔
    LifeStore types baseStore (.drop [x]) t) :=
  rejectsAcceptance (actual := (lifeStore types (.drop [x]) baseStore))
    (fun t => lifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `lifeStore_iff`, direction over-refusing). -/
theorem lifeStore_iff_planted_3 : ¬ (∀ t, refuseStoreDrop (.drop []) baseStore = some t ↔ LifeStore types baseStore (.drop []) t) :=
  rejectsRefusal (actual := (lifeStore types (.drop []) baseStore))
    (fun t => lifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `instrumentLife_iff`, case `admitted operation`). -/
theorem instrumentLife_iff_instance_1 : ∃ t, instrumentLife controlOperands types (.drop []) baseStore = some t ∧
    InstrumentLife concreteIdentity types baseStore (.drop []) t :=
  admits (fun t => instrumentLife_iff controlOperands concreteIdentity concreteIdentity_iff _ _ _ t) (by decide)

/-- Planted (law `instrumentLife_iff`, direction over-accepting). -/
theorem instrumentLife_iff_planted_1 : ¬ (∀ t, instrumentWithPermissiveDrop (.drop [x]) baseStore = some t ↔
    InstrumentLife concreteIdentity types baseStore (.drop [x]) t) :=
  rejectsAcceptance (actual := (instrumentLife controlOperands types (.drop [x]) baseStore))
    (fun t => instrumentLife_iff controlOperands concreteIdentity concreteIdentity_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `instrumentLife_iff`, direction over-refusing). -/
theorem instrumentLife_iff_planted_2 : ¬ (∀ t, instrumentRefusingEmptyDrop (.drop []) baseStore = some t ↔
    InstrumentLife concreteIdentity types baseStore (.drop []) t) :=
  rejectsRefusal (actual := (instrumentLife controlOperands types (.drop []) baseStore))
    (fun t => instrumentLife_iff controlOperands concreteIdentity concreteIdentity_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `adoptDown_iff`, case `admitted operation`). -/
theorem adoptDown_iff_instance_1 : ∃ t, adoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending = some t ∧
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t :=
  admits (fun t => adoptDown_iff _ _ t) (by decide)

/-- Planted (law `adoptDown_iff`, direction over-accepting). -/
theorem adoptDown_iff_planted_3 : ¬ (∀ t, adoptAbsent initial [(y, ⟨9⟩)] = some t ↔ AdoptDown initial [(y, ⟨9⟩)] t) :=
  rejectsAcceptance (actual := (adoptDown initial [(y, ⟨9⟩)]))
    (fun t => adoptDown_iff _ _ t) (by decide) (by decide)

/-- Planted (law `adoptDown_iff`, direction over-refusing). -/
theorem adoptDown_iff_planted_4 : ¬ (∀ t, refuseMultiAdopt Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending = some t ↔
    AdoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending t) :=
  rejectsRefusal (actual := (adoptDown Round5Evidence.twoBefore Round5Evidence.twoWaiting.pending))
    (fun t => adoptDown_iff _ _ t) (by decide) (by decide)

/-- Instance (law `callLife_iff`, case `admitted operation`). -/
theorem callLife_iff_instance_1 : ∃ t, callLife types (.enter []) nestedCaller = some t ∧ CallLife types nestedCaller (.enter []) t :=
  admits (fun t => callLife_iff _ _ _ t) (by decide)

/-- Planted (law `callLife_iff`, direction over-accepting). -/
theorem callLife_iff_planted_8 : ¬ (∀ t, ignorePendingOnDrop (.drop []) PendingEvidence.down = some t ↔
    CallLife types PendingEvidence.down (.drop []) t) :=
  rejectsAcceptance (actual := (callLife types (.drop []) PendingEvidence.down))
    (fun t => callLife_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `callLife_iff`, direction over-refusing). -/
theorem callLife_iff_planted_9 : ¬ (∀ t, refuseEmptyCallEntry (.enter []) nestedCaller = some t ↔
    CallLife types nestedCaller (.enter []) t) :=
  rejectsRefusal (actual := (callLife types (.enter []) nestedCaller))
    (fun t => callLife_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `callLifeStore_iff`, case `admitted operation`). -/
theorem callLifeStore_iff_instance_1 : ∃ t, callLifeStore types (.enter []) ((7, nestedCaller) : Nat × CallState) = some t ∧
    CallLifeStore types ((7, nestedCaller) : Nat × CallState) (.enter []) t :=
  admits (fun t => callLifeStore_iff _ _ _ t) (by decide)

/-- Planted (law `callLifeStore_iff`, direction over-accepting). -/
theorem callLifeStore_iff_planted_2 : ¬ (∀ t, (ignorePendingOnDrop (.drop []) PendingEvidence.down).map (fun u => (7, u)) = some t ↔
    CallLifeStore types ((7, PendingEvidence.down) : Nat × CallState) (.drop []) t) :=
  rejectsAcceptance (actual := (callLifeStore types (.drop []) ((7, PendingEvidence.down) : Nat × CallState)))
    (fun t => callLifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `callLifeStore_iff`, direction over-refusing). -/
theorem callLifeStore_iff_planted_3 : ¬ (∀ t, (refuseEmptyCallEntry (.enter []) nestedCaller).map (fun u => (7, u)) = some t ↔
    CallLifeStore types ((7, nestedCaller) : Nat × CallState) (.enter []) t) :=
  rejectsRefusal (actual := (callLifeStore types (.enter []) ((7, nestedCaller) : Nat × CallState)))
    (fun t => callLifeStore_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `enterDown_iff`, case `admitted operation`). -/
theorem enterDown_iff_instance_1 : ∃ t, enterDown exclusive singletonEntry = some t ∧ EnterDown exclusive singletonEntry t :=
  admits (fun t => enterDown_iff _ _ t) (by decide)

/-- Planted (law `enterDown_iff`, direction over-accepting). -/
theorem enterDown_iff_planted_3 : ¬ (∀ t, acceptFrozenDownward exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] = some t ↔
    EnterDown exclusive [(x, ⟨0⟩), (y, ⟨0⟩)] t) :=
  rejectsAcceptance (actual := (enterDown exclusive [(x, ⟨0⟩), (y, ⟨0⟩)]))
    (fun t => enterDown_iff _ _ t) (by decide) (by decide)

/-- Substitute only the binding step's omitted parameter-identity check. -/
def enterDownAllowDuplicate (s : State) (args : List (Var × LoanId)) : Option State := do
  if ¬ CallerSources s args then none else
  let s ← enter (fun _ => none) [] s
  args.foldlM (fun s (x, i) => do
    let a ← loan s i
    let f ← s.frames.head?
    let u ← reborrowTo s i [] a.mode f.id
    some { u with frames := { f with views := (x, ⟨s.nextLoan⟩) :: f.views } :: s.frames.tail }) s

/-- Planted (law `enterDown_iff`, direction over-accepting).
Two shared grants may name the same source, but may not supply the same parameter. -/
theorem enterDown_rejectsDuplicateParameter :
    ¬ (∀ t, enterDownAllowDuplicate shared [(y, ⟨0⟩), (y, ⟨0⟩)] = some t ↔
      EnterDown shared [(y, ⟨0⟩), (y, ⟨0⟩)] t) :=
  rejectsAcceptance (actual := enterDown shared [(y, ⟨0⟩), (y, ⟨0⟩)])
    (fun t => enterDown_iff _ _ t) (by decide) (by decide)

/-- Planted (law `enterDown_iff`, direction over-refusing). -/
theorem enterDown_iff_planted_4 : ¬ (∀ t, refuseStandaloneEntry exclusive singletonEntry = some t ↔
    EnterDown exclusive singletonEntry t) :=
  rejectsRefusal (actual := (enterDown exclusive singletonEntry))
    (fun t => enterDown_iff _ _ t) (by decide) (by decide)

/-- Instance (law `passDown_iff`, case `admitted operation`). -/
theorem passDown_iff_instance_1 : ∃ t, passDown PendingEvidence.down ⟨0⟩ y = some t ∧ PassDown PendingEvidence.down ⟨0⟩ y t :=
  admits (fun t => passDown_iff _ _ _ t) (by decide)

/-- Planted (law `passDown_iff`, direction over-accepting). -/
theorem passDown_iff_planted_3 : ¬ (∀ t, acceptFrozenView ⟨child, []⟩ ⟨0⟩ y = some t ↔ PassDown ⟨child, []⟩ ⟨0⟩ y t) :=
  rejectsAcceptance (actual := (passDown ⟨child, []⟩ ⟨0⟩ y))
    (fun t => passDown_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `passDown_iff`, direction over-refusing). -/
theorem passDown_iff_planted_4 : ¬ (∀ t, refuseSecondPendingView PendingEvidence.down ⟨0⟩ y = some t ↔
    PassDown PendingEvidence.down ⟨0⟩ y t) :=
  rejectsRefusal (actual := (passDown PendingEvidence.down ⟨0⟩ y))
    (fun t => passDown_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `passOwner_iff`, case `admitted operation`). -/
theorem passOwner_iff_instance_1 : ∃ t, passOwner baseCall ⟨0⟩ [] .mutable y = some t ∧ PassOwner baseCall ⟨0⟩ [] .mutable y t :=
  admits (fun t => passOwner_iff _ _ _ _ _ t) (by decide)

/-- Planted (law `passOwner_iff`, direction over-accepting). -/
theorem passOwner_iff_planted_2 : ¬ (∀ t, acceptFrozenOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y = some t ↔
    PassOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y t) :=
  rejectsAcceptance (actual := (passOwner ⟨exclusive, []⟩ ⟨0⟩ [] .mutable y))
    (fun t => passOwner_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Planted (law `passOwner_iff`, direction over-refusing). -/
theorem passOwner_iff_planted_3 : ¬ (∀ t, refuseMutableArgument baseCall ⟨0⟩ [] .mutable y = some t ↔
    PassOwner baseCall ⟨0⟩ [] .mutable y t) :=
  rejectsRefusal (actual := (passOwner baseCall ⟨0⟩ [] .mutable y))
    (fun t => passOwner_iff _ _ _ _ _ t) (by decide) (by decide)

/-- Instance (law `issuanceStep_iff`, case `admitted operation`). -/
theorem issuanceStep_iff_instance_1 : ∃ t, issuanceStep types baseCall (.entry []) = some t ∧ Issuance types baseCall (.entry []) t :=
  admits (fun t => issuanceStep_iff _ _ _ t) (by decide)

/-- Planted (law `issuanceStep_iff`, direction over-accepting). -/
theorem issuanceStep_iff_planted_2 : ¬ (∀ t, endAdopted DirectArgumentWitnesses.called adoptedEnd = some t ↔
    Issuance types DirectArgumentWitnesses.called adoptedEnd t) :=
  rejectsAcceptance (actual := (issuanceStep types DirectArgumentWitnesses.called adoptedEnd))
    (fun t => issuanceStep_iff _ _ _ t) (by decide) (by decide)

/-- Keep standalone entry intact, except for allowing a buffered call to cross it. -/
def crossPendingEntry (s : CallState) (e : IssuanceAction) : Option CallState :=
  match e with
  | .entry args => (enterDown s.own args).map fun own => { s with own := own }
  | _ => issuanceStep types s e

/-- Planted (law `issuanceStep_iff`, direction over-accepting). -/
theorem issuanceStep_iff_planted_3 : ¬ (∀ t, crossPendingEntry PendingEvidence.down (.entry []) = some t ↔
    Issuance types PendingEvidence.down (.entry []) t) :=
  rejectsAcceptance (actual := (issuanceStep types PendingEvidence.down (.entry [])))
    (fun t => issuanceStep_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `issuanceStep_iff`, direction over-refusing). -/
theorem issuanceStep_iff_planted_4 : ¬ (∀ t, refuseIssuanceStep baseCall (.entry []) = some t ↔ Issuance types baseCall (.entry []) t) :=
  rejectsRefusal (actual := (issuanceStep types baseCall (.entry [])))
    (fun t => issuanceStep_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `issuanceTrace_iff`, case `admitted operation`). -/
theorem issuanceTrace_iff_instance_1 : ∃ t, [IssuanceAction.entry []].foldlM (issuanceStep types) baseCall = some t ∧
    Trace (Issuance types) baseCall [.entry []] t :=
  admits (fun t => issuanceTrace_iff _ _ _ t) (by decide)

/-- Planted (law `issuanceTrace_iff`, direction over-accepting). -/
theorem issuanceTrace_iff_planted_1 : ¬ (∀ t, [adoptedEnd].foldlM endAdopted DirectArgumentWitnesses.called = some t ↔
    Trace (Issuance types) DirectArgumentWitnesses.called [adoptedEnd] t) :=
  rejectsAcceptance (actual := ([adoptedEnd].foldlM (issuanceStep types) DirectArgumentWitnesses.called))
    (fun t => issuanceTrace_iff _ _ _ t) (by decide) (by decide)

/-- Planted (law `issuanceTrace_iff`, direction over-refusing). -/
theorem issuanceTrace_iff_planted_2 : ¬ (∀ t, [IssuanceAction.entry []].foldlM refuseIssuanceStep baseCall = some t ↔
    Trace (Issuance types) baseCall [.entry []] t) :=
  rejectsRefusal (actual := ([IssuanceAction.entry []].foldlM (issuanceStep types) baseCall))
    (fun t => issuanceTrace_iff _ _ _ t) (by decide) (by decide)

/-- Instance (law `issuance_wellFormed`, case `PendingEvidence.down`). -/
theorem issuance_wellFormed_instance_1 : ∃ t, Issuance types PendingEvidence.down (.view ⟨0⟩ y) t ∧ t.WellFormed := by
  obtain ⟨t, _, legal⟩ :=
    admits (issuanceStep_iff types PendingEvidence.down (.view ⟨0⟩ y)) (by decide)
  exact ⟨t, legal, issuance_wellFormed types legal (by decide)⟩

/-- The declarative mutant omits only the protection of the frame's bound loan. -/
def issuanceWithoutBindingGuard (s : CallState) (e : IssuanceAction) (t : CallState) : Prop :=
  match e with
  | .ordinary (.endLoan i) =>
    OrdinaryIssuanceAllowed s (.endLoan i) ∧
    (∀ a ∈ s.own.loans, a.id = i → a.pendingArgument = false) ∧
    ∃ u, EndLeafAtBoundary s.own i u ∧ t = { s with own := u }
  | _ => Issuance types s e t

/-- Planted (law `issuance_wellFormed`, mutant `issuanceWithoutBindingGuard`).
Ending a direct argument before return leaves a dead loan in the callee's view index. -/
theorem issuance_wellFormed_planted_1 :
    ¬ (∀ t, issuanceWithoutBindingGuard DirectArgumentWitnesses.called adoptedEnd t →
      DirectArgumentWitnesses.called.WellFormed → t.WellFormed) := by
  let t := (endAdopted DirectArgumentWitnesses.called adoptedEnd).getD DirectArgumentWitnesses.called
  have legal : issuanceWithoutBindingGuard DirectArgumentWitnesses.called adoptedEnd t :=
    ⟨by decide, by decide, t.own, (endLeafAtBoundary_iff ..).mp (by decide), rfl⟩
  have bad : ¬ t.WellFormed := by decide
  exact fun preserve => bad (preserve t legal (by decide))

/-- Planted (law `endLoan_iff`, direction over-accepting).
An adopted call loan is protected by ordinary ending itself. -/
theorem endLoan_rejectsAdoptedArgument :
    ¬ (∀ t, endWithoutBindingGuard DirectArgumentWitnesses.called.own ⟨1⟩ = some t ↔
      EndLoan DirectArgumentWitnesses.called.own ⟨1⟩ t) :=
  rejectsAcceptance (actual := endLoan DirectArgumentWitnesses.called.own ⟨1⟩)
    (fun t => endLoan_iff _ _ t) (by decide) (by decide)

/-- The direct argument stays frozen before return, which still retires the loan. -/
example : endLoan DirectArgumentWitnesses.called.own ⟨1⟩ = none ∧
    ownerAccess DirectArgumentWitnesses.called.own ⟨1⟩ .mutable = false ∧
    callLife types .leave DirectArgumentWitnesses.called = some DirectArgumentWitnesses.returned ∧
    ownerAccess DirectArgumentWitnesses.returned.own ⟨1⟩ .mutable = true := by decide

/-- The mixed dispatcher can still end a live ordinary leaf when it has no pending obligation. -/
example : ∃ t, Issuance types ⟨child, []⟩ (.ordinary (.endLoan ⟨1⟩)) t ∧ t.WellFormed := by
  obtain ⟨t, _, legal⟩ :=
    admits (issuanceStep_iff types ⟨child, []⟩ (.ordinary (.endLoan ⟨1⟩))) (by decide)
  exact ⟨t, legal, issuance_wellFormed types legal (by decide)⟩

def copyParameter (_ : Var) : Option Descriptor := some ⟨.int .i32, true, true⟩

/-- Substitute only the omitted duplicate check for owned parameters. -/
def ownerEntryWithoutSignatureCheck (params : List Var) (s : State) : Option State :=
  params.foldlM (fun own x => bind copyParameter x own) (pushFrame s)

/-- Planted (law `enter_iff`, mutant `ownerEntryWithoutSignatureCheck`).
A duplicate Copy parameter would otherwise be accepted as a reinitialization. -/
theorem enter_rejectsDuplicateOwnerParameter :
    ¬ (∀ t, ownerEntryWithoutSignatureCheck [y, y] initial = some t ↔
      Enter copyParameter initial [y, y] t) :=
  rejectsAcceptance (actual := enter copyParameter [y, y] initial)
    (fun t => enter_iff _ _ _ t) (by decide) (by decide)

def adoptionWithExistingView : CallState :=
  (passDown ⟨(enterDown shared [(y, ⟨0⟩)]).getD shared, []⟩ ⟨0⟩ y).getD ⟨shared, []⟩

/-- Substitute only the omitted check against already adopted views. -/
def adoptWithoutViewCheck (s : State) (args : List (Var × LoanId)) : Option State := do
  let f ← s.frames.head?
  if f.id < s.nextActivation ∧ (args.map Prod.fst).Nodup ∧ (args.map Prod.snd).Nodup ∧
      (∀ e ∈ args, ∃ l ∈ s.loans, l.id = e.2 ∧ l.pendingArgument = true) ∧
      (∀ e ∈ args, AList.lookup f.locals e.1 = none) then
    some { s with
      loans := s.loans.map fun l =>
        if l.id ∈ args.map Prod.snd then { l with activation := f.id, pendingArgument := false } else l
      frames := { f with views := args ++ f.views } :: s.frames.tail }
  else none

/-- Planted (law `adoptDown_iff`, mutant `adoptWithoutViewCheck`). -/
theorem adoptDown_rejectsExistingView :
    let s := adoptionWithExistingView
    ¬ (∀ t, adoptWithoutViewCheck s.own s.pending = some t ↔ AdoptDown s.own s.pending t) :=
  rejectsAcceptance (actual := adoptDown adoptionWithExistingView.own adoptionWithExistingView.pending)
    (fun t => adoptDown_iff _ _ t) (by decide) (by decide)

/-- Substitute only the omitted complete-signature check. -/
def callEntryWithoutSignatureCheck (params : List Var) (s : CallState) : Option CallState := do
  if ∀ e ∈ s.pending, e.1 ∈ params then
    let own ← enter types (params.filter fun x => decide (x ∉ s.pending.map Prod.fst)) s.own
    let own ← adoptDown own s.pending
    pure ⟨own, []⟩
  else none

/-- Planted (law `callLife_iff`, mutant `callEntryWithoutSignatureCheck`). -/
theorem callLife_rejectsDuplicateViewParameter :
    ¬ (∀ t, callEntryWithoutSignatureCheck [y, y] CallWitnesses.waiting = some t ↔
      CallLife types CallWitnesses.waiting (.enter [y, y]) t) :=
  rejectsAcceptance (actual := callLife types (.enter [y, y]) CallWitnesses.waiting)
    (fun t => callLife_iff _ _ _ t) (by decide) (by decide)

/-- Substitute only the omitted check for a view with the same identity. -/
def bindWithoutViewCheck (x : Var) (s : State) : Option State := do
  let f ← s.frames.head?
  match AList.lookup f.locals x with
  | some p => reinitialize s p
  | none =>
    let d ← types x
    if f.id < s.nextActivation then some { s with
      nextPlace := s.nextPlace + 1
      slots := ⟨⟨s.nextPlace⟩, f.id, d, .owned⟩ :: s.slots
      frames := { f with locals := (x, ⟨s.nextPlace⟩) :: f.locals } :: s.frames.tail }
    else none

def viewParameterFrame : State :=
  (enterDown shared [(y, ⟨0⟩)]).getD shared

/-- Planted (law `bind_iff`, mutant `bindWithoutViewCheck`). -/
theorem bind_rejectsViewIdentity :
    ¬ (∀ t, bindWithoutViewCheck y viewParameterFrame = some t ↔ Bind types viewParameterFrame y t) :=
  rejectsAcceptance (actual := bind types y viewParameterFrame)
    (fun t => bind_iff _ _ _ t) (by decide) (by decide)

/-- Substitute only the omitted caller-side source check. -/
def enterDownWithoutCallerSources (s : State) (args : List (Var × LoanId)) : Option State := do
  let s ← enter (fun _ => none) [] s
  args.foldlM enterDownParameter s

def siblingArguments : List (Var × LoanId) := [(x, ⟨0⟩), (y, ⟨1⟩)]

/-- Planted (law `enterDown_iff`, mutant `enterDownWithoutCallerSources`).
Buffered evaluation also refuses the freshly issued pending child as a source. -/
theorem entry_rejectsSiblingSource :
    (passDown ⟨shared, []⟩ ⟨0⟩ x >>= fun waiting => passDown waiting ⟨1⟩ y) = none ∧
    ¬ (∀ t, enterDownWithoutCallerSources shared siblingArguments = some t ↔
      EnterDown shared siblingArguments t) :=
  ⟨by decide, rejectsAcceptance (actual := enterDown shared siblingArguments)
    (fun t => enterDown_iff _ _ t) (by decide) (by decide)⟩

def sharedCaller : CallState := ⟨shared, []⟩

def departedLoan : CallState := ⟨{ shared with
  nextActivation := 2
  loans := shared.loans.map fun l => { l with activation := 1 } }, []⟩

/-- Instance (law `ledgerAccessLoan_iff`, case `shared loan in its live caller frame`). -/
theorem ledgerAccessLoan_live :
    LedgerStep types sharedCaller (.accessLoan ⟨0⟩ .shared) sharedCaller :=
  ⟨by decide, (accessLoan_iff ..).mp (by decide), rfl⟩

/-- Planted (law `ledgerAccessLoan_iff`, direction over-accepting).
The raw permission succeeds after omitting destination liveness at admission. -/
theorem ledgerAccessLoan_rejectsDeparted :
    WellFormed departedLoan.own ∧ ¬ departedLoan.WellFormed ∧
    (AccessLoan departedLoan.own ⟨0⟩ .shared departedLoan.own ∧
      departedLoan.pending = departedLoan.pending) ∧
    ¬ LedgerStep types departedLoan (.accessLoan ⟨0⟩ .shared) departedLoan := by
  refine ⟨by decide, by decide, ⟨(accessLoan_iff ..).mp (by decide), rfl⟩, ?_⟩
  exact fun step => (by decide : ¬ LiveDestinations departedLoan.own)
    ((ledgerAccessLoan_iff ..).mp step).1

/-- Instance (law `ledgerTrace_wellFormed`, case `issuance`). -/
theorem ledgerTrace_issue :
    ∃ t, Trace (LedgerStep types) baseCall [.issue (.entry [])] t ∧ t.WellFormed := by
  obtain ⟨t, _, step⟩ := admits (issuanceStep_iff types baseCall (.entry [])) (by decide)
  have trace : Trace (LedgerStep types) baseCall [.issue (.entry [])] t :=
    .cons step (.nil _)
  exact ⟨t, trace, ledgerTrace_wellFormed types trace (by decide)⟩

/-- Instance (law `ledgerTrace_wellFormed`, case `loan access`). -/
theorem ledgerTrace_loanAccess :
    Trace (LedgerStep types) sharedCaller [.accessLoan ⟨0⟩ .shared] sharedCaller ∧
      sharedCaller.WellFormed := by
  have trace := Trace.cons ledgerAccessLoan_live (.nil sharedCaller)
  exact ⟨trace, ledgerTrace_wellFormed types trace (by decide)⟩

/-- Instance (law `ledgerTrace_wellFormed`, case `owner access`). -/
theorem ledgerStep_ownerAccess :
    LedgerStep types baseCall (.accessOwner ⟨0⟩ .mutable) baseCall :=
  ⟨(accessOwner_iff ..).mp (by decide), rfl⟩

/-- Instance (law `ledgerTrace_wellFormed`, case `owner move`). -/
theorem ledgerStep_move :
    ∃ t, LedgerStep types baseCall (.move ⟨0⟩) t := by
  obtain ⟨u, moved⟩ := Option.isSome_iff_exists.mp
    (show (move initial ⟨0⟩).isSome = true from by decide)
  exact ⟨⟨u, []⟩, (move_iff ..).mp moved, rfl⟩

/-- Instance (law `ledgerTrace_wellFormed`, case `owner access followed by move`). -/
theorem ledgerTrace_accessThenMove :
    ∃ t, Trace (LedgerStep types) baseCall [.accessOwner ⟨0⟩ .mutable, .move ⟨0⟩] t ∧
      t.WellFormed := by
  obtain ⟨u, moved⟩ := Option.isSome_iff_exists.mp
    (show (move initial ⟨0⟩).isSome = true from by decide)
  have access : LedgerStep types baseCall (.accessOwner ⟨0⟩ .mutable) baseCall :=
    ⟨(accessOwner_iff ..).mp (by decide), rfl⟩
  have moveStep : LedgerStep types baseCall (.move ⟨0⟩) ⟨u, []⟩ :=
    ⟨(move_iff ..).mp moved, rfl⟩
  have trace := Trace.cons access (Trace.cons moveStep (.nil _))
  exact ⟨_, trace, ledgerTrace_wellFormed types trace (by decide)⟩

/-- Planted (law `ledgerTrace_wellFormed`, direction over-accepting).
An owner access that fabricates a pending binding corrupts the ledger. -/
theorem ledgerTrace_rejectsFabricatedPending :
    let broken : CallState := ⟨initial, [(y, ⟨0⟩)]⟩
    let mutant := fun s e t => LedgerStep types s e t ∨
      (s = baseCall ∧ e = LedgerEvent.accessOwner ⟨0⟩ .mutable ∧ t = broken)
    ¬ (∀ s events t, Trace mutant s events t → s.WellFormed → t.WellFormed) := by
  intro broken mutant preserve
  have bad : Trace mutant baseCall [.accessOwner ⟨0⟩ .mutable] broken :=
    .cons (Or.inr ⟨rfl, rfl, rfl⟩) (.nil _)
  exact (show ¬ broken.WellFormed from by decide) (preserve _ _ _ bad (by decide))

/-- Re-lending changes only the new pending child even if a malformed input reuses the supply. -/
example :
    let s : CallState := ⟨{ exclusive with nextLoan := 0 }, []⟩
    ∃ t, passDown s ⟨0⟩ y = some t ∧ t.own.loans.tail = s.own.loans :=
  ⟨(passDown ⟨{ exclusive with nextLoan := 0 }, []⟩ ⟨0⟩ y).getD baseCall, rfl, rfl⟩

/-- Mutant allowing an empty standalone entry to reuse the caller's activation. -/
def entryMayReuse (s : CallState) (e : IssuanceAction) (t : CallState) : Prop :=
  Issuance types s e t ∨ (e = .entry [] ∧ t = s)

/-- Instance (law `issuance_deterministic`, case `empty standalone entry`). -/
theorem issuance_deterministic_instance_1 : Issuance types baseCall (.entry []) { baseCall with own := pushFrame initial } ∧
    (∀ t, Issuance types baseCall (.entry []) t → t = { baseCall with own := pushFrame initial }) := by
  have step : Issuance types baseCall (.entry []) { baseCall with own := pushFrame initial } :=
    ⟨rfl, pushFrame initial, ⟨by decide, .nil _⟩, rfl⟩
  exact ⟨step, fun _ other => issuance_deterministic types other step⟩

/-- Planted (law `issuance_deterministic`, mutant `entryMayReuse`). -/
theorem issuance_deterministic_planted_1 : ¬ UniqueStep entryMayReuse := by
  intro unique
  have original : entryMayReuse baseCall (.entry []) { baseCall with own := pushFrame initial } :=
    Or.inl ⟨rfl, pushFrame initial, ⟨by decide, .nil _⟩, rfl⟩
  have alternative : entryMayReuse baseCall (.entry []) baseCall := Or.inr ⟨rfl, rfl⟩
  exact (by decide : { baseCall with own := pushFrame initial } ≠ baseCall)
    (unique original alternative)

/-- Instance (law `issuanceTrace_deterministic`, case `standalone entry trace`). -/
theorem issuanceTrace_deterministic_instance_1 : Trace (Issuance types) baseCall [.entry []] { baseCall with own := pushFrame initial } ∧
    (∀ t, Trace (Issuance types) baseCall [.entry []] t →
      t = { baseCall with own := pushFrame initial }) := by
  have trace : Trace (Issuance types) baseCall [.entry []] { baseCall with own := pushFrame initial } :=
    .cons ⟨rfl, pushFrame initial, ⟨by decide, .nil _⟩, rfl⟩ (.nil _)
  exact ⟨trace, fun _ other => issuanceTrace_deterministic types other trace⟩

/-- Planted (law `issuanceTrace_deterministic`, mutant `entryMayReuse`). -/
theorem issuanceTrace_deterministic_planted_1 : ¬ UniqueStep (Trace entryMayReuse) := by
  intro unique
  have original : Trace entryMayReuse baseCall [.entry []] { baseCall with own := pushFrame initial } :=
    .cons (Or.inl ⟨rfl, pushFrame initial, ⟨by decide, .nil _⟩, rfl⟩) (.nil _)
  have alternative : Trace entryMayReuse baseCall [.entry []] baseCall :=
    .cons (Or.inr ⟨rfl, rfl⟩) (.nil _)
  exact (by decide : { baseCall with own := pushFrame initial } ≠ baseCall)
    (unique original alternative)

end Controls

end LegalStep
end ConcreteSpec.Core.Own

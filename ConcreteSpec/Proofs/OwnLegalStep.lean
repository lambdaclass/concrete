import ConcreteSpec.Proofs.Own
import ConcreteSpec.Proofs.OwnProtocols
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Ident
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
    t = { u with frames := { f with views := (e.1, ⟨u.nextLoan - 1⟩) :: f.views } :: u.frames.tail }

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
  x ∉ s.pending.map Prod.fst ∧ ∃ a f u,
    loan s.own i = some a ∧ s.own.frames.head? = some f ∧
    ReborrowTo s.own i [] a.mode f.id u ∧
    t = ⟨{ u with loans := u.loans.map fun l =>
      if l.id.id = s.own.nextLoan then { l with pendingArgument := true } else l },
      s.pending ++ [(x, ⟨s.own.nextLoan⟩)]⟩

/-- Law (authority: docs/decisions.md#od23). A view argument grants a fresh pending child. -/
theorem passDown_iff (s : CallState) (i : LoanId) (x : Var) (t : CallState) :
    passDown s i x = some t ↔ PassDown s i x t := by
  cases h : s.own.frames.head? <;>
    simp [passDown, PassDown, reborrow, h, Option.bind_eq_some_iff, reborrowTo_iff,
      eq_comm (a := t)]

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
  | .ordinary op => ∃ u, LoanStep s.own op u ∧ t = { s with own := u }
  | .owner p path m x => PassOwner s p path m x t
  | .view i x => PassDown s i x t
  | .entry args => ∃ u, EnterDown s.own args u ∧ t = { s with own := u }
  | .lifecycle e => CallLife types s e t

/-- Law (authority: docs/decisions.md#od15; #od23). Issuance realizes each ledger rule. -/
theorem issuanceStep_iff (types : Var → Option Descriptor) (s : CallState)
    (e : IssuanceAction) (t : CallState) : issuanceStep types s e = some t ↔ Issuance types s e t := by
  cases e <;>
    simp [issuanceStep, Issuance, passOwner_iff, passDown_iff, callLife_iff, Option.map_eq_some_iff,
      loanAction_iff, enterDown_iff, eq_comm (a := t)]

/-- Law (authority: docs/decisions.md#od15; #od23). Issuance traces compose legal steps. -/
theorem issuanceTrace_iff (types : Var → Option Descriptor) (s : CallState)
    (es : List IssuanceAction) (t : CallState) :
    es.foldlM (issuanceStep types) s = some t ↔ Trace (Issuance types) s es t :=
  fold_iff (issuanceStep types) (issuanceStep_iff types) s es t

/-- Helper (inversion). Exact executable realization implies relational determinism. -/
theorem deterministic {α : Type} {step : Option α} {rule : α → Prop}
    (law : ∀ t, step = some t ↔ rule t) {u v : α} (hu : rule u) (hv : rule v) : u = v :=
  Option.some.inj ((law u).mpr hu |>.symm.trans ((law v).mpr hv))

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
      | .ordinary op => Admitted LoanStep s.own op
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
    | ordinary | entry =>
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

end Controls

end LegalStep
end ConcreteSpec.Core.Own

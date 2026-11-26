import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Slice1

/-!
# Failed-obligation diagnostics (R-0599)

The payload uses Core obligation identities, rule revisions, integer types and trap kinds.
The slice-1 adapter walks the checker to its first rejected statement. It takes lexical origins
from elaboration and uses the statement ordinal only to navigate the semantic body.

Production attachment depends on the typed admission refusal from step 4(c) and lexical origins
from 4(d). The manifest control depends on the B4 payload extension to the fixture format in
`origin/wip/typed-fixtures:tests/manifest.tsv`.
-/

namespace ConcreteSpec.FailedObligation

inductive SearchResult (Evidence : Type) where
  | refutedReplayed (evidence : Evidence)
  | noWitness (casesChecked : Nat)
  deriving DecidableEq, Repr

/-- The two R-0599 statuses, shared by diagnostic renderers. -/
def SearchResult.message {Evidence : Type} : SearchResult Evidence → String
  | .refutedReplayed _ => "refuted, replayed"
  | .noWitness n => s!"no witness within {n} cases"

/-- Put the type extremes first, then in-range path constants in their original order,
with duplicates removed. -/
def boundaryValues (lower upper : Int) (constants : List Int) : Array Int :=
  ((lower :: upper :: constants).filter (fun n => lower ≤ n && n ≤ upper)).eraseDups.toArray

/-- Index a Cartesian product without constructing it. The first axis varies fastest;
the empty product contains exactly one input (the closed program's empty argument list). -/
def candidateAt {Input : Type} : List (Array Input) → Nat → Option (List Input)
  | [], index => if index = 0 then some [] else none
  | axis :: rest, index => do
    let value ← axis[index % axis.size]?
    let values ← candidateAt rest (index / axis.size)
    pure (value :: values)

private def candidateReplay {Input Evidence : Type} (input : List Input)
    (onPath : List Input → Bool) (replay : List Input → Option Evidence) : Option Evidence :=
  if onPath input then replay input else none

/-- The budget counts candidates examined, including ones excluded by path constraints.
The result records the actual number examined when the product runs out first. -/
def search {Input Evidence : Type} (axes : List (Array Input)) (budget : Nat)
    (onPath : List Input → Bool) (replay : List Input → Option Evidence) : SearchResult Evidence :=
  go 0 budget
where
  go (index : Nat) : Nat → SearchResult Evidence
    | 0 => .noWitness index
    | remaining + 1 =>
      match candidateAt axes index with
      | none => .noWitness index
      | some input =>
        match candidateReplay input onPath replay with
        | some evidence => .refutedReplayed evidence
        | none => go (index + 1) remaining

theorem search_refuted {Input Evidence : Type} {axes : List (Array Input)} {budget : Nat}
    {onPath : List Input → Bool} {replay : List Input → Option Evidence} {evidence : Evidence}
    (h : search axes budget onPath replay = .refutedReplayed evidence) :
    ∃ index < budget, ∃ input, candidateAt axes index = some input ∧
      onPath input = true ∧ replay input = some evidence := by
  have go : ∀ remaining index,
      search.go axes onPath replay index remaining = .refutedReplayed evidence →
      ∃ i, index ≤ i ∧ i < index + remaining ∧ ∃ input, candidateAt axes i = some input ∧
        onPath input = true ∧ replay input = some evidence := by
    intro remaining
    induction remaining with
    | zero => simp [search.go]
    | succ remaining ih =>
      intro index hr
      simp only [search.go] at hr
      split at hr
      · contradiction
      · rename_i input hc
        split at hr
        · rename_i found hf
          cases hr
          refine ⟨index, Nat.le_refl _, by omega, input, hc, ?_⟩
          simpa [candidateReplay, apply_ite] using hf
        · obtain ⟨i, hlo, hhi, rest⟩ := ih (index + 1) hr
          exact ⟨i, by omega, by omega, rest⟩
  obtain ⟨i, _, hi, rest⟩ := go budget 0 h
  exact ⟨i, by simpa using hi, rest⟩

theorem search_exhausted {Input Evidence : Type} {axes : List (Array Input)} {budget cases : Nat}
    {onPath : List Input → Bool} {replay : List Input → Option Evidence}
    (h : search axes budget onPath replay = .noWitness cases) :
    cases ≤ budget ∧ ∀ index < cases, ∀ input, candidateAt axes index = some input →
      onPath input = true → replay input = none := by
  have go : ∀ remaining start,
      search.go axes onPath replay start remaining = .noWitness cases →
      cases ≤ start + remaining ∧ ∀ index, start ≤ index → index < cases →
        ∀ input, candidateAt axes index = some input → onPath input = true → replay input = none := by
    intro remaining
    induction remaining with
    | zero =>
      intro start hr
      have he : start = cases := by simpa [search.go] using hr
      exact ⟨by omega, fun _ hlo hhi => by omega⟩
    | succ remaining ih =>
      intro start hr
      simp only [search.go] at hr
      split at hr
      · cases hr
        exact ⟨by omega, fun _ hlo hhi => by omega⟩
      · rename_i candidate hc
        split at hr
        · contradiction
        · rename_i hf
          obtain ⟨hmax, hprior⟩ := ih (start + 1) hr
          refine ⟨by omega, ?_⟩
          intro index hlo hhi input hinput hp
          by_cases he : index = start
          · subst index
            rw [hc] at hinput
            cases hinput
            simpa [candidateReplay, hp] using hf
          · exact hprior index (by omega) hhi input hinput hp
  obtain ⟨hb, ht⟩ := go budget 0 h
  exact ⟨by simpa using hb, fun i hi => ht i (Nat.zero_le i) hi⟩

theorem boundaryValues_bounds {lower upper value : Int} {constants : List Int}
    (h : value ∈ (boundaryValues lower upper constants).toList) :
    lower ≤ value ∧ value ≤ upper := by
  exact (show (_ ∨ _ ∨ _) ∧ lower ≤ value ∧ value ≤ upper by
    simpa [boundaryValues] using h).2

theorem boundaryValues_endpoints {lower upper : Int} (constants : List Int) (h : lower ≤ upper) :
    lower ∈ (boundaryValues lower upper constants).toList ∧
      upper ∈ (boundaryValues lower upper constants).toList := by
  simp [boundaryValues, h]

private theorem eraseDups_nodup (values : List Int) : values.eraseDups.Nodup := by
  cases values with
  | nil => simp
  | cons value rest =>
    rw [List.eraseDups_cons, List.nodup_cons]
    exact ⟨by simp, eraseDups_nodup _⟩
termination_by values.length
decreasing_by
  exact Nat.lt_succ_of_le (List.length_filter_le _ _)

theorem boundaryValues_nodup (lower upper : Int) (constants : List Int) :
    (boundaryValues lower upper constants).toList.Nodup := by
  exact eraseDups_nodup _

structure Bounds where
  lower : Int
  upper : Int
  deriving DecidableEq, Repr

def Bounds.contains (bounds : Bounds) (value : Int) : Bool :=
  bounds.lower ≤ value && value ≤ bounds.upper

structure Obligation where
  id : Core.ObligationId
  rule : Core.RuleId
  trap : Core.TrapKind
  type : Core.IntTy
  left : Bounds
  right : Bounds
  deriving DecidableEq, Repr

structure Witness where
  inputs : List (Nat × Int)
  left : Int
  right : Int
  deriving DecidableEq, Repr

structure Report where
  obligation : Obligation
  caseBudget : Nat
  replayFuel : Nat
  result : SearchResult Witness
  deriving DecidableEq, Repr

private def coreType : IntTy → Core.IntTy
  | .i8 => .i8
  | .i16 => .i16
  | .i32 => .i32
  | .i64 => .i64
  | .u8 => .u8
  | .u16 => .u16
  | .u32 => .u32
  | .u64 => .u64

private theorem coreType_inRange (type : IntTy) (value : Int) :
    decide ((coreType type).InRange value) = type.inRange value := by
  cases type <;> rfl

private structure RejectedAdd where
  index : Nat
  type : IntTy
  left : Bounds
  right : Bounds

/-- Select only the first statement the checker refuses, and only if supplying the arithmetic
evidence would suffice to pass every other check on that statement. -/
private def firstRejectedAdd (decls : Decls) (ctx : Ctx) (index : Nat) :
    List Stmt → Option RejectedAdd
  | [] => none
  | stmt :: rest =>
    match stmtOut decls ctx stmt with
    | some next => firstRejectedAdd decls next (index + 1) rest
    | none => do
      let .let_ _ (.scalar (.int type)) (.add left right) := stmt | none
      if admitted ctx (.add left right) || (stmtOutWith decls ctx (fun _ => true) stmt).isNone then
        none
      else
        let (_, llo, lhi) ← atomBounds ctx left
        let (_, rlo, rhi) ← atomBounds ctx right
        pure ⟨index, type, ⟨llo, lhi⟩, ⟨rlo, rhi⟩⟩

private def reachStatement (decls : Decls) :
    Nat → Nat → Env → List Stmt → Option (Env × Stmt)
  | _ + 1, 0, env, stmt :: _ => some (env, stmt)
  | fuel + 1, index + 1, env, stmt :: rest =>
    match execStmt decls env stmt with
    | .ok next => reachStatement decls fuel index next rest
    | _ => none
  | _, _, _, _ => none

private def replay (decls : Decls) (body : List Stmt) (target : RejectedAdd)
    (fuel : Nat) (env : Env) (inputs : List (Nat × Int)) : Option Witness := do
  let (before, stmt) ← reachStatement decls fuel target.index env body
  let .let_ _ _ (.add left right) := stmt | none
  let .ok (.int leftTy a, next) := evalAtom decls before left | none
  let .ok (.int rightTy b, _) := evalAtom decls next right | none
  if leftTy = target.type && rightTy = target.type &&
      target.type.inRange a && target.type.inRange b &&
      target.left.contains a && target.right.contains b &&
      !target.type.inRange (a + b) then
    match execStmt decls before stmt with
    | .trap .arithmeticOverflow => some ⟨inputs, a, b⟩
    | _ => none
  else none

private theorem reachStatement_trap {decls : Decls} {ending : Terminator} (fuel : Nat)
    {index : Nat} {env before : Env} {body : List Stmt} {stmt : Stmt}
    (h : reachStatement decls fuel index env body = some (before, stmt))
    (ht : execStmt decls before stmt = .trap .arithmeticOverflow) :
    runBody decls fuel env body ending = .trap .arithmeticOverflow := by
  induction fuel generalizing index env body with
  | zero => simp [reachStatement] at h
  | succ fuel ih =>
    cases body with
    | nil => cases index <;> simp [reachStatement] at h
    | cons head rest =>
      cases index with
      | zero =>
        simp [reachStatement] at h
        obtain ⟨rfl, rfl⟩ := h
        simp [runBody, ht]
      | succ index =>
        simp only [reachStatement] at h
        split at h
        · rename_i next he
          exact (show runBody decls (fuel + 1) env (head :: rest) ending = _ by
            simpa only [runBody, he] using ih h)
        · contradiction

private theorem replay_sound {decls : Decls} {body : List Stmt} {target : RejectedAdd}
    {fuel : Nat} {env : Env} {inputs : List (Nat × Int)} {witness : Witness}
    (ending : Terminator) (h : replay decls body target fuel env inputs = some witness) :
    runBody decls fuel env body ending = .trap .arithmeticOverflow ∧
      (coreType target.type).InRange witness.left ∧ (coreType target.type).InRange witness.right ∧
      ¬ (coreType target.type).InRange (witness.left + witness.right) := by
  unfold replay at h
  simp only [Option.bind_eq_bind, Option.bind_eq_some_iff] at h
  obtain ⟨⟨before, stmt⟩, hr, h⟩ := h
  split at h
  · split at h
    · split at h
      · split at h
        · rename_i hp
          split at h
          · rename_i ht
            cases h
            simp only [Bool.and_eq_true, decide_eq_true_eq] at hp
            refine ⟨reachStatement_trap fuel hr ht, ?_, ?_, ?_⟩
            · apply of_decide_eq_true
              rw [coreType_inRange]
              exact hp.1.1.1.1.2
            · apply of_decide_eq_true
              rw [coreType_inRange]
              exact hp.1.1.1.2
            · apply of_decide_eq_false
              rw [coreType_inRange]
              simpa using hp.2
          · contradiction
        · contradiction
      · contradiction
    · contradiction
  · contradiction

private def diagnose (decls : Decls) (ctx : Ctx) (body : List Stmt)
    (origin : Nat → Option Core.SiteId) (axes : List (Array Int))
    (onPath : List Int → Bool) (bind : List Int → Env × List (Nat × Int))
    (caseBudget replayFuel : Nat) : Option Report := do
  if !decide (DeclsOk decls) then none else
    let target ← firstRejectedAdd decls ctx 0 body
    let site ← origin target.index
    let obligation : Obligation :=
      ⟨⟨site, .safety, 0⟩, ⟨``admitted, 1⟩, .overflow, coreType target.type,
        target.left, target.right⟩
    let result := search axes caseBudget onPath fun candidate =>
      let (env, inputs) := bind candidate
      replay decls body target replayFuel env inputs
    pure ⟨obligation, caseBudget, replayFuel, result⟩

private theorem firstRejectedAdd_refuses {decls : Decls} {ending : Terminator} {target : RejectedAdd}
    {ctx : Ctx} {index : Nat} {body : List Stmt}
    (h : firstRejectedAdd decls ctx index body = some target) :
    bodyOkB decls ctx body ending = false := by
  induction body generalizing ctx index with
  | nil => simp [firstRejectedAdd] at h
  | cons stmt rest ih =>
    simp only [firstRejectedAdd] at h
    simp only [bodyOkB]
    split at h
    · rename_i next hs
      simpa only [hs] using ih h
    · rename_i hs
      simp [hs]

private theorem diagnose_search {decls : Decls} {ctx : Ctx} {body : List Stmt}
    {origin : Nat → Option Core.SiteId} {axes : List (Array Int)}
    {onPath : List Int → Bool} {bind : List Int → Env × List (Nat × Int)}
    {budget fuel : Nat} {report : Report} {witness : Witness}
    (h : diagnose decls ctx body origin axes onPath bind budget fuel = some report)
    (hw : report.result = .refutedReplayed witness) :
    ∃ target index input, firstRejectedAdd decls ctx 0 body = some target ∧
      report.obligation.type = coreType target.type ∧ index < budget ∧ candidateAt axes index = some input ∧ onPath input = true ∧
      replay decls body target fuel (bind input).1 (bind input).2 = some witness := by
  unfold diagnose at h
  split at h
  · contradiction
  · simp only [Option.bind_eq_bind, Option.bind_eq_some_iff] at h
    obtain ⟨target, ht, site, _, hr⟩ := h
    cases hr
    obtain ⟨i, hi, input, hc, hp, he⟩ := search_refuted hw
    exact ⟨target, i, input, ht, rfl, hi, hc, hp, he⟩

/-- Diagnose a closed semantic program using lexical sites from its elaboration. -/
def diagnoseClosed (program : Program) (origin : Nat → Option Core.SiteId)
    (caseBudget replayFuel : Nat) : Option Report :=
  diagnose program.decls [] program.body origin [] (fun _ => true) (fun _ => ([], []))
    caseBudget replayFuel

/-- A closed replay witness is a counterexample in both reference semantics, independently
of the origin table and of the search budget that found it. -/
theorem diagnoseClosed_sound {program : Program} {origin : Nat → Option Core.SiteId}
    {budget fuel : Nat} {report : Report} {witness : Witness}
    (h : diagnoseClosed program origin budget fuel = some report)
    (hw : report.result = .refutedReplayed witness) :
    run program fuel = .trap .arithmeticOverflow ∧
      Exec program.decls [] program.body program.ending (.trap .arithmeticOverflow) ∧
      check program = false ∧ ¬ Checked program ∧
      report.obligation.type.InRange witness.left ∧ report.obligation.type.InRange witness.right ∧
      ¬ report.obligation.type.InRange (witness.left + witness.right) := by
  obtain ⟨target, _, _, ht, htype, _, _, _, hr⟩ := diagnose_search h hw
  obtain ⟨hrun, hbounds⟩ := replay_sound program.ending hr
  have hb : bodyOkB program.decls [] program.body program.ending = false :=
    firstRejectedAdd_refuses ht
  have hc : check program = false := by simp [check, hb]
  refine ⟨hrun, runBody_sound hrun (by decide), hc, ?_, htype ▸ hbounds⟩
  intro checked
  have admittedProgram := (check_iff program).mpr checked
  simp [hc] at admittedProgram

/-- Even an exhausted closed diagnosis still refers to a checker rejection. -/
theorem diagnoseClosed_refused {program : Program} {origin : Nat → Option Core.SiteId}
    {budget fuel : Nat} {report : Report}
    (h : diagnoseClosed program origin budget fuel = some report) : ¬ Checked program := by
  unfold diagnoseClosed diagnose at h
  split at h
  · contradiction
  · simp only [Option.bind_eq_bind, Option.bind_eq_some_iff] at h
    obtain ⟨target, ht, _⟩ := h
    have hb : bodyOkB program.decls [] program.body program.ending = false :=
      firstRejectedAdd_refuses ht
    intro checked
    have admittedProgram := (check_iff program).mpr checked
    simp [check, hb] at admittedProgram

/-- Exercise the same replay on an open slice-1 body with integer inputs. Each candidate initializes
exactly the variables declared in the checker context. Path filtering counts against the budget. -/
def diagnoseInputs (program : Program) (origin : Nat → Option Core.SiteId)
    (parameters : List (Var × IntTy)) (constants : List Int) (onPath : List Int → Bool)
    (caseBudget replayFuel : Nat) : Option Report :=
  if !(parameters.map Prod.fst).Nodup then none else
    let ctx : Ctx := parameters.map fun (var, type) =>
      (var, { ty := .scalar (.int type), live := true, used := false, known := none })
    let axes := parameters.map fun (_, type) => boundaryValues type.min type.max constants
    diagnose program.decls ctx program.body origin axes onPath (fun values =>
      let bindings := parameters.zip values
      (bindings.map (fun ((var, type), value) => (var, .val (.scalar (.int type value)))),
        bindings.map (fun ((var, _), value) => (var.id, value)))) caseBudget replayFuel

end ConcreteSpec.FailedObligation

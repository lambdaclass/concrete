import ConcreteSpec.Diagnostics.Repr
import ConcreteSpec.Spec.Rules
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Surface1
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Proofs.Checker
import ConcreteSpec.Proofs.Slice1

/-!
# Failed-obligation diagnostics (R-0599)

The payload uses Core obligation identities, rule revisions, integer types and trap kinds.
The slice-1 adapter walks the checker to its first rejected statement. Source reports use the
lexical site emitted with that statement during elaboration.

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

theorem boundaryValues_constant {lower upper value : Int} {constants : List Int}
    (hm : value ∈ constants) (hl : lower ≤ value) (hu : value ≤ upper) :
    value ∈ (boundaryValues lower upper constants).toList := by
  simp [boundaryValues, hm, hl, hu]

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

/-- Semantic bodies have a statement index. Only elaboration supplies a lexical identity. -/
inductive Location where
  | statement (index : Nat)
  | source (id : Core.ObligationId)
  deriving DecidableEq, Repr

structure Obligation where
  location : Location
  rule : Core.RuleId
  trap : Core.TrapKind
  type : Core.IntTy
  left : Bounds
  right : Bounds
  deriving DecidableEq, Repr

structure Witness where
  inputs : List (Var × Int)
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

private def RejectedAdd.obligation (target : RejectedAdd) (location : Location) : Obligation :=
  ⟨location, ⟨``ProgramPermitted, 1⟩, .overflow, coreType target.type,
    target.left, target.right⟩

/-- Select only the first statement the checker refuses, and only if supplying the arithmetic
evidence would suffice to pass every other check on that statement. -/
private def firstRejectedAdd (decls : Decls) (ctx : Ctx) :
    List Stmt → Option RejectedAdd
  | [] => none
  | stmt :: rest =>
    match stmtOut decls ctx stmt with
    | some next => (firstRejectedAdd decls next rest).map fun target => { target with index := target.index + 1 }
    | none => do
      let .let_ _ (.scalar (.int type)) (.add left right) := stmt | none
      if admitted ctx (.add left right) || (stmtOutWith decls ctx (fun _ => true) stmt).isNone then
        none
      else
        let (_, llo, lhi) ← atomBounds ctx left
        let (_, rlo, rhi) ← atomBounds ctx right
        pure ⟨0, type, ⟨llo, lhi⟩, ⟨rlo, rhi⟩⟩

private def reachStatement (decls : Decls) :
    Nat → Nat → Env → List Stmt → Option (Env × Stmt)
  | _ + 1, 0, env, stmt :: _ => some (env, stmt)
  | fuel + 1, index + 1, env, stmt :: rest =>
    match execStmt decls env stmt with
    | .ok next => reachStatement decls fuel index next rest
    | _ => none
  | _, _, _, _ => none

private def replay (decls : Decls) (body : List Stmt) (target : RejectedAdd)
    (fuel : Nat) (env : Env) (inputs : List (Var × Int)) : Option Witness := do
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
    {fuel : Nat} {env : Env} {inputs : List (Var × Int)} {witness : Witness}
    (ending : Terminator) (h : replay decls body target fuel env inputs = some witness) :
    runBody decls fuel env body ending = .trap .arithmeticOverflow ∧
      (coreType target.type).InRange witness.left ∧ (coreType target.type).InRange witness.right ∧
      ¬ (coreType target.type).InRange (witness.left + witness.right) ∧
      target.left.contains witness.left = true ∧ target.right.contains witness.right = true ∧
      witness.inputs = inputs := by
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
            refine ⟨reachStatement_trap fuel hr ht, ?_, ?_, ?_, hp.1.1.2, hp.1.2, rfl⟩
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
    (origin : Nat → Option Location) (axes : List (Array Int))
    (onPath : List Int → Bool) (bind : List Int → Env × List (Var × Int))
    (caseBudget replayFuel : Nat) : Option Report := do
  if !decide (DeclsOk decls) then none else
    let target ← firstRejectedAdd decls ctx body
    let location ← origin target.index
    let result := search axes caseBudget onPath fun candidate =>
      let (env, inputs) := bind candidate
      replay decls body target replayFuel env inputs
    pure ⟨target.obligation location, caseBudget, replayFuel, result⟩

private theorem firstRejectedAdd_refuses {decls : Decls} {ending : Terminator} {target : RejectedAdd}
    {ctx : Ctx} {body : List Stmt}
    (h : firstRejectedAdd decls ctx body = some target) :
    bodyOkB decls ctx body ending = false := by
  induction body generalizing ctx target with
  | nil => simp [firstRejectedAdd] at h
  | cons stmt rest ih =>
    simp only [firstRejectedAdd] at h
    simp only [bodyOkB]
    split at h
    · rename_i next hs
      obtain ⟨prior, hp, _⟩ := Option.map_eq_some_iff.mp h
      simpa only [hs] using ih hp
    · rename_i hs
      simp [hs]

private theorem diagnosis {decls : Decls} {ctx : Ctx} {body : List Stmt}
    {origin : Nat → Option Location} {axes : List (Array Int)}
    {onPath : List Int → Bool} {bind : List Int → Env × List (Var × Int)}
    {budget fuel : Nat} {report : Report}
    (h : diagnose decls ctx body origin axes onPath bind budget fuel = some report) :
    DeclsOk decls ∧ ∃ target location, firstRejectedAdd decls ctx body = some target ∧
      origin target.index = some location ∧
      report = ⟨target.obligation location, budget, fuel,
        search axes budget onPath (fun input =>
          replay decls body target fuel (bind input).1 (bind input).2)⟩ := by
  unfold diagnose at h
  split at h
  · contradiction
  · rename_i hd
    have valid : DeclsOk decls := by simpa using hd
    simp only [Option.bind_eq_bind, Option.bind_eq_some_iff] at h
    obtain ⟨target, ht, location, hs, hr⟩ := h
    exact ⟨valid, target, location, ht, hs, (Option.some.inj hr).symm⟩

private theorem diagnostic_rule : (⟨``ProgramPermitted, 1⟩ : Core.RuleId) ∈ Core.registeredRules :=
  by decide

/-- Semantic-body diagnostics use statement ordinals as local identities. Use
`diagnoseSource` when the obligation needs a lexical source site. -/
def diagnoseClosed (program : Program) (caseBudget replayFuel : Nat) : Option Report :=
  diagnose program.decls [] program.body (fun index => some (.statement index)) []
    (fun _ => true) (fun _ => ([], [])) caseBudget replayFuel

/-- Diagnose a source program using the statement-to-site map from the very same elaboration. -/
def diagnoseSource (source : Surface1.SourceProgram) (caseBudget replayFuel : Nat) : Option Report := do
  let out ← Surface1.elaborate source
  diagnose out.core.decls [] out.core.body
    (fun index => out.statementSites[index]?.join.map (fun site => .source ⟨site, .safety, 0⟩))
    [] (fun _ => true) (fun _ => ([], [])) caseBudget replayFuel

private theorem firstRejectedAdd_statement {decls : Decls} {ctx : Ctx}
    {body : List Stmt} {target : RejectedAdd}
    (h : firstRejectedAdd decls ctx body = some target) :
    ∃ binder ty left right,
      body[target.index]? = some (Stmt.let_ binder ty (.add left right)) := by
  induction body generalizing ctx target with
  | nil => simp [firstRejectedAdd] at h
  | cons stmt rest ih =>
    simp only [firstRejectedAdd] at h
    split at h
    · obtain ⟨prior, hp, rfl⟩ := Option.map_eq_some_iff.mp h
      obtain ⟨binder, ty, left, right, hs⟩ := ih hp
      exact ⟨binder, ty, left, right, by simpa using hs⟩
    · split at h
      · split at h
        · contradiction
        · simp only [Option.bind_eq_bind, Option.bind_eq_some_iff] at h
          obtain ⟨⟨_, _, _⟩, _, ⟨_, _, _⟩, _, he⟩ := h
          cases he
          exact ⟨_, _, _, _, rfl⟩
      · contradiction

theorem diagnoseSource_origin {source : Surface1.SourceProgram} {budget fuel : Nat} {report : Report}
    (h : diagnoseSource source budget fuel = some report) :
    report.obligation.rule ∈ Core.registeredRules ∧
    ∃ (out : Surface1.Elaborated) (index : Nat) (site : Core.SiteId),
      Surface1.elaborate source = some out ∧
      report.obligation.location = .source ⟨site, .safety, 0⟩ ∧
      (∃ binder ty left right, out.core.body[index]? = some (Stmt.let_ binder ty (.add left right))) ∧
      out.statementSites[index]? = some (some site) := by
  simp only [diagnoseSource, Option.bind_eq_bind, Option.bind_eq_some_iff] at h
  obtain ⟨out, he, h⟩ := h
  obtain ⟨_, target, location, ht, hs, rfl⟩ := diagnosis h
  cases hx : out.statementSites[target.index]? with
  | none => simp [hx] at hs
  | some lexical =>
    cases lexical with
    | none => simp [hx] at hs
    | some site =>
      simp [hx] at hs
      subst location
      exact ⟨diagnostic_rule, out, target.index, site, he, rfl, firstRejectedAdd_statement ht, hx⟩

def inputContext (parameters : List (Var × IntTy)) : Ctx :=
  parameters.map fun (var, type) =>
    (var, { ty := .scalar (.int type), live := true, used := false, known := none })

def inputAxes (parameters : List (Var × IntTy)) (constants : List Int) : List (Array Int) :=
  parameters.map fun (_, type) => boundaryValues type.min type.max constants

def inputBindings (parameters : List (Var × IntTy)) (values : List Int) :
    Env × List (Var × Int) :=
  let bindings := parameters.zip values
  (bindings.map (fun ((var, type), value) => (var, .val (.scalar (.int type value)))),
    bindings.map (fun ((var, _), value) => (var, value)))

/-- Path filtering counts against the budget. Identities in the report are semantic statement
ordinals, as in `diagnoseClosed`. -/
def diagnoseInputs (program : Program) (parameters : List (Var × IntTy))
    (constants : List Int) (onPath : List Int → Bool) (caseBudget replayFuel : Nat) : Option Report :=
  if !(parameters.map Prod.fst).Nodup then none else
    diagnose program.decls (inputContext parameters) program.body (fun index => some (.statement index))
      (inputAxes parameters constants) onPath (inputBindings parameters) caseBudget replayFuel

/-- The replayed evaluator outcome and the checks performed on the actual operands. -/
structure ReplayValid (program : Program) (env : Env) (report : Report)
    (witness : Witness) : Prop where
  run : runBody program.decls report.replayFuel env program.body program.ending =
    .trap .arithmeticOverflow
  exec : Exec program.decls env program.body program.ending (.trap .arithmeticOverflow)
  leftInRange : report.obligation.type.InRange witness.left
  rightInRange : report.obligation.type.InRange witness.right
  sumOutOfRange : ¬ report.obligation.type.InRange (witness.left + witness.right)
  leftInBounds : report.obligation.left.contains witness.left = true
  rightInBounds : report.obligation.right.contains witness.right = true

private theorem replay_valid {program : Program} {env : Env} {target : RejectedAdd}
    {fuel budget : Nat} {location : Location} {result : SearchResult Witness}
    {inputs : List (Var × Int)} {witness : Witness}
    (h : replay program.decls program.body target fuel env inputs = some witness) :
    ReplayValid program env ⟨target.obligation location, budget, fuel, result⟩ witness ∧
      witness.inputs = inputs := by
  obtain ⟨hr, hl, hh, hs, hbl, hbr, hi⟩ := replay_sound program.ending h
  exact ⟨⟨hr, runBody_sound hr (by decide), hl, hh, hs, hbl, hbr⟩, hi⟩

private theorem refused {decls : Decls} {ending : Terminator} {ctx : Ctx}
    {body : List Stmt} {target : RejectedAdd}
    (h : firstRejectedAdd decls ctx body = some target) :
    bodyOkB decls ctx body ending = false ∧ ¬ BodyOk decls ctx body ending := by
  have hb := firstRejectedAdd_refuses (ending := ending) h
  exact ⟨hb, fun hok => Bool.noConfusion (hb.symm.trans (bodyOkB_iff.mpr hok))⟩

private theorem closed_refusal {program : Program} {target : RejectedAdd}
    (h : firstRejectedAdd program.decls [] program.body = some target) :
    check program = false ∧ ¬ Checked program := by
  have hb := firstRejectedAdd_refuses (ending := program.ending) h
  have hc : check program = false := by simp [check, hb]
  exact ⟨hc, fun hok => Bool.noConfusion (hc.symm.trans ((check_iff program).mpr hok))⟩

theorem diagnoseClosed_refused {program : Program} {budget fuel : Nat} {report : Report}
    (h : diagnoseClosed program budget fuel = some report) : ¬ Checked program := by
  obtain ⟨_, _, _, ht, _, _⟩ := diagnosis h
  exact (closed_refusal (program := program) ht).2

theorem diagnoseClosed_sound {program : Program} {budget fuel : Nat} {report : Report}
    {witness : Witness} (h : diagnoseClosed program budget fuel = some report)
    (hw : report.result = .refutedReplayed witness) :
    DeclsOk program.decls ∧ check program = false ∧ ¬ Checked program ∧
      witness.inputs = [] ∧ ReplayValid program [] report witness := by
  obtain ⟨hd, _, location, ht, _, rfl⟩ := diagnosis h
  obtain ⟨_, _, _, _, _, hr⟩ := search_refuted hw
  obtain ⟨hv, hi⟩ := replay_valid (program := program) (location := location) (budget := budget) hr
  obtain ⟨hb, hn⟩ := closed_refusal (program := program) ht
  exact ⟨hd, hb, hn, hi, hv⟩

theorem diagnoseSource_refused {source : Surface1.SourceProgram} {budget fuel : Nat} {report : Report}
    (h : diagnoseSource source budget fuel = some report) :
    ¬ Surface1.SourcePermitted source := by
  simp only [diagnoseSource, Option.bind_eq_bind, Option.bind_eq_some_iff] at h
  obtain ⟨out, he, h⟩ := h
  obtain ⟨_, _, _, ht, _, _⟩ := diagnosis h
  simp [Surface1.SourcePermitted, he, (closed_refusal (program := out.core) ht).1]

theorem diagnoseSource_sound {source : Surface1.SourceProgram} {budget fuel : Nat} {report : Report}
    {witness : Witness} (h : diagnoseSource source budget fuel = some report)
    (hw : report.result = .refutedReplayed witness) :
    ∃ out, Surface1.elaborate source = some out ∧ DeclsOk out.core.decls ∧
      witness.inputs = [] ∧ ReplayValid out.core [] report witness := by
  simp only [diagnoseSource, Option.bind_eq_bind, Option.bind_eq_some_iff] at h
  obtain ⟨out, he, h⟩ := h
  obtain ⟨hd, _, location, _, _, rfl⟩ := diagnosis h
  obtain ⟨_, _, _, _, _, hr⟩ := search_refuted hw
  obtain ⟨hv, hi⟩ := replay_valid (program := out.core) (location := location) (budget := budget) hr
  exact ⟨out, he, hd, hi, hv⟩

theorem diagnoseSource_exhausted {source : Surface1.SourceProgram} {budget fuel cases : Nat}
    {report : Report} (h : diagnoseSource source budget fuel = some report)
    (hw : report.result = .noWitness cases) :
    cases ≤ report.caseBudget ∧ report.caseBudget = budget := by
  simp only [diagnoseSource, Option.bind_eq_bind, Option.bind_eq_some_iff] at h
  obtain ⟨_, _, h⟩ := h
  obtain ⟨_, _, _, _, _, rfl⟩ := diagnosis h
  exact ⟨(search_exhausted hw).1, rfl⟩

theorem diagnoseClosed_registered {program : Program} {budget fuel : Nat} {report : Report}
    (h : diagnoseClosed program budget fuel = some report) :
    report.obligation.rule ∈ Core.registeredRules := by
  obtain ⟨_, _, _, _, _, rfl⟩ := diagnosis h
  exact diagnostic_rule

theorem diagnoseClosed_exhausted {program : Program} {budget fuel cases : Nat} {report : Report}
    (h : diagnoseClosed program budget fuel = some report)
    (hw : report.result = .noWitness cases) :
    cases ≤ report.caseBudget ∧ report.caseBudget = budget := by
  obtain ⟨_, _, _, _, _, rfl⟩ := diagnosis h
  exact ⟨(search_exhausted hw).1, rfl⟩

/-- Parameters and chosen values pair one-to-one, in order and within each declared type. -/
inductive InputsInRange : List (Var × IntTy) → List Int → Prop where
  | nil : InputsInRange [] []
  | cons {var type params value values} : type.inRange value = true →
      InputsInRange params values → InputsInRange ((var, type) :: params) (value :: values)

private theorem inputCandidate {parameters : List (Var × IntTy)} {constants values : List Int}
    {index : Nat} (h : candidateAt (inputAxes parameters constants) index = some values) :
    InputsInRange parameters values := by
  induction parameters generalizing index values with
  | nil =>
    simp only [inputAxes, List.map_nil, candidateAt] at h
    split at h
    · cases h
      exact .nil
    · contradiction
  | cons param rest ih =>
    obtain ⟨var, type⟩ := param
    simp only [inputAxes, List.map_cons, candidateAt, Option.bind_eq_bind,
      Option.bind_eq_some_iff] at h
    obtain ⟨value, hv, tail, ht, heq⟩ := h
    cases Option.some.inj heq
    obtain ⟨hlt, he⟩ := Array.getElem?_eq_some_iff.mp hv
    have hm := Array.getElem_mem_toList hlt
    rw [he] at hm
    obtain ⟨hl, hu⟩ := boundaryValues_bounds hm
    exact .cons (by simp [IntTy.inRange, hl, hu]) (ih ht)

private theorem inputCorr {program : Program} {parameters : List (Var × IntTy)} {values : List Int}
    (h : InputsInRange parameters values) :
    Corr program.decls (inputContext parameters) (inputBindings parameters values).1 ∧
      (inputBindings parameters values).2 = (parameters.map Prod.fst).zip values := by
  induction h with
  | nil => exact ⟨.nil, rfl⟩
  | cons hv _ ih =>
    refine ⟨.cons (fun _ => ⟨_, rfl, .scalar (.int hv)⟩)
      (fun _ h => by contradiction) (fun _ _ h => by cases h) ih.1, ?_⟩
    simp only [inputBindings, List.zip_cons_cons, List.map_cons] at *
    rw [ih.2]

private theorem input_diagnosis {program : Program} {parameters : List (Var × IntTy)}
    {constants : List Int} {onPath : List Int → Bool} {budget fuel : Nat} {report : Report}
    (h : diagnoseInputs program parameters constants onPath budget fuel = some report) :
    (parameters.map Prod.fst).Nodup ∧
    diagnose program.decls (inputContext parameters) program.body (fun index => some (.statement index))
      (inputAxes parameters constants) onPath (inputBindings parameters) budget fuel = some report := by
  unfold diagnoseInputs at h
  split at h
  · contradiction
  · rename_i hn
    exact ⟨by simpa using hn, h⟩

theorem diagnoseInputs_refused {program : Program} {parameters : List (Var × IntTy)}
    {constants : List Int} {onPath : List Int → Bool} {budget fuel : Nat} {report : Report}
    (h : diagnoseInputs program parameters constants onPath budget fuel = some report) :
    bodyOkB program.decls (inputContext parameters) program.body program.ending = false ∧
      ¬ BodyOk program.decls (inputContext parameters) program.body program.ending := by
  obtain ⟨_, _, _, ht, _, _⟩ := diagnosis (input_diagnosis h).2
  exact refused ht

theorem diagnoseInputs_registered {program : Program} {parameters : List (Var × IntTy)}
    {constants : List Int} {onPath : List Int → Bool} {budget fuel : Nat} {report : Report}
    (h : diagnoseInputs program parameters constants onPath budget fuel = some report) :
    report.obligation.rule ∈ Core.registeredRules := by
  obtain ⟨_, _, _, _, _, rfl⟩ := diagnosis (input_diagnosis h).2
  exact diagnostic_rule

theorem diagnoseInputs_exhausted {program : Program} {parameters : List (Var × IntTy)}
    {constants : List Int} {onPath : List Int → Bool} {budget fuel cases : Nat} {report : Report}
    (h : diagnoseInputs program parameters constants onPath budget fuel = some report)
    (hw : report.result = .noWitness cases) :
    cases ≤ report.caseBudget ∧ report.caseBudget = budget := by
  obtain ⟨_, _, _, _, _, rfl⟩ := diagnosis (input_diagnosis h).2
  exact ⟨(search_exhausted hw).1, rfl⟩

theorem diagnoseInputs_sound {program : Program} {parameters : List (Var × IntTy)}
    {constants : List Int} {onPath : List Int → Bool} {budget fuel : Nat} {report : Report}
    {witness : Witness} (h : diagnoseInputs program parameters constants onPath budget fuel = some report)
    (hw : report.result = .refutedReplayed witness) :
    DeclsOk program.decls ∧ (parameters.map Prod.fst).Nodup ∧
    bodyOkB program.decls (inputContext parameters) program.body program.ending = false ∧
    ¬ BodyOk program.decls (inputContext parameters) program.body program.ending ∧
    ∃ index < report.caseBudget, ∃ values,
      candidateAt (inputAxes parameters constants) index = some values ∧
      onPath values = true ∧ InputsInRange parameters values ∧
      witness.inputs = (parameters.map Prod.fst).zip values ∧
      Corr program.decls (inputContext parameters) (inputBindings parameters values).1 ∧
      ReplayValid program (inputBindings parameters values).1 report witness := by
  obtain ⟨hn, hi⟩ := input_diagnosis h
  obtain ⟨hd, _, location, ht, _, rfl⟩ := diagnosis hi
  obtain ⟨hb, hr⟩ := refused (ending := program.ending) ht
  obtain ⟨index, hindex, values, hc, hp, he⟩ := search_refuted hw
  have hv := inputCandidate hc
  obtain ⟨hCorr, hzip⟩ := inputCorr (program := program) hv
  obtain ⟨hReplay, hInputs⟩ := replay_valid (program := program) (location := location) (budget := budget) he
  exact ⟨hd, hn, hb, hr, index, hindex, values, hc, hp, hv, hInputs.trans hzip, hCorr, hReplay⟩

end ConcreteSpec.FailedObligation

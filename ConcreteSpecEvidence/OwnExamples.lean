import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Spec.Scalar
import ConcreteSpec.Spec.Hybrid
import ConcreteSpec.Spec.Exec
import ConcreteSpec.Spec.Own

namespace ConcreteSpec.Core.Own

namespace Examples

def x : Var := ⟨0⟩
def y : Var := ⟨1⟩
def types (_ : Var) : Option Descriptor := some ⟨.int .i32, false, true⟩
def initial : State :=
  { slots := [⟨⟨0⟩, 0, ⟨.int .i32, false, true⟩, .owned⟩]
    frames := [⟨0, [(x, ⟨0⟩)], []⟩]
    nextPlace := 1
    nextActivation := 1 }

def shared : State := (borrow initial ⟨0⟩ [.index 0] .shared).getD initial
def exclusive : State := (borrow initial ⟨0⟩ [.index 0] .mutable).getD initial
def child : State := (reborrow exclusive ⟨0⟩ [.index 1] .mutable).getD initial

/-- Instance: Accepted (rule `ConcreteSpec.Core.Own.ownerAccess`): shared loans allow owner reads. -/
theorem example_Accepted_rule_ConcreteSpec_Core_Own_ownerAccess : ownerAccess shared ⟨0⟩ .shared = true := by decide
/-- Instance: Refused (rule `ConcreteSpec.Core.Own.ownerAccess`): shared loans freeze owner writes. -/
theorem example_Refused_rule_ConcreteSpec_Core_Own_ownerAccess : ownerAccess shared ⟨0⟩ .mutable = false := by decide
/-- Instance: Refused: shared loans also prevent moving the owner. -/
theorem example_Refused_shared_loans_also_prevent_moving : move shared ⟨0⟩ = none := by decide
/-- Instance: Accepted: shared loans of disjoint or overlapping projections coexist. -/
theorem example_Accepted_shared_loans_disjoint_or_overlapping : (borrow shared ⟨0⟩ [.index 1] .shared).isSome = true := by decide
/-- Instance: Refused: disjoint indices do not bypass mutable whole-root conflicts. -/
theorem example_Refused_disjoint_indices_do_not_bypass : borrow exclusive ⟨0⟩ [.index 999] .shared = none := by decide
/-- Instance: Refused: an exclusive loan blocks even owner reads. -/
theorem example_Refused_exclusive_loan_blocks_even_owner : ownerAccess exclusive ⟨0⟩ .shared = false := by decide
/-- Instance: Accepted (rule `ConcreteSpec.Core.Own.loanAccess`): the mutable child accesses its root. -/
theorem example_Accepted_rule_ConcreteSpec_Core_Own_loanAccess : loanAccess child ⟨1⟩ .mutable = true := by decide
/-- Instance: Refused (rule `ConcreteSpec.Core.Own.loanAccess`): a mutable child suspends its parent. -/
theorem example_Refused_rule_ConcreteSpec_Core_Own_loanAccess : loanAccess child ⟨0⟩ .shared = false := by decide
/-- Instance: Refused: shared lending cannot upgrade to mutable access. -/
theorem example_Refused_shared_lending_cannot_upgrade_mutable : reborrow shared ⟨0⟩ [] .mutable = none := by decide
/-- Instance: Refused: a shared child freezes writes through its mutable parent, but permits reads. -/
theorem example_Refused_shared_child_freezes_writes_through : (do
    let s ← reborrow exclusive ⟨0⟩ [] .shared
    pure (loanAccess s ⟨0⟩ .shared, loanAccess s ⟨0⟩ .mutable)) = some (true, false) := by decide
/-- Instance: Accepted (rule `ConcreteSpec.Core.Own.endLoan`): end the child, then use the parent. -/
theorem example_Accepted_rule_ConcreteSpec_Core_Own_endLoan : ((endLoan child ⟨1⟩).map fun s => loanAccess s ⟨0⟩ .mutable) = some true := by decide
/-- Instance: Refused (rule `ConcreteSpec.Core.Own.endLoan`): parents cannot end before their children. -/
theorem example_Refused_rule_ConcreteSpec_Core_Own_endLoan : endLoan child ⟨0⟩ = none := by decide
/-- Instance: Refused: moving twice or reading a moved slot; reinitializing makes it usable again. -/
theorem example_Refused_moving_twice_or_reading_moved : (do
    let s ← move initial ⟨0⟩
    pure ((move s ⟨0⟩).isNone, ownerAccess s ⟨0⟩ .shared,
      (reinitialize s ⟨0⟩).isSome)) = some (true, false, true) := by decide
/-- Instance: Refused: leaving a live linear owner would silently discard its obligation. -/
theorem example_Refused_leaving_live_linear_owner_would : leave initial = none := by decide
/-- Instance: Accepted: downward lending preserves the root; leaving restores the caller's parent access. -/
theorem example_Accepted_downward_lending_preserves_root_leaving : (do
    let s ← enterDown exclusive [(y, ⟨0⟩)]
    let frozen := loanAccess s ⟨0⟩ .shared
    let s ← leave s
    pure (frozen, loanAccess s ⟨0⟩ .mutable, s.nextLoan)) = some (false, true, 2) := by decide
/-- Instance: Refused: a root with a loan cannot leave scope. -/
theorem example_Refused_root_loan_cannot_leave_scope : drop [x] { shared with slots := shared.slots.map fun x =>
    { x with desc := { x.desc with copy := true } } } = none := by decide
/-- Instance: Accepted: identities are not reused even after the prior loan ends. -/
theorem example_Accepted_identities_are_not_reused_even : (do
    let s ← endLoan shared ⟨0⟩
    let s ← borrow s ⟨0⟩ [] .shared
    pure (s.loans.map Loan.id)) = some [⟨1⟩] := by decide

/-- Instance: Refused: a logical token has a linear obligation independently of representation
(Q3.8). No C4 representation-size sentinel is used as evidence. -/
theorem example_Refused_logical_token_has_linear_obligation : drop [x] { initial with slots := [⟨⟨0⟩, 0, ⟨.unit, false, true⟩, .owned⟩] } = none := by decide
/-- Instance: Accepted: explicitly consuming the logical unit obligation, without selecting its byte layout permits its slot to end. -/
theorem example_Accepted_explicitly_consuming_logical_unit_obligation : (do
    let s ← move { initial with slots := [⟨⟨0⟩, 0, ⟨.unit, false, true⟩, .owned⟩] } ⟨0⟩
    drop [x] s).isSome = true := by decide

/-- A small operand instance: all loan commands modify instrumentation only. There are no
reference-valued fields, returns or aggregate operands. -/
inductive E where
  | lit (n : Int)
  | fail (k : TrapKind)
  | move (x : Var)
  | read (x : Var)
  | lend (root : Hybrid.Place) (mode : Mode)
  | reborrow (parent : LoanId)
  | endLoan (id : LoanId)
  | use (id : LoanId)

def eval (locals : AList Var Int) (s : State) : E → Res (Int × State)
  | .lit n => .ok (n, s)
  | .fail k => .trap k
  | .move x => match s.frames.head?.bind (fun f => AList.lookup f.locals x),
      AList.lookup locals x with
    | some p, some n => match Own.move s p with
      | some s => .ok (n, s)
      | none => .stuck
    | _, _ => .stuck
  | .read x => match s.frames.head?.bind (fun f => AList.lookup f.locals x),
      AList.lookup locals x with
    | some p, some n => if ownerAccess s p .shared then .ok (n, s) else .stuck
    | _, _ => .stuck
  | .lend p m => match borrow s p [] m with
    | some s => .ok (0, s)
    | none => .stuck
  | .reborrow i => match Own.reborrow s i [] .mutable with
    | some s => .ok (0, s)
    | none => .stuck
  | .endLoan i => match Own.endLoan s i with
    | some s => .ok (0, s)
    | none => .stuck
  | .use i => if loanAccess s i .mutable then .ok (0, s) else .stuck

abbrev ops : Exec.Operands where
  Val := Int
  Expr := E
  Ty := Unit
  Store := State
  Prim := Empty
  Event := Empty
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => false
  eval := eval
  life := Own.life types
  test := fun n => some (n != 0)
  allows := fun p => nomatch p

def runBody (body : Exec.Stmt ops) : Exec.Report ops :=
  Exec.run ⟨[]⟩ (fun _ p => nomatch p) 100 ⟨.exec body, [(x, 7)], initial, [.functionEnd]⟩

def stuck (r : Exec.Report ops) : Bool := match r.ending with
  | .stuck => true
  | _ => false

/-- Instance: Refused: an actual C1 run gets stuck when its next operand reads a moved owner. -/
theorem example_Refused_actual_C1_run_gets_stuck : stuck (runBody (.seq (.bind y (.move x) ⟨0⟩) (.ret (.read x) ⟨1⟩))) = true := by decide

/-- Ownership permission transition for `E`, coupled to `erasedOps` through
`instrumentOperands`. -/
def permitE (_ : AList Var Int) : E → State → Option State
  | .lit _, s | .fail _, s => some s
  | .move x, s => (s.frames.head?.bind fun f => AList.lookup f.locals x).bind (Own.move s)
  | .read x, s => (s.frames.head?.bind fun f => AList.lookup f.locals x).bind fun p =>
      accessOwner s p .shared
  | .lend p m, s => borrow s p [] m
  | .reborrow i, s => Own.reborrow s i [] .mutable
  | .endLoan i, s => Own.endLoan s i
  | .use i, s => accessLoan s i .mutable

/-- The concrete operand instance erases all permission bookkeeping: reads and moves fetch
their concrete value from `locals`, and the primitive emits its argument as an event while
adding it to the concrete `Int` store. -/
def erasedEval (locals : AList Var Int) (st : Int) : E → Res (Int × Int)
  | .lit n => .ok (n, st)
  | .fail k => .trap k
  | .read x | .move x => match AList.lookup locals x with
    | some n => .ok (n, st)
    | none => .stuck
  | _ => .ok (0, st)

abbrev erasedOps : Exec.Operands where
  Val := Int
  Expr := E
  Ty := Unit
  Store := Int
  Prim := Unit
  Event := Int
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => false
  eval := erasedEval
  life := fun _ s => some s
  test := fun n => some (n != 0)
  allows := fun _ vs st a => match vs, a.event, a.result with
    | [v], some ev, .ok r => decide (ev = v) && decide (r = st + v) && decide (a.store = st + v)
    | _, _, _ => false

abbrev instrumentedOps : Exec.Operands :=
  instrumentOperands erasedOps types permitE

def erasedOracle : Exec.Oracle erasedOps := fun _ _ vs _ st => match vs with
  | [v] => some ⟨some v, .ok (st + v), st + v⟩
  | _ => none

def moveConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.seq (.bind y (.move x) ⟨0⟩) (.ret (.read x) ⟨1⟩)), [(x, 7)], ⟨0, initial⟩, [.functionEnd]⟩

def acceptedMoveConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.seq (.bind y (.move x) ⟨0⟩) (.ret (.move y) ⟨1⟩)), [(x, 7)], ⟨0, initial⟩, [.functionEnd]⟩

def acceptedStuckConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.assign y (.lit 1) ⟨0⟩), [(x, 7)], ⟨0, initial⟩, [.functionEnd]⟩

def primTraceConfig : Exec.Config instrumentedOps :=
  ⟨.exec (.seq (.prim (some y) () [.move x] ⟨0⟩)
    (.seq (.prim none () [.lit 3] ⟨1⟩) (.ret (.move y) ⟨2⟩))), [(x, 7)], ⟨0, initial⟩, [.functionEnd]⟩

def badOracle : Exec.Oracle erasedOps := fun _ _ _ _ st =>
  some ⟨some 99, .ok st, st⟩

def instrumentedMoveRun : Exec.Report instrumentedOps :=
  Exec.run ⟨[]⟩ (liftOracle erasedOps types permitE erasedOracle) 100 moveConfig

def erasedMoveRun : Exec.Report erasedOps :=
  Exec.run ⟨[]⟩ erasedOracle 100 (eraseConfig moveConfig)

/-- Instance: an unsafe execution refutes unconditional equality of final classifications:
the instrumented run is stuck, but the erased concrete run returns 7. A checker-safety
premise or a one-way successful-execution theorem is necessary for C1 erasure. -/
theorem example_unsafe_execution_refutes_unconditional_equality_final : stuck (runBody (.seq (.bind y (.move x) ⟨0⟩) (.ret (.read x) ⟨1⟩))) = true ∧
    Ending.IsStuck instrumentedMoveRun.ending = true ∧
    (match erasedMoveRun.ending with
      | .final (.returned n) => n == 7
      | _ => false) = true := by decide
/-- Instance: erased execution does not refuse the use after move. -/
theorem example_erased_execution_does_not_refuse_use : ¬ (match erasedMoveRun.ending with
    | .stuck => true
    | _ => false) = true := by decide

/-- Planted: omit exactly one operand's permission transition, retaining its erased
concrete evaluation. The selector is a fixture operation kind, never a source policy. -/
def operationKind : E → Nat
  | .move _ => 0 | .read _ => 1 | .lend _ _ => 2
  | .reborrow _ => 3 | .endLoan _ => 4 | .use _ => 5
  | _ => 6

def omitPermission (kind : Nat) (locals : AList Var Int) (e : E) (s : State) : Option State :=
  if operationKind e = kind then some s else permitE locals e s

/-- Instance: each populated operation is refused for its own permission violation.
The move/read roots exist in concrete storage; lending conflicts with an existing
exclusive loan; reborrow attempts a shared upgrade; ending a parent strands its child;
using a parent bypasses its suspended access. -/
def refusalCases : List (Nat × E × State) :=
  [(0, .move x, shared), (1, .read x, exclusive),
   (2, .lend ⟨0⟩ .shared, exclusive), (3, .reborrow ⟨0⟩, shared),
   (4, .endLoan ⟨0⟩, child), (5, .use ⟨0⟩, child)]

theorem example_refusalCases_all_e_own : refusalCases.all (fun (_, e, own) =>
    match instrumentEval erasedOps permitE [(x, 7)] ⟨0, own⟩ e with
    | .stuck => true | _ => false) = true := by decide

/-- Planted: six separate omission mutants all accept the corresponding refused
operand. No concrete failure or another ownership check masks any omission. -/
theorem planted_six_separate_omission_mutants_all_accept : refusalCases.all (fun (kind, e, own) =>
    match instrumentEval erasedOps (omitPermission kind) [(x, 7)] ⟨0, own⟩ e with
    | .ok _ => true | _ => false) = true := by decide

/-- Instance: Accepted: a C1 run lends, reborrows, ends the child, then uses its parent and ends it.
Conditions evaluate these instrumentation-only operands without creating ordinary bindings. -/
theorem example_Accepted_C1_run_lends_reborrows_ends : (match (runBody
    (.cond (.lend ⟨0⟩ .mutable) .skip
      (.cond (.reborrow ⟨0⟩) .skip
        (.cond (.endLoan ⟨1⟩) .skip
          (.cond (.use ⟨0⟩) .skip
            (.cond (.endLoan ⟨0⟩) .skip (.ret (.move x) ⟨1⟩) ⟨0⟩) ⟨0⟩) ⟨0⟩) ⟨0⟩) ⟨0⟩)).ending with
    | .final (.returned 7) => true
    | _ => false) = true := by decide

/-- Instance: Refused: an existing owner loan or reborrow is not a pending argument child and cannot
be adopted into a callee, even when all supplied loan identities exist. -/
theorem example_Refused_existing_owner_loan_or_reborrow : (do
    let s ← enter types [] exclusive
    adoptDown s [(y, ⟨0⟩)]) = none := by decide
/-- Instance: an existing child is not a pending argument. -/
theorem example_existing_child_is_not_pending_argument : (do
    let s ← enter types [] child
    adoptDown s [(y, ⟨1⟩)]) = none := by decide
/-- Instance: Refused: an incomplete locals table that omits an active frame's slot violates `WellFormed`. -/
theorem example_Refused_incomplete_locals_table_omits_active : ¬ WellFormed { initial with frames := [⟨0, [], []⟩] } := by decide

/-- The following C1 instance exercises actual calls with downward view parameters. -/
inductive CE where
  | value (e : E)
  | argument (source : LoanId) (parameter : Var)
  | ownerArgument (root : Hybrid.Place) (mode : Mode) (parameter : Var)
  | useView (parameter : Var)

def callEval (locals : AList Var Int) (s : CallState) : CE → Res (Int × CallState)
  | .value e => (eval locals s.own e).map fun (v, own) => (v, { s with own := own })
  | .argument i x => match passDown s i x with
    | some s => .ok (0, s)
    | none => .stuck
  | .ownerArgument p m x => match passOwner s p [] m x with
    | some s => .ok (0, s)
    | none => .stuck
  | .useView x => match s.own.frames.head?.bind (fun f => AList.lookup f.views x) with
    | some i => if loanAccess s.own i .mutable then .ok (0, s) else .stuck
    | none => .stuck

abbrev callOps : Exec.Operands where
  Val := Int
  Expr := CE
  Ty := Unit
  Store := CallState
  Prim := Empty
  Event := Empty
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => false
  eval := callEval
  life := callLife types
  test := fun n => some (n != 0)
  allows := fun p => nomatch p

def callee : FnId := ⟨ModuleId.mk ⟨⟨"integration-fixtures"⟩, ⟨⟨"0"⟩⟩⟩ [⟨"fixtures"⟩] (by decide), ⟨"callee"⟩⟩
def callProgram : Exec.Program callOps :=
  ⟨[(callee, ⟨[(y, ())], (), ⟨1⟩,
    .cond (.useView y) .skip .skip ⟨0⟩⟩)]⟩

def callRun (body : Exec.Stmt callOps) : Exec.Report callOps :=
  Exec.run callProgram (fun _ p => nomatch p) 100
    ⟨.exec body, [(x, 7)], ⟨exclusive, []⟩, [.functionEnd]⟩

/-- Instance: Accepted: a real C1 call lends the mutable parent to its parameter; return ends the child,
then the caller can use its original parent again. No reference is a returned/stored value. -/
theorem example_Accepted_real_C1_call_lends_mutable : (match (callRun (.seq (.call none callee [.argument ⟨0⟩ y] ⟨0⟩)
    (.seq (.cond (.value (.use ⟨0⟩)) .skip .skip ⟨0⟩)
      (.seq (.cond (.value (.endLoan ⟨0⟩)) .skip .skip ⟨0⟩)
        (.ret (.value (.move x)) ⟨1⟩))))).ending with
    | .final (.returned 7) => true
    | _ => false) = true := by decide

/-- Two parameters make argument refusal independent of C1's arity check. -/
def freezeProgram : Exec.Program callOps :=
  ⟨[(callee, ⟨[(y, ()), (x, ())], (), ⟨1⟩, .ret (.value (.move x)) ⟨0⟩⟩)]⟩

def freezeArgs : List CE := [.argument ⟨0⟩ y, .value (.use ⟨0⟩)]

/-- Instance: OD23 refuses the second operand itself, before lookup, arity or entry. -/
theorem example_OD23_refuses_second_operand_itself_before : Exec.evalArgs callOps [(x, 7)] ⟨exclusive, []⟩ freezeArgs = .stuck := by decide
/-- Instance: the correctly shaped actual call stops at that same operand boundary. -/
theorem example_correctly_shaped_actual_call_stops_at : (Exec.run freezeProgram (fun _ p => nomatch p) 20
    ⟨.exec (.call none callee freezeArgs ⟨0⟩), [(x, 7)], ⟨exclusive, []⟩,
      [.functionEnd]⟩).fuel = 20 := by rfl

/-- Planted: omit only the parent-access check on the second argument. The identical
argument observation now succeeds, so it cannot satisfy the refusal Instance. -/
abbrev omitArgumentAccess : Exec.Operands := { callOps with
  eval := fun locals s e => match e with
    | .value (.use _) => .ok (0, s)
    | _ => callEval locals s e }

/-- Planted: bypassing the second operand access check defeats argument refusal. -/
theorem planted_bypassing_second_operand_access_check_defeats : (match Exec.evalArgs omitArgumentAccess [(x, 7)] ⟨exclusive, []⟩ freezeArgs with
    | .ok _ => true | _ => false) =
    true := by decide

/-- Populated unrelated root and loan: call lending must preserve both exactly. -/
def ownerCaller : State :=
  { initial with
    slots := initial.slots ++ [⟨⟨1⟩, 0, ⟨.int .i32, true, true⟩, .owned⟩]
    frames := [⟨0, [(x, ⟨0⟩), (⟨2⟩, ⟨1⟩)], []⟩]
    loans := [⟨⟨0⟩, ⟨1⟩, [], .shared, none, [], 0, false, false⟩]
    nextPlace := 2
    nextLoan := 1 }

/-- Instance: direct-owner lending cannot bypass a populated conflicting loan.
The root and current activation exist; only whole-root exclusion refuses it. -/
theorem example_direct_owner_lending_cannot_bypass_populated : passOwner ⟨exclusive, []⟩ ⟨0⟩ [] .shared y = none := by decide

/-- Observe the loan during the callee, including whole-owner exclusion and the
unchanged unrelated caller loan. This operand has no permission side effects. -/
abbrev ownerOps : Exec.Operands := { callOps with eval := fun locals s e => match e with
  | .useView _ =>
    if !ownerAccess s.own ⟨0⟩ .shared &&
        loan s.own ⟨0⟩ == loan ownerCaller ⟨0⟩ then callEval locals s e else .stuck
  | _ => callEval locals s e }

def ownerProgram : Exec.Program ownerOps :=
  ⟨[(callee, ⟨[(y, ())], (), ⟨1⟩, .cond (.useView y) .skip .skip ⟨0⟩⟩)]⟩

def ownerBody (deferred : Bool) : Exec.Stmt ownerOps :=
  .block ⟨2⟩ (if deferred then
    .seq (.defer callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩)
      (.cond (.value (.read x)) .skip .skip ⟨0⟩)
    else .call none callee [.ownerArgument ⟨0⟩ .mutable y] ⟨0⟩)

def ownerRun (deferred : Bool) : Exec.Report ownerOps :=
  Exec.run ownerProgram (fun _ p => nomatch p) 100
    ⟨.exec (ownerBody deferred), [(x, 7), (⟨2⟩, 9)], ⟨ownerCaller, []⟩, []⟩

/-- Instance: immediate direct-owner lending is live during the callee and ends on
return. The unrelated populated loan survives unchanged; owner reads are restored. -/
theorem example_immediate_direct_owner_lending_is_live : (match (ownerRun false).ending with | .final .normal => true | _ => false) = true ∧
    (ownerRun false).residual.store.own.loans = ownerCaller.loans ∧
    ownerAccess (ownerRun false).residual.store.own ⟨0⟩ .mutable = true := by decide

/-- Instance: deferred registration leaves owner reads available until cleanup.
The callee observes the same exclusion as an immediate call; return restores access
without ending the pre-existing loan. -/
theorem example_deferred_registration_leaves_owner_reads_available : (match (ownerRun true).ending with | .final .normal => true | _ => false) = true ∧
    (ownerRun true).residual.store.own.loans = ownerCaller.loans ∧
    ownerAccess (ownerRun true).residual.store.own ⟨0⟩ .mutable = true := by decide

/-- Planted: omit direct-owner lending entirely. Both populated executions refuse,
so neither positive call Instance can be satisfied by unconditional refusal. -/
abbrev omitOwner : Exec.Operands := { ownerOps with eval := fun locals s e => match e with
  | .ownerArgument _ _ _ => .stuck
  | _ => ownerOps.eval locals s e }

/-- Planted: refusing all owner arguments fails the required accepting call path. -/
theorem planted_refusing_all_owner_arguments_fails_required : Exec.evalArgs omitOwner [(x, 7)] ⟨ownerCaller, []⟩
    [.ownerArgument ⟨0⟩ .mutable y] = .stuck := by decide

/-- The source can present this occurrence under the second spelling; identity remains 0. -/
def renamedX : Var := ⟨0⟩

/-- Instance: assignment after consumption uses the same numeric local, then its fresh
obligation is explicitly consumed before the shared function-end boundary. -/
theorem example_assignment_after_consumption_uses_same_numeric : (runBody (.seq (.cond (.move x) .skip .skip ⟨0⟩)
    (.seq (.assign renamedX (.lit 9) ⟨1⟩) (.ret (.move x) ⟨2⟩)))).ending =
    .final (.returned 9) := by rfl

/-- Instance: numeric identity removes a renamed local's storage; the other number stays.
This populated two-owner state distinguishes removal of one identity from removal of all. -/
theorem example_numeric_identity_removes_renamed_local_s : (do
    let s ← bind types y initial
    let s ← move s ⟨0⟩
    let s ← drop [renamedX] s
    pure ((slot s ⟨0⟩).isNone, (slot s ⟨1⟩).isSome,
      s.frames.head?.bind fun f => AList.lookup f.locals y)) = some (true, true, some ⟨1⟩) := by decide

/-- Instance: moving twice refuses even with no loan present; reinitialization restores the
obligation but does not create a second slot for the renamed occurrence. -/
theorem example_moving_twice_refuses_even_no_loan : (do
    let s ← move initial ⟨0⟩
    let refused := (move s ⟨0⟩).isNone
    let s ← bind types renamedX s
    pure (refused, s.nextPlace, s.slots.length, ownerAccess s ⟨0⟩ .shared)) =
    some (true, 1, 1, true) := by decide

/-- Instance: the final function boundary refuses a still-live linear owner. -/
theorem example_final_function_boundary_refuses_still_live : (runBody .skip).ending = .stuck := by rfl

/-- Instance: explicit consumption permits that same final boundary. -/
theorem example_explicit_consumption_permits_same_final_boundary : (runBody (.ret (.move renamedX) ⟨0⟩)).ending = .final (.returned 7) := by rfl

/-- Instance: a final block cannot silently discard a newly created linear obligation. -/
def finalBlock : Exec.Stmt ops :=
  .seq (.cond (.move x) .skip .skip ⟨0⟩) (.block ⟨1⟩ (.bind y (.lit 3) ⟨0⟩))

/-- Instance: the outer obligation is consumed. Refusal retains the inner scope and
its live local, before the independent function-end check can run. -/
theorem example_outer_obligation_is_consumed_Refusal_retains : (runBody finalBlock).residual.frames.length = 2 ∧
    Ending.IsStuck (runBody finalBlock).ending = true := by decide

/-- Planted: omit only lifecycle drop instrumentation. Function exit still refuses,
but the scope has disappeared, violating the residual observation above. -/
abbrev omitDrop : Exec.Operands := { ops with life := fun e s => match e with
  | .drop _ => some s
  | _ => Own.life types e s }

/-- Planted: skipped scope admission reaches function exit with only one frame. -/
theorem planted_skipped_scope_admission_reaches_function_exit : (Exec.run (O := omitDrop) ⟨[]⟩ (fun _ p => nomatch p) 100
    ⟨.exec (.seq (.cond (.move x) .skip .skip ⟨0⟩)
      (.block ⟨1⟩ (.bind y (.lit 3) ⟨0⟩))), [(x, 7)], initial, [.functionEnd]⟩).residual.frames.length = 1 := by decide

/-- Copy removes the linear-obligation obstacle, isolating the independent live-loan guard. -/
def copyShared : State :=
  { shared with slots := shared.slots.map fun x => { x with desc := { x.desc with copy := true } } }

/-- Instance: real scope-exit instrumentation refuses a live loan even for a Copy root. -/
theorem example_real_scope_exit_instrumentation_refuses_live : (Exec.run (O := ops) ⟨[]⟩ (fun _ p => nomatch p) 20
    ⟨.done, [(renamedX, 7)], copyShared, [.scope ⟨1⟩ [] [], .functionEnd]⟩).ending = .stuck := by rfl

/-- Instance: ending just that loan permits scope teardown and then final function teardown. -/
theorem example_ending_just_loan_permits_scope_teardown : (Exec.run (O := ops) ⟨[]⟩ (fun _ p => nomatch p) 20
    ⟨.done, [(renamedX, 7)], (endLoan copyShared ⟨0⟩).getD copyShared,
      [.scope ⟨1⟩ [] [], .functionEnd]⟩).ending = .final .normal := by rfl

/-- Instance: downward-only lending rejects an issued but noncurrent activation, with the
same live parent that successfully lends into the current frame. -/
theorem example_downward_only_lending_rejects_issued_but : (do
    let s ← enter types [] exclusive
    pure ((reborrowTo s ⟨0⟩ [] .mutable 0).isNone,
      (reborrowTo s ⟨0⟩ [] .mutable 1).isSome)) = some (true, true) := by decide

end Examples

end ConcreteSpec.Core.Own

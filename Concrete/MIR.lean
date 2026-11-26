import ConcreteSpec.Proofs.Scalar
import ConcreteSpec.Spec.Scalar
import Concrete.Admission

namespace Concrete.MIR

/-- An addition either has a discharged result or transfers to the trap at its source site
on overflow. The destination binder identifies the operation across the passes. -/
inductive Op where
  | copy (value : Core.Atom)
  | add (lhs rhs : Core.Atom) (origin : Elab.SourceSite) (discharged : Option Int)

structure Assignment where
  dest : ConcreteSpec.Var
  op : Op

structure Program where
  body : List Assignment
  result : Core.Atom

private def lowerStmt (sites : List Elab.SourceSite) (tokens : List Admission.Token)
    (stmt : Core.Stmt) : Except Refusal Assignment := do
  match stmt.e with
  | .atom a => return ⟨stmt.x, .copy a⟩
  | .add a b =>
    let some site := sites.find? (·.binding == stmt.x)
      | throw (.internal "missing source site for addition")
    let evidence := (tokens.find? (·.binding == stmt.x)).map (·.value)
    return ⟨stmt.x, .add a b site evidence⟩

private def lowerWith (core : Elab.Output) (tokens : List Admission.Token) : Except Refusal Program := do
  let body ← core.program.body.mapM (lowerStmt core.sites tokens)
  return ⟨body, core.program.ret⟩

def lower (out : Admission.Output) : Except Refusal Program :=
  lowerWith out.core out.tokens

def lowerUnchecked (core : Elab.Output) : Except Refusal Program :=
  lowerWith core []


/-- Read an atom in the current store, using the newest binding. -/
def value (env : List (ConcreteSpec.Var × Int)) :
    Core.Atom → Except ConcreteSpec.Outcome Int
  | .lit n => .ok n
  | .var x => match env.lookup x with
    | some n => .ok n
    | none => .error (.stuck .unbound)

/-- Evaluate the right-hand side of one assignment. -/
def evalOp (env : List (ConcreteSpec.Var × Int)) :
    Op → Except ConcreteSpec.Outcome Int
  | .copy a => value env a
  | .add lhs rhs _ discharged => do
    let a ← value env lhs
    let b ← value env rhs
    if discharged.isSome then
      return ConcreteSpec.Core.wrap .u8 (a + b)
    else
      match ConcreteSpec.checkedAdd .u8 a b with
      | .ok (.scalar (.int .u8 n)) => return n
      | .trap k => throw (.trap k)
      | _ => throw (.stuck .typeError)

def runFrom (env : List (ConcreteSpec.Var × Int)) (body : List Assignment)
    (ret : Core.Atom) : ConcreteSpec.Outcome :=
  match (do
    let env ← body.foldlM (fun env stmt => do
      let n ← evalOp env stmt.op
      return (stmt.dest, n) :: env) env
    value env ret : Except ConcreteSpec.Outcome Int) with
  | .ok n => .exit n.toNat
  | .error e => e

def run (p : Program) : ConcreteSpec.Outcome := runFrom [] p.body p.result

open ConcreteSpec (Env Outcome R Val IntTy)
open ConcreteSpec.Core (AList)

private def coreEnv (env : List (ConcreteSpec.Var × Int)) : Env :=
  env.map fun (x, n) => (x, .val (.scalar (.int .u8 n)))

private theorem lookup_coreEnv (env : List (ConcreteSpec.Var × Int)) (x : ConcreteSpec.Var) :
    AList.lookup (coreEnv env) x = (env.lookup x).map (fun n => .val (.scalar (.int .u8 n))) := by
  induction env with
  | nil => rfl
  | cons entry env ih =>
    obtain ⟨y, n⟩ := entry
    change AList.lookup ((y, .val (.scalar (.int .u8 n))) :: coreEnv env) x = _
    by_cases h : x = y
    · subst y
      simp [AList.lookup, List.lookup]
    · have hy : y ≠ x := Ne.symm h
      have hb : (x == y) = false := decide_eq_false h
      simp [AList.lookup, List.lookup, hb, hy, ih]

private theorem evalAtom_coreEnv (env : List (ConcreteSpec.Var × Int)) (a : Core.Atom) :
    ConcreteSpec.evalAtom [] (coreEnv env) a.toCore =
      match value env a with
      | .ok n => .ok (.int .u8 n, coreEnv env)
      | .error _ => .stuck .unbound := by
  cases a with
  | lit => rfl
  | var x =>
    simp only [ConcreteSpec.Slice0.Atom.toCore, ConcreteSpec.evalAtom, ConcreteSpec.readVar,
      lookup_coreEnv, value]
    cases env.lookup x <;> rfl


/-- Decode an expression step into the MIR value, retaining Core's failure outcome. -/
private def stepValue : R (Val × Env) → Except Outcome Int
  | .ok (.scalar (.int .u8 n), _) => .ok n
  | .ok _ => .error (.stuck .typeError)
  | .trap k => .error (.trap k)
  | .stuck w => .error (.stuck w)

private theorem evalExpr_atom (env : List (ConcreteSpec.Var × Int)) (a : Core.Atom) :
    ConcreteSpec.evalExpr [] (coreEnv env) (.atom a.toCore) =
      match value env a with
      | .ok n => .ok (.scalar (.int .u8 n), coreEnv env)
      | .error _ => .stuck .unbound := by
  cases a with
  | lit => rfl
  | var x =>
    simp only [ConcreteSpec.Slice0.Atom.toCore, ConcreteSpec.evalExpr, ConcreteSpec.readVar,
      lookup_coreEnv, value]
    cases env.lookup x <;> rfl

private theorem evalExpr_add (env : List (ConcreteSpec.Var × Int)) (lhs rhs : Core.Atom) :
    ConcreteSpec.evalExpr [] (coreEnv env) (.add lhs.toCore rhs.toCore) =
      match value env lhs, value env rhs with
      | .ok a, .ok b =>
        if IntTy.u8.inRange (a + b) then .ok (.scalar (.int .u8 (a + b)), coreEnv env)
        else .trap .arithmeticOverflow
      | _, _ => .stuck .unbound := by
  have hl := evalAtom_coreEnv env lhs
  have hr := evalAtom_coreEnv env rhs
  rw [ConcreteSpec.evalExpr]
  rw [hl]
  cases value env lhs with
  | error => rfl
  | ok a =>
    simp only
    rw [hr]
    cases value env rhs with
    | error => rfl
    | ok b =>
      by_cases h : IntTy.u8.inRange (a + b) = true <;>
        simp [ConcreteSpec.checkedAdd, h]

private theorem value_error {env a e} (h : value env a = .error e) :
    e = .stuck .unbound := by
  cases a with
  | lit => contradiction
  | var x =>
    simp only [value] at h
    split at h <;> cases h
    rfl

private theorem lowerStmt_eval {sites tokens stmt assignment}
    (h : lowerStmt sites tokens stmt = .ok assignment) (env : List (ConcreteSpec.Var × Int)) :
    assignment.dest = stmt.x ∧
      (evalOp env assignment.op = stepValue (ConcreteSpec.evalExpr [] (coreEnv env) stmt.e.toCore) ∨
        tokens ≠ [] ∧ ConcreteSpec.evalExpr [] (coreEnv env) stmt.e.toCore = .trap .arithmeticOverflow) := by
  obtain ⟨dest, expr⟩ := stmt
  cases expr with
  | atom a =>
    change Except.ok ⟨dest, .copy a⟩ = .ok assignment at h
    cases h
    refine ⟨rfl, Or.inl ?_⟩
    change value env a = stepValue (ConcreteSpec.evalExpr [] (coreEnv env) (.atom a.toCore))
    rw [evalExpr_atom]
    cases a with
    | lit => rfl
    | var x => simp only [value]; cases env.lookup x <;> rfl
  | add lhs rhs =>
    cases hs : sites.find? (·.binding == dest) with
    | none => simp [lowerStmt, hs] at h
    | some site =>
      simp [lowerStmt, hs, pure, Except.pure] at h
      subst assignment
      refine ⟨rfl, ?_⟩
      change evalOp env (.add lhs rhs site _) = stepValue
        (ConcreteSpec.evalExpr [] (coreEnv env) (.add lhs.toCore rhs.toCore)) ∨
        tokens ≠ [] ∧ ConcreteSpec.evalExpr [] (coreEnv env) (.add lhs.toCore rhs.toCore) = _
      rw [evalExpr_add]
      unfold evalOp
      cases hl : value env lhs with
      | error e =>
        left
        have he : e = .stuck .unbound := value_error hl
        subst e
        simp [hl, stepValue, bind, Except.bind]
      | ok a =>
        cases hr : value env rhs with
        | error e =>
          left
          have he : e = .stuck .unbound := value_error hr
          subst e
          simp [hl, hr, stepValue, bind, Except.bind]
        | ok b =>
          by_cases hf : IntTy.u8.inRange (a + b) = true
          · have hi : 0 ≤ a + b ∧ a + b ≤ 255 := of_decide_eq_true hf
            have hw := ConcreteSpec.Core.wrap_of_inRange .u8 _ hi
            left
            cases tokens.find? (·.binding == dest) <;>
              simp [stepValue, ConcreteSpec.checkedAdd, hf, hl, hr, hw, bind, Except.bind, pure, Except.pure]
          · cases ht : tokens.find? (·.binding == dest) with
            | none =>
              left
              simp [stepValue, ConcreteSpec.checkedAdd, hf, hl, hr, bind, Except.bind]
              rfl
            | some token =>
              exact Or.inr ⟨by intro hn; simp [hn] at ht, by simp [hf]⟩

private theorem evalExpr_shape (env : List (ConcreteSpec.Var × Int)) (expr : Core.Expr) :
    (∃ n, ConcreteSpec.evalExpr [] (coreEnv env) expr.toCore =
      .ok (.scalar (.int .u8 n), coreEnv env)) ∨
    (∃ w, ConcreteSpec.evalExpr [] (coreEnv env) expr.toCore = .stuck w) ∨
    (∃ k, ConcreteSpec.evalExpr [] (coreEnv env) expr.toCore = .trap k) := by
  cases expr with
  | atom a =>
    simp only [ConcreteSpec.Slice0.Expr.toCore, evalExpr_atom]
    cases value env a with
    | ok n => exact Or.inl ⟨n, rfl⟩
    | error => exact Or.inr (Or.inl ⟨_, rfl⟩)
  | add lhs rhs =>
    simp only [ConcreteSpec.Slice0.Expr.toCore, evalExpr_add]
    cases value env lhs with
    | error => exact Or.inr (Or.inl ⟨_, rfl⟩)
    | ok a =>
      cases value env rhs with
      | error => exact Or.inr (Or.inl ⟨_, rfl⟩)
      | ok b =>
        by_cases hf : IntTy.u8.inRange (a + b) = true
        · exact Or.inl ⟨a + b, by simp [hf]⟩
        · exact Or.inr (Or.inr ⟨.arithmeticOverflow, by simp [hf]⟩)

private theorem lowerBody_correct {sites tokens} {ss : List Core.Stmt} {body : List Assignment}
    (h : ss.mapM (lowerStmt sites tokens) = .ok body)
    (env : List (ConcreteSpec.Var × Int)) (ret : Core.Atom)
    (safe : tokens = [] ∨
      ConcreteSpec.runBody [] (ss.length + 1) (coreEnv env) (ss.map ConcreteSpec.Slice0.Stmt.toCore)
        (.ret ret.toCore) ≠ .trap .arithmeticOverflow) :
    runFrom env body ret =
      ConcreteSpec.runBody [] (ss.length + 1) (coreEnv env) (ss.map ConcreteSpec.Slice0.Stmt.toCore)
        (.ret ret.toCore) := by
  induction ss generalizing body env with
  | nil =>
    change Except.ok [] = Except.ok body at h
    cases h
    simp only [runFrom, List.foldlM_nil, pure, Except.pure, bind, Except.bind,
      List.length_nil, List.map_nil, ConcreteSpec.runBody, ConcreteSpec.finish,
      ConcreteSpec.retOutcome, evalAtom_coreEnv]
    cases ret with
    | lit => rfl
    | var x => simp only [value]; cases env.lookup x <;> rfl
  | cons s ss ih =>
    obtain ⟨a, tail, ha, ht, rfl⟩ :
        ∃ a tail, lowerStmt sites tokens s = .ok a ∧
          ss.mapM (lowerStmt sites tokens) = .ok tail ∧ body = a :: tail := by
      rw [List.mapM_cons] at h
      cases ha : lowerStmt sites tokens s with
      | error => simp [ha, bind, Except.bind] at h
      | ok a =>
        cases ht : ss.mapM (lowerStmt sites tokens) with
        | error => simp [ha, ht, bind, Except.bind] at h
        | ok tail =>
          simp [ha, ht, bind, Except.bind, pure, Except.pure] at h
          exact ⟨a, tail, rfl, rfl, h.symm⟩
    obtain ⟨hd, hop⟩ := lowerStmt_eval ha env
    simp only [List.length_cons, List.map_cons, ConcreteSpec.runBody,
      ConcreteSpec.Slice0.Stmt.toCore, ConcreteSpec.execStmt] at safe ⊢
    rcases hop with he | ⟨htokens, htrap⟩
    · rcases evalExpr_shape env s.e with ⟨n, hn⟩ | ⟨w, hw⟩ | ⟨k, hk⟩
      · rw [hn] at he safe ⊢
        change evalOp env a.op = .ok n at he
        simp only [runFrom, List.foldlM_cons, bind, Except.bind, he, pure, Except.pure, hd]
        exact ih ht ((s.x, n) :: env) safe
      · rw [hw] at he ⊢
        change evalOp env a.op = .error (.stuck w) at he
        simp [runFrom, List.foldlM_cons, bind, Except.bind, he]
      · rw [hk] at he ⊢
        change evalOp env a.op = .error (.trap k) at he
        simp [runFrom, List.foldlM_cons, bind, Except.bind, he]
    · rw [htrap] at safe
      exact False.elim (safe.elim htokens (fun h => h rfl))

private theorem lowerWith_correct {source tokens mir}
    (h : lowerWith source tokens = .ok mir)
    (safe : tokens = [] ∨
      Core.run (Core.toCore source.program) (source.program.body.length + 1) ≠
        .trap .arithmeticOverflow) :
    run mir = Core.run (Core.toCore source.program) ((Core.toCore source.program).body.length + 1) := by
  cases hb : source.program.body.mapM (lowerStmt source.sites tokens) with
  | error => simp [lowerWith, hb, bind, Except.bind] at h
  | ok body =>
    simp [lowerWith, hb, bind, Except.bind, pure, Except.pure] at h
    subst mir
    rw [show (Core.toCore source.program).body.length = source.program.body.length from List.length_map _]
    exact lowerBody_correct hb [] source.program.ret safe

/-- Lowering without admission preserves the Core outcome, including overflow traps. -/
theorem lowerUnchecked_correct {source mir} (h : lowerUnchecked source = .ok mir) :
    let p := Core.toCore source.program
    run mir = Core.run p (p.body.length + 1) :=
  lowerWith_correct h (.inl rfl)

/-- The spec's no-trap guarantee validates omitting overflow checks after admission. -/
theorem lower_correct {source admitted mir} (ha : Admission.check source = .ok admitted)
    (hl : lower admitted = .ok mir) :
    let p := Core.toCore source.program
    run mir = Core.run p (p.body.length + 1) := by
  have h := lowerWith_correct hl
    (.inr (Admission.admit_never_traps ha _ .arithmeticOverflow))
  simpa only [(Admission.check_evidence ha).1] using h

end Concrete.MIR

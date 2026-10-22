import Concrete.Interp.Interp
import Concrete.Proof.ProofCore

/-!
# Model vs runtime, per extracted function (bug 080 prevention)

A proof is about the extracted `PExpr` model; the program runs Core. Nothing checked that the two
compute the same thing, and bug 080 lived in the gap: `PExpr.cast` was the identity while the runtime
wrapped, so `x as u8` was 300 in the model and 44 in the binary.

This evaluates, IN PROCESS, both sides of every extracted function whose parameters and result are
integers or booleans: `Proof.eval` on the extracted body (callees resolved from the other extracted
entries) and the interpreter on the Core function, on boundary values of each parameter's type. The
interpreter is the reference the differential fuzzer already ties to the compiled binary.

Verdicts per input:
- `agree` — same value;
- `MISMATCH` — both produce a value and they differ: the model is about a different function;
- `partial` — the model produces a value where the runtime traps (checked overflow, division by
  zero): the model's unbounded integers make a claim about it partial correctness, not a wrong value;
- `model_stuck` — the model produces nothing where the runtime does (fuel, or an unmodeled shape);
- `ERROR` — NEITHER side produces a value: the input compared nothing, so it is not agreement.

A gate requires zero `MISMATCH` rows across the corpus. It is a test, not a proof: it samples
boundaries, it cannot show agreement everywhere.
-/

namespace Concrete

/-- One sampled argument: an integer of the parameter's type, or a boolean. -/
inductive ModelDiffArg where
  | int (n : Int)
  | bool (b : Bool)
  deriving BEq, Inhabited

def ModelDiffArg.render : ModelDiffArg → String
  | .int n => toString n
  | .bool b => toString b

structure ModelDiffRow where
  fn       : String
  claimed  : Bool
  inputs   : List ModelDiffArg
  model    : String
  runtime  : String
  verdict  : String

/-- Boundary values of a scalar parameter type, in range: both booleans for `bool`. -/
def modelDiffBoundaries (ty : Ty) : List ModelDiffArg :=
  if ty == .bool then [.bool false, .bool true] else
  match IntArith.intRange ty with
  | some (lo, hi) =>
    (([lo, lo + 1, -1, 0, 1, 2, 7, 8, 127, 128, 255, 256, 300, 65535, 65536,
      4294967295, 4294967296, hi - 1, hi].filter (fun v => lo ≤ v && v ≤ hi)).eraseDups).map .int
  | none => []

/-- Input tuples: each parameter swept over its boundaries with the others held at a small in-range
    value, plus the diagonal (every parameter at its k-th boundary). A nullary function has exactly
    one input, the empty tuple. -/
def modelDiffInputs (tys : List Ty) : List (List ModelDiffArg) :=
  if tys.isEmpty then [[]] else
  let bs := tys.map modelDiffBoundaries
  let base := bs.map fun b => if b.contains (.int 1) then .int 1 else b.headD (.int 0)
  let sweeps := (List.range tys.length).flatMap fun i =>
    (bs.getD i []).map fun v => base.set i v
  let diag := (List.range ((bs.map List.length).foldl max 0)).map fun k =>
    bs.zip base |>.map fun (b, d) => b.getD k d
  (sweeps ++ diag).eraseDups

private def isScalar (ty : Ty) : Bool := ty == .bool || (IntArith.intBitWidth ty).isSome

/-- The model's function table: every extracted entry, keyed as Core calls spell it. -/
def modelDiffTable (pc : ProofCore) : Proof.FnTable :=
  let es := pc.closureView.entries
  Proof.FnTable.ofGlobals fun key =>
    match es.find? (·.fn.name == key) with
    | some e => e.extracted.map fun body =>
        ({ operationalKey := key, displayName := key, params := e.params, body } : Proof.PFnDef)
    | none => none

/-- The functions the report compares: extracted, with a scalar-to-scalar signature. -/
def modelDiffEligible (e : ProofCoreEntry) : Bool :=
  e.extracted.isSome && e.fn.params.all (isScalar ·.2) && isScalar e.fn.retTy

/-- The runtime value of one sampled input: a value, a trap, or `exhausted` — the interpreter's
    loop budget ran out (bug 106), so the input compared nothing. -/
def modelDiffRuntimeBudget : Nat := 10000

/-- A verdict from the two sides' values. The ONE comparator: every row is judged here. -/
def modelDiffVerdict (model runtime : Option String) : String :=
  match model, runtime with
  | some a, some b => if a == b then "agree" else "MISMATCH"
  | some _, none => "partial"
  | none, some _ => "model_stuck"
  | none, none => "ERROR"

/-- TEST-ONLY negative control (`CONCRETE_MODEL_DIFF_PERTURB=<qualified fn>`): that function's
    MODEL value is changed (an integer plus one, a boolean negated) before the production comparator
    sees it, so the gate can require `MISMATCH` — the comparator is shown to compare, not merely
    re-derived from an all-agreeing corpus. -/
def modelDiffPerturb : Option String → Option String
  | some "true" => some "false"
  | some "false" => some "true"
  | some v => some (match v.toInt? with | some n => toString (n + 1) | none => v ++ "'")
  | none => none

/-- One function's rows, or none when its signature is not scalar-to-scalar. -/
def modelDiffOf (pc : ProofCore) (fns : List CFnDef) (enums : List CEnumDef)
    (perturb : Option String) (e : ProofCoreEntry) : List ModelDiffRow :=
  match e.extracted with
  | none => []
  | some body =>
    if !modelDiffEligible e then [] else
    let tys := e.fn.params.map (·.2)
    let table := modelDiffTable pc
    (modelDiffInputs tys).map fun args =>
      let toPVal : ModelDiffArg → Proof.PVal := fun | .int v => .int v | .bool b => .bool b
      let env := (e.params.zip args).foldl (fun en (n, v) => en.bind n (toPVal v)) Proof.Env.empty
      let model : Option String := match Proof.eval table env 2000 body with
        | some (.int n) => some (toString n)
        | some (.bool b) => some (toString b)
        | _ => none
      let model := if perturb == some e.qualName then modelDiffPerturb model else model
      let argEs : List CExpr := (args.zip tys).map fun
        | (.int v, ty) => .intLit v ty
        | (.bool b, _) => .boolLit b
      let call : CExpr := .call (.direct e.fn.name) [] argEs e.fn.retTy
      -- The runtime side runs under a loop budget (bug 106): a sampled boundary input can make
      -- a loop run 2^63 times, which hung the report. Running out is its own outcome,
      -- `exhausted` — compared nothing, never a trap and never agreement.
      let result := Interp.evalClosed { fns, enums, loopFuel := modelDiffRuntimeBudget } call
      let exhausted := match result with
        | .error msg => msg.startsWith Interp.loopBudgetExhausted
        | _ => false
      let runtime : Option String :=
        match result with
        | .ok (.int n _) => some (toString n)
        | .ok (.bool b) => some (toString b)
        | _ => none
      { fn := e.qualName, claimed := e.spec.isSome, inputs := args
      , model := model.getD "—", runtime := if exhausted then "exhausted" else runtime.getD "trap"
      , verdict := if exhausted then "exhausted" else modelDiffVerdict model runtime }

def modelDiffRows (pc : ProofCore) (modules : List CModule) (perturb : Option String := none) :
    List ModelDiffRow :=
  let fns := Interp.collectFns modules
  let enums := Interp.collectEnums modules
  pc.entries.flatMap (modelDiffOf pc fns enums perturb)

/-- `--report model-diff`: one line per (function, input), then one `MODEL-DIFF-ELIGIBLE` line per
    compared function (so a gate can require that each has rows), then a summary. -/
def modelDiffReport (pc : ProofCore) (modules : List CModule) (perturb : Option String := none) :
    String :=
  let rows := modelDiffRows pc modules perturb
  let line := fun (r : ModelDiffRow) =>
    s!"{r.verdict}\t{r.fn}\t{if r.claimed then "claimed" else "unclaimed"}\t({", ".intercalate (r.inputs.map (·.render))})\tmodel={r.model}\truntime={r.runtime}"
  let count := fun (v : String) => (rows.filter (·.verdict == v)).length
  let eligible := (pc.entries.filter modelDiffEligible).map (·.qualName)
  let fnsChecked := (rows.map (·.fn)).eraseDups.length
  String.intercalate "\n" (rows.map line) ++
    String.join (eligible.map fun f => s!"\nMODEL-DIFF-ELIGIBLE\t{f}") ++
    s!"\nMODEL-DIFF: functions={fnsChecked} eligible={eligible.length} inputs={rows.length} agree={count "agree"} mismatch={count "MISMATCH"} partial={count "partial"} model_stuck={count "model_stuck"} error={count "ERROR"} exhausted={count "exhausted"}\n"

end Concrete

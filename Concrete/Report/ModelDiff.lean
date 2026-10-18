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
- `model_stuck` — the model produces nothing where the runtime does (fuel, or an unmodeled shape).

A gate requires zero `MISMATCH` rows across the corpus. It is a test, not a proof: it samples
boundaries, it cannot show agreement everywhere.
-/

namespace Concrete

structure ModelDiffRow where
  fn       : String
  claimed  : Bool
  inputs   : List Int
  model    : String
  runtime  : String
  verdict  : String

/-- Boundary values of an integer type, in range. -/
def modelDiffBoundaries (ty : Ty) : List Int :=
  match IntArith.intRange ty with
  | some (lo, hi) =>
    ([lo, lo + 1, -1, 0, 1, 2, 7, 8, 127, 128, 255, 256, 300, 65535, 65536,
      4294967295, 4294967296, hi - 1, hi].filter (fun v => lo ≤ v && v ≤ hi)).eraseDups
  | none => []

/-- Input tuples: each parameter swept over its boundaries with the others held at a small in-range
    value, plus the diagonal (every parameter at its k-th boundary). -/
def modelDiffInputs (tys : List Ty) : List (List Int) :=
  let bs := tys.map modelDiffBoundaries
  let base := bs.map fun b => if b.contains 1 then 1 else b.headD 0
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

/-- One function's rows, or none when its signature is not scalar-to-scalar. -/
def modelDiffOf (pc : ProofCore) (fns : List CFnDef) (enums : List CEnumDef)
    (e : ProofCoreEntry) : List ModelDiffRow :=
  match e.extracted with
  | none => []
  | some body =>
    let tys := e.fn.params.map (·.2)
    if !(tys.all isScalar) || !isScalar e.fn.retTy then [] else
    let table := modelDiffTable pc
    (modelDiffInputs tys).map fun args =>
      let env := (e.params.zip args).foldl (fun en (n, v) => en.bind n (.int v)) Proof.Env.empty
      let model : Option String := match Proof.eval table env 2000 body with
        | some (.int n) => some (toString n)
        | some (.bool b) => some (toString b)
        | _ => none
      let argEs : List CExpr := (args.zip tys).map fun (v, ty) => .intLit v ty
      let call : CExpr := .call (.direct e.fn.name) [] argEs e.fn.retTy
      let runtime : Option String :=
        match Interp.evalExprVal fns enums [(Interp.stdoutVar, .string "")] call with
        | .ok (_, .int n _) => some (toString n)
        | .ok (_, .bool b) => some (toString b)
        | _ => none
      let verdict := match model, runtime with
        | some a, some b => if a == b then "agree" else "MISMATCH"
        | some _, none => "partial"
        | none, some _ => "model_stuck"
        | none, none => "agree"
      { fn := e.qualName, claimed := e.spec.isSome, inputs := args
      , model := model.getD "—", runtime := runtime.getD "trap", verdict }

def modelDiffRows (pc : ProofCore) (modules : List CModule) : List ModelDiffRow :=
  let fns := Interp.collectFns modules
  let enums := Interp.collectEnums modules
  pc.entries.flatMap (modelDiffOf pc fns enums)

/-- `--report model-diff`: one line per (function, input), then a summary. -/
def modelDiffReport (pc : ProofCore) (modules : List CModule) : String :=
  let rows := modelDiffRows pc modules
  let line := fun (r : ModelDiffRow) =>
    s!"{r.verdict}\t{r.fn}\t{if r.claimed then "claimed" else "unclaimed"}\t({", ".intercalate (r.inputs.map toString)})\tmodel={r.model}\truntime={r.runtime}"
  let count := fun (v : String) => (rows.filter (·.verdict == v)).length
  let fnsChecked := (rows.map (·.fn)).eraseDups.length
  String.intercalate "\n" (rows.map line) ++
    s!"\nMODEL-DIFF: functions={fnsChecked} inputs={rows.length} agree={count "agree"} mismatch={count "MISMATCH"} partial={count "partial"} model_stuck={count "model_stuck"}\n"

end Concrete

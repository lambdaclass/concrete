import Concrete.Proof.Proof
import Concrete.ProofKit

/-!
# proof_patterns — teaching + regression corpus

Small, source-linked proof patterns a new author can copy. Each pattern's
`.con` lives under `examples/proof_patterns/<pattern>/`; its spec PExpr + proof
theorem live here in the per-example namespace `Examples.ProofPatterns.Proofs`
(NOT in `Concrete.Proof` — see the namespace guard). Staleness is caught by the
in-source `#[proof_fingerprint]`; these patterns are intentionally NOT registered
in `Concrete.Proof.specs` (they follow the `loop_invariant` fingerprint model).
-/

namespace Examples.ProofPatterns.Proofs

open Concrete.Proof

/-! ## 1. Straight-line refinement

The extracted body of `straight_line.add_three` computes exactly `x + 3`. -/

/-- Spec = the extracted body of `add_three`. -/
def addThreeExpr : PExpr :=
  .binOp .add (.var "x") (.lit (.int 3))

/-- `add_three(x)` evaluates to `x + 3` for every integer `x`. -/
theorem add_three_correct (x : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "x" (.int x)) (fuel + 1) addThreeExpr
      = some (.int (x + 3)) := by
  simp [addThreeExpr, eval, Env.bind, evalBinOp]

/-! ## (workspace fixture) straight-line multiply

`workspace.scale_by_two` — the function the `--workspace` fixture generates from. -/

/-- Spec = the extracted body of `scale_by_two`. -/
def scaleByTwoExpr : PExpr :=
  .binOp .mul (.var "x") (.lit (.int 2))

/-- `scale_by_two(x)` evaluates to `x * 2` for every integer `x`. -/
theorem scale_by_two_correct (x : Int) (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "x" (.int x)) (fuel + 1) scaleByTwoExpr
      = some (.int (x * 2)) := by
  simp [scaleByTwoExpr, eval, Env.bind, evalBinOp]

/-! ## 2. Array read/write frame

`arr.put` writes one cell of a fixed `[i32; 4]`. The proof shows the written
cell (index 1) takes the new value while every other cell is framed (unchanged):
`[10, 11, 12, 13]` with `v = 99` becomes `[10, 99, 12, 13]`. -/

/-- Spec = the extracted body of `put`: write index 1, return the array. -/
def putExpr : PExpr :=
  .letIn "a" (.arraySet (.var "a") (.lit (.int 1)) (.var "v")) (.var "a")

/-- Writing index 1 changes that cell and frames the rest (point proof over a
    concrete 4-element array): `[10,11,12,13][1 := 99] = [10,99,12,13]`. -/
theorem put_writes_index_1_frames_rest (fuel : Nat) :
    eval FnTable.empty
      ((Env.empty.bind "a" (.array_ [.int 10, .int 11, .int 12, .int 13])).bind "v" (.int 99))
      (fuel + 3) putExpr
      = some (.array_ [.int 10, .int 99, .int 12, .int 13]) := by
  simp [putExpr, eval, Env.bind]

/-! ## 3. Loop copy

`loopcopy.copy2` copies a fixed `[i32; 2]` element-by-element into a fresh
buffer. The proof runs the whole copy loop on a concrete input and shows the
copy is faithful (point coverage): `copy2([5, 7]) = [5, 7]`. -/

/-- Spec = the extracted body of `copy2` (zero-init `dst`, counted copy loop). -/
def copy2Expr : PExpr :=
  .letIn "dst"
    (.arrayLit [(.lit (.int 0)), (.lit (.int 0))])
    (.letIn "i"
      (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.lit (.int 2)))
        [ ("dst", .arraySet (.var "dst") (.var "i") (.arrayIndex (.var "src") (.var "i")))
        , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
        (.var "dst")))

/-- The copy loop faithfully copies a concrete two-element array. -/
theorem copy2_copies_faithfully (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "src" (.array_ [.int 5, .int 7])) (fuel + 10) copy2Expr
      = some (.array_ [.int 5, .int 7]) := by
  simp [copy2Expr, eval, evalBinOp, Env.bind,
        eval.evalAssigns, eval.evalElems, eval.lookupIndex]

/-! ## 4. Fold / reduction

`fold.sum4` sums a fixed `[i32; 4]` with a counted loop. The proof runs the fold
on a concrete input and shows the total is correct (point coverage):
`sum4([1, 2, 3, 4]) = 10`. -/

/-- Spec = the extracted body of `sum4` (acc += a[i] over a counted loop). -/
def sum4Expr : PExpr :=
  .letIn "acc"
    (.lit (.int 0))
    (.letIn "i"
      (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.lit (.int 4)))
        [ ("acc", .binOp .add (.var "acc") (.arrayIndex (.var "a") (.var "i")))
        , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
        (.var "acc")))

/-- The fold computes the correct total on a concrete four-element array. -/
theorem sum4_totals_concrete (fuel : Nat) :
    eval FnTable.empty (Env.empty.bind "a" (.array_ [.int 1, .int 2, .int 3, .int 4])) (fuel + 14) sum4Expr
      = some (.int 10) := by
  simp [sum4Expr, eval, evalBinOp, Env.bind,
        eval.evalAssigns, eval.lookupIndex]

/-! ## 5. Call composition

`calls.combine` calls two proved helpers (`inc`, `dbl`). The proof evaluates
through the `FnTable` / call scaffolding and establishes the composed result for
every input: `combine(x) = (x + 1) + (x * 2)`. -/

/-- `inc(x) = x + 1`. -/
def incExpr : PExpr := .binOp .add (.var "x") (.lit (.int 1))
/-- `dbl(x) = x * 2`. -/
def dblExpr : PExpr := .binOp .mul (.var "x") (.lit (.int 2))

-- MIGRATED (R-0004 step 5). Identity, operational key and body digest adopted from
-- `concrete examples/proof_patterns/composition/src/main.con --report lean-stubs`.
-- Both committed specs hash to the generated digests, so populating `entries`
-- cannot change what the composition theorems prove.
def incFn : PFnDef :=
  { identity := .semantic (Concrete.CallableId.ofUser "calls" "inc")
    operationalKey := "inc"
    sourceBodyDigest := some { value := "547e67b5f2b072131034d8cec278c032" }
    displayName := "inc", params := ["x"], body := incExpr }
def dblFn : PFnDef :=
  { identity := .semantic (Concrete.CallableId.ofUser "calls" "dbl")
    operationalKey := "dbl"
    sourceBodyDigest := some { value := "b78225e71dcabeba3282cf29cdc93ef5" }
    displayName := "dbl", params := ["x"], body := dblExpr }

/-- Function table the composition resolves its calls against. -/
def combineFnsGlobals : String → Option PFnDef
  | "inc" => some incFn
  | "dbl" => some dblFn
  | _     => none

/-- MIGRATED: entries populated, canonical order (dbl before inc, by rendered
    CallableId). `globals` kept so the existing `simp [combineFnsGlobals]` proofs
    still rewrite. -/
def combineFns : FnTable :=
  -- ATTESTED (R-0004 package 2, step 8). An OUT-OF-BUILD table, and it selects from the SAME
  -- producer every in-build site uses: `proofs/` already imports `Concrete.Proof.Proof`, so
  -- `Concrete.Proof.GeneratedAttestations` is in scope and the dependency direction is valid. A
  -- second generated file under `proofs/` would be a second producer of implementation identity,
  -- which is the defect R-0004 exists to remove — the table being defined outside the compiler
  -- changes where it lives, not where its identities come from.
  --
  -- FIVE attestations of TWO models across three packages. `composition_trusted_helper` binds only
  -- `inc`, because there `dbl` is a TRUSTED helper: its edge is witnessed by the declared trust
  -- boundary rather than by table membership, so the manifest requests no reference for it.
  --
  -- The manifest also offers three `calls.combine` references this table CANNOT bind, and that is
  -- structural rather than a gap: `combineFns` models the callees `combine`'s proof unfolds, and the
  -- subject of a proof need not appear in its own table. The reconciliation separates "cannot bind"
  -- from "chose not to bind" by asking the site which declarations it models, so this needs no
  -- exclusion entry — and an exclusion here would dress an impossibility up as a judgement.
  FnTable.withAttestations
    { entries := #[dblFn, incFn], globals := combineFnsGlobals }
    [ AttestedPFnDef.of dblFn Concrete.Proof.GeneratedAttestations.combineFns_cb56f94b_dbl
    , AttestedPFnDef.of incFn Concrete.Proof.GeneratedAttestations.combineFns_cb56f94b_inc
    , AttestedPFnDef.of incFn Concrete.Proof.GeneratedAttestations.combineFns_9b3a95ab_inc
    , AttestedPFnDef.of dblFn Concrete.Proof.GeneratedAttestations.combineFns_2bf158ef_dbl
    , AttestedPFnDef.of incFn Concrete.Proof.GeneratedAttestations.combineFns_2bf158ef_inc ]

-- Keeps `simp only [eval, combineFns_globals, combineFnsGlobals]` working WITHOUT delta-unfolding
-- the bare `combineFns`. The old `def combineFns : FnTable | "x" => …` produced equation lemmas
-- that rewrote only APPLIED occurrences; a plain def unfolds everywhere, which
-- broke `exact`s whose statements mention the table by name.
@[simp] theorem combineFns_globals : combineFns.globals = combineFnsGlobals := rfl

/-- Spec = the extracted body of `combine`: `inc(x) + dbl(x)`. -/
def combineExpr : PExpr :=
  .binOp .add (.call "inc" [(.var "x")]) (.call "dbl" [(.var "x")])

/-- `inc(x) = x + 1`, for every input.

    R-0004 slice 3: the helpers carry their OWN proof links now. The composition
    example claimed to compose "two proved helpers" while neither had a link, so
    `combine`'s claim rested on bodies nothing pinned — editing `inc` would have
    left `combine` reporting proved. This is the MODULAR proof style: the caller
    depends on current proofs for its callees. The alternative (a closed-subject
    proof, where the caller's receipt binds the transitive bodies) is not
    expressible until the receipt carries a transitive dependency root. -/
theorem inc_correct (x : Int) (fuel : Nat) :
    eval combineFns (Env.empty.bind "x" (.int x)) (fuel + 1) incExpr
      = some (.int (x + 1)) := by
  simp [incExpr, eval, Env.bind, evalBinOp]

/-- `dbl(x) = x * 2`, for every input. -/
theorem dbl_correct (x : Int) (fuel : Nat) :
    eval combineFns (Env.empty.bind "x" (.int x)) (fuel + 1) dblExpr
      = some (.int (x * 2)) := by
  simp [dblExpr, eval, Env.bind, evalBinOp]

/-- `combine(x)` composes its two proved helpers: `(x + 1) + (x * 2)`. -/
theorem combine_correct (x : Int) (fuel : Nat) :
    eval combineFns (Env.empty.bind "x" (.int x)) (fuel + 5) combineExpr
      = some (.int ((x + 1) + (x * 2))) := by
  simp [combineExpr, eval, eval.evalArgs, combineFns_globals, combineFnsGlobals, incFn, incExpr,
        dblFn, dblExpr, Env.bind, evalBinOp, bindArgs]

/-! ## 6. Ghost-assisted proof

`ghost.with_ghost` uses a `ghost let` to name the loop bound for its invariant.
A `ghost let` is erased before Core, so it never reaches the extracted body or
its `#[proof_fingerprint]`: the extracted body below contains no `n`, and the
sibling `ghost.plain` (same runtime body, literal invariant, no ghost) shares the
SAME spec, proof, and fingerprint — demonstrating ghost/contract metadata does
not affect the fingerprint. -/

/-- Spec = the extracted body of `with_ghost` / `plain` (the ghost `n` is erased;
    only the runtime loop summing `0..4` remains). -/
def ghostSumExpr : PExpr :=
  .letIn "acc"
    (.lit (.int 0))
    (.letIn "i"
      (.lit (.int 0))
      (.while_
        (.binOp .lt (.var "i") (.lit (.int 4)))
        [ ("acc", .binOp .add (.var "acc") (.var "i"))
        , ("i", .binOp .add (.var "i") (.lit (.int 1))) ]
        (.var "acc")))

/-- The ghost-assisted loop sums `0 + 1 + 2 + 3 = 6` (point coverage). -/
theorem ghost_sum_correct (fuel : Nat) :
    eval FnTable.empty Env.empty (fuel + 14) ghostSumExpr = some (.int 6) := by
  simp [ghostSumExpr, eval, evalBinOp, Env.bind, eval.evalAssigns]

/-! ## Specification parameters (bug 079)

The subject binding compares a claimed function's parameters with its specification's. These
specifications have no `PFnDef`, so their inputs are declared here, by name and in order. -/
def addThreeSpec : SpecDecl := { params := ["x"], body := addThreeExpr }
def putSpec : SpecDecl := { params := ["a", "v"], body := putExpr }
def copy2Spec : SpecDecl := { params := ["src"], body := copy2Expr }
def sum4Spec : SpecDecl := { params := ["a"], body := sum4Expr }
def combineSpec : SpecDecl := { params := ["x"], body := combineExpr }
def ghostSumSpec : SpecDecl := { params := [], body := ghostSumExpr }

end Examples.ProofPatterns.Proofs

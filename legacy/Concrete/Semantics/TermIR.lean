/-
# Typed term IR for prover-neutral obligations (R-0455, slice 1)

The current lowering fuses three jobs into string templates plus an exit code: how to SAY
the obligation, how to TRY to prove it, and how to BELIEVE the answer. Every defect R-0455
names follows from that fusion — `div`/`mod` sub-terms dropped because the operator table is
infix-only, spec-function calls dropped because there are no uninterpreted symbols, and a
whole cloned driver (`rocqNiaLowering`) existing to change one word in a proof script.

This module is the first half of the fix: a typed term IR that both sides of a
transformation are expressed in, so a transformation can be *stated* and therefore *proved*.

## Why this is Register B and not a refactor

Why3, whose shape this adopts, does not prove its transformations — they are trusted. Here
each transformation owes **transformed goal implies input goal**, as a theorem over this IR.
That is tractable in a way Register A is not: a syntactic claim relating two representations
of the same term over a small fragment, rather than a semantic claim about program execution.

## What this module deliberately does NOT attempt

Print fidelity. Proving "this printer emits syntax denoting the same proposition" needs a
formal semantics of the TARGET syntax, and there is none for SMT-LIB or Coq's parser that is
ours. Formalising them relocates the trust rather than removing it. The honest split, from
`docs/verification/PROVER_NEUTRAL_OBLIGATIONS.md`:

  obligation → transformed goal   both sides ours   PROVABLE — this module's scope
  transformed goal → target text  target not ours   validated, plausibly forever

## One design decision worth stating

`evalTerm` below is **structurally recursive, not `partial`**. That is not stylistic. The
reference evaluator this IR will eventually replace (`ReportObligations.evalIntEnv`) is a
`partial def`, so the kernel cannot reduce it and no theorem or `rfl` example can say
anything about its behaviour — the evaluator every lowering-agreement check measures against
is itself outside the reach of proof. Recursing on an explicit fuel-free structural argument
is what buys the theorems at the bottom of this file.
-/
import Concrete.Semantics.IntArith

namespace Concrete
namespace TermIR

/-- Sorts. Bitvectors carry their width because the bv tier is width-indexed and a
    transformation that changes width is a different transformation. -/
inductive Srt where
  | int
  | bool
  | bv (w : Nat)
  deriving DecidableEq, Repr, Inhabited

/-- Operators carry ARITY and FIXITY, which is the fix for a stated defect: the existing
    operator table is infix-only, so a prefix form like Rocq's `Z.quot` has nowhere to go and
    `div`/`mod` sub-terms are dropped from the prover lowering. `CoreExtract` already renders
    those same operators as prefix in the extraction path, so this is a limit of the table,
    not of the targets. -/
inductive Fixity where
  | infixOp
  | prefixOp
  deriving DecidableEq, Repr, Inhabited

inductive Op where
  | add | sub | mul
  /-- Truncating division and remainder — `IntArith.tdiv`/`tmod`, matching the runtime.
      Present here precisely because they are what the string layer drops. -/
  | tdiv | tmod
  | le | lt | eq
  | and_ | or_ | not_
  deriving DecidableEq, Repr, Inhabited

/-- Arity, so a malformed application is a representable-but-rejected state rather than a
    silent misinterpretation. -/
def Op.arity : Op → Nat
  | .not_ => 1
  | _     => 2

/-- Fixity of the operator's natural rendering. Division and remainder are PREFIX, which is
    the whole reason they can be carried here and not in an infix-only table. -/
def Op.fixity : Op → Fixity
  | .tdiv | .tmod => .prefixOp
  | .not_         => .prefixOp
  | _             => .infixOp

/-- The result sort of an operator. -/
def Op.resultSrt : Op → Srt
  | .add | .sub | .mul | .tdiv | .tmod => .int
  | .le | .lt | .eq | .and_ | .or_ | .not_ => .bool

/-- Terms. `sym` is an UNINTERPRETED symbol — the other stated defect: spec-function calls
    are dropped today because the string layer has no way to carry a symbol it cannot
    interpret. Here it is carried, and a driver that cannot express it must transform rather
    than silently drop it. -/
inductive Term where
  | lit  (v : Int)
  | blit (b : Bool)
  | var  (n : String)
  /-- Uninterpreted symbol application: a spec function, opaque to every transformation. -/
  | sym  (n : String) (args : List Term)
  | bin  (op : Op) (l r : Term)
  | un   (op : Op) (a : Term)
  /-- A width-changing cast, carrying the TARGET's width and signedness.

      Modelled rather than dropped, and modelled as a WRAP rather than as identity. The
      reference (`Interp.evalCast`) is `IntArith.wrapToType`: `as` keeps the low bits, a
      silent two's-complement truncation. Treating a cast as transparent would make the IR
      denote a different value than the program computes — the silent misinterpretation this
      IR exists to remove — so it is carried with the parameters that decide the wrap.

      Measured motivation: before this node existed, every obligation carrying a division
      subterm ALSO carried a cast (`arr[(a / b) as Int]`), so it was dropped by the IR too and
      the IR recovered nothing. -/
  | cast (w : Nat) (signed : Bool) (a : Term)
  deriving Repr, Inhabited

/-- A goal: hypotheses and a conclusion, both over the same term language. -/
structure Goal where
  hyps : List Term
  concl : Term
  deriving Repr, Inhabited

/-! ## Evaluation

Two mutually-defined evaluators over an environment. Structural recursion, so the kernel can
reduce them and the soundness theorems below are provable.

`Option` rather than a default: a term outside the fragment, or a variable with no binding,
yields `none` and every operator propagates it. Guessing a value would let a transformation
look sound on inputs where the reference has no opinion — the same fail-open shape this
codebase keeps finding.
-/

/-- Uninterpreted symbols are given meaning by an oracle, not by this evaluator. A
    transformation must be sound for EVERY oracle, which is exactly what "uninterpreted"
    means and what makes the quantification in the theorems below load-bearing. -/
abbrev SymEnv := String → List Int → Option Int

/-- Boolean variable bindings.

    `evalBool` used to return `none` for `.var _`, so a boolean VARIABLE was not evaluable at
    all. That is fine while every obligation is arithmetic — a bool-sorted variable never
    appears — and fatal the moment a non-arithmetic obligation exists, because the
    lowering-agreement check measures every rendering against this evaluator. Without bool
    bindings, a boolean goal could be lowered to Rocq/Isabelle/Lean and NOTHING could confirm
    the three lowerings mean the same proposition. -/
abbrev BoolEnv := List (String × Bool)

mutual
  /-- Integer-sorted evaluation. -/
  def evalInt (env : List (String × Int)) (se : SymEnv) : Term → Option Int
    | .lit v => some v
    | .blit _ => none
    | .var n => env.lookup n
    | .sym n args => (evalIntArgs env se args).bind (se n)
    | .un _ _ => none
    | .cast w signed a =>
      -- EXACTLY the reference's wrap, not a re-derivation: `BitVec.ofInt` at the target
      -- width, read back signed or unsigned. `IntArith.wrapToType` is the same expression
      -- keyed by `Ty`; this one is keyed by the width the IR carries.
      (evalInt env se a).map (fun n =>
        let bv := BitVec.ofInt w n
        if signed then bv.toInt else Int.ofNat bv.toNat)
    | .bin op l r => do
      let a ← evalInt env se l
      let b ← evalInt env se r
      match op with
      | .add => some (a + b)
      | .sub => some (a - b)
      | .mul => some (a * b)
      -- Same convention as the runtime and the reference evaluator: TRUNCATING. `.tdiv`
      -- here is `IntArith.tdiv`, not Lean's `/`, and the two disagree at negative operands.
      | .tdiv => if b == 0 then none else some (IntArith.tdiv a b)
      | .tmod => if b == 0 then none else some (IntArith.tmod a b)
      | _ => none

  /-- Argument lists, defined here so the mutual block stays structural. -/
  def evalIntArgs (env : List (String × Int)) (se : SymEnv) : List Term → Option (List Int)
    | [] => some []
    | t :: ts => do
      let v ← evalInt env se t
      let vs ← evalIntArgs env se ts
      some (v :: vs)
end

/-- Boolean-sorted evaluation. -/
def evalBool (env : List (String × Int)) (se : SymEnv) (benv : BoolEnv) :
    Term → Option Bool
  | .blit b => some b
  | .lit _ => none
  -- A bool-sorted variable, looked up in the BOOLEAN environment. `none` when unbound, which
  -- keeps an unbound variable un-evaluable rather than defaulting it to `false` — a default
  -- here would make an agreement check pass by assuming a value nobody supplied.
  | .var n => benv.lookup n
  | .sym _ _ => none
  | .un .not_ a => (evalBool env se benv a).map (! ·)
  | .un _ _ => none
  | .cast _ _ _ => none
  | .bin op l r =>
    match op with
    | .and_ => do let a ← evalBool env se benv l; let b ← evalBool env se benv r; some (a && b)
    | .or_  => do let a ← evalBool env se benv l; let b ← evalBool env se benv r; some (a || b)
    | .le => do let a ← evalInt env se l; let b ← evalInt env se r; some (decide (a ≤ b))
    | .lt => do let a ← evalInt env se l; let b ← evalInt env se r; some (decide (a < b))
    -- `eq` on two BOOLEAN operands is boolean equivalence, tried first; falling back to integer
    -- equality keeps every existing arithmetic obligation behaving exactly as before.
    | .eq =>
      match evalBool env se benv l, evalBool env se benv r with
      | some a, some b => some (a == b)
      | _, _ => do let a ← evalInt env se l; let b ← evalInt env se r; some (decide (a = b))
    | _ => none

/-- A goal HOLDS in an environment when every hypothesis evaluates to `true` and the
    conclusion does too. A hypothesis that does not evaluate makes the goal not-hold, which
    is the conservative direction: an unevaluable hypothesis must not license a conclusion. -/
def Goal.holds (g : Goal) (env : List (String × Int)) (se : SymEnv) (benv : BoolEnv := []) :
    Bool :=
  g.hyps.all (fun h => evalBool env se benv h == some true)
    && (evalBool env se benv g.concl == some true)

/-! ## Register B row 1 — `eliminate_div_mod`

The transformation the string layer could not express, and therefore dropped.

`a / b` inside a larger expression is replaced by a fresh variable `q`, with the defining
constraints added as hypotheses. The divisor-nonzero *obligation* was never affected — only
`div`/`mod` occurring inside a bigger term — which is why the defect was easy to miss and
why it is worth a theorem rather than a comment.

**The row's obligation is `transformed goal implies input goal`**, and the direction matters.
A transformation may STRENGTHEN (prove more than needed) without unsoundness; it may never
weaken. So the theorem shows: if the transformed goal holds in an environment extended with
the witness, the original goal holds in the original environment.

Difficulty, recorded because R-0455 says the register must: this row is a REWRITING argument
and lands. `eliminate_algebraic` — axiomatizing datatypes conservatively — is
model-theoretic, and is deliberately not attempted here. -/

mutual
  /-- Does this term contain a `tdiv`/`tmod` node? The transformation's precondition, and
      the thing whose absence makes the transformation a no-op. -/
  def hasDivMod : Term → Bool
    | .bin .tdiv _ _ => true
    | .bin .tmod _ _ => true
    | .bin _ l r => hasDivMod l || hasDivMod r
    | .un _ a => hasDivMod a
    | .sym _ args => hasDivModArgs args
    | _ => false
  def hasDivModArgs : List Term → Bool
    | [] => false
    | t :: ts => hasDivMod t || hasDivModArgs ts
end

/-- A goal read as an IMPLICATION: if every hypothesis holds, the conclusion holds. This is
    what a prover is asked, and it is the shape a soundness direction is stated over. -/
def Goal.entails (g : Goal) (env : List (String × Int)) (se : SymEnv)
    (benv : BoolEnv := []) : Prop :=
  (∀ h ∈ g.hyps, evalBool env se benv h = some true) → evalBool env se benv g.concl = some true

/-! ### Register B row 1 — `eliminate_tmod`

**The transformation:** rewrite every `a tmod b` into `a - b * (a tdiv b)`.

This is what R-0455 means by "a driver states what a target *cannot* express so the pipeline
transforms instead of silently dropping". A target with quotient but no remainder currently
loses the subterm; here it gets an equivalent term over the operators it does have. The
operator set a driver must support shrinks by one, which is the same lever that makes
`rocqNiaLowering` — a whole cloned driver existing to change one word — unnecessary.

**Why this row and not `eliminate_div_mod` (substitute a fresh variable):** substitution needs
decidable equality on `Term`, which Lean cannot derive through the nested `List Term` in `sym`
and which must therefore be hand-written. An earlier attempt here compared terms by their
`repr` STRINGS — the same category of error as validating a rendering by reading it back. And
the fresh-variable version's real content is the magnitude constraint `|r| < |b|`, which needs
`Int.natAbs` reasoning this repo has no Mathlib for. Both are recorded as open rows below
rather than approximated. Difficulty is skewed and R-0455 says the register must say so. -/

mutual
  /-- Does the term still contain a `tmod`? Used to show the transformation has EFFECT, not
      only that it preserves meaning — a meaning-preserving no-op would satisfy the soundness
      theorem while transforming nothing. -/
  def hasTmod : Term → Bool
    | .bin .tmod _ _ => true
    | .bin _ l r => hasTmod l || hasTmod r
    | .un _ a => hasTmod a
    | .cast _ _ a => hasTmod a
    | .sym _ args => hasTmodArgs args
    | _ => false
  def hasTmodArgs : List Term → Bool
    | [] => false
    | t :: ts => hasTmod t || hasTmodArgs ts
end

mutual
  /-- Rewrite `a tmod b` to `a - b * (a tdiv b)` everywhere. Structural and total: no term
      equality, no fresh names, no side conditions. -/
  def elimTmod : Term → Term
    | .bin .tmod l r =>
      let l' := elimTmod l
      let r' := elimTmod r
      .bin .sub l' (.bin .mul r' (.bin .tdiv l' r'))
    | .bin op l r => .bin op (elimTmod l) (elimTmod r)
    | .un op a => .un op (elimTmod a)
    | .cast w sg a => .cast w sg (elimTmod a)
    | .sym n args => .sym n (elimTmodArgs args)
    | t => t
  def elimTmodArgs : List Term → List Term
    | [] => []
    | t :: ts => elimTmod t :: elimTmodArgs ts
end

/-- The transformation lifted to a goal. -/
def Goal.elimTmod (g : Goal) : Goal :=
  { hyps := g.hyps.map TermIR.elimTmod, concl := TermIR.elimTmod g.concl }

mutual
  /-- **Row 1 discharged: `elimTmod` preserves meaning.**

      Stated over an ARBITRARY `SymEnv`, which is what makes uninterpreted symbols honest —
      the transformation knows nothing about spec functions, so it must hold whatever they
      mean. The `tmod` case is the content: it rests on `Int.mul_tdiv_add_tmod`, core Lean's
      division identity, and on the divisor-nonzero guard both sides share. -/
  theorem evalInt_elimTmod (env : List (String × Int)) (se : SymEnv) :
      ∀ t, evalInt env se (elimTmod t) = evalInt env se t := by
    intro t
    match t with
    | .lit _ | .blit _ | .var _ => rfl
    | .un _ _ => rfl
    | .cast w sg a =>
      -- The proof obligation the new node creates, and it is not free: the wrap is applied to
      -- the TRANSFORMED operand, so preservation has to be threaded through it.
      simp only [elimTmod, evalInt, evalInt_elimTmod env se a]
    | .sym n args =>
      simp only [elimTmod, evalInt, evalIntArgs_elimTmodArgs env se args]
    | .bin op l r =>
      have hl := evalInt_elimTmod env se l
      have hr := evalInt_elimTmod env se r
      match op with
      | .tmod =>
        simp only [elimTmod, evalInt, hl, hr]
        cases hlv : evalInt env se l with
        | none => simp
        | some av =>
          cases hrv : evalInt env se r with
          | none => simp
          | some bv =>
            by_cases hz : bv = 0
            · simp [hz]
            · -- The arithmetic fact, isolated. `omega` cannot see it directly because
              -- `bv * Int.tdiv av bv` is a product of two VARIABLES and therefore nonlinear
              -- from its point of view; generalizing the product to an atom makes the
              -- remaining step linear. Rewriting the goal with core's identity instead does
              -- not work — it replaces `av` inside `tdiv` as well.
              have key : av - bv * Int.tdiv av bv = Int.tmod av bv := by
                have hid := Int.mul_tdiv_add_tmod av bv
                generalize bv * Int.tdiv av bv = P at hid ⊢
                omega
              simp [hz, IntArith.tdiv, IntArith.tmod, key]
      | .add | .sub | .mul | .tdiv | .le | .lt | .eq | .and_ | .or_ | .not_ =>
        simp only [elimTmod, evalInt, hl, hr]

  theorem evalIntArgs_elimTmodArgs (env : List (String × Int)) (se : SymEnv) :
      ∀ ts, evalIntArgs env se (elimTmodArgs ts) = evalIntArgs env se ts := by
    intro ts
    match ts with
    | [] => rfl
    | t :: rest =>
      simp only [elimTmodArgs, evalIntArgs, evalInt_elimTmod env se t,
        evalIntArgs_elimTmodArgs env se rest]
end

/-- Boolean side of the same row. -/
theorem evalBool_elimTmod (env : List (String × Int)) (se : SymEnv) (benv : BoolEnv) :
    ∀ t, evalBool env se benv (elimTmod t) = evalBool env se benv t := by
  intro t
  match t with
  | .lit _ | .blit _ | .var _ | .sym _ _ => rfl
  | .cast _ _ _ => rfl
  | .un op a =>
    match op with
    | .not_ => simp only [elimTmod, evalBool, evalBool_elimTmod env se benv a]
    | .add | .sub | .mul | .tdiv | .tmod | .le | .lt | .eq | .and_ | .or_ => rfl
  | .bin op l r =>
    match op with
    | .and_ | .or_ =>
      simp only [elimTmod, evalBool, evalBool_elimTmod env se benv l, evalBool_elimTmod env se benv r]
    | .le | .lt =>
      simp only [elimTmod, evalBool, evalInt_elimTmod env se l, evalInt_elimTmod env se r]
    -- `.eq` now dispatches on whether both sides are BOOLEAN, so meaning preservation needs
    -- both rewrites: the boolean branch and the integer fallback.
    | .eq =>
      simp only [elimTmod, evalBool, evalBool_elimTmod env se benv l,
        evalBool_elimTmod env se benv r, evalInt_elimTmod env se l, evalInt_elimTmod env se r]
    | .add | .sub | .mul | .tdiv | .tmod | .not_ =>
      simp only [elimTmod, evalBool]

/-- **The Register B obligation, in the form the register states it:
    `transformed goal implies input goal`.** Immediate from meaning preservation, and worth
    writing separately because that is the sentence the row owes. -/
theorem elimTmod_sound {g : Goal} {env : List (String × Int)} {se : SymEnv} {benv : BoolEnv}
    (h : g.elimTmod.entails env se benv) : g.entails env se benv := by
  intro hg
  have : evalBool env se benv (elimTmod g.concl) = some true := by
    apply h
    intro f hf
    simp only [Goal.elimTmod, List.mem_map] at hf
    obtain ⟨x, hx, rfl⟩ := hf
    rw [evalBool_elimTmod]
    exact hg x hx
  rwa [evalBool_elimTmod] at this

/-! ## Behavioural locks

These are `rfl` examples rather than shell assertions, and they are only possible because
`evalInt`/`evalBool` are structural. The convention pinned here is the one whose two
spellings agree on positives and diverge on negatives, which is where a silent re-pointing
of the reference would hide. -/

private def eSym : SymEnv := fun _ _ => none

-- Truncating division, on the negative case that distinguishes the conventions.
example : evalInt [("a", -7), ("b", 2)] eSym (.bin .tdiv (.var "a") (.var "b")) = some (-3) := rfl
example : evalInt [("a", -7), ("b", 2)] eSym (.bin .tmod (.var "a") (.var "b")) = some (-1) := rfl
-- Division by zero has no value; it is never guessed.
example : evalInt [("a", -7), ("b", 0)] eSym (.bin .tdiv (.var "a") (.var "b")) = none := rfl
-- An unbound variable propagates `none` rather than defaulting.
example : evalInt [] eSym (.bin .add (.var "x") (.lit 1)) = none := rfl
-- Uninterpreted symbols get their meaning from the oracle, not from here.
example : evalInt [] eSym (.sym "f" [.lit 1]) = none := rfl
example : evalInt [] (fun n vs => if n == "f" then some (vs.length) else none)
    (.sym "f" [.lit 1, .lit 2]) = some 2 := rfl
-- ROW 1's transformation actually removes the operator, rather than being a no-op that
-- trivially preserves meaning. A soundness theorem about a transformation that changes
-- nothing is the vacuity failure this codebase keeps finding, so the effect is pinned too.
example : hasTmod (.bin .tmod (.var "a") (.var "b")) = true := rfl
example : hasTmod (elimTmod (.bin .tmod (.var "a") (.var "b"))) = false := rfl
-- ...including under a `sym`, which is where the string layer dropped subterms entirely.
example : hasTmod (elimTmod (.sym "f" [.bin .tmod (.var "a") (.var "b")])) = false := rfl
-- ...and nested inside arithmetic.
example : hasTmod (elimTmod (.bin .add (.lit 1) (.bin .tmod (.var "a") (.var "b")))) = false := rfl
-- Meaning is preserved on a concrete negative case, where truncating and flooring differ.
example : evalInt [("a", -7), ("b", 2)] eSym (elimTmod (.bin .tmod (.var "a") (.var "b")))
        = some (-1) := rfl
-- A term with no `tmod` is returned unchanged, so the pass is not gratuitously rewriting.
example : elimTmod (.bin .add (.var "a") (.lit 1)) = .bin .add (.var "a") (.lit 1) := rfl

-- Operators carry arity and fixity; div/mod are PREFIX, which is why they can be carried.
example : Op.fixity .tdiv = Fixity.prefixOp := rfl
example : Op.fixity .add = Fixity.infixOp := rfl
example : Op.arity .not_ = 1 := rfl

end TermIR
end Concrete

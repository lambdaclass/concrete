/-
Every theorem Concrete's Lean sources declare — the population check_external_checker.sh exports —
read from the elaborated environment, not grepped from source: a theorem a macro or `deriving`
produced is counted, a commented-out one is not. Compiler-generated auxiliaries (`Name.isInternal`:
`_proof_`, `match_`, `_private` …) are excluded here; they are exported anyway as dependencies of the
theorems that use them.

  lake env lean scripts/tools/proof_layer_theorems.lean
      one line per theorem: `<name>\t<axiom> <axiom> …` (axioms sorted; empty when none)
  PROOF_LAYER_MODE=modules lake env lean scripts/tools/proof_layer_theorems.lean
      the modules that declare at least one theorem of the population, one per line — checked
      against the source files that declare one, so the population cannot shrink unnoticed
  PROOF_LAYER_MODE=statements lake env lean scripts/tools/proof_layer_theorems.lean
      one line per theorem: `<name>\t<universe params>\t<statement>`, the statement rendered from
      the kernel's own `Expr` in the canonical form `export_inventory.py statements` renders from a
      lean4export stream. The two must agree byte for byte: that is the check that the exporter
      handed the external checkers the statements Lean checked, not merely statements of the same
      names.

Canonical form (shared with export_inventory.py; change both or neither) is the KERNEL's view of a
statement: names are their components joined by `.`, strings are `<utf-8 byte length>:<bytes>`, and
what the kernel ignores is erased — metadata, binder names, binder annotations and the let `nondep`
flag. Erasing them is not a convenience: lean4export shares one index among expressions equal under
`Expr` BEq, which ignores exactly those, so the export legitimately carries whichever spelling it met
first (measured on this tree: 185 statements differ in binder names, 32 in `nondep`, none otherwise).
-/
import Concrete
import Examples
import DesignExamples

open Lean

def inProofLayer (env : Environment) (n : Name) : Bool :=
  match env.getModuleIdxFor? n with
  | some i => match env.header.moduleNames[i.toNat]?.map (·.getRoot) with
    | some `Concrete | some `Examples | some `DesignExamples => true
    | _ => false
  | none => false

def nm : Name → String
  | .anonymous => ""
  | .str p s => if p.isAnonymous then s else nm p ++ "." ++ s
  | .num p n => if p.isAnonymous then toString n else nm p ++ "." ++ toString n

def lstr (s : String) : String := s!"{s.utf8ByteSize}:{s}"

def lvl : Level → String
  | .zero => "0"
  | .succ l => s!"(s {lvl l})"
  | .max a b => s!"(max {lvl a} {lvl b})"
  | .imax a b => s!"(imax {lvl a} {lvl b})"
  | .param n => s!"(p {lstr (nm n)})"
  | .mvar _ => "(mvar)"

partial def ex : Expr → String
  | .bvar i => s!"(b {i})"
  | .sort l => s!"(S {lvl l})"
  | .const n us => s!"(c {lstr (nm n)} [{" ".intercalate (us.map lvl)}])"
  | .app f a => s!"(a {ex f} {ex a})"
  | .lam _ t b _ => s!"(l {ex t} {ex b})"
  | .forallE _ t b _ => s!"(f {ex t} {ex b})"
  | .letE _ t v b _ => s!"(let {ex t} {ex v} {ex b})"
  | .lit (.natVal v) => s!"(n {v})"
  | .lit (.strVal s) => s!"(str {lstr s})"
  | .mdata _ e => ex e
  | .proj s i e => s!"(j {lstr (nm s)} {i} {ex e})"
  | .fvar _ => "(fvar)"
  | .mvar _ => "(mvar)"

#eval show CoreM Unit from do
  let env ← getEnv
  let mode := (← IO.getEnv "PROOF_LAYER_MODE").getD ""
  let mut rows : Array String := #[]
  for (n, ci) in env.constants.map₁.toList do
    if let .thmInfo t := ci then
      if n.isInternal || !inProofLayer env n then continue
      if mode == "modules" then
        if let some i := env.getModuleIdxFor? n then
          if let some m := env.header.moduleNames[i.toNat]? then rows := rows.push (nm m)
      else if mode == "statements" then
        rows := rows.push s!"{nm n}\t{" ".intercalate (t.levelParams.map (lstr ∘ nm))}\t{ex t.type}"
      else
        let axs := ((← collectAxioms n).map toString).qsort (· < ·)
        rows := rows.push s!"{n}\t{" ".intercalate axs.toList}"
  for r in (rows.qsort (· < ·)).toList.eraseDups do IO.println r

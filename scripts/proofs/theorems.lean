/-
The proof population of the specification, read from the elaborated environment (not grepped from
source): every constant the `ConcreteSpec` library declares. The proof-integrity checks
(scripts/proofs/axioms.sh, trust_ban.sh, external.sh) all read it from here, so they agree on what
"every ConcreteSpec theorem" means.

  lake env lean scripts/proofs/theorems.lean
      one line per theorem: `<name>\t<axiom> <axiom> …` (transitive axioms, sorted; empty when none).
      Compiler-generated auxiliaries (`Name.isInternal`) are left out; they are exported anyway as
      dependencies of the theorems that use them.
  PROOF_MODE=constants …
      one line per constant of ANY kind, auxiliaries included: `<name>\t<kind>\t<axioms>`. A
      definition, instance or generated `match_`/`proof_` helper can carry `sorryAx` or a native
      axiom without any theorem being about it, so the trust ban reads every constant.
  PROOF_MODE=modules …
      the modules that declare at least one theorem of the population, one per line.
  PROOF_MODE=imported …
      every `ConcreteSpec` module in the import closure of this file (the root `ConcreteSpec`).
  PROOF_MODE=statements …
      one line per theorem: `<name>\t<universe params>\t<statement>`, rendered from the kernel's own
      `Expr` in the canonical form `export_inventory.py statements` renders from a lean4export stream.

Canonical form (shared with export_inventory.py; change both or neither) is the KERNEL's view of a
statement: names are their components joined by `.`, strings are `<utf-8 byte length>:<bytes>`, and
what the kernel ignores is erased: metadata, binder names, binder annotations and the let `nondep`
flag (lean4export shares one index among expressions equal under `Expr` BEq, which ignores exactly
those, so the export legitimately carries whichever spelling it met first).

NEGATIVE CONTROLS. A declaration made in this file itself under the namespace `ConcreteSpecControl`
is part of the population too. The checks insert their deliberately bad declarations (a `sorry`, a
`native_decide`, an extra axiom) at the marker line below, in a temporary copy, and require the run to
fail; nothing else is ever declared under that namespace.
-/
import Lean
import ConcreteSpec

open Lean

def inPopulation (env : Environment) (n : Name) : Bool :=
  match env.getModuleIdxFor? n with
  | some i => (env.header.moduleNames[i.toNat]?.map (·.getRoot)) == some `ConcreteSpec
  | none => (`ConcreteSpecControl).isPrefixOf n

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

def kind : ConstantInfo → String
  | .axiomInfo _ => "axiom"
  | .defnInfo _ => "def"
  | .thmInfo _ => "theorem"
  | .opaqueInfo _ => "opaque"
  | .quotInfo _ => "quot"
  | .inductInfo _ => "inductive"
  | .ctorInfo _ => "ctor"
  | .recInfo _ => "recursor"

-- CONTROL DECLARATIONS (negative controls are inserted here, in a temporary copy only)

#eval show CoreM Unit from do
  let env ← getEnv
  let mode := (← IO.getEnv "PROOF_MODE").getD "theorems"
  unless ["theorems", "constants", "modules", "imported", "statements"].contains mode do
    throwError s!"unknown PROOF_MODE '{mode}'"
  let mut rows : Array String := #[]
  if mode == "imported" then
    for m in env.header.moduleNames do
      if m.getRoot == `ConcreteSpec then rows := rows.push (nm m)
  else
    let consts := env.constants.map₁.toList ++ env.constants.map₂.toList
    for (n, ci) in consts do
      if !inPopulation env n then continue
      let axs := fun () => do
        return " ".intercalate (((← collectAxioms n).map toString).qsort (· < ·)).toList
      if mode == "constants" then
        rows := rows.push s!"{n}\t{kind ci}\t{← axs ()}"
      else if let .thmInfo t := ci then
        if n.isInternal then continue
        if mode == "modules" then
          if let some i := env.getModuleIdxFor? n then
            if let some m := env.header.moduleNames[i.toNat]? then rows := rows.push (nm m)
        else if mode == "statements" then
          rows := rows.push s!"{nm n}\t{" ".intercalate (t.levelParams.map (lstr ∘ nm))}\t{ex t.type}"
        else
          rows := rows.push s!"{n}\t{← axs ()}"
  for r in (rows.qsort (· < ·)).toList.eraseDups do IO.println r

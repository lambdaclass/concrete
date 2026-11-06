/-
The proof population of the specification, read from the elaborated environment (not grepped from
source): every constant the `ConcreteSpec` library declares. The proof-integrity checks
(scripts/proofs/axioms.sh, trust_ban.sh, external.sh) all read it from here, so they agree on what
"every ConcreteSpec theorem" means.

  lake env lean scripts/proofs/theorems.lean
      one line per theorem: `<name>\t<axiom> <axiom> …` (transitive axioms, sorted; empty when none).
      EVERY theorem constant is listed: private ones (`_private.…`) and generated ones (`proof_N`,
      `match_N.eq_N`, `eq_N`) included, so none can be dropped from the inventory or the export.
  PROOF_MODE=constants …
      one line per constant of ANY kind: `<name>\t<kind>\t<axioms>`. A definition, instance or
      generated helper can carry `sorryAx` or a native axiom without any theorem being about it, so
      the trust ban reads every constant.
  PROOF_MODE=modules …
      the modules that declare at least one theorem of the population, one per line.
  PROOF_MODE=imported …
      every `ConcreteSpec` module in the import closure of this file (the root `ConcreteSpec`).
  PROOF_MODE=decls PROOF_NAMES=<file> PROOF_OUT=<out> …
      writes to <out> every constant whose name is listed in <file> (one per line; the declarations
      of a lean4export stream), rendered in the canonical declaration form `export_inventory.py decls` renders from
      that stream. The two must be equal: that is the check that the exporter handed the external
      checkers the declarations Lean checked (types, definition bodies, proofs, inductive types,
      constructors and recursors), not merely declarations of the same names.

CANONICAL DECLARATION FORM (shared with export_inventory.py; change both or neither). Declarations
are visited in order of their names; each field's expression is visited before the next field, and
an expression's children left to right. Every distinct expression node gets the next number the
first time it is met, and a line `<number>\t<node>` where the node names its children by number, so
the text stays linear in the size of the shared term graph. After its expressions, each declaration
is the line `D\t<name>\t<kind>\t<fields>`. Names are their components joined by `.`, strings are
`<utf-8 byte length>:<bytes>`, and what the kernel ignores is erased: metadata, binder names, binder
annotations and the let `nondep` flag (lean4export shares one index among expressions equal under
`Expr` BEq, which ignores exactly those, so the export legitimately carries whichever spelling it
met first).

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
  | none => (`ConcreteSpecControl).isPrefixOf (privateToUserName n)

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

structure Dag where
  ids : Std.HashMap String Nat := {}
  memo : Std.HashMap Expr Nat := {}
  out : Array String := #[]

-- A state reference, and every lookup and update inside one `modifyGet`: the maps stay uniquely
-- referenced and are updated in place (a lookup through `get` would share them, and each insert
-- would then copy a map of hundreds of thousands of entries).
abbrev DagM := StateRefT Dag IO

/-- The number of a node, allocating the next one (and its line) the first time it is met. -/
def node (s : String) : DagM Nat :=
  modifyGet fun st => match st.ids[s]? with
    | some i => (i, st)
    | none =>
      let i := st.ids.size
      (i, { st with ids := st.ids.insert s i, out := st.out.push s!"{i}\t{s}" })

def seen (e : Expr) : DagM (Option Nat) := modifyGet fun st => (st.memo[e]?, st)

partial def dag (e : Expr) : DagM Nat := do
  if let some i ← seen e then return i
  let i ← match e with
    | .bvar i => node s!"(b {i})"
    | .sort l => node s!"(S {lvl l})"
    | .const n us => node s!"(c {lstr (nm n)} [{" ".intercalate (us.map lvl)}])"
    | .app f a => do let f ← dag f; let a ← dag a; node s!"(a {f} {a})"
    | .lam _ t b _ => do let t ← dag t; let b ← dag b; node s!"(l {t} {b})"
    | .forallE _ t b _ => do let t ← dag t; let b ← dag b; node s!"(f {t} {b})"
    | .letE _ t v b _ => do let t ← dag t; let v ← dag v; let b ← dag b; node s!"(let {t} {v} {b})"
    | .lit (.natVal v) => node s!"(n {v})"
    | .lit (.strVal s) => node s!"(str {lstr s})"
    | .mdata _ e => dag e
    | .proj s i e => do let e ← dag e; node s!"(j {lstr (nm s)} {i} {e})"
    | .fvar _ => node "(fvar)"
    | .mvar _ => node "(mvar)"
  modify fun st => { st with memo := st.memo.insert e i }
  return i

def names (ns : List Name) : String := " ".intercalate (ns.map (lstr ∘ nm))
def flag (b : Bool) : String := if b then "1" else "0"

/-- The declaration line of a constant, after its expressions (canonical form, above). -/
def declLine (n : Name) (ci : ConstantInfo) : DagM String := do
  let lp := names ci.levelParams
  let t ← dag ci.type
  let row := fun (k : String) (rest : String) => s!"D\t{lstr (nm n)}\t{k}\t[{lp}]\tT={t}{rest}"
  match ci with
  | .axiomInfo v => return row "axiom" s!" unsafe={flag v.isUnsafe}"
  | .defnInfo v =>
    let hints := match v.hints with
      | .opaque => "opaque" | .abbrev => "abbrev" | .regular h => s!"regular {h}"
    let safety := match v.safety with
      | .safe => "safe" | .unsafe => "unsafe" | .partial => "partial"
    return row "def" s!" V={← dag v.value} hints={hints} safety={safety} all=[{names v.all}]"
  | .thmInfo v => return row "thm" s!" V={← dag v.value} all=[{names v.all}]"
  | .opaqueInfo v => return row "opaque" s!" V={← dag v.value} unsafe={flag v.isUnsafe} all=[{names v.all}]"
  | .quotInfo v =>
    let k := match v.kind with | .type => "type" | .ctor => "ctor" | .lift => "lift" | .ind => "ind"
    return row "quot" s!" kind={k}"
  | .inductInfo v => return row "inductive" s!" params={v.numParams} indices={v.numIndices} \
      all=[{names v.all}] ctors=[{names v.ctors}] rec={flag v.isRec} unsafe={flag v.isUnsafe} \
      reflexive={flag v.isReflexive} nested={v.numNested}"
  | .ctorInfo v => return row "ctor" s!" induct={lstr (nm v.induct)} cidx={v.cidx} \
      params={v.numParams} fields={v.numFields} unsafe={flag v.isUnsafe}"
  | .recInfo v =>
    let mut rules : Array String := #[]
    for r in v.rules do rules := rules.push s!"{lstr (nm r.ctor)} {r.nfields} {← dag r.rhs}"
    return row "rec" s!" all=[{names v.all}] params={v.numParams} indices={v.numIndices} \
      motives={v.numMotives} minors={v.numMinors} k={flag v.k} unsafe={flag v.isUnsafe} \
      rules=[{"; ".intercalate rules.toList}]"

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
  unless ["theorems", "constants", "modules", "imported", "decls"].contains mode do
    throwError s!"unknown PROOF_MODE '{mode}'"
  if mode == "decls" then
    let some path ← IO.getEnv "PROOF_NAMES" | throwError "PROOF_MODE=decls needs PROOF_NAMES"
    let some out ← IO.getEnv "PROOF_OUT" | throwError "PROOF_MODE=decls needs PROOF_OUT"
    let wanted : Std.HashSet String := Std.HashSet.ofList
      ((← IO.FS.lines path).toList.filter (· != ""))
    let mut found : Std.HashMap String (Name × ConstantInfo) := {}
    for (n, ci) in env.constants.map₁.toList ++ env.constants.map₂.toList do
      let s := nm n
      if wanted.contains s then
        if found.contains s then throwError s!"two constants render as the name {s}"
        found := found.insert s (n, ci)
    let order := (found.toArray.map (·.1)).qsort (· < ·)
    let run : DagM Unit := do
      for s in order do
        let some (n, ci) := found[s]? | unreachable!
        let line ← declLine n ci
        modify fun st => { st with out := st.out.push line }
    let ((), st) ← (run.run {} : IO (Unit × Dag))
    -- Written to a file: `#eval` turns what it prints into a message, and a message of 400k lines
    -- took minutes to emit.
    IO.FS.writeFile out ("\n".intercalate st.out.toList ++ "\n")
    return
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
      else if ci matches .thmInfo _ then
        if mode == "modules" then
          if let some i := env.getModuleIdxFor? n then
            if let some m := env.header.moduleNames[i.toNat]? then rows := rows.push (nm m)
        else
          rows := rows.push s!"{n}\t{← axs ()}"
  for r in (rows.qsort (· < ·)).toList.eraseDups do IO.println r

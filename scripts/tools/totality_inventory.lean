/-
Totality inventory of the compiler (scripts/tests/check_totality_ratchet.sh).

Imports the given modules and walks the ELABORATED environment, not the source text: a `partial`
produced by a macro, a `deriving` handler or a `mutual` block is a declaration here even when no
line of source says `partial def` next to its name. Every declaration whose defining module is
`Concrete`, `Concrete.*`, or an executable root (`Main`, `Browser`) is classified, and one TSV row is printed per declaration that is
not total, or that is not trusted the way a total definition is:

  partial         `partial def`: kernel-opaque; compiled from its `<name>._unsafe_rec` twin
  unsafe          `unsafe def` / `unsafe opaque` (not a partial def's `_unsafe_rec` twin)
  opaque          `opaque` with no implementation the kernel can see
  implemented_by  `@[implemented_by]`: the compiled code is not the definition the kernel sees
  extern          `@[extern]`: the compiled code is foreign
  axiom           an `axiom` declared in a compiler module
  sorry           the declaration's type or value mentions `sorryAx`

Columns: kind, declaration (private and macro-scoped names shown as their user name), module,
recursion, recursive argument types, line.
`recursion` is meaningful for `partial`: `self` (its implementation calls itself), `mutual` (it
reaches itself only through other partial definitions), `none` (it never reaches itself, so the
keyword is not what makes it partial — removing it is the first thing to try).
For every other kind it is `-`.
`recursive argument types` (partial only) names the nested or mutual inductive types among the
argument types, comma-separated, or `-`: recursion over `Expr`, whose constructors hold a
`List Expr`, cannot be structural over a plain match, which is the commonest reason a walker is
partial.

`sorry` is checked directly on each compiler declaration. That is the transitive check here:
the `Concrete` library requires no package (lakefile.toml), so whatever a compiler declaration
depends on outside these modules is Lean core, which has no `sorryAx`.

Usage: lake env lean --run scripts/tools/totality_inventory.lean <Module.Name>...
       lake env lean --run scripts/tools/totality_inventory.lean --control
-/
import Lean
open Lean

def userName (n : Name) : Name :=
  let n := (privateToUserName? n).getD n
  if n.hasMacroScopes then n.eraseMacroScopes else n

def unsafeRecSuffix : String := "_unsafe_rec"

def isUnsafeRecTwin (n : Name) : Bool :=
  match n with
  | .str _ s => s == unsafeRecSuffix
  | _ => false

def twinOf (n : Name) : Name := .str n unsafeRecSuffix

/-- The `_unsafe_rec` twins a twin's body calls. -/
def twinCalls (env : Environment) (twin : Name) : Array Name :=
  match env.find? twin with
  | some (.defnInfo d) => d.value.getUsedConstants.filter isUnsafeRecTwin
  | _ => #[]

/-- Does `start` reach itself through the twin call graph? Returns `self`, `mutual` or `none`. -/
def recursionOf (env : Environment) (start : Name) : String := Id.run do
  let first := twinCalls env start
  if first.contains start then return "self"
  let mut seen : NameSet := {}
  let mut todo := first.toList
  while !todo.isEmpty do
    match todo with
    | [] => break
    | t :: rest =>
      todo := rest
      if seen.contains t then continue
      seen := seen.insert t
      let calls := twinCalls env t
      if calls.contains start then return "mutual"
      todo := todo ++ calls.toList
  return "none"

def mentionsSorry (ci : ConstantInfo) : Bool :=
  ci.type.getUsedConstants.contains ``sorryAx ||
    (match ci.value? (allowOpaque := true) with
     | some v => v.getUsedConstants.contains ``sorryAx
     | none => false)

/-- The nested or mutual inductive types among a declaration's argument types: what a structural
recursion over the arguments would have to go through. `-` when there are none. -/
def recursiveArgTypes (env : Environment) (ty : Expr) : String := Id.run do
  let mut out : Array Name := #[]
  let mut t := ty
  while true do
    match t with
    | .forallE _ dom body _ =>
      for c in dom.getUsedConstants do
        match env.find? c with
        | some (.inductInfo iv) =>
          if (iv.isNested || iv.all.length > 1) && !out.contains c then out := out.push c
        | _ => pure ()
      t := body
    | _ => break
  return if out.isEmpty then "-" else ",".intercalate (out.qsort (·.toString < ·.toString) |>.map toString).toList

def lineOf (env : Environment) (n : Name) : Nat :=
  match declRangeExt.find? env n with
  | some r => r.range.pos.line
  | none => 0

def classify (env : Environment) (n : Name) (ci : ConstantInfo) : Array (String × String) := Id.run do
  let mut out : Array (String × String) := #[]
  if mentionsSorry ci then out := out.push ("sorry", "-")
  -- An `_unsafe_rec` twin is the compiled implementation of the declaration it is named after:
  -- of a `partial def` (an opaque, reported as `partial` below), or of a definition by
  -- well-founded recursion (a total `def` whose kernel body is `WellFounded.fix`; Lean compiles it
  -- through a twin too). Either way it is reported through that declaration. Only a twin whose
  -- declaration exists is skipped: anything else merely named `..._unsafe_rec` is classified
  -- like any other declaration.
  if isUnsafeRecTwin n && env.contains n.getPrefix then return out
  let ib := (Compiler.implementedByAttr.getParam? env n).isSome
  match ci with
  | .opaqueInfo o =>
    if env.contains (twinOf n) then
      out := out.push ("partial", s!"{recursionOf env (twinOf n)}\t{recursiveArgTypes env ci.type}")
    else if o.isUnsafe then out := out.push ("unsafe", "-")
    else if !ib && !isExtern env n then out := out.push ("opaque", "-")
  | .defnInfo d =>
    if d.safety == .unsafe then out := out.push ("unsafe", "-")
    else if d.safety == .partial then out := out.push ("partial", s!"-\t{recursiveArgTypes env ci.type}")
  | .axiomInfo _ => out := out.push ("axiom", "-")
  | _ => pure ()
  if ib then out := out.push ("implemented_by", "-")
  if isExtern env n then out := out.push ("extern", "-")
  return out

/-- One row per non-total declaration whose module `moduleOf` accepts. -/
def inventory (env : Environment) (moduleOf : Name → Option Name) : Array String := Id.run do
  let mut rows : Array String := #[]
  for (n, ci) in env.constants.toList do
    let some m := moduleOf n | continue
    for (kind, recur) in classify env n ci do
      let recur := if recur.contains '\t' then recur else recur ++ "\t-"
      rows := rows.push s!"{kind}\t{userName n}\t{m}\t{recur}\t{lineOf env n}"
  return rows.qsort (· < ·)

/-- The classifier's own negative and positive controls, elaborated from source text into a
module named `Concrete.TotalityControl`. scripts/tests/check_totality_ratchet.sh holds the rows
this must print, and that the total and inert-`partial` definitions print none. -/
def controlSource : String := "
inductive Tree where
  | leaf : Nat → Tree
  | node : List Tree → Tree
  deriving BEq

def totalSum : List Nat → Nat
  | [] => 0
  | x :: xs => x + totalSum xs

partial def inertPartial (n : Nat) : Nat := n + 1

partial def selfLoop (n : Nat) : Nat := if n == 0 then 0 else selfLoop (n / 2)

mutual
  partial def evenSteps (n : Nat) : Bool := if n == 0 then true else oddSteps (n - 1)
  partial def oddSteps (n : Nat) : Bool := if n == 0 then false else evenSteps (n - 1)
end

def leafCount : Tree → Nat
  | .leaf _ => 1
  | .node ts => go ts
where
  go (ts : List Tree) : Nat := Id.run do
    let mut acc := 0
    for t in ts do acc := acc + leafCount t
    return acc
  decreasing_by all_goals sorry

unsafe def unsafeImpl (n : Nat) : Nat := n

@[implemented_by unsafeImpl] def viaImplementedBy (n : Nat) : Nat := n

opaque opaqueNat : Nat

axiom controlAxiom : 1 = 1

theorem sorried : 2 = 2 := sorry

unsafe def bogus._unsafe_rec (n : Nat) : Nat := n
"

unsafe def runControl : IO UInt32 := do
  let some env ← Elab.runFrontend controlSource {} "TotalityControl.lean" `Concrete.TotalityControl
    | IO.eprintln "error: the control source did not elaborate"; return 2
  for r in inventory env (fun n => if (env.getModuleIdxFor? n).isNone then some `Concrete.TotalityControl else none) do
    IO.println r
  return 0

/-- The compiler's modules: the `Concrete` library and the executable roots that build on it. -/
def isCompilerModule (m : Name) : Bool :=
  m == `Concrete || (`Concrete).isPrefixOf m || m == `Main || m == `Browser

unsafe def main (args : List String) : IO UInt32 := do
  enableInitializersExecution
  initSearchPath (← findSysroot)
  if args == ["--control"] then return (← runControl)
  if args.isEmpty then
    IO.eprintln "usage: totality_inventory.lean <Module.Name>... | --control"
    return 2
  let mods := args.toArray.map fun a => ({ module := a.toName } : Import)
  let env ← importModules mods {} 0 (loadExts := true)
  let moduleOf (n : Name) : Option Name := do
    let idx ← env.getModuleIdxFor? n
    let m := env.header.moduleNames[idx.toNat]!
    if isCompilerModule m then some m else none
  for r in inventory env moduleOf do
    IO.println r
  -- Which modules the walk actually saw declarations from, on stderr: the gate requires every
  -- module it asked for that defines anything to appear here, so a module that silently failed to
  -- load cannot make the inventory look clean.
  let mut counts : NameMap Nat := {}
  for (n, _) in env.constants.toList do
    if let some m := moduleOf n then counts := counts.insert m ((counts.find? m).getD 0 + 1)
  for (m, c) in counts.toList do
    IO.eprintln s!"visited\t{m}\t{c}"
  return 0

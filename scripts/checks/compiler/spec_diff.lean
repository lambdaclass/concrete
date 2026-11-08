import Concrete
import ConcreteSpec.Spec.Slice0

namespace ConcreteSpecDiff.Random

structure Rng where
  state : UInt64
  deriving DecidableEq, Repr

def mix64 (x : UInt64) : UInt64 :=
  let z := (x + 0x9E3779B97F4A7C15)
  let z := UInt64.xor z (UInt64.shiftRight z 30) * 0xBF58476D1CE4E5B9
  let z := UInt64.xor z (UInt64.shiftRight z 27) * 0x94D049BB133111EB
  UInt64.xor z (UInt64.shiftRight z 31)

def seedHash (seed : String) : UInt64 :=
  seed.toUTF8.foldl (fun h b => UInt64.xor h (UInt64.ofNat b.toNat) * 0x100000001B3)
    0xCBF29CE484222325

def forSeed (seed : String) (index : Nat) : Rng :=
  ⟨mix64 (UInt64.xor (seedHash seed) (mix64 (UInt64.ofNat index)))⟩

def next (r : Rng) : UInt64 × Rng :=
  let state := r.state + 0x9E3779B97F4A7C15
  (mix64 state, ⟨state⟩)

def below (n : Nat) (r : Rng) : Nat × Rng :=
  let (x, r') := next r
  (x.toNat % n, r')

def pick {α : Type} (xs : List (Nat × α)) (r : Rng) : Option α × Rng :=
  let total := (xs.map Prod.fst).sum
  if total = 0 then (none, r) else
    let (ticket, r') := below total r
    let rec go (left : Nat) : List (Nat × α) → Option α
      | [] => none
      | (w, x) :: rest => if left < w then some x else go (left - w) rest
    (go ticket xs, r')

end ConcreteSpecDiff.Random

/-!
The PRNG and both generators deliberately live in this one test file: Lean 4.34.1's `lean --run`
does not import source-only modules through LEAN_SRC_PATH without separately built oleans. Nothing
in the Concrete library imports them (enforced by `scripts/checks/import_layering.sh`). Normal
commands accept no mutation option. Planted defects require the explicit `control-*` commands and
the planted-control flag.
-/

namespace ConcreteSpecDiff.SpecGen
open ConcreteSpec ConcreteSpec.Slice0
open ConcreteSpecDiff.Random

inductive Production where
  | letLiteral | letCopy | addLiteral | addVariable | returnLiteral | returnVariable
  | noDefect | outOfRange | knownOverflow | unusedBinding
  deriving DecidableEq, Repr

def Production.name : Production → String
  | .letLiteral => "let_literal"
  | .letCopy => "let_copy"
  | .addLiteral => "add_literal"
  | .addVariable => "add_variable"
  | .returnLiteral => "return_literal"
  | .returnVariable => "return_variable"
  | .noDefect => "no_defect"
  | .outOfRange => "out_of_range_literal"
  | .knownOverflow => "known_overflow"
  | .unusedBinding => "unused_binding"

/-- Every slice brief extends this one table. Statement and return weights are both consumed. -/
def weightedProductions : List (Nat × Production) :=
  [(2, .letLiteral), (4, .letCopy), (4, .addLiteral), (4, .addVariable),
   (1, .returnLiteral), (5, .returnVariable),
   (16, .noDefect), (1, .outOfRange), (1, .knownOverflow), (1, .unusedBinding)]

structure Binding where
  var : Var
  value : Nat

structure State where
  bindings : List Binding
  unread : List Var
  overflow : Bool

structure Generated where
  program : Slice0.Program
  constructs : List String
  rejection : Option String

def literal (r : Rng) : Slice0.Atom × Rng :=
  let (n, r') := below 256 r
  (.lit n, r')

def statementProductions : List (Nat × Production) :=
  weightedProductions.filter fun (_, p) =>
    p = .letLiteral || p = .letCopy || p = .addLiteral || p = .addVariable

def returnProductions : List (Nat × Production) :=
  weightedProductions.filter fun (_, p) => p = .returnLiteral || p = .returnVariable

def defectProductions : List (Nat × Production) :=
  weightedProductions.filter fun (_, p) =>
    p = .noDefect || p = .outOfRange || p = .knownOverflow || p = .unusedBinding

def markRead (x : Var) (unread : List Var) : List Var := unread.filter fun y => y != x

def previous (state : State) : Binding :=
  match state.bindings.reverse with
  | b :: _ => b
  | [] => ⟨⟨"v", 0⟩, 0⟩

/-- Generate statements in source order, carrying known values and unread bindings.
`.addVariable` prioritizes compatible unread bindings so `.letLiteral` bindings are consumed. -/
def statements (fuel : Nat) (state : State) (r : Rng) :
    (List Slice0.Stmt × State) × Rng :=
  match fuel with
  | 0 => (([], state), r)
  | fuel + 1 =>
    let (choice, r₁) := pick statementProductions r
    let production := choice.getD .addLiteral
    let prev := previous state
    let ((stmtsHere, newBindings, value, unread), r₂) :
        (List Slice0.Stmt × List Binding × Nat × List Var) × Rng :=
      match production with
      | .letLiteral =>
        let (n, r') := below (256 - prev.value) r₁
        let b : Binding := ⟨⟨"v", state.bindings.length⟩, n⟩
        (([⟨b.var, .atom (.lit n)⟩], [b], n, state.unread ++ [b.var]), r')
      | .letCopy =>
        let b : Binding := ⟨⟨"v", state.bindings.length⟩, prev.value⟩
        (([⟨b.var, .atom (.var prev.var)⟩], [b], prev.value,
          (markRead prev.var state.unread) ++ [b.var]), r₁)
      | .addVariable =>
        let compatibleAll := state.bindings.filter (fun (b : Binding) => b.value ≤ 255 - prev.value)
        let compatibleUnread := compatibleAll.filter (fun (b : Binding) =>
          b.var != prev.var && state.unread.contains b.var)
        let pool := if compatibleUnread.isEmpty then compatibleAll else compatibleUnread
        if pool.isEmpty then
          let (auxVal, r') := below (256 - prev.value) r₁
          let auxVar : Var := ⟨"v", state.bindings.length⟩
          let sumVar : Var := ⟨"v", state.bindings.length + 1⟩
          let sumVal := prev.value + auxVal
          let unread := (markRead prev.var state.unread) ++ [sumVar]
          (([⟨auxVar, .atom (.lit auxVal)⟩,
             ⟨sumVar, .add (.var prev.var) (.var auxVar)⟩],
            [⟨auxVar, auxVal⟩, ⟨sumVar, sumVal⟩], sumVal, unread), r')
        else
          let (which, r') := below pool.length r₁
          let other := pool.getD which prev
          let b : Binding := ⟨⟨"v", state.bindings.length⟩, prev.value + other.value⟩
          let unread := (markRead other.var (markRead prev.var state.unread)) ++ [b.var]
          (([⟨b.var, .add (.var prev.var) (.var other.var)⟩], [b],
            prev.value + other.value, unread), r')
      | _ =>
        let (n, r') := below (256 - prev.value) r₁
        let b : Binding := ⟨⟨"v", state.bindings.length⟩, prev.value + n⟩
        let unread := (markRead prev.var state.unread) ++ [b.var]
        (([⟨b.var, .add (.var prev.var) (.lit n)⟩], [b], prev.value + n, unread), r')
    let next : State :=
      { bindings := state.bindings ++ newBindings, unread := unread,
        overflow := state.overflow || value > 255 }
    let ((rest, final), r₃) := statements fuel next r₂
    ((stmtsHere ++ rest, final), r₃)

/-- Fold any remaining compatible unread bindings into the final binding so `.letLiteral`
statements do not leave unintended unused-binding rejections on non-defect candidates. -/
def drainUnread : List Var → State → List Slice0.Stmt × State
  | [], state => ([], state)
  | u :: rest, state =>
    let last := previous state
    if u == last.var then
      drainUnread rest state
    else
      match state.bindings.find? (fun b => b.var == u) with
      | some ub =>
        if ub.value ≤ 255 - last.value then
          let nextVar : Var := ⟨"v", state.bindings.length⟩
          let nextVal := last.value + ub.value
          let nextBinding : Binding := ⟨nextVar, nextVal⟩
          let unread' := (markRead u (markRead last.var state.unread)) ++ [nextVar]
          let state' : State :=
            { bindings := state.bindings ++ [nextBinding],
              unread := unread',
              overflow := state.overflow || nextVal > 255 }
          let (tailStmts, finalState) := drainUnread rest state'
          (⟨nextVar, .add (.var last.var) (.var u)⟩ :: tailStmts, finalState)
        else
          drainUnread rest state
      | none => drainUnread rest state

def base (returnProduction : Production) (r : Rng) : (Slice0.Program × State) × Rng :=
  match returnProduction with
  | .returnLiteral =>
    let (a, r') := literal r
    (({ body := [], ret := a }, ⟨[], [], false⟩), r')
  | _ =>
    let (count, r₁) := below 4 r
    let count := count + 1
    let (a, r₂) := literal r₁
    let firstValue := match a with | .lit n => n | .var _ => 0
    let first : Binding := ⟨⟨"v", 0⟩, firstValue⟩
    let initial : State := ⟨[first], [first.var], false⟩
    let ((rest, state), r₃) := statements count initial r₂
    let (drainStmts, drainedState) := drainUnread state.unread state
    let last := previous drainedState
    let unread := markRead last.var drainedState.unread
    (({ body := ⟨first.var, .atom a⟩ :: (rest ++ drainStmts), ret := .var last.var },
      { drainedState with unread := unread }), r₃)

def stmtConstructs (s : Slice0.Stmt) : List String :=
  match s.e with
  | .atom (.lit _) => ["let_literal"]
  | .atom (.var _) => ["let_copy"]
  | .add (.var _) (.var _) => ["add_variable"]
  | .add _ _ => ["add_literal"]

def constructs (q : Slice0.Program) : List String :=
  let body := q.body.flatMap stmtConstructs
  (match q.ret with
  | .lit _ => body ++ ["return_literal"]
  | .var _ => body ++ ["return_variable"]).eraseDups

def make (q : Slice0.Program) (rejection : Option String) : Generated :=
  ⟨q, constructs q, rejection⟩

def rejectionOf (state : State) : Option String :=
  if !state.unread.isEmpty then some "unused_binding"
  else if state.overflow then some "known_overflow"
  else none

/-- One candidate, with no hidden retry: `seed` and `index` alone determine it. -/
def generate (seed : String) (index : Nat) : Generated :=
  let r := forSeed seed index
  let (returnChoice, r₁) := pick returnProductions r
  let (defectChoice, r₂) := pick defectProductions r₁
  let (retSlot, _) := below 6 (forSeed seed 999990)
  let returnProduction :=
    if index < 6 then
      if index = retSlot then .returnLiteral else .returnVariable
    else
      returnChoice.getD .returnVariable
  let (defectOffset, _) := below 3 (forSeed seed 999991)
  let defect :=
    if index < 24 then
      if index % 8 = 7 then
        match ((index / 8) + defectOffset) % 3 with
        | 0 => .outOfRange
        | 1 => .knownOverflow
        | _ => .unusedBinding
      else
        .noDefect
    else
      defectChoice.getD .noDefect
  let ((q, state), r₃) := base returnProduction r₂
  match defect with
  | .outOfRange =>
    let (extra, _) := below 744 r₃
    let badLit := 256 + extra
    match q.body with
    | [] =>
      make { body := [⟨⟨"v", 0⟩, .atom (.lit badLit)⟩], ret := .var ⟨"v", 0⟩ }
        (some "out_of_range_literal")
    | _ =>
      let last := previous state
      let x : Var := ⟨"v", q.body.length⟩
      make { body := q.body ++ [⟨x, .add (.var last.var) (.lit badLit)⟩], ret := .var x }
        (some "out_of_range_literal")
  | .knownOverflow =>
    let (deltaA, r₄) := below 128 r₃
    let lhsVal := 128 + deltaA
    let minRhs := 256 - lhsVal
    let (deltaB, _) := below (256 - minRhs) r₄
    let rhsVal := minRhs + deltaB
    match q.body with
    | [] =>
      let a : Var := ⟨"v", 0⟩
      let b : Var := ⟨"v", 1⟩
      let c : Var := ⟨"v", 2⟩
      make { body := [⟨a, .atom (.lit lhsVal)⟩, ⟨b, .atom (.lit rhsVal)⟩,
                      ⟨c, .add (.var a) (.var b)⟩], ret := .var c }
        (some "known_overflow")
    | _ =>
      let last := previous state
      let n := q.body.length
      let a : Var := ⟨"v", n⟩
      let b : Var := ⟨"v", n + 1⟩
      let c : Var := ⟨"v", n + 2⟩
      let aInit := if last.value ≤ lhsVal then
        Slice0.Expr.add (.var last.var) (.lit (lhsVal - last.value))
      else
        Slice0.Expr.add (.var last.var) (.lit 0)
      let bVal := if last.value ≤ lhsVal then rhsVal else (256 - last.value)
      make { body := q.body ++ [⟨a, aInit⟩, ⟨b, .atom (.lit bVal)⟩, ⟨c, .add (.var a) (.var b)⟩],
             ret := .var c }
        (some "known_overflow")
  | .unusedBinding =>
    let (unusedAtom, _) := literal r₃
    let x : Var := ⟨"v", q.body.length⟩
    make { q with body := q.body ++ [⟨x, .atom unusedAtom⟩] } (some "unused_binding")
  | _ => make q (rejectionOf state)

end ConcreteSpecDiff.SpecGen

namespace ConcreteSpecDiff.BirGen
open Concrete.BIR Concrete.Backend
open ConcreteSpec (IntTy)
open ConcreteSpecDiff.Random

inductive Production where
  | checked | wrapped | widening | twoSites | chain
  deriving DecidableEq, Repr

def Production.name : Production → String
  | .checked => "checked"
  | .wrapped => "wrapped"
  | .widening => "widening"
  | .twoSites => "two_sites"
  | .chain => "chain"

/-- Every slice brief extends this one table with its new BIR shapes. -/
def weightedProductions : List (Nat × Production) :=
  [(4, .checked), (4, .wrapped), (3, .widening), (2, .twoSites), (3, .chain)]

structure Generated where
  program : Program
  production : Production
  constructs : List String

def tyName : IntTy → String
  | .i8 => "i8" | .i16 => "i16" | .i32 => "i32" | .i64 => "i64"
  | .u8 => "u8" | .u16 => "u16" | .u32 => "u32" | .u64 => "u64"

def opName : BinOp → String
  | .add => "add" | .sub => "sub" | .mul => "mul"

def randomTy (r : Rng) : IntTy × Rng :=
  let (n, r') := below 4 r
  ([IntTy.u8, .i8, .u16, .i16].getD n .u8, r')

def randomOp (r : Rng) : BinOp × Rng :=
  let (n, r') := below 3 r
  ([BinOp.add, .sub, .mul].getD n .add, r')

def randomValue (t : IntTy) (r : Rng) : Int × Rng :=
  let (n, r') := below (2 ^ t.bits) r
  (t.min + Int.ofNat n, r')

def binaryOperands (t : IntTy) (r : Rng) : (BinOp × Int × Int) × Rng :=
  let (op, r₁) := randomOp r
  let (a, r₂) := randomValue t r₁
  let (b, r₃) := randomValue t r₂
  ((op, a, b), r₃)

def twoSiteTable : SiteTable :=
  [Examples.plusSite, ⟨.arithmeticOverflow, ⟨7⟩, ⟨80, 88⟩, ⟨"generated/chain.con", 4, 9⟩⟩]

def chained (op : BinOp) (t : IntTy) (a b : Int) (c : Int) : Program :=
  { blocks := [
      { params := [], insts := [.binOvf op t 0 1 (.imm t a) (.imm t b)],
        term := .condbr 1 1 [] 2 [.reg 0] },
      { params := [], insts := [], term := .trap 0 },
      { params := [(2, .int t)], insts := [.trunc t .u8 3 (.reg 2)], term := .br 3 [.reg 3] },
      { params := [(4, .int .u8)],
        insts := [.binOvf .add .u8 5 6 (.reg 4) (.imm .u8 c)],
        term := .condbr 6 4 [] 5 [.reg 5] },
      { params := [], insts := [], term := .trap 1 },
      { params := [(7, .int .u8)], insts := [], term := .ret (.reg 7) }],
    sites := twoSiteTable }

/-- One candidate, with no hidden retry: `seed` and `index` alone determine it. -/
def generate (seed : String) (index : Nat) : Generated :=
  let r := forSeed seed index
  let (choice, r₁) := pick weightedProductions r
  let (prodOffset, _) := below 5 (forSeed seed 999992)
  let production :=
    if index < 15 then
      match (index + prodOffset) % 5 with
      | 0 => .checked
      | 1 => .wrapped
      | 2 => .widening
      | 3 => .twoSites
      | _ => .chain
    else
      choice.getD .checked
  match production with
  | .checked =>
    let (tyTicket, r₂) := below 4 r₁
    let t := [IntTy.u8, .i8, .u16, .i16].getD ((index + tyTicket) % 4) .u8
    let ((op, a, b), r₃) := binaryOperands t r₂
    let (op, t, a, b) :=
      if index < 5 then
        let (smallA, r₄) := below 100 r₃
        let (smallB, _) := below 100 r₄
        (BinOp.add, IntTy.u8, Int.ofNat smallA, Int.ofNat smallB)
      else if index < 10 then
        let (bigA, r₄) := below 50 r₃
        let (bigB, _) := below 50 r₄
        (BinOp.add, IntTy.u8, Int.ofNat (200 + bigA), Int.ofNat (100 + bigB))
      else
        (op, t, a, b)
    ⟨Examples.checkedOp op t a b, production,
      [Production.name production, "binOvf", "condbr", "trap", opName op, tyName t]⟩
  | .wrapped =>
    let (tyTicket, r₂) := below 4 r₁
    let t := [IntTy.u8, .i8, .u16, .i16].getD ((index + tyTicket) % 4) .u8
    let (opTicket, r₃) := below 3 r₂
    let op := [BinOp.add, .sub, .mul].getD ((index + opTicket) % 3) .add
    let (a, r₄) := randomValue t r₃
    let (b, _) := randomValue t r₄
    ⟨Examples.wrapped op t a b, production,
      [Production.name production, "binOvf", "trunc", "condbr", opName op, tyName t]⟩
  | .widening =>
    let (pairTicket, r₂) := below 3 r₁
    let (fr, to, pairTag) : IntTy × IntTy × String :=
      match (index + pairTicket) % 3 with
      | 0 => (.u8, .u16, "widen_u8_u16")
      | 1 => (.i8, .i16, "widen_i8_i16")
      | _ => (.u8, .i16, "widen_u8_i16")
    let (a, r₃) := randomValue fr r₂
    let ((op, _, b), _) := binaryOperands to r₃
    ⟨Examples.observe [.trunc fr to 0 (.imm fr a)] op to (.reg 0) (.imm to b), production,
      [Production.name production, pairTag, "binOvf", "trunc", "condbr", opName op, tyName fr, tyName to]⟩
  | .twoSites =>
    let (a, r₂) := randomValue .u8 r₁
    let (b, r₃) := randomValue .u8 r₂
    let (c, _) := randomValue .u8 r₃
    ⟨Examples.twoSites a b c, production,
      [Production.name production, "binOvf", "condbr", "trap", "multi_block", "multi_site", "add", "u8"]⟩
  | .chain =>
    let (tyTicket, r₂) := below 4 r₁
    let t := [IntTy.u8, .i8, .u16, .i16].getD ((index + tyTicket) % 4) .u8
    let (opTicket, r₃) := below 3 r₂
    let op := [BinOp.add, .sub, .mul].getD ((index + opTicket) % 3) .add
    let (a, r₄) := randomValue t r₃
    let (b, r₅) := randomValue t r₄
    let (c, _) := randomValue .u8 r₅
    ⟨chained op t a b c, production,
      [Production.name production, "binOvf", "trunc", "br", "condbr", "trap", "multi_block",
       "multi_site", opName op, tyName t]⟩

end ConcreteSpecDiff.BirGen

open Concrete.BIR Concrete.Backend
open ConcreteSpec (Trap)
open LeanCompilerBackend (Width)
open LeanCompilerBackend.Verified

namespace ConcreteSpecDiff

/-- Same expectation bytes and backend artifacts as slice0_bir.lean; the script module itself is
not on Lake's import path, so its small IO adapter is mirrored here over the same backend calls. -/
def writeExpected (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.writeFile (dir / "status") (status ++ "\n")
  IO.FS.writeFile (dir / "stderr") stderr

def writeLcb (dir : System.FilePath) (target : LCB.Target)
    (emitted : Source.Function × SiteTable) : IO Bool := do
  let (f, sites) := emitted
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "lcb_source.txt") (reprStr f ++ "\n")
  let job : FunctionJob :=
    { code := { symbol := LCB.bodySymbol, body := f, abi := planAbi f, layout := planFrame f } }
  IO.FS.writeFile (dir / "lcb_job.txt") (reprStr job ++ "\n")
  if let .error msg := LCB.assemble .aarch64Macos f then
    IO.eprintln s!"{dir}: LCB first path: {msg}"; return false
  match LCB.assemble target f with
  | .error msg => IO.eprintln s!"{dir}: LCB: {msg}"; return false
  | .ok asm =>
    IO.FS.writeFile (dir / "lcb_body.s") asm
    IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe sites)
    IO.FS.writeFile (dir / "lcb_body.ssa") (LCB.render f)
    return true

def writeBackends (dir : System.FilePath) (target : LCB.Target) (p : Program)
    (emitted : Source.Function × SiteTable) : IO Bool := do
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (dir / "llvm.ll") (LLVM.module p)
  writeLcb dir target emitted

def unwiredMessage := "no Core→BIR lowering yet (slice 1)"

/-- Whether Core→BIR lowering for the supported slice-0 fragment is available (X3).
While `false`, `compileCore` must return `.error unwiredMessage` on every candidate and the gate
reports 0 compiled programs loudly. Once lowering lands, set `loweringAvailable := true`; every
supported accepted candidate must then compile (`compiled = supportedAccepted = accepted`), and
any refusal or partial compilation fails the gate. -/
def loweringAvailable : Bool := false

/-- THE integration point for the lowering branch. Its implementation must lower the spec's Core
program to BIR; until that branch lands, the spec leg runs its generator/oracle invariants but has
no compiled programs and never claims a spec/native comparison. -/
def compileCore (_ : ConcreteSpec.Program) : Except String Program :=
  .error unwiredMessage

inductive Mutation where
  | none | dropOverflowLLVM | lcbOffByOne
  deriving DecidableEq, Repr

def mutationOfName : String → Option Mutation
  | "drop-overflow-llvm" => some .dropOverflowLLVM
  | "lcb-off-by-one" => some .lcbOffByOne
  | _ => none

/-- Replace every overflow-flag extraction by constant false. The wrapped value and control-flow
shape remain, so a trapping generated program exits through its non-trap arm instead. -/
def dropOverflowLLVM (text : String) : String :=
  let lines := text.splitOn "\n"
  String.intercalate "\n" <| lines.map fun line =>
    match line.splitOn " = extractvalue " with
    | [lhs, _] => if line.endsWith ", 1" then lhs ++ " = xor i1 0, 0" else line
    | _ => line

/-- Add one to each BIR return with fresh block-local registers, before adaptation. This preserves
LCB's canonical numbering by construction; the control uses only exits below 255. -/
def lcbOffByOne (p : Program) : Program :=
  let firstFresh := p.defs.foldl (fun next (r, _) => max next (r + 1)) 0
  let blocks := (p.blocks.zip (List.range p.blocks.length)).map fun (b, j) =>
    match b.term with
    | .ret a =>
      let fresh := firstFresh + j
      { b with insts := b.insts ++ [.bin .add .u8 fresh a (.imm .u8 1)], term := .ret (.reg fresh) }
    | _ => b
  { p with blocks := blocks }

/-- A semantics-side model of the LLVM control mutation: wrapped results remain, every overflow
flag is false, and each overflow conditional takes its non-trap arm. Unreachable trap blocks are
left in the model because it is used only with `run`, not emitted as WF BIR. -/
def dropOverflowBir (p : Program) : Program :=
  let flags := p.blocks.flatMap fun b => b.insts.filterMap fun
    | .binOvf _ _ _ f _ _ => some f
    | _ => none
  let blocks := p.blocks.map fun b =>
    let insts := b.insts.map fun
      | .binOvf op t d _ a b => Inst.bin op t d a b
      | i => i
    let term := match b.term with
      | .condbr c _ _ no args => if flags.contains c then Term.br no args else b.term
      | t => t
    { b with insts := insts, term := term }
  { p with blocks := blocks }

def hexDigit (n : Nat) : Char :=
  if n < 10 then Char.ofNat ('0'.toNat + n) else Char.ofNat ('a'.toNat + (n - 10))

def hexUInt64 (x : UInt64) : String :=
  String.ofList <| (List.range 16).reverse.map fun i =>
    hexDigit ((UInt64.shiftRight x (UInt64.ofNat (i * 4))).toNat % 16)

/-- Collision-resistant, manifest-valid (`[a-z0-9_]+`) seed identifier combining a lowercase
sanitized prefix with the 64-bit FNV-1a hex digest of the raw seed string. -/
def safeSeed (seed : String) : String :=
  let cleaned := (seed.toList.take 24).map fun c =>
    if c.isAlpha then c.toLower
    else if c.isDigit || c = '_' then c
    else '_'
  let prefixStr := String.ofList cleaned
  let prefixStr := if prefixStr.isEmpty then "seed" else prefixStr
  s!"{prefixStr}_{hexUInt64 (Random.seedHash seed)}"

def regressionName (leg : String) (seed : String) (index : Nat) : String :=
  s!"regression_{leg}_{safeSeed seed}_{index}"

def renderAtom : ConcreteSpec.Slice0.Atom → String
  | .lit n => toString n
  | .var x => s!"{x.hint}_{x.id}"

def renderExpr : ConcreteSpec.Slice0.Expr → String
  | .atom a => renderAtom a
  | .add l r => s!"{renderAtom l} + {renderAtom r}"

def renderStmt (s : ConcreteSpec.Slice0.Stmt) : String :=
  s!"    let {s.x.hint}_{s.x.id}: u8 = {renderExpr s.e};\n"

def renderSlice0 (q : ConcreteSpec.Slice0.Program) : String :=
  let body := String.join (q.body.map renderStmt)
  s!"fn main() -> u8 \{\n{body}    return {renderAtom q.ret};\n}\n"

def trapKindName : Trap → String
  | .arithmeticOverflow => "arithmeticOverflow"

def outcomeName : Outcome → String
  | .exited _ => "exit"
  | .trapped _ => "trap"
  | .stuck => "stuck"
  | .outOfFuel => "out_of_fuel"

def writeObservation (dir : System.FilePath) (status : String) (stderr : String) : IO Unit := do
  IO.FS.createDirAll dir
  writeExpected dir status stderr
  IO.FS.writeFile (dir / "stdout") ""

def writeOutcome (dir : System.FilePath) (outcome : Outcome) (sites : SiteTable) : IO Bool := do
  match outcome with
  | .exited c => writeObservation dir (toString c) ""; return true
  | .trapped s => match sites[s]? with
    | some e => writeObservation dir "134" (Runtime.trapLine e ++ "\n"); return true
    | none => IO.eprintln s!"{dir}: no trap site {s}"; return false
  | _ => IO.eprintln s!"{dir}: outcome {repr outcome} has no final observation"; return false

def evaluatorObservation (f : Source.Function) (sites : SiteTable) : String × String :=
  match LCB.evalBody f 100000 with
  | none => ("incomplete", "")
  | some w =>
    if w < 256 then (toString w, "") else
      match sites[w - 256]? with
      | some e => ("134", Runtime.trapLine e ++ "\n")
      | none => ("134", Runtime.invalidLine ++ "\n")

def writeEvaluator (dir : System.FilePath) (f : Source.Function) (sites : SiteTable) : IO Unit := do
  let (status, stderr) := evaluatorObservation f sites
  IO.FS.writeFile (dir / "lcb_eval.status") (status ++ "\n")
  IO.FS.writeFile (dir / "lcb_eval.stderr") stderr
  IO.FS.writeFile (dir / "lcb_eval.stdout") ""

def writeBirRegressionMetadata (dir : System.FilePath) (seed : String) (index : Nat)
    (outcome : Outcome) (sites : SiteTable) : IO Unit := do
  let name := regressionName "bir" seed index
  let expectation ← match outcome with
    | .exited c => pure s!"exit {c}"
    | .trapped s => match sites[s]? with
      | some e => pure s!"trap {trapKindName e.kind} site {s}"
      | none => pure "INVALID"
    | _ => pure "INVALID"
  IO.FS.writeFile (dir / "safe-seed.txt") (safeSeed seed ++ "\n")
  IO.FS.writeFile (dir / "regression-name.txt") (name ++ "\n")
  IO.FS.writeFile (dir / "manifest-row.txt")
    s!"{name}\tbir:{name}\t{expectation}\tslice0\n"

def writeSpecRegressionMetadata (dir : System.FilePath) (seed : String) (index : Nat)
    (q : ConcreteSpec.Slice0.Program) (exitCode : Nat) : IO Unit := do
  let name := regressionName "spec" seed index
  IO.FS.writeFile (dir / "safe-seed.txt") (safeSeed seed ++ "\n")
  IO.FS.writeFile (dir / "regression-name.txt") (name ++ "\n")
  IO.FS.writeFile (dir / "program.con") (renderSlice0 q)
  IO.FS.writeFile (dir / "manifest-row.txt")
    s!"{name}\tcon:tests/slice0/{name}.con\texit {exitCode}\tslice0\n"

/-- Emit exactly the files consumed by slice0_native.sh, plus the evaluator's observation and the
same judge inputs as the manifest runner. The expectation is supplied separately for the spec leg. -/
def emitArtifacts (dir : System.FilePath) (target : LCB.Target) (p : Program) (mutation : Mutation) :
    IO Bool := do
  let lcbProgram := if mutation = .lcbOffByOne then lcbOffByOne p else p
  unless wf p && wf lcbProgram do
    IO.eprintln s!"{dir}: generated or control-mutated BIR is not well formed"
    return false
  match LCB.emit lcbProgram with
  | .error msg => IO.eprintln s!"{dir}: adapter: {msg}"; return false
  | .ok (f, sites) =>
    unless ← writeBackends dir target p (f, sites) do return false
    if mutation = .dropOverflowLLVM then
      let original ← IO.FS.readFile (dir / "llvm.ll")
      let changed := dropOverflowLLVM original
      if changed = original then IO.eprintln s!"{dir}: drop-overflow-llvm changed nothing"; return false
      IO.FS.writeFile (dir / "llvm.ll") changed
    writeEvaluator dir f sites
    return true

def emitBirCandidate (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target)
    (mutation : Mutation) : IO UInt32 := do
  let g := BirGen.generate seed index
  unless wf g.program do
    IO.eprintln s!"seed {seed} index {index}: rejected: generated BIR is not well formed"
    return 4
  match LCB.emit g.program with
  | .error msg => IO.eprintln s!"seed {seed} index {index}: rejected: {msg}"; return 4
  | .ok _ =>
    let outcome := run g.program 1000
    unless outcome.final && outcome != .stuck do
      IO.eprintln s!"seed {seed} index {index}: non-final/stuck outcome {repr outcome}"
      return 1
    unless ← emitArtifacts dir target g.program mutation do return 1
    unless ← writeOutcome dir outcome g.program.sites do return 1
    if mutation = .dropOverflowLLVM then
      match run (dropOverflowBir g.program) 1000 with
      | .exited c => writeObservation (dir / "control-llvm") (toString c) ""
      | o =>
        IO.eprintln s!"{dir}: non-trap control model gives {repr o}"
        return 1
    if mutation = .lcbOffByOne then
      match outcome with
      | .exited c =>
        unless c < 255 do IO.eprintln s!"{dir}: off-by-one control needs an exit below 255"; return 1
        writeObservation (dir / "control-lcb") (toString (c + 1)) ""
      | _ => IO.eprintln s!"{dir}: off-by-one control needs an exit"; return 1
    IO.FS.writeFile (dir / "program.lean.txt") (reprStr g.program ++ "\n")
    IO.FS.writeFile (dir / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
    writeBirRegressionMetadata dir seed index outcome g.program.sites
    IO.println s!"seed {seed} index {index}: {outcomeName outcome} {BirGen.Production.name g.production}"
    return 0

abbrev Counts := List (String × Nat)

def bump (key : String) : Counts → Counts
  | [] => [(key, 1)]
  | (k, n) :: rest => if k = key then (k, n + 1) :: rest else (k, n) :: bump key rest

def addConstructs (counts : Counts) (xs : List String) : Counts :=
  xs.foldl (fun acc x => bump x acc) counts

def countsLines (kind : String) (counts : Counts) : String :=
  String.join <| counts.map fun (name, n) => s!"{kind}\t{name}\t{n}\n"

/-- Generate and filter N independent BIR candidates. Rejections are data and are counted; the
shell gate imposes its stated yield floor and coverage/vacuity requirements. -/
def genBir (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  IO.FS.createDirAll dir
  let mut programs := ""
  let mut rejects := ""
  let mut constructs : Counts := []
  let mut accepted := 0
  let mut rejected := 0
  let mut exits := 0
  let mut traps := 0
  for index in List.range n do
    let g := BirGen.generate seed index
    if !wf g.program then
      rejected := rejected + 1
      rejects := rejects ++ s!"reject\t{index}\tnot well formed\n"
    else match LCB.emit g.program with
      | .error msg =>
        rejected := rejected + 1
        rejects := rejects ++ s!"reject\t{index}\t{msg}\n"
      | .ok _ =>
        let outcome := run g.program 1000
        if !outcome.final || outcome == .stuck then
          IO.eprintln s!"seed {seed} index {index}: generated outcome {repr outcome}"
          return 1
        let child := dir / s!"p{index}"
        unless ← emitArtifacts child target g.program .none do return 1
        unless ← writeOutcome child outcome g.program.sites do return 1
        IO.FS.writeFile (child / "program.lean.txt") (reprStr g.program ++ "\n")
        IO.FS.writeFile (child / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
        writeBirRegressionMetadata child seed index outcome g.program.sites
        accepted := accepted + 1
        constructs := addConstructs constructs g.constructs
        if outcomeName outcome = "exit" then exits := exits + 1 else traps := traps + 1
        programs := programs ++
          s!"program\t{index}\t{outcomeName outcome}\t{BirGen.Production.name g.production}\n"
    IO.FS.writeFile (dir / "summary.tsv")
      (s!"seed\t{seed}\naccepted\t{accepted}\nrejected\t{rejected}\nattempted\t{n}\n" ++
       s!"outcome\texit\t{exits}\noutcome\ttrap\t{traps}\n" ++
       countsLines "construct" constructs ++ programs ++ rejects)
  IO.println s!"bir leg: {accepted} accepted, {rejected} rejected, exits {exits}, traps {traps}"
  IO.print <| countsLines "construct" constructs
  return 0

def specOutcome (q : ConcreteSpec.Slice0.Program) : ConcreteSpec.Outcome :=
  ConcreteSpec.run (ConcreteSpec.Slice0.toCore q) (q.body.length + 1)

/-- The spec outcome supplies status and kind but no site. Until it carries one, two lowered sites
of the trapping kind are ambiguous and are a setup error rather than an arbitrary first-site choice. -/
def writeSpecOutcome (dir : System.FilePath) (p : Program) (o : ConcreteSpec.Outcome) : IO Bool := do
  match o with
  | .exit c => writeObservation dir (toString c) ""; return true
  | .trap k => match p.sites.filter (fun e => e.kind = k) with
    | [e] => writeObservation dir "134" (Runtime.trapLine e ++ "\n"); return true
    | [] => IO.eprintln s!"{dir}: spec trap {repr k} has no lowered site"; return false
    | _ => IO.eprintln s!"{dir}: spec trap {repr k} has multiple lowered sites and no spec site"; return false
  | _ => IO.eprintln s!"{dir}: spec outcome {repr o} has no observation"; return false

def specOutcomeName : ConcreteSpec.Outcome → String
  | .exit _ => "exit" | .trap _ => "trap" | .stuck _ => "stuck" | .outOfFuel => "out_of_fuel"

/-- Generate accepted slice-0 Core programs, run the spec oracle, and enforce the lowering
capability contract (X3):
- When `avail = false`, `compiler` must return `.error unwiredMessage` on every accepted candidate
  (`compiled = 0`).
- When `avail = true`, `compiler` must succeed on every supported accepted candidate
  (`compiled = supportedAccepted = accepted`). Partial lowering is a fatal failure. -/
def genSpecWith (avail : Bool) (compiler : ConcreteSpec.Program → Except String Program)
    (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  IO.FS.createDirAll dir
  let mut programs := ""
  let mut rejects := ""
  let mut constructs : Counts := []
  let mut rejections : Counts := []
  let mut accepted := 0
  let mut supportedAccepted := 0
  let mut rejected := 0
  let mut compiled := 0
  let mut exits := 0
  let mut traps := 0
  for index in List.range n do
    let g := SpecGen.generate seed index
    let core := ConcreteSpec.Slice0.toCore g.program
    if !ConcreteSpec.check core then
      rejected := rejected + 1
      let reason := g.rejection.getD "spec_check"
      rejections := bump reason rejections
      rejects := rejects ++ s!"reject\t{index}\t{reason}\n"
    else
      if g.rejection.isSome then
        IO.eprintln s!"seed {seed} index {index}: checker accepted predicted rejection {repr g.rejection}"
        return 1
      let outcome := specOutcome g.program
      match outcome with
      | .exit c =>
        unless c < 256 do IO.eprintln s!"seed {seed} index {index}: invalid spec exit {c}"; return 1
        accepted := accepted + 1
        supportedAccepted := supportedAccepted + 1
        exits := exits + 1
        constructs := addConstructs constructs g.constructs
        let child := dir / s!"p{index}"
        IO.FS.createDirAll child
        IO.FS.writeFile (child / "program.lean.txt") (reprStr g.program ++ "\n")
        IO.FS.writeFile (child / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
        IO.FS.writeFile (child / "oracle.txt") s!"exit {c}\n"
        writeObservation child (toString c) ""
        writeSpecRegressionMetadata child seed index g.program c
        let mut didCompile := false
        match compiler core with
        | .error msg =>
          if avail then
            IO.eprintln s!"FAIL spec leg capability contract: loweringAvailable = true but compileCore refused supported accepted candidate (seed {seed} index {index}): {msg}"
            return 1
          if msg != unwiredMessage then
            IO.eprintln s!"seed {seed} index {index}: compileCore: {msg}"
            return 1
        | .ok bir =>
          if !avail then
            IO.eprintln s!"FAIL spec leg capability contract: compileCore compiled seed {seed} index {index} while loweringAvailable = false"
            return 1
          unless ← emitArtifacts child target bir .none do return 1
          unless ← writeSpecOutcome child bir outcome do return 1
          IO.FS.writeFile (child / "native-ready") ""
          compiled := compiled + 1
          didCompile := true
        programs := programs ++
          s!"program\t{index}\texit\t{if didCompile then "true" else "false"}\n"
      | .trap _ =>
        accepted := accepted + 1
        traps := traps + 1
        IO.eprintln s!"seed {seed} index {index}: accepted slice-0 program trapped"
        return 1
      | _ =>
        IO.eprintln s!"seed {seed} index {index}: spec oracle gives {repr outcome}"
        return 1
    IO.FS.writeFile (dir / "summary.tsv")
      (s!"seed\t{seed}\nlowering_available\t{avail}\naccepted\t{accepted}\nsupported_accepted\t{supportedAccepted}\nrejected\t{rejected}\nattempted\t{n}\ncompiled\t{compiled}\n" ++
       s!"outcome\texit\t{exits}\noutcome\ttrap\t{traps}\n" ++
       countsLines "construct" constructs ++ countsLines "rejection" rejections ++ programs ++ rejects)
  if !avail && compiled != 0 then
    IO.eprintln s!"FAIL spec leg capability contract: compiled {compiled} != 0 while loweringAvailable = false"
    return 1
  if avail && (compiled != supportedAccepted || supportedAccepted != accepted) then
    IO.eprintln s!"FAIL spec leg capability contract: partial lowering compiled {compiled} of {supportedAccepted} supported accepted candidates"
    return 1
  IO.FS.writeFile (dir / "summary.tsv")
    (s!"seed\t{seed}\nlowering_available\t{avail}\naccepted\t{accepted}\nsupported_accepted\t{supportedAccepted}\nrejected\t{rejected}\nattempted\t{n}\ncompiled\t{compiled}\n" ++
     s!"outcome\texit\t{exits}\noutcome\ttrap\t{traps}\n" ++
     countsLines "construct" constructs ++ countsLines "rejection" rejections ++ programs ++ rejects)
  IO.println s!"spec leg: {accepted} accepted ({supportedAccepted} supported), {rejected} rejected, exits {exits}, traps {traps}"
  IO.print <| countsLines "construct" constructs
  IO.print <| countsLines "rejection" rejections
  if compiled = 0 then
    IO.println "spec leg: 0 programs compiled (no Core→BIR lowering; becomes required in the lowering branch)"
  else
    IO.println s!"spec leg: {compiled} programs compiled for native comparison"
  return 0

def genSpec (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target) : IO UInt32 :=
  genSpecWith loweringAvailable compileCore seed n dir target

/-- Planted partial-compilation control (X3): simulates a partially wired `compileCore` that
lowers empty-body programs (`return <lit>`) to `Examples.answer` and refuses multi-statement
slice-0 programs with `unwiredMessage`. With `avail = true`, `genSpecWith` must reject the partial
lowering as soon as a supported accepted candidate is refused. -/
def controlPartialLowering (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target) :
    IO UInt32 := do
  let partialCompiler : ConcreteSpec.Program → Except String Program := fun core =>
    if core.body.isEmpty then .ok Concrete.BIR.Examples.answer else .error unwiredMessage
  genSpecWith true partialCompiler seed n dir target

/-- Replay exactly one accepted spec candidate through compileCore. While the hook is unwired this
writes the generated oracle inputs and refuses to claim a native comparison. -/
def replaySpec (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target) :
    IO UInt32 := do
  let g := SpecGen.generate seed index
  let core := ConcreteSpec.Slice0.toCore g.program
  if !ConcreteSpec.check core || g.rejection.isSome then
    IO.eprintln s!"seed {seed} index {index}: rejected spec candidate"
    return 4
  match specOutcome g.program with
  | .exit c =>
    IO.FS.createDirAll dir
    IO.FS.writeFile (dir / "program.lean.txt") (reprStr g.program ++ "\n")
    IO.FS.writeFile (dir / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
    IO.FS.writeFile (dir / "oracle.txt") s!"exit {c}\n"
    writeObservation dir (toString c) ""
    writeSpecRegressionMetadata dir seed index g.program c
    match compileCore core with
    | .error msg =>
      if loweringAvailable then
        IO.eprintln s!"FAIL spec leg capability contract: loweringAvailable = true but compileCore refused seed {seed} index {index}: {msg}"
        return 1
      IO.eprintln s!"spec leg: 0 programs compiled ({msg})"
      return 4
    | .ok bir =>
      if !loweringAvailable then
        IO.eprintln s!"FAIL spec leg capability contract: compileCore compiled seed {seed} index {index} while loweringAvailable = false"
        return 1
      unless ← emitArtifacts dir target bir .none do return 1
      unless ← writeSpecOutcome dir bir (.exit c) do return 1
      IO.FS.writeFile (dir / "native-ready") ""
      return 0
  | o => IO.eprintln s!"seed {seed} index {index}: spec oracle gives {repr o}"; return 1

/-- Stand-in compiled output for the spec-oracle control: Slice0.answer and BIR answer both exit 42,
but the judge receives a planted expected 43. -/
def controlSpecOracle (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let q := ConcreteSpec.Slice0.answer
  let p := Concrete.BIR.Examples.answer
  unless ← emitArtifacts dir target p .none do return 1
  unless ← writeSpecOutcome (dir / "spec-expected") p (specOutcome q) do return 1
  writeObservation (dir / "wrong-expected") "43" ""
  IO.println "control spec-oracle: spec exits 42; planted expectation exits 43"
  return 0

def withTarget (name : String) (k : LCB.Target → IO UInt32) : IO UInt32 :=
  match LCB.Target.ofName name with
  | some t => k t
  | none => do IO.eprintln s!"unknown target {name}"; return 2

def parseNat (value : String) : Option Nat := value.toNat?

end ConcreteSpecDiff

open ConcreteSpecDiff

def main (args : List String) : IO UInt32 := do
  match args with
  | ["gen-spec", seed, n, dir] => match parseNat n with
    | some n => do
      let target := (← IO.getEnv "SPEC_DIFF_TARGET").getD "x86_64-linux"
      withTarget target (genSpec seed n dir)
    | none => return 2
  | ["gen-bir", seed, n, dir, target] => match parseNat n with
    | some n => withTarget target (genBir seed n dir)
    | none => return 2
  | ["replay", seed, index, dir, target] => match parseNat index with
    | some index => withTarget target (fun t => emitBirCandidate seed index dir t .none)
    | none => return 2
  | ["replay-spec", seed, index, dir, target] => match parseNat index with
    | some index => withTarget target (replaySpec seed index dir)
    | none => return 2
  | ["control-emit", "--mutate", mutation, seed, index, dir, target,
      "--control", "SPEC_DIFF_PLANTED_CONTROL"] => match mutationOfName mutation, parseNat index with
    | some mutation, some index =>
      withTarget target (fun t => emitBirCandidate seed index dir t mutation)
    | _, _ => return 2
  | ["control-spec-oracle", dir, target, "--control", "SPEC_DIFF_PLANTED_CONTROL"] =>
    withTarget target (controlSpecOracle dir)
  | ["control-partial-lowering", seed, n, dir, target,
      "--control", "SPEC_DIFF_PLANTED_CONTROL"] => match parseNat n with
    | some n => withTarget target (controlPartialLowering seed n dir)
    | none => return 2
  | _ =>
    IO.eprintln "usage: spec_diff.lean gen-spec SEED N DIR | gen-bir SEED N DIR TARGET | (replay | replay-spec) SEED INDEX DIR TARGET | control-emit --mutate NAME SEED INDEX DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL | control-spec-oracle DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL | control-partial-lowering SEED N DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL"
    return 2

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

inductive PlantedFailure where
  | none | lowering | emission | assembly | nativeSetup
  deriving DecidableEq, Repr

def plantedFailureOfName : String → Option PlantedFailure
  | "lowering" => some .lowering
  | "emission" => some .emission
  | "assembly" => some .assembly
  | "native-setup" => some .nativeSetup
  | _ => none

def targetName : LCB.Target → String
  | .x86_64Linux => "x86_64-linux"
  | .aarch64Linux => "aarch64-linux"
  | .aarch64Macos => "aarch64-macos"

structure CandidateCtx where
  leg : String
  seed : String
  index : Nat
  dir : System.FilePath
  target : LCB.Target

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

/-- Write the one durable failure record into the candidate scratch directory before returning any
failure. `collect_available` in `spec_diff.sh` persists every directory carrying `failure-kinds.txt`
into `SPEC_DIFF_REGRESSIONS` before scratch cleanup. -/
def recordCandidateFailure (ctx : CandidateCtx) (kind : String) (subject : String)
    (diagnostic : String) : IO Unit := do
  IO.FS.createDirAll ctx.dir
  IO.FS.writeFile (ctx.dir / "leg") (ctx.leg ++ "\n")
  IO.FS.writeFile (ctx.dir / "seed") (ctx.seed ++ "\n")
  IO.FS.writeFile (ctx.dir / "safe-seed.txt") (safeSeed ctx.seed ++ "\n")
  IO.FS.writeFile (ctx.dir / "index") (toString ctx.index ++ "\n")
  IO.FS.writeFile (ctx.dir / "regression-name.txt") (regressionName ctx.leg ctx.seed ctx.index ++ "\n")
  let kindsPath := ctx.dir / "failure-kinds.txt"
  let existingKinds ← if ← kindsPath.pathExists then IO.FS.readFile kindsPath else pure ""
  unless (existingKinds.splitOn "\n").contains kind do
    IO.FS.writeFile kindsPath (existingKinds ++ kind ++ "\n")
  let diagPath := ctx.dir / "diagnostics.txt"
  let existingDiag ← if ← diagPath.pathExists then IO.FS.readFile diagPath else pure ""
  unless (existingDiag.splitOn "\n").contains diagnostic do
    IO.FS.writeFile diagPath (existingDiag ++ diagnostic ++ "\n")
  let recPath := ctx.dir / "failure-record.tsv"
  let existingRec ← if ← recPath.pathExists then IO.FS.readFile recPath else pure ""
  IO.FS.writeFile recPath (existingRec ++ s!"{kind}\t{subject}\t{diagnostic}\n")
  IO.eprintln diagnostic

/-- An append-only, flushed stage journal survives an uncatchable worker exit. Ordinary IO
exceptions also record candidate context; the shell coordinator owns process termination. -/
def candidateStage (ctx : CandidateCtx) (stage : String) (action : Unit → IO α) : IO α := do
  IO.FS.createDirAll ctx.dir
  let append (state : String) : IO Unit := do
    let h ← IO.FS.Handle.mk (ctx.dir / "stage-journal.tsv") .append
    h.putStr s!"{stage}\t{state}\t{← IO.Process.getPID}\n"
    h.flush
  append "started"
  if (← IO.getEnv "SPEC_DIFF_WORKER_CONTROL") == some "SPEC_DIFF_PLANTED_CONTROL" &&
      (← IO.getEnv "SPEC_DIFF_WORKER_EXIT_STAGE") == some stage && ctx.index == 0 then
    IO.Process.exit 7
  try
    let result ← action ()
    append "completed"
    return result
  catch e =>
    recordCandidateFailure ctx stage "driver_exception"
      s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [{stage}]: driver exception: {e}"
    append "exception"
    throw e

def writeLcbImpl (ctx : CandidateCtx) (emitted : Source.Function × SiteTable)
    (planted : PlantedFailure := .none) : IO Bool := do
  let (f, sites) := emitted
  IO.FS.createDirAll ctx.dir
  IO.FS.writeFile (ctx.dir / "lcb_source.txt") (reprStr f ++ "\n")
  let job : FunctionJob :=
    { code := { symbol := LCB.bodySymbol, body := f, abi := planAbi f, layout := planFrame f } }
  IO.FS.writeFile (ctx.dir / "lcb_job.txt") (reprStr job ++ "\n")
  IO.FS.writeFile (ctx.dir / "lcb_rt.ssa") (Runtime.qbe sites)
  IO.FS.writeFile (ctx.dir / "lcb_body.ssa") (LCB.render f)
  let firstAsm :=
    if planted == .assembly then Except.error "planted LCB assembly refusal"
    else LCB.assemble .aarch64Macos f
  if let .error msg := firstAsm then
    recordCandidateFailure ctx "assembly" "aarch64-macos"
      s!"FAIL {ctx.leg} assembly: seed {ctx.seed} index {ctx.index} [aarch64-macos]: {msg}"
    return false
  match LCB.assemble ctx.target f with
  | .error msg =>
    let tname := targetName ctx.target
    recordCandidateFailure ctx "assembly" tname
      s!"FAIL {ctx.leg} assembly: seed {ctx.seed} index {ctx.index} [{tname}]: {msg}"
    return false
  | .ok asm =>
    IO.FS.writeFile (ctx.dir / "lcb_body.s") asm
    return true

def writeLcb (ctx : CandidateCtx) (emitted : Source.Function × SiteTable)
    (planted : PlantedFailure := .none) : IO Bool :=
  candidateStage ctx "assembly" (fun _ => writeLcbImpl ctx emitted planted)

def writeBackends (ctx : CandidateCtx) (p : Program)
    (emitted : Source.Function × SiteTable) (planted : PlantedFailure := .none) : IO Bool := do
  IO.FS.createDirAll ctx.dir
  IO.FS.writeFile (ctx.dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (ctx.dir / "llvm.ll") (LLVM.module p)
  writeLcb ctx emitted planted

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

def writeOutcome (ctx : CandidateCtx) (outcome : Outcome) (sites : SiteTable) : IO Bool := do
  match outcome with
  | .exited c => writeObservation ctx.dir (toString c) ""; return true
  | .trapped s => match sites[s]? with
    | some e => writeObservation ctx.dir "134" (Runtime.trapLine e ++ "\n"); return true
    | none =>
      recordCandidateFailure ctx "native_setup" "trap_site"
        s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: no trap site {s}"
      return false
  | _ =>
    recordCandidateFailure ctx "native_setup" "bir_oracle"
      s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: outcome {repr outcome} has no final observation"
    return false

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
  IO.FS.writeFile (dir / "lcb_eval.stderr") stderr
  IO.FS.writeFile (dir / "lcb_eval.stdout") ""
  -- Publish the completion marker last and atomically: collection may follow a killed driver.
  IO.FS.writeFile (dir / "lcb_eval.status.tmp") (status ++ "\n")
  IO.FS.rename (dir / "lcb_eval.status.tmp") (dir / "lcb_eval.status")

def writeBirRegressionMetadata (dir : System.FilePath) (seed : String) (index : Nat)
    (outcome : Outcome) (sites : SiteTable) : IO Unit := do
  let name := regressionName "bir" seed index
  let expectation ← match outcome with
    | .exited c => pure s!"exit {c}"
    | .trapped s => match sites[s]? with
      | some e => pure s!"trap {trapKindName e.kind} site {s}"
      | none => pure "INVALID"
    | _ => pure "INVALID"
  IO.FS.writeFile (dir / "leg") "bir\n"
  IO.FS.writeFile (dir / "seed") (seed ++ "\n")
  IO.FS.writeFile (dir / "safe-seed.txt") (safeSeed seed ++ "\n")
  IO.FS.writeFile (dir / "index") (toString index ++ "\n")
  IO.FS.writeFile (dir / "regression-name.txt") (name ++ "\n")
  IO.FS.writeFile (dir / "manifest-row.txt")
    s!"{name}\tbir:{name}\t{expectation}\tslice0\n"

def writeSpecRegressionMetadata (dir : System.FilePath) (seed : String) (index : Nat)
    (q : ConcreteSpec.Slice0.Program) (exitCode : Nat) : IO Unit := do
  let name := regressionName "spec" seed index
  IO.FS.writeFile (dir / "leg") "spec\n"
  IO.FS.writeFile (dir / "seed") (seed ++ "\n")
  IO.FS.writeFile (dir / "safe-seed.txt") (safeSeed seed ++ "\n")
  IO.FS.writeFile (dir / "index") (toString index ++ "\n")
  IO.FS.writeFile (dir / "regression-name.txt") (name ++ "\n")
  IO.FS.writeFile (dir / "program.con") (renderSlice0 q)
  IO.FS.writeFile (dir / "manifest-row.txt")
    s!"{name}\tcon:tests/slice0/{name}.con\texit {exitCode}\tslice0\n"

/-- Emit exactly the files consumed by slice0_native.sh, plus the evaluator's observation and the
same judge inputs as the manifest runner. The expectation is supplied separately for the spec leg. -/
def emitArtifactsImpl (ctx : CandidateCtx) (p : Program) (mutation : Mutation)
    (planted : PlantedFailure := .none) : IO Bool := do
  let lcbProgram := if mutation = .lcbOffByOne then lcbOffByOne p else p
  IO.FS.createDirAll ctx.dir
  IO.FS.writeFile (ctx.dir / "bir.txt") (reprStr p ++ "\n")
  IO.FS.writeFile (ctx.dir / "llvm.ll") (LLVM.module p)
  let legLabel := if ctx.leg == "bir" then "BIR" else ctx.leg
  unless wf p && wf lcbProgram do
    let msg := "generated or control-mutated BIR is not well formed"
    recordCandidateFailure ctx "emission" "wf"
      s!"FAIL {legLabel} emission: seed {ctx.seed} index {ctx.index}: {msg}"
    return false
  let emitted :=
    if planted == .emission then Except.error "planted selective emission refusal"
    else LCB.emit lcbProgram
  match emitted with
  | .error msg =>
    recordCandidateFailure ctx "emission" "lcb_emit"
      s!"FAIL {legLabel} emission: seed {ctx.seed} index {ctx.index}: {msg}"
    return false
  | .ok (f, sites) =>
    unless ← writeBackends ctx p (f, sites) planted do return false
    if mutation = .dropOverflowLLVM then
      let original ← IO.FS.readFile (ctx.dir / "llvm.ll")
      let changed := dropOverflowLLVM original
      if changed = original then
        recordCandidateFailure ctx "native_setup" "drop-overflow-llvm"
          s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: drop-overflow-llvm changed nothing"
        return false
      IO.FS.writeFile (ctx.dir / "llvm.ll") changed
    unless planted == .nativeSetup do
      writeEvaluator ctx.dir f sites
    return true

def emitArtifacts (ctx : CandidateCtx) (p : Program) (mutation : Mutation)
    (planted : PlantedFailure := .none) : IO Bool :=
  candidateStage ctx "emission" (fun _ => emitArtifactsImpl ctx p mutation planted)

def initBirCandidateDirImpl (ctx : CandidateCtx) (g : BirGen.Generated) (outcome : Outcome) : IO Bool := do
  IO.FS.createDirAll ctx.dir
  IO.FS.writeFile (ctx.dir / "program.lean.txt") (reprStr g.program ++ "\n")
  IO.FS.writeFile (ctx.dir / "bir.txt") (reprStr g.program ++ "\n")
  IO.FS.writeFile (ctx.dir / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
  writeBirRegressionMetadata ctx.dir ctx.seed ctx.index outcome g.program.sites
  unless outcome.final && outcome != .stuck do
    recordCandidateFailure ctx "native_setup" "bir_oracle"
      s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: non-final/stuck outcome {repr outcome}"
    return false
  writeOutcome ctx outcome g.program.sites

def initBirCandidateDir (ctx : CandidateCtx) (g : BirGen.Generated) (outcome : Outcome) : IO Bool :=
  candidateStage ctx "initialization" (fun _ => initBirCandidateDirImpl ctx g outcome)

def emitBirCandidateImpl (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target)
    (mutation : Mutation) (planted : PlantedFailure := .none) : IO UInt32 := do
  let g := BirGen.generate seed index
  unless wf g.program do
    IO.eprintln s!"seed {seed} index {index}: rejected: generated BIR is not well formed"
    return 4
  let ctx : CandidateCtx := ⟨"bir", seed, index, dir, target⟩
  let outcome := run g.program 1000
  unless ← initBirCandidateDir ctx g outcome do return 1
  if mutation = .dropOverflowLLVM then
    match run (dropOverflowBir g.program) 1000 with
    | .exited c => writeObservation (dir / "control-llvm") (toString c) ""
    | o =>
      recordCandidateFailure ctx "native_setup" "control-llvm"
        s!"FAIL {ctx.leg} seed {seed} index {index} [native_setup]: non-trap control model gives {repr o}"
      return 1
  if mutation = .lcbOffByOne then
    match outcome with
    | .exited c =>
      unless c < 255 do
        recordCandidateFailure ctx "native_setup" "control-lcb"
          s!"FAIL {ctx.leg} seed {seed} index {index} [native_setup]: off-by-one control needs an exit below 255"
        return 1
      writeObservation (dir / "control-lcb") (toString (c + 1)) ""
    | _ =>
      recordCandidateFailure ctx "native_setup" "control-lcb"
        s!"FAIL {ctx.leg} seed {seed} index {index} [native_setup]: off-by-one control needs an exit"
      return 1
  unless ← emitArtifacts ctx g.program mutation planted do return 1
  IO.println s!"seed {seed} index {index}: {outcomeName outcome} {BirGen.Production.name g.production}"
  return 0

/-- Keep candidate context active across initialization and the gaps between backend stages. -/
def emitBirCandidate (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target)
    (mutation : Mutation) (planted : PlantedFailure := .none) : IO UInt32 :=
  candidateStage ⟨"bir", seed, index, dir, target⟩ "generation"
    (fun _ => emitBirCandidateImpl seed index dir target mutation planted)

abbrev Counts := List (String × Nat)

def bump (key : String) : Counts → Counts
  | [] => [(key, 1)]
  | (k, n) :: rest => if k = key then (k, n + 1) :: rest else (k, n) :: bump key rest

def addConstructs (counts : Counts) (xs : List String) : Counts :=
  xs.foldl (fun acc x => bump x acc) counts

def countsLines (kind : String) (counts : Counts) : String :=
  String.join <| counts.map fun (name, n) => s!"{kind}\t{name}\t{n}\n"

/-- Admission is independent of the backend: the generator constructs the supported fragment,
and `wf` checks it. Every admitted candidate must emit; refusal is a regression, never rejection. -/
def genBir (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target)
    (plantIndex : Option Nat := none) (planted : PlantedFailure := .emission) : IO UInt32 := do
  IO.FS.createDirAll dir
  let mut programs := ""
  let mut rejects := ""
  let mut constructs : Counts := []
  let mut accepted := 0
  let mut rejected := 0
  let mut exits := 0
  let mut traps := 0
  for index in List.range n do
    let ctx : CandidateCtx := ⟨"bir", seed, index, dir / s!"p{index}", target⟩
    let (g, wellFormed, outcome) ← candidateStage ctx "generation" fun _ => do
      let g := BirGen.generate seed index
      return (g, wf g.program, run g.program 1000)
    if !wellFormed then
      rejected := rejected + 1
      rejects := rejects ++ s!"reject\t{index}\tnot well formed\n"
    else
      let child := dir / s!"p{index}"
      let candPlanted := if plantIndex == some index then planted else .none
      unless (← emitBirCandidate seed index child target .none candPlanted) == 0 do return 1
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
def writeSpecOutcome (ctx : CandidateCtx) (p : Program) (o : ConcreteSpec.Outcome) : IO Bool := do
  match o with
  | .exit c => writeObservation ctx.dir (toString c) ""; return true
  | .trap k => match p.sites.filter (fun e => e.kind = k) with
    | [e] => writeObservation ctx.dir "134" (Runtime.trapLine e ++ "\n"); return true
    | [] =>
      recordCandidateFailure ctx "native_setup" "spec_trap_site"
        s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: spec trap {repr k} has no lowered site"
      return false
    | _ =>
      recordCandidateFailure ctx "native_setup" "spec_trap_site"
        s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: spec trap {repr k} has multiple lowered sites and no spec site"
      return false
  | _ =>
    recordCandidateFailure ctx "native_setup" "spec_outcome"
      s!"FAIL {ctx.leg} seed {ctx.seed} index {ctx.index} [native_setup]: spec outcome {repr o} has no observation"
    return false

def specOutcomeName : ConcreteSpec.Outcome → String
  | .exit _ => "exit" | .trap _ => "trap" | .stuck _ => "stuck" | .outOfFuel => "out_of_fuel"

def compileSpecCandidateImpl (ctx : CandidateCtx) (avail : Bool)
    (compiler : ConcreteSpec.Program → Except String Program)
    (g : SpecGen.Generated) (core : ConcreteSpec.Program)
    (planted : PlantedFailure := .none) : IO (Except UInt32 (ConcreteSpec.Outcome × Bool)) := do
  IO.FS.createDirAll ctx.dir
  IO.FS.writeFile (ctx.dir / "program.lean.txt") (reprStr g.program ++ "\n")
  IO.FS.writeFile (ctx.dir / "constructs.txt") (String.intercalate "," g.constructs ++ "\n")
  writeSpecRegressionMetadata ctx.dir ctx.seed ctx.index g.program 0
  if g.rejection.isSome then
    recordCandidateFailure ctx "lowering" "spec_check"
      s!"FAIL spec seed {ctx.seed} index {ctx.index} [lowering]: checker accepted predicted rejection {repr g.rejection}"
    return .error 1
  let outcome := specOutcome g.program
  match outcome with
  | .exit c =>
    unless c < 256 do
      recordCandidateFailure ctx "lowering" "spec_oracle"
        s!"FAIL spec seed {ctx.seed} index {ctx.index} [lowering]: invalid spec exit {c}"
      return .error 1
    IO.FS.writeFile (ctx.dir / "oracle.txt") s!"exit {c}\n"
    writeObservation ctx.dir (toString c) ""
    writeSpecRegressionMetadata ctx.dir ctx.seed ctx.index g.program c
    match compiler core with
    | .error msg =>
      if avail then
        recordCandidateFailure ctx "lowering" "compileCore"
          s!"FAIL spec leg capability contract: loweringAvailable = true but compileCore refused supported accepted candidate (seed {ctx.seed} index {ctx.index}): {msg}"
        return .error 1
      if msg != unwiredMessage then
        recordCandidateFailure ctx "lowering" "compileCore"
          s!"FAIL spec seed {ctx.seed} index {ctx.index} [lowering]: compileCore: {msg}"
        return .error 1
      return .ok (outcome, false)
    | .ok bir =>
      IO.FS.writeFile (ctx.dir / "bir.txt") (reprStr bir ++ "\n")
      if !avail then
        recordCandidateFailure ctx "lowering" "compileCore"
          s!"FAIL spec leg capability contract: compileCore compiled seed {ctx.seed} index {ctx.index} while loweringAvailable = false"
        return .error 1
      unless ← emitArtifacts ctx bir .none planted do return .error 1
      unless ← writeSpecOutcome ctx bir outcome do return .error 1
      unless planted == .nativeSetup do
        IO.FS.writeFile (ctx.dir / "native-ready") ""
      return .ok (outcome, true)
  | .trap _ =>
    recordCandidateFailure ctx "lowering" "spec_oracle"
      s!"FAIL spec seed {ctx.seed} index {ctx.index} [lowering]: accepted slice-0 program trapped"
    return .error 1
  | _ =>
    recordCandidateFailure ctx "lowering" "spec_oracle"
      s!"FAIL spec seed {ctx.seed} index {ctx.index} [lowering]: spec oracle gives {repr outcome}"
    return .error 1

def compileSpecCandidate (ctx : CandidateCtx) (avail : Bool)
    (compiler : ConcreteSpec.Program → Except String Program)
    (g : SpecGen.Generated) (core : ConcreteSpec.Program)
    (planted : PlantedFailure := .none) : IO (Except UInt32 (ConcreteSpec.Outcome × Bool)) :=
  candidateStage ctx "lowering" (fun _ => compileSpecCandidateImpl ctx avail compiler g core planted)

/-- Generate accepted slice-0 Core programs, run the spec oracle, and enforce the lowering
capability contract (X3):
- When `avail = false`, `compiler` must return `.error unwiredMessage` on every accepted candidate
  (`compiled = 0`).
- When `avail = true`, `compiler` must succeed on every supported accepted candidate
  (`compiled = supportedAccepted = accepted`). Partial lowering is a fatal failure. -/
def genSpecWith (avail : Bool) (compiler : ConcreteSpec.Program → Except String Program)
    (seed : String) (n : Nat) (dir : System.FilePath) (target : LCB.Target)
    (generate : String → Nat → SpecGen.Generated := SpecGen.generate)
    (planted : PlantedFailure := .none) : IO UInt32 := do
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
    let ctx : CandidateCtx := ⟨"spec", seed, index, dir / s!"p{index}", target⟩
    let (g, core, acceptedByChecker) ← candidateStage ctx "generation" fun _ => do
      let g := generate seed index
      let core := ConcreteSpec.Slice0.toCore g.program
      return (g, core, ConcreteSpec.check core)
    if !acceptedByChecker then
      rejected := rejected + 1
      let reason := g.rejection.getD "spec_check"
      rejections := bump reason rejections
      rejects := rejects ++ s!"reject\t{index}\t{reason}\n"
    else
      let child := dir / s!"p{index}"
      let ctx : CandidateCtx := ⟨"spec", seed, index, child, target⟩
      match ← compileSpecCandidate ctx avail compiler g core planted with
      | .error rc => return rc
      | .ok (_, didCompile) =>
        accepted := accepted + 1
        supportedAccepted := supportedAccepted + 1
        exits := exits + 1
        constructs := addConstructs constructs g.constructs
        if didCompile then
          compiled := compiled + 1
          IO.println s!"spec compiled candidate: index {index}, compiled {compiled}"
        programs := programs ++
          s!"program\t{index}\texit\t{if didCompile then "true" else "false"}\n"
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

def partialLoweringCorpus : String → Nat → SpecGen.Generated := fun _ index =>
  if index < 2 then
    ⟨{ body := [], ret := .lit 42 }, ["return_literal"], none⟩
  else
    ⟨ConcreteSpec.Slice0.answer, ["let_literal", "return_variable"], none⟩

def partialCompiler : ConcreteSpec.Program → Except String Program := fun core =>
  if core.body.isEmpty then .ok Concrete.BIR.Examples.answer
  else .error "planted partial-lowering capability refusal"

/-- Fixed X3 corpus: two supported literal returns compile before a supported binding
program is refused. This exercises the production generation/capability path independently of seed. -/
def controlPartialLowering (seed : String) (_n : Nat) (dir : System.FilePath) (target : LCB.Target) :
    IO UInt32 :=
  genSpecWith true partialCompiler seed 3 dir target partialLoweringCorpus

/-- Replay one accepted spec candidate through the shared spec candidate compiler. While
`compileCore` is unwired this writes the generated oracle inputs and refuses to claim a native
comparison. -/
def replaySpecWithImpl (avail : Bool) (compiler : ConcreteSpec.Program → Except String Program)
    (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target)
    (generate : String → Nat → SpecGen.Generated := SpecGen.generate)
    (planted : PlantedFailure := .none) : IO UInt32 := do
  let g := generate seed index
  let core := ConcreteSpec.Slice0.toCore g.program
  if !ConcreteSpec.check core then
    IO.eprintln s!"seed {seed} index {index}: rejected spec candidate"
    return 4
  let ctx : CandidateCtx := ⟨"spec", seed, index, dir, target⟩
  match ← compileSpecCandidate ctx avail compiler g core planted with
  | .error rc => return rc
  | .ok (_, true) => return 0
  | .ok (_, false) =>
    IO.eprintln s!"spec leg: 0 programs compiled ({unwiredMessage})"
    return 4

def replaySpecWith (avail : Bool) (compiler : ConcreteSpec.Program → Except String Program)
    (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target)
    (generate : String → Nat → SpecGen.Generated := SpecGen.generate)
    (planted : PlantedFailure := .none) : IO UInt32 :=
  candidateStage ⟨"spec", seed, index, dir, target⟩ "generation"
    (fun _ => replaySpecWithImpl avail compiler seed index dir target generate planted)

def replaySpec (seed : String) (index : Nat) (dir : System.FilePath) (target : LCB.Target) :
    IO UInt32 :=
  replaySpecWith loweringAvailable compileCore seed index dir target

/-- Stand-in compiled output for the spec-oracle control: Slice0.answer and BIR answer both exit 42,
but the judge receives a planted expected 43. -/
def controlSpecOracle (dir : System.FilePath) (target : LCB.Target) : IO UInt32 := do
  let q := ConcreteSpec.Slice0.answer
  let p := Concrete.BIR.Examples.answer
  let ctx : CandidateCtx := ⟨"spec", "control-spec-oracle", 0, dir, target⟩
  let ctxExpected : CandidateCtx := ⟨"spec", "control-spec-oracle", 0, dir / "spec-expected", target⟩
  unless ← emitArtifacts ctx p .none do return 1
  unless ← writeSpecOutcome ctxExpected p (specOutcome q) do return 1
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
  | ["control-selective-refusal", seed, n, dir, target,
      "--control", "SPEC_DIFF_PLANTED_CONTROL"] => match parseNat n with
    | some n => withTarget target (fun t => genBir seed n dir t (some (n - 1)) .emission)
    | none => return 2
  | ["control-plant-bir", kind, seed, n, dir, target,
      "--control", "SPEC_DIFF_PLANTED_CONTROL"] => match plantedFailureOfName kind, parseNat n with
    | some planted, some n =>
      withTarget target (fun t => genBir seed n dir t (some (n - 1)) planted)
    | _, _ => return 2
  | ["replay", seed, index, dir, target] => match parseNat index with
    | some index => withTarget target (fun t => emitBirCandidate seed index dir t .none)
    | none => return 2
  | ["replay", seed, index, dir, target,
      "--plant-failure", kind, "--control", "SPEC_DIFF_PLANTED_CONTROL"] =>
      match plantedFailureOfName kind, parseNat index with
    | some planted, some index =>
      withTarget target (fun t => emitBirCandidate seed index dir t .none planted)
    | _, _ => return 2
  | ["replay", seed, index, dir, target,
      "--mutate", mutation, "--control", "SPEC_DIFF_PLANTED_CONTROL"] =>
      match mutationOfName mutation, parseNat index with
    | some mutation, some index =>
      withTarget target (fun t => emitBirCandidate seed index dir t mutation .none)
    | _, _ => return 2
  | ["replay-spec", seed, index, dir, target] => match parseNat index with
    | some index => withTarget target (replaySpec seed index dir)
    | none => return 2
  | ["replay-spec", seed, index, dir, target,
      "--plant-failure", "lowering", "--control", "SPEC_DIFF_PLANTED_CONTROL"] =>
      match parseNat index with
    | some index =>
      withTarget target (fun t => replaySpecWith true partialCompiler seed index dir t
        partialLoweringCorpus .lowering)
    | none => return 2
  | ["replay-spec", seed, index, dir, target,
      "--plant-failure", "native-setup", "--control", "SPEC_DIFF_PLANTED_CONTROL"] =>
      match parseNat index with
    | some index =>
      withTarget target (fun t => replaySpecWith true
        (fun _ => .ok Concrete.BIR.Examples.answer) seed index dir t
        partialLoweringCorpus .nativeSetup)
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
    IO.eprintln "usage: spec_diff.lean gen-spec SEED N DIR | gen-bir SEED N DIR TARGET | (replay | replay-spec) SEED INDEX DIR TARGET | control-emit --mutate NAME SEED INDEX DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL | control-plant-bir KIND SEED N DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL | control-spec-oracle DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL | control-partial-lowering SEED N DIR TARGET --control SPEC_DIFF_PLANTED_CONTROL"
    return 2

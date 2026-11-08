import Concrete.Backend.LCB
import Concrete.SpecDiffRandom

/-!
Seeded BIR generation over the shapes accepted by the current LCB adapter. Each slice brief must
extend `weightedProductions` here with its new constructs; this is the one production table for
this leg. Candidates are checked by `BIR.wf` and `LCB.emit` by the driver, never assumed accepted.
-/

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

def weightedProductions : List (Nat × Production) :=
  [(4, .checked), (4, .wrapped), (2, .widening), (2, .twoSites), (3, .chain)]

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

/-- Checked arithmetic followed by truncation, an unconditional edge, and a second checked site. -/
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
  let production := choice.getD .checked
  match production with
  | .checked =>
    let (t, r₂) := randomTy r₁
    let ((op, a, b), _) := binaryOperands t r₂
    ⟨Examples.checkedOp op t a b, production,
      [Production.name production, "binOvf", "condbr", "trap", opName op, tyName t]⟩
  | .wrapped =>
    let (t, r₂) := randomTy r₁
    let ((op, a, b), _) := binaryOperands t r₂
    ⟨Examples.wrapped op t a b, production,
      [Production.name production, "binOvf", "trunc", "condbr", opName op, tyName t]⟩
  | .widening =>
    let (signed, r₂) := below 2 r₁
    let fr : IntTy := if signed = 0 then .u8 else .i8
    let to : IntTy := if signed = 0 then .u16 else .i16
    let (a, r₃) := randomValue fr r₂
    let ((op, _, b), _) := binaryOperands to r₃
    ⟨Examples.observe [.trunc fr to 0 (.imm fr a)] op to (.reg 0) (.imm to b), production,
      [Production.name production, "binOvf", "trunc", "condbr", opName op, tyName to]⟩
  | .twoSites =>
    let (a, r₂) := randomValue .u8 r₁
    let (b, r₃) := randomValue .u8 r₂
    let (c, _) := randomValue .u8 r₃
    ⟨Examples.twoSites a b c, production,
      [Production.name production, "binOvf", "condbr", "trap", "multi_block", "multi_site"]⟩
  | .chain =>
    let (t, r₂) := randomTy r₁
    let ((op, a, b), r₃) := binaryOperands t r₂
    let (c, _) := randomValue .u8 r₃
    ⟨chained op t a b c, production,
      [Production.name production, "binOvf", "trunc", "br", "condbr", "trap", "multi_block",
       "multi_site", opName op, tyName t]⟩

end ConcreteSpecDiff.BirGen

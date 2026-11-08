import Concrete.Backend.Runtime
import Std.Data.HashMap.Lemmas

/-!
# BIR → LLVM text (UNVERIFIED)

PROVISIONAL (slice 0). The printer is labelled unverified: what its text means is premise
`P_print` (`Concrete/README.md`), and LLVM itself is inside `P_toolchain[llvm]`. It prints `main`'s
body as `define i32 @concrete_body()` returning the result word; the runtime entry
(`Runtime.llvm`) is linked with it.

* an integer of type `t` is an `i{bits}` holding its two's-complement bit pattern; a flag is `i1`;
* `binOvf` is `@llvm.{s,u}{add,sub,mul}.with.overflow.i{bits}`;
* `trunc` is `trunc`, `sext`/`zext` by the source's signedness, or a copy at equal width;
* block parameters become `phi` nodes over the predecessors' arguments;
* `ret a` returns `zext a`; `trap s` returns `256 + s`.
-/

namespace Concrete.Backend.LLVM
open Concrete.BIR
open ConcreteSpec (IntTy Trap)
open Concrete.Backend.Runtime (indexed)

def intTy (t : IntTy) : String := s!"i{t.bits}"

def ty : Ty → String
  | .int t => intTy t
  | .bool => "i1"

/-- The bit pattern of `v` in `t`, as an unsigned decimal. -/
def pattern (t : IntTy) (v : Int) : Nat := (v % ((2 : Int) ^ t.bits)).toNat

def operand : Operand → String
  | .reg r => s!"%r{r}"
  | .imm t v => toString (pattern t v)

def opName : BinOp → String
  | .add => "add"
  | .sub => "sub"
  | .mul => "mul"

def intrinsic (op : BinOp) (t : IntTy) : String :=
  s!"llvm.{if t.signed then "s" else "u"}{opName op}.with.overflow.{intTy t}"

def inst : Inst → String
  | .bin op t d a b => s!"  %r{d} = {opName op} {intTy t} {operand a}, {operand b}\n"
  | .binOvf op t d f a b =>
    let pair := s!"\{{intTy t}, i1}"
    s!"  %o{d} = call {pair} @{intrinsic op t}({intTy t} {operand a}, {intTy t} {operand b})\n" ++
    s!"  %r{d} = extractvalue {pair} %o{d}, 0\n" ++
    s!"  %r{f} = extractvalue {pair} %o{d}, 1\n"
  | .trunc fr to d a =>
    let conv := if to.bits < fr.bits then "trunc" else if fr.signed then "sext" else "zext"
    if fr.bits = to.bits then s!"  %r{d} = add {intTy to} {operand a}, 0\n"
    else s!"  %r{d} = {conv} {intTy fr} {operand a} to {intTy to}\n"

/-- The edges into each block: (target, predecessor, arguments). -/
def edges (p : Program) : List (Nat × Nat × List Operand) :=
  (indexed p.blocks).flatMap fun (j, b) =>
    match b.term with
    | .br t args => [(t, j, args)]
    | .condbr _ t ta e ea => [(t, j, ta), (e, j, ea)]
    | _ => []

/-- Shared phi renderer. Each edge payload is converted once by the caller. -/
def phiText (incoming : List (Nat × Array Operand)) (b : Block) : String :=
  String.join ((indexed b.params).map fun (k, (r, pty)) =>
    let arms := incoming.map fun (pred, args) =>
      s!"[ {operand (args[k]?.getD (.imm .u8 0))}, %b{pred} ]"
    s!"  %r{r} = phi {ty pty} {", ".intercalate arms}\n")

def phis (p : Program) (j : Nat) (b : Block) : String :=
  phiText (((edges p).filter (·.1 == j)).map fun e => (e.2.1, e.2.2.toArray)) b

/-- The shared renderer preserves the original list-indexing reference, including missing arms. -/
theorem phis_eq_reference (p : Program) (j : Nat) (b : Block) :
    phis p j b =
      String.join ((indexed b.params).map fun (k, (r, pty)) =>
        let arms := ((edges p).filter (·.1 == j)).map fun (_, pred, args) =>
          s!"[ {operand (args.getD k (.imm .u8 0))}, %b{pred} ]"
        s!"  %r{r} = phi {ty pty} {", ".intercalate arms}\n") := by
  simp [phis, phiText, List.map_map, Function.comp_def, List.getElem?_toArray, List.getD_eq_getElem?_getD]

def term (j : Nat) : Term → String
  | .ret a => s!"  %ret{j} = zext i8 {operand a} to i32\n  ret i32 %ret{j}\n"
  | .trap s => s!"  ret i32 {256 + s}\n"
  | .br t _ => s!"  br label %b{t}\n"
  | .condbr c t _ e _ => s!"  br i1 %r{c}, label %b{t}, label %b{e}\n"

/-- The intrinsics the body uses, each declared once. -/
def declarations (p : Program) : String :=
  let used := (p.blocks.flatMap (·.insts)).filterMap fun
    | .binOvf op t _ _ _ _ => some (op, t)
    | _ => none
  let distinct := used.eraseDups
  String.join (distinct.map fun (op, t) =>
    s!"declare \{{intTy t}, i1} @{intrinsic op t}({intTy t}, {intTy t})\n")

/-- `main`'s body as LLVM text. -/
def printSpec (p : Program) : String :=
  declarations p ++ "\ndefine i32 @concrete_body() {\n" ++
  String.join ((indexed p.blocks).map fun (j, b) =>
    s!"b{j}:\n" ++ phis p j b ++ String.join (b.insts.map inst) ++ term j b.term) ++
  "}\n"

/-- Finite predecessor maps allocate per edge, independent of target magnitude. -/
abbrev EdgeTable := Std.HashMap Nat (List (Nat × Array Operand))

def bucket (bs : EdgeTable) (j : Nat) : List (Nat × Array Operand) := bs[j]?.getD []

def addEdges : List (Nat × Nat × List Operand) → EdgeTable → EdgeTable
  | [], bs => bs
  | e :: es, bs => addEdges es (bs.insert e.1 ((e.2.1, e.2.2.toArray) :: bucket bs e.1))

def edgeBuckets (p : Program) : EdgeTable :=
  (addEdges (edges p) ∅).map (fun _ xs => xs.reverse)

theorem bucket_addEdges : ∀ (es : List (Nat × Nat × List Operand))
    (bs : EdgeTable) (j : Nat),
    bucket (addEdges es bs) j =
      ((es.filter (·.1 == j)).map (fun e => (e.2.1, e.2.2.toArray))).reverse ++ bucket bs j
  | [], bs, j => by simp [addEdges]
  | e :: es, bs, j => by
    rw [addEdges, bucket_addEdges]
    by_cases h : e.1 = j
    · subst j
      simp [bucket, List.reverse_cons, List.append_assoc]
    · simp [bucket, Std.HashMap.getElem?_insert, h]

theorem bucket_edgeBuckets (p : Program) (j : Nat) :
    bucket (edgeBuckets p) j =
      ((edges p).filter (·.1 == j)).map (fun e => (e.2.1, e.2.2.toArray)) := by
  have hm : bucket (edgeBuckets p) j = (bucket (addEdges (edges p) ∅) j).reverse := by
    simp only [edgeBuckets, bucket, Std.HashMap.getElem?_map]
    cases (addEdges (edges p) ∅)[j]? <;> rfl
  rw [hm, bucket_addEdges]
  simp [bucket]

/-- Phi rendering from the prepared predecessor table, sharing the reference renderer. -/
def phisFast (bs : EdgeTable) (j : Nat) (b : Block) : String := phiText (bucket bs j) b

theorem phisFast_eq (p : Program) (j : Nat) (b : Block) :
    phisFast (edgeBuckets p) j b = phis p j b := by
  simp only [phisFast, phis, bucket_edgeBuckets]

/-- The body printer with one predecessor-table construction per program. -/
def printFast (p : Program) : String :=
  let bs := edgeBuckets p
  declarations p ++ "\ndefine i32 @concrete_body() {\n" ++
  String.join ((indexed p.blocks).map fun (j, b) =>
    s!"b{j}:\n" ++ phisFast bs j b ++ String.join (b.insts.map inst) ++ term j b.term) ++
  "}\n"

theorem printFast_eq (p : Program) : printFast p = printSpec p := by
  simp only [printFast, printSpec]
  congr 2
  congr 1
  apply List.map_congr_left
  intro (j, b) hj
  rw [phisFast_eq]

def print (p : Program) : String := printFast p

theorem print_eq_spec (p : Program) : print p = printSpec p := printFast_eq p

/-- The whole LLVM module: the body and the runtime entry. -/
def module (p : Program) : String := print p ++ "\n" ++ Runtime.llvm p.sites

theorem module_eq_reference (p : Program) :
    module p = printSpec p ++ "\n" ++ Runtime.llvm p.sites := by
  rw [module, print, printFast_eq]

end Concrete.Backend.LLVM

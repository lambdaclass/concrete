import Concrete.Backend.Runtime

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

def phis (p : Program) (j : Nat) (b : Block) : String :=
  let incoming := (edges p).filter (·.1 == j)
  String.join ((indexed b.params).map fun (k, (r, pty)) =>
    let arms := incoming.map fun (_, pred, args) =>
      s!"[ {operand (args.getD k (.imm .u8 0))}, %b{pred} ]"
    s!"  %r{r} = phi {ty pty} {", ".intercalate arms}\n")

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
def print (p : Program) : String :=
  declarations p ++ s!"\ndefine i32 @concrete_body() \{\n" ++
  String.join ((indexed p.blocks).map fun (j, b) =>
    s!"b{j}:\n" ++ phis p j b ++ String.join (b.insts.map inst) ++ term j b.term) ++
  "}\n"

/-- The whole LLVM module: the body and the runtime entry. -/
def module (p : Program) : String := print p ++ "\n" ++ Runtime.llvm p.sites

end Concrete.Backend.LLVM

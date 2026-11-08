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
  declarations p ++ "\ndefine i32 @concrete_body() {\n" ++
  String.join ((indexed p.blocks).map fun (j, b) =>
    s!"b{j}:\n" ++ phis p j b ++ String.join (b.insts.map inst) ++ term j b.term) ++
  "}\n"

/-- One more than the largest edge target, so even malformed targets have a bucket. -/
def edgeBound : List (Nat × Nat × List Operand) → Nat
  | [] => 0
  | e :: es => max (e.1 + 1) (edgeBound es)

/-- Add edges to reverse-order buckets. Reversing a bucket restores predecessor and conditional-arm
order without the quadratic repeated append. -/
def addEdges (es : List (Nat × Nat × List Operand))
    (bs : Array (List (Nat × Nat × List Operand))) : Array (List (Nat × Nat × List Operand)) :=
  match es with
  | [] => bs
  | e :: es =>
    let bs := if h : e.1 < bs.size then bs.setIfInBounds e.1 (e :: bs[e.1]) else bs
    addEdges es bs

/-- The predecessor table, computed once per program. -/
def edgeBuckets (p : Program) : Array (List (Nat × Nat × List Operand)) :=
  let es := edges p
  addEdges es (Array.replicate (edgeBound es) [])

def bucket (bs : Array (List (Nat × Nat × List Operand))) (j : Nat) : List (Nat × Nat × List Operand) :=
  (bs[j]?).getD []

theorem edgeBound_lt {e : Nat × Nat × List Operand} {es : List (Nat × Nat × List Operand)}
    (h : e ∈ es) : e.1 < edgeBound es := by
  induction es with
  | nil => simp at h
  | cons a es ih =>
    simp only [List.mem_cons] at h
    rcases h with rfl | h
    · simp only [edgeBound]; omega
    · simp only [edgeBound]; have := ih h; omega

theorem size_addEdges : ∀ (es : List (Nat × Nat × List Operand))
    (bs : Array (List (Nat × Nat × List Operand))), (addEdges es bs).size = bs.size
  | [], _ => rfl
  | e :: es, bs => by
    simp only [addEdges]
    rw [size_addEdges]
    split <;> simp

theorem bucket_addEdges : ∀ (es : List (Nat × Nat × List Operand))
    (bs : Array (List (Nat × Nat × List Operand))) (j : Nat), j < bs.size →
    bucket (addEdges es bs) j = (es.filter (·.1 == j)).reverse ++ bucket bs j
  | [], bs, j, _ => by simp [addEdges, bucket]
  | e :: es, bs, j, hj => by
    simp only [addEdges]
    by_cases hlt : e.1 < bs.size
    · simp only [hlt, ↓reduceDIte]
      rw [bucket_addEdges es _ j (by simpa using hj)]
      by_cases h : e.1 = j
      · subst h
        have hget : bs[e.1]?.getD [] = bs[e.1] := by
          rw [Array.getElem?_eq_getElem hlt]; rfl
        simp [bucket, Array.getElem?_setIfInBounds_self_of_lt (by simpa using hlt), hget,
          List.reverse_cons, List.append_assoc]
      · have hne : j ≠ e.1 := Ne.symm h
        simp [bucket, h]
    · simp only [hlt, ↓reduceDIte]
      rw [bucket_addEdges es bs j hj]
      have hne : e.1 ≠ j := by omega
      simp [hne]

theorem bucket_edgeBuckets (p : Program) (j : Nat) :
    (bucket (edgeBuckets p) j).reverse = (edges p).filter (·.1 == j) := by
  by_cases hj : j < (Array.replicate (edgeBound (edges p)) ([] : List (Nat × Nat × List Operand))).size
  · have h := bucket_addEdges (edges p)
      (Array.replicate (edgeBound (edges p)) ([] : List (Nat × Nat × List Operand))) j hj
    have hinit : bucket (Array.replicate (edgeBound (edges p))
        ([] : List (Nat × Nat × List Operand))) j = [] := by
      simp only [bucket]
      rw [Array.getElem?_eq_getElem hj]
      simp
    change (bucket (addEdges (edges p)
      (Array.replicate (edgeBound (edges p)) [])) j).reverse = _
    rw [h, hinit]
    simp
  · have hnone : (edgeBuckets p)[j]? = none := by
      apply Array.getElem?_eq_none
      rw [edgeBuckets, size_addEdges]
      simpa using hj
    have hnoedge : (edges p).filter (·.1 == j) = [] := by
      apply List.filter_eq_nil_iff.mpr
      intro e he
      have hlt := edgeBound_lt he
      simp only [beq_iff_eq]
      intro h
      subst h
      exact hj (by simpa using hlt)
    simp [bucket, hnone, hnoedge]

/-- Phi rendering from the prepared predecessor table. -/
def phisFast (bs : Array (List (Nat × Nat × List Operand))) (j : Nat) (b : Block) : String :=
  let incoming := (bucket bs j).reverse
  String.join ((indexed b.params).map fun (k, (r, pty)) =>
    let arms := incoming.map fun (_, pred, args) =>
      s!"[ {operand (args.getD k (.imm .u8 0))}, %b{pred} ]"
    s!"  %r{r} = phi {ty pty} {", ".intercalate arms}\n")

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

theorem printFast_eq (p : Program) : printFast p = print p := by
  simp only [printFast, print]
  congr 2
  congr 1
  apply List.map_congr_left
  intro (j, b) hj
  rw [phisFast_eq]

/-- The whole LLVM module: the body and the runtime entry. -/
def module (p : Program) : String := printFast p ++ "\n" ++ Runtime.llvm p.sites

theorem module_eq_reference (p : Program) :
    module p = print p ++ "\n" ++ Runtime.llvm p.sites := by
  rw [module, printFast_eq]

end Concrete.Backend.LLVM

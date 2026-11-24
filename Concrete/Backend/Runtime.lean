import Concrete.BIR
import Concrete.Backend.LLVMIR

/-!
# The runtime entry (both backends)

PROVISIONAL (slice 0). Every backend emits `main`'s body as `concrete_body`, returning the result
word (`Concrete.BIR.encode`). The runtime entry is fixed: it calls the body and then

* exits with the word if it is below 256;
* otherwise writes the trap line of site `word - 256` to descriptor 2 (looping on partial writes,
  ignoring write errors) and exits 134 (`ConcreteSpec.Observe.Observation.status`);
* on a word that encodes nothing (no well-formed body returns one, `emit_encodes`), writes
  `invalidLine` and aborts.

Its correctness is premise `P_rt`, and the site it reports is premise `P_trap_site`
(`Concrete/README.md`). This file emits it as LLVM text and as QBE text for LCB.

The trap line is the owner's decided form (2026-11-06):
`concrete: trap: <kind> at <path>:<line>:<col> (site <id>)`, the path relative to the project
root with forward slashes, line and column 1-based. That the executable writes exactly this line
for the site it trapped at is premise `P_trap_site`.
-/

namespace Concrete.Backend.Runtime
open Concrete.BIR
open Concrete.Backend.LLVM
open ConcreteSpec (IntTy Trap)
open ConcreteSpec.Core (toBits)

/-- The trap's name in the trap line. -/
def trapName : Trap → String
  | .arithmeticOverflow => "overflow"

/-- The trap line for one site, without the final newline. -/
def trapLine (e : SiteEntry) : String :=
  s!"concrete: trap: {trapName e.kind} at {e.loc.path}:{decimal e.loc.line}:{decimal e.loc.col} (site {decimal e.site.id})"

/-- What the runtime writes before aborting on a result word that encodes nothing. -/
def invalidLine : String := "concrete: runtime: invalid result word"

/-- The decided form, checked against a literal (not against `trapLine` itself). -/
example : trapLine ⟨.arithmeticOverflow, ⟨9⟩, ⟨40, 47⟩, ⟨"src/main.con", 4, 12⟩⟩ =
    "concrete: trap: overflow at src/main.con:4:12 (site 9)" := by decide

/-- Pair every element with its index. -/
def indexedFrom {α : Type} (n : Nat) : List α → List (Nat × α)
  | [] => []
  | a :: l => (n, a) :: indexedFrom (n + 1) l

def indexed {α : Type} (l : List α) : List (Nat × α) := indexedFrom 0 l

theorem getElem?_indexedFrom {α : Type} (l : List α) (n k : Nat) :
    (indexedFrom n l)[k]? = l[k]?.map (n + k, ·) := by
  induction l generalizing n k with
  | nil => simp [indexedFrom]
  | cons a l ih =>
    cases k with
    | zero => simp [indexedFrom]
    | succ k =>
      simp only [indexedFrom, List.getElem?_cons_succ, ih]
      congr 1; funext x; congr 1; omega

theorem getElem?_indexed {α : Type} (l : List α) (k : Nat) :
    (indexed l)[k]? = l[k]?.map (k, ·) := by
  simpa [indexed] using getElem?_indexedFrom l 0 k

theorem mem_indexedFrom {α : Type} {l : List α} {n k : Nat} {x : α} (h : l[k]? = some x) :
    (n + k, x) ∈ indexedFrom n l := by
  induction l generalizing n k with
  | nil => simp at h
  | cons a l ih =>
    cases k with
    | zero => simp at h; subst h; simp [indexedFrom]
    | succ k =>
      simp only [List.getElem?_cons_succ] at h
      have := ih (n := n + 1) h
      simp only [indexedFrom, List.mem_cons]
      exact Or.inr (by rwa [show n + (k + 1) = n + 1 + k by omega])

theorem mem_indexed {α : Type} {l : List α} {k : Nat} {x : α} (h : l[k]? = some x) :
    (k, x) ∈ indexed l := by
  simpa [indexed] using mem_indexedFrom (n := 0) h

private def siteBytes (e : SiteEntry) : ByteArray := (trapLine e ++ "\n").toUTF8

private def invalidBytes : ByteArray := (invalidLine ++ "\n").toUTF8

/-- The fixed runtime entry and its byte globals as LLVM assembly items. -/
def llvmItems (tbl : SiteTable) : List Item :=
  let sites := indexed tbl
  let i32 := TypeCode.integer .u32
  let i64 := TypeCode.integer .u64
  let byte := TypeCode.integer .u8
  let count (n : Nat) : Argument := .immediate (.natural (toBits .u64 (Int.ofNat n)))
  let zero := Argument.immediate (.natural 0)
  let threshold := Argument.immediate (.natural 256)
  let writeAll (name : Global) (data : ByteArray) :=
    Item.instruction (.call none .void .writeAll [(.ptr, .global name), (i64, count data.size)])
  let abort := Item.instruction (.call none .void .abort [])
  let trapExit := Item.instruction (.call none .void .exit [(i32, .immediate (.natural 134))])
  let globals := sites.map fun (i, e) => Item.constant (.site i) (siteBytes e)
  let cases := sites.map fun (i, _) => (Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite i)
  let traps := sites.flatMap fun (i, e) =>
    [Item.label (.trapSite i), writeAll (.site i) (siteBytes e), trapExit, .terminator .unreachable]
  globals ++
  [.constant .bad invalidBytes,
   .declare i64 .write [i32, .ptr, i64],
   .declare .void .exit [i32],
   .declare .void .abort [],
   .blank,
   .define true .void .writeAll [(.ptr, .p), (i64, .n)],
   .label .entry,
   .terminator (.branch .loop),
   .label .loop,
   .instruction (.phi .pp .ptr [(.local .p, .entry), (.local .np, .more)]),
   .instruction (.phi .nn i64 [(.local .n, .entry), (.local .rest, .more)]),
   .instruction (.compare .done .eq i64 (.local .nn) zero),
   .terminator (.cond .done .out .wr),
   .label .wr,
   .instruction (.call (some .k) i64 .write
     [(i32, .immediate (.natural 2)), (.ptr, .local .pp), (i64, .local .nn)]),
   .instruction (.compare .bad .sle i64 (.local .k) zero),
   .terminator (.cond .bad .out .more),
   .label .more,
   .instruction (.gep .np byte (.ptr, .local .pp) (i64, .local .k)),
   .instruction (.bin .rest .sub i64 (.local .nn) (.local .k)),
   .terminator (.branch .loop),
   .label .out,
   .terminator .retVoid,
   .endFunction,
   .blank,
   .define false i32 .main [],
   .label .entry,
   .instruction (.call (some .w) i32 .body []),
   .instruction (.compare .ok .ult i32 (.local .w) threshold),
   .terminator (.cond .ok .exit .trap),
   .label .exit,
   .terminator (.ret (i32, .local .w)),
   .label .trap,
   .instruction (.bin .i .sub i32 (.local .w) threshold),
   .terminator (.switch .u32 .i .bad cases)] ++
  traps ++
  [.label .bad,
   writeAll .bad invalidBytes,
   abort,
   .terminator .unreachable,
   .endFunction]

/-- Numeric part of the runtime contract: lengths are UTF-8 byte counts, switch
keys encode consecutive site indices at i32, and the trap path writes to stderr and exits 134.
Each assembly item occupies one entry, including items with no integer fields. -/
def expectedInventory (tbl : SiteTable) : List (List IntegerField) :=
  let size (e : SiteEntry) := (trapLine e ++ "\n").utf8ByteSize
  let invalidSize := (invalidLine ++ "\n").utf8ByteSize
  let w (position width : Nat) : IntegerField := (.typeWidth position 0, none, width)
  (tbl.map fun e => [(.byteLength, none, size e), w 0 8]) ++
  [[(.byteLength, none, invalidSize), w 0 8],
   [w 0 64, w 1 32, w 3 64], [w 1 32], [], [],
   [w 2 64], -- writeAll header
   [], [], -- entry
   [], [], [w 0 64], [(.comparisonRight, some 64, 0), w 0 64], [w 0 1], -- loop
   [], [(.callArgument 0, some 32, 2), w 1 32, w 3 64, w 0 64],
   [(.comparisonRight, some 64, 0), w 0 64], [w 0 1], -- wr
   [], [w 0 8, w 2 64], [w 0 64], [], -- more
   [], [], [], [], -- out, end of writeAll, and blank line
   [w 0 32], -- main header
   [], [w 0 32], [(.comparisonRight, some 32, 256), w 0 32], [w 0 1], -- entry
   [], [w 0 32], -- exit
   [], [(.right, some 32, 256), w 0 32], -- trap
   w 0 32 :: (List.range tbl.length).flatMap (fun i =>
     [(.switchKey i (.trapSite i), some 32, toBits .u32 (Int.ofNat i)), w (i + 1) 32])] ++
  (tbl.flatMap fun e => [[], [(.callArgument 1, some 64, toBits .u64 (Int.ofNat (size e))), w 2 64],
    [(.callArgument 0, some 32, 134), w 1 32], []]) ++
  [[], [(.callArgument 1, some 64, toBits .u64 (Int.ofNat invalidSize)), w 2 64], [], [], []]

/-- The runtime as standalone LLVM text. -/
def llvm (tbl : SiteTable) : String := render (llvmItems tbl)

/-! ## Runtime inventory laws -/

private theorem map_indexedFrom {α β : Type} (xs : List α) (n : Nat) (f : α → β) :
    (indexedFrom n xs).map (fun x => f x.2) = xs.map f := by
  induction xs generalizing n with
  | nil => rfl
  | cons x xs ih => simp [indexedFrom, ih]

private theorem map_indexedFrom_fst {α : Type} (xs : List α) (n : Nat) :
    (indexedFrom n xs).map Prod.fst = List.range' n xs.length := by
  induction xs generalizing n with
  | nil => rfl
  | cons x xs ih => simp [indexedFrom, ih, List.range'_succ]

private theorem flatMap_indexedFrom {α β : Type} (xs : List α) (n : Nat) (f : α → List β) :
    (indexedFrom n xs).flatMap (fun x => f x.2) = xs.flatMap f := by
  induction xs generalizing n with
  | nil => rfl
  | cons x xs ih => simp [indexedFrom, ih]

/-- Every runtime assembly item contributes exactly the integers in the runtime contract. -/
theorem llvmItems_inventory (tbl : SiteTable) :
    (llvmItems tbl).map Item.inventory = expectedInventory tbl := by
  simp only [llvmItems, expectedInventory, List.map_append, List.map_map, List.map_flatMap,
    List.map_cons, List.map_nil, List.flatMap_cons, List.flatMap_nil, Function.comp_def, List.zipIdx_cons, List.zipIdx_nil, Item.inventory, Instruction.inventory,
    Argument.inventory, TypeCode.integerWidth, TypeCode.widthFields, Global.widthFields, Width.bits, Immediate.natural,
    List.nil_append, List.append_nil, List.cons_append, indexed]
  rw [map_indexedFrom tbl 0 (fun e =>
    [((.byteLength, none, (siteBytes e).size) : IntegerField), (.typeWidth 0 0, none, 8)])]
  let fields (i : Nat) : List IntegerField :=
    [(.switchKey i (.trapSite i), some 32, toBits .u32 (Int.ofNat i)), (.typeWidth (i + 1) 0, none, 32)]
  have keys : ((indexedFrom 0 tbl).flatMap fun x => fields x.1) =
      (List.range tbl.length).flatMap fields := by
    rw [List.range_eq_range', ← map_indexedFrom_fst tbl 0, List.flatMap_map]
  have positions (xs : SiteTable) (n : Nat) :
      (((indexedFrom n xs).map (fun x => ((⟨toBits .u32 (Int.ofNat x.1)⟩ : Immediate), Label.trapSite x.1))).zipIdx n).flatMap
        (fun x => [(IntegerRole.switchKey x.2 x.1.2, some 32, x.1.1.value),
          (.typeWidth (x.2 + 1) 0, none, 32)]) =
      (indexedFrom n xs).flatMap (fun x => fields x.1) := by
    induction xs generalizing n with
    | nil => rfl
    | cons x xs ih =>
      simp only [indexedFrom, List.map_cons, List.zipIdx_cons, List.flatMap_cons, fields, List.cons_append,
        List.nil_append, List.cons.injEq, true_and]
      exact ih (n + 1)
  rw [positions tbl 0, keys]
  have traps := flatMap_indexedFrom tbl 0 (fun e =>
    [[], [(IntegerRole.callArgument 1, some 64, toBits .u64 (Int.ofNat (siteBytes e).size)), (.typeWidth 2 0, none, 64)],
      [(IntegerRole.callArgument 0, some 32, 134), (.typeWidth 1 0, none, 32)], []])
  rw [traps]
  simp [siteBytes, invalidBytes, fields, List.append_assoc]

/-- Law. Reading the standalone runtime text recovers its prescribed integer inventory. -/
theorem llvm_inventory (tbl : SiteTable) :
    Assembly.inventory (llvm tbl) = some (expectedInventory tbl) := by
  rw [llvm, Assembly.inventory_render, llvmItems_inventory]

/-- QBE data bytes for a string: one `b N` per UTF-8 byte, so no character needs escaping. -/
def qbeBytes (s : String) : String := ", ".intercalate (s.toUTF8.toList.map fun b => s!"b {b}")

/-- The runtime entry as QBE text for LCB, for the site table `tbl`. -/
def qbe (tbl : SiteTable) : String :=
  let sites := indexed tbl
  let datas := sites.map fun (i, e) => s!"data $site{i} = \{ {qbeBytes (trapLine e ++ "\n")} }\n"
  let dispatch := sites.map fun (i, e) =>
    s!"@c{i}\n    %c{i} =w ceqw %i, {i}\n    jnz %c{i}, @t{i}, @c{i + 1}\n" ++
    s!"@t{i}\n    call $concrete_rt_write_all(l $site{i}, l {(trapLine e ++ "\n").utf8ByteSize})\n" ++
    "    call $_exit(w 134)\n    ret 0\n"
  String.join datas ++
  s!"data $bad = \{ {qbeBytes (invalidLine ++ "\n")} }\n" ++
  s!"function $concrete_rt_write_all(l %p, l %n) \{\n" ++
  "@start\n    jmp @loop\n" ++
  "@loop\n    %done =w ceql %n, 0\n    jnz %done, @out, @wr\n" ++
  "@wr\n    %k =l call $write(w 2, l %p, l %n)\n    %bad =w cslel %k, 0\n    jnz %bad, @out, @more\n" ++
  "@more\n    %p =l add %p, %k\n    %n =l sub %n, %k\n    jmp @loop\n" ++
  "@out\n    ret\n}\n\n" ++
  s!"export function w $main() \{\n" ++
  "@start\n    %w =w call $concrete_body()\n    %ok =w cultw %w, 256\n    jnz %ok, @exit, @trap\n" ++
  "@exit\n    ret %w\n" ++
  "@trap\n    %i =w sub %w, 256\n    jmp @c0\n" ++
  String.join dispatch ++
  s!"@c{tbl.length}\n    call $concrete_rt_write_all(l $bad, l {(invalidLine ++ "\n").utf8ByteSize})\n" ++
  "    call $abort()\n    ret 0\n" ++
  "}\n"

end Concrete.Backend.Runtime

import Concrete.Backend.LLVMIR

/-!
# The runtime entry (both backends)

Every backend emits `main`'s body as `concrete_body`, returning the encoded result word.
The runtime entry is fixed: it calls the body and then

* exits with the word if it is below 256;
* otherwise writes the diagnostic line at index `word - 256` to descriptor 2 (looping on partial
  writes, ignoring write errors) and exits 134 (`ConcreteSpec.Observe.Observation.status`);
* on a word that encodes nothing, writes `invalidLine` and aborts.

This frozen leaf renders that fixed runtime entry from ordered, newline-terminated diagnostic
lines as both LLVM assembly items (`llvmDiagnostics`) and QBE text (`qbeDiagnostics`), with no
dependency on provisional `Concrete.BIR`.
-/

namespace Concrete.Backend.Runtime
open Concrete.Backend.LLVM
open ConcreteSpec.Core (toBits)

/-- What the runtime writes before aborting on a result word that encodes nothing. -/
def invalidLine : String := "concrete: runtime: invalid result word"

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

private def invalidBytes : ByteArray := (invalidLine ++ "\n").toUTF8

/-- The fixed runtime entry and its byte globals as LLVM assembly items, consuming the same
ordered, newline-terminated diagnostics as `qbeDiagnostics`. -/
def llvmDiagnostics (diagnostics : List String) : List Item :=
  let sites := indexed diagnostics
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
  let globals := sites.map fun (i, message) => Item.constant (.site i) message.toUTF8
  let cases := sites.map fun (i, _) => (Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite i)
  let traps := sites.flatMap fun (i, message) =>
    [Item.label (.trapSite i), writeAll (.site i) message.toUTF8, trapExit, .terminator .unreachable]
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

/-- QBE data bytes for a string: one `b N` per UTF-8 byte, so no character needs escaping. -/
def qbeBytes (s : String) : String := ", ".intercalate (s.toUTF8.toList.map fun b => s!"b {b}")

/-- Render the exact runtime input from its ordered, newline-terminated diagnostics. -/
def qbeDiagnostics (diagnostics : List String) : String :=
  let sites := indexed diagnostics
  let datas := sites.map fun (i, message) => s!"data $site{i} = \{ {qbeBytes message} }\n"
  let dispatch := sites.map fun (i, message) =>
    s!"@c{i}\n    %c{i} =w ceqw %i, {i}\n    jnz %c{i}, @t{i}, @c{i + 1}\n" ++
    s!"@t{i}\n    call $concrete_rt_write_all(l $site{i}, l {message.utf8ByteSize})\n" ++
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
  s!"@c{diagnostics.length}\n    call $concrete_rt_write_all(l $bad, l {(invalidLine ++ "\n").utf8ByteSize})\n" ++
  "    call $abort()\n    ret 0\n" ++
  "}\n"

end Concrete.Backend.Runtime

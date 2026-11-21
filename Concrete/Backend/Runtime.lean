import Concrete.BIR
import Concrete.Backend.Decimal

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
open Concrete.Backend.LLVM (decimal integerType)
open ConcreteSpec (IntTy Trap)

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

/-- An LLVM `c"..."` string body over the UTF-8 bytes: printable ASCII except `"` and `\` verbatim,
every other byte as `\XX`. -/
def llvmEscape (s : String) : String :=
  String.join (s.toUTF8.toList.map fun b =>
    let n := b.toNat
    if 32 ≤ n ∧ n < 127 ∧ n ≠ 34 ∧ n ≠ 92 then (Char.ofNat n).toString
    else
      let hex := Nat.toDigits 16 n
      "\\" ++ String.ofList ((if hex.length < 2 then ['0'] else []) ++ hex.map Char.toUpper))

def llvmByteString (s : String) : String :=
  "c\"" ++ llvmEscape s ++ "\""

/-- The runtime entry as LLVM text, for the site table `tbl`. -/
def llvm (tbl : SiteTable) : String :=
  let sites := indexed tbl
  let globals := sites.map fun (i, e) =>
    let line := trapLine e ++ "\n"
    s!"@.site{decimal i} = private constant [{decimal line.utf8ByteSize} x {integerType 8}] {llvmByteString line}\n"
  let cases := String.join (sites.map fun (i, _) => s!" {integerType 32} {decimal i}, label %t{decimal i}")
  let traps := sites.map fun (i, e) =>
    s!"t{decimal i}:\n  call void @concrete_rt_write_all(ptr @.site{decimal i}, {integerType 64} {decimal ((trapLine e ++ "\n").utf8ByteSize)})\n" ++
    s!"  call void @_exit({integerType 32} {decimal 134})\n  unreachable\n"
  String.join globals ++
  s!"@.bad = private constant [{decimal ((invalidLine ++ "\n").utf8ByteSize)} x {integerType 8}] {llvmByteString (invalidLine ++ "\n")}\n" ++
  s!"declare {integerType 64} @write({integerType 32}, ptr, {integerType 64})\n" ++
  s!"declare void @_exit({integerType 32})\n" ++
  "declare void @abort()\n\n" ++
  s!"define internal void @concrete_rt_write_all(ptr %p, {integerType 64} %n) \{\n" ++
  "entry:\n  br label %loop\n" ++
  "loop:\n  %pp = phi ptr [ %p, %entry ], [ %np, %more ]\n" ++
  s!"  %nn = phi {integerType 64} [ %n, %entry ], [ %rest, %more ]\n" ++
  s!"  %done = icmp eq {integerType 64} %nn, {decimal 0}\n  br {integerType 1} %done, label %out, label %wr\n" ++
  s!"wr:\n  %k = call {integerType 64} @write({integerType 32} {decimal 2}, ptr %pp, {integerType 64} %nn)\n" ++
  s!"  %bad = icmp sle {integerType 64} %k, {decimal 0}\n  br {integerType 1} %bad, label %out, label %more\n" ++
  s!"more:\n  %np = getelementptr {integerType 8}, ptr %pp, {integerType 64} %k\n  %rest = sub {integerType 64} %nn, %k\n  br label %loop\n" ++
  "out:\n  ret void\n}\n\n" ++
  s!"define {integerType 32} @main() \{\n" ++
  s!"entry:\n  %w = call {integerType 32} @concrete_body()\n  %ok = icmp ult {integerType 32} %w, {decimal 256}\n" ++
  s!"  br {integerType 1} %ok, label %exit, label %trap\n" ++
  s!"exit:\n  ret {integerType 32} %w\n" ++
  s!"trap:\n  %i = sub {integerType 32} %w, {decimal 256}\n" ++
  s!"  switch {integerType 32} %i, label %bad [{cases} ]\n" ++
  String.join traps ++
  "bad:\n" ++
  s!"  call void @concrete_rt_write_all(ptr @.bad, {integerType 64} {decimal ((invalidLine ++ "\n").utf8ByteSize)})\n" ++
  "  call void @abort()\n  unreachable\n}\n"

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

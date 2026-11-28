import Concrete.Interfaces.Behavior
import Concrete.Backend.Observation
import Concrete.Backend.Runtime
import Concrete.Backend.LCBJob

/-!
Focused controls for Slice 0 freeze unit F2 (compiler boundary cleanup):
- `DiagnosticSite` and `DiagnosticLocation` over `ConcreteSpec.Core.SiteId` (zero `Concrete.BIR` import);
- `Runtime.llvmDiagnostics` and `Runtime.qbeDiagnostics` over ordered newline-terminated diagnostics;
- generic `Backend.lcbSemanticsFor` without provisional BIR/LCB dependencies;
- `behaviorRefl` and `behaviorTrans`, including terminating trace preservation, `ProgramSafe`
  propagation, diverging infinite-prefix equivalence in both directions under different timings,
  and negative controls.
-/

namespace StageInterfaceControls.BoundaryCleanup

open Concrete.Contracts
open Concrete.Interfaces
open Concrete.Backend
open Concrete.Backend.LLVM
open Concrete.Backend.Runtime
open LeanCompilerBackend.Verified

-- 1. Pure DiagnosticSite and DiagnosticLocation over ConcreteSpec.Core.SiteId.

private def sampleSite : DiagnosticSite :=
  ⟨⟨9⟩, ⟨"src/main.con", 4, 12⟩⟩

example :
    sampleSite.id = (⟨9⟩ : ConcreteSpec.Core.SiteId) ∧
    sampleSite.location = ⟨"src/main.con", 4, 12⟩ ∧
    sampleSite ≠ ⟨⟨10⟩, ⟨"src/main.con", 4, 12⟩⟩ ∧
    sampleSite ≠ ⟨⟨9⟩, ⟨"src/other.con", 4, 12⟩⟩ ∧
    sampleSite ≠ ⟨⟨9⟩, ⟨"src/main.con", 5, 12⟩⟩ ∧
    sampleSite ≠ ⟨⟨9⟩, ⟨"src/main.con", 4, 13⟩⟩ := by
  decide

-- 2. Frozen Runtime.llvmDiagnostics and Runtime.qbeDiagnostics over ordered diagnostics.

private def diag0 : String := "concrete: trap: overflow at src/a.con:1:5 (site 0)\n"
private def diag1 : String := "concrete: trap: overflow at src/b.con:2:9 (site 1)\n"

example :
    let items := llvmDiagnostics [diag0, diag1]
    items.contains (Item.constant (.site 0) diag0.toUTF8) = true ∧
    items.contains (Item.constant (.site 1) diag1.toUTF8) = true ∧
    items.contains (Item.constant .bad (invalidLine ++ "\n").toUTF8) = true ∧
    items.contains (Item.terminator (.switch .u32 .i .bad
      [(Immediate.natural 0, Label.trapSite 0), (Immediate.natural 1, Label.trapSite 1)])) = true ∧
    items.contains (Item.instruction (.call none .void .exit
      [(.integer .u32, .immediate (.natural 134))])) = true ∧
    (llvmDiagnostics [diag0] == llvmDiagnostics [diag1]) = false ∧
    (llvmDiagnostics [] == llvmDiagnostics [diag0]) = false ∧
    render [Item.constant (.site 0) diag0.toUTF8] ≠ render [Item.constant (.site 0) diag1.toUTF8] := by
  decide

private def qbeShortMiddle : String :=
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
  "@trap\n    %i =w sub %w, 256\n    jmp @c0\n"

private def qbeShortDispatch : String :=
  "@c0\n    %c0 =w ceqw %i, 0\n    jnz %c0, @t0, @c1\n" ++
  "@t0\n    call $concrete_rt_write_all(l $site0, l 2)\n" ++
  "    call $_exit(w 134)\n    ret 0\n"

private def qbeShortEnd : String :=
  s!"@c1\n    call $concrete_rt_write_all(l $bad, l {(invalidLine ++ "\n").utf8ByteSize})\n" ++
  "    call $abort()\n    ret 0\n" ++
  "}\n"

private def expectedShortQBE (bytes : String) : String :=
  s!"data $site0 = \{ {bytes} }\n" ++ qbeShortMiddle ++ qbeShortDispatch ++ qbeShortEnd

private theorem string_join_singleton (s : String) : String.join [s] = s := by
  simp [String.join]

private theorem qbeDiagnostics_short (s bytes : String)
    (hBytes : qbeBytes s = bytes) (hSize : s.utf8ByteSize = 2) :
    qbeDiagnostics [s] = expectedShortQBE bytes := by
  have hHead : (s!"data $site{(0 : Nat)} = \{ {bytes} }\n") = s!"data $site0 = \{ {bytes} }\n" := by
    change toString "data $site" ++ toString (0 : Nat) ++ toString " = { " ++ toString bytes ++ toString " }\n" = _
    have h0 : toString (0 : Nat) = "0" := by decide
    rw [h0]
    simp only [toString, String.append_assoc]
    rfl
  have hDisp :
      s!"@c{(0 : Nat)}\n    %c{(0 : Nat)} =w ceqw %i, {(0 : Nat)}\n    jnz %c{(0 : Nat)}, @t{(0 : Nat)}, @c{(0 : Nat) + 1}\n" ++
      s!"@t{(0 : Nat)}\n    call $concrete_rt_write_all(l $site{(0 : Nat)}, l {(2 : Nat)})\n" ++
      "    call $_exit(w 134)\n    ret 0\n" = qbeShortDispatch := by
    decide
  have hEnd :
      s!"@c{(1 : Nat)}\n    call $concrete_rt_write_all(l $bad, l {(invalidLine ++ "\n").utf8ByteSize})\n" ++
      "    call $abort()\n    ret 0\n" ++ "}\n" = qbeShortEnd := by
    unfold qbeShortEnd
    simp only [toString, String.append_assoc]
    rfl
  unfold qbeDiagnostics expectedShortQBE qbeShortMiddle
  simp only [indexed, indexedFrom, List.map, List.length, string_join_singleton, hBytes, hSize,
    hHead, hDisp]
  simp only [String.append_assoc]
  rw [← hEnd]
  simp only [String.append_assoc]

private theorem prefixByte (head tail : String) (idx : Nat) (h : idx < head.toUTF8.data.size) :
    (head ++ tail).toUTF8.data[idx]? = head.toUTF8.data[idx]? := by
  change (head.toUTF8 ++ tail.toUTF8).data[idx]? = _
  rw [ByteArray.data_append, Array.getElem?_append_left h]

example :
    qbeDiagnostics ["a\n"] = expectedShortQBE "b 97, b 10" ∧
    qbeDiagnostics ["a\n"] ≠ qbeDiagnostics ["b\n"] ∧
    qbeDiagnostics [] ≠ qbeDiagnostics ["a\n"] := by
  have hA : qbeDiagnostics ["a\n"] = expectedShortQBE "b 97, b 10" :=
    qbeDiagnostics_short "a\n" "b 97, b 10" (by cbv) (by decide)
  have hB : qbeDiagnostics ["b\n"] = expectedShortQBE "b 98, b 10" :=
    qbeDiagnostics_short "b\n" "b 98, b 10" (by cbv) (by decide)
  refine ⟨hA, ?_, ?_⟩
  · intro h
    rw [hA, hB] at h
    have b := congrArg (fun s => s.toUTF8.data[19]?) h
    have h1 : (expectedShortQBE "b 97, b 10").toUTF8.data[19]? = some 55 := by
      unfold expectedShortQBE
      rw [String.append_assoc, String.append_assoc,
        prefixByte _ _ 19 (by decide)]
      rfl
    have h2 : (expectedShortQBE "b 98, b 10").toUTF8.data[19]? = some 56 := by
      unfold expectedShortQBE
      rw [String.append_assoc, String.append_assoc,
        prefixByte _ _ 19 (by decide)]
      rfl
    rw [h1, h2] at b
    exact (by decide : (some (55 : UInt8) ≠ some 56)) b
  · intro h
    rw [hA] at h
    have b := congrArg (fun s => s.toUTF8.data[6]?) h
    have h1 : (qbeDiagnostics []).toUTF8.data[6]? = some 98 := rfl
    have h2 : (expectedShortQBE "b 97, b 10").toUTF8.data[6]? = some 115 := by
      unfold expectedShortQBE
      rw [String.append_assoc, String.append_assoc,
        prefixByte _ _ 6 (by decide)]
      rfl
    rw [h1, h2] at b
    exact (by decide : (some (98 : UInt8) ≠ some 115)) b

-- 3. Generic lcbSemanticsFor works directly from Verified.Source without provisional BIR/LCB.

private def returnWordFn (word : BitVec 32) : Source.Function :=
  ⟨.word, #[], #[], #[⟨#[], #[], .ret (.literal (.word word))⟩]⟩

private def directProject (_ : BitVec 32) : Source.Value → Option Observation
  | .word w =>
    if w.toNat < 256 then some (.returned (UInt8.ofNat w.toNat) ByteArray.empty)
    else if w.toNat == 256 then some (.trapped 134 .overflow sampleSite diag0.toUTF8)
    else none
  | _ => none

private abbrev directLCB : Semantics (BitVec 32) Nat :=
  lcbSemanticsFor returnWordFn directProject

example :
    directLCB.observes 42 (.returned 42 ByteArray.empty) ∧
    directLCB.observes 256 (.trapped 134 .overflow sampleSite diag0.toUTF8) ∧
    validateJob (LCBJob.lcbFunctionJob (returnWordFn 42)) = .ok () := by
  with_unfolding_all
    exact ⟨⟨some ⟨0, 0, #[]⟩, ⟨some ⟨0, 0, #[]⟩, rfl, .nil _⟩, rfl⟩,
      ⟨some ⟨0, 0, #[]⟩, ⟨some ⟨0, 0, #[]⟩, rfl, .nil _⟩, rfl⟩, rfl⟩

-- 4. behaviorRefl and behaviorTrans: terminating, unsafe-source, and diverging controls.

private def constantSem (o : Observation) : Semantics Unit Nat where
  State := Unit
  initial _ _ := True
  initialized _ := ⟨(), trivial⟩
  step _ _ _ := False
  result _ _ obs := obs = o
  terminal _ _ _ _ _ h := h
  safe _ _ := True
  trace _ _ := o.stderr.data.toList.map ProcessEvent.stderr
  permitted _ _ := True
  resultTrace _ _ obs h := by
    subst h
    generalize obs.stderr.data.toList = bytes
    induction bytes with
    | nil => rfl
    | cons b rest ih =>
      change b :: stderrBytes (rest.map ProcessEvent.stderr) = b :: rest
      rw [ih]
  traceGrows _ _ _ h := h.elim

private def stuckSem : Semantics Unit Nat where
  State := Unit
  initial _ _ := True
  initialized _ := ⟨(), trivial⟩
  step _ _ _ := False
  result _ _ _ := False
  terminal _ _ _ h := h.elim
  safe _ _ := False
  trace _ _ := []
  permitted _ _ := True
  resultTrace _ _ _ h := h.elim
  traceGrows _ _ _ h := h.elim

-- behaviorRefl introduces ProgramSafe internally, so it holds for both safe and unsafe programs.
example :
    BehaviorRefinement (constantSem (.returned 42 ByteArray.empty))
      (constantSem (.returned 42 ByteArray.empty))
      (constantSem (.returned 42 ByteArray.empty)).result
      (constantSem (.returned 42 ByteArray.empty)).result () () ∧
    BehaviorRefinement stuckSem stuckSem stuckSem.result stuckSem.result () () :=
  ⟨behaviorRefl (), behaviorRefl ()⟩

-- Terminating 3-stage transitivity across distinct step counts preserves results and traces.
private def countedSem (steps : Nat) (o : Observation) : Semantics Unit Nat where
  State := Nat
  initial _ n := n = 0
  initialized _ := ⟨0, rfl⟩
  step _ n m := n < steps ∧ m = n + 1
  result _ n obs := n = steps ∧ obs = o
  terminal _ _ _ h _ hstep := Nat.ne_of_lt hstep.1 h.1
  safe _ _ := True
  trace _ n := if n = steps then o.stderr.data.toList.map ProcessEvent.stderr else []
  permitted _ _ := True
  resultTrace _ n obs h := by
    obtain ⟨rfl, rfl⟩ := h
    simp only [↓reduceIte]
    generalize obs.stderr.data.toList = bytes
    induction bytes with
    | nil => rfl
    | cons b rest ih =>
      change b :: stderrBytes (rest.map ProcessEvent.stderr) = b :: rest
      rw [ih]
  traceGrows _ n _ h := by
    have neq : n ≠ steps := Nat.ne_of_lt h.1
    simp only [neq, ↓reduceIte, List.nil_append]
    exact ⟨_, rfl⟩

private theorem countedReach {steps : Nat} {o : Observation} (n : Nat) (le : n ≤ steps) :
    (countedSem steps o).finite () n := by
  induction n with
  | zero => exact ⟨(0 : Nat), rfl, .nil (0 : Nat)⟩
  | succ k ih =>
    obtain ⟨start, init, path⟩ := ih (Nat.le_of_succ_le le)
    have appendSteps : ∀ {a b c : (countedSem steps o).State},
        Steps ((countedSem steps o).step ()) a b →
        (countedSem steps o).step () b c →
        Steps ((countedSem steps o).step ()) a c := by
      intro a b c p tick
      induction p with
      | nil => exact .cons tick (.nil _)
      | cons h _ ihp => exact .cons h (ihp tick)
    exact ⟨start, init, appendSteps path ⟨Nat.lt_of_succ_le le, rfl⟩⟩

private theorem countedRefines {k m : Nat} {o : Observation} :
    BehaviorRefinement (countedSem k o) (countedSem m o)
      (countedSem k o).result (countedSem m o).result () () := by
  intro _
  refine ⟨?_, ?_, ?_⟩
  · intro q obs _ hres
    change Nat at q
    obtain ⟨hq, ho⟩ := hres
    subst ho
    have hq' : q = m := hq
    subst hq'
    exact ⟨k, countedReach k (Nat.le_refl k), ⟨rfl, rfl⟩, by simp [countedSem]⟩
  · intro run div
    change Nat → Nat at run
    have atStep (i : Nat) : run i = i := by
      induction i with
      | zero => exact div.1
      | succ i ih => exact (div.2 i).2.trans (congrArg (· + 1) ih)
    have hlt : run m < m := (div.2 m).1
    rw [atStep m] at hlt
    exact (Nat.lt_irrefl m hlt).elim
  · intro q stuck
    change Nat at q
    by_cases hlt : q < m
    · exact stuck.2.1 (q + 1) ⟨hlt, rfl⟩
    · have reachLe : q ≤ m := by
        obtain ⟨start, init, path⟩ := stuck.1
        have bound : ∀ {a b : Nat}, Steps ((countedSem m o).step ()) a b → a ≤ m → b ≤ m := by
          intro a b path
          induction path with
          | nil => exact id
          | cons step _ ih => intro _; exact ih (step.2 ▸ Nat.succ_le_of_lt step.1)
        exact bound path (init ▸ Nat.zero_le m)
      have eq : q = m := Nat.le_antisymm reachLe (Nat.not_lt.mp hlt)
      exact stuck.2.2 o ⟨eq, rfl⟩

example :
    let o := Observation.trapped 134 .overflow sampleSite diag0.toUTF8
    BehaviorRefinement (countedSem 1 o) (countedSem 3 o)
      (countedSem 1 o).result (countedSem 3 o).result () () :=
  behaviorTrans (countedRefines (k := 1) (m := 2)) (countedRefines (k := 2) (m := 3))

-- Diverging 3-stage transitivity with different trace timings:
-- emits `count n` copies of `.effect 0` at step `n` for any monotone `count : Nat → Nat`.
private def repeatEvent (n : Nat) : List (ProcessEvent Nat) :=
  List.replicate n (.effect 0)

private theorem repeatEvent_append (a b : Nat) :
    repeatEvent a ++ repeatEvent b = repeatEvent (a + b) := by
  induction a with
  | zero => simp [repeatEvent]
  | succ a ih =>
    rw [Nat.succ_add]
    change ProcessEvent.effect 0 :: (repeatEvent a ++ repeatEvent b) =
      ProcessEvent.effect 0 :: repeatEvent (a + b)
    rw [ih]

private theorem repeatEvent_prefix {a b : Nat} (le : a ≤ b) :
    TracePrefix (repeatEvent a) (repeatEvent b) := by
  refine ⟨repeatEvent (b - a), ?_⟩
  rw [repeatEvent_append, Nat.add_sub_cancel' le]

private theorem prefix_trans {E : Type} {xs ys zs : List E}
    (h1 : TracePrefix xs ys) (h2 : TracePrefix ys zs) : TracePrefix xs zs := by
  obtain ⟨s1, rfl⟩ := h1
  obtain ⟨s2, rfl⟩ := h2
  exact ⟨s1 ++ s2, (List.append_assoc xs s1 s2).symm⟩

private def streamSem (count : Nat → Nat) (mono : ∀ n, count n ≤ count (n + 1)) :
    Semantics Unit Nat where
  State := Nat
  initial _ n := n = 0
  initialized _ := ⟨0, rfl⟩
  step _ n m := m = n + 1
  result _ _ _ := False
  terminal _ _ _ h := h.elim
  safe _ _ := True
  trace _ n := repeatEvent (count n)
  permitted _ _ := True
  resultTrace _ _ _ h := h.elim
  traceGrows _ n _ h := h ▸ repeatEvent_prefix (mono n)

private theorem streamSem_run_eq {count : Nat → Nat} {mono : ∀ n, count n ≤ count (n + 1)}
    {run : Nat → Nat} (div : (streamSem count mono).diverges () run) (i : Nat) :
    run i = i := by
  induction i with
  | zero => exact div.1
  | succ i ih => exact (div.2 i).trans (congrArg (· + 1) ih)

private theorem streamSem_refines {c1 c2 : Nat → Nat}
    {m1 : ∀ n, c1 n ≤ c1 (n + 1)} {m2 : ∀ n, c2 n ≤ c2 (n + 1)}
    (cofinal12 : ∀ i, ∃ j, c1 i ≤ c2 j)
    (cofinal21 : ∀ j, ∃ i, c2 j ≤ c1 i) :
    BehaviorRefinement (streamSem c1 m1) (streamSem c2 m2)
      (streamSem c1 m1).result (streamSem c2 m2).result () () := by
  intro _
  refine ⟨fun _ _ _ h => h.elim, ?_, fun q stuck => by
    change Nat at q
    exact stuck.2.1 (q + 1) rfl⟩
  intro run div
  refine ⟨id, ⟨rfl, fun _ => rfl⟩, ?_⟩
  intro events
  constructor
  · rintro ⟨i, pref⟩
    obtain ⟨j, le⟩ := cofinal12 i
    refine ⟨j, ?_⟩
    change TracePrefix events (repeatEvent (c2 (run j)))
    rw [streamSem_run_eq div j]
    exact prefix_trans pref (repeatEvent_prefix le)
  · rintro ⟨j, pref⟩
    obtain ⟨i, le⟩ := cofinal21 j
    refine ⟨i, ?_⟩
    change TracePrefix events (repeatEvent (c2 (run j))) at pref
    rw [streamSem_run_eq div j] at pref
    exact prefix_trans pref (repeatEvent_prefix le)

private abbrev fastStream : Semantics Unit Nat :=
  streamSem (fun n => n) (fun n => Nat.le_succ n)

private abbrev stutterStream : Semantics Unit Nat :=
  streamSem (fun n => n / 2) (fun n => Nat.div_le_div_right (Nat.le_succ n))

private abbrev batchStream : Semantics Unit Nat :=
  streamSem (fun n => 2 * (n / 3)) (fun n => Nat.mul_le_mul_left 2 (Nat.div_le_div_right (Nat.le_succ n)))

-- Compose fast -> stutter -> batch with behaviorTrans and check SameInfiniteTrace in BOTH directions.
example :
    BehaviorRefinement fastStream batchStream fastStream.result batchStream.result () () ∧
    ∀ run, batchStream.diverges () run →
      ∃ matching, fastStream.diverges () matching ∧
        (∀ events, (∃ i, TracePrefix events (fastStream.trace () (matching i))) →
          ∃ j, TracePrefix events (batchStream.trace () (run j))) ∧
        (∀ events, (∃ j, TracePrefix events (batchStream.trace () (run j))) →
          ∃ i, TracePrefix events (fastStream.trace () (matching i))) := by
  have h1 : BehaviorRefinement fastStream stutterStream fastStream.result stutterStream.result () () :=
    streamSem_refines (fun i => ⟨2 * i, by omega⟩) (fun j => ⟨j / 2, Nat.le_refl _⟩)
  have h2 : BehaviorRefinement stutterStream batchStream stutterStream.result batchStream.result () () :=
    streamSem_refines (fun i => ⟨3 * i, by omega⟩) (fun j => ⟨4 * (j / 3), by omega⟩)
  have composed := behaviorTrans h1 h2
  refine ⟨composed, ?_⟩
  intro run div
  have safeFast : ProgramSafe fastStream () := fun q stuck => by
    change Nat at q
    exact stuck.2.1 (q + 1) rfl
  obtain ⟨matching, divFast, same⟩ := (composed safeFast).2.1 run div
  exact ⟨matching, divFast, fun e => (same e).mp, fun e => (same e).mpr⟩

-- Negative controls:
-- 1) Safe source cannot refine a stuck target.
example :
    ¬ BehaviorRefinement (constantSem (.returned 0 ByteArray.empty)) stuckSem
      (constantSem (.returned 0 ByteArray.empty)).result stuckSem.result () () := by
  intro ref
  have safeSrc : ProgramSafe (constantSem (.returned 0 ByteArray.empty)) () :=
    fun _ stuck => stuck.2.2 (.returned 0 ByteArray.empty) rfl
  exact (ref safeSrc).2.2 () ⟨⟨(), trivial, .nil ()⟩, fun _ h => h, fun _ h => h⟩

-- 2) A silent diverging target cannot refine an event-emitting diverging source.
private abbrev silentStream : Semantics Unit Nat :=
  streamSem (fun _ => 0) (fun _ => Nat.le_refl 0)

example :
    ¬ BehaviorRefinement fastStream silentStream fastStream.result silentStream.result () () := by
  intro ref
  have safeFast : ProgramSafe fastStream () := fun q stuck => by
    change Nat at q
    exact stuck.2.1 (q + 1) rfl
  obtain ⟨matching, divFast, same⟩ := (ref safeFast).2.1 id ⟨rfl, fun _ => rfl⟩
  have hasEvent : ∃ i, TracePrefix [.effect 0] (fastStream.trace () (matching i)) := by
    refine ⟨1, ?_⟩
    change TracePrefix [.effect 0] (repeatEvent (matching 1))
    rw [streamSem_run_eq divFast 1]
    exact ⟨[], rfl⟩
  obtain ⟨j, suffix, eq⟩ := (same [.effect 0]).mp hasEvent
  cases eq

end StageInterfaceControls.BoundaryCleanup

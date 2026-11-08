import ConcreteSpec.Spec.Frame
import ConcreteSpec.Proofs.Memory
import ConcreteSpec.Proofs.Exec

/-!
# Laws of logical footprints and frames (foundation C10)

Byte-range disjointness, preservation under sequential composition, and the frame laws of C4 byte
store, release, exposure and fresh allocation. The allocation theorem also shows that the fresh block
is disjoint from every range in an old block.
-/
namespace ConcreteSpec.Core.Frame
open ConcreteSpec.Core.Mem
open ConcreteSpec.Core.Hybrid

/-- The empty footprint frames every operation. -/
theorem preserves_refl (st : State) : Footprint.empty.Preserves st st := by
  constructor
  · intro b i x hx ho
    simp only [hx, Option.map_some]
  · intro p hp
    rfl

/-- Membership outside a union is membership outside both components. -/
theorem byteOutside_union {f g : Footprint} {b : MemBlockId} {i : Nat}
    (h : (f.union g).ByteOutside b i) : f.ByteOutside b i ∧ g.ByteOutside b i := by
  constructor <;> intro r hr <;> apply h r <;> simp [Footprint.union, hr]

/-- Place membership outside a union is membership outside both components. -/
theorem placeOutside_union {f g : Footprint} {p : Place}
    (h : (f.union g).PlaceOutside p) : f.PlaceOutside p ∧ g.PlaceOutside p := by
  have hp : p ∉ f.places ++ g.places := h
  simp only [List.mem_append, not_or] at hp
  exact hp

/-- Sequential composition frames the union of the two operations' footprints. -/
theorem preserves_seq {st st' st'' : State} {f g : Footprint}
    (h₁ : f.Preserves st st') (h₂ : g.Preserves st' st'') : (f.union g).Preserves st st'' := by
  constructor
  · intro b i x hx ho
    obtain ⟨hf, hg⟩ := byteOutside_union ho
    have he₁ := h₁.1 b i x hx hf
    cases hy : st'.memory.block b with
    | none => simp [hy] at he₁
    | some y =>
      simp only [hy, Option.map_some, Option.some.injEq] at he₁
      have he₂ := h₂.1 b i y hy hg
      exact he₂.trans (congrArg some he₁)
  · intro p hp
    obtain ⟨hf, hg⟩ := placeOutside_union hp
    exact (h₂.2 p hg).trans (h₁.2 p hf)

/-- A successful byte store preserves every old byte outside the exact access range and every
structured place. A zero-byte store has the empty footprint. -/
theorem store_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt) (m' : Memory)
    (p : Ptr) (bs : List MByte) (a : Nat) (access : Option (Option (MemBlockId × Nat)))
    (ha : st.memory.access tgt p bs.length a = access)
    (h : st.memory.store tgt p bs a = some m') :
    Footprint.Preserves st { st with memory := m' } (accessFootprint access bs.length) := by
  cases access with
  | none => simp [Memory.store, ha] at h
  | some r =>
    cases r with
    | none =>
      simp only [Memory.store, ha, Option.some.injEq] at h
      cases h
      exact preserves_refl st
    | some pair =>
      obtain ⟨b, off⟩ := pair
      constructor
      · intro c i x hx ho
        by_cases hc : c = b
        · subst c
          apply store_same_block_frame tgt st.memory _ hw p bs a b off i x ha hx h
          have hn : ¬ ByteRange.Contains ⟨b, off, bs.length⟩ b i :=
            ho _ (by simp [accessFootprint, Footprint.byte])
          simp only [ByteRange.Contains, true_and] at hn
          omega
        · rw [store_other_block tgt st.memory m' p bs a b off c ha h hc, hx]
          rfl
      · intro q hq
        rfl

/-- Releasing a block may change its lifecycle metadata, but frames all bytes outside its full-block
footprint and every structured place. -/
theorem free_frame (st : State) (b : MemBlockId) (x : Block) (m' : Memory)
    (hx : st.memory.block b = some x) (h : st.memory.free b = some m') :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b x.size)) := by
  unfold Memory.free at h
  rw [hx] at h
  by_cases hl : x.live = true
  · simp only [hl, ↓reduceIte, Option.some.injEq] at h
    subst m'
    constructor
    · intro c i y hy ho
      by_cases hc : c = b
      · subst c
        rw [hx] at hy
        cases hy
        simp only [block_modify, ↓reduceIte, hx, Option.map_some]
      · simp only [block_modify, hc, ↓reduceIte, hy, Option.map_some]
    · intro q hq
      rfl
  · simp [hl] at h

/-- Exposure may change only the exposed flag of the provenance block; every byte outside its
full-block footprint and every place is preserved. -/
theorem expose_frame (st : State) (m' : Memory) (p : Ptr) (b : MemBlockId) (x : Block)
    (hp : p.prov = some b) (hx : st.memory.block b = some x) (h : st.memory.expose p = some m') :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b x.size)) := by
  obtain ⟨c, y, hp', hx', hl, rfl⟩ := expose_result st.memory m' p h
  rw [hp] at hp'
  cases hp'
  rw [hx] at hx'
  cases hx'
  constructor
  · intro d i z hz ho
    by_cases hd : d = b
    · subst d
      rw [hx] at hz
      cases hz
      simp only [block_modify, ↓reduceIte, hx, Option.map_some]
    · simp only [block_modify, hd, ↓reduceIte, hz, Option.map_some]
  · intro q hq
    rfl

/-- Fresh allocation preserves every old byte and place. The footprint is the complete new block;
old bytes are necessarily outside it because their allocation identities differ. -/
theorem alloc_frame (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : st.memory.alloc tgt space base size align = .ok (b, m')) :
    Footprint.Preserves st { st with memory := m' } (.byte (fullBlock b size)) := by
  obtain ⟨hb, hm, hfits⟩ := alloc_result tgt st.memory space base size align b m' h
  constructor
  · intro c i x hx ho
    have hne : b ≠ c := alloc_ne_existing tgt st.memory hw space base size align b c m' x h hx
    subst b
    rw [hm]
    change AList.lookup st.memory.blocks c = some x at hx
    simp only [Memory.block, AList.lookup, hne, ↓reduceIte, hx, Option.map_some]
  · intro p hp
    rfl

/-- The allocated identity is disjoint from every range whose block existed before allocation. -/
theorem alloc_fresh_disjoint (tgt : Target) (st : State) (hw : st.memory.WellFormed tgt)
    (space : AddrSpace) (base size align : Nat) (b : MemBlockId) (m' : Memory)
    (h : st.memory.alloc tgt space base size align = .ok (b, m'))
    (r : ByteRange) (x : Block) (hx : st.memory.block r.block = some x) :
    (fullBlock b size).Disjoint r :=
  .inl (alloc_ne_existing tgt st.memory hw space base size align b r.block m' x h hx)

/-- Entering a C1 call saves the caller's locals in a `.ret` frame, binds the evaluated arguments to
callee parameters, and preserves the caller's continuation below that frame. -/
theorem step_call_saves_caller {O : Exec.Operands} (P : Exec.Program O) (dst : Option Var)
    (f : FnId) (args : List O.Expr) (site : SiteId) (caller : AList Var O.Val)
    (st stArgs stEnter : O.Store) (k : List (Exec.Frame O)) (vs : List O.Val) (fn : Exec.Fn O)
    (hv : Exec.evalArgs O caller st args = .ok (vs, stArgs))
    (hf : AList.lookup P.fns f = some fn) (hlen : fn.params.length = vs.length)
    (hl : O.life (.enter (fn.params.map Prod.fst)) stArgs = some stEnter) :
    Exec.step P ⟨.exec (.call dst f args site), caller, st, k⟩ =
      some (.next ⟨.exec fn.body, (fn.params.map Prod.fst).zip vs, stEnter,
        .scope fn.block (fn.params.map Prod.fst) [] :: .ret dst caller :: k⟩) := by
  simp [Exec.step, Exec.onRes, hv, hf, hlen, hl]

/-- A returned value reaching a `.ret` frame with no result destination restores the caller's locals
and continuation exactly. Only the lifecycle-transformed shared state is threaded through. -/
theorem step_return_restores_caller {O : Exec.Operands} (P : Exec.Program O) (v : O.Val)
    (caller callee : AList Var O.Val) (st st' : O.Store) (k : List (Exec.Frame O))
    (hl : O.life .leave st = some st') :
    Exec.step P ⟨.exiting (.ret v), callee, st, .ret none caller :: k⟩ =
      some (.next ⟨.done, caller, st', k⟩) := by
  simpa [Exec.bindDst] using Exec.step_return_caller P v none caller callee st k st' st' hl rfl

/-- Successful cleanup does not disturb call-frame ownership: after every exited scope has run its
deferred calls, a return with no destination restores exactly the caller's locals and continuation. -/
theorem return_cleanup_restores_caller {O : Exec.Operands} (P : Exec.Program O) (v : O.Val)
    (caller : AList Var O.Val) (k pre : List (Exec.Frame O)) {L : AList Var O.Val}
    {st stClean stLeave : O.Store} {tr : List O.Event}
    (h : Exec.Cleans P pre L st tr stClean) (hp : ∀ f ∈ pre, Exec.Passes (.ret v) f)
    (hl : O.life .leave stClean = some stLeave) :
    Exec.Steps P ⟨.exiting (.ret v), L, st, pre ++ .ret none caller :: k⟩ tr
      ⟨.done, caller, stLeave, k⟩ := by
  simpa [Exec.bindDst] using Exec.return_reaches_caller P v none caller k h hp stLeave stLeave hl rfl

/-- Every complete `C1Call` ends with the result bound into the saved caller locals at the saved
continuation. With `dst = none`, this is literal equality with the caller locals. -/
theorem c1Call_restores_caller {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {entry exit : Exec.Config O} (h : C1Call O f P entry exit) :
    ∃ (dst : Option Var) (result : O.Val) (caller : AList Var O.Val) (k : List (Exec.Frame O)),
      exit.locals = Exec.bindDst dst result caller ∧ exit.frames = k := by
  obtain ⟨fn, dst, caller, k, tr, result, hf, hec, hef, hxc, hxl, hxf, hs⟩ := h
  exact ⟨dst, result, caller, k, hxl, hxf⟩

/-- A complete C1 call has exactly the frame law supplied by its callee contract. -/
theorem c1_call_frame {O : Exec.Operands} {f : FnId} {P : Exec.Program O}
    {project : O.Store → State} (contract : C1CallContract O f P project)
    {entry exit : Exec.Config O} (h : C1Call O f P entry exit) :
    contract.footprint.Preserves (project entry.store) (project exit.store) :=
  contract.frame h

namespace ABI

/-- A returning ABI call restores every callee-saved register exactly. -/
theorem callee_saved_registers_equal {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) :
    ∀ r ∈ sig.calleeSaved, after.regs r = before.regs r :=
  h.preserved.1

/-- Registers outside the clobber set are unchanged; equivalently, clobbers are the only registers
permitted to differ after return. -/
theorem clobbers_only_changed_registers {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) :
    ∀ r, r ∉ sig.clobbers → after.regs r = before.regs r :=
  h.clobbers

/-- The caller's stack slots below its stack pointer are restored exactly. -/
theorem caller_stack_slots_equal {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) :
    ∀ slot, slot < before.stackPointer → after.slots slot = before.slots slot :=
  h.preserved.2.1

/-- Returning restores the caller's stack pointer exactly. -/
theorem stack_pointer_restored {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) : after.stackPointer = before.stackPointer :=
  h.preserved.2.2

/-- Typed arguments occupy their signature locations at entry. -/
theorem typed_arguments_at_entry {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) : ArgumentsHold sig.encode sig.arguments args before :=
  h.arguments

/-- The typed result occupies its return-convention locations after return. -/
theorem typed_result_at_return {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    {before after : Machine Reg Word} {args : List Val} {result : Val}
    (h : sig.Call before after args result) : sig.result.Holds sig.encode result after :=
  h.result

/-- A well-formed signature's frame size is a multiple of its positive power-of-two alignment. -/
theorem frame_size_aligned {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    (h : sig.WellFormed) : IsPow2 sig.frameAlign ∧ sig.frameSize % sig.frameAlign = 0 :=
  ⟨h.1, h.2.1⟩

/-- The callee's own frame is included in its final-artifact stack bound. -/
theorem frame_within_stack_bound {Ty Val Reg Word : Type} {sig : Signature Ty Val Reg Word}
    (h : sig.WellFormed) : sig.frameSize ≤ sig.stackBound :=
  h.2.2.1

/-- Stack accounting of a call is exactly caller frame plus callee bound. -/
theorem call_stack_account {Ty Val Reg Word : Type} (callerFrame : Nat)
    (sig : Signature Ty Val Reg Word) : callStack callerFrame sig = callerFrame + sig.stackBound :=
  rfl

/-- Whole-artifact accounting adds launcher/runtime bytes to that same handoff. -/
theorem artifact_stack_account {Ty Val Reg Word : Type} (launcherRuntime callerFrame : Nat)
    (sig : Signature Ty Val Reg Word) :
    artifactStack launcherRuntime callerFrame sig =
      launcherRuntime + callerFrame + sig.stackBound := by
  simp [artifactStack, callStack, Nat.add_assoc]

/-- A kept receipt has exactly one retention transition: the paired completion returns the same
handle to Concrete ownership. -/
theorem kept_only_paired_completion {F H : Type} (pair : KeptPair F H) (h : H)
    (event : ForeignEvent F H) (after : RetentionState H)
    (hs : KeptStep pair (.kept h) event after) :
    event = .call pair.complete h ∧ after = .owned pair.handle := by
  cases event with
  | call f h' =>
    cases after with
    | owned h'' =>
      obtain ⟨hf, hh, hh', hh''⟩ := hs
      cases hf
      cases hh
      cases hh'
      cases hh''
      exact ⟨rfl, rfl⟩
    | kept h'' => cases hs

/-- Submission of an owned handle calls the paired submitting extern and produces a receipt for the
same handle. -/
theorem submit_produces_paired_receipt {F H : Type} (pair : KeptPair F H) (h : H)
    (event : ForeignEvent F H) (after : RetentionState H)
    (hs : KeptStep pair (.owned h) event after) :
    event = .call pair.submit h ∧ after = .kept pair.handle := by
  cases event with
  | call f h' =>
    cases after with
    | owned h'' => cases hs
    | kept h'' =>
      obtain ⟨hf, hh, hh', hh''⟩ := hs
      cases hf
      cases hh
      cases hh'
      cases hh''
      exact ⟨rfl, rfl⟩

/-- No other extern can consume a kept receipt. -/
theorem kept_receipt_rejects_other_extern {F H : Type} (pair : KeptPair F H) (h h' : H)
    (f : F) (after : RetentionState H) (hf : f ≠ pair.complete) :
    ¬ KeptStep pair (.kept h) (.call f h') after := by
  intro hs
  have he := (kept_only_paired_completion pair h (.call f h') after hs).1
  cases he
  exact hf rfl

/-- A reservation below the final artifact bound refuses startup with status 125. -/
theorem stack_under_reservation {Input : Type} (bound reserved : Nat) (input : Input)
    (h : reserved < bound) : stackAdmission bound reserved input = .refused125 := by
  simp [stackAdmission, h]

/-- A reservation at least the bound admits the same launcher input. -/
theorem stack_adequate {Input : Type} (bound reserved : Nat) (input : Input)
    (h : bound ≤ reserved) : stackAdmission bound reserved input = .entered input := by
  simp [stackAdmission, Nat.not_lt_of_ge h]

/-- A launcher run relates its exact retained closure to the stated entry and exit observations. -/
theorem launcher_entry_exit {Artifact Input Result Observation : Type}
    {l : Launcher Artifact Input Result Observation} {artifact : Artifact} {reserved : Nat}
    {input : Input} {result : Result} {observation : Observation}
    (run : l.Run artifact reserved input result observation) :
    l.retainedClosure artifact ∧ l.entry artifact input ∧ l.exit artifact result observation :=
  ⟨run.closure, run.entry, run.exit⟩

/-- Every launched run passes stack admission with its recorded reservation. -/
theorem launcher_run_admitted {Artifact Input Result Observation : Type}
    {l : Launcher Artifact Input Result Observation} {artifact : Artifact} {reserved : Nat}
    {input : Input} {result : Result} {observation : Observation}
    (run : l.Run artifact reserved input result observation) :
    stackAdmission (l.stackBound artifact) reserved input = .entered input :=
  stack_adequate _ _ _ run.adequateStack

end ABI

end ConcreteSpec.Core.Frame

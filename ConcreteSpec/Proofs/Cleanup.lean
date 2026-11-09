import ConcreteSpec.Proofs.Basics

/-!
# Part 3: `defer_cleanup_releases`, every exit runs what is registered for the scopes it leaves, in order.

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Cleanup

/-- Helper (inversion). Restrict the dependency-order derivation to the left append segment. (approved: REV-base1b-agy-01025713b). -/
theorem depOrder_append_left : ∀ {l₁ l₂ : List Deferred}, DepOrder (l₁ ++ l₂) → DepOrder l₁
  | [], _, _ => .nil
  | d :: l₁, l₂, h => by
    cases h with
    | cons hs hall hrest =>
      exact .cons hs (fun d' hd' => hall d' (List.mem_append_left _ hd'))
        (depOrder_append_left hrest)

/-- Helper (inversion). Restrict the dependency-order derivation to the right append segment. (approved: REV-base1b-agy-01025713b). -/
theorem depOrder_append_right : ∀ {l₁ l₂ : List Deferred}, DepOrder (l₁ ++ l₂) → DepOrder l₂
  | [], _, h => h
  | _ :: l₁, _, h => by cases h with | cons _ _ hrest => exact depOrder_append_right hrest

/-- Representation lemma. The defining flattening equation for a stack frame. (approved: REV-base1b-agy-01025713b). -/
theorem pending_cons (f : Frame) (st : Stack) : pending (f :: st) = f.defers ++ pending st := by
  simp [pending]

/-- Representation lemma. Flattening distributes over stack concatenation. (approved: REV-base1b-agy-01025713b). -/
theorem pending_append (fs st : List Frame) : pending (fs ++ st) = pending fs ++ pending st := by
  simp [pending]

/-- Helper (inversion). Extract the cross-segment dependency constraint from the ordering derivation. (approved: REV-base1b-agy-01025713b). -/
theorem depOrder_cross : ∀ {l₁ l₂ : List Deferred}, DepOrder (l₁ ++ l₂) →
    ∀ x ∈ l₁, ∀ y ∈ l₂, x.target ∉ y.needs
  | [], _, _, x, hx, _, _ => by cases hx
  | d :: l₁, l₂, h, x, hx, y, hy => by
    cases h with
    | cons _ hall hrest =>
      rcases List.mem_cons.mp hx with rfl | hx
      · exact hall y (List.mem_append_right _ hy)
      · exact depOrder_cross hrest x hx y hy

/-- What registration guarantees about the pending calls: they are dependency-ordered, each
resource is consumed at most once and is no longer owned, and everything a call needs is owned or
consumed by a later-running call. -/
def RegInv (live : Res → Bool) (st : Stack) : Prop :=
  DepOrder (pending st) ∧ (targets (pending st)).Nodup ∧
  (∀ d ∈ pending st, live d.target = false) ∧
  (∀ d ∈ pending st, ∀ r ∈ d.needs, live r = true ∨ r ∈ targets (pending st))

/-- Law (subject `ConcreteSpec.Cleanup.Reg`). -/
theorem reg_inv {live : Res → Bool} {st : Stack} (h : Reg live st) : RegInv live st := by
  induction h with
  | empty => exact ⟨.nil, .nil, by simp [pending], by simp [pending]⟩
  | enter _ ih => simpa [RegInv, pending_cons] using ih
  | @push live k ds st d _ hlive hself hneeds hall ih =>
    obtain ⟨hdo, hnd, hdead, hav⟩ := ih
    rw [pending_cons] at hdo hnd hdead hav hneeds hall
    refine ⟨?_, ?_, ?_, ?_⟩
    · rw [pending_cons]; exact .cons hself hall hdo
    · rw [pending_cons]
      simp only [targets, List.map_cons, List.cons_append] at hnd ⊢
      refine List.nodup_cons.mpr ⟨?_, hnd⟩
      intro hm
      obtain ⟨d', hd', he⟩ := List.mem_map.mp hm
      have := hdead d' hd'; rw [he, hlive] at this; cases this
    · rw [pending_cons]
      intro d' hd'
      simp only [List.cons_append, List.mem_cons] at hd'
      rcases hd' with rfl | hd'
      · simp
      · simp [hdead d' hd']
    · rw [pending_cons]
      intro d' hd' r hr
      simp only [List.cons_append, List.mem_cons] at hd'
      have step : ∀ r, (live r = true ∨ r ∈ targets (ds ++ pending st)) →
          ((live r && r != d.target) = true ∨ r ∈ targets (d :: (ds ++ pending st))) := by
        intro r hr
        by_cases hrd : r = d.target
        · right; subst hrd; simp [targets]
        · rcases hr with hr | hr
          · left; simp [hr, hrd]
          · right; simp only [targets, List.map_cons, List.mem_cons]; right; exact hr
      simp only [List.cons_append] at step ⊢
      rcases hd' with rfl | hd'
      · exact step r (hneeds r hr)
      · exact step r (hav d' hd' r hr)
  | @leave live f st _ ih =>
    obtain ⟨hdo, hnd, hdead, hav⟩ := ih
    rw [pending_cons] at hdo hnd hdead hav
    refine ⟨depOrder_append_right hdo, ?_, fun d hd => hdead d (List.mem_append_right _ hd), ?_⟩
    · simp only [targets, List.map_append] at hnd
      exact hnd.sublist (List.sublist_append_right _ _)
    · intro d hd r hr
      rcases hav d (List.mem_append_right _ hd) r hr with h | h
      · exact .inl h
      · right
        simp only [targets, List.map_append, List.mem_append] at h
        rcases h with h | h
        · obtain ⟨x, hx, rfl⟩ := List.mem_map.mp h
          exact absurd hr (depOrder_cross hdo x hx d hd)
        · exact h

/-- Helper (inversion). Decompose a successful recursive unwind into the removed prefix and retained suffix. (approved: REV-base1b-agy-01025713b). -/
theorem unwindTo_prefix {p stop : FrameKind → Bool} :
    ∀ {st : Stack} {fs : List Frame}, unwindTo p stop st = some fs → ∃ rest, st = fs ++ rest
  | [], fs, h => by simp [unwindTo] at h
  | f :: st, fs, h => by
    simp only [unwindTo] at h
    split at h
    · cases h; exact ⟨st, rfl⟩
    split at h
    · cases h
    · cases hu : unwindTo p stop st with
      | none => rw [hu] at h; cases h
      | some fs' =>
        rw [hu] at h
        cases h
        obtain ⟨rest, rfl⟩ := unwindTo_prefix hu
        exact ⟨rest, rfl⟩

/-- Helper (inversion). Extract the stack-prefix decomposition from a successful exit unwind. The conclusion contains no mutable operation; it is used to restrict the registration invariant to the exited scopes. (approved: REV-base1b-agy-01025713b). An exit leaves a prefix of the scopes: the innermost ones. -/
theorem unwound_prefix {e : Exit} {st : Stack} {fs : List Frame} (h : unwound e st = some fs) :
    ∃ rest, st = fs ++ rest := by
  cases e with
  | fallthrough =>
    cases st with
    | nil => simp [unwound] at h
    | cons f st => simp [unwound] at h; subst h; exact ⟨st, rfl⟩
  | brk l => exact unwindTo_prefix h
  | cont l => exact unwindTo_prefix h
  | ret => exact unwindTo_prefix h
  | abort => simp [unwound] at h; subst h; exact ⟨st, rfl⟩

/-- Law (subject `ConcreteSpec.Cleanup.RunCleanup`). -/
theorem runCleanup_all_ok {ok : Deferred → Bool} :
    ∀ {ds : List Deferred}, (∀ d ∈ ds, ok d = true) →
      ∀ tr r, RunCleanup ok ds tr r ↔ tr = expected ds ∧ r = .done
  | [], _, tr, r => by
    constructor
    · intro h; cases h; exact ⟨rfl, rfl⟩
    · rintro ⟨rfl, rfl⟩; exact .nil
  | d :: ds, hok, tr, r => by
    have hd : ok d = true := hok d (List.Mem.head _)
    have ih := runCleanup_all_ok (ds := ds) (fun x hx => hok x (List.Mem.tail _ hx))
    constructor
    · intro h
      cases h with
      | step _ h' =>
        obtain ⟨rfl, rfl⟩ := (ih _ _).mp h'
        exact ⟨by simp [expected], rfl⟩
      | trap hf => rw [hd] at hf; cases hf
    · rintro ⟨rfl, rfl⟩
      have := RunCleanup.step (ok := ok) hd ((ih (expected ds) .done).mpr ⟨rfl, rfl⟩)
      simpa [expected] using this

/-- Law (subject `ConcreteSpec.Cleanup.RunCleanup`). -/
theorem runCleanup_trap {ok : Deferred → Bool} {d : Deferred} {post : List Deferred} :
    ∀ {pre : List Deferred}, (∀ x ∈ pre, ok x = true) → ok d = false →
      ∀ tr r, RunCleanup ok (pre ++ d :: post) tr r ↔ tr = expected pre ++ [.call d] ∧ r = .trapped
  | [], _, hd, tr, r => by
    simp only [List.nil_append, expected, List.flatMap_nil]
    constructor
    · intro h
      cases h with
      | step h' _ => rw [hd] at h'; cases h'
      | trap _ => exact ⟨rfl, rfl⟩
    · rintro ⟨rfl, rfl⟩; exact .trap hd
  | x :: pre, hok, hd, tr, r => by
    have hx : ok x = true := hok x (List.Mem.head _)
    have ih := runCleanup_trap (post := post) (pre := pre) (fun y hy => hok y (List.Mem.tail _ hy)) hd
    simp only [List.cons_append]
    constructor
    · intro h
      cases h with
      | step _ h' =>
        obtain ⟨rfl, rfl⟩ := (ih _ _).mp h'
        exact ⟨by simp [expected], rfl⟩
      | trap hf => rw [hx] at hf; cases hf
    · rintro ⟨rfl, rfl⟩
      have := RunCleanup.step (ok := ok) hx ((ih (expected pre ++ [.call d]) .trapped).mpr ⟨rfl, rfl⟩)
      simpa [expected] using this

/-- Helper (inversion). Locate the two calls in the order mandated by the dependency-order derivation. (approved: REV-base1b-agy-01025713b). In a dependency-ordered run list, a call runs before the release of an allocator it depends
on. -/
theorem depOrder_before :
    ∀ {ds : List Deferred}, DepOrder ds → ∀ {d da : Deferred}, d ∈ ds → da ∈ ds →
      da.target ∈ d.needs → ∃ pre mid post, ds = pre ++ d :: mid ++ da :: post
  | [], _, _, _, hd, _, _ => by cases hd
  | x :: ds, .cons hself hall hrest, d, da, hd, hda, hdep => by
    by_cases hxa : x = da
    · subst hxa
      by_cases hxd : x = d
      · subst hxd; exact absurd hdep hself
      · have hd' : d ∈ ds := by
          cases hd with
          | head => exact absurd rfl hxd
          | tail _ h => exact h
        exact absurd hdep (hall d hd')
    · have hda' : da ∈ ds := by
        cases hda with
        | head => exact absurd rfl hxa
        | tail _ h => exact h
      by_cases hxd : x = d
      · subst hxd
        obtain ⟨mid, post, hsplit⟩ := List.append_of_mem hda'
        exact ⟨[], mid, post, by simp [hsplit]⟩
      · have hd' : d ∈ ds := by
          cases hd with
          | head => exact absurd rfl hxd
          | tail _ h => exact h
        obtain ⟨pre, mid, post, hsplit⟩ := depOrder_before hrest hd' hda' hdep
        exact ⟨x :: pre, mid, post, by simp [hsplit]⟩

/-- Helper (inversion). Extract inequality of an earlier target and a later dependency. (approved: REV-base1b-agy-01025713b). In a dependency-ordered run list, no call consumes what a later call needs. -/
theorem depOrder_needs_unconsumed {pre post : List Deferred} {d : Deferred}
    (h : DepOrder (pre ++ d :: post)) : ∀ r ∈ d.needs, ∀ x ∈ pre, x.target ≠ r := by
  intro r hr x hx he
  subst he
  exact depOrder_cross h x hx d (List.mem_cons_self ..) hr

/-- Law (subject `ConcreteSpec.Cleanup.Reg`, `ConcreteSpec.Cleanup.RunCleanup`). **defer_cleanup_releases** (Q9.14, Q9.8, Q1.5). For every cleanup stack a block can build
from the resources it owns, and every structured exit (falling through, break L, continue L,
return), the calls of the scopes the exit leaves:

1. when every call returns, run exactly in reverse registration order, innermost scope first,
   each call immediately followed by the release of each loan it held (expected);
2. keep the allocator-dependency check: a call runs before the call that consumes anything it
   needs (an allocator it depends on, or a loan's source);
3. when a call traps, stop there: the earlier calls ran with their loan releases, the trapping
   call is the last event, and no later cleanup runs;
4. consume each resource at most once;
5. find everything a call needs unconsumed when the call and its loan releases run: no earlier
   call consumed it, and it was owned or is consumed by a later-running call. -/
theorem defer_cleanup_releases {ok : Deferred → Bool} {live : Res → Bool} {st : Stack}
    (hreg : Reg live st) {e : Exit} (he : e ≠ .abort) {fs : List Frame}
    (hu : unwound e st = some fs) :
    ((∀ d ∈ pending fs, ok d = true) →
      ∀ tr r, RunCleanup ok (pending fs) tr r ↔ tr = expected (pending fs) ∧ r = .done) ∧
    (∀ d da, d ∈ pending fs → da ∈ pending fs → da.target ∈ d.needs →
      ∃ t₁ t₂ t₃, expected (pending fs) = t₁ ++ .call d :: t₂ ++ .call da :: t₃) ∧
    (∀ pre d post, pending fs = pre ++ d :: post → (∀ x ∈ pre, ok x = true) → ok d = false →
      ∀ tr r, RunCleanup ok (pending fs) tr r ↔ tr = expected pre ++ [.call d] ∧ r = .trapped) ∧
    (targets (pending fs)).Nodup ∧
    (∀ pre d post, pending fs = pre ++ d :: post → ∀ r ∈ d.needs,
      (∀ x ∈ pre, x.target ≠ r) ∧ (live r = true ∨ r ∈ targets (pending st))) := by
  have _ := he
  obtain ⟨rest, hst⟩ := unwound_prefix hu
  obtain ⟨hdo0, hnd0, _, hav⟩ := reg_inv hreg
  have hdo : DepOrder (pending fs) := by
    have := hdo0
    rw [hst, pending_append] at this
    exact depOrder_append_left this
  refine ⟨fun hok => runCleanup_all_ok hok, ?_, ?_, ?_, ?_⟩
  · intro d da hd hda hdep
    obtain ⟨pre, mid, post, hsplit⟩ := depOrder_before hdo hd hda hdep
    refine ⟨expected pre, d.loans.map .release ++ expected mid,
      da.loans.map .release ++ expected post, ?_⟩
    rw [hsplit]
    simp [expected, List.flatMap_append]
  · intro pre d post hsplit hok hd tr r
    rw [hsplit]
    exact runCleanup_trap hok hd tr r
  · have := hnd0
    rw [hst, pending_append] at this
    simp only [targets, List.map_append] at this ⊢
    exact this.sublist (List.sublist_append_left _ _)
  · intro pre d post hsplit r hr
    refine ⟨depOrder_needs_unconsumed (hsplit ▸ hdo) r hr, hav d ?_ r hr⟩
    rw [hst, pending_append, hsplit]
    exact List.mem_append_left _ (List.mem_append_right _ (List.mem_cons_self ..))

/-- Representation lemma. The defining abort branch unwinds no scopes, whose flattened pending list is empty. (approved: REV-base1b-agy-01025713b). An abort runs no cleanup (Q9.8). -/
theorem abort_no_cleanup (st : Stack) : unwound .abort st = some [] ∧ pending [] = [] := ⟨rfl, rfl⟩


namespace Evidence

/-- Instance (law `ConcreteSpec.Cleanup.reg_inv`, case `registeredStack`). -/
example := reg_inv stack_reg

/-- Instance (law `ConcreteSpec.Cleanup.runCleanup_all_ok`, case `releaseLoan`). -/
example := @runCleanup_all_ok (fun _ => true) [desV] (by intro d hd; rfl) (expected [desV]) .done

/-- Instance (law `ConcreteSpec.Cleanup.runCleanup_trap`, case `secondCallTraps`). -/
example := @runCleanup_trap (fun d => d != desB) desB [relA] [desV]
  (by intro d hd; cases hd with
      | head => decide
      | tail _ h => cases h) (by decide) (expected [desV] ++ [.call desB]) .trapped

/-- Instance (law `ConcreteSpec.Cleanup.defer_cleanup_releases`, case `returnStack`). -/
example := defer_cleanup_releases (ok := fun _ => true) stack_reg (e := .ret) (by decide)
  (fs := stack) rfl

end Evidence

end ConcreteSpec.Cleanup

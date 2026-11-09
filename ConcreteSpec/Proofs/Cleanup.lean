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

/-- Law (subject `ConcreteSpec.Cleanup.Reg`). Cleanup of exited scopes calls each dependent
before the allocator or loan source it needs. This applies also to abort's empty prefix. -/
theorem defer_cleanup_releases_before {live : Res → Bool} {st : Stack}
    (hreg : Reg live st) {e : Exit} {fs : List Frame}
    (hu : unwound e st = some fs) {d da : Deferred}
    (hd : d ∈ pending fs) (hda : da ∈ pending fs) (hdep : da.target ∈ d.needs) :
    ∃ t₁ t₂ t₃, expected (pending fs) = t₁ ++ .call d :: t₂ ++ .call da :: t₃ := by
  obtain ⟨rest, hst⟩ := unwound_prefix hu
  obtain ⟨hdo0, _, _, _⟩ := reg_inv hreg
  rw [hst, pending_append] at hdo0
  have hdo := depOrder_append_left hdo0
  obtain ⟨pre, mid, post, hsplit⟩ := depOrder_before hdo hd hda hdep
  refine ⟨expected pre, d.loans.map .release ++ expected mid,
    da.loans.map .release ++ expected post, ?_⟩
  rw [hsplit]
  simp [expected, List.flatMap_append]

/-- Law (subject `ConcreteSpec.Cleanup.Reg`). Exiting scopes never consumes a resource twice.
Abort unwinds no frames, so it consumes none. -/
theorem defer_cleanup_releases_nodup {live : Res → Bool} {st : Stack}
    (hreg : Reg live st) {e : Exit} {fs : List Frame}
    (hu : unwound e st = some fs) : (targets (pending fs)).Nodup := by
  obtain ⟨rest, hst⟩ := unwound_prefix hu
  obtain ⟨_, hnd, _, _⟩ := reg_inv hreg
  rw [hst, pending_append] at hnd
  simp only [targets, List.map_append] at hnd ⊢
  exact hnd.sublist (List.sublist_append_left _ _)

/-- Law (subject `ConcreteSpec.Cleanup.Reg`). A call's dependencies remain unconsumed by
all preceding calls and are either owned or scheduled for cleanup. -/
theorem defer_cleanup_releases_needs {live : Res → Bool} {st : Stack}
    (hreg : Reg live st) {e : Exit} {fs : List Frame}
    (hu : unwound e st = some fs) {pre post : List Deferred} {d : Deferred}
    (hsplit : pending fs = pre ++ d :: post) {r : Res} (hr : r ∈ d.needs) :
    (∀ x ∈ pre, x.target ≠ r) ∧ (live r = true ∨ r ∈ targets (pending st)) := by
  obtain ⟨rest, hst⟩ := unwound_prefix hu
  obtain ⟨hdo0, _, _, hav⟩ := reg_inv hreg
  have hdo : DepOrder (pending fs) := by
    rw [hst, pending_append] at hdo0
    exact depOrder_append_left hdo0
  refine ⟨depOrder_needs_unconsumed (hsplit ▸ hdo) r hr, hav d ?_ r hr⟩
  rw [hst, pending_append, hsplit]
  exact List.mem_append_left _ (List.mem_append_right _ (List.mem_cons_self ..))

/-! The five cleanup obligations have independent theorem parameters: runCleanup_all_ok and
runCleanup_trap specify returning and trapping execution; the three laws above specify
registration order, unique consumption, and dependency availability. No shared choice of ok
can hide the trapping obligation behind the returning case. The spec's abort branch selects
an empty frame prefix, so these prefix properties require no structured-exit restriction. -/

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

/-- Instance (law `ConcreteSpec.Cleanup.defer_cleanup_releases_before`, case `allocatorDependency`). -/
example := @defer_cleanup_releases_before ownedAfter stack stack_reg .ret stack rfl
  desV relA (by decide) (by decide) (by decide)

/-- Instance (law `ConcreteSpec.Cleanup.defer_cleanup_releases_nodup`, case `returnStack`). -/
example := @defer_cleanup_releases_nodup ownedAfter stack stack_reg .ret stack rfl

/-- Instance (law `ConcreteSpec.Cleanup.defer_cleanup_releases_needs`, case `loanSource`). -/
example := @defer_cleanup_releases_needs ownedAfter stack stack_reg .ret stack rfl
  [] [desB, relA] desV rfl 2 (by decide)

end Evidence

end ConcreteSpec.Cleanup

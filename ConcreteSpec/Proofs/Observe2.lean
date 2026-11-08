import ConcreteSpec.Spec.Observe

/-!
# The laws of observations and the environment (`Spec/Observe.lean`, C6)

The finite observation laws identify executions after removing any number of silent steps. The
infinite law compares every step-indexed visible prefix with an eventually available prefix of the
other execution; in particular, two silently divergent executions may be equivalent, but neither
is equivalent to a finite terminated execution.

The trap-line theorem is proved by decoding the line, not by restricting source paths. Decimal
fields are parsed from right to left, so colons and parentheses in a path cannot make two different
`(kind, site, span)` triples render identically. The only numeric decoder needed is Lean core's
`Nat.ofDigitChars`, whose round trip through `Nat.toDigits` is already proved.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

variable {S : EventSignature}

/-- Helper (inversion). Inverts the same-kind constructor; this is not independent producer correctness. (approved: PENDING)
 Matching responses really do have the request's operation kind. -/
theorem RequestResponse.kind {q : Request S} {r : Response S} (h : RequestResponse q r) :
    q.fst = r.fst := by
  cases h
  rfl

/-- Helper (inversion). Inverts admission; contract compliance must instead be established for an independent producer. (approved: PENDING)
 Every response admitted by the environment relation satisfies its operation contract. -/
theorem Responds.allows {C : OperationContract S} {input : C.Input} {hist : List (Event S)}
    {q : Request S} {r : Response S} (h : Responds C input hist q (.response r)) :
    C.allows input hist q r := by
  cases h with
  | response _ allows => exact allows

/-- Helper (inversion). Inverts admission; this is not independent evidence of correct response routing. (approved: PENDING)
 Every response admitted by the environment relation matches its request. -/
theorem Responds.matches {C : OperationContract S} {input : C.Input} {hist : List (Event S)}
    {q : Request S} {r : Response S} (h : Responds C input hist q (.response r)) :
    RequestResponse q r := by
  cases h with
  | response matching _ => exact matching

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 Inserting silent steps leaves exactly the same visible events, in the same order. -/
theorem SilentInsertion.visible {before after : List (Step S)} (h : SilentInsertion before after) :
    visible before = visible after := by
  induction h with
  | nil => rfl
  | silent _ ih => exact ih
  | keepSilent _ ih => exact ih
  | emit event _ ih => exact congrArg (List.cons event) ih

/-- Law (subject `ConcreteSpec.Core.Observe.visible`).
 Any finite number of silent steps may be inserted anywhere in a finite terminated run. -/
theorem silent_internal_steps {before after : List (Step S)} (h : SilentInsertion before after)
    (terminal : Terminal) : SameObservation (.finite before terminal) (.finite after terminal) := by
  exact ⟨h.visible, rfl⟩

/-- Law (subject `ConcreteSpec.Core.Observe.visible`).
 The same arbitrary finite stuttering preserves a blocked run and its pending request. -/
theorem blocked_silent_internal_steps {before after : List (Step S)}
    (h : SilentInsertion before after) (request : Request S) :
    SameObservation (.blocked before request) (.blocked after request) := by
  exact ⟨h.visible, rfl⟩

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING)
 Any finite visible prefix is immediately a prefix of the same execution's prefix. -/
theorem PrefixEventually.refl (steps : Nat → Step S) : PrefixEventually steps steps := by
  intro n
  exact ⟨n, [], by simp⟩

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING)
 Eventual prefix containment composes; the second execution may need more internal steps. -/
theorem PrefixEventually.trans {xs ys zs : Nat → Step S}
    (hxy : PrefixEventually xs ys) (hyz : PrefixEventually ys zs) : PrefixEventually xs zs := by
  intro n
  obtain ⟨m, rest₁, h₁⟩ := hxy n
  obtain ⟨p, rest₂, h₂⟩ := hyz m
  exact ⟨p, rest₁ ++ rest₂, by rw [h₂, h₁, List.append_assoc]⟩

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING) -/
theorem SameInfiniteObservation.refl (steps : Nat → Step S) : SameInfiniteObservation steps steps :=
  ⟨PrefixEventually.refl steps, PrefixEventually.refl steps⟩

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING) -/
theorem SameInfiniteObservation.symm {xs ys : Nat → Step S}
    (h : SameInfiniteObservation xs ys) : SameInfiniteObservation ys xs := ⟨h.2, h.1⟩

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING) -/
theorem SameInfiniteObservation.trans {xs ys zs : Nat → Step S}
    (hxy : SameInfiniteObservation xs ys) (hyz : SameInfiniteObservation ys zs) :
    SameInfiniteObservation xs zs := ⟨hxy.1.trans hyz.1, hyz.2.trans hxy.2⟩

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 One more step adds exactly its optional event to the visible prefix. -/
theorem visiblePrefix_succ (steps : Nat → Step S) (n : Nat) :
    visiblePrefix steps (n + 1) = visiblePrefix steps n ++ (steps n).event?.toList := by
  cases step : steps n <;>
    simp [visiblePrefix, List.range_succ, visible, List.filterMap_append, Step.event?, step]

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 A shorter step prefix's events are a prefix of a longer step prefix's events. -/
theorem visiblePrefix_prefix (steps : Nat → Step S) {m n : Nat} (h : m ≤ n) :
    ∃ rest, visiblePrefix steps n = visiblePrefix steps m ++ rest := by
  obtain ⟨d, rfl⟩ := Nat.exists_eq_add_of_le h
  induction d with
  | zero => exact ⟨[], by simp⟩
  | succ d ih =>
    obtain ⟨rest, pref⟩ := ih (by omega)
    refine ⟨rest ++ (steps (m + d)).event?.toList, ?_⟩
    rw [show m + (d + 1) = m + d + 1 by omega, visiblePrefix_succ, pref, List.append_assoc]

/-- Helper (arithmetic). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 A strictly increasing step-index map is monotone on every pair of indices. -/
theorem StepIndexIncreasing.monotone {f : Nat → Nat} (increasing : StepIndexIncreasing f)
    {m n : Nat} (h : m ≤ n) : f m ≤ f n := by
  induction n generalizing m with
  | zero => simp [show m = 0 by omega]
  | succ n ih =>
    by_cases atEnd : m = n + 1
    · subst m; exact Nat.le_refl _
    · exact Nat.le_trans (ih (by omega)) (Nat.le_of_lt (increasing n))

/-- Helper (arithmetic). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 A strictly increasing step-index map strictly increases on every ordered pair. -/
theorem StepIndexIncreasing.strict {f : Nat → Nat} (increasing : StepIndexIncreasing f)
    {m n : Nat} (h : m < n) : f m < f n := by
  have first := increasing m
  have rest := increasing.monotone (show m + 1 ≤ n by omega)
  omega

/-- Helper (arithmetic). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 A strictly increasing map on natural step indices is unbounded. -/
theorem StepIndexIncreasing.apply_le {f : Nat → Nat} (increasing : StepIndexIncreasing f) (n : Nat) :
    n ≤ f n := by
  induction n with
  | zero => exact Nat.zero_le _
  | succ n ih => have next := increasing n; omega

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 At every stuttered index `m`, some old prefix `n` has exactly the same events: all retained
indices below `m` are below `n`, and `f n` is the next retained index at or after `m`. -/
theorem InfiniteStuttering.prefix_indices {steps : Nat → Step S} {f : Nat → Nat}
    {stutter : Nat → Step S} (h : InfiniteStuttering steps f stutter) (m : Nat) :
    ∃ n, n ≤ m ∧ m ≤ f n ∧ visiblePrefix stutter m = visiblePrefix steps n ∧
      ∀ i < n, f i < m := by
  obtain ⟨increasing, kept, silent⟩ := h
  induction m with
  | zero => exact ⟨0, Nat.le_refl _, Nat.zero_le _, rfl, by intro i hi; omega⟩
  | succ m ih =>
    obtain ⟨n, oldBound, nextBound, pref, before⟩ := ih
    by_cases atIndex : f n = m
    · refine ⟨n + 1, by omega, ?_, ?_, ?_⟩
      · have next := increasing n
        omega
      · rw [visiblePrefix_succ, visiblePrefix_succ, pref]
        have retained : stutter m = steps n := by rw [← atIndex]; exact kept n
        rw [retained]
      · intro i hi
        by_cases same : i = n
        · subst i; omega
        · have earlier := before i (by omega)
          omega
    · refine ⟨n, by omega, by omega, ?_, ?_⟩
      · rw [visiblePrefix_succ]
        have noIndex : ∀ i, f i ≠ m := by
          intro i
          by_cases earlier : i < n
          · have index := before i earlier
            omega
          · by_cases same : i = n
            · subst i; exact atIndex
            · have later : n < i := by omega
              have index := increasing.strict later
              omega
        rw [silent m noIndex]
        simpa [Step.event?] using pref
      · intro i hi
        have index := before i hi
        omega

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 The stuttered prefix through the position just before `f n` is exactly the old prefix before
`n`; leading and intermediate silent steps contribute nothing. -/
theorem InfiniteStuttering.visiblePrefix_at {steps : Nat → Step S} {f : Nat → Nat}
    {stutter : Nat → Step S} (h : InfiniteStuttering steps f stutter) (n : Nat) :
    visiblePrefix stutter (f n) = visiblePrefix steps n := by
  obtain ⟨k, oldBound, nextBound, pref, before⟩ := h.prefix_indices (f n)
  have same : k = n := by
    apply Nat.le_antisymm
    · by_cases le : k ≤ n
      · exact le
      · have later : n < k := by omega
        have impossible := before n later
        omega
    · by_cases le : n ≤ k
      · exact le
      · have earlier : k < n := by omega
        have index := h.1.strict earlier
        omega
  rw [same] at pref
  exact pref

/-- Law (subject `ConcreteSpec.Core.Observe.visiblePrefix`).
 **Infinite stuttering.** Retaining step `n` at `f n` for a strictly increasing `f`, with every
other step silent, preserves the complete observation in both eventual-prefix directions. This
also covers runs that emit only finitely often and then remain silent forever. -/
theorem infinite_stuttering {steps : Nat → Step S} {f : Nat → Nat} {stutter : Nat → Step S}
    (h : InfiniteStuttering steps f stutter) :
    SameObservation (.infinite steps) (.infinite stutter) := by
  change SameInfiniteObservation steps stutter
  constructor
  · intro n
    exact ⟨f n, [], by simp [h.visiblePrefix_at]⟩
  · intro m
    obtain ⟨n, oldBound, nextBound, pref, before⟩ := h.prefix_indices m
    obtain ⟨rest, extended⟩ := visiblePrefix_prefix steps oldBound
    exact ⟨m, rest, by rw [extended, pref]⟩

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING)
 Full-observation equivalence is reflexive. -/
theorem SameObservation.refl (execution : Execution S) : SameObservation execution execution := by
  cases execution with
  | finite steps terminal => exact ⟨rfl, rfl⟩
  | blocked steps request => exact ⟨rfl, rfl⟩
  | infinite steps => exact SameInfiniteObservation.refl steps

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING)
 Full-observation equivalence is symmetric. -/
theorem SameObservation.symm {x y : Execution S} (h : SameObservation x y) : SameObservation y x := by
  cases x <;> cases y <;> simp only [SameObservation] at h ⊢
  all_goals first | exact h.elim | exact ⟨h.1.symm, h.2.symm⟩ | exact h.symm

/-- Representation lemma. Algebra of the observation representation; not independent semantic soundness evidence. (approved: PENDING)
 Full-observation equivalence is transitive. -/
theorem SameObservation.trans {x y z : Execution S}
    (hxy : SameObservation x y) (hyz : SameObservation y z) : SameObservation x z := by
  cases x <;> cases y <;> cases z <;> simp only [SameObservation] at hxy hyz ⊢
  all_goals first | exact hxy.elim | exact hyz.elim |
    exact ⟨hxy.1.trans hyz.1, hxy.2.trans hyz.2⟩ | exact hxy.trans hyz

/-- Representation lemma. Relational transport of a represented observation, not execution or final-store adequacy. (approved: PENDING)
 **Full-observation preservation.** An observation projected from `x` is also an observation of
every `y` equivalent to `x`. Finite runs preserve their events and terminal action, blocked runs
also preserve their pending request, and infinite runs transport both eventually silent and
infinitely emitting histories. No infinite run is related to a finite termination. -/
theorem full_observation_preservation {x y : Execution S} {observation : FullObservation S}
    (h : SameObservation x y) (hx : HasFullObservation x observation) :
    HasFullObservation y observation := by
  cases x with
  | finite xs terminal =>
    cases y with
    | finite ys terminal' =>
      cases observation with
      | terminal observation =>
        simp only [HasFullObservation] at hx ⊢
        exact hx.trans (by rw [h.1, h.2])
      | blocked events request => exact hx.elim
      | finiteDivergence events => exact hx.elim
      | infiniteDivergence events => exact hx.elim
    | blocked ys request => exact h.elim
    | infinite ys => exact h.elim
  | blocked xs request =>
    cases y with
    | blocked ys request' =>
      cases observation with
      | blocked events request'' =>
        exact ⟨by rw [← h.1, ← h.2]; exact hx.1, h.2.symm.trans hx.2⟩
      | terminal observation => exact hx.elim
      | finiteDivergence events => exact hx.elim
      | infiniteDivergence events => exact hx.elim
    | finite ys terminal => exact h.elim
    | infinite ys => exact h.elim
  | infinite xs =>
    cases y with
    | infinite ys =>
      cases observation with
      | finiteDivergence events => exact SameInfiniteObservation.trans (SameInfiniteObservation.symm h) hx
      | infiniteDivergence events => exact SameInfiniteObservation.trans (SameInfiniteObservation.symm h) hx
      | terminal observation => exact hx.elim
      | blocked events request => exact hx.elim
    | finite ys terminal => exact h.elim
    | blocked ys request => exact h.elim

/-- Representation lemma. Distinguishes observation constructors; it does not validate an execution transformation. (approved: PENDING)
 No infinite execution, including one that eventually becomes silent forever, is equivalent to
any finite successful or trapped termination. -/
theorem infinite_not_termination (steps : Nat → Step S) (finiteSteps : List (Step S))
    (terminal : Terminal) :
    ¬ SameObservation (.infinite steps) (.finite finiteSteps terminal) := by
  simp [SameObservation]

/-- Representation lemma. Distinguishes observation constructors; operational transformation evidence is separate. (approved: PENDING)
 In particular, an infinite silent run never acquires a terminal observation. -/
theorem infinite_silent_not_termination (terminal : Terminal) :
    ¬ SameObservation (.infinite (fun _ => Step.silent)) (.finite (S := S) [] terminal) :=
  infinite_not_termination _ [] terminal

/-! ## Decoding the exact trap line -/

private def stripPrefix : List Char → List Char → Option (List Char)
  | [], rest => some rest
  | _ :: _, [] => none
  | c :: cs, d :: ds => if c = d then stripPrefix cs ds else none

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING) -/
private theorem stripPrefix_append (pre rest : List Char) :
    stripPrefix pre (pre ++ rest) = some rest := by
  induction pre with
  | nil => rfl
  | cons c cs ih => simp [stripPrefix, ih]

/-- Parse a decimal at the start of a reversed line, returning the value and the unused suffix. -/
private def parseRevNat (cs : List Char) : Option (Nat × List Char) :=
  let digits := cs.takeWhile Char.isDigit
  if digits.isEmpty then none
  else some (Nat.ofDigitChars 10 digits.reverse 0, cs.dropWhile Char.isDigit)

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING) -/
private theorem parseRevNat_digits (n : Nat) (c : Char) (rest : List Char)
    (hc : c.isDigit = false) :
    parseRevNat ((Nat.toDigits 10 n).reverse ++ c :: rest) = some (n, c :: rest) := by
  have hd : ∀ d ∈ (Nat.toDigits 10 n).reverse, d.isDigit := by
    intro d hd
    exact Nat.isDigit_of_mem_toDigits (b := 10) (n := n) (by decide) (by decide) (by simpa using hd)
  simp [parseRevNat, List.takeWhile_append_of_pos hd, List.dropWhile_append_of_pos hd, hc]

private def linePrefix (kind : TrapKind) : List Char :=
  "concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList

private def decodeKinds : List TrapKind → List Char → Option TrapKind
  | [], _ => none
  | kind :: rest, cs => match stripPrefix (linePrefix kind) cs with
    | some _ => some kind
    | none => decodeKinds rest cs

private def decodeKind (cs : List Char) : Option TrapKind := decodeKinds TrapKind.all cs

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING) -/
private theorem decodeKind_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeKind (trapLineChars kind site sp) = some kind := by
  cases kind <;>
    simp [decodeKind, decodeKinds, TrapKind.all, linePrefix, trapLineChars, TrapKind.name,
      stripPrefix, List.append_assoc]

private def decodeWithKind (kind : TrapKind) (cs : List Char) : Option (SiteId × Span) := do
  let rest ← stripPrefix (linePrefix kind) cs
  let rest ← stripPrefix ")".toList rest.reverse
  let (site, rest) ← parseRevNat rest
  let rest ← stripPrefix " (site ".toList.reverse rest
  let (col, rest) ← parseRevNat rest
  let rest ← stripPrefix ":".toList rest
  let (line, rest) ← parseRevNat rest
  let rest ← stripPrefix ":".toList rest
  if hline : 0 < line then
    if hcol : 0 < col then
      some (⟨site⟩, ⟨String.ofList rest.reverse, line, col, hline, hcol⟩)
    else none
  else none

private def decodeTrapLine (line : String) : Option (TrapKind × SiteId × Span) := do
  let kind ← decodeKind line.toList
  let (site, sp) ← decodeWithKind kind line.toList
  some (kind, site, sp)

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING) -/
private theorem trapLineChars_prefix (kind : TrapKind) (site : SiteId) (sp : Span) :
    trapLineChars kind site sp = linePrefix kind ++
      (sp.path.toList ++ ":".toList ++ (toString sp.line).toList ++ ":".toList ++
        (toString sp.col).toList ++ " (site ".toList ++ (toString site.id).toList ++ ")".toList) := by
  simp [trapLineChars, linePrefix, List.append_assoc]

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING) -/
private theorem decodeWithKind_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeWithKind kind (trapLineChars kind site sp) = some (site, sp) := by
  have hline : 0 < sp.line := sp.line_pos
  have hcol : 0 < sp.col := sp.col_pos
  unfold decodeWithKind
  rw [trapLineChars_prefix, stripPrefix_append]
  simp [List.reverse_append, stripPrefix, parseRevNat_digits, hline, hcol]

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING) -/
private theorem decodeTrapLine_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeTrapLine (trapLine kind site sp) = some (kind, site, sp) := by
  simp [decodeTrapLine, trapLine, decodeKind_render, decodeWithKind_render]

/-- Helper (inversion). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 **Trap-line injectivity.** Equal rendered lines have the same trap kind, typed site and full
source span (path, 1-based line and byte column). No restriction on punctuation in the path is
needed. -/
theorem trapLine_injective {kind₁ kind₂ : TrapKind} {site₁ site₂ : SiteId} {sp₁ sp₂ : Span}
    (h : trapLine kind₁ site₁ sp₁ = trapLine kind₂ site₂ sp₂) :
    kind₁ = kind₂ ∧ site₁ = site₂ ∧ sp₁ = sp₂ := by
  have decoded := congrArg decodeTrapLine h
  rw [decodeTrapLine_render, decodeTrapLine_render] at decoded
  cases decoded
  exact ⟨rfl, rfl, rfl⟩

/-- Helper (computation). Substitutes a caller-supplied column equality. Independent source-construction evidence is provided by the source trap-column law. (approved: PENDING)
 Rendering uses the byte column supplied by elaboration, not a Unicode-scalar column. -/
theorem trap_line_uses_byte_columns (kind : TrapKind) (site : SiteId) (sp : Span) (before : String)
    (h : Span.HasByteColumn sp before) :
    trapLine kind site sp = String.ofList (
      "concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList ++ sp.path.toList ++
      ":".toList ++ (toString sp.line).toList ++ ":".toList ++ (toString (byteColumn before)).toList ++
      " (site ".toList ++ (toString site.id).toList ++ ")".toList) := by
  rw [trapLine, trapLineChars, h]

/-- Law (subject `ConcreteSpec.Core.Observe.byteColumn`).
 A tab advances the byte column by exactly one. -/
theorem byteColumn_tab (before : String) : byteColumn (before ++ "\t") = byteColumn before + 1 := by
  have htab : "\t".utf8ByteSize = 1 := by decide
  simp [byteColumn, String.utf8ByteSize_append, htab, Nat.add_assoc]

/-! ## C1 observations -/

variable {O : Exec.Operands}

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 Output-only extensions preserve a complete valid trace. -/
theorem ValidTrace.outputs {C : OperationContract S} {input : C.Input} {hist : List (Event S)}
    (h : ValidTrace C input none hist) (outputs : List S.Output) :
    ValidTrace C input none (hist ++ outputs.map Event.output) := by
  induction outputs generalizing hist with
  | nil => simpa
  | cons output outputs ih =>
    simpa [List.append_assoc] using ih (ValidTrace.output h)

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 The adapter's output/request/response/output answer shape extends a complete valid trace. -/
theorem ValidTrace.answer_projection {C : OperationContract S} {input : C.Input}
    {hist before after : List (Event S)} {q : Request S} {r : Response S}
    (h : CompleteTrace C input hist)
    (beforeOutputs : ∃ outputs : List S.Output, before = outputs.map Event.output)
    (afterOutputs : ∃ outputs : List S.Output, after = outputs.map Event.output)
    (matching : RequestResponse q r)
    (allows : C.allows input (hist ++ before ++ [.request q]) q r) :
    CompleteTrace C input (hist ++ (before ++ [.request q, .response r] ++ after)) := by
  obtain ⟨beforeOutputs, rfl⟩ := beforeOutputs
  obtain ⟨afterOutputs, rfl⟩ := afterOutputs
  have pending := ValidTrace.request (q := q) (h.outputs beforeOutputs)
  have answered := ValidTrace.response pending matching allows
  simpa [CompleteTrace, List.append_assoc] using answered.outputs afterOutputs

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 Projection distributes over the C1 event-history append performed by `runFrom`. -/
theorem ExecAdapter.project_append (A : ExecAdapter O S) (xs ys : List O.Event) :
    A.project (xs ++ ys) = A.project xs ++ A.project ys := by
  simp [ExecAdapter.project, List.flatMap_append]

/-- Helper (computation). Evaluator induction supporting the operational report law; its blocking component inherits the environment policy and does not validate that policy independently. (approved: PENDING)
 Every C1 evaluator report from a well-formed adapter and oracle has a complete valid event
history. If it ends in `noAnswer`, the pending typed request is also permitted to block in the
history ending in that request. -/
theorem runFrom_validTrace (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S)
    (input : C.Input) (ρ : Exec.Oracle O) (wellFormed : ExecAdapter.WellFormed A C input ρ) :
    ∀ (fuel : Nat) (hist : List O.Event) (c : Exec.Config O),
      CompleteTrace C input (A.project hist) →
      CompleteTrace C input (A.project (Exec.runFrom P ρ fuel hist c).events) ∧
      ∀ {c' : Exec.Config O} {p : O.Prim} {vs : List O.Val} {site : SiteId} {store : O.Store}
        {k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O)} {q : Request S},
        (Exec.runFrom P ρ fuel hist c).ending = .noAnswer c' →
        Exec.step P c' = some (.request p vs site store k) →
        A.request p vs site store = some q →
        C.mayBlock input (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q := by
  intro fuel
  induction fuel with
  | zero =>
    intro hist c valid
    cases final : c.final? with
    | none => simp [Exec.runFrom, final, valid]
    | some value => simp [Exec.runFrom, final, valid]
  | succ fuel ih =>
    intro hist c valid
    cases final : c.final? with
    | some value => simp [Exec.runFrom, final, valid]
    | none =>
      cases next : Exec.step P c with
      | none => simp [Exec.runFrom, final, next, valid]
      | some out =>
        cases out with
        | next c' => simpa [Exec.runFrom, final, next] using ih hist c' valid
        | request p vs site store k =>
          cases answer : ρ hist p vs site store with
          | none =>
            constructor
            · simpa [Exec.runFrom, final, next, answer] using valid
            · intro c' p' vs' site' store' k' q ending pending typed
              simp [Exec.runFrom, final, next, answer] at ending
              cases ending
              rw [next] at pending
              cases pending
              simpa [Exec.runFrom, final, next, answer] using wellFormed.blocked answer typed
          | some a =>
            by_cases allowed : O.allows p vs store a = true
            · cases continued : k a with
              | none => simp [Exec.runFrom, final, next, answer, allowed, continued, valid]
              | some c' =>
                have answerValid : CompleteTrace C input (A.project (hist ++ a.event.toList)) := by
                  rw [ExecAdapter.project_append]
                  cases typed : A.request p vs site store with
                  | none =>
                    obtain ⟨outputs, projection⟩ := wellFormed.internal answer typed
                    rw [projection]
                    exact valid.outputs outputs
                  | some q =>
                    obtain ⟨before, r, after, projection, beforeOutputs, afterOutputs, matching,
                      allows⟩ := wellFormed.answered answer typed
                    rw [projection]
                    exact ValidTrace.answer_projection valid beforeOutputs afterOutputs matching allows
                simpa [Exec.runFrom, final, next, answer, allowed, continued] using
                  ih (hist ++ a.event.toList) c' answerValid
            · simp [Exec.runFrom, final, next, answer, allowed, valid]

/-- Helper (projection). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 A blocked C1 observation has exactly the pending `ValidTrace` state and history that its
operation contract uses. -/
theorem runFrom_noAnswer_pending_valid (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (ρ : Exec.Oracle O)
    (wellFormed : ExecAdapter.WellFormed A C input ρ)
    {fuel : Nat} {hist : List O.Event} {c c' : Exec.Config O}
    (valid : CompleteTrace C input (A.project hist))
    {p : O.Prim} {vs : List O.Val} {site : SiteId} {store : O.Store}
    {k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O)} {q : Request S}
    (ending : (Exec.runFrom P ρ fuel hist c).ending = .noAnswer c')
    (pending : Exec.step P c' = some (.request p vs site store k))
    (typed : A.request p vs site store = some q) :
    ValidTrace C input (some q)
      (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) ∧
      C.mayBlock input (A.project (Exec.runFrom P ρ fuel hist c).events ++ [.request q]) q := by
  have laws := runFrom_validTrace P A C input ρ wellFormed fuel hist c valid
  exact ⟨ValidTrace.request laws.1, laws.2 ending pending typed⟩

/-- Helper (projection). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 A well-formed C1 run supplies the blocking premise required by `reportObservation`. -/
theorem runFrom_blockingPermitted (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (ρ : Exec.Oracle O)
    (wellFormed : ExecAdapter.WellFormed A C input ρ)
    {fuel : Nat} {hist : List O.Event} {c : Exec.Config O}
    (valid : CompleteTrace C input (A.project hist)) :
    ExecAdapter.BlockingPermitted P A C input (Exec.runFrom P ρ fuel hist c) := by
  intro c' p vs site store k q ending pending typed
  exact (runFrom_validTrace P A C input ρ wellFormed fuel hist c valid).2 ending pending typed

/-- Helper (computation). Projection of a certified step history. This does not establish independent output correspondence for an arbitrary adapter. (approved: PENDING)
 **Output before a trap is kept.** The terminal C1 observation contains the projection of the
whole `Steps` history, not merely a suffix or the trap itself, and contains the exact trap line. -/
theorem output_before_trap_kept (A : ExecAdapter O S) {P : Exec.Program O} {c c' : Exec.Config O}
    {events : List O.Event} (steps : Exec.Steps P c events c') {kind : TrapKind} {site : SiteId}
    {sp : Span} (final : c'.final? = some (.trapped kind site))
    (resolved : AList.lookup A.sites site = some sp) :
    stepsObservation A steps = some ⟨A.project events, .trapped kind site sp (trapLine kind site sp)⟩ := by
  simp [stepsObservation, final, ExecAdapter.terminal?, trapTerminal, resolved]

/-- Helper (projection). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 The premise on `reportObservation` really supplies the operation contract's blocking
permission, in the history ending in the current request. -/
theorem report_noAnswer_mayBlock (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (events : List O.Event) (c : Exec.Config O)
    (permitted : ExecAdapter.BlockingPermitted P A C input ⟨events, .noAnswer c⟩)
    {p : O.Prim} {vs : List O.Val} {site : SiteId} {store : O.Store}
    {k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O)} {request : Request S}
    (pending : Exec.step P c = some (.request p vs site store k))
    (typed : A.request p vs site store = some request) :
    C.mayBlock input (A.project events ++ [.request request]) request :=
  permitted rfl pending typed

/-- Helper (computation). Computes a report branch; independent reachable-run trace evidence is provided by the operational report law. (approved: PENDING)
 A permitted pending C1 request observed through `noAnswer` retains the earlier event history
and ends in the request itself; it is not mistaken for termination or divergence. -/
theorem report_noAnswer_blocked (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input)
    (events : List O.Event) (c : Exec.Config O) (p : O.Prim) (vs : List O.Val) (site : SiteId)
    (store : O.Store) (k : Exec.Answer O.Val O.Event O.Store → Option (Exec.Config O))
    (request : Request S)
    (permitted : ExecAdapter.BlockingPermitted P A C input ⟨events, .noAnswer c⟩)
    (pending : Exec.step P c = some (.request p vs site store k))
    (typed : A.request p vs site store = some request) :
    reportObservation P A C input ⟨events, .noAnswer c⟩ permitted =
      some (.blocked (A.project events ++ [.request request]) request) := by
  simp [reportObservation, pending, typed]

/-- Helper (computation). Computes the exhausted-fuel branch, not an independent operational classification law. (approved: PENDING)
 Fuel exhaustion produces no fabricated external observation or event. -/
theorem report_outOfFuel_none (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (events : List O.Event) (c : Exec.Config O)
    (permitted : ExecAdapter.BlockingPermitted P A C input ⟨events, .outOfFuel c⟩) :
    reportObservation P A C input ⟨events, .outOfFuel c⟩ permitted = none := rfl

/-! ### Non-vacuity of the C1 adapter laws

A one-primitive instance doubles its integer request. Its ordinary oracle accepts the response and
its blocking oracle leaves the request unanswered. The examples below apply every law whose C1
continuation became partial, and mutate the response, blocking permission or expected observation
to show that the corresponding conclusion is not obtained freely. -/

namespace AdapterDemo

inductive Op where
  | double

/-- Integer doubling requests and responses, with text output unused by these runs. -/
abbrev signature : EventSignature where
  Kind := Op
  Request := fun _ => Int
  Response := fun _ => Int
  Output := String

def query (n : Int) : Request signature := ⟨.double, n⟩
def request (n : Int) : Event signature := .request (query n)
def response (n : Int) : Event signature := .response ⟨.double, n⟩

/-- The response contract is fixed; each instance chooses whether blocking is permitted. -/
def makeContract (blocking : Unit → List (Event signature) → Request signature → Prop) :
    OperationContract signature where
  Input := Unit
  allows := fun _ _ q r => q.2 * 2 = r.2
  mayBlock := blocking

abbrev permittingContract : OperationContract signature := makeContract (fun _ _ _ => True)
abbrev forbiddingContract : OperationContract signature := makeContract (fun _ _ _ => False)

/-- A total operand instance whose primitive `n` requests doubling `n`. -/
abbrev ops : Exec.Operands where
  Val := Int
  Expr := Int
  Ty := Unit
  Store := Unit
  Prim := Int
  Event := Int
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := fun _ st n => .ok (n, st)
  life := fun _ st => some st
  test := fun _ => none
  allows := fun p _ _ a => match a.event, a.result, a.store with
    | some ev, .ok 0, () => decide (ev = p)
    | _, _, _ => false

/-- One oracle supplies the contracted response; the other leaves every request unanswered. -/
def answeringOracle : Exec.Oracle ops := fun _ p _ _ _ => some ⟨some p, .ok 0, ()⟩
def blockingOracle : Exec.Oracle ops := fun _ _ _ _ _ => none

/-- The single C1 event of an answer projects to its typed request and response. -/
def adapter : ExecAdapter ops signature where
  events := fun n => [request n, response (n * 2)]
  request := fun p _ _ _ => some (query p)
  status := fun _ => none
  sites := []

def program : Exec.Program ops := ⟨[]⟩
def requesting (n : Int) : Exec.Config ops := ⟨.exec (.prim none n [] ⟨0⟩), [], (), []⟩

/-- A successful response and a genuinely pending request, both after one evaluator step. -/
def answeredReport : Exec.Report ops := Exec.run program answeringOracle 1 (requesting 3)
def blockedReport : Exec.Report ops := Exec.run program blockingOracle 1 (requesting 3)

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 The answering oracle satisfies the adapter contract for either blocking policy. -/
theorem answeringWellFormed (blocking : Unit → List (Event signature) → Request signature → Prop) :
    ExecAdapter.WellFormed adapter (makeContract blocking) () answeringOracle := by
  constructor
  · intro hist p vs site store a answer typed
    simp [adapter] at typed
  · intro hist p vs site store a q answer typed
    cases answer
    cases typed
    exact ⟨[], ⟨.double, p * 2⟩, [], rfl, ⟨[], rfl⟩, ⟨[], rfl⟩,
      RequestResponse.matched (S := signature) Op.double p (p * 2), rfl⟩
  · intro hist p vs site store q answer typed
    simp [answeringOracle] at answer
  · intro site sp resolved
    cases resolved

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 The blocking oracle is well formed exactly because this contract permits blocking. -/
theorem blockingWellFormed : ExecAdapter.WellFormed adapter permittingContract () blockingOracle := by
  constructor
  · intro hist p vs site store a answer typed
    simp [blockingOracle] at answer
  · intro hist p vs site store a q answer typed
    simp [blockingOracle] at answer
  · intro hist p vs site store q answer typed
    trivial
  · intro site sp resolved
    cases resolved

/-- Computation example for `runFrom_validTrace`: the accepted answer really runs and its request and
response form a complete valid trace. -/
example : CompleteTrace forbiddingContract () (adapter.project answeredReport.events) :=
  (runFrom_validTrace program adapter forbiddingContract () answeringOracle
    (answeringWellFormed _) 1 [] (requesting 3) .idle).1

/-- Rejected response example for `runFrom_validTrace`: `7` does not double `3`. -/
example : ¬ CompleteTrace forbiddingContract () [request 3, response 7] := by
  intro invalid
  generalize htrace : [request 3, response 7] = events at invalid
  cases invalid with
  | idle => simp at htrace
  | @response hist q r pending matching allows =>
    have reversed : response 7 :: [request 3] = Event.response r :: hist.reverse := by
      simpa using congrArg List.reverse htrace
    have eventEq : response 7 = Event.response r := (List.cons.inj reversed).1
    cases eventEq
    change q.2 * 2 = 7 at allows
    cases matching
    rename_i n
    change Int at n
    change n * 2 = 7 at allows
    omega
  | @output hist o pending =>
    have reversed : response 7 :: [request 3] = Event.output o :: hist.reverse := by
      simpa using congrArg List.reverse htrace
    have eventEq : response 7 = Event.output o := (List.cons.inj reversed).1
    cases eventEq

/-- Computation example for `runFrom_noAnswer_pending_valid`: the run is pending on `3`, and the pending
trace and blocking permission both hold. -/
example :
    ValidTrace permittingContract () (some (query 3))
      (adapter.project blockedReport.events ++ [request 3]) ∧
    permittingContract.mayBlock ()
      (adapter.project blockedReport.events ++ [request 3]) (query 3) :=
  runFrom_noAnswer_pending_valid program adapter permittingContract () blockingOracle
    blockingWellFormed (fuel := 1) (hist := []) (c := requesting 3) (c' := requesting 3)
    (q := query 3) .idle rfl rfl rfl

/-- Blocking refusal example for `runFrom_noAnswer_pending_valid`: the same pending trace cannot
supply a blocking permission that its contract denies. -/
example : ¬ (ValidTrace forbiddingContract () (some (query 3)) [request 3] ∧
    forbiddingContract.mayBlock () [request 3] (query 3)) := by
  intro invalid
  exact invalid.2

/-- Helper (projection). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 Concrete evidence used by the two report-level examples below. -/
theorem blockedPermitted : ExecAdapter.BlockingPermitted program adapter permittingContract ()
    blockedReport :=
  runFrom_blockingPermitted program adapter permittingContract () blockingOracle
    blockingWellFormed (fuel := 1) (hist := []) (c := requesting 3) .idle

/-- Computation example for `runFrom_blockingPermitted`: its premise is satisfied by a real `noAnswer`
report. -/
example : ExecAdapter.BlockingPermitted program adapter permittingContract () blockedReport :=
  blockedPermitted

/-- Blocking refusal example for `runFrom_blockingPermitted`: this same report does not satisfy
`BlockingPermitted` when the operation may not block. -/
example : ¬ ExecAdapter.BlockingPermitted program adapter forbiddingContract () blockedReport := by
  intro permitted
  exact permitted (c := requesting 3) (p := 3) (vs := []) (site := ⟨0⟩) (store := ())
    (q := query 3) rfl rfl rfl

/-- Computation example for `report_noAnswer_mayBlock`: the report premise supplies the actual operation
permission for the pending request. -/
example : permittingContract.mayBlock ()
    (adapter.project blockedReport.events ++ [request 3]) (query 3) :=
  report_noAnswer_mayBlock program adapter permittingContract () [] (requesting 3)
    blockedPermitted rfl rfl

/-- Blocking refusal example for `report_noAnswer_mayBlock`: changing only `mayBlock` makes the
claimed permission false. -/
example : ¬ forbiddingContract.mayBlock () [request 3] (query 3) := by
  intro forbidden
  exact forbidden

/-- Helper (computation). Auxiliary proof supporting the observation constructions and their operational laws; not a standalone contract law. (approved: PENDING)
 The permitted report has exactly its pending blocked observation. -/
theorem blockedObservation :
    reportObservation program adapter permittingContract () blockedReport blockedPermitted =
      some (.blocked (adapter.project blockedReport.events ++ [request 3]) (query 3)) :=
  report_noAnswer_blocked program adapter permittingContract () [] (requesting 3)
    3 [] ⟨0⟩ () _ (query 3) blockedPermitted rfl rfl

/-- Computation example for `report_noAnswer_blocked`: the pending request and its history are observed. -/
example : reportObservation program adapter permittingContract () blockedReport blockedPermitted =
    some (.blocked (adapter.project blockedReport.events ++ [request 3]) (query 3)) :=
  blockedObservation

/-- Distinct observation example for `report_noAnswer_blocked`: the pending request cannot instead be
observed as successful termination. -/
example : reportObservation program adapter permittingContract () blockedReport blockedPermitted ≠
    some (.terminal ⟨[request 3], .exited 0⟩) := by
  rw [blockedObservation]
  intro invalid
  cases invalid

end AdapterDemo

end ConcreteSpec.Core.Observe

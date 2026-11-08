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

/-- Matching responses really do have the request's operation kind. -/
theorem RequestResponse.kind {q : Request S} {r : Response S} (h : RequestResponse q r) :
    q.fst = r.fst := by
  cases h
  rfl

/-- Every response admitted by the environment relation satisfies its operation contract. -/
theorem Responds.allows {C : OperationContract S} {input : C.Input} {hist : List (Event S)}
    {q : Request S} {r : Response S} (h : Responds C input hist q (.response r)) :
    C.allows input hist q r := by
  cases h with
  | response _ allows => exact allows

/-- Every response admitted by the environment relation matches its request. -/
theorem Responds.matches {C : OperationContract S} {input : C.Input} {hist : List (Event S)}
    {q : Request S} {r : Response S} (h : Responds C input hist q (.response r)) :
    RequestResponse q r := by
  cases h with
  | response matching _ => exact matching

/-- Silent steps do not change a finite observation. -/
theorem silent_internal_steps (steps : List (Step S)) (terminal : Terminal) :
    SameObservation (.finite (.silent :: steps) terminal) (.finite steps terminal) := by
  exact ⟨rfl, rfl⟩

/-- Any finite visible prefix is immediately a prefix of the same execution's prefix. -/
theorem PrefixEventually.refl (steps : Nat → Step S) : PrefixEventually steps steps := by
  intro n
  exact ⟨n, [], by simp⟩

/-- Eventual prefix containment composes; the second execution may need more internal steps. -/
theorem PrefixEventually.trans {xs ys zs : Nat → Step S}
    (hxy : PrefixEventually xs ys) (hyz : PrefixEventually ys zs) : PrefixEventually xs zs := by
  intro n
  obtain ⟨m, rest₁, h₁⟩ := hxy n
  obtain ⟨p, rest₂, h₂⟩ := hyz m
  exact ⟨p, rest₁ ++ rest₂, by rw [h₂, h₁, List.append_assoc]⟩

theorem SameInfiniteObservation.refl (steps : Nat → Step S) : SameInfiniteObservation steps steps :=
  ⟨PrefixEventually.refl steps, PrefixEventually.refl steps⟩

theorem SameInfiniteObservation.symm {xs ys : Nat → Step S}
    (h : SameInfiniteObservation xs ys) : SameInfiniteObservation ys xs := ⟨h.2, h.1⟩

theorem SameInfiniteObservation.trans {xs ys zs : Nat → Step S}
    (hxy : SameInfiniteObservation xs ys) (hyz : SameInfiniteObservation ys zs) :
    SameInfiniteObservation xs zs := ⟨hxy.1.trans hyz.1, hyz.2.trans hxy.2⟩

/-- Full-observation equivalence is reflexive. -/
theorem SameObservation.refl (execution : Execution S) : SameObservation execution execution := by
  cases execution with
  | finite steps terminal => exact ⟨rfl, rfl⟩
  | blocked steps request => exact ⟨rfl, rfl⟩
  | infinite steps => exact SameInfiniteObservation.refl steps

/-- Full-observation equivalence is symmetric. -/
theorem SameObservation.symm {x y : Execution S} (h : SameObservation x y) : SameObservation y x := by
  cases x <;> cases y <;> simp only [SameObservation] at h ⊢
  all_goals first | exact h.elim | exact ⟨h.1.symm, h.2.symm⟩ | exact h.symm

/-- Full-observation equivalence is transitive. -/
theorem SameObservation.trans {x y z : Execution S}
    (hxy : SameObservation x y) (hyz : SameObservation y z) : SameObservation x z := by
  cases x <;> cases y <;> cases z <;> simp only [SameObservation] at hxy hyz ⊢
  all_goals first | exact hxy.elim | exact hyz.elim |
    exact ⟨hxy.1.trans hyz.1, hxy.2.trans hyz.2⟩ | exact hxy.trans hyz

private theorem observation_congr {xs ys : List (Event S)} {terminal terminal' : Terminal}
    (hxs : xs = ys) (ht : terminal = terminal') :
    (Observation.mk xs terminal : Observation S) = Observation.mk ys terminal' := by
  subst ys
  subst terminal'
  rfl

private theorem blocked_congr {xs ys : List (Event S)} {request request' : Request S}
    (hxs : xs = ys) (hr : request = request') :
    (FullObservation.blocked xs request : FullObservation S) = .blocked ys request' := by
  subst ys
  subst request'
  rfl

/-- **Full-observation preservation.** Silent internal steps may differ freely, but finite runs
have identical events and terminal actions, blocked runs also have the same request, and infinite
runs have the same complete event history. Every mixed finite/infinite case is impossible. -/
theorem full_observation_preservation {x y : Execution S} (h : SameObservation x y) :
    (match x, y with
      | .finite xs terminal, .finite ys terminal' =>
          (⟨visible xs, terminal⟩ : Observation S) = ⟨visible ys, terminal'⟩
      | .blocked xs request, .blocked ys request' =>
          (FullObservation.blocked (S := S) (visible xs) request) =
            .blocked (visible ys) request'
      | .infinite xs, .infinite ys => SameInfiniteObservation xs ys
      | _, _ => False) := by
  cases x <;> cases y <;> simp only [SameObservation] at h
  all_goals first | exact h.elim | exact h | exact observation_congr h.1 h.2 |
    exact blocked_congr h.1 h.2

/-- In particular, an infinite silent run never acquires a terminal observation. -/
theorem infinite_silent_not_termination (terminal : Terminal) :
    ¬ SameObservation (.infinite (fun _ => Step.silent)) (.finite (S := S) [] terminal) := by
  simp [SameObservation]

/-! ## Decoding the exact trap line -/

private def stripPrefix : List Char → List Char → Option (List Char)
  | [], rest => some rest
  | _ :: _, [] => none
  | c :: cs, d :: ds => if c = d then stripPrefix cs ds else none

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

private theorem trapLineChars_prefix (kind : TrapKind) (site : SiteId) (sp : Span) :
    trapLineChars kind site sp = linePrefix kind ++
      (sp.path.toList ++ ":".toList ++ (toString sp.line).toList ++ ":".toList ++
        (toString sp.col).toList ++ " (site ".toList ++ (toString site.id).toList ++ ")".toList) := by
  simp [trapLineChars, linePrefix, List.append_assoc]

private theorem decodeWithKind_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeWithKind kind (trapLineChars kind site sp) = some (site, sp) := by
  have hline : 0 < sp.line := sp.line_pos
  have hcol : 0 < sp.col := sp.col_pos
  unfold decodeWithKind
  rw [trapLineChars_prefix, stripPrefix_append]
  simp [List.reverse_append, stripPrefix, parseRevNat_digits, hline, hcol]

private theorem decodeTrapLine_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeTrapLine (trapLine kind site sp) = some (kind, site, sp) := by
  simp [decodeTrapLine, trapLine, decodeKind_render, decodeWithKind_render]

/-- **Trap-line injectivity.** Equal rendered lines have the same trap kind, typed site and full
source span (path, 1-based line and byte column). No restriction on punctuation in the path is
needed. -/
theorem trapLine_injective {kind₁ kind₂ : TrapKind} {site₁ site₂ : SiteId} {sp₁ sp₂ : Span}
    (h : trapLine kind₁ site₁ sp₁ = trapLine kind₂ site₂ sp₂) :
    kind₁ = kind₂ ∧ site₁ = site₂ ∧ sp₁ = sp₂ := by
  have decoded := congrArg decodeTrapLine h
  rw [decodeTrapLine_render, decodeTrapLine_render] at decoded
  cases decoded
  exact ⟨rfl, rfl, rfl⟩

/-- Rendering uses the byte column supplied by elaboration, not a Unicode-scalar column. -/
theorem trap_line_uses_byte_columns (kind : TrapKind) (site : SiteId) (sp : Span) (before : String)
    (h : Span.HasByteColumn sp before) :
    trapLine kind site sp = String.ofList (
      "concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList ++ sp.path.toList ++
      ":".toList ++ (toString sp.line).toList ++ ":".toList ++ (toString (byteColumn before)).toList ++
      " (site ".toList ++ (toString site.id).toList ++ ")".toList) := by
  rw [trapLine, trapLineChars, h]

/-- A tab advances the byte column by exactly one. -/
theorem byteColumn_tab (before : String) : byteColumn (before ++ "\t") = byteColumn before + 1 := by
  have htab : "\t".utf8ByteSize = 1 := by decide
  simp [byteColumn, String.utf8ByteSize_append, htab, Nat.add_assoc]

/-! ## C1 observations -/

variable {O : Exec.Operands}

/-- **Output before a trap is kept.** The terminal C1 observation contains the projection of the
whole `Steps` history, not merely a suffix or the trap itself, and contains the exact trap line. -/
theorem output_before_trap_kept (A : ExecAdapter O S) {P : Exec.Program O} {c c' : Exec.Config O}
    {events : List O.Event} (steps : Exec.Steps P c events c') {kind : TrapKind} {site : SiteId}
    {sp : Span} (final : c'.final? = some (.trapped kind site))
    (resolved : AList.lookup A.sites site = some sp) :
    stepsObservation A steps = some ⟨A.project events, .trapped kind site sp (trapLine kind site sp)⟩ := by
  simp [stepsObservation, final, ExecAdapter.terminal?, trapTerminal, resolved]

/-- A pending C1 request observed through `noAnswer` retains the earlier event history and the
request itself; it is not mistaken for termination or divergence. -/
theorem report_noAnswer_blocked (P : Exec.Program O) (A : ExecAdapter O S)
    (events : List O.Event) (c : Exec.Config O) (p : O.Prim) (vs : List O.Val) (site : SiteId)
    (store : O.Store) (k : Exec.Answer O.Val O.Event O.Store → Exec.Config O) (request : Request S)
    (pending : Exec.step P c = some (.request p vs site store k))
    (typed : A.request p vs site store = some request) :
    reportObservation P A ⟨events, .noAnswer c⟩ = some (.blocked (A.project events) request) := by
  simp [reportObservation, pending, typed]

/-- Fuel exhaustion produces no fabricated external observation or event. -/
theorem report_outOfFuel_none (P : Exec.Program O) (A : ExecAdapter O S)
    (events : List O.Event) (c : Exec.Config O) :
    reportObservation P A ⟨events, .outOfFuel c⟩ = none := rfl

end ConcreteSpec.Core.Observe

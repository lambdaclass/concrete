import ConcreteSpec.Spec.Observe
import ConcreteSpec.Proofs.Ident

/-!
# The laws of observations and the environment (`Spec/Observe.lean`, C6)

The finite observation laws identify executions after removing any number of silent steps. The
infinite law compares every step-indexed visible prefix with an eventually available prefix of the
other execution; in particular, two silently divergent executions may be equivalent, but neither
is equivalent to a finite terminated execution.

The trap-line theorem is proved by decoding the line, not by restricting source paths. Decimal
fields are parsed from right to left, so colons and parentheses in a path cannot make two different
`(kind, site, path, line, col)` rendered tuples identical. Byte ranges are not printed or recovered.
The only numeric decoder needed is Lean core's
`Nat.ofDigitChars`, whose round trip through `Nat.toDigits` is already proved.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

variable {S : EventSignature}

/-- Helper (computation). Parsing every rendered file recovers its identity and bytes.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.parseFiles_render (sources : SourceTable) :
    LoadedProject.parseFiles (List.map (fun entry => (entry.1.render, entry.2)) sources) = .ok sources := by
  induction sources with
  | nil => rfl
  | cons entry rest ih => simp [LoadedProject.parseFiles, SourcePath.parse_render, ih]

/-- Helper (inversion). Successful file parsing preserves every raw spelling and byte sequence.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.parseFiles_exact (files : AList String SourceText) (sources : SourceTable)
    (parsed : LoadedProject.parseFiles files = .ok sources) :
    List.map (fun entry => (entry.1.render, entry.2)) sources = files := by
  induction files generalizing sources with
  | nil => cases parsed; rfl
  | cons file rest ih =>
    cases hp : SourcePath.parse file.1 with
    | error issue => simp [LoadedProject.parseFiles, hp] at parsed
    | ok path =>
      cases ht : LoadedProject.parseFiles rest with
      | error issue => simp [LoadedProject.parseFiles, hp, ht] at parsed
      | ok tail =>
        simp only [LoadedProject.parseFiles, hp, ht, Except.ok.injEq] at parsed
        subst sources
        simp only [List.map_cons, SourcePath.accepted_render file.1 path hp, ih tail ht]

/-- Helper (computation). File parsing is exactly the representation relation, in both directions.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.parseFiles_iff (files : AList String SourceText) (sources : SourceTable) :
    LoadedProject.parseFiles files = .ok sources ↔
      List.map (fun entry => (entry.1.render, entry.2)) sources = files := by
  constructor
  · exact LoadedProject.parseFiles_exact files sources
  · intro h; rw [← h]; exact LoadedProject.parseFiles_render sources

/-- Helper (computation). The collision search finds none exactly for lists without repeats.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.firstDuplicate_none {α : Type} [DecidableEq α] (keys : List α) :
    LoadedProject.firstDuplicate keys = none ↔ keys.Nodup := by
  induction keys with
  | nil => simp [LoadedProject.firstDuplicate]
  | cons key rest ih =>
    by_cases h : key ∈ rest <;> simp [LoadedProject.firstDuplicate, h, ih]

/-- Helper (computation). A site's source-binding check succeeds exactly for a matching file
whose bytes establish the span's location. (approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.siteIssue_none (sources : SourceTable) (entry : SiteId × Span) :
    LoadedProject.siteIssue sources entry = none ↔ ∃ bytes,
      AList.lookup sources entry.2.path = some bytes ∧ entry.2.LocationOk bytes := by
  unfold LoadedProject.siteIssue
  cases found : AList.lookup sources entry.2.path with
  | none => simp
  | some bytes => by_cases h : entry.2.LocationOk bytes <;> simp [h]

/-- Helper (computation). The source-binding scan covers every supplied site, without filtering.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.findSiteIssue_none (sources : SourceTable) (sites : SiteTable) :
    LoadedProject.findSiteIssue sources sites = none ↔
      ∀ entry ∈ sites, ∃ bytes,
        AList.lookup sources entry.2.path = some bytes ∧ entry.2.LocationOk bytes := by
  induction sites with
  | nil => simp [LoadedProject.findSiteIssue]
  | cons entry rest ih =>
    rw [List.forall_mem_cons]
    rw [← LoadedProject.siteIssue_none, ← ih]
    cases issue : LoadedProject.siteIssue sources entry <;>
      simp [LoadedProject.findSiteIssue, issue]

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). Admission is precisely whole
input preservation with unique identities and byte-correct spans. In particular, a partial table
or a table retaining the first duplicate is never an admitted interpretation of the input. -/
theorem LoadedProject.load_admitted (input : ProjectSources) (project : LoadedProject) :
    LoadedProject.load input = .admitted project ↔ input.Realizes project := by
  constructor
  · intro admitted
    unfold LoadedProject.load at admitted
    cases hp : LoadedProject.parseFiles input.files with
    | error issue => simp [hp] at admitted
    | ok sources =>
      cases hf : LoadedProject.firstDuplicate (List.map Prod.fst sources) with
      | some path => simp [hp, hf] at admitted
      | none =>
        cases hs : LoadedProject.firstDuplicate (List.map Prod.fst input.sites) with
        | some site => simp [hp, hf, hs] at admitted
        | none =>
          cases hi : LoadedProject.findSiteIssue sources input.sites with
          | some issue => simp [hp, hf, hs, hi] at admitted
          | none =>
            simp only [hp, hf, hs, hi, SourceLoadResult.admitted.injEq] at admitted
            subst project
            exact ⟨LoadedProject.parseFiles_exact _ _ hp,
              (LoadedProject.firstDuplicate_none _).mp hf, rfl,
              (LoadedProject.firstDuplicate_none _).mp hs,
              (LoadedProject.findSiteIssue_none _ _).mp hi⟩
  · rintro ⟨raw, filesUnique, sitesEqual, sitesUnique, locations⟩
    simp only [LoadedProject.load, (LoadedProject.parseFiles_iff _ _).mpr raw,
      (LoadedProject.firstDuplicate_none _).mpr filesUnique,
      (LoadedProject.firstDuplicate_none _).mpr sitesUnique,
      (LoadedProject.findSiteIssue_none _ _).mpr locations]
    cases project
    simp_all

/-- Helper (computation). Unique keys make membership and lookup agree on the *same* span,
rather than merely finding a value for that identity. (approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.lookup_iff_mem {κ α : Type} [DecidableEq κ]
    (table : AList κ α) (unique : (List.map Prod.fst table).Nodup) (key : κ) (value : α) :
    AList.lookup table key = some value ↔ (key, value) ∈ table := by
  induction table with
  | nil => simp [AList.lookup]
  | cons entry rest ih =>
    rcases entry with ⟨k, v⟩
    simp only [List.map_cons, List.nodup_cons] at unique
    by_cases same : k = key
    · subst key
      have absent : ∀ a, (k, a) ∉ rest := by
        intro a h; exact unique.1 (List.mem_map.mpr ⟨(k, a), h, rfl⟩)
      simp [AList.lookup, absent, eq_comm]
    · simp [AList.lookup, same, Ne.symm same, ih unique.2]

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). Every input site resolves to
its own span and every resolved site came from the input. Refusal has no partial table. -/
theorem LoadedProject.load_complete (input : ProjectSources) :
    match LoadedProject.load input with
    | .refused _ => True
    | .admitted project => ∀ site sp,
        AList.lookup project.sites site = some sp ↔ (site, sp) ∈ input.sites := by
  cases loaded : LoadedProject.load input with
  | refused _ => trivial
  | admitted project =>
    have h := (LoadedProject.load_admitted input project).mp loaded
    dsimp only
    rw [h.2.2.1]
    exact LoadedProject.lookup_iff_mem input.sites h.2.2.2.1

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). Every admitted span is bound
to a loaded file and to its actual byte-derived range, line and column. -/
theorem LoadedProject.load_locations (input : ProjectSources) :
    match LoadedProject.load input with
    | .refused _ => True
    | .admitted project => SiteTable.Valid project.sources project.sites := by
  cases loaded : LoadedProject.load input with
  | refused _ => trivial
  | admitted project =>
    have h := (LoadedProject.load_admitted input project).mp loaded
    intro site sp found
    have member : ∀ site sp, AList.lookup project.sites site = some sp ↔
        (site, sp) ∈ input.sites := by
      simpa only [loaded] using LoadedProject.load_complete input
    obtain ⟨bytes, bound, loc⟩ := h.2.2.2.2 (site, sp) ((member site sp).mp found)
    exact ⟨⟨sp.path.render_safe.1, sp.path.render_safe.2.2.2.1,
      sp.path.render_safe.2.2.2.2.2⟩, bytes, bound, loc⟩

/-- Helper (computation). A reported duplicate is an identity in the original list.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.firstDuplicate_mem {α : Type} [DecidableEq α] (keys : List α)
    (key : α) (found : LoadedProject.firstDuplicate keys = some key) : key ∈ keys := by
  induction keys with
  | nil => contradiction
  | cons first rest ih =>
    unfold LoadedProject.firstDuplicate at found
    split at found
    · cases found; simp
    · exact List.Mem.tail _ (ih found)

/-- Helper (inversion). Parsing failures name an original file and its exact raw spelling.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.parseFiles_refusal_names (files : AList String SourceText)
    (sites : SiteTable) (issue : SourceLoadRefusal)
    (refused : LoadedProject.parseFiles files = .error issue) :
    issue.Names ⟨files, sites⟩ := by
  induction files with
  | nil => contradiction
  | cons file rest ih =>
    cases hp : SourcePath.parse file.1 with
    | error diagnostic =>
      simp only [LoadedProject.parseFiles, hp, Except.error.injEq] at refused
      subst issue
      exact ⟨by simp, SourcePath.refusal_input _ _ hp⟩
    | ok path =>
      cases ht : LoadedProject.parseFiles rest with
      | ok tail => simp [LoadedProject.parseFiles, hp, ht] at refused
      | error diagnostic =>
        simp only [LoadedProject.parseFiles, hp, ht, Except.error.injEq] at refused
        subst issue
        have h := ih ht
        cases diagnostic <;> simp_all [SourceLoadRefusal.Names]

/-- Helper (inversion). A per-site diagnostic names the supplied entry.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.siteIssue_names (sources : SourceTable) (entry : SiteId × Span)
    (input : ProjectSources) (member : entry ∈ input.sites) (issue : SourceLoadRefusal)
    (found : LoadedProject.siteIssue sources entry = some issue) : issue.Names input := by
  unfold LoadedProject.siteIssue at found
  cases hf : AList.lookup sources entry.2.path with
  | none => simp only [hf, Option.some.injEq] at found; subst issue; exact member
  | some bytes =>
    simp only [hf] at found
    split at found
    · contradiction
    · cases found; exact member

/-- Helper (inversion). The source-binding scan's diagnostic names an entry in its input.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.findSiteIssue_names (sources : SourceTable) (sites : SiteTable)
    (input : ProjectSources) (subset : ∀ entry ∈ sites, entry ∈ input.sites)
    (issue : SourceLoadRefusal) (found : LoadedProject.findSiteIssue sources sites = some issue) :
    issue.Names input := by
  induction sites with
  | nil => contradiction
  | cons entry rest ih =>
    cases hi : LoadedProject.siteIssue sources entry with
    | some diagnostic =>
      simp only [LoadedProject.findSiteIssue, hi, Option.some.injEq] at found
      subst issue
      exact LoadedProject.siteIssue_names sources entry input (subset entry (by simp)) _ hi
    | none =>
      simp only [LoadedProject.findSiteIssue, hi] at found
      exact ih (fun e h => subset e (by simp [h])) found

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). A refusal always names an
original file or site; admission never invents a diagnostic naming unrelated input. -/
theorem LoadedProject.load_refusal_names (input : ProjectSources) :
    match LoadedProject.load input with
    | .admitted _ => True
    | .refused issue => issue.Names input := by
  unfold LoadedProject.load
  cases hp : LoadedProject.parseFiles input.files with
  | error issue => exact LoadedProject.parseFiles_refusal_names _ _ _ hp
  | ok sources =>
    dsimp only
    cases hf : LoadedProject.firstDuplicate (List.map Prod.fst sources) with
    | some path =>
      have member := LoadedProject.firstDuplicate_mem _ _ hf
      have raw := LoadedProject.parseFiles_exact _ _ hp
      change path.render ∈ List.map Prod.fst input.files
      rw [← raw, List.map_map]
      obtain ⟨entry, he, eq⟩ := List.mem_map.mp member
      exact List.mem_map.mpr ⟨entry, he, congrArg SourcePath.render eq⟩
    | none =>
      dsimp only
      cases hs : LoadedProject.firstDuplicate (List.map Prod.fst input.sites) with
      | some site => exact LoadedProject.firstDuplicate_mem _ _ hs
      | none =>
        dsimp only
        cases hi : LoadedProject.findSiteIssue sources input.sites with
        | none => trivial
        | some issue =>
          exact LoadedProject.findSiteIssue_names sources input.sites input
            (fun _ h => h) issue hi

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). No interpretation of invalid
input can be admitted; all malformed, duplicate, missing-file and bad-location inputs are refused. -/
theorem LoadedProject.load_refuses (input : ProjectSources)
    (invalid : ¬ ∃ project, input.Realizes project) :
    ∃ issue, LoadedProject.load input = .refused issue := by
  cases loaded : LoadedProject.load input with
  | refused issue => exact ⟨issue, rfl⟩
  | admitted project =>
    exact False.elim (invalid ⟨project, (LoadedProject.load_admitted input project).mp loaded⟩)

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). Every raw-path parser refusal
at any position forces whole-project refusal with a diagnostic naming original input. This covers
empty, dot, traversal, absolute, drive, empty-component and preserved backslash refusals. -/
theorem LoadedProject.load_refuses_malformed (input : ProjectSources) (file : String × SourceText)
    (diagnostic : SourcePath.Refusal) (member : file ∈ input.files)
    (malformed : SourcePath.parse file.1 = .error diagnostic) :
    ∃ issue, LoadedProject.load input = .refused issue ∧ issue.Names input := by
  have invalid : ¬ ∃ project, input.Realizes project := by
    rintro ⟨project, realizes⟩
    rw [← realizes.1] at member
    obtain ⟨entry, _, equal⟩ := List.mem_map.mp member
    have spelling : entry.1.render = file.1 := congrArg Prod.fst equal
    have parsed := SourcePath.parse_render entry.1
    rw [spelling, malformed] at parsed
    contradiction
  obtain ⟨issue, refused⟩ := LoadedProject.load_refuses input invalid
  have named := LoadedProject.load_refusal_names input
  rw [refused] at named
  exact ⟨issue, refused, named⟩

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). Admission establishes unique
site identities, even when duplicate input entries would have identical spans. -/
theorem LoadedProject.load_unique (input : ProjectSources) :
    match LoadedProject.load input with
    | .refused _ => True
    | .admitted project => (List.map Prod.fst project.sites).Nodup := by
  cases loaded : LoadedProject.load input with
  | refused _ => trivial
  | admitted project =>
    have h := (LoadedProject.load_admitted input project).mp loaded
    simpa only [h.2.2.1] using h.2.2.2.1

/-- Representation lemma. A lookup is functional: two successful resolutions of one identity
return the same span. This is finite-map algebra, not independent loader soundness.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.lookup_functional (project : LoadedProject) (site : SiteId) (a b : Span)
    (first : AList.lookup project.sites site = some a)
    (second : AList.lookup project.sites site = some b) : a = b := by
  exact Option.some.inj (first.symm.trans second)

/-- Helper (computation). A permutation of mapped input can be lifted to its original records.
No decoder or fallback value is needed. (approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.lift_permutation {α β : Type} (f : α → β)
    {left right : List β} (permutation : left.Perm right) (records : List α)
    (mapped : List.map f records = left) :
    ∃ reordered, List.map f reordered = right ∧ records.Perm reordered := by
  induction permutation generalizing records with
  | nil =>
    cases records with
    | nil => exact ⟨[], rfl, .nil⟩
    | cons _ _ => simp at mapped
  | cons value permutation ih =>
    cases records with
    | nil => simp at mapped
    | cons record rest =>
      simp only [List.map_cons, List.cons.injEq] at mapped
      obtain ⟨tail, equal, perm⟩ := ih rest mapped.2
      exact ⟨record :: tail, by simp only [List.map_cons, equal, mapped.1], perm.cons record⟩
  | swap first second rest =>
    cases records with
    | nil => simp at mapped
    | cons a tail =>
      cases tail with
      | nil => simp at mapped
      | cons b tail =>
        simp only [List.map_cons, List.cons.injEq] at mapped
        exact ⟨b :: a :: tail, by simp only [List.map_cons, mapped.1, mapped.2.1,
          mapped.2.2], List.Perm.swap b a tail⟩
  | trans _ _ ih₁ ih₂ =>
    obtain ⟨middle, hm, pm⟩ := ih₁ records mapped
    obtain ⟨last, hl, pl⟩ := ih₂ middle hm
    exact ⟨last, hl, pm.trans pl⟩

/-- Representation lemma. Reordering a finite map with unique keys preserves lookup.
(approved: REV-C9-agy-115a73b2d). -/
theorem LoadedProject.lookup_permutation {κ α : Type} [DecidableEq κ]
    (left right : AList κ α) (permutation : left.Perm right)
    (unique : (List.map Prod.fst left).Nodup) (key : κ) :
    AList.lookup left key = AList.lookup right key := by
  have rightUnique := (permutation.map Prod.fst).nodup unique
  cases hl : AList.lookup left key with
  | none =>
    cases hr : AList.lookup right key with
    | none => rfl
    | some value =>
      have member := (LoadedProject.lookup_iff_mem right rightUnique key value).mp hr
      have equal := (LoadedProject.lookup_iff_mem left unique key value).mpr
        (permutation.symm.subset member)
      simp [hl] at equal
  | some value =>
    have member := (LoadedProject.lookup_iff_mem left unique key value).mp hl
    exact ((LoadedProject.lookup_iff_mem right rightUnique key value).mpr
      (permutation.subset member)).symm

/-- Helper (congruence). The independent admission specification transports across arbitrary
file and site permutations, including different byte sequences in distinct files.
(approved: REV-C9-agy-115a73b2d). -/
theorem ProjectSources.realizes_permutation (input other : ProjectSources)
    (files : input.files.Perm other.files) (sites : input.sites.Perm other.sites)
    (project : LoadedProject) (realizes : input.Realizes project) :
    ∃ reordered, other.Realizes reordered := by
  obtain ⟨sources, rendered, perm⟩ := LoadedProject.lift_permutation
    (fun entry : SourcePath × SourceText => (entry.1.render, entry.2)) files
    project.sources realizes.1
  refine ⟨⟨sources, other.sites⟩, rendered,
    (perm.map Prod.fst).nodup realizes.2.1, rfl,
    (sites.map Prod.fst).nodup realizes.2.2.2.1, ?_⟩
  intro entry member
  obtain ⟨bytes, found, loc⟩ := realizes.2.2.2.2 entry (sites.symm.subset member)
  exact ⟨bytes, (LoadedProject.lookup_permutation project.sources sources perm
    realizes.2.1 entry.2.path).symm.trans found, loc⟩

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). Reordering either complete
input list changes neither admission nor refusal. The selected first error and table enumeration
may change; the theorem deliberately promises admission equivalence, not diagnostic equality. -/
theorem LoadedProject.load_order_independent (input other : ProjectSources)
    (files : input.files.Perm other.files) (sites : input.sites.Perm other.sites) :
    (∃ project, LoadedProject.load input = .admitted project) ↔
      (∃ project, LoadedProject.load other = .admitted project) := by
  constructor
  · rintro ⟨project, admitted⟩
    obtain ⟨reordered, realizes⟩ := ProjectSources.realizes_permutation input other files sites
      project ((LoadedProject.load_admitted input project).mp admitted)
    exact ⟨reordered, (LoadedProject.load_admitted other reordered).mpr realizes⟩
  · rintro ⟨project, admitted⟩
    obtain ⟨reordered, realizes⟩ := ProjectSources.realizes_permutation other input files.symm sites.symm
      project ((LoadedProject.load_admitted other project).mp admitted)
    exact ⟨reordered, (LoadedProject.load_admitted input reordered).mpr realizes⟩

/-- Law (subject `ConcreteSpec.Core.Observe.LoadedProject.load`). C6's source-trap consumer
interface: supply the successful load and the program's site-set equality, then membership in that
site set supplies a span, a valid structured path, byte correspondence and a resolved trap terminal.
C6’s runFrom_source_trap needs no caller-supplied found or pathOk premises: use the witnesses here. -/
theorem LoadedProject.load_trap_resolves (input : ProjectSources) (kind : TrapKind) :
    match LoadedProject.load input with
    | .refused _ => True
    | .admitted project => ∀ programSites : List SiteId,
        programSites = List.map Prod.fst input.sites → ∀ site, site ∈ programSites →
        ∃ sp bytes, AList.lookup project.sites site = some sp ∧
          sp.PathOk ∧ AList.lookup project.sources sp.path = some bytes ∧ sp.LocationOk bytes ∧
          trapTerminal project.sites kind site = some (.trapped kind site sp (trapLine kind site sp)) := by
  cases loaded : LoadedProject.load input with
  | refused _ => trivial
  | admitted project =>
    intro programSites programEqual site member
    rw [programEqual] at member
    obtain ⟨entry, entryMember, entrySite⟩ := List.mem_map.mp member
    rcases entry with ⟨entryId, sp⟩
    dsimp only at entrySite
    subst entryId
    have complete := LoadedProject.load_complete input
    rw [loaded] at complete
    have found := (complete site sp).mpr entryMember
    have valid := LoadedProject.load_locations input
    rw [loaded] at valid
    obtain ⟨pathOk, bytes, fileFound, loc⟩ := valid site sp found
    exact ⟨sp, bytes, found, pathOk, fileFound, loc, by simp [trapTerminal, found]⟩

namespace SourceLoadDemo

/-- Closed boundary inputs, independently supplied bytes and spans. -/
def good : ProjectSources := ⟨[("src/main.con", tabSource)], [(⟨0⟩, tabSpan)]⟩
def admitted : LoadedProject := ⟨[(mainSourcePath, tabSource)], good.sites⟩
def duplicate : ProjectSources := { good with sites := good.sites ++ good.sites }
def duplicateFiles : ProjectSources := { good with files := good.files ++ good.files }
def missing : ProjectSources := { good with files := [] }
def badSpan : Span := { tabSpan with col := 1, col_pos := by decide }
def badLocation : ProjectSources :=
  { good with sites := [(⟨0⟩, badSpan)] }
def parentPath : ProjectSources := ⟨[("..", tabSource)], []⟩
def twoSites : ProjectSources := { good with sites := [(⟨0⟩, tabSpan), (⟨1⟩, tabSpan)] }
def reversed : ProjectSources := { twoSites with sites := twoSites.sites.reverse }

/-- Mutant that accepts an out-of-project spelling as the main source file. -/
def acceptParent (input : ProjectSources) : SourceLoadResult :=
  LoadedProject.load { input with files := List.map (fun entry =>
    (if entry.1 = ".." then mainSourcePath.render else entry.1, entry.2)) input.files }

/-- Mutant that keeps the first duplicate site and discards the second. -/
def keepFirst (input : ProjectSources) : SourceLoadResult :=
  let sites := input.sites.foldl (fun kept entry =>
    if (AList.lookup kept entry.1).isSome then kept else kept ++ [entry]) []
  LoadedProject.load { input with sites := sites }

/-- Mutant that drops the bad entries of a missing-file input and reports partial success. -/
def dropBad (input : ProjectSources) : SourceLoadResult :=
  match LoadedProject.parseFiles input.files with
  | .error _ => .admitted ⟨[], []⟩
  | .ok sources => .admitted ⟨sources, input.sites.filter (fun entry =>
      (LoadedProject.siteIssue sources entry).isNone)⟩

/-- Mutant that admits stored coordinates without checking the source bytes. -/
def ignoreLocation (input : ProjectSources) : SourceLoadResult :=
  match LoadedProject.parseFiles input.files with
  | .error issue => .refused issue
  | .ok sources => .admitted ⟨sources, input.sites⟩

/-- Mutant that fabricates an unrelated site diagnostic. -/
def fabricatedRefusal (_input : ProjectSources) : SourceLoadResult := .refused (.duplicateSite ⟨99⟩)

/-- Mutant that admits duplicate identities unchanged. -/
def allowDuplicates (input : ProjectSources) : SourceLoadResult :=
  match LoadedProject.parseFiles input.files with
  | .error issue => .refused issue
  | .ok sources => .admitted ⟨sources, input.sites⟩

/-- Mutant that imposes an unrequested order on otherwise valid site input. -/
def orderedOnly (input : ProjectSources) : SourceLoadResult :=
  if (List.map (fun entry => entry.1.id) input.sites).Pairwise (· < ·) then
    LoadedProject.load input else .refused (.duplicateSite ⟨0⟩)

/-- Mutant that loses the complete admitted site table. -/
def loseSites (input : ProjectSources) : SourceLoadResult :=
  match LoadedProject.load input with
  | .refused issue => .refused issue
  | .admitted project => .admitted { project with sites := [] }

/-- Helper (computation). The parent spelling contradicts the structured renderer independently
of the loader's checks. (approved: REV-C9-agy-115a73b2d). -/
theorem parent_invalid : ¬ ∃ project, parentPath.Realizes project := by
  rintro ⟨project, raw, _⟩
  have member : ("..", tabSource) ∈ List.map
      (fun entry : SourcePath × SourceText => (entry.1.render, entry.2)) project.sources := by
    rw [raw]; simp [parentPath]
  obtain ⟨entry, _, equal⟩ := List.mem_map.mp member
  exact entry.1.render_safe.2.2.1 (congrArg Prod.fst equal)

/-- Helper (computation). Two equal site identities are not a unique input set.
(approved: REV-C9-agy-115a73b2d). -/
theorem duplicate_invalid : ¬ ∃ project, duplicate.Realizes project := by
  rintro ⟨project, h⟩
  have unique := h.2.2.2.1
  change [SiteId.mk 0, SiteId.mk 0].Nodup at unique
  simp at unique

/-- Helper (computation). A referenced file cannot be resolved in an empty file input.
(approved: REV-C9-agy-115a73b2d). -/
theorem missing_invalid : ¬ ∃ project, missing.Realizes project := by
  rintro ⟨project, raw, _, _, _, locations⟩
  have empty : project.sources = [] := by
    cases hs : project.sources with
    | nil => rfl
    | cons _ _ => simp [hs, missing] at raw
  obtain ⟨bytes, found, _⟩ := locations (⟨0⟩, tabSpan) (by simp [missing, good])
  simp [empty, AList.lookup] at found

/-- Helper (computation). Repeated raw files parse to repeated structured identities, so the
independent unique-file requirement rejects them. (approved: REV-C9-agy-115a73b2d). -/
theorem duplicateFiles_invalid : ¬ ∃ project, duplicateFiles.Realizes project := by
  rintro ⟨project, h⟩
  have parsed := (LoadedProject.parseFiles_iff duplicateFiles.files project.sources).mpr h.1
  have computed : LoadedProject.parseFiles duplicateFiles.files =
      .ok [(mainSourcePath, tabSource), (mainSourcePath, tabSource)] := rfl
  rw [computed, Except.ok.injEq] at parsed
  have unique := h.2.1
  rw [← parsed] at unique
  simp at unique

/-- Helper (computation). The independently supplied bytes refute the fabricated column.
(approved: REV-C9-agy-115a73b2d). -/
theorem badLocation_invalid : ¬ ∃ project, badLocation.Realizes project := by
  rintro ⟨project, h⟩
  obtain ⟨bytes, found, loc⟩ := h.2.2.2.2 (⟨0⟩, badSpan) (by simp [badLocation])
  have member := (LoadedProject.lookup_iff_mem project.sources h.2.1 badSpan.path bytes).mp found
  have rawMember : (badSpan.path.render, bytes) ∈ badLocation.files := by
    rw [← h.1]
    exact List.mem_map.mpr ⟨(badSpan.path, bytes), member, rfl⟩
  have equal : badSpan.path.render = "src/main.con" ∧ bytes = tabSource := by
    simpa [badLocation, good] using rawMember
  rw [equal.2] at loc
  exact (by decide : ¬ badSpan.LocationOk tabSource) loc

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_admitted`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.good`). -/
example : LoadedProject.load good = .admitted admitted ↔ good.Realizes admitted :=
  LoadedProject.load_admitted good admitted

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_admitted`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.good`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.loseSites`). -/
example : ¬ (loseSites good = .admitted admitted ↔ good.Realizes admitted) := by
  intro equivalent
  have realizes : good.Realizes admitted := by
    refine ⟨rfl, by decide, rfl, by decide, ?_⟩
    intro entry member
    have equal : entry = (SiteId.mk 0, tabSpan) := by simpa [good] using member
    subst entry
    exact ⟨tabSource, rfl, by decide⟩
  have h := equivalent.mpr realizes
  have different : loseSites good ≠ .admitted admitted := by decide
  exact different h

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_admitted`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`). -/
example : LoadedProject.load duplicate = .admitted admitted ↔ duplicate.Realizes admitted :=
  LoadedProject.load_admitted duplicate admitted

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_admitted`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.keepFirst`). -/
example : ¬ (keepFirst duplicate = .admitted admitted ↔ duplicate.Realizes admitted) := by
  intro equivalent
  exact duplicate_invalid ⟨admitted, equivalent.mp (by decide)⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_admitted`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentPath`). -/
example : LoadedProject.load parentPath = .admitted ⟨admitted.sources, parentPath.sites⟩ ↔
    parentPath.Realizes ⟨admitted.sources, parentPath.sites⟩ :=
  LoadedProject.load_admitted parentPath ⟨admitted.sources, parentPath.sites⟩

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_admitted`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentPath`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.acceptParent`). -/
example : ¬ (acceptParent parentPath = .admitted ⟨admitted.sources, parentPath.sites⟩ ↔
    parentPath.Realizes ⟨admitted.sources, parentPath.sites⟩) := by
  intro equivalent
  exact parent_invalid ⟨_, equivalent.mp (by decide)⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_complete`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.good`). -/
example : match LoadedProject.load good with
    | .refused _ => True
    | .admitted project => ∀ site sp,
        AList.lookup project.sites site = some sp ↔ (site, sp) ∈ good.sites :=
  LoadedProject.load_complete good

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_complete`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.good`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.loseSites`). -/
example : ¬ (match loseSites good with
    | .refused _ => True
    | .admitted project => ∀ site sp,
        AList.lookup project.sites site = some sp ↔ (site, sp) ∈ good.sites) := by
  intro complete
  have impossible := (complete ⟨0⟩ tabSpan).mpr (by decide)
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_complete`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.missing`). -/
example : match LoadedProject.load missing with
    | .refused _ => True
    | .admitted project => ∀ site sp,
        AList.lookup project.sites site = some sp ↔ (site, sp) ∈ missing.sites :=
  LoadedProject.load_complete missing

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_complete`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.missing`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.dropBad`). -/
example : ¬ (match dropBad missing with
    | .refused _ => True
    | .admitted project => ∀ site sp,
        AList.lookup project.sites site = some sp ↔ (site, sp) ∈ missing.sites) := by
  intro complete
  have impossible := (complete ⟨0⟩ tabSpan).mpr (by decide)
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_locations`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation`). -/
example : match LoadedProject.load badLocation with
    | .refused _ => True
    | .admitted project => SiteTable.Valid project.sources project.sites :=
  LoadedProject.load_locations badLocation

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_locations`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.ignoreLocation`). -/
example : ¬ (match ignoreLocation badLocation with
    | .refused _ => True
    | .admitted project => SiteTable.Valid project.sources project.sites) := by
  intro valid
  obtain ⟨_, bytes, found, loc⟩ := valid ⟨0⟩ badSpan (by rfl)
  have equal : bytes = tabSource := by
    change some tabSource = some bytes at found
    exact (Option.some.inj found).symm
  subst bytes
  exact (by decide : ¬ badSpan.LocationOk tabSource) loc

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentPath`). -/
example : match LoadedProject.load parentPath with
    | .admitted _ => True
    | .refused issue => issue.Names parentPath :=
  LoadedProject.load_refusal_names parentPath

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentPath`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.fabricatedRefusal`). -/
example : ¬ (match fabricatedRefusal parentPath with
    | .admitted _ => True
    | .refused issue => issue.Names parentPath) := by
  change ¬ (SiteId.mk 99 ∈ ([] : List SiteId))
  simp

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentPath`). -/
example : ∃ issue, LoadedProject.load parentPath = .refused issue :=
  LoadedProject.load_refuses parentPath parent_invalid

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentPath`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.acceptParent`). -/
example : (¬ ∃ project, parentPath.Realizes project) ∧
    ¬ (∃ issue, acceptParent parentPath = .refused issue) := by
  refine ⟨parent_invalid, ?_⟩
  rintro ⟨issue, h⟩
  have accepted : acceptParent parentPath = .admitted ⟨admitted.sources, parentPath.sites⟩ := by decide
  rw [accepted] at h
  cases h

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_unique`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`). -/
example : match LoadedProject.load duplicate with
    | .refused _ => True
    | .admitted project => (List.map Prod.fst project.sites).Nodup :=
  LoadedProject.load_unique duplicate

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_unique`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.allowDuplicates`). -/
example : ¬ (match allowDuplicates duplicate with
    | .refused _ => True
    | .admitted project => (List.map Prod.fst project.sites).Nodup) := by
  change ¬ [SiteId.mk 0, SiteId.mk 0].Nodup
  simp

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_order_independent`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.twoSites`). -/
example : (∃ project, LoadedProject.load twoSites = .admitted project) ↔
    (∃ project, LoadedProject.load reversed = .admitted project) :=
  LoadedProject.load_order_independent twoSites reversed (.refl _) (List.reverse_perm _).symm

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_order_independent`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.twoSites`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.orderedOnly`). -/
example : twoSites.files.Perm reversed.files ∧ twoSites.sites.Perm reversed.sites ∧
    ¬ ((∃ project, orderedOnly twoSites = .admitted project) ↔
      (∃ project, orderedOnly reversed = .admitted project)) := by
  refine ⟨.refl _, (List.reverse_perm _).symm, ?_⟩
  intro equivalent
  have left : ∃ project, orderedOnly twoSites = .admitted project := ⟨⟨admitted.sources, twoSites.sites⟩, by decide⟩
  obtain ⟨project, impossible⟩ := equivalent.mp left
  have refused : orderedOnly reversed = .refused (.duplicateSite ⟨0⟩) := by decide
  rw [refused] at impossible
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_trap_resolves`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.good`). -/
example : match LoadedProject.load good with
    | .refused _ => True
    | .admitted project => ∀ programSites : List SiteId,
        programSites = List.map Prod.fst good.sites → ∀ site, site ∈ programSites →
        ∃ sp bytes, AList.lookup project.sites site = some sp ∧
          sp.PathOk ∧ AList.lookup project.sources sp.path = some bytes ∧ sp.LocationOk bytes ∧
          trapTerminal project.sites .abort site = some (.trapped .abort site sp (trapLine .abort site sp)) :=
  LoadedProject.load_trap_resolves good .abort

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_trap_resolves`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.good`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.loseSites`). -/
example : ¬ (match loseSites good with
    | .refused _ => True
    | .admitted project => ∀ programSites : List SiteId,
        programSites = List.map Prod.fst good.sites → ∀ site, site ∈ programSites →
        ∃ sp bytes, AList.lookup project.sites site = some sp ∧
          sp.PathOk ∧ AList.lookup project.sources sp.path = some bytes ∧ sp.LocationOk bytes ∧
          trapTerminal project.sites .abort site = some (.trapped .abort site sp (trapLine .abort site sp))) := by
  intro resolves
  obtain ⟨sp, _, impossible, _⟩ := resolves [⟨0⟩] rfl ⟨0⟩ (by decide)
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`). -/
example : ∃ issue, LoadedProject.load duplicate = .refused issue :=
  LoadedProject.load_refuses duplicate duplicate_invalid

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.allowDuplicates`). -/
example : (¬ ∃ project, duplicate.Realizes project) ∧
    ¬ (∃ issue, allowDuplicates duplicate = .refused issue) := by
  refine ⟨duplicate_invalid, ?_⟩
  rintro ⟨issue, impossible⟩
  have accepted : allowDuplicates duplicate = .admitted ⟨admitted.sources, duplicate.sites⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`). -/
example : match LoadedProject.load duplicate with
    | .admitted _ => True
    | .refused issue => issue.Names duplicate :=
  LoadedProject.load_refusal_names duplicate

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicate`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.fabricatedRefusal`). -/
example : ¬ (match fabricatedRefusal duplicate with
    | .admitted _ => True
    | .refused issue => issue.Names duplicate) := by
  change ¬ (SiteId.mk 99 ∈ List.map Prod.fst duplicate.sites)
  decide

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicateFiles`). -/
example : ∃ issue, LoadedProject.load duplicateFiles = .refused issue :=
  LoadedProject.load_refuses duplicateFiles duplicateFiles_invalid

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicateFiles`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.allowDuplicates`). -/
example : (¬ ∃ project, duplicateFiles.Realizes project) ∧
    ¬ (∃ issue, allowDuplicates duplicateFiles = .refused issue) := by
  refine ⟨duplicateFiles_invalid, ?_⟩
  rintro ⟨issue, impossible⟩
  have accepted : allowDuplicates duplicateFiles = .admitted ⟨[(mainSourcePath, tabSource), (mainSourcePath, tabSource)], duplicateFiles.sites⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicateFiles`). -/
example : match LoadedProject.load duplicateFiles with
    | .admitted _ => True
    | .refused issue => issue.Names duplicateFiles :=
  LoadedProject.load_refusal_names duplicateFiles

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.duplicateFiles`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.fabricatedRefusal`). -/
example : ¬ (match fabricatedRefusal duplicateFiles with
    | .admitted _ => True
    | .refused issue => issue.Names duplicateFiles) := by
  change ¬ (SiteId.mk 99 ∈ List.map Prod.fst duplicateFiles.sites)
  decide

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.missing`). -/
example : ∃ issue, LoadedProject.load missing = .refused issue :=
  LoadedProject.load_refuses missing missing_invalid

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.missing`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.dropBad`). -/
example : (¬ ∃ project, missing.Realizes project) ∧
    ¬ (∃ issue, dropBad missing = .refused issue) := by
  refine ⟨missing_invalid, ?_⟩
  rintro ⟨issue, impossible⟩
  have accepted : dropBad missing = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.missing`). -/
example : match LoadedProject.load missing with
    | .admitted _ => True
    | .refused issue => issue.Names missing :=
  LoadedProject.load_refusal_names missing

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.missing`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.fabricatedRefusal`). -/
example : ¬ (match fabricatedRefusal missing with
    | .admitted _ => True
    | .refused issue => issue.Names missing) := by
  change ¬ (SiteId.mk 99 ∈ List.map Prod.fst missing.sites)
  decide

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation`). -/
example : ∃ issue, LoadedProject.load badLocation = .refused issue :=
  LoadedProject.load_refuses badLocation badLocation_invalid

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.ignoreLocation`). -/
example : (¬ ∃ project, badLocation.Realizes project) ∧
    ¬ (∃ issue, ignoreLocation badLocation = .refused issue) := by
  refine ⟨badLocation_invalid, ?_⟩
  rintro ⟨issue, impossible⟩
  have accepted : ignoreLocation badLocation = .admitted ⟨admitted.sources, badLocation.sites⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation`). -/
example : match LoadedProject.load badLocation with
    | .admitted _ => True
    | .refused issue => issue.Names badLocation :=
  LoadedProject.load_refusal_names badLocation

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refusal_names`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.badLocation`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.fabricatedRefusal`). -/
example : ¬ (match fabricatedRefusal badLocation with
    | .admitted _ => True
    | .refused issue => issue.Names badLocation) := by
  change ¬ (SiteId.mk 99 ∈ List.map Prod.fst badLocation.sites)
  decide

/-- A raw file input shared by the malformed-category evidence. -/
def malformedInput (raw : String) : ProjectSources := ⟨[(raw, tabSource)], []⟩

/-- Mutant that drops a malformed source input and claims successful admission. -/
def forgiveMalformed (input : ProjectSources) : SourceLoadResult :=
  match LoadedProject.parseFiles input.files with
  | .error _ => .admitted ⟨[], []⟩
  | .ok _ => LoadedProject.load input

/-- Closed empty refusal input and exact typed diagnostic. -/
def emptyInput : ProjectSources := malformedInput ""
def emptyDiagnostic : SourcePath.Refusal := ⟨"", "".toList, .empty⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.emptyInput`). -/
example : ∃ issue, LoadedProject.load emptyInput = .refused issue ∧ issue.Names emptyInput :=
  LoadedProject.load_refuses_malformed emptyInput ("", tabSource) emptyDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.emptyInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("", tabSource) ∈ emptyInput.files ∧
    SourcePath.parse "" = .error emptyDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed emptyInput = .refused issue ∧ issue.Names emptyInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed emptyInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed dot refusal input and exact typed diagnostic. -/
def dotInput : ProjectSources := malformedInput "."
def dotDiagnostic : SourcePath.Refusal := ⟨".", ".".toList, .dot⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.dotInput`). -/
example : ∃ issue, LoadedProject.load dotInput = .refused issue ∧ issue.Names dotInput :=
  LoadedProject.load_refuses_malformed dotInput (".", tabSource) dotDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.dotInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : ((".", tabSource) ∈ dotInput.files ∧
    SourcePath.parse "." = .error dotDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed dotInput = .refused issue ∧ issue.Names dotInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed dotInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed parent refusal input and exact typed diagnostic. -/
def parentInput : ProjectSources := malformedInput ".."
def parentDiagnostic : SourcePath.Refusal := ⟨"..", "..".toList, .outOfProject⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentInput`). -/
example : ∃ issue, LoadedProject.load parentInput = .refused issue ∧ issue.Names parentInput :=
  LoadedProject.load_refuses_malformed parentInput ("..", tabSource) parentDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.parentInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("..", tabSource) ∈ parentInput.files ∧
    SourcePath.parse ".." = .error parentDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed parentInput = .refused issue ∧ issue.Names parentInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed parentInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed absolute refusal input and exact typed diagnostic. -/
def absoluteInput : ProjectSources := malformedInput "/src"
def absoluteDiagnostic : SourcePath.Refusal := ⟨"/src", "".toList, .absolute⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.absoluteInput`). -/
example : ∃ issue, LoadedProject.load absoluteInput = .refused issue ∧ issue.Names absoluteInput :=
  LoadedProject.load_refuses_malformed absoluteInput ("/src", tabSource) absoluteDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.absoluteInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("/src", tabSource) ∈ absoluteInput.files ∧
    SourcePath.parse "/src" = .error absoluteDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed absoluteInput = .refused issue ∧ issue.Names absoluteInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed absoluteInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed drive refusal input and exact typed diagnostic. -/
def driveInput : ProjectSources := malformedInput "C:main"
def driveDiagnostic : SourcePath.Refusal := ⟨"C:main", "C:main".toList, .drivePrefixed⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.driveInput`). -/
example : ∃ issue, LoadedProject.load driveInput = .refused issue ∧ issue.Names driveInput :=
  LoadedProject.load_refuses_malformed driveInput ("C:main", tabSource) driveDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.driveInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("C:main", tabSource) ∈ driveInput.files ∧
    SourcePath.parse "C:main" = .error driveDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed driveInput = .refused issue ∧ issue.Names driveInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed driveInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed emptyComponent refusal input and exact typed diagnostic. -/
def emptyComponentInput : ProjectSources := malformedInput "src//main"
def emptyComponentDiagnostic : SourcePath.Refusal := ⟨"src//main", "".toList, .emptyComponent⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.emptyComponentInput`). -/
example : ∃ issue, LoadedProject.load emptyComponentInput = .refused issue ∧ issue.Names emptyComponentInput :=
  LoadedProject.load_refuses_malformed emptyComponentInput ("src//main", tabSource) emptyComponentDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.emptyComponentInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("src//main", tabSource) ∈ emptyComponentInput.files ∧
    SourcePath.parse "src//main" = .error emptyComponentDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed emptyComponentInput = .refused issue ∧ issue.Names emptyComponentInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed emptyComponentInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed backslash refusal input and exact typed diagnostic. -/
def backslashInput : ProjectSources := malformedInput "src\\main"
def backslashDiagnostic : SourcePath.Refusal := ⟨"src\\main", "src\\main".toList, .backslash⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.backslashInput`). -/
example : ∃ issue, LoadedProject.load backslashInput = .refused issue ∧ issue.Names backslashInput :=
  LoadedProject.load_refuses_malformed backslashInput ("src\\main", tabSource) backslashDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.backslashInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("src\\main", tabSource) ∈ backslashInput.files ∧
    SourcePath.parse "src\\main" = .error backslashDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed backslashInput = .refused issue ∧ issue.Names backslashInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed backslashInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed innerDot refusal input and exact typed diagnostic. -/
def innerDotInput : ProjectSources := malformedInput "a/./b"
def innerDotDiagnostic : SourcePath.Refusal := ⟨"a/./b", ".".toList, .dot⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.innerDotInput`). -/
example : ∃ issue, LoadedProject.load innerDotInput = .refused issue ∧ issue.Names innerDotInput :=
  LoadedProject.load_refuses_malformed innerDotInput ("a/./b", tabSource) innerDotDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.innerDotInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("a/./b", tabSource) ∈ innerDotInput.files ∧
    SourcePath.parse "a/./b" = .error innerDotDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed innerDotInput = .refused issue ∧ issue.Names innerDotInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed innerDotInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed innerParent refusal input and exact typed diagnostic. -/
def innerParentInput : ProjectSources := malformedInput "a/../b"
def innerParentDiagnostic : SourcePath.Refusal := ⟨"a/../b", "..".toList, .outOfProject⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.innerParentInput`). -/
example : ∃ issue, LoadedProject.load innerParentInput = .refused issue ∧ issue.Names innerParentInput :=
  LoadedProject.load_refuses_malformed innerParentInput ("a/../b", tabSource) innerParentDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.innerParentInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("a/../b", tabSource) ∈ innerParentInput.files ∧
    SourcePath.parse "a/../b" = .error innerParentDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed innerParentInput = .refused issue ∧ issue.Names innerParentInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed innerParentInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

/-- Closed trailingSlash refusal input and exact typed diagnostic. -/
def trailingSlashInput : ProjectSources := malformedInput "a/"
def trailingSlashDiagnostic : SourcePath.Refusal := ⟨"a/", "".toList, .emptyComponent⟩

/-- Instance (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.trailingSlashInput`). -/
example : ∃ issue, LoadedProject.load trailingSlashInput = .refused issue ∧ issue.Names trailingSlashInput :=
  LoadedProject.load_refuses_malformed trailingSlashInput ("a/", tabSource) trailingSlashDiagnostic
    (by decide) (by rfl)

/-- Planted (law `ConcreteSpec.Core.Observe.LoadedProject.load_refuses_malformed`, case `ConcreteSpec.Core.Observe.SourceLoadDemo.trailingSlashInput`, subject `ConcreteSpec.Core.Observe.LoadedProject.load`, mutant `ConcreteSpec.Core.Observe.SourceLoadDemo.forgiveMalformed`). -/
example : (("a/", tabSource) ∈ trailingSlashInput.files ∧
    SourcePath.parse "a/" = .error trailingSlashDiagnostic) ∧
    ¬ (∃ issue, forgiveMalformed trailingSlashInput = .refused issue ∧ issue.Names trailingSlashInput) := by
  refine ⟨⟨by decide, rfl⟩, ?_⟩
  rintro ⟨issue, impossible, _⟩
  have accepted : forgiveMalformed trailingSlashInput = .admitted ⟨[], []⟩ := rfl
  rw [accepted] at impossible
  cases impossible

end SourceLoadDemo

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

/-- Inserting silent steps leaves exactly the same visible events, in the same order. -/
theorem SilentInsertion.visible {before after : List (Step S)} (h : SilentInsertion before after) :
    visible before = visible after := by
  induction h with
  | nil => rfl
  | silent _ ih => exact ih
  | keepSilent _ ih => exact ih
  | emit event _ ih => exact congrArg (List.cons event) ih

/-- Any finite number of silent steps may be inserted anywhere in a finite terminated run. -/
theorem silent_internal_steps {before after : List (Step S)} (h : SilentInsertion before after)
    (terminal : Terminal) : SameObservation (.finite before terminal) (.finite after terminal) := by
  exact ⟨h.visible, rfl⟩

/-- The same arbitrary finite stuttering preserves a blocked run and its pending request. -/
theorem blocked_silent_internal_steps {before after : List (Step S)}
    (h : SilentInsertion before after) (request : Request S) :
    SameObservation (.blocked before request) (.blocked after request) := by
  exact ⟨h.visible, rfl⟩

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

/-- One more step adds exactly its optional event to the visible prefix. -/
theorem visiblePrefix_succ (steps : Nat → Step S) (n : Nat) :
    visiblePrefix steps (n + 1) = visiblePrefix steps n ++ (steps n).event?.toList := by
  cases step : steps n <;>
    simp [visiblePrefix, List.range_succ, visible, List.filterMap_append, Step.event?, step]

/-- A shorter step prefix's events are a prefix of a longer step prefix's events. -/
theorem visiblePrefix_prefix (steps : Nat → Step S) {m n : Nat} (h : m ≤ n) :
    ∃ rest, visiblePrefix steps n = visiblePrefix steps m ++ rest := by
  obtain ⟨d, rfl⟩ := Nat.exists_eq_add_of_le h
  induction d with
  | zero => exact ⟨[], by simp⟩
  | succ d ih =>
    obtain ⟨rest, pref⟩ := ih (by omega)
    refine ⟨rest ++ (steps (m + d)).event?.toList, ?_⟩
    rw [show m + (d + 1) = m + d + 1 by omega, visiblePrefix_succ, pref, List.append_assoc]

/-- A strictly increasing step-index map is monotone on every pair of indices. -/
theorem StepIndexIncreasing.monotone {f : Nat → Nat} (increasing : StepIndexIncreasing f)
    {m n : Nat} (h : m ≤ n) : f m ≤ f n := by
  induction n generalizing m with
  | zero => simp [show m = 0 by omega]
  | succ n ih =>
    by_cases atEnd : m = n + 1
    · subst m; exact Nat.le_refl _
    · exact Nat.le_trans (ih (by omega)) (Nat.le_of_lt (increasing n))

/-- A strictly increasing step-index map strictly increases on every ordered pair. -/
theorem StepIndexIncreasing.strict {f : Nat → Nat} (increasing : StepIndexIncreasing f)
    {m n : Nat} (h : m < n) : f m < f n := by
  have first := increasing m
  have rest := increasing.monotone (show m + 1 ≤ n by omega)
  omega

/-- A strictly increasing map on natural step indices is unbounded. -/
theorem StepIndexIncreasing.apply_le {f : Nat → Nat} (increasing : StepIndexIncreasing f) (n : Nat) :
    n ≤ f n := by
  induction n with
  | zero => exact Nat.zero_le _
  | succ n ih => have next := increasing n; omega

/-- At every stuttered index `m`, some old prefix `n` has exactly the same events: all retained
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

/-- The stuttered prefix through the position just before `f n` is exactly the old prefix before
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

/-- **Infinite stuttering.** Retaining step `n` at `f n` for a strictly increasing `f`, with every
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

/-- **Full-observation preservation.** An observation projected from `x` is also an observation of
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

/-- No infinite execution, including one that eventually becomes silent forever, is equivalent to
any finite successful or trapped termination. -/
theorem infinite_not_termination (steps : Nat → Step S) (finiteSteps : List (Step S))
    (terminal : Terminal) :
    ¬ SameObservation (.infinite steps) (.finite finiteSteps terminal) := by
  simp [SameObservation]

/-- In particular, an infinite silent run never acquires a terminal observation. -/
theorem infinite_silent_not_termination (terminal : Terminal) :
    ¬ SameObservation (.infinite (fun _ => Step.silent)) (.finite (S := S) [] terminal) :=
  infinite_not_termination _ [] terminal

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

private def decodeWithKind (kind : TrapKind) (cs : List Char) :
    Option (SiteId × String × Nat × Nat) := do
  let rest ← stripPrefix (linePrefix kind) cs
  let rest ← stripPrefix ")".toList rest.reverse
  let (site, rest) ← parseRevNat rest
  let rest ← stripPrefix " (site ".toList.reverse rest
  let (col, rest) ← parseRevNat rest
  let rest ← stripPrefix ":".toList rest
  let (line, rest) ← parseRevNat rest
  let rest ← stripPrefix ":".toList rest
  if 0 < line then
    if 0 < col then
      some (⟨site⟩, String.ofList rest.reverse, line, col)
    else none
  else none

private def decodeTrapLine (line : String) :
    Option (TrapKind × SiteId × String × Nat × Nat) := do
  let kind ← decodeKind line.toList
  let (site, path, line, col) ← decodeWithKind kind line.toList
  some (kind, site, path, line, col)

/-- Helper (computation). Regroups the exact trap-line characters. (approved: REV-C9-agy-115a73b2d). -/
private theorem trapLineChars_prefix (kind : TrapKind) (site : SiteId) (sp : Span) :
    trapLineChars kind site sp = linePrefix kind ++
      (sp.path.render.toList ++ ":".toList ++ (toString sp.line).toList ++ ":".toList ++
        (toString sp.col).toList ++ " (site ".toList ++ (toString site.id).toList ++ ")".toList) := by
  simp [trapLineChars, linePrefix, List.append_assoc]

/-- Helper (computation). Computes the decoder on rendered fields. (approved: REV-C9-agy-115a73b2d). -/
private theorem decodeWithKind_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeWithKind kind (trapLineChars kind site sp) = some (site, sp.path.render, sp.line, sp.col) := by
  have hline : 0 < sp.line := sp.line_pos
  have hcol : 0 < sp.col := sp.col_pos
  unfold decodeWithKind
  rw [trapLineChars_prefix, stripPrefix_append]
  simp [List.reverse_append, stripPrefix, parseRevNat_digits, hline, hcol]

/-- Helper (computation). Computes the complete renderer/decoder round trip. (approved: REV-C9-agy-115a73b2d). -/
private theorem decodeTrapLine_render (kind : TrapKind) (site : SiteId) (sp : Span) :
    decodeTrapLine (trapLine kind site sp) = some (kind, site, sp.path.render, sp.line, sp.col) := by
  simp [decodeTrapLine, trapLine, decodeKind_render, decodeWithKind_render]

/-- Law (subject `ConcreteSpec.Core.Observe.trapLine`).
**Trap-line injectivity over the rendered fields.** Equal rendered lines have the same trap
kind, typed site, path, 1-based line and byte column. Byte offsets are deliberately absent from the
trap-line format and are not claimed to be recovered. -/
theorem trapLine_injective (kind₁ : TrapKind) (site₁ : SiteId) (sp₁ : Span) :
    ∀ (kind₂ : TrapKind) (site₂ : SiteId) (sp₂ : Span),
    trapLine kind₁ site₁ sp₁ = trapLine kind₂ site₂ sp₂ →
    kind₁ = kind₂ ∧ site₁ = site₂ ∧ sp₁.path = sp₂.path ∧
      sp₁.line = sp₂.line ∧ sp₁.col = sp₂.col := by
  intro kind₂ site₂ sp₂ h
  have decoded := congrArg decodeTrapLine h
  rw [decodeTrapLine_render, decodeTrapLine_render] at decoded
  simp only [Option.some.injEq, Prod.mk.injEq] at decoded
  exact ⟨decoded.1, decoded.2.1, SourcePath.render_injective _ _ decoded.2.2.1,
    decoded.2.2.2⟩

/-- A second range with exactly the fields rendered by the trap line. -/
def spDifferentRange : Span :=
  ⟨Demo.sp.path, 41, 41, by decide, Demo.sp.line, Demo.sp.col, Demo.sp.line_pos, Demo.sp.col_pos⟩

/-- Instance (law `ConcreteSpec.Core.Observe.trapLine_injective`, case `ConcreteSpec.Core.Observe.Demo.sp`). -/
example : ∀ kind site sp, trapLine .overflow Demo.site Demo.sp = trapLine kind site sp →
    TrapKind.overflow = kind ∧ Demo.site = site ∧ Demo.sp.path = sp.path ∧
      Demo.sp.line = sp.line ∧ Demo.sp.col = sp.col :=
  trapLine_injective .overflow Demo.site Demo.sp

/-- Counter-example to the old full-span claim: the lines agree but the ranges do not. -/
example : trapLine .overflow Demo.site Demo.sp = trapLine .overflow Demo.site spDifferentRange ∧
    Demo.sp ≠ spDifferentRange := by decide

/-- A renderer mutant that omits the byte column while retaining every other rendered field. -/
def trapLineWithoutColumn (kind : TrapKind) (site : SiteId) (sp : Span) : String :=
  "concrete: trap: " ++ kind.name ++ " at " ++ sp.path.render ++ ":" ++ toString sp.line ++
    " (site " ++ toString site.id ++ ")"

/-- A span differing from the demo only in its rendered byte column. -/
def spDifferentColumn : Span := { Demo.sp with col := Demo.sp.col + 1, col_pos := by omega }

/-- Planted (law `ConcreteSpec.Core.Observe.trapLine_injective`, case `ConcreteSpec.Core.Observe.Demo.sp`, subject `ConcreteSpec.Core.Observe.trapLine`, mutant `ConcreteSpec.Core.Observe.trapLineWithoutColumn`). -/
example : ¬ (∀ kind site sp,
    trapLineWithoutColumn .overflow Demo.site Demo.sp = trapLineWithoutColumn kind site sp →
      TrapKind.overflow = kind ∧ Demo.site = site ∧ Demo.sp.path = sp.path ∧
        Demo.sp.line = sp.line ∧ Demo.sp.col = sp.col) := by
  intro injective
  have equal := injective .overflow Demo.site spDifferentColumn (by decide)
  have different : Demo.sp.col ≠ spDifferentColumn.col := by decide
  exact different equal.2.2.2.2

/-- Helper (inversion). Recovers the validated span and the renderer equation from a returned
trap. This is lookup plumbing, never independent soundness evidence. (approved: APPR-C9-agy-3e22e6417). -/
theorem trapTerminal_valid_location_inv (sources : SourceTable) (sites : SiteTable)
    (valid : SiteTable.Valid sources sites) (kind : TrapKind) (site : SiteId)
    (sp : Span) (line : String)
    (trapped : trapTerminal sites kind site = some (.trapped kind site sp line)) :
    sp.PathOk ∧ ∃ source, AList.lookup sources sp.path = some source ∧
      sp.endOffset ≤ source.length ∧
      sp.line = source.lineAt sp.startOffset ∧
      sp.col = source.columnAt sp.startOffset ∧
      line = trapLine kind site sp := by
  unfold trapTerminal at trapped
  cases hlookup : AList.lookup sites site with
  | none => simp [hlookup] at trapped
  | some found =>
    simp only [hlookup, Option.map_some, Option.some.injEq, Terminal.trapped.injEq,
      true_and] at trapped
    rcases trapped with ⟨rfl, rfl⟩
    obtain ⟨pathOk, source, hsource, locOk⟩ := valid site found hlookup
    exact ⟨pathOk, source, hsource, locOk.1, locOk.2.1, locOk.2.2, rfl⟩

/-- Concrete source and site tables connecting `SiteTable.Valid`, `tabSource`/`tabSpan`, and
`trapTerminal`. -/
def validSources : SourceTable := [(mainSourcePath, tabSource)]
def validSites : SiteTable := [(⟨0⟩, tabSpan)]
def badTabSpan : Span := { tabSpan with col := 1, col_pos := by decide }

/-- Mutant `trapTerminal` that preserves the registered span in the terminal structure while
rendering a fabricated column-1 line instead of the source-derived byte column. -/
def fabricatedTrapTerminal (sites : SiteTable) (kind : TrapKind) (site : SiteId) : Option Terminal :=
  (AList.lookup sites site).map fun sp =>
    let badSpan : Span := { sp with col := 1, col_pos := by decide }
    .trapped kind site sp (trapLine kind site badSpan)

/-- Mutant `trapTerminal` that silently drops every trap observation instead of consulting `sites`. -/
def silentTrapTerminal (_sites : SiteTable) (_kind : TrapKind) (_site : SiteId) : Option Terminal :=
  none

/-- Independently specified source observation: an unregistered site produces `none`, while a
registered site produces a trap terminal for that exact site's span whose decoded line recovers the
requested identity and byte-derived source coordinates. Non-trap terminals cannot describe a trap. -/
def SourceTrapObservation (sources : SourceTable) (sites : SiteTable) (kind : TrapKind)
    (site : SiteId) : Option Terminal → Prop
  | none => AList.lookup sites site = none
  | some (.trapped actualKind actualSite sp line) =>
    actualKind = kind ∧ actualSite = site ∧ AList.lookup sites site = some sp ∧ sp.PathOk ∧
      ∃ source, AList.lookup sources sp.path = some source ∧ sp.endOffset ≤ source.length ∧
        decodeTrapLine line = some (kind, site, sp.path.render,
          source.lineAt sp.startOffset, source.columnAt sp.startOffset)
  | some _ => False

/-- Law (subject `ConcreteSpec.Core.Observe.trapTerminal`).
Parsing a produced trap observation recovers the requested identity, its registered site span, and
its source-derived coordinates, while an unregistered site is refused (`none`). -/
theorem trapTerminal_valid_location (sources : SourceTable) (sites : SiteTable)
    (valid : SiteTable.Valid sources sites) (kind : TrapKind) (site : SiteId) :
    SourceTrapObservation sources sites kind site (trapTerminal sites kind site) := by
  cases found : AList.lookup sites site with
  | none => simp [trapTerminal, found, SourceTrapObservation]
  | some sp =>
    obtain ⟨pathOk, source, sourceFound, bounds, line, col, _⟩ :=
      trapTerminal_valid_location_inv sources sites valid kind site sp (trapLine kind site sp)
        (by simp [trapTerminal, found])
    simp only [trapTerminal, found, Option.map_some, SourceTrapObservation]
    refine ⟨True.intro, True.intro, True.intro, pathOk, source, sourceFound, bounds, ?_⟩
    rw [decodeTrapLine_render, line, col]

/-- Helper (computation). The concrete tab source and site table satisfy the table invariant.
(approved: APPR-C9-agy-3e22e6417). -/
theorem validSites_valid : SiteTable.Valid validSources validSites := by
  intro site sp hlookup
  by_cases h : (⟨0⟩ : SiteId) = site
  · subst site
    simp [validSites, AList.lookup] at hlookup
    subst sp
    exact ⟨by decide, tabSource, rfl, by decide⟩
  · simp [validSites, AList.lookup, h] at hlookup

/-- Instance (law `ConcreteSpec.Core.Observe.trapTerminal_valid_location`, case `ConcreteSpec.Core.Observe.validSites`). -/
example : SourceTrapObservation validSources validSites .abort ⟨0⟩
    (trapTerminal validSites .abort ⟨0⟩) :=
  trapTerminal_valid_location validSources validSites validSites_valid .abort ⟨0⟩

/-- Planted (law `ConcreteSpec.Core.Observe.trapTerminal_valid_location`, case `ConcreteSpec.Core.Observe.validSites`, subject `ConcreteSpec.Core.Observe.trapTerminal`, mutant `ConcreteSpec.Core.Observe.fabricatedTrapTerminal`). -/
example : SiteTable.Valid validSources validSites ∧
    ¬ SourceTrapObservation validSources validSites .abort ⟨0⟩
      (fabricatedTrapTerminal validSites .abort ⟨0⟩) := by
  refine ⟨validSites_valid, ?_⟩
  rintro ⟨_, _, _, _, source, found, _, decoded⟩
  have equal : source = tabSource := by
    have hf : mainSourcePath = tabSpan.path ∧ tabSource = source := by
      simpa [validSources, fabricatedTrapTerminal, validSites, AList.lookup] using found
    exact hf.2.symm
  subst source
  rw [decodeTrapLine_render] at decoded
  have badColumn := congrArg (fun tuple => tuple.2.2.2.2) (Option.some.inj decoded)
  revert badColumn
  decide

/-- Planted (law `ConcreteSpec.Core.Observe.trapTerminal_valid_location`, case `ConcreteSpec.Core.Observe.validSites`, subject `ConcreteSpec.Core.Observe.trapTerminal`, mutant `ConcreteSpec.Core.Observe.silentTrapTerminal`). -/
example : SiteTable.Valid validSources validSites ∧
    ¬ SourceTrapObservation validSources validSites .abort ⟨0⟩
      (silentTrapTerminal validSites .abort ⟨0⟩) := by
  refine ⟨validSites_valid, ?_⟩
  dsimp [silentTrapTerminal, SourceTrapObservation]
  decide

/-- Rendering uses the byte column supplied by elaboration, not a Unicode-scalar column. -/
theorem trap_line_uses_byte_columns (kind : TrapKind) (site : SiteId) (sp : Span) (before : String)
    (h : Span.HasByteColumn sp before) :
    trapLine kind site sp = String.ofList (
      "concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList ++ sp.path.render.toList ++
      ":".toList ++ (toString sp.line).toList ++ ":".toList ++ (toString (byteColumn before)).toList ++
      " (site ".toList ++ (toString site.id).toList ++ ")".toList) := by
  rw [trapLine, trapLineChars, h]

/-- A tab advances the byte column by exactly one. -/
theorem byteColumn_tab (before : String) : byteColumn (before ++ "\t") = byteColumn before + 1 := by
  have htab : "\t".utf8ByteSize = 1 := by decide
  simp [byteColumn, String.utf8ByteSize_append, htab, Nat.add_assoc]

/-! ## C1 observations -/

variable {O : Exec.Operands}

/-- Output-only extensions preserve a complete valid trace. -/
theorem ValidTrace.outputs {C : OperationContract S} {input : C.Input} {hist : List (Event S)}
    (h : ValidTrace C input none hist) (outputs : List S.Output) :
    ValidTrace C input none (hist ++ outputs.map Event.output) := by
  induction outputs generalizing hist with
  | nil => simpa
  | cons output outputs ih =>
    simpa [List.append_assoc] using ih (ValidTrace.output h)

/-- The adapter's output/request/response/output answer shape extends a complete valid trace. -/
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

/-- Projection distributes over the C1 event-history append performed by `runFrom`. -/
theorem ExecAdapter.project_append (A : ExecAdapter O S) (xs ys : List O.Event) :
    A.project (xs ++ ys) = A.project xs ++ A.project ys := by
  simp [ExecAdapter.project, List.flatMap_append]

/-- Every C1 evaluator report from a well-formed adapter and oracle has a complete valid event
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

/-- A blocked C1 observation has exactly the pending `ValidTrace` state and history that its
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

/-- A well-formed C1 run supplies the blocking premise required by `reportObservation`. -/
theorem runFrom_blockingPermitted (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (ρ : Exec.Oracle O)
    (wellFormed : ExecAdapter.WellFormed A C input ρ)
    {fuel : Nat} {hist : List O.Event} {c : Exec.Config O}
    (valid : CompleteTrace C input (A.project hist)) :
    ExecAdapter.BlockingPermitted P A C input (Exec.runFrom P ρ fuel hist c) := by
  intro c' p vs site store k q ending pending typed
  exact (runFrom_validTrace P A C input ρ wellFormed fuel hist c valid).2 ending pending typed

/-- **Output before a trap is kept.** The terminal C1 observation contains the projection of the
whole `Steps` history, not merely a suffix or the trap itself, and contains the exact trap line. -/
theorem output_before_trap_kept (A : ExecAdapter O S) {P : Exec.Program O} {c c' : Exec.Config O}
    {events : List O.Event} (steps : Exec.Steps P c events c') {kind : TrapKind} {site : SiteId}
    {sp : Span} (final : c'.final? = some (.trapped kind site))
    (resolved : AList.lookup A.sites site = some sp) :
    stepsObservation A steps = some ⟨A.project events, .trapped kind site sp (trapLine kind site sp)⟩ := by
  simp [stepsObservation, final, ExecAdapter.terminal?, trapTerminal, resolved]

/-- The premise on `reportObservation` really supplies the operation contract's blocking
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

/-- A permitted pending C1 request observed through `noAnswer` retains the earlier event history
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

/-- Fuel exhaustion produces no fabricated external observation or event. -/
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

/-- The answering oracle satisfies the adapter contract for either blocking policy. -/
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

/-- The blocking oracle is well formed exactly because this contract permits blocking. -/
theorem blockingWellFormed : ExecAdapter.WellFormed adapter permittingContract () blockingOracle := by
  constructor
  · intro hist p vs site store a answer typed
    simp [blockingOracle] at answer
  · intro hist p vs site store a q answer typed
    simp [blockingOracle] at answer
  · intro hist p vs site store q answer typed
    trivial

/-- Non-vacuity for `runFrom_validTrace`: the accepted answer really runs and its request and
response form a complete valid trace. -/
example : CompleteTrace forbiddingContract () (adapter.project answeredReport.events) :=
  (runFrom_validTrace program adapter forbiddingContract () answeringOracle
    (answeringWellFormed _) 1 [] (requesting 3) .idle).1

/-- Planted bad response for `runFrom_validTrace`: `7` does not double `3`. -/
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

/-- Non-vacuity for `runFrom_noAnswer_pending_valid`: the run is pending on `3`, and the pending
trace and blocking permission both hold. -/
example :
    ValidTrace permittingContract () (some (query 3))
      (adapter.project blockedReport.events ++ [request 3]) ∧
    permittingContract.mayBlock ()
      (adapter.project blockedReport.events ++ [request 3]) (query 3) :=
  runFrom_noAnswer_pending_valid program adapter permittingContract () blockingOracle
    blockingWellFormed (fuel := 1) (hist := []) (c := requesting 3) (c' := requesting 3)
    (q := query 3) .idle rfl rfl rfl

/-- Planted blocking refusal for `runFrom_noAnswer_pending_valid`: the same pending trace cannot
supply a blocking permission that its contract denies. -/
example : ¬ (ValidTrace forbiddingContract () (some (query 3)) [request 3] ∧
    forbiddingContract.mayBlock () [request 3] (query 3)) := by
  intro invalid
  exact invalid.2

/-- Concrete evidence used by the two report-level examples below. -/
theorem blockedPermitted : ExecAdapter.BlockingPermitted program adapter permittingContract ()
    blockedReport :=
  runFrom_blockingPermitted program adapter permittingContract () blockingOracle
    blockingWellFormed (fuel := 1) (hist := []) (c := requesting 3) .idle

/-- Non-vacuity for `runFrom_blockingPermitted`: its premise is satisfied by a real `noAnswer`
report. -/
example : ExecAdapter.BlockingPermitted program adapter permittingContract () blockedReport :=
  blockedPermitted

/-- Planted blocking refusal for `runFrom_blockingPermitted`: this same report does not satisfy
`BlockingPermitted` when the operation may not block. -/
example : ¬ ExecAdapter.BlockingPermitted program adapter forbiddingContract () blockedReport := by
  intro permitted
  exact permitted (c := requesting 3) (p := 3) (vs := []) (site := ⟨0⟩) (store := ())
    (q := query 3) rfl rfl rfl

/-- Non-vacuity for `report_noAnswer_mayBlock`: the report premise supplies the actual operation
permission for the pending request. -/
example : permittingContract.mayBlock ()
    (adapter.project blockedReport.events ++ [request 3]) (query 3) :=
  report_noAnswer_mayBlock program adapter permittingContract () [] (requesting 3)
    blockedPermitted rfl rfl

/-- Planted blocking refusal for `report_noAnswer_mayBlock`: changing only `mayBlock` makes the
claimed permission false. -/
example : ¬ forbiddingContract.mayBlock () [request 3] (query 3) := by
  intro forbidden
  exact forbidden

/-- The permitted report has exactly its pending blocked observation. -/
theorem blockedObservation :
    reportObservation program adapter permittingContract () blockedReport blockedPermitted =
      some (.blocked (adapter.project blockedReport.events ++ [request 3]) (query 3)) :=
  report_noAnswer_blocked program adapter permittingContract () [] (requesting 3)
    3 [] ⟨0⟩ () _ (query 3) blockedPermitted rfl rfl

/-- Non-vacuity for `report_noAnswer_blocked`: the pending request and its history are observed. -/
example : reportObservation program adapter permittingContract () blockedReport blockedPermitted =
    some (.blocked (adapter.project blockedReport.events ++ [request 3]) (query 3)) :=
  blockedObservation

/-- Planted observation for `report_noAnswer_blocked`: the pending request cannot instead be
observed as successful termination. -/
example : reportObservation program adapter permittingContract () blockedReport blockedPermitted ≠
    some (.terminal ⟨[request 3], .exited 0⟩) := by
  rw [blockedObservation]
  intro invalid
  cases invalid

end AdapterDemo

end ConcreteSpec.Core.Observe

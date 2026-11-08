import ConcreteSpec.Spec.Ident

/-!
# Laws of typed and persistent identities

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). See `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Core

/-- **Injective renumbering preserves site-table well-formedness.** No two distinct old sites can
become one new site, so the renamed table still has no duplicate sites. -/
theorem SiteKeyTable.nodup_renumber (r : SiteRenumbering) (table : SiteKeyTable)
    (distinct : table.NoDupSites) : (table.renumber r).NoDupSites := by
  induction table with
  | nil => exact distinct
  | cons entry rest ih =>
    rcases entry with ⟨site, key⟩
    simp only [SiteKeyTable.NoDupSites, SiteKeyTable.renumber, SiteKeyTable.renumberSites,
      List.map_cons, List.nodup_cons] at distinct ⊢
    constructor
    · intro member
      simp only [List.map_map, Function.comp_def] at member
      obtain ⟨oldSite, oldMember, equal⟩ := List.mem_map.1 member
      apply distinct.1
      apply List.mem_map.2
      refine ⟨oldSite, oldMember, ?_⟩
      have ids : oldSite.1.id = site.id := r.injective (congrArg SiteId.id equal)
      rcases oldSite with ⟨oldSite, oldValue⟩
      cases site with
      | mk siteId =>
        cases oldSite with
        | mk oldId => simp_all
    · exact ih distinct.2

/-- **Lookup is equivariant under injective renumbering.** Looking up the renamed site in the
renamed table returns exactly the original persistent key; no entry is merged or lost. -/
theorem SiteKeyTable.lookup_renumber (r : SiteRenumbering) :
    ∀ (table : SiteKeyTable) (site : SiteId),
      AList.lookup (table.renumber r) ⟨r.apply site.id⟩ = AList.lookup table site
  | [], _ => rfl
  | (entrySite, key) :: rest, site => by
    by_cases same : entrySite = site
    · subst site
      simp [SiteKeyTable.renumber, SiteKeyTable.renumberSites, AList.lookup]
    · have renamed : (⟨r.apply entrySite.id⟩ : SiteId) ≠ ⟨r.apply site.id⟩ := by
        intro equal
        apply same
        have ids : entrySite.id = site.id := r.injective (congrArg SiteId.id equal)
        cases entrySite
        cases site
        simp_all
      simp only [SiteKeyTable.renumber, SiteKeyTable.renumberSites, List.map_cons,
        AList.lookup, renamed, same, ↓reduceIte]
      exact SiteKeyTable.lookup_renumber r rest site

/-- **An outside-function edit preserves that function's persistent-key sequence.** This is the
consequence consumers use from elaboration's named `OutsideFunctionEdit` obligation. -/
theorem OutsideFunctionEdit.persistentKeys {before after : FnId → SiteKeyTable} {edited : FnId}
    (edit : OutsideFunctionEdit before after edited) (f : FnId) (outside : f ≠ edited) :
    List.map Prod.snd (after f) = List.map Prod.snd (before f) := edit.keysUnchanged f outside

/-- **A different-kind insertion does not rekey an existing sibling.** The per-kind ordinal filter
is the actual key-assignment operation, not an invariant rechecked by the theorem. -/
theorem KeyConstruct.key_insert_differentKind (before after : List KeyConstruct)
    (inserted target : KeyConstruct) (different : inserted.kind ≠ target.kind) :
    target.key (before ++ inserted :: after) = target.key (before ++ after) := by
  simp [KeyConstruct.key, KeyConstruct.segment, List.filter_append, different]

/-- **A named insertion does not consume an unnamed ordinal.** Every existing sibling key is
therefore unchanged, independently of where the named sibling is inserted. -/
theorem KeyConstruct.key_insert_named (before after : List KeyConstruct)
    (label : String) (kind : ConstructKind) (children : List KeyConstruct) (target : KeyConstruct) :
    target.key (before ++ KeyConstruct.mk kind (.named label) children :: after) =
      target.key (before ++ after) := by
  simp [KeyConstruct.key, KeyConstruct.segment, List.filter_append, ConstructName.isUnnamed]

/-- Helper (projection). Renaming preserves participation in unnamed-sibling ordinals. -/
theorem ConstructName.isUnnamed_rename (constructName : ConstructName) (name : String) :
    (constructName.rename name).isUnnamed = constructName.isUnnamed := by
  cases constructName <;> rfl

/-- **Renaming one item leaves every sibling key unchanged.** Renaming preserves the item's kind
and named/unnamed status, so it cannot change any sibling's ordinal. -/
theorem KeyConstruct.key_rename_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (name : String) :
    other.key (before ++ target.rename name :: after) = other.key (before ++ target :: after) := by
  cases target with
  | mk kind constructName children =>
    cases constructName with
    | named old =>
      simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.rename,
        ConstructName.rename, ConstructName.isUnnamed, List.filter_append]
    | unnamed label =>
      cases label <;>
        by_cases same : kind = other.kind <;>
          simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.rename,
            ConstructName.rename, ConstructName.isUnnamed, List.filter_append, same]

/-- Prefix a descendant's relative path by its enclosing item's segment. -/
def KeyConstruct.under (preceding : List KeyConstruct) (construct : KeyConstruct)
    (descendant : FunctionRelativeKey) : FunctionRelativeKey :=
  ⟨construct.segment preceding :: descendant.path⟩

/-- **Renaming a named item recomputes its assigned key to the new label.** Together with
`key_rename_other`, this confines the effect of renaming to the item and its descendants. -/
theorem KeyConstruct.key_rename_named (preceding : List KeyConstruct) (kind : ConstructKind)
    (old : String) (children : List KeyConstruct) (name : String) :
    ((KeyConstruct.mk kind (.named old) children).rename name).key preceding =
      ⟨[⟨kind, .label name⟩]⟩ := by
  simp [KeyConstruct.key, KeyConstruct.segment, KeyConstruct.rename, ConstructName.rename]

/-- Inserting a different-kind sibling preserves every descendant's complete assigned path,
including descendants several unnamed levels beneath the root. -/
theorem KeyConstruct.descendant_insert_differentKind (before after : List KeyConstruct)
    (inserted target : KeyConstruct) (route : List Nat) (different : inserted.kind ≠ target.kind) :
    target.descendantKey (before ++ inserted :: after) route =
      target.descendantKey (before ++ after) route := by
  simp [KeyConstruct.descendantKey, KeyConstruct.segment, List.filter_append, different]

/-- Inserting a named sibling preserves complete descendant paths, not just root segments. -/
theorem KeyConstruct.descendant_insert_named (before after : List KeyConstruct)
    (label : String) (kind : ConstructKind) (children : List KeyConstruct)
    (target : KeyConstruct) (route : List Nat) :
    target.descendantKey (before ++ KeyConstruct.mk kind (.named label) children :: after) route =
      target.descendantKey (before ++ after) route := by
  simp [KeyConstruct.descendantKey, KeyConstruct.segment, List.filter_append,
    ConstructName.isUnnamed]

/-- Renaming a sibling cannot change the complete assigned path of any descendant of another
item. The root's preceding siblings are scanned by the same assignment used for full trees. -/
theorem KeyConstruct.descendant_rename_other (before after : List KeyConstruct)
    (target other : KeyConstruct) (name : String) (route : List Nat) :
    other.descendantKey (before ++ target.rename name :: after) route =
      other.descendantKey (before ++ target :: after) route := by
  have root := KeyConstruct.key_rename_other before after target other name
  have segment := congrArg (fun key => key.path) root
  simp only [KeyConstruct.key, List.cons.injEq, and_true] at segment
  simp only [KeyConstruct.descendantKey, segment]

/-- Renaming a named ancestor replaces its segment throughout its descendants' assigned paths.
The suffix is computed from the actual subtree and remains unchanged. -/
theorem KeyConstruct.descendant_rename_named (preceding : List KeyConstruct)
    (kind : ConstructKind) (old name : String) (children : List KeyConstruct) (route : List Nat) :
    ((KeyConstruct.mk kind (.named old) children).rename name).descendantKey preceding route =
      (KeyConstruct.pathAt children route).map (fun suffix =>
        ⟨⟨kind, .label name⟩ :: suffix.path⟩) := by
  simp [KeyConstruct.descendantKey, KeyConstruct.rename, ConstructName.rename,
    KeyConstruct.segment]

/-- Helper (projection). A successful table lookup is in bounds. -/
theorem SpecializationTable.entryAt_lt_length {Ty Cap : Type}
    {entries : List (SpecializationId Ty Cap)} {index : Nat} {requested : SpecializationId Ty Cap}
    (present : entryAt entries index = some requested) : index < entries.length :=
  (List.getElem?_eq_some_iff.1 present).1

/-- Helper (projection). A successful entry search resolves to the requested specialization. -/
theorem SpecializationTable.entryIndex_sound {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap] :
    ∀ (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap) (index : Nat),
      entryIndex entries requested = some index → entryAt entries index = some requested
  | [], requested, index, found => by simp [entryIndex] at found
  | entry :: rest, requested, index, found => by
    by_cases same : entry = requested
    · subst requested
      simp [entryIndex] at found
      subst index
      rfl
    · simp only [entryIndex, same, ↓reduceIte, Option.map_eq_some_iff] at found
      obtain ⟨old, oldIndex, equal⟩ := found
      subst index
      exact entryIndex_sound rest requested old oldIndex

/-- Helper (projection). Interning resolves its requested specialization at the returned index. -/
theorem SpecializationTable.intern_sound {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap) :
    entryAt (intern entries requested).1 (intern entries requested).2 = some requested := by
  unfold intern
  cases found : entryIndex entries requested with
  | none => simp [entryAt]
  | some index => exact entryIndex_sound entries requested index found

/-- Helper (projection). Interning preserves every old numeric entry. -/
theorem SpecializationTable.intern_preserves {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (entries : List (SpecializationId Ty Cap)) (requested : SpecializationId Ty Cap)
    (index : Nat) (old : SpecializationId Ty Cap)
    (present : entryAt entries index = some old) :
    entryAt (intern entries requested).1 index = some old := by
  unfold intern
  cases found : entryIndex entries requested with
  | some existing => exact present
  | none =>
    have bounded : index < entries.length := entryAt_lt_length present
    unfold entryAt at present ⊢
    rw [List.getElem?_append_left bounded]
    exact present

/-- Helper (projection). A failed entry search really means the specialization is absent. -/
theorem SpecializationTable.entryIndex_none_not_mem {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] : ∀ (entries : List (SpecializationId Ty Cap))
    (requested : SpecializationId Ty Cap), entryIndex entries requested = none → requested ∉ entries
  | [], requested, found => by simp
  | entry :: rest, requested, found => by
    by_cases same : entry = requested
    · subst requested
      simp [entryIndex] at found
    · simp only [entryIndex, same, ↓reduceIte, Option.map_eq_none_iff] at found
      simp only [List.mem_cons, not_or]
      exact ⟨Ne.symm same, entryIndex_none_not_mem rest requested found⟩

/-- Canonicalization never creates duplicate specialization entries. -/
theorem SpecializationTable.canonicalize_nodup {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap] :
    ∀ requests : List (SpecializationRequest Ty Cap), (canonicalize requests).entries.Nodup
  | [] => by simp [canonicalize]
  | request :: rest => by
    have tail := canonicalize_nodup rest
    simp only [canonicalize]
    unfold intern
    cases found : entryIndex (canonicalize rest).entries request.specialization with
    | some index => simpa [found] using tail
    | none =>
      simp only [List.nodup_append]
      refine ⟨tail, by simp, ?_⟩
      intro old oldMember new newMember
      simp only [List.mem_singleton] at newMember
      subst new
      intro equal
      subst old
      exact entryIndex_none_not_mem (canonicalize rest).entries request.specialization found oldMember

/-- **Every canonical request edge resolves to its own specialization.** Edges inherited from the
tail remain valid whether the new head reuses an entry or appends one. -/
theorem SpecializationTable.canonicalize_edge_sound {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] : ∀ (requests : List (SpecializationRequest Ty Cap))
    (edge : SpecializationEdge Ty Cap), edge ∈ (canonicalize requests).edges →
      entryAt (canonicalize requests).entries edge.entry = some edge.request.specialization
  | [], edge, member => by simp [canonicalize] at member
  | request :: rest, edge, member => by
    simp only [canonicalize, List.mem_cons] at member
    cases member with
    | inl head =>
      subst edge
      exact intern_sound (canonicalize rest).entries request.specialization
    | inr tail =>
      exact intern_preserves (canonicalize rest).entries request.specialization edge.entry
        edge.request.specialization (canonicalize_edge_sound rest edge tail)

/-- **Every request keeps an edge.** No request is dropped during canonicalization, and its edge
resolves to the requested entry. -/
theorem SpecializationTable.canonicalize_complete {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] : ∀ (requests : List (SpecializationRequest Ty Cap))
    (request : SpecializationRequest Ty Cap), request ∈ requests →
      ∃ edge ∈ (canonicalize requests).edges, edge.request = request ∧
        entryAt (canonicalize requests).entries edge.entry = some request.specialization
  | [], request, member => by simp at member
  | head :: rest, request, member => by
    simp only [List.mem_cons] at member
    cases member with
    | inl equal =>
      subst request
      let assigned := intern (canonicalize rest).entries head.specialization
      exact ⟨⟨head, assigned.2⟩, by simp [canonicalize, assigned], rfl,
        intern_sound (canonicalize rest).entries head.specialization⟩
    | inr tail =>
      obtain ⟨edge, kept, identity, resolved⟩ := canonicalize_complete rest request tail
      refine ⟨edge, ?_, identity, intern_preserves (canonicalize rest).entries head.specialization
        edge.entry request.specialization resolved⟩
      simp only [canonicalize, List.mem_cons]
      exact Or.inr kept

/-- Helper (projection). Two numeric entries holding one specialization in a duplicate-free table
are one entry. -/
theorem SpecializationTable.entry_unique {Ty Cap : Type}
    {entries : List (SpecializationId Ty Cap)} (distinct : entries.Nodup)
    {first second : Nat} {requested : SpecializationId Ty Cap}
    (atFirst : entryAt entries first = some requested) (atSecond : entryAt entries second = some requested) :
    first = second := by
  apply distinct.eq_of_getElem?_eq (entryAt_lt_length atFirst)
  exact atFirst.trans atSecond.symm

/-- **Equal generic and argument lists share one canonical entry even at distinct request sites.**
The proof compares the actual edges produced by canonicalization. -/
theorem SpecializationTable.canonicalize_share {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] (requests : List (SpecializationRequest Ty Cap))
    (first second : SpecializationRequest Ty Cap) (firstMember : first ∈ requests)
    (secondMember : second ∈ requests)
    (same : first.specialization = second.specialization) :
    ∃ firstEdge secondEdge, firstEdge ∈ (canonicalize requests).edges ∧
      secondEdge ∈ (canonicalize requests).edges ∧ firstEdge.request = first ∧
      secondEdge.request = second ∧ firstEdge.entry = secondEdge.entry := by
  obtain ⟨firstEdge, firstKept, firstIdentity, firstResolved⟩ :=
    canonicalize_complete requests first firstMember
  obtain ⟨secondEdge, secondKept, secondIdentity, secondResolved⟩ :=
    canonicalize_complete requests second secondMember
  refine ⟨firstEdge, secondEdge, firstKept, secondKept, firstIdentity, secondIdentity, ?_⟩
  apply entry_unique (canonicalize_nodup requests) firstResolved
  rw [secondResolved, same]

/-- **Different arguments cannot be coalesced.** The canonical edges of requests with different
specialization identities point to different numeric entries. -/
theorem SpecializationTable.canonicalize_distinct {Ty Cap : Type} [DecidableEq Ty]
    [DecidableEq Cap] (requests : List (SpecializationRequest Ty Cap))
    (first second : SpecializationRequest Ty Cap) (firstMember : first ∈ requests)
    (secondMember : second ∈ requests) (different : first.specialization ≠ second.specialization) :
    ∃ firstEdge secondEdge, firstEdge ∈ (canonicalize requests).edges ∧
      secondEdge ∈ (canonicalize requests).edges ∧ firstEdge.request = first ∧
      secondEdge.request = second ∧ firstEdge.entry ≠ secondEdge.entry := by
  obtain ⟨firstEdge, firstKept, firstIdentity, firstResolved⟩ :=
    canonicalize_complete requests first firstMember
  obtain ⟨secondEdge, secondKept, secondIdentity, secondResolved⟩ :=
    canonicalize_complete requests second secondMember
  refine ⟨firstEdge, secondEdge, firstKept, secondKept, firstIdentity, secondIdentity, ?_⟩
  intro sameEntry
  apply different
  apply Option.some.inj
  rw [← firstResolved, ← secondResolved, sameEntry]

/-- Helper (projection). A collision returned by the pair check has exactly the checked modules as its two sides. -/
theorem LoadCollision.between?_sound (first second : LoadedModule) (collision : LoadCollision)
    (found : between? first second = some collision) :
    collision.first = first ∧ collision.second = second := by
  by_cases package : first.module.package = second.module.package
  · by_cases origin : first.origin = second.origin
    · simp [between?, package, origin] at found
    · simp [between?, package, origin] at found
      subst collision
      exact ⟨rfl, rfl⟩
  · by_cases top : first.module.topLevel = second.module.topLevel
    · simp [between?, package, top] at found
      subst collision
      exact ⟨rfl, rfl⟩
    · simp [between?, package, top] at found

/-- Helper (projection). A collision returned by the one-module scan names that module and a member of the scanned list. -/
theorem LoadCollision.withModule_sound (first : LoadedModule) :
    ∀ (modules : List LoadedModule) (collision : LoadCollision),
      withModule first modules = some collision →
        collision.first = first ∧ collision.second ∈ modules
  | [], collision, found => by simp [withModule] at found
  | second :: rest, collision, found => by
    unfold withModule at found
    cases pair : between? first second with
    | some direct =>
      rw [pair] at found
      simp only [Option.some.injEq] at found
      subst collision
      exact ⟨(between?_sound first second direct pair).1,
        List.mem_cons.2 (Or.inl (between?_sound first second direct pair).2)⟩
    | none =>
      rw [pair] at found
      obtain ⟨same, member⟩ := withModule_sound first rest collision found
      exact ⟨same, List.mem_cons.2 (Or.inr member)⟩

/-- Helper (projection). Soundness of the complete ordered-pair search. -/
theorem LoadedModules.findCollisionFrom_sound (candidates modules : List LoadedModule)
    (collision : LoadCollision) (found : findCollisionFrom candidates modules = some collision) :
    collision.first ∈ candidates ∧ collision.second ∈ modules := by
  induction candidates with
  | nil => simp [findCollisionFrom] at found
  | cons first rest ih =>
    unfold findCollisionFrom at found
    cases pair : LoadCollision.withModule first modules with
    | some direct =>
      rw [pair] at found
      simp only [Option.some.injEq] at found
      subst collision
      obtain ⟨same, member⟩ := LoadCollision.withModule_sound first modules direct pair
      exact ⟨List.mem_cons.2 (Or.inl same), member⟩
    | none =>
      rw [pair] at found
      obtain ⟨firstMember, secondMember⟩ := ih found
      exact ⟨List.mem_cons.2 (Or.inr firstMember), secondMember⟩

/-- **Load refusal is sound.** A refused collision names two modules that really occur in the
loaded input; neither package identity nor origin is discarded from the diagnostic. -/
theorem LoadedModules.load_refusal_sound (modules : List LoadedModule) (collision : LoadCollision)
    (refused : load modules = .refused collision) : HasCollision modules collision := by
  unfold load at refused
  cases found : findCollision modules with
  | none => simp [found] at refused
  | some foundCollision =>
    simp only [found, LoadResult.refused.injEq] at refused
    subst collision
    exact findCollisionFrom_sound modules modules foundCollision found

/-- Helper (projection). A real pair collision is visible to the ordered-pair checker. -/
theorem LoadCollision.between?_complete (first second : LoadedModule) (collision : LoadCollision)
    (sides : collision.first = first ∧ collision.second = second) :
    ∃ detected, between? first second = some detected := by
  cases collision with
  | topLevel a b sameTop differentPackage =>
    obtain ⟨rfl, rfl⟩ := sides
    exact ⟨.topLevel a b sameTop differentPackage, by
      simp [between?, LoadCollision.first, LoadCollision.second, sameTop, differentPackage]⟩
  | packageOrigin a b samePackage differentOrigins =>
    obtain ⟨rfl, rfl⟩ := sides
    exact ⟨.packageOrigin a b samePackage differentOrigins, by
      simp [between?, LoadCollision.first, LoadCollision.second, samePackage, differentOrigins]⟩

/-- Helper (projection). The one-module scan finds every collision with a member of its input list. -/
theorem LoadCollision.withModule_complete (first : LoadedModule) :
    ∀ (modules : List LoadedModule) (collision : LoadCollision),
      collision.first = first → collision.second ∈ modules →
        ∃ detected, withModule first modules = some detected
  | [], collision, same, member => by simp at member
  | second :: rest, collision, same, member => by
    simp only [List.mem_cons] at member
    cases member with
    | inl equal =>
      obtain ⟨detected, found⟩ := between?_complete first second collision ⟨same, equal⟩
      exact ⟨detected, by simp [withModule, found]⟩
    | inr later =>
      cases pair : between? first second with
      | some detected => exact ⟨detected, by simp [withModule, pair]⟩
      | none =>
        obtain ⟨detected, found⟩ := withModule_complete first rest collision same later
        exact ⟨detected, by simp [withModule, pair, found]⟩

/-- **Load refusal is complete.** Every top-level or same-package/different-origin collision in the
input causes refusal with a two-sided diagnostic. -/
theorem LoadedModules.load_refusal_complete (modules : List LoadedModule) (collision : LoadCollision)
    (present : HasCollision modules collision) : ∃ detected, load modules = .refused detected := by
  obtain ⟨firstMember, secondMember⟩ := present
  have search : ∀ candidates : List LoadedModule, collision.first ∈ candidates →
      ∃ detected, findCollisionFrom candidates modules = some detected := by
    intro candidates candidateMember
    induction candidates with
    | nil => simp at candidateMember
    | cons first rest ih =>
      simp only [List.mem_cons] at candidateMember
      cases candidateMember with
      | inl equal =>
        subst first
        obtain ⟨detected, found⟩ := LoadCollision.withModule_complete collision.first modules collision
          rfl secondMember
        exact ⟨detected, by simp [findCollisionFrom, found]⟩
      | inr later =>
        cases pair : LoadCollision.withModule first modules with
        | some detected => exact ⟨detected, by simp [findCollisionFrom, pair]⟩
        | none =>
          obtain ⟨detected, found⟩ := ih later
          exact ⟨detected, by simp [findCollisionFrom, pair, found]⟩
  obtain ⟨detected, found⟩ := search modules firstMember
  exact ⟨detected, by simp [load, findCollision, found]⟩

/-- Admission is exactly the absence of a collision, not a second coexisting interpretation. -/
theorem LoadedModules.load_admitted (modules : List LoadedModule) (admitted : List LoadedModule)
    (result : load modules = .admitted admitted) : admitted = modules ∧ WellFormed modules := by
  unfold load at result
  cases found : findCollision modules with
  | some collision => simp [found] at result
  | none =>
    simp only [found, LoadResult.admitted.injEq] at result
    subst admitted
    refine ⟨rfl, ?_⟩
    intro collision present
    obtain ⟨detected, refused⟩ := load_refusal_complete modules collision present
    simp [load, found] at refused

/-- Appending a byte updates the source line count by its actual newline step. -/
theorem SourceText.lineAt_append (source : SourceText) (byte : UInt8) :
    (source ++ [byte]).lineAt (source.length + 1) = source.lineAt source.length + if byte = 10 then 1 else 0 := by
  have whole : (source ++ [byte]).take (source.length + 1) = source ++ [byte] :=
    List.take_of_length_le (by simp)
  simp only [SourceText.lineAt]
  rw [whole]
  by_cases newline : byte = 10 <;> simp [newline]

/-- A newline starts a new line. -/
theorem SourceText.lineAt_append_newline (source : SourceText) :
    (source ++ [10]).lineAt (source.length + 1) = source.lineAt source.length + 1 := by
  simpa using SourceText.lineAt_append source 10

/-- A non-newline byte leaves the line unchanged. -/
theorem SourceText.lineAt_append_other (source : SourceText) (byte : UInt8) (other : byte ≠ 10) :
    (source ++ [byte]).lineAt (source.length + 1) = source.lineAt source.length := by
  simpa [other] using SourceText.lineAt_append source byte

/-- Appending a byte updates the byte column: newline resets to one, every other byte adds one. -/
theorem SourceText.columnAt_append (source : SourceText) (byte : UInt8) :
    (source ++ [byte]).columnAt (source.length + 1) =
      if byte = 10 then 1 else source.columnAt source.length + 1 := by
  have whole : (source ++ [byte]).take (source.length + 1) = source ++ [byte] :=
    List.take_of_length_le (by simp)
  simp only [SourceText.columnAt]
  rw [whole]
  by_cases newline : byte = 10 <;> simp [newline]

/-- A tab is one byte and therefore advances exactly one column. -/
theorem SourceText.columnAt_append_tab (source : SourceText) :
    (source ++ [9]).columnAt (source.length + 1) = source.columnAt source.length + 1 := by
  simpa using SourceText.columnAt_append source 9

/-- A newline resets the byte column to one. -/
theorem SourceText.columnAt_append_newline (source : SourceText) :
    (source ++ [10]).columnAt (source.length + 1) = 1 := by
  simpa using SourceText.columnAt_append source 10

/-- Helper (projection). `LocationOk` projects to bounds and the two computed coordinates. -/
theorem Span.locationOk_iff (source : SourceText) (sp : Span) :
    sp.LocationOk source ↔ sp.endOffset ≤ source.length ∧
      sp.line = source.lineAt sp.startOffset ∧ sp.col = source.columnAt sp.startOffset := Iff.rfl

/-- A known category survives encoding and reading at the same specification version. -/
theorem EncodedPremise.read_encode (record : PremiseRecord) (current : record.spec = specVersion) :
    record.encode.read specVersion = some record := by
  cases record with
  | mk kind scope spec =>
    simp only at current
    subst spec
    cases kind <;> rfl

/-- An unknown category is refused rather than assigned a catch-all meaning. -/
theorem EncodedPremise.read_unknown (reader : SpecVersion) (record : EncodedPremise)
    (current : record.spec = reader) (unknown : PremiseKind.decode reader record.category = none) :
    record.read reader = none := by
  simp [EncodedPremise.read, current, unknown]

namespace Vacuity

/-- Helper (projection). A package and module used by the identity examples. -/
def package (name : String := "demo") : PackageId := ⟨name, ⟨"1"⟩⟩

/-- Helper (projection). An enclosing function used by the persistent-key examples. -/
def function : FnId := FnId.mk (ModuleId.mk package ["demo"] (by decide)) "f"

/-- Helper (projection). A second enclosing function. -/
def otherFunction : FnId := FnId.mk (ModuleId.mk package ["demo"] (by decide)) "g"

/-- Helper (projection). A declaration and constant that own obligations outside function bodies. -/
def declaration : DeclId := ⟨function.module, "S"⟩
def constant : ConstId := ⟨function.module, "N"⟩

/-- Helper (projection). Structured paths for a binding, a call and a struct invariant. -/
def bindingKey : StableKey := ⟨.function function, ⟨[⟨.binding, .ordinal 0⟩]⟩⟩
def callKey : StableKey := ⟨.function function, ⟨[⟨.call, .ordinal 0⟩]⟩⟩
def invariantKey : StableKey := ⟨.declaration declaration, ⟨[]⟩⟩
def initializerKey : StableKey := ⟨.constant constant, ⟨[⟨.return, .ordinal 0⟩]⟩⟩

/-- Helper (projection). Two entries whose lexical and persistent identities differ. -/
def siteTable : SiteKeyTable := [(⟨0⟩, bindingKey), (⟨1⟩, callKey)]

/-- Helper (projection). Successor is an injective lexical renumbering. -/
def successor : SiteRenumbering := ⟨Nat.succ, fun _ _ h => Nat.succ.inj h⟩

/-- Helper (projection). A non-injective site function that merges both sites. -/
def collapse : Nat → Nat := fun _ => 0

/-- Instance (law `ConcreteSpec.Core.SiteKeyTable.lookup_renumber`). The second site's key survives
successor renumbering and is found at site two. -/
example : AList.lookup (siteTable.renumber successor) ⟨2⟩ = some callKey := by
  rw [show (⟨2⟩ : SiteId) = ⟨successor.apply (⟨1⟩ : SiteId).id⟩ from rfl,
    SiteKeyTable.lookup_renumber]
  decide

/-- Instance (law `ConcreteSpec.Core.SiteKeyTable.nodup_renumber`). The input has two distinct
sites and the renamed table remains duplicate-free. -/
example : siteTable.NoDupSites ∧ (siteTable.renumber successor).NoDupSites := by
  exact ⟨by decide, SiteKeyTable.nodup_renumber successor siteTable (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.SiteKeyTable.lookup_renumber`). A constant renumbering merges
site one's lookup with site zero's, so the equivariance equation fails. -/
example : AList.lookup (siteTable.renumberSites collapse) ⟨collapse (⟨1⟩ : SiteId).id⟩ ≠
    AList.lookup siteTable ⟨1⟩ := by decide

/-- Planted (law `ConcreteSpec.Core.SiteKeyTable.nodup_renumber`). The same constant renumbering
produces duplicate sites. -/
example : ¬ (siteTable.renumberSites collapse).NoDupSites := by decide

/-- Helper (projection). An assignment outside `otherFunction` leaves `function` untouched. -/
def stableBefore : FnId → SiteKeyTable := fun f => if f = function then siteTable else []
def stableAfter : FnId → SiteKeyTable := fun f =>
  if f = otherFunction then [(⟨2⟩, ⟨.function f, ⟨[⟨.call, .ordinal 0⟩]⟩⟩)]
  else (stableBefore f).renumber successor

/-- Instance (law `ConcreteSpec.Core.OutsideFunctionEdit.persistentKeys`). The concrete structured
assignment satisfies the outside-edit obligation and preserves both keys. -/
example : OutsideFunctionEdit stableBefore stableAfter otherFunction ∧
    List.map Prod.snd (stableAfter function) = [bindingKey, callKey] := by
  refine ⟨⟨?_⟩, rfl⟩
  intro f outside
  simp [stableAfter, outside, SiteKeyTable.renumber, SiteKeyTable.renumberSites, List.map_map,
    Function.comp_def]

/-- Helper (projection). Global-site-derived keys change when another function gains a site. -/
def globalBefore : FnId → SiteKeyTable := fun f =>
  if f = function then [(⟨0⟩, ⟨.function f, ⟨[⟨.binding, .ordinal 3⟩]⟩⟩)] else []
def globalAfter : FnId → SiteKeyTable := fun f =>
  if f = function then [(⟨0⟩, ⟨.function f, ⟨[⟨.binding, .ordinal 4⟩]⟩⟩)] else []

/-- Planted (law `ConcreteSpec.Core.OutsideFunctionEdit.persistentKeys`). Assigning keys by global
site number violates the outside-edit obligation even though the function's own body is unchanged. -/
example : ¬ OutsideFunctionEdit globalBefore globalAfter otherFunction := by
  intro edit
  have changed := edit.persistentKeys function (by decide)
  simp [globalBefore, globalAfter, function] at changed

/-- Helper (projection). An unnamed binding and a named call used by edit-stability examples. -/
def unnamedBinding : KeyConstruct := KeyConstruct.mk .binding (.unnamed none) []
def namedCall : KeyConstruct := KeyConstruct.mk .call (.named "work") []
def labelledBlock : KeyConstruct := KeyConstruct.mk .block (.unnamed (some "outer")) []

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind`). Inserting a call before
an unnamed binding leaves the binding's zero ordinal unchanged. -/
example : unnamedBinding.key [KeyConstruct.mk .call (.unnamed none) []] = unnamedBinding.key [] ∧
    unnamedBinding.key [] = ⟨[⟨.binding, .ordinal 0⟩]⟩ := by
  exact ⟨KeyConstruct.key_insert_differentKind [] []
    (KeyConstruct.mk .call (.unnamed none) []) unnamedBinding (by decide), rfl⟩

/-- Helper (projection). A bad assignment uses one ordinal across all unnamed construct kinds. -/
def allKindsOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal (preceding.filter fun sibling => sibling.name.isUnnamed).length⟩]⟩

/-- Helper (projection). A bad assignment uses raw child indices, counting named items as well. -/
def rawOrdinalKey (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal preceding.length⟩]⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind`). A single ordinal across
all construct kinds changes the binding key after that same insertion. -/
example : allKindsOrdinal [KeyConstruct.mk .call (.unnamed none) []] unnamedBinding ≠
    allKindsOrdinal [] unnamedBinding := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`). A named binding inserted before
an unnamed binding does not consume its ordinal. -/
example : unnamedBinding.key [KeyConstruct.mk .binding (.named "x") []] =
    unnamedBinding.key [] := by
  exact KeyConstruct.key_insert_named [] [] "x" .binding [] unnamedBinding

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def countNamedKey (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal (preceding.filter fun sibling => sibling.kind == target.kind).length⟩]⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`). Counting named siblings in the
ordinal changes the unrelated key. -/
example : countNamedKey [KeyConstruct.mk .binding (.named "x") []] unnamedBinding ≠
    countNamedKey [] unnamedBinding := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`). Renaming the first binding
leaves the second binding's ordinal key unchanged. -/
example : unnamedBinding.key [unnamedBinding.rename "x", unnamedBinding] =
    unnamedBinding.key [unnamedBinding, unnamedBinding] := by
  exact KeyConstruct.key_rename_other [] [unnamedBinding] unnamedBinding unnamedBinding "x"

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def labelOnlyOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  let count := (preceding.filter fun sibling => sibling.kind == target.kind &&
    match sibling.name with | .named _ => false | .unnamed label => label.isNone).length
  ⟨[⟨target.kind, .ordinal count⟩]⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`). Recomputing ordinals from
labels rather than stable named/unnamed status can change a sibling key. -/
example : labelOnlyOrdinal [KeyConstruct.mk .binding (.unnamed (some "x")) []] unnamedBinding ≠
    labelOnlyOrdinal [KeyConstruct.mk .binding (.unnamed none) []] unnamedBinding := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_rename_named`). The real assignment changes
the named call's key to the new label. -/
example : (namedCall.rename "renamed").key [] = ⟨[⟨.call, .label "renamed"⟩]⟩ := by decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def rawChildUnder (preceding : List KeyConstruct) (construct : KeyConstruct) (childIndex : Nat) :
    FunctionRelativeKey := ⟨construct.segment preceding :: [⟨.call, .ordinal childIndex⟩]⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_rename_named`). Rebuilding the key from a raw
child index leaves the old parent label, so it is not the renaming law. -/
example : ¬ (namedCall.key [] = (namedCall.rename "renamed").key []) := by decide

/-- Helper (projection). A nested toy function body with two unnamed levels and a labelled leaf. -/
def nestedBody : KeyConstruct := ⟨.block, .unnamed none,
  [⟨.loop, .unnamed none, [⟨.primitive, .unnamed (some "effect"), []⟩]⟩]⟩
def namedNested : KeyConstruct := ⟨.block, .named "body", nestedBody.children⟩
def nestedPath : FunctionRelativeKey :=
  ⟨[⟨.block, .ordinal 0⟩, ⟨.loop, .ordinal 0⟩, ⟨.primitive, .label "effect"⟩]⟩
def insertedCall : KeyConstruct := ⟨.call, .unnamed none, []⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind`). In a whole
function body, an earlier unnamed call does not renumber the nested block or its descendants. -/
example : KeyConstruct.pathAt [insertedCall, nestedBody] [1, 0, 0] = some nestedPath ∧
    nestedBody.descendantKey [insertedCall] [0, 0] = nestedBody.descendantKey [] [0, 0] := by
  exact ⟨by decide, KeyConstruct.descendant_insert_differentKind [] [] insertedCall nestedBody
    [0, 0] (by decide)⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind`). One ordinal
across all kinds changes the existing block's prefix after the earlier unnamed call. -/
example : ¬ (allKindsOrdinal [insertedCall] nestedBody = allKindsOrdinal [] nestedBody) := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`). An earlier named block
of the same kind leaves the complete three-segment path unchanged. -/
example : nestedBody.descendantKey [namedNested] [0, 0] = some nestedPath ∧
    nestedBody.descendantKey [namedNested] [0, 0] = nestedBody.descendantKey [] [0, 0] := by
  exact ⟨by decide, KeyConstruct.descendant_insert_named [] [] "body" .block nestedBody.children
    nestedBody [0, 0]⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`). Raw child indices change
an unrelated block's prefix when a named sibling is inserted. -/
example : ¬ (rawOrdinalKey [namedNested] nestedBody = rawOrdinalKey [] nestedBody) := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`). Renaming a preceding
unnamed block leaves the entire unrelated descendant path intact. -/
example : nestedBody.descendantKey [nestedBody.rename "pinned"] [0, 0] =
    nestedBody.descendantKey [nestedBody] [0, 0] :=
  KeyConstruct.descendant_rename_other [] [] nestedBody nestedBody "pinned" [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`). Excluding labelled
unnamed siblings from the ordinal sequence changes the unrelated root's prefix. -/
example : ¬ (labelOnlyOrdinal [nestedBody.rename "pinned"] nestedBody =
    labelOnlyOrdinal [nestedBody] nestedBody) := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_named`). Renaming the named
ancestor updates its prefix while preserving both actual descendant segments. -/
example : (namedNested.rename "renamed").descendantKey [] [0, 0] =
    some ⟨[⟨.block, .label "renamed"⟩, ⟨.loop, .ordinal 0⟩, ⟨.primitive, .label "effect"⟩]⟩ := by decide

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_named`). Retaining the old
ancestor prefix after renaming fails the computed descendant-key law. -/
example : ¬ (namedNested.descendantKey [] [0, 0] =
    (namedNested.rename "renamed").descendantKey [] [0, 0]) := by decide

/-- Helper (projection). Three specializations of one generic, differing one argument list at a time. -/
def numeric : SpecializationId Nat String := ⟨.function function, [0], ["Fs"]⟩
def otherType : SpecializationId Nat String := ⟨.function function, [1], ["Fs"]⟩
def otherCapability : SpecializationId Nat String := ⟨.function function, [0], ["Console"]⟩

/-- Helper (projection). Distinct request sites for the same specialization. -/
def requestZero : SpecializationRequest Nat String := ⟨⟨0⟩, numeric⟩
def requestOne : SpecializationRequest Nat String := ⟨⟨1⟩, numeric⟩
def requestOtherType : SpecializationRequest Nat String := ⟨⟨2⟩, otherType⟩
def requestOtherCapability : SpecializationRequest Nat String := ⟨⟨3⟩, otherCapability⟩

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_complete`). Both requests have
real edges, and both resolve to the numeric specialization. -/
example : ∃ zeroEdge oneEdge, zeroEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ oneEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ zeroEdge.request = requestZero ∧ oneEdge.request = requestOne := by
  obtain ⟨zeroEdge, zeroKept, zeroIdentity, _⟩ :=
    SpecializationTable.canonicalize_complete [requestZero, requestOne] requestZero (by simp)
  obtain ⟨oneEdge, oneKept, oneIdentity, _⟩ :=
    SpecializationTable.canonicalize_complete [requestZero, requestOne] requestOne (by simp)
  exact ⟨zeroEdge, oneEdge, zeroKept, oneKept, zeroIdentity, oneIdentity⟩

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def dropFirstEdges {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  { SpecializationTable.canonicalize requests with edges := (SpecializationTable.canonicalize requests).edges.tail }

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_complete`). Dropping the first
request's edge leaves no edge for it. -/
example : ¬ ∃ edge ∈ (dropFirstEdges [requestZero, requestOne]).edges, edge.request = requestZero := by decide

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound`). The concrete
second edge resolves through the actual table to `numeric`. -/
example : SpecializationTable.entryAt (SpecializationTable.canonicalize
    [requestZero, requestOne]).entries 0 = some numeric := by decide

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound`). Pointing that edge
beyond the table does not resolve to its request. -/
example : SpecializationTable.entryAt (SpecializationTable.canonicalize
    [requestZero, requestOne]).entries 9 ≠ some numeric := by decide

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_share`). Requests at sites zero
and one share canonical entry zero. -/
example : ∃ zeroEdge oneEdge, zeroEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ oneEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ zeroEdge.request = requestZero ∧
    oneEdge.request = requestOne ∧ zeroEdge.entry = oneEdge.entry :=
  SpecializationTable.canonicalize_share [requestZero, requestOne] requestZero requestOne
    (by simp) (by simp) rfl

/-- Helper (projection). A planted interner parameterized by a lossy request key. It retains
request edges and entries, so mutations exercise the same observable table contract as the real
canonicalizer rather than merely projecting fields from identity records. -/
def canonicalizeByKey {Key : Type} [DecidableEq Key]
    (key : SpecializationRequest Nat String → Key) :
    List (SpecializationRequest Nat String) → List Key × SpecializationTable Nat String
  | [] => ([], ⟨[], []⟩)
  | request :: rest =>
    let (keys, table) := canonicalizeByKey key rest
    let index := keys.idxOf (key request)
    if index < keys.length then
      (keys, { table with edges := ⟨request, index⟩ :: table.edges })
    else
      (keys ++ [key request], ⟨table.entries ++ [request.specialization],
        ⟨request, table.entries.length⟩ :: table.edges⟩)

/-- Helper (projection). Canonicalizing by lexical request site incorrectly separates equal
specializations requested from different sites. -/
def bySite (requests : List (SpecializationRequest Nat String)) : SpecializationTable Nat String :=
  (canonicalizeByKey (·.site) requests).2

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_share`). The site-keyed interner
keeps both request edges but fails to give them one shared entry. -/
example : ¬ (∃ first second, first ∈ (bySite [requestZero, requestOne]).edges ∧
    second ∈ (bySite [requestZero, requestOne]).edges ∧ first.request = requestZero ∧
    second.request = requestOne ∧ first.entry = second.entry) := by
  have index : List.idxOf (⟨0⟩ : SiteId) [⟨1⟩] = 1 := by decide
  simp [bySite, canonicalizeByKey, requestZero, requestOne, index]

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`). Different type
arguments receive different canonical entries. -/
example : ∃ typeEdge otherEdge, typeEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherType]).edges ∧ otherEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherType]).edges ∧ typeEdge.request = requestZero ∧
    otherEdge.request = requestOtherType ∧ typeEdge.entry ≠ otherEdge.entry :=
  SpecializationTable.canonicalize_distinct [requestZero, requestOtherType] requestZero requestOtherType
    (by simp) (by simp) (by decide)

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`). Different capability
arguments also receive different entries. -/
example : ∃ capabilityEdge otherEdge, capabilityEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherCapability]).edges ∧ otherEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherCapability]).edges ∧ capabilityEdge.request = requestZero ∧
    otherEdge.request = requestOtherCapability ∧ capabilityEdge.entry ≠ otherEdge.entry :=
  SpecializationTable.canonicalize_distinct [requestZero, requestOtherCapability] requestZero
    requestOtherCapability (by simp) (by simp) (by decide)

/-- Helper (projection). A generic-only interner drops both argument lists from its key. -/
def onlyGeneric (requests : List (SpecializationRequest Nat String)) : SpecializationTable Nat String :=
  (canonicalizeByKey (·.specialization.generic) requests).2

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`). The generic-only
interner retains type-different requests but merges their numeric entries. -/
example : ¬ (∃ first second, first ∈ (onlyGeneric [requestZero, requestOtherType]).edges ∧
    second ∈ (onlyGeneric [requestZero, requestOtherType]).edges ∧ first.request = requestZero ∧
    second.request = requestOtherType ∧ first.entry ≠ second.entry) := by
  simp [onlyGeneric, canonicalizeByKey, requestZero, requestOtherType, numeric, otherType]

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`). Dropping capability
arguments also merges distinct requests; retaining type arguments alone would not repair this. -/
example : ¬ (∃ first second, first ∈ (onlyGeneric [requestZero, requestOtherCapability]).edges ∧
    second ∈ (onlyGeneric [requestZero, requestOtherCapability]).edges ∧ first.request = requestZero ∧
    second.request = requestOtherCapability ∧ first.entry ≠ second.entry) := by
  simp [onlyGeneric, canonicalizeByKey, requestZero, requestOtherCapability, numeric, otherCapability]

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup`). Two equal requests
produce one entry, not a duplicate. -/
example : (SpecializationTable.canonicalize [requestZero, requestOne]).entries.Nodup := by decide

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup`). Concatenating request
identities without interning produces a duplicate entry. -/
example : ¬ ([numeric, numeric] : List (SpecializationId Nat String)).Nodup := by decide

/-- Helper (projection). Loaded modules with explicit package versions and origins. -/
def firstModule : LoadedModule := ⟨⟨package "p1", ["first"], by decide⟩, ⟨"registry-a"⟩⟩
def secondModule : LoadedModule := ⟨⟨package "p1", ["second"], by decide⟩, ⟨"registry-a"⟩⟩
def collidingModule : LoadedModule := ⟨⟨package "p2", ["first"], by decide⟩, ⟨"registry-b"⟩⟩
def samePackageOtherOrigin : LoadedModule := ⟨⟨package "p1", ["other"], by decide⟩, ⟨"mirror"⟩⟩
def collidingTop : LoadCollision := .topLevel firstModule collidingModule rfl (by decide)
def collidingOrigin : LoadCollision := .packageOrigin firstModule samePackageOtherOrigin (by decide) (by decide)

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`). The real loader refuses the
OD44 collision and its diagnostic names both loaded modules. -/
example : ∃ collision, LoadedModules.load [firstModule, collidingModule] = .refused collision ∧
    collision.first = firstModule ∧ collision.second = collidingModule := by
  exact ⟨collidingTop, rfl, rfl, rfl⟩

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def oneSided (_modules : List LoadedModule) (first : LoadedModule) : LoadedModule := first

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`). A one-sided diagnostic drops
the second package and therefore is not the collision returned by the real load check. -/
example : oneSided [firstModule, collidingModule] firstModule ≠ collidingModule := by decide

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_refusal_complete`). Equal package name/version
from a second origin is refused even though the module paths differ. -/
example : ∃ detected, LoadedModules.load [firstModule, samePackageOtherOrigin] = .refused detected ∧
    detected.first = firstModule ∧ detected.second = samePackageOtherOrigin := by
  exact ⟨collidingOrigin, rfl, rfl, rfl⟩

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def admitAll (modules : List LoadedModule) : LoadResult := .admitted modules

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_complete`). A loader that admits equal
name/version from different origins violates the package-origin refusal rule. -/
example : ¬ (∀ modules collision, LoadedModules.HasCollision modules collision →
    ∃ detected, admitAll modules = .refused detected) := by
  intro complete
  obtain ⟨detected, impossible⟩ := complete [firstModule, samePackageOtherOrigin] collidingOrigin
    ⟨by decide, by decide⟩
  cases impossible

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_admitted`). Distinct top-level names from one
package and origin are admitted unchanged. -/
example : LoadedModules.load [firstModule, secondModule] = .admitted [firstModule, secondModule] := rfl

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_admitted`). Adding the top-level collision
makes the same admission result impossible. -/
example : LoadedModules.load [firstModule, collidingModule] ≠ .admitted [firstModule, collidingModule] := by
  intro admitted
  cases admitted

/-- Helper (projection). Typed byte strings for newline, tab, EOF and multibyte examples. -/
def aSource : SourceText := [97]
def aNewline : SourceText := aSource ++ [10]
def aTab : SourceText := aSource ++ [9]
def aTabNewline : SourceText := aTab ++ [10]
def utf8Source : SourceText := [195, 169]
def otherTabSource : SourceText := [10, 97, 98]

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`). Appending a newline after a
non-newline byte advances from line one to line two. -/
example : aNewline.lineAt 2 = aSource.lineAt 1 + 1 ∧ aNewline.lineAt 2 = 2 := by decide

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`). Leaving the line unchanged
would compute line one, contradicting the byte law. -/
example : aNewline.lineAt 2 ≠ aSource.lineAt 1 := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_other`). A tab does not start a line. -/
example : aTab.lineAt 2 = aSource.lineAt 1 := by decide

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_other`). Treating a tab as a newline
would advance the line. -/
example : aTab.lineAt 2 ≠ aSource.lineAt 1 + 1 := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`). The tab advances column two to
column three. -/
example : aTab.columnAt 2 = aSource.columnAt 1 + 1 ∧ aTab.columnAt 2 = 3 := by decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def tabStopFour (source : SourceText) (offset : Nat) : Nat := source.columnAt offset + 3

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`). A four-column tab stop violates
the one-byte step law. -/
example : tabStopFour aSource 1 ≠ aTab.columnAt 2 := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`). A newline resets to column
one after preceding bytes. -/
example : aTabNewline.columnAt 3 = 1 := by decide

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`). Advancing instead of
resetting would produce the old column plus one. -/
example : aTabNewline.columnAt 3 ≠ aTab.columnAt 2 + 1 := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append`). The generic line step computes the
newline branch and the ordinary-byte branch on real bytes. -/
example : aNewline.lineAt 2 = 2 ∧ aTab.lineAt 2 = 1 := by decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def constantLine (_source : SourceText) (_offset : Nat) : Nat := 1

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append`). A constant line function fails on a
source ending in a newline. -/
example : constantLine aNewline 2 ≠ aNewline.lineAt 2 := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append`). UTF-8 continuation bytes each count
one column, exactly as tabs do; offset two is also the UTF-8 EOF boundary. -/
example : utf8Source.columnAt 2 = 3 ∧ aTab.columnAt 2 = aSource.columnAt 1 + 1 := by decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def scalarColumn (_source : SourceText) (_offset : Nat) : Nat := 2

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append`). Counting Unicode scalar values
instead of bytes gives column two after `é`, not the required column three. -/
example : scalarColumn utf8Source 2 ≠ utf8Source.columnAt 2 := by decide

/-- Helper (projection). The same start with a different end, and a bad state with a wrong column. -/
def tabPointSpan : Span := { tabSpan with endOffset := 2, range_ok := by decide }
def wrongTabSpan : Span := { tabSpan with col := 2, col_pos := by decide }

/-- Accepted: the tab span satisfies the `LocationOk` projection at a real in-bounds range. -/
example : tabSpan.LocationOk tabSource ↔ tabSpan.endOffset ≤ tabSource.length ∧
    tabSpan.line = tabSource.lineAt tabSpan.startOffset ∧
      tabSpan.col = tabSource.columnAt tabSpan.startOffset := Span.locationOk_iff tabSource tabSpan

/-- Refused: the same stored coordinates do not satisfy `LocationOk` for a source with a different
line structure. -/
example : ¬ tabSpan.LocationOk otherTabSource ∧
    ¬ (tabSpan.endOffset ≤ otherTabSource.length ∧
      tabSpan.line = otherTabSource.lineAt tabSpan.startOffset ∧
      tabSpan.col = otherTabSource.columnAt tabSpan.startOffset) := by decide

/-- Helper (projection). A source `assume`, a package stack assumption, and their wire records. -/
def sourceAssume : PremiseId :=
  ⟨.assume, .function (StableKey.mk (.function function) ⟨[⟨.assumption, .ordinal 0⟩]⟩)⟩
def stackAssumption : PremiseId := ⟨.stack, .package package ⟨"defaultStack"⟩⟩
def stackRecord : PremiseRecord := ⟨stackAssumption.kind, stackAssumption.scope, specVersion⟩
def unknownRecord : EncodedPremise := ⟨99, stackAssumption.scope, specVersion⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_encode`). A known stack premise round-trips
with its typed scope and writer version. -/
example : stackRecord.encode.read specVersion = some stackRecord := by decide

/-- Helper (projection). A faulty encoder discards the known premise category, retaining the
writer version and typed scope. -/
def encodeDroppingKind (record : PremiseRecord) : EncodedPremise :=
  { record.encode with category := 0 }

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_encode`). A category-dropping encoder fails
the round trip for a known stack premise under the current version, with no mismatched version
premise to switch off the law. -/
example : ¬ ((encodeDroppingKind stackRecord).read specVersion = some stackRecord) := by decide

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_unknown`). Category 99 has no meaning in the
closed schema and is refused. -/
example : unknownRecord.read specVersion = none := by decide

/-- Helper (projection). Mutated implementation used by the following planted example. -/
def catchAllRead (reader : SpecVersion) (record : EncodedPremise) : Option PremiseRecord :=
  if record.spec = reader then some ⟨.assume, record.scope, record.spec⟩ else none

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_unknown`). A catch-all reader accepts the
unknown category by silently calling it `assume`. -/
example : ¬ (catchAllRead specVersion unknownRecord = none) := by decide

/-- Accepted: declaration and constant owners cover non-function obligations without fake functions. -/
example : invariantKey.owner = .declaration declaration ∧ initializerKey.owner = .constant constant := by
  exact ⟨rfl, rfl⟩

/-- Two facts with the same conclusion but different premise dependencies. -/
def conditional : ConditionalFact String := ⟨"bounded", [sourceAssume]⟩
def unconditional : ConditionalFact String := ⟨"bounded", []⟩

/-- Planted counter-example: comparing only conclusions loses the conditional fact's premise debt. -/
def conclusionOnly (first second : ConditionalFact String) : Prop :=
  first.conclusion = second.conclusion

example : conclusionOnly conditional unconditional ∧ conditional ≠ unconditional := by
  exact ⟨rfl, by decide⟩

end Vacuity
end ConcreteSpec.Core

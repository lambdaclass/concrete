import ConcreteSpecEvidence.Rules
import ConcreteSpec.Spec.Basics
import ConcreteSpec.Spec.Ident
import ConcreteSpec.Spec.Outcome
import ConcreteSpec.Proofs.Ident

namespace ConcreteSpec.Core

namespace Vacuity

/-- A package and module used by the identity examples. -/
def package (name : Name := ⟨"demo"⟩) : PackageId := ⟨name, ⟨⟨"1"⟩⟩⟩

/-- An enclosing function used by the persistent-key examples. -/
def function : FnId := FnId.mk (ModuleId.mk package [⟨"demo"⟩] (by decide)) ⟨"f"⟩

/-- A second enclosing function. -/
def otherFunction : FnId := FnId.mk (ModuleId.mk package [⟨"demo"⟩] (by decide)) ⟨"g"⟩

/-- A declaration and constant that own obligations outside function bodies. -/
def declaration : DeclId := ⟨function.module, ⟨"S"⟩⟩
def constant : ConstId := ⟨function.module, ⟨"N"⟩⟩

/-- Structured paths for a binding, a call and a struct invariant. -/
def bindingKey : StableKey := ⟨.function function, ⟨[⟨.binding, .ordinal 0⟩]⟩⟩
def callKey : StableKey := ⟨.function function, ⟨[⟨.call, .ordinal 0⟩]⟩⟩
def invariantKey : StableKey := ⟨.declaration declaration, ⟨[]⟩⟩
def initializerKey : StableKey := ⟨.constant constant, ⟨[⟨.return, .ordinal 0⟩]⟩⟩

/-- Two entries whose lexical and persistent identities differ. -/
def siteTable : SiteKeyTable := [(⟨0⟩, bindingKey), (⟨1⟩, callKey)]

/-- Successor is an injective lexical renumbering. -/
def successor : SiteRenumbering := ⟨Nat.succ, fun _ _ h => Nat.succ.inj h⟩

/-- A non-injective site function that merges both sites. -/
def collapse : Nat → Nat := fun _ => 0

/-- Mutant table renumbering that collapses all site numbers to zero regardless of `r`. -/
def collapseRenumber (_r : SiteRenumbering) (table : SiteKeyTable) : SiteKeyTable :=
  table.renumberSites collapse

/-- Instance (law `ConcreteSpec.Core.SiteKeyTable.lookup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`). -/
theorem lookup_renumber_example_siteTable : AList.lookup (siteTable.renumber successor) ⟨successor.apply (⟨1⟩ : SiteId).id⟩ =
    AList.lookup siteTable ⟨1⟩ :=
  SiteKeyTable.lookup_renumber successor siteTable ⟨1⟩

/-- Planted (law `ConcreteSpec.Core.SiteKeyTable.lookup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`, subject `ConcreteSpec.Core.SiteKeyTable.renumber`, mutant `ConcreteSpec.Core.Vacuity.collapseRenumber`). -/
theorem lookup_renumber_planted_siteTable_collapseRenumber : ¬ (AList.lookup (collapseRenumber successor siteTable) ⟨successor.apply (⟨1⟩ : SiteId).id⟩ =
    AList.lookup siteTable ⟨1⟩) := by decide

/-- Instance (law `ConcreteSpec.Core.SiteKeyTable.nodup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`). -/
theorem nodup_renumber_example_siteTable : (siteTable.renumber successor).NoDupSites :=
  SiteKeyTable.nodup_renumber successor siteTable (by decide)

/-- Planted (law `ConcreteSpec.Core.SiteKeyTable.nodup_renumber`, case `ConcreteSpec.Core.Vacuity.siteTable`, subject `ConcreteSpec.Core.SiteKeyTable.renumber`, mutant `ConcreteSpec.Core.Vacuity.collapseRenumber`). -/
theorem nodup_renumber_planted_siteTable_collapseRenumber : siteTable.NoDupSites ∧ ¬ (collapseRenumber successor siteTable).NoDupSites :=
  ⟨by decide, by decide⟩

/-- An unnamed binding, a named binding, a named call, and an unnamed loop used by key-stability examples. -/
def unnamedBinding : KeyConstruct := KeyConstruct.mk .binding (.unnamed none) []
def namedBinding : KeyConstruct := KeyConstruct.mk .binding (.named ⟨"x"⟩) []
def namedCall : KeyConstruct := KeyConstruct.mk .call (.named ⟨"work"⟩) []
def unnamedLoop : KeyConstruct := KeyConstruct.mk .loop (.unnamed none) []
def unnamedBlock : KeyConstruct := KeyConstruct.mk .block (.unnamed none) []

/-- Function bodies before and after editing `otherFunction`: `function`'s body is unchanged. -/
def bodiesBefore (f : FnId) : List KeyConstruct :=
  if f = function then [unnamedBinding, namedCall] else []

def bodiesAfter (f : FnId) : List KeyConstruct :=
  if f = otherFunction then [unnamedLoop, unnamedBinding, namedCall]
  else bodiesBefore f

/-- Starting lexical site offsets before and after editing `otherFunction`: `function`'s starting
site shifts from `0` to `10` because `otherFunction` gained sites. -/
def startSitesBefore (f : FnId) : Nat := if f = function then 0 else 5
def startSitesAfter (f : FnId) : Nat := if f = function then 10 else 0

/-- Mutant program key-table assignment that bakes the function's global starting lexical site
offset into the assigned persistent keys. -/
def assignProgramTablesFromSiteOffset (bodies : FnId → List KeyConstruct)
    (startSite : FnId → Nat) (f : FnId) : SiteKeyTable :=
  (bodies f).map fun c =>
    (⟨startSite f⟩, ⟨.function f, ⟨[⟨c.kind, .ordinal (startSite f)⟩]⟩⟩)

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignProgramTables_outsideEdit`, case `ConcreteSpec.Core.Vacuity.bodiesBefore`). -/
theorem assignProgramTables_outsideEdit_example_bodiesBefore : OutsideFunctionEdit
    (KeyConstruct.assignProgramTables bodiesBefore startSitesBefore)
    (KeyConstruct.assignProgramTables bodiesAfter startSitesAfter)
    otherFunction :=
  KeyConstruct.assignProgramTables_outsideEdit bodiesBefore bodiesAfter
    startSitesBefore startSitesAfter otherFunction
    (fun f hf => by simp [bodiesAfter, hf])

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignProgramTables_outsideEdit`, case `ConcreteSpec.Core.Vacuity.bodiesBefore`, subject `ConcreteSpec.Core.KeyConstruct.assignProgramTables`, mutant `ConcreteSpec.Core.Vacuity.assignProgramTablesFromSiteOffset`). -/
theorem assignProgramTables_outsideEdit_planted_bodiesBefore_assignProgramTablesFromSiteOffset : (∀ f, f ≠ otherFunction → bodiesAfter f = bodiesBefore f) ∧
    ¬ OutsideFunctionEdit
      (assignProgramTablesFromSiteOffset bodiesBefore startSitesBefore)
      (assignProgramTablesFromSiteOffset bodiesAfter startSitesAfter)
      otherFunction := by
  refine ⟨fun f hf => by simp [bodiesAfter, hf], ?_⟩
  intro h
  have hf := h function (by decide)
  revert hf
  decide

/-- A nested toy function body with two unnamed levels and a pinned leaf. -/
def nestedBody : KeyConstruct := ⟨.block, .unnamed none,
  [⟨.loop, .unnamed none, [⟨.primitive, .unnamed (some ⟨"effect"⟩), []⟩]⟩]⟩
def namedNested : KeyConstruct := ⟨.block, .named ⟨"body"⟩, nestedBody.children⟩
def nestedPath : FunctionRelativeKey :=
  ⟨[⟨.block, .ordinal 0⟩, ⟨.loop, .ordinal 0⟩, ⟨.primitive, .pinned ⟨"effect"⟩⟩]⟩
def insertedCall : KeyConstruct := ⟨.call, .unnamed none, []⟩

private def callBlock (name : String) : KeyConstruct :=
  ⟨.block, .named ⟨name⟩, [insertedCall]⟩

/-- In [L { call }, R { call }], focus on L's children. -/
def leftBlock : KeyConstruct.ParentContext :=
  .children .body [] (callBlock "L") [callBlock "R"]

private def blockKey (name : String) (calls : List KeySegment := []) : StableKey :=
  ⟨.function function, ⟨⟨.block, .named ⟨name⟩⟩ :: calls⟩⟩

private def afterLeftInsertion : List StableKey :=
  [blockKey "L", blockKey "L" [⟨.call, .ordinal 0⟩], blockKey "L" [⟨.call, .ordinal 1⟩],
   blockKey "R", blockKey "R" [⟨.call, .ordinal 0⟩]]

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_insert_locality`, case `ConcreteSpec.Core.Vacuity.leftBlock`). -/
theorem assignFunctionTable_insert_locality_example_leftBlock :
    List.map Prod.snd (KeyConstruct.assignFunctionTable function 0
      (leftBlock.fill [insertedCall, insertedCall])) = afterLeftInsertion := by
  with_unfolding_all exact
    (KeyConstruct.assignFunctionTable_insert_locality function 0 0 leftBlock [] [insertedCall] insertedCall).2

/-- Issue the table for an insertion before R's call, leaving L untouched. -/
def insertUnderRight : SiteKeyTable :=
  let right : KeyConstruct.ParentContext := .children .body [callBlock "L"] (callBlock "R") []
  KeyConstruct.assignFunctionTable function 0 (right.fill [insertedCall, insertedCall])

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_insert_locality`, case `ConcreteSpec.Core.Vacuity.leftBlock`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.insertUnderRight`). -/
theorem assignFunctionTable_insert_locality_planted_leftBlock_insertUnderRight :
    ¬ leftBlock.InsertionKeys function [] [insertedCall] insertedCall
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 0 (leftBlock.fill [insertedCall])))
      (List.map Prod.snd insertUnderRight) := by
  intro wrong
  have different : List.map Prod.snd insertUnderRight ≠ afterLeftInsertion :=
    of_decide_eq_true (by with_unfolding_all rfl)
  with_unfolding_all exact different wrong.2

/-- Mutant collector that reproduces the old omission of every descendant. -/
def shallowCollect (scope : List KeyConstruct) : List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | root :: rest => root.key scope :: shallowCollect (scope ++ [root]) rest

/-- Mutant collector that forgets to prefix descendant keys with their enclosing construct's segment. -/
def unprefixedCollect (scope : List KeyConstruct) : List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | construct :: rest =>
    construct.key scope ::
      (unprefixedCollect (construct.childScope scope) construct.children ++
       unprefixedCollect (scope ++ [construct]) rest)
termination_by forest => sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases construct <;> simp_all <;> omega

/-- Mutant occurrence resolver that loses every route, with exactly the resolver's type. -/
def missingPathAtIn (_scope _forest : List KeyConstruct) (_route : Numbers) :
    Option FunctionRelativeKey := none

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem collectKeysIn_sound_example_nestedBody : ∃ route, route ≠ [] ∧
    KeyConstruct.pathAtIn [] [nestedBody] route = some nestedPath :=
  KeyConstruct.collectKeysIn_sound [] [nestedBody] nestedPath
    (KeyConstruct.collectKeysIn_complete [] [nestedBody] [0, 0, 0] nestedPath
      (by decide) (by decide))

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAtIn`, mutant `ConcreteSpec.Core.Vacuity.missingPathAtIn`). -/
theorem collectKeysIn_sound_planted_nestedBody_missingPathAtIn : nestedPath ∈ KeyConstruct.collectKeysIn [] [nestedBody] ∧
    ¬ (∃ route, route ≠ [] ∧ missingPathAtIn [] [nestedBody] route = some nestedPath) := by
  refine ⟨KeyConstruct.collectKeysIn_complete [] [nestedBody] [0, 0, 0] nestedPath
    (by decide) (by decide), ?_⟩
  rintro ⟨route, _, found⟩
  cases found

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_collectionSound`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem collectKeysIn_collectionSound_example_nestedBody : KeyConstruct.CollectionSound [] [nestedBody]
    (KeyConstruct.collectKeysIn [] [nestedBody]) :=
  KeyConstruct.collectKeysIn_collectionSound [] [nestedBody]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_collectionSound`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`, mutant `ConcreteSpec.Core.Vacuity.unprefixedCollect`). -/
theorem collectKeysIn_collectionSound_planted_nestedBody_unprefixedCollect : ¬ KeyConstruct.CollectionSound [] [nestedBody]
    (unprefixedCollect [] [nestedBody]) := by
  intro sound
  obtain ⟨route, nonempty, found⟩ := sound ⟨[⟨.loop, .ordinal 0⟩]⟩ (by
    simp [unprefixedCollect, nestedBody, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal,
      KeyConstruct.childScope])
  cases route with
  | nil => exact nonempty rfl
  | cons index tail =>
    cases index with
    | zero =>
      rw [KeyConstruct.pathAtIn] at found
      simp only [List.getElem?_cons_zero, List.take_zero, List.append_nil, Option.bind_eq_bind,
        Option.bind_some] at found
      cases hchild : KeyConstruct.pathAtIn (nestedBody.childScope []) nestedBody.children tail with
      | none =>
        rw [hchild] at found
        cases found
      | some suffix =>
        rw [hchild] at found
        simp [nestedBody, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal] at found
    | succ n =>
      simp [KeyConstruct.pathAtIn] at found

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem collectKeysIn_complete_example_nestedBody : nestedPath ∈ KeyConstruct.collectKeysIn [] [nestedBody] :=
  KeyConstruct.collectKeysIn_complete [] [nestedBody] [0, 0, 0] nestedPath (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.collectKeysIn_complete`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.collectKeysIn`, mutant `ConcreteSpec.Core.Vacuity.shallowCollect`). -/
theorem collectKeysIn_complete_planted_nestedBody_shallowCollect : (([0, 0, 0] : Numbers) ≠ [] ∧
    KeyConstruct.pathAtIn [] [nestedBody] [0, 0, 0] = some nestedPath) ∧
    ¬ nestedPath ∈ shallowCollect [] [nestedBody] := by decide

/-- Mutant assignment that drops descendants before assigning site numbers. -/
def shallowAssign (f : FnId) (startSite : Nat) (body : List KeyConstruct) : SiteKeyTable :=
  KeyConstruct.numberFunctionKeys f startSite (shallowCollect [] body)

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_occurrences`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem assignFunctionTable_occurrences_example_nestedBody : (⟨.function function, nestedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7 [nestedBody])) ↔
    ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath :=
  KeyConstruct.assignFunctionTable_occurrences function 7 [nestedBody] nestedPath

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_occurrences`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.shallowAssign`). -/
theorem assignFunctionTable_occurrences_planted_nestedBody_shallowAssign : ¬ ((⟨.function function, nestedPath⟩ : StableKey) ∈
      (List.map Prod.snd (shallowAssign function 7 [nestedBody])) ↔
    ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath) := by
  intro equal
  have member := equal.mpr ⟨[0, 0, 0], by decide, by decide⟩
  revert member
  decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_lookup`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem assignFunctionTable_lookup_example_nestedBody : AList.lookup (KeyConstruct.assignFunctionTable function 7 [nestedBody]) ⟨7 + 2⟩ =
      some ⟨.function function, nestedPath⟩ ↔
    (KeyConstruct.collectKeys [nestedBody])[2]? = some nestedPath ∧
      ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath :=
  KeyConstruct.assignFunctionTable_lookup function 7 2 [nestedBody] nestedPath

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_lookup`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.shallowAssign`). -/
theorem assignFunctionTable_lookup_planted_nestedBody_shallowAssign : ¬ (AList.lookup (shallowAssign function 7 [nestedBody]) ⟨7 + 2⟩ =
      some ⟨.function function, nestedPath⟩ ↔
    (KeyConstruct.collectKeys [nestedBody])[2]? = some nestedPath ∧
      ∃ route, route ≠ [] ∧ KeyConstruct.pathAt [nestedBody] route = some nestedPath) := by
  intro equal
  have found := equal.mpr ⟨by
    simp [KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, nestedBody, nestedPath,
      KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal, KeyConstruct.childScope],
    [0, 0, 0], by decide, by decide⟩
  revert found
  decide

/-- Suffix below the toy block, independently resolved among its children. -/
def nestedSuffix : FunctionRelativeKey := ⟨[⟨.loop, .ordinal 0⟩, ⟨.primitive, .pinned ⟨"effect"⟩⟩]⟩
def renamedRootKey : FunctionRelativeKey := ⟨[⟨.block, .named ⟨"new"⟩⟩]⟩
def renamedPath : FunctionRelativeKey := ⟨⟨.block, .named ⟨"new"⟩⟩ :: nestedSuffix.path⟩
def pinnedRootKey : FunctionRelativeKey := ⟨[⟨.block, .pinned ⟨"pin"⟩⟩]⟩
def pinnedPath : FunctionRelativeKey := ⟨⟨.block, .pinned ⟨"pin"⟩⟩ :: nestedSuffix.path⟩

/-- Mutant rename that leaves the construct's old name unchanged. -/
def noopRename (construct : KeyConstruct) (_name : Name) : KeyConstruct := construct

/-- Mutant pin that ignores the supplied user label. -/
def noopPin (construct : KeyConstruct) (_label : Option Name) : KeyConstruct := construct

/-- Mutant collector that fails to reset the sibling scope when entering a `.named` construct. -/
def unresetScopeCollect (scope : List KeyConstruct) : List KeyConstruct → List FunctionRelativeKey
  | [] => []
  | construct :: rest =>
    construct.key scope ::
      ((unresetScopeCollect scope construct.children).map
        (fun key => ⟨construct.segment scope :: key.path⟩) ++
       unresetScopeCollect (scope ++ [construct]) rest)
termination_by forest => sizeOf forest
decreasing_by
  all_goals simp_wf
  all_goals cases construct <;> simp_all <;> omega

/-- Mutant function-table assignment that fails to reset scope when entering a `.named` construct. -/
def unresetScopeAssign (f : FnId) (startSite : Nat) (body : List KeyConstruct) : SiteKeyTable :=
  KeyConstruct.numberFunctionKeys f startSite (unresetScopeCollect [] body)

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
theorem assignFunctionTable_named_scope_example_namedNested : ((List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [namedCall]))).drop
        (KeyConstruct.collectKeys [unnamedLoop]).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length =
    ((List.map Prod.snd (KeyConstruct.assignFunctionTable function 19
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [unnamedBinding]))).drop
        (KeyConstruct.collectKeys []).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length :=
  KeyConstruct.assignFunctionTable_named_scope function 7 19 [unnamedLoop] [namedCall]
    [] [unnamedBinding] .block ⟨"body"⟩ nestedBody.children

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_named_scope`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeAssign`). -/
theorem assignFunctionTable_named_scope_planted_namedNested_unresetScopeAssign : ¬ (((List.map Prod.snd (unresetScopeAssign function 7
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [namedCall]))).drop
        (KeyConstruct.collectKeys [unnamedLoop]).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length =
    ((List.map Prod.snd (unresetScopeAssign function 19
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [unnamedBinding]))).drop
        (KeyConstruct.collectKeys []).length).take
          (KeyConstruct.collectKeys [KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children]).length) := by
  simp [unresetScopeAssign, KeyConstruct.numberFunctionKeys_snd, unresetScopeCollect,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, KeyConstruct.key,
    KeyConstruct.segment, KeyConstruct.segmentAtOrdinal, KeyConstruct.countsToward, KeyConstruct.childScope, ConstructName.isUnnamed,
    unnamedLoop, nestedBody, namedCall, unnamedBinding]

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`). -/
theorem assignFunctionTable_rename_named_example_namedNested : (⟨.function function, renamedRootKey⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: []))) ∧
    (⟨.function function, renamedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: []))) :=
  KeyConstruct.assignFunctionTable_rename_named function 7 [unnamedLoop] [] .block ⟨"old"⟩ ⟨"new"⟩
    nestedBody.children [0, 0] nestedSuffix (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
theorem assignFunctionTable_rename_named_planted_namedNested_noopRename : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, renamedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([unnamedLoop] ++ noopRename (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children) ⟨"new"⟩ :: []))) ∧
      (⟨.function function, renamedPath⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([unnamedLoop] ++ noopRename (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children) ⟨"new"⟩ :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopRename, unnamedLoop, nestedBody,
    renamedRootKey, KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal, KeyConstruct.childScope] at member

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedNested`, subject `ConcreteSpec.Core.KeyConstruct.assignFunctionTable`, mutant `ConcreteSpec.Core.Vacuity.unresetScopeAssign`). -/
theorem assignFunctionTable_rename_named_planted_namedNested_unresetScopeAssign : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, renamedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (unresetScopeAssign function 7
          ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: []))) ∧
      (⟨.function function, renamedPath⟩ : StableKey) ∈
        (List.map Prod.snd (unresetScopeAssign function 7
          ([unnamedLoop] ++ (KeyConstruct.mk .block (.named ⟨"old"⟩) nestedBody.children).rename ⟨"new"⟩ :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨_, member⟩
  simp [unresetScopeAssign, KeyConstruct.numberFunctionKeys_snd, unresetScopeCollect,
    KeyConstruct.rename, ConstructName.rename, unnamedLoop, nestedBody, renamedPath, nestedSuffix,
    KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal] at member
  revert member
  decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`). -/
theorem assignFunctionTable_rename_named_example_namedCall : (⟨.function function, ⟨[⟨.call, .named ⟨"renamed"⟩⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .call (.named ⟨"work"⟩) []).rename ⟨"renamed"⟩ :: []))) ∧
    (⟨.function function, ⟨⟨.call, .named ⟨"renamed"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .call (.named ⟨"work"⟩) []).rename ⟨"renamed"⟩ :: []))) :=
  KeyConstruct.assignFunctionTable_rename_named function 7 [] [] .call ⟨"work"⟩ ⟨"renamed"⟩
    [] [] ⟨[]⟩ rfl

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_rename_named`, case `ConcreteSpec.Core.Vacuity.namedCall`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.noopRename`). -/
theorem assignFunctionTable_rename_named_planted_namedCall_noopRename : KeyConstruct.pathAt ([] : List KeyConstruct) [] = some ⟨[]⟩ ∧
    ¬ ((⟨.function function, ⟨[⟨.call, .named ⟨"renamed"⟩⟩]⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopRename (KeyConstruct.mk .call (.named ⟨"work"⟩) []) ⟨"renamed"⟩ :: []))) ∧
      (⟨.function function, ⟨⟨.call, .named ⟨"renamed"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopRename (KeyConstruct.mk .call (.named ⟨"work"⟩) []) ⟨"renamed"⟩ :: [])))) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopRename, KeyConstruct.key,
    KeyConstruct.segment, KeyConstruct.segmentAtOrdinal] at member

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem assignFunctionTable_pin_example_nestedBody : (⟨.function function, pinnedRootKey⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .block (.unnamed none) nestedBody.children).pin (some ⟨"pin"⟩) :: []))) ∧
    (⟨.function function, pinnedPath⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .block (.unnamed none) nestedBody.children).pin (some ⟨"pin"⟩) :: []))) :=
  KeyConstruct.assignFunctionTable_pin function 7 [] [] .block none ⟨"pin"⟩ nestedBody.children
    [0, 0] nestedSuffix (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
theorem assignFunctionTable_pin_planted_nestedBody_noopPin : KeyConstruct.pathAt nestedBody.children [0, 0] = some nestedSuffix ∧
    ¬ ((⟨.function function, pinnedRootKey⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .block (.unnamed none) nestedBody.children) (some ⟨"pin"⟩) :: []))) ∧
      (⟨.function function, pinnedPath⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .block (.unnamed none) nestedBody.children) (some ⟨"pin"⟩) :: [])))) := by
  refine ⟨by decide, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopPin, nestedBody, pinnedRootKey,
    KeyConstruct.key, KeyConstruct.segment, KeyConstruct.segmentAtOrdinal, KeyConstruct.childScope] at member

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
theorem assignFunctionTable_pin_example_unnamedBinding : (⟨.function function, ⟨[⟨.binding, .pinned ⟨"pinned"⟩⟩]⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .binding (.unnamed none) []).pin (some ⟨"pinned"⟩) :: []))) ∧
    (⟨.function function, ⟨⟨.binding, .pinned ⟨"pinned"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
      (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
        ([] ++ (KeyConstruct.mk .binding (.unnamed none) []).pin (some ⟨"pinned"⟩) :: []))) :=
  KeyConstruct.assignFunctionTable_pin function 7 [] [] .binding none ⟨"pinned"⟩ [] [] ⟨[]⟩ rfl

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.assignFunctionTable_pin`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.pin`, mutant `ConcreteSpec.Core.Vacuity.noopPin`). -/
theorem assignFunctionTable_pin_planted_unnamedBinding_noopPin : KeyConstruct.pathAt ([] : List KeyConstruct) [] = some ⟨[]⟩ ∧
    ¬ ((⟨.function function, ⟨[⟨.binding, .pinned ⟨"pinned"⟩⟩]⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some ⟨"pinned"⟩) :: []))) ∧
      (⟨.function function, ⟨⟨.binding, .pinned ⟨"pinned"⟩⟩ :: ([] : List KeySegment)⟩⟩ : StableKey) ∈
        (List.map Prod.snd (KeyConstruct.assignFunctionTable function 7
          ([] ++ noopPin (KeyConstruct.mk .binding (.unnamed none) []) (some ⟨"pinned"⟩) :: [])))) := by
  refine ⟨rfl, ?_⟩
  rintro ⟨member, _⟩
  simp [KeyConstruct.assignFunctionTable, KeyConstruct.numberFunctionKeys_snd,
    KeyConstruct.collectKeys, KeyConstruct.collectKeysIn, noopPin, KeyConstruct.key,
    KeyConstruct.segment, KeyConstruct.segmentAtOrdinal] at member

/-- A bad assignment that uses one ordinal across all unnamed construct kinds. -/
def allKindsOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal (preceding.filter fun sibling => sibling.name.isUnnamed).length⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
theorem key_insert_differentKind_example_unnamedBinding : unnamedBinding.key ([] ++ insertedCall :: []) = unnamedBinding.key ([] ++ []) :=
  KeyConstruct.key_insert_differentKind [] [] insertedCall unnamedBinding (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.allKindsOrdinal`). -/
theorem key_insert_differentKind_planted_unnamedBinding_allKindsOrdinal : insertedCall.kind ≠ unnamedBinding.kind ∧
    ¬ (allKindsOrdinal ([] ++ insertedCall :: []) unnamedBinding =
      allKindsOrdinal ([] ++ []) unnamedBinding) :=
  ⟨by decide, by decide⟩

/-- A bad assignment that counts named siblings in the ordinal. -/
def countNamedKey (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  ⟨[⟨target.kind, .ordinal (preceding.filter fun sibling => sibling.kind == target.kind).length⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
theorem key_insert_named_example_unnamedBinding : unnamedBinding.key ([] ++ KeyConstruct.mk .binding (.named ⟨"x"⟩) [] :: []) =
    unnamedBinding.key ([] ++ []) :=
  KeyConstruct.key_insert_named [] [] ⟨"x"⟩ .binding [] unnamedBinding

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_insert_named`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.countNamedKey`). -/
theorem key_insert_named_planted_unnamedBinding_countNamedKey : ¬ (countNamedKey ([] ++ KeyConstruct.mk .binding (.named ⟨"x"⟩) [] :: []) unnamedBinding =
    countNamedKey ([] ++ []) unnamedBinding) := by decide

/-- Mutant rename that demotes a named construct to an unnamed pinned construct, causing it to
consume an unnamed ordinal. -/
def demoteRename (construct : KeyConstruct) (name : Name) : KeyConstruct :=
  { construct with name := .unnamed (some name) }

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`, case `ConcreteSpec.Core.Vacuity.namedBinding`). -/
theorem key_rename_other_example_namedBinding : unnamedBinding.key ([] ++ namedBinding.rename ⟨"y"⟩ :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ namedBinding :: [unnamedBinding]) :=
  KeyConstruct.key_rename_other [] [unnamedBinding] namedBinding unnamedBinding ⟨"y"⟩

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_rename_other`, case `ConcreteSpec.Core.Vacuity.namedBinding`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.demoteRename`). -/
theorem key_rename_other_planted_namedBinding_demoteRename : ¬ (unnamedBinding.key ([] ++ demoteRename namedBinding ⟨"y"⟩ :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ namedBinding :: [unnamedBinding])) := by decide

/-- Mutant key assignment that excludes pinned unnamed siblings from the per-kind ordinal count. -/
def labelOnlyOrdinal (preceding : List KeyConstruct) (target : KeyConstruct) : FunctionRelativeKey :=
  let count := (preceding.filter fun sibling => sibling.kind == target.kind &&
    match sibling.name with | .named _ => false | .unnamed label => label.isNone).length
  ⟨[⟨target.kind, .ordinal count⟩]⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.key_pin_other`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`). -/
theorem key_pin_other_example_unnamedBinding : unnamedBinding.key ([] ++ unnamedBinding.pin (some ⟨"x"⟩) :: [unnamedBinding]) =
    unnamedBinding.key ([] ++ unnamedBinding :: [unnamedBinding]) :=
  KeyConstruct.key_pin_other [] [unnamedBinding] unnamedBinding unnamedBinding (some ⟨"x"⟩)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.key_pin_other`, case `ConcreteSpec.Core.Vacuity.unnamedBinding`, subject `ConcreteSpec.Core.KeyConstruct.key`, mutant `ConcreteSpec.Core.Vacuity.labelOnlyOrdinal`). -/
theorem key_pin_other_planted_unnamedBinding_labelOnlyOrdinal : ¬ (labelOnlyOrdinal ([] ++ unnamedBinding.pin (some ⟨"x"⟩) :: [unnamedBinding]) unnamedBinding =
    labelOnlyOrdinal ([] ++ unnamedBinding :: [unnamedBinding]) unnamedBinding) := by decide

/-- Helper computation example (not law evidence). -/
theorem example_Helper_computation_not_law_evidence : ((KeyConstruct.mk .call (.named ⟨"work"⟩) []).rename ⟨"renamed"⟩).key [] =
    ⟨[⟨.call, .named ⟨"renamed"⟩⟩]⟩ :=
  KeyConstruct.key_rename_named [] .call ⟨"work"⟩ [] ⟨"renamed"⟩

/-- Helper computation example (not law evidence). -/
theorem example_Helper_computation_not_law_evidence_2 : ((KeyConstruct.mk .binding (.unnamed none) []).pin (some ⟨"pinned"⟩)).key [] =
    ⟨[⟨.binding, .pinned ⟨"pinned"⟩⟩]⟩ :=
  KeyConstruct.key_pin_unnamed [] .binding none [] ⟨"pinned"⟩

/-- Mutant `descendantKey` that uses one ordinal across all construct kinds for the root segment. -/
def allKindsDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(allKindsOrdinal preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem descendant_insert_differentKind_example_nestedBody : nestedBody.descendantKey ([] ++ insertedCall :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ []) [0, 0] :=
  KeyConstruct.descendant_insert_differentKind [] [] insertedCall nestedBody [0, 0] (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.allKindsDescendantKey`). -/
theorem descendant_insert_differentKind_planted_nestedBody_allKindsDescendantKey : insertedCall.kind ≠ nestedBody.kind ∧
    ¬ (allKindsDescendantKey ([] ++ insertedCall :: []) nestedBody [0, 0] =
      allKindsDescendantKey ([] ++ []) nestedBody [0, 0]) :=
  ⟨by decide, by decide⟩

/-- Mutant `descendantKey` that counts named siblings in the root segment's ordinal. -/
def countNamedDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(countNamedKey preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem descendant_insert_named_example_nestedBody : nestedBody.descendantKey ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ []) [0, 0] :=
  KeyConstruct.descendant_insert_named [] [] ⟨"body"⟩ .block nestedBody.children nestedBody [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.countNamedDescendantKey`). -/
theorem descendant_insert_named_planted_nestedBody_countNamedDescendantKey : ¬ (countNamedDescendantKey ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) nestedBody [0, 0] =
    countNamedDescendantKey ([] ++ []) nestedBody [0, 0]) := by decide

/-- Mutant `pathAt` that counts ordinals across the whole forest rather than `forest.take index`,
so inserting a sibling after `target` shifts `target`'s ordinal. -/
def wholeForestPathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route => do
    let construct ← forest[index]?
    let descendant ← KeyConstruct.pathAtIn (construct.childScope forest) construct.children route
    pure ⟨construct.segment forest :: descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_after`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem pathAt_insert_after_example_nestedBody : KeyConstruct.pathAt ([] ++ nestedBody :: ([] ++ unnamedBlock :: []))
      (([] : List KeyConstruct).length :: [0, 0]) =
    KeyConstruct.pathAt ([] ++ nestedBody :: ([] ++ []))
      (([] : List KeyConstruct).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_after [] [] [] nestedBody unnamedBlock [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_after`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.wholeForestPathAt`). -/
theorem pathAt_insert_after_planted_nestedBody_wholeForestPathAt : ¬ (wholeForestPathAt ([] ++ nestedBody :: ([] ++ unnamedBlock :: []))
      (([] : List KeyConstruct).length :: [0, 0]) =
    wholeForestPathAt ([] ++ nestedBody :: ([] ++ []))
      (([] : List KeyConstruct).length :: [0, 0])) := by decide

/-- Mutant `pathAt` that counts named siblings in the root segment's ordinal. -/
def countNamedPathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      countNamedDescendantKey (forest.take index) construct route

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem pathAt_insert_named_example_nestedBody : KeyConstruct.pathAt
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) ++ nestedBody :: [])
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []).length :: [0, 0]) =
    KeyConstruct.pathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_named [] [] [] ⟨"body"⟩ .block nestedBody.children nestedBody [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_named`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.countNamedPathAt`). -/
theorem pathAt_insert_named_planted_nestedBody_countNamedPathAt : ¬ (countNamedPathAt
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []) ++ nestedBody :: [])
      (([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: []).length :: [0, 0]) =
    countNamedPathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0])) := by decide

/-- Mutant `pathAt` that counts all unnamed siblings regardless of kind. -/
def allKindsPathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      allKindsDescendantKey (forest.take index) construct route

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem pathAt_insert_differentKind_example_nestedBody : KeyConstruct.pathAt
      (([] ++ insertedCall :: []) ++ nestedBody :: [])
      (([] ++ insertedCall :: []).length :: [0, 0]) =
    KeyConstruct.pathAt
      (([] ++ []) ++ nestedBody :: [])
      (([] ++ ([] : List KeyConstruct)).length :: [0, 0]) :=
  KeyConstruct.pathAt_insert_differentKind [] [] [] insertedCall nestedBody [0, 0] (by decide)

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.pathAt_insert_differentKind`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.pathAt`, mutant `ConcreteSpec.Core.Vacuity.allKindsPathAt`). -/
theorem pathAt_insert_differentKind_planted_nestedBody_allKindsPathAt : insertedCall.kind ≠ nestedBody.kind ∧
    ¬ (allKindsPathAt
        (([] ++ insertedCall :: []) ++ nestedBody :: [])
        (([] ++ insertedCall :: []).length :: [0, 0]) =
      allKindsPathAt
        (([] ++ []) ++ nestedBody :: [])
        (([] ++ ([] : List KeyConstruct)).length :: [0, 0])) :=
  ⟨by decide, by decide⟩

/-- Mutant `descendantKey` that fails to reset the sibling scope when entering a `.named`
construct, leaking outer siblings into the named construct's scope. -/
def unresetScopeDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn preceding construct.children route).map
    fun descendant => ⟨construct.segment preceding :: descendant.path⟩

/-- Mutant `pathAt` that fails to reset the sibling scope when entering a `.named` construct. -/
def unresetScopePathAt (forest : List KeyConstruct) : Numbers → Option FunctionRelativeKey
  | [] => some ⟨[]⟩
  | index :: route =>
    match forest[index]? with
    | none => none
    | some construct =>
      unresetScopeDescendantKey (forest.take index) construct route

/-- Regression example (not independent soundness evidence). -/
theorem example_Regression_not_independent_soundness_evidence : KeyConstruct.pathAt
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([unnamedLoop] : List KeyConstruct).length :: [0, 0]) =
    KeyConstruct.pathAt
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([] : List KeyConstruct).length :: [0, 0]) :=
  KeyConstruct.pathAt_named_ancestor_resets_scope [unnamedLoop] [] [] [] .block ⟨"body"⟩
    nestedBody.children [0, 0]

/-- Regression example (not independent soundness evidence). -/
theorem example_Regression_not_independent_soundness_evidence_2 : ¬ (unresetScopePathAt
      ([unnamedLoop] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([unnamedLoop] : List KeyConstruct).length :: [0, 0]) =
    unresetScopePathAt
      ([] ++ KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children :: [])
      (([] : List KeyConstruct).length :: [0, 0])) := by decide

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem descendant_rename_other_example_nestedBody : nestedBody.descendantKey ([] ++ namedNested.rename ⟨"otherName"⟩ :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ namedNested :: []) [0, 0] :=
  KeyConstruct.descendant_rename_other [] [] namedNested nestedBody ⟨"otherName"⟩ [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_rename_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.rename`, mutant `ConcreteSpec.Core.Vacuity.demoteRename`). -/
theorem descendant_rename_other_planted_nestedBody_demoteRename : ¬ (nestedBody.descendantKey ([] ++ demoteRename namedNested ⟨"otherName"⟩ :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ namedNested :: []) [0, 0]) := by decide

/-- Mutant `descendantKey` that excludes pinned unnamed siblings from the root's ordinal count. -/
def labelOnlyDescendantKey (preceding : List KeyConstruct) (construct : KeyConstruct)
    (route : Numbers) : Option FunctionRelativeKey :=
  (KeyConstruct.pathAtIn (construct.childScope preceding) construct.children route).map
    fun descendant => ⟨(labelOnlyOrdinal preceding construct).path ++ descendant.path⟩

/-- Instance (law `ConcreteSpec.Core.KeyConstruct.descendant_pin_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`). -/
theorem descendant_pin_other_example_nestedBody : nestedBody.descendantKey ([] ++ nestedBody.pin (some ⟨"pinned"⟩) :: []) [0, 0] =
    nestedBody.descendantKey ([] ++ nestedBody :: []) [0, 0] :=
  KeyConstruct.descendant_pin_other [] [] nestedBody nestedBody (some ⟨"pinned"⟩) [0, 0]

/-- Planted (law `ConcreteSpec.Core.KeyConstruct.descendant_pin_other`, case `ConcreteSpec.Core.Vacuity.nestedBody`, subject `ConcreteSpec.Core.KeyConstruct.descendantKey`, mutant `ConcreteSpec.Core.Vacuity.labelOnlyDescendantKey`). -/
theorem descendant_pin_other_planted_nestedBody_labelOnlyDescendantKey : ¬ (labelOnlyDescendantKey ([] ++ nestedBody.pin (some ⟨"pinned"⟩) :: []) nestedBody [0, 0] =
    labelOnlyDescendantKey ([] ++ nestedBody :: []) nestedBody [0, 0]) := by decide

/-- Representation example (not law evidence). -/
theorem example_Representation_not_law_evidence : ((KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children).rename ⟨"renamed"⟩).descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named ⟨"renamed"⟩⟩ :: suffix.path⟩) :=
  KeyConstruct.descendant_rename_named [] .block ⟨"body"⟩ ⟨"renamed"⟩ nestedBody.children [0, 0]

/-- Representation example (not law evidence). -/
theorem example_Representation_not_law_evidence_2 : ¬ ((noopRename (KeyConstruct.mk .block (.named ⟨"body"⟩) nestedBody.children) ⟨"renamed"⟩).descendantKey [] [0, 0] =
    (KeyConstruct.pathAt nestedBody.children [0, 0]).map (fun suffix =>
      ⟨⟨.block, .named ⟨"renamed"⟩⟩ :: suffix.path⟩)) := by decide

/-- Three specializations of one generic, differing one argument list at a time. -/
def numeric : SpecializationId Nat Name := ⟨.function function, [0], [⟨"Fs"⟩]⟩
def otherType : SpecializationId Nat Name := ⟨.function function, [1], [⟨"Fs"⟩]⟩
def otherCapability : SpecializationId Nat Name := ⟨.function function, [0], [⟨"Console"⟩]⟩

/-- Distinct request sites for the same specialization. -/
def requestZero : SpecializationRequest Nat Name := ⟨⟨0⟩, numeric⟩
def requestOne : SpecializationRequest Nat Name := ⟨⟨1⟩, numeric⟩
def requestOtherType : SpecializationRequest Nat Name := ⟨⟨2⟩, otherType⟩
def requestOtherCapability : SpecializationRequest Nat Name := ⟨⟨3⟩, otherCapability⟩
def edgeZero : SpecializationEdge Nat Name := ⟨requestZero, 0⟩

/-- Mutated implementation that drops the first request's edge. -/
def dropFirstEdges {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  { SpecializationTable.canonicalize requests with edges := (SpecializationTable.canonicalize requests).edges.tail }

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_complete`, case `ConcreteSpec.Core.Vacuity.requestZero`). -/
theorem canonicalize_complete_example_requestZero : ∃ edge ∈ (SpecializationTable.canonicalize [requestZero, requestOne]).edges,
    edge.request = requestZero ∧
      SpecializationTable.entryAt (SpecializationTable.canonicalize [requestZero, requestOne]).entries edge.entry =
        some requestZero.specialization :=
  SpecializationTable.canonicalize_complete [requestZero, requestOne] requestZero (by decide)

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_complete`, case `ConcreteSpec.Core.Vacuity.requestZero`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.dropFirstEdges`). -/
theorem canonicalize_complete_planted_requestZero_dropFirstEdges : requestZero ∈ [requestZero, requestOne] ∧
    ¬ (∃ edge ∈ (dropFirstEdges [requestZero, requestOne]).edges,
      edge.request = requestZero ∧
        SpecializationTable.entryAt (dropFirstEdges [requestZero, requestOne]).entries edge.entry =
          some requestZero.specialization) :=
  ⟨by decide, by decide⟩

/-- Mutant `entryAt` that offsets the lookup index out of bounds. -/
def oobEntryAt {Ty Cap : Type} (entries : List (SpecializationId Ty Cap))
    (index : Nat) : Option (SpecializationId Ty Cap) :=
  entries[index + 9]?

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound`, case `ConcreteSpec.Core.Vacuity.edgeZero`). -/
theorem canonicalize_edge_sound_example_edgeZero : SpecializationTable.entryAt
    (SpecializationTable.canonicalize [requestZero, requestOne]).entries edgeZero.entry =
    some edgeZero.request.specialization :=
  SpecializationTable.canonicalize_edge_sound [requestZero, requestOne] edgeZero (by decide)

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_edge_sound`, case `ConcreteSpec.Core.Vacuity.edgeZero`, subject `ConcreteSpec.Core.SpecializationTable.entryAt`, mutant `ConcreteSpec.Core.Vacuity.oobEntryAt`). -/
theorem canonicalize_edge_sound_planted_edgeZero_oobEntryAt : edgeZero ∈ (SpecializationTable.canonicalize [requestZero, requestOne]).edges ∧
    ¬ (oobEntryAt (SpecializationTable.canonicalize [requestZero, requestOne]).entries edgeZero.entry =
      some edgeZero.request.specialization) :=
  ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_share`, case `ConcreteSpec.Core.Vacuity.requestZero`). -/
theorem canonicalize_share_example_requestZero : ∃ zeroEdge oneEdge, zeroEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ oneEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOne]).edges ∧ zeroEdge.request = requestZero ∧
    oneEdge.request = requestOne ∧ zeroEdge.entry = oneEdge.entry :=
  SpecializationTable.canonicalize_share [requestZero, requestOne] requestZero requestOne
    (by simp) (by simp) rfl

/-- A planted interner parameterized by a lossy request key. -/
def canonicalizeByKey {Key : Type} [DecidableEq Key]
    (key : SpecializationRequest Nat Name → Key) :
    List (SpecializationRequest Nat Name) → List Key × SpecializationTable Nat Name
  | [] => ([], ⟨[], []⟩)
  | request :: rest =>
    let (keys, table) := canonicalizeByKey key rest
    let index := keys.idxOf (key request)
    if index < keys.length then
      (keys, { table with edges := ⟨request, index⟩ :: table.edges })
    else
      (keys ++ [key request], ⟨table.entries ++ [request.specialization],
        ⟨request, table.entries.length⟩ :: table.edges⟩)

/-- Canonicalizing by lexical request site incorrectly separates equal specializations requested
from different sites. -/
def bySite {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  ⟨requests.map (·.specialization),
    requests.zipIdx.map fun (req, idx) => ⟨req, idx⟩⟩

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_share`, case `ConcreteSpec.Core.Vacuity.requestZero`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.bySite`). -/
theorem canonicalize_share_planted_requestZero_bySite : (requestZero ∈ [requestZero, requestOne] ∧
    requestOne ∈ [requestZero, requestOne] ∧
    requestZero.specialization = requestOne.specialization) ∧
    ¬ (∃ first second, first ∈ (bySite [requestZero, requestOne]).edges ∧
      second ∈ (bySite [requestZero, requestOne]).edges ∧ first.request = requestZero ∧
      second.request = requestOne ∧ first.entry = second.entry) := by
  refine ⟨⟨by decide, by decide, rfl⟩, ?_⟩
  simp [bySite, requestZero, requestOne]

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherType`). -/
theorem canonicalize_distinct_example_requestOtherType : ∃ typeEdge otherEdge, typeEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherType]).edges ∧ otherEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherType]).edges ∧ typeEdge.request = requestZero ∧
    otherEdge.request = requestOtherType ∧ typeEdge.entry ≠ otherEdge.entry :=
  SpecializationTable.canonicalize_distinct [requestZero, requestOtherType] requestZero requestOtherType
    (by simp) (by simp) (by decide)

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherCapability`). -/
theorem canonicalize_distinct_example_requestOtherCapability : ∃ capabilityEdge otherEdge, capabilityEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherCapability]).edges ∧ otherEdge ∈ (SpecializationTable.canonicalize
    [requestZero, requestOtherCapability]).edges ∧ capabilityEdge.request = requestZero ∧
    otherEdge.request = requestOtherCapability ∧ capabilityEdge.entry ≠ otherEdge.entry :=
  SpecializationTable.canonicalize_distinct [requestZero, requestOtherCapability] requestZero
    requestOtherCapability (by simp) (by simp) (by decide)

/-- A generic-only interner that coalesces all requests into entry `0`. -/
def onlyGeneric {Ty Cap : Type} [DecidableEq Ty] [DecidableEq Cap]
    (requests : List (SpecializationRequest Ty Cap)) : SpecializationTable Ty Cap :=
  ⟨match requests.head? with | some r => [r.specialization] | none => [],
    requests.map fun req => ⟨req, 0⟩⟩

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherType`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.onlyGeneric`). -/
theorem canonicalize_distinct_planted_requestOtherType_onlyGeneric : (requestZero ∈ [requestZero, requestOtherType] ∧
    requestOtherType ∈ [requestZero, requestOtherType] ∧
    requestZero.specialization ≠ requestOtherType.specialization) ∧
    ¬ (∃ first second, first ∈ (onlyGeneric [requestZero, requestOtherType]).edges ∧
      second ∈ (onlyGeneric [requestZero, requestOtherType]).edges ∧ first.request = requestZero ∧
      second.request = requestOtherType ∧ first.entry ≠ second.entry) := by
  refine ⟨⟨by decide, by decide, by decide⟩, ?_⟩
  simp [onlyGeneric]

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_distinct`, case `ConcreteSpec.Core.Vacuity.requestOtherCapability`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.onlyGeneric`). -/
theorem canonicalize_distinct_planted_requestOtherCapability_onlyGeneric : (requestZero ∈ [requestZero, requestOtherCapability] ∧
    requestOtherCapability ∈ [requestZero, requestOtherCapability] ∧
    requestZero.specialization ≠ requestOtherCapability.specialization) ∧
    ¬ (∃ first second, first ∈ (onlyGeneric [requestZero, requestOtherCapability]).edges ∧
      second ∈ (onlyGeneric [requestZero, requestOtherCapability]).edges ∧ first.request = requestZero ∧
      second.request = requestOtherCapability ∧ first.entry ≠ second.entry) := by
  refine ⟨⟨by decide, by decide, by decide⟩, ?_⟩
  simp [onlyGeneric]

/-- Instance (law `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup`, case `ConcreteSpec.Core.Vacuity.requestZero`). -/
theorem canonicalize_nodup_example_requestZero : (SpecializationTable.canonicalize [requestZero, requestOne]).entries.Nodup :=
  SpecializationTable.canonicalize_nodup [requestZero, requestOne]

/-- Planted (law `ConcreteSpec.Core.SpecializationTable.canonicalize_nodup`, case `ConcreteSpec.Core.Vacuity.requestZero`, subject `ConcreteSpec.Core.SpecializationTable.canonicalize`, mutant `ConcreteSpec.Core.Vacuity.bySite`). -/
theorem canonicalize_nodup_planted_requestZero_bySite : ¬ (bySite [requestZero, requestOne]).entries.Nodup := by decide

/-- Loaded modules with explicit package versions and origins. -/
def firstModule : LoadedModule := ⟨⟨package ⟨"p1"⟩, [⟨"first"⟩], by decide⟩, ⟨⟨"registry-a"⟩⟩⟩
def secondModule : LoadedModule := ⟨⟨package ⟨"p1"⟩, [⟨"second"⟩], by decide⟩, ⟨⟨"registry-a"⟩⟩⟩
def collidingModule : LoadedModule := ⟨⟨package ⟨"p2"⟩, [⟨"first"⟩], by decide⟩, ⟨⟨"registry-b"⟩⟩⟩
def samePackageOtherOrigin : LoadedModule := ⟨⟨package ⟨"p1"⟩, [⟨"other"⟩], by decide⟩, ⟨⟨"mirror"⟩⟩⟩
def collidingTop : LoadCollision := .topLevel firstModule collidingModule rfl (by decide)
def collidingOrigin : LoadCollision := .packageOrigin firstModule samePackageOtherOrigin (by decide) (by decide)

/-- Mutant loader that fabricates a collision naming a module not in the loaded list. -/
def fabricatedRefusalLoad (_modules : List LoadedModule) : LoadResult :=
  .refused collidingOrigin

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`). -/
theorem load_refusal_sound_example_collidingTop : match LoadedModules.load [firstModule, collidingModule] with
    | .refused collision => LoadedModules.HasCollision [firstModule, collidingModule] collision
    | .admitted admitted => admitted = [firstModule, collidingModule] ∧
        LoadedModules.WellFormed [firstModule, collidingModule] :=
  LoadedModules.load_refusal_sound [firstModule, collidingModule]

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.fabricatedRefusalLoad`). -/
theorem load_refusal_sound_planted_collidingTop_fabricatedRefusalLoad : ¬ (match fabricatedRefusalLoad [firstModule, collidingModule] with
    | .refused collision => LoadedModules.HasCollision [firstModule, collidingModule] collision
    | .admitted admitted => admitted = [firstModule, collidingModule] ∧
        LoadedModules.WellFormed [firstModule, collidingModule]) := by
  change ¬ LoadedModules.HasCollision [firstModule, collidingModule] collidingOrigin
  unfold LoadedModules.HasCollision
  decide

/-- Mutant loader that admits all module lists without checking for collisions. -/
def admitAll (modules : List LoadedModule) : LoadResult := .admitted modules

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_sound`, case `ConcreteSpec.Core.Vacuity.collidingTop`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
theorem load_refusal_sound_planted_collidingTop_admitAll : ¬ (match admitAll [firstModule, collidingModule] with
    | .refused collision => LoadedModules.HasCollision [firstModule, collidingModule] collision
    | .admitted admitted => admitted = [firstModule, collidingModule] ∧
        LoadedModules.WellFormed [firstModule, collidingModule]) := by
  rintro ⟨_, wf⟩
  exact wf collidingTop ⟨by decide, by decide⟩

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_refusal_complete`, case `ConcreteSpec.Core.Vacuity.collidingOrigin`). -/
theorem load_refusal_complete_example_collidingOrigin : ∃ detected, LoadedModules.load [firstModule, samePackageOtherOrigin] = .refused detected :=
  LoadedModules.load_refusal_complete [firstModule, samePackageOtherOrigin] collidingOrigin
    ⟨by decide, by decide⟩

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_refusal_complete`, case `ConcreteSpec.Core.Vacuity.collidingOrigin`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
theorem load_refusal_complete_planted_collidingOrigin_admitAll : LoadedModules.HasCollision [firstModule, samePackageOtherOrigin] collidingOrigin ∧
    ¬ (∃ detected, admitAll [firstModule, samePackageOtherOrigin] = .refused detected) := by
  refine ⟨⟨by decide, by decide⟩, ?_⟩
  rintro ⟨detected, h⟩
  cases h

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.firstModule`). -/
theorem load_admitted_example_firstModule : LoadedModules.load [firstModule, secondModule] =
      .admitted [firstModule, secondModule] ↔
    [firstModule, secondModule] = [firstModule, secondModule] ∧
      LoadedModules.WellFormed [firstModule, secondModule] :=
  LoadedModules.load_admitted [firstModule, secondModule] [firstModule, secondModule]

/-- Mutant loader that discards the input instead of admitting it unchanged. -/
def discardLoad (_modules : List LoadedModule) : LoadResult := .admitted []

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.firstModule`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.discardLoad`). -/
theorem load_admitted_planted_firstModule_discardLoad : ¬ (discardLoad [firstModule, secondModule] =
      .admitted [firstModule, secondModule] ↔
    [firstModule, secondModule] = [firstModule, secondModule] ∧
      LoadedModules.WellFormed [firstModule, secondModule]) := by
  intro equal
  have admitted := equal.mpr ⟨rfl, (LoadedModules.load_admitted [firstModule, secondModule]
    [firstModule, secondModule]).mp rfl |>.2⟩
  cases admitted

/-- Instance (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.collidingModule`). -/
theorem load_admitted_example_collidingModule : LoadedModules.load [firstModule, collidingModule] =
      .admitted [firstModule, collidingModule] ↔
    [firstModule, collidingModule] = [firstModule, collidingModule] ∧
      LoadedModules.WellFormed [firstModule, collidingModule] :=
  LoadedModules.load_admitted [firstModule, collidingModule] [firstModule, collidingModule]

/-- Planted (law `ConcreteSpec.Core.LoadedModules.load_admitted`, case `ConcreteSpec.Core.Vacuity.collidingModule`, subject `ConcreteSpec.Core.LoadedModules.load`, mutant `ConcreteSpec.Core.Vacuity.admitAll`). -/
theorem load_admitted_planted_collidingModule_admitAll : ¬ (admitAll [firstModule, collidingModule] =
      .admitted [firstModule, collidingModule] ↔
    [firstModule, collidingModule] = [firstModule, collidingModule] ∧
      LoadedModules.WellFormed [firstModule, collidingModule]) := by
  intro equal
  have wf := (equal.mp rfl).2
  exact wf collidingTop ⟨by decide, by decide⟩

/-- Typed byte strings for newline, tab, EOF and multibyte examples. -/
def aSource : SourceText := SourceText.ofBytes [97]
def aNewline : SourceText := aSource.push 10
def aTab : SourceText := aSource.push 9
def aTabNewline : SourceText := aTab.push 10
def utf8Prefix : SourceText := SourceText.ofBytes [195]
def utf8Source : SourceText := SourceText.ofBytes [195, 169]
def otherTabSource : SourceText := SourceText.ofBytes [10, 97, 98]

theorem example_aSource_aTab : aSource ≠ aTab := by decide

/-- Mutant line function with a zero-based origin. -/
def zeroBasedLine (source : SourceText) (offset : Nat) : Nat :=
  (source.bytes.take offset).count 10

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
theorem lineAt_zero_example_aSource : aSource.lineAt 0 = 1 := SourceText.lineAt_zero aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.zeroBasedLine`). -/
theorem lineAt_zero_planted_aSource_zeroBasedLine : ¬ (zeroBasedLine aSource 0 = 1) := by decide

/-- Mutant column function with a zero-based origin. -/
def zeroBasedColumn (source : SourceText) (offset : Nat) : Nat :=
  ((source.bytes.take offset).reverse.takeWhile (fun byte => byte != 10)).length

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
theorem columnAt_zero_example_aSource : aSource.columnAt 0 = 1 := SourceText.columnAt_zero aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_zero`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.zeroBasedColumn`). -/
theorem columnAt_zero_planted_aSource_zeroBasedColumn : ¬ (zeroBasedColumn aSource 0 = 1) := by decide

/-- Mutant line function that always returns line 1. -/
def constantLine (_source : SourceText) (_offset : Nat) : Nat := 1

/-- Mutant line function that increments on every byte regardless of newline. -/
def offsetLine (_source : SourceText) (offset : Nat) : Nat := offset + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
theorem lineAt_append_newline_example_aSource : (aSource.push 10).lineAt (aSource.length + 1) = aSource.lineAt aSource.length + 1 :=
  SourceText.lineAt_append_newline aSource (by decide)

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
theorem lineAt_append_newline_planted_aSource_constantLine :
    (aSource.positionAt aSource.length).afterCR = false ∧
      ¬ (constantLine (aSource.push 10) (aSource.length + 1) =
        constantLine aSource aSource.length + 1) := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
theorem lineAt_append_other_example_aSource : (aSource.push 9).lineAt (aSource.length + 1) = aSource.lineAt aSource.length :=
  SourceText.lineAt_append_other aSource 9 (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append_other`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.offsetLine`). -/
theorem lineAt_append_other_planted_aSource_offsetLine : (9 : UInt8) ≠ 10 ∧ (9 : UInt8) ≠ 13 ∧
    ¬ (offsetLine (aSource.push 9) (aSource.length + 1) = offsetLine aSource aSource.length) :=
  ⟨by decide, by decide, by decide⟩

/-- Mutant column function that counts a tab byte (`9`) as four columns instead of one. -/
def tabStopFour (source : SourceText) (offset : Nat) : Nat :=
  ((source.toBytes.take offset).reverse.takeWhile (fun byte => byte != 10)).foldl
    (fun acc byte => acc + if byte == 9 then 4 else 1) 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
theorem columnAt_append_tab_example_aSource : (aSource.push 9).columnAt (aSource.length + 1) = aSource.columnAt aSource.length + 1 :=
  SourceText.columnAt_append_tab aSource

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_tab`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.tabStopFour`). -/
theorem columnAt_append_tab_planted_aSource_tabStopFour : ¬ (tabStopFour (aSource.push 9) (aSource.length + 1) =
    tabStopFour aSource aSource.length + 1) := by decide

/-- Mutant column function that never resets at a newline byte. -/
def noResetColumn (source : SourceText) (offset : Nat) : Nat :=
  (source.toBytes.take offset).length + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`). -/
theorem columnAt_append_newline_example_aTab : (aTab.push 10).columnAt (aTab.length + 1) = 1 :=
  SourceText.columnAt_append_newline aTab

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append_newline`, case `ConcreteSpec.Core.Vacuity.aTab`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.noResetColumn`). -/
theorem columnAt_append_newline_planted_aTab_noResetColumn : ¬ (noResetColumn (aTab.push 10) (aTab.length + 1) = 1) := by decide

/-- Instance (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`). -/
theorem lineAt_append_example_aSource : (aSource.push 10).lineAt (aSource.length + 1) =
    aSource.lineAt aSource.length + if (10 : UInt8) = 13 ∨
      ((10 : UInt8) = 10 ∧ !(aSource.positionAt aSource.length).afterCR) then 1 else 0 :=
  SourceText.lineAt_append aSource 10

/-- Planted (law `ConcreteSpec.Core.SourceText.lineAt_append`, case `ConcreteSpec.Core.Vacuity.aSource`, subject `ConcreteSpec.Core.SourceText.lineAt`, mutant `ConcreteSpec.Core.Vacuity.constantLine`). -/
theorem lineAt_append_planted_aSource_constantLine : ¬ (constantLine (aSource.push 10) (aSource.length + 1) =
    constantLine aSource aSource.length + if (10 : UInt8) = 13 ∨
      ((10 : UInt8) = 10 ∧ !(aSource.positionAt aSource.length).afterCR) then 1 else 0) := by decide

/-- Mutant column function that skips UTF-8 continuation bytes (`128 ≤ b < 192`). -/
def scalarColumn (source : SourceText) (offset : Nat) : Nat :=
  (((source.toBytes.take offset).reverse.takeWhile (fun byte => byte != 10)).filter
    (fun byte => decide (byte < 128 ∨ 192 ≤ byte))).length + 1

/-- Instance (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`). -/
theorem columnAt_append_example_utf8Prefix : (utf8Prefix.push 169).columnAt (utf8Prefix.length + 1) =
    if (169 : UInt8) = 13 ∨ (169 : UInt8) = 10 then 1 else utf8Prefix.columnAt utf8Prefix.length + 1 :=
  SourceText.columnAt_append utf8Prefix 169

/-- Planted (law `ConcreteSpec.Core.SourceText.columnAt_append`, case `ConcreteSpec.Core.Vacuity.utf8Prefix`, subject `ConcreteSpec.Core.SourceText.columnAt`, mutant `ConcreteSpec.Core.Vacuity.scalarColumn`). -/
theorem columnAt_append_planted_utf8Prefix_scalarColumn : ¬ (scalarColumn (utf8Prefix.push 169) (utf8Prefix.length + 1) =
    if (169 : UInt8) = 13 ∨ (169 : UInt8) = 10 then 1 else scalarColumn utf8Prefix utf8Prefix.length + 1) := by decide

/-- A source `assume`, a package stack assumption, and their wire records. -/
def sourceAssume : PremiseId :=
  ⟨.assume, .function (StableKey.mk (.function function) ⟨[⟨.assumption, .ordinal 0⟩]⟩)⟩
def stackAssumption : PremiseId := ⟨.stack, .package package ⟨⟨"defaultStack"⟩⟩⟩
def stackRecord : PremiseRecord := ⟨stackAssumption.kind, stackAssumption.scope, specVersion⟩
def unknownRecord : EncodedPremise := ⟨99, stackAssumption.scope, specVersion⟩
def futureVersion : SpecVersion := ⟨0, 2⟩
def mismatchedRecord : EncodedPremise := ⟨PremiseKind.stack.encode, stackAssumption.scope, futureVersion⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_encode`, case `ConcreteSpec.Core.Vacuity.stackRecord`). -/
theorem read_encode_example_stackRecord : stackRecord.encode.read specVersion = some stackRecord :=
  EncodedPremise.read_encode stackRecord rfl

/-- A faulty encoder that discards the known premise category, retaining the writer version and
typed scope. -/
def encodeDroppingKind (record : PremiseRecord) : EncodedPremise :=
  { record.encode with category := 0 }

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_encode`, case `ConcreteSpec.Core.Vacuity.stackRecord`, subject `ConcreteSpec.Core.PremiseRecord.encode`, mutant `ConcreteSpec.Core.Vacuity.encodeDroppingKind`). -/
theorem read_encode_planted_stackRecord_encodeDroppingKind : stackRecord.spec = specVersion ∧
    ¬ ((encodeDroppingKind stackRecord).read specVersion = some stackRecord) :=
  ⟨rfl, by decide⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_unknown`, case `ConcreteSpec.Core.Vacuity.unknownRecord`). -/
theorem read_unknown_example_unknownRecord : unknownRecord.read specVersion = none :=
  EncodedPremise.read_unknown specVersion unknownRecord (by intro kind; cases kind <;> decide)

/-- Mutated reader that assigns a catch-all `.assume` category to unknown numbers. -/
def catchAllRead (reader : SpecVersion) (record : EncodedPremise) : Option PremiseRecord :=
  if record.spec = reader then some ⟨.assume, record.scope, record.spec⟩ else none

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_unknown`, case `ConcreteSpec.Core.Vacuity.unknownRecord`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.catchAllRead`). -/
theorem read_unknown_planted_unknownRecord_catchAllRead : (∀ kind : PremiseKind, kind.encode ≠ unknownRecord.category) ∧
    ¬ (catchAllRead specVersion unknownRecord = none) :=
  ⟨by intro kind; cases kind <;> decide, by decide⟩

/-- Mutated reader that ignores version mismatches and decodes every record under `specVersion`. -/
def versionIgnoringRead (_reader : SpecVersion) (record : EncodedPremise) : Option PremiseRecord :=
  (PremiseKind.decode specVersion record.category).map fun kind => ⟨kind, record.scope, record.spec⟩

/-- Decoded premise record corresponding to `mismatchedRecord` when version checks are skipped. -/
def mismatchedDecoded : PremiseRecord := ⟨.stack, stackAssumption.scope, futureVersion⟩

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.mismatchedRecord`). -/
theorem read_iff_example_mismatchedRecord : mismatchedRecord.read specVersion = some mismatchedDecoded ↔
    mismatchedRecord.spec = specVersion ∧ specVersion = specVersion ∧
      ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
        mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩ :=
  EncodedPremise.read_iff specVersion mismatchedRecord mismatchedDecoded

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.mismatchedRecord`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.versionIgnoringRead`). -/
theorem read_iff_planted_mismatchedRecord_versionIgnoringRead : ¬ (versionIgnoringRead specVersion mismatchedRecord = some mismatchedDecoded ↔
    mismatchedRecord.spec = specVersion ∧ specVersion = specVersion ∧
      ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
        mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩) := by
  intro equal
  have hspec := (equal.mp rfl).1
  revert hspec
  decide

/-- Instance (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.futureVersion`).
Writer and reader agree on the future version, isolating the current-schema guard. -/
theorem read_iff_example_futureVersion : mismatchedRecord.read futureVersion = some mismatchedDecoded ↔
    mismatchedRecord.spec = futureVersion ∧ futureVersion = specVersion ∧
      ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
        mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩ :=
  EncodedPremise.read_iff futureVersion mismatchedRecord mismatchedDecoded

/-- Planted (law `ConcreteSpec.Core.EncodedPremise.read_iff`, case `ConcreteSpec.Core.Vacuity.futureVersion`, subject `ConcreteSpec.Core.EncodedPremise.read`, mutant `ConcreteSpec.Core.Vacuity.versionIgnoringRead`).
The category is known and writer equals reader; only the current-schema guard fails. -/
theorem read_iff_planted_futureVersion_versionIgnoringRead : mismatchedRecord.spec = futureVersion ∧
    PremiseKind.decode specVersion mismatchedRecord.category = some .stack ∧
    ¬ (versionIgnoringRead futureVersion mismatchedRecord = some mismatchedDecoded ↔
      mismatchedRecord.spec = futureVersion ∧ futureVersion = specVersion ∧
        ∃ kind : PremiseKind, kind.encode = mismatchedRecord.category ∧
          mismatchedDecoded = ⟨kind, mismatchedRecord.scope, mismatchedRecord.spec⟩) := by
  refine ⟨rfl, rfl, ?_⟩
  intro equal
  have hreader := (equal.mp rfl).2.1
  revert hreader
  decide

/-- Accepted: declaration and constant owners cover non-function obligations without fake functions. -/
theorem example_Accepted_declaration_and_constant_owners_cover : invariantKey.owner = .declaration declaration ∧ initializerKey.owner = .constant constant := by
  exact ⟨rfl, rfl⟩

/-- Two facts with the same conclusion but different premise dependencies. -/
def conditional : ConditionalFact Name := ⟨⟨"bounded"⟩, [sourceAssume]⟩
def unconditional : ConditionalFact Name := ⟨⟨"bounded"⟩, []⟩

/-- Planted counter-example: comparing only conclusions loses the conditional fact's premise debt. -/
def conclusionOnly (first second : ConditionalFact Name) : Prop :=
  first.conclusion = second.conclusion

theorem example_conclusionOnly_conditional_unconditional_conditional_unconditional : conclusionOnly conditional unconditional ∧ conditional ≠ unconditional := by
  exact ⟨rfl, by decide⟩

end Vacuity

namespace SourcePath

namespace LocalControls

open Registry

/-- The selected reason, half-open range, byte offset and exact context. -/
def reportedLocation (result : Except Refusal SourcePath) :
    Option (Reason × Nat × Nat × Nat × ConcreteSpec.Numbers) :=
  match result with
  | .ok _ => none
  | .error refusal =>
    let location := refusal.violation.location
    some (refusal.violation.reason, location.start, location.stop, location.offset, refusal.contextBytes)

/-- Read the selected reason and byte offset from a production parse. -/
def reported (result : Except Refusal SourcePath) : Option (Reason × Nat) :=
  match result with
  | .ok _ => none
  | .error refusal => some (refusal.violation.reason, refusal.violation.location.offset)

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_empty_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem emptyAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_empty_iff _).2 ⟨emptyAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_empty_iff`, case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`). -/
theorem emptyRejected : ¬∃ path, parse emptyRefusedInput = .ok path :=
  fun admitted => emptyRefused ((admission_empty_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`). -/
theorem emptyReported :
    reportedLocation (parse emptyRefusedInput) = some (.empty, 0, 0, 0, []) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_absolute_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem absoluteAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_absolute_iff _).2 ⟨absoluteAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_absolute_iff`, case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`). -/
theorem absoluteRejected : ¬∃ path, parse absoluteRefusedInput = .ok path :=
  fun admitted => absoluteRefused ((admission_absolute_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`). -/
theorem absoluteReported :
    reportedLocation (parse absoluteRefusedInput) = some (.absolute, 0, 2, 0, [47, 65]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_drive_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem driveAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_drive_iff _).2 ⟨driveAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_drive_iff`, case `ConcreteSpec.Core.SourcePath.Registry.driveRefusedInput`). -/
theorem driveRejected : ¬∃ path, parse driveRefusedInput = .ok path :=
  fun admitted => driveRefused ((admission_drive_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.driveRefusedInput`). -/
theorem driveReported :
    reportedLocation (parse driveRefusedInput) = some (.drive, 0, 2, 1, [67, 58]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem traversalAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_traversal_iff _).2 ⟨traversalAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`, case `ConcreteSpec.Core.SourcePath.Registry.traversalRefusedInput`). -/
theorem traversalRejected : ¬∃ path, parse traversalRefusedInput = .ok path :=
  fun admitted => traversalRefused ((admission_traversal_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.traversalRefusedInput`). -/
theorem traversalReported :
    reportedLocation (parse traversalRefusedInput) = some (.traversal, 2, 4, 2, [46, 46]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem singleDotAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_singleDot_iff _).2 ⟨singleDotAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.singleDotRefusedInput`). -/
theorem singleDotRejected : ¬∃ path, parse singleDotRefusedInput = .ok path :=
  fun admitted => singleDotRefused ((admission_singleDot_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.singleDotRefusedInput`). -/
theorem singleDotReported :
    reportedLocation (parse singleDotRefusedInput) = some (.singleDot, 2, 3, 2, [46]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem emptyComponentAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_emptyComponent_iff _).2 ⟨emptyComponentAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`, case `ConcreteSpec.Core.SourcePath.Registry.emptyComponentRefusedInput`). -/
theorem emptyComponentRejected : ¬∃ path, parse emptyComponentRefusedInput = .ok path :=
  fun admitted => emptyComponentRefused ((admission_emptyComponent_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.emptyComponentRefusedInput`). -/
theorem emptyComponentReported :
    reportedLocation (parse emptyComponentRefusedInput) = some (.emptyComponent, 2, 2, 2, []) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem nonAsciiAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_nonAscii_iff _).2 ⟨nonAsciiAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`, case `ConcreteSpec.Core.SourcePath.Registry.nonAsciiRefusedInput`). -/
theorem nonAsciiRejected : ¬∃ path, parse nonAsciiRefusedInput = .ok path :=
  fun admitted => nonAsciiRefused ((admission_nonAscii_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.nonAsciiRefusedInput`). -/
theorem nonAsciiReported :
    reportedLocation (parse nonAsciiRefusedInput) = some (.nonAscii, 0, 2, 1, [65, 128]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem backslashAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_backslash_iff _).2 ⟨backslashAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`, case `ConcreteSpec.Core.SourcePath.Registry.backslashRefusedInput`). -/
theorem backslashRejected : ¬∃ path, parse backslashRefusedInput = .ok path :=
  fun admitted => backslashRefused ((admission_backslash_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.backslashRefusedInput`). -/
theorem backslashReported :
    reportedLocation (parse backslashRefusedInput) = some (.backslash, 0, 3, 1, [65, 92, 66]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_control_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem controlAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_control_iff _).2 ⟨controlAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_control_iff`, case `ConcreteSpec.Core.SourcePath.Registry.controlRefusedInput`). -/
theorem controlRejected : ¬∃ path, parse controlRefusedInput = .ok path :=
  fun admitted => controlRefused ((admission_control_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.controlRefusedInput`). -/
theorem controlReported :
    reportedLocation (parse controlRefusedInput) = some (.control, 0, 2, 1, [65, 127]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_space_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem spaceAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_space_iff _).2 ⟨spaceAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_space_iff`, case `ConcreteSpec.Core.SourcePath.Registry.spaceRefusedInput`). -/
theorem spaceRejected : ¬∃ path, parse spaceRefusedInput = .ok path :=
  fun admitted => spaceRefused ((admission_space_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.spaceRefusedInput`). -/
theorem spaceReported :
    reportedLocation (parse spaceRefusedInput) = some (.space, 0, 2, 1, [65, 32]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem leadingDotAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_leadingDot_iff _).2 ⟨leadingDotAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.leadingDotRefusedInput`). -/
theorem leadingDotRejected : ¬∃ path, parse leadingDotRefusedInput = .ok path :=
  fun admitted => leadingDotRefused ((admission_leadingDot_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.leadingDotRefusedInput`). -/
theorem leadingDotReported :
    reportedLocation (parse leadingDotRefusedInput) = some (.leadingDot, 0, 2, 0, [46, 65]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem trailingDotAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_trailingDot_iff _).2 ⟨trailingDotAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`, case `ConcreteSpec.Core.SourcePath.Registry.trailingDotRefusedInput`). -/
theorem trailingDotRejected : ¬∃ path, parse trailingDotRefusedInput = .ok path :=
  fun admitted => trailingDotRefused ((admission_trailingDot_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.trailingDotRefusedInput`). -/
theorem trailingDotReported :
    reportedLocation (parse trailingDotRefusedInput) = some (.trailingDot, 0, 2, 1, [65, 46]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`, case `ConcreteSpec.Core.SourcePath.Registry.reservedAcceptedInput`). -/
theorem reservedAdmitted : ∃ path, parse reservedAcceptedInput = .ok path :=
  (admission_reserved_iff _).2 ⟨reservedAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`, case `ConcreteSpec.Core.SourcePath.Registry.reservedRefusedInput`). -/
theorem reservedRejected : ¬∃ path, parse reservedRefusedInput = .ok path :=
  fun admitted => reservedRefused ((admission_reserved_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.reservedRefusedInput`). -/
theorem reservedReported :
    reportedLocation (parse reservedRefusedInput) = some (.reserved, 0, 5, 0, [99, 79, 78, 46, 67]) := by decide

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.componentCapAcceptedInput`). -/
theorem componentCapAdmitted : ∃ path, parse componentCapAcceptedInput = .ok path :=
  (admission_componentCap_iff _).2 ⟨componentCapAccepted, of_decide_eq_true (by cbv)⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.componentCapRefusedInput`). -/
theorem componentCapRejected : ¬∃ path, parse componentCapRefusedInput = .ok path :=
  fun admitted => componentCapRefused ((admission_componentCap_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.componentCapRefusedInput`). -/
theorem componentCapReported :
    reportedLocation (parse componentCapRefusedInput) = some (.componentCap, 0, 101, 100, List.replicate 101 65) := of_decide_eq_true (by cbv)

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.pathCapRefusedInput`). -/
theorem pathCapRejected : ¬∃ path, parse pathCapRefusedInput = .ok path :=
  fun admitted => pathCapRefused ((admission_pathCap_iff _).1 admitted).1

-- Reduce long fixture scans one component at a time using the production scanner equations.
private theorem scan_rev_congr {length start : Nat}
    (left right rest : ConcreteSpec.Numbers) (same : left = right)
    (a : start + left.length + rest.length = length)
    (b : start + right.length + rest.length = length) :
    scan start left rest a = scan start right rest b := by
  subst right
  rfl

private theorem scan_replicate {length : Nat} (start : Nat) (rev rest : ConcreteSpec.Numbers)
    (byte count : Nat) (ordinary : byte ≠ 47)
    (total : start + rev.length + (List.replicate count byte ++ rest).length = length) :
    scan start rev (List.replicate count byte ++ rest) total =
      scan start (List.replicate count byte ++ rev) rest (by simp at total ⊢; omega) := by
  induction count generalizing rev with
  | zero => rfl
  | succ count ih =>
    change scan start rev (byte :: (List.replicate count byte ++ rest)) _ = _
    simp only [scan, ordinary, ↓reduceIte]
    rw [ih]
    apply scan_rev_congr
    simp [List.replicate_succ', List.append_assoc]

private theorem scan_repeat_slash {length : Nat} (start : Nat) (rest : ConcreteSpec.Numbers)
    (byte count : Nat) (ordinary : byte ≠ 47)
    (total : start + [].length + (List.replicate count byte ++ 47 :: rest).length = length) :
    scan start [] (List.replicate count byte ++ 47 :: rest) total =
      ⟨start, List.replicate count byte, by simp at total ⊢; omega⟩ ::
        scan (start + count + 1) [] rest (by simp at total ⊢; omega) := by
  rw [scan_replicate start [] _ byte count ordinary]
  simp [scan, List.reverse_replicate]

private theorem scan_repeat_end {length : Nat} (start byte count : Nat) (ordinary : byte ≠ 47)
    (total : start + [].length + (List.replicate count byte ++ []).length = length) :
    scan start [] (List.replicate count byte ++ []) total =
      [⟨start, List.replicate count byte, by simp at total ⊢; omega⟩] := by
  rw [scan_replicate start [] [] byte count ordinary]
  simp [scan, List.reverse_replicate]

private theorem pathCapRefusedInput_length : pathCapRefusedInput.length = 201 := by
  simp only [pathCapRefusedInput, SourceText.length, List.length_append, List.length_replicate,
    List.length_cons, List.length_nil]

private theorem scan_rest_congr {length start : Nat} (rev left right : ConcreteSpec.Numbers)
    (same : left = right) (a : start + rev.length + left.length = length)
    (b : start + rev.length + right.length = length) :
    scan start rev left a = scan start rev right b := by
  subst right
  rfl

private theorem pathCapRefusedInput_components :
    components pathCapRefusedInput =
      [⟨0, List.replicate 98 65, by simp [pathCapRefusedInput_length]⟩,
       ⟨99, List.replicate 100 66, by simp [pathCapRefusedInput_length]⟩,
       ⟨200, [67], by simp [pathCapRefusedInput_length]⟩] := by
  unfold components
  calc
    _ = scan 0 [] (List.replicate 98 65 ++ 47 :: (List.replicate 100 66 ++ [47, 67]))
          (by simp only [pathCapRefusedInput_length, List.length_append, List.length_cons,
                List.length_replicate, List.length_nil]) := by
      apply scan_rest_congr
      simp only [pathCapRefusedInput, List.append_assoc, List.cons_append, List.nil_append]
    _ = _ := by
      simp only [scan_repeat_slash 0 _ 65 98 (by decide),
        scan_repeat_slash 99 [67] 66 100 (by decide), scan]
      rfl

private theorem pathCapAcceptedInput_length : pathCapAcceptedInput.length = 200 := by
  simp only [pathCapAcceptedInput, SourceText.length, List.length_append, List.length_replicate,
    List.length_cons, List.length_nil]

private theorem pathCapAcceptedInput_components :
    components pathCapAcceptedInput =
      [⟨0, List.replicate 99 65, by simp [pathCapAcceptedInput_length]⟩,
       ⟨100, List.replicate 100 66, by simp [pathCapAcceptedInput_length]⟩] := by
  unfold components
  calc
    _ = scan 0 [] (List.replicate 99 65 ++ 47 :: List.replicate 100 66)
          (by simp only [pathCapAcceptedInput_length, List.length_append, List.length_cons,
                List.length_replicate, List.length_nil]) := by
      apply scan_rest_congr
      simp only [pathCapAcceptedInput, List.append_assoc, List.cons_append, List.nil_append]
    _ = _ := by
      simp only [scan_repeat_slash 0 _ 65 99 (by decide)]
      have last := scan_repeat_end (length := pathCapAcceptedInput.length) 100 66 100 (by decide)
        (by simp [pathCapAcceptedInput_length])
      simpa only [List.append_nil] using congrArg (List.cons ⟨0, List.replicate 99 65,
        by simp [pathCapAcceptedInput_length]⟩) last

-- Reduce the reference offenses component by component on long inputs.
private theorem pathCapAcceptedInput_otherRules : OtherRules .pathCap pathCapAcceptedInput := by
  have pieces : Reference.pieces pathCapAcceptedInput.bytes =
      [(0, List.replicate 99 65), (100, List.replicate 100 66)] := by
    rw [← components_reference, pathCapAcceptedInput_components]
    rfl
  intro reason _ other
  cases reason <;> try exact False.elim (other rfl)
  all_goals
    unfold Permitted
    simp only [Reference.offsets]
    try rw [pieces]
    cbv

private theorem pathCapRefusedInput_otherRules : OtherRules .pathCap pathCapRefusedInput := by
  have pieces : Reference.pieces pathCapRefusedInput.bytes =
      [(0, List.replicate 98 65), (99, List.replicate 100 66), (200, [67])] := by
    rw [← components_reference, pathCapRefusedInput_components]
    rfl
  intro reason _ other
  cases reason <;> try exact False.elim (other rfl)
  all_goals
    unfold Permitted
    simp only [Reference.offsets]
    try rw [pieces]
    cbv

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`, case `ConcreteSpec.Core.SourcePath.Registry.pathCapAcceptedInput`). -/
theorem pathCapAdmitted : ∃ path, parse pathCapAcceptedInput = .ok path :=
  (admission_pathCap_iff _).2 ⟨pathCapAccepted, pathCapAcceptedInput_otherRules⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.pathCapRefusedInput`). -/
theorem pathCapReported :
    reportedLocation (parse pathCapRefusedInput) =
      some (.pathCap, 0, 201, 200, List.replicate 98 65 ++ [47] ++ List.replicate 100 66 ++ [47, 67]) := by
  let witness : Violation pathCapRefusedInput.length :=
    ⟨.pathCap, ⟨0, 201, 200, by decide, by decide, by simp [pathCapRefusedInput_length]⟩⟩
  have hit : firstViolation pathCapRefusedInput = some witness := by
    unfold firstViolation selectViolation candidatesWith
    rw [pathCapRefusedInput_components]
    cbv
  rw [parse_of_hit _ _ hit]
  change some (Reason.pathCap, 0, 201, 200, pathCapRefusedInput.bytes.take 201) =
    some (Reason.pathCap, 0, 201, 200, pathCapRefusedInput.bytes)
  rw [← pathCapRefusedInput_length]
  simp [SourceText.length]

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`, case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`). -/
theorem disallowedAdmitted : ∃ path, parse mixedCaseInput = .ok path :=
  (admission_disallowed_iff _).2 ⟨disallowedAccepted, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`, case `ConcreteSpec.Core.SourcePath.Registry.disallowedRefusedInput`). -/
theorem disallowedRejected : ¬∃ path, parse disallowedRefusedInput = .ok path :=
  fun admitted => disallowedRefused ((admission_disallowed_iff _).1 admitted).1

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`, case `ConcreteSpec.Core.SourcePath.Registry.disallowedRefusedInput`). -/
theorem disallowedReported :
    reportedLocation (parse disallowedRefusedInput) = some (.disallowed, 2, 5, 3, [66, 58, 67]) := by decide

/-- Case: a noninitial 101-byte component starts at byte 4. -/
def noninitialCap : SourceText := SourceText.ofBytes ([100, 105, 114, 47] ++ List.replicate 101 65)

/-- Case: the whole-path cap is reached at byte 200 and the third component cap at 222. -/
def competingCaps : SourceText :=
  ⟨List.replicate 60 65 ++ [47] ++ List.replicate 60 66 ++ [47] ++ List.replicate 101 67, by
    intro byte member
    simp only [List.mem_append, List.mem_replicate, List.mem_cons, List.not_mem_nil, or_false] at member
    omega⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`,
case `ConcreteSpec.Core.SourcePath.LocalControls.noninitialCap`).
Component-cap coordinates are absolute byte offsets. -/
theorem noninitialCapReported :
    reportedLocation (parse noninitialCap) = some (.componentCap, 4, 105, 104, List.replicate 101 65) :=
  of_decide_eq_true (by cbv)

private theorem competingCaps_length : competingCaps.length = 223 := by
  simp only [competingCaps, SourceText.length, List.length_append, List.length_replicate,
    List.length_cons, List.length_nil]

private theorem competingCaps_components :
    components competingCaps =
      [⟨0, List.replicate 60 65, by simp [competingCaps_length]⟩,
       ⟨61, List.replicate 60 66, by simp [competingCaps_length]⟩,
       ⟨122, List.replicate 101 67, by simp [competingCaps_length]⟩] := by
  unfold components
  calc
    _ = scan 0 [] (List.replicate 60 65 ++ 47 ::
          (List.replicate 60 66 ++ 47 :: (List.replicate 101 67 ++ [])))
          (by simp only [competingCaps_length, List.length_append, List.length_cons,
                List.length_replicate, List.length_nil]) := by
      apply scan_rest_congr
      simp only [competingCaps, List.append_assoc, List.cons_append, List.nil_append, List.append_nil]
    _ = _ := by
      simp only [scan_repeat_slash 0 _ 65 60 (by decide),
        scan_repeat_slash 61 _ 66 60 (by decide), scan_repeat_end 122 67 101 (by decide)]

/-- Instance: an earlier byte beats a higher-priority check at a later byte. -/
theorem competingCapsReported : reported (parse competingCaps) = some (.pathCap, 200) := by
  let witness : Violation competingCaps.length :=
    ⟨.pathCap, ⟨0, 223, 200, by decide, by decide, by simp [competingCaps_length]⟩⟩
  have hit : firstViolation competingCaps = some witness := by
    unfold firstViolation selectViolation candidatesWith
    rw [competingCaps_components]
    cbv
  rw [parse_of_hit _ _ hit]
  rfl

/-- Instance: each of the lower control bytes is reported at the first offending position. -/
theorem controlBytesReported :
    reported (parse (SourceText.ofBytes [65, 0, 9])) = some (.control, 1) ∧
    reported (parse (SourceText.ofBytes [65, 9])) = some (.control, 1) := by decide

/-- Instance: a 99-byte component and a 199-byte path both remain admissible. -/
theorem belowCapsAdmitted :
    RawOk (SourceText.ofBytes (List.replicate 99 65)) ∧
    RawOk (SourceText.ofBytes (List.replicate 98 65 ++ [47] ++ List.replicate 100 66)) := of_decide_eq_true (by cbv)

/-- Mutant: fold ASCII names while rendering them. -/
def renderFolded (path : SourcePath) : ConcreteSpec.Name :=
  ⟨String.ofList (path.raw.bytes.map fun b => Char.ofNat (foldAscii b))⟩

/-- Case: a valid mixed-case path. -/
def mixedCasePath : SourcePath := ⟨mixedCaseInput, by decide⟩

/-- Instance (law `ConcreteSpec.Core.SourcePath.render_exact`, case `ConcreteSpec.Core.SourcePath.LocalControls.mixedCasePath`). -/
theorem mixedCaseRendered :
    SourceText.ofBytes mixedCasePath.render.text.toByteArray.data.toList = mixedCasePath.renderBytes :=
  render_exact mixedCasePath

/-- Planted (law `ConcreteSpec.Core.SourcePath.render_exact`,
case `ConcreteSpec.Core.SourcePath.LocalControls.mixedCasePath`,
subject `ConcreteSpec.Core.SourcePath.render`, mutant `ConcreteSpec.Core.SourcePath.LocalControls.renderFolded`). -/
theorem foldedRenderingChangesIdentity :
    SourceText.ofBytes (renderFolded mixedCasePath).text.toByteArray.data.toList ≠ mixedCasePath.renderBytes := by
  decide

/-- Mutant: select a reason by priority before looking at byte offset. -/
def priorityFirst (raw : SourceText) : Option (Violation raw.length) :=
  selectViolation localPolicy raw fun v => Reason.inOrder.idxOf v.reason

/-- Case: the early backslash should precede the later non-ASCII byte. -/
def earlyBackslash : SourceText := SourceText.ofBytes [65, 92, 66, 128]

/-- Instance: the first offending byte wins even when a later reason has higher priority. -/
theorem earlyBackslashReported : reported (parse earlyBackslash) = some (.backslash, 1) := by decide

/-- Observe the exact admitted bytes without projecting the representation's proof. -/
def admittedBytes (result : Except Refusal SourcePath) : Option SourceText :=
  match result with
  | .ok path => some path.renderBytes
  | .error _ => none

/-- Law (subject `ConcreteSpec.Core.SourcePath.parse`).
The parser's admission observation returns the original bytes exactly for valid input. -/
theorem admittedBytes_iff (raw : SourceText) : admittedBytes (parse raw) = some raw ↔ Reference.Valid raw := by
  cases parsed : parse raw with
  | ok path =>
    have exactBytes := (parse_iff raw path).1 parsed
    simp [admittedBytes, exactBytes.1, exactBytes.2]
  | error refusal =>
    simp [admittedBytes, (parse_refusal_iff raw).1 ⟨refusal, parsed⟩]

/-- A mutation uses the parser's decision before the successful branch packages its proof.
The production specialization is equivalent to observing an actual successful parse. -/
def admitsWith (policy : ProbePolicy) (raw : SourceText) : Prop :=
  selectViolation policy raw Violation.key = none

/-- Representation lemma for substituting a probe policy in an admission counterexample. -/
theorem admitsWith_local_iff (raw : SourceText) :
    admitsWith localPolicy raw ↔ ∃ path, parse raw = .ok path :=
  (noViolation_iff raw).trans ((rawOk_reference_iff raw).trans (admitted_iff raw).symm)

private theorem rejectsValid {policy : ProbePolicy} {raw : SourceText} {reason : Reason}
    (valid : Reference.Valid raw) (rejected : ¬ admitsWith policy raw) :
    ¬ (admitsWith policy raw ↔ Permitted reason raw ∧ OtherRules reason raw) :=
  fun law => rejected (law.mpr ⟨valid reason (reference_reason reason), fun rule member _ => valid rule member⟩)

private theorem admitsInvalid {policy : ProbePolicy} {raw : SourceText} {reason : Reason}
    (_other : OtherRules reason raw) (invalid : ¬ Permitted reason raw)
    (admitted : admitsWith policy raw) :
    ¬ (admitsWith policy raw ↔ Permitted reason raw ∧ OtherRules reason raw) :=
  fun law => invalid (law.mp admitted).1

/-- Case: valid input at a tightened probe boundary. -/
def oneByteInput : SourceText := SourceText.ofBytes [65]

/-- Mutant: refuse a one-byte path as empty. -/
def tightEmpty : ProbePolicy :=
  { localPolicy with empty := fun bytes => bytes.length ≤ 1 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_empty_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.oneByteInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightEmpty`). -/
theorem emptyRejectsAllowed :
    ¬ (admitsWith tightEmpty oneByteInput ↔
      Nonempty oneByteInput ∧ OtherRules .empty oneByteInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: delete the empty-path probe. -/
def withoutEmpty : ProbePolicy :=
  { localPolicy with empty := fun _ => false }

/-- Mutant: treat an initial A as a root marker. -/
def tightAbsolute : ProbePolicy :=
  { localPolicy with absolute := fun bytes => localPolicy.absolute bytes || bytes.head? == some 65 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_absolute_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.oneByteInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightAbsolute`). -/
theorem absoluteRejectsAllowed :
    ¬ (admitsWith tightAbsolute oneByteInput ↔
      Relative oneByteInput ∧ OtherRules .absolute oneByteInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: delete the absolute-path probe. -/
def withoutAbsolute : ProbePolicy :=
  { localPolicy with absolute := fun _ => false }

/-- Case: valid input at a tightened probe boundary. -/
def digitAfterInitialLetter : SourceText := SourceText.ofBytes [67, 49]

/-- Mutant: treat a 1 after an initial ASCII letter as a drive separator. -/
def tightDrive : ProbePolicy :=
  { localPolicy with drive := fun letter next => asciiLetter letter && (next == 58 || next == 49) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_drive_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.digitAfterInitialLetter`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightDrive`). -/
theorem driveRejectsAllowed :
    ¬ (admitsWith tightDrive digitAfterInitialLetter ↔
      NoDrivePrefix digitAfterInitialLetter ∧ OtherRules .drive digitAfterInitialLetter) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: delete the drive probe. -/
def withoutDrive : ProbePolicy :=
  { localPolicy with drive := fun _ _ => false }

/-- Case: valid input at a tightened probe boundary. -/
def ordinaryTwoByteComponent : SourceText := SourceText.ofBytes [65, 47, 65, 65]

/-- Mutant: treat all two-byte components as traversal. -/
def tightTraversal : ProbePolicy :=
  { localPolicy with traversal := fun bytes => bytes.length == 2 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.ordinaryTwoByteComponent`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightTraversal`). -/
theorem traversalRejectsAllowed :
    ¬ (admitsWith tightTraversal ordinaryTwoByteComponent ↔
      NoTraversal ordinaryTwoByteComponent ∧ OtherRules .traversal ordinaryTwoByteComponent) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for three dots instead of two. -/
def looseTraversal : ProbePolicy :=
  { localPolicy with traversal := (· == [46, 46, 46]) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_traversal_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.traversalRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseTraversal`). -/
theorem traversalAllowsRefused :
    ¬ (admitsWith looseTraversal traversalRefusedInput ↔
      NoTraversal traversalRefusedInput ∧ OtherRules .traversal traversalRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) traversalRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def ordinaryOneByteComponent : SourceText := SourceText.ofBytes [65, 47, 66]

/-- Mutant: treat every one-byte component as a single-dot component. -/
def tightSingleDot : ProbePolicy :=
  { localPolicy with singleDot := fun bytes => bytes.length == 1 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.ordinaryOneByteComponent`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightSingleDot`). -/
theorem singleDotRejectsAllowed :
    ¬ (admitsWith tightSingleDot ordinaryOneByteComponent ↔
      NoSingleDot ordinaryOneByteComponent ∧ OtherRules .singleDot ordinaryOneByteComponent) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for two dots instead of one. -/
def looseSingleDot : ProbePolicy :=
  { localPolicy with singleDot := (· == [46, 46]) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_singleDot_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.singleDotRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseSingleDot`). -/
theorem singleDotAllowsRefused :
    ¬ (admitsWith looseSingleDot singleDotRefusedInput ↔
      NoSingleDot singleDotRefusedInput ∧ OtherRules .singleDot singleDotRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) singleDotRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: treat one-byte components as empty. -/
def tightEmptyComponent : ProbePolicy :=
  { localPolicy with emptyComponent := fun bytes => bytes.length ≤ 1 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.ordinaryOneByteComponent`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightEmptyComponent`). -/
theorem emptyComponentRejectsAllowed :
    ¬ (admitsWith tightEmptyComponent ordinaryOneByteComponent ↔
      NonemptyComponents ordinaryOneByteComponent ∧ OtherRules .emptyComponent ordinaryOneByteComponent) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: allow empty components. -/
def looseEmptyComponent : ProbePolicy :=
  { localPolicy with emptyComponent := fun _ => false }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_emptyComponent_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyComponentRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseEmptyComponent`). -/
theorem emptyComponentAllowsRefused :
    ¬ (admitsWith looseEmptyComponent emptyComponentRefusedInput ↔
      NonemptyComponents emptyComponentRefusedInput ∧ OtherRules .emptyComponent emptyComponentRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) emptyComponentRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def highestFilenameLetter : SourceText := SourceText.ofBytes [65, 122]

/-- Mutant: move the non-ASCII cutoff to 122. -/
def tightNonAscii : ProbePolicy :=
  { localPolicy with nonAscii := (122 ≤ ·) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.highestFilenameLetter`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightNonAscii`). -/
theorem nonAsciiRejectsAllowed :
    ¬ (admitsWith tightNonAscii highestFilenameLetter ↔
      Ascii highestFilenameLetter ∧ OtherRules .nonAscii highestFilenameLetter) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: allow the first non-ASCII byte. -/
def looseNonAscii : ProbePolicy :=
  { localPolicy with nonAscii := (129 ≤ ·) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.nonAsciiRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseNonAscii`). -/
theorem nonAsciiAllowsRefused :
    ¬ (admitsWith looseNonAscii nonAsciiRefusedInput ↔
      Ascii nonAsciiRefusedInput ∧ OtherRules .nonAscii nonAsciiRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) nonAsciiRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def underscoreInput : SourceText := SourceText.ofBytes [65, 95, 66]

/-- Mutant: treat underscore as another backslash. -/
def tightBackslash : ProbePolicy :=
  { localPolicy with backslash := fun byte => byte == 92 || byte == 95 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.underscoreInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightBackslash`). -/
theorem backslashRejectsAllowed :
    ¬ (admitsWith tightBackslash underscoreInput ↔
      SlashSeparator underscoreInput ∧ OtherRules .backslash underscoreInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: check for a closing bracket in place of backslash. -/
def looseBackslash : ProbePolicy :=
  { localPolicy with backslash := (· == 93) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_backslash_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.backslashRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseBackslash`). -/
theorem backslashAllowsRefused :
    ¬ (admitsWith looseBackslash backslashRefusedInput ↔
      SlashSeparator backslashRefusedInput ∧ OtherRules .backslash backslashRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) backslashRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def hyphenInput : SourceText := SourceText.ofBytes [65, 45]

/-- Mutant: classify hyphen as a control byte. -/
def tightControl : ProbePolicy :=
  { localPolicy with control := fun byte => localPolicy.control byte || byte == 45 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_control_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.hyphenInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightControl`). -/
theorem controlRejectsAllowed :
    ¬ (admitsWith tightControl hyphenInput ↔
      NoControls hyphenInput ∧ OtherRules .control hyphenInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: omit DEL from the control bytes. -/
def looseControl : ProbePolicy :=
  { localPolicy with control := (· < 32) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_control_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.controlRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseControl`). -/
theorem controlAllowsRefused :
    ¬ (admitsWith looseControl controlRefusedInput ↔
      NoControls controlRefusedInput ∧ OtherRules .control controlRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) controlRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: classify hyphen as a space. -/
def tightSpace : ProbePolicy :=
  { localPolicy with space := fun byte => byte == 32 || byte == 45 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_space_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.hyphenInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightSpace`). -/
theorem spaceRejectsAllowed :
    ¬ (admitsWith tightSpace hyphenInput ↔
      NoSpaces hyphenInput ∧ OtherRules .space hyphenInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for byte 160 instead of ASCII space. -/
def looseSpace : ProbePolicy :=
  { localPolicy with space := (· == 160) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_space_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.spaceRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseSpace`). -/
theorem spaceAllowsRefused :
    ¬ (admitsWith looseSpace spaceRefusedInput ↔
      NoSpaces spaceRefusedInput ∧ OtherRules .space spaceRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) spaceRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def leadingUnderscore : SourceText := SourceText.ofBytes [95, 65]

/-- Mutant: reject an initial underscore as a dot. -/
def tightLeadingDot : ProbePolicy :=
  { localPolicy with leadingDot := fun byte => byte == 46 || byte == 95 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.leadingUnderscore`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightLeadingDot`). -/
theorem leadingDotRejectsAllowed :
    ¬ (admitsWith tightLeadingDot leadingUnderscore ↔
      NoLeadingDot leadingUnderscore ∧ OtherRules .leadingDot leadingUnderscore) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for an initial underscore in place of a dot. -/
def looseLeadingDot : ProbePolicy :=
  { localPolicy with leadingDot := (· == 95) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_leadingDot_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.leadingDotRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseLeadingDot`). -/
theorem leadingDotAllowsRefused :
    ¬ (admitsWith looseLeadingDot leadingDotRefusedInput ↔
      NoLeadingDot leadingDotRefusedInput ∧ OtherRules .leadingDot leadingDotRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) leadingDotRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Case: valid input at a tightened probe boundary. -/
def trailingUnderscore : SourceText := SourceText.ofBytes [65, 95]

/-- Mutant: reject a final underscore as a dot. -/
def tightTrailingDot : ProbePolicy :=
  { localPolicy with trailingDot := fun byte => byte == 46 || byte == 95 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.trailingUnderscore`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightTrailingDot`). -/
theorem trailingDotRejectsAllowed :
    ¬ (admitsWith tightTrailingDot trailingUnderscore ↔
      NoTrailingDot trailingUnderscore ∧ OtherRules .trailingDot trailingUnderscore) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: look for a final underscore in place of a dot. -/
def looseTrailingDot : ProbePolicy :=
  { localPolicy with trailingDot := (· == 95) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_trailingDot_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.trailingDotRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseTrailingDot`). -/
theorem trailingDotAllowsRefused :
    ¬ (admitsWith looseTrailingDot trailingDotRefusedInput ↔
      NoTrailingDot trailingDotRefusedInput ∧ OtherRules .trailingDot trailingDotRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) trailingDotRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: add COM0 and LPT0 to the reserved stems. -/
def tightReserved : ProbePolicy :=
  { localPolicy with reserved := reservedStemWith foldAscii 48 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.reservedAcceptedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightReserved`). -/
theorem reservedRejectsAllowed :
    ¬ (admitsWith tightReserved reservedAcceptedInput ↔
      NoReservedStem reservedAcceptedInput ∧ OtherRules .reserved reservedAcceptedInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: match reserved stems without ASCII case folding. -/
def looseReserved : ProbePolicy :=
  { localPolicy with reserved := reservedStemWith id 49 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_reserved_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.reservedRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseReserved`). -/
theorem reservedAllowsRefused :
    ¬ (admitsWith looseReserved reservedRefusedInput ↔
      NoReservedStem reservedRefusedInput ∧ OtherRules .reserved reservedRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) reservedRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: cap components at 99 bytes. -/
def tightComponentCap : ProbePolicy :=
  { localPolicy with componentCap := 99 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.componentCapAcceptedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightComponentCap`). -/
theorem componentCapRejectsAllowed :
    ¬ (admitsWith tightComponentCap componentCapAcceptedInput ↔
      ComponentByteCap componentCapAcceptedInput ∧ OtherRules .componentCap componentCapAcceptedInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: cap components at 101 bytes. -/
def looseComponentCap : ProbePolicy :=
  { localPolicy with componentCap := 101 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_componentCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.componentCapRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseComponentCap`). -/
theorem componentCapAllowsRefused :
    ¬ (admitsWith looseComponentCap componentCapRefusedInput ↔
      ComponentByteCap componentCapRefusedInput ∧ OtherRules .componentCap componentCapRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) componentCapRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: cap paths at 199 bytes. -/
def tightPathCap : ProbePolicy :=
  { localPolicy with pathCap := 199 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.pathCapAcceptedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightPathCap`). -/
theorem pathCapRejectsAllowed :
    ¬ (admitsWith tightPathCap pathCapAcceptedInput ↔
      PathByteCap pathCapAcceptedInput ∧ OtherRules .pathCap pathCapAcceptedInput) := by
  apply rejectsValid (reason := .pathCap)
  · intro reason member
    by_cases same : reason = .pathCap
    · exact same ▸ pathCapAccepted
    · exact pathCapAcceptedInput_otherRules reason member same
  · intro admitted
    let witness : Violation pathCapAcceptedInput.length :=
      ⟨.pathCap, ⟨0, 200, 199, by decide, by decide, by simp [pathCapAcceptedInput_length]⟩⟩
    have hit : selectViolation tightPathCap pathCapAcceptedInput Violation.key = some witness := by
      unfold selectViolation candidatesWith
      rw [pathCapAcceptedInput_components]
      cbv
    change selectViolation tightPathCap pathCapAcceptedInput Violation.key = none at admitted
    rw [admitted] at hit
    contradiction

/-- Mutant: cap paths at 201 bytes. -/
def loosePathCap : ProbePolicy :=
  { localPolicy with pathCap := 201 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_pathCap_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.pathCapRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.loosePathCap`). -/
theorem pathCapAllowsRefused :
    ¬ (admitsWith loosePathCap pathCapRefusedInput ↔
      PathByteCap pathCapRefusedInput ∧ OtherRules .pathCap pathCapRefusedInput) := by
  apply admitsInvalid pathCapRefusedInput_otherRules pathCapRefused
  unfold admitsWith selectViolation candidatesWith
  rw [pathCapRefusedInput_components]
  cbv

/-- Mutant: remove hyphen from the filename whitelist. -/
def tightDisallowed : ProbePolicy :=
  { localPolicy with disallowed := fun byte => localPolicy.disallowed byte || byte == 45 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.hyphenInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.tightDisallowed`). -/
theorem disallowedRejectsAllowed :
    ¬ (admitsWith tightDisallowed hyphenInput ↔
      FilenameCharacters hyphenInput ∧ OtherRules .disallowed hyphenInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; exact of_decide_eq_true (by cbv))

/-- Mutant: allow colons through the filename whitelist. -/
def looseDisallowed : ProbePolicy :=
  { localPolicy with disallowed := fun byte => localPolicy.disallowed byte && byte != 58 }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.disallowedRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.looseDisallowed`). -/
theorem disallowedAllowsRefused :
    ¬ (admitsWith looseDisallowed disallowedRefusedInput ↔
      FilenameCharacters disallowedRefusedInput ∧ OtherRules .disallowed disallowedRefusedInput) := by
  exact admitsInvalid (of_decide_eq_true (by cbv)) disallowedRefused (by unfold admitsWith; exact of_decide_eq_true (by cbv))


/-- The refusal-order obligation with a replacement diagnostic search. -/
def KeepsRefusalOrder (raw : SourceText) (selection : Option (Violation raw.length)) : Prop :=
  ∀ chosen, selection = some chosen →
    Reference.IsFirst raw chosen.reason chosen.location.offset

/-- Representation lemma for substituting the search used by the parser's refusal branch. -/
theorem keepsProductionRefusalOrder (raw : SourceText) :
    KeepsRefusalOrder raw (firstViolation raw) :=
  fun chosen found => parse_refusal_order raw ⟨raw, chosen⟩ (parse_of_hit raw chosen found)

/-- Mutant: preserve byte order but prefer later rules when offsets tie. -/
def reverseTieKey {length : Nat} (violation : Violation length) : Nat :=
  violation.location.offset * Reason.inOrder.length +
    (Reason.inOrder.length - 1 - Reason.inOrder.idxOf violation.reason)

private theorem losesRefusalOrder {raw : SourceText} {selection : Option (Violation raw.length)}
    (chosen : Violation raw.length) (found : selection = some chosen)
    (reason : Reason) (offset : Nat) (offense : offset ∈ Reference.offsets reason raw)
    (late : ¬ Reference.Precedes chosen.reason chosen.location.offset reason offset) :
    ¬ KeepsRefusalOrder raw selection :=
  fun order => late ((order chosen found).2 reason offset offense)

/-- Planted (law `ConcreteSpec.Core.SourcePath.firstViolation_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.earlyBackslash`,
subject `ConcreteSpec.Core.SourcePath.firstViolation`, mutant `ConcreteSpec.Core.SourcePath.LocalControls.priorityFirst`). -/
theorem priorityFirstMissesEarlyByte :
    ¬ KeepsRefusalOrder earlyBackslash (priorityFirst earlyBackslash) := by
  exact losesRefusalOrder ⟨.nonAscii, ⟨0, 4, 3, by decide, by decide, by decide⟩⟩
    rfl .backslash 1 (by decide) (by unfold Reference.Precedes; decide)

/-- Mutant: give empty components precedence over empty paths on a tie. -/
def swappedEmptyPriority : List Reason :=
  Reason.inOrder.map fun
    | .empty => .emptyComponent
    | .emptyComponent => .empty
    | reason => reason

def swappedEmptyKey {length : Nat} (violation : Violation length) : Nat :=
  violation.location.offset * swappedEmptyPriority.length +
    swappedEmptyPriority.idxOf violation.reason

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.swappedEmptyKey`). -/
theorem swappedEmptyLosesPriority :
    ¬ KeepsRefusalOrder emptyRefusedInput
      (selectViolation localPolicy emptyRefusedInput swappedEmptyKey) := by
  exact losesRefusalOrder ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩
    rfl .empty 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Case: two offending bytes in one component, at whole-path offsets 3 and 5. -/
def repeatedBackslash : SourceText := SourceText.ofBytes [65, 47, 66, 92, 67, 92]

/-- Skip the first matching byte and look for another match in the same component. -/
def skipFirstMatch (bytes : ConcreteSpec.Numbers) (test : Nat → Bool) :
    Option (Fin bytes.length) := do
  let first ← bytes.findFinIdx? test
  let start := first.val + 1
  let next ← (bytes.drop start).findFinIdx? test
  pure ⟨start + next.val, by
    have shorter : next.val < bytes.length - start :=
      Nat.lt_of_lt_of_eq next.isLt (List.length_drop (i := start))
    omega⟩

/-- Mutant: select the second match of the backslash probe. -/
def skipEarlierBackslash : ProbePolicy :=
  { localPolicy with findByte := fun reason bytes test =>
      match reason with
      | .backslash => skipFirstMatch bytes test
      | _ => bytes.findFinIdx? test }

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.repeatedBackslash`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.skipEarlierBackslash`). -/
theorem skippedBackslashLosesEarliest :
    ¬ KeepsRefusalOrder repeatedBackslash
      (selectViolation skipEarlierBackslash repeatedBackslash Violation.key) := by
  exact losesRefusalOrder ⟨.backslash, ⟨2, 6, 5, by decide, by decide, by decide⟩⟩
    rfl .backslash 3 (by decide) (by unfold Reference.Precedes; decide)

/-- Case: a drive prefix with a following component. -/
def driveWithSuffix : SourceText := SourceText.ofBytes [67, 58, 47, 65]

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`,
case `ConcreteSpec.Core.SourcePath.LocalControls.driveWithSuffix`). -/
theorem driveWithSuffixReported :
    reportedLocation (parse driveWithSuffix) = some (.drive, 0, 2, 1, [67, 58]) := by decide

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.withoutEmpty`). -/
theorem deleteEmptyLosesPriority :
    ¬ KeepsRefusalOrder emptyRefusedInput (selectViolation withoutEmpty emptyRefusedInput Violation.key) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .empty 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.emptyRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.reverseTieKey`). -/
theorem reverseEmptyLosesPriority :
    ¬ KeepsRefusalOrder emptyRefusedInput (selectViolation localPolicy emptyRefusedInput reverseTieKey) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .empty 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.withoutAbsolute`). -/
theorem deleteAbsoluteLosesPriority :
    ¬ KeepsRefusalOrder absoluteRefusedInput (selectViolation withoutAbsolute absoluteRefusedInput Violation.key) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .absolute 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.Registry.absoluteRefusedInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.reverseTieKey`). -/
theorem reverseAbsoluteLosesPriority :
    ¬ KeepsRefusalOrder absoluteRefusedInput (selectViolation localPolicy absoluteRefusedInput reverseTieKey) := by
  exact losesRefusalOrder
    ⟨.emptyComponent, ⟨0, 0, 0, by decide, by decide, by decide⟩⟩ rfl .absolute 0 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.driveWithSuffix`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.withoutDrive`). -/
theorem deleteDriveLosesPriority :
    ¬ KeepsRefusalOrder driveWithSuffix (selectViolation withoutDrive driveWithSuffix Violation.key) := by
  exact losesRefusalOrder
    ⟨.disallowed, ⟨0, 2, 1, by decide, by decide, by decide⟩⟩ rfl .drive 1 (by decide) (by unfold Reference.Precedes; decide)

/-- Planted (law `ConcreteSpec.Core.SourcePath.parse_refusal_order`,
case `ConcreteSpec.Core.SourcePath.LocalControls.driveWithSuffix`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.reverseTieKey`). -/
theorem reverseDriveLosesPriority :
    ¬ KeepsRefusalOrder driveWithSuffix (selectViolation localPolicy driveWithSuffix reverseTieKey) := by
  exact losesRefusalOrder
    ⟨.disallowed, ⟨0, 2, 1, by decide, by decide, by decide⟩⟩ rfl .drive 1 (by decide) (by unfold Reference.Precedes; decide)


/-- Mutant: admit ASCII filenames only if they have no uppercase letters. -/
def lowercaseOnly : ProbePolicy :=
  { localPolicy with disallowed := fun byte => localPolicy.disallowed byte || (65 ≤ byte && byte ≤ 90) }

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_disallowed_iff`,
case `ConcreteSpec.Core.SourcePath.Registry.mixedCaseInput`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.lowercaseOnly`). -/
theorem lowercaseOnlyRejectsMixedCase :
    ¬ (admitsWith lowercaseOnly mixedCaseInput ↔
      FilenameCharacters mixedCaseInput ∧ OtherRules .disallowed mixedCaseInput) := by
  exact rejectsValid (of_decide_eq_true (by cbv)) (by unfold admitsWith; decide)

/-- Case: the exact UTF-8 bytes for Aé, including both bytes of é. -/
def utf8Input : SourceText := SourceText.ofBytes [65, 195, 169]

/-- Mutant: replace non-ASCII bytes with underscores before admission. -/
def replaceNonAscii (raw : SourceText) : Prop :=
  let replaced := SourceText.ofBytes (raw.toBytes.map fun byte => if byte.toNat < 128 then byte else 95)
  admitsWith localPolicy replaced

/-- Instance (law `ConcreteSpec.Core.SourcePath.parse_refusal_location`,
case `ConcreteSpec.Core.SourcePath.LocalControls.utf8Input`). -/
theorem utf8RefusalPreservesBytes :
    reportedLocation (parse utf8Input) = some (.nonAscii, 0, 3, 1, [65, 195, 169]) := by decide

/-- Planted (law `ConcreteSpec.Core.SourcePath.admission_nonAscii_iff`,
case `ConcreteSpec.Core.SourcePath.LocalControls.utf8Input`, subject `ConcreteSpec.Core.SourcePath.parse`,
mutant `ConcreteSpec.Core.SourcePath.LocalControls.replaceNonAscii`). -/
theorem replacementAdmitsNonAscii :
    ¬ (replaceNonAscii utf8Input ↔ Ascii utf8Input ∧ OtherRules .nonAscii utf8Input) := by
  have admitted : replaceNonAscii utf8Input := by rfl
  have invalid : ¬ Permitted .nonAscii utf8Input := by decide
  exact fun law => invalid (law.mp admitted).1

end LocalControls
end SourcePath

/-- Instance (law `ConcreteSpec.Core.Span.locationOk_iff`, case `ConcreteSpec.Core.tabSpan`).
the span after a tab matches the original source's byte coordinates. -/
theorem tabSpan_location : tabSpan.LocationOk tabSource :=
  (Span.locationOk_iff _ _).2 (by decide)

/-- Instance (law `ConcreteSpec.Core.Span.locationOk_iff`, case `ConcreteSpec.Core.Vacuity.otherTabSource`).
the unchanged span fails for the source with a different line structure. -/
theorem tabSpan_otherSource : ¬tabSpan.LocationOk Vacuity.otherTabSource := by
  rw [Span.locationOk_iff]
  decide

/-- Instance (law `ConcreteSpec.Core.Span.locationOk_iff`, case `ConcreteSpec.Core.tabSpan`).
an incorrect column or an out-of-bounds end invalidates the span. -/
theorem tabSpan_badCoordinates :
    ¬({ tabSpan with col := 2, col_pos := by decide }).LocationOk tabSource ∧
    ¬({ tabSpan with endOffset := 4, range_ok := by decide }).LocationOk tabSource := by
  simp only [Span.locationOk_iff]
  decide

end ConcreteSpec.Core

namespace ConcreteSpec.Core.SourcePath.Project

namespace Controls

open ConcreteSpec.Core.SourcePath.Project.Registry (fileA fileB lowerDirectory upperDirectory)

def sameDirectory : SourcePath := ⟨SourceText.ofBytes [97, 47, 99, 46, 99, 111, 110], by decide⟩
def lowerFile : SourcePath := ⟨SourceText.ofBytes [97, 47, 98, 46, 99, 111, 110], by decide⟩

def directoryCase : List SourcePath := [lowerDirectory, upperDirectory]
def filenameCase : List SourcePath := [lowerDirectory, lowerFile]
def exactPrefix : List SourcePath := [fileA, lowerDirectory]
def sharedDirectory : List SourcePath := [lowerDirectory, sameDirectory]
def evidenceInput : List SourcePath := [fileA, lowerDirectory, fileB]

def ignoreDirectoryCase : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .caseCollision =>
      if left.raw.bytes.map foldAscii = right.raw.bytes.map foldAscii then some .caseCollision
      else none
    | other => other

def caseSensitive : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .caseCollision => none
    | other => other

def ignoreFileDirectory : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .fileDirectoryConflict => none
    | other => other

def rejectSharedDirectory : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right, (components left.raw).map Component.bytes,
        (components right.raw).map Component.bytes with
    | none, first :: _ :: _, second :: _ :: _ =>
      if first = second then some .caseCollision else none
    | other, _, _ => other

def wrongSecondOccurrence (paths : List SourcePath) : Except Refusal Unit :=
  (check paths).mapError fun refusal => { refusal with second := refusal.second + 1 }

def onlyFileBeforeDirectory : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .fileDirectoryConflict =>
      if left.raw.length < right.raw.length then some .fileDirectoryConflict else none
    | other => other

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.directoryCase`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.ignoreDirectoryCase`). -/
theorem ignoreDirectoryCase_violates_safety : ¬ Safety ignoreDirectoryCase := by
  intro law
  have admitted := law directoryCase (by decide)
  rw [Reference.Admitted, directoryCase, List.pairwise_cons] at admitted
  exact admitted.1 upperDirectory (by simp)
    .caseCollision ⟨1, by decide, by decide, by decide, by decide⟩

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.filenameCase`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.caseSensitive`). -/
theorem caseSensitive_violates_safety : ¬ Safety caseSensitive := by
  intro law
  have admitted := law filenameCase (by decide)
  rw [Reference.Admitted, filenameCase, List.pairwise_cons] at admitted
  exact admitted.1 lowerFile (by simp)
    .caseCollision ⟨2, by decide, by decide, by decide, by decide⟩

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.exactPrefix`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.ignoreFileDirectory`). -/
theorem ignoreFileDirectory_violates_safety : ¬ Safety ignoreFileDirectory := by
  intro law
  have admitted := law exactPrefix (by decide)
  rw [Reference.Admitted, exactPrefix, List.pairwise_cons] at admitted
  exact admitted.1 lowerDirectory (by simp) .fileDirectoryConflict
    ⟨by decide, Or.inl ⟨[[66, 46, 99, 111, 110]], rfl⟩⟩

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_progress`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.sharedDirectory`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.rejectSharedDirectory`). -/
theorem rejectSharedDirectory_violates_progress : ¬ Progress rejectSharedDirectory := by
  intro law
  have admitted : Reference.Admitted sharedDirectory := by
    unfold Reference.Admitted sharedDirectory
    simp only [List.pairwise_cons, List.mem_singleton, List.not_mem_nil, false_implies,
      implies_true, List.Pairwise.nil, and_true]
    intro path eq reason conflict
    subst path
    have found := (classify_correct _ _ _).mpr conflict
    change none = some reason at found
    contradiction
  exact (by decide : rejectSharedDirectory sharedDirectory ≠ .ok ()) (law _ admitted)

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_refusal_evidence`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.evidenceInput`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.wrongSecondOccurrence`). -/
theorem wrongSecondOccurrence_violates_refusal_evidence : ¬ RefusalEvidence wrongSecondOccurrence := by
  intro law
  obtain ⟨_, _, _, conflict⟩ := law evidenceInput ⟨.fileDirectoryConflict, 0, 2⟩ (by decide)
  have found := (classify_correct _ _ _).mpr conflict
  change none = some Reason.fileDirectoryConflict at found
  contradiction

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_order_independent`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.exactPrefix`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.onlyFileBeforeDirectory`). -/
theorem onlyFileBeforeDirectory_violates_order_independence :
    ¬ OrderIndependent onlyFileBeforeDirectory := by
  intro law
  have perm : exactPrefix.Perm [lowerDirectory, fileA] := List.Perm.swap ..
  exact (by decide : onlyFileBeforeDirectory exactPrefix ≠ .ok ()) ((law _ _ perm).mpr (by decide))


def beforeLocalRefusal : List SourceText := [lowerDirectory.raw, upperDirectory.raw]

def prematureProjectCheck (inputs : List SourceText) :
    Except (SourcePath.Refusal ⊕ Refusal) (List SourcePath) :=
  let admitted := inputs.filterMap fun raw => (SourcePath.parse raw).toOption
  match check admitted with
  | .error refusal => .error (.inr refusal)
  | .ok () => parse inputs

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.parse_local_first`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.beforeLocalRefusal`, subject `ConcreteSpec.Core.SourcePath.Project.parse`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.prematureProjectCheck`). -/
theorem prematureProjectCheck_violates_local_first : ¬ LocalFirst prematureProjectCheck := by
  intro law
  let raw := SourceText.ofBytes []
  let refusal : SourcePath.Refusal := ⟨raw, ⟨.empty, wholeLocation raw 0 (by decide)⟩⟩
  have refused : SourcePath.parse raw = .error refusal := rfl
  have earlier : ∀ input ∈ beforeLocalRefusal, ∃ path, SourcePath.parse input = .ok path := by
    intro input member
    simp only [beforeLocalRefusal, List.mem_cons, List.not_mem_nil, or_false] at member
    rcases member with rfl | rfl
    · exact ⟨lowerDirectory, rfl⟩
    · exact ⟨upperDirectory, rfl⟩
  have project : prematureProjectCheck (beforeLocalRefusal ++ [raw]) =
      .error (.inr ⟨.caseCollision, 0, 1⟩) := rfl
  have localResult := law beforeLocalRefusal [] raw refusal earlier refused
  rw [project] at localResult
  injection localResult with incompatible
  cases incompatible

/-- Two occurrences of the same admitted path. -/
def duplicateOccurrences : List SourcePath := [fileA, fileA]

def ignoreDuplicate : List SourcePath → Except Refusal Unit :=
  checkWith fun left right =>
    match classify left right with
    | some .duplicate => none
    | other => other

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.check_safety`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.duplicateOccurrences`, subject `ConcreteSpec.Core.SourcePath.Project.check`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.ignoreDuplicate`). -/
theorem ignoreDuplicate_violates_safety : ¬ Safety ignoreDuplicate := by
  intro law
  have admitted := law duplicateOccurrences (by decide)
  rw [Reference.Admitted, duplicateOccurrences, List.pairwise_cons] at admitted
  exact admitted.1 fileA (by simp) .duplicate rfl

/-- The raw inputs corresponding to the two repeated occurrences. -/
def duplicateInputs : List SourceText := duplicateOccurrences.map SourcePath.raw

def skipProjectCheck (inputs : List SourceText) :
    Except (SourcePath.Refusal ⊕ Refusal) (List SourcePath) :=
  (parseLocal inputs).mapError Sum.inl

/-- Planted (law `ConcreteSpec.Core.SourcePath.Project.parse_checks_locally_parsed`,
case `ConcreteSpec.Core.SourcePath.Project.Controls.duplicateInputs`, subject `ConcreteSpec.Core.SourcePath.Project.parse`,
mutant `ConcreteSpec.Core.SourcePath.Project.Controls.skipProjectCheck`). -/
theorem skipProjectCheck_violates_checks_locally_parsed :
    ¬ ChecksLocallyParsed skipProjectCheck := by
  intro law
  have forwarded := (law duplicateInputs duplicateOccurrences rfl).2 ⟨.duplicate, 0, 1⟩ rfl
  have bypassed : skipProjectCheck duplicateInputs = .ok duplicateOccurrences := rfl
  rw [bypassed] at forwarded
  contradiction

end Controls

end ConcreteSpec.Core.SourcePath.Project

namespace ConcreteSpec.Core.SourcePathSpan

namespace Controls

/-- The original tab range with an end beyond the loaded file. -/
def pastEnd : Span := { tabSpan with endOffset := 4, range_ok := by decide }

/-- The tab span with only its stored line changed. -/
def wrongLine : Span := { tabSpan with line := 2, line_pos := by decide }

/-- The tab span with only its stored column changed. -/
def wrongColumn : Span := { tabSpan with col := 2, col_pos := by decide }

/-- Mutant that projects ordered ranges without checking the end bound. -/
def projectPastEnd (project : Project) (path : SourcePath) (start stop : Nat) : Option Span := do
  let id ← project.findId path
  let file ← project.fileAt id
  if ordered : start ≤ stop then
    if bounded : start ≤ file.length then some (fileSpan id file start stop ordered bounded)
    else none
  else none

/-- Mutant that rejects ranges beginning after the first byte. -/
def projectAtStartOnly (project : Project) (path : SourcePath) (start stop : Nat) : Option (ScopedSpan project) :=
  if start = 0 then projectSpan project path start stop else none

/-- Mutant that skips only the end bound. -/
def bindPastEnd (file : File) (span : Span) : Bool :=
  decide (span.line = file.source.lineAt span.startOffset ∧
    span.col = file.source.columnAt span.startOffset)

/-- Mutant that skips only the line match. -/
def bindIgnoringLine (file : File) (span : Span) : Bool :=
  decide (span.endOffset ≤ file.source.length ∧ span.col = file.source.columnAt span.startOffset)

/-- Mutant that skips only the column match. -/
def bindIgnoringColumn (file : File) (span : Span) : Bool :=
  decide (span.endOffset ≤ file.source.length ∧ span.line = file.source.lineAt span.startOffset)

/-- Mutant that rejects ranges ending exactly at EOF. -/
def bindBeforeEof (origin project : Project) (span : ScopedSpan origin) : Option File := do
  let file ← bindSource origin project span
  if span.endOffset < file.source.length then some file else none

/-- Planted (law `ConcreteSpec.Core.SourcePathSpan.projectLocation_correct`,
case `ConcreteSpec.Core.SourcePathSpan.Controls.pastEnd`, subject `ConcreteSpec.Core.SourcePathSpan.projectLocation`,
mutant `ConcreteSpec.Core.SourcePathSpan.Controls.projectPastEnd`). -/
theorem projectPastEnd_violates_correct : ¬ ProjectionLocationCorrect projectPastEnd := by
  intro law
  obtain ⟨file, member, _, bounds, _, _⟩ :=
    law tabProject mainSourcePath 2 4 pastEnd (by decide)
  have eq : file = File.load mainSourcePath tabSource := Option.some.inj member.symm
  subst file
  exact (by decide : ¬ pastEnd.endOffset ≤ tabSource.length) bounds

/-- Planted (law `ConcreteSpec.Core.SourcePathSpan.projectSpan_correct`,
case `ConcreteSpec.Core.tabSpan`, subject `ConcreteSpec.Core.SourcePathSpan.projectSpan`,
mutant `ConcreteSpec.Core.SourcePathSpan.Controls.projectAtStartOnly`). -/
theorem projectAtStartOnly_violates_correct : ¬ ProjectionCorrect projectAtStartOnly := by
  intro law
  have reference := (projectSpan_correct tabProject mainSourcePath 2 3 scopedTabSpan).mp projectTabSpan
  have result := (law tabProject mainSourcePath 2 3 scopedTabSpan).mpr reference
  contradiction

/-- Planted (law `ConcreteSpec.Core.SourcePathSpan.matchesLocation_correct`,
case `ConcreteSpec.Core.SourcePathSpan.Controls.pastEnd`, subject `ConcreteSpec.Core.SourcePathSpan.File.matchesLocation`,
mutant `ConcreteSpec.Core.SourcePathSpan.Controls.bindPastEnd`). -/
theorem bindPastEnd_violates_correct : ¬ LocationCheckCorrect bindPastEnd := by
  intro law
  exact (by decide : ¬ pastEnd.LocationOk tabSource)
    ((law (File.load mainSourcePath tabSource) pastEnd).mp (by decide))

/-- Planted (law `ConcreteSpec.Core.SourcePathSpan.matchesLocation_correct`,
case `ConcreteSpec.Core.SourcePathSpan.Controls.wrongLine`, subject `ConcreteSpec.Core.SourcePathSpan.File.matchesLocation`,
mutant `ConcreteSpec.Core.SourcePathSpan.Controls.bindIgnoringLine`). -/
theorem bindIgnoringLine_violates_correct : ¬ LocationCheckCorrect bindIgnoringLine := by
  intro law
  exact (by decide : ¬ wrongLine.LocationOk tabSource)
    ((law (File.load mainSourcePath tabSource) wrongLine).mp (by decide))

/-- Planted (law `ConcreteSpec.Core.SourcePathSpan.matchesLocation_correct`,
case `ConcreteSpec.Core.SourcePathSpan.Controls.wrongColumn`, subject `ConcreteSpec.Core.SourcePathSpan.File.matchesLocation`,
mutant `ConcreteSpec.Core.SourcePathSpan.Controls.bindIgnoringColumn`). -/
theorem bindIgnoringColumn_violates_correct : ¬ LocationCheckCorrect bindIgnoringColumn := by
  intro law
  exact (by decide : ¬ wrongColumn.LocationOk tabSource)
    ((law (File.load mainSourcePath tabSource) wrongColumn).mp (by decide))

/-- Planted (law `ConcreteSpec.Core.SourcePathSpan.bindSource_correct`,
case `ConcreteSpec.Core.tabSpan`, subject `ConcreteSpec.Core.SourcePathSpan.bindSource`,
mutant `ConcreteSpec.Core.SourcePathSpan.Controls.bindBeforeEof`). -/
theorem bindBeforeEof_violates_correct : ¬ BindingCorrect bindBeforeEof := by
  intro law
  have reference := (bindSource_correct tabProject tabProject scopedTabSpan _).mp bindTabSpan
  exact (by decide : bindBeforeEof tabProject tabProject scopedTabSpan ≠ some (File.load mainSourcePath tabSource))
    ((law tabProject tabProject scopedTabSpan _).mpr reference)

end Controls

end ConcreteSpec.Core.SourcePathSpan

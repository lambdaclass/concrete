import Concrete.Interfaces.Contracts
import ConcreteSpec.Spec.Rules

/-! Source location validation and provenance lemmas for the frozen stage carriers. -/

namespace Concrete.Origins

open Concrete.Interfaces
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

/-- Validate supplied coordinates through the spec's span issuer. -/
def validateLocation (project : Project) (supplied : FileSpan) :
    Except LocationRefusal {span : Span // supplied.Binds project span} :=
  match issueSpanAtM (m := Id) project supplied.file supplied.startOffset supplied.endOffset
      (some (supplied.line, supplied.col)) with
  | .error refusal => .error refusal.val
  | .ok ⟨span, sameFile, start, stop, coordinates⟩ =>
    .ok ⟨span.toSpan, by
      obtain ⟨file, location, found⟩ := span.issued
      have same := Option.some.inj (coordinates.resolve_left (by simp))
      exact ⟨file, by simpa [Project.fileAt, Project.files, sameFile] using found,
        sameFile.symm, location, start.symm, stop.symm, congrArg Prod.fst same,
        congrArg Prod.snd same⟩⟩

theorem validateLocation_refusal_sound {project : Project} {supplied : FileSpan}
    {refusal : LocationRefusal} (result : validateLocation project supplied = .error refusal) :
    refusal.Violates project supplied.file supplied.startOffset supplied.endOffset
      (some (supplied.line, supplied.col)) := by
  unfold validateLocation at result
  split at result
  · rename_i certified _
    cases result
    exact certified.property
  · contradiction

/-- Every location satisfying the frozen binding judgment passes validation. -/
theorem validateLocation_complete {project : Project} {supplied : FileSpan} {span : Span}
    (valid : supplied.Binds project span) :
    (validateLocation project supplied).isOk = true := by
  obtain ⟨file, found, _fileId, location, start, stop, line, col⟩ := valid
  have resolved : project.fileAt supplied.file = some file := by
    simpa [Project.fileAt, Project.files] using found
  have ordered : supplied.startOffset ≤ supplied.endOffset := start ▸ stop ▸ span.range_ok
  have sameLine : supplied.line = file.source.lineAt supplied.startOffset :=
    line.trans (location.2.1.trans (congrArg file.source.lineAt start).symm)
  have sameCol : supplied.col = file.source.columnAt supplied.startOffset :=
    col.trans (location.2.2.trans (congrArg file.source.columnAt start).symm)
  cases result : validateLocation project supplied with
  | ok certified => rfl
  | error refusal =>
    have violated := validateLocation_refusal_sound result
    exfalso
    cases refusal with
    | unknownFile => exact violated (by simp [SlotResolves, resolved])
    | reversedRange => exact violated ordered
    | pastEnd length =>
      obtain ⟨actual, lookup, _, outside⟩ := violated
      cases Option.some.inj (lookup.symm.trans resolved)
      exact outside (stop ▸ location.1)
    | wrongLine expected =>
      obtain ⟨actual, rawLine, rawCol, lookup, coordinates, wanted, mismatch⟩ := violated
      cases Option.some.inj (lookup.symm.trans resolved)
      exact mismatch ((congrArg Prod.fst (Option.some.inj coordinates)).symm.trans
        (sameLine.trans wanted.symm))
    | wrongColumn expected =>
      obtain ⟨actual, rawLine, rawCol, lookup, coordinates, wanted, mismatch⟩ := violated
      cases Option.some.inj (lookup.symm.trans resolved)
      exact mismatch ((congrArg Prod.snd (Option.some.inj coordinates)).symm.trans
        (sameCol.trans wanted.symm))

/-- A backend slot resolves to an entry carrying the complete original operation
origin. Append only at BIR formation, where the table and its terminators are built. -/
def appendTrap (table : TrapTable) (kind : TrapKind) (origin : Origin) :
    TrapIndex × TrapTable :=
  (⟨table.size⟩, table.push ⟨kind, origin⟩)

theorem appendTrap_resolves (table : TrapTable) (kind : TrapKind) (origin : Origin) :
    let (slot, extended) := appendTrap table kind origin
    extended[slot.index]? = some ⟨kind, origin⟩ := by
  simp [appendTrap]

theorem appendTrap_preserves_lookup (table : TrapTable) (kind : TrapKind) (origin : Origin)
    (slot : TrapIndex) (within : slot.index < table.size) :
    (appendTrap table kind origin).2[slot.index]? = table[slot.index]? := by
  simp [appendTrap, Array.getElem?_push, Nat.ne_of_lt within]

/-- The Core handoff binds every origin to the project admitted for that payload. -/
theorem coreOrigins_bind {language : CoreLanguage} {input : CoreInput language}
    (locations : input.LocationsWF) :
    ∀ origin ∈ List.map Prod.snd input.machineProgram.origins,
      ∃ span, origin.lexical.Binds input.project span := by
  intro origin member
  obtain ⟨⟨site, entry⟩, inTable, rfl⟩ := List.mem_map.mp member
  exact (locations.2.2.2.2.2 site entry inTable).2

/-- Location validity follows the complete origin across an edge. The contract
allows copies and requires discharged operations to retain an origin. -/
theorem binds_of_preservesOrigins {sourceProject targetProject : Project}
    {source target : List Origin} (sameProject : targetProject = sourceProject)
    (preserves : PreservesOrigins source target)
    (valid : ∀ origin ∈ source, ∃ span, origin.lexical.Binds sourceProject span) :
    ∀ origin ∈ target, ∃ span, origin.lexical.Binds targetProject span := by
  subst targetProject
  exact fun origin member => valid origin (preserves.1 origin member)

/-- Looking up the emitted slot yields the same lexical location in the same
admitted project as the operation that issued the trap. -/
theorem appendTrap_binds (table : TrapTable) (kind : TrapKind) (origin : Origin)
    (project : Project) (span : Span) (bound : origin.lexical.Binds project span) :
    let (slot, extended) := appendTrap table kind origin
    ∃ entry, extended[slot.index]? = some entry ∧ entry.kind = kind ∧
      entry.origin = origin ∧ entry.origin.lexical.Binds project span :=
  ⟨⟨kind, origin⟩, appendTrap_resolves table kind origin, rfl, rfl, bound⟩

/-- The matching block in the emitted function names a slot that resolves to
the SSA trap's kind and its source location. Membership includes unreachable blocks. -/
theorem emittedTrap_binds {Ty : Type} {lang : LoweringLanguage Ty} {profile : Profile}
    {ssa : SSA lang profile} {bir : BIR lang profile}
    (matched : bir.TrapsMatch ssa) (sameProject : bir.project = ssa.project)
    {fn} (inFunction : fn ∈ ssa.functions)
    {block} (inBlock : block ∈ fn.2.2.blocks.toList)
    {kind origin} (ending : block.ending = .trap kind origin)
    {span} (bound : origin.lexical.Binds ssa.project span) :
    ∃ after ∈ bir.functions, after.1 = fn.1 ∧
      ∃ next ∈ after.2.2.blocks.toList, next.id = block.id ∧
        ∃ slot, next.ending = .trap slot ∧
          ∃ entry, bir.sites[slot.index]? = some entry ∧ entry.kind = kind ∧
            entry.origin = origin ∧ entry.origin.lexical.Binds bir.project span := by
  obtain ⟨after, inAfter, functionId, next, inNext, blockId, slot, trap,
    entry, lookup, trapKind, trapOrigin⟩ := matched fn inFunction block inBlock kind origin ending
  exact ⟨after, inAfter, functionId, next, inNext, blockId, slot, trap,
    entry, lookup, trapKind, trapOrigin, by simpa only [trapOrigin, sameProject] using bound⟩

section Controls

private def atEOF : FileSpan := ⟨⟨0⟩, 3, 3, 1, 4⟩

example : (validateLocation tabProject atEOF).toOption.map Subtype.val =
    some ⟨⟨0⟩, 3, 3, by decide, 1, 4, by decide, by decide⟩ := by decide

-- The exact path exists even though slot 1 in this one-file project does not.
example : FilePresent (tabProject, mainSourcePath) ∧ ¬ SlotResolves (tabProject, ⟨1⟩) ∧
    (match validateLocation tabProject { atEOF with file := ⟨1⟩ } with
    | .error (reason@(.unknownFile)) =>
      decide (reason.ruleId = ⟨``SourcePathSpan.SlotResolves, 1⟩) &&
        (validateRule reason.ruleId).isSome
    | _ => false) = true := by
  refine ⟨?_, ?_, ?_⟩
  · simp [FilePresent, tabProject, Project.singleton, Project.files, Project.load, File.load]
  · intro resolves
    exact resolves rfl
  · decide

example : (match validateLocation tabProject { atEOF with startOffset := 4 } with
    | .error (.reversedRange) => true
    | _ => false) = true := by decide

example : (match validateLocation tabProject { atEOF with endOffset := 4 } with
    | .error (.pastEnd 3) => true
    | _ => false) = true := by decide

-- The offsets still fit, but a newline in the issuing file changes both coordinates.
example : (match validateLocation
    (Project.singleton mainSourcePath (SourceText.ofBytes [97, 10, 98])) atEOF with
    | .error (reason@(.wrongLine 2)) => decide (reason.ruleId = ⟨``SourcePathSpan.LineMatches, 2⟩)
    | _ => false) = true := by decide

example : (match validateLocation tabProject { atEOF with col := 0 } with
    | .error (reason@(.wrongColumn 4)) => decide (reason.ruleId = ⟨``SourcePathSpan.ColumnMatches, 2⟩)
    | _ => false) = true := by decide

private def originatingOp : Origin :=
  ⟨⟨47⟩, atEOF, ⟨.function ⟨loadedA.module, ⟨"main"⟩⟩, ⟨[⟨.primitive, .ordinal 0⟩]⟩⟩⟩

-- Two occurrences of a copied operation use different backend slots and both
-- retain the operation ID, location and persistent key.
example :
    let first := appendTrap #[] .overflow originatingOp
    let second := appendTrap first.2 .overflow originatingOp
    first.1.index = 0 ∧ second.1.index = 1 ∧
      (second.2[first.1.index]?.map TrapEntry.origin) = some originatingOp ∧
      (second.2[second.1.index]?.map TrapEntry.origin) = some originatingOp := by
  decide

private def dischargedOp : Origin :=
  { originatingOp with
    operation := ⟨48⟩
    evidenceKey := { originatingOp.evidenceKey with relative := ⟨[⟨.primitive, .ordinal 1⟩]⟩ } }

private def loweredOrigins (copy : Origin) (boundary : List Origin) : List Origin :=
  let graph : CFG Unit (LocatedOp Unit) (Terminator Unit Unit) :=
    ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨copy, ()⟩], .trap .overflow originatingOp, boundary⟩]⟩
  graph.origins Terminator.origins

-- A discharged operation survives in boundary metadata, and a residual operation
-- can have copies in the body and terminator.
example : PreservesOrigins [originatingOp, dischargedOp]
    (loweredOrigins originatingOp [dischargedOp]) := by
  simp [PreservesOrigins, loweredOrigins, CFG.origins, Terminator.origins]

example : ¬ PreservesOrigins [originatingOp, dischargedOp]
    (loweredOrigins originatingOp []) := by
  intro preserves
  have retained := preserves.2 dischargedOp (by simp)
  change dischargedOp ∈ [originatingOp, originatingOp] at retained
  exact (by decide : dischargedOp ∉ [originatingOp, originatingOp]) retained

example :
    let altered := { originatingOp with evidenceKey := dischargedOp.evidenceKey }
    ¬ PreservesOrigins [originatingOp, dischargedOp]
      (loweredOrigins altered [dischargedOp]) := by
  intro altered preserves
  have original := preserves.1 altered (by
    change altered ∈ [altered, dischargedOp, originatingOp]
    simp)
  exact (by decide : altered ∉ [originatingOp, dischargedOp]) original

end Controls

end Concrete.Origins

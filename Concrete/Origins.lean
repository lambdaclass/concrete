import Concrete.Interfaces.Contracts

/-! Source location validation and provenance lemmas for the frozen stage carriers. -/

namespace Concrete.Origins

open Concrete.Interfaces
open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

/-- A rejected location retains the supplied coordinates and, when available,
the exact expected coordinates or the length of the issuing file. -/
inductive LocationRefusal where
  | unknownFile (supplied : FileSpan)
  | reversedRange (supplied : FileSpan)
  | pastEnd (supplied : FileSpan) (fileLength : Nat)
  | wrongCoordinates (supplied : FileSpan) (expected : Span)
  deriving DecidableEq, Repr

/-- Resolve a location against the exact admitted file. Use the spec's indexed
position lookup and canonical span constructor. Empty ranges are valid. -/
def validateLocation (project : Project) (supplied : FileSpan) :
    Except LocationRefusal {span : Span // supplied.Binds project span} :=
  match resolved : project.fileAt supplied.file with
  | none => .error (.unknownFile supplied)
  | some file => do
      if ordered : supplied.startOffset ≤ supplied.endOffset then
        if fits : supplied.endOffset ≤ file.length then
          let expected := fileSpan supplied.file file supplied.startOffset supplied.endOffset
            ordered (Nat.le_trans ordered fits)
          if line : supplied.line = expected.line then
            if col : supplied.col = expected.col then
              return ⟨expected, file, by simpa [Project.fileAt, Project.files] using resolved,
                rfl, fileSpan_location _ _ _ _ ordered fits, rfl, rfl, line, col⟩
            else throw (.wrongCoordinates supplied expected)
          else throw (.wrongCoordinates supplied expected)
        else throw (.pastEnd supplied file.length)
      else throw (.reversedRange supplied)

/-- Every location satisfying the frozen binding judgment passes validation. -/
theorem validateLocation_complete {project : Project} {supplied : FileSpan} {span : Span}
    (valid : supplied.Binds project span) :
    (validateLocation project supplied).isOk = true := by
  obtain ⟨file, found, _fileId, location, start, stop, line, col⟩ := valid
  have resolved : project.fileAt supplied.file = some file := by
    simpa [Project.fileAt, Project.files] using found
  have ordered : supplied.startOffset ≤ supplied.endOffset := start ▸ stop ▸ span.range_ok
  have fits : supplied.endOffset ≤ file.length := by
    rw [stop, File.length_eq]
    exact location.1
  have canonical := fileSpan_location supplied.file file supplied.startOffset supplied.endOffset
    ordered fits
  have canonicalLine := canonical.2.1
  have canonicalCol := canonical.2.2
  change _ = file.source.lineAt supplied.startOffset at canonicalLine
  change _ = file.source.columnAt supplied.startOffset at canonicalCol
  have sameLine := line.trans (location.2.1.trans ((congrArg file.source.lineAt start).symm.trans
    canonicalLine.symm))
  have sameCol := col.trans (location.2.2.trans ((congrArg file.source.columnAt start).symm.trans
    canonicalCol.symm))
  unfold validateLocation
  split
  · rename_i absent
    rw [resolved] at absent
    contradiction
  · rename_i actual foundActual
    have identical := Option.some.inj (resolved.symm.trans foundActual)
    subst actual
    simp [ordered, fits, sameLine, sameCol, pure, Except.pure, Except.isOk, Except.toBool]

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
    ∀ origin ∈ List.map Prod.snd input.payload.origins,
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

example : (match validateLocation tabProject { atEOF with file := ⟨1⟩ } with
    | .error (.unknownFile _) => true
    | _ => false) = true := by decide

example : (match validateLocation tabProject { atEOF with startOffset := 4 } with
    | .error (.reversedRange _) => true
    | _ => false) = true := by decide

example : (match validateLocation tabProject { atEOF with endOffset := 4 } with
    | .error (.pastEnd _ 3) => true
    | _ => false) = true := by decide

-- The offsets still fit, but a newline in the issuing file changes both coordinates.
example : (match validateLocation
    (Project.singleton mainSourcePath (SourceText.ofBytes [97, 10, 98])) atEOF with
    | .error (.wrongCoordinates _ expected) => expected.line == 2 && expected.col == 2
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

-- A discharged operation can survive solely as boundary metadata. A copied
-- residual operation can occur both in the body and at its trap terminator.
example :
    let discharged := { originatingOp with
      operation := ⟨48⟩
      evidenceKey := { originatingOp.evidenceKey with relative := ⟨[⟨.primitive, .ordinal 1⟩]⟩ } }
    let graph : CFG Unit (LocatedOp Unit) (Terminator Unit Unit) :=
      ⟨⟨0⟩, #[⟨⟨0⟩, [], [⟨originatingOp, ()⟩], .trap .overflow originatingOp, [discharged]⟩]⟩
    graph.origins Terminator.origins = [originatingOp, discharged, originatingOp] := by rfl

end Controls

end Concrete.Origins

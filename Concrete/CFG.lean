import Concrete.Interfaces.Carriers

namespace Concrete.CFG

open Concrete.Interfaces

open ConcreteSpec.Core ConcreteSpec.Core.SourcePathSpan ConcreteSpec.StageInterface

/-- Check the frozen CFG contract on the executable graph. An unresolved
terminator is rejected even when the block is unreachable. -/
def check {P A C I Ty : Type} [DecidableEq Ty]
    (graph : CFG P I (Terminator A C)) (paramType : P → Ty) (argType : A → Ty) : Bool :=
  let blocks := graph.blocks.toList
  decide ((blocks.map Block.id).Nodup) &&
    blocks.any (fun block => decide (block.id = graph.entry)) &&
    blocks.all (fun block =>
      match block.ending with
      | .invalid => false
      | term => term.successors.all fun edge =>
        blocks.any fun target => decide
          (target.id = edge.target ∧ edge.arguments.map argType = target.params.map paramType))

theorem check_iff {P A C I Ty : Type} [DecidableEq Ty]
    (graph : CFG P I (Terminator A C)) (paramType : P → Ty) (argType : A → Ty) :
    check graph paramType argType = true ↔
      graph.WF Terminator.successors paramType argType ∧
        ∀ block ∈ graph.blocks.toList, block.ending ≠ .invalid := by
  have blockCheck (block : Block P I (Terminator A C)) :
      (match block.ending with
      | .invalid => false
      | term => term.successors.all fun edge =>
        graph.blocks.toList.any fun target => decide
          (target.id = edge.target ∧ edge.arguments.map argType = target.params.map paramType)) =
        true ↔
      (∀ edge ∈ block.ending.successors, ∃ next ∈ graph.blocks.toList,
        next.id = edge.target ∧ edge.arguments.map argType = next.params.map paramType) ∧
      block.ending ≠ .invalid := by
    cases block.ending <;> simp [Terminator.successors, -Array.any_toList]
  simp only [check, Bool.and_eq_true, decide_eq_true_eq, List.any_eq_true, List.all_eq_true,
    CFG.WF, blockCheck]
  constructor
  · rintro ⟨⟨unique, entry⟩, edges⟩
    exact ⟨⟨unique, entry, fun b hb => (edges b hb).1⟩, fun b hb => (edges b hb).2⟩
  · rintro ⟨⟨unique, entry, edges⟩, valid⟩
    exact ⟨⟨unique, entry⟩, fun b hb => ⟨edges b hb, valid b hb⟩⟩

/-- Reconstruct the spec span from the stored coordinates and check it in the
issuing project. -/
def checkSpan (project : Project) (span : FileSpan) : Bool :=
  if hr : span.startOffset ≤ span.endOffset then
    if hl : 0 < span.line then
      if hc : 0 < span.col then
        match project.files[span.file.index]? with
        | none => false
        | some file =>
          let specSpan : Span :=
            ⟨span.file, span.startOffset, span.endOffset, hr, span.line, span.col, hl, hc⟩
          decide (specSpan.LocationOk file.source)
      else false
    else false
  else false

theorem checkSpan_iff (project : Project) (span : FileSpan) :
    checkSpan project span = true ↔ ∃ specSpan, span.Binds project specSpan := by
  constructor
  · unfold checkSpan
    split
    · rename_i hr
      split
      · rename_i hl
        split
        · rename_i hc
          split
          · simp
          · rename_i file lookup
            intro valid
            exact ⟨_, file, lookup, rfl, of_decide_eq_true valid, rfl, rfl, rfl, rfl⟩
        · simp
      · simp
    · simp
  · rintro ⟨specSpan, file, lookup, fileId, valid, start, ending, line, col⟩
    have hr : span.startOffset ≤ span.endOffset := by rw [start, ending]; exact specSpan.range_ok
    have hl : 0 < span.line := by rw [line]; exact specSpan.line_pos
    have hc : 0 < span.col := by rw [col]; exact specSpan.col_pos
    simp only [checkSpan, dite_eq_left hr, dite_eq_left hl, dite_eq_left hc, lookup, decide_eq_true_eq]
    cases specSpan
    simp_all [Span.LocationOk, EndInSource, LineMatches, ColumnMatches]

private def oneBlock (ending : Terminator Nat Nat) : CFG Nat Unit (Terminator Nat Nat) :=
  ⟨⟨0⟩, #[⟨⟨0⟩, [], [], ending, []⟩]⟩

example : check (oneBlock (.jump ⟨⟨0⟩, []⟩)) id id = true := by decide
example : check (oneBlock (.jump ⟨⟨0⟩, [0]⟩)) id id = false := by decide
example : check (oneBlock .invalid) id id = false := by decide

private def project : Project := Project.singleton mainSourcePath (SourceText.ofBytes [52, 50])

example : checkSpan project ⟨⟨0⟩, 0, 2, 1, 1⟩ = true := by decide
example : checkSpan project ⟨⟨0⟩, 0, 3, 1, 1⟩ = false := by decide
example : checkSpan project ⟨⟨0⟩, 0, 2, 1, 2⟩ = false := by decide
example : checkSpan project ⟨⟨1⟩, 0, 2, 1, 1⟩ = false := by decide

end Concrete.CFG

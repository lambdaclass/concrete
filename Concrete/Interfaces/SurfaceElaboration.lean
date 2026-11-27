import ConcreteSpec.StageInterface
import ConcreteSpec.Spec.Surface0
import Concrete.Refusal

/-! The frozen Slice 0 elaboration used by both the driver and the stage contract. -/
namespace Concrete.Elab
open ConcreteSpec.Surface0

open ConcreteSpec.StageInterface (SyntaxTree FileSpan)

private def surfaceSpan (span : FileSpan) : Span :=
  ⟨span.startOffset, span.endOffset, span.line, span.col, span.file⟩

private def SyntaxTree.expression : SyntaxTree → Option Expr
  | .leaf ⟨span, .integer n _ suffix⟩ => some (.integer n suffix (surfaceSpan span))
  | .leaf ⟨span, .name name⟩ => some (.local name (surfaceSpan span))
  | .node .add [left, .leaf ⟨op, .fixed .plus _⟩, right] => do
    return .add (← expression left) (← expression right) (surfaceSpan op)
  | _ => none

private def SyntaxTree.statement : SyntaxTree → Option Statement
  | .node .return_ [.leaf ⟨start, .fixed .return_ _⟩, value, .leaf ⟨stop, .fixed .semi _⟩] =>
    return .ret (← expression value) (surfaceSpan {start with endOffset := stop.endOffset})
  | .node .binding [.leaf ⟨start, .fixed .let_ _⟩, .leaf ⟨nameSpan, .name name⟩,
      value, .leaf ⟨stop, .fixed .semi _⟩] =>
    return .bind ⟨name, surfaceSpan nameSpan, ← expression value⟩
      (surfaceSpan {start with endOffset := stop.endOffset})
  | _ => none

/-- Interpret the grammar's constructor tree. Token payloads and spans are
read directly from its leaves. -/
def surfaceProgram : SyntaxTree → Option Program
  | .node .program [.leaf ⟨span, .name name⟩, .node .body statements] =>
    return ⟨name, surfaceSpan span, ← statements.mapM SyntaxTree.statement⟩
  | _ => none

inductive ElabFailure where
  | unexpectedEntry (span : Span)
  | wildcard (span : Span)

def ElabFailure.diagnostic : ElabFailure → Refusal
  | .unexpectedEntry span => .unsupported s!"expected the entry point 'main' at byte {span.start}"
  | .wildcard span => .unsupported s!"wildcard binding at byte {span.start}"

/-- The handoff preserves every name, suffix, statement and coordinate.
Only unsupported syntax or entry shape can fail here. -/
def elaborateLocated (ast : Program) : Except ElabFailure Program :=
  if ast.name != "main" then .error (.unexpectedEntry ast.nameSpan)
  else match ast.body.findSome? (fun
      | .bind b _ => if b.name = "_" then some b.span else none
      | .ret .. => none) with
    | some span => .error (.wildcard span)
    | none => .ok ast

theorem elaborateLocated_faithful {ast out : Program} (h : elaborateLocated ast = .ok out) :
    ast = out := by
  unfold elaborateLocated at h
  split at h
  · cases h
  · split at h
    · cases h
    · exact Except.ok.inj h

end Concrete.Elab

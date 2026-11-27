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

end Concrete.Elab

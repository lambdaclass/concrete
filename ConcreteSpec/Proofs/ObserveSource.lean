import ConcreteSpec.Proofs.Observe2

/-!
# Source-derived trap positions

The operation constructs its column from the source prefix; callers do not promise that a
pre-existing column is correct. The discriminator below tests the rendered diagnostic, using
the independently proved injectivity of its complete parser/renderer pair.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Law (subject `ConcreteSpec.Core.Observe.sourceSpan`).
The diagnostic for a source-derived position differs from every diagnostic that supplies a
wrong byte column. In particular a scalar-counting or tab-expanding source-position producer
cannot satisfy this law by rejecting an admission premise about its own output. -/
theorem source_trap_column (path : String) (line : Nat) (linePositive : 0 < line)
    (before : String) (kind : TrapKind) (site : SiteId) (column : Nat)
    (columnPositive : 0 < column) (wrong : column ≠ before.utf8ByteSize + 1) :
    trapLine kind site (sourceSpan path line linePositive before) ≠
      trapLine kind site ⟨path, line, column, linePositive, columnPositive⟩ := by
  intro equal
  obtain ⟨_, _, spanEqual⟩ := trapLine_injective equal
  cases spanEqual
  exact wrong rfl

/-- A planted source-position producer counts Unicode scalars instead of UTF-8 bytes. -/
def scalarColumnSpan (path : String) (line : Nat) (linePositive : 0 < line)
    (before : String) : Span :=
  ⟨path, line, before.toList.length + 1, linePositive, Nat.zero_lt_succ _⟩

/-- Instance (law `ConcreteSpec.Core.Observe.source_trap_column`, case `utf8_tab`). -/
example : trapLine .overflow ⟨7⟩ (sourceSpan "src/main.con" 1 (by decide) "a\té") ≠
    trapLine .overflow ⟨7⟩ ⟨"src/main.con", 1, 4, by decide, by decide⟩ :=
  source_trap_column "src/main.con" 1 (by decide) "a\té" .overflow ⟨7⟩ 4
    (by decide) (by decide)

/-- Planted (law `ConcreteSpec.Core.Observe.source_trap_column`, case `utf8_tab`,
subject `ConcreteSpec.Core.Observe.sourceSpan`, mutant `ConcreteSpec.Core.Observe.scalarColumnSpan`). -/
example : (0 < 1) ∧ (0 < 4) ∧ (4 ≠ "a\té".utf8ByteSize + 1) ∧
    ¬ (trapLine .overflow ⟨7⟩ (scalarColumnSpan "src/main.con" 1 (by decide) "a\té") ≠
      trapLine .overflow ⟨7⟩ ⟨"src/main.con", 1, 4, by decide, by decide⟩) := by
  decide

end ConcreteSpec.Core.Observe

import Concrete.Contracts
import Concrete.Backend.LCB

namespace Concrete.Backend

open Contracts
open LeanCompilerBackend.Verified

/-- The slice-0 runtime observation of a completed BIR body. The table index selects the
lexical origin, while the rendered bytes use the entry's diagnostic site identity. -/
def birObservation (sites : BIR.SiteTable) : BIR.Outcome → Option Observation
  | .exited status =>
    if status < 256 then some (.returned (UInt8.ofNat status) ByteArray.empty) else none
  | .trapped index => do
    let entry ← sites[index]?
    let kind := match entry.kind with | .arithmeticOverflow => ConcreteSpec.Core.TrapKind.overflow
    let site := ⟨⟨entry.span.start, entry.span.stop, entry.loc.line, entry.loc.col⟩, entry.loc⟩
    return .trapped 134 kind site ((Runtime.trapLine entry ++ "\n").toUTF8)
  | .stuck | .outOfFuel => none

/-- Adequacy of the BIR evaluator, stated using the same projection as the native adapter. -/
def BIREvaluatorAdequacy (semantics : Semantics BIR.Program) : Prop :=
  EvaluatorAdequacy semantics (fun p budget => birObservation p.sites (BIR.run p budget))

/-- The LCB source boundary is interpreted using the pinned package's own behavior relation.
The slice-0 ABI encoding is used here to relate a body return to the runtime projection. -/
def LCBSourceAdequacy
    (semantics : Semantics (Source.Function × BIR.SiteTable × ByteArray)) : Prop :=
  ∀ f sites, let input := (f, sites, (Runtime.qbe sites).toUTF8)
    (∀ o, semantics.observes input o ↔
      ∃ outcome word, BIR.Outcome.Encodable sites outcome ∧ BIR.encode outcome = some word ∧
        Source.HasBehavior f #[] (.returned (.word (BitVec.ofNat 32 word))) ∧
        birObservation sites outcome = some o) ∧
    ((∃ run, semantics.diverges input run) ↔ Source.HasBehavior f #[] .diverges)

end Concrete.Backend

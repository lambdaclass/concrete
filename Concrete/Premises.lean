/-
The slice-0 trusted base (PROVISIONAL): the open premises and the headline theorems whose axiom
closure the trusted-base golden records (Concrete/README.md gate 9).

This is the premise REGISTRY, the one list of the open premises: each entry has its trust class,
what retires it, and its statement. The statements are text until the structures that state them
as Lean propositions (`FrontendPremises`, `NativePremises`, whose design is in Concrete/README.md)
land with the front end; each entry then names its field instead. `scripts/checks/compiler/slice0_trusted_base.sh` prints
this registry and the axioms of `theorems` and compares them with `tests/golden/slice0_trusted_base.txt`,
so a premise added, removed or reworded fails until the golden is updated with it.
-/
import Lean.Meta.Basic
import Lean.PrettyPrinter
import Lean.Util.CollectAxioms
import Concrete.BIR.Progress
import Concrete.Backend.LCB
import Concrete.Lex

namespace Concrete.Premises

inductive Trust where
  | implementation | toolchain | environment
  deriving Repr, DecidableEq

def Trust.name : Trust → String
  | .implementation => "implementation"
  | .toolchain => "toolchain"
  | .environment => "environment"

structure Premise where
  id : String
  trust : Trust
  retiredBy : String
  statement : String

/-- The open premises for slice 0, in the README's order. -/
def all : List Premise := [
  ⟨"P_lex", .implementation, "`lex_correct`",
    "every token list the lexer returns for a source is one the source lexes to"⟩,
  ⟨"P_parse", .implementation, "the generic table-parser theorem",
    "every tree the parser returns for a token list is one the grammar derives from it"⟩,
  ⟨"P_print", .implementation, "an LLVM-IR semantics; not planned for slice 0",
    "the printed LLVM text of a WF BIR means at most the BIR behaviour, encoded"⟩,
  ⟨"P_toolchain[llvm]", .toolchain, "not planned",
    "every result the linked LLVM body produces is one its text means"⟩,
  ⟨"P_toolchain[lcb]", .toolchain, "LCB's own proofs (C01–C03 and beyond)",
    "every result the linked LCB body produces is one its function means (LCB's named trust boundaries)"⟩,
  ⟨"P_rt", .implementation, "a proof of the decoding entry against a libc model; not planned for slice 0",
    "every launch of an image built from a WF BIR that is not killed has the behaviour of some result its body produces (decode w for a returned word w, diverges for divergence)"⟩,
  ⟨"P_trap_site", .implementation, "the same runtime-entry proof (owner, 2026-11-06: a premise now)",
    "a launch whose body returns 256 + i writes exactly trapLine tbl[i] and a newline on fd 2"⟩,
  ⟨"P_launch", .environment, "not planned",
    "an admitted launch of a WF image within the frame bound is never killed, and fd 2 accepts the trap line"⟩,
  ⟨"P_live", .environment, "not planned",
    "an admitted launch happens and has a behaviour"⟩
]

/-- The headline theorems, whose axioms the golden records. -/
def theorems : List Lean.Name := [
  ``Concrete.Lex.decodeAt_sound,
  ``Concrete.Lex.decodeAt_complete,
  ``Concrete.Lex.decodeUtf8_sound,
  ``Concrete.Lex.decodeUtf8_complete,
  ``Concrete.Lex.sourceAt_iff,
  ``Concrete.Lex.checkSource_sound,
  ``Concrete.Lex.checkSource_complete,
  ``Concrete.Lex.identStart?_iff,
  ``Concrete.Lex.identContinue?_iff,
  ``Concrete.Lex.scanLine_sound,
  ``Concrete.Lex.scanLine_complete,
  ``Concrete.Lex.lineComment_sound,
  ``Concrete.Lex.lineComment_complete,
  ``Concrete.Lex.decode_ok_iff,
  ``Concrete.Lex.lineComment_some_iff,
  ``Concrete.Lex.decode_error_not_decodes,
  ``Concrete.BIR.decode_encode,
  ``Concrete.BIR.run_progress,
  ``Concrete.BIR.behaviour_diverges_iff,
  ``Concrete.Backend.LCB.Proof.adapter_correct,
  ``Concrete.Backend.LCB.Proof.emit_correct,
  ``Concrete.Backend.LCB.Proof.artifact_of_emit_error,
  ``Concrete.Backend.LCB.Proof.artifact_correct
]

/-- The premise's row in Concrete/README.md's open-premise table (a generated view). -/
def Premise.tableRow (p : Premise) : String :=
  s!"| {p.id} | {p.trust.name} | {p.retiredBy} |"

def Premise.line (p : Premise) : String :=
  s!"premise {p.id} [{p.trust.name}] retired-by: {p.retiredBy} :: {p.statement}"

open Lean in
/-- The trusted-base report: one line per premise, then for each theorem its sorted axioms, then
    each axiom once with its type (so an axiom restated under the same name changes the report). -/
def report (thms : List Name) : CoreM (List String) := do
  let mut lines := all.map Premise.line
  let mut seen : Array Name := #[]
  for n in thms do
    let axs := (← collectAxioms n).qsort Name.lt
    lines := lines ++ [s!"theorem {n} axioms: {", ".intercalate (axs.map toString).toList}"]
    for a in axs do
      unless seen.contains a do seen := seen.push a
  for a in seen.qsort Name.lt do
    let ty ← Meta.MetaM.run' (PrettyPrinter.ppExpr (← getConstInfo a).type)
    lines := lines ++ [s!"axiom {a} : {toString ty}"]
  return lines

end Concrete.Premises

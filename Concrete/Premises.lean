/-
The slice-0 trusted base (PROVISIONAL): the open premises and the headline theorems whose axiom
closure the trusted-base golden records (Concrete/README.md gate 9).

This is the premise REGISTRY, the one list of the open premises: each entry has its trust class,
what retires it, and its statement. The frontend and lowering contracts below are Lean
propositions, as are the native contracts. See Concrete/README.md for their design. `scripts/checks/compiler/slice0_trusted_base.sh` prints
this registry and the axioms of `theorems` and compares them with `tests/golden/slice0_trusted_base.txt`,
so a premise added, removed or reworded fails until the golden is updated with it.
-/
import Lean.Meta.Basic
import Lean.PrettyPrinter
import Lean.Util.CollectAxioms
import Concrete.BIR.Progress
import Concrete.Backend.LCB
import Concrete.Stages

namespace Concrete

open ConcreteSpec.Surface0 in
structure FrontendPremises where
  lex : ∀ src toks, Lex.lex src = .ok toks → Lexes src toks
  parse : ∀ toks ast, Parse.parse toks = .ok ast → Derives toks ast
  elaborate : ∀ ast out, Elab.elaborate ast = .ok out →
    let p := Core.toCore out.program
    Elab.eval ast = Core.run p (p.body.length + 1)

/-- Open implementation contracts for the lowering stages. The production path gets
its evidence from admission. The conformance path carries an empty evidence list. -/
structure LoweringPremises where
  lower :
    (∀ source admitted mir, Admission.check source = .ok admitted → MIR.lower admitted = .ok mir →
      let p := Core.toCore source.program
      MIR.run mir = Core.run p (p.body.length + 1)) ∧
    (∀ source mir, MIR.lowerUnchecked source = .ok mir →
      let p := Core.toCore source.program
      MIR.run mir = Core.run p (p.body.length + 1))
  ssa : ∀ path mem bir, SSA.lower path mem = .ok bir →
    match BIR.run bir (bir.blocks.length + 1) with
    | .exited n => Memory.run mem = .exit n
    | .trapped site => ∃ entry, bir.sites[site]? = some entry ∧ Memory.run mem = .trap entry.kind
    | _ => False

namespace Native
open BIR Backend

structure BackendModel where
  Artifact : Type
  emit : BIR.Program → Except String Artifact
  means : Artifact → BodyResult → Prop

def llvm (meaning : String → BodyResult → Prop) : BackendModel :=
  ⟨String, fun p => .ok (LLVM.module p), meaning⟩

def lcb : BackendModel :=
  ⟨LeanCompilerBackend.Verified.Source.Function × SiteTable, LCB.emit,
    fun (f, _) result => LCB.bodyResult f result⟩

def EmitCorrect (backend : BackendModel) : Prop :=
  ∀ p artifact result, BIR.WF p → backend.emit p = .ok artifact → backend.means artifact result →
    result.behaviour p.sites = BIR.behaviour p

theorem lcb_emit_correct : EmitCorrect lcb := by
  intro p artifact result hwf hem hr
  obtain ⟨f, sites⟩ := artifact
  have he : LCB.emit p = .ok (f, sites) := hem
  obtain ⟨_, rfl⟩ := LCB.Proof.emit_ok he
  exact LCB.Proof.emit_correct hwf he result hr

/-- The LLVM premise is about the exact text emitted, including the runtime and its site table. -/
structure LLVMPremises (meaning : String → BodyResult → Prop) : Prop where
  print : EmitCorrect (llvm meaning)

/-- One pinned build and execution environment. Its launch identity ties body execution to the
observation and the bytes on descriptor 2 from that same launch. -/
structure World (backend : BackendModel) where
  Image : Type
  Launch : Type
  built : backend.Artifact → Image → Prop
  launches : Image → Launch → Prop
  admitted : Launch → Prop
  bodyRuns : Launch → BodyResult → Prop
  observes : Launch → Behaviour → String → Prop

structure Premises (backend : BackendModel) (world : World backend) (maxSlots : Nat) : Prop where
  toolchain : ∀ p artifact image launch result, BIR.WF p → backend.emit p = .ok artifact →
    world.built artifact image → world.launches image launch → world.bodyRuns launch result →
    backend.means artifact result
  runtime : ∀ p artifact image launch output stderr, BIR.WF p → backend.emit p = .ok artifact →
    world.built artifact image → world.launches image launch → world.observes launch output stderr →
    output ≠ .killed → ∃ result, world.bodyRuns launch result ∧ output = result.behaviour p.sites
  trapSite : ∀ p artifact image launch output stderr i entry, BIR.WF p →
    backend.emit p = .ok artifact → world.built artifact image → world.launches image launch →
    world.admitted launch → world.observes launch output stderr →
    world.bodyRuns launch (.returns (BitVec.ofNat 32 (256 + i))) → p.sites[i]? = some entry →
    stderr = Runtime.trapLine entry ++ "\n"
  launch : ∀ p artifact image launch output stderr, BIR.WF p → p.frameSlots ≤ maxSlots →
    backend.emit p = .ok artifact → world.built artifact image → world.launches image launch →
    world.admitted launch → world.observes launch output stderr → output ≠ .killed
  live : ∀ p artifact image, BIR.WF p → p.frameSlots ≤ maxSlots → backend.emit p = .ok artifact →
    world.built artifact image →
    ∃ launch output stderr, world.launches image launch ∧ world.admitted launch ∧
      world.observes launch output stderr

theorem observed_behaviour {backend : BackendModel} {world : World backend} {maxSlots : Nat}
    (premises : Premises backend world maxSlots) (emitCorrect : EmitCorrect backend)
    {p : BIR.Program} {artifact : backend.Artifact} {image : world.Image} {launch : world.Launch}
    {output : Behaviour} {stderr : String} (hwf : BIR.WF p) (hframe : p.frameSlots ≤ maxSlots)
    (hemit : backend.emit p = .ok artifact) (hbuilt : world.built artifact image)
    (hlaunch : world.launches image launch) (hadmitted : world.admitted launch)
    (hobs : world.observes launch output stderr) : output = BIR.behaviour p := by
  have alive := premises.launch p artifact image launch output stderr hwf hframe hemit hbuilt
    hlaunch hadmitted hobs
  obtain ⟨result, hbody, rfl⟩ :=
    premises.runtime p artifact image launch output stderr hwf hemit hbuilt hlaunch hobs alive
  exact emitCorrect p artifact result hwf hemit
    (premises.toolchain p artifact image launch result hwf hemit hbuilt hlaunch hbody)

end Native

end Concrete

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
  ⟨"P_elab", .implementation, "ANF and resolution preservation",
    "successful elaboration preserves the source AST evaluator's outcome in Core"⟩,
  ⟨"P_lower", .implementation, "checked-evidence validity and MIR preservation",
    "MIR preserves Core outcomes for unchecked lowering and for evidence produced by admission"⟩,
  ⟨"P_ssa", .implementation, "SSA semantic preservation",
    "successful SSA lowering preserves Memory CFG outcomes, including the trap kind"⟩,
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
  ``ConcreteSpec.Surface0.grammar0_ll1,
  ``Concrete.Parse.table0_checked,
  ``Concrete.Admission.admit_checked,
  ``Concrete.Admission.check_evidence,
  ``Concrete.Admission.admit_never_traps,
  ``Concrete.Memory.lower_correct,
  ``Concrete.SSA.lower_wf,
  ``Concrete.Stages.compile_of_edges,
  ``Concrete.Native.lcb_emit_correct,
  ``Concrete.Native.observed_behaviour,
  ``Concrete.Lex.checkSource_ok_iff,
  ``Concrete.Lex.checkSource_error_offset_iff,
  ``Concrete.Lex.checkSource_error_complete,
  ``Concrete.Lex.checkSource_error_iff,
  ``Concrete.Lex.lineComment_none_iff,
  ``Concrete.Lex.lineComment_error_iff,
  ``Concrete.Lex.decodeStep_complete,
  ``Concrete.Lex.decodeStep_error_first,
  ``Concrete.Lex.decodeLoop_sound,
  ``Concrete.Lex.decodeLoop_complete,
  ``Concrete.Lex.decodeLoop_error_first,
  ``Concrete.Lex.decode_error_first,
  ``Concrete.Lex.decode_error_offset_iff,
  ``Concrete.Lex.decode_error_complete,
  ``Concrete.Lex.decode_error_iff,
  ``Concrete.Lex.sourceLoop_error_first,
  ``Concrete.Lex.checkSource_error_first,
  ``Concrete.Lex.checkSource_error_not_sourceOk,
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
  ``Concrete.Lex.whitespace?_iff,
  ``Concrete.Lex.commentBody_rest_length_le,
  ``Concrete.Lex.lineComment_rest_length_lt,
  ``Concrete.Lex.skipTriviaAux_sound,
  ``Concrete.Lex.skipTriviaAux_complete,
  ``Concrete.Lex.skipTrivia_sound,
  ``Concrete.Lex.skipTrivia_complete,
  ``Concrete.Lex.trivia_unique,
  ``Concrete.Lex.scanIdentBody_sound,
  ``Concrete.Lex.scanIdentBody_complete,
  ``Concrete.Lex.identifier_sound,
  ``Concrete.Lex.identifier_complete,
  ``Concrete.Lex.identifier_unique,
  ``Concrete.Lex.decodeEscape_sound,
  ``Concrete.Lex.decodeEscape_complete,
  ``Concrete.Lex.stringPrepend_ok,
  ``Concrete.Lex.stringBody_sound,
  ``Concrete.Lex.stringBody_complete,
  ``Concrete.Lex.stringBody_unique,
  ``Concrete.Lex.decodeByteEscape_sound,
  ``Concrete.Lex.decodeByteEscape_complete,
  ``Concrete.Lex.byteLiteral_sound,
  ``Concrete.Lex.byteLiteral_complete,
  ``Concrete.Lex.byteLiteral_unique,
  ``Concrete.Lex.byteLiteral_none_iff,
  ``Concrete.Lex.label_sound,
  ``Concrete.Lex.label_complete,
  ``Concrete.Lex.label_unique,
  ``Concrete.Lex.prefixRest_sound,
  ``Concrete.Lex.prefixRest_complete,
  ``Concrete.Lex.longestMatch_cons_of_no_match,
  ``Concrete.Lex.longestMatchAux_sound,
  ``Concrete.Lex.longestMatch_sound,
  ``Concrete.Lex.longestMatch_unique,
  ``Concrete.Lex.longestMatch_complete,
  ``Concrete.Lex.scanDigits_sound,
  ``Concrete.Lex.scanDigits_complete,
  ``Concrete.Lex.decodeSuffix_sound,
  ``Concrete.Lex.decodeSuffix_complete,
  ``Concrete.Lex.splitRadix_spec,
  ``Concrete.Lex.integerDigits_sound,
  ``Concrete.Lex.integerDigits_complete,
  ``Concrete.Lex.integerLiteral_sound,
  ``Concrete.Lex.decimal_prefixed_refused,
  ``Concrete.Lex.integerLiteral_complete,
  ``Concrete.Lex.integerLiteral_unique,
  -- F30 representation correspondences: retain all existing headlines and audit the bridge too.
  ``Concrete.BIR.ScalarBridge.fromCore_toCore,
  ``Concrete.BIR.ScalarBridge.toCore_fromCore,
  ``Concrete.BIR.ScalarBridge.bits,
  ``Concrete.BIR.ScalarBridge.signed,
  ``Concrete.BIR.ScalarBridge.min,
  ``Concrete.BIR.ScalarBridge.max,
  ``Concrete.BIR.ScalarBridge.inRange,
  ``Concrete.BIR.ScalarBridge.checked,
  ``Concrete.BIR.ScalarBridge.wrap,
  ``Concrete.BIR.ScalarBridge.toBits,
  ``Concrete.BIR.ScalarBridge.ofBits,
  ``Concrete.BIR.ScalarBridge.intValue_valid,
  ``Concrete.BIR.hasTy_int_core,
  ``Concrete.Backend.LCB.Proof.enc_int_core,
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
    let names := if axs.isEmpty then "" else " " ++ ", ".intercalate (axs.map toString).toList
    lines := lines ++ [s!"theorem {n} axioms:{names}"]
    for a in axs do
      unless seen.contains a do seen := seen.push a
  for a in seen.qsort Name.lt do
    let ty ← Meta.MetaM.run' (PrettyPrinter.ppExpr (← getConstInfo a).type)
    lines := lines ++ [s!"axiom {a} : {toString ty}"]
  return lines

end Concrete.Premises

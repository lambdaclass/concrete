/-
The slice-0 trusted base (PROVISIONAL): the trusted-component registry, the open premises and the
headline theorems whose axiom closure the trusted-base golden records (Concrete/README.md gate 9).

This is THE trusted-base list. Each component records what is trusted, why, the slice that added it,
how it could be removed, the premises it carries and the concrete names matched by gate 9's checks.
The premise statements are text until the structures that state them as Lean propositions
(`FrontendPremises`, `NativePremises`, whose design is in Concrete/README.md) land with the front
end; each entry then names its field instead.
-/
import Lean
import Lean.Util.CollectAxioms
import Concrete.BIR.Progress
import Concrete.Backend.LCB

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

inductive ComponentKind where
  | kernel | axiom | leanRuntime | buildTool | implementation | backend | notTrusted | runtime
  | toolchain | pipelineTool | system | gateTool
  deriving Repr, DecidableEq

def ComponentKind.name : ComponentKind → String
  | .kernel => "kernel"
  | .axiom => "axiom"
  | .leanRuntime => "Lean compiler/runtime"
  | .buildTool => "build tool"
  | .implementation => "trusted Lean code"
  | .backend => "unproved backend"
  | .notTrusted => "not trusted (proved)"
  | .runtime => "runtime"
  | .toolchain => "toolchain"
  | .pipelineTool => "pipeline tool"
  | .system => "OS/libc"
  | .gateTool => "gate tool"

def ComponentKind.all : List ComponentKind := [
  .kernel, .axiom, .leanRuntime, .buildTool, .implementation, .backend, .notTrusted, .runtime,
  .toolchain, .pipelineTool, .system, .gateTool
]

structure Component where
  id : String
  kind : ComponentKind
  what : String
  why : String
  slice : String
  removal : String
  premises : List String
  declarations : List String := []
  «matches» : List String

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

/-- Every trusted piece and the proved adapter explicitly excluded from the trusted base. -/
def components : List Component := [
  { id := "lean-kernel", kind := .kernel, slice := "slice0", premises := [], «matches» := []
    what := "the Lean kernel that checks the compiler's proofs"
    why := "proof checking is the root of the kernel-checked guarantees"
    removal := "not planned; independent checker replay can reduce reliance on this one implementation" },
  { id := "axiom-propext", kind := .axiom, slice := "slice0", premises := [], «matches» := ["propext"]
    what := "Lean's propositional extensionality axiom"
    why := "allowed logical axiom used by the proofs"
    removal := "remove its uses from the proofs and delete this entry" },
  { id := "axiom-classical-choice", kind := .axiom, slice := "slice0", premises := [], «matches» := ["Classical.choice"]
    what := "Lean's classical choice axiom"
    why := "allowed logical axiom used by the proofs"
    removal := "remove its uses from the proofs and delete this entry" },
  { id := "axiom-quot-sound", kind := .axiom, slice := "slice0", premises := [], «matches» := ["Quot.sound"]
    what := "Lean's quotient soundness axiom"
    why := "allowed logical axiom used by the proofs"
    removal := "remove its uses from the proofs and delete this entry" },
  { id := "lean-compiler-runtime", kind := .leanRuntime, slice := "slice0", premises := [], «matches» := ["lean", "leanc"]
    what := "the Lean compiler, runtime and interpreter executing Concrete via lake env lean --run and the compiled lean binary"
    why := "the compiler's executable behaviour is produced by Lean code outside its kernel proofs"
    removal := "not planned; per-compilation certificates attest the emitted output" },
  { id := "lake", kind := .buildTool, slice := "slice0", premises := [], «matches» := ["lake", "git", "ar"]
    what := "Lake dependency resolution and builds, including the pinned lean-compiler-backend revision"
    why := "Lake selects and builds the code that the compiler and gates execute"
    removal := "not planned; replace with another pinned, reproducible dependency and build mechanism" },
  { id := "lexer", kind := .implementation, slice := "slice0", premises := ["P_lex"]
    «matches» := ["Concrete.Lex.lex"]
    what := "the Concrete lexer implementation"
    why := "its correspondence with the independent byte-level lexing specification is not yet proved"
    removal := "prove lex_correct and retire P_lex" },
  { id := "parser", kind := .implementation, slice := "slice0", premises := ["P_parse"]
    «matches» := ["Concrete.Parse.parse"]
    what := "the Concrete table-driven parser implementation"
    why := "its correspondence with the surface grammar's derivation relation is not yet proved"
    removal := "prove the generic table-parser theorem and retire P_parse" },
  { id := "llvm-printer", kind := .backend, slice := "slice0", premises := ["P_print"]
    «matches» := ["Concrete.Backend.LLVM.print", "Concrete.Backend.LLVM.module"]
    what := "the unproved BIR-to-LLVM printer in Concrete/Backend/LLVM.lean"
    why := "no LLVM-IR semantics yet relates the printed text to BIR behaviour"
    removal := "prove the printer against an LLVM-IR semantics and retire P_print" },
  { id := "bir-lcb-adapter", kind := .notTrusted, slice := "slice0", premises := []
    «matches» := ["Concrete.Backend.LCB.Proof.emit_correct"]
    what := "the BIR-to-LCB adapter in Concrete/Backend/LCB.lean"
    why := "NOT trusted: emit_correct proves accepted adaptations against LCB's imported source semantics"
    removal := "not applicable; this entry records the proved boundary, not a trusted component" },
  { id := "runtime-entry", kind := .runtime, slice := "slice0", premises := ["P_rt", "P_trap_site"]
    «matches» := ["Concrete.Backend.Runtime.llvm", "Concrete.Backend.Runtime.qbe"]
    what := "the fixed runtime entry in Concrete/Backend/Runtime.lean that decodes result words and reports trap sites"
    why := "its native behaviour and exact trap-site output are not yet proved against a libc model"
    removal := "prove the decoding entry against a libc model and retire P_rt and P_trap_site" },
  { id := "lcb-lowering-assembler", kind := .implementation, slice := "slice0", premises := ["P_toolchain[lcb]"]
    «matches» := ["LeanCompilerBackend.Verified.compileFunction", "LeanCompilerBackend.Verified.reify",
      "LeanCompilerBackend.AMD64.emitModuleValidated", "LeanCompilerBackend.AArch64.emitModuleValidated"]
    what := "LCB's in-process lowering and assembly rendering"
    why := "native execution of the adapted function relies on LCB's named trust boundaries"
    removal := "LCB's preservation proofs for the slice-0 domain retire P_toolchain[lcb]" },
  { id := "lcb-command", kind := .pipelineTool, slice := "slice0", premises := [], «matches» := ["lcb"]
    what := "the lcb command line that assembles the runtime entry's QBE text"
    why := "the runtime QBE text is assembled outside the proved BIR-to-LCB adapter path"
    removal := "emit the runtime through LCB's proved object path, or prove this command's assembly path" },
  { id := "clang-llvm", kind := .toolchain, slice := "slice0", premises := ["P_toolchain[llvm]"]
    «matches» := ["clang", "clang-21"]
    what := "clang and LLVM compiling the printed LLVM IR and driving native assembly and linking"
    why := "native execution of LLVM output relies on the correctness of this toolchain"
    removal := "not planned; a verified LLVM path could retire P_toolchain[llvm]" },
  { id := "system-linker", kind := .pipelineTool, slice := "slice0", premises := []
    «matches» := ["ld", "ld64", "ld.lld"]
    what := "the system linker invoked by clang (ld, ld64 or ld.lld)"
    why := "the linked executable's layout and symbol resolution are not kernel-checked"
    removal := "not planned; use a verified linker and object path" },
  { id := "assembler", kind := .pipelineTool, slice := "slice0", premises := [], «matches» := ["as"]
    what := "the assembler, integrated in clang or invoked by clang as as"
    why := "assembly text is translated to native machine code outside Lean's proofs"
    removal := "use LCB's verified object emission for every native input" },
  { id := "libc", kind := .system, slice := "slice0", premises := [], «matches» := ["write", "_exit", "abort"]
    what := "libc's write, _exit and abort used by the runtime entry"
    why := "their native contracts and implementations are outside the Lean model"
    removal := "replace them with a minimal direct-syscall launcher and prove that launcher" },
  { id := "os-kernel-loader", kind := .system, slice := "slice0", premises := ["P_launch", "P_live"], «matches» := []
    what := "the OS kernel and loader that launch images and provide process and file-descriptor behaviour"
    why := "launch liveness, resource behaviour and trap-line delivery are environmental assumptions"
    removal := "not planned for a hosted native executable" },
  { id := "nix-flake", kind := .buildTool, slice := "slice0", premises := [], «matches» := ["nix"]
    what := "Nix, the devshell and the pinned flake inputs supplying the toolchain"
    why := "the development and CI environment determines the programs used by the pipeline"
    removal := "not planned; another reproducibly pinned environment would replace this entry" },
  { id := "native-shell-utilities", kind := .pipelineTool, slice := "slice0", premises := []
    «matches» := ["bash", "env", "uname", "mktemp", "realpath", "rm"]
    what := "the shell and coreutils used by the native build helper and Nix compiler/linker wrappers"
    why := "they select targets, construct the reduced environment, manage scratch files and resolve wrapper paths"
    removal := "replace the shell helper and Nix shell wrappers with a pinned, attested driver using direct filesystem APIs" },
  { id := "gate-analysis-tools", kind := .gateTool, slice := "slice0", premises := []
    «matches» := ["gdb", "lldb", "llvm-objdump", "llvm-size", "python3", "strace"]
    what := "gdb, lldb, llvm-objdump, llvm-size, python3 and strace used only by gates"
    why := "trusted only for the gates' analyses and verdicts, not as compilation-pipeline components"
    removal := "remove or replace the corresponding gate analyses; these tools are not needed to compile a program" }
]

def registryErrors (cs : List Component := components) : List String := Id.run do
  let mut errors := []
  for p in all do
    let carriers := cs.foldl (fun n c => n + (c.premises.filter (· == p.id)).length) 0
    unless carriers == 1 do
      errors := errors ++ [s!"premise {p.id} is carried by {carriers} components, expected exactly one"]
  for c in cs do
    let idOwners := (cs.filter (·.id == c.id)).length
    unless idOwners == 1 do
      errors := errors ++ [s!"component id {c.id} occurs {idOwners} times, expected exactly one"]
    for p in c.premises do
      unless all.any (·.id == p) do errors := errors ++ [s!"component {c.id} names unknown premise {p}"]
    for m in c.declarations do
      let owners := cs.foldl (fun n c => n + (c.declarations.filter (· == m)).length) 0
      unless owners == 1 do
        errors := errors ++ [s!"declaration match {m} occurs {owners} times, expected exactly one"]
    for m in c.«matches» do
      let owners := cs.foldl (fun n c => n + (c.«matches».filter (· == m)).length) 0
      unless owners == 1 do
        errors := errors ++ [s!"match {m} is owned by {owners} components, expected exactly one"]
  for k in ComponentKind.all do
    unless cs.any (·.kind == k) do errors := errors ++ [s!"component kind {k.name} has no entry"]
  return errors

/-- The premise's row in Concrete/README.md's open-premise table (a generated view). -/
def Premise.tableRow (p : Premise) : String :=
  s!"| {p.id} | {p.trust.name} | {p.retiredBy} |"

def Premise.line (p : Premise) : String :=
  s!"premise {p.id} [{p.trust.name}] retired-by: {p.retiredBy} :: {p.statement}"

def Component.tableRow (c : Component) : String :=
  let premises := if c.premises.isEmpty then "—" else ", ".intercalate c.premises
  let names := c.«matches» ++ c.declarations.map ("declaration: " ++ ·)
  let toolNames := if names.isEmpty then "—" else ", ".intercalate names
  s!"| {c.id} | {c.kind.name} | {c.slice} | {premises} | {c.what} | {c.why} | {c.removal} | {toolNames} |"

def Component.line (c : Component) : String :=
  let premises := if c.premises.isEmpty then "-" else ", ".intercalate c.premises
  let toolNames := if c.«matches».isEmpty then "-" else ", ".intercalate c.«matches»
  let declarations := if c.declarations.isEmpty then "-" else ", ".intercalate c.declarations
  s!"component {c.id} [{c.kind.name}] slice: {c.slice} premises: {premises} matches: {toolNames} declarations: {declarations} :: {c.what} :: why: {c.why} :: removal: {c.removal}"

/-- Names executable by the compilation pipeline; gate-only tools are deliberately excluded. -/
def pipelineMatches (cs : List Component := components) : List String :=
  (cs.filter fun c => !(#[ComponentKind.kernel, .axiom, .implementation, .backend, .notTrusted, .runtime, .system, .gateTool].contains c.kind)).flatMap (·.«matches»)

/-- The headline theorems, whose axioms the golden records. -/
def theorems : List Lean.Name := [
  ``Concrete.BIR.decode_encode,
  ``Concrete.BIR.run_progress,
  ``Concrete.BIR.behaviour_diverges_iff,
  ``Concrete.Backend.LCB.Proof.adapter_correct,
  ``Concrete.Backend.LCB.Proof.emit_correct,
  ``Concrete.Backend.LCB.Proof.artifact_of_emit_error,
  ``Concrete.Backend.LCB.Proof.artifact_correct
]

open Lean

def inConcreteLibrary (env : Environment) (localPrefix : Option Name) (n : Name) : Bool :=
  match env.getModuleIdxFor? n with
  | some i => (env.header.moduleNames[i.toNat]?.map (·.getRoot)) == some `Concrete
  | none => match localPrefix with
    | some p => p.isPrefixOf ((privateToUserName? n).getD n)
    | none => false

def concreteDeclarations (env : Environment) (localPrefix : Option Name := none) :
    Array (Name × ConstantInfo) := Id.run do
  let mut out := #[]
  for (n, ci) in env.constants.toList do
    if inConcreteLibrary env localPrefix n then out := out.push (n, ci)
  return out.qsort fun a b => a.1.toString < b.1.toString

def allowedAxioms (cs : List Component := components) : List Name :=
  (cs.filter (·.kind == .axiom)).flatMap (·.«matches») |>.map (·.toName)

/-- Every axiom in every Concrete declaration's transitive closure must have an axiom component. -/
def axiomErrors (env : Environment) (localPrefix : Option Name := none)
    (cs : List Component := components) : CoreM (List String) := do
  let allowed := allowedAxioms cs
  let mut errors := []
  for (n, _) in concreteDeclarations env localPrefix do
    for ax in ← collectAxioms n do
      unless allowed.contains ax do errors := errors ++ [s!"unlisted axiom {ax} in {n}"]
  return errors

/-- A total recursive parent's compiler-recorded recursion tag licenses its generated
runtime twin. A suffix or declaration shape alone is never evidence of generation. -/
def isGeneratedPartialTwin (env : Environment) (n : Name) : Bool :=
  match Compiler.isUnsafeRecName? n with
  | some parent => Meta.recExt.isTagged env parent &&
      (env.find? n).any (·.isPartial)
  | none => false

def exprHasPartialLoop (e : Expr) : Bool :=
  (e.find? fun sub =>
    sub.isConstOf ``Lean.Loop.forIn || sub.isConstOf ``Lean.Loop || sub.isConstOf ``Lean.Loop.mk).isSome

def appendUnique (xs : List String) (ys : List String) : List String :=
  ys.foldl (fun acc y => if acc.contains y then acc else acc ++ [y]) xs

def isAuxOf (n c : Name) : Bool :=
  let uN := (privateToUserName? n).getD n
  let uC := (privateToUserName? c).getD c
  c != n && uN != uC && uN.isPrefixOf uC

/-- Every trust-relevant declaration fact read from the elaborated environment.
Adding a constructor requires handling it in `name`, `all`, `holds` and `moduleAttributedNames`.
See `scripts/checks/compiler/README.md` for the Lean 4.34.1 toolchain extension-point audit
from which this inventory is derived. -/
inductive TrustedFact where
  | extern
  | implementedBy
  | csimp
  | cpass
  | init
  | builtinInit
  | exportSym
  | «unsafe»
  | «partial»
  | «opaque»
  | partialLoop
  deriving Repr, DecidableEq

def TrustedFact.name : TrustedFact → String
  | .extern => "extern"
  | .implementedBy => "implemented_by"
  | .csimp => "csimp"
  | .cpass => "cpass"
  | .init => "init"
  | .builtinInit => "builtin_init"
  | .exportSym => "export"
  | .«unsafe» => "unsafe"
  | .«partial» => "partial"
  | .«opaque» => "opaque"
  | .partialLoop => "partial-loop"

def TrustedFact.all : List TrustedFact := [
  .extern, .implementedBy, .csimp, .cpass, .init, .builtinInit, .exportSym,
  .«unsafe», .«partial», .«opaque», .partialLoop
]

theorem TrustedFact.mem_all (f : TrustedFact) : f ∈ TrustedFact.all := by
  cases f <;> decide

def hasCSimpFact (env : Environment) (n : Name) : Bool :=
  Compiler.hasCSimpAttribute env n ||
  (Compiler.CSimp.ext.ext.getState env).stateStack.any (·.state.thmNames.contains n) ||
  (Compiler.CSimp.ext.ext.getState env).newEntries.any (fun | .global e | .scoped _ e => e.thmName == n) ||
  match env.getModuleIdxFor? n with
  | some m => (Compiler.CSimp.ext.ext.getModuleEntries env m).any (fun | .global e | .scoped _ e => e.thmName == n)
  | none => false

def hasCPassFact (env : Environment) (n : Name) : Bool :=
  (Compiler.LCNF.passManagerExt.getState env).1.contains n ||
  (Compiler.LCNF.passManagerExt.toEnvExtension.getState env).importedEntries.any (·.contains n) ||
  (Compiler.LCNF.Simp.ConstantFold.folderExt.getState env).1.any
    (fun e => e.declName == n || e.folderDeclName == n) ||
  (Compiler.LCNF.Simp.ConstantFold.folderExt.toEnvExtension.getState env).importedEntries.any
    (·.any fun e => e.declName == n || e.folderDeclName == n)

def TrustedFact.holds (env : Environment) (n : Name) (ci : ConstantInfo)
    (extraCSimp : NameSet := {}) : TrustedFact → Bool
  | .extern => isExtern env n
  | .implementedBy => (Compiler.implementedByAttr.getParam? env n).isSome
  | .csimp => hasCSimpFact env n || extraCSimp.contains n
  | .cpass => hasCPassFact env n
  | .init => (regularInitAttr.getParam? env n).isSome
  | .builtinInit => (builtinInitAttr.getParam? env n).isSome
  | .exportSym => (getExportNameFor? env n).isSome
  | .«unsafe» => !isGeneratedPartialTwin env n && ci.isUnsafe
  | .«partial» => !isGeneratedPartialTwin env n &&
      (ci.isPartial || ((ci matches .opaqueInfo _) && (env.find? (Compiler.mkUnsafeRecName n)).any (·.isPartial)))
  | .«opaque» => !isGeneratedPartialTwin env n &&
      (ci matches .opaqueInfo _) && !(env.find? (Compiler.mkUnsafeRecName n)).any (·.isPartial)
  | .partialLoop => ci.value?.any exprHasPartialLoop

def TrustedFact.moduleAttributedNames (env : Environment) : TrustedFact → List Name
  | .extern => (externAttr.ext.getState env).1
  | .implementedBy => (Compiler.implementedByAttr.ext.getState env).1
  | .csimp =>
      let active := (Compiler.CSimp.ext.getState env).thmNames.foldStage2 (fun acc n _ => n :: acc) []
      let entries := (Compiler.CSimp.ext.ext.getState env).newEntries.map fun
        | .global e | .scoped _ e => e.thmName
      active ++ entries
  | .cpass =>
      (Compiler.LCNF.passManagerExt.getState env).1 ++
      (Compiler.LCNF.Simp.ConstantFold.folderExt.getState env).1.flatMap
        (fun e => [e.declName, e.folderDeclName])
  | .init => (regularInitAttr.ext.getState env).1
  | .builtinInit => (builtinInitAttr.ext.getState env).1
  | .exportSym => (exportAttr.ext.getState env).1
  | .«unsafe» | .«partial» | .«opaque» | .partialLoop => []

def moduleAttributedDeclarations (env : Environment) : List Name :=
  TrustedFact.all.flatMap (·.moduleAttributedNames env)

def directTrustedDeclarationKinds (env : Environment) (n : Name) (ci : ConstantInfo)
    (extraCSimp : NameSet := {}) : List String :=
  (TrustedFact.all.filter (·.holds env n ci extraCSimp)).map TrustedFact.name

def trustedDeclarationKinds (env : Environment) (n : Name) (ci : ConstantInfo)
    (extraCSimp : NameSet := {}) : CoreM (List String) := do
  let mut kinds := directTrustedDeclarationKinds env n ci extraCSimp
  let modIdx := env.getModuleIdxFor? n
  let mut queue : Array (Name × ConstantInfo) := #[]
  let mut seen : NameSet := NameSet.empty.insert n
  if let some v := ci.value? then
    for c in v.getUsedConstants do
      if !seen.contains c && env.getModuleIdxFor? c == modIdx then
        seen := seen.insert c
        if let some cCi := env.find? c then
          queue := queue.push (c, cCi)
  let mut idx := 0
  -- Each constant is enqueued once. The environment cardinality is therefore a
  -- structural bound on this finite graph, not a resource budget. One extra
  -- iteration observes the empty queue. Fail explicitly if that invariant breaks.
  let constants := env.constants
  let cardinality := constants.map₁.size + constants.map₂.toList.length
  for _ in [:cardinality + 1] do
    if h : idx < queue.size then
      let (c, cCi) := queue[idx]
      idx := idx + 1
      let cKinds := directTrustedDeclarationKinds env c cCi extraCSimp
      if cKinds.contains "partial-loop" then
        kinds := appendUnique kinds ["partial-loop"]
      if isAuxOf n c || c.isInternalDetail || isPrivateName c then
        kinds := appendUnique kinds cKinds
      if let some v := cCi.value? then
        for next in v.getUsedConstants do
          if !seen.contains next && env.getModuleIdxFor? next == modIdx then
            seen := seen.insert next
            if let some nextCi := env.find? next then
              queue := queue.push (next, nextCi)
    else return kinds
  throwError "trusted declaration closure exhausted environment cardinality with pending constants"

def trustedDeclarations (env : Environment) (localPrefix : Option Name := none) :
    CoreM (Array (Name × List String)) := do
  let mut out := #[]
  for (n, ci) in concreteDeclarations env localPrefix do
    let kinds ← trustedDeclarationKinds env n ci
    unless kinds.isEmpty do out := out.push (n, kinds)
  return out

def declarationOwners (n : Name) (cs : List Component) (env : Option Environment := none) : List Component :=
  let user := (privateToUserName? n).getD n
  let direct := cs.filter fun c => c.declarations.contains n.toString || c.declarations.contains user.toString
  if !direct.isEmpty then direct
  else match env, Compiler.isUnsafeRecName? n with
    | some e, some parent =>
        if (e.find? n).any (·.isPartial) && (e.find? parent).isSome then
          let pUser := (privateToUserName? parent).getD parent
          cs.filter fun c => c.declarations.contains parent.toString || c.declarations.contains pUser.toString
        else []
    | _, _ => []

/-- Every compiled escape hatch must have exactly one owner, and not a non-trusted component. -/
def trustedDeclarationErrors (env : Environment) (localPrefix : Option Name := none)
    (cs : List Component := components) : CoreM (List String) := do
  let mut errors := []
  let trusted ← trustedDeclarations env localPrefix
  for (n, kinds) in trusted do
    match declarationOwners n cs (some env) with
    | [] => errors := errors ++ [s!"unlisted trusted declaration {n} [{", ".intercalate kinds}]"]
    | [owner] =>
        if !(#[ComponentKind.implementation, .backend, .runtime].contains owner.kind) then
          errors := errors ++ [s!"trusted declaration {n} belongs to non-code component {owner.id}"]
    | owners =>
        errors := errors ++ [s!"trusted declaration {n} is owned by {owners.length} components, expected exactly one"]
  for c in cs do
    for m in c.declarations do
      unless trusted.any (fun (n, _) =>
          n.toString == m || ((privateToUserName? n).getD n).toString == m) do
        errors := errors ++ [s!"stale trusted declaration {m} in component {c.id}"]
  return errors

/-- The trusted-base report: components, premises, each theorem's sorted axioms, then each axiom
    once with its type (so an axiom restated under the same name changes the report). -/
def report (thms : List Name) : CoreM (List String) := do
  unless registryErrors.isEmpty do
    throwError "trusted-component registry is inconsistent: {registryErrors}"
  let mut lines := components.map Component.line ++ all.map Premise.line
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

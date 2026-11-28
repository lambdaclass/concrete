# shellcheck shell=bash
# Reusable dual-route native build, pinned replay, process observation,
# checked-publication verification, and fault-injection helpers extending
# scripts/checks/lib/slice0_native.sh.
#
# Callers source gate_env.sh and bounded.sh first, then source this file and
# call `dual_route_init ROOT_DIR WORK_DIR`.
#
# Every subprocess runs under bounded_run and limited_exec.sh. No multi-name
# declaration uses an expansion initializer (scripts/checks/shell_assignments.sh).

dual_route_init() {
  local root_arg=$1
  local work_arg=$2
  DUAL_ROUTE_ROOT=$(cd -- "$root_arg" && pwd -P)
  DUAL_ROUTE_WORK=$(cd -- "$work_arg" && pwd -P)
  DUAL_ROUTE_NATIVE="$DUAL_ROUTE_ROOT/scripts/checks/lib/slice0_native.sh"
  DUAL_ROUTE_MANIFEST_PY="$DUAL_ROUTE_ROOT/scripts/checks/lib/test_manifest.py"
  DUAL_ROUTE_LIMITED_EXEC="$DUAL_ROUTE_ROOT/scripts/checks/lib/limited_exec.sh"
  DUAL_ROUTE_CLI=${CONCRETE_DUAL_ROUTE_CLI:-"$DUAL_ROUTE_ROOT/.lake/build/bin/concrete"}
  DUAL_ROUTE_LCB="$DUAL_ROUTE_ROOT/.lake/packages/lean-compiler-backend/.lake/build/bin/lcb"
  DUAL_ROUTE_EMIT_SECS=${DUAL_ROUTE_EMIT_SECS:-120}
  DUAL_ROUTE_NATIVE_SECS=${DUAL_ROUTE_NATIVE_SECS:-300}
  DUAL_ROUTE_RUN_SECS=${DUAL_ROUTE_RUN_SECS:-10}
  DUAL_ROUTE_RUN_RETRY=${DUAL_ROUTE_RUN_RETRY:-20}
  DUAL_ROUTE_CAPPED=(bash "$DUAL_ROUTE_LIMITED_EXEC" 65536)

  : "${CONCRETE_SLICE0_CLANG:?run inside nix develop}"
  : "${CONCRETE_SLICE0_REFERENCE_CLANG:?run inside nix develop}"
  [[ "$CONCRETE_SLICE0_CLANG" = /* ]] || {
    echo "slice0_dual_route: CONCRETE_SLICE0_CLANG must be an absolute path" >&2
    return 2
  }
  [[ "$CONCRETE_SLICE0_REFERENCE_CLANG" = /* ]] || {
    echo "slice0_dual_route: CONCRETE_SLICE0_REFERENCE_CLANG must be an absolute path" >&2
    return 2
  }
  DUAL_ROUTE_TARGET=$(bash "$DUAL_ROUTE_NATIVE" target)

  # Small POSIX waitpid observer: runs one target executable with RLIMIT_CORE=0
  # and optional RLIMIT_AS, waits on the child directly, and records normal exit
  # status and terminating signal separately without collapsing signals into 128+S.
  DUAL_ROUTE_OBSERVER_PY="$DUAL_ROUTE_WORK/dual_route_observe.py"
  cat >"$DUAL_ROUTE_OBSERVER_PY" <<'PYTHON'
import os
import platform
import resource
import sys


def main(argv):
    try:
        sep = argv.index("--")
    except ValueError:
        print("dual_route_observe: missing --", file=sys.stderr)
        return 2
    opts = argv[:sep]
    cmd = argv[sep + 1:]
    if len(opts) != 4 or opts[0] != "--meta" or opts[2] != "--mem-mb" or not cmd:
        print("dual_route_observe: usage: --meta FILE --mem-mb INT -- EXE [ARGS...]", file=sys.stderr)
        return 2
    meta = opts[1]
    try:
        mem_mb = int(opts[3])
    except ValueError:
        print(f"dual_route_observe: invalid mem-mb {opts[3]!r}", file=sys.stderr)
        return 2
    if mem_mb < 0:
        print(f"dual_route_observe: negative mem-mb {mem_mb}", file=sys.stderr)
        return 2

    try:
        os.unlink(meta)
    except FileNotFoundError:
        pass

    sys.stdout.flush()
    sys.stderr.flush()

    r_fd, w_fd = os.pipe()
    os.set_inheritable(r_fd, False)
    os.set_inheritable(w_fd, False)
    pid = os.fork()
    if pid == 0:
        os.close(r_fd)
        try:
            resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
            if platform.system() == "Linux" and mem_mb > 0:
                limit = mem_mb * 1024 * 1024
                _, hard = resource.getrlimit(resource.RLIMIT_AS)
                if hard != resource.RLIM_INFINITY:
                    limit = min(limit, hard)
                resource.setrlimit(resource.RLIMIT_AS, (limit, limit))
            os.execv(cmd[0], cmd)
        except BaseException as exc:
            msg = f"{type(exc).__name__}: {exc}".encode("utf-8", "replace")
            try:
                os.write(w_fd, msg)
            except OSError:
                pass
            os.close(w_fd)
            os._exit(127)

    os.close(w_fd)
    err_chunks = []
    while True:
        try:
            chunk = os.read(r_fd, 4096)
        except InterruptedError:
            continue
        if not chunk:
            break
        err_chunks.append(chunk)
    os.close(r_fd)

    while True:
        try:
            _, wstatus = os.waitpid(pid, 0)
            break
        except InterruptedError:
            continue

    err_bytes = b"".join(err_chunks)
    if err_bytes:
        print(f"dual_route_observe: exec failed: {err_bytes.decode('utf-8', 'replace')}", file=sys.stderr)
        return 126

    if os.WIFEXITED(wstatus):
        record = f"exit\t{os.WEXITSTATUS(wstatus)}\tnone\n"
    elif os.WIFSIGNALED(wstatus):
        record = f"signal\tnone\t{os.WTERMSIG(wstatus)}\n"
    else:
        print(f"dual_route_observe: unexpected wait status {wstatus}", file=sys.stderr)
        return 125

    tmp_meta = f"{meta}.{os.getpid()}.tmp"
    with open(tmp_meta, "w", encoding="utf-8") as fh:
        fh.write(record)
    os.replace(tmp_meta, meta)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
PYTHON

  DUAL_ROUTE_EMITTER_LEAN="$DUAL_ROUTE_WORK/dual_route_emit.lean"
  cat >"$DUAL_ROUTE_EMITTER_LEAN" <<'LEAN'
import Concrete.Stages
import Concrete.Driver.Launcher

open Concrete Concrete.Backend

private def hasSubstr (haystack needle : String) : Bool :=
  !needle.isEmpty && (haystack.splitOn needle).length > 1

private def birOvfCount (bir : BIR.Program) : Nat :=
  bir.blocks.foldl (fun acc b =>
    acc + b.insts.foldl (fun n ins =>
      match ins with
      | .binOvf .. => n + 1
      | _ => n) 0) 0

private def birTrapCount (bir : BIR.Program) : Nat :=
  bir.blocks.foldl (fun acc b =>
    match b.term with
    | .trap _ => acc + 1
    | _ => acc) 0

private def allCoreIntTys : List ConcreteSpec.Core.IntTy :=
  [.i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64]

private def allLLVMArith : List LLVM.Arithmetic := [.add, .sub, .mul]

/-- Derive each LLVM overflow intrinsic symbol from LLVMIR's own renderer. -/
private def overflowIntrinsicSymbol (op : LLVM.Arithmetic) (ty : ConcreteSpec.Core.IntTy) : String :=
  let w := LLVM.Width.ofCore ty
  let decl := LLVM.render [.declare (.overflowPair w) (.overflow op ty) [.integer w, .integer w]]
  match decl.splitOn "@" with
  | [_, rest] =>
    match rest.splitOn "(" with
    | sym :: _ => "@" ++ sym
    | _ => ""
  | _ => ""

private def allOverflowIntrinsicSymbols : List String :=
  allLLVMArith.flatMap fun op => allCoreIntTys.map fun ty => overflowIntrinsicSymbol op ty

private def llvmSitePrefix : String :=
  let line := LLVM.render [.constant (.site 0) ByteArray.empty]
  match line.splitOn "0 =" with
  | pfx :: _ => pfx
  | _ => ""

private def llvmConstantBytesToken (bytes : ByteArray) : String :=
  let line := LLVM.render [.constant .bad bytes]
  match line.splitOn "] " with
  | [_, rest] =>
    match rest.splitOn "\n" with
    | tok :: _ => tok
    | _ => ""
  | _ => ""

private def qbeSitePrefix : String :=
  let rendered := Runtime.qbeDiagnostics ["x\n"]
  match rendered.splitOn "0 =" with
  | pfx :: _ => pfx
  | _ => ""

private def countLLVMOverflowDecls (text : String) : Nat :=
  (text.splitOn "\n").foldl (fun acc line =>
    if line.startsWith "declare " &&
        allOverflowIntrinsicSymbols.any (fun sym => hasSubstr line (sym ++ "(")) then
      acc + 1
    else acc) 0

private def countLLVMOverflowCalls (text : String) : Nat :=
  (text.splitOn "\n").foldl (fun acc line =>
    if hasSubstr line "call " &&
        allOverflowIntrinsicSymbols.any (fun sym => hasSubstr line (sym ++ "(")) then
      acc + 1
    else acc) 0

private def countLLVMSiteGlobals (text : String) : Nat :=
  (text.splitOn "\n").foldl (fun acc line =>
    if !llvmSitePrefix.isEmpty && line.startsWith llvmSitePrefix then acc + 1 else acc) 0

private def countQBESiteGlobals (text : String) : Nat :=
  (text.splitOn "\n").foldl (fun acc line =>
    if !qbeSitePrefix.isEmpty && line.startsWith qbeSitePrefix then acc + 1 else acc) 0

private def siteMatchesEntry (s : Elab.SourceSite) (e : BIR.SiteEntry) : Bool :=
  e.span.start == s.operator.start && e.span.stop == s.operator.stop &&
    e.loc.line == s.operator.line && e.loc.col == s.operator.column

private def checkDerivedMarkers : IO Bool := do
  if llvmSitePrefix.isEmpty || qbeSitePrefix.isEmpty ||
      allOverflowIntrinsicSymbols.any (fun s => s.isEmpty) then
    IO.eprintln "INTERNAL: failed to derive LLVM/LCB backend markers from renderers"
    return false
  return true

private def writeBackendInputs (bir : BIR.Program) (outDir targetName backend : String) : IO UInt32 := do
  unless BIR.wf bir do
    IO.eprintln "INTERNAL: emitted BIR is not well formed"
    return 4
  let dir := System.FilePath.mk outDir
  IO.FS.createDirAll dir
  if backend != "lcb" then
    let some pair := Driver.Launcher.prepareLLVM bir
      | IO.eprintln "INTERNAL: invalid LLVM link inputs"; return 4
    unless pair.matches pair.component pair.launcher do
      IO.eprintln "INTERNAL: LLVM launcher pair failed self-match"
      return 4
    IO.FS.writeFile (dir / "component.ll") pair.component
    IO.FS.writeFile (dir / "launcher.ll") pair.launcher
  if backend != "llvm" then
    let some target := LCB.Target.ofName targetName
      | IO.eprintln s!"INTERNAL: unknown LCB target {targetName}"; return 4
    let f ← match Stages.prepareLCB bir with
      | .ok f => pure f
      | .error refusal => IO.eprintln refusal.render; return refusal.exitCode
    if let .error refusal := LCB.assemble .aarch64Macos f then
      IO.eprintln s!"LCB first path: {refusal.render}"
      return refusal.exitCode
    let asm ← match LCB.assemble target f with
      | .ok asm => pure asm
      | .error refusal => IO.eprintln refusal.render; return refusal.exitCode
    IO.FS.writeFile (dir / "lcb_body.s") asm
    IO.FS.writeFile (dir / "lcb_rt.ssa") (Runtime.qbe bir.sites)
  return 0

private def emitOne (route relPath srcPath outDir targetName backend : String) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes
    | IO.eprintln s!"INTERNAL: invalid project path: {relPath}"; return 4
  let compiled : Except Refusal BIR.Program := match route with
    | "checked" => (Stages.compile input).map (·.bir)
    | "unchecked" => Stages.unchecked input
    | _ => .error (.usageError s!"unknown route {route}")
  match compiled with
  | .error refusal =>
    IO.eprintln refusal.render
    return refusal.exitCode
  | .ok bir =>
    writeBackendInputs bir outDir targetName backend

/-- Negative-control emitter reproducing the reviewer's mutation: keep checked
admission (`Stages.compile`), then emit unchecked lowering (`Stages.unchecked`)
for the selected backend(s) after admission succeeds. -/
private def emitUncheckedAfterAdmission (mode relPath srcPath outDir targetName : String) : IO UInt32 := do
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes
    | IO.eprintln s!"INTERNAL: invalid project path: {relPath}"; return 4
  let checkedOut ← match Stages.compile input with
    | .ok out => pure out
    | .error refusal =>
      IO.eprintln refusal.render
      return refusal.exitCode
  let uncheckedBir ← match Stages.unchecked input with
    | .ok bir => pure bir
    | .error refusal =>
      IO.eprintln refusal.render
      return refusal.exitCode
  let (llvmBir, lcbBir) ← match mode with
    | "both" => pure (uncheckedBir, uncheckedBir)
    | "llvm" => pure (uncheckedBir, checkedOut.bir)
    | "lcb" => pure (checkedOut.bir, uncheckedBir)
    | _ =>
      IO.eprintln s!"emit-unchecked-after-admission: unknown mode {mode}"
      return 2
  let rc1 ← writeBackendInputs llvmBir outDir targetName "llvm"
  if rc1 != 0 then return rc1
  writeBackendInputs lcbBir outDir targetName "lcb"

private def readRequiredFile (context : String) (path : System.FilePath) : IO (Option String) := do
  try
    return some (← IO.FS.readFile path)
  catch e =>
    IO.eprintln s!"{context}: cannot read {path}: {e}"
    return none

/-- Verify a checked publication directory against admission's own discharged set
and the independently emitted checked backend inputs. -/
private def verifyChecked (relPath srcPath pubDir targetName backend : String) : IO UInt32 := do
  unless ← checkDerivedMarkers do return 4
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes
    | IO.eprintln s!"verify-checked: invalid project path: {relPath}"; return 1
  let out ← match Stages.compile input with
    | .ok o => pure o
    | .error r => IO.eprintln s!"verify-checked: checked compilation failed: {r.render}"; return 1
  let admitted ← match Admission.check out.core with
    | .ok a => pure a
    | .error r => IO.eprintln s!"verify-checked: Admission.check failed: {r.render}"; return 1
  unless admitted.tokens == out.evidence do
    IO.eprintln "verify-checked: Stages.compile evidence differs from Admission.check output"
    return 1
  let allSites := out.core.sites
  let dischargedBindings := admitted.tokens.map (·.binding)
  unless dischargedBindings.eraseDups.length == dischargedBindings.length do
    IO.eprintln "verify-checked: duplicate binding in Admission.check tokens"
    return 1
  unless dischargedBindings.all (fun v => allSites.any (fun s => s.binding == v)) do
    IO.eprintln "verify-checked: Admission.check token does not match any elaborated addition site"
    return 1
  let dischargedSites := allSites.filter (fun s => dischargedBindings.contains s.binding)
  let residualSites := allSites.filter (fun s => !(dischargedBindings.contains s.binding))

  let uncheckedBir ← match Stages.unchecked input with
    | .ok b => pure b
    | .error r => IO.eprintln s!"verify-checked: unchecked baseline failed: {r.render}"; return 1
  let dischargedEntries := uncheckedBir.sites.filter (fun e => dischargedSites.any (fun s => siteMatchesEntry s e))
  unless dischargedEntries.length == dischargedSites.length do
    IO.eprintln s!"verify-checked: expected {dischargedSites.length} discharged site(s) in unchecked baseline, found {dischargedEntries.length}"
    return 1

  unless BIR.wf out.bir do
    IO.eprintln "verify-checked: admitted BIR is not well formed"
    return 1
  let ovfBir := birOvfCount out.bir
  let trapBir := birTrapCount out.bir
  unless ovfBir == residualSites.length && trapBir == residualSites.length &&
      out.bir.sites.length == residualSites.length do
    IO.eprintln s!"verify-checked: admitted BIR has binOvf={ovfBir}, trap={trapBir}, sites={out.bir.sites.length}, expected {residualSites.length} residual site(s) ({dischargedSites.length} discharged)"
    return 1
  if out.bir.sites.any (fun e => dischargedSites.any (fun s => siteMatchesEntry s e)) then
    IO.eprintln "verify-checked: admitted BIR retained a trap site discharged by admission"
    return 1

  let dir := System.FilePath.mk pubDir
  let mut passed := true
  if backend != "lcb" then
    match ← readRequiredFile "verify-checked" (dir / "component.ll"),
          ← readRequiredFile "verify-checked" (dir / "launcher.ll") with
    | some pubComp, some pubLaunch =>
      let ovfCalls := countLLVMOverflowCalls pubComp
      let ovfDecls := countLLVMOverflowDecls pubComp
      if ovfCalls != residualSites.length then
        IO.eprintln s!"verify-checked: published component.ll has {ovfCalls} overflow intrinsic call(s), expected {residualSites.length} (admission discharged {dischargedSites.length} of {allSites.length} site(s))"
        passed := false
      if residualSites.isEmpty && ovfDecls != 0 then
        IO.eprintln s!"verify-checked: published component.ll declares {ovfDecls} overflow intrinsic(s) despite all {dischargedSites.length} site(s) being discharged"
        passed := false
      let launchSites := countLLVMSiteGlobals pubLaunch
      if launchSites != residualSites.length then
        IO.eprintln s!"verify-checked: published launcher.ll has {launchSites} trap site(s), expected {residualSites.length} (admission discharged {dischargedSites.length} of {allSites.length} site(s))"
        passed := false
      for entry in dischargedEntries do
        let tok := llvmConstantBytesToken (Runtime.trapLine entry ++ "\n").toUTF8
        if hasSubstr pubLaunch tok then
          IO.eprintln s!"verify-checked: published launcher.ll retained discharged trap diagnostic for site {entry.site.id}"
          passed := false
      match Driver.Launcher.prepareLLVM out.bir with
      | none =>
        IO.eprintln "verify-checked: invalid checked LLVM link inputs"
        return 4
      | some pair =>
        unless pubComp == pair.component do
          IO.eprintln "verify-checked: published component.ll differs from independently emitted checked LLVM component"
          passed := false
        unless pubLaunch == pair.launcher do
          IO.eprintln "verify-checked: published launcher.ll differs from independently emitted checked LLVM launcher"
          passed := false
    | _, _ => passed := false

  if backend != "llvm" then
    let some target := LCB.Target.ofName targetName
      | IO.eprintln s!"verify-checked: unknown LCB target {targetName}"; return 4
    match ← readRequiredFile "verify-checked" (dir / "lcb_body.s"),
          ← readRequiredFile "verify-checked" (dir / "lcb_rt.ssa") with
    | some pubBody, some pubRt =>
      let lcbSites := countQBESiteGlobals pubRt
      if lcbSites != residualSites.length then
        IO.eprintln s!"verify-checked: published lcb_rt.ssa has {lcbSites} trap site(s), expected {residualSites.length} (admission discharged {dischargedSites.length} of {allSites.length} site(s))"
        passed := false
      for entry in dischargedEntries do
        let qbytes := Runtime.qbeBytes (Runtime.trapLine entry ++ "\n")
        if hasSubstr pubRt qbytes then
          IO.eprintln s!"verify-checked: published lcb_rt.ssa retained discharged trap diagnostic for site {entry.site.id}"
          passed := false
      let f ← match Stages.prepareLCB out.bir with
        | .ok f => pure f
        | .error r => IO.eprintln s!"verify-checked: checked LCB prepare failed: {r.render}"; return 1
      let expectedAsm ← match LCB.assemble target f with
        | .ok asm => pure asm
        | .error r => IO.eprintln s!"verify-checked: checked LCB assemble failed: {r.render}"; return 1
      let expectedRt := Runtime.qbe out.bir.sites
      unless pubRt == expectedRt do
        IO.eprintln "verify-checked: published lcb_rt.ssa differs from independently emitted checked LCB runtime"
        passed := false
      unless pubBody == expectedAsm do
        IO.eprintln "verify-checked: published lcb_body.s differs from independently emitted checked LCB body"
        passed := false
    | _, _ => passed := false
  return if passed then 0 else 1

private def verifyUnchecked (relPath srcPath stageDir targetName backend : String) : IO UInt32 := do
  unless ← checkDerivedMarkers do return 4
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes
    | IO.eprintln s!"verify-unchecked: invalid project path: {relPath}"; return 1
  let tokens ← match Lex.lex input.source input.fileId with
    | .ok t => pure t
    | .error r => IO.eprintln s!"verify-unchecked: lex failed: {r.render}"; return 1
  let ast ← match Parse.parseForDriver input.eof tokens with
    | .ok a => pure a
    | .error r => IO.eprintln s!"verify-unchecked: parse failed: {r.render}"; return 1
  let core ← match Admission.prepareParsed .eligibility ast with
    | .ok c => pure c
    | .error r => IO.eprintln s!"verify-unchecked: elab failed: {r.render}"; return 1
  let expectedSites := core.sites.length
  let bir ← match Stages.unchecked input with
    | .ok b => pure b
    | .error r => IO.eprintln s!"verify-unchecked: unchecked compile failed: {r.render}"; return 1
  unless BIR.wf bir && bir.sites.length == expectedSites &&
      birOvfCount bir == expectedSites && birTrapCount bir == expectedSites do
    IO.eprintln s!"verify-unchecked: unchecked BIR site/check counts do not match {expectedSites} elaborated addition site(s)"
    return 1
  let dir := System.FilePath.mk stageDir
  if backend != "lcb" then
    let stageComp ← IO.FS.readFile (dir / "component.ll")
    let stageLaunch ← IO.FS.readFile (dir / "launcher.ll")
    let ovfCalls := countLLVMOverflowCalls stageComp
    let launchSites := countLLVMSiteGlobals stageLaunch
    unless ovfCalls == expectedSites && launchSites == expectedSites do
      IO.eprintln s!"verify-unchecked: LLVM stage has {ovfCalls} overflow call(s) and {launchSites} trap site(s), expected {expectedSites}"
      return 1
    let some pair := Driver.Launcher.prepareLLVM bir
      | IO.eprintln "verify-unchecked: invalid unchecked LLVM link inputs"; return 4
    unless stageComp == pair.component && stageLaunch == pair.launcher do
      IO.eprintln "verify-unchecked: LLVM stage differs from independently emitted unchecked inputs"
      return 1
  if backend != "llvm" then
    let some target := LCB.Target.ofName targetName
      | IO.eprintln s!"verify-unchecked: unknown LCB target {targetName}"; return 4
    let stageBody ← IO.FS.readFile (dir / "lcb_body.s")
    let stageRt ← IO.FS.readFile (dir / "lcb_rt.ssa")
    let lcbSites := countQBESiteGlobals stageRt
    unless lcbSites == expectedSites do
      IO.eprintln s!"verify-unchecked: LCB stage has {lcbSites} trap site(s), expected {expectedSites}"
      return 1
    let f ← match Stages.prepareLCB bir with
      | .ok f => pure f
      | .error r => IO.eprintln s!"verify-unchecked: LCB prepare failed: {r.render}"; return 1
    let expectedAsm ← match LCB.assemble target f with
      | .ok asm => pure asm
      | .error r => IO.eprintln s!"verify-unchecked: LCB assemble failed: {r.render}"; return 1
    unless stageRt == Runtime.qbe bir.sites && stageBody == expectedAsm do
      IO.eprintln "verify-unchecked: LCB stage differs from independently emitted unchecked inputs"
      return 1
  return 0

private def verifyRouteSeparation (relPath srcPath checkedDir uncheckedDir targetName : String) : IO UInt32 := do
  let rc1 ← verifyChecked relPath srcPath checkedDir targetName "both"
  if rc1 != 0 then return rc1
  let rc2 ← verifyUnchecked relPath srcPath uncheckedDir targetName "both"
  if rc2 != 0 then return rc2
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes | return 1
  let .ok out := Stages.compile input | return 1
  if out.evidence.isEmpty then
    IO.eprintln "verify-separation: admission discharged 0 sites; route separation would be vacuous"
    return 1
  let cDir := System.FilePath.mk checkedDir
  let uDir := System.FilePath.mk uncheckedDir
  for name in ["component.ll", "launcher.ll", "lcb_body.s", "lcb_rt.ssa"] do
    let cText ← IO.FS.readFile (cDir / name)
    let uText ← IO.FS.readFile (uDir / name)
    if cText == uText then
      IO.eprintln s!"verify-separation: checked and unchecked {name} are identical despite {out.evidence.length} discharged site(s)"
      return 1
  return 0

private def siteLine (relPath srcPath idxStr outPath : String) : IO UInt32 := do
  let some idx := idxStr.toNat?
    | IO.eprintln s!"site-line: invalid site index {idxStr}"; return 2
  let bytes ← IO.FS.readBinFile srcPath
  let .ok input := Stages.Input.load relPath bytes
    | IO.eprintln s!"site-line: invalid project path: {relPath}"; return 1
  let bir ← match Stages.unchecked input with
    | .ok b => pure b
    | .error r => IO.eprintln s!"site-line: unchecked compile failed: {r.render}"; return 1
  let some entry := bir.sites[idx]?
    | IO.eprintln s!"site-line: site index {idx} out of range (size {bir.sites.length})"; return 1
  let line := Runtime.trapLine entry
  if outPath == "-" then
    IO.println line
  else
    IO.FS.writeFile outPath (line ++ "\n")
  return 0

/-- Emit a native LLVM module that writes the exact bytes of `stderrPath` to
fd 2 and terminates via `abort()` (raising SIGABRT). -/
private def emitSigabrtControl (stderrPath outDir : String) : IO UInt32 := do
  let msg ← IO.FS.readBinFile stderrPath
  let i32 := LLVM.TypeCode.integer .u32
  let i64 := LLVM.TypeCode.integer .u64
  let byte := LLVM.TypeCode.integer .u8
  let count : LLVM.Argument := .immediate (.natural (ConcreteSpec.Core.toBits .u64 (Int.ofNat msg.size)))
  let zero := LLVM.Argument.immediate (.natural 0)
  let items : List LLVM.Item :=
    [.constant .bad msg,
     .declare i64 .write [i32, .ptr, i64],
     .declare .void .abort [],
     .blank,
     .define true .void .writeAll [(.ptr, .p), (i64, .n)],
     .label .entry,
     .terminator (.branch .loop),
     .label .loop,
     .instruction (.phi .pp .ptr [(.local .p, .entry), (.local .np, .more)]),
     .instruction (.phi .nn i64 [(.local .n, .entry), (.local .rest, .more)]),
     .instruction (.compare .done .eq i64 (.local .nn) zero),
     .terminator (.cond .done .out .wr),
     .label .wr,
     .instruction (.call (some .k) i64 .write
       [(i32, .immediate (.natural 2)), (.ptr, .local .pp), (i64, .local .nn)]),
     .instruction (.compare .bad .sle i64 (.local .k) zero),
     .terminator (.cond .bad .out .more),
     .label .more,
     .instruction (.gep .np byte (.ptr, .local .pp) (i64, .local .k)),
     .instruction (.bin .rest .sub i64 (.local .nn) (.local .k)),
     .terminator (.branch .loop),
     .label .out,
     .terminator .retVoid,
     .endFunction,
     .blank,
     .define false i32 .main [],
     .label .entry,
     .instruction (.call none .void .writeAll [(.ptr, .global .bad), (i64, count)]),
     .instruction (.call none .void .abort []),
     .terminator .unreachable,
     .endFunction]
  let dir := System.FilePath.mk outDir
  IO.FS.createDirAll dir
  IO.FS.writeFile (dir / "llvm.ll") (LLVM.render items)
  return 0

def main (args : List String) : IO UInt32 := do
  match args with
  | ["emit", route, relPath, srcPath, outDir, targetName, backend] =>
    emitOne route relPath srcPath outDir targetName backend
  | ["emit-unchecked-after-admission", mode, relPath, srcPath, outDir, targetName] =>
    emitUncheckedAfterAdmission mode relPath srcPath outDir targetName
  | ["verify-checked", relPath, srcPath, pubDir, targetName, backend] =>
    verifyChecked relPath srcPath pubDir targetName backend
  | ["verify-unchecked", relPath, srcPath, stageDir, targetName, backend] =>
    verifyUnchecked relPath srcPath stageDir targetName backend
  | ["verify-separation", relPath, srcPath, checkedDir, uncheckedDir, targetName] =>
    verifyRouteSeparation relPath srcPath checkedDir uncheckedDir targetName
  | ["site-line", relPath, srcPath, idxStr, outPath] =>
    siteLine relPath srcPath idxStr outPath
  | ["emit-sigabrt", stderrPath, outDir] =>
    emitSigabrtControl stderrPath outDir
  | ["batch", specPath] =>
    let text ← IO.FS.readFile specPath
    for line in text.splitOn "\n" do
      if line.isEmpty then continue
      match line.splitOn "\t" with
      | [route, relPath, srcPath, outDir, targetName, backend, expectedRc] =>
        let rc ← emitOne route relPath srcPath outDir targetName backend
        if toString rc != expectedRc then
          IO.eprintln s!"batch mismatch for {route} {relPath}: expected {expectedRc}, got {rc}"
          return 1
      | _ =>
        IO.eprintln s!"malformed batch spec line: {line}"
        return 2
    return 0
  | ["ops-batch", specPath] =>
    let text ← IO.FS.readFile specPath
    for line in text.splitOn "\n" do
      if line.isEmpty then continue
      let rc ← match line.splitOn "\t" with
        | ["verify-checked", relPath, srcPath, pubDir, targetName, backend] =>
          verifyChecked relPath srcPath pubDir targetName backend
        | ["verify-unchecked", relPath, srcPath, stageDir, targetName, backend] =>
          verifyUnchecked relPath srcPath stageDir targetName backend
        | ["verify-separation", relPath, srcPath, checkedDir, uncheckedDir, targetName] =>
          verifyRouteSeparation relPath srcPath checkedDir uncheckedDir targetName
        | ["site-line", relPath, srcPath, idxStr, outPath] =>
          siteLine relPath srcPath idxStr outPath
        | ["emit-sigabrt", stderrPath, outDir] =>
          emitSigabrtControl stderrPath outDir
        | _ =>
          IO.eprintln s!"malformed ops-batch spec line: {line}"
          pure 2
      if rc != 0 then return rc
    return 0
  | _ =>
    IO.eprintln "usage: dual_route_emit.lean emit|emit-unchecked-after-admission|verify-checked|verify-unchecked|verify-separation|site-line|emit-sigabrt|batch|ops-batch ..."
    return 2
LEAN
}

# dual_route_bounded SECS CMD...
dual_route_bounded() {
  local secs=$1
  local rc=0
  shift
  bounded_run -i /dev/null "$secs" -- "${DUAL_ROUTE_CAPPED[@]}" "$@" || rc=$?
  if [ "$rc" != 0 ] && bounded_is_incomplete "$rc"; then
    echo "slice0_dual_route: INCOMPLETE (timed out twice or lost its runner): $*" >&2
  fi
  return "$rc"
}

# dual_route_emit ROUTE REL_PATH SRC_PATH STAGE_DIR BACKEND
# ROUTE is checked|unchecked; BACKEND is llvm|lcb|both.
dual_route_emit() {
  local route=$1
  local rel_path=$2
  local src_path=$3
  local stage_dir=$4
  local backend=${5:-both}
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      emit "$route" "$rel_path" "$src_path" "$stage_dir" "$DUAL_ROUTE_TARGET" "$backend"
  )
}

# dual_route_emit_batch SPEC_FILE
# Each TSV line: ROUTE  REL_PATH  SRC_PATH  STAGE_DIR  TARGET  BACKEND  EXPECTED_RC
dual_route_emit_batch() {
  local spec_file=$1
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      batch "$spec_file"
  )
}

# dual_route_ops_batch SPEC_FILE
dual_route_ops_batch() {
  local spec_file=$1
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      ops-batch "$spec_file"
  )
}

# dual_route_verify_checked REL_PATH SRC_PATH PUB_DIR [BACKEND]
dual_route_verify_checked() {
  local rel_path=$1
  local src_path=$2
  local pub_dir=$3
  local backend=${4:-both}
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      verify-checked "$rel_path" "$src_path" "$pub_dir" "$DUAL_ROUTE_TARGET" "$backend"
  )
}

# dual_route_verify_unchecked REL_PATH SRC_PATH STAGE_DIR [BACKEND]
dual_route_verify_unchecked() {
  local rel_path=$1
  local src_path=$2
  local stage_dir=$3
  local backend=${4:-both}
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      verify-unchecked "$rel_path" "$src_path" "$stage_dir" "$DUAL_ROUTE_TARGET" "$backend"
  )
}

# dual_route_verify_route_separation REL_PATH SRC_PATH CHECKED_DIR UNCHECKED_DIR
dual_route_verify_route_separation() {
  local rel_path=$1
  local src_path=$2
  local checked_dir=$3
  local unchecked_dir=$4
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      verify-separation "$rel_path" "$src_path" "$checked_dir" "$unchecked_dir" "$DUAL_ROUTE_TARGET"
  )
}

# dual_route_emit_unchecked_after_admission MODE REL_PATH SRC_PATH OUT_DIR
dual_route_emit_unchecked_after_admission() {
  local mode=$1
  local rel_path=$2
  local src_path=$3
  local out_dir=$4
  (
    cd -- "$DUAL_ROUTE_ROOT"
    dual_route_bounded "$DUAL_ROUTE_EMIT_SECS" lake env lean --run "$DUAL_ROUTE_EMITTER_LEAN" \
      emit-unchecked-after-admission "$mode" "$rel_path" "$src_path" "$out_dir" "$DUAL_ROUTE_TARGET"
  )
}

# dual_route_materialize_expect TEST_ID OUT_DIR [MANIFEST]
dual_route_materialize_expect() {
  local test_id=$1
  local out_dir=$2
  local manifest=${3:-"$DUAL_ROUTE_ROOT/tests/manifest.tsv"}
  dual_route_bounded 30 python3 -I "$DUAL_ROUTE_MANIFEST_PY" expect "$test_id" "$out_dir" "$manifest"
}

# dual_route_load_con_row TEST_ID OUT_DIR MANIFEST EXPECTED_KIND
# Materializes the expectation and resolved source from one manifest row and
# verifies the row is a con: source contained strictly inside the manifest's tests/.
dual_route_load_con_row() {
  local test_id=$1
  local out_dir=$2
  local manifest=$3
  local expected_kind=$4
  local program=""
  local expect_kind=""
  local rel_path=""
  local source_path=""
  local project_root=""
  local tests_dir=""
  local cur=""
  local part=""
  local -a parts=()
  dual_route_materialize_expect "$test_id" "$out_dir" "$manifest" >"$out_dir.row" || return 1
  program=$(cat "$out_dir/program") || return 1
  if [[ "$program" != con:* ]]; then
    echo "slice0_dual_route: manifest row '$test_id' must be a con: source, got '$program'" >&2
    return 1
  fi
  expect_kind=$(cat "$out_dir/expect_kind") || return 1
  if [ "$expect_kind" != "$expected_kind" ]; then
    echo "slice0_dual_route: manifest row '$test_id' must have '$expected_kind' expectation, got '$expect_kind'" >&2
    return 1
  fi
  rel_path=$(cat "$out_dir/rel_path") || return 1
  source_path=$(cat "$out_dir/source_path") || return 1
  project_root=$(cat "$out_dir/project_root") || return 1
  tests_dir=$(cat "$out_dir/tests_dir") || return 1
  if [ -z "$rel_path" ] || [ -z "$source_path" ] || [ -z "$project_root" ] || [ -z "$tests_dir" ]; then
    echo "slice0_dual_route: manifest row '$test_id' did not resolve source metadata" >&2
    return 1
  fi
  if [ -L "$tests_dir" ] || [ ! -d "$tests_dir" ] || [ "$(basename -- "$tests_dir")" != tests ]; then
    echo "slice0_dual_route: manifest directory '$tests_dir' must be a real tests/ directory" >&2
    return 1
  fi
  if [ "$source_path" != "$project_root/$rel_path" ] || [[ "$source_path" != "$tests_dir/"* ]]; then
    echo "slice0_dual_route: manifest row '$test_id' source '$source_path' is outside '$tests_dir'" >&2
    return 1
  fi
  if [ -L "$source_path" ] || [ ! -f "$source_path" ]; then
    echo "slice0_dual_route: manifest row '$test_id' source '$source_path' is not a regular file" >&2
    return 1
  fi
  IFS='/' read -r -a parts <<<"$rel_path"
  cur="$project_root"
  for part in "${parts[@]}"; do
    cur="$cur/$part"
    if [ -L "$cur" ]; then
      echo "slice0_dual_route: manifest row '$test_id' traverses symlink '$cur'" >&2
      return 1
    fi
  done
}

# dual_route_snapshot_inputs STAGE BACKEND SNAPSHOT_FILE
# Verifies every expected staged input exists as a nonempty regular non-symlink
# file and records its SHA-256 digest before native compilation.
dual_route_snapshot_inputs() {
  local stage=$1
  local backend=$2
  local snapshot=$3
  local -a inputs=()
  local name
  if [ "$backend" != lcb ]; then
    inputs+=(component.ll launcher.ll)
  fi
  if [ "$backend" != llvm ]; then
    inputs+=(lcb_body.s lcb_rt.ssa)
  fi
  for name in "${inputs[@]}"; do
    if [ -L "$stage/$name" ] || [ ! -f "$stage/$name" ] || [ ! -s "$stage/$name" ]; then
      echo "slice0_dual_route: missing or non-regular staged input: $name" >&2
      return 1
    fi
  done
  (cd -- "$stage" && sha256sum "${inputs[@]}") >"$snapshot"
}

# dual_route_check_inputs STAGE SNAPSHOT_FILE
# Confirms every staged input is byte-for-byte unchanged after native compilation.
dual_route_check_inputs() {
  local stage=$1
  local snapshot=$2
  local expected_hash
  local name
  local actual_hash
  while read -r expected_hash name; do
    [ -n "$name" ] || continue
    if [ -L "$stage/$name" ] || [ ! -f "$stage/$name" ]; then
      echo "slice0_dual_route: staged link input changed during native build: $name" >&2
      return 1
    fi
    actual_hash=$(sha256sum "$stage/$name" | awk '{print $1}')
    if [ "$actual_hash" != "$expected_hash" ]; then
      echo "slice0_dual_route: staged link input changed during native build: $name" >&2
      return 1
    fi
  done <"$snapshot"
}

# dual_route_check_inventory STAGE BACKEND
# Enforces exact canonical link-inventory entry names, order, checksums, and
# runtime-premises receipt.
dual_route_check_inventory() {
  local stage=$1
  local backend=$2
  local expected_premises='Runtime loader and system libraries are unbound environment premises.'
  local actual_list
  local expected_list
  if [ ! -f "$stage/runtime-premises.txt" ] || [ -L "$stage/runtime-premises.txt" ] ||
      ! printf '%s\n' "$expected_premises" | cmp -s - "$stage/runtime-premises.txt"; then
    echo "slice0_dual_route: missing or mismatched runtime-premises.txt" >&2
    return 1
  fi
  if [ "$backend" != lcb ]; then
    expected_list=$(printf '%s\n' component.ll launcher.ll component.o launcher.o llvm.exe)
    actual_list=$(awk '{print $2}' "$stage/llvm.link.sha256")
    if [ "$actual_list" != "$expected_list" ]; then
      echo "slice0_dual_route: unexpected llvm.link.sha256 inventory order" >&2
      return 1
    fi
    (cd -- "$stage" && sha256sum --quiet --strict -c llvm.link.sha256) || return 1
    [ -x "$stage/llvm.exe" ] || { echo "slice0_dual_route: llvm.exe is not executable" >&2; return 1; }
  fi
  if [ "$backend" != llvm ]; then
    expected_list=$(printf '%s\n' lcb_body.s lcb_rt.ssa lcb_body.o lcb_rt.s lcb_rt.o lcb.exe)
    actual_list=$(awk '{print $2}' "$stage/lcb.link.sha256")
    if [ "$actual_list" != "$expected_list" ]; then
      echo "slice0_dual_route: unexpected lcb.link.sha256 inventory order" >&2
      return 1
    fi
    (cd -- "$stage" && sha256sum --quiet --strict -c lcb.link.sha256) || return 1
    [ -x "$stage/lcb.exe" ] || { echo "slice0_dual_route: lcb.exe is not executable" >&2; return 1; }
  fi
}

# dual_route_bound_build STAGE BACKEND LOG_FILE [CANDIDATE_CLANG]
dual_route_bound_build() {
  local stage=$1
  local backend=$2
  local log_file=$3
  local candidate_clang=${4:-"$CONCRETE_SLICE0_CLANG"}
  local lcb_arg="-"
  local -a backend_args=()
  if [ "$backend" != llvm ]; then
    lcb_arg="$DUAL_ROUTE_LCB"
  fi
  if [ "$backend" != both ]; then
    backend_args=("$backend")
  fi
  CONCRETE_SLICE0_CLANG="$candidate_clang" \
    dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    bash "$DUAL_ROUTE_NATIVE" bound "$stage" "$lcb_arg" \
    ${backend_args[@]+"${backend_args[@]}"} >"$log_file" 2>&1
}

# dual_route_verify_build STAGE BACKEND LOG_FILE
dual_route_verify_build() {
  local stage=$1
  local backend=$2
  local log_file=$3
  local lcb_arg="-"
  local -a backend_args=()
  if [ "$backend" != llvm ]; then
    lcb_arg="$DUAL_ROUTE_LCB"
  fi
  if [ "$backend" != both ]; then
    backend_args=("$backend")
  fi
  dual_route_bounded "$DUAL_ROUTE_NATIVE_SECS" \
    bash "$DUAL_ROUTE_NATIVE" verify "$stage" "$lcb_arg" \
    ${backend_args[@]+"${backend_args[@]}"} >"$log_file" 2>&1
}

# dual_route_build_and_verify STAGE BACKEND LOG_PREFIX [CANDIDATE_CLANG]
dual_route_build_and_verify() {
  local stage=$1
  local backend=$2
  local log_prefix=$3
  local candidate_clang=${4:-"$CONCRETE_SLICE0_CLANG"}
  local snapshot="$log_prefix.inputs.sha256"
  mkdir -p "$(dirname -- "$log_prefix")"
  dual_route_snapshot_inputs "$stage" "$backend" "$snapshot" 2>"$log_prefix.snapshot.log" || {
    cat "$log_prefix.snapshot.log" >&2
    return 1
  }
  dual_route_bound_build "$stage" "$backend" "$log_prefix.bound.log" "$candidate_clang" || {
    cat "$log_prefix.bound.log" >&2
    return 1
  }
  dual_route_check_inputs "$stage" "$snapshot" 2>"$log_prefix.inputs.log" || {
    cat "$log_prefix.inputs.log" >&2
    return 1
  }
  dual_route_check_inventory "$stage" "$backend" 2>"$log_prefix.inventory.log" || {
    cat "$log_prefix.inventory.log" >&2
    return 1
  }
  dual_route_verify_build "$stage" "$backend" "$log_prefix.verify.log" || {
    cat "$log_prefix.verify.log" >&2
    return 1
  }
}

# dual_route_observe EXE OUT_PREFIX [SECS [RETRY [MEM_MB]]]
# Runs EXE under bounded_run through the POSIX waitpid observer so normal exit
# status and terminating signal are recorded separately (.reason, .status, .signal).
dual_route_observe() {
  local exe=$1
  local out_prefix=$2
  local secs=${3:-"$DUAL_ROUTE_RUN_SECS"}
  local retry=${4:-"$DUAL_ROUTE_RUN_RETRY"}
  local mem_mb=${5:-1024}
  local meta="$out_prefix.observe.meta"
  local rc=0
  local obs_reason=""
  local obs_status=""
  local obs_signal=""
  local extra=""
  rm -f -- "$meta" "$out_prefix.reason" "$out_prefix.status" "$out_prefix.signal" \
    "$out_prefix.stdout" "$out_prefix.stderr"
  bounded_run -i /dev/null -r "$retry" "$secs" -- \
    "${DUAL_ROUTE_CAPPED[@]}" python3 -I "$DUAL_ROUTE_OBSERVER_PY" \
    --meta "$meta" --mem-mb "$mem_mb" -- "$exe" \
    >"$out_prefix.stdout" 2>"$out_prefix.stderr" || rc=$?
  if bounded_is_incomplete "$rc"; then
    rm -f -- "$meta"
    case "$(tail -n 1 "$BOUNDED_LOG")" in
      "incomplete "*)
        printf '%s\n' incomplete >"$out_prefix.reason"
        printf '%s\n' none >"$out_prefix.status"
        printf '%s\n' none >"$out_prefix.signal"
        ;;
      *)
        printf '%s\n' runner-failed >"$out_prefix.reason"
        printf '%s\n' none >"$out_prefix.status"
        printf '%s\n' none >"$out_prefix.signal"
        ;;
    esac
    return 0
  fi
  if [ "$rc" -ne 0 ] || [ ! -f "$meta" ] || [ -L "$meta" ]; then
    rm -f -- "$meta"
    printf '%s\n' runner-failed >"$out_prefix.reason"
    printf '%s\n' none >"$out_prefix.status"
    printf '%s\n' none >"$out_prefix.signal"
    return 0
  fi
  if IFS=$'\t' read -r obs_reason obs_status obs_signal extra <"$meta" && [ -z "$extra" ] &&
      _dual_route_valid_term "$obs_reason" "$obs_status" "$obs_signal" &&
      [ "$obs_reason" != incomplete ]; then
    printf '%s\n' "$obs_reason" >"$out_prefix.reason"
    printf '%s\n' "$obs_status" >"$out_prefix.status"
    printf '%s\n' "$obs_signal" >"$out_prefix.signal"
  else
    printf '%s\n' runner-failed >"$out_prefix.reason"
    printf '%s\n' none >"$out_prefix.status"
    printf '%s\n' none >"$out_prefix.signal"
  fi
  rm -f -- "$meta"
}

_dual_route_valid_term() {
  local reason=$1
  local status=$2
  local sig=$3
  case "$reason" in
    exit)
      [[ "$status" =~ ^[0-9]+$ ]] && [ "$status" -le 255 ] && [ "$sig" = none ]
      ;;
    signal)
      [ "$status" = none ] && [[ "$sig" =~ ^[1-9][0-9]*$ ]]
      ;;
    incomplete)
      [ "$status" = none ] && [ "$sig" = none ]
      ;;
    *)
      return 1
      ;;
  esac
}

# dual_route_judge EXPECTED_DIR OBS_PREFIX
# Requires a valid termination reason (exit, signal, or explicit incomplete;
# never runner-failed), consistent separate status and signal fields,
# byte-identical reason/status/signal/stderr, and strictly empty stdout.
dual_route_judge() {
  local expected_dir=$1
  local obs_prefix=$2
  local exp_reason=""
  local exp_status=""
  local exp_signal=""
  local obs_reason=""
  local obs_status=""
  local obs_signal=""
  [ -f "$expected_dir/reason" ] && [ -f "$expected_dir/status" ] &&
    [ -f "$expected_dir/signal" ] && [ -f "$expected_dir/stderr" ] || return 1
  [ -f "$obs_prefix.reason" ] && [ -f "$obs_prefix.status" ] &&
    [ -f "$obs_prefix.signal" ] && [ -f "$obs_prefix.stderr" ] &&
    [ -f "$obs_prefix.stdout" ] || return 1
  exp_reason=$(cat "$expected_dir/reason") || return 1
  exp_status=$(cat "$expected_dir/status") || return 1
  exp_signal=$(cat "$expected_dir/signal") || return 1
  obs_reason=$(cat "$obs_prefix.reason") || return 1
  obs_status=$(cat "$obs_prefix.status") || return 1
  obs_signal=$(cat "$obs_prefix.signal") || return 1
  _dual_route_valid_term "$exp_reason" "$exp_status" "$exp_signal" &&
    _dual_route_valid_term "$obs_reason" "$obs_status" "$obs_signal" &&
    cmp -s "$expected_dir/reason" "$obs_prefix.reason" &&
    cmp -s "$expected_dir/status" "$obs_prefix.status" &&
    cmp -s "$expected_dir/signal" "$obs_prefix.signal" &&
    cmp -s "$expected_dir/stderr" "$obs_prefix.stderr" &&
    [ ! -s "$obs_prefix.stdout" ]
}

# dual_route_make_clang_wrapper WRAPPER_PATH MODE MARKER_FILE [REPLACEMENT_DIR]
# Creates a standalone candidate clang wrapper (safe under slice0_native.sh's
# `env -i`) that injects one specific link-order, object-substitution, or
# input-mutation fault and records activation in MARKER_FILE outside the stage.
dual_route_make_clang_wrapper() {
  local wrapper_path=$1
  local mode=$2
  local marker_file=$3
  local replacement_dir=${4:-""}
  {
    printf '#!/usr/bin/env bash\n'
    printf 'set -euo pipefail\n'
    printf 'MODE=%q\n' "$mode"
    printf 'MARKER=%q\n' "$marker_file"
    printf 'REPLACEMENT=%q\n' "$replacement_dir"
    printf 'REAL_CLANG=%q\n' "$CONCRETE_SLICE0_CLANG"
    cat <<'BODY'
has_arg() {
  local needle=$1
  local arg
  shift
  for arg in "$@"; do
    if [ "$arg" = "$needle" ]; then return 0; fi
  done
  return 1
}

declare -a rewritten=()
case "$MODE" in
  reverse-llvm-link-order)
    if has_arg llvm.exe "$@" && has_arg component.o "$@" && has_arg launcher.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        case "$arg" in
          component.o) rewritten+=(launcher.o) ;;
          launcher.o) rewritten+=(component.o) ;;
          *) rewritten+=("$arg") ;;
        esac
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  reverse-lcb-link-order)
    if has_arg lcb.exe "$@" && has_arg lcb_body.o "$@" && has_arg lcb_rt.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        case "$arg" in
          lcb_body.o) rewritten+=(lcb_rt.o) ;;
          lcb_rt.o) rewritten+=(lcb_body.o) ;;
          *) rewritten+=("$arg") ;;
        esac
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-llvm-component)
    if has_arg llvm.exe "$@" && has_arg component.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = component.o ]; then rewritten+=("$REPLACEMENT/component.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-llvm-launcher)
    if has_arg llvm.exe "$@" && has_arg launcher.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = launcher.o ]; then rewritten+=("$REPLACEMENT/launcher.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-lcb-body)
    if has_arg lcb.exe "$@" && has_arg lcb_body.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = lcb_body.o ]; then rewritten+=("$REPLACEMENT/lcb_body.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  link-substitute-lcb-rt)
    if has_arg lcb.exe "$@" && has_arg lcb_rt.o "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      for arg in "$@"; do
        if [ "$arg" = lcb_rt.o ]; then rewritten+=("$REPLACEMENT/lcb_rt.o")
        else rewritten+=("$arg"); fi
      done
      exec "$REAL_CLANG" "${rewritten[@]}"
    fi
    ;;
  overwrite-llvm-component-obj)
    if has_arg launcher.ll "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      cp -- "$REPLACEMENT/component.o" component.o
    fi
    ;;
  overwrite-lcb-body-obj)
    if has_arg lcb_rt.s "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      cp -- "$REPLACEMENT/lcb_body.o" lcb_body.o
    fi
    ;;
  mutate-llvm-launcher-ll)
    if has_arg llvm.exe "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      printf '\n; mutated\n' >>launcher.ll
    fi
    ;;
  mutate-lcb-body-s)
    if has_arg lcb.exe "$@"; then
      printf '%s\n' "$MODE" >"$MARKER"
      printf '\n' >>lcb_body.s
    fi
    ;;
  *)
    echo "dual_route_make_clang_wrapper: unknown mode $MODE" >&2
    exit 2
    ;;
esac
exec "$REAL_CLANG" "$@"
BODY
  } >"$wrapper_path"
  chmod +x "$wrapper_path"
}

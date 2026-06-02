import Concrete.Core
import Concrete.Layout
import Concrete.FileSummary
import Concrete.AST
import Concrete.Intrinsic
import Concrete.Proof
import Concrete.ProofCore
import Concrete.SSA
import Concrete.Diagnostic
import Concrete.Format

namespace Concrete
namespace Report

-- ============================================================
-- Source location lookup
-- ============================================================

/-- Structured source location: (file, line). -/
abbrev SourceLoc := String × Nat

/-- Per-function parsed AST info for span lookups. -/
structure FnLocEntry where
  qualName : String
  file     : String
  fnSpan   : Span
  body     : List Stmt      -- parsed AST body (carries spans on every node)

/-- Map from qualified function name to location + parsed body. -/
abbrev FnLocMap := List FnLocEntry

/-- Collect function locations from a parsed AST module tree. -/
partial def buildFnLocMap (modules : List Module) (file : String) (pfx : String := "") : FnLocMap :=
  modules.foldl (fun acc m =>
    let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
    let fnLocs := m.functions.map fun f =>
      { qualName := qualPrefix ++ "." ++ f.name, file, fnSpan := f.span, body := f.body }
    let implLocs := m.implBlocks.foldl (fun acc2 ib =>
      acc2 ++ ib.methods.map fun f =>
        { qualName := qualPrefix ++ "." ++ f.name, file, fnSpan := f.span, body := f.body }) []
    let traitImplLocs := m.traitImpls.foldl (fun acc2 ti =>
      acc2 ++ ti.methods.map fun f =>
        { qualName := qualPrefix ++ "." ++ f.name, file, fnSpan := f.span, body := f.body }) []
    let subLocs := buildFnLocMap m.submodules file qualPrefix
    acc ++ fnLocs ++ implLocs ++ traitImplLocs ++ subLocs) []

/-- Look up a function's source location. -/
def lookupLoc (locMap : FnLocMap) (qualName : String) : Option SourceLoc :=
  match locMap.find? fun e => e.qualName == qualName with
  | some e => some (e.file, e.fnSpan.line)
  | none => none

/-- Look up a function's parsed body for violation-span extraction. -/
def lookupBody (locMap : FnLocMap) (qualName : String) : Option FnLocEntry :=
  locMap.find? fun e => e.qualName == qualName

/-- Format a source location as "file:line". -/
def fmtLoc : Option SourceLoc → String
  | some (file, line) => s!"{file}:{line}"
  | none => ""

-- ============================================================
-- Violation-span extraction from parsed AST
-- ============================================================

/-- Find the first while/for loop span in a parsed statement list. -/
partial def findLoopSpan : List Stmt → Option Span
  | [] => none
  | s :: rest =>
    match s with
    | .while_ sp _ _ _ => some sp
    | .forLoop sp _ _ _ _ _ => some sp
    | .ifElse _ _ thenB (some elseB) =>
      findLoopSpan thenB |>.orElse fun _ => findLoopSpan elseB |>.orElse fun _ => findLoopSpan rest
    | .ifElse _ _ thenB none =>
      findLoopSpan thenB |>.orElse fun _ => findLoopSpan rest
    | .borrowIn _ _ _ _ _ body =>
      findLoopSpan body |>.orElse fun _ => findLoopSpan rest
    | _ => findLoopSpan rest

/-- Find the span of the first call to any of the given function names. -/
partial def findCallSpan (targets : List String) : List Stmt → Option Span
  | [] => none
  | s :: rest =>
    let fromExprs := findCallSpanExpr targets s
    match fromExprs with
    | some sp => some sp
    | none => findCallSpan targets rest
where
  findCallSpanExpr (targets : List String) : Stmt → Option Span
    | .expr _ e => findCallSpanInExpr targets e
    | .letDecl _ _ _ _ e => findCallSpanInExpr targets e
    | .assign _ _ e => findCallSpanInExpr targets e
    | .return_ _ (some e) => findCallSpanInExpr targets e
    | .ifElse _ cond thenB (some elseB) =>
      findCallSpanInExpr targets cond
      |>.orElse fun _ => findCallSpan targets thenB
      |>.orElse fun _ => findCallSpan targets elseB
    | .ifElse _ cond thenB none =>
      findCallSpanInExpr targets cond
      |>.orElse fun _ => findCallSpan targets thenB
    | .while_ _ cond body _ =>
      findCallSpanInExpr targets cond
      |>.orElse fun _ => findCallSpan targets body
    | .forLoop _ _ cond _ body _ =>
      findCallSpanInExpr targets cond
      |>.orElse fun _ => findCallSpan targets body
    | .borrowIn _ _ _ _ _ body => findCallSpan targets body
    | _ => none
  findCallSpanInExpr (targets : List String) : Expr → Option Span
    | .call sp fn _ args => if targets.contains fn then some sp
      else args.foldl (fun acc a => acc.orElse fun _ => findCallSpanInExpr targets a) none
    | .methodCall _ _ _ _ args => args.foldl (fun acc a => acc.orElse fun _ => findCallSpanInExpr targets a) none
    | .binOp _ _ l r => (findCallSpanInExpr targets l).orElse fun _ => findCallSpanInExpr targets r
    | .unaryOp _ _ e => findCallSpanInExpr targets e
    | .ifExpr _ cond thenB elseB => (findCallSpanInExpr targets cond).orElse fun _ =>
        (findCallSpan targets thenB).orElse fun _ => findCallSpan targets elseB
    | _ => none

-- ============================================================
-- Helpers
-- ============================================================

def ppCapSet : CapSet → String
  | .empty => "(pure)"
  | .concrete caps => ", ".intercalate caps
  | .var name => name
  | .union a b => s!"{ppCapSet a}, {ppCapSet b}"

private def ppTyList (tys : List (String × Ty)) : String :=
  ", ".intercalate (tys.map fun (n, t) => s!"{n}: {tyToStr t}")

/-- Right-pad a string to the given width. -/
private def padRight (s : String) (w : Nat) : String :=
  if s.length >= w then s
  else s ++ String.ofList (List.replicate (w - s.length) ' ')

/-- Left-pad a number string to the given width. -/
private def padNum (n : Nat) (w : Nat) : String :=
  let s := toString n
  if s.length >= w then s
  else String.ofList (List.replicate (w - s.length) ' ') ++ s

-- ============================================================
-- Body fingerprinting (proof identity verification)
-- ============================================================
-- Produces a canonical string from CExpr/CStmt structure.
-- Used to verify that a function's body matches the PExpr
-- encoding in Proof.lean. If the body changes, the fingerprint
-- changes, and "proved" evidence is revoked.

/-- Print body fingerprints for all functions (development tool). -/
def fingerprintReport (pc : Concrete.ProofCore) : String :=
  let allEntries := pc.entries.map fun e => (e.qualName, e.fingerprint)
  let allExcluded := pc.excluded.map fun e => (e.qualName, e.fingerprint)
  let all := allEntries ++ allExcluded
  let lines := all.map fun (qn, fp) =>
    s!"  {qn}: \"{fp}\""
  "=== Body Fingerprints ===\n" ++ "\n".intercalate lines ++ "\n"

-- ============================================================
-- Report-specific data types
-- ============================================================

/-- A call site found in a function body. -/
structure CallSite where
  callee : String
  deriving BEq

/-- Info about allocation-related activity in a function body. -/
structure AllocInfo where
  allocCalls : List String   -- intrinsic names: alloc, vec_new, etc.
  freeCalls  : List String   -- intrinsic names: free, destroy, vec_free, etc.
  deferExprs : List String   -- descriptions of deferred expressions
  hasAllocCall : Bool        -- CExpr.allocCall node (with(Alloc = ...))


-- ============================================================
-- Callee CapSet lookup
-- ============================================================

/-- A flat map of function/extern names → CapSet, built once per report. -/
abbrev CapLookup := List (String × CapSet)

private partial def buildCapLookupModule (m : CModule) : CapLookup :=
  let fnEntries := m.functions.map fun f => (f.name, f.capSet)
  let externEntries := m.externFns.map fun (n, _, _, trusted) =>
    (n, if trusted then .empty else .concrete [unsafeCapName])
  fnEntries ++ externEntries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildCapLookupModule sub) []

private def buildCapLookup (modules : List CModule) : CapLookup :=
  modules.foldl (fun acc m => acc ++ buildCapLookupModule m) []

/-- Qualified-name cap lookup — keys match buildCallGraph's qualified nodes. -/
private partial def buildQualCapLookupModule (m : CModule) (pfx : String := "")
    : CapLookup :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let fnEntries := m.functions.map fun f =>
    (qualPrefix ++ "." ++ f.name, f.capSet)
  let externEntries := m.externFns.map fun (n, _, _, trusted) =>
    (n, if trusted then .empty else .concrete [unsafeCapName])
  fnEntries ++ externEntries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildQualCapLookupModule sub qualPrefix) []

private def buildQualCapLookup (modules : List CModule) : CapLookup :=
  modules.foldl (fun acc m => acc ++ buildQualCapLookupModule m) []

/-- Map bare function names → qualified names across all modules. -/
private partial def buildBareToQualMap (m : CModule) (pfx : String := "")
    : List (String × String) :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let entries := m.functions.map fun f => (f.name, qualPrefix ++ "." ++ f.name)
  entries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildBareToQualMap sub qualPrefix) []

/-- Look up a callee's capability set. Checks user fns, externs, then intrinsics. -/
private def lookupCalleeCap (lookup : CapLookup) (name : String) : Option CapSet :=
  match lookup.find? (fun (n, _) => n == name) with
  | some (_, cs) => some cs
  | none =>
    match resolveIntrinsic name with
    | some iid =>
      match iid.capability with
      | some cap => some (.concrete [cap])
      | none => some .empty
    | none => none

/-- Classify a callee for display purposes. -/
private def calleeTag (lookup : CapLookup) (name : String) : String :=
  match lookup.find? (fun (n, _) => n == name) with
  | some _ => ""
  | none =>
    if (resolveIntrinsic name).isSome then " (intrinsic)"
    else " (unknown)"


-- ============================================================
-- Report 1: Capability Summary with "why" traces (--report caps)
-- ============================================================

/-- Count functions across module tree. -/
private partial def countModuleFns (m : CModule) : Nat :=
  m.functions.length + m.submodules.foldl (fun acc sub => acc + countModuleFns sub) 0

private partial def countModulePure (m : CModule) : Nat :=
  let local_ := (m.functions.filter fun f => f.capSet == .empty).length
  local_ + m.submodules.foldl (fun acc sub => acc + countModulePure sub) 0

private partial def countModuleExterns (m : CModule) : Nat :=
  m.externFns.length + m.submodules.foldl (fun acc sub => acc + countModuleExterns sub) 0

/-- Build capability "why" trace lines for a function.
    For each concrete cap the function requires, find which direct callees
    contribute that cap. -/
private def capWhyTrace (lookup : CapLookup) (f : CFnDef) (indent : String) : List String :=
  let (concreteCaps, _) := f.capSet.normalize
  if concreteCaps.isEmpty then []
  else
    let callees := collectCallsStmts f.body |>.eraseDups
    concreteCaps.filterMap fun cap =>
      -- Find callees that require this cap
      let contributors := callees.filter fun callee =>
        match lookupCalleeCap lookup callee with
        | some cs =>
          let (calleeCaps, _) := cs.normalize
          calleeCaps.contains cap
        | none => false
      let contribStr := if contributors.isEmpty then "<- declared"
        else
          let tagged := contributors.map fun c => s!"{c}{calleeTag lookup c}"
          s!"<- calls {", ".intercalate tagged}"
      some s!"{indent}    {padRight cap 10} {contribStr}"

private partial def capReportModule (lookup : CapLookup) (m : CModule) (indent : String) : String :=
  let header := s!"{indent}module {m.name}:"
  let fnLines := m.functions.foldl (fun acc f =>
    let pubStr := if f.isPublic then "pub " else "    "
    let capsStr := ppCapSet f.capSet
    let mainLine := s!"{indent}  {pubStr}{f.name} : {capsStr}"
    let traceLines := capWhyTrace lookup f indent
    acc ++ [mainLine] ++ traceLines) []
  let trustedExterns := m.externFns.filter fun (_, _, _, t) => t
  let untrustedExterns := m.externFns.filter fun (_, _, _, t) => !t
  let externLines := if untrustedExterns.isEmpty then []
    else [s!"{indent}  extern:"] ++ untrustedExterns.map fun (n, _, _, _) =>
      s!"{indent}      {n} : Unsafe"
  let externLines := externLines ++ (if trustedExterns.isEmpty then []
    else [s!"{indent}  trusted extern:"] ++ trustedExterns.map fun (n, _, _, _) =>
      s!"{indent}      {n} : (none)")
  let subLines := m.submodules.map (capReportModule lookup · (indent ++ "  "))
  let body := fnLines ++ externLines ++ subLines
  if body.isEmpty then header
  else s!"{header}\n{"\n".intercalate body}"

def capabilityReport (modules : List CModule) : String :=
  let header := "=== Capability Summary ==="
  let lookup := buildCapLookup modules
  let body := modules.map (capReportModule lookup · "")
  let totalFns := modules.foldl (fun acc m => acc + countModuleFns m) 0
  let pureFns := modules.foldl (fun acc m => acc + countModulePure m) 0
  let externCount := modules.foldl (fun acc m => acc + countModuleExterns m) 0
  let summary := s!"\nTotals: {totalFns} functions ({pureFns} pure), {externCount} externs"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================
-- Report 2: Unsafe Signature Summary with trust boundary
--           analysis (--report unsafe)
-- ============================================================

private def hasUnsafeCap (cs : CapSet) : Bool :=
  cs.concreteCaps.contains unsafeCapName

private def usesRawPtr : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

private def fnUsesRawPtrs (f : CFnDef) : Bool :=
  f.params.any (fun (_, t) => usesRawPtr t) || usesRawPtr f.retTy

/-- Count unsafe-related items across module tree. -/
private partial def unsafeCounts (m : CModule)
    : Nat × Nat × Nat × Nat × Nat :=
  let unsafeFns := (m.functions.filter fun f => hasUnsafeCap f.capSet).length
  let ptrFns := (m.functions.filter fnUsesRawPtrs).length
  let externFns := (m.externFns.filter fun (_, _, _, t) => !t).length
  let trustedExterns := (m.externFns.filter fun (_, _, _, t) => t).length
  let trustedFns := (m.functions.filter fun f => f.isTrusted).length
  m.submodules.foldl (fun (a, b, c, d, e) sub =>
    let (a', b', c', d', e') := unsafeCounts sub
    (a + a', b + b', c + c', d + d', e + e'))
    (unsafeFns, ptrFns, externFns, trustedExterns, trustedFns)

/-- Analyze what a trusted function wraps — scan its body for unsafe operations. -/
private def trustBoundaryAnalysis (externNames : List String) (f : CFnDef) : List String :=
  let callees := collectCallsStmts f.body |>.eraseDups
  let ops : List String := []
  -- Check for raw pointer operations in body
  let ops := if hasRawPtrOpsStmts f.body then ops ++ ["pointer dereference"] else ops
  -- Check for calls to extern functions
  let externCalls := callees.filter fun c => externNames.contains c
  let ops := if externCalls.isEmpty then ops
    else ops ++ externCalls.map fun c => s!"extern {c}"
  -- Check for calls to functions with Unsafe capability
  -- (this is approximate — we check if callee name contains known unsafe patterns)
  let ops := if callees.any (fun c => c == "alloc" || c == "free") then
    ops ++ ["memory management"]
  else ops
  if ops.isEmpty then ["(safe body — no raw ops detected)"]
  else ops

private partial def unsafeReportModule (externNames : List String)
    (m : CModule) (indent : String) : Option String :=
  let unsafeFns := m.functions.filter fun f => hasUnsafeCap f.capSet
  let externFns := m.externFns.filter fun (_, _, _, t) => !t
  let trustedExternFns := m.externFns.filter fun (_, _, _, t) => t
  let ptrFns := m.functions.filter fnUsesRawPtrs
  let trustedFns := m.functions.filter fun f => f.isTrusted
  let subReports := m.submodules.filterMap (unsafeReportModule externNames · (indent ++ "  "))
  if unsafeFns.isEmpty && externFns.isEmpty && trustedExternFns.isEmpty && ptrFns.isEmpty && trustedFns.isEmpty && subReports.isEmpty then
    none
  else
    let lines : List String := [s!"{indent}module {m.name}:"]
    let lines := if unsafeFns.isEmpty then lines
      else lines ++ [s!"{indent}  Functions with Unsafe capability:"] ++
        unsafeFns.map fun f =>
          s!"{indent}    fn {f.name}({ppTyList f.params}) -> {tyToStr f.retTy}"
    let lines := if externFns.isEmpty then lines
      else lines ++ [s!"{indent}  Extern functions:"] ++
        externFns.map fun (n, ps, rt, _) =>
          s!"{indent}    extern fn {n}({ppTyList ps}) -> {tyToStr rt}"
    let lines := if trustedExternFns.isEmpty then lines
      else lines ++ [s!"{indent}  Trusted extern functions:"] ++
        trustedExternFns.map fun (n, ps, rt, _) =>
          s!"{indent}    trusted extern fn {n}({ppTyList ps}) -> {tyToStr rt}"
    let lines := if ptrFns.isEmpty then lines
      else lines ++ [s!"{indent}  Functions with raw pointer signatures:"] ++
        ptrFns.map fun f =>
          s!"{indent}    fn {f.name}({ppTyList f.params}) -> {tyToStr f.retTy}"
    -- Trusted boundaries with body analysis
    let trustedStandalone := trustedFns.filter fun f => f.trustedImplOrigin.isNone
    let trustedImplFns := trustedFns.filter fun f => f.trustedImplOrigin.isSome
    let trustedImplNames := (trustedImplFns.filterMap (·.trustedImplOrigin)).eraseDups
    let lines := if trustedStandalone.isEmpty && trustedImplFns.isEmpty then lines
      else
        let lines := lines ++ [s!"{indent}  Trust boundary analysis:"]
        let lines := if trustedStandalone.isEmpty then lines
          else trustedStandalone.foldl (fun lines f =>
            let ops := trustBoundaryAnalysis externNames f
            lines ++ [s!"{indent}    trusted fn {f.name}:"] ++
              ops.map fun op => s!"{indent}      wraps: {op}"
          ) lines
        let lines := trustedImplNames.foldl (fun lines implName =>
          let methods := trustedImplFns.filter fun f => f.trustedImplOrigin == some implName
          let methodLines := methods.foldl (fun acc f =>
            let shortName := if f.name.startsWith (implName ++ "_") then
              f.name.drop (implName.length + 1) else f.name
            let ops := trustBoundaryAnalysis externNames f
            acc ++ [s!"{indent}      fn {shortName}:"] ++
              ops.map fun op => s!"{indent}        wraps: {op}"
          ) []
          lines ++ [s!"{indent}    trusted impl {implName}:"] ++ methodLines
        ) lines
        lines
    let lines := lines ++ subReports
    some ("\n".intercalate lines)

def unsafeReport (modules : List CModule) (pc : Concrete.ProofCore) : String :=
  let header := "=== Unsafe Signature Summary ==="
  let externNames := pc.externNames
  let body := modules.filterMap (unsafeReportModule externNames · "")
  let (unsafeCount, ptrCount, externCount, trustedExternCount, trustedCount) :=
    modules.foldl (fun (a, b, c, d, e) m =>
      let (a', b', c', d', e') := unsafeCounts m
      (a + a', b + b', c + c', d + d', e + e')) (0, 0, 0, 0, 0)
  let total := unsafeCount + externCount + trustedExternCount + ptrCount + trustedCount
  if body.isEmpty then s!"{header}\n\nNo unsafe signatures found.\n"
  else
    let parts : List String := []
    let parts := if unsafeCount > 0 then parts ++ [s!"{unsafeCount} unsafe"] else parts
    let parts := if externCount > 0 then parts ++ [s!"{externCount} extern"] else parts
    let parts := if trustedExternCount > 0 then parts ++ [s!"{trustedExternCount} trusted extern"] else parts
    let parts := if ptrCount > 0 then parts ++ [s!"{ptrCount} raw-pointer"] else parts
    let parts := if trustedCount > 0 then parts ++ [s!"{trustedCount} trusted"] else parts
    let summary := s!"\nTotals: {total} unsafe-related signatures ({", ".intercalate parts})"
    s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================
-- Report 3: Layout Report (--report layout)
-- ============================================================

private partial def collectSubStructs (m : CModule) : List CStructDef :=
  m.submodules.foldl (fun acc sub => acc ++ sub.structs ++ collectSubStructs sub) []

private partial def collectSubEnums (m : CModule) : List CEnumDef :=
  m.submodules.foldl (fun acc sub => acc ++ sub.enums ++ collectSubEnums sub) []

private partial def collectSubNewtypes (m : CModule) : List NewtypeDef :=
  m.submodules.foldl (fun acc sub => acc ++ sub.newtypes ++ collectSubNewtypes sub) []

private def buildLayoutCtx (modules : List CModule) : Layout.Ctx :=
  let structs := modules.foldl (fun acc m => acc ++ m.structs ++ collectSubStructs m) []
  let enums := modules.foldl (fun acc m => acc ++ m.enums ++ collectSubEnums m) []
  let newtypes := modules.foldl (fun acc m => acc ++ m.newtypes ++ collectSubNewtypes m) []
  { structDefs := structs, enumDefs := enums, newtypes := newtypes }

private def layoutStructReport (ctx : Layout.Ctx) (sd : CStructDef) : Option String :=
  if !sd.typeParams.isEmpty then none
  else
    let size := Layout.tySize ctx (.named sd.name)
    let align := Layout.tyAlign ctx (.named sd.name)
    let reprStr := if sd.isReprC then "  #[repr(C)]" else ""
    let packedStr := if sd.isPacked then "  #[packed]" else ""
    let header := s!"struct {sd.name}{reprStr}{packedStr}  (size: {size}, align: {align})"
    -- Compute max widths for aligned columns
    let fieldData := sd.fields.map fun (fname, fty) =>
      let off := Layout.fieldOffset ctx sd.name fname
      let fsz := Layout.tySize ctx fty
      let falign := Layout.tyAlign ctx fty
      (fname, fty, off, fsz, falign)
    let maxOffW := fieldData.foldl (fun acc (_, _, off, _, _) => max acc (toString off).length) 1
    let maxSzW := fieldData.foldl (fun acc (_, _, _, fsz, _) => max acc (toString fsz).length) 1
    let maxAlW := fieldData.foldl (fun acc (_, _, _, _, fa) => max acc (toString fa).length) 1
    let fieldLines := fieldData.map fun (fname, fty, off, fsz, falign) =>
      s!"    offset {padNum off (maxOffW + 1)}  size {padNum fsz (maxSzW + 1)}  align {padNum falign (maxAlW + 1)}  {fname}: {tyToStr fty}"
    some (s!"{header}\n{"\n".intercalate fieldLines}")

private def layoutEnumReport (ctx : Layout.Ctx) (ed : CEnumDef) : Option String :=
  if !ed.typeParams.isEmpty then none
  else
    let size := Layout.tySize ctx (.named ed.name)
    let align := Layout.tyAlign ctx (.named ed.name)
    let tagSize := 4
    let payloadOff := Layout.enumPayloadOffset ctx ed
    let maxPayload := Layout.enumPayloadSize ctx ed
    let header := s!"enum {ed.name}  (size: {size}, align: {align}, tag: {tagSize}, payload_offset: {payloadOff}, max_payload: {maxPayload})"
    let variantLines := ed.variants.map fun (vn, fields) =>
      if fields.isEmpty then s!"    {vn}"
      else
        let fs := fields.map fun (fn, ft) => s!"{fn}: {tyToStr ft}"
        s!"    {vn} \{ {", ".intercalate fs} }"
    some (s!"{header}\n{"\n".intercalate variantLines}")

/-- Collect all structs and enums from a module including submodules. -/
private partial def collectAllStructs (m : CModule) : List CStructDef :=
  m.structs ++ m.submodules.foldl (fun acc sub => acc ++ collectAllStructs sub) []

private partial def collectAllEnums (m : CModule) : List CEnumDef :=
  m.enums ++ m.submodules.foldl (fun acc sub => acc ++ collectAllEnums sub) []

def layoutReport (modules : List CModule) : String :=
  let ctx := buildLayoutCtx modules
  let header := "=== Type Layout Report ==="
  let allStructs := modules.foldl (fun acc m => acc ++ collectAllStructs m) []
  let allEnums := modules.foldl (fun acc m => acc ++ collectAllEnums m) []
  let structReports := allStructs.filterMap (layoutStructReport ctx)
  let enumReports := allEnums.filterMap (layoutEnumReport ctx)
  let body := structReports ++ enumReports
  if body.isEmpty then s!"{header}\n\nNo concrete types found.\n"
  else
    let sPlural := if structReports.length == 1 then "struct" else "structs"
    let ePlural := if enumReports.length == 1 then "enum" else "enums"
    let summary := s!"\nTotals: {structReports.length} {sPlural}, {enumReports.length} {ePlural}"
    s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================
-- Report 4: Interface Summary (--report interface)
-- ============================================================

private def interfaceModule (name : String) (fs : FileSummary) : String :=
  let pubFns := fs.functions.filter fun (n, _) => fs.publicNames.contains n
  let pubExternFns := fs.externFns.filter (·.isPublic)
  let pubStructs := fs.structs.filter (·.isPublic)
  let pubEnums := fs.enums.filter (·.isPublic)
  let pubTraits := fs.traits.filter (·.isPublic)
  let pubConstants := fs.constants.filter (·.isPublic)
  let pubAliases := fs.typeAliases.filter (·.isPublic)
  let pubNewtypes := fs.newtypes.filter (·.isPublic)
  let exportCount := pubFns.length + pubExternFns.length + pubStructs.length +
    pubEnums.length + pubTraits.length + pubConstants.length +
    pubAliases.length + pubNewtypes.length
  let header := if exportCount == 0 then s!"module {name} (no exports)"
    else s!"module {name} ({exportCount} exports):"
  let lines : List String := []
  let lines := if fs.imports.isEmpty then lines
    else lines ++ ["  imports:"] ++
      fs.imports.map fun imp =>
        let symStrs := imp.symbols.map fun s =>
          match s.alias with
          | some a => s!"{s.name} as {a}"
          | none => s.name
        s!"    use {imp.moduleName} \{ {", ".intercalate symStrs} }"
  let lines := if exportCount == 0 then lines
    else
      let lines := lines ++ ["  public API:"]
      let lines := lines ++ pubFns.map fun (n, sig) =>
        let params := sig.params.map fun (pn, pt) => s!"{pn}: {tyToStr pt}"
        let capsStr := ppCapSet sig.capSet
        s!"    fn {n}({", ".intercalate params}) -> {tyToStr sig.retTy}  [{capsStr}]"
      let lines := lines ++ pubExternFns.map fun ef =>
        let params := ef.params.map fun p => s!"{p.name}: {tyToStr p.ty}"
        let kw := if ef.isTrusted then "trusted extern fn" else "extern fn"
        s!"    {kw} {ef.name}({", ".intercalate params}) -> {tyToStr ef.retTy}"
      let lines := lines ++ pubStructs.map fun sd =>
        let fields := sd.fields.map fun sf => s!"{sf.name}: {tyToStr sf.ty}"
        s!"    struct {sd.name} \{ {", ".intercalate fields} }"
      let lines := lines ++ pubEnums.map fun ed =>
        let variants := ed.variants.map (·.name)
        s!"    enum {ed.name} \{ {", ".intercalate variants} }"
      let lines := lines ++ pubTraits.map fun td =>
        let methods := td.methods.map (·.name)
        s!"    trait {td.name} \{ {", ".intercalate methods} }"
      let lines := lines ++ pubConstants.map fun c =>
        s!"    const {c.name}: {tyToStr c.ty}"
      let lines := lines ++ pubAliases.map fun a =>
        s!"    type {a.name} = {tyToStr a.targetTy}"
      let lines := lines ++ pubNewtypes.map fun nt =>
        s!"    newtype {nt.name}({tyToStr nt.innerTy})"
      lines
  s!"{header}\n{"\n".intercalate lines}"

-- ---- call-site contract obligations (source-contracts slice) ----

/-- Substitute callee parameter names with the actual argument exprs inside a
    precondition (used to specialize a `#[requires]` at a call site). -/
partial def substContract (subst : List (String × Expr)) : Expr → Expr
  | .ident sp name => match subst.find? (·.1 == name) with
    | some (_, a) => a
    | none => .ident sp name
  | .binOp sp op l r => .binOp sp op (substContract subst l) (substContract subst r)
  | .unaryOp sp op e => .unaryOp sp op (substContract subst e)
  | .paren sp e => .paren sp (substContract subst e)
  | .call sp fn ta args => .call sp fn ta (args.map (substContract subst))
  | e => e

/-- Constant integer evaluation (literal/arithmetic only; `none` if not constant). -/
partial def cEvalInt : Expr → Option Int
  | .intLit _ v => some v
  | .paren _ e => cEvalInt e
  | .binOp _ op l r => match cEvalInt l, cEvalInt r with
    | some a, some b => match op with
      | .add => some (a + b) | .sub => some (a - b) | .mul => some (a * b)
      | .div => if b == 0 then none else some (a / b)
      | .mod => if b == 0 then none else some (a % b)
      | _ => none
    | _, _ => none
  | _ => none

/-- Constant boolean evaluation of a (substituted) precondition. `none` when the
    expression still mentions non-constant values — i.e. `unproven_at_callsite`. -/
partial def cEvalBool : Expr → Option Bool
  | .boolLit _ b => some b
  | .paren _ e => cEvalBool e
  | .binOp _ op l r =>
    match op with
    | .and_ => match cEvalBool l, cEvalBool r with | some a, some b => some (a && b) | _, _ => none
    | .or_  => match cEvalBool l, cEvalBool r with | some a, some b => some (a || b) | _, _ => none
    | _ => match cEvalInt l, cEvalInt r with
      | some a, some b => match op with
        | .lt => some (decide (a < b)) | .gt => some (decide (a > b))
        | .leq => some (decide (a ≤ b)) | .geq => some (decide (a ≥ b))
        | .eq => some (a == b) | .neq => some (a != b)
        | _ => none
      | _, _ => none
  | _ => none

mutual
  /-- Collect every `(span, fnName, args)` call in an expression. -/
  partial def collectCallsE : Expr → List (Span × String × List Expr)
    | .call sp fn _ args => (sp, fn, args) :: args.flatMap collectCallsE
    | .binOp _ _ l r => collectCallsE l ++ collectCallsE r
    | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
    | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ | .arrowAccess _ x _ => collectCallsE x
    | .arrayLit _ es => es.flatMap collectCallsE
    | .arrayIndex _ a i => collectCallsE a ++ collectCallsE i
    | .methodCall _ o _ _ args => collectCallsE o ++ args.flatMap collectCallsE
    | .staticMethodCall _ _ _ _ args => args.flatMap collectCallsE
    | .structLit _ _ _ fs | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectCallsE fe)
    | .allocCall _ x a => collectCallsE x ++ collectCallsE a
    | .ifExpr _ c t el | .whileExpr _ c t el =>
        collectCallsE c ++ t.flatMap collectCallsS ++ el.flatMap collectCallsS
    | .match_ _ s _ => collectCallsE s
    | _ => []
  /-- Collect every `(span, fnName, args)` call in a statement. -/
  partial def collectCallsS : Stmt → List (Span × String × List Expr)
    | .letDecl _ _ _ _ v | .assign _ _ v | .expr _ v | .defer _ v => collectCallsE v
    | .return_ _ (some v) => collectCallsE v
    | .ifElse _ c t el => collectCallsE c ++ t.flatMap collectCallsS ++ (el.getD []).flatMap collectCallsS
    | .while_ _ c b _ => collectCallsE c ++ b.flatMap collectCallsS
    | .forLoop _ init c step b _ =>
        (init.map collectCallsS).getD [] ++ collectCallsE c ++ (step.map collectCallsS).getD [] ++ b.flatMap collectCallsS
    | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v => collectCallsE o ++ collectCallsE v
    | .arrayIndexAssign _ a i v => collectCallsE a ++ collectCallsE i ++ collectCallsE v
    | _ => []
end

/-- `(prefix, fn)` for every function in a module tree (recursing submodules). -/
partial def allFunctions (m : Module) : List (String × FnDef) :=
  let pfx := if m.name.isEmpty then "" else m.name ++ "."
  m.functions.map (fun f => (pfx, f)) ++ m.submodules.flatMap allFunctions

/-- Top-level `let NAME = <const>` bindings in a body, as NAME → literal expr.
    Lets the call-site checker see e.g. `let n = 7; rotr(x, n)`. -/
def letConstMap (body : List Stmt) : List (String × Expr) :=
  body.filterMap fun s => match s with
    | .letDecl _ name _ _ v =>
      match cEvalInt v with
      | some k => some (name, .intLit default k)
      | none => match cEvalBool v with | some b => some (name, .boolLit default b) | none => none
    | _ => none

/-- No free identifiers remain (every leaf is a literal): the expr is a closed,
    decidable obligation suitable for a kernel decision procedure. -/
partial def isClosed : Expr → Bool
  | .intLit _ _ | .boolLit _ _ => true
  | .ident _ _ => false
  | .binOp _ _ l r => isClosed l && isClosed r
  | .unaryOp _ _ e | .paren _ e => isClosed e
  | _ => false

/-- Lower a closed contract expr to a Lean `BitVec 32` Bool expression for
    `bv_decide` (u32 model, unsigned comparisons). `none` if outside the
    supported subset (int comparisons, &&/||, simple arithmetic). -/
partial def toLeanBV : Expr → Option String
  | .intLit _ v => if v < 0 then none else some s!"({v}#32)"
  | .boolLit _ b => some (if b then "true" else "false")
  | .paren _ e => toLeanBV e
  | .binOp _ op l r => do
    let L ← toLeanBV l
    let R ← toLeanBV r
    match op with
    | .leq => some s!"(BitVec.ule {L} {R})"
    | .lt  => some s!"(BitVec.ult {L} {R})"
    | .geq => some s!"(BitVec.ule {R} {L})"
    | .gt  => some s!"(BitVec.ult {R} {L})"
    | .eq  => some s!"({L} == {R})"
    | .neq => some s!"(!({L} == {R}))"
    | .and_ => some s!"({L} && {R})"
    | .or_  => some s!"({L} || {R})"
    | .add => some s!"({L} + {R})"
    | .sub => some s!"({L} - {R})"
    | .mul => some s!"({L} * {R})"
    | _ => none
  | _ => none

/-- One call-site obligation: a precondition of a callee, specialized to a call. -/
structure CallObligation where
  caller     : String
  callStr    : String          -- e.g. "rotr(x, n)"
  specExpr   : Expr            -- precondition with the call's args substituted
  baseStatus : String          -- "proved_at_callsite" | "failed_at_callsite" | "unproven"
  leanGoal   : Option String   -- bv_decide goal (closed after let-const subst), when baseStatus is unproven

/-- Build the ordered list of call-site obligations across all callers. The fast
    constant folder classifies the literal/arithmetic cases; obligations it
    cannot fold but that become closed after substituting caller let-constants
    carry a `leanGoal` for the `bv_decide` discharge backend (run by Main). -/
def callSiteObligations (modules : List Module) : List CallObligation := Id.run do
  let fns := modules.flatMap allFunctions
  let reqMap : List (String × (List Param × List Expr)) :=
    fns.filterMap (fun (_, f) => if f.requires.isEmpty then none else some (f.name, (f.params, f.requires)))
  if reqMap.isEmpty then return []
  let mut obs : List CallObligation := []
  for (pfx, f) in fns do
    let lets := letConstMap f.body
    for (_, fn, args) in f.body.flatMap collectCallsS do
      match reqMap.find? (·.1 == fn) with
      | none => pure ()
      | some (_, (params, reqs)) =>
        let argSubst := (params.zip args).map (fun (p, a) => (p.name, a))
        let callStr := s!"{fn}({", ".intercalate (args.map Concrete.fmtExpr)})"
        for r in reqs do
          let spec := substContract argSubst r
          let (baseStatus, leanGoal) := match cEvalBool spec with
            | some true  => ("proved_at_callsite", none)
            | some false => ("failed_at_callsite", none)
            | none =>
              -- tier 2: also substitute caller let-constants
              let spec2 := substContract lets spec
              match cEvalBool spec2 with
              | some false => ("failed_at_callsite", none)        -- closed and violated
              | _ => if isClosed spec2 then ("unproven", toLeanBV spec2)  -- closed → bv_decide candidate
                     else ("unproven", none)                              -- still has free vars
          obs := obs ++ [{ caller := pfx ++ f.name, callStr, specExpr := spec, baseStatus, leanGoal }]
  return obs

/-- Render the call-site obligation section. `provedByBV` is the list of indices
    (into `obs`) that the `bv_decide` backend kernel-checked. -/
def renderCallSites (obs : List CallObligation) (provedByBV : List Nat) : String := Id.run do
  if obs.isEmpty then return ""
  let mut out := "\n\n=== Call-site obligations ==="
  let mut curCaller := ""
  for (i, o) in (List.range obs.length).zip obs do
    if o.caller != curCaller then out := out ++ s!"\n\n{o.caller}"; curCaller := o.caller
    out := out ++ s!"\n  call {o.callStr}\n    requires {Concrete.fmtExpr o.specExpr}"
    let status := match o.baseStatus with
      | "unproven" =>
        if provedByBV.contains i then "proved_by_kernel_decision\n    engine:  bv_decide"
        else "unproven_at_callsite (non-constant; no discharge backend succeeded)"
      | s => s
    out := out ++ s!"\n    status:  {status}"
  return out ++ "\n"

/-- Free identifiers in an expression (for quantifying the generated VC). -/
partial def collectIdents : Expr → List String
  | .ident _ n => [n]
  | .paren _ e => collectIdents e
  | .binOp _ _ l r => collectIdents l ++ collectIdents r
  | .unaryOp _ _ e => collectIdents e
  | .call _ _ _ args => args.flatMap collectIdents
  | _ => []

/-- Lower a contract expression to a Lean `Prop`/`Int` term (`&&`→`∧`, `<=`→`≤`,
    spec-fn calls as applications). `none` if outside the supported subset. -/
partial def toLeanProp : Expr → Option String
  | .intLit _ v => some s!"{v}"
  | .ident _ n => some n
  | .paren _ e => toLeanProp e
  | .call _ fn _ args => do
    let as ← args.mapM toLeanProp
    some s!"{fn} {" ".intercalate as}"
  | .binOp _ op l r => do
    let L ← toLeanProp l
    let R ← toLeanProp r
    match op with
    | .leq => some s!"{L} ≤ {R}" | .lt => some s!"{L} < {R}"
    | .geq => some s!"{L} ≥ {R}" | .gt => some s!"{L} > {R}"
    | .eq => some s!"{L} = {R}" | .neq => some s!"{L} ≠ {R}"
    | .and_ => some s!"({L} ∧ {R})" | .or_ => some s!"({L} ∨ {R})"
    | .add => some s!"({L} + {R})" | .sub => some s!"({L} - {R})" | .mul => some s!"({L} * {R})"
    | _ => none
  | _ => none

/-- **Generate the invariant-preservation VC** from a loop contract: substitute
    the body's assignments into the invariant (`invariant'`), and state
    `∀ vars, invariant → guard → invariant'`. The compiler produces the
    obligation shape; discharge is hand-linked for now. `none` if the contract
    is outside the lowerable subset. -/
def genPreservationVC (lc : LoopContract) : Option String := do
  let guard ← lc.guard
  if lc.invariants.isEmpty then failure
  let invs' := lc.invariants.map (substContract lc.body)
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents guard
                ++ lc.body.flatMap (fun (_, e) => collectIdents e)).eraseDups
  let invStrs ← lc.invariants.mapM toLeanProp
  let guardStr ← toLeanProp guard
  let inv'Strs ← invs'.mapM toLeanProp
  let conj := fun (ss : List String) => " ∧ ".intercalate ss
  let binder := if vars.isEmpty then "" else s!"({" ".intercalate vars} : Int), "
  let bodyDesc := "; ".intercalate (lc.body.map (fun (n, e) => s!"{n} := {Concrete.fmtExpr e}"))
  let pad := "\n           "
  some <| String.join [
    s!"∀ {binder}",
    s!"{pad}{conj invStrs} →      -- invariant (assumed)",
    s!"{pad}{guardStr} →      -- guard (assumed)",
    s!"{pad}{conj inv'Strs}      -- invariant' after body [ {bodyDesc} ]" ]

/-- invariant_init VC: the invariant holds in the loop-entry state (the for-init
    and preceding let-constants substituted). -/
def genInitVC (lc : LoopContract) (extraLets : List (String × Expr)) : Option String := do
  if lc.invariants.isEmpty then failure
  let inits := lc.invariants.map (substContract (lc.entrySubst ++ extraLets))
  let vars := (inits.flatMap collectIdents).eraseDups
  let strs ← inits.mapM toLeanProp
  let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
  some s!"{binder}{" ∧ ".intercalate strs}"

/-- variant_nonnegative VC: invariant ∧ guard → 0 ≤ variant. -/
def genVariantNonneg (lc : LoopContract) : Option String := do
  let v ← lc.variant
  let g ← lc.guard
  if lc.invariants.isEmpty then failure
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents g ++ collectIdents v).eraseDups
  let invs ← lc.invariants.mapM toLeanProp
  let gs ← toLeanProp g
  let vs ← toLeanProp v
  some s!"∀ ({" ".intercalate vars} : Int), {" ∧ ".intercalate invs} → {gs} → 0 ≤ {vs}"

/-- variant_decreases VC: invariant ∧ guard → variant[body] < variant. -/
def genVariantDecreases (lc : LoopContract) : Option String := do
  let v ← lc.variant
  let g ← lc.guard
  if lc.invariants.isEmpty then failure
  let v' := substContract lc.body v
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents g ++ collectIdents v).eraseDups
  let invs ← lc.invariants.mapM toLeanProp
  let gs ← toLeanProp g
  let vs ← toLeanProp v
  let vs' ← toLeanProp v'
  some s!"∀ ({" ".intercalate vars} : Int), {" ∧ ".intercalate invs} → {gs} → {vs'} < {vs}"

/-- The loop-contract section: for each `#[invariant]`/`#[variant]`-annotated
    loop, enumerate the verification obligations it induces. Every obligation now
    carries a compiler-generated VC shape; the preservation obligation's
    discharge is hand-linked via a `coverage: invariant` registry entry, the
    rest are `planned`. -/
def loopContractSection (modules : List Module) (registry : ProofRegistry) : String := Id.run do
  let withLoops := (modules.flatMap allFunctions).filter (fun (_, f) => !f.loopContracts.isEmpty)
  if withLoops.isEmpty then return ""
  let mut out := "\n\n=== Loop contracts ==="
  for (pfx, f) in withLoops do
    -- a registered `coverage: invariant` proof discharges invariant_preservation
    let preserveProof : Option String :=
      match registry.find? (fun e => e.function == pfx ++ f.name) with
      | some e => if e.coverage == "invariant" && !e.proof.isEmpty then some e.proof else none
      | none => none
    let extraLets := letConstMap f.body
    for lc in f.loopContracts do
      out := out ++ s!"\n\n{pfx}{f.name}  (loop @ line {lc.line})"
      for inv in lc.invariants do
        out := out ++ s!"\n  invariant {Concrete.fmtExpr inv}"
      match lc.variant with
      | some v => out := out ++ s!"\n  variant   {Concrete.fmtExpr v}"
      | none => pure ()
      out := out ++ "\n  obligations:"
      let planned := "planned (no discharge backend linked yet)"
      let vc := fun (g : Option String) => match g with | some s => s!"\n       generated VC:  {s}" | none => ""
      -- O1 invariant_init — generated shape, discharge planned
      out := out ++ s!"\n    O1 invariant_init          status:  {planned}{vc (genInitVC lc extraLets)}"
      -- O2 invariant_preservation — generated shape + hand-linked discharge
      match preserveProof with
      | some thm => out := out ++ s!"\n    O2 invariant_preservation  status:  proved_by_lean\n                                theorem: {thm}{vc (genPreservationVC lc)}"
      | none     => out := out ++ s!"\n    O2 invariant_preservation  status:  {planned}{vc (genPreservationVC lc)}"
      -- O3 exit_link needs a postcondition; generated when the fn has #[ensures]
      out := out ++ s!"\n    O3 loop_exit_post_link     status:  {planned}"
      match lc.variant with
      | some _ =>
        out := out ++ s!"\n    O4 variant_nonnegative     status:  {planned}{vc (genVariantNonneg lc)}"
        out := out ++ s!"\n    O5 variant_decreases       status:  {planned}{vc (genVariantDecreases lc)}"
      | none => pure ()
  return out ++ "\n"

partial def contractsReport (modules : List Module) (registry : ProofRegistry) : String := Id.run do
  -- Discharge status for an obligation on `qual`: a registry entry whose
  -- `ensures_proof` names the theorem that proves it → proved_by_lean; else missing.
  let discharge (qual : String) : String :=
    match registry.find? (fun e => e.function == qual) with
    | some e => match e.ensuresProof with
      | some thm => s!"\n     status:  proved_by_lean\n     theorem: {thm}"
      | none     => "\n     status:  missing (registry entry has no ensures_proof)"
    | none => "\n     status:  missing (no proof-registry entry for this function)"
  let rec go (m : Module) (acc : String) : String := Id.run do
    let mut out := acc
    let pfx := if m.name.isEmpty then "" else m.name ++ "."
    for sf in m.specFns do
      let ps := ", ".intercalate (sf.params.map (fun p => s!"{p.name}: {Concrete.fmtTy p.ty}"))
      out := out ++ s!"\nspec fn {pfx}{sf.name}({ps}) -> {Concrete.fmtTy sf.retTy}"
    for f in m.functions do
      if !f.ensures.isEmpty || !f.requires.isEmpty then
        out := out ++ s!"\n\n{pfx}{f.name}"
        -- preconditions: assumed on entry (no call-site checking in this slice)
        let mut ri := 1
        for r in f.requires do
          out := out ++ s!"\n  R{ri}  requires {Concrete.fmtExpr r}\n     status:  assumed_at_entry (callers not yet checked)"
          ri := ri + 1
        -- postconditions: discharged by a registered ensures_proof, or missing
        let mut i := 1
        for e in f.ensures do
          out := out ++ s!"\n  O{i}  ensures {Concrete.fmtExpr e}{discharge (pfx ++ f.name)}"
          i := i + 1
    for sub in m.submodules do
      out := go sub out
    return out
  let body := modules.foldl (fun acc m => go m acc) ""
  let body := if body.isEmpty then "\n(no spec fns or #[ensures] contracts found)" else body
  return s!"=== Source Contracts ==={body}\n{loopContractSection modules registry}"

/-- Whether any module (or submodule) carries a source contract — a `spec fn`
    or an `#[ensures(...)]`. Used to decide whether `audit` appends the
    contracts section. -/
partial def hasContracts (modules : List Module) : Bool :=
  modules.any fun m =>
    !m.specFns.isEmpty
    || m.functions.any (fun f => !f.ensures.isEmpty || !f.requires.isEmpty || !f.loopContracts.isEmpty)
    || hasContracts m.submodules

def interfaceReport (summaryTable : List (String × FileSummary)) : String :=
  let header := "=== Interface Summary ==="
  let body := summaryTable.map fun (name, fs) => interfaceModule name fs
  let totalExports := summaryTable.foldl (fun acc (_, fs) =>
    let pubCount := (fs.functions.filter fun (n, _) => fs.publicNames.contains n).length +
      (fs.externFns.filter (·.isPublic)).length +
      (fs.structs.filter (·.isPublic)).length +
      (fs.enums.filter (·.isPublic)).length +
      (fs.traits.filter (·.isPublic)).length +
      (fs.constants.filter (·.isPublic)).length +
      (fs.typeAliases.filter (·.isPublic)).length +
      (fs.newtypes.filter (·.isPublic)).length
    acc + pubCount) 0
  let summary := s!"\nTotals: {summaryTable.length} modules, {totalExports} public exports"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================
-- Report 5: Monomorphization Report (--report mono)
-- ============================================================

private partial def collectSubFnNames (m : CModule) : List String :=
  m.submodules.foldl (fun acc sub =>
    acc ++ sub.functions.map (·.name) ++ collectSubFnNames sub) []

private def collectFnNames (modules : List CModule) : List String :=
  modules.foldl (fun acc m =>
    acc ++ m.functions.map (·.name) ++ collectSubFnNames m) []

/-- Count generic functions across all modules including submodules. -/
private partial def countGenericFns (m : CModule) : Nat :=
  let local_ := (m.functions.filter fun f => !f.typeParams.isEmpty).length
  local_ + m.submodules.foldl (fun acc sub => acc + countGenericFns sub) 0

/-- Count all functions across module tree. -/
private partial def countAllFns (m : CModule) : Nat :=
  m.functions.length + m.submodules.foldl (fun acc sub => acc + countAllFns sub) 0

def monoReport (preMono postMono : List CModule) : String :=
  let header := "=== Monomorphization Report ==="
  let preNames := collectFnNames preMono
  let postNames := collectFnNames postMono
  let specializations := postNames.filter fun n =>
    (n.splitOn "_for_").length > 1 && !preNames.contains n
  let genericCount := preMono.foldl (fun acc m => acc + countGenericFns m) 0
  let preFnCount := preMono.foldl (fun acc m => acc + countAllFns m) 0
  let postFnCount := postMono.foldl (fun acc m => acc + countAllFns m) 0
  let statsLines := [
    s!"Functions before mono: {preFnCount}",
    s!"Functions after mono:  {postFnCount}",
    s!"Generic functions:     {genericCount}",
    s!"Specializations:       {specializations.length}"
  ]
  let specLines := if specializations.isEmpty then []
    else ["", "Specializations:"] ++ specializations.map fun n =>
      let parts := n.splitOn "_for_"
      match parts with
      | [base, suffix] =>
        let typeArgs := suffix.splitOn "_"
        s!"  {base}<{", ".intercalate typeArgs}> -> {n}"
      | _ => s!"  {n}"
  let body := statsLines ++ specLines
  s!"{header}\n\n{"\n".intercalate body}\n"

-- ============================================================
-- Report 6: Allocation/Cleanup Summary (--report alloc)
-- ============================================================


/-- Analyze allocation patterns in a single function. -/
private def analyzeFnAlloc (f : CFnDef) : Option String :=
  let callees := collectCallsStmts f.body |>.eraseDups
  let allocs := callees.filter isAllocCall
  let frees := callees.filter isFreeCall
  let defers := collectDefersStmts f.body
  -- Skip functions with no allocation activity
  if allocs.isEmpty && frees.isEmpty && defers.isEmpty then none
  else
    let lines : List String := [s!"  fn {f.name}:"]
    let lines := if allocs.isEmpty then lines
      else lines ++ [s!"    allocates: {", ".intercalate allocs}"]
    let lines := if frees.isEmpty then lines
      else lines ++ [s!"    frees: {", ".intercalate frees}"]
    let lines := if defers.isEmpty then lines
      else lines ++ defers.map fun d => s!"    cleanup: {d}"
    -- Warn if allocates but no free or defer
    let lines := if !allocs.isEmpty && frees.isEmpty && defers.isEmpty then
      if returnsAllocation f.retTy then
        lines ++ [s!"    note: allocates and returns — caller responsible for cleanup"]
      else
        lines ++ [s!"    WARNING: allocates but has no matching free/defer"]
    else lines
    some ("\n".intercalate lines)

private partial def allocReportModule (m : CModule) (indent : String) : Option String :=
  let fnReports := m.functions.filterMap analyzeFnAlloc
  let subReports := m.submodules.filterMap (allocReportModule · (indent ++ "  "))
  if fnReports.isEmpty && subReports.isEmpty then none
  else
    let header := s!"{indent}module {m.name}:"
    let body := fnReports ++ subReports
    some (s!"{header}\n{"\n".intercalate body}")

/-- Count allocation-related functions across module tree. -/
private partial def allocCounts (m : CModule) : Nat × Nat × Nat :=
  let fnResults := m.functions.map fun f =>
    let callees := collectCallsStmts f.body |>.eraseDups
    let allocs := callees.filter isAllocCall
    let frees := callees.filter isFreeCall
    let defers := collectDefersStmts f.body
    (allocs.isEmpty, frees.isEmpty, defers.isEmpty)
  let allocating := (fnResults.filter fun (a, _, _) => !a).length
  let freeing := (fnResults.filter fun (_, f, _) => !f).length
  let deferring := (fnResults.filter fun (_, _, d) => !d).length
  m.submodules.foldl (fun (a, f, d) sub =>
    let (a', f', d') := allocCounts sub
    (a + a', f + f', d + d')) (allocating, freeing, deferring)

def allocReport (modules : List CModule) : String :=
  let header := "=== Allocation/Cleanup Summary ==="
  let body := modules.filterMap (allocReportModule · "")
  let (allocCount, freeCount, deferCount) :=
    modules.foldl (fun (a, f, d) m =>
      let (a', f', d') := allocCounts m
      (a + a', f + f', d + d')) (0, 0, 0)
  if body.isEmpty then s!"{header}\n\nNo allocation activity found.\n"
  else
    let summary := s!"\nTotals: {allocCount} functions allocate, {freeCount} free, {deferCount} use defer"
    s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================
-- Report 7: Authority Budget (--report authority)
-- ============================================================
-- For each capability, show which functions require it and
-- compute the transitive call chain that introduces it.


/-- All function defs across modules (flat). -/
private partial def collectAllFnDefs (m : CModule) : List CFnDef :=
  m.functions ++ m.submodules.foldl (fun acc sub => acc ++ collectAllFnDefs sub) []

/-- Find one call chain from `fn` to any function that directly declares `cap`.
    Returns the chain as a list of function names, or empty if no chain found.
    Uses BFS with visited set to avoid cycles. -/
private def findCapChain (callGraph : CallGraph) (lookup : CapLookup)
    (fnName : String) (cap : String) (maxDepth : Nat := 20) : List String :=
  let rec bfs (queue : List (String × List String)) (visited : List String)
      (fuel : Nat) : List String :=
    match fuel, queue with
    | 0, _ => []
    | _, [] => []
    | fuel + 1, (current, path) :: rest =>
      -- Check if current function directly has this cap
      let directCaps := match lookup.find? (fun (n, _) => n == current) with
        | some (_, cs) => cs.concreteCaps
        | none =>
          match resolveIntrinsic current with
          | some iid => match iid.capability with
            | some c => [c]
            | none => []
          | none => []
      if directCaps.contains cap && path.length > 0 then
        path ++ [current]
      else
        let callees := match callGraph.find? (fun (n, _) => n == current) with
          | some (_, cs) => cs
          | none => []
        let newVisited := visited ++ [current]
        let newEntries := callees.filter (fun c => !visited.contains c) |>.map fun c =>
          (c, path ++ [current])
        bfs (rest ++ newEntries) newVisited fuel
  bfs [(fnName, [])] [] maxDepth

/-- Group functions by capability for the authority report. -/
private partial def collectFnsWithCap (m : CModule) (cap : String) : List CFnDef :=
  let matching := m.functions.filter fun f =>
    let (caps, _) := f.capSet.normalize
    caps.contains cap
  matching ++ m.submodules.foldl (fun acc sub => acc ++ collectFnsWithCap sub cap) []

def authorityReport (modules : List CModule) : String :=
  let header := "=== Authority Report ==="
  let qualLookup := buildQualCapLookup modules
  let callGraph := buildCallGraph modules
  let bareToQual := modules.foldl (fun acc m => acc ++ buildBareToQualMap m) []
  let allCaps := validCaps  -- all 9 capabilities
  let sections := allCaps.filterMap fun cap =>
    let fns := modules.foldl (fun acc m => acc ++ collectFnsWithCap m cap) []
    if fns.isEmpty then none
    else
      let capHeader := s!"capability {cap} ({fns.length} functions):"
      let fnLines := fns.map fun f =>
        let pubStr := if f.isPublic then "pub " else "    "
        let qualName := match bareToQual.find? (fun (b, _) => b == f.name) with
          | some (_, q) => q | none => f.name
        let chain := findCapChain callGraph qualLookup qualName cap
        -- Display bare names in chain output for readability
        let bareChain := chain.map fun n =>
          match n.splitOn "." |>.getLast? with | some b => b | none => n
        let chainStr := if bareChain.length <= 1 then "  <- declared"
          else s!"  <- {" -> ".intercalate bareChain}"
        s!"  {pubStr}{f.name}{chainStr}"
      some (s!"{capHeader}\n{"\n".intercalate fnLines}")
  let allFns := modules.foldl (fun acc m => acc ++ collectAllFnDefs m) []
  let pureFns := (allFns.filter fun f => f.capSet.isEmpty).length
  let totalFns := allFns.length
  let externCount := modules.foldl (fun acc m => acc + countModuleExterns m) 0
  let summary := s!"\nTotals: {totalFns} functions ({pureFns} pure, {totalFns - pureFns} with capabilities), {externCount} externs"
  if sections.isEmpty then s!"{header}\n\nAll functions are pure — no capabilities required.\n"
  else s!"{header}\n\n{"\n\n".intercalate sections}\n{summary}\n"

-- ============================================================
-- Report 8: Proof Eligibility (--report proof)
-- ============================================================
-- Determines which functions could be extracted for ProofCore
-- (pure, no trusted, no extern calls, no raw pointer ops).

/-- Reasons a function is excluded from ProofCore. -/
private def proofExclusionReasons (externNames : List String) (f : CFnDef) : List String :=
  let reasons : List String := []
  -- 1. Has capabilities
  let reasons := if !f.capSet.isEmpty then
    let (caps, _) := f.capSet.normalize
    reasons ++ [s!"requires capabilities: {", ".intercalate caps}"]
  else reasons
  -- 2. Is trusted
  let reasons := if f.isTrusted then reasons ++ ["trusted boundary"] else reasons
  -- 3. Calls extern functions
  let callees := collectCallsStmts f.body |>.eraseDups
  let externCalls := callees.filter fun c => externNames.contains c
  let reasons := if !externCalls.isEmpty then
    reasons ++ [s!"calls extern: {", ".intercalate externCalls}"]
  else reasons
  -- 4. Raw pointer operations
  let reasons := if hasRawPtrOpsStmts f.body then
    reasons ++ ["raw pointer operations"]
  else reasons
  reasons

private partial def proofReportModule (externNames : List String) (m : CModule) (indent : String)
    : String :=
  let header := s!"{indent}module {m.name}:"
  let fnLines := m.functions.map fun f =>
    let reasons := proofExclusionReasons externNames f
    if reasons.isEmpty then
      s!"{indent}  ✓ {f.name}"
    else
      let reasonStr := ", ".intercalate reasons
      s!"{indent}  ✗ {f.name}  ({reasonStr})"
  let subLines := m.submodules.map (proofReportModule externNames · (indent ++ "  "))
  let body := fnLines ++ subLines
  s!"{header}\n{"\n".intercalate body}"

def proofReport (modules : List CModule) (pc : Concrete.ProofCore) : String :=
  let header := "=== Proof Eligibility Report ==="
  let externNames := pc.externNames
  let body := modules.map (proofReportModule externNames · "")
  let allFns := modules.foldl (fun acc m => acc ++ collectAllFnDefs m) []
  let eligible := allFns.filter fun f =>
    (proofExclusionReasons externNames f).isEmpty
  let excluded := allFns.length - eligible.length
  let summary := s!"\nTotals: {allFns.length} functions, {eligible.length} eligible for ProofCore, {excluded} excluded"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"


-- ============================================================
-- Report 10: Combined Effects Summary (--report effects)
-- ============================================================
-- Per-function view unifying: capabilities, allocation class,
-- recursion/cycle status, loop boundedness, crosses FFI, uses trusted.

/-- Per-function effects summary record. -/
private structure FnEffects where
  name       : String         -- bare name for display
  qualName   : String         -- qualified name for facts (e.g. "main.parse_byte")
  capSet     : CapSet
  allocates  : Bool
  frees      : Bool
  defers     : Bool
  recursion  : String       -- "none", "direct", or "mutual: a, b, c"
  loops      : String       -- "no loops", "bounded", "unbounded", or "mixed"
  crossesFfi : Bool         -- calls any extern function
  isTrusted  : Bool
  isPublic   : Bool
  evidence   : String       -- "enforced", "reported", or "trusted-assumption"
  loc        : Option SourceLoc  -- structured (file, line), not pre-formatted

private def fmtEffectsRow (e : FnEffects) : String :=
  let pub := if e.isPublic then "pub " else "    "
  let caps := ppCapSet e.capSet
  let allocClass :=
    if e.allocates && e.defers then "alloc+defer"
    else if e.allocates && e.frees then "alloc+free"
    else if e.allocates then "alloc"
    else if e.frees then "free-only"
    else if e.defers then "defer-only"
    else "none"
  let trusted := if e.isTrusted then "yes" else "no"
  let ffi := if e.crossesFfi then "yes" else "no"
  let locSuffix := match e.loc with | some l => s!"  @ {fmtLoc (some l)}" | none => ""
  s!"  {pub}{e.name}\n    caps: {caps}  alloc: {allocClass}  recursion: {e.recursion}  loops: {e.loops}  ffi: {ffi}  trusted: {trusted}  evidence: {e.evidence}{locSuffix}"

private partial def effectsForModule
    (externNames : List String)
    (recMap : List (String × RecursionKind × List String))
    (locMap : FnLocMap)
    (pc : Concrete.ProofCore)
    (m : CModule) (modulePath : String := "") : List FnEffects :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let fns := m.functions.map fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let callees := collectCallsStmts f.body |>.eraseDups
    let allocs := callees.filter isAllocCall
    let frees := callees.filter isFreeCall
    let defs := collectDefersStmts f.body
    let rec_ := match recMap.find? (fun (n, _, _) => n == qualName) with
      | some (_, .direct, _) => "direct"
      | some (_, .mutual, members) =>
        let others := members.filter (· != qualName)
        s!"mutual: {", ".intercalate others}"
      | _ => "none"
    let crossesFfi := callees.any fun c => externNames.contains c
    let loopClass := classifyLoops f.body
    -- Evidence level: enforced if passes all 5 predictable gates
    let (concreteCaps, _) := f.capSet.normalize
    let hasRecursion := rec_ != "none"
    let hasUnboundedLoops := loopClass == "unbounded" || loopClass == "mixed"
    let hasAllocEvidence := !allocs.isEmpty || concreteCaps.any (· == "Alloc")
    let hasFfi := crossesFfi
    let hasBlocking := concreteCaps.any fun c =>
      c == "File" || c == "Network" || c == "Process"
    let passesProfile := !hasRecursion && !hasUnboundedLoops && !hasAllocEvidence && !hasFfi && !hasBlocking
    let obl := pc.obligations.find? fun o => o.functionId.qualName == qualName
    let evidenceLevel :=
      if f.isTrusted then "trusted-assumption"
      else match obl with
      | some o => match o.status with
        | .proved => if passesProfile then "proved" else "reported"
        | .stale => if passesProfile then "enforced (proof stale: body changed)" else "reported"
        | .trusted => "trusted-assumption"
        | _ => if passesProfile then "enforced" else "reported"
      | none => if passesProfile then "enforced" else "reported"
    { name := f.name
      qualName := qualName
      capSet := f.capSet
      allocates := !allocs.isEmpty
      frees := !frees.isEmpty
      defers := !defs.isEmpty
      recursion := rec_
      loops := loopClass
      crossesFfi := crossesFfi
      isTrusted := f.isTrusted
      isPublic := f.isPublic
      evidence := evidenceLevel
      loc := lookupLoc locMap qualName }
  fns ++ m.submodules.foldl (fun acc sub =>
    acc ++ effectsForModule externNames recMap locMap pc sub qualPrefix) []

def effectsReport (modules : List CModule) (locMap : FnLocMap := [])
    (pc : Concrete.ProofCore) : String :=
  let header := "=== Combined Effects Report ==="
  -- Use shared analysis results from ProofCore
  let recMap := pc.recMap
  let externNames := pc.externNames
  -- Collect per-function effects
  let allEffects := modules.foldl (fun acc m =>
    acc ++ effectsForModule externNames recMap locMap pc m) []
  -- Format per-module
  let body := modules.map fun m =>
    let modEffects := effectsForModule externNames recMap locMap pc m
    let fnLines := modEffects.map fmtEffectsRow
    s!"module {m.name}:\n{"\n".intercalate fnLines}"
  -- Summary counts
  let total := allEffects.length
  let pure := (allEffects.filter fun e => e.capSet == .empty).length
  let allocating := (allEffects.filter (·.allocates)).length
  let recursive := (allEffects.filter fun e => e.recursion != "none").length
  let unboundedLoops := (allEffects.filter fun e => e.loops == "unbounded" || e.loops == "mixed").length
  let ffi := (allEffects.filter (·.crossesFfi)).length
  let trusted := (allEffects.filter (·.isTrusted)).length
  let proved := (allEffects.filter fun e => e.evidence == "proved").length
  let enforced := (allEffects.filter fun e => e.evidence.startsWith "enforced").length
  let trustedAssumption := (allEffects.filter fun e => e.evidence == "trusted-assumption").length
  let reported := (allEffects.filter fun e => e.evidence == "reported").length
  let summary := s!"\nTotals: {total} functions, {pure} pure, {allocating} allocating, {recursive} recursive, {unboundedLoops} unbounded loops, {ffi} cross FFI, {trusted} trusted\nEvidence: {proved} proved, {enforced} enforced, {trustedAssumption} trusted-assumption, {reported} reported"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

/-- Format the recursion report. -/
def recursionReport (pc : Concrete.ProofCore) : String :=
  let header := "=== Recursion / Call-Cycle Report ==="
  let graph := pc.callGraph
  let sccs := tarjanSCC graph
  let classifications := pc.recMap
  -- Separate into categories
  let directRec := classifications.filter fun (_, k, _) => k == .direct
  let mutualRec := classifications.filter fun (_, k, _) => k == .mutual
  let nonRec := classifications.filter fun (_, k, _) => k == .none
  -- Group mutual recursion by cycle (deduplicate SCC listings)
  let mutualCycles := sccs.filter (fun scc => scc.length > 1)
  -- Build output
  let directSection :=
    if directRec.isEmpty then []
    else
      [s!"Direct recursion ({directRec.length} functions):"] ++
      directRec.map (fun (fn, _, _) => s!"  {fn} -> {fn}") ++ [""]
  let mutualSection :=
    if mutualCycles.isEmpty then []
    else
      [s!"Mutual recursion ({mutualCycles.length} cycles):"] ++
      mutualCycles.map (fun cycle =>
        let cycleStr := " -> ".intercalate cycle
        match cycle.head? with
        | some h => s!"  cycle: {cycleStr} -> {h}"
        | none => s!"  cycle: {cycleStr}") ++ [""]
  let totalFns := classifications.length
  let recursiveFns := directRec.length + mutualRec.length
  let summaryLine := s!"Totals: {totalFns} functions, {nonRec.length} non-recursive, {directRec.length} direct recursion, {mutualRec.length} in mutual cycles"
  let acyclicNote :=
    if recursiveFns == 0 then ["", "No recursion detected — all call paths are acyclic."]
    else []
  let allLines := [header, ""] ++ directSection ++ mutualSection ++ [summaryLine] ++ acyclicNote
  "\n".intercalate allLines ++ "\n"

-- ============================================================
-- Report 11: Stack-Depth Report (--report stack-depth)
-- ============================================================
-- For functions that pass the no-recursion profile (recursion: none),
-- reports:
--   1. Frame size (bytes): sum of parameter and local variable sizes
--   2. Max call depth: longest path in the acyclic call graph
--   3. Worst-case stack bound (bytes): sum of frame sizes along deepest chain
--
-- Recursive functions are listed separately with "unbounded" stack depth.

/-- Per-function stack-depth record. -/
private structure FnStackInfo where
  qualName   : String
  name       : String
  frameBytes : Nat
  callDepth  : Nat           -- 0 = leaf
  stackBound : Nat           -- worst-case bytes through this call chain
  isRecursive : Bool
  loc        : Option SourceLoc

/-- Collect all local variable types from a function body (recursive into nested scopes). -/
private partial def collectLocalTys : List CStmt → List Ty
  | [] => []
  | s :: rest =>
    let here := match s with
    | .letDecl _ _ ty _ => [ty]
    | .ifElse _ then_ (some else_) => collectLocalTys then_ ++ collectLocalTys else_
    | .ifElse _ then_ none => collectLocalTys then_
    | .while_ _ body _ step => collectLocalTys body ++ collectLocalTys step
    | .borrowIn _ _ _ _ refTy body => [refTy] ++ collectLocalTys body
    | _ => []
    here ++ collectLocalTys rest

/-- Compute frame size in bytes for a function: parameters + locals. -/
private def computeFrameBytes (ctx : Layout.Ctx) (f : CFnDef) : Nat :=
  let paramBytes := f.params.foldl (fun acc (_, ty) => acc + Layout.tySize ctx ty) 0
  let localTys := collectLocalTys f.body
  let localBytes := localTys.foldl (fun acc ty => acc + Layout.tySize ctx ty) 0
  -- Return address (8 bytes on 64-bit)
  8 + paramBytes + localBytes

/-- Compute max call depth and worst-case stack bound for each non-recursive function.
    Uses memoized DFS on the acyclic call graph. -/
private partial def computeCallDepths
    (frameSizes : List (String × Nat))
    (callGraph : CallGraph)
    (recMap : List (String × RecursionKind × List String))
    : List (String × Nat × Nat) :=  -- (qualName, maxDepth, stackBound)
  let isRecursive (fn : String) : Bool :=
    match recMap.find? (fun (n, _, _) => n == fn) with
    | some (_, .none, _) => false
    | some _ => true
    | none => false
  let getFrame (fn : String) : Nat :=
    match frameSizes.find? (fun (n, _) => n == fn) with
    | some (_, sz) => sz
    | none => 0
  let getCallees (fn : String) : List String :=
    match callGraph.find? (fun (n, _) => n == fn) with
    | some (_, cs) => cs
    | none => []
  -- Memoized DFS: returns (depth, stackBound) for a function
  let rec dfs (fn : String) (visited : List String)
      (memo : List (String × Nat × Nat)) (fuel : Nat)
      : (Nat × Nat) × List (String × Nat × Nat) :=
    match fuel with
    | 0 => ((0, getFrame fn), memo)
    | fuel + 1 =>
      match memo.find? (fun (n, _, _) => n == fn) with
      | some (_, d, s) => ((d, s), memo)
      | none =>
        if isRecursive fn || visited.contains fn then
          ((0, getFrame fn), memo)
        else
          let callees := getCallees fn |>.filter (fun c =>
            !isRecursive c && frameSizes.any (fun (n, _) => n == c))
          let (maxD, maxS, memo) := callees.foldl (fun (bestD, bestS, m) c =>
            let ((cd, cs), m) := dfs c (fn :: visited) m fuel
            let d := cd + 1
            let s := cs + getFrame fn
            if s > bestS then (d, s, m) else (bestD, bestS, m)
          ) (0, getFrame fn, memo)
          let memo := memo ++ [(fn, maxD, maxS)]
          ((maxD, maxS), memo)
  -- Process all non-recursive functions
  let allFns := frameSizes.filter (fun (n, _) => !isRecursive n)
  let fuel := frameSizes.length * frameSizes.length + frameSizes.length
  let memo := allFns.foldl (fun (memo : List (String × Nat × Nat)) (fn, _) =>
    let (_, memo) := dfs fn [] memo fuel
    memo
  ) []
  memo

private def fmtStackRow (info : FnStackInfo) : String :=
  let locSuffix := match info.loc with | some l => s!"  @ {fmtLoc (some l)}" | none => ""
  if info.isRecursive then
    s!"  {info.name}\n    frame: {info.frameBytes} bytes  depth: unbounded (recursive)  stack: unbounded{locSuffix}"
  else
    s!"  {info.name}\n    frame: {info.frameBytes} bytes  depth: {info.callDepth}  stack: {info.stackBound} bytes{locSuffix}"

private partial def collectStackFns
    (ctx : Layout.Ctx)
    (recMap : List (String × RecursionKind × List String))
    (locMap : FnLocMap)
    (m : CModule) (pfx : String := "")
    : List (String × String × Nat × Bool × Option SourceLoc) :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let fns := m.functions.map fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let frame := computeFrameBytes ctx f
    let isRec := match recMap.find? (fun (n, _, _) => n == qualName) with
      | some (_, .none, _) => false
      | some _ => true
      | none => false
    (qualName, f.name, frame, isRec, lookupLoc locMap qualName)
  fns ++ m.submodules.foldl (fun acc sub =>
    acc ++ collectStackFns ctx recMap locMap sub qualPrefix) []

/-- Stack-depth report for functions passing the no-recursion profile. -/
def stackDepthReport (modules : List CModule) (locMap : FnLocMap := [])
    (pc : Concrete.ProofCore) : String :=
  let ctx := buildLayoutCtx modules
  let recMap := pc.recMap
  let callGraph := pc.callGraph
  let allFns := modules.foldl (fun acc m =>
    acc ++ collectStackFns ctx recMap locMap m) []
  let frameSizes := allFns.map fun (qn, _, frame, _, _) => (qn, frame)
  -- Compute call depths
  let depths := computeCallDepths frameSizes callGraph recMap
  -- Build FnStackInfo records
  let infos := allFns.map fun (qn, name, frame, isRec, loc) =>
    if isRec then
      { qualName := qn, name := name, frameBytes := frame,
        callDepth := 0, stackBound := 0, isRecursive := true, loc := loc : FnStackInfo }
    else
      match depths.find? (fun (n, _, _) => n == qn) with
      | some (_, d, s) =>
        { qualName := qn, name := name, frameBytes := frame,
          callDepth := d, stackBound := s, isRecursive := false, loc := loc : FnStackInfo }
      | none =>
        { qualName := qn, name := name, frameBytes := frame,
          callDepth := 0, stackBound := frame, isRecursive := false, loc := loc : FnStackInfo }
  -- Format
  let header := "=== Stack-Depth Report ==="
  let body := modules.map fun m =>
    let qualPrefix := m.name
    let modInfos := infos.filter fun i => i.qualName.startsWith (qualPrefix ++ ".")
    let fnLines := modInfos.map fmtStackRow
    s!"module {m.name}:\n{"\n".intercalate fnLines}"
  -- Summary
  let bounded := infos.filter (!·.isRecursive)
  let recursive := infos.filter (·.isRecursive)
  let maxStack := bounded.foldl (fun best i => if i.stackBound > best then i.stackBound else best) 0
  let maxName := match bounded.foldl (fun best i =>
    match best with
    | none => some i
    | some b => if i.stackBound > b.stackBound then some i else best) none with
    | some i => i.name
    | none => "(none)"
  let summary := s!"\nTotals: {infos.length} functions, {bounded.length} bounded, {recursive.length} recursive (unbounded)\nMax stack bound: {maxStack} bytes ({maxName})"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

-- ============================================================
-- Profile checks (--check predictable)
-- ============================================================
-- Enforces the predictable-execution profile gate:
--   1. No recursion / call cycles
--   2. No unbounded or mixed loop classifications
--   3. No allocation (alloc/vec_new intrinsic calls)
--   4. No FFI (extern function calls)
--   5. No blocking (File/Network/Process capabilities)
-- Returns a list of violation strings (empty = pass).

/-- Capabilities that imply potentially blocking I/O. -/
private def blockingCaps : List String :=
  ["File", "Network", "Process"]

/-- A single profile violation. -/
structure ProfileViolation where
  fnName        : String         -- bare name for display
  qualName      : String         -- qualified name for facts (e.g. "main.parse_byte")
  reason        : String
  hint          : String := ""                -- suggested fix
  loc           : Option SourceLoc := none    -- function definition
  violationLoc  : Option SourceLoc := none    -- offending construct (loop, call, etc.)
  violationSpan : Option Span := none         -- full span for caret rendering

partial def checkPredictableModule
    (recMap : List (String × RecursionKind × List String))
    (externNames : List String)
    (locMap : FnLocMap)
    (m : CModule) (modulePath : String := "") : List ProfileViolation :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let fnViolations := m.functions.foldl (fun acc f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let fnLoc := lookupLoc locMap qualName
    let entry := lookupBody locMap qualName
    let astBody := match entry with | some e => e.body | none => []
    let fileStr := match entry with | some e => e.file | none => ""
    let mkViolLoc (sp : Option Span) : Option SourceLoc :=
      sp.bind fun s => if fileStr == "" then none else some (fileStr, s.line)
    -- 1. Recursion (function-level only for now)
    let recViolations := match recMap.find? (fun (n, _, _) => n == qualName) with
      | some (_, .direct, _) =>
        [{ fnName := f.name, qualName := qualName, reason := "direct recursion"
         , hint := "Use a loop or iterative approach instead of self-calls."
         , loc := fnLoc }]
      | some (_, .mutual, members) =>
        let others := members.filter (· != qualName)
        [{ fnName := f.name, qualName := qualName, reason := s!"mutual recursion with {", ".intercalate others}"
         , hint := "Break the cycle by restructuring into a loop or state machine."
         , loc := fnLoc }]
      | _ => []
    -- 2. Loop boundedness — point at the offending loop
    let loopClass := classifyLoops f.body
    let loopSpan := findLoopSpan astBody
    let loopViolLoc := mkViolLoc loopSpan
    let loopViolations :=
      if loopClass == "unbounded" then
        [{ fnName := f.name, qualName := qualName, reason := "unbounded loops"
         , hint := "Use a for loop with an explicit bound: for (let mut i = 0; i < n; i = i + 1)"
         , loc := fnLoc, violationLoc := loopViolLoc, violationSpan := loopSpan }]
      else if loopClass == "mixed" then
        [{ fnName := f.name, qualName := qualName, reason := "mixed loop boundedness (some loops are unbounded)"
         , hint := "Replace while(true) or while(flag) with a bounded for loop."
         , loc := fnLoc, violationLoc := loopViolLoc, violationSpan := loopSpan }]
      else []
    -- 3. Allocation — point at the allocating call
    let callees := collectCallsStmts f.body |>.eraseDups
    let allocs := callees.filter isAllocCall
    let (fnCaps, _) := f.capSet.normalize
    let hasAllocCap := fnCaps.any (· == "Alloc")
    let allocSpan := findCallSpan allocs astBody
    let allocViolLoc := mkViolLoc allocSpan
    let allocViolations :=
      if !allocs.isEmpty then
        [{ fnName := f.name, qualName := qualName, reason := s!"allocates ({", ".intercalate allocs})"
         , hint := "Use a fixed-size array or stack buffer instead of heap allocation."
         , loc := fnLoc, violationLoc := allocViolLoc, violationSpan := allocSpan }]
      else if hasAllocCap then
        [{ fnName := f.name, qualName := qualName, reason := "has Alloc capability"
         , hint := "Remove Alloc from the with(...) clause if this function does not need heap allocation."
         , loc := fnLoc, violationSpan := match entry with | some e => some e.fnSpan | none => none }]
      else []
    -- 4. FFI — point at the extern call
    let externCalls := callees.filter fun c => externNames.contains c
    let ffiSpan := findCallSpan externCalls astBody
    let ffiViolLoc := mkViolLoc ffiSpan
    let ffiViolations := if externCalls.isEmpty then []
      else [{ fnName := f.name, qualName := qualName, reason := s!"calls extern ({", ".intercalate externCalls})"
            , hint := "Move the extern call to a non-predictable wrapper and call that instead."
            , loc := fnLoc, violationLoc := ffiViolLoc, violationSpan := ffiSpan }]
    -- 5. Blocking — points at function signature (with-clause has no separate span)
    let (concreteCaps, _) := f.capSet.normalize
    let blockingUsed := concreteCaps.filter fun c => blockingCaps.contains c
    let blockViolations := if blockingUsed.isEmpty then []
      else [{ fnName := f.name, qualName := qualName, reason := s!"may block ({", ".intercalate blockingUsed})"
            , hint := s!"Remove {", ".intercalate blockingUsed} from with(...) or move I/O to a non-predictable caller."
            , loc := fnLoc, violationSpan := match entry with | some e => some e.fnSpan | none => none }]
    acc ++ recViolations ++ loopViolations ++ allocViolations ++ ffiViolations ++ blockViolations) []
  fnViolations ++ m.submodules.foldl (fun acc sub =>
    acc ++ checkPredictableModule recMap externNames locMap sub qualPrefix) []

/-- Extract a 1-indexed line from source text. Returns "" if out of bounds. -/
private def getSourceLine (source : String) (lineNum : Nat) : String :=
  let lines := source.splitOn "\n"
  if lineNum == 0 then ""
  else match lines[lineNum - 1]? with
    | some l => l
    | none => ""

/-- Render a violation with Elm-style snippet formatting. -/
private def renderViolation (v : ProfileViolation) (sourceMap : SourceMap) : String :=
  -- Header: location + label
  let locStr := match v.loc with | some l => s!"{fmtLoc (some l)}: " | none => ""
  let header := s!"-- {locStr}{v.fnName} — {v.reason}"
  -- Source snippet at the violation point (or function if no violation span)
  let snippetSpan := v.violationSpan.orElse fun _ =>
    match v.loc with | some (_, line) => some { line, col := 1 } | none => none
  let snippetFile := match v.violationLoc with
    | some (f, _) => f
    | none => match v.loc with | some (f, _) => f | none => ""
  let source := sourceMap.lookup snippetFile
  let snippet := match snippetSpan, source with
    | some sp, some src =>
      let line := getSourceLine src sp.line
      if line.isEmpty then ""
      else
        let lineNumStr := toString sp.line
        let pad := lineNumStr.length
        let gutter := String.ofList (List.replicate pad ' ')
        let caretStart := if sp.col > 1 then sp.col - 1 else 0
        let caretLen := if sp.endCol > sp.col then sp.endCol - sp.col else line.length - caretStart
        let spaces := String.ofList (List.replicate caretStart ' ')
        let carets := String.ofList (List.replicate caretLen '^')
        s!"\n\n {lineNumStr} | {line}\n {gutter} | {spaces}{carets}"
    | _, _ => ""
  -- Hint
  let hintStr := if v.hint.isEmpty then "" else s!"\n\n  hint: {v.hint}"
  s!"{header}{snippet}{hintStr}"

/-- Check the predictable-execution profile. Returns (pass, report string). -/
def checkPredictable (modules : List CModule) (locMap : FnLocMap := [])
    (sourceMap : SourceMap := []) (pc : Concrete.ProofCore) : Bool × String :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  let violations := modules.foldl (fun acc m =>
    acc ++ checkPredictableModule recMap externNames locMap m) []
  let allFns := modules.foldl (fun acc m => acc ++ collectAllFnDefs m) []
  let violatingFns := (violations.map (·.fnName)).eraseDups
  let passingFns := allFns.length - violatingFns.length
  if violations.isEmpty then
    (true, s!"predictable profile: pass ({allFns.length} functions checked)\n")
  else
    let header := "predictable profile: FAIL"
    let lines := violations.map fun v => renderViolation v sourceMap
    let summary := s!"{violatingFns.length} function(s) failed, {passingFns} passed"
    (false, s!"{header}\n\n{"\n\n".intercalate lines}\n\n{summary}\n")

-- ============================================================
-- Proof Eligibility Assessment (first-class, shared by all
-- proof pipeline consumers: proof-status, extraction,
-- obligations, traceability, effects evidence)
-- ============================================================
-- This is the single authoritative source for "is this function
-- in the provable subset?" It runs BEFORE extraction or proof
-- matching, and every downstream consumer reads it.

/-- Render the eligibility report (--report eligibility). -/
def eligibilityReport (pc : Concrete.ProofCore) : String :=
  let entries := pc.allEligibility
  let header := "=== Proof Eligibility Assessment ==="
  let body := entries.map fun e =>
    let locStr := fmtLoc e.loc
    if e.isTrusted then
      s!"  trusted    `{e.qualName}`  @ {locStr}\n             proof bypassed (trusted assumption)"
    else if e.eligible then
      s!"  eligible   `{e.qualName}`  @ {locStr}\n             in provable subset: pure, bounded, no FFI"
    else
      let srcStr := if e.sourceReasons.isEmpty then "" else
        s!"\n             source: {", ".intercalate e.sourceReasons}"
      let profStr := if e.profileReasons.isEmpty then "" else
        s!"\n             profile: {", ".intercalate e.profileReasons}"
      s!"  excluded   `{e.qualName}`  @ {locStr}{srcStr}{profStr}"
  -- Summary
  let eligible := (entries.filter (·.eligible)).length
  let excluded := (entries.filter fun e => !e.eligible && !e.isTrusted).length
  let trusted := (entries.filter (·.isTrusted)).length
  let sourceOnly := (entries.filter fun e =>
    !e.eligible && !e.isTrusted && !e.sourceReasons.isEmpty && e.profileReasons.isEmpty).length
  let profileOnly := (entries.filter fun e =>
    !e.eligible && !e.isTrusted && e.sourceReasons.isEmpty && !e.profileReasons.isEmpty).length
  let bothReasons := (entries.filter fun e =>
    !e.eligible && !e.isTrusted && !e.sourceReasons.isEmpty && !e.profileReasons.isEmpty).length
  let summary := s!"Totals: {entries.length} functions — {eligible} eligible, {excluded} excluded ({sourceOnly} source, {profileOnly} profile, {bothReasons} both), {trusted} trusted"
  s!"{header}\n\n{"\n".intercalate body}\n\n{summary}\n"

-- ============================================================
-- Report: Proof Status (--report proof-status)
-- ============================================================
-- Per-function proof evidence with Elm-clear diagnostics for
-- stale, missing, and ineligible states.

/-- Proof status for a single function. -/
inductive ProofState where
  | proved          -- name + fingerprint match, extraction succeeded
  | stale           -- name matches but fingerprint changed
  | notProved       -- passes profile, extractable, no registered proof
  | blocked         -- eligible but extraction failed (unsupported constructs)
  | notEligible     -- fails profile gates (recursion, alloc, etc.)
  | trusted         -- marked trusted (bypasses proof)

/-- Canonical string for a ProofState. Uses the same terminology as
    ObligationStatus.canonical to prevent cross-surface drift. -/
def ProofState.canonical : ProofState → String
  | .proved     => "proved"
  | .stale      => "stale"
  | .notProved  => "missing"
  | .blocked    => "blocked"
  | .notEligible => "ineligible"
  | .trusted    => "trusted"

/-- Per-function proof status record. -/
structure ProofStatusEntry where
  qualName      : String
  bareName      : String
  state         : ProofState
  currentFp     : String       -- current body fingerprint
  expectedFp    : String       -- registered fingerprint (empty if no proof)
  profileGates  : List String  -- reasons the function fails profile (empty if passes)
  unsupported   : List String  -- unsupported constructs (empty unless blocked)
  specName      : String       -- spec name (from registry or derived)
  proofName     : String       -- proof/theorem name (from registry or derived)
  proofSource   : String       -- "registry" | "hardcoded" | "none"
  coverage      : String       -- proof coverage kind: point|one_direction|iff|invariant|runtime_error|full_contract|""
  loc           : Option SourceLoc
  fnSpan        : Option Span

/-- Convert ProofCore ObligationStatus to Report-side ProofState. -/
private def obligationStatusToProofState : Concrete.ObligationStatus → ProofState
  | .proved => .proved
  | .stale => .stale
  | .missing => .notProved
  | .blocked => .blocked
  | .ineligible => .notEligible
  | .trusted => .trusted

private partial def collectProofStatus
    (pc : Concrete.ProofCore)
    (locMap : FnLocMap)
    (m : CModule) (modulePath : String := "")
    (registry : ProofRegistry := []) : List ProofStatusEntry :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let entries := m.functions.map fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let entry := lookupBody locMap qualName
    let fnLoc := lookupLoc locMap qualName
    let fnSp := entry.map (·.fnSpan)
    -- Look up pre-computed obligation from ProofCore
    let obl := pc.obligations.find? fun o => o.functionId.qualName == qualName
    let state := match obl with
      | some o => obligationStatusToProofState o.status
      | none => .notEligible
    let fp := match obl with
      | some o => o.functionId.fingerprint
      | none => bodyFingerprint f.body
    let gates := match obl with
      | some o => o.profileGates
      | none => []
    let expectedFp := match obl with
      | some o => o.expectedFp
      | none => ""
    let (sName, pName, pSrc) := match obl with
      | some o => match o.spec with
        | some a => (a.specId.name, a.proofName,
            match a.source with | .registry => "registry" | .hardcoded => "hardcoded")
        | none => ("", "", "none")
      | none => ("", "", "none")
    -- Look up unsupported constructs from ProofCore entry (for blocked functions)
    let unsup := match pc.entries.find? fun e => e.qualName == qualName with
      | some e => e.unsupported
      | none => []
    -- Look up coverage classification from the registry entry.
    let coverage := match registry.find? fun re => re.function == qualName with
      | some re => re.coverage
      | none => ""
    { qualName, bareName := f.name, state, currentFp := fp, expectedFp
    , profileGates := gates, unsupported := unsup, specName := sName, proofName := pName
    , proofSource := pSrc, coverage, loc := fnLoc, fnSpan := fnSp }
  entries ++ m.submodules.foldl (fun acc sub =>
    acc ++ collectProofStatus pc locMap sub qualPrefix registry) []

/-- Render a single proof status entry with Elm-clear formatting. -/
private def renderProofStatusEntry (e : ProofStatusEntry) (sourceMap : SourceMap) : String :=
  let locStr := fmtLoc e.loc
  let fileStr := match e.loc with | some (f, _) => f | none => ""
  let source := sourceMap.lookup fileStr
  -- Source snippet
  let snippet := match e.fnSpan, source with
    | some sp, some src =>
      let line := getSourceLine src sp.line
      if line.isEmpty then ""
      else
        let lineNumStr := toString sp.line
        let pad := lineNumStr.length
        let gutter := String.ofList (List.replicate pad ' ')
        let caretLen := line.length
        let carets := String.ofList (List.replicate caretLen '^')
        s!"\n\n {lineNumStr} | {line}\n {gutter} | {carets}"
    | _, _ => ""
  match e.state with
  | .proved =>
    let coverageTag := if e.coverage.isEmpty then "" else s!" [{e.coverage}]"
    s!"-- proved{coverageTag} {String.ofList (List.replicate 48 '-')} {locStr}\n\n  ✓ `{e.qualName}` — proof matches current body.{snippet}\n\n  coverage: {if e.coverage.isEmpty then "unclassified" else e.coverage}"
  | .stale =>
    s!"-- proof stale {String.ofList (List.replicate 44 '-')} {locStr}\n\n  Function `{e.qualName}` has a registered proof, but the body changed.{snippet}\n\n  expected fingerprint:\n    {e.expectedFp}\n\n  current fingerprint:\n    {e.currentFp}\n\n  hint: Update the Lean proof in Concrete/Proof.lean, or restore the proved implementation."
  | .notProved =>
    s!"-- no proof {String.ofList (List.replicate 47 '-')} {locStr}\n\n  `{e.qualName}` passes the predictable profile but has no registered proof.{snippet}\n\n  current fingerprint:\n    {e.currentFp}\n\n  hint: Add a Lean proof for this function in Concrete/Proof.lean with the fingerprint above."
  | .blocked =>
    let unsupStr := if e.unsupported.isEmpty then "unsupported constructs"
        else ", ".intercalate e.unsupported
    s!"-- blocked {String.ofList (List.replicate 48 '-')} {locStr}\n\n  `{e.qualName}` is eligible but uses unsupported constructs — extraction failed.{snippet}\n\n  unsupported: {unsupStr}\n\n  hint: Remove {unsupStr} to enable proof extraction."
  | .notEligible =>
    let gateStr := ", ".intercalate e.profileGates
    s!"-- not eligible {String.ofList (List.replicate 43 '-')} {locStr}\n\n  `{e.qualName}` cannot be proved: fails predictable profile ({gateStr}).{snippet}\n\n  reasons: {gateStr}\n\n  hint: Address these constraints to make this function eligible for proof."
  | .trusted =>
    s!"-- trusted {String.ofList (List.replicate 48 '-')} {locStr}\n\n  `{e.qualName}` is marked trusted — proof is bypassed (trusted assumption).{snippet}"

/-- Proof status report with Elm-clear diagnostics. -/
def proofStatusReport (modules : List CModule) (locMap : FnLocMap := [])
    (sourceMap : SourceMap := []) (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let header := "=== Proof Status Report ==="
  let entries := modules.foldl (fun acc m =>
    acc ++ collectProofStatus pc locMap m "" registry) []
  let body := entries.map fun e => renderProofStatusEntry e sourceMap
  -- Summary
  let proved := (entries.filter fun e => e.state matches .proved).length
  let stale := (entries.filter fun e => e.state matches .stale).length
  let notProved := (entries.filter fun e => e.state matches .notProved).length
  let blockedCnt := (entries.filter fun e => e.state matches .blocked).length
  let notEligible := (entries.filter fun e => e.state matches .notEligible).length
  let trusted := (entries.filter fun e => e.state matches .trusted).length
  let summary := s!"Totals: {entries.length} functions — {proved} proved, {stale} stale, {notProved} unproved, {blockedCnt} blocked, {notEligible} ineligible, {trusted} trusted"
  s!"{header}\n\n{"\n\n".intercalate body}\n\n{summary}\n"

/-- Program-level conformance check against `docs/PROVABLE_V1.md`.
    Classifies each function as:
    - in:        proof-eligible AND extraction succeeds (fits ProvableV1)
    - blocked:   proof-eligible BUT extraction failed on unsupported constructs
                 (the only case that counts against conformance)
    - excluded:  ineligible or trusted — outside ProvableV1 by profile design,
                 not counted against conformance
    Status is `full` iff there are no blocked functions. -/
def provableV1ConformanceReport (modules : List CModule) (locMap : FnLocMap := [])
    (registry : ProofRegistry := []) (pc : Concrete.ProofCore) : String :=
  let header := "=== ProvableV1 Conformance ==="
  let entries := modules.foldl (fun acc m =>
    acc ++ collectProofStatus pc locMap m "" registry) []
  let inEntries := entries.filter fun e =>
    e.state matches .proved || e.state matches .stale ||
    e.state matches .notProved
  let blockedEntries := entries.filter fun e => e.state matches .blocked
  let excludedEntries := entries.filter fun e =>
    e.state matches .notEligible || e.state matches .trusted
  let conformance := if blockedEntries.isEmpty then "full" else "partial"
  let inLines := inEntries.map fun e => s!"  in        `{e.qualName}`"
  let blockedLines := blockedEntries.map fun e =>
    let reason := if e.unsupported.isEmpty then "blocked"
                  else ", ".intercalate e.unsupported
    s!"  blocked   `{e.qualName}` — {reason}"
  let excludedLines := excludedEntries.map fun e =>
    let reason :=
      if e.state matches .trusted then "trusted"
      else if e.profileGates.isEmpty then "by profile"
      else ", ".intercalate e.profileGates
    s!"  excluded  `{e.qualName}` — by design ({reason})"
  let chunks :=
    (if inLines.isEmpty then []
     else [s!"Inside ProvableV1 ({inEntries.length}):"] ++ inLines) ++
    (if blockedLines.isEmpty then []
     else [""] ++ [s!"Outside ProvableV1 — construct violations ({blockedEntries.length}):"]
            ++ blockedLines) ++
    (if excludedLines.isEmpty then []
     else [""] ++ [s!"Outside ProvableV1 — by profile design ({excludedEntries.length}):"]
            ++ excludedLines)
  let footer :=
    if blockedEntries.isEmpty then
      "Every proof-eligible function fits the ProvableV1 supported surface.\nExcluded functions are excluded by the profile itself (entry points,\ntrusted impls, capability use), not counted against conformance."
    else
      s!"{blockedEntries.length} function(s) are proof-eligible but use constructs\noutside the current ProvableV1 supported surface (see docs/PROVABLE_V1.md)."
  s!"{header}\n\n{"\n".intercalate chunks}\n\nStatus: {conformance}\n{footer}\n"

/-- Compact one-line proof summary suitable for build output. -/
def proofSummaryLine (pc : Concrete.ProofCore) : String :=
  let obls := pc.obligations
  let proved := (obls.filter fun o => o.status == .proved).length
  let stale := (obls.filter fun o => o.status == .stale).length
  let missing := (obls.filter fun o => o.status == .missing).length
  let blocked := (obls.filter fun o => o.status == .blocked).length
  let ineligible := (obls.filter fun o => o.status == .ineligible).length
  let trusted := (obls.filter fun o => o.status == .trusted).length
  let eligible := proved + stale + missing + blocked
  if eligible == 0 && ineligible == 0 && trusted == 0 then
    "Proofs: no eligible functions"
  else if eligible == 0 then
    s!"Proofs: no eligible functions ({ineligible} ineligible, {trusted} trusted)"
  else if stale == 0 && missing == 0 && blocked == 0 then
    s!"Proofs: {proved}/{eligible} proved"
  else
    let parts : List String :=
      (if proved != 0 then [s!"{proved} proved"] else []) ++
      (if stale != 0 then [s!"{stale} stale"] else []) ++
      (if missing != 0 then [s!"{missing} missing"] else []) ++
      (if blocked != 0 then [s!"{blocked} blocked"] else [])
    s!"Proofs: {", ".intercalate parts}"

/-- Actionable "next steps" for the proof workflow — at most 3 items,
    prioritized: stale first, then missing, then blocked. -/
def proofNextSteps (pc : Concrete.ProofCore) : String :=
  let stales := pc.obligations.filter fun o => o.status == .stale
  let missing := pc.obligations.filter fun o => o.status == .missing
  let staleItems := stales.map fun o =>
    s!"  - fix stale proof for {o.functionId.qualName} (body changed, update fingerprint)"
  let missingItems := missing.map fun o =>
    s!"  - add proof for {o.functionId.qualName} (eligible, extractable, no registry entry)"
  let blockedItems := (pc.entries.filter fun e => e.extracted.isNone && !e.unsupported.isEmpty).map fun e =>
    s!"  - unblock {e.qualName} (uses {", ".intercalate e.unsupported})"
  let allItems := staleItems ++ missingItems ++ blockedItems
  if allItems.isEmpty then ""
  else
    let shown := allItems.take 3
    let more := if allItems.length != shown.length
        then s!"\n  ... and {allItems.length - shown.length} more"
        else ""
    s!"Next steps:\n{"\n".intercalate shown}{more}"

-- ============================================================
-- Proof obligations report (--report obligations)
-- ============================================================

/-- A single proof obligation entry (presentation layer). -/
structure ObligationEntry where
  function     : String       -- qualified name
  spec         : String       -- spec name (from registry, or empty)
  proof        : String       -- proof name (from registry/hardcoded, or empty)
  status       : String       -- proved | stale | missing | blocked | ineligible | trusted
  dependencies : List String  -- qualified names of proved helpers this function calls
  staleDeps    : List String  -- proved callees whose proof has gone stale
  fingerprint  : String       -- current body fingerprint
  source       : String       -- "registry" | "hardcoded" | "none"
  loc          : Option SourceLoc

/-- Convert a ProofCore obligation to a Report-side ObligationEntry. -/
private def obligationToEntry (o : Concrete.Obligation) : ObligationEntry :=
  let statusStr := o.status.canonical
  let (sName, pName, src) := match o.spec with
    | some a => (a.specId.name, a.proofName,
        match a.source with | .registry => "registry" | .hardcoded => "hardcoded")
    | none => ("", "", "none")
  { function := o.functionId.qualName, spec := sName, proof := pName
  , status := statusStr, dependencies := o.dependencies, staleDeps := o.staleDeps
  , fingerprint := o.functionId.fingerprint, source := src, loc := o.loc }

/-- Render the obligations report as human-readable output. -/
def obligationsReport (_modules : List CModule) (_locMap : FnLocMap := [])
    (_registry : ProofRegistry := []) (pc : Concrete.ProofCore) : String :=
  let entries := pc.obligations.map obligationToEntry
  let header := "=== Proof Obligations ==="
  let body := entries.map fun e =>
    let locStr := fmtLoc e.loc
    let depsStr := if e.dependencies.isEmpty then "none"
      else ", ".intercalate e.dependencies
    let staleDepsStr := if e.staleDeps.isEmpty then ""
      else s!"\n    stale deps:   {", ".intercalate e.staleDeps}"
    let specStr := if e.spec.isEmpty then "(none)" else e.spec
    let proofStr := if e.proof.isEmpty then "(none)" else e.proof
    s!"  {e.function}\n    status:       {e.status}\n    spec:         {specStr}\n    proof:        {proofStr}\n    source:       {e.source}\n    fingerprint:  {e.fingerprint}\n    dependencies: {depsStr}{staleDepsStr}\n    loc:          {locStr}"
  let proved := (entries.filter fun e => e.status == "proved").length
  let stale := (entries.filter fun e => e.status == "stale").length
  let missing := (entries.filter fun e => e.status == "missing").length
  let blockedCnt := (entries.filter fun e => e.status == "blocked").length
  let inelig := (entries.filter fun e => e.status == "ineligible").length
  let trusted := (entries.filter fun e => e.status == "trusted").length
  let summary := s!"Totals: {entries.length} obligations — {proved} proved, {stale} stale, {missing} missing, {blockedCnt} blocked, {inelig} ineligible, {trusted} trusted"
  s!"{header}\n\n{"\n\n".intercalate body}\n\n{summary}\n"

-- ============================================================
-- Proof dependency graph report (--report proof-deps)
-- ============================================================

/-- Render the proof dependency graph as a human-readable report.
    Shows proved functions and their proved/stale helper dependencies. -/
def proofDepsReport (pc : Concrete.ProofCore) : String :=
  let header := "=== Proof Dependency Graph ==="
  -- Show only obligations that have dependencies or stale deps
  let withDeps := pc.obligations.filter fun o =>
    !o.dependencies.isEmpty || !o.staleDeps.isEmpty
  let noDeps := pc.obligations.filter fun o =>
    o.status == .proved && o.dependencies.isEmpty && o.staleDeps.isEmpty
  let body := withDeps.map fun o =>
    let statusTag := o.status.canonical
    let depLines := o.dependencies.map fun d => s!"    → {d} (proved)"
    let staleLines := o.staleDeps.map fun d => s!"    → {d} (stale)"
    let allLines := depLines ++ staleLines
    s!"  {o.functionId.qualName} [{statusTag}]\n{"\n".intercalate allLines}"
  let isolatedNames := noDeps.map fun o => s!"  {o.functionId.qualName} [proved, no dependencies]"
  let provedCount := (pc.obligations.filter fun o => o.status == .proved).length
  let withStaleCount := (withDeps.filter fun o => !o.staleDeps.isEmpty).length
  let summary := s!"Summary: {provedCount} proved functions, {withDeps.length} with dependencies, {withStaleCount} with stale dependencies"
  let sections := if body.isEmpty && isolatedNames.isEmpty then ["  (no proof dependencies)"]
    else body ++ (if isolatedNames.isEmpty then [] else [""] ++ isolatedNames)
  s!"{header}\n\n{"\n\n".intercalate sections}\n\n{summary}\n"

-- ============================================================
-- Proof diagnostics report (--report proof-diagnostics)
-- ============================================================

/-- Render a proof diagnostic kind as a short label. -/
private def diagnosticKindLabel : Concrete.ProofDiagnosticKind → String
  | .staleProof => "stale_proof"
  | .missingProof => "missing_proof"
  | .ineligible => "ineligible"
  | .unsupportedConstruct => "unsupported_construct"
  | .trusted => "trusted"
  | .attachmentIntegrity => "attachment_integrity"
  | .theoremLookup => "theorem_lookup"
  | .leanCheckFailure => "lean_check_failure"

/-- Render a proof diagnostic severity as a string. -/
private def diagnosticSeverityLabel : Concrete.ProofDiagnosticSeverity → String
  | .error => "error"
  | .warning => "warning"
  | .info => "info"

/-- Render a single proof diagnostic in Elm-clear format. -/
private def renderProofDiagnostic (d : Concrete.ProofDiagnostic) : String :=
  let locStr := fmtLoc d.loc
  let sevStr := diagnosticSeverityLabel d.severity
  let kindStr := diagnosticKindLabel d.kind
  let header := s!"-- {kindStr} [{sevStr}] {String.ofList (List.replicate (40 - kindStr.length) '-')} {locStr}"
  let body := s!"  {d.message}"
  let classStr := s!"\n  failure:     {d.failureClass}\n  repair:      {d.repairClass}"
  let detailStr := if d.details.isEmpty then ""
    else s!"\n  details: {", ".intercalate d.details}"
  let fpStr := if d.fingerprint.isEmpty then ""
    else s!"\n  fingerprint: {d.fingerprint}"
  let expStr := if d.expectedFp.isEmpty then ""
    else s!"\n  expected:    {d.expectedFp}"
  let hintStr := if d.hint.isEmpty then ""
    else s!"\n  hint: {d.hint}"
  s!"{header}\n\n{body}{classStr}{detailStr}{fpStr}{expStr}{hintStr}"

/-- Human-readable proof diagnostics report. -/
def proofDiagnosticsReport (pc : Concrete.ProofCore) : String :=
  let header := "=== Proof Diagnostics ==="
  let diags := pc.diagnostics
  if diags.isEmpty then
    s!"{header}\n\nNo proof diagnostics.\n"
  else
    let body := diags.map renderProofDiagnostic
    let errors := (diags.filter fun d => d.severity == .error).length
    let warnings := (diags.filter fun d => d.severity == .warning).length
    let infos := (diags.filter fun d => d.severity == .info).length
    let summary := s!"Totals: {diags.length} diagnostics — {errors} errors, {warnings} warnings, {infos} info"
    s!"{header}\n\n{"\n\n".intercalate body}\n\n{summary}\n"

-- proofDiagnosticToFact and collectProofDiagnosticFacts are defined after locToJson

-- ============================================================
-- Source-to-ProofCore extraction report (--report extraction)
-- ============================================================

/-- Pretty-print a PExpr as a readable S-expression. -/
private partial def renderPExpr : Proof.PExpr → String
  | .lit (.int n) => toString n
  | .lit (.bool b) => toString b
  | .lit (.struct_ name _) => s!"<struct {name}>"
  | .lit (.enum_ enumName variant _) => s!"<{enumName}::{variant}>"
  | .lit (.array_ elems) => s!"<array (size {elems.length})>"
  | .var name => name
  | .binOp op lhs rhs =>
    let opStr := match op with
      | .add => "+" | .sub => "-" | .mul => "*"
      | .addw _ _ => "+w"     -- wrapping add at width (mod 2^w)
      | .mod _ true => "%"    -- signed mod (i32 srem semantics)
      | .mod _ false => "%u"  -- unsigned mod (urem semantics)
      | .div _ true => "/"    -- signed div (i32 sdiv semantics)
      | .div _ false => "/u"  -- unsigned div (udiv semantics)
      | .bitxor _ true => "^"
      | .bitxor _ false => "^u"
      | .bitor _ true => "|"
      | .bitor _ false => "|u"
      | .bitand _ true => "&"
      | .bitand _ false => "&u"
      | .shr _ _ => ">>"
      | .shl _ _ => "<<"
      | .eq => "==" | .ne => "!=" | .lt => "<"
      | .le => "<=" | .gt => ">" | .ge => ">="
    s!"({renderPExpr lhs} {opStr} {renderPExpr rhs})"
  | .letIn name val body =>
    s!"let {name} = {renderPExpr val}; {renderPExpr body}"
  | .ifThenElse cond t e =>
    s!"if {renderPExpr cond} then {renderPExpr t} else {renderPExpr e}"
  | .call fn args =>
    let argsStr := ", ".intercalate (args.map renderPExpr)
    s!"{fn}({argsStr})"
  | .structLit name fields =>
    let fieldsStr := ", ".intercalate (fields.map fun (fname, fexpr) =>
      s!"{fname}: {renderPExpr fexpr}")
    s!"{name} \{ {fieldsStr} }"
  | .enumLit enumName variant fields =>
    let fieldsStr := ", ".intercalate (fields.map fun (fname, fexpr) =>
      s!"{fname}: {renderPExpr fexpr}")
    if fields.isEmpty then s!"{enumName}::{variant}"
    else s!"{enumName}::{variant} \{ {fieldsStr} }"
  | .fieldAccess obj field =>
    s!"{renderPExpr obj}.{field}"
  | .arrayIndex arr idx =>
    s!"{renderPExpr arr}[{renderPExpr idx}]"
  | .match_ scrutinee arms =>
    let armStrs := arms.map fun (pat, body) =>
      let patStr := match pat with
        | .enumPat eName variant [] => s!"{eName}::{variant}"
        | .enumPat eName variant bindings =>
          s!"{eName}::{variant} \{ {", ".intercalate bindings} }"
        | .litPat (.int n) => toString n
        | .litPat (.bool b) => toString b
        | .litPat _ => "<lit>"
        | .varPat name => name
      s!"{patStr} => {renderPExpr body}"
    s!"match {renderPExpr scrutinee} \{ {"; ".intercalate armStrs} }"
  | .cast inner => s!"({renderPExpr inner} as _)"
  | .arrayLit elems => s!"[{", ".intercalate (elems.map renderPExpr)}]"
  | .arraySet arr idx val =>
    s!"{renderPExpr arr}.set({renderPExpr idx}, {renderPExpr val})"
  | .while_ cond assigns cont =>
    let assignStrs := assigns.map fun (n, e) => s!"{n} = {renderPExpr e}"
    s!"while {renderPExpr cond} \{ {"; ".intercalate assignStrs} }; {renderPExpr cont}"
  | .while_step cond carried step cont =>
    s!"while_step {renderPExpr cond} carried=[{", ".intercalate carried}] \{ {renderPExpr step} }; {renderPExpr cont}"

/-- Extraction entry for one function. -/
structure ExtractionEntry where
  qualName    : String
  eligible    : Bool
  extracted   : Option Proof.PExpr
  excluded    : List String  -- reasons if not eligible
  unsupported : List String  -- constructs that blocked extraction
  fingerprint : String
  params      : List String
  specName    : String       -- spec name (from registry or derived)
  proofName   : String       -- proof name (from registry or derived)
  loc         : Option SourceLoc

/-- Extract spec/proof names from a SpecAttachment, or empty strings if none. -/
private def specNames (att : Option SpecAttachment) : String × String :=
  match att with
  | some a => (a.specId.name, a.proofName)
  | none => ("", "")

/-- Build extraction entries from ProofCore — the single source of truth
    for eligibility, extraction, and fingerprinting. -/
private def extractionEntriesFromPC (pc : Concrete.ProofCore)
    (_registry : ProofRegistry := []) : List ExtractionEntry :=
  let eligible := pc.entries.map fun e =>
    let (sName, pName) := specNames e.spec
    { qualName := e.qualName, eligible := true, extracted := e.extracted
    , excluded := [], unsupported := e.unsupported, fingerprint := e.fingerprint
    , params := e.params, specName := sName, proofName := pName, loc := e.loc }
  let excludedEntries := pc.excluded.map fun e =>
    let (sName, pName) := specNames e.spec
    let reasons := e.eligibility.sourceReasons ++ e.eligibility.profileReasons
    { qualName := e.qualName, eligible := false, extracted := none
    , excluded := reasons, unsupported := [], fingerprint := e.fingerprint
    , params := e.fn.params.map (·.1), specName := sName, proofName := pName, loc := e.loc }
  eligible ++ excludedEntries

/-- Render the source-to-ProofCore extraction report. -/
def extractionReport (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let entries := extractionEntriesFromPC pc registry
  let header := "=== Source-to-ProofCore Extraction ==="
  let body := entries.map fun e =>
    let locStr := fmtLoc e.loc
    let statusStr := if e.eligible then
      match e.extracted with
      | some pexpr => s!"extracted\n    ProofCore: {renderPExpr pexpr}"
      | none => s!"eligible (extraction failed)\n    unsupported: {", ".intercalate e.unsupported}"
    else
      s!"excluded\n    reasons: {", ".intercalate e.excluded}"
    let paramStr := if e.params.isEmpty then "()" else "(" ++ ", ".intercalate e.params ++ ")"
    let specStr := if e.specName.isEmpty then "" else s!"\n    spec: {e.specName}"
    let proofStr := if e.proofName.isEmpty then "" else s!"\n    proof: {e.proofName}"
    s!"  {e.qualName}{paramStr}\n    status: {statusStr}{specStr}{proofStr}\n    fingerprint: {e.fingerprint}\n    loc: {locStr}"
  let extracted := (entries.filter fun e => e.eligible && e.extracted.isSome).length
  let eligFailed := (entries.filter fun e => e.eligible && e.extracted.isNone).length
  let excluded := (entries.filter fun e => !e.eligible).length
  let summary := s!"Totals: {entries.length} functions — {extracted} extracted, {eligFailed} eligible but not extractable, {excluded} excluded"
  s!"{header}\n\n{"\n\n".intercalate body}\n\n{summary}\n"

-- ============================================================
-- Lean theorem stubs (--report lean-stubs)
-- ============================================================

/-- Render a PExpr as Lean constructor syntax (Concrete.Proof.PExpr). -/
private partial def renderPExprAsLean : Proof.PExpr → String
  | .lit (.int n) =>
    if n < 0 then s!".lit (.int ({n}))"
    else s!".lit (.int {n})"
  | .lit (.bool b) => s!".lit (.bool {b})"
  | .lit (.struct_ name _) =>
    -- Struct literals as raw Lean constructor syntax are noisy; the
    -- canonical surface for proof attachment is the source-level
    -- fingerprint, not the Lean stub. Emit a placeholder.
    s!"/- struct value of {name} (raw form) -/"
  | .lit (.enum_ enumName variant _) =>
    s!"/- enum value {enumName}::{variant} (raw form) -/"
  | .lit (.array_ _) =>
    s!"/- array value (raw form) -/"
  | .var name => s!".var \"{name}\""
  | .binOp op lhs rhs =>
    let opStr := match op with
      | .add => ".add" | .sub => ".sub" | .mul => ".mul"
      | .addw w s => s!"(.addw {w} {s})"
      | .mod w s => s!"(.mod {w} {s})"
      | .div w s => s!"(.div {w} {s})"
      | .bitxor w s => s!"(.bitxor {w} {s})"
      | .bitor w s => s!"(.bitor {w} {s})"
      | .bitand w s => s!"(.bitand {w} {s})"
      | .shr w s => s!"(.shr {w} {s})"
      | .shl w s => s!"(.shl {w} {s})"
      | .eq => ".eq" | .ne => ".ne" | .lt => ".lt"
      | .le => ".le" | .gt => ".gt" | .ge => ".ge"
    s!".binOp {opStr}\n      ({renderPExprAsLean lhs})\n      ({renderPExprAsLean rhs})"
  | .letIn name val body =>
    s!".letIn \"{name}\"\n      ({renderPExprAsLean val})\n      ({renderPExprAsLean body})"
  | .ifThenElse cond t e =>
    s!".ifThenElse\n      ({renderPExprAsLean cond})\n      ({renderPExprAsLean t})\n      ({renderPExprAsLean e})"
  | .call fn args =>
    let argsLean := args.map fun a => s!"({renderPExprAsLean a})"
    s!".call \"{fn}\" [{", ".intercalate argsLean}]"
  | .structLit name fields =>
    let fieldsLean := fields.map fun (fname, fexpr) =>
      s!"(\"{fname}\", {renderPExprAsLean fexpr})"
    s!".structLit \"{name}\" [{", ".intercalate fieldsLean}]"
  | .enumLit enumName variant fields =>
    let fieldsLean := fields.map fun (fname, fexpr) =>
      s!"(\"{fname}\", {renderPExprAsLean fexpr})"
    s!".enumLit \"{enumName}\" \"{variant}\" [{", ".intercalate fieldsLean}]"
  | .fieldAccess obj field =>
    s!".fieldAccess ({renderPExprAsLean obj}) \"{field}\""
  | .arrayIndex arr idx =>
    s!".arrayIndex ({renderPExprAsLean arr}) ({renderPExprAsLean idx})"
  | .match_ scrutinee arms =>
    let armsLean := arms.map fun (pat, body) =>
      let patLean := match pat with
        | .enumPat eName variant bindings =>
          let bs := bindings.map fun b => s!"\"{b}\""
          s!".enumPat \"{eName}\" \"{variant}\" [{", ".intercalate bs}]"
        | .litPat (.int n) =>
          if n < 0 then s!".litPat (.int ({n}))" else s!".litPat (.int {n})"
        | .litPat (.bool b) => s!".litPat (.bool {b})"
        | .litPat _ => "/- non-int/bool litPat (raw form) -/"
        | .varPat name => s!".varPat \"{name}\""
      s!"({patLean}, {renderPExprAsLean body})"
    s!".match_\n      ({renderPExprAsLean scrutinee})\n      [{", ".intercalate armsLean}]"
  | .cast inner => s!".cast ({renderPExprAsLean inner})"
  | .arrayLit elems =>
    let elemsLean := elems.map fun e => s!"({renderPExprAsLean e})"
    s!".arrayLit [{", ".intercalate elemsLean}]"
  | .arraySet arr idx val =>
    s!".arraySet\n      ({renderPExprAsLean arr})\n      ({renderPExprAsLean idx})\n      ({renderPExprAsLean val})"
  | .while_ cond assigns cont =>
    let assignsLean := assigns.map fun (n, e) =>
      s!"(\"{n}\", {renderPExprAsLean e})"
    s!".while_\n      ({renderPExprAsLean cond})\n      [{", ".intercalate assignsLean}]\n      ({renderPExprAsLean cont})"
  | .while_step cond carried step cont =>
    let carriedLean := carried.map fun n => s!"\"{n}\""
    s!".while_step\n      ({renderPExprAsLean cond})\n      [{", ".intercalate carriedLean}]\n      ({renderPExprAsLean step})\n      ({renderPExprAsLean cont})"

/-- Convert a function's bare name to a Lean-safe identifier. -/
private def leanIdent (name : String) : String :=
  name.map fun c => if c == '-' then '_' else c

/-- Generate Lean theorem stubs for all extracted functions. -/
def leanStubsReport (pc : Concrete.ProofCore)
    (registry : ProofRegistry := []) : String :=
  let entries := extractionEntriesFromPC pc registry
  let extracted := entries.filter fun e => e.eligible && e.extracted.isSome
  if extracted.isEmpty then "-- No extractable functions found.\n"
  else
  let header := "import Concrete.Proof\n\nnamespace Concrete.Proof.Generated\n\nopen Concrete.Proof"
  -- Build function table entries
  let fnDefs := extracted.map fun e =>
    let name := leanIdent (e.qualName.splitOn "." |>.getLast!)
    let pexpr := match e.extracted with | some p => renderPExprAsLean p | none => "sorry"
    let paramsLean := e.params.map fun p => s!"\"{p}\""
    let paramsList := "[" ++ ", ".intercalate paramsLean ++ "]"
    s!"/-- Extracted from `{e.qualName}`. -/\ndef {name}Expr : PExpr :=\n    {pexpr}\n\ndef {name}Fn : PFnDef :=\n  \{ name := \"{name}\", params := {paramsList}, body := {name}Expr }"
  -- Build function table
  let tableCases := extracted.map fun e =>
    let name := leanIdent (e.qualName.splitOn "." |>.getLast!)
    s!"  | \"{name}\" => some {name}Fn"
  let tableStr := s!"def generatedFns : FnTable\n{"\n".intercalate tableCases}\n  | _ => none"
  -- Build eval helpers
  let evalHelpers := extracted.map fun e =>
    let name := leanIdent (e.qualName.splitOn "." |>.getLast!)
    let paramBinds := e.params.foldl (fun acc p =>
      if acc.isEmpty then s!"(Env.empty.bind \"{p}\" (.int {p}))"
      else s!"({acc}.bind \"{p}\" (.int {p}))"
    ) ""
    let paramSig := " ".intercalate (e.params.map fun p => s!"({p} : Int)")
    s!"def eval_{name} {paramSig} (fuel : Nat := 20) : Option PVal :=\n  eval generatedFns {paramBinds} fuel {name}Expr"
  -- Build theorem stubs
  let theoremStubs := extracted.map fun e =>
    let name := leanIdent (e.qualName.splitOn "." |>.getLast!)
    let paramSig := " ".intercalate (e.params.map fun p => s!"({p} : Int)")
    let paramBinds := e.params.foldl (fun acc p =>
      if acc.isEmpty then s!"(Env.empty.bind \"{p}\" (.int {p}))"
      else s!"({acc}.bind \"{p}\" (.int {p}))"
    ) ""
    let specName := if e.specName.isEmpty then s!"{name}_correct" else
      s!"{name}_correct"
    let pcStr := match e.extracted with | some p => renderPExpr p | none => "?"
    s!"/-- TODO: State the correctness property for `{e.qualName}`.\n    Current ProofCore: {pcStr} -/\ntheorem {specName} {paramSig} (fuel : Nat) :\n    eval generatedFns {paramBinds}\n      (fuel + 1) {name}Expr\n    = sorry := by\n  sorry"
  -- Assemble
  let body := [
    header,
    "\n" ++ "\n\n".intercalate fnDefs,
    "\n\n" ++ tableStr,
    "\n\n" ++ "\n\n".intercalate evalHelpers,
    "\n\n-- ============================================================",
    "-- Theorem stubs — fill in the specification and proof",
    "-- ============================================================",
    "\n" ++ "\n\n".intercalate theoremStubs,
    "\nend Concrete.Proof.Generated"
  ]
  "\n".intercalate body ++ "\n"

-- ============================================================
-- Source/Core/SSA/LLVM traceability (--report traceability)
-- ============================================================

/-- A traceability entry for one function through the pipeline. -/
structure TraceEntry where
  sourceFunction : String       -- qualified source name
  fingerprint    : String       -- body fingerprint at Core
  extractionStatus : String     -- extracted | eligible_not_extractable | excluded
  proofCoreForm  : String       -- readable PExpr if extracted, else ""
  evidenceLevel  : String       -- proved | enforced | reported | trusted-assumption
  specName       : String       -- spec name (from registry or derived)
  proofName      : String       -- proof name (from registry or derived)
  coreNames      : List String  -- Core function names (pre-mono)
  monoNames      : List String  -- monomorphized specialization names
  ssaNames       : List String  -- SSA function names
  llvmNames      : List String  -- LLVM symbol names
  claimBoundary  : String       -- where the claim stops being guaranteed
  loc            : Option SourceLoc

/-- Collect all function names from SSA modules. -/
private partial def collectSSANames (sm : SModule) (pfx : String := "") : List (String × String) :=
  let modPfx := if pfx.isEmpty then sm.name else pfx ++ "." ++ sm.name
  let fns := sm.functions.map fun f => (f.name, modPfx)
  fns

/-- Collect all function names from monomorphized Core modules. -/
private partial def collectMonoFnNames (m : CModule) (pfx : String := "") : List String :=
  let modPfx := if pfx.isEmpty then m.name else pfx ++ "." ++ m.name
  let fns := m.functions.map fun f => modPfx ++ "." ++ f.name
  fns ++ m.submodules.foldl (fun acc sub => acc ++ collectMonoFnNames sub modPfx) []

/-- Determine the LLVM symbol name for a function. -/
private def llvmSymbol (name : String) (isEntry : Bool) : String :=
  if isEntry then "user_main" else name

/-- Determine the claim boundary for a function. -/
private def claimBoundaryFor (evidence : String) (extracted : Bool) : String :=
  match evidence with
  | "proved" =>
    if extracted then "ProofCore (source-level proof, not preserved past Core)"
    else "source (proof not extractable to ProofCore)"
  | "enforced" => "source (passes predictable profile, no proof)"
  | "trusted-assumption" => "source (trusted, no verification)"
  | "reported" => "source (fails predictable profile)"
  | _ => "source"

/-- Build traceability entries by walking Core functions and looking up
    their counterparts in mono/SSA stages. -/
private def collectTraceEntries
    (coreModules : List CModule)
    (monoModules : List CModule)
    (ssaModules : List SModule)
    (locMap : FnLocMap := [])
    (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : List TraceEntry :=
  -- Build extraction entries from ProofCore
  let extractionEntries := extractionEntriesFromPC pc registry
  -- Build proof status entries
  let proofEntries := coreModules.foldl (fun acc m =>
    acc ++ collectProofStatus pc locMap m "" registry) []
  -- Collect mono names
  let allMonoNames := monoModules.foldl (fun acc m => acc ++ collectMonoFnNames m) []
  -- Collect SSA names
  let allSSAFns := ssaModules.foldl (fun acc sm =>
    acc ++ sm.functions.map fun f => (f.name, f.isEntryPoint)) []
  -- Build entries
  extractionEntries.map fun ext =>
    let bareName := match ext.qualName.splitOn "." with
      | parts => parts.getLast!
    -- Find proof status
    let proofEntry := proofEntries.find? fun e => e.qualName == ext.qualName
    let evidence := match proofEntry with
      | some e => match e.state with
        | .proved => "proved"
        | .stale => "stale"
        | .notProved => "enforced"
        | .blocked => "blocked"
        | .notEligible => "reported"
        | .trusted => "trusted-assumption"
      | none => "missing"
    let extStatus := if ext.eligible then
      (if ext.extracted.isSome then "extracted" else "eligible_not_extractable")
    else "excluded"
    let pcForm := match ext.extracted with
      | some pexpr => renderPExpr pexpr
      | none => ""
    -- Mono names: find specializations that start with the bare name
    let specPrefix := bareName ++ "_for_"
    let matchesMono (mn : String) : Bool :=
      if mn.endsWith ("." ++ bareName) then true
      else
        let parts := mn.splitOn specPrefix
        parts.length != 1
    let monoMatches := allMonoNames.filter matchesMono
    -- SSA names: find matching functions
    let ssaMatches := allSSAFns.filter fun (sn, _) =>
      sn == bareName || sn.startsWith (bareName ++ "_for_")
    let ssaNames := ssaMatches.map (·.1)
    let llvmNames := ssaMatches.map fun (sn, isEntry) => llvmSymbol sn isEntry
    let boundary := claimBoundaryFor evidence ext.extracted.isSome
    { sourceFunction := ext.qualName
    , fingerprint := ext.fingerprint
    , extractionStatus := extStatus
    , proofCoreForm := pcForm
    , evidenceLevel := evidence
    , coreNames := [ext.qualName]
    , monoNames := monoMatches
    , ssaNames := ssaNames
    , llvmNames := llvmNames
    , specName := ext.specName
    , proofName := ext.proofName
    , claimBoundary := boundary
    , loc := ext.loc }

/-- Render the traceability report. -/
def traceabilityReport
    (coreModules : List CModule)
    (monoModules : List CModule)
    (ssaModules : List SModule)
    (locMap : FnLocMap := [])
    (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let entries := collectTraceEntries coreModules monoModules ssaModules locMap registry pc
  let header := "=== Source/Core/SSA/LLVM Traceability ==="
  let body := entries.map fun e =>
    let locStr := fmtLoc e.loc
    let monoStr := if e.monoNames.isEmpty then "(not monomorphized)"
      else ", ".intercalate e.monoNames
    let ssaStr := if e.ssaNames.isEmpty then "(not lowered)"
      else ", ".intercalate e.ssaNames
    let llvmStr := if e.llvmNames.isEmpty then "(not emitted)"
      else ", ".intercalate e.llvmNames
    let pcStr := if e.proofCoreForm.isEmpty then ""
      else s!"\n    proof_core:   {e.proofCoreForm}"
    let specStr := if e.specName.isEmpty then "" else s!"\n    spec:         {e.specName}"
    let proofStr := if e.proofName.isEmpty then "" else s!"\n    proof:        {e.proofName}"
    s!"  {e.sourceFunction}\n    evidence:     {e.evidenceLevel}\n    extraction:   {e.extractionStatus}{pcStr}{specStr}{proofStr}\n    core:         {", ".intercalate e.coreNames}\n    mono:         {monoStr}\n    ssa:          {ssaStr}\n    llvm:         {llvmStr}\n    boundary:     {e.claimBoundary}\n    fingerprint:  {e.fingerprint}\n    loc:          {locStr}"
  let evidenceCounts := entries.foldl (fun acc e =>
    match e.evidenceLevel with
    | "proved" => { acc with fst := acc.fst + 1 }
    | "enforced" => { acc with snd := { acc.snd with fst := acc.snd.fst + 1 } }
    | "reported" => { acc with snd := { acc.snd with snd := { acc.snd.snd with fst := acc.snd.snd.fst + 1 } } }
    | _ => { acc with snd := { acc.snd with snd := { acc.snd.snd with snd := acc.snd.snd.snd + 1 } } }
    ) (0, (0, (0, 0)))
  let (proved, (enforced, (reported, other))) := evidenceCounts
  let summary := s!"Totals: {entries.length} functions — {proved} proved, {enforced} enforced, {reported} reported, {other} other"
  s!"{header}\n\n{"\n\n".intercalate body}\n\n{summary}\n"

-- ============================================================
-- Machine-readable facts (--report diagnostics-json)
-- ============================================================
-- Structured diagnostic records for predictable violations and
-- proof-status entries. JSON output, no external dependencies.

namespace Json

/-- Escape a string for JSON output. -/
private def escapeStr (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
    | '"' => "\\\""
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | '\t' => "\\t"
    | c => c.toString) ""

/-- A minimal JSON value. -/
inductive Val where
  | str : String → Val
  | num : Int → Val
  | bool : Bool → Val
  | null : Val
  | arr : List Val → Val
  | obj : List (String × Val) → Val

/-- Render a JSON value. -/
partial def Val.render : Val → String
  | .str s => s!"\"{escapeStr s}\""
  | .num n => toString n
  | .bool b => if b then "true" else "false"
  | .null => "null"
  | .arr vs => s!"[{", ".intercalate (vs.map Val.render)}]"
  | .obj kvs =>
    let fields := kvs.map fun (k, v) => s!"\"{escapeStr k}\": {v.render}"
    s!"\{{", ".intercalate fields}}"

end Json

-- ============================================================
-- Minimal JSON parser (reads back our own diagnostics output)
-- ============================================================

namespace JsonParser

/-- Work on an Array of characters with Nat indices to avoid String.Pos issues. -/
private def skipWS (cs : Array Char) (pos : Nat) : Nat :=
  if h : pos < cs.size then
    let c := cs[pos]
    if c == ' ' || c == '\n' || c == '\r' || c == '\t' then skipWS cs (pos + 1)
    else pos
  else pos

private partial def parseString (cs : Array Char) (pos : Nat) : Option (String × Nat) :=
  if pos >= cs.size || cs[pos]! != '"' then none
  else
    let rec go (i : Nat) (acc : String) : Option (String × Nat) :=
      if i >= cs.size then none
      else
        let c := cs[i]!
        if c == '"' then some (acc, i + 1)
        else if c == '\\' then
          if i + 1 >= cs.size then none
          else
            let esc := cs[i + 1]!
            let ch := match esc with
              | '"' => '"'
              | '\\' => '\\'
              | 'n' => '\n'
              | 't' => '\t'
              | '/' => '/'
              | _ => esc
            go (i + 2) (acc.push ch)
        else go (i + 1) (acc.push c)
    go (pos + 1) ""

private partial def parseNumber (cs : Array Char) (pos : Nat) : Option (Int × Nat) :=
  let neg := pos < cs.size && cs[pos]! == '-'
  let start := if neg then pos + 1 else pos
  let rec go (i : Nat) (acc : Nat) : (Nat × Nat) :=
    if i >= cs.size then (acc, i)
    else
      let c := cs[i]!
      if c.isDigit then go (i + 1) (acc * 10 + (c.toNat - '0'.toNat))
      else (acc, i)
  let (n, endPos) := go start 0
  if endPos == start then none
  else some (if neg then -↑n else ↑n, endPos)

private partial def matchWord (cs : Array Char) (pos : Nat) (word : String) : Bool :=
  let wcs := word.toList
  let rec go (i : Nat) (ws : List Char) : Bool :=
    match ws with
    | [] => true
    | w :: rest =>
      if pos + i >= cs.size then false
      else if cs[pos + i]! == w then go (i + 1) rest
      else false
  go 0 wcs

partial def parseValue (cs : Array Char) (pos : Nat) : Option (Json.Val × Nat) :=
  let p := skipWS cs pos
  if p >= cs.size then none
  else
    let c := cs[p]!
    if c == '"' then
      match parseString cs p with
      | some (str, next) => some (.str str, next)
      | none => none
    else if c == '[' then
      parseArray cs (p + 1)
    else if c == '{' then
      parseObject cs (p + 1)
    else if c == 't' && matchWord cs p "true" then
      some (.bool true, p + 4)
    else if c == 'f' && matchWord cs p "false" then
      some (.bool false, p + 5)
    else if c == 'n' && matchWord cs p "null" then
      some (.null, p + 4)
    else if c == '-' || c.isDigit then
      match parseNumber cs p with
      | some (n, next) => some (.num n, next)
      | none => none
    else none

where
  parseArray (cs : Array Char) (pos : Nat) : Option (Json.Val × Nat) :=
    let p := skipWS cs pos
    if p < cs.size && cs[p]! == ']' then some (.arr [], p + 1)
    else
      let rec go (i : Nat) (acc : List Json.Val) : Option (Json.Val × Nat) :=
        match parseValue cs i with
        | none => none
        | some (v, next) =>
          let next := skipWS cs next
          if next >= cs.size then none
          else if cs[next]! == ']' then some (.arr (acc ++ [v]), next + 1)
          else if cs[next]! == ',' then go (next + 1) (acc ++ [v])
          else none
      go p []

  parseObject (cs : Array Char) (pos : Nat) : Option (Json.Val × Nat) :=
    let p := skipWS cs pos
    if p < cs.size && cs[p]! == '}' then some (.obj [], p + 1)
    else
      let rec go (i : Nat) (acc : List (String × Json.Val)) : Option (Json.Val × Nat) :=
        let i := skipWS cs i
        match parseString cs i with
        | none => none
        | some (key, next) =>
          let next := skipWS cs next
          if next >= cs.size || cs[next]! != ':' then none
          else
            match parseValue cs (next + 1) with
            | none => none
            | some (v, next) =>
              let next := skipWS cs next
              if next >= cs.size then none
              else if cs[next]! == '}' then some (.obj (acc ++ [(key, v)]), next + 1)
              else if cs[next]! == ',' then go (next + 1) (acc ++ [(key, v)])
              else none
      go p []

def parse (s : String) : Option Json.Val :=
  let cs := s.toList.toArray
  match parseValue cs 0 with
  | some (v, _) => some v
  | none => none

end JsonParser

/-- Schema version for the machine-readable JSON API.
    Bump this when adding required fields or removing fields.
    Adding optional fields is backwards-compatible and does not bump the version. -/
def schemaVersion : Nat := 1

/-- Known fact kinds that the compiler produces. -/
def knownFactKinds : List String :=
  ["proof_diagnostic", "predictable_violation", "proof_status", "eligibility",
   "obligation", "extraction", "traceability", "effects", "capability", "unsafe", "alloc"]

/-- Known semantic query prefixes. -/
def knownQueryKinds : List String :=
  ["predictable", "proof", "evidence", "audit", "fn", "why-capability", "traceability"]

open Json in
/-- Wrap a list of facts in a versioned envelope. -/
def factsEnvelope (facts : List Val) : Val :=
  .obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("schema_kind", .str "facts"),
    ("fact_kinds", .arr (knownFactKinds.map .str)),
    ("fact_count", .num (Int.ofNat facts.length)),
    ("facts", .arr facts)
  ]

open Json in
/-- Convert a SourceLoc to a JSON object. -/
private def locToJson : Option SourceLoc → Val
  | some (file, line) => .obj [("file", .str file), ("line", .num line)]
  | none => .null

open Json in
/-- Convert a proof diagnostic to a JSON fact. -/
private def proofDiagnosticToFact (d : Concrete.ProofDiagnostic) : Val :=
  .obj ([
    ("kind", .str "proof_diagnostic"),
    ("code", .str d.kind.code),
    ("diagnostic_kind", .str (diagnosticKindLabel d.kind)),
    ("severity", .str (diagnosticSeverityLabel d.severity)),
    ("failure_class", .str d.failureClass),
    ("repair_class", .str d.repairClass),
    ("function", .str d.function),
    ("message", .str d.message),
    ("loc", locToJson d.loc)
  ] ++ (if d.hint.isEmpty then [] else [("hint", .str d.hint)])
    ++ (if d.details.isEmpty then [] else [("details", .arr (d.details.map .str))])
    ++ (if d.fingerprint.isEmpty then [] else [("fingerprint", .str d.fingerprint)])
    ++ (if d.expectedFp.isEmpty then [] else [("expected_fingerprint", .str d.expectedFp)]))

open Json in
/-- Collect proof diagnostic facts from ProofCore. -/
def collectProofDiagnosticFacts (pc : Concrete.ProofCore) : List Val :=
  pc.diagnostics.map proofDiagnosticToFact

open Json in
/-- Convert a ProfileViolation to a JSON fact. -/
private def violationToFact (v : ProfileViolation) : Val :=
  .obj [
    ("kind", .str "predictable_violation"),
    ("function", .str v.qualName),
    ("state", .str "failed"),
    ("reason", .str v.reason),
    ("hint", .str v.hint),
    ("loc", locToJson v.loc),
    ("violation_loc", locToJson v.violationLoc)
  ]

open Json in
/-- Convert a ProofStatusEntry to a JSON fact. -/
private def proofStatusToFact (e : ProofStatusEntry) : Val :=
  let stateStr := e.state.canonical
  let hintStr := match e.state with
    | .stale => "Update the Lean proof in Concrete/Proof.lean, or restore the proved implementation."
    | .notProved => "Add a Lean proof for this function in Concrete/Proof.lean with the current fingerprint."
    | .blocked =>
      let unsupStr := if e.unsupported.isEmpty then "unsupported constructs"
          else ", ".intercalate e.unsupported
      s!"Remove {unsupStr} to enable proof extraction."
    | .notEligible => "Address these constraints to make this function eligible for proof."
    | _ => ""
  .obj ([
    ("kind", .str "proof_status"),
    ("function", .str e.qualName),
    ("state", .str stateStr),
    ("loc", locToJson e.loc),
    ("current_fingerprint", .str e.currentFp)
  ] ++ (if e.expectedFp.isEmpty then [] else [("expected_fingerprint", .str e.expectedFp)])
    ++ (if e.specName.isEmpty then [] else [("spec", .str e.specName)])
    ++ (if e.proofName.isEmpty then [] else [("proof", .str e.proofName)])
    ++ (if e.proofSource == "none" then [] else [("source", .str e.proofSource)])
    ++ (if e.profileGates.isEmpty then [] else [("profile_gates", .arr (e.profileGates.map .str))])
    ++ (if e.unsupported.isEmpty then [] else [("unsupported", .arr (e.unsupported.map .str))])
    ++ (if hintStr.isEmpty then [] else [("hint", .str hintStr)]))

open Json in
/-- Collect predictable violations as structured facts. -/
def collectPredictableFacts (modules : List CModule) (locMap : FnLocMap := [])
    (pc : Concrete.ProofCore) : List Val :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  let violations := modules.foldl (fun acc m =>
    acc ++ checkPredictableModule recMap externNames locMap m) []
  violations.map violationToFact

open Json in
/-- Convert an eligibility entry to a JSON fact. -/
private def eligibilityToFact (e : EligibilityEntry) : Val :=
  let statusStr := if e.isTrusted then "trusted"
    else if e.eligible then "eligible"
    else "excluded"
  let exclusionStr := match e.exclusionKind with
    | some .source => "source"
    | some .profile => "profile"
    | some .both => "both"
    | none => "none"
  .obj ([
    ("kind", .str "eligibility"),
    ("function", .str e.qualName),
    ("status", .str statusStr),
    ("exclusion_kind", .str exclusionStr),
    ("source_reasons", .arr (e.sourceReasons.map .str)),
    ("profile_reasons", .arr (e.profileReasons.map .str)),
    ("loc", locToJson e.loc)
  ])

open Json in
/-- Collect eligibility facts for all functions. -/
def collectEligibilityFacts (pc : Concrete.ProofCore) : List Val :=
  let entries := pc.allEligibility
  entries.map eligibilityToFact

open Json in
/-- Collect proof-status entries as structured facts. -/
def collectProofStatusFacts (modules : List CModule) (locMap : FnLocMap := [])
    (registry : ProofRegistry := []) (pc : Concrete.ProofCore) : List Val :=
  let entries := modules.foldl (fun acc m =>
    acc ++ collectProofStatus pc locMap m "" registry) []
  entries.map proofStatusToFact

open Json in
/-- Convert an obligation entry to a JSON fact. -/
private def obligationToFact (e : ObligationEntry) : Val :=
  .obj ([
    ("kind", .str "obligation"),
    ("function", .str e.function),
    ("status", .str e.status),
    ("spec", .str e.spec),
    ("proof", .str e.proof),
    ("source", .str e.source),
    ("fingerprint", .str e.fingerprint),
    ("dependencies", .arr (e.dependencies.map .str)),
    ("stale_deps", .arr (e.staleDeps.map .str)),
    ("loc", locToJson e.loc)
  ])

open Json in
/-- Collect obligation facts for all functions. -/
def collectObligationFacts (_modules : List CModule) (_locMap : FnLocMap := [])
    (_registry : ProofRegistry := []) (pc : Concrete.ProofCore) : List Val :=
  let entries := pc.obligations.map obligationToEntry
  entries.map obligationToFact

open Json in
/-- Convert an extraction entry to a JSON fact. -/
private def extractionToFact (e : ExtractionEntry) : Val :=
  let statusStr := if e.eligible then
    match e.extracted with
    | some _ => "extracted"
    | none => "eligible_not_extractable"
  else "excluded"
  let proofCoreStr := match e.extracted with
    | some pexpr => renderPExpr pexpr
    | none => ""
  .obj ([
    ("kind", .str "extraction"),
    ("function", .str e.qualName),
    ("status", .str statusStr),
    ("eligible", .bool e.eligible),
    ("fingerprint", .str e.fingerprint),
    ("params", .arr (e.params.map .str)),
    ("loc", locToJson e.loc)
  ] ++ (if proofCoreStr.isEmpty then [] else [("proof_core", .str proofCoreStr)])
    ++ (if e.specName.isEmpty then [] else [("spec", .str e.specName)])
    ++ (if e.proofName.isEmpty then [] else [("proof", .str e.proofName)])
    ++ (if e.excluded.isEmpty then [] else [("excluded_reasons", .arr (e.excluded.map .str))])
    ++ (if e.unsupported.isEmpty then [] else [("unsupported", .arr (e.unsupported.map .str))]))

open Json in
/-- Collect extraction facts for all functions. -/
def collectExtractionFacts (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : List Val :=
  let entries := extractionEntriesFromPC pc registry
  entries.map extractionToFact

open Json in
/-- Convert a traceability entry to a JSON fact. -/
private def traceToFact (e : TraceEntry) : Val :=
  .obj ([
    ("kind", .str "traceability"),
    ("function", .str e.sourceFunction),
    ("evidence", .str e.evidenceLevel),
    ("extraction", .str e.extractionStatus),
    ("core", .arr (e.coreNames.map .str)),
    ("mono", .arr (e.monoNames.map .str)),
    ("ssa", .arr (e.ssaNames.map .str)),
    ("llvm", .arr (e.llvmNames.map .str)),
    ("boundary", .str e.claimBoundary),
    ("fingerprint", .str e.fingerprint),
    ("spec", .str e.specName),
    ("proof", .str e.proofName),
    ("loc", locToJson e.loc)
  ] ++ (if e.proofCoreForm.isEmpty then [] else [("proof_core", .str e.proofCoreForm)]))

open Json in
/-- Collect traceability facts for all functions. -/
def collectTraceabilityFacts
    (coreModules : List CModule)
    (monoModules : List CModule)
    (ssaModules : List SModule)
    (locMap : FnLocMap := [])
    (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : List Val :=
  let entries := collectTraceEntries coreModules monoModules ssaModules locMap registry pc
  entries.map traceToFact

open Json in
/-- Query traceability facts, optionally filtered by function name. -/
def queryTraceability
    (coreModules : List CModule)
    (monoModules : List CModule)
    (ssaModules : List SModule)
    (locMap : FnLocMap := [])
    (fnFilter : Option String := none)
    (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let allFacts := collectTraceabilityFacts coreModules monoModules ssaModules locMap registry pc
  let getStr (v : Val) (key : String) : Option String :=
    match v with
    | .obj kvs =>
      match kvs.find? (fun (k, _) => k == key) with
      | some (_, .str s) => some s
      | _ => none
    | _ => none
  let filtered := match fnFilter with
    | none => allFacts
    | some fnName => allFacts.filter fun v =>
      match getStr v "function" with
      | some f => f == fnName || f.endsWith ("." ++ fnName)
      | none => false
  (factsEnvelope filtered).render

open Json in
/-- Convert an FnEffects record to a JSON fact. -/
private def effectsToFact (e : FnEffects) : Val :=
  let (concreteCaps, _) := e.capSet.normalize
  .obj [
    ("kind", .str "effects"),
    ("function", .str e.qualName),
    ("capabilities", .arr (concreteCaps.map .str)),
    ("is_pure", .bool (e.capSet == .empty)),
    ("allocates", .bool e.allocates),
    ("frees", .bool e.frees),
    ("defers", .bool e.defers),
    ("recursion", .str e.recursion),
    ("loops", .str e.loops),
    ("crosses_ffi", .bool e.crossesFfi),
    ("is_trusted", .bool e.isTrusted),
    ("is_public", .bool e.isPublic),
    ("evidence", .str e.evidence),
    ("loc", locToJson e.loc)
  ]

open Json in
/-- Collect effects facts for all functions. -/
def collectEffectsFacts (modules : List CModule) (locMap : FnLocMap := [])
    (pc : Concrete.ProofCore) : List Val :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  let allEffects := modules.foldl (fun acc m =>
    acc ++ effectsForModule externNames recMap locMap pc m) []
  allEffects.map effectsToFact

open Json in
/-- Convert a per-function capability entry with why-traces to a JSON fact. -/
private def capToFact (lookup : CapLookup) (qualName : String) (f : CFnDef) : Val :=
  let (concreteCaps, _) := f.capSet.normalize
  let callees := collectCallsStmts f.body |>.eraseDups
  let traces := concreteCaps.map fun cap =>
    let contributors := callees.filter fun callee =>
      match lookupCalleeCap lookup callee with
      | some cs => let (cc, _) := cs.normalize; cc.contains cap
      | none => false
    .obj [
      ("capability", .str cap),
      ("source", .str (if contributors.isEmpty then "declared"
        else ", ".intercalate contributors))
    ]
  .obj [
    ("kind", .str "capability"),
    ("function", .str qualName),
    ("capabilities", .arr (concreteCaps.map .str)),
    ("is_pure", .bool concreteCaps.isEmpty),
    ("is_public", .bool f.isPublic),
    ("why", .arr traces)
  ]

open Json in
/-- Collect capability facts with why-traces for all functions. -/
private partial def collectCapFactsModule (lookup : CapLookup) (m : CModule) (modulePath : String := "") : List Val :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let fnFacts := m.functions.map fun f => capToFact lookup (qualPrefix ++ "." ++ f.name) f
  let externFacts := m.externFns.map fun (n, _, _, trusted) =>
    .obj [
      ("kind", .str "capability"),
      ("function", .str (qualPrefix ++ "." ++ n)),
      ("capabilities", .arr (if trusted then [] else [Val.str unsafeCapName])),
      ("is_pure", .bool trusted),
      ("is_extern", .bool true),
      ("is_trusted", .bool trusted),
      ("why", .arr [])
    ]
  fnFacts ++ externFacts ++ m.submodules.foldl (fun acc sub =>
    acc ++ collectCapFactsModule lookup sub qualPrefix) []

open Json in
def collectCapFacts (modules : List CModule) : List Val :=
  let lookup := buildCapLookup modules
  modules.foldl (fun acc m => acc ++ collectCapFactsModule lookup m) []

open Json in
/-- Convert a function's unsafe/trust boundary info to a JSON fact. -/
private partial def collectUnsafeFactsModule (externNames : List String) (m : CModule) (modulePath : String := "") : List Val :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let fnFacts := m.functions.filterMap fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    let hasUnsafe := hasUnsafeCap f.capSet
    let hasRawPtrs := fnUsesRawPtrs f
    let trusted := f.isTrusted
    if !hasUnsafe && !hasRawPtrs && !trusted then none
    else
      let boundary := if trusted then trustBoundaryAnalysis externNames f else []
      some (.obj [
        ("kind", .str "unsafe"),
        ("function", .str qualName),
        ("has_unsafe_cap", .bool hasUnsafe),
        ("has_raw_pointers", .bool hasRawPtrs),
        ("is_trusted", .bool trusted),
        ("trust_boundary", .arr (boundary.map .str))
      ])
  let externFacts := m.externFns.map fun (n, _, _, trusted) =>
    .obj [
      ("kind", .str "unsafe"),
      ("function", .str (qualPrefix ++ "." ++ n)),
      ("is_extern", .bool true),
      ("is_trusted", .bool trusted)
    ]
  fnFacts ++ externFacts ++ m.submodules.foldl (fun acc sub =>
    acc ++ collectUnsafeFactsModule externNames sub qualPrefix) []

open Json in
def collectUnsafeFacts (modules : List CModule) : List Val :=
  let externNames := modules.foldl (fun acc m => acc ++ collectExternNames m) []
  modules.foldl (fun acc m => acc ++ collectUnsafeFactsModule externNames m) []

open Json in
/-- Convert a function's allocation info to a JSON fact. -/
private def allocToFact (qualName : String) (f : CFnDef) : Option Val :=
  let callees := collectCallsStmts f.body |>.eraseDups
  let allocs := callees.filter isAllocCall
  let frees := callees.filter isFreeCall
  let defers := collectDefersStmts f.body
  if allocs.isEmpty && frees.isEmpty && defers.isEmpty then none
  else
    let returnsAlloc := returnsAllocation f.retTy
    let leaks := !allocs.isEmpty && frees.isEmpty && defers.isEmpty && !returnsAlloc
    some (.obj [
      ("kind", .str "alloc"),
      ("function", .str qualName),
      ("allocates", .arr (allocs.map .str)),
      ("frees", .arr (frees.map .str)),
      ("defers", .arr (defers.map .str)),
      ("returns_allocation", .bool returnsAlloc),
      ("potential_leak", .bool leaks)
    ])

open Json in
private partial def collectAllocFactsModule (m : CModule) (modulePath : String := "") : List Val :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let fnFacts := m.functions.filterMap fun f => allocToFact (qualPrefix ++ "." ++ f.name) f
  fnFacts ++ m.submodules.foldl (fun acc sub =>
    acc ++ collectAllocFactsModule sub qualPrefix) []

open Json in
def collectAllocFacts (modules : List CModule) : List Val :=
  modules.foldl (fun acc m => acc ++ collectAllocFactsModule m) []

open Json in
/-- Collect all core facts (everything except traceability) into a flat list. -/
def collectCoreFacts (modules : List CModule) (locMap : FnLocMap := [])
    (registry : ProofRegistry := []) (pc : Concrete.ProofCore) : List Val :=
  let eligibility := collectEligibilityFacts pc
  let predictable := collectPredictableFacts modules locMap pc
  let proofStatus := collectProofStatusFacts modules locMap registry pc
  let obligations := collectObligationFacts modules locMap registry pc
  let extraction := collectExtractionFacts (registry := registry) (pc := pc)
  let effects := collectEffectsFacts modules locMap pc
  let caps := collectCapFacts modules
  let unsafeFacts := collectUnsafeFacts modules
  let alloc := collectAllocFacts modules
  let proofDiags := collectProofDiagnosticFacts pc
  let base := eligibility ++ predictable ++ proofStatus ++ obligations ++ extraction
  base ++ proofDiags ++ effects ++ caps ++ unsafeFacts ++ alloc

open Json in
/-- Produce JSON diagnostics combining all fact types in a versioned envelope. -/
def diagnosticsJson (modules : List CModule) (locMap : FnLocMap := [])
    (registry : ProofRegistry := []) (pc : Concrete.ProofCore) : String :=
  (factsEnvelope (collectCoreFacts modules locMap registry pc)).render

open Json in
/-- Build proof-specific summary from proof-related facts. -/
private def buildProofSummary (pc : Concrete.ProofCore) : Val :=
  let obCount (s : Concrete.ObligationStatus) := (pc.obligations.filter fun o => o.status == s).length
  let diagCount (k : Concrete.ProofDiagnosticKind) := (pc.diagnostics.filter fun d => d.kind == k).length
  let extractedCount := (pc.entries.filter fun e => e.extracted.isSome).length
  let blockedCount := (pc.entries.filter fun e => e.extracted.isNone && !e.unsupported.isEmpty).length
  let excludedCount := pc.excluded.length
  let depsCount := (pc.obligations.filter fun o => !o.dependencies.isEmpty).length
  let staleDepsCount := (pc.obligations.filter fun o => !o.staleDeps.isEmpty).length
  .obj [
    ("total_functions", .num (pc.obligations.length)),
    ("proved", .num (obCount .proved)),
    ("stale", .num (obCount .stale)),
    ("missing", .num (obCount .missing)),
    ("blocked", .num (obCount .blocked)),
    ("ineligible", .num (obCount .ineligible)),
    ("trusted", .num (obCount .trusted)),
    ("extracted", .num extractedCount),
    ("extraction_blocked", .num blockedCount),
    ("excluded", .num excludedCount),
    ("diagnostics_errors", .num ((pc.diagnostics.filter fun d => d.severity == .error).length)),
    ("diagnostics_warnings", .num ((pc.diagnostics.filter fun d => d.severity == .warning).length)),
    ("diagnostics_info", .num ((pc.diagnostics.filter fun d => d.severity == .info).length)),
    ("stale_proof_count", .num (diagCount .staleProof)),
    ("missing_proof_count", .num (diagCount .missingProof)),
    ("attachment_integrity_count", .num (diagCount .attachmentIntegrity)),
    ("functions_with_dependencies", .num depsCount),
    ("functions_with_stale_deps", .num staleDepsCount)
  ]

open Json in
/-- Convert a registry entry to a JSON value. -/
private def registryEntryToJson (e : Concrete.ProofRegistryEntry) : Val :=
  .obj [
    ("function", .str e.function),
    ("body_fingerprint", .str e.bodyFingerprint),
    ("proof", .str e.proof),
    ("spec", .str e.spec)
  ]

open Json in
/-- Convert a dependency edge to a JSON value. -/
private def depEdgeToJson (o : Concrete.Obligation) : Val :=
  .obj [
    ("function", .str o.functionId.qualName),
    ("status", .str o.status.canonical),
    ("proved_deps", .arr (o.dependencies.map .str)),
    ("stale_deps", .arr (o.staleDeps.map .str))
  ]

open Json in
/-- Produce a proof-workflow evidence bundle combining all proof-related data. -/
def proofBundleJson
    (sourcePath : String)
    (timestamp : String)
    (compilerIdent : String)
    (modules : List CModule)
    (locMap : FnLocMap := [])
    (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  -- Proof-related facts
  let proofStatusFacts := collectProofStatusFacts modules locMap registry pc
  let obligationFacts := collectObligationFacts modules locMap registry pc
  let extractionFacts := collectExtractionFacts (registry := registry) (pc := pc)
  let diagnosticFacts := collectProofDiagnosticFacts pc
  let eligibilityFacts := collectEligibilityFacts pc
  let allFacts := proofStatusFacts ++ obligationFacts ++ extractionFacts ++ diagnosticFacts ++ eligibilityFacts
  -- Registry entries
  let registryJson := registry.map registryEntryToJson
  -- Dependency edges (only functions that have deps or stale deps)
  let depEdges := (pc.obligations.filter fun o =>
    !o.dependencies.isEmpty || !o.staleDeps.isEmpty).map depEdgeToJson
  -- Assumptions
  let assumptions := Val.obj [
    ("proof_model", .str "PExpr with Lean unbounded Int, pure functional semantics"),
    ("compilation_chain", .str "Core IR → SSA → LLVM IR → binary: unverified"),
    ("integer_model", .str "PExpr uses unbounded Int; binary uses 64-bit fixed-width"),
    ("composition", .str "per-function proofs; cross-function composition not verified"),
    ("checker_soundness", .str "Concrete checker (Check.lean) correctness is assumed, not proved"),
    ("fingerprint_stability", .str "stable within compiler version; may change across versions")
  ]
  let bundle := Val.obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("schema_kind", .str "proof_bundle"),
    ("source", .str sourcePath),
    ("timestamp", .str timestamp),
    ("compiler", .str compilerIdent),
    ("summary", buildProofSummary pc),
    ("assumptions", assumptions),
    ("registry", .arr registryJson),
    ("dependency_graph", .arr depEdges),
    ("fact_count", .num allFacts.length),
    ("facts", .arr allFacts)
  ]
  bundle.render

open Json in
/-- Extract a string field from a JSON object. -/
def jsonGetStr (v : Val) (key : String) : Option String :=
  match v with
  | .obj kvs =>
    match kvs.find? (fun (k, _) => k == key) with
    | some (_, .str s) => some s
    | _ => none
  | _ => none

-- ============================================================
-- Semantic query: why-capability trace
-- ============================================================

/-- Build a flat lookup of bare function names to CFnDef across module tree. -/
private partial def buildFnLookupModule (m : CModule) : List (String × CFnDef) :=
  let fns := m.functions.map fun f => (f.name, f)
  fns ++ m.submodules.foldl (fun acc sub => acc ++ buildFnLookupModule sub) []

private def buildFnLookup (modules : List CModule) : List (String × CFnDef) :=
  modules.foldl (fun acc m => acc ++ buildFnLookupModule m) []

/-- Build a flat extern name → trusted lookup. -/
private partial def buildExternLookupModule (m : CModule) : List (String × Bool) :=
  let exts := m.externFns.map fun (n, _, _, t) => (n, t)
  exts ++ m.submodules.foldl (fun acc sub => acc ++ buildExternLookupModule sub) []

private def buildExternLookup (modules : List CModule) : List (String × Bool) :=
  modules.foldl (fun acc m => acc ++ buildExternLookupModule m) []

open Json in
/-- Trace why a function requires a specific capability.
    Returns a list of trace steps from the queried function down to the origin.
    Stops at: declared (with clause), extern, intrinsic, or depth limit.
    visited prevents cycles. -/
private partial def traceCapability
    (fnLookup : List (String × CFnDef))
    (externLookup : List (String × Bool))
    (capLookup : CapLookup)
    (locMap : FnLocMap)
    (fnName : String) (cap : String)
    (visited : List String := []) (depth : Nat := 0) : List Val :=
  if depth > 20 then [.obj [("function", .str fnName), ("error", .str "depth limit")]]
  else if visited.contains fnName then [.obj [("function", .str fnName), ("error", .str "cycle")]]
  else
    -- Is it an intrinsic?
    match resolveIntrinsic fnName with
    | some iid =>
      match iid.capability with
      | some icap =>
        if icap == cap then [.obj [("function", .str fnName), ("origin", .str "intrinsic")]]
        else []
      | none => []
    | none =>
    -- Is it an extern?
    match externLookup.find? (fun (n, _) => n == fnName) with
    | some (_, trusted) =>
      if !trusted then
        -- Untrusted externs have Unsafe capability
        if cap == unsafeCapName then
          [.obj [("function", .str fnName), ("origin", .str "extern")]]
        else []
      else []  -- trusted externs have no capabilities
    | none =>
    -- Is it a user function?
    match fnLookup.find? (fun (n, _) => n == fnName) with
    | none => []
    | some (_, f) =>
      let (concreteCaps, _) := f.capSet.normalize
      -- Check if this function even has the cap
      if !concreteCaps.contains cap then []
      else
        let callees := collectCallsStmts f.body |>.eraseDups
        let visited' := fnName :: visited
        -- Find callees that contribute this cap
        let contributors := callees.filter fun callee =>
          match lookupCalleeCap capLookup callee with
          | some cs => let (cc, _) := cs.normalize; cc.contains cap
          | none => false
        if contributors.isEmpty then
          -- No callee contributes it → declared via with(...)
          let loc := locMap.find? (fun e => e.qualName.endsWith ("." ++ fnName) || e.qualName == fnName)
          let locVal := match loc with
            | some e => locToJson (some (e.file, e.fnSpan.line))
            | none => Val.null
          [.obj [("function", .str fnName), ("origin", .str "declared"), ("loc", locVal)]]
        else
          -- Trace through each contributor
          contributors.foldl (fun acc callee =>
            let subTrace := traceCapability fnLookup externLookup capLookup locMap
              callee cap visited' (depth + 1)
            if subTrace.isEmpty then acc
            else
              let step := Val.obj [("function", .str fnName), ("edge", .str "calls"), ("callee", .str callee)]
              acc ++ [step] ++ subTrace
          ) []

open Json in
/-- Handle a why-capability query. Returns answer-shaped JSON. -/
def whyCapabilityQuery (modules : List CModule) (locMap : FnLocMap)
    (fnName : String) (cap : String) : String :=
  let fnLookup := buildFnLookup modules
  let externLookup := buildExternLookup modules
  let capLookup := buildCapLookup modules
  let trace := traceCapability fnLookup externLookup capLookup locMap fnName cap
  let answer :=
    if trace.isEmpty then "not_required"
    else
      -- Check if first trace step is a declaration (no transitive path)
      match trace with
      | [.obj kvs] =>
        match kvs.find? (fun (k, _) => k == "origin") with
        | some (_, .str "declared") => "declared"
        | some (_, .str "intrinsic") => "intrinsic"
        | some (_, .str "extern") => "extern"
        | _ => "transitive"
      | _ => "transitive"
  let result := Val.obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("kind", .str "query_answer"),
    ("query", .str s!"why-capability:{fnName}:{cap}"),
    ("function", .str fnName),
    ("capability", .str cap),
    ("answer", .str answer),
    ("trace", .arr trace)
  ]
  result.render

open Json in
/-- Handle a predictable query for a single function. Returns answer-shaped JSON. -/
def predictableQuery (modules : List CModule) (locMap : FnLocMap)
    (fnName : String) (pc : Concrete.ProofCore) : String :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  let violations := modules.foldl (fun acc m =>
    acc ++ checkPredictableModule recMap externNames locMap m) []
  let fnViolations := violations.filter fun v =>
    v.fnName == fnName
  let answer := if fnViolations.isEmpty then "pass" else "fail"
  let gates := fnViolations.map fun v =>
    .obj ([
      ("gate", .str v.reason),
      ("hint", .str v.hint)
    ] ++ match v.loc with
      | some l => [("loc", locToJson (some l))]
      | none => []
    ++ match v.violationLoc with
      | some l => [("violation_loc", locToJson (some l))]
      | none => [])
  let result := Val.obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("kind", .str "query_answer"),
    ("query", .str s!"predictable:{fnName}"),
    ("function", .str fnName),
    ("answer", .str answer),
    ("gates_failed", .num (Int.ofNat fnViolations.length)),
    ("violations", .arr gates)
  ]
  result.render

open Json in
/-- Handle a proof query for a single function. Returns answer-shaped JSON. -/
def proofQuery (modules : List CModule) (locMap : FnLocMap)
    (fnName : String) (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let entries := modules.foldl (fun acc m =>
    acc ++ collectProofStatus pc locMap m "" registry) []
  let fnEntry := entries.find? fun e =>
    e.bareName == fnName || e.qualName == fnName || e.qualName.endsWith ("." ++ fnName)
  match fnEntry with
  | none =>
    (Val.obj [
      ("schema_version", .num (Int.ofNat schemaVersion)),
      ("kind", .str "query_answer"),
      ("query", .str s!"proof:{fnName}"),
      ("function", .str fnName),
      ("answer", .str "not_found")
    ]).render
  | some e =>
    let stateStr := e.state.canonical
    let hintStr := match e.state with
      | .stale => "Update the Lean proof in Concrete/Proof.lean, or restore the proved implementation."
      | .notProved => "Add a Lean proof for this function in Concrete/Proof.lean with the current fingerprint."
      | .blocked => "Remove unsupported constructs to enable proof extraction."
      | .notEligible => s!"Remove {", ".intercalate e.profileGates} to make this function eligible for proof."
      | _ => ""
    (Val.obj ([
      ("schema_version", .num (Int.ofNat schemaVersion)),
      ("kind", .str "query_answer"),
      ("query", .str s!"proof:{fnName}"),
      ("function", .str e.qualName),
      ("answer", .str stateStr),
      ("current_fingerprint", .str e.currentFp)
    ] ++ (if e.expectedFp.isEmpty then [] else [("expected_fingerprint", .str e.expectedFp)])
      ++ (if e.profileGates.isEmpty then [] else [("profile_gates", .arr (e.profileGates.map .str))])
      ++ (if hintStr.isEmpty then [] else [("hint", .str hintStr)])
      ++ [("loc", locToJson e.loc)])).render

open Json in
/-- Handle an evidence query for a single function. Returns answer-shaped JSON
    combining predictable profile, proof status, and trust into one answer. -/
def evidenceQuery (modules : List CModule) (locMap : FnLocMap)
    (fnName : String) (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  -- Get effects for evidence level
  let allEffects := modules.foldl (fun acc m =>
    acc ++ effectsForModule externNames recMap locMap pc m) []
  let matchFn (e : FnEffects) := e.name == fnName || e.qualName == fnName || e.qualName.endsWith ("." ++ fnName)
  let fnEffects := allEffects.find? matchFn
  -- Get violations
  let violations := modules.foldl (fun acc m =>
    acc ++ checkPredictableModule recMap externNames locMap m) []
  let fnViolations := violations.filter fun v => v.fnName == fnName || v.qualName == fnName || v.qualName.endsWith ("." ++ fnName)
  -- Get proof status
  let entries := modules.foldl (fun acc m =>
    acc ++ collectProofStatus pc locMap m "" registry) []
  let fnProof := entries.find? fun e =>
    e.bareName == fnName || e.qualName == fnName || e.qualName.endsWith ("." ++ fnName)
  match fnEffects with
  | none =>
    (Val.obj [
      ("schema_version", .num (Int.ofNat schemaVersion)),
      ("kind", .str "query_answer"),
      ("query", .str s!"evidence:{fnName}"),
      ("function", .str fnName),
      ("answer", .str "not_found")
    ]).render
  | some eff =>
    let proofState := match fnProof with
      | some e => e.state.canonical
      | none => "missing"
    let gatesFailed := fnViolations.map fun v => Val.str v.reason
    (Val.obj [
      ("schema_version", .num (Int.ofNat schemaVersion)),
      ("kind", .str "query_answer"),
      ("query", .str s!"evidence:{fnName}"),
      ("function", .str eff.qualName),
      ("answer", .str eff.evidence),
      ("is_trusted", .bool eff.isTrusted),
      ("passes_predictable", .bool fnViolations.isEmpty),
      ("proof_state", .str proofState),
      ("gates_failed", .arr gatesFailed),
      ("loc", locToJson eff.loc)
    ]).render

open Json in
/-- Handle an audit query for a single function. Bundles authority, predictable
    profile, proof status, evidence, trust, and allocation into one answer. -/
def auditQuery (modules : List CModule) (locMap : FnLocMap)
    (fnName : String) (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let recMap := pc.recMap
  let externNames := pc.externNames
  let capLookup := buildCapLookup modules
  let fnLookup := buildFnLookup modules
  let externLookup := buildExternLookup modules
  -- Effects
  let allEffects := modules.foldl (fun acc m =>
    acc ++ effectsForModule externNames recMap locMap pc m) []
  let fnEffects := allEffects.find? fun e => e.name == fnName || e.qualName == fnName || e.qualName.endsWith ("." ++ fnName)
  match fnEffects with
  | none =>
    (Val.obj [
      ("schema_version", .num (Int.ofNat schemaVersion)),
      ("kind", .str "query_answer"),
      ("query", .str s!"audit:{fnName}"),
      ("function", .str fnName),
      ("answer", .str "not_found")
    ]).render
  | some eff =>
    -- Capabilities with why traces
    let (concreteCaps, _) := eff.capSet.normalize
    let capTraces := concreteCaps.map fun cap =>
      let trace := traceCapability fnLookup externLookup capLookup locMap fnName cap
      let origin :=
        if trace.isEmpty then "not_required"
        else match trace with
          | [.obj kvs] =>
            match kvs.find? (fun (k, _) => k == "origin") with
            | some (_, .str o) => o
            | _ => "transitive"
          | _ => "transitive"
      .obj [("capability", .str cap), ("origin", .str origin), ("trace", .arr trace)]
    -- Predictable
    let violations := modules.foldl (fun acc m =>
      acc ++ checkPredictableModule recMap externNames locMap m) []
    let fnViolations := violations.filter fun v => v.fnName == fnName || v.qualName == fnName || v.qualName.endsWith ("." ++ fnName)
    let violationFacts := fnViolations.map fun v =>
      .obj ([("gate", .str v.reason), ("hint", .str v.hint)]
        ++ match v.violationLoc with
          | some l => [("violation_loc", locToJson (some l))]
          | none => [])
    -- Proof
    let entries := modules.foldl (fun acc m =>
      acc ++ collectProofStatus pc locMap m "" registry) []
    let fnProof := entries.find? fun e =>
      e.bareName == fnName || e.qualName.endsWith ("." ++ fnName)
    let proofState := match fnProof with
      | some e => e.state.canonical
      | none => "missing"
    let fingerprint := match fnProof with
      | some e => e.currentFp | none => "<missing>"
    -- Allocation
    let fnDef := fnLookup.find? (fun (n, _) => n == fnName)
    let allocInfo := match fnDef with
      | some (_, f) =>
        let callees := collectCallsStmts f.body |>.eraseDups
        let allocs := callees.filter isAllocCall
        let frees := callees.filter isFreeCall
        let defers := collectDefersStmts f.body
        .obj [
          ("allocates", .arr (allocs.map .str)),
          ("frees", .arr (frees.map .str)),
          ("defers", .arr (defers.map .str)),
          ("returns_allocation", .bool (returnsAllocation f.retTy))
        ]
      | none => .obj [("allocates", .arr []), ("frees", .arr []), ("defers", .arr []),
                       ("returns_allocation", .bool false)]
    (Val.obj [
      ("schema_version", .num (Int.ofNat schemaVersion)),
      ("kind", .str "query_answer"),
      ("query", .str s!"audit:{fnName}"),
      ("function", .str eff.qualName),
      ("loc", locToJson eff.loc),
      ("evidence", .str eff.evidence),
      ("is_public", .bool eff.isPublic),
      ("is_trusted", .bool eff.isTrusted),
      ("authority", .obj [
        ("capabilities", .arr (concreteCaps.map .str)),
        ("is_pure", .bool concreteCaps.isEmpty),
        ("traces", .arr capTraces)
      ]),
      ("predictable", .obj [
        ("passes", .bool fnViolations.isEmpty),
        ("violations", .arr violationFacts)
      ]),
      ("proof", .obj ([
        ("state", .str proofState),
        ("fingerprint", .str fingerprint)
      ] ++ match fnProof with
        | some e => if e.profileGates.isEmpty then []
          else [("profile_gates", .arr (e.profileGates.map .str))]
        | none => [])),
      ("allocation", allocInfo)
    ]).render

open Json in
/-- Query compiler facts by kind and optional function name.
    Query formats:
    - "KIND"                  — filter all facts by kind
    - "KIND:FUNCTION"         — filter by kind + function
    - "fn:FUNCTION"           — all facts for one function
    - "why-capability:FN:CAP" — trace why a function requires a capability
    - "predictable:FN"        — predictable profile answer for one function
    - "proof:FN"              — proof status answer for one function
    - "evidence:FN"           — combined evidence answer for one function
    Returns Except.error for malformed/unknown queries, Except.ok for valid results. -/
def queryFacts (modules : List CModule) (locMap : FnLocMap := [])
    (query : String) (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : Except String String :=
  let parts := query.splitOn ":"
  let allKindsStr := s!"{", ".intercalate knownQueryKinds} (semantic), {", ".intercalate knownFactKinds} (fact filter)"
  -- Reject empty query
  if query.isEmpty then
    .error s!"empty query string. Use --query KIND, --query KIND:FUNCTION, or --query predictable:FUNCTION. Known kinds: {allKindsStr}"
  else
  -- Reject queries with empty segments (leading/trailing/double colons)
  if parts.any (·.isEmpty) then
    .error s!"malformed query '{query}': contains empty segment. Use --query KIND:FUNCTION, not ':FUNCTION' or 'KIND:'"
  else
  -- Semantic queries: three-part (why-capability:fn:cap)
  if parts.length == 3 then
    match parts with
    | ["why-capability", fnName, cap] => .ok (whyCapabilityQuery modules locMap fnName cap)
    | [kind, _, _] =>
      .error s!"unknown three-part query kind '{kind}'. Only 'why-capability:FN:CAP' uses three parts"
    | _ => .error s!"malformed three-part query '{query}'"
  else
  -- Semantic queries: two-part (predictable:fn, proof:fn, evidence:fn)
  if parts.length == 2 then
    match parts with
    | ["predictable", fnName] => .ok (predictableQuery modules locMap fnName pc)
    | ["proof", fnName] => .ok (proofQuery modules locMap fnName registry pc)
    | ["evidence", fnName] => .ok (evidenceQuery modules locMap fnName registry pc)
    | ["audit", fnName] => .ok (auditQuery modules locMap fnName registry pc)
    | ["fn", fnName] =>
      let allFacts := collectCoreFacts modules locMap registry pc
      let filtered := allFacts.filter fun v =>
        match jsonGetStr v "function" with
        | some f => f == fnName || f.endsWith ("." ++ fnName)
        | none => false
      .ok (factsEnvelope filtered).render
    | [filterKind, filterFn] =>
      -- kind:function filter — validate the kind is known
      if knownFactKinds.contains filterKind then
        let allFacts := collectCoreFacts modules locMap registry pc
        let byKind := allFacts.filter fun v =>
          jsonGetStr v "kind" == some filterKind
        let filtered := byKind.filter fun v =>
          match jsonGetStr v "function" with
          | some f => f == filterFn || f.endsWith ("." ++ filterFn)
          | none => false
        .ok (factsEnvelope filtered).render
      else
        .error s!"unknown query kind '{filterKind}'. Known kinds: {allKindsStr}"
    | _ => .error s!"malformed two-part query '{query}'"
  else if parts.length == 1 then
    -- Single-word filter: all facts of this kind — validate kind is known
    if knownFactKinds.contains query then
      let allFacts := collectCoreFacts modules locMap registry pc
      let filtered := allFacts.filter fun v =>
        jsonGetStr v "kind" == some query
      .ok (factsEnvelope filtered).render
    else
      .error s!"unknown query kind '{query}'. Known kinds: {allKindsStr}"
  else
    .error s!"malformed query '{query}': too many ':' separators (max 2, for why-capability:FN:CAP)"

-- ============================================================
-- Semantic diff / trust drift
-- ============================================================
-- Compare two fact bundles (from diagnostics-json) and report
-- changes in capabilities, allocation, evidence, proof state,
-- spec/proof attachment, obligation status, etc.

open Json in
/-- Extract a string from a Val, returning "" for non-strings. -/
private def valStr : Val → String
  | .str s => s
  | .num n => toString n
  | .bool b => if b then "true" else "false"
  | .null => "null"
  | .arr vs => s!"[{", ".intercalate (vs.map valStr)}]"
  | .obj _ => "{...}"

open Json in
/-- Get a field value from a JSON object as a Val. -/
private def jsonGetVal (v : Val) (key : String) : Option Val :=
  match v with
  | .obj kvs => (kvs.find? fun (k, _) => k == key).map (·.2)
  | _ => none

open Json in
/-- Render a Val to a short display string. -/
private def valDisplay : Val → String
  | .str s => s
  | .num n => toString n
  | .bool b => if b then "true" else "false"
  | .null => "null"
  | .arr vs => s!"[{", ".intercalate (vs.map valDisplay)}]"
  | .obj _ => "{…}"

/-- A single field change in a diff entry. -/
structure FieldChange where
  field : String
  oldVal : String
  newVal : String

/-- A single diff entry for one (kind, function) pair. -/
structure DiffEntry where
  kind : String
  function : String
  category : String       -- "added" | "removed" | "changed"
  changes : List FieldChange
  drift : String          -- "weakened" | "strengthened" | "neutral"

/-- Fields to compare for trust-relevant changes per fact kind. -/
private def trustFields (kind : String) : List String :=
  match kind with
  | "proof_status" => ["state", "spec", "proof", "source", "current_fingerprint"]
  | "obligation" => ["status", "spec", "proof", "source", "fingerprint"]
  | "extraction" => ["status", "eligible", "spec", "proof", "proof_core", "fingerprint"]
  | "effects" => ["capabilities", "is_pure", "allocates", "frees", "recursion",
                   "loops", "crosses_ffi", "is_trusted", "evidence"]
  | "capability" => ["capabilities", "is_pure"]
  | "unsafe" => ["has_unsafe_cap", "has_raw_pointers", "is_trusted"]
  | "alloc" => ["allocates", "frees", "defers", "potential_leak"]
  | "predictable_violation" => ["state", "reason"]
  | "traceability" => ["evidence", "extraction", "boundary", "spec", "proof", "fingerprint"]
  | _ => []

/-- Evidence level ordering for drift detection (higher = stronger). -/
private def evidenceRank (s : String) : Nat :=
  match s with
  | "proved" => 5
  | "stale" => 4
  | "enforced" => 3
  | "trusted-assumption" => 2
  | "reported" => 1
  | _ => 0

/-- Proof state ordering (higher = stronger). -/
private def proofStateRank (s : String) : Nat :=
  match s with
  | "proved" => 4
  | "stale" => 3
  | "missing" => 2
  | "ineligible" => 1
  | "trusted" => 2
  | "blocked" => 1
  | _ => 0

/-- Determine if a field change represents trust weakening. -/
private def isWeakening (kind : String) (field : String) (oldV newV : String) : Bool :=
  match kind, field with
  | "proof_status", "state" => proofStateRank newV < proofStateRank oldV
  | "obligation", "status" => proofStateRank newV < proofStateRank oldV
  | "effects", "evidence" => evidenceRank newV < evidenceRank oldV
  | "effects", "is_pure" => oldV == "true" && newV == "false"
  | "effects", "is_trusted" => oldV == "false" && newV == "true"
  | "effects", "crosses_ffi" => oldV == "false" && newV == "true"
  | "capability", "is_pure" => oldV == "true" && newV == "false"
  | "alloc", "potential_leak" => oldV == "false" && newV == "true"
  | "traceability", "evidence" => evidenceRank newV < evidenceRank oldV
  | "extraction", "status" =>
    let oldRank := match oldV with | "extracted" => 3 | "eligible_not_extractable" => 2 | "excluded" => 1 | _ => 0
    let newRank := match newV with | "extracted" => 3 | "eligible_not_extractable" => 2 | "excluded" => 1 | _ => 0
    newRank < oldRank
  | _, _ => false

/-- Determine if a field change represents trust strengthening. -/
private def isStrengthening (kind : String) (field : String) (oldV newV : String) : Bool :=
  isWeakening kind field newV oldV

open Json in
/-- Compare two facts and return field changes. -/
private def compareFacts (kind : String) (oldFact newFact : Val) : List FieldChange :=
  let fields := trustFields kind
  fields.filterMap fun f =>
    let oldV := (jsonGetVal oldFact f).map valDisplay |>.getD "<missing>"
    let newV := (jsonGetVal newFact f).map valDisplay |>.getD "<missing>"
    if oldV == newV then none
    else some { field := f, oldVal := oldV, newVal := newV }

open Json in
/-- Build a keyed map: (kind, function) → Val for a list of facts.
    For fact kinds that allow multiple entries per function (e.g.,
    predictable_violation), the key includes a disambiguator. -/
private def keyFacts (facts : List Val) : List ((String × String) × Val) :=
  facts.filterMap fun v =>
    match jsonGetStr v "kind", jsonGetStr v "function" with
    | some k, some f =>
      -- Disambiguate multi-per-function fact kinds
      let suffix := match k with
        | "predictable_violation" => (jsonGetStr v "reason").getD ""
        | "proof_diagnostic" => (jsonGetStr v "diagnostic_kind").getD ""
        | _ => ""
      let key := if suffix.isEmpty then (k, f) else (k, f ++ ":" ++ suffix)
      some (key, v)
    | _, _ => none

open Json in
/-- Classify a newly-added fact as weakened or neutral based on its content.
    New functions with weak evidence, non-pure capabilities, FFI, or trust
    markers are real drift and should be flagged. -/
private def classifyNewFact (kind : String) (v : Val) : String :=
  match kind with
  | "predictable_violation" => "weakened"
  | "unsafe" => "weakened"
  | "effects" =>
    let ev := (jsonGetVal v "evidence").map valDisplay
    let pure := (jsonGetVal v "is_pure").map valDisplay
    let ffi := (jsonGetVal v "crosses_ffi").map valDisplay
    let trusted := (jsonGetVal v "is_trusted").map valDisplay
    let caps := (jsonGetVal v "capabilities").map valDisplay
    -- Missing fields in a new fact are suspicious — treat as weakened
    if ev.isNone || pure.isNone || ffi.isNone || trusted.isNone then "weakened"
    else if ev == some "reported" || ev == some "trusted-assumption" then "weakened"
    else if pure == some "false" then "weakened"
    else if ffi == some "true" then "weakened"
    else if trusted == some "true" then "weakened"
    else if caps != some "[]" && caps.isSome then "weakened"
    else "neutral"
  | "capability" =>
    let pure := (jsonGetVal v "is_pure").map valDisplay
    if pure.isNone then "weakened"  -- missing field is suspicious
    else if pure == some "false" then "weakened" else "neutral"
  | "alloc" =>
    let leak := (jsonGetVal v "potential_leak").map valDisplay
    if leak.isNone then "weakened"
    else if leak == some "true" then "weakened" else "neutral"
  | "proof_status" =>
    let state := (jsonGetVal v "state").map valDisplay
    if state.isNone then "weakened"
    else if state == some "missing" || state == some "ineligible" || state == some "stale" then "weakened"
    else "neutral"
  | "obligation" =>
    let status := (jsonGetVal v "status").map valDisplay
    if status.isNone then "weakened"
    else if status == some "missing" || status == some "stale" then "weakened"
    else "neutral"
  | "extraction" =>
    let status := (jsonGetVal v "status").map valDisplay
    if status.isNone then "weakened"
    else if status == some "excluded" then "weakened" else "neutral"
  | "traceability" =>
    let ev := (jsonGetVal v "evidence").map valDisplay
    if ev.isNone then "weakened"
    else if evidenceRank (ev.getD "") < 3 then "weakened" else "neutral"  -- below "enforced"
  | "eligibility" | "proof_diagnostic" => "neutral"  -- informational fact kinds
  | _ => "weakened"  -- unknown fact kind in new facts is suspicious

open Json in
/-- Find duplicate (kind, function) keys in a keyed fact list. -/
private def findDuplicateKeys (keyed : List ((String × String) × Val))
    : List (String × String) :=
  let keys := keyed.map (·.1)
  keys.foldl (fun (seen, dupes) key =>
    if seen.contains key then
      if dupes.contains key then (seen, dupes)
      else (seen, dupes ++ [key])
    else (seen ++ [key], dupes)
  ) ([], []) |>.2

open Json in
/-- Diff two fact bundles and produce a list of DiffEntries.
    Returns an error if either bundle contains duplicate (kind, function) keys. -/
def diffFacts (oldFacts newFacts : List Val) : Except String (List DiffEntry) :=
  let oldKeyed := keyFacts oldFacts
  let newKeyed := keyFacts newFacts
  -- Reject duplicate keys
  let oldDupes := findDuplicateKeys oldKeyed
  let newDupes := findDuplicateKeys newKeyed
  if !oldDupes.isEmpty then
    let desc := oldDupes.map fun (k, f) => s!"({k}, {f})"
    .error s!"duplicate keys in old bundle: {", ".intercalate desc}"
  else if !newDupes.isEmpty then
    let desc := newDupes.map fun (k, f) => s!"({k}, {f})"
    .error s!"duplicate keys in new bundle: {", ".intercalate desc}"
  else
  -- Removed: in old but not new
  let removed := oldKeyed.filterMap fun ((k, f), _) =>
    if newKeyed.find? (fun (key, _) => key == (k, f)) |>.isNone then
      some { kind := k, function := f, category := "removed"
           , changes := [], drift := "weakened" : DiffEntry }
    else none
  -- Added: in new but not old
  let added := newKeyed.filterMap fun ((k, f), v) =>
    if oldKeyed.find? (fun (key, _) => key == (k, f)) |>.isNone then
      let drift := classifyNewFact k v
      some { kind := k, function := f, category := "added"
           , changes := [], drift := drift : DiffEntry }
    else none
  -- Changed: in both, fields differ
  let changed := oldKeyed.filterMap fun ((k, f), oldV) =>
    match newKeyed.find? (fun (key, _) => key == (k, f)) with
    | none => none
    | some (_, newV) =>
      let fieldChanges := compareFacts k oldV newV
      if fieldChanges.isEmpty then none
      else
        let hasWeakening := fieldChanges.any fun fc => isWeakening k fc.field fc.oldVal fc.newVal
        let hasStrengthening := fieldChanges.any fun fc => isStrengthening k fc.field fc.oldVal fc.newVal
        let drift := if hasWeakening then "weakened"
          else if hasStrengthening then "strengthened"
          else "neutral"
        some { kind := k, function := f, category := "changed"
             , changes := fieldChanges, drift := drift : DiffEntry }
  .ok (removed ++ added ++ changed)

/-- Render a diff report as human-readable text. -/
def renderDiffReport (entries : List DiffEntry) : String :=
  if entries.isEmpty then "No trust-relevant changes detected.\n"
  else
    let header := "=== Semantic Diff / Trust Drift ==="
    -- Group by drift direction
    let weakened := entries.filter (·.drift == "weakened")
    let strengthened := entries.filter (·.drift == "strengthened")
    let neutral := entries.filter (·.drift == "neutral")
    let renderEntry (e : DiffEntry) : String :=
      let tag := match e.category with
        | "added" => "[+]"
        | "removed" => "[-]"
        | _ => "[~]"
      let changesStr := if e.changes.isEmpty then ""
        else "\n" ++ (e.changes.map fun fc =>
          s!"      {fc.field}: {fc.oldVal} → {fc.newVal}").foldl (· ++ "\n" ++ ·) ""
      s!"    {tag} {e.kind} / {e.function}{changesStr}"
    let sections := []
    let sections := if weakened.isEmpty then sections
      else sections ++ [s!"  TRUST WEAKENED ({weakened.length}):\n{"\n".intercalate (weakened.map renderEntry)}"]
    let sections := if strengthened.isEmpty then sections
      else sections ++ [s!"  TRUST STRENGTHENED ({strengthened.length}):\n{"\n".intercalate (strengthened.map renderEntry)}"]
    let sections := if neutral.isEmpty then sections
      else sections ++ [s!"  OTHER CHANGES ({neutral.length}):\n{"\n".intercalate (neutral.map renderEntry)}"]
    let summary := s!"Summary: {entries.length} changes — {weakened.length} weakened, {strengthened.length} strengthened, {neutral.length} neutral"
    s!"{header}\n\n{"\n\n".intercalate sections}\n\n{summary}\n"

open Json in
/-- Render a diff as JSON for machine consumption. -/
def renderDiffJson (entries : List DiffEntry) : String :=
  let vals := entries.map fun e =>
    Val.obj [
      ("kind", .str e.kind),
      ("function", .str e.function),
      ("category", .str e.category),
      ("drift", .str e.drift),
      ("changes", .arr (e.changes.map fun fc =>
        Val.obj [("field", .str fc.field), ("old", .str fc.oldVal), ("new", .str fc.newVal)]))
    ]
  (Val.arr vals).render

open Json in
/-- Parse a JSON string into a list of fact Vals.
    Accepts either a raw JSON array or a snapshot object with a "facts" field.
    Returns (facts, warnings) where warnings flag schema issues. -/
def parseFactsWarn (jsonStr : String) : Option (List Val) × List String :=
  match JsonParser.parse jsonStr with
  | some (.arr vs) =>
    let warnings := validateFactSchema vs
    (some vs, warnings)
  | some (.obj kvs) =>
    match kvs.find? (fun (k, _) => k == "facts") with
    | some (_, .arr vs) =>
      let warnings := validateFactSchema vs
      (some vs, warnings)
    | _ => (none, ["error: snapshot JSON object has no \"facts\" array field"])
  | some _ => (none, ["error: snapshot JSON is not an array or object"])
  | none => (none, ["error: snapshot JSON failed to parse"])
where
  /-- Validate fact entries have required fields. -/
  validateFactSchema (facts : List Val) : List String :=
    let issues := facts.foldl (fun (acc : List String × Nat) v =>
      let (ws, idx) := acc
      let kindOk := (jsonGetStr v "kind").isSome
      let funcOk := (jsonGetStr v "function").isSome
      let ws := if !kindOk then
        ws ++ [s!"warning: fact at index {idx} missing required \"kind\" field"]
      else ws
      let ws := if !funcOk then
        ws ++ [s!"warning: fact at index {idx} missing required \"function\" field"]
      else ws
      (ws, idx + 1)
    ) ([], 0)
    issues.1

open Json in
/-- Backward-compatible wrapper: parse facts without warnings. -/
def parseFacts (jsonStr : String) : Option (List Val) :=
  (parseFactsWarn jsonStr).1

-- ============================================================
-- Fact artifact snapshot
-- ============================================================

open Json in
/-- Build a summary object from a list of facts. -/
private def buildSummaryFact (facts : List Val) : Val :=
  let count (kind : String) := facts.filter (fun v => jsonGetStr v "kind" == some kind) |>.length
  let proofFacts := facts.filter fun v => jsonGetStr v "kind" == some "proof_status"
  let proofState (s : String) := proofFacts.filter (fun v => jsonGetStr v "state" == some s) |>.length
  let obFacts := facts.filter fun v => jsonGetStr v "kind" == some "obligation"
  let obStatus (s : String) := obFacts.filter (fun v => jsonGetStr v "status" == some s) |>.length
  let extFacts := facts.filter fun v => jsonGetStr v "kind" == some "extraction"
  let extStatus (s : String) := extFacts.filter (fun v => jsonGetStr v "status" == some s) |>.length
  let eligFacts := facts.filter fun v => jsonGetStr v "kind" == some "eligibility"
  let eligStatus (s : String) := eligFacts.filter (fun v => jsonGetStr v "status" == some s) |>.length
  .obj [
    ("total_functions", .num (proofFacts.length)),
    ("proved", .num (proofState "proved")),
    ("stale", .num (proofState "stale")),
    ("missing", .num (proofState "missing")),
    ("ineligible", .num (proofState "ineligible")),
    ("trusted", .num (proofState "trusted")),
    ("eligibility_eligible", .num (eligStatus "eligible")),
    ("eligibility_excluded", .num (eligStatus "excluded")),
    ("eligibility_trusted", .num (eligStatus "trusted")),
    ("predictable_violations", .num (count "predictable_violation")),
    ("obligations_proved", .num (obStatus "proved")),
    ("obligations_missing", .num (obStatus "missing")),
    ("obligations_blocked", .num (obStatus "blocked")),
    ("obligations_stale", .num (obStatus "stale")),
    ("extracted", .num (extStatus "extracted")),
    ("excluded", .num (extStatus "excluded")),
    ("effects_facts", .num (count "effects")),
    ("capability_facts", .num (count "capability")),
    ("unsafe_facts", .num (count "unsafe")),
    ("alloc_facts", .num (count "alloc")),
    ("traceability_facts", .num (count "traceability"))
  ]

open Json in
/-- Build the full snapshot JSON object with metadata, facts, and summary. -/
def snapshotJson
    (sourcePath : String)
    (timestamp : String)
    (coreFacts : List Val)
    (traceFacts : List Val := []) : String :=
  let allFacts := coreFacts ++ traceFacts
  let summary := buildSummaryFact allFacts
  let snapshot := Val.obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("version", .num 1),
    ("source", .str sourcePath),
    ("timestamp", .str timestamp),
    ("fact_count", .num allFacts.length),
    ("summary", summary),
    ("facts", .arr allFacts)
  ]
  snapshot.render

/-- Single-command audit composing the surfaces a reviewer needs to answer
    "what does this program claim, and where do the trust boundaries sit?".
    The audit is the per-program rendering of the proof-story matrix
    (`docs/PROOF_STORY_MATRIX.md`): authority, trust, allocation, proof
    evidence with coverage, obligations.  Each section is the existing
    aspect-specific report under a labeled banner; later versions can
    add machine-readable JSON output and a ProvableV1 conformance check. -/
def auditReport (modules : List CModule) (locMap : FnLocMap := [])
    (sourceMap : SourceMap := []) (registry : ProofRegistry := [])
    (pc : Concrete.ProofCore) : String :=
  let banner := String.intercalate "\n"
    [ "=== Concrete Audit Report ==="
    , ""
    , "Governing frame: every construct is one of"
    , "  proved / enforced / reported / assumed / trusted."
    , "See docs/PROVABLE_V1.md and docs/PROOF_STORY_MATRIX.md for context."
    ]
  let sectionHeader (name : String) : String :=
    s!"\n\n--- {name} ---\n"
  String.intercalate "" [
    banner,
    sectionHeader "Authority",
    capabilityReport modules,
    sectionHeader "Allocation",
    allocReport modules,
    sectionHeader "Unsafe / Trust",
    unsafeReport modules pc,
    sectionHeader "Effects",
    effectsReport modules locMap pc,
    sectionHeader "Eligibility",
    eligibilityReport pc,
    sectionHeader "Proof Status",
    proofStatusReport modules locMap sourceMap (registry := registry) (pc := pc),
    sectionHeader "ProvableV1 Conformance",
    provableV1ConformanceReport modules locMap (registry := registry) (pc := pc),
    sectionHeader "Obligations",
    obligationsReport modules locMap registry pc
  ]

open Json in
/-- Produce the JSON API schema definition. Documents all fact kinds, their fields,
    query response shapes, location encoding, and compatibility rules. -/
def schemaReport : String :=
  let loc := Val.obj [
    ("type", .str "object|null"),
    ("fields", .obj [("file", .str "string"), ("line", .str "number")]),
    ("note", .str "null when source location is unavailable")
  ]
  let fieldSpec (required optional : List (String × String)) : Val :=
    .obj [
      ("required", .arr (required.map fun (k, v) => .obj [("name", .str k), ("type", .str v)])),
      ("optional", .arr (optional.map fun (k, v) => .obj [("name", .str k), ("type", .str v)]))
    ]
  let factSchemas := Val.obj [
    ("proof_diagnostic", fieldSpec
      [("kind", "string"), ("code", "string"), ("diagnostic_kind", "string"), ("severity", "string"),
       ("function", "string"), ("message", "string"), ("loc", "location"),
       ("failure_class", "string"), ("repair_class", "string")]
      [("hint", "string"), ("details", "string[]"), ("fingerprint", "string"),
       ("expected_fingerprint", "string")]),
    ("predictable_violation", fieldSpec
      [("kind", "string"), ("function", "string"), ("state", "string"),
       ("reason", "string"), ("hint", "string"), ("loc", "location"),
       ("violation_loc", "location")]
      []),
    ("proof_status", fieldSpec
      [("kind", "string"), ("function", "string"), ("state", "string"),
       ("loc", "location"), ("current_fingerprint", "string")]
      [("expected_fingerprint", "string"), ("spec", "string"), ("proof", "string"),
       ("source", "string"), ("profile_gates", "string[]"), ("hint", "string")]),
    ("eligibility", fieldSpec
      [("kind", "string"), ("function", "string"), ("status", "string"),
       ("exclusion_kind", "string"), ("source_reasons", "string[]"),
       ("profile_reasons", "string[]"), ("loc", "location")]
      []),
    ("obligation", fieldSpec
      [("kind", "string"), ("function", "string"), ("status", "string"),
       ("spec", "string"), ("proof", "string"), ("source", "string"),
       ("fingerprint", "string"), ("dependencies", "string[]"),
       ("stale_deps", "string[]"), ("loc", "location")]
      []),
    ("extraction", fieldSpec
      [("kind", "string"), ("function", "string"), ("status", "string"),
       ("eligible", "boolean"), ("fingerprint", "string"), ("params", "string[]"),
       ("loc", "location")]
      [("proof_core", "string"), ("spec", "string"), ("proof", "string"),
       ("excluded_reasons", "string[]"), ("unsupported", "string[]")]),
    ("traceability", fieldSpec
      [("kind", "string"), ("function", "string"), ("evidence", "string"),
       ("extraction", "string"), ("core", "string[]"), ("mono", "string[]"),
       ("ssa", "string[]"), ("llvm", "string[]"), ("boundary", "string"),
       ("fingerprint", "string"), ("spec", "string"), ("proof", "string"),
       ("loc", "location")]
      [("proof_core", "string")]),
    ("effects", fieldSpec
      [("kind", "string"), ("function", "string"), ("capabilities", "string[]"),
       ("is_pure", "boolean"), ("allocates", "boolean"), ("frees", "boolean"),
       ("defers", "boolean"), ("recursion", "string"), ("loops", "string"),
       ("crosses_ffi", "boolean"), ("is_trusted", "boolean"), ("is_public", "boolean"),
       ("evidence", "string"), ("loc", "location")]
      []),
    ("capability", fieldSpec
      [("kind", "string"), ("function", "string"), ("capabilities", "string[]"),
       ("is_pure", "boolean"), ("why", "object[]")]
      [("is_public", "boolean"), ("is_extern", "boolean"), ("is_trusted", "boolean")]),
    ("unsafe", fieldSpec
      [("kind", "string"), ("function", "string")]
      [("has_unsafe_cap", "boolean"), ("has_raw_pointers", "boolean"),
       ("is_trusted", "boolean"), ("trust_boundary", "string[]"),
       ("is_extern", "boolean")]),
    ("alloc", fieldSpec
      [("kind", "string"), ("function", "string"), ("allocates", "string[]"),
       ("frees", "string[]"), ("defers", "string[]"), ("returns_allocation", "boolean"),
       ("potential_leak", "boolean")]
      [])
  ]
  let querySchemas := Val.obj [
    ("why-capability", .obj [
      ("format", .str "why-capability:FUNCTION:CAPABILITY"),
      ("response", .str "query_answer with answer, trace")]),
    ("predictable", .obj [
      ("format", .str "predictable:FUNCTION"),
      ("response", .str "query_answer with answer, gates_failed, violations")]),
    ("proof", .obj [
      ("format", .str "proof:FUNCTION"),
      ("response", .str "query_answer with answer, current_fingerprint")]),
    ("evidence", .obj [
      ("format", .str "evidence:FUNCTION"),
      ("response", .str "query_answer with answer, is_trusted, passes_predictable, proof_state")]),
    ("audit", .obj [
      ("format", .str "audit:FUNCTION"),
      ("response", .str "query_answer with authority, predictable, proof, allocation")]),
    ("fn", .obj [
      ("format", .str "fn:FUNCTION"),
      ("response", .str "facts envelope — all facts for one function")]),
    ("traceability", .obj [
      ("format", .str "traceability[:FUNCTION]"),
      ("response", .str "facts envelope — traceability facts, optionally filtered")])
  ]
  let schema := Val.obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("fact_kinds", .arr (knownFactKinds.map .str)),
    ("query_kinds", .arr (knownQueryKinds.map .str)),
    ("location_encoding", loc),
    ("fact_schemas", factSchemas),
    ("query_schemas", querySchemas),
    ("envelopes", .obj [
      ("facts", .obj [
        ("description", .str "Wraps fact arrays from diagnostics-json and fact-filter queries"),
        ("fields", .obj [
          ("schema_version", .str "number"),
          ("schema_kind", .str "\"facts\""),
          ("fact_kinds", .str "string[] — canonical list of known fact kinds"),
          ("fact_count", .str "number"),
          ("facts", .str "object[] — the fact objects")])]),
      ("query_answer", .obj [
        ("description", .str "Wraps semantic query responses"),
        ("fields", .obj [
          ("schema_version", .str "number"),
          ("kind", .str "\"query_answer\""),
          ("query", .str "string — the original query string"),
          ("function", .str "string — qualified function name"),
          ("answer", .str "string — query-specific answer value")])]),
      ("snapshot", .obj [
        ("description", .str "Full snapshot with metadata, summary, and facts"),
        ("fields", .obj [
          ("schema_version", .str "number"),
          ("version", .str "number — snapshot format version"),
          ("source", .str "string"),
          ("timestamp", .str "string"),
          ("fact_count", .str "number"),
          ("summary", .str "object"),
          ("facts", .str "object[]")])])
    ]),
    ("policies", .obj [
      ("empty_result", .str "A query that matches zero facts returns a facts envelope with an empty facts array and fact_count 0. Semantic queries for unknown functions return a query_answer with answer \"not_found\". Neither case is an error."),
      ("error_result", .str "Malformed or unknown queries return an error string on stderr and exit code 1. Errors are never encoded in JSON — they use plain text on stderr."),
      ("function_names", .str "All function fields use qualified names (e.g. \"main.parse_byte\"). Queries accept either bare or qualified names."),
      ("compatibility", .str "Adding new optional fields or new fact kinds is backwards-compatible and does not bump schema_version. Removing fields, changing field types, renaming fields, or changing required/optional status bumps schema_version. Consumers should ignore unknown fields and unknown fact kinds for forward-compatibility.")
    ])
  ]
  schema.render

open Json in
/-- Produce a machine-readable listing of all stable diagnostic error codes. -/
def diagnosticCodesReport : String :=
  let entry (code pass severity description : String) : Val :=
    .obj [("code", .str code), ("pass", .str pass), ("severity", .str severity), ("description", .str description)]
  let codes : List Val := [
    -- Parse (E0001)
    entry "E0001" "parse" "error" "syntax error",
    -- Resolve (E0100–E0111)
    entry "E0100" "resolve" "error" "undeclared variable",
    entry "E0101" "resolve" "error" "unknown function",
    entry "E0102" "resolve" "error" "unknown struct type",
    entry "E0103" "resolve" "error" "unknown enum variant",
    entry "E0104" "resolve" "error" "not an enum",
    entry "E0105" "resolve" "error" "unknown enum",
    entry "E0106" "resolve" "error" "unknown static method",
    entry "E0107" "resolve" "error" "unknown function reference",
    entry "E0108" "resolve" "error" "unknown type",
    entry "E0109" "resolve" "error" "Self outside impl block",
    entry "E0110" "resolve" "error" "unknown module",
    entry "E0111" "resolve" "error" "symbol not public in module",
    -- Check: name/variable/linearity (E0200–E0219)
    entry "E0200" "check" "error" "Self outside impl block",
    entry "E0201" "check" "error" "undeclared variable",
    entry "E0202" "check" "error" "assignment to undeclared variable",
    entry "E0203" "check" "error" "variable frozen by borrow",
    entry "E0204" "check" "error" "cannot move linear variable: borrowed",
    entry "E0205" "check" "error" "linear variable used after move",
    entry "E0206" "check" "error" "variable reserved by defer",
    entry "E0207" "check" "error" "cannot consume linear variable in loop",
    entry "E0208" "check" "error" "linear variable never consumed",
    entry "E0209" "check" "error" "match arms disagree on consumption",
    entry "E0210" "check" "error" "break skips unconsumed linear variable",
    entry "E0211" "check" "error" "continue skips unconsumed linear variable",
    entry "E0212" "check" "error" "linear consumed in one branch not other",
    entry "E0213" "check" "error" "linear consumed in then-branch (no else)",
    entry "E0214" "check" "error" "borrow ref shadows existing name",
    entry "E0215" "check" "error" "borrow region shadows existing name",
    entry "E0216" "check" "error" "unknown loop label",
    entry "E0217" "check" "error" "assignment to immutable variable",
    entry "E0218" "check" "error" "assignment to frozen variable",
    entry "E0219" "check" "error" "reassignment of linear variable",
    -- Check: type mismatch (E0220–E0229)
    entry "E0220" "check" "error" "type mismatch",
    entry "E0221" "check" "error" "dereference of non-reference type",
    entry "E0222" "check" "error" "while break/else type mismatch",
    entry "E0223" "check" "error" "if condition not Bool",
    entry "E0224" "check" "error" "if branch type mismatch",
    entry "E0225" "check" "error" "match arm type mismatch",
    entry "E0226" "check" "error" "break value type mismatch",
    -- Check: borrow/escape (E0230–E0235)
    entry "E0230" "check" "error" "cannot borrow: already moved",
    entry "E0231" "check" "error" "cannot borrow: already mutably borrowed",
    entry "E0232" "check" "error" "cannot mutably borrow: already borrowed",
    entry "E0233" "check" "error" "cannot mutably borrow immutable variable",
    entry "E0234" "check" "error" "reference escapes borrow block",
    entry "E0235" "check" "error" "cannot mutably borrow: already immutably borrowed",
    -- Check: capability (E0240–E0242)
    entry "E0240" "check" "error" "missing capability",
    entry "E0241" "check" "error" "trait bound not satisfied",
    entry "E0242" "check" "error" "cannot infer capability variable",
    -- Check: struct/enum/function (E0250–E0277)
    entry "E0250" "check" "error" "unknown struct type",
    entry "E0251" "check" "error" "struct has no field",
    entry "E0252" "check" "error" "missing field in literal",
    entry "E0253" "check" "error" "unknown field in literal",
    entry "E0254" "check" "error" "field access on non-struct",
    entry "E0255" "check" "error" "heap access required",
    entry "E0256" "check" "error" "arrow access on non-heap type",
    entry "E0257" "check" "error" "arrow access on non-struct inner type",
    entry "E0258" "check" "error" "arrow assign on non-heap type",
    entry "E0259" "check" "error" "arrow assign on non-struct",
    entry "E0260" "check" "error" "unknown variant",
    entry "E0261" "check" "error" "unknown enum type",
    entry "E0262" "check" "error" "wrong argument count",
    entry "E0263" "check" "error" "undeclared function",
    entry "E0264" "check" "error" "no method on type",
    entry "E0265" "check" "error" "no method on type variable",
    entry "E0266" "check" "error" "method call on non-named type",
    entry "E0267" "check" "error" "unknown function reference",
    entry "E0268" "check" "error" "builtin wrong argument count",
    entry "E0269" "check" "error" "builtin wrong type argument count",
    entry "E0270" "check" "error" "builtin wrong first argument type",
    entry "E0271" "check" "error" "builtin bad key type",
    entry "E0272" "check" "error" "destroy requires named type",
    entry "E0273" "check" "error" "type does not implement Destroy",
    entry "E0274" "check" "error" "free requires Heap type",
    entry "E0275" "check" "error" "? operator requires Result type",
    entry "E0276" "check" "error" "? operator requires Ok/Err variants",
    entry "E0277" "check" "error" "Ok variant has no value field",
    -- Check: control flow (E0280–E0285)
    entry "E0280" "check" "error" "break outside loop",
    entry "E0281" "check" "error" "continue outside loop",
    entry "E0282" "check" "error" "defer body must be a function call",
    entry "E0283" "check" "error" "reserved identifier",
    entry "E0284" "check" "error" "unknown module",
    entry "E0285" "check" "error" "symbol not public in module",
    -- Elab (E0400–E0419)
    entry "E0400" "elab" "error" "Self outside impl block",
    entry "E0401" "elab" "error" "undeclared variable",
    entry "E0402" "elab" "error" "undeclared function",
    entry "E0403" "elab" "error" "unknown function reference",
    entry "E0404" "elab" "error" "assignment to undeclared variable",
    entry "E0405" "elab" "error" "borrow of undeclared variable",
    entry "E0406" "elab" "error" "unknown struct type",
    entry "E0407" "elab" "error" "arrow access on unknown struct",
    entry "E0408" "elab" "error" "struct has no field",
    entry "E0409" "elab" "error" "field access on non-struct",
    entry "E0410" "elab" "error" "unknown enum type",
    entry "E0411" "elab" "error" "unknown variant",
    entry "E0412" "elab" "error" "missing field in variant",
    entry "E0413" "elab" "error" "no method on type variable",
    entry "E0414" "elab" "error" "no method on type",
    entry "E0415" "elab" "error" "method call on non-named type",
    entry "E0416" "elab" "error" "empty array literal",
    entry "E0417" "elab" "error" "submodule elaboration error",
    entry "E0418" "elab" "error" "unknown module",
    entry "E0419" "elab" "error" "symbol not public in module",
    -- Core-check: type consistency (E0500–E0509)
    entry "E0500" "core-check" "error" "variable type mismatch",
    entry "E0501" "core-check" "error" "arithmetic on non-numeric type",
    entry "E0502" "core-check" "error" "binary operand type mismatch",
    entry "E0503" "core-check" "error" "comparison operand type mismatch",
    entry "E0504" "core-check" "error" "comparison result not Bool",
    entry "E0505" "core-check" "error" "logical operator on non-Bool",
    entry "E0506" "core-check" "error" "bitwise operator on non-integer",
    entry "E0507" "core-check" "error" "negation on non-numeric type",
    entry "E0508" "core-check" "error" "logical not on non-Bool",
    entry "E0509" "core-check" "error" "bitwise not on non-integer",
    -- Core-check: capability (E0520–E0522)
    entry "E0520" "core-check" "error" "insufficient capabilities",
    entry "E0521" "core-check" "error" "missing capability",
    entry "E0522" "core-check" "error" "argument count mismatch",
    -- Core-check: match coverage (E0530–E0534)
    entry "E0530" "core-check" "error" "non-exhaustive match: missing variant",
    entry "E0531" "core-check" "error" "match arm wrong enum",
    entry "E0532" "core-check" "error" "duplicate match arm",
    entry "E0533" "core-check" "error" "variant field count mismatch",
    entry "E0534" "core-check" "error" "non-enum match without default arm",
    -- Core-check: control flow (E0540–E0543)
    entry "E0540" "core-check" "error" "while condition not Bool",
    entry "E0541" "core-check" "error" "if condition not Bool",
    entry "E0542" "core-check" "error" "break outside loop",
    entry "E0543" "core-check" "error" "continue outside loop",
    -- Core-check: type legality (E0550–E0560)
    entry "E0550" "core-check" "error" "empty array literal",
    entry "E0551" "core-check" "error" "array index not integer",
    entry "E0552" "core-check" "error" "indexing non-array type",
    entry "E0553" "core-check" "error" "cannot cast between types",
    entry "E0554" "core-check" "error" "dereference of non-reference type",
    entry "E0555" "core-check" "error" "assign through non-mutable reference",
    entry "E0560" "core-check" "error" "return type mismatch",
    -- Core-check: module validation (E0570–E0582)
    entry "E0570" "core-check" "error" "Copy/Destroy conflict",
    entry "E0571" "core-check" "error" "Copy struct has non-Copy field",
    entry "E0572" "core-check" "error" "repr(C) struct has generics",
    entry "E0573" "core-check" "error" "repr(C) field not FFI-safe",
    entry "E0574" "core-check" "error" "extern fn parameter not FFI-safe",
    entry "E0575" "core-check" "error" "extern fn return not FFI-safe",
    entry "E0576" "core-check" "error" "repr(packed) and repr(align) conflict",
    entry "E0577" "core-check" "error" "repr(align) not power of two",
    entry "E0578" "core-check" "error" "reserved function name",
    entry "E0579" "core-check" "error" "builtin trait redeclared",
    entry "E0580" "core-check" "error" "unknown trait",
    entry "E0581" "core-check" "error" "missing trait method",
    entry "E0582" "core-check" "error" "trait method return type mismatch",
    -- Verify/lower (E0600–E0602)
    entry "E0600" "verify" "error" "post-elab: placeholder type survived elaboration",
    entry "E0601" "verify" "error" "post-mono: type variable survived monomorphization",
    entry "E0602" "lower" "error" "lowering failure",
    -- Policy (E0610–E0612)
    entry "E0610" "policy" "error" "predictable profile violation",
    entry "E0611" "policy" "error" "denied capability used",
    entry "E0612" "policy" "error" "proof-eligible function unproved",
    -- SSA-verify (E0700–E0715)
    entry "E0700" "ssa-verify" "error" "duplicate register definition",
    entry "E0701" "ssa-verify" "error" "function has no blocks",
    entry "E0702" "ssa-verify" "error" "use before definition",
    entry "E0703" "ssa-verify" "error" "use in non-dominating block",
    entry "E0704" "ssa-verify" "error" "undefined register",
    entry "E0705" "ssa-verify" "error" "branch to unknown label",
    entry "E0706" "ssa-verify" "error" "phi missing predecessor",
    entry "E0707" "ssa-verify" "error" "phi extra predecessor",
    entry "E0708" "ssa-verify" "error" "phi not dominated by source",
    entry "E0709" "ssa-verify" "error" "phi uses undefined register",
    entry "E0710" "ssa-verify" "error" "phi type mismatch",
    entry "E0711" "ssa-verify" "error" "call arity mismatch",
    entry "E0712" "ssa-verify" "error" "ret void in non-void function",
    entry "E0713" "ssa-verify" "error" "ret value in void function",
    entry "E0714" "ssa-verify" "error" "phi on aggregate type",
    entry "E0715" "ssa-verify" "error" "binop operand type mismatch",
    -- Proof diagnostics (E0800–E0807)
    entry "E0800" "proof" "error" "stale proof: fingerprint changed",
    entry "E0801" "proof" "warning" "missing proof: eligible but unproved",
    entry "E0802" "proof" "info" "ineligible: fails profile gates",
    entry "E0803" "proof" "error" "blocked: unsupported construct in extraction",
    entry "E0804" "proof" "info" "trusted: marked trusted",
    entry "E0805" "proof" "error" "attachment integrity: registry entry is invalid",
    entry "E0806" "proof" "warning" "theorem lookup: Lean proof name not found",
    entry "E0807" "proof" "error" "lean check failure: Lean kernel rejected proof"
  ]
  (Val.obj [
    ("schema_version", .num (Int.ofNat schemaVersion)),
    ("code_count", .num (Int.ofNat codes.length)),
    ("codes", .arr codes),
    ("severity_meanings", .obj [
      ("error", .str "blocks compilation or proof pipeline"),
      ("warning", .str "needs attention but allows continuation"),
      ("info", .str "informational, no action required"),
      ("note", .str "additional context for another diagnostic")
    ]),
    ("compatibility", .str "Error codes are stable identifiers. A code will not be reassigned to a different meaning. New codes may be added. Codes may be retired (no longer emitted) but not reused. Consumers should tolerate unknown codes.")
  ]).render

end Report
end Concrete

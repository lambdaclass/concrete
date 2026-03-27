import Concrete.Core
import Concrete.Layout
import Concrete.FileSummary
import Concrete.AST
import Concrete.Intrinsic

namespace Concrete
namespace Report

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
-- Body-walking infrastructure
-- ============================================================
-- Shared recursive traversal of Core IR (CExpr/CStmt/CMatchArm)
-- for collecting call sites, defer nodes, pointer ops, etc.

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

mutual
partial def collectCallsExpr (e : CExpr) : List String :=
  match e with
  | .call fn _ args _ => [fn] ++ args.foldl (fun acc a => acc ++ collectCallsExpr a) []
  | .binOp _ l r _ => collectCallsExpr l ++ collectCallsExpr r
  | .unaryOp _ e _ => collectCallsExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCallsExpr v) []
  | .fieldAccess obj _ _ => collectCallsExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectCallsExpr v) []
  | .match_ scrut arms _ => collectCallsExpr scrut ++ arms.foldl (fun acc a => acc ++ collectCallsArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectCallsExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectCallsExpr e) []
  | .arrayIndex arr idx _ => collectCallsExpr arr ++ collectCallsExpr idx
  | .cast inner _ | .try_ inner _ => collectCallsExpr inner
  | .allocCall inner alloc _ => collectCallsExpr inner ++ collectCallsExpr alloc
  | .whileExpr cond body elseBody _ =>
    collectCallsExpr cond ++ collectCallsStmts body ++ collectCallsStmts elseBody
  | _ => []  -- intLit, floatLit, boolLit, strLit, charLit, ident, fnRef

partial def collectCallsArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ body => collectCallsStmts body
  | .litArm v body => collectCallsExpr v ++ collectCallsStmts body
  | .varArm _ _ body => collectCallsStmts body

partial def collectCallsStmt (s : CStmt) : List String :=
  match s with
  | .letDecl _ _ _ v => collectCallsExpr v
  | .assign _ v => collectCallsExpr v
  | .return_ (some v) _ => collectCallsExpr v
  | .return_ none _ => []
  | .expr e => collectCallsExpr e
  | .ifElse c t el =>
    collectCallsExpr c ++ collectCallsStmts t ++
    match el with | some stmts => collectCallsStmts stmts | none => []
  | .while_ c body _ step =>
    collectCallsExpr c ++ collectCallsStmts body ++ collectCallsStmts step
  | .fieldAssign obj _ v => collectCallsExpr obj ++ collectCallsExpr v
  | .derefAssign t v => collectCallsExpr t ++ collectCallsExpr v
  | .arrayIndexAssign arr idx v =>
    collectCallsExpr arr ++ collectCallsExpr idx ++ collectCallsExpr v
  | .break_ (some v) _ => collectCallsExpr v
  | .break_ none _ | .continue_ _ => []
  | .defer body => collectCallsExpr body
  | .borrowIn _ _ _ _ _ body => collectCallsStmts body

partial def collectCallsStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectCallsStmt s) []

-- Defer collection

partial def collectDefersExpr (e : CExpr) : List String :=
  match e with
  | .call _ _ args _ => args.foldl (fun acc a => acc ++ collectDefersExpr a) []
  | .binOp _ l r _ => collectDefersExpr l ++ collectDefersExpr r
  | .unaryOp _ e _ => collectDefersExpr e
  | .structLit _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectDefersExpr v) []
  | .fieldAccess obj _ _ => collectDefersExpr obj
  | .enumLit _ _ _ fields _ => fields.foldl (fun acc (_, v) => acc ++ collectDefersExpr v) []
  | .match_ scrut arms _ =>
    collectDefersExpr scrut ++ arms.foldl (fun acc a => acc ++ collectDefersArm a) []
  | .borrow inner _ | .borrowMut inner _ | .deref inner _ => collectDefersExpr inner
  | .arrayLit elems _ => elems.foldl (fun acc e => acc ++ collectDefersExpr e) []
  | .arrayIndex arr idx _ => collectDefersExpr arr ++ collectDefersExpr idx
  | .cast inner _ | .try_ inner _ => collectDefersExpr inner
  | .allocCall inner alloc _ => collectDefersExpr inner ++ collectDefersExpr alloc
  | .whileExpr cond body elseBody _ =>
    collectDefersExpr cond ++ collectDefersStmts body ++ collectDefersStmts elseBody
  | _ => []

partial def collectDefersArm (arm : CMatchArm) : List String :=
  match arm with
  | .enumArm _ _ _ body => collectDefersStmts body
  | .litArm v body => collectDefersExpr v ++ collectDefersStmts body
  | .varArm _ _ body => collectDefersStmts body

partial def collectDefersStmt (s : CStmt) : List String :=
  match s with
  | .defer body =>
    -- Describe the deferred expression
    let desc := match body with
      | .call fn _ _ _ => s!"defer {fn}(...)"
      | _ => "defer <expr>"
    [desc] ++ collectDefersExpr body
  | .letDecl _ _ _ v => collectDefersExpr v
  | .assign _ v => collectDefersExpr v
  | .return_ (some v) _ => collectDefersExpr v
  | .return_ none _ => []
  | .expr e => collectDefersExpr e
  | .ifElse c t el =>
    collectDefersExpr c ++ collectDefersStmts t ++
    match el with | some stmts => collectDefersStmts stmts | none => []
  | .while_ c body _ step =>
    collectDefersExpr c ++ collectDefersStmts body ++ collectDefersStmts step
  | .fieldAssign obj _ v => collectDefersExpr obj ++ collectDefersExpr v
  | .derefAssign t v => collectDefersExpr t ++ collectDefersExpr v
  | .arrayIndexAssign arr idx v =>
    collectDefersExpr arr ++ collectDefersExpr idx ++ collectDefersExpr v
  | .break_ (some v) _ => collectDefersExpr v
  | .break_ none _ | .continue_ _ => []
  | .borrowIn _ _ _ _ _ body => collectDefersStmts body

partial def collectDefersStmts (ss : List CStmt) : List String :=
  ss.foldl (fun acc s => acc ++ collectDefersStmt s) []

-- Raw pointer operation detection

partial def hasRawPtrOpsExpr (e : CExpr) : Bool :=
  match e with
  | .deref inner ty =>
    match ty with
    | .ptrMut _ | .ptrConst _ => true
    | _ => hasRawPtrOpsExpr inner
  | .call _ _ args _ => args.any hasRawPtrOpsExpr
  | .binOp _ l r _ => hasRawPtrOpsExpr l || hasRawPtrOpsExpr r
  | .unaryOp _ e _ => hasRawPtrOpsExpr e
  | .structLit _ _ fields _ => fields.any (fun (_, v) => hasRawPtrOpsExpr v)
  | .fieldAccess obj _ _ => hasRawPtrOpsExpr obj
  | .enumLit _ _ _ fields _ => fields.any (fun (_, v) => hasRawPtrOpsExpr v)
  | .match_ scrut arms _ =>
    hasRawPtrOpsExpr scrut || arms.any hasRawPtrOpsArm
  | .borrow inner _ | .borrowMut inner _ => hasRawPtrOpsExpr inner
  | .arrayLit elems _ => elems.any hasRawPtrOpsExpr
  | .arrayIndex arr idx _ => hasRawPtrOpsExpr arr || hasRawPtrOpsExpr idx
  | .cast inner _ | .try_ inner _ => hasRawPtrOpsExpr inner
  | .allocCall inner alloc _ => hasRawPtrOpsExpr inner || hasRawPtrOpsExpr alloc
  | .whileExpr cond body elseBody _ =>
    hasRawPtrOpsExpr cond || hasRawPtrOpsStmts body || hasRawPtrOpsStmts elseBody
  | _ => false

partial def hasRawPtrOpsArm (arm : CMatchArm) : Bool :=
  match arm with
  | .enumArm _ _ _ body => hasRawPtrOpsStmts body
  | .litArm v body => hasRawPtrOpsExpr v || hasRawPtrOpsStmts body
  | .varArm _ _ body => hasRawPtrOpsStmts body

partial def hasRawPtrOpsStmt (s : CStmt) : Bool :=
  match s with
  | .derefAssign _ _ => true
  | .letDecl _ _ _ v => hasRawPtrOpsExpr v
  | .assign _ v => hasRawPtrOpsExpr v
  | .return_ (some v) _ => hasRawPtrOpsExpr v
  | .return_ none _ => false
  | .expr e => hasRawPtrOpsExpr e
  | .ifElse c t el =>
    hasRawPtrOpsExpr c || hasRawPtrOpsStmts t ||
    match el with | some stmts => hasRawPtrOpsStmts stmts | none => false
  | .while_ c body _ step =>
    hasRawPtrOpsExpr c || hasRawPtrOpsStmts body || hasRawPtrOpsStmts step
  | .fieldAssign obj _ v => hasRawPtrOpsExpr obj || hasRawPtrOpsExpr v
  | .arrayIndexAssign arr idx v =>
    hasRawPtrOpsExpr arr || hasRawPtrOpsExpr idx || hasRawPtrOpsExpr v
  | .break_ (some v) _ => hasRawPtrOpsExpr v
  | .break_ none _ | .continue_ _ => false
  | .defer body => hasRawPtrOpsExpr body
  | .borrowIn _ _ _ _ _ body => hasRawPtrOpsStmts body

partial def hasRawPtrOpsStmts (ss : List CStmt) : Bool :=
  ss.any hasRawPtrOpsStmt
end

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
-- Extern name lookup (for unsafe body analysis)
-- ============================================================

private partial def collectExternNames (m : CModule) : List String :=
  m.externFns.map (fun (n, _, _, _) => n) ++
  m.submodules.foldl (fun acc sub => acc ++ collectExternNames sub) []

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

def unsafeReport (modules : List CModule) : String :=
  let header := "=== Unsafe Signature Summary ==="
  let externNames := modules.foldl (fun acc m => acc ++ collectExternNames m) []
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

private def buildLayoutCtx (modules : List CModule) : Layout.Ctx :=
  let structs := modules.foldl (fun acc m => acc ++ m.structs ++ collectSubStructs m) []
  let enums := modules.foldl (fun acc m => acc ++ m.enums ++ collectSubEnums m) []
  { structDefs := structs, enumDefs := enums }

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

/-- Intrinsic names that represent allocation. -/
private def allocIntrinsics : List String :=
  ["alloc", "vec_new", "Vec_new"]

/-- Intrinsic names that represent deallocation/cleanup. -/
private def freeIntrinsics : List String :=
  ["free", "destroy", "vec_free", "Vec_free", "drop_string", "String_drop"]

/-- Is this call name an alloc-family intrinsic? -/
private def isAllocCall (name : String) : Bool :=
  allocIntrinsics.contains name ||
  -- Also catch Type_destroy patterns as not-alloc
  match resolveIntrinsic name with
  | some .alloc | some .vecNew => true
  | _ => false

/-- Is this call name a free/cleanup-family intrinsic? -/
private def isFreeCall (name : String) : Bool :=
  freeIntrinsics.contains name ||
  name.endsWith "_destroy" ||
  match resolveIntrinsic name with
  | some .free | some .destroy | some .vecFree | some .dropString => true
  | _ => false

/-- Check if a return type suggests the allocation is returned to the caller. -/
private def returnsAllocation : Ty → Bool
  | .heap _ | .heapArray _ => true
  | .generic "Vec" _ => true
  | _ => false

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

/-- A flat map of function name → list of direct callees. -/
abbrev CallGraph := List (String × List String)

/-- Build a call graph from all modules. -/
private partial def buildCallGraphModule (m : CModule) : CallGraph :=
  let fnEntries := m.functions.map fun f =>
    (f.name, collectCallsStmts f.body |>.eraseDups)
  fnEntries ++ m.submodules.foldl (fun acc sub => acc ++ buildCallGraphModule sub) []

private def buildCallGraph (modules : List CModule) : CallGraph :=
  modules.foldl (fun acc m => acc ++ buildCallGraphModule m) []

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
  let lookup := buildCapLookup modules
  let callGraph := buildCallGraph modules
  let allCaps := validCaps  -- all 9 capabilities
  let sections := allCaps.filterMap fun cap =>
    let fns := modules.foldl (fun acc m => acc ++ collectFnsWithCap m cap) []
    if fns.isEmpty then none
    else
      let capHeader := s!"capability {cap} ({fns.length} functions):"
      let fnLines := fns.map fun f =>
        let pubStr := if f.isPublic then "pub " else "    "
        let chain := findCapChain callGraph lookup f.name cap
        let chainStr := if chain.length <= 1 then "  <- declared"
          else s!"  <- {" -> ".intercalate chain}"
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

def proofReport (modules : List CModule) : String :=
  let header := "=== Proof Eligibility Report ==="
  let externNames := modules.foldl (fun acc m => acc ++ collectExternNames m) []
  let body := modules.map (proofReportModule externNames · "")
  let allFns := modules.foldl (fun acc m => acc ++ collectAllFnDefs m) []
  let eligible := allFns.filter fun f =>
    (proofExclusionReasons externNames f).isEmpty
  let excluded := allFns.length - eligible.length
  let summary := s!"\nTotals: {allFns.length} functions, {eligible.length} eligible for ProofCore, {excluded} excluded"
  s!"{header}\n\n{"\n\n".intercalate body}\n{summary}\n"

end Report
end Concrete

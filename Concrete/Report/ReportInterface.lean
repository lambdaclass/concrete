import Concrete.Elab.Core
import Concrete.Check.Layout
import Concrete.Resolve.FileSummary
import Concrete.Frontend.AST
import Concrete.Resolve.Intrinsic
import Concrete.Proof.Proof
import Concrete.Proof.ProofCore
import Concrete.IR.SSA
import Concrete.Report.Diagnostic
import Concrete.Frontend.Format
import Concrete.Report.ReportBase
import Concrete.Report.ReportVC

namespace Concrete
namespace Report


-- ============================================================
-- Report 1: Capability Summary with "why" traces (--report caps)
-- ============================================================

/-- Count functions across module tree. -/
partial def countModuleFns (m : CModule) : Nat :=
  m.functions.length + m.submodules.foldl (fun acc sub => acc + countModuleFns sub) 0

partial def countModulePure (m : CModule) : Nat :=
  let local_ := (m.functions.filter fun f => f.capSet == .empty).length
  local_ + m.submodules.foldl (fun acc sub => acc + countModulePure sub) 0

partial def countModuleExterns (m : CModule) : Nat :=
  m.externFns.length + m.submodules.foldl (fun acc sub => acc + countModuleExterns sub) 0

/-- Build capability "why" trace lines for a function.
    For each concrete cap the function requires, find which direct callees
    contribute that cap. -/
def capWhyTrace (lookup : CapLookup) (f : CFnDef) (indent : String) : List String :=
  let (concreteCaps, _) := f.capSet.normalize
  if concreteCaps.isEmpty then []
  else
    let callees := collectCallsStmts f.body |>.eraseDups
    concreteCaps.filterMap fun cap =>
      -- Find callees that require this cap
      let contributors := callees.filter fun callee =>
        match lookupCalleeCap lookup callee with
        | some cs => Capabilities.capSetHas cs cap
        | none => false
      let contribStr := if contributors.isEmpty then "<- declared"
        else
          let tagged := contributors.map fun c => s!"{c}{calleeTag lookup c}"
          s!"<- calls {", ".intercalate tagged}"
      some s!"{indent}    {padRight cap 10} {contribStr}"

partial def capReportModule (lookup : CapLookup) (m : CModule) (indent : String) : String :=
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
-- Arithmetic site classification (--report arithmetic)
-- ROADMAP #10 §3.2 / §9.1: every arithmetic site is classified as exactly one
-- of proved / runtime-checked / explicit-wrapping / explicit-saturating, so a
-- reviewer can see both an operation's meaning (from the source spelling) and
-- how a checked one is discharged.
-- ============================================================

inductive ArithClass where
  | runtimeChecked | proved | wrapping | saturating
  deriving BEq

def ArithClass.label : ArithClass → String
  | .runtimeChecked => "runtime-checked"
  | .proved         => "proved"
  | .wrapping       => "explicit-wrapping"
  | .saturating     => "explicit-saturating"

def arithIsIntTy : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | _ => false

def arithIsFloatTy : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

/-- One arithmetic site: the operation's source label and its class. -/
abbrev ArithSite := String × ArithClass

/-- Classify a binary operator at a given operand type, or `none` if it is not
    an overflow-relevant arithmetic site (comparisons, logical, and pure bitwise
    `& | ^` are total and not classified; float `+ - * /` use IEEE semantics, not
    the checked-integer trap, so they are excluded too). -/
def classifyArithBinOp (op : BinOp) (operandTy : Ty) : Option ArithSite :=
  match op with
  | .wrappingAdd => some ("wrapping_add", .wrapping)
  | .wrappingSub => some ("wrapping_sub", .wrapping)
  | .wrappingMul => some ("wrapping_mul", .wrapping)
  | .saturatingAdd => some ("saturating_add", .saturating)
  | .saturatingSub => some ("saturating_sub", .saturating)
  | .saturatingMul => some ("saturating_mul", .saturating)
  | .add => if arithIsIntTy operandTy then some ("+", .runtimeChecked) else none
  | .sub => if arithIsIntTy operandTy then some ("-", .runtimeChecked) else none
  | .mul => if arithIsIntTy operandTy then some ("*", .runtimeChecked) else none
  | .div => if arithIsIntTy operandTy then some ("/", .runtimeChecked) else none
  | .mod => if arithIsIntTy operandTy then some ("%", .runtimeChecked) else none
  | .shl => if arithIsIntTy operandTy then some ("<<", .runtimeChecked) else none
  | .shr => if arithIsIntTy operandTy then some (">>", .runtimeChecked) else none
  | _ => none

mutual
/-- Collect every arithmetic site in an expression. -/
partial def arithSitesE : CExpr → List ArithSite
  | .binOp op l r _ =>
      (match classifyArithBinOp op (CExpr.ty l) with | some s => [s] | none => [])
        ++ arithSitesE l ++ arithSitesE r
  | .unaryOp op operand _ =>
      (match op with
       | .neg => if arithIsIntTy (CExpr.ty operand) then [("unary -", ArithClass.runtimeChecked)] else []
       | _ => [])
        ++ arithSitesE operand
  | .cast inner targetTy =>
      (if arithIsFloatTy (CExpr.ty inner) && arithIsIntTy targetTy
       then [("float->int cast", ArithClass.runtimeChecked)] else [])
        ++ arithSitesE inner
  | .call _ _ args _ => (args.map arithSitesE).flatten
  | .structLit _ _ fields _ => (fields.map (fun f => arithSitesE f.2)).flatten
  | .fieldAccess obj _ _ => arithSitesE obj
  | .enumLit _ _ _ fields _ => (fields.map (fun f => arithSitesE f.2)).flatten
  | .match_ scrut arms _ => arithSitesE scrut ++ (arms.map arithSitesArm).flatten
  | .borrow inner _ => arithSitesE inner
  | .borrowMut inner _ => arithSitesE inner
  | .deref inner _ => arithSitesE inner
  | .arrayLit elems _ => (elems.map arithSitesE).flatten
  | .arrayIndex arr idx _ => arithSitesE arr ++ arithSitesE idx
  | .try_ inner _ => arithSitesE inner
  | .allocCall inner allocExpr _ => arithSitesE inner ++ arithSitesE allocExpr
  | .ifExpr cond then_ else_ _ =>
      arithSitesE cond ++ (then_.map arithSitesS).flatten ++ (else_.map arithSitesS).flatten
  | _ => []

partial def arithSitesArm : CMatchArm → List ArithSite
  | .enumArm _ _ _ guard body =>
      (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten
  | .litArm value guard body =>
      arithSitesE value ++ (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten
  | .varArm _ _ guard body =>
      (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten
  | .rangeArm lo hi _ guard body =>
      arithSitesE lo ++ arithSitesE hi ++ (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten

partial def arithSitesS : CStmt → List ArithSite
  | .letDecl _ _ _ value => arithSitesE value
  | .assign _ value => arithSitesE value
  | .return_ value _ => match value with | some v => arithSitesE v | none => []
  | .expr e _ => arithSitesE e
  | .ifElse cond then_ else_ =>
      arithSitesE cond ++ (then_.map arithSitesS).flatten
        ++ (match else_ with | some e => (e.map arithSitesS).flatten | none => [])
  | .while_ cond body _ step =>
      arithSitesE cond ++ (body.map arithSitesS).flatten ++ (step.map arithSitesS).flatten
  | .fieldAssign obj _ value => arithSitesE obj ++ arithSitesE value
  | .derefAssign target value => arithSitesE target ++ arithSitesE value
  | .arrayIndexAssign arr idx value => arithSitesE arr ++ arithSitesE idx ++ arithSitesE value
  | .break_ value _ => match value with | some v => arithSitesE v | none => []
  | .continue_ _ => []
  | .defer body => arithSitesE body
  | .borrowIn _ _ _ _ _ body => (body.map arithSitesS).flatten
end

structure ArithFnRow where
  qualName : String
  loc : Option SourceLoc
  sites : List ArithSite

partial def arithRowsForModule (locMap : FnLocMap) (m : CModule)
    (modulePath : String := "") : List ArithFnRow :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let rows : List ArithFnRow := m.functions.map fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    { qualName := qualName, loc := lookupLoc locMap qualName,
      sites := (f.body.map arithSitesS).flatten }
  rows ++ m.submodules.foldl (fun acc sub => acc ++ arithRowsForModule locMap sub qualPrefix) []

def countClass (sites : List ArithSite) (c : ArithClass) : Nat :=
  (sites.filter fun s => s.2 == c).length

/-- `--report arithmetic`: per-function classification of every arithmetic site
    (ROADMAP #10 §3.2). `proved` is currently always zero — the proof model
    discharges refinement against unbounded-`Int` specs with an implicit
    no-overflow assumption rather than discharging overflow obligations, so no
    site is yet "overflow proved"; this becomes non-zero when overflow proofs
    land (then the now-redundant runtime check may be elided). -/
def arithmeticReport (modules : List CModule) (locMap : FnLocMap := []) : String :=
  let header := "=== Arithmetic Site Classification (--report arithmetic) ==="
  let rows := modules.foldl (fun acc m => acc ++ arithRowsForModule locMap m) []
  let rowsWithSites := rows.filter fun r => !r.sites.isEmpty
  let fmtRow := fun (r : ArithFnRow) =>
    let locStr := match r.loc with | some (f, l) => s!"{f}:{l}" | none => "(no source)"
    let rc := countClass r.sites .runtimeChecked
    let wr := countClass r.sites .wrapping
    let sa := countClass r.sites .saturating
    let pr := countClass r.sites .proved
    let detail := ", ".intercalate (r.sites.map fun s => s!"{s.1} [{s.2.label}]")
    s!"-- {r.qualName}  ({locStr})\n   runtime-checked: {rc}  explicit-wrapping: {wr}  explicit-saturating: {sa}  proved: {pr}\n   {detail}"
  let body := if rowsWithSites.isEmpty then "(no arithmetic sites)"
              else "\n\n".intercalate (rowsWithSites.map fmtRow)
  let allSites := (rows.map (·.sites)).flatten
  let total := allSites.length
  let rc := countClass allSites .runtimeChecked
  let wr := countClass allSites .wrapping
  let sa := countClass allSites .saturating
  let pr := countClass allSites .proved
  let summary := s!"\nTotals: {total} arithmetic sites — {rc} runtime-checked, {pr} proved, {wr} explicit-wrapping, {sa} explicit-saturating"
  s!"{header}\n\n{body}\n{summary}\n"

-- ============================================================
-- Report 2: Unsafe Signature Summary with trust boundary
--           analysis (--report unsafe)
-- ============================================================

-- The "does this cap set literally list Unsafe" classification is the one
-- capability-fact source (Phase 6.5 #5); kept under this name for callers.
def hasUnsafeCap (cs : CapSet) : Bool := Capabilities.capSetHasUnsafe cs

def usesRawPtr : Ty → Bool
  | .ptrMut _ | .ptrConst _ => true
  | _ => false

def fnUsesRawPtrs (f : CFnDef) : Bool :=
  f.params.any (fun (_, t) => usesRawPtr t) || usesRawPtr f.retTy

/-- Count unsafe-related items across module tree. -/
partial def unsafeCounts (m : CModule)
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
def trustBoundaryAnalysis (externNames : List String) (f : CFnDef) : List String :=
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

partial def unsafeReportModule (externNames : List String)
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

partial def collectSubStructs (m : CModule) : List CStructDef :=
  m.submodules.foldl (fun acc sub => acc ++ sub.structs ++ collectSubStructs sub) []

partial def collectSubEnums (m : CModule) : List CEnumDef :=
  m.submodules.foldl (fun acc sub => acc ++ sub.enums ++ collectSubEnums sub) []

partial def collectSubNewtypes (m : CModule) : List NewtypeDef :=
  m.submodules.foldl (fun acc sub => acc ++ sub.newtypes ++ collectSubNewtypes sub) []

def buildLayoutCtx (modules : List CModule) : Layout.Ctx :=
  let structs := modules.foldl (fun acc m => acc ++ m.structs ++ collectSubStructs m) []
  let enums := modules.foldl (fun acc m => acc ++ m.enums ++ collectSubEnums m) []
  let newtypes := modules.foldl (fun acc m => acc ++ m.newtypes ++ collectSubNewtypes m) []
  { structDefs := structs, enumDefs := enums, newtypes := newtypes }

def layoutStructReport (ctx : Layout.Ctx) (sd : CStructDef) : Option String :=
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

def layoutEnumReport (ctx : Layout.Ctx) (ed : CEnumDef) : Option String :=
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
partial def collectAllStructs (m : CModule) : List CStructDef :=
  m.structs ++ m.submodules.foldl (fun acc sub => acc ++ collectAllStructs sub) []

partial def collectAllEnums (m : CModule) : List CEnumDef :=
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
end Report
end Concrete

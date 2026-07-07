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
        | some cs =>
          let (calleeCaps, _) := cs.normalize
          calleeCaps.contains cap
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
  | .whileExpr cond body elseBody _ =>
      arithSitesE cond ++ (body.map arithSitesS).flatten ++ (elseBody.map arithSitesS).flatten
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

def hasUnsafeCap (cs : CapSet) : Bool :=
  cs.concreteCaps.contains unsafeCapName

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

-- ============================================================
-- Report 4: Interface Summary (--report interface)
-- ============================================================

def interfaceModule (name : String) (fs : FileSummary) : String :=
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
    | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectCallsE fe) ++ (base.map collectCallsE).getD []
    | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectCallsE fe)
    | .allocCall _ x a => collectCallsE x ++ collectCallsE a
    | .ifExpr _ c t el | .whileExpr _ c t el =>
        collectCallsE c ++ t.flatMap collectCallsS ++ el.flatMap collectCallsS
    | .match_ _ s _ => collectCallsE s
    | _ => []
  /-- Collect every `(span, fnName, args)` call in a statement. -/
  partial def collectCallsS : Stmt → List (Span × String × List Expr)
    | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectCallsE v
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

/-- `(qualName, body fingerprint)` for every Core function — same flat qualified
    naming as `allFunctions`, used to synthesize a fingerprint for in-source
    proof links (the registry fingerprint is computed, not stored in source). -/
partial def coreFnFingerprints (m : CModule) : List (String × String) :=
  let pfx := if m.name.isEmpty then "" else m.name ++ "."
  m.functions.map (fun f => (pfx ++ f.name, bodyFingerprint f.body))
    ++ m.submodules.flatMap coreFnFingerprints

/-- Synthesize `ProofRegistry` entries from in-source `#[proof_by]`/`#[spec]`/
    `#[ensures_proof]`/`#[proof_coverage]` links. The body fingerprint is
    computed from the current Core body, so a source edit is detected by the
    same spec-drift / fingerprint machinery as a JSON entry. -/
def synthesizeSourceLinks (astModules : List Module) (coreModules : List CModule) : ProofRegistry := Id.run do
  let fps := coreModules.flatMap coreFnFingerprints
  let mut entries : ProofRegistry := []
  for (pfx, f) in astModules.flatMap allFunctions do
    match f.proofLink with
    | none => pure ()
    | some link =>
      let qual := pfx ++ f.name
      let fp := (fps.find? (·.1 == qual)).map (·.2) |>.getD ""
      let entry : ProofRegistryEntry :=
        { function := qual, bodyFingerprint := fp,
          proof := link.proofBy.getD "", spec := link.spec.getD "",
          coverage := link.coverage.getD "", ensuresProof := link.ensuresProof,
          -- when present, staleness compares hash(currentFp) against this stored
          -- hash — so source-linked functions get drift detection even without
          -- spec-drift coverage (the soundness gap that kept point proofs on JSON).
          expectedHash := link.fingerprint, sourceLinked := true }
      entries := entries ++ [entry]
  return entries

/-- Merge JSON-registry entries with synthesized in-source link entries. A
    function defining a proof link in BOTH places is an error — one source of
    truth. -/
def mergeSourceLinks (jsonRegistry sourceEntries : ProofRegistry) : Except String ProofRegistry :=
  let jsonFns := jsonRegistry.map (·.function)
  let conflicts := sourceEntries.filter (fun e => jsonFns.contains e.function)
  if conflicts.isEmpty then .ok (jsonRegistry ++ sourceEntries)
  else .error s!"function(s) have both an in-source proof link and a proof-registry.json entry: {", ".intercalate (conflicts.map (·.function))}. Use one source of truth."

-- ---- source-contract expression validation ----

/-- Function/spec/extern names that contract expressions may call. Includes
    bare and qualified names so examples can use either local specs (`ch_spec`)
    or fully-qualified links as the module system evolves. -/
partial def callableContractNames (modules : List Module) : List String := Id.run do
  let rec go (pfx : String) (m : Module) : List String :=
    let q (n : String) := if pfx.isEmpty then n else pfx ++ "." ++ n
    let localFns := m.functions.map (fun f => [f.name, q f.name])
    let localSpecs := m.specFns.map (fun s => [s.name, q s.name])
    let localExterns := m.externFns.map (fun e => [e.name, q e.name])
    let subPfx := if pfx.isEmpty then m.name else pfx ++ "." ++ m.name
    (localFns ++ localSpecs ++ localExterns).flatten ++ m.submodules.flatMap (go subPfx)
  return (modules.flatMap (go "")).eraseDups

/-- Constant names that contract expressions may reference directly. -/
partial def contractConstNames (modules : List Module) : List String := Id.run do
  let rec go (pfx : String) (m : Module) : List String :=
    let q (n : String) := if pfx.isEmpty then n else pfx ++ "." ++ n
    let localConsts := m.constants.map (fun c => [c.name, q c.name])
    let subPfx := if pfx.isEmpty then m.name else pfx ++ "." ++ m.name
    localConsts.flatten ++ m.submodules.flatMap (go subPfx)
  return (modules.flatMap (go "")).eraseDups

/-- Bare + qualified names of functions that require capabilities (effectful).
    The spec/ghost language must be pure and total, so a contract may not call
    these. (`spec fn`s are body-less and Lean-backed — total by construction —
    and pure helpers have no capabilities, so neither is flagged.) -/
partial def impureFnNames (modules : List Module) : List String := Id.run do
  let rec go (pfx : String) (m : Module) : List String :=
    let q (n : String) := if pfx.isEmpty then n else pfx ++ "." ++ n
    let here := m.functions.filterMap (fun f => if f.capSet.isEmpty then none else some [f.name, q f.name])
    let subPfx := if pfx.isEmpty then m.name else pfx ++ "." ++ m.name
    here.flatten ++ m.submodules.flatMap (go subPfx)
  return (modules.flatMap (go "")).eraseDups

/-- Names of impure (capability-requiring) functions CALLED inside a contract
    expression. A non-empty result means the contract is not pure/total. -/
partial def contractImpureCalls (impureFns : List String) : Expr → List String
  | .call _ fn _ args =>
      (if impureFns.contains fn then [fn] else []) ++ args.flatMap (contractImpureCalls impureFns)
  | .staticMethodCall _ ty meth _ args =>
      (if impureFns.contains (ty ++ "." ++ meth) || impureFns.contains meth then [ty ++ "." ++ meth] else [])
        ++ args.flatMap (contractImpureCalls impureFns)
  | .methodCall _ o _ _ args => contractImpureCalls impureFns o ++ args.flatMap (contractImpureCalls impureFns)
  | .binOp _ _ l r => contractImpureCalls impureFns l ++ contractImpureCalls impureFns r
  | .unaryOp _ _ e | .paren _ e | .borrow _ e | .borrowMut _ e | .deref _ e
  | .try_ _ e | .cast _ e _ | .fieldAccess _ e _ | .arrowAccess _ e _ => contractImpureCalls impureFns e
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, e) => contractImpureCalls impureFns e) ++ (base.map (contractImpureCalls impureFns)).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, e) => contractImpureCalls impureFns e)
  | .arrayLit _ es => es.flatMap (contractImpureCalls impureFns)
  | .arrayIndex _ a i => contractImpureCalls impureFns a ++ contractImpureCalls impureFns i
  | _ => []

mutual
  /-- Names introduced by a statement body. This is intentionally conservative:
      loop invariants may mention counters/ghost lets and other locals, and the
      later VC generator decides whether the fact is actually usable. -/
  partial def localNamesS : Stmt → List String
    | .letDecl _ n _ _ v _ => n :: localNamesE v
    | .assign _ n v => n :: localNamesE v
    | .return_ _ (some v) | .expr _ v _ | .defer _ v => localNamesE v
    | .return_ _ none | .break_ _ none _ | .continue_ _ _ => []
    | .ifElse _ c t el => localNamesE c ++ localNamesB t ++ localNamesB (el.getD [])
    | .while_ _ c b _ => localNamesE c ++ localNamesB b
    | .forLoop _ init c step b _ =>
      ((init.map localNamesS).getD []) ++ localNamesE c ++ ((step.map localNamesS).getD []) ++ localNamesB b
    | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v => localNamesE o ++ localNamesE v
    | .arrayIndexAssign _ a i v => localNamesE a ++ localNamesE i ++ localNamesE v
    | .break_ _ (some v) _ => localNamesE v
    | .borrowIn _ v r _ _ b => [v, r] ++ localNamesB b
    | .letDestructure _ _ _ bs v el => bs ++ localNamesE v ++ localNamesB (el.getD [])
    | .letStructDestructure _ _ bs v => bs ++ localNamesE v
    | .assert_ _ c | .assume_ _ c => localNamesE c

  partial def localNamesB (body : List Stmt) : List String :=
    (body.flatMap localNamesS).eraseDups

  partial def localNamesE : Expr → List String
    | .binOp _ _ l r => localNamesE l ++ localNamesE r
    | .unaryOp _ _ e | .paren _ e | .borrow _ e | .borrowMut _ e | .deref _ e
    | .try_ _ e | .cast _ e _ | .fieldAccess _ e _ | .arrowAccess _ e _ => localNamesE e
    | .call _ _ _ args | .staticMethodCall _ _ _ _ args => args.flatMap localNamesE
    | .methodCall _ o _ _ args => localNamesE o ++ args.flatMap localNamesE
    | .structLit _ _ _ fs base => fs.flatMap (fun (_, e) => localNamesE e) ++ (base.map localNamesE).getD []
    | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, e) => localNamesE e)
    | .match_ _ s arms => localNamesE s ++ arms.flatMap localNamesArm
    | .arrayLit _ es => es.flatMap localNamesE
    | .arrayIndex _ a i => localNamesE a ++ localNamesE i
    | .allocCall _ e a => localNamesE e ++ localNamesE a
    | .whileExpr _ c b el | .ifExpr _ c b el => localNamesE c ++ localNamesB b ++ localNamesB el
    | _ => []

  partial def localNamesArm : MatchArm → List String
    | .mk _ _ _ bs g b => (g.map localNamesE).getD [] ++ bs ++ localNamesB b
    | .litArm _ v g b => localNamesE v ++ (g.map localNamesE).getD [] ++ localNamesB b
    | .varArm _ bnd g b => bnd :: ((g.map localNamesE).getD [] ++ localNamesB b)
    | .rangeArm _ lo hi _ g b => localNamesE lo ++ localNamesE hi ++ (g.map localNamesE).getD [] ++ localNamesB b
end

mutual
  /-- Return human-readable validation problems for a source-contract
      expression. This is deliberately scoped, not a full theorem checker: it
      catches silent typos/unknown names without rejecting existing flagship
      contracts that use params, result, locals, ghost lets, and spec calls. -/
  partial def validateContractExpr (allowedVars callables : List String) : Expr → List String
    | .ident _ n =>
      if allowedVars.contains n then [] else [s!"unknown identifier '{n}'"]
    | .fnRef _ n =>
      if callables.contains n then [] else [s!"unknown function/spec '{n}'"]
    | .call _ fn _ args =>
      let here := if callables.contains fn then [] else [s!"unknown function/spec '{fn}'"]
      here ++ args.flatMap (validateContractExpr allowedVars callables)
    | .staticMethodCall _ ty meth _ args =>
      let fn := ty ++ "." ++ meth
      let here := if callables.contains fn || callables.contains meth then [] else [s!"unknown function/spec '{fn}'"]
      here ++ args.flatMap (validateContractExpr allowedVars callables)
    | .methodCall _ obj _ _ args =>
      validateContractExpr allowedVars callables obj ++ args.flatMap (validateContractExpr allowedVars callables)
    | .binOp _ _ l r => validateContractExpr allowedVars callables l ++ validateContractExpr allowedVars callables r
    | .unaryOp _ _ e | .paren _ e | .borrow _ e | .borrowMut _ e | .deref _ e
    | .try_ _ e | .cast _ e _ | .fieldAccess _ e _ | .arrowAccess _ e _ => validateContractExpr allowedVars callables e
    | .structLit _ _ _ fs base => fs.flatMap (fun (_, e) => validateContractExpr allowedVars callables e) ++ (base.map (validateContractExpr allowedVars callables)).getD []
    | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, e) => validateContractExpr allowedVars callables e)
    | .match_ _ s arms => validateContractExpr allowedVars callables s ++ arms.flatMap (validateContractArm allowedVars callables)
    | .arrayLit _ es => es.flatMap (validateContractExpr allowedVars callables)
    | .arrayIndex _ a i => validateContractExpr allowedVars callables a ++ validateContractExpr allowedVars callables i
    | .allocCall _ e a => validateContractExpr allowedVars callables e ++ validateContractExpr allowedVars callables a
    | .whileExpr _ c b el | .ifExpr _ c b el =>
      validateContractExpr allowedVars callables c ++ b.flatMap (validateContractStmt allowedVars callables) ++ el.flatMap (validateContractStmt allowedVars callables)
    | _ => []

  partial def validateContractStmt (allowedVars callables : List String) : Stmt → List String
    | .letDecl _ _ _ _ v _ => validateContractExpr allowedVars callables v
    | .assign _ n v =>
      (if allowedVars.contains n then [] else [s!"unknown identifier '{n}'"]) ++ validateContractExpr allowedVars callables v
    | .return_ _ (some v) | .expr _ v _ | .defer _ v => validateContractExpr allowedVars callables v
    | .return_ _ none | .break_ _ none _ | .continue_ _ _ => []
    | .ifElse _ c t el => validateContractExpr allowedVars callables c ++ t.flatMap (validateContractStmt allowedVars callables) ++ (el.getD []).flatMap (validateContractStmt allowedVars callables)
    | .while_ _ c b _ => validateContractExpr allowedVars callables c ++ b.flatMap (validateContractStmt allowedVars callables)
    | .forLoop _ init c step b _ =>
      ((init.map (validateContractStmt allowedVars callables)).getD []) ++ validateContractExpr allowedVars callables c ++ ((step.map (validateContractStmt allowedVars callables)).getD []) ++ b.flatMap (validateContractStmt allowedVars callables)
    | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v => validateContractExpr allowedVars callables o ++ validateContractExpr allowedVars callables v
    | .arrayIndexAssign _ a i v => validateContractExpr allowedVars callables a ++ validateContractExpr allowedVars callables i ++ validateContractExpr allowedVars callables v
    | .break_ _ (some v) _ => validateContractExpr allowedVars callables v
    | .borrowIn _ v r _ _ b => b.flatMap (validateContractStmt (v :: r :: allowedVars) callables)
    | .letDestructure _ _ _ bs v el => validateContractExpr allowedVars callables v ++ (el.getD []).flatMap (validateContractStmt (bs ++ allowedVars) callables)
    | .letStructDestructure _ _ _ v => validateContractExpr allowedVars callables v
    | .assert_ _ c | .assume_ _ c => validateContractExpr allowedVars callables c

  partial def validateContractArm (allowedVars callables : List String) : MatchArm → List String
    | .mk _ _ _ bs g b =>
      (g.map (validateContractExpr (bs ++ allowedVars) callables)).getD [] ++
      b.flatMap (validateContractStmt (bs ++ allowedVars) callables)
    | .litArm _ v g b =>
      validateContractExpr allowedVars callables v ++
      (g.map (validateContractExpr allowedVars callables)).getD [] ++
      b.flatMap (validateContractStmt allowedVars callables)
    | .varArm _ bnd g b =>
      (g.map (validateContractExpr (bnd :: allowedVars) callables)).getD [] ++
      b.flatMap (validateContractStmt (bnd :: allowedVars) callables)
    | .rangeArm _ lo hi _ g b =>
      validateContractExpr allowedVars callables lo ++ validateContractExpr allowedVars callables hi ++
      (g.map (validateContractExpr allowedVars callables)).getD [] ++
      b.flatMap (validateContractStmt allowedVars callables)
end

def contractIssueStatus (issues : List String) : String :=
  let uniq := issues.eraseDups
  if uniq.isEmpty then ""
  else s!"\n     status:  invalid_contract_expression\n     reason:  {", ".intercalate uniq}"

/-- Top-level `let NAME = <const>` bindings in a body, as NAME → literal expr.
    Lets the call-site checker see e.g. `let n = 7; rotr(x, n)`. -/
def letConstMap (body : List Stmt) : List (String × Expr) :=
  body.filterMap fun s => match s with
    | .letDecl _ name _ _ v _ =>
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
  key        : String := ""    -- stable id "<caller>#pre<i>" for omega discharge keying
  hyps       : List Expr := [] -- caller's #[requires] + enclosing guards/loop invariants in scope at the call

-- `callSiteObligations` is defined below, after the scoped-hypothesis helpers
-- (`loopHypsAt` / `dropStaleHyps` / `assignedScalarsS`) and `toLeanProp`, which
-- it needs to discharge a precondition from the caller's in-scope facts.

/-- Render the call-site obligation section. `provedByBV` are the indices (into
    `obs`) the `bv_decide` backend kernel-checked; `provedByOmega` are the
    obligation `key`s the `omega` backend discharged from the caller's
    `#[requires]` / enclosing guards / loop invariants. -/
def renderCallSites (obs : List CallObligation) (provedByBV : List Nat)
    (provedByOmega : List String := []) : String := Id.run do
  if obs.isEmpty then return ""
  let mut out := "\n\n=== Call-site obligations ==="
  let mut curCaller := ""
  for (i, o) in (List.range obs.length).zip obs do
    if o.caller != curCaller then out := out ++ s!"\n\n{o.caller}"; curCaller := o.caller
    out := out ++ s!"\n  call {o.callStr}\n    requires {Concrete.fmtExpr o.specExpr}"
    let status := match o.baseStatus with
      | "unproven" =>
        if provedByOmega.contains o.key then "proved_by_kernel_decision\n    engine:  omega (from caller's #[requires] / guards)"
        else if provedByBV.contains i then "proved_by_kernel_decision\n    engine:  bv_decide"
        else "unproven_at_callsite (caller does not establish the precondition)"
      -- Constant-folded call-site verdicts: render the CANONICAL status (matching
      -- the ObligationCore ledger's `ofVC`/mkVC mapping), not the internal
      -- `*_at_callsite` baseStatus token — one vocabulary across surfaces (#2/#15).
      | "proved_at_callsite" => "proved_by_kernel_decision\n    engine:  constant_fold (precondition constant-folds true)"
      | "failed_at_callsite" => "counterexample\n    engine:  constant_fold (precondition constant-folds false at this call site)"
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

/-! ### Obligation-expression lowering layer (Phase 3 #12)

The single source of truth for how an obligation's binary operator is spelled in
each lowering target, so the human / Lean-prop / SMT-LIB renderings cannot drift.
Lean is infix (`sym`), parenthesized for the arithmetic/logical ops and bare for
comparisons (matching surface precedence); SMT-LIB is prefix `(sym L R)`. Every
Lean lowering (`toLeanProp` / `toLeanPropD` / `exprToLeanProp`) and the SMT
lowering (`exprToSmt`) consult this table, so adding an operator is a one-line
change checked by `check_obligation_lowering.sh`. -/

/-- Lean infix spelling of a binary operator: `(symbol, wrap-in-parens)`. -/
def obBinOpLean : BinOp → Option (String × Bool)
  | .leq => some ("≤", false) | .lt => some ("<", false)
  | .geq => some ("≥", false) | .gt => some (">", false)
  | .eq  => some ("=", false) | .neq => some ("≠", false)
  | .and_ => some ("∧", true) | .or_ => some ("∨", true)
  | .add => some ("+", true)  | .sub => some ("-", true) | .mul => some ("*", true)
  | .div => some ("/", true)  | .mod => some ("%", true)
  | _ => none

/-- SMT-LIB prefix symbol of a binary operator (`(sym L R)`). `none` for operators
    with no direct prefix form: `neq` is rendered specially as `(not (= L R))`,
    and `/`/`%` are not lowered to SMT here. -/
def obBinOpSmt : BinOp → Option String
  | .leq => some "<=" | .lt => some "<" | .geq => some ">=" | .gt => some ">"
  | .eq  => some "="  | .and_ => some "and" | .or_ => some "or"
  | .add => some "+"  | .sub => some "-" | .mul => some "*"
  | _ => none

/-- Render a binary operator's Lean infix form from the shared table. -/
def leanBinOp (op : BinOp) (L R : String) : Option String :=
  (obBinOpLean op).map fun (sym, paren) =>
    if paren then s!"({L} {sym} {R})" else s!"{L} {sym} {R}"

/-- Lower a contract expression to a Lean `Prop`/`Int` term (`&&`→`∧`, `<=`→`≤`,
    spec-fn calls as applications). `none` if outside the supported subset.
    Division/modulo are excluded here (see `toLeanPropD`, which is sound only
    under `divSound`). -/
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
    | .div | .mod => none
    | _ => leanBinOp op L R
  | _ => none

/-! ### Sound division/modulo lowering (Phase 2 #21)

`toLeanProp` drops any clause containing `/` or `%`, so a block-count summary like
`(len + 9 + 63) / 64 <= max` never becomes a VC. Concrete's `/`/`%` truncate toward
zero (LLVM `sdiv`/`srem`); Lean's `Int` `/`/`%` are E-division (floor), which agree
with Concrete ONLY when the dividend is non-negative. So `toLeanPropD` lowers
division too, but it is SOUND to use only when `divSound` holds — every `/`/`%` has
a provably non-negative dividend and a positive-literal divisor. Otherwise the
clause stays non-lowerable (no VC), never mis-proved. -/

/-- Program variables a hypothesis set proves `≥ 0` (`0 ≤ v` / `0 < v`, recursing
    through conjunctions). Conservative — only used to gate division lowering. -/
partial def nonNegVarsOf : Expr → List String
  | .binOp _ .and_ l r => nonNegVarsOf l ++ nonNegVarsOf r
  | .paren _ e => nonNegVarsOf e
  | .binOp _ .leq lo (.ident _ v) => match cEvalInt lo with | some k => if k ≥ 0 then [v] else [] | _ => []
  | .binOp _ .lt  lo (.ident _ v) => match cEvalInt lo with | some k => if k ≥ 0 then [v] else [] | _ => []
  | _ => []

def nonNegFromHyps (hyps : List Expr) : List String := (hyps.flatMap nonNegVarsOf).eraseDups

/-- `e` is provably `≥ 0` from the non-negative variable set `nn`. -/
partial def exprNonNeg (nn : List String) : Expr → Bool
  | .intLit _ v => v ≥ 0
  | .ident _ n => nn.contains n
  | .paren _ e => exprNonNeg nn e
  | .binOp _ .add l r => exprNonNeg nn l && exprNonNeg nn r
  | .binOp _ .mul l r => exprNonNeg nn l && exprNonNeg nn r
  | .binOp _ .div l r => exprNonNeg nn l && exprNonNeg nn r
  | .binOp _ .mod _ r => exprNonNeg nn r
  | _ => false

/-- Every `/`/`%` in `e` is sound to lower to Lean's E-division: non-negative
    dividend (so toward-zero = floor) and a positive-literal divisor. -/
partial def divSound (nn : List String) : Expr → Bool
  | .binOp _ .div a b => exprNonNeg nn a && (match cEvalInt b with | some k => k > 0 | _ => false) && divSound nn a && divSound nn b
  | .binOp _ .mod a b => exprNonNeg nn a && (match cEvalInt b with | some k => k > 0 | _ => false) && divSound nn a && divSound nn b
  | .binOp _ _ l r => divSound nn l && divSound nn r
  | .unaryOp _ _ e | .paren _ e => divSound nn e
  | _ => true

/-- A clause contains `/` or `%`. -/
partial def exprHasDiv : Expr → Bool
  | .binOp _ op l r => (op == .div || op == .mod) || exprHasDiv l || exprHasDiv r
  | .unaryOp _ _ e | .paren _ e => exprHasDiv e
  | _ => false

/-- `toLeanProp` extended with `/`/`%` (→ Lean E-division). Use ONLY under `divSound`. -/
partial def toLeanPropD : Expr → Option String
  | .intLit _ v => some s!"{v}"
  | .ident _ n => some n
  | .paren _ e => toLeanPropD e
  | .call _ fn _ args => do let as ← args.mapM toLeanPropD; some s!"{fn} {" ".intercalate as}"
  | .binOp _ op l r => do
    let L ← toLeanPropD l
    let R ← toLeanPropD r
    leanBinOp op L R
  | _ => none

/-- Lower an assert/contract clause, soundly handling division: a clause with
    `/`/`%` is lowered (E-division) only when `divSound nn` holds; otherwise it is
    left non-lowerable. Division-free clauses go through `toLeanProp` unchanged. -/
def toLeanPropSound (nn : List String) (e : Expr) : Option String :=
  if exprHasDiv e then (if divSound nn e then toLeanPropD e else none) else toLeanProp e

/-- Omega goals that witness an UNSATISFIABLE contract (the vacuity risk): for a
    function's `#[requires]` conjunction and each loop `#[invariant]`,
    `∀ vars, ¬(P)`. If omega proves it, no input satisfies P, so the contract is
    vacuous — any postcondition holds trivially and must be reported `vacuous`,
    never `proved`. (The constant-false case, e.g. `#[requires(false)]`, is caught
    separately by the `cEvalBool` folder; this tier catches symbolic
    contradictions like `x > 0 && x < 0`.) -/
def vacuityGoals (modules : List Module) : List (String × String) := Id.run do
  let mut goals : List (String × String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    if !f.requires.isEmpty then
      let props := f.requires.filterMap toLeanProp
      if props.length == f.requires.length then
        let vars := (f.requires.flatMap collectIdents).eraseDups
        let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
        let conj := " ∧ ".intercalate (props.map (fun p => s!"({p})"))
        goals := goals ++ [(s!"{fq}#requires_vac", s!"{binder}¬({conj})")]
    for lc in f.loopContracts do
      for (i, inv) in (List.range lc.invariants.length).zip lc.invariants do
        match toLeanProp inv with
        | some p =>
          let vars := (collectIdents inv).eraseDups
          let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
          goals := goals ++ [(s!"{fq}@{lc.line}#inv_vac{i}", s!"{binder}¬({p})")]
        | none => pure ()
  return goals

/-- `(isAssume, condition)` for every `assert`/`assume` in a statement body
    (recursing into nested blocks). `assert` claims the condition; `assume`
    trusts it. -/
partial def collectAssertAssumeS : Stmt → List (Bool × Expr)
  | .assert_ _ c => [(false, c)]
  | .assume_ _ c => [(true, c)]
  | .ifElse _ _ t el => t.flatMap collectAssertAssumeS ++ (el.getD []).flatMap collectAssertAssumeS
  | .while_ _ _ b _ => b.flatMap collectAssertAssumeS
  | .forLoop _ init _ step b _ =>
      (init.map collectAssertAssumeS).getD [] ++ (step.map collectAssertAssumeS).getD [] ++ b.flatMap collectAssertAssumeS
  | .borrowIn _ _ _ _ _ b => b.flatMap collectAssertAssumeS
  | _ => []

/-- Negate a guard, staying inside the lowerable fragment by flipping the
    comparison (and De Morgan over ∧/∨) rather than introducing `¬`. `none` when
    the guard isn't a (combination of) comparisons — then it's simply dropped from
    scope, which is sound (fewer hypotheses). Lets the fall-through of `if c { … }`
    carry `¬c`. -/
partial def negateGuard : Expr → Option Expr
  | .paren _ e => negateGuard e
  | .unaryOp _ .not_ e => some e
  | .binOp sp op l r =>
    match op with
    | .lt  => some (.binOp sp .geq l r) | .leq => some (.binOp sp .gt  l r)
    | .gt  => some (.binOp sp .leq l r) | .geq => some (.binOp sp .lt  l r)
    | .eq  => some (.binOp sp .neq l r) | .neq => some (.binOp sp .eq  l r)
    | .and_ => do let nl ← negateGuard l; let nr ← negateGuard r; some (.binOp sp .or_  nl nr)
    | .or_  => do let nl ← negateGuard l; let nr ← negateGuard r; some (.binOp sp .and_ nl nr)
    | _ => none
  | _ => none

mutual
/-- A statement transfers control out of the enclosing block on every path
    (so the statements after it run only when an earlier guard was false). -/
partial def stmtTerminates : Stmt → Bool
  | .return_ _ _ | .break_ _ _ _ | .continue_ _ _ => true
  | .ifElse _ _ t (some el) => blockTerminates t && blockTerminates el
  | _ => false
partial def blockTerminates : List Stmt → Bool
  | [] => false
  | [s] => stmtTerminates s
  | _ :: rest => blockTerminates rest
end

/-- Render the `assert`/`assume` section: each `assert` is an obligation
    (proved_by_kernel_decision when omega closed its key, a VIOLATION when it
    folds to false, else unproven); each `assume` is `assumed` (trust, not
    proof). A function that contains any `assume` is flagged TAINTED — its
    evidence is not a clean proof. -/
def renderAssertAssume (modules : List Module) (provedAsserts : List String) : String := Id.run do
  let withAA := (modules.flatMap allFunctions).filterMap fun (pfx, f) =>
    let aa := f.body.flatMap collectAssertAssumeS
    if aa.isEmpty then none else some (pfx ++ f.name, aa)
  if withAA.isEmpty then return ""
  let mut out := "\n\n=== assert / assume ==="
  for (fq, aa) in withAA do
    out := out ++ s!"\n\n{fq}"
    if aa.any (·.1) then
      out := out ++ "\n  ⚠ TAINTED — depends on assume(...): evidence is `assumed` (trust), NOT a clean proof"
    let mut i := 0
    for (isAssume, cond) in aa do
      if isAssume then
        out := out ++ s!"\n  assume {Concrete.fmtExpr cond}\n     status:  assumed (trust, not proof — audit-visible; release policy may forbid)"
      else
        let st :=
          if cEvalBool cond == some false then "VIOLATION: assert is always false"
          else if provedAsserts.contains s!"{fq}#aa{i}" then "proved_by_kernel_decision (omega)"
          else "unproven (assert not discharged — never silently accepted)"
        out := out ++ s!"\n  assert {Concrete.fmtExpr cond}\n     status:  {st}"
      i := i + 1
  return out ++ "\n"

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

/-- The **arithmetic** half of invariant_preservation as a single-line Lean
    goal: `∀ vars, (invariant) → guard → (invariant')`. This is the part a
    kernel decision procedure (`omega`) can close directly — it says the
    invariant is inductive as arithmetic, independent of how the loop body is
    realized. The remaining *operational* half (the extracted body actually
    performs the substitution) is `genPreservationShape` and still needs Lean. -/
def genPreservationGoal (lc : LoopContract) (outer : List Expr := []) : Option String := do
  let guard ← lc.guard
  if lc.invariants.isEmpty then failure
  let invs' := lc.invariants.map (substContract lc.body)
  let outerStrs := outer.filterMap toLeanProp
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents guard
                ++ outer.flatMap collectIdents
                ++ lc.body.flatMap (fun (_, e) => collectIdents e)).eraseDups
  let invStrs ← lc.invariants.mapM toLeanProp
  let guardStr ← toLeanProp guard
  let inv'Strs ← invs'.mapM toLeanProp
  let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
  some s!"{binder}({" ∧ ".intercalate (outerStrs ++ invStrs)}) → {guardStr} → ({" ∧ ".intercalate inv'Strs})"

/-- The **operational** half of invariant_preservation as a Lean theorem
    *shape* (not a claim): the extracted loop body, evaluated, yields the
    substituted state, and the invariant is preserved. This is what still needs
    Lean (a `simp` over `evalAssigns`, as in `count_up_loop_preserves`); the
    compiler points to the shape so the proof author knows exactly what to
    write. `none` if the body has no lowerable assignments. -/
def genPreservationShape (lc : LoopContract) (fnQual : String) : Option String := do
  if lc.body.isEmpty then failure
  let name := (fnQual.replace "." "_") ++ "_loop_preserves"
  let stepDesc := "; ".intercalate (lc.body.map (fun (n, e) => s!"{n} := {Concrete.fmtExpr e}"))
  let invDesc := " ∧ ".intercalate (lc.invariants.map Concrete.fmtExpr)
  some <| String.join [
    s!"theorem {name} (fns : FnTable) (env : Env) (fuel : Nat) … :",
    s!"\n             eval.evalAssigns fns env fuel <body> = some <env after [ {stepDesc} ]>",
    s!"\n             ∧ ({invDesc})      -- arithmetic half above is omega-discharged" ]

/-- invariant_init VC: the invariant holds in the loop-entry state (the for-init
    and preceding let-constants substituted). -/
def genInitVC (lc : LoopContract) (extraLets : List (String × Expr))
    (outer : List Expr := []) : Option String := do
  if lc.invariants.isEmpty then failure
  let inits := lc.invariants.map (substContract (lc.entrySubst ++ extraLets))
  let outerStrs := outer.filterMap toLeanProp
  let vars := (inits.flatMap collectIdents ++ outer.flatMap collectIdents).eraseDups
  let strs ← inits.mapM toLeanProp
  let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
  let hyp := if outerStrs.isEmpty then "" else s!"({" ∧ ".intercalate outerStrs}) → "
  some s!"{binder}{hyp}{" ∧ ".intercalate strs}"

/-- variant_nonnegative VC: invariant ∧ guard → 0 ≤ variant. -/
def genVariantNonneg (lc : LoopContract) (outer : List Expr := []) : Option String := do
  let v ← lc.variant
  let g ← lc.guard
  if lc.invariants.isEmpty then failure
  let outerStrs := outer.filterMap toLeanProp
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents g ++ collectIdents v
                ++ outer.flatMap collectIdents).eraseDups
  let invs ← lc.invariants.mapM toLeanProp
  let gs ← toLeanProp g
  let vs ← toLeanProp v
  some s!"∀ ({" ".intercalate vars} : Int), {" ∧ ".intercalate (outerStrs ++ invs)} → {gs} → 0 ≤ {vs}"

/-- variant_decreases VC: invariant ∧ guard → variant[body] < variant. -/
def genVariantDecreases (lc : LoopContract) (outer : List Expr := []) : Option String := do
  let v ← lc.variant
  let g ← lc.guard
  if lc.invariants.isEmpty then failure
  let v' := substContract lc.body v
  let outerStrs := outer.filterMap toLeanProp
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents g ++ collectIdents v
                ++ outer.flatMap collectIdents).eraseDups
  let invs ← lc.invariants.mapM toLeanProp
  let gs ← toLeanProp g
  let vs ← toLeanProp v
  let vs' ← toLeanProp v'
  some s!"∀ ({" ".intercalate vars} : Int), {" ∧ ".intercalate (outerStrs ++ invs)} → {gs} → {vs'} < {vs}"

/-- The function's loop-exit return expression, when the loop is immediately
    followed by a single `return e`. In that shape the loop-exit state IS the
    returned state (no intervening mutation), so substituting `result := e` into
    `#[ensures]` is sound. `none` otherwise — keeps O3 conservative rather than
    claiming a post that post-loop code could invalidate. -/
def loopExitReturn (body : List Stmt) : Option Expr :=
  match body.reverse with
  | (.return_ _ (some e)) :: prev :: _ =>
    match prev with
    | .forLoop .. | .while_ .. => some e
    | _ => none
  | _ => none

/-- **exit_implies_post VC (O3)**: the loop's exit facts discharge the function
    postcondition. The exit hypothesis is `invariant ∧ ¬guard`; the goal is the
    `#[ensures]` with `result` replaced by the loop-exit return expression.
    States `∀ vars, invariant ∧ ¬guard → ensures'`. `none` when there is no
    ensures, no clean loop-exit return, or a `result` we cannot ground (so the
    bridge stays honest). -/
def genExitVC (lc : LoopContract) (ensures : List Expr) (retExpr : Option Expr)
    (outer : List Expr := []) : Option String := do
  let g ← lc.guard
  if lc.invariants.isEmpty || ensures.isEmpty then failure
  let subst := match retExpr with | some e => [("result", e)] | none => []
  let posts := ensures.map (substContract subst)
  -- a still-free `result` means we could not ground the postcondition → bail
  if posts.any (fun e => (collectIdents e).contains "result") then failure
  let outerStrs := outer.filterMap toLeanProp
  let invs ← lc.invariants.mapM toLeanProp
  let gs ← toLeanProp g
  let postStrs ← posts.mapM toLeanProp
  let vars := (lc.invariants.flatMap collectIdents ++ collectIdents g
                ++ outer.flatMap collectIdents ++ posts.flatMap collectIdents).eraseDups
  let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
  some s!"{binder}{" ∧ ".intercalate (outerStrs ++ invs)} ∧ ¬({gs}) → {" ∧ ".intercalate postStrs}"

-- ============================================================

end Report
end Concrete

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
    | .expr _ e _ => findCallSpanInExpr targets e
    | .letDecl _ _ _ _ e _ => findCallSpanInExpr targets e
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

private def arithIsIntTy : Ty → Bool
  | .int | .uint | .i8 | .i16 | .i32 | .u8 | .u16 | .u32 => true
  | _ => false

private def arithIsFloatTy : Ty → Bool
  | .float32 | .float64 => true
  | _ => false

/-- One arithmetic site: the operation's source label and its class. -/
abbrev ArithSite := String × ArithClass

/-- Classify a binary operator at a given operand type, or `none` if it is not
    an overflow-relevant arithmetic site (comparisons, logical, and pure bitwise
    `& | ^` are total and not classified; float `+ - * /` use IEEE semantics, not
    the checked-integer trap, so they are excluded too). -/
private def classifyArithBinOp (op : BinOp) (operandTy : Ty) : Option ArithSite :=
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
private partial def arithSitesE : CExpr → List ArithSite
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

private partial def arithSitesArm : CMatchArm → List ArithSite
  | .enumArm _ _ _ guard body =>
      (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten
  | .litArm value guard body =>
      arithSitesE value ++ (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten
  | .varArm _ _ guard body =>
      (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten
  | .rangeArm lo hi _ guard body =>
      arithSitesE lo ++ arithSitesE hi ++ (match guard with | some g => arithSitesE g | none => []) ++ (body.map arithSitesS).flatten

private partial def arithSitesS : CStmt → List ArithSite
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

private structure ArithFnRow where
  qualName : String
  loc : Option SourceLoc
  sites : List ArithSite

private partial def arithRowsForModule (locMap : FnLocMap) (m : CModule)
    (modulePath : String := "") : List ArithFnRow :=
  let qualPrefix := if modulePath == "" then m.name else modulePath ++ "." ++ m.name
  let rows : List ArithFnRow := m.functions.map fun f =>
    let qualName := qualPrefix ++ "." ++ f.name
    { qualName := qualName, loc := lookupLoc locMap qualName,
      sites := (f.body.map arithSitesS).flatten }
  rows ++ m.submodules.foldl (fun acc sub => acc ++ arithRowsForModule locMap sub qualPrefix) []

private def countClass (sites : List ArithSite) (c : ArithClass) : Nat :=
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
-- Runtime-safety obligations: array bounds
-- ============================================================
-- A runtime-error class. Every `arr[idx]` into a fixed-size array generates the
-- obligation `0 ≤ idx < N`. Constant indices are evaluated (in-bounds / VIOLATION);
-- variable indices are discharged by `omega` under the function's #[requires]
-- (a kernel decision procedure — statically in bounds, no runtime check needed),
-- or left `unproven` (needs a precondition or a runtime check). This is the
-- runtime_checked evidence class.

mutual
/-- `(arrayName, indexExpr)` for every `arr[idx]` with an identifier base. -/
partial def collectIndexUsesE : Expr → List (String × Expr)
  | .arrayIndex _ (.ident _ arr) idx => (arr, idx) :: collectIndexUsesE idx
  | .arrayIndex _ a idx => collectIndexUsesE a ++ collectIndexUsesE idx
  | .binOp _ _ l r => collectIndexUsesE l ++ collectIndexUsesE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ | .arrowAccess _ x _ => collectIndexUsesE x
  | .arrayLit _ es => es.flatMap collectIndexUsesE
  | .call _ _ _ args => args.flatMap collectIndexUsesE
  | .methodCall _ o _ _ args => collectIndexUsesE o ++ args.flatMap collectIndexUsesE
  | .staticMethodCall _ _ _ _ args => args.flatMap collectIndexUsesE
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectIndexUsesE fe) ++ (base.map collectIndexUsesE).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectIndexUsesE fe)
  | .allocCall _ x a => collectIndexUsesE x ++ collectIndexUsesE a
  | .ifExpr _ c t el | .whileExpr _ c t el =>
      collectIndexUsesE c ++ t.flatMap collectIndexUsesS ++ el.flatMap collectIndexUsesS
  | .match_ _ s _ => collectIndexUsesE s
  | _ => []
partial def collectIndexUsesS : Stmt → List (String × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectIndexUsesE v
  | .return_ _ (some v) => collectIndexUsesE v
  | .ifElse _ c t el => collectIndexUsesE c ++ t.flatMap collectIndexUsesS ++ (el.getD []).flatMap collectIndexUsesS
  | .while_ _ c b _ => collectIndexUsesE c ++ b.flatMap collectIndexUsesS
  | .forLoop _ init c step b _ =>
      (init.map collectIndexUsesS).getD [] ++ collectIndexUsesE c
        ++ (step.map collectIndexUsesS).getD [] ++ b.flatMap collectIndexUsesS
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v => collectIndexUsesE o ++ collectIndexUsesE v
  | .arrayIndexAssign _ (.ident _ arr) idx v => (arr, idx) :: (collectIndexUsesE idx ++ collectIndexUsesE v)
  | .arrayIndexAssign _ a i v => collectIndexUsesE a ++ collectIndexUsesE i ++ collectIndexUsesE v
  | _ => []
end

-- ============================================================
-- Loop-invariant scope for runtime-safety obligations
-- ============================================================
-- A runtime-safety obligation that occurs inside a loop body may ASSUME the
-- loop's invariant and guard: at the top of the body the invariant holds and
-- the guard was just taken. Feeding those facts to omega lets a body access
-- like `a[i]` discharge from `#[invariant(0 <= i && i <= N)]` + guard `i < N`,
-- instead of demanding a `#[requires]`. SOUNDNESS: the invariant only provably
-- holds until the body mutates a variable it mentions, so the ordered walk
-- below DROPS a hypothesis as soon as a statement assigns to one of its
-- variables (array-element / field / deref stores touch no integer counter, so
-- they invalidate nothing; the canonical `a[i] = …; i = i + 1` therefore keeps
-- the bound at the access and loses it only for statements after the `i = …`).

/-- Loop invariants + guard for the loop whose statement begins on `line`
    (matched against `FnDef.loopContracts` by source line). The facts assumable
    for an obligation in that loop's body. -/
def loopHypsAt (lcs : List LoopContract) (line : Nat) : List Expr :=
  match lcs.find? (·.line == line) with
  | some lc => lc.invariants ++ lc.guard.toList
  | none    => []

/-- Scalar variables a statement assigns to. Mutating one invalidates any
    in-scope hypothesis that mentions it. Array-element / field / deref stores
    assign no integer counter (the domain our invariants range over), so they
    return `[]`; a nested loop invalidates whatever its body assigns. -/
partial def assignedScalarsS : Stmt → List String
  | .assign _ n _ => [n]
  | .letDecl _ n _ _ _ _ => [n]
  | .ifElse _ _ t el => t.flatMap assignedScalarsS ++ (el.getD []).flatMap assignedScalarsS
  | .while_ _ _ b _ => b.flatMap assignedScalarsS
  | .forLoop _ init _ step b _ =>
      (init.map assignedScalarsS).getD [] ++ (step.map assignedScalarsS).getD []
        ++ b.flatMap assignedScalarsS
  | _ => []

/-- Drop every in-scope hypothesis that mentions a just-assigned variable. -/
def dropStaleHyps (scope : List Expr) (assigned : List String) : List Expr :=
  scope.filter fun h => (collectIdents h).all (fun v => !assigned.contains v)

-- ────────────────────────────────────────────────────────────────────────────
-- The ONE scoped context collector (ROADMAP Phase 3 #3).
--
-- Shared engine for every scoped obligation family (call-site preconditions,
-- array bounds, div/mod, overflow, asserts, …). Migrated families walk the body
-- once with the SAME scope-threading discipline, parameterised only by a
-- per-statement `leaf` extractor. The walker threads the full fact set the
-- roadmap requires:
--   • enclosing `if`-guards          → the then-branch assumes `c`,
--   • negated guards                 → the else-branch assumes `¬c`,
--   • early-return fall-through       → after `if c { …return… }`, assume `¬c`,
--   • loop invariants + guards        → the body assumes `loopHypsAt`,
--   • one shared invalidation rule    → `dropStaleHyps` after each assignment.
-- `leaf scope s` sees the hypotheses in scope at `s` and returns this statement's
-- own (non-recursive) obligations; the walker owns ALL recursion into branches,
-- loop bodies, and for-loop init/step, so no family can drift in how it threads
-- scope. Migrated families instantiate this with their own leaf (Phase 3 #4-9).
mutual
partial def scopedWalkS {α} (leaf : List Expr → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) : Stmt → List α
  | s@(.ifElse _ c t el) =>
      leaf scope s
        ++ scopedWalkB leaf lcs (scope ++ [c]) t
        ++ scopedWalkB leaf lcs (scope ++ (negateGuard c).toList) (el.getD [])
  | s@(.while_ sp _ b _) =>
      leaf scope s ++ scopedWalkB leaf lcs (scope ++ loopHypsAt lcs sp.line) b
  | s@(.forLoop sp init _ step b _) =>
      -- init, then this statement's own leaves (the loop condition), then step,
      -- then body — the traversal ORDER every family's old walker used, so the
      -- positional `#idx`/`#pre`/… keys are preserved across the migration.
      ((init.map (scopedWalkS leaf lcs scope)).getD [])
        ++ leaf scope s
        ++ ((step.map (scopedWalkS leaf lcs scope)).getD [])
        ++ scopedWalkB leaf lcs (scope ++ loopHypsAt lcs sp.line) b
  | s@(.borrowIn _ _ _ _ _ b) => leaf scope s ++ scopedWalkB leaf lcs scope b
  | s => leaf scope s
partial def scopedWalkB {α} (leaf : List Expr → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) : List Stmt → List α
  | [] => []
  | s :: rest =>
      let restScope := match s with
        | .ifElse _ c t none => if blockTerminates t then scope ++ (negateGuard c).toList
                                else dropStaleHyps scope (assignedScalarsS s)
        | _ => dropStaleHyps scope (assignedScalarsS s)
      scopedWalkS leaf lcs scope s ++ scopedWalkB leaf lcs restScope rest
end

/-- Array-index leaf: the index uses in a statement's OWN expression positions
    (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's index uses).
    A store `a[idx] = v` carries its target bound `(a, idx)` FIRST, matching the
    old walker's ordering exactly. -/
def boundsLeaf (scope : List Expr) : Stmt → List (String × Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .return_ _ (some v) => (collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .ifElse _ c _ _ => (collectIndexUsesE c).map fun (a, i) => (a, i, scope)
  | .while_ _ c _ _ => (collectIndexUsesE c).map fun (a, i) => (a, i, scope)
  | .forLoop _ _ c _ _ _ => (collectIndexUsesE c).map fun (a, i) => (a, i, scope)
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v =>
      (collectIndexUsesE o ++ collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .arrayIndexAssign _ (.ident _ arr) idx v =>
      (arr, idx, scope) :: (collectIndexUsesE idx ++ collectIndexUsesE v).map fun (a, i) => (a, i, scope)
  | .arrayIndexAssign _ a i v =>
      (collectIndexUsesE a ++ collectIndexUsesE i ++ collectIndexUsesE v).map fun (x, j) => (x, j, scope)
  | _ => []

/-- Index uses paired with the hypotheses in scope at the access (Phase 3 #5 —
    migrated onto the unified `scopedWalk`). Like the call-site migration, the
    collector now threads enclosing `if`-guards (then assumes `c`, else assumes
    `¬c`), early-return fall-through, and loop invariants/guards — strictly more
    sound context than the old bounds walker, so a bounds obligation can only move
    `unproven → proved_by_kernel_decision` (e.g. `if 0 ≤ i && i < n { a[i] }`),
    never the reverse, and a reassigned index still drops its stale guard. -/
def scopedBoundsB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (String × Expr × List Expr) :=
  scopedWalkB boundsLeaf lcs scope body

/-- Call-site leaf: the calls in a statement's OWN expression positions (the
    walker owns recursion into branches, loop bodies, and for-loop init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's calls). -/
def callLeaf (scope : List Expr) : Stmt → List (String × List Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | .return_ _ (some v) => (collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | .ifElse _ c _ _ => (collectCallsE c).map fun (_, fn, args) => (fn, args, scope)
  | .while_ _ c _ _ => (collectCallsE c).map fun (_, fn, args) => (fn, args, scope)
  | .forLoop _ _ c _ _ _ => (collectCallsE c).map fun (_, fn, args) => (fn, args, scope)
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v =>
      (collectCallsE o ++ collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | .arrayIndexAssign _ a i v =>
      (collectCallsE a ++ collectCallsE i ++ collectCallsE v).map fun (_, fn, args) => (fn, args, scope)
  | _ => []

/-- Calls paired with the hypotheses in scope at the call (Phase 3 #4 — migrated
    onto the unified `scopedWalk`). The collector threads the FULL fact set:
    enclosing `if`-guards (then assumes `c`, else assumes `¬c`), early-return
    fall-through (`¬c` after `if c { …return… }`), loop invariants/guards, and one
    shared stale-hypothesis rule. This is strictly more (sound) context than the
    old call walker threaded, so a call precondition can only move `unproven →
    proved`, never the reverse; it also closes the old gap of skipping calls
    inside `borrow … in { }` bodies. -/
def scopedCallsB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (String × List Expr × List Expr) :=
  scopedWalkB callLeaf lcs scope body

/-- `assert`/`assume` leaf: the only own-obligation statements are `assert`/
    `assume` themselves; everything else is pure recursion the walker owns. -/
def assertLeaf (scope : List Expr) : Stmt → List (Bool × Expr × List Expr)
  | .assert_ _ c => [(false, c, scope)]
  | .assume_ _ c => [(true, c, scope)]
  | _ => []

/-- `assert`/`assume` with the path conditions in scope at each one (the first
    family on the unified collector — Phase 3 #3). Enclosing `if`-guards (the
    guard on the then-branch, its negation on the else-branch), loop invariants in
    the body, and `¬c` for the fall-through of an early-return `if c { …return… }`.
    Mirrors `collectAssertAssumeS`'s traversal ORDER exactly, so the `i`-th item
    keeps the same `#aa<i>` key the renderer uses. -/
def scopedAssertsB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (Bool × Expr × List Expr) :=
  scopedWalkB assertLeaf lcs scope body

/-- Omega goals for `assert(e)` obligations: `∀ vars, (path conditions) → (e)`.
    The hypotheses are the function's `#[requires]` PLUS the path conditions in
    scope at the assert (if-guards, negated guards, loop invariants) — threaded
    like the call-site/bounds/div VCs. Discharged by the same omega backend; a
    success means the assert holds. `assume` produces no goal (trusted, not
    proved). Keyed `<fq>#aa<i>` by position in the assert/assume stream, matching
    `renderAssertAssume`. -/
def assertGoals (modules : List Module) : List (String × String) := Id.run do
  let mut goals : List (String × String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isAssume, cond, scope) in scopedAssertsB f.loopContracts [] f.body do
      if !isAssume then
        let hyps := f.requires ++ scope
        let nn := nonNegFromHyps hyps
        match toLeanPropSound nn cond with
        | some p =>
          let hypProps := hyps.filterMap (toLeanPropSound nn)
          let vars := (collectIdents cond ++ hyps.flatMap collectIdents).eraseDups
          let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
          let hyp := if hypProps.isEmpty then "" else s!"({" ∧ ".intercalate (hypProps.map (fun q => s!"({q})"))}) → "
          goals := goals ++ [(s!"{fq}#aa{i}", s!"{binder}{hyp}({p})")]
        | none => pure ()
      i := i + 1
  return goals

/-- `assume(e)` facts with the path conditions in scope (Phase 3 #8). An `assume`
    is NOT an obligation to discharge — it is a trusted assumption fact that the
    audit ledger must surface loudly: it carries status `assumed` (never a proof),
    is a policy input (`forbid-assume`, E0614), and crucially produces no goal, so
    it can never launder trust into kernel evidence for a later assert. Keyed
    `<fq>#aa<i>` by the SAME position scheme as `assertGoals` (assume and assert
    occupy disjoint positions in the shared stream). -/
def assumeFacts (modules : List Module) : List (String × String × List String) := Id.run do
  let mut facts : List (String × String × List String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isAssume, cond, scope) in scopedAssertsB f.loopContracts [] f.body do
      if isAssume then
        let hyps := f.requires ++ scope
        let nn := nonNegFromHyps hyps
        let concl := (toLeanPropSound nn cond).getD (Concrete.fmtExpr cond)
        facts := facts ++ [(s!"{fq}#aa{i}", concl, hyps.filterMap (toLeanPropSound nn))]
      i := i + 1
  return facts

/-- Build the ordered list of call-site obligations across all callers. The fast
    constant folder classifies the literal/arithmetic cases; an obligation that
    stays non-constant carries a `bv_decide` `leanGoal` (when closed after
    let-const subst) and the `hyps` in scope at the call so `callPrecondGoals`
    can try to discharge it with `omega` from the caller's `#[requires]` /
    enclosing guards / loop invariants. -/
def callSiteObligations (modules : List Module) : List CallObligation := Id.run do
  let fns := modules.flatMap allFunctions
  let reqMap : List (String × (List Param × List Expr)) :=
    fns.filterMap (fun (_, f) => if f.requires.isEmpty then none else some (f.name, (f.params, f.requires)))
  if reqMap.isEmpty then return []
  let mut obs : List CallObligation := []
  let mut gi := 0
  for (pfx, f) in fns do
    let lets := letConstMap f.body
    for (fn, args, scope) in scopedCallsB f.loopContracts [] f.body do
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
              let spec2 := substContract lets spec
              match cEvalBool spec2 with
              | some false => ("failed_at_callsite", none)
              | _ => if isClosed spec2 then ("unproven", toLeanBV spec2)
                     else ("unproven", none)
          obs := obs ++ [{ caller := pfx ++ f.name, callStr, specExpr := spec, baseStatus, leanGoal
                         , key := s!"{pfx ++ f.name}#pre{gi}", hyps := f.requires ++ scope }]
          gi := gi + 1
  return obs

/-- Omega goals for call-site preconditions that are not constant-decidable:
    `∀ (vars : Int), (caller hyps) → precondition`. Discharged by the same
    `omega` backend as the bounds/div goals; a success means the caller's
    `#[requires]` / guards / loop invariants establish the callee's precondition. -/
def callPrecondGoals (modules : List Module) : List (String × String) :=
  (callSiteObligations modules).filterMap fun o =>
    if o.baseStatus != "unproven" then none
    else match toLeanProp o.specExpr with
      | none => none
      | some specStr =>
        let vars := (collectIdents o.specExpr ++ o.hyps.flatMap collectIdents).eraseDups
        let reqs := o.hyps.filterMap toLeanProp
        let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
        let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
        some (o.key, s!"{binder}{hyp}({specStr})")

/-- One array-bounds obligation. `closedVerdict` is set when the index is a
    compile-time constant; otherwise `leanGoal` is the omega goal (if lowerable). -/
structure BoundsObl where
  fnQual        : String
  key           : String
  arrName       : String
  idxExpr       : Expr
  size          : Nat
  closedVerdict : Option Bool
  leanGoal      : Option String

/-- Identifier → fixed-array size, from array-typed params and annotated lets. -/
def arraySizeMap (f : FnDef) : List (String × Nat) :=
  let ps := f.params.filterMap fun p => match p.ty with | .array _ n => some (p.name, n) | _ => none
  let ls := f.body.filterMap fun s => match s with
    | .letDecl _ nm _ (some (.array _ n)) _ _ => some (nm, n) | _ => none
  ps ++ ls

/-- Generate array-bounds obligations for every indexed access into a known
    fixed-size array. -/
def boundsObligations (modules : List Module) : List BoundsObl := Id.run do
  let mut out : List BoundsObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let sizes := arraySizeMap f
    let mut i := 0
    for (arr, idx, scope) in scopedBoundsB f.loopContracts [] f.body do
      match sizes.find? (·.1 == arr) with
      | none => pure ()
      | some (_, n) =>
        let key := s!"{fq}#bounds{i}"
        let cv : Option Bool × Option String := match cEvalInt idx with
          | some k => (some (decide (0 ≤ k) && decide (k < (Int.ofNat n))), none)
          | none => match toLeanProp idx with
            | none => (none, none)
            | some idxStr =>
              let hyps := f.requires ++ scope
              let vars := (collectIdents idx ++ hyps.flatMap collectIdents).eraseDups
              let reqs := hyps.filterMap toLeanProp
              let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
              let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
              (none, some s!"{binder}{hyp}(0 ≤ {idxStr} ∧ {idxStr} < {n})")
        out := out ++ [{ fnQual := fq, key, arrName := arr, idxExpr := idx, size := n
                        , closedVerdict := cv.1, leanGoal := cv.2 }]
        i := i + 1
  return out

/-- Lean goals for the non-constant bounds obligations, for omega discharge. -/
def boundsGoals (modules : List Module) : List (String × String) :=
  (boundsObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Render the array-bounds section. `provedKeys` are the omega-discharged keys. -/
def renderBounds (obls : List BoundsObl) (provedKeys : List String) : String := Id.run do
  if obls.isEmpty then return ""
  let mut out := "\n\n=== Runtime-safety obligations (array bounds) ==="
  let mut cur := ""
  for o in obls do
    if o.fnQual != cur then out := out ++ s!"\n\n{o.fnQual}"; cur := o.fnQual
    let status := match o.closedVerdict with
      | some true  => "checked: in bounds (constant index)"
      | some false => "VIOLATION: index out of bounds (constant index)"
      | none => match o.leanGoal with
        | some _ =>
          if provedKeys.contains o.key
          then "proved_by_kernel_decision (omega) — statically in bounds, no runtime check needed"
          else "unproven — bound the index with a #[requires], or insert a runtime check"
        | none => "unproven — index not statically analyzable; needs a runtime check"
    out := out ++ s!"\n  {o.arrName}[{Concrete.fmtExpr o.idxExpr}]  (array size {o.size})\n    status: {status}"
  return out ++ "\n"

-- ============================================================
-- Runtime-safety obligations: division by zero
-- ============================================================
-- Every `/` and `%` generates the obligation `divisor ≠ 0`. Same shape as array
-- bounds: constant divisors are evaluated; variable divisors discharge by omega
-- under the function's #[requires] (statically nonzero), or stay `unproven`.

mutual
/-- `(isMod, divisorExpr)` for every `/` and `%` in an expression. -/
partial def collectDivisorsE : Expr → List (Bool × Expr)
  | .binOp _ .div l r => (false, r) :: (collectDivisorsE l ++ collectDivisorsE r)
  | .binOp _ .mod l r => (true, r) :: (collectDivisorsE l ++ collectDivisorsE r)
  | .binOp _ _ l r => collectDivisorsE l ++ collectDivisorsE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ | .arrowAccess _ x _ => collectDivisorsE x
  | .arrayLit _ es => es.flatMap collectDivisorsE
  | .arrayIndex _ a i => collectDivisorsE a ++ collectDivisorsE i
  | .call _ _ _ args => args.flatMap collectDivisorsE
  | .methodCall _ o _ _ args => collectDivisorsE o ++ args.flatMap collectDivisorsE
  | .staticMethodCall _ _ _ _ args => args.flatMap collectDivisorsE
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectDivisorsE fe) ++ (base.map collectDivisorsE).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectDivisorsE fe)
  | .allocCall _ x a => collectDivisorsE x ++ collectDivisorsE a
  | .ifExpr _ c t el | .whileExpr _ c t el =>
      collectDivisorsE c ++ t.flatMap collectDivisorsS ++ el.flatMap collectDivisorsS
  | .match_ _ s _ => collectDivisorsE s
  | _ => []
partial def collectDivisorsS : Stmt → List (Bool × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectDivisorsE v
  | .return_ _ (some v) => collectDivisorsE v
  | .ifElse _ c t el => collectDivisorsE c ++ t.flatMap collectDivisorsS ++ (el.getD []).flatMap collectDivisorsS
  | .while_ _ c b _ => collectDivisorsE c ++ b.flatMap collectDivisorsS
  | .forLoop _ init c step b _ =>
      (init.map collectDivisorsS).getD [] ++ collectDivisorsE c
        ++ (step.map collectDivisorsS).getD [] ++ b.flatMap collectDivisorsS
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v => collectDivisorsE o ++ collectDivisorsE v
  | .arrayIndexAssign _ a i v => collectDivisorsE a ++ collectDivisorsE i ++ collectDivisorsE v
  | _ => []
end

/-- Divisor leaf: the `/`/`%` divisors in a statement's OWN expression positions
    (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's divisors).
    Each item is `(isMod, divisorExpr, scope)`. -/
def divLeaf (scope : List Expr) : Stmt → List (Bool × Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | .return_ _ (some v) => (collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | .ifElse _ c _ _ => (collectDivisorsE c).map fun (m, e) => (m, e, scope)
  | .while_ _ c _ _ => (collectDivisorsE c).map fun (m, e) => (m, e, scope)
  | .forLoop _ _ c _ _ _ => (collectDivisorsE c).map fun (m, e) => (m, e, scope)
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v =>
      (collectDivisorsE o ++ collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | .arrayIndexAssign _ a i v =>
      (collectDivisorsE a ++ collectDivisorsE i ++ collectDivisorsE v).map fun (m, e) => (m, e, scope)
  | _ => []

/-- Divisor uses paired with the hypotheses in scope at the `/`/`%` (Phase 3 #6 —
    migrated onto the unified `scopedWalk`). The collector threads enclosing
    guards / negated guards / fall-through / loop invariants, so a `divisor ≠ 0`
    obligation can only move `unproven → proved` (e.g. `if d != 0 { n / d }`),
    never the reverse. The SOUND division/modulo lowering is unchanged: it still
    flows through `divSound`/`toLeanPropSound`, which lower `/`/`%` to Lean
    E-division ONLY when the dividend is provably non-negative — keeping Concrete's
    truncating semantics from being confused with Lean's floor division. -/
def scopedDivB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (Bool × Expr × List Expr) :=
  scopedWalkB divLeaf lcs scope body

/-- One division-by-zero obligation. -/
structure DivObl where
  fnQual        : String
  key           : String
  divExpr       : Expr
  isMod         : Bool
  closedVerdict : Option Bool
  leanGoal      : Option String

/-- Generate `divisor ≠ 0` obligations for every `/` and `%`. -/
def divObligations (modules : List Module) : List DivObl := Id.run do
  let mut out : List DivObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isMod, dv, scope) in scopedDivB f.loopContracts [] f.body do
      let key := s!"{fq}#div{i}"
      let cv : Option Bool × Option String := match cEvalInt dv with
        | some k => (some (decide (k ≠ 0)), none)
        | none => match toLeanProp dv with
          | none => (none, none)
          | some dStr =>
            let hyps := f.requires ++ scope
            let vars := (collectIdents dv ++ hyps.flatMap collectIdents).eraseDups
            let reqs := hyps.filterMap toLeanProp
            let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
            let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
            (none, some s!"{binder}{hyp}({dStr} ≠ 0)")
      out := out ++ [{ fnQual := fq, key, divExpr := dv, isMod, closedVerdict := cv.1, leanGoal := cv.2 }]
      i := i + 1
  return out

/-- Lean goals for the non-constant divisor obligations, for omega discharge. -/
def divGoals (modules : List Module) : List (String × String) :=
  (divObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Render the division-by-zero section. -/
def renderDiv (obls : List DivObl) (provedKeys : List String) : String := Id.run do
  if obls.isEmpty then return ""
  let mut out := "\n\n=== Runtime-safety obligations (division: non-zero divisor) ==="
  let mut cur := ""
  for o in obls do
    if o.fnQual != cur then out := out ++ s!"\n\n{o.fnQual}"; cur := o.fnQual
    let opname := if o.isMod then "%" else "/"
    let status := match o.closedVerdict with
      | some true  => "checked: divisor is a nonzero constant"
      | some false => "VIOLATION: division by zero (constant divisor)"
      | none => match o.leanGoal with
        | some _ =>
          if provedKeys.contains o.key
          then "proved_by_kernel_decision (omega) — divisor nonzero, no runtime check needed"
          else "unproven — require the divisor nonzero (#[requires]), or insert a runtime check"
        | none => "unproven — divisor not statically analyzable; needs a runtime check"
    out := out ++ s!"\n  {opname} divisor {Concrete.fmtExpr o.divExpr}\n    status: {status}"
  return out ++ "\n"

-- ============================================================
-- Runtime-safety obligations: integer overflow (opt-in)
-- ============================================================
-- Under `#[overflow_checked]`, each fixed-width `+`/`-`/`*` generates
-- `MIN ≤ result ≤ MAX` for that width. Opt-in, because Concrete's default
-- integer overflow semantics are profile-dependent and emitting this for every
-- arithmetic op would flood the audit. Same disposition shape as bounds/div.

/-- Inclusive value range of a fixed-width integer type (none = arbitrary/`Int`). -/
def intRange : Ty → Option (Int × Int)
  | .i8  => some (-128, 127)        | .i16 => some (-32768, 32767)
  | .i32 => some (-2147483648, 2147483647)
  | .u8  => some (0, 255)           | .u16 => some (0, 65535)
  | .u32 => some (0, 4294967295)
  | _ => none

/-- Best-effort fixed-width int type of an expression, from a var→type map. -/
partial def exprIntTy (vt : List (String × Ty)) : Expr → Option Ty
  | .ident _ n => vt.lookup n
  | .paren _ e => exprIntTy vt e
  | .unaryOp _ _ e => exprIntTy vt e
  | .cast _ _ t => some t
  | .binOp _ _ l r => match exprIntTy vt l with | some t => some t | none => exprIntTy vt r
  | _ => none

mutual
/-- Every `+`/`-`/`*` binop node in an expression (the whole `a op b`). -/
partial def collectArithE : Expr → List Expr
  | e@(.binOp _ op l r) =>
    let here := match op with | .add | .sub | .mul => [e] | _ => []
    here ++ collectArithE l ++ collectArithE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .try_ _ x | .cast _ x _ | .fieldAccess _ x _ | .arrowAccess _ x _ => collectArithE x
  | .arrayLit _ es => es.flatMap collectArithE
  | .arrayIndex _ a i => collectArithE a ++ collectArithE i
  | .call _ _ _ args => args.flatMap collectArithE
  | .methodCall _ o _ _ args => collectArithE o ++ args.flatMap collectArithE
  | .staticMethodCall _ _ _ _ args => args.flatMap collectArithE
  | .structLit _ _ _ fs base => fs.flatMap (fun (_, fe) => collectArithE fe) ++ (base.map collectArithE).getD []
  | .enumLit _ _ _ _ fs => fs.flatMap (fun (_, fe) => collectArithE fe)
  | .allocCall _ x a => collectArithE x ++ collectArithE a
  | .ifExpr _ c t el | .whileExpr _ c t el =>
      collectArithE c ++ t.flatMap collectArithS ++ el.flatMap collectArithS
  | .match_ _ s _ => collectArithE s
  | _ => []
partial def collectArithS : Stmt → List Expr
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectArithE v
  | .return_ _ (some v) => collectArithE v
  | .ifElse _ c t el => collectArithE c ++ t.flatMap collectArithS ++ (el.getD []).flatMap collectArithS
  | .while_ _ c b _ => collectArithE c ++ b.flatMap collectArithS
  | .forLoop _ init c step b _ =>
      (init.map collectArithS).getD [] ++ collectArithE c
        ++ (step.map collectArithS).getD [] ++ b.flatMap collectArithS
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v => collectArithE o ++ collectArithE v
  | .arrayIndexAssign _ a i v => collectArithE a ++ collectArithE i ++ collectArithE v
  | _ => []
end

/-- Arithmetic-op leaf: the `+`/`-`/`*` op nodes in a statement's OWN expression
    positions (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's op nodes). -/
def arithLeaf (scope : List Expr) : Stmt → List (Expr × List Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectArithE v).map fun e => (e, scope)
  | .return_ _ (some v) => (collectArithE v).map fun e => (e, scope)
  | .ifElse _ c _ _ => (collectArithE c).map fun e => (e, scope)
  | .while_ _ c _ _ => (collectArithE c).map fun e => (e, scope)
  | .forLoop _ _ c _ _ _ => (collectArithE c).map fun e => (e, scope)
  | .fieldAssign _ o _ v | .arrowAssign _ o _ v | .derefAssign _ o v =>
      (collectArithE o ++ collectArithE v).map fun e => (e, scope)
  | .arrayIndexAssign _ a i v =>
      (collectArithE a ++ collectArithE i ++ collectArithE v).map fun e => (e, scope)
  | _ => []

/-- Arithmetic-op nodes paired with the hypotheses in scope (Phase 3 #7 —
    migrated onto the unified `scopedWalk`). The collector now threads enclosing
    guards / negated guards / fall-through / loop invariants into the in-scope
    facts, so an overflow obligation's interval/`bv_decide`/SMT discharge sees
    strictly MORE sound bounds — proofs can only get stronger
    (`unproven → proved`), never weaker. The three-route discharge downstream is
    unchanged: omega/interval/`bv_decide` are kernel-owned, external SMT remains
    opt-in (`--smt`) and may only touch obligations the kernel tiers left
    unproved; stale bounds are still dropped by the shared invalidation rule. -/
def scopedArithB (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) :
    List (Expr × List Expr) :=
  scopedWalkB arithLeaf lcs scope body

/-- One integer-overflow obligation. -/
structure OverflowObl where
  fnQual        : String
  key           : String
  opExpr        : Expr
  lo            : Int
  hi            : Int
  closedVerdict : Option Bool
  leanGoal      : Option String           -- omega goal (linear)
  bvGoal        : Option String := none    -- widened bv_decide goal (nonlinear, interval-gated)
  hyps          : List Expr := []          -- the in-scope #[requires]/guards (for the SMT query)

/-- All annotated `let` bindings in a statement tree, including those declared
    inside loop inits/steps/bodies (so a loop counter `i` from a `for`-init is
    typed for the overflow check). -/
partial def collectLetTys : Stmt → List (String × Ty)
  | .letDecl _ n _ (some t) _ _ => [(n, t)]
  | .ifElse _ _ t el => t.flatMap collectLetTys ++ (el.getD []).flatMap collectLetTys
  | .while_ _ _ b _ => b.flatMap collectLetTys
  | .forLoop _ init _ step b _ =>
      (init.map collectLetTys).getD [] ++ (step.map collectLetTys).getD [] ++ b.flatMap collectLetTys
  | _ => []

/-- Var→type map from params and annotated lets (recursively, see
    `collectLetTys`). -/
def varTyMap (f : FnDef) : List (String × Ty) :=
  f.params.map (fun p => (p.name, p.ty)) ++ f.body.flatMap collectLetTys

-- ============================================================
-- Nonlinear overflow discharge: interval analysis + bv_decide
-- ============================================================
-- omega is LINEAR, so a product of two variables (`sample * gain`) is left
-- `unproven` by the omega path. When every operand has a non-negative, bounded
-- range (from #[requires] / loop invariants), interval analysis computes the
-- result range; if it fits the type, we emit a WIDENED unsigned `bv_decide`
-- goal so the no-overflow fact is KERNEL-checked (not merely computed here).
-- Restricted to +/* of non-negative bounded operands so the unsigned model is
-- sound (no underflow); anything else stays honestly `unproven`.

/-- Flatten an `&&`-conjunction into its conjuncts. -/
partial def conjuncts : Expr → List Expr
  | .binOp _ .and_ l r => conjuncts l ++ conjuncts r
  | .paren _ e => conjuncts e
  | e => [e]

/-- Per-variable integer bounds `[lo,hi]` from hypothesis conjuncts of the form
    `k <= v`, `v <= k`, `k < v`, `v < k` (and ≥/> mirrors), merged to the
    tightest known bound. Only variables with BOTH a lower and upper bound. -/
def varBoundsFromHyps (hyps : List Expr) : List (String × (Int × Int)) := Id.run do
  let mut los : List (String × Int) := []
  let mut his : List (String × Int) := []
  let upd := fun (l : List (String × Int)) (v : String) (k : Int) (f : Int → Int → Int) =>
    match l.lookup v with
    | some old => (l.filter (·.1 != v)) ++ [(v, f old k)]
    | none => l ++ [(v, k)]
  for c in hyps.flatMap conjuncts do
    match c with
    | .binOp _ .leq (.intLit _ k) (.ident _ v) => los := upd los v k max
    | .binOp _ .leq (.ident _ v) (.intLit _ k) => his := upd his v k min
    | .binOp _ .lt  (.intLit _ k) (.ident _ v) => los := upd los v (k+1) max
    | .binOp _ .lt  (.ident _ v) (.intLit _ k) => his := upd his v (k-1) min
    | .binOp _ .geq (.ident _ v) (.intLit _ k) => los := upd los v k max
    | .binOp _ .geq (.intLit _ k) (.ident _ v) => his := upd his v k min
    | .binOp _ .gt  (.ident _ v) (.intLit _ k) => los := upd los v (k+1) max
    | .binOp _ .gt  (.intLit _ k) (.ident _ v) => his := upd his v (k-1) min
    | _ => pure ()
  return los.filterMap fun (v, l) => (his.lookup v).map fun h => (v, (l, h))

/-- Conservative interval `(lo, hi)` of an arithmetic expr plus the maximum
    magnitude seen across the whole subtree (to choose a wrap-free bit width).
    `none` if any operand is unbounded or the op is outside `+`/`-`/`*`. -/
partial def exprIntervalMax (bounds : List (String × (Int × Int))) : Expr → Option (Int × Int × Nat)
  | .intLit _ k => some (k, k, k.natAbs)
  | .paren _ e => exprIntervalMax bounds e
  | .ident _ v => (bounds.lookup v).map fun (l, h) => (l, h, max l.natAbs h.natAbs)
  | .binOp _ op l r => do
    let (la, lb, lm) ← exprIntervalMax bounds l
    let (ra, rb, rm) ← exprIntervalMax bounds r
    let sub := max lm rm
    let mk := fun (a b : Int) => some (a, b, max sub (max a.natAbs b.natAbs))
    match op with
    | .add => mk (la + ra) (lb + rb)
    | .sub => mk (la - rb) (lb - ra)
    | .mul =>
      let ps := [la*ra, la*rb, lb*ra, lb*rb]
      mk (ps.foldl min (la*ra)) (ps.foldl max (la*ra))
    | _ => none
  | _ => none

/-- Lower an arithmetic expr to a `BitVec w` term (`+`/`*` of vars and
    non-negative literals only; `-` is excluded so the unsigned model can't
    underflow). -/
partial def arithToBVW (w : Nat) : Expr → Option String
  | .intLit _ k => if k < 0 then none else some s!"({k}#{w})"
  | .paren _ e => arithToBVW w e
  | .ident _ v => some v
  | .binOp _ op l r => do
    let L ← arithToBVW w l
    let R ← arithToBVW w r
    match op with
    | .add => some s!"({L} + {R})"
    | .mul => some s!"({L} * {R})"
    | _ => none
  | _ => none

/-- A widened unsigned `bv_decide` goal proving `e` cannot overflow `[lo,hi]`,
    when interval analysis shows the result is non-negative and in range and the
    operands are `+`/`*` of non-negative bounded vars. `none` otherwise (→ the
    obligation stays `unproven`). -/
def overflowBVGoal (e : Expr) (lo hi : Int) (hyps : List Expr) : Option String := do
  let bounds := varBoundsFromHyps hyps
  let (elo, ehi, maxMag) ← exprIntervalMax bounds e
  guard (lo ≤ elo)          -- lower bound holds by interval
  guard (ehi ≤ hi)          -- upper bound holds by interval
  guard (0 ≤ elo)           -- non-negative result → unsigned model is sound
  let w ← if maxMag < 2147483648 then some 32
          else if maxMag < 9223372036854775808 then some 64 else none
  let eBV ← arithToBVW w e
  let vars := (collectIdents e).eraseDups
  guard (!vars.isEmpty)
  -- every operand needs a non-negative upper bound to model it as unsigned BitVec
  let varHyps ← vars.mapM fun v => (bounds.lookup v).bind fun (vl, vh) =>
    if vl < 0 then none else some s!"BitVec.ule {v} ({vh}#{w})"
  some s!"∀ ({" ".intercalate vars} : BitVec {w}), {" → ".intercalate varHyps} → BitVec.ule {eBV} ({hi}#{w})"

/-- Generate no-overflow obligations for `#[overflow_checked]` functions. -/
def overflowObligations (modules : List Module) : List OverflowObl := Id.run do
  let mut out : List OverflowObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    if !f.overflowChecked then continue
    let fq := pfx ++ f.name
    let vt := varTyMap f
    let mut i := 0
    for (e, scope) in scopedArithB f.loopContracts [] f.body do
      match (exprIntTy vt e).bind intRange, toLeanProp e with
      | some (lo, hi), some eStr =>
        let key := s!"{fq}#ovf{i}"
        let hyps := f.requires ++ scope
        let cv : Option Bool × Option String := match cEvalInt e with
          | some k => (some (decide (lo ≤ k ∧ k ≤ hi)), none)
          | none =>
            let vars := (collectIdents e ++ hyps.flatMap collectIdents).eraseDups
            let reqs := hyps.filterMap toLeanProp
            let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
            let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
            (none, some s!"{binder}{hyp}({lo} ≤ {eStr} ∧ {eStr} ≤ {hi})")
        -- nonlinear/bv fallback: interval-gated widened unsigned goal (only when
        -- omega's linear goal won't close it — i.e. the constant case is skipped).
        let bvGoal := if cv.1.isSome then none else overflowBVGoal e lo hi hyps
        out := out ++ [{ fnQual := fq, key, opExpr := e, lo, hi
                       , closedVerdict := cv.1, leanGoal := cv.2, bvGoal, hyps }]
        i := i + 1
      | _, _ => pure ()
  return out

/-- Lean goals for the non-constant overflow obligations, for omega discharge. -/
def overflowGoals (modules : List Module) : List (String × String) :=
  (overflowObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Widened unsigned `bv_decide` goals for nonlinear overflow obligations (the
    interval-gated `var * var` cases omega cannot close). Run by Main after omega,
    only for obligations omega left unproven. -/
def overflowBVGoals (modules : List Module) : List (String × String) :=
  (overflowObligations modules).filterMap fun o => o.bvGoal.map (fun g => (o.key, g))

/-! ## External-SMT path (Phase 2 #8) — narrow first slice

The kernel-checked tiers (constant fold → omega → `bv_decide`) are exhausted
first. What genuinely remains outside them is *nonlinear* integer arithmetic —
a product of two program variables that interval analysis cannot bound. For that
one narrow VC class we can emit a standard SMT-LIB query and hand it to an
external solver. The result is NEVER a kernel-checked class: it is
`solver_trusted` / `proved_by_smt` (the solver enters the TCB), kept distinct
from `proved_by_kernel_decision`, and only ever produced behind an explicit flag.
The translation targets structured `Expr`s (not the Lean-syntax strings), so the
emitted SMT-LIB is well-formed by construction. -/

/-- Lower a contract `Expr` to an SMT-LIB (QF_NIA) s-expression. Same fragment as
    `toLeanProp`: integer literals/vars, add/sub/mul, comparisons, and/or/not. -/
partial def exprToSmt : Expr → Option String
  | .intLit _ v => some (if v < 0 then s!"(- {-v})" else s!"{v}")
  | .ident _ n => some n
  | .paren _ e => exprToSmt e
  | .unaryOp _ op e => do
    let E ← exprToSmt e
    match op with
    | .neg  => some s!"(- {E})"
    | .not_ => some s!"(not {E})"
    | _     => none
  | .binOp _ op l r => do
    let L ← exprToSmt l
    let R ← exprToSmt r
    match op with
    | .neq => some s!"(not (= {L} {R}))"
    | _ => (obBinOpSmt op).map fun s => s!"({s} {L} {R})"
  | _ => none

/-- True when `e` contains a multiplication of two non-constant operands — the
    genuinely nonlinear shape `omega` cannot own and interval `bv_decide` may miss. -/
partial def exprHasNonlinMul : Expr → Bool
  | .binOp _ .mul l r => (cEvalInt l).isNone && (cEvalInt r).isNone
      || exprHasNonlinMul l || exprHasNonlinMul r
  | .binOp _ _ l r => exprHasNonlinMul l || exprHasNonlinMul r
  | .paren _ e => exprHasNonlinMul e
  | _ => false

/-- The SMT-eligible VC class (v1): `#[overflow_checked]` obligations whose operand
    is genuinely nonlinear (a product of variables), not constant, and not already
    closed by the interval `bv_decide` path. Returns `(vcKey, smtlibScript)`. The
    script asserts the in-scope hypotheses and the NEGATION of the range goal:
    `unsat` ⇒ no overflow (solver-proved); `sat` ⇒ a counterexample exists. -/
def overflowSmtGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in overflowObligations modules do
    if o.closedVerdict.isSome then continue          -- constant tier owns it
    if o.bvGoal.isSome then continue                 -- interval bv_decide owns it
    if !exprHasNonlinMul o.opExpr then continue      -- omega owns the linear case
    -- soundness: require the operand AND every hypothesis to lower. If any
    -- #[requires]/guard falls outside the SMT fragment, DROP the whole query
    -- rather than emit one missing a constraint (which could read as a spurious
    -- counterexample). Never emit an unsound query.
    match exprToSmt o.opExpr, o.hyps.mapM exprToSmt with
    | some eSmt, some hypSmts =>
      let vars := (collectIdents o.opExpr ++ o.hyps.flatMap collectIdents).eraseDups
      let decls := vars.map (fun v => s!"(declare-const {v} Int)")
      let hypAsserts := hypSmts.map (fun s => s!"(assert {s})")
      let neg := s!"(assert (not (and (<= {o.lo} {eSmt}) (<= {eSmt} {o.hi}))))"
      let script := "\n".intercalate
        (["; VC " ++ o.key, "; no-overflow of an operand in [" ++ toString o.lo ++ ", " ++ toString o.hi ++ "]",
          "(set-logic QF_NIA)"] ++ decls ++ hypAsserts ++ [neg, "(check-sat)", "(get-model)"])
      out := out ++ [(o.key, script)]
    | _, _ => pure ()
  return out

/-! ## Lean replay artifact (Phase 2 #12)

For each SMT VC we also emit a standalone Lean theorem that states the SAME
obligation — hypotheses as binders, the range goal as the conclusion — with an
in-toolchain proof attempt (`by omega`). If a kernel-checked tactic closes it, the
claim no longer depends on the external solver and graduates `solver_trusted` →
`proved_by_lean_replay` (a Lean/kernel class, no solver in the TCB). The bounded
*nonlinear* fragment we route to SMT is, by construction, outside `omega`'s reach
(and `nlinarith` is Mathlib, which is deliberately NOT a dependency), so today the
attempt does not close and the VC honestly stays `solver_trusted`. The artifact is
still emitted so a reviewer — or a Mathlib-enabled build that swaps `omega` for
`nlinarith` — can check it and graduate the evidence. -/

/-- Lower a contract `Expr` to Lean `Prop`/`Int` syntax for the replay theorem.
    Same fragment as `toLeanProp` but ALSO handles unary negation (`-30000`) — the
    signed bounds that make a VC SMT-eligible in the first place. Kept local to the
    replay path so `toLeanProp`'s callers are unaffected. -/
partial def exprToLeanProp : Expr → Option String
  | .intLit _ v => some s!"{v}"
  | .ident _ n => some n
  | .paren _ e => exprToLeanProp e
  | .unaryOp _ op e => do
    let E ← exprToLeanProp e
    match op with | .neg => some s!"(-{E})" | .not_ => some s!"(¬ {E})" | _ => none
  | .binOp _ op l r => do
    let L ← exprToLeanProp l; let R ← exprToLeanProp r
    match op with
    | .div | .mod => none
    | _ => leanBinOp op L R
  | _ => none

/-- `(vcKey, leanTheoremSource)` for each SMT-eligible VC: a self-contained Lean
    theorem restating the obligation, with a `by omega` proof attempt. Same
    selection as `overflowSmtGoals`. -/
def leanReplayGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in overflowObligations modules do
    if o.closedVerdict.isSome then continue
    if o.bvGoal.isSome then continue
    if !exprHasNonlinMul o.opExpr then continue
    match exprToLeanProp o.opExpr, o.hyps.mapM exprToLeanProp with
    | some eProp, some hypProps =>
      let vars := (collectIdents o.opExpr ++ o.hyps.flatMap collectIdents).eraseDups
      let binders := if vars.isEmpty then "" else s!"({" ".intercalate vars} : Int) "
      let hypBinders := " ".intercalate
        ((List.range hypProps.length).zip hypProps |>.map (fun (i, h) => s!"(h{i} : {h})"))
      let concl := s!"({o.lo} ≤ {eProp}) ∧ ({eProp} ≤ {o.hi})"
      -- `by omega` is the in-toolchain attempt; a Mathlib build swaps in `nlinarith`.
      let src := s!"theorem vc_replay {binders}{hypBinders} : {concl} := by omega"
      out := out ++ [(o.key, src)]
    | _, _ => pure ()
  return out

/-- Render the integer-overflow section. `provedKeys` are omega-discharged;
    `bvProvedKeys` are discharged by the widened `bv_decide` (nonlinear) path. -/
def renderOverflow (obls : List OverflowObl) (provedKeys : List String)
    (bvProvedKeys : List String := []) : String := Id.run do
  if obls.isEmpty then return ""
  let mut out := "\n\n=== Runtime-safety obligations (integer overflow, #[overflow_checked]) ==="
  let mut cur := ""
  for o in obls do
    if o.fnQual != cur then out := out ++ s!"\n\n{o.fnQual}"; cur := o.fnQual
    let status := match o.closedVerdict with
      | some true  => "checked: result in range (constant)"
      | some false => "VIOLATION: constant overflows the type"
      | none =>
        if provedKeys.contains o.key then
          "proved_by_kernel_decision (omega) — cannot overflow, no runtime check needed"
        else if bvProvedKeys.contains o.key then
          "proved_by_kernel_decision (bv_decide) — bounded operands cannot overflow (interval + bitvector), no runtime check needed"
        else match o.leanGoal with
          | some _ => "unproven — bound the operands (#[requires]), or use a wrapping/checked profile"
          | none => "unproven — operands not statically analyzable"
    out := out ++ s!"\n  {Concrete.fmtExpr o.opExpr}  (range [{o.lo}, {o.hi}])\n    status: {status}"
  return out ++ "\n"

/-- Runtime-safety obligations that have already been discharged to FALSE.
    These are compile-time proofs that the safe program is wrong, not merely
    `unproven` obligations. Build/check paths use this as a hard-error gate;
    report paths still render the obligations so users can inspect them. -/
def provenViolationDiagnostics (modules : List Module) : Diagnostics := Id.run do
  -- Safe-code only: functions marked `trusted` or holding the `Unsafe`
  -- capability carry audit responsibility and are exempt (ROADMAP Phase 12 #0).
  let unsafeQuals : List String := (modules.flatMap allFunctions).filterMap fun (pfx, f) =>
    if f.isTrusted || f.capSet.concreteCaps.contains "Unsafe" then some (pfx ++ f.name) else none
  let mut ds : Diagnostics := []
  for o in boundsObligations modules do
    if o.closedVerdict == some false && !unsafeQuals.contains o.fnQual then
      let d : Diagnostic := {
        severity := .error,
        message := s!"proven runtime-safety violation: {o.arrName}[{Concrete.fmtExpr o.idxExpr}] is always out of bounds for array size {o.size}",
        pass := "runtime-safety",
        span := some o.idxExpr.getSpan,
        hint := some "fix the index, change the array size, or move this behind an explicit trusted/Unsafe boundary",
        code := "E0900",
        evidence := [("obligation", o.key), ("status", "violation"), ("kind", "array_bounds")]
      }
      ds := ds ++ [d]
  for o in divObligations modules do
    if o.closedVerdict == some false && !unsafeQuals.contains o.fnQual then
      let opname := if o.isMod then "%" else "/"
      let d : Diagnostic := {
        severity := .error,
        message := s!"proven runtime-safety violation: {opname} divisor {Concrete.fmtExpr o.divExpr} is always zero",
        pass := "runtime-safety",
        span := some o.divExpr.getSpan,
        hint := some "require/prove a nonzero divisor, use a checked API, or move this behind an explicit trusted/Unsafe boundary",
        code := "E0900",
        evidence := [("obligation", o.key), ("status", "violation"), ("kind", "division_nonzero")]
      }
      ds := ds ++ [d]
  for o in overflowObligations modules do
    if o.closedVerdict == some false && !unsafeQuals.contains o.fnQual then
      let d : Diagnostic := {
        severity := .error,
        message := s!"proven runtime-safety violation: {Concrete.fmtExpr o.opExpr} always overflows range [{o.lo}, {o.hi}]",
        pass := "runtime-safety",
        span := some o.opExpr.getSpan,
        hint := some "widen the type, bound the operands, or use an explicit wrapping/checked arithmetic profile",
        code := "E0900",
        evidence := [("obligation", o.key), ("status", "violation"), ("kind", "integer_overflow")]
      }
      ds := ds ++ [d]
  return ds

/-- Stable identity for one loop obligation, shared by the goal collector and
    the renderer so discharge results map back to the right line. -/
def loopVCKey (fnQual : String) (line : Nat) (obl : String) : String :=
  s!"{fnQual}@{line}#{obl}"

/-- Collect the loop VCs that a kernel decision procedure can discharge:
    `invariant_init` (O1), `variant_nonnegative` (O4), `variant_decreases` (O5).
    Each is `(key, leanGoal)` where the goal is the same string the report
    shows — it is already valid Lean (`∀ (i : Int), … `) provable by
    `intros; omega`. Preservation (O2) stays hand-linked; exit-link (O3) needs a
    postcondition. -/
def loopVCGoals (modules : List Module) : List (String × String) := Id.run do
  let withLoops := (modules.flatMap allFunctions).filter (fun (_, f) => !f.loopContracts.isEmpty)
  let mut out : List (String × String) := []
  for (pfx, f) in withLoops do
    let extraLets := letConstMap f.body
    let fq := pfx ++ f.name
    let retExpr := loopExitReturn f.body
    -- Phase 3 #9: thread the function's `#[requires]` into every loop obligation
    -- (the unified scoped context). Adding hypotheses is monotonic — it can only
    -- turn an `unproven` loop VC `proved` (e.g. an init `0 ≤ n` from a precondition),
    -- never the reverse.
    let outer := f.requires
    for lc in f.loopContracts do
      if let some g := genInitVC lc extraLets outer then
        out := out ++ [(loopVCKey fq lc.line "O1", g)]
      -- O2 arithmetic half: invariant is inductive (omega); operational half
      -- still needs Lean (genPreservationShape).
      if let some g := genPreservationGoal lc outer then
        out := out ++ [(loopVCKey fq lc.line "O2", g)]
      if let some g := genExitVC lc f.ensures retExpr outer then
        out := out ++ [(loopVCKey fq lc.line "O3", g)]
      if lc.variant.isSome then
        if let some g := genVariantNonneg lc outer then
          out := out ++ [(loopVCKey fq lc.line "O4", g)]
        if let some g := genVariantDecreases lc outer then
          out := out ++ [(loopVCKey fq lc.line "O5", g)]
  return out

/-- The loop-contract section: for each `#[invariant]`/`#[variant]`-annotated
    loop, enumerate the verification obligations it induces. Every obligation now
    carries a compiler-generated VC shape. Discharge: preservation (O2) is
    hand-linked via a `coverage: invariant` registry entry; init/variant
    (O1/O4/O5) are kernel-discharged by `omega` when their key appears in
    `provedVCs`; the rest are `planned`. -/
def loopContractSection (modules : List Module) (registry : ProofRegistry)
    (provedVCs : List String := []) (provedVacuous : List String := []) : String := Id.run do
  let withLoops := (modules.flatMap allFunctions).filter (fun (_, f) => !f.loopContracts.isEmpty)
  if withLoops.isEmpty then return ""
  let callables := callableContractNames modules
  let consts := contractConstNames modules
  let impures := impureFnNames modules
  let mut out := "\n\n=== Loop contracts ==="
  for (pfx, f) in withLoops do
    -- a registered `coverage: invariant` proof discharges invariant_preservation
    let preserveProof : Option String :=
      match registry.find? (fun e => e.function == pfx ++ f.name) with
      | some e => if e.coverage == "invariant" && !e.proof.isEmpty then some e.proof else none
      | none => none
    let extraLets := letConstMap f.body
    let fq := pfx ++ f.name
    let outer := f.requires  -- Phase 3 #9: function preconditions are in scope for loop VCs
    let contractVars := (f.params.map (·.name) ++ localNamesB f.body ++ consts).eraseDups
    for lc in f.loopContracts do
      out := out ++ s!"\n\n{pfx}{f.name}  (loop @ line {lc.line})"
      let mut invIdx := 0
      for inv in lc.invariants do
        let issues := validateContractExpr contractVars callables inv
          ++ (contractImpureCalls impures inv).map (fun fn => s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
        let vac := cEvalBool inv == some false || provedVacuous.contains s!"{fq}@{lc.line}#inv_vac{invIdx}"
        let st :=
          if !issues.isEmpty then contractIssueStatus issues
          else if vac then "\n     status:  invalid/vacuous (unsatisfiable invariant — the loop obligations below are meaningless)"
          else ""
        out := out ++ s!"\n  invariant {Concrete.fmtExpr inv}{st}"
        invIdx := invIdx + 1
      match lc.variant with
      | some v => out := out ++ s!"\n  variant   {Concrete.fmtExpr v}{contractIssueStatus (validateContractExpr contractVars callables v)}"
      | none => pure ()
      out := out ++ "\n  obligations:"
      let planned := "planned (no discharge backend linked yet)"
      let vc := fun (g : Option String) => match g with | some s => s!"\n       generated VC:  {s}" | none => ""
      -- Kernel-decision status for an obligation: omega-discharged when its key
      -- is in `provedVCs`, else planned. `omega` is a kernel decision procedure
      -- (linear integer arithmetic), no external SMT in the TCB.
      let kstat := fun (obl : String) =>
        if provedVCs.contains (loopVCKey fq lc.line obl)
        then "proved_by_kernel_decision\n                                engine:  omega"
        else planned
      -- O1 invariant_init — generated shape, kernel-discharged
      out := out ++ s!"\n    O1 invariant_init          status:  {kstat "O1"}{vc (genInitVC lc extraLets outer)}"
      -- O2 invariant_preservation — split into the two things it actually
      -- requires: (1) the arithmetic step (invariant is inductive), now
      -- auto-discharged by omega; (2) the operational step (the extracted body
      -- realizes the substitution), which still needs Lean. (1) removes most of
      -- the hand-linking; (2) points to the theorem shape when unproved.
      let arithStep :=
        if provedVCs.contains (loopVCKey fq lc.line "O2")
        then "proved_by_kernel_decision (omega)" else planned
      let opStep := match preserveProof with
        | some thm => s!"proved_by_lean\n                          theorem: {thm}"
        | none => match genPreservationShape lc fq with
          | some shape => s!"planned — needs Lean (operational realization), shape:\n           {shape}"
          | none => planned
      out := out ++ s!"\n    O2 invariant_preservation"
      out := out ++ s!"\n       arithmetic step:   {arithStep}"
      out := out ++ s!"\n       operational step:  {opStep}{vc (genPreservationVC lc)}"
      -- O3 exit_implies_post: bridges loop exit facts (invariant ∧ ¬guard) to
      -- the function #[ensures]. Generated only when there is an ensures and a
      -- clean loop-exit return; kernel-discharged by omega like O1/O4/O5.
      let exitVC := genExitVC lc f.ensures (loopExitReturn f.body) outer
      let o3status :=
        if exitVC.isSome then kstat "O3"
        else if f.ensures.isEmpty then "n/a (no #[ensures] postcondition)"
        else planned
      out := out ++ s!"\n    O3 loop_exit_post_link     status:  {o3status}{vc exitVC}"
      match lc.variant with
      | some _ =>
        out := out ++ s!"\n    O4 variant_nonnegative     status:  {kstat "O4"}{vc (genVariantNonneg lc outer)}"
        out := out ++ s!"\n    O5 variant_decreases       status:  {kstat "O5"}{vc (genVariantDecreases lc outer)}"
      | none => pure ()
  return out ++ "\n"

partial def contractsReport (modules : List Module) (registry : ProofRegistry)
    (provedVCs : List String := []) (provedVacuous : List String := []) : String := Id.run do
  let callables := callableContractNames modules
  let consts := contractConstNames modules
  let impures := impureFnNames modules
  -- Discharge status for an `#[ensures]` on `qual`. Tiers, honest about partial
  -- coverage: a registered `ensures_proof` → fully proved_by_lean. Otherwise, a
  -- registered `proof` with directional coverage (`one_direction`) discharges
  -- ONE direction of the postcondition — shown as partial, with the converse
  -- still outstanding — rather than collapsing to a bare "missing".
  let discharge (qual : String) : String :=
    match registry.find? (fun e => e.function == qual) with
    | some e => match e.ensuresProof with
      | some thm =>
        -- `iff` coverage with both a `proof` and an `ensures_proof` means the
        -- two directions of an iff postcondition are each kernel-checked.
        if e.coverage == "iff" && !e.proof.isEmpty then
          s!"\n     status:  proved_by_lean (full iff)\n     forward direction:  {e.proof}\n     converse direction: {thm}"
        else s!"\n     status:  proved_by_lean\n     theorem: {thm}"
      | none =>
        if !e.proof.isEmpty && e.coverage == "one_direction" then
          s!"\n     status:  partial — one direction proved_by_lean, converse outstanding\n     theorem: {e.proof}  (coverage: one_direction)\n     note:    full postcondition not yet discharged; the converse is the next obligation"
        else "\n     status:  missing (registry entry has no ensures_proof)"
    | none => "\n     status:  missing (no in-source proof link for this function)"
  let rec go (m : Module) (acc : String) : String := Id.run do
    let mut out := acc
    let pfx := if m.name.isEmpty then "" else m.name ++ "."
    for sf in m.specFns do
      let ps := ", ".intercalate (sf.params.map (fun p => s!"{p.name}: {Concrete.fmtTy p.ty}"))
      out := out ++ s!"\nspec fn {pfx}{sf.name}({ps}) -> {Concrete.fmtTy sf.retTy}"
    for f in m.functions do
      if !f.ensures.isEmpty || !f.requires.isEmpty then
        out := out ++ s!"\n\n{pfx}{f.name}"
        let paramVars := f.params.map (·.name)
        let postVars := (paramVars ++ ["result"] ++ localNamesB f.body ++ consts).eraseDups
        let preVars := (paramVars ++ consts).eraseDups
        -- vacuity: an unsatisfiable precondition makes every #[ensures] hold
        -- trivially — a misleading green. Caught by the constant folder
        -- (#[requires(false)]) or by omega refuting the conjunction (x>0 && x<0).
        let vacuous := !f.requires.isEmpty
          && (f.requires.any (fun r => cEvalBool r == some false)
              || provedVacuous.contains s!"{pfx ++ f.name}#requires_vac")
        if vacuous then
          out := out ++ "\n  ⚠ VACUOUS — preconditions are unsatisfiable; any #[ensures] holds trivially (NOT a real proof)"
        -- preconditions: assumed on entry here; each call site is checked
        -- separately (see the "Call-site obligations" section).
        let mut ri := 1
        for r in f.requires do
          let issues := validateContractExpr preVars callables r
            ++ (contractImpureCalls impures r).map (fun fn => s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
          let st :=
            if !issues.isEmpty then contractIssueStatus issues
            else if vacuous then "\n     status:  vacuous (unsatisfiable precondition)"
            else "\n     status:  assumed_at_entry (each call site checked separately)"
          out := out ++ s!"\n  R{ri}  requires {Concrete.fmtExpr r}{st}"
          ri := ri + 1
        -- postconditions: vacuous if the precondition is unsatisfiable, else
        -- discharged by a registered ensures_proof, or missing.
        let mut i := 1
        for e in f.ensures do
          let issues := validateContractExpr postVars callables e
            ++ (contractImpureCalls impures e).map (fun fn => s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
          let st :=
            if !issues.isEmpty then contractIssueStatus issues
            else if vacuous then "\n     status:  vacuous (precondition unsatisfiable — postcondition holds trivially, NOT proved)"
            else discharge (pfx ++ f.name)
          out := out ++ s!"\n  O{i}  ensures {Concrete.fmtExpr e}{st}"
          i := i + 1
    for sub in m.submodules do
      out := go sub out
    return out
  let body := modules.foldl (fun acc m => go m acc) ""
  let body := if body.isEmpty then "\n(no spec fns or #[ensures] contracts found)" else body
  return s!"=== Source Contracts ==={body}\n{loopContractSection modules registry provedVCs provedVacuous}"

/-- Whether any module (or submodule) carries a source contract — a `spec fn`
    or an `#[ensures(...)]`. Used to decide whether `audit` appends the
    contracts section. -/
partial def hasContracts (modules : List Module) : Bool :=
  modules.any fun m =>
    !m.specFns.isEmpty
    || m.functions.any (fun f => !f.ensures.isEmpty || !f.requires.isEmpty || !f.loopContracts.isEmpty
        || !(f.body.flatMap collectAssertAssumeS).isEmpty)
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
      -- Float arithmetic gets a dedicated, honest framing: it is not a generic
      -- profile miss but an absent proof profile. Says the useful thing.
      let floatReason := "floating-point arithmetic has no active proof profile"
      let floatStr := if e.profileReasons.contains floatReason then
        s!"\n             float semantics: unprofiled\n             proof eligibility: excluded\n             reason: {floatReason}"
        else ""
      let otherProfile := e.profileReasons.filter (· != floatReason)
      let srcStr := if e.sourceReasons.isEmpty then "" else
        s!"\n             source: {", ".intercalate e.sourceReasons}"
      let profStr := if otherProfile.isEmpty then "" else
        s!"\n             profile: {", ".intercalate otherProfile}"
      s!"  excluded   `{e.qualName}`  @ {locStr}{srcStr}{profStr}{floatStr}"
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
  origin        : String       -- "source_linked" | "json_backed" | "hardcoded" | "" (link provenance)
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
    -- Look up coverage classification + link provenance from the registry entry.
    let regEntry := registry.find? fun re => re.function == qualName
    let coverage := match regEntry with | some re => re.coverage | none => ""
    -- Registry entries are always synthesized from in-source links now (JSON
    -- registries were removed); `hardcoded` is a built-in Proof.provedFunctions proof.
    let origin := match regEntry with
      | some _ => "source_linked"
      | none => if pSrc == "hardcoded" then "hardcoded" else ""
    { qualName, bareName := f.name, state, currentFp := fp, expectedFp
    , profileGates := gates, unsupported := unsup, specName := sName, proofName := pName
    , proofSource := pSrc, origin, coverage, loc := fnLoc, fnSpan := fnSp }
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
    let originLine := if e.origin.isEmpty then "" else s!"\n\n  origin: {e.origin}"
    s!"-- proved{coverageTag} {String.ofList (List.replicate 48 '-')} {locStr}\n\n  ✓ `{e.qualName}` — proof matches current body.{snippet}\n\n  coverage: {if e.coverage.isEmpty then "unclassified" else e.coverage}{originLine}"
  | .stale =>
    let originLine := if e.origin.isEmpty then "" else s!"\n\n  origin: {e.origin}"
    s!"-- proof stale {String.ofList (List.replicate 44 '-')} {locStr}\n\n  Function `{e.qualName}` has a registered proof, but the body changed.{snippet}\n\n  expected fingerprint:\n    {e.expectedFp}\n\n  current fingerprint:\n    {e.currentFp}{originLine}\n\n  hint: Update the Lean proof in Concrete/Proof.lean, or restore the proved implementation."
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

/-- The raw per-function proof-link freshness entries (Phase 3 #11): the same
    records `proofStatusReport` renders, exposed so the ObligationCore ledger can
    project proof links / fingerprint drift / missing / blocked / ineligible /
    trusted facts into the one ledger instead of a separate proof-status model. -/
def proofStatusEntries (modules : List CModule) (locMap : FnLocMap := [])
    (registry : ProofRegistry := []) (pc : Concrete.ProofCore) : List ProofStatusEntry :=
  modules.foldl (fun acc m => acc ++ collectProofStatus pc locMap m "" registry) []

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
-- `concrete prove <function>` — per-function proof scaffold
-- ============================================================
-- Read-only generator: given a function, print the imports, fingerprint,
-- extracted PExpr, contract/VC list with current discharge, a theorem
-- skeleton, ProofKit hints from detected features, and the next obligation.
-- It writes nothing (the CLI's --out is opt-in) and never auto-proves.

mutual
/-- Detected proof-relevant features in a Core expression, as tags
    ("loop", "array", "bitvec", "call") — drives ProofKit hints. -/
private partial def proveExprFeatures : CExpr → List String
  | .binOp op l r _ =>
    (match op with | .bitand | .bitor | .bitxor | .shl | .shr => ["bitvec"] | _ => [])
      ++ proveExprFeatures l ++ proveExprFeatures r
  | .call _ _ args _ => "call" :: args.flatMap proveExprFeatures
  | .arrayIndex a i _ => "array" :: (proveExprFeatures a ++ proveExprFeatures i)
  | .arrayLit es _ => "array" :: es.flatMap proveExprFeatures
  | .unaryOp _ e _ | .cast e _ | .fieldAccess e _ _
  | .borrow e _ | .borrowMut e _ | .deref e _ | .try_ e _ => proveExprFeatures e
  | .structLit _ _ fs _ | .enumLit _ _ _ fs _ => fs.flatMap (fun (_, e) => proveExprFeatures e)
  | .match_ s arms _ => proveExprFeatures s ++ arms.flatMap proveArmFeatures
  | .whileExpr c b eb _ =>
    "loop" :: (proveExprFeatures c ++ b.flatMap proveStmtFeatures ++ eb.flatMap proveStmtFeatures)
  | .ifExpr c t e _ =>
    proveExprFeatures c ++ t.flatMap proveStmtFeatures ++ e.flatMap proveStmtFeatures
  | .allocCall a b _ => proveExprFeatures a ++ proveExprFeatures b
  | _ => []

private partial def proveArmFeatures : CMatchArm → List String
  | .enumArm _ _ _ _ body | .litArm _ _ body | .varArm _ _ _ body | .rangeArm _ _ _ _ body => body.flatMap proveStmtFeatures

private partial def proveStmtFeatures : CStmt → List String
  | .letDecl _ _ _ v | .assign _ v | .expr v _ => proveExprFeatures v
  | .return_ (some v) _ => proveExprFeatures v
  | .return_ none _ => []
  | .ifElse c t e =>
    proveExprFeatures c ++ t.flatMap proveStmtFeatures ++ (e.getD []).flatMap proveStmtFeatures
  | .while_ c b _ step =>
    "loop" :: (proveExprFeatures c ++ b.flatMap proveStmtFeatures ++ step.flatMap proveStmtFeatures)
  | _ => []
end

/-- Render the `concrete prove` scaffold for one resolved function entry.
    `e` (Core) carries the extracted PExpr / fingerprint / params; `astFn` (AST)
    carries the source contracts (`requires`/`ensures`/`loopContracts`) since the
    Core `CFnDef` does not. `provedVCs` are the loop-VC keys the caller's omega
    pass discharged. -/
def proveReportEntry (registry : ProofRegistry) (e : ProofCoreEntry)
    (astFn : FnDef) (provedVCs : List String) : String := Id.run do
  let f := astFn  -- contracts + loop VC generation operate on the AST function
  let leanName := leanIdent (e.qualName.splitOn "." |>.getLast!)
  -- (1) imports
  let imports := "import Concrete.ProofKit\nimport Concrete.Proof"
  -- (2) fingerprint, (3) extracted PExpr
  let pcStr := match e.extracted with | some p => renderPExpr p | none => "(not extracted)"
  -- (4) contract / VC list
  let preserveProof : Option String :=
    match registry.find? (fun re => re.function == e.qualName) with
    | some re => if re.coverage == "invariant" && !re.proof.isEmpty then some re.proof else none
    | none => none
  let mut vcLines : List String := []
  -- requires / ensures
  for r in f.requires do
    vcLines := vcLines ++ [s!"  requires {Concrete.fmtExpr r}    assumed_at_entry"]
  let regEntry := registry.find? (fun re => re.function == e.qualName)
  let ensuresProof : Option String := regEntry.bind (·.ensuresProof)
  -- A registered directional proof discharges one direction of the iff.
  let oneDir := match regEntry with
    | some re => re.ensuresProof.isNone && !re.proof.isEmpty && re.coverage == "one_direction"
    | none => false
  let isIff := match regEntry with
    | some re => re.ensuresProof.isSome && !re.proof.isEmpty && re.coverage == "iff"
    | none => false
  let fwdProof := match regEntry with | some re => re.proof | none => "?"
  for ens in f.ensures do
    let st := match ensuresProof with
      | some thm =>
        if isIff then s!"proved_by_lean (full iff): forward {fwdProof}, converse {thm}"
        else s!"linked to Lean theorem {thm}"
      | none => if oneDir then "partial — one direction proved_by_lean, converse outstanding"
                else "missing (no registered ensures_proof)"
    vcLines := vcLines ++ [s!"  ensures {Concrete.fmtExpr ens}    {st}"]
  -- loop obligations
  let extraLets := letConstMap f.body
  let retExpr := loopExitReturn f.body
  let mut nextOb : Option (String × String) := none  -- (id, reason)
  -- The function postcondition IS the contract; completing it takes priority
  -- over the loop's operational step (which the literal-bound proof can bypass).
  if !f.ensures.isEmpty && ensuresProof.isNone then
    if oneDir then
      nextOb := some ("#[ensures] converse direction",
        "the proved direction discharges half the postcondition; prove the converse (the other direction of the iff) to fully discharge #[ensures]")
    else
      nextOb := some ("#[ensures] postcondition",
        "state the spec and prove the postcondition; no registered theorem discharges it yet")
  for lc in f.loopContracts do
    let omegaSt := fun (obl : String) =>
      if provedVCs.contains (loopVCKey e.qualName lc.line obl) then "discharged by omega" else "PLANNED"
    if (genInitVC lc extraLets).isSome then
      vcLines := vcLines ++ [s!"  O1 invariant_init          {omegaSt "O1"}"]
    -- O2 split: arithmetic (omega) + operational (Lean)
    let o2arith := omegaSt "O2"
    let o2op := match preserveProof with
      | some thm => s!"linked to Lean theorem {thm}"
      | none => "needs Lean (operational realization)"
    vcLines := vcLines ++ [s!"  O2 invariant_preservation  arithmetic: {o2arith}; operational: {o2op}"]
    if preserveProof.isNone && nextOb.isNone then
      nextOb := some ("O2 invariant_preservation",
        "operational step: the extracted loop body realizes the state transition; this is an eval-level Lean proof, not pure linear arithmetic")
    if (genExitVC lc f.ensures retExpr).isSome then
      vcLines := vcLines ++ [s!"  O3 loop_exit_post_link     {omegaSt "O3"}"]
    if lc.variant.isSome then
      vcLines := vcLines ++ [s!"  O4 variant_nonnegative     {omegaSt "O4"}"]
      vcLines := vcLines ++ [s!"  O5 variant_decreases       {omegaSt "O5"}"]
  -- function-level status fallback for the "next" pointer
  if nextOb.isNone then
    if e.extracted.isNone then
      nextOb := some ("extraction", s!"function is not extractable: {", ".intercalate e.unsupported}")
    else if f.ensures.any (fun _ => ensuresProof.isNone) then
      nextOb := some ("ensures", "state the refinement spec and prove `<fn> refines spec`; no registered theorem yet")
  let vcSection := if vcLines.isEmpty then "  (no source contracts or loop obligations on this function)"
                   else "\n".intercalate vcLines
  -- (5) theorem skeleton
  let paramSig := " ".intercalate (e.params.map fun p => s!"({p} : Int)")
  let nextTodo := match nextOb with | some (id, _) => s!"-- TODO: {id}" | none => "-- TODO: state and prove the property"
  let skeleton :=
    s!"theorem {leanName}_refines_spec {paramSig} (fuel : Nat) : True := by\n  {nextTodo}\n  trivial"
  -- (6) ProofKit hints from detected features (scanned over the Core body)
  let feats := (e.fn.body.flatMap proveStmtFeatures).eraseDups
  let hasLoop := feats.contains "loop" || !f.loopContracts.isEmpty
  let mut hints : List String := []
  if hasLoop then hints := hints ++ ["  loops    → Concrete.ProofKit.Loops: eval_while_count, counter-loop invariants"]
  if feats.contains "array" then hints := hints ++ ["  arrays   → Concrete.ProofKit.Array: lookupIndex_set_self/ne, length_set"]
  if feats.contains "bitvec" then hints := hints ++ ["  bitvecs  → bv_decide for closed bitvector goals; Concrete.ProofKit.BitVec bridges"]
  if feats.contains "call" then hints := hints ++ ["  calls    → Concrete.ProofKit.Calls: FnTable skeleton + call wrappers"]
  let hintSection := if hints.isEmpty then "  (no loop/array/bitvec/call features detected)" else "\n".intercalate hints
  -- (7) next obligation
  let nextSection := match nextOb with
    | some (id, reason) => s!"next: {id}\nreason: {reason}"
    | none => "next: nothing outstanding from generated obligations (a full refinement spec may still be unstated)"
  -- assemble
  return String.join [
    s!"=== concrete prove: {e.qualName} ===\n\n",
    s!"-- (1) suggested imports\n{imports}\n\n",
    s!"-- (2) body fingerprint\n{e.fingerprint}\n\n",
    s!"-- (3) extracted ProofCore body\n{pcStr}\n\n",
    s!"-- (4) contracts / verification conditions\n{vcSection}\n\n",
    s!"-- (5) theorem skeleton (fill in the spec; replace `True`/`trivial`)\n{skeleton}\n\n",
    s!"-- (6) ProofKit hints\n{hintSection}\n\n",
    s!"-- (7) {nextSection}\n" ]

/-- Resolve a `concrete prove` target to a single entry/excluded function.
    Accepts a fully-qualified name, or a bare name if it is unique. Returns the
    resolved `qualName`, or an error message listing candidates. -/
def proveResolve (pc : Concrete.ProofCore) (target : String) : Except String String :=
  let allNames := pc.entries.map (·.qualName) ++ pc.excluded.map (·.qualName)
  match allNames.find? (· == target) with
  | some q => .ok q
  | none =>
    let byBare := allNames.filter fun q => (q.splitOn ".").getLast! == target
    match byBare with
    | [q] => .ok q
    | [] => .error s!"no function '{target}'. Known functions:\n  {"\n  ".intercalate allNames}"
    | many => .error s!"'{target}' is ambiguous; qualify it:\n  {"\n  ".intercalate many}"

/-- Top-level `concrete prove` report for a resolved `qualName`. Looks up the
    AST function (for source contracts) and the Core entry (for the extracted
    body); handles the excluded case (prints why, no stub). -/
def proveReport (pc : Concrete.ProofCore) (registry : ProofRegistry)
    (modules : List Module) (qualName : String) (provedVCs : List String) : String :=
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName)
    |>.map Prod.snd
  match pc.entries.find? (·.qualName == qualName), astFn? with
  | some e, some astFn => proveReportEntry registry e astFn provedVCs
  | some e, none =>
    -- Extracted but no AST match (shouldn't happen): fall back without contracts.
    proveReportEntry registry e { name := e.bareName, params := [], retTy := .unit, body := [] } provedVCs
  | none, _ =>
    match pc.excluded.find? (·.qualName == qualName) with
    | some x =>
      let reasons := x.eligibility.sourceReasons ++ x.eligibility.profileReasons
      s!"=== concrete prove: {qualName} ===\n\nThis function is NOT in the provable subset, so there is no proof to scaffold.\n  reasons: {", ".intercalate reasons}\n\nMake it eligible (remove the listed constraints) or prove it as a trusted boundary."
    | none => s!"no function '{qualName}' in ProofCore."

/-- Single source of truth for one loop obligation's identity and content.
    Returns `(kind, hypotheses, conclusion, status)` for `oblId` ∈ O1..O5, or
    `none` when that obligation does not exist on this loop (gating: O1 needs a
    groundable init, O3 a groundable exit postcondition, O4/O5 a variant).
    `status` is `proved_by_kernel_decision` (omega closed), `arithmetic_proved`
    (O2 arithmetic half closed, operational pending), or `planned`. Used by both
    `--show-obligation` and `--json` so ids/kinds/hypotheses never drift. -/
def loopObInfo (lc : LoopContract) (oblId qualName : String) (ensures : List Expr)
    (extraLets : List (String × Expr)) (retExpr : Option Expr) (provedVCs : List String)
    (outer : List Expr := []) :
    Option (String × List String × String × String) :=
  let invs := lc.invariants.filterMap toLeanProp
  -- Phase 3 #9: the function's `#[requires]` are in scope for every loop
  -- obligation; surface them in the hypotheses (monotonic — see `loopVCGoals`).
  let outerStrs := outer.filterMap toLeanProp
  let guardStr := (lc.guard.bind toLeanProp).getD "<guard>"
  let variantStr := (lc.variant.bind toLeanProp).getD "<variant>"
  let variantBody := (lc.variant.map (substContract lc.body)).bind toLeanProp |>.getD "<variant'>"
  let omegaDone := provedVCs.contains (loopVCKey qualName lc.line oblId)
  let status := fun (leanOp : Bool) =>
    if omegaDone && !leanOp then "proved_by_kernel_decision"
    else if omegaDone && leanOp then "arithmetic_proved"
    else "planned"
  match oblId with
  | "O1" => (genInitVC lc extraLets outer).map fun g =>
      ("invariant_init", outerStrs ++ ["loop-entry state (counter at its initializer)"], g, status false)
  | "O2" => some ("invariant_preservation", outerStrs ++ invs ++ [guardStr],
      " ∧ ".intercalate ((lc.invariants.map (substContract lc.body)).filterMap toLeanProp), status true)
  | "O3" => (genExitVC lc ensures retExpr outer).map fun g =>
      ("loop_exit_post_link", outerStrs ++ invs ++ [s!"¬({guardStr})"], g, status false)
  | "O4" => if lc.variant.isSome then
      some ("variant_nonnegative", outerStrs ++ invs ++ [guardStr], s!"0 ≤ {variantStr}", status false) else none
  | "O5" => if lc.variant.isSome then
      some ("variant_decreases", outerStrs ++ invs ++ [guardStr], s!"{variantBody} < {variantStr}", status false) else none
  | _ => none

/-! ## Verification-condition schema v1 (Phase 2 foundation)

A single, project-wide, machine-readable view over every obligation the compiler
generates. Each VC carries the schema-v1 fields: a stable `id`, source `loc`
(span), `kind`, separated `hypotheses` + `conclusion`, the `origin` function,
`dependencies` (proof links it leans on), an `arith_profile`, and the
`expected_discharge` mode. This is deliberately a DESCRIPTION of the obligation
and which backend *should* own it — it does not itself run any solver. Discharge
status stays in the existing per-kind reports; keeping the schema separate is
what stops a solver from silently becoming a "proved" path. -/

def vcSchemaVersion : Nat := 1

/-- The ONE obligation record (Phase 3 #18d). Formerly two types — `Report.VC`
    and `ObligationCore.Obligation` — now a single struct hosted here (the module
    `collectVCs` lives in; `ObligationCore` imports `Report`, so the canonical
    record must live at or below `Report`). `ObligationCore.Obligation` is an
    `abbrev` of this, and `VC` is kept as a compatibility alias for the existing
    discharge/schema/render call sites. The fields after `leanReplay` are the
    ledger-view fields (formerly ObligationCore-only); they default empty so a
    plain VC literal is unchanged and the VC reports never observe them. -/
structure Obligation where
  id            : String
  kind          : String       -- ∈ kindVocabulary
  fn            : String
  file          : String
  line          : Nat
  hypotheses    : List String
  conclusion    : String
  origin        : String
  dependencies  : List String
  arithProfile  : String       -- constant | linear | bitvector | nonlinear | refinement | operational | unsupported
  dischargeMode : String       -- constant_fold | omega | bv_decide | lean | smt | none
  -- discharge OUTCOME (filled in by `dischargeVCs` after the backends run):
  status        : String       -- ∈ statusVocabulary
  engine        : String       -- constant_fold | omega | bv_decide | lean | "" (none yet)
  -- concrete source-level counterexample (var → value), when a solver returned `sat`:
  counterexample : List (String × String) := []
  -- external-solver provenance (set only for SMT-routed VCs), for determinism/replay:
  smtHash       : String := ""  -- stable digest of the exact SMT-LIB query
  smtQuery      : String := ""  -- the SMT-LIB script itself (the replay artifact)
  solver        : String := ""  -- solver identity + version, e.g. "z3 4.16.0" (when run)
  leanReplay    : String := ""  -- the standalone Lean replay theorem (the artifact to kernel-check)
  -- ledger-view fields (Phase 3 #18d; formerly ObligationCore.Obligation-only):
  variables      : List String := []   -- typed variables in scope (names)
  allowedEngines : List String := []   -- which backends may discharge it
  replay         : String := ""        -- human replay hint
  policyImpact   : String := ""        -- release-policy consequence of `status`

/-- Compatibility alias: `VC` is the obligation record, kept for the existing
    discharge/schema/render call sites. -/
abbrev VC := Obligation

/-- Split a compiler-generated goal string `∀ (vars : T), (h1) → h2 → (concl)`
    into (hypotheses, conclusion): drop the binder, split the body on top-level
    ` → `, the conclusion is the final component. The goal strings are
    compiler-generated with a regular shape, so this is exact, not heuristic. -/
def splitVCGoal (g : String) : List String × String :=
  let body :=
    if g.startsWith "∀" then
      match g.splitOn "), " with
      | _ :: rest => "), ".intercalate rest
      | [] => g
    else g
  let parts := body.splitOn " → "
  match parts.reverse with
  | concl :: hypsRev => (hypsRev.reverse, concl)
  | [] => ([], body)

/-- Function qualname embedded in a VC key: strip the `#suffix` and any `@line`. -/
private def vcFnOfKey (key : String) : String :=
  let beforeHash := (key.splitOn "#").head?.getD key
  (beforeHash.splitOn "@").head?.getD beforeHash

/-- Contract-clause diagnostics for the ledger (Phase 3 #10): every `#[requires]`/
    `#[ensures]`/`#[invariant]`/`#[variant]` clause that fails validation surfaces
    as a ledger obligation — `impure_contract_call` when it calls a
    capability-requiring function, else `invalid_contract_expression` (unknown
    name, non-total construct, …). Status is `ineligible`: such a clause is not a
    provable obligation, it is a malformed contract the audit must show loudly.
    Mirrors the checks `contractsReport`/`loopContractSection` already render. -/
def contractClauseDiagnostics (modules : List Module) :
    List (String × String × String × String × List String) := Id.run do
  let callables := callableContractNames modules
  let consts := contractConstNames modules
  let impures := impureFnNames modules
  let mut out : List (String × String × String × String × List String) := []
  let diag := fun (allowed : List String) (e : Expr) =>
    let invalid := validateContractExpr allowed callables e
    let impure := (contractImpureCalls impures e).map (fun fn =>
      s!"impure call '{fn}' — spec/ghost must be pure and total (no capabilities)")
    (invalid, impure)
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let paramVars := f.params.map (·.name)
    let preVars := (paramVars ++ consts).eraseDups
    let postVars := (paramVars ++ ["result"] ++ localNamesB f.body ++ consts).eraseDups
    let loopVars := (paramVars ++ localNamesB f.body ++ consts).eraseDups
    let emit := fun (acc : List (String × String × String × String × List String))
        (key clauseTxt : String) (allowed : List String) (e : Expr) =>
      let (invalid, impure) := diag allowed e
      if !impure.isEmpty then
        acc ++ [(key, "impure_contract_call", fq, clauseTxt, impure)]
      else if !invalid.isEmpty then
        acc ++ [(key, "invalid_contract_expression", fq, clauseTxt, invalid.eraseDups)]
      else acc
    let mut ri := 1
    for r in f.requires do
      out := emit out s!"{fq}#req_diag{ri}" s!"requires {Concrete.fmtExpr r}" preVars r; ri := ri + 1
    let mut oi := 1
    for e in f.ensures do
      out := emit out s!"{fq}#ens_diag{oi}" s!"ensures {Concrete.fmtExpr e}" postVars e; oi := oi + 1
    for lc in f.loopContracts do
      let mut ii := 1
      for inv in lc.invariants do
        out := emit out s!"{fq}@{lc.line}#inv_diag{ii}" s!"invariant {Concrete.fmtExpr inv}" loopVars inv; ii := ii + 1
      match lc.variant with
      | some v => out := emit out s!"{fq}@{lc.line}#var_diag" s!"variant {Concrete.fmtExpr v}" loopVars v
      | none => pure ()
  return out

/-- Collect the project-wide VC schedule. Covers preconditions (call sites),
    postconditions (`#[ensures]`), asserts, vacuity, the runtime-safety
    obligations (array bounds, div/mod nonzero, `#[overflow_checked]`), the
    loop obligations (O1 init, O2 preservation, O3 exit→post, O4/O5 variant),
    and the contract-clause diagnostics (Phase 3 #10). -/
def collectVCs (modules : List Module) (locMap : FnLocMap)
    (registry : ProofRegistry) : List VC := Id.run do
  let loc := fun (fq : String) =>
    match locMap.find? (·.qualName == fq) with
    | some e => (e.file, e.fnSpan.line)
    | none => ("", 0)
  let mkVC := fun (id kind fq file : String) (line : Nat) (hyps : List String)
      (concl origin : String) (deps : List String) (profile mode status engine : String) =>
    ({ id := id, kind := kind, fn := fq, file := file, line := line, hypotheses := hyps,
       conclusion := concl, origin := origin, dependencies := deps,
       arithProfile := profile, dischargeMode := mode, status := status, engine := engine } : VC)
  -- a VC whose discharge cannot be decided without running a backend is "planned";
  -- the constant-fold and lean-link verdicts ARE decidable here and set directly.
  let mkSplit := fun (key kind profile mode : String) (goal : String) (deps : List String) =>
    let (hyps, concl) := splitVCGoal goal
    let fq := vcFnOfKey key
    let (file, line) := loc fq
    mkVC key kind fq file line hyps concl s!"{kind} in {fq}" deps profile mode "planned" ""
  let mut out : List VC := []
  -- preconditions at call sites (structural: includes the constant-decided ones).
  for o in callSiteObligations modules do
    let (file, line) := loc o.caller
    let concl := (toLeanProp o.specExpr).getD (Concrete.fmtExpr o.specExpr)
    let hyps := o.hyps.filterMap toLeanProp
    let (profile, mode, status, engine) := match o.baseStatus with
      | "proved_at_callsite" => ("constant", "constant_fold", "proved_by_kernel_decision", "constant_fold")
      | "failed_at_callsite" => ("constant", "constant_fold", "counterexample", "constant_fold")
      | _ => match o.leanGoal with
             | some _ => ("bitvector", "bv_decide", "planned", "")
             | none   => if (toLeanProp o.specExpr).isSome then ("linear", "omega", "planned", "") else ("unsupported", "none", "unproven", "")
    out := out ++ [mkVC o.key "precondition" o.caller file line hyps concl
      s!"precondition of {o.callStr} in {o.caller}" [] profile mode status engine]
  -- asserts and vacuity (goal-string generators; omega decides at discharge time).
  for (k, g) in assertGoals modules do out := out ++ [mkSplit k "assert" "linear" "omega" g []]
  -- assume facts: trusted assumptions surfaced in the one ledger as `assumed`
  -- (never a proof; no goal, so they cannot launder trust into kernel evidence).
  for (k, concl, hyps) in assumeFacts modules do
    let fq := vcFnOfKey k
    let (file, line) := loc fq
    out := out ++ [mkVC k "assume" fq file line hyps concl s!"assume in {fq}" [] "operational" "none" "assumed" "assumed"]
  for (k, g) in vacuityGoals modules do out := out ++ [mkSplit k "vacuity" "linear" "omega" g []]
  -- constant-false preconditions are vacuous too, but the constant folder (not
  -- omega) decides them — `vacuityGoals` skips them because `false` does not
  -- lower to a Lean prop. Surface them in the ledger as constant-fold vacuity so
  -- the obligation ledger is the complete source of vacuous functions (Phase 3
  -- #14: policy reads vacuity from the ledger, not a side channel).
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    if f.requires.any (fun r => cEvalBool r == some false) then
      let (file, line) := loc fq
      out := out ++ [mkVC s!"{fq}#requires_vac" "vacuity" fq file line []
        "False" s!"vacuity in {fq}" [] "constant" "constant_fold" "proved_by_kernel_decision" "constant_fold"]
  -- a constant runtime-safety verdict → kernel-decided here; else planned/omega/bv.
  let constStatus := fun (cv : Option Bool) =>
    match cv with
    | some true  => ("proved_by_kernel_decision", "constant_fold")
    | some false => ("counterexample", "constant_fold")
    | none       => ("unproven", "")
  -- array bounds.
  for o in boundsObligations modules do
    let (file, line) := loc o.fnQual
    let (hyps, concl, profile, mode, status, engine) := match o.leanGoal with
      | some g => let (h, c) := splitVCGoal g; (h, c, "linear", "omega", "planned", "")
      | none =>
        let c := s!"0 ≤ {Concrete.fmtExpr o.idxExpr} ∧ {Concrete.fmtExpr o.idxExpr} < {o.size}"
        let (st, en) := constStatus o.closedVerdict
        if o.closedVerdict.isSome then ([], c, "constant", "constant_fold", st, en) else ([], c, "unsupported", "none", st, en)
    out := out ++ [mkVC o.key "array_bounds" o.fnQual file line hyps concl
      s!"index {o.arrName}[{Concrete.fmtExpr o.idxExpr}] (size {o.size}) in {o.fnQual}" [] profile mode status engine]
  -- div/mod nonzero.
  for o in divObligations modules do
    let (file, line) := loc o.fnQual
    let (hyps, concl, profile, mode, status, engine) := match o.leanGoal with
      | some g => let (h, c) := splitVCGoal g; (h, c, "linear", "omega", "planned", "")
      | none =>
        let c := s!"{Concrete.fmtExpr o.divExpr} ≠ 0"
        let (st, en) := constStatus o.closedVerdict
        if o.closedVerdict.isSome then ([], c, "constant", "constant_fold", st, en) else ([], c, "unsupported", "none", st, en)
    out := out ++ [mkVC o.key "div_nonzero" o.fnQual file line hyps concl
      s!"{if o.isMod then "%" else "/"} divisor {Concrete.fmtExpr o.divExpr} in {o.fnQual}" [] profile mode status engine]
  -- overflow (omega tier, then the interval-gated bv_decide fallback, then const).
  for o in overflowObligations modules do
    let (file, line) := loc o.fnQual
    let (hyps, concl, profile, mode, status, engine) := match o.leanGoal, o.bvGoal with
      | some g, _ => let (h, c) := splitVCGoal g; (h, c, "linear", "omega", "planned", "")
      | none, some g => let (h, c) := splitVCGoal g; (h, c, "bitvector", "bv_decide", "planned", "")
      | none, none =>
        let c := s!"{o.lo} ≤ {Concrete.fmtExpr o.opExpr} ∧ {Concrete.fmtExpr o.opExpr} ≤ {o.hi}"
        let (st, en) := constStatus o.closedVerdict
        if o.closedVerdict.isSome then ([], c, "constant", "constant_fold", st, en) else ([], c, "unsupported", "none", st, en)
    out := out ++ [mkVC o.key "no_overflow" o.fnQual file line hyps concl
      s!"no-overflow of {Concrete.fmtExpr o.opExpr} in [{o.lo}, {o.hi}] in {o.fnQual}" [] profile mode status engine]
  -- loop obligations (reuse loopObInfo for kind/hyps/conclusion). Discharge is
  -- decided by omega (O1/O3/O4/O5) or stays operational/lean (O2) → planned here.
  for (pfx, f) in (modules.flatMap allFunctions).filter (fun (_, f) => !f.loopContracts.isEmpty) do
    let fq := pfx ++ f.name
    let (file, _) := loc fq
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    let reg := registry.find? (·.function == fq)
    let invDep := match reg with | some e => if e.coverage == "invariant" && !e.proof.isEmpty then [e.proof] else [] | none => []
    for lc in f.loopContracts do
      for oid in ["O1", "O2", "O3", "O4", "O5"] do
        match loopObInfo lc oid fq f.ensures extraLets retExpr [] f.requires with
        | some (kind, hyps, concl, _) =>
          let (profile, mode) := if oid == "O2" then ("operational", "lean") else ("linear", "omega")
          let deps := if oid == "O2" then invDep else []
          out := out ++ [mkVC (loopVCKey fq lc.line oid) kind fq file lc.line hyps concl
            s!"{kind} (loop @ line {lc.line}) in {fq}" deps profile mode "planned" ""]
        | none => pure ()
  -- postconditions (#[ensures]) discharged by registered Lean proofs.
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    if f.ensures.isEmpty then continue
    let (file, line) := loc fq
    let reg := registry.find? (·.function == fq)
    let dep := match reg with
      | some e => (match e.ensuresProof with | some t => [t] | none => []) ++ (if !e.proof.isEmpty then [e.proof] else [])
      | none => []
    -- a one-direction registry proof discharges ONE direction of the
    -- postcondition → `partial`, not a full `proved_by_lean` (Phase 3 #10).
    let onePartial := match reg with
      | some e => e.ensuresProof.isNone && e.coverage == "one_direction" && !e.proof.isEmpty
      | none => false
    let mut ei := 0
    for ens in f.ensures do
      let (mode, status, engine) :=
        if dep.isEmpty then ("none", "missing", "")
        else if onePartial then ("lean", "partial", "lean")
        else ("lean", "proved_by_lean", "lean")
      out := out ++ [mkVC s!"{fq}#ensures{ei}" "postcondition" fq file line
        (f.requires.map Concrete.fmtExpr) (Concrete.fmtExpr ens) s!"postcondition in {fq}"
        dep "refinement" mode status engine]
      ei := ei + 1
  -- contract-clause diagnostics: malformed #[requires]/#[ensures]/#[invariant]/
  -- #[variant] clauses surface as `ineligible` ledger obligations (Phase 3 #10).
  for (key, kind, fq, clauseTxt, issues) in contractClauseDiagnostics modules do
    let (file, line) := loc fq
    out := out ++ [mkVC key kind fq file line [] (", ".intercalate issues)
      s!"{clauseTxt} in {fq}" [] "unsupported" "none" "ineligible" ""]
  return out

/-! ### Backend discharge adapters (Phase 3 #13)

Every backend that can change a VC's status is expressed as ONE adapter type with
a DECLARED set of evidence classes it may produce. `DischargeAdapter.fold` applies
a backend's results only to VCs in the adapter's precondition status AND only when
the resulting class is in the adapter's `allowed` set — so a backend can NEVER emit
a class it does not own (the evidence-class firewall is structural, not a
convention). omega/bv_decide own the kernel classes, external SMT owns only the
solver classes, Lean-replay owns only `proved_by_lean_replay`. -/

structure DischargeAdapter where
  engine     : String              -- the engine string stamped on a touched VC
  allowed    : List String         -- the ONLY statuses this backend may assign
  actsOn     : String → Bool       -- precondition: which CURRENT status it may act on
  finalize   : String → String → String  -- (vcKind, rawResultClass) → final status
  setsSolver : Bool := false       -- whether to stamp the solver-identity field

/-- Apply a backend's `(id, rawClass, model)` results through the adapter
    firewall. A result is applied to VC `v` only when `v.id` matches, `actsOn
    v.status` holds, and the finalized class is in `allowed`; otherwise `v` is
    left exactly as is. This is the single choke point through which any
    status-changing backend must pass. -/
def DischargeAdapter.fold (a : DischargeAdapter) (solverId : String)
    (vcs : List VC) (results : List (String × String × List (String × String))) : List VC :=
  vcs.map fun v =>
    match results.find? (·.1 == v.id) with
    | none => v
    | some (_, raw, model) =>
      let cls := a.finalize v.kind raw
      if a.actsOn v.status && a.allowed.contains cls then
        { v with status := cls, engine := a.engine,
                 counterexample := if cls == "counterexample" then model else v.counterexample,
                 solver := if a.setsSolver then solverId else v.solver }
      else v

/-- omega: a kernel decision procedure. Owns `proved_by_kernel_decision`, plus
    `arithmetic_proved` for loop-invariant preservation (where omega closes only
    the arithmetic half). Acts only on `planned` VCs. -/
def omegaAdapter : DischargeAdapter :=
  { engine := "omega", allowed := ["proved_by_kernel_decision", "arithmetic_proved"],
    actsOn := (· == "planned"),
    finalize := fun kind _ =>
      if kind == "loop_invariant_preservation" then "arithmetic_proved"
      else "proved_by_kernel_decision" }

/-- bv_decide: a kernel bit-vector decision procedure. Owns only
    `proved_by_kernel_decision`. Acts only on `planned` VCs. -/
def bvAdapter : DischargeAdapter :=
  { engine := "bv_decide", allowed := ["proved_by_kernel_decision"],
    actsOn := (· == "planned"), finalize := fun _ _ => "proved_by_kernel_decision" }

/-- External SMT: opt-in, untrusted relative to the kernel. Owns ONLY the solver
    classes — it can never produce a kernel/Lean class. Acts only on VCs the
    kernel tiers left `unproven`, and stamps the solver identity. -/
def smtAdapter : DischargeAdapter :=
  { engine := "smt:z3",
    allowed := ["solver_trusted", "counterexample", "unknown", "timeout", "solver_error"],
    actsOn := (· == "unproven"), finalize := fun _ raw => raw, setsSolver := true }

/-- Lean replay: an independent kernel re-check of a solver_trusted VC. Owns only
    `proved_by_lean_replay`, and acts only on `solver_trusted` VCs. -/
def replayAdapter : DischargeAdapter :=
  { engine := "lean:omega", allowed := ["proved_by_lean_replay"],
    actsOn := (· == "solver_trusted"), finalize := fun _ _ => "proved_by_lean_replay" }

/-- Constant fold: decides literal/arithmetic obligations at collect time. Owns
    `proved_by_kernel_decision` (in range) and `counterexample` (a constant
    violation). Assigned directly in `collectVCs`; declared here so its owned
    classes are part of the one firewall spec. -/
def constantFoldAdapter : DischargeAdapter :=
  { engine := "constant_fold", allowed := ["proved_by_kernel_decision", "counterexample"],
    actsOn := (· == "planned"), finalize := fun _ raw => raw }

/-- Linked Lean theorem: a registered in-source proof. Owns only `proved_by_lean`. -/
def linkedLeanAdapter : DischargeAdapter :=
  { engine := "lean", allowed := ["proved_by_lean", "partial"],
    actsOn := (· == "planned"), finalize := fun _ raw => raw }

/-- Oracle / differential test evidence. Owns only `tested_by_oracle` — never a
    proof class. -/
def oracleAdapter : DischargeAdapter :=
  { engine := "oracle", allowed := ["tested_by_oracle"],
    actsOn := (· == "planned"), finalize := fun _ _ => "tested_by_oracle" }

/-- Runtime enforcement: a runtime check or capability gate. Owns
    `runtime_checked` / `enforced` — NEVER a static proof class. -/
def runtimeAdapter : DischargeAdapter :=
  { engine := "runtime", allowed := ["runtime_checked", "enforced"],
    actsOn := (· == "planned"), finalize := fun _ raw => raw }

/-- Assumption / trust boundary: `assume(...)` and `#[trusted]`. Owns `assumed` /
    `trusted` — a trust escape hatch, NEVER a proof class. -/
def assumptionAdapter : DischargeAdapter :=
  { engine := "assumed", allowed := ["assumed", "trusted"],
    actsOn := fun _ => true, finalize := fun _ raw => raw }

/-- Every backend adapter, for the firewall properties below and documentation. -/
def dischargeAdapters : List DischargeAdapter :=
  [constantFoldAdapter, omegaAdapter, bvAdapter, linkedLeanAdapter, replayAdapter,
   smtAdapter, oracleAdapter, runtimeAdapter, assumptionAdapter]

/-- The static-proof evidence classes — the ones an untrusted backend (SMT,
    runtime, oracle, assumption) must NEVER be able to emit. -/
def proofClasses : List String :=
  ["proved_by_kernel_decision", "proved_by_lean", "proved_by_lean_replay", "arithmetic_proved"]

/-! ### The evidence-class firewall, proved at compile time (Phase 3 #13)

These `example`s are kernel-checked proofs that no adapter can emit a class it
does not own — the firewall holds by construction, independent of any caller. -/

-- Untrusted backends declare NO static-proof class in their `allowed` set.
example : smtAdapter.allowed.all (fun c => !proofClasses.contains c) = true := rfl
example : runtimeAdapter.allowed.all (fun c => !proofClasses.contains c) = true := rfl
example : assumptionAdapter.allowed.all (fun c => !proofClasses.contains c) = true := rfl
example : oracleAdapter.allowed.all (fun c => !proofClasses.contains c) = true := rfl

private def fwVC (st : String) : VC :=
  { id := "f#x", kind := "no_overflow", fn := "f", file := "", line := 0,
    hypotheses := [], conclusion := "c", origin := "", dependencies := [],
    arithProfile := "nonlinear", dischargeMode := "smt", status := st, engine := "" }

-- external SMT may NOT emit a kernel class onto an unproven VC: rejected.
example : (smtAdapter.fold "z3" [fwVC "unproven"]
    [("f#x", "proved_by_kernel_decision", [])]).map (·.status) = ["unproven"] := rfl
-- external SMT may emit its OWN class.
example : (smtAdapter.fold "z3" [fwVC "unproven"]
    [("f#x", "solver_trusted", [])]).map (·.status) = ["solver_trusted"] := rfl
-- external SMT may NOT overwrite a kernel-proved VC (wrong precondition status).
example : (smtAdapter.fold "z3" [fwVC "proved_by_kernel_decision"]
    [("f#x", "counterexample", [])]).map (·.status) = ["proved_by_kernel_decision"] := rfl
-- omega cannot be coerced into a solver class: it always finalizes to its own.
example : (omegaAdapter.fold "" [fwVC "planned"]
    [("f#x", "solver_trusted", [])]).map (·.status) = ["proved_by_kernel_decision"] := rfl
-- replay acts ONLY on solver_trusted: a planned VC is untouched.
example : (replayAdapter.fold "" [fwVC "planned"]
    [("f#x", "x", [])]).map (·.status) = ["planned"] := rfl

def dischargeVCs (vcs : List VC) (omegaProved bvProved : List String) : List VC :=
  -- omega then bv_decide through the adapter firewall; whatever stays `planned`
  -- afterwards was discharged by no kernel tier → `unproven`.
  let idResults := fun (ids : List String) => ids.map (fun k => (k, "proved", ([] : List (String × String))))
  let afterOmega := omegaAdapter.fold "" vcs (idResults omegaProved)
  let afterBv := bvAdapter.fold "" afterOmega (idResults bvProved)
  afterBv.map fun v => if v.status == "planned" then { v with status := "unproven" } else v

/-- Mark the SMT-eligible VCs as routed to the external solver: `arith_profile`
    becomes `nonlinear`, `expected_discharge` becomes `smt`, and the determinism /
    replay provenance is attached — a stable digest of the exact SMT-LIB query
    (`smtHash`) and the query text itself (`smtQuery`, the replay artifact).
    Applied ONLY when the external-SMT path is engaged (an explicit flag) — so by
    default no VC ever advertises `smt`. Does not change `status`. -/
def markSmtEligible (vcs : List VC) (smtGoals : List (String × String))
    (replayGoals : List (String × String) := []) : List VC :=
  vcs.map fun v =>
    match smtGoals.find? (·.1 == v.id) with
    | some (_, script) =>
      { v with arithProfile := "nonlinear", dischargeMode := "smt",
               smtHash := shortHash script, smtQuery := script,
               leanReplay := (replayGoals.find? (·.1 == v.id)).map (·.2) |>.getD "" }
    | none => v

/-- Fold Lean-replay results into the VC schedule. `replayed` lists the VC ids a
    kernel-checked Lean tactic INDEPENDENTLY closed. Such a VC graduates from
    `solver_trusted` to `proved_by_lean_replay` (engine `lean:omega`) — the external
    solver is no longer part of the claim, so it is NOT subject to the solver-evidence
    policy. Applied only to VCs currently `solver_trusted`; everything else is left
    exactly as is, so the class boundary stays crisp. -/
def foldReplayResults (vcs : List VC) (replayed : List String) : List VC :=
  replayAdapter.fold "" vcs (replayed.map (fun k => (k, "replayed", [])))

/-- Fold external-solver results into the VC schedule. A result class
    (`solver_trusted` / `counterexample` / `unknown` / `timeout` / `solver_error`)
    is applied ONLY to a VC the kernel-checked tiers left `unproven` — so an
    external solver can NEVER override or be confused with a `proved_by_kernel_decision`
    or `proved_by_lean` result. The engine records the solver; a `counterexample`
    carries the solver's model mapped back to source variable names; `solver`
    records the exact solver identity+version that produced the verdict. -/
def foldSmtResults (vcs : List VC) (results : List (String × String × List (String × String)))
    (solverId : String) : List VC :=
  smtAdapter.fold solverId vcs results

/-- Human-readable VC schedule (post-discharge), grouped by originating function.
    `vcs` is the output of `collectVCs` after `dischargeVCs` has folded in results. -/
def vcsReport (vcs : List VC) : String := Id.run do
  if vcs.isEmpty then return "=== Verification Conditions (schema v1) ===\n\n(no VCs generated)"
  let mut out := "=== Verification Conditions (schema v1) ==="
  let fns := (vcs.map (·.fn)).eraseDups
  for fq in fns do
    out := out ++ s!"\n\n{fq}"
    for v in vcs.filter (·.fn == fq) do
      let hyps := if v.hypotheses.isEmpty then "(none)" else " ∧ ".intercalate v.hypotheses
      let deps := if v.dependencies.isEmpty then "" else s!"\n      dependencies:  {", ".intercalate v.dependencies}"
      let eng := if v.engine.isEmpty then "" else s!" ({v.engine})"
      let cex := if v.counterexample.isEmpty then ""
        else s!"\n      counterexample:  {", ".intercalate (v.counterexample.map (fun (n, x) => s!"{n} = {x}"))}"
      let prov := if v.smtHash.isEmpty then ""
        else s!"\n      solver:  {if v.solver.isEmpty then "(not run)" else v.solver} | logic QF_NIA | timeout 5s | smtlib-sha {v.smtHash}"
          ++ (if v.leanReplay.isEmpty then "" else "\n      lean-replay:  artifact emitted (check with `--emit-lean-replay`); kernel-checks → proved_by_lean_replay")
      out := out ++ s!"\n  [{v.id}]  {v.kind}"
        ++ s!"\n      status:  {v.status}{eng}"
        ++ s!"\n      profile/expected:  {v.arithProfile} / {v.dischargeMode}"
        ++ s!"\n      hypotheses:  {hyps}"
        ++ s!"\n      conclusion:  {v.conclusion}{deps}{cex}{prov}"
  -- summary by status
  let proved := (vcs.filter (·.status == "proved_by_kernel_decision")).length
  let lean := (vcs.filter (·.status == "proved_by_lean")).length
  let arith := (vcs.filter (·.status == "arithmetic_proved")).length
  let cex := (vcs.filter (·.status == "counterexample")).length
  let unproven := (vcs.filter (fun v => v.status == "unproven" || v.status == "missing" || v.status == "planned")).length
  out := out ++ s!"\n\nTotal: {vcs.length} VCs — {proved} proved_by_kernel_decision, {lean} proved_by_lean, {arith} arithmetic_proved, {cex} counterexample, {unproven} outstanding"
  return out

/-- Compact VC-evidence summary for the audit report (the reviewer artifact):
    counts per evidence class, plus the audit-critical lines — every
    `solver_trusted` VC (solver identity + SMT-LIB hash; NOT kernel evidence) and
    every `counterexample` (source-level model). Empty when there are no VCs. -/
def vcAuditSummary (vcs : List VC) : String := Id.run do
  if vcs.isEmpty then return "(no verification conditions)"
  let count := fun (s : String) => (vcs.filter (·.status == s)).length
  let outstanding := (vcs.filter (fun v => v.status == "unproven" || v.status == "missing" || v.status == "planned")).length
  let mut out := s!"{vcs.length} verification conditions:"
  out := out ++ s!"\n  proved_by_kernel_decision:  {count "proved_by_kernel_decision"}  (omega / bv_decide / constant — kernel-checked)"
  out := out ++ s!"\n  proved_by_lean:             {count "proved_by_lean"}  (registered Lean proof)"
  out := out ++ s!"\n  proved_by_lean_replay:      {count "proved_by_lean_replay"}  (solver result replayed in Lean)"
  out := out ++ s!"\n  arithmetic_proved:          {count "arithmetic_proved"}  (omega closed the arithmetic half; operational step needs Lean)"
  out := out ++ s!"\n  solver_trusted:             {count "solver_trusted"}  (external SMT — solver in the TCB, NOT kernel evidence)"
  out := out ++ s!"\n  counterexample:             {count "counterexample"}  (non-proof)"
  out := out ++ s!"\n  outstanding:                {outstanding}  (unproven / missing)"
  -- audit-critical detail: solver-trusted evidence and counterexamples by name.
  let trusted := vcs.filter (·.status == "solver_trusted")
  if !trusted.isEmpty then
    out := out ++ "\n  external-solver evidence (review: solver in the trusted base):"
    for v in trusted do
      out := out ++ s!"\n    {v.id}  [{if v.solver.isEmpty then "solver" else v.solver}, smtlib-sha {v.smtHash}]"
  let cexs := vcs.filter (·.status == "counterexample")
  if !cexs.isEmpty then
    out := out ++ "\n  counterexamples (non-proofs):"
    for v in cexs do
      let m := if v.counterexample.isEmpty then "" else " — " ++ ", ".intercalate (v.counterexample.map (fun (n, x) => s!"{n} = {x}"))
      out := out ++ s!"\n    {v.id}{m}"
  -- by default the audit does not invoke an external solver; note where SMT lives.
  if (count "solver_trusted") == 0 && (count "counterexample") == 0 then
    out := out ++ "\n  (external-solver evidence is opt-in: see `--report vcs --smt`)"
  return out

/-- `concrete prove <file> <fn> --json`: the primary machine-readable proof
    context. Same data as `proveReport`, structured, with `next_actions`. -/
def proveReportJson (pc : Concrete.ProofCore) (registry : ProofRegistry)
    (modules : List Module) (qualName : String) (provedVCs : List String)
    (inputPath schemaVersion : String) : String := Id.run do
  let esc := fun (s : String) => s.foldl (fun a c => a ++ (match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | '\t' => "\\t" | c => c.toString)) ""
  let q := fun (s : String) => "\"" ++ esc s ++ "\""
  let jarr := fun (xs : List String) => "[" ++ ", ".intercalate xs ++ "]"
  let qarr := fun (xs : List String) => jarr (xs.map q)
  let replay := s!"concrete prove {inputPath} {qualName} --replay"
  -- next_actions object builder
  let action := fun (kind cmd fmt resolves : String) =>
    String.join ["{\"kind\": ", q kind, ", \"command\": ", q cmd,
                 ", \"output_format\": ", q fmt, ", \"resolves\": ", q resolves, "}"]
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName) |>.map Prod.snd
  let head := s!"  \"schema_version\": {q schemaVersion},\n  \"function\": {q qualName},"
  match pc.entries.find? (·.qualName == qualName) with
  | none =>
    match pc.excluded.find? (·.qualName == qualName) with
    | some x =>
      let reasons := x.eligibility.sourceReasons ++ x.eligibility.profileReasons
      return String.join [
        "{\n", head, "\n",
        "  \"eligible\": false,\n",
        s!"  \"exclusion_reason\": {q (", ".intercalate reasons)},\n",
        s!"  \"body_fingerprint\": {q x.fingerprint},\n",
        "  \"proof_link\": null,\n",
        "  \"status\": \"ineligible\",\n",
        "  \"evidence_class\": \"ineligible\",\n",
        "  \"obligations\": [],\n",
        s!"  \"next_actions\": [{action "open_docs" "see docs/PROFILES.md" "text" "ineligible"}]\n",
        "}" ]
    | none =>
      return String.join ["{\n", head, s!"\n  \"error\": {q s!"no function '{qualName}' in ProofCore"},\n  \"next_actions\": []\n", "}"]
  | some e =>
    let regEntry := registry.find? (fun re => re.function == e.qualName)
    -- status + evidence from the obligation
    let obl := pc.obligations.find? (·.functionId.qualName == qualName)
    let status := match obl with | some o => o.status.canonical | none => "missing"
    let origin := match regEntry with
      | some re => if re.sourceLinked then "source_linked" else "hardcoded"
      | none => "hardcoded"
    -- proof_link
    let linkJson := match regEntry with
      | some re => String.join [
          "{\"spec\": ", q re.spec, ", \"proof_by\": ", q re.proof,
          ", \"ensures_proof\": ", (match re.ensuresProof with | some t => q t | none => "null"),
          ", \"coverage\": ", q re.coverage,
          ", \"proof_fingerprint\": ", q (shortHash re.bodyFingerprint),
          ", \"origin\": ", q origin, "}" ]
      | none => "null"
    -- obligations (loop VCs)
    let f := astFn?.getD { name := e.bareName, params := [], retTy := .unit, body := [] }
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    let mut obls : List String := []
    -- stable id: "<qual>@<line>#<Ox>" (same key as --report contracts / --replay).
    let oblObj := fun (id : String) (line : Int) (kind : String) (hyps : List String) (concl st : String) =>
      String.join ["{\"id\": ", q id, ", \"kind\": ", q kind, ", \"status\": ", q st,
        ", \"source_line\": ", toString line,
        ", \"hypotheses\": ", qarr hyps, ", \"conclusion\": ", q concl, "}"]
    for lc in f.loopContracts do
      for oid in ["O1", "O2", "O3", "O4", "O5"] do
        match loopObInfo lc oid e.qualName f.ensures extraLets retExpr provedVCs with
        | some (kind, hyps, concl, st) =>
          obls := obls ++ [oblObj (loopVCKey e.qualName lc.line oid) (Int.ofNat lc.line) kind hyps concl st]
        | none => pure ()
    -- the function postcondition (#[ensures]) as an obligation
    for ens in f.ensures do
      let reqHyps := f.requires.map Concrete.fmtExpr
      obls := obls ++ [oblObj s!"{e.qualName}#ensures" (Int.ofNat (f.span.line)) "ensures" reqHyps (Concrete.fmtExpr ens) status]
    -- proofkit hints
    let feats := (e.fn.body.flatMap proveStmtFeatures).eraseDups
    let mut hints : List String := []
    if feats.contains "loop" || !f.loopContracts.isEmpty then hints := hints ++ ["Concrete.ProofKit.Loops"]
    if feats.contains "array" then hints := hints ++ ["Concrete.ProofKit.Array"]
    if feats.contains "bitvec" then hints := hints ++ ["Concrete.ProofKit.BitVec"]
    if feats.contains "call" then hints := hints ++ ["Concrete.ProofKit.Calls"]
    let leanName := leanIdent (e.qualName.splitOn "." |>.getLast!)
    -- next_actions by status
    let na := match status with
      | "missing" => [
          action "show_obligation" s!"concrete prove {inputPath} {qualName} --show-obligation <id>" "text" "missing",
          action "emit_link" s!"concrete prove {inputPath} {qualName} --emit-link" "text" "missing" ]
      | "stale" => [
          action "emit_link" s!"concrete prove {inputPath} {qualName} --emit-link" "text" "stale",
          action "check_proofs" s!"concrete {inputPath} --report check-proofs" "text" "stale" ]
      | "blocked" => [ action "open_docs" "see docs/PROOF_WORKFLOW.md (extraction gates)" "text" "blocked" ]
      | "proved" => [
          action "check_proofs" s!"concrete {inputPath} --report check-proofs" "text" "proved",
          action "replay" replay "text" "proved",
          action "run_audit" s!"concrete audit {inputPath}" "text" "proved" ]
      | _ => [ action "run_audit" s!"concrete audit {inputPath}" "text" status ]
    return String.join [
      "{\n", head, "\n",
      "  \"eligible\": true,\n",
      "  \"exclusion_reason\": null,\n",
      s!"  \"body_fingerprint\": {q e.fingerprint},\n",
      s!"  \"proof_link\": {linkJson},\n",
      s!"  \"status\": {q status},\n",
      s!"  \"evidence_class\": {q (if status == "proved" then "proved_by_lean" else status)},\n",
      s!"  \"obligations\": {jarr obls},\n",
      s!"  \"replay_command\": {q replay},\n",
      s!"  \"proofkit_imports\": {qarr hints},\n",
      s!"  \"suggested_theorems\": {qarr [leanName ++ "_correct"]},\n",
      s!"  \"next_actions\": {jarr na}\n",
      "}" ]

/-- `concrete prove --emit-link`: print the in-source proof-link attribute block
    for `qual`, from its current registry/source-link data. Missing fields are
    emitted as commented placeholders so the author sees what still needs a name.
    Read-only: the author pastes the block above the function and deletes the
    JSON entry. -/
def emitProofLink (registry : ProofRegistry) (qual : String) : String :=
  match registry.find? (·.function == qual) with
  | none => s!"// no proof link or registry entry for `{qual}` — nothing to emit"
  | some e =>
    let req := fun (key val : String) =>
      if val.isEmpty then s!"// #[{key}(...)]   ← missing, fill in" else s!"#[{key}({val})]"
    let ens := match e.ensuresProof with
      | some t => s!"#[ensures_proof({t})]"
      | none => "// #[ensures_proof(...)]   ← none (omit if the postcondition is single-direction)"
    -- The registry entry's bodyFingerprint is the CURRENT body fingerprint;
    -- hashing it gives the staleness token to store in source.
    let fp := s!"#[proof_fingerprint(\"{shortHash e.bodyFingerprint}\")]"
    String.join [
      s!"// in-source proof link for `{qual}` — paste this block above the function.\n",
      req "spec" e.spec, "\n",
      req "proof_by" e.proof, "\n",
      ens, "\n",
      req "proof_coverage" e.coverage, "\n",
      fp, "\n" ]

/-- `concrete prove --show-obligation <id>`: print one generated loop obligation
    (O1/O2/O3/O4/O5) in full — source span, hypotheses, conclusion, current
    discharge status, suggested ProofKit imports, and the theorem shape to
    write (or a note that omega already closes it). `provedVCs` are the omega
    discharge results from the caller. -/
def showObligation (modules : List Module) (qualName oblId0 : String)
    (provedVCs : List String) : String := Id.run do
  -- Accept either the short id ("O4") or the stable id ("<qual>@<line>#O4").
  let oblId := (oblId0.splitOn "#").getLast!
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName)
    |>.map Prod.snd
  match astFn? with
  | none => return s!"no function `{qualName}` found."
  | some f =>
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    for lc in f.loopContracts do
      let invs := lc.invariants.filterMap toLeanProp
      let guardStr := (lc.guard.bind toLeanProp).getD "<guard>"
      let variantStr := (lc.variant.bind toLeanProp).getD "<variant>"
      let variantBody := (lc.variant.map (substContract lc.body)).bind toLeanProp |>.getD "<variant'>"
      -- (label, hypotheses, conclusion, theorem-shape-when-not-omega)
      let info : Option (String × List String × String × String) := match oblId with
        | "O1" => some ("invariant_init", ["loop-entry state (counter at its initializer)"],
                        (genInitVC lc extraLets).getD "?", "(none — omega closes it)")
        | "O2" => some ("invariant_preservation",
                        invs ++ [guardStr],
                        " ∧ ".intercalate ((lc.invariants.map (substContract lc.body)).filterMap toLeanProp),
                        (genPreservationShape lc qualName).getD "(operational step needs a Lean eval lemma)")
        | "O3" => some ("loop_exit_post_link", invs ++ [s!"¬({guardStr})"],
                        (genExitVC lc f.ensures retExpr).getD "(no groundable #[ensures] at loop exit)", "(none — omega closes it once grounded)")
        | "O4" => some ("variant_nonnegative", invs ++ [guardStr], s!"0 ≤ {variantStr}", "(none — omega closes it)")
        | "O5" => some ("variant_decreases", invs ++ [guardStr], s!"{variantBody} < {variantStr}", "(none — omega closes it)")
        | _ => none
      match info with
      | none => pure ()
      | some (label, hyps, concl, shape) =>
        let omegaDone := provedVCs.contains (loopVCKey qualName lc.line oblId)
        let leanOp := oblId == "O2"
        let status :=
          if omegaDone && !leanOp then "proved_by_kernel_decision (omega)"
          else if omegaDone && leanOp then "arithmetic step: proved_by_kernel_decision (omega); operational step: needs Lean"
          else "planned"
        let kitHint :=
          if leanOp then "Concrete.ProofKit.Loops (eval_while_count) for the operational step; Concrete.ProofKit.Eval"
          else "Concrete.ProofKit.Loops; the arithmetic leaf closes with omega — no ProofKit lemma needed"
        return String.join [
          s!"=== obligation {oblId} ({label}) for {qualName} ===\n\n",
          s!"source:      {qualName} loop @ line {lc.line}\n",
          s!"status:      {status}\n\n",
          "hypotheses:\n", String.join (hyps.map (s!"  {·}\n")),
          s!"\nconclusion:\n  {concl}\n\n",
          s!"ProofKit:    {kitHint}\n",
          s!"theorem shape:\n  {shape}\n" ]
    return s!"obligation `{oblId}` not found on {qualName} (loops expose O1/O2/O3/O4/O5)."

/-- JSON-escape and quote a string. Shared by the prove JSON emitters. -/
def jsonStr (s : String) : String :=
  "\"" ++ s.foldl (fun a c => a ++ (match c with
    | '"' => "\\\"" | '\\' => "\\\\" | '\n' => "\\n" | '\t' => "\\t" | c => c.toString)) "" ++ "\""

/-- `concrete prove --show-obligation <id> --json`: one obligation, structured.
    Carries the same stable id as `--json` / `--report contracts`. Accepts the
    short id ("O4") or the stable id ("<qual>@<line>#O4"). -/
def showObligationJson (modules : List Module) (qualName oblId0 : String)
    (provedVCs : List String) (inputPath : String) : String := Id.run do
  let oblId := (oblId0.splitOn "#").getLast!
  let q := jsonStr
  let qarr := fun (xs : List String) => "[" ++ ", ".intercalate (xs.map q) ++ "]"
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName) |>.map Prod.snd
  match astFn? with
  | none => return s!"\{\"error\": {q s!"no function '{qualName}'"}}"
  | some f =>
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    for lc in f.loopContracts do
      match loopObInfo lc oblId qualName f.ensures extraLets retExpr provedVCs with
      | none => pure ()
      | some (kind, hyps, concl, status) =>
        let shape := if oblId == "O2" then (genPreservationShape lc qualName).getD "" else ""
        let kit := if oblId == "O2" then ["Concrete.ProofKit.Loops", "Concrete.ProofKit.Eval"] else ["Concrete.ProofKit.Loops"]
        let na := if status == "planned" || status == "arithmetic_proved" then
            [String.join ["{\"kind\": \"emit_lean\", \"command\": ", q s!"concrete prove {inputPath} {qualName} --emit-lean", ", \"output_format\": \"text\", \"resolves\": ", q status, "}"]]
          else
            [String.join ["{\"kind\": \"replay\", \"command\": ", q s!"concrete prove {inputPath} {qualName} --replay", ", \"output_format\": \"text\", \"resolves\": \"proved\"}"]]
        return String.join [
          "{\n",
          s!"  \"id\": {q (loopVCKey qualName lc.line oblId)},\n",
          s!"  \"function\": {q qualName},\n",
          s!"  \"kind\": {q kind},\n",
          s!"  \"status\": {q status},\n",
          s!"  \"source_line\": {toString lc.line},\n",
          s!"  \"hypotheses\": {qarr hyps},\n",
          s!"  \"conclusion\": {q concl},\n",
          s!"  \"theorem_shape\": {q shape},\n",
          s!"  \"proofkit_imports\": {qarr kit},\n",
          s!"  \"next_actions\": [{", ".intercalate na}]\n",
          "}" ]
    return s!"\{\"error\": {q s!"obligation '{oblId}' not found on {qualName}"}}"

/-- `concrete prove --emit-link --json`: the link fields + the pasteable block. -/
def emitProofLinkJson (registry : ProofRegistry) (qual inputPath : String) : String :=
  let q := jsonStr
  match registry.find? (·.function == qual) with
  | none => s!"\{\"error\": {q s!"no proof link for '{qual}'"}}"
  | some e =>
    let block := emitProofLink registry qual
    String.join [
      "{\n",
      s!"  \"function\": {q qual},\n",
      s!"  \"spec\": {q e.spec},\n",
      s!"  \"proof_by\": {q e.proof},\n",
      s!"  \"ensures_proof\": {match e.ensuresProof with | some t => q t | none => "null"},\n",
      s!"  \"coverage\": {q e.coverage},\n",
      s!"  \"proof_fingerprint\": {q (shortHash e.bodyFingerprint)},\n",
      s!"  \"link_block\": {q block},\n",
      s!"  \"next_actions\": [\{\"kind\": \"paste_link\", \"command\": \"<paste link_block above the function in source>\", \"output_format\": \"none\", \"resolves\": \"missing\"}, \{\"kind\": \"check_proofs\", \"command\": {q s!"concrete {inputPath} --report check-proofs"}, \"output_format\": \"text\", \"resolves\": \"proved\"}]\n",
      "}" ]

/-- Static recipe (tactic, lemmas, note) for an obligation kind — the
    proof-recipe map (linear → omega; preservation → eval_while_count; overflow →
    bv_decide; arrays → ProofKit.Array; etc.). -/
def lemmaRecipeFor (kind : String) : String × List String × String :=
  match kind with
  | "invariant_init" | "variant_nonnegative" | "variant_decreases" | "loop_exit_post_link" =>
      ("omega", [], "linear-integer leaf — omega closes it (no lemma needed)")
  | "invariant_preservation" =>
      ("intros; (omega | eval)", ["Concrete.ProofKit.Loops.eval_while_count", "Concrete.ProofKit.Eval"],
       "arithmetic half closes with omega; operational step uses eval_while_count")
  | "array_bounds" =>
      ("intros; omega", ["Concrete.ProofKit.Array.lookupIndex_set_self", "Concrete.ProofKit.Array.set_in_counter_map"],
       "bound from #[requires]/invariant via omega; array lemmas for body updates")
  | "division_nonzero" => ("intros; omega", [], "divisor ≠ 0 follows from #[requires] via omega")
  | "integer_overflow" =>
      ("intros; bv_decide", [], "interval analysis + widened unsigned bv_decide for var*var; omega for linear")
  | "ensures" =>
      ("(state spec, then refine)", ["Concrete.ProofKit.Refinement"],
       "state the spec; prove `<fn> refines spec`; run --emit-link once proved")
  | _ => ("(unknown)", [], "")

/-- `concrete prove <file> <fn> --nearest-lemmas [<id>] [--json]`: proof-recipe
    hints mapping each obligation kind + detected features to local tactics/
    lemmas. With `oblFilter` set (a stable `<qual>@<line>#<Ox>` id or a short
    `O4`/`ensures`), the recipes are scoped to that one obligation. -/
def nearestLemmas (pc : Concrete.ProofCore) (modules : List Module) (qualName : String)
    (provedVCs : List String) (json : Bool) (oblFilter : Option String := none) : String := Id.run do
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName) |>.map Prod.snd
  let entry? := pc.entries.find? (·.qualName == qualName)
  match astFn?, entry? with
  | some f, some e =>
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    -- collect (id, kind) obligations
    let mut obls : List (String × String) := []
    for lc in f.loopContracts do
      for oid in ["O1", "O2", "O3", "O4", "O5"] do
        match loopObInfo lc oid qualName f.ensures extraLets retExpr provedVCs with
        | some (kind, _, _, _) => obls := obls ++ [(loopVCKey qualName lc.line oid, kind)]
        | none => pure ()
    for _ in f.ensures do obls := obls ++ [(s!"{qualName}#ensures", "ensures")]
    -- scope to a single obligation when an id is given (stable or short form)
    match oblFilter with
    | some want =>
      obls := obls.filter fun (id, _) =>
        id == want || (id.splitOn "#").getLast! == want || id == s!"{qualName}#{want}"
      if obls.isEmpty then
        return if json then s!"\{\"error\": {jsonStr s!"no obligation '{want}' for '{qualName}'"}}"
               else s!"no obligation '{want}' for '{qualName}'."
    | none => pure ()
    -- feature-level lemma families
    let feats := (e.fn.body.flatMap proveStmtFeatures).eraseDups
    let mut featLemmas : List String := []
    if feats.contains "loop" || !f.loopContracts.isEmpty then featLemmas := featLemmas ++ ["Concrete.ProofKit.Loops"]
    if feats.contains "array" then featLemmas := featLemmas ++ ["Concrete.ProofKit.Array"]
    if feats.contains "bitvec" then featLemmas := featLemmas ++ ["Concrete.ProofKit.BitVec", "bv_decide"]
    if feats.contains "call" then featLemmas := featLemmas ++ ["Concrete.ProofKit.Calls"]
    if json then
      let q := jsonStr
      let qarr := fun (xs : List String) => "[" ++ ", ".intercalate (xs.map q) ++ "]"
      let recipes := obls.map fun (id, kind) =>
        let (tac, lemmas, note) := lemmaRecipeFor kind
        String.join ["{\"id\": ", q id, ", \"kind\": ", q kind, ", \"tactic\": ", q tac,
          ", \"lemmas\": ", qarr lemmas, ", \"note\": ", q note, "}"]
      return String.join [
        "{\n",
        s!"  \"function\": {q qualName},\n",
        s!"  \"recipes\": [{", ".intercalate recipes}],\n",
        s!"  \"feature_lemmas\": {qarr featLemmas},\n",
        s!"  \"next_actions\": [\{\"kind\": \"emit_lean\", \"command\": {q s!"concrete prove <file> {qualName} --emit-lean"}, \"output_format\": \"text\", \"resolves\": \"missing\"}]\n",
        "}" ]
    else
      let mut out := s!"=== nearest lemmas / proof recipes: {qualName} ===\n\n"
      for (id, kind) in obls do
        let (tac, lemmas, note) := lemmaRecipeFor kind
        out := out ++ s!"  {id}  ({kind})\n    tactic: {tac}\n"
        if !lemmas.isEmpty then out := out ++ s!"    lemmas: {", ".intercalate lemmas}\n"
        out := out ++ s!"    note:   {note}\n\n"
      out := out ++ s!"feature lemma families: {if featLemmas.isEmpty then "(none)" else ", ".intercalate featLemmas}\n"
      return out
  | _, _ =>
    if json then s!"\{\"error\": {jsonStr s!"no extractable function '{qualName}'"}}"
    else s!"no extractable function '{qualName}' (it may be excluded or blocked)."

/-- `concrete prove <file> <fn> --emit-lean`: a compilable Lean stub for ONE
    function — imports, namespace, the extracted PExpr + PFnDef, a single-entry
    FnTable, an eval helper, obligation TODO blocks (from the loop VCs +
    `#[ensures]`), suggested ProofKit lemmas, and a theorem stub ending in
    `sorry` (no invented proof). -/
def emitLeanStub (pc : Concrete.ProofCore) (registry : ProofRegistry)
    (modules : List Module) (qualName : String) (provedVCs : List String) : String := Id.run do
  let entries := extractionEntriesFromPC pc registry
  match entries.find? (·.qualName == qualName) with
  | none => return s!"-- no function '{qualName}' in ProofCore.\n"
  | some e =>
    if !(e.eligible && e.extracted.isSome) then
      return s!"-- `{qualName}` is not extractable (excluded or blocked); no Lean stub to emit.\n"
    let name := leanIdent (qualName.splitOn "." |>.getLast!)
    let pexpr := match e.extracted with | some p => renderPExprAsLean p | none => "sorry"
    let paramsList := "[" ++ ", ".intercalate (e.params.map (s!"\"{·}\"")) ++ "]"
    let paramSig := " ".intercalate (e.params.map fun p => s!"({p} : Int)")
    let paramBinds := e.params.foldl (fun acc p =>
      s!"({acc}.bind \"{p}\" (.int {p}))") "Env.empty"
    -- obligation TODO blocks (loop VCs + ensures), with recipes
    let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName) |>.map Prod.snd
    let mut oblTodos : List String := []
    let mut featLemmas : List String := []
    match astFn? with
    | some f =>
      let extraLets := letConstMap f.body
      let retExpr := loopExitReturn f.body
      for lc in f.loopContracts do
        for oid in ["O1", "O2", "O3", "O4", "O5"] do
          match loopObInfo lc oid qualName f.ensures extraLets retExpr provedVCs with
          | some (kind, hyps, concl, status) =>
            let (tac, lemmas, _) := lemmaRecipeFor kind
            oblTodos := oblTodos ++ [s!"--   [{loopVCKey qualName lc.line oid}] {kind} ({status})\n--     hyps: {" ∧ ".intercalate hyps}\n--     goal: {concl}\n--     recipe: {tac}{if lemmas.isEmpty then "" else s!" — {", ".intercalate lemmas}"}"]
          | none => pure ()
      for ens in f.ensures do
        oblTodos := oblTodos ++ [s!"--   [{qualName}#ensures] ensures: {Concrete.fmtExpr ens}\n--     recipe: state the spec; prove `{name}_refines_spec`"]
      let feats := match pc.entries.find? (·.qualName == qualName) with
        | some pe => (pe.fn.body.flatMap proveStmtFeatures).eraseDups
        | none => []
      if feats.contains "loop" || !f.loopContracts.isEmpty then featLemmas := featLemmas ++ ["Concrete.ProofKit.Loops"]
      if feats.contains "array" then featLemmas := featLemmas ++ ["Concrete.ProofKit.Array"]
      if feats.contains "bitvec" then featLemmas := featLemmas ++ ["Concrete.ProofKit.BitVec", "bv_decide"]
      if feats.contains "call" then featLemmas := featLemmas ++ ["Concrete.ProofKit.Calls"]
    | none => pure ()
    let oblSection := if oblTodos.isEmpty then "--   (no generated loop/ensures obligations)"
                      else "\n".intercalate oblTodos
    let kitSection := if featLemmas.isEmpty then "Concrete.ProofKit (general)" else ", ".intercalate featLemmas
    return String.join [
      s!"-- Lean proof stub for `{qualName}` — fill in the spec and replace `sorry`.\n",
      s!"-- Suggested ProofKit lemmas: {kitSection}\n",
      "import Concrete.Proof\nimport Concrete.ProofKit\n\n",
      s!"namespace Concrete.Proof.Generated.{name}\nopen Concrete.Proof\n\n",
      s!"/-- Extracted from `{qualName}`. -/\ndef {name}Expr : PExpr :=\n    {pexpr}\n\n",
      s!"def {name}Fn : PFnDef :=\n  \{ name := \"{name}\", params := {paramsList}, body := {name}Expr }\n\n",
      s!"def fns : FnTable\n  | \"{name}\" => some {name}Fn\n  | _ => none\n\n",
      s!"def eval_{name} {paramSig} (fuel : Nat := 20) : Option PVal :=\n  eval fns {paramBinds} fuel {name}Expr\n\n",
      "-- Obligations to discharge:\n", oblSection, "\n\n",
      s!"/-- TODO: replace `sorry` (RHS) with the spec, then prove it. -/\n",
      s!"theorem {name}_refines_spec {paramSig} (fuel : Nat) :\n    eval fns {paramBinds} (fuel + 1) {name}Expr = sorry := by\n  sorry\n\n",
      s!"end Concrete.Proof.Generated.{name}\n" ]

/-- One failed-obligation artifact bundle: a sanitized directory name plus the
    four files written under `.build/prove/<fn>/<dirName>/`. -/
structure ProveArtifact where
  oblId : String
  dirName : String
  context : String      -- context.json
  failedLean : String   -- failed.lean
  command : String      -- command.txt
  readme : String       -- README.txt
  deriving Inhabited

/-- Sanitize an obligation id (`<qual>@<line>#<Ox>`) into a path-safe segment. -/
def artifactDirName (oblId : String) : String :=
  String.ofList (oblId.toList.map fun c =>
    if c.isAlphanum || c == '_' || c == '-' then c else '_')

/-- `concrete prove <file> <fn> --emit-artifacts`: collect every obligation that
    does NOT currently close — loop VCs absent from `provedVCs`, call-site VCs in
    `failingCalls`, and (when the function itself is `missing`/`stale`/`blocked`)
    a function-level refinement artifact. Each becomes a reproducible bundle an
    agent can pick up without loading the whole flagship proof. Returns `[]` for a
    cleanly-proved function. -/
def proveArtifacts (pc : Concrete.ProofCore) (registry : ProofRegistry)
    (modules : List Module) (qualName inputPath : String)
    (provedVCs : List String) (failingCalls : List (Nat × String))
    (proveStatus : String) : List ProveArtifact := Id.run do
  let q := jsonStr
  let qarr := fun (xs : List String) => "[" ++ ", ".intercalate (xs.map q) ++ "]"
  let stub := emitLeanStub pc registry modules qualName provedVCs
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName) |>.map Prod.snd
  -- shared builder for the four files given the obligation facts
  let mk := fun (oblId shortId kind status srcLine : String)
                (hyps : List String) (concl tactic : String) (lemmas : List String) (note : String) =>
    let ctx := String.join [
      "{\n",
      s!"  \"id\": {q oblId},\n",
      s!"  \"function\": {q qualName},\n",
      s!"  \"input_file\": {q inputPath},\n",
      s!"  \"kind\": {q kind},\n",
      s!"  \"status\": {q status},\n",
      s!"  \"source_line\": {srcLine},\n",
      s!"  \"hypotheses\": {qarr hyps},\n",
      s!"  \"conclusion\": {q concl},\n",
      s!"  \"recipe\": \{\"tactic\": {q tactic}, \"lemmas\": {qarr lemmas}, \"note\": {q note}}\n",
      "}\n" ]
    let cmd := String.join [
      "# Failed obligation: ", oblId, " (", kind, ") — ", status, "\n\n",
      "# 1. inspect this obligation in full:\n",
      s!"concrete prove {inputPath} {qualName} --show-obligation {shortId} --json\n\n",
      "# 2. (re)generate the Lean stub for this function:\n",
      s!"concrete prove {inputPath} {qualName} --emit-lean --out failed.lean --force\n\n",
      "# recipe: ", tactic, (if lemmas.isEmpty then "" else "  [" ++ ", ".intercalate lemmas ++ "]"), "\n\n",
      "# 3. write the proof in failed.lean (or Concrete/Proof.lean), then re-check:\n",
      s!"concrete prove {inputPath} {qualName} --replay --json\n",
      s!"concrete {inputPath} --report check-proofs\n" ]
    let readme := String.join [
      "FAILED OBLIGATION ", oblId, "\n",
      "================", String.ofList (List.replicate oblId.length '='), "\n\n",
      "function:   ", qualName, "\n",
      "file:       ", inputPath, "\n",
      "kind:       ", kind, "\n",
      "status:     ", status, "\n",
      (if srcLine == "0" then "" else s!"source line: {srcLine}\n"),
      "\nhypotheses:\n",
      (if hyps.isEmpty then "  (none)\n" else String.join (hyps.map (s!"  - {·}\n"))),
      "\ngoal:\n  ", concl, "\n\n",
      "suggested recipe:\n  tactic: ", tactic, "\n",
      (if lemmas.isEmpty then "" else "  lemmas: " ++ ", ".intercalate lemmas ++ "\n"),
      "  note:   ", note, "\n\n",
      "Files in this directory:\n",
      "  context.json  — machine-readable obligation facts (id matches --json / --replay / --report contracts)\n",
      "  failed.lean   — compilable single-function Lean stub (ends in `sorry`; fill in and prove)\n",
      "  command.txt   — exact commands to inspect, regenerate, and re-check\n",
      "  README.txt    — this file\n\n",
      "This bundle is self-contained: it does not require loading the flagship proof.\n" ]
    let banner := s!"-- FAILED OBLIGATION: {oblId} ({kind}) — {status}\n-- See command.txt / README.txt in this directory.\n\n"
    ({ oblId, dirName := artifactDirName oblId,
       context := ctx, failedLean := banner ++ stub, command := cmd, readme } : ProveArtifact)
  let mut arts : List ProveArtifact := []
  -- failing loop obligations
  match astFn? with
  | some f =>
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    for lc in f.loopContracts do
      for oid in ["O1", "O2", "O3", "O4", "O5"] do
        match loopObInfo lc oid qualName f.ensures extraLets retExpr provedVCs with
        | some (kind, hyps, concl, _) =>
          let key := loopVCKey qualName lc.line oid
          if !provedVCs.contains key then
            let (tac, lemmas, note) := lemmaRecipeFor kind
            arts := arts ++ [mk key oid kind "fails_to_close" (toString lc.line) hyps concl tac lemmas note]
        | none => pure ()
  | none => pure ()
  -- failing call-site obligations
  for (i, goal) in failingCalls do
    let (tac, lemmas, note) := lemmaRecipeFor "integer_overflow"
    arts := arts ++ [mk s!"{qualName}#call{i}" s!"call{i}" "call_site_safety" "fails_to_close" "0" [] goal tac lemmas note]
  -- function-level: no/stale/blocked proof link
  if proveStatus == "missing" || proveStatus == "stale" || proveStatus == "blocked" then
    let (tac, lemmas, note) := lemmaRecipeFor "ensures"
    let concl := match astFn? with
      | some f => match f.ensures with
                  | e :: _ => Concrete.fmtExpr e
                  | [] => s!"{qualName} refines its specification"
      | none => s!"{qualName} refines its specification"
    let kind := match proveStatus with
      | "stale" => "stale_proof_link" | "blocked" => "blocked" | _ => "missing_proof_link"
    arts := arts ++ [mk s!"{qualName}#refines_spec" "refines_spec" kind proveStatus "0" [] concl tac lemmas note]
  return arts

/-- `concrete prove <file> <fn> --workspace DIR`: compose the read-only prove
    surfaces into one self-contained directory (disposable build output, NOT a
    proof registry). Returns `(relativePath, contents)` pairs the caller writes
    under DIR:
      manifest.json          — `--json` proof report (status/fingerprint/link/next_actions)
      context.json           — proof-authoring inputs (spec/proof refs, ProofKit imports, theorems, feature hints)
      <Fn>Proofs.lean        — the `--emit-lean` stub
      link.con.txt           — the `--emit-link` source attributes
      obligations/<id>.json  — per-obligation facts + lemma recipe + replay/check command
      check.sh / replay.sh   — exact local commands
      README.md              — function-specific workflow -/
def workspaceFiles (pc : Concrete.ProofCore) (registry : ProofRegistry)
    (modules : List Module) (qualName inputPath schemaVersion : String)
    (provedVCs : List String) : List (String × String) := Id.run do
  let q := jsonStr
  let qarr := fun (xs : List String) => "[" ++ ", ".intercalate (xs.map q) ++ "]"
  let leanName := leanIdent (qualName.splitOn "." |>.getLast!)
  let capName := match leanName.toList with
    | c :: rest => String.ofList (c.toUpper :: rest)
    | [] => leanName
  let leanFile := s!"{capName}Proofs.lean"
  let astFn? := (modules.flatMap allFunctions).find? (fun (pfx, fn) => pfx ++ fn.name == qualName) |>.map Prod.snd
  let regEntry := registry.find? (·.function == qualName)
  -- context.json: the proof-authoring inputs (distinct from manifest's status report)
  let mut hints : List String := []
  let mut suggested : List String := [leanName ++ "_refines_spec"]
  match pc.entries.find? (·.qualName == qualName), astFn? with
  | some e, some f =>
    let feats := (e.fn.body.flatMap proveStmtFeatures).eraseDups
    if feats.contains "loop" || !f.loopContracts.isEmpty then hints := hints ++ ["Concrete.ProofKit.Loops"]
    if feats.contains "array" then hints := hints ++ ["Concrete.ProofKit.Array"]
    if feats.contains "bitvec" then hints := hints ++ ["Concrete.ProofKit.BitVec"]
    if feats.contains "call" then hints := hints ++ ["Concrete.ProofKit.Calls"]
  | _, _ => pure ()
  let linkRefs := match regEntry with
    | some re => String.join [
        "  \"spec\": ", q re.spec, ",\n  \"proof_by\": ", q re.proof,
        ",\n  \"ensures_proof\": ", (match re.ensuresProof with | some t => q t | none => "null"),
        ",\n  \"proof_fingerprint\": ", q (shortHash re.bodyFingerprint), ",\n" ]
    | none => "  \"spec\": null,\n  \"proof_by\": null,\n  \"ensures_proof\": null,\n  \"proof_fingerprint\": null,\n"
  let contextJson := String.join [
    "{\n  \"function\": ", q qualName, ",\n  \"schema_version\": ", q schemaVersion, ",\n",
    linkRefs,
    "  \"proofkit_imports\": ", qarr hints, ",\n",
    "  \"suggested_theorems\": ", qarr suggested, ",\n",
    "  \"lean_stub_file\": ", q leanFile, ",\n",
    "  \"link_file\": \"link.con.txt\"\n}\n" ]
  let mut files : List (String × String) := []
  files := files ++ [("manifest.json", proveReportJson pc registry modules qualName provedVCs inputPath schemaVersion ++ "\n")]
  files := files ++ [("context.json", contextJson)]
  files := files ++ [(leanFile, emitLeanStub pc registry modules qualName provedVCs)]
  files := files ++ [("link.con.txt", emitProofLink registry qualName ++ "\n")]
  -- obligations/<id>.json — per-obligation facts + recipe + commands
  let oblFile := fun (id kind status srcLine : String) (hyps : List String) (concl : String) =>
    let (tac, lemmas, note) := lemmaRecipeFor kind
    String.join [
      "{\n  \"id\": ", q id, ",\n  \"function\": ", q qualName,
      ",\n  \"kind\": ", q kind, ",\n  \"status\": ", q status,
      ",\n  \"source_line\": ", srcLine,
      ",\n  \"hypotheses\": ", qarr hyps, ",\n  \"conclusion\": ", q concl,
      ",\n  \"recipe\": {\"tactic\": ", q tac, ", \"lemmas\": ", qarr lemmas, ", \"note\": ", q note, "}",
      ",\n  \"replay_command\": ", q s!"concrete prove {inputPath} {qualName} --replay --json",
      ",\n  \"check_command\": ", q s!"concrete prove {inputPath} {qualName} --check --json", "\n}\n" ]
  match astFn? with
  | some f =>
    let extraLets := letConstMap f.body
    let retExpr := loopExitReturn f.body
    for lc in f.loopContracts do
      for oid in ["O1", "O2", "O3", "O4", "O5"] do
        match loopObInfo lc oid qualName f.ensures extraLets retExpr provedVCs with
        | some (kind, hyps, concl, st) =>
          let id := loopVCKey qualName lc.line oid
          files := files ++ [(s!"obligations/{artifactDirName id}.json", oblFile id kind st (toString lc.line) hyps concl)]
        | none => pure ()
    for ens in f.ensures do
      let reqHyps := f.requires.map Concrete.fmtExpr
      let st := (pc.obligations.find? (·.functionId.qualName == qualName)).map (·.status.canonical) |>.getD "missing"
      files := files ++ [(s!"obligations/{artifactDirName s!"{qualName}#ensures"}.json", oblFile s!"{qualName}#ensures" "ensures" st (toString f.span.line) reqHyps (Concrete.fmtExpr ens))]
  | none => pure ()
  files := files ++ [("check.sh", s!"#!/usr/bin/env bash\n# Kernel-verify this function's linked Lean proof (structured JSON).\nconcrete prove {inputPath} {qualName} --check --json\n")]
  files := files ++ [("replay.sh", s!"#!/usr/bin/env bash\n# Re-run omega / bv_decide discharge for this function (structured JSON).\nconcrete prove {inputPath} {qualName} --replay --json\n")]
  let readme := String.join [
    s!"# Proof workspace — `{qualName}`\n\n",
    s!"Generated by `concrete prove {inputPath} {qualName} --workspace`.\n\n",
    "This is a **disposable build output**, not a proof registry. The source of\n",
    "truth stays the `.con` file plus its in-source proof attributes. Do not\n",
    "commit this directory.\n\n",
    "## Files\n\n",
    "- `manifest.json` — machine-readable status: eligibility, fingerprint, proof link, next actions.\n",
    "- `context.json` — proof-authoring inputs: spec/proof references, ProofKit imports, suggested theorem names.\n",
    s!"- `{leanFile}` — compilable Lean stub (ends in `sorry`; write the proof here or in Concrete/Proof.lean).\n",
    "- `obligations/<id>.json` — one file per obligation: hypotheses, conclusion, status, lemma recipe, replay/check command.\n",
    "- `link.con.txt` — the in-source `#[spec]/#[proof_by]/...` attributes to paste above the function once proved.\n",
    "- `check.sh` / `replay.sh` — exact local verification commands.\n\n",
    "## Workflow\n\n",
    "1. Read `manifest.json` for status + next actions, and the `obligations/` files for what to prove.\n",
    s!"2. Write the proof in `{leanFile}` (see PROOFKIT_GUIDE for the lemmas in `context.json`).\n",
    "3. `bash check.sh` — kernel-verify. Iterate until `\"all_checked\": true`.\n",
    "4. Paste `link.con.txt` above the function in the `.con` source.\n",
    s!"5. `concrete {inputPath} --report check-proofs` and `concrete audit {inputPath}` to confirm.\n" ]
  files := files ++ [("README.md", readme)]
  return files

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
   "obligation", "extraction", "traceability", "effects", "capability", "unsafe", "alloc",
   "contract"]

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
/-- Collect per-function source-contract facts from the AST (not Core — contracts
    are AST metadata, erased before Core). One fact per function that declares any
    `#[requires]` / `#[ensures]` / loop `#[invariant]`. The clause sets are joined
    with `∧` and diffed at the clause level (see `isWeakening "contract"`), so the
    semantic-diff surface can classify a contract change as a breaking precondition
    strengthening, a breaking postcondition weakening, or a flagged invariant
    change. Emitted by `concrete snapshot`; consumed by `concrete diff`. -/
def collectContractFacts (modules : List Module) : List Val :=
  (modules.flatMap allFunctions).filterMap fun (pfx, f) =>
    let invs := f.loopContracts.flatMap (·.invariants)
    if f.requires.isEmpty && f.ensures.isEmpty && invs.isEmpty then none
    else
      let canon (es : List Expr) := " ∧ ".intercalate ((es.map Concrete.fmtExpr).eraseDups)
      some (.obj [
        ("kind", .str "contract"),
        ("function", .str (pfx ++ f.name)),
        ("requires", .str (canon f.requires)),
        ("ensures", .str (canon f.ensures)),
        ("invariants", .str (canon invs)) ])

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
/-- One VC as a JSON object (schema v1). -/
def vcToJson (v : VC) : Val :=
  .obj [
    ("id", .str v.id),
    ("kind", .str v.kind),
    ("function", .str v.fn),
    ("loc", .obj [("file", .str v.file), ("line", .num (Int.ofNat v.line))]),
    ("hypotheses", .arr (v.hypotheses.map (.str ·))),
    ("conclusion", .str v.conclusion),
    ("origin", .str v.origin),
    ("dependencies", .arr (v.dependencies.map (.str ·))),
    ("arith_profile", .str v.arithProfile),
    ("expected_discharge", .str v.dischargeMode),
    ("status", .str v.status),
    ("engine", .str v.engine),
    ("counterexample", .obj (v.counterexample.map (fun (n, x) => (n, Json.Val.str x)))),
    -- SMT provenance (determinism / replay). Present only for SMT-routed VCs;
    -- `null` otherwise, so the default report carries no solver data.
    ("smt", if v.smtHash.isEmpty then Json.Val.null else .obj [
      ("logic", .str "QF_NIA"),
      ("timeout_sec", .num 5),
      ("smtlib_sha", .str v.smtHash),
      ("solver", .str (if v.solver.isEmpty then "(not run)" else v.solver)),
      ("query", .str v.smtQuery),
      ("replay", .str s!"save `query` to vc.smt2 and run: z3 -T:5 vc.smt2"),
      -- Lean replay artifact: the SAME obligation as a kernel-checkable theorem.
      ("lean_replay", if v.leanReplay.isEmpty then Json.Val.null else .obj [
        ("theorem", .str v.leanReplay),
        ("proof_attempt", .str "by omega (in-toolchain; a Mathlib build can swap in nlinarith)"),
        ("check_command", .str "save to vc_replay.lean and run: lake env lean vc_replay.lean"),
        ("note", .str "if Lean closes it, the VC graduates solver_trusted → proved_by_lean_replay and no longer depends on the solver")])
    ])
  ]

open Json in
/-- Versioned JSON envelope of a (post-discharge) VC schedule (schema v1). -/
def vcsJson (vcs : List VC) (schemaVer : Nat) : String :=
  (Val.obj [
    ("schema_version", .num (Int.ofNat schemaVer)),
    ("schema_kind", .str "vcs"),
    ("vc_schema_version", .num (Int.ofNat vcSchemaVersion)),
    ("count", .num (Int.ofNat vcs.length)),
    ("vcs", .arr (vcs.map vcToJson))
  ]).render

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
  | "contract" => ["requires", "ensures", "invariants"]
  | _ => []

/-- Split a `∧`-joined clause field into a trimmed, non-empty clause set. -/
private def splitClauses (s : String) : List String :=
  (s.splitOn " ∧ ").map (·.trimAscii.toString) |>.filter (!·.isEmpty)

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
  -- Source-contract API stability, sound at the conjunctive clause-set level.
  | "contract", "requires" =>
    -- precondition = conjunction of clauses. A clause in NEW but not OLD
    -- STRENGTHENS the precondition → existing callers may now violate it →
    -- a breaking change (blocker).
    let oldS := splitClauses oldV
    !(splitClauses newV).all oldS.contains
  | "contract", "ensures" =>
    -- postcondition = conjunction. A clause in OLD but not NEW WEAKENS the
    -- guarantee → callers that relied on it break → a breaking change.
    let newS := splitClauses newV
    !(splitClauses oldV).all newS.contains
  | "contract", "invariants" =>
    -- a public invariant change alters the loop's guarantee surface; flag any.
    oldV != newV
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
  | "contract" =>
    -- a function that gained a contract. A new precondition (`requires`) can
    -- break existing callers, so flag it; a function that only added/declared a
    -- postcondition is a compatible strengthening.
    let reqs := (jsonGetVal v "requires").map valDisplay
    if reqs.isSome && reqs != some "" then "weakened" else "neutral"
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
    (pc : Concrete.ProofCore) (vcSummary : String := "") : String :=
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
    obligationsReport modules locMap registry pc,
    sectionHeader "Verification Conditions",
    (if vcSummary.isEmpty then "(no verification conditions)" else vcSummary)
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
      []),
    ("contract", fieldSpec
      [("kind", "string"), ("function", "string"),
       ("requires", "string — ∧-joined precondition clauses"),
       ("ensures", "string — ∧-joined postcondition clauses"),
       ("invariants", "string — ∧-joined loop-invariant clauses")]
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
          ("facts", .str "object[]")])]),
      ("vcs", .obj [
        ("description", .str "Verification-condition schedule from --report vcs --json (schema v1). A VC describes an obligation and the backend that SHOULD discharge it; it does not itself run a solver."),
        ("fields", .obj [
          ("schema_version", .str "number"),
          ("schema_kind", .str "\"vcs\""),
          ("vc_schema_version", .str "number — VC schema version (currently 1)"),
          ("count", .str "number"),
          ("vcs", .str "object[] — each: id, kind, function, loc{file,line}, hypotheses[], conclusion, origin, dependencies[], arith_profile, expected_discharge, status, engine, counterexample{var:value}. status ∈ {planned, proved_by_kernel_decision, proved_by_lean, arithmetic_proved, counterexample, unproven, missing} plus the OPT-IN external-solver classes {solver_trusted, unknown, timeout, solver_error} and the replay class proved_by_lean_replay (a Lean/kernel class — set only when a kernel tactic independently closes the replay theorem, dropping the solver from the claim); engine ∈ {constant_fold, omega, bv_decide, lean, lean:omega, smt:<solver>, \"\"}. counterexample maps SOURCE variable names to concrete values, populated only on a `sat` solver result. smt (null unless the VC was SMT-routed) carries determinism/replay provenance: {logic, timeout_sec, smtlib_sha (stable digest of the exact query), solver (name+version), query (the SMT-LIB script), replay, lean_replay{theorem, proof_attempt, check_command, note}}. A decision procedure can only yield proved_by_kernel_decision; an external solver (behind --smt) yields solver_trusted/counterexample/unknown/timeout/solver_error and only ever on a VC the kernel tiers left unproven — the classes never merge.")])])
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

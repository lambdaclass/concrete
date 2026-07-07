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

def ppTyList (tys : List (String × Ty)) : String :=
  ", ".intercalate (tys.map fun (n, t) => s!"{n}: {tyToStr t}")

/-- Right-pad a string to the given width. -/
def padRight (s : String) (w : Nat) : String :=
  if s.length >= w then s
  else s ++ String.ofList (List.replicate (w - s.length) ' ')

/-- Left-pad a number string to the given width. -/
def padNum (n : Nat) (w : Nat) : String :=
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

partial def buildCapLookupModule (m : CModule) : CapLookup :=
  let fnEntries := m.functions.map fun f => (f.name, f.capSet)
  let externEntries := m.externFns.map fun (n, _, _, trusted) =>
    (n, if trusted then .empty else .concrete [unsafeCapName])
  fnEntries ++ externEntries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildCapLookupModule sub) []

def buildCapLookup (modules : List CModule) : CapLookup :=
  modules.foldl (fun acc m => acc ++ buildCapLookupModule m) []

/-- Qualified-name cap lookup — keys match buildCallGraph's qualified nodes. -/
partial def buildQualCapLookupModule (m : CModule) (pfx : String := "")
    : CapLookup :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let fnEntries := m.functions.map fun f =>
    (qualPrefix ++ "." ++ f.name, f.capSet)
  let externEntries := m.externFns.map fun (n, _, _, trusted) =>
    (n, if trusted then .empty else .concrete [unsafeCapName])
  fnEntries ++ externEntries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildQualCapLookupModule sub qualPrefix) []

def buildQualCapLookup (modules : List CModule) : CapLookup :=
  modules.foldl (fun acc m => acc ++ buildQualCapLookupModule m) []

/-- Map bare function names → qualified names across all modules. -/
partial def buildBareToQualMap (m : CModule) (pfx : String := "")
    : List (String × String) :=
  let qualPrefix := if pfx == "" then m.name else pfx ++ "." ++ m.name
  let entries := m.functions.map fun f => (f.name, qualPrefix ++ "." ++ f.name)
  entries ++ m.submodules.foldl (fun acc sub =>
    acc ++ buildBareToQualMap sub qualPrefix) []

/-- Look up a callee's capability set. Checks user fns, externs, then intrinsics. -/
def lookupCalleeCap (lookup : CapLookup) (name : String) : Option CapSet :=
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
def calleeTag (lookup : CapLookup) (name : String) : String :=
  match lookup.find? (fun (n, _) => n == name) with
  | some _ => ""
  | none =>
    if (resolveIntrinsic name).isSome then " (intrinsic)"
    else " (unknown)"



end Report
end Concrete

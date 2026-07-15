import Concrete.Frontend.AST
import Concrete.Report.Diagnostic
import Concrete.Resolve.FileSummary
import Concrete.Resolve.Resolve
import Concrete.Frontend.Parser
import Concrete.Check.Check
import Concrete.Elab.Core
import Concrete.Elab.Elab
import Concrete.Elab.CoreCanonicalize
import Concrete.Check.CoreCheck
import Concrete.IR.Mono
import Concrete.IR.SSA
import Concrete.IR.Lower
import Concrete.IR.SSAVerify
import Concrete.IR.SSACleanup
import Concrete.Backend.EmitSSA
import Concrete.Check.Verify

namespace Concrete

/-! ## Pipeline — cacheable compiler artifacts

Each pipeline stage produces a named artifact type.
Runner functions wrap the underlying pass with diagnostic handling.
No serialization yet — these types are the prerequisite for future `ToJson`/`FromJson` instances.
-/

-- ============================================================
-- Artifact types
-- ============================================================

structure ParsedProgram where
  modules : List Module

structure SummaryTable where
  entries : List (String × FileSummary)

structure ResolvedProgram where
  modules : List ResolvedModule

structure ElaboratedProgram where
  coreModules : List CModule

/-- Core IR that has passed `coreCheckProgram`.
    This is the proof-oriented artifact boundary: downstream passes (Mono, Lower, Emit)
    may assume all Core-level invariants hold.  Only `Pipeline.coreCheck` constructs this. -/
structure ValidatedCore where
  coreModules : List CModule

structure MonomorphizedProgram where
  coreModules : List CModule

structure SSAProgram where
  ssaModules : List SModule

-- ============================================================
-- Pipeline runner functions
-- ============================================================

namespace Pipeline

/-- Parse source code into a `ParsedProgram`. Expands capability aliases. -/
def parse (source : String) : Except Diagnostics ParsedProgram :=
  match Concrete.parse source with
  | .ok modules => .ok { modules := modules.map Module.expandCapAliases }
  | .error ds => .error ds

/-- Parse error-tolerantly: returns the (best-effort) program ALONGSIDE every
    recovered parse error, so a single bad declaration does not hide the rest
    (ROADMAP Phase 4 #12c). For the tolerant diagnostics path only. -/
def parsePartial (source : String) : ParsedProgram × Diagnostics :=
  let (modules, ds) := Concrete.parseProgramPartial source
  ({ modules := modules.map Module.expandCapAliases }, ds)

/-- Resolve `mod X;` declarations by reading sub-module files from disk.
    Wraps `resolveAllModules` (IO because it reads files).
    Returns the resolved program and a source map for diagnostics. -/
def resolveFiles (baseDir : String) (prog : ParsedProgram) (inputPath : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (Except Diagnostics (ParsedProgram × SourceMap)) := do
  match ← resolveAllModules baseDir prog.modules inputPath with
  | .error e =>
    return .error [{ severity := .error, message := e, pass := "resolve", span := none, hint := none }]
  | .ok (modules, srcMap) =>
    return .ok ({ modules }, srcMap)

/-- Build the cross-file summary table from parsed modules. -/
def buildSummary (prog : ParsedProgram) : SummaryTable :=
  { entries := buildSummaryTable prog.modules }

/-- Name-resolution pass: validates all identifiers and imports. -/
def resolve (prog : ParsedProgram) (summary : SummaryTable) : Except Diagnostics ResolvedProgram :=
  match resolveProgram prog.modules summary.entries with
  | .ok resolved => .ok { modules := resolved }
  | .error ds => .error ds

/-- Name-resolution pass, error-tolerant: returns the (always structurally complete)
    resolved program ALONGSIDE diagnostics, never discarding it. For the tolerant
    diagnostics path only (ROADMAP Phase 4 #12a). -/
def resolvePartial (prog : ParsedProgram) (summary : SummaryTable) : ResolvedProgram × Diagnostics :=
  let (resolved, ds) := resolveProgramPartial prog.modules summary.entries
  ({ modules := resolved }, ds)

/-- Desugar destructuring let statements before Check and Elab see them. -/
def desugar (resolved : ResolvedProgram) : ResolvedProgram :=
  { modules := resolved.modules.map fun rm =>
      { rm with module := desugarModule rm.module } }

/-- Type-checking pass. Consumes resolved program (proving name resolution happened). -/
def check (resolved : ResolvedProgram) (summary : SummaryTable) : Except Diagnostics Unit :=
  checkProgram resolved.modules summary.entries

/-- Elaborate and canonicalize (no validation yet). Consumes resolved program. -/
def elaborate (resolved : ResolvedProgram) (summary : SummaryTable) : Except Diagnostics ElaboratedProgram :=
  match elabProgram resolved.modules summary.entries with
  | .error ds => .error ds
  | .ok coreModules => .ok { coreModules := canonicalizeProgram coreModules }

/-- Validate elaborated Core IR.  This is the only way to construct a `ValidatedCore`.
    Note: the post-Elab placeholder check (`verifyPostElab`) is **not** run here — it is
    only available via `--report verify`. Placeholder legitimately survives elaboration
    in try/defer expressions and is resolved during lowering. -/
def coreCheck (elabProg : ElaboratedProgram) : Except Diagnostics ValidatedCore :=
  match coreCheckProgram elabProg.coreModules with
  | .error ds => .error ds
  | .ok () => .ok { coreModules := elabProg.coreModules }

/-- Run the post-Elab verifier (placeholder check) as a non-blocking diagnostic pass.
    Returns warnings for any Ty.placeholder found; does not block compilation. -/
def verifyPostElab (modules : List CModule) : Diagnostics :=
  (verifyNoPlaceholders modules).map fun d => { d with severity := .warning }

/-- Monomorphize generic functions.  Runs the post-Mono verifier (no Ty.typeVar). -/
def monomorphize (vc : ValidatedCore) : Except Diagnostics MonomorphizedProgram :=
  match monoProgram vc.coreModules with
  | .ok modules =>
    let monoDs := verifyPostMono modules
    if !monoDs.isEmpty then .error monoDs
    else .ok { coreModules := modules }
  | .error ds => .error ds

/-- Lower to SSA, verify, and clean up. -/
def lower (mono : MonomorphizedProgram) : Except Diagnostics SSAProgram := do
  let ssaModules ← mono.coreModules.mapM (fun m => lowerModule m)
  match ssaVerifyProgram ssaModules with
  | .error ds => .error ds
  | .ok () =>
    let ssaModules := ssaCleanupProgram ssaModules
    -- Verify cleanup preserved SSA invariants
    match ssaVerifyProgram ssaModules with
    | .error ds => .error ds
    | .ok () => .ok { ssaModules }

/-- Same as `lower`, but skip both verifier passes AND the cleanup
    pass. Diagnostic-only — used by debugging entry points to print
    raw SSA that the verifier rejects. Not on the production compile
    path. -/
def lowerUnverified (mono : MonomorphizedProgram) : Except Diagnostics SSAProgram := do
  let ssaModules ← mono.coreModules.mapM (fun m => lowerModule m)
  .ok { ssaModules }

/-- Per-gate verify report.  Each field collects diagnostics produced
    at a specific named pipeline boundary; an empty list means the gate
    passed cleanly. Currently:

      `postElab`        Ty.placeholder leak after elab (warnings —
                        try/defer have documented exceptions per
                        `docs/VERIFY_GATES.md`).
      `postMono`        Ty.typeVar leak + Copy-field violations after
                        monomorphization (errors).
      `postLower`       SSA structural / dominator invariants on the
                        raw `lowerModule` output (errors).
      `postCleanup`     Same invariants re-checked after
                        `ssaCleanupProgram` (errors — non-empty means
                        cleanup broke something Lower got right).

    Earlier-pass failures (parse / resolve / check / elab / coreCheck /
    mono) are not represented here; they are surfaced through the
    Except return of the corresponding Pipeline.* function, since they
    block construction of the input artifact this gate runs on. -/
structure VerifyReport where
  postElab    : Diagnostics
  postMono    : Diagnostics
  postLower   : Diagnostics
  postCleanup : Diagnostics
  deriving Inhabited

namespace VerifyReport

def empty : VerifyReport :=
  { postElab := [], postMono := [], postLower := [], postCleanup := [] }

def isClean (r : VerifyReport) : Bool :=
  r.postElab.isEmpty && r.postMono.isEmpty
    && r.postLower.isEmpty && r.postCleanup.isEmpty

def errorCount (r : VerifyReport) : Nat :=
  let count (ds : Diagnostics) : Nat := (ds.filter (·.severity == .error)).length
  count r.postElab + count r.postMono + count r.postLower + count r.postCleanup

def warningCount (r : VerifyReport) : Nat :=
  let count (ds : Diagnostics) : Nat := (ds.filter (·.severity == .warning)).length
  count r.postElab + count r.postMono + count r.postLower + count r.postCleanup

def allDiagnostics (r : VerifyReport) : Diagnostics :=
  r.postElab ++ r.postMono ++ r.postLower ++ r.postCleanup

end VerifyReport

/-- Run every per-gate verifier on a validated Core program, returning
    the per-boundary diagnostics. Earlier-pass failures must already
    have been handled by the caller; this function consumes
    ValidatedCore and produces the report or short-circuits if a
    downstream pass (mono / lower) fails outright. -/
def runVerifyGates (vc : ValidatedCore) : VerifyReport :=
  let postElab := verifyPostElab vc.coreModules
  match monoProgram vc.coreModules with
  | .error ds =>
    -- Mono refused to even run; surface its diagnostics as post-mono
    -- errors. We keep the per-gate label on what the diagnostics
    -- actually represent (a failure of the post-mono boundary's
    -- input artifact to be well-formed), rather than dropping them.
    { VerifyReport.empty with postElab := postElab, postMono := ds }
  | .ok monoMods =>
    let postMono := verifyPostMono monoMods
    if !postMono.isEmpty then
      { VerifyReport.empty with postElab := postElab, postMono := postMono }
    else
      let ssa := monoMods.map lowerModule
      -- Each lowerModule produces Except — short-circuit on first error.
      let lowerErrors : Diagnostics := ssa.foldl (fun acc r =>
        match r with | .error ds => acc ++ ds | .ok _ => acc) []
      if !lowerErrors.isEmpty then
        { VerifyReport.empty with postElab := postElab, postMono := postMono,
                                  postLower := lowerErrors }
      else
        let ssaMods : List SModule := ssa.foldl (fun acc r =>
          match r with | .ok m => acc ++ [m] | .error _ => acc) []
        let postLower := match ssaVerifyProgram ssaMods with
          | .error ds => ds
          | .ok () => []
        -- post-cleanup checks that ssaCleanupProgram preserved SSA
        -- invariants. That claim is only meaningful when post-lower
        -- was clean; running cleanup on already-invalid SSA and
        -- reporting it as "post-cleanup ok" would imply cleanup was
        -- meaningfully checked on bad input. Skip it instead.
        let postCleanup :=
          if !postLower.isEmpty then ([] : Diagnostics)
          else
            let cleaned := ssaCleanupProgram ssaMods
            match ssaVerifyProgram cleaned with
            | .error ds => ds
            | .ok () => []
        { postElab := postElab, postMono := postMono,
          postLower := postLower, postCleanup := postCleanup }

/-- Emit LLVM IR from SSA modules. -/
def emit (ssa : SSAProgram) (testMode : Bool := false) (moduleFilter : Option String := none) (echoResult : Bool := false) : String :=
  emitSSAProgram ssa.ssaModules testMode moduleFilter echoResult

-- ============================================================
-- Shared frontend helper
-- ============================================================

/-- Run the shared frontend: parse → resolveFiles → buildSummary → resolve → check → elaborate → coreCheck.
    This is the common prefix of all CLI entry points (except interface report).
    Returns validated Core alongside earlier artifacts for diagnostic rendering. -/
def runFrontend (inputPath source : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (Except Diagnostics (ParsedProgram × SummaryTable × ValidatedCore × SourceMap)) := do
  let mainSrcMap : SourceMap := [(inputPath, source)]
  match Pipeline.parse source with
  | .error ds => return .error ds
  | .ok parsed =>
  let baseDir := let parts := inputPath.splitOn "/"
    match parts.reverse with
    | _ :: rest => "/".intercalate rest.reverse
    | [] => "."
  match ← Pipeline.resolveFiles baseDir parsed inputPath resolveAllModules with
  | .error ds => return .error ds
  | .ok (resolved, subSrcMap) =>
    let srcMap := mainSrcMap ++ subSrcMap
    let summary := Pipeline.buildSummary resolved
    match Pipeline.resolve resolved summary with
    | .error ds => return .error ds
    | .ok resolvedProg =>
    let resolvedProg := Pipeline.desugar resolvedProg
    match Pipeline.check resolvedProg summary with
    | .error ds => return .error ds
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds => return .error ds
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds => return .error ds
    | .ok validCore => return .ok (resolved, summary, validCore, srcMap)

/-- Error-tolerant frontend for tooling and reports (ROADMAP Phase 4 #12a).

    Unlike `runFrontend`, this never short-circuits at the first failing pass and
    never produces a `ValidatedCore`: it returns ONLY `Diagnostics` plus a `partial`
    flag, so it is structurally incapable of feeding codegen, proof, or policy as if
    it were complete. A bad reference in one function no longer erases type
    diagnostics elsewhere — when name resolution fails we still run the typechecker
    on the (always structurally complete, side-table-resolved) program.

    `partial = true` whenever a pass was SKIPPED (parse/file-resolution could not
    produce their artifact, or name resolution failed so we did not run the
    elaboration/core-check passes whose inputs must be fully resolved). When the
    full chain runs, `partial = false` even if later passes reported errors. -/
def runFrontendDiagnostics (inputPath source : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (Diagnostics × Bool) := do
  -- Parse error-tolerantly: recovered parse errors are collected and the
  -- well-formed declarations still flow downstream (#12c).
  let (parsed, parseDs) := Pipeline.parsePartial source
  let baseDir := let parts := inputPath.splitOn "/"
    match parts.reverse with
    | _ :: rest => "/".intercalate rest.reverse
    | [] => "."
  match ← Pipeline.resolveFiles baseDir parsed inputPath resolveAllModules with
  | .error ds => return (parseDs ++ ds, true)   -- could not load all modules
  | .ok (resolved, _) =>
    let summary := Pipeline.buildSummary resolved
    let (resolvedProg, resolveDs) := Pipeline.resolvePartial resolved summary
    let resolvedProg := Pipeline.desugar resolvedProg
    -- Typechecking is safe on the structurally complete resolved program even when
    -- name resolution reported errors, so run it regardless and merge diagnostics.
    let checkResult := Pipeline.check resolvedProg summary
    let checkDs0 := match checkResult with | .error ds => ds | .ok () => []
    -- Cascade suppression: a check diagnostic at the SAME span as a resolve
    -- diagnostic is an echo of that root cause (e.g. resolve's "unknown function"
    -- and check's "call to undeclared function" at the identical call site). Keep
    -- the root (resolve), drop the echo, so tolerance adds unrelated diagnostics
    -- without re-reporting ones the earlier pass already owns.
    let sameSpan : Option Span → Option Span → Bool
      | some a, some b => a.line == b.line && a.col == b.col && a.endLine == b.endLine && a.endCol == b.endCol
      | _, _ => false
    let checkDs := checkDs0.filter fun c => !(resolveDs.any fun r => sameSpan c.span r.span)
    -- Elaboration / core-check require fully-PARSED, fully-resolved AND type-correct
    -- input, so we only run them when parse, resolve and check are all clean —
    -- matching the strict pipeline's precondition. This keeps ownership/capability
    -- (core-check) diagnostics available without ever feeding them incomplete,
    -- unresolved, or ill-typed input. `partial` is set whenever any pass was skipped,
    -- so the consumer knows later-pass diagnostics may be missing.
    let base := parseDs ++ resolveDs ++ checkDs
    if !parseDs.isEmpty || hasErrors resolveDs then
      return (base, true)
    match checkResult with
    | .error _ => return (base, true)
    | .ok () =>
      match Pipeline.elaborate resolvedProg summary with
      | .error eds => return (base ++ eds, true)
      | .ok elabProg =>
        match Pipeline.coreCheck elabProg with
        | .error cds => return (base ++ cds, false)
        | .ok _ => return (base, false)

-- ============================================================
-- Phase 6C #1: per-stage structural telemetry (stable-schema JSON)
-- ============================================================

/-- Count `(modules, functions, enums, structs)` across a Core module forest,
    recursing through submodules. Shared by the core and post-mono stages. -/
partial def countCore (ms : List CModule) : Nat × Nat × Nat × Nat :=
  ms.foldl (fun (acc : Nat × Nat × Nat × Nat) cm =>
    let (m, f, e, s) := acc
    let (sm, sf, se, ss) := countCore cm.submodules
    (m + 1 + sm, f + cm.functions.length + sf,
     e + cm.enums.length + se, s + cm.structs.length + ss)) (0, 0, 0, 0)

/-- Count `(modules, functions, blocks, instructions)` across the SSA forest. -/
def countSSA (ms : List SModule) : Nat × Nat × Nat × Nat :=
  ms.foldl (fun (acc : Nat × Nat × Nat × Nat) sm =>
    let (mods, fns, blks, insts) := acc
    let fBlks := sm.functions.foldl (fun a fn => a + fn.blocks.length) 0
    let fInsts := sm.functions.foldl (fun a fn =>
      a + fn.blocks.foldl (fun b blk => b + blk.insts.length) 0) 0
    (mods + 1, fns + sm.functions.length, blks + fBlks, insts + fInsts)) (0, 0, 0, 0)

-- Phase 6C #1 (hardening): count AST expr+stmt nodes reachable from all function
-- bodies (functions + impl/trait methods, recursing submodules). Approximate size
-- signal for telemetry — not a semantic fact.
mutual
partial def astExprNodes : Expr → Nat
  | .intLit .. | .floatLit .. | .boolLit .. | .strLit .. | .charLit .. | .ident .. | .fnRef .. => 1
  | .binOp _ _ l r => 1 + astExprNodes l + astExprNodes r
  | .unaryOp _ _ x => 1 + astExprNodes x
  | .call _ _ _ args => 1 + (args.map astExprNodes).foldl (·+·) 0
  | .paren _ x => 1 + astExprNodes x
  | .structLit _ _ _ fields base => 1 + (fields.map (fun f => astExprNodes f.2)).foldl (·+·) 0 + (base.map astExprNodes).getD 0
  | .fieldAccess _ o _ => 1 + astExprNodes o
  | .enumLit _ _ _ _ fields => 1 + (fields.map (fun f => astExprNodes f.2)).foldl (·+·) 0
  | .match_ _ s arms => 1 + astExprNodes s + (arms.map astArmNodes).foldl (·+·) 0
  | .borrow _ x => 1 + astExprNodes x
  | .borrowMut _ x => 1 + astExprNodes x
  | .deref _ x => 1 + astExprNodes x
  | .try_ _ x => 1 + astExprNodes x
  | .arrayLit _ elems => 1 + (elems.map astExprNodes).foldl (·+·) 0
  | .arrayIndex _ a i => 1 + astExprNodes a + astExprNodes i
  | .cast _ x _ => 1 + astExprNodes x
  | .methodCall _ o _ _ args => 1 + astExprNodes o + (args.map astExprNodes).foldl (·+·) 0
  | .staticMethodCall _ _ _ _ args => 1 + (args.map astExprNodes).foldl (·+·) 0
  | .allocCall _ x a => 1 + astExprNodes x + astExprNodes a
  | .ifExpr _ c t el => 1 + astExprNodes c + (t.map astStmtNodes).foldl (·+·) 0 + (el.map astStmtNodes).foldl (·+·) 0
partial def astArmNodes : MatchArm → Nat
  | .mk _ _ _ _ g body => 1 + (g.map astExprNodes).getD 0 + (body.map astStmtNodes).foldl (·+·) 0
  | .litArm _ v g body => 1 + astExprNodes v + (g.map astExprNodes).getD 0 + (body.map astStmtNodes).foldl (·+·) 0
  | .varArm _ _ g body => 1 + (g.map astExprNodes).getD 0 + (body.map astStmtNodes).foldl (·+·) 0
  | .rangeArm _ lo hi _ g body => 1 + astExprNodes lo + astExprNodes hi + (g.map astExprNodes).getD 0 + (body.map astStmtNodes).foldl (·+·) 0
partial def astStmtNodes : Stmt → Nat
  | .letDecl _ _ _ _ v _ => 1 + astExprNodes v
  | .assign _ _ v => 1 + astExprNodes v
  | .return_ _ v => 1 + (v.map astExprNodes).getD 0
  | .expr _ x _ => 1 + astExprNodes x
  | .ifElse _ c t el => 1 + astExprNodes c + (t.map astStmtNodes).foldl (·+·) 0 + ((el.getD []).map astStmtNodes).foldl (·+·) 0
  | .while_ _ c body _ => 1 + astExprNodes c + (body.map astStmtNodes).foldl (·+·) 0
  | .forLoop _ init c step body _ => 1 + (init.map astStmtNodes).getD 0 + astExprNodes c + (step.map astStmtNodes).getD 0 + (body.map astStmtNodes).foldl (·+·) 0
  | .fieldAssign _ o _ v => 1 + astExprNodes o + astExprNodes v
  | .derefAssign _ t v => 1 + astExprNodes t + astExprNodes v
  | .arrayIndexAssign _ a i v => 1 + astExprNodes a + astExprNodes i + astExprNodes v
  | .break_ _ v _ => 1 + (v.map astExprNodes).getD 0
  | .continue_ .. => 1
  | .defer _ b => 1 + astExprNodes b
  | .assert_ _ c => 1 + astExprNodes c
  | .assume_ _ c => 1 + astExprNodes c
  | .borrowIn _ _ _ _ _ body => 1 + (body.map astStmtNodes).foldl (·+·) 0
  | .letDestructure _ _ _ _ v eb => 1 + astExprNodes v + ((eb.getD []).map astStmtNodes).foldl (·+·) 0
  | .letStructDestructure _ _ _ v => 1 + astExprNodes v
end

partial def astModuleNodes (ms : List Module) : Nat :=
  ms.foldl (fun acc m =>
    let bodies := m.functions.map (·.body)
      ++ m.implBlocks.flatMap (fun ib => ib.methods.map (·.body))
      ++ m.traitImpls.flatMap (fun ti => ti.methods.map (·.body))
    let n := (bodies.map (fun b => (b.map astStmtNodes).foldl (·+·) 0)).foldl (·+·) 0
    acc + n + astModuleNodes m.submodules) 0

/-- Phase 6C #1 (hardening): count Core nodes AND trap sites in one pass. Trap sites
    = the operations that lower to a runtime abort: checked arithmetic
    (`add/sub/mul/div/mod`, but NOT wrapping/saturating) plus array-index reads and
    writes (bounds checks). Returns `(nodes, trapSites)`. -/
def binOpTraps : BinOp → Nat
  | .add | .sub | .mul | .div | .mod => 1
  | _ => 0

/-- Sum a list of `(nodes, traps)` pairs componentwise. -/
def sumNT (xs : List (Nat × Nat)) : Nat × Nat :=
  xs.foldl (fun (a b : Nat × Nat) => (a.1 + b.1, a.2 + b.2)) (0, 0)
/-- `(0,0)` for an absent optional child. -/
def optNT (o : Option (Nat × Nat)) : Nat × Nat := o.getD (0, 0)

mutual
partial def cExprNT : CExpr → Nat × Nat
  | .intLit .. | .floatLit .. | .boolLit .. | .strLit .. | .charLit .. | .ident .. | .fnRef .. => (1, 0)
  | .binOp op l r _ => let (nl,tl) := cExprNT l; let (nr,tr) := cExprNT r; (1+nl+nr, binOpTraps op + tl + tr)
  | .unaryOp _ x _ => let (n,t) := cExprNT x; (1+n, t)
  | .call _ _ args _ => let (n,t) := sumNT (args.map cExprNT); (1+n, t)
  | .structLit _ _ fields _ => let (n,t) := sumNT (fields.map (fun f => cExprNT f.2)); (1+n, t)
  | .fieldAccess o _ _ => let (n,t) := cExprNT o; (1+n, t)
  | .enumLit _ _ _ fields _ => let (n,t) := sumNT (fields.map (fun f => cExprNT f.2)); (1+n, t)
  | .match_ s arms _ => let (ns,ts) := cExprNT s; let (na,ta) := sumNT (arms.map cArmNT); (1+ns+na, ts+ta)
  | .borrow x _ => let (n,t) := cExprNT x; (1+n, t)
  | .borrowMut x _ => let (n,t) := cExprNT x; (1+n, t)
  | .deref x _ => let (n,t) := cExprNT x; (1+n, t)
  | .arrayLit elems _ => let (n,t) := sumNT (elems.map cExprNT); (1+n, t)
  | .arrayIndex a i _ => let (na,ta) := cExprNT a; let (ni,ti) := cExprNT i; (1+na+ni, 1+ta+ti)
  | .cast x _ => let (n,t) := cExprNT x; (1+n, t)
  | .try_ x _ => let (n,t) := cExprNT x; (1+n, t)
  | .allocCall x a _ => let (nx,tx) := cExprNT x; let (na,ta) := cExprNT a; (1+nx+na, tx+ta)
  | .ifExpr c t_ el _ => let (nc,tc):=cExprNT c; let (nt,tt):=sumNT (t_.map cStmtNT); let (ne,te):=sumNT (el.map cStmtNT); (1+nc+nt+ne, tc+tt+te)
partial def cArmNT : CMatchArm → Nat × Nat
  | .enumArm _ _ _ g body => let (ng,tg):=optNT (g.map cExprNT); let (nb,tb):=sumNT (body.map cStmtNT); (1+ng+nb, tg+tb)
  | .litArm v g body => let (nv,tv):=cExprNT v; let (ng,tg):=optNT (g.map cExprNT); let (nb,tb):=sumNT (body.map cStmtNT); (1+nv+ng+nb, tv+tg+tb)
  | .varArm _ _ g body => let (ng,tg):=optNT (g.map cExprNT); let (nb,tb):=sumNT (body.map cStmtNT); (1+ng+nb, tg+tb)
  | .rangeArm lo hi _ g body => let (nl,tl):=cExprNT lo; let (nh,th):=cExprNT hi; let (ng,tg):=optNT (g.map cExprNT); let (nb,tb):=sumNT (body.map cStmtNT); (1+nl+nh+ng+nb, tl+th+tg+tb)
partial def cStmtNT : CStmt → Nat × Nat
  | .letDecl _ _ _ v => let (n,t):=cExprNT v; (1+n, t)
  | .assign _ v => let (n,t):=cExprNT v; (1+n, t)
  | .return_ v _ => let (n,t):=optNT (v.map cExprNT); (1+n, t)
  | .expr e _ => let (n,t):=cExprNT e; (1+n, t)
  | .ifElse c t_ el => let (nc,tc):=cExprNT c; let (nt,tt):=sumNT (t_.map cStmtNT); let (ne,te):=sumNT ((el.getD []).map cStmtNT); (1+nc+nt+ne, tc+tt+te)
  | .while_ c body _ step => let (nc,tc):=cExprNT c; let (nb,tb):=sumNT (body.map cStmtNT); let (ns,ts):=sumNT (step.map cStmtNT); (1+nc+nb+ns, tc+tb+ts)
  | .fieldAssign o _ v => let (no,to):=cExprNT o; let (nv,tv):=cExprNT v; (1+no+nv, to+tv)
  | .derefAssign t_ v => let (nt,tt):=cExprNT t_; let (nv,tv):=cExprNT v; (1+nt+nv, tt+tv)
  | .arrayIndexAssign a i v => let (na,ta):=cExprNT a; let (ni,ti):=cExprNT i; let (nv,tv):=cExprNT v; (1+na+ni+nv, 1+ta+ti+tv)
  | .break_ v _ => let (n,t):=optNT (v.map cExprNT); (1+n, t)
  | .continue_ _ => (1, 0)
  | .defer b => let (n,t):=cExprNT b; (1+n, t)
  | .borrowIn _ _ _ _ _ body => let (n,t):=sumNT (body.map cStmtNT); (1+n, t)
end

/-- `(nodes, trapSites)` across a Core module forest (recursing submodules). -/
partial def coreNodesTraps (ms : List CModule) : Nat × Nat :=
  ms.foldl (fun (acc : Nat × Nat) cm =>
    let bodyNT := cm.functions.foldl (fun (a : Nat×Nat) fn =>
      (fn.body.map cStmtNT).foldl (fun (x y : Nat×Nat) => (x.1+y.1, x.2+y.2)) a) (0,0)
    let (sn, st) := coreNodesTraps cm.submodules
    (acc.1 + bodyNT.1 + sn, acc.2 + bodyNT.2 + st)) (0, 0)

/-- Phase 6C #1 (v2): build the pipeline telemetry trace as a stable-schema JSON
    string. Records per-stage structural counts (AST nodes, Core modules/functions/
    enums/structs/nodes, post-mono, SSA blocks/instructions), trap-site count,
    per-stage `timing_ms`, and a platform-graceful `rss_kb` (null when unavailable),
    plus the opaque compiler identity and diagnostic count. This is NOT a performance
    claim — the gate pins schema stability + monotonic sanity, never timing values
    (Phase 17 owns benchmarks). Built by concatenation to keep the JSON unambiguous. -/
def telemetryJson (compiler : String) (astNodes : Nat) (core : ValidatedCore)
    (mono : MonomorphizedProgram) (ssa : SSAProgram) (diagnostics : Nat)
    (timingMs : List (String × Nat) := []) (rssKb : Option Nat := none) : String :=
  let (cMods, cFns, cEnums, cStructs) := countCore core.coreModules
  let (cNodes, cTraps) := coreNodesTraps core.coreModules
  let (mMods, mFns, _, _) := countCore mono.coreModules
  let (sMods, sFns, sBlks, sInsts) := countSSA ssa.ssaModules
  let esc := fun (t : String) => t.replace "\\" "\\\\" |>.replace "\"" "\\\""
  let lb := "{"; let rb := "}"
  let timingJson := lb ++ String.intercalate ","
    (timingMs.map (fun (s, ms) => "\"" ++ esc s ++ "\":" ++ toString ms)) ++ rb
  let rssJson := match rssKb with | some kb => toString kb | none => "null"
  lb ++ "\"schema\":\"concrete.pipeline.telemetry.v2\","
     ++ "\"compiler\":\"" ++ esc compiler ++ "\","
     ++ "\"stages\":" ++ lb
       ++ "\"ast\":" ++ lb ++ s!"\"nodes\":{astNodes}" ++ rb ++ ","
       ++ "\"core\":" ++ lb
         ++ s!"\"modules\":{cMods},\"functions\":{cFns},\"enums\":{cEnums},\"structs\":{cStructs},\"nodes\":{cNodes}" ++ rb ++ ","
       ++ "\"mono\":" ++ lb
         ++ s!"\"modules\":{mMods},\"functions\":{mFns}" ++ rb ++ ","
       ++ "\"ssa\":" ++ lb
         ++ s!"\"modules\":{sMods},\"functions\":{sFns},\"blocks\":{sBlks},\"instructions\":{sInsts}" ++ rb
     ++ rb ++ ","
     ++ "\"trap_sites\":" ++ toString cTraps ++ ","
     ++ "\"timing_ms\":" ++ timingJson ++ ","
     ++ "\"rss_kb\":" ++ rssJson ++ ","
     ++ "\"diagnostics\":" ++ toString diagnostics
     ++ rb

-- ============================================================
-- Phase 6C #3: per-stage pipeline trace (`concrete trace-pipeline --json`)
-- ============================================================

/-- Build the pipeline-trace JSON. `okStages` are the stages that passed, in
    order; `fail` names the FIRST failing stage and its diagnostic code (`none`
    for an accepted program); `counts` is a telemetry-JSON body (or `"null"`).
    The point of the trace is to name the first phase that introduced/preserved/
    rejected the relevant fact — `firstFailingStage` for a rejected program, all
    stages `ok` for an accepted one. -/
def traceJson (compiler input : String) (okStages : List String)
    (fail : Option (String × String)) (counts : String) : String :=
  let esc := fun (t : String) => t.replace "\\" "\\\\" |>.replace "\"" "\\\""
  -- Emit only the file's basename, never the absolute path: the trace must not
  -- leak private/host directory paths (guarded by check_trace_pipeline.sh).
  let baseName := (input.splitOn "/").getLastD input
  let lb := "{"; let rb := "}"
  let okJsons := okStages.map (fun s =>
    lb ++ "\"stage\":\"" ++ s ++ "\",\"status\":\"ok\"" ++ rb)
  let failJsons := match fail with
    | some (s, c) => [lb ++ "\"stage\":\"" ++ s ++ "\",\"status\":\"error\",\"code\":\"" ++ esc c ++ "\"" ++ rb]
    | none => []
  let stageArr := "[" ++ String.intercalate "," (okJsons ++ failJsons) ++ "]"
  let outcome := if fail.isSome then "rejected" else "accepted"
  let failStage := match fail with | some (s, _) => "\"" ++ s ++ "\"" | none => "null"
  lb ++ "\"schema\":\"concrete.pipeline.trace.v1\","
     ++ "\"compiler\":\"" ++ esc compiler ++ "\","
     ++ "\"input\":\"" ++ esc baseName ++ "\","
     ++ "\"outcome\":\"" ++ outcome ++ "\","
     ++ "\"firstFailingStage\":" ++ failStage ++ ","
     ++ "\"stages\":" ++ stageArr ++ ","
     ++ "\"counts\":" ++ counts
     ++ rb

end Pipeline
end Concrete

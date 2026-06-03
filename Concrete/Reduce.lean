import Concrete.Parser
import Concrete.Format
import Concrete.Pipeline
import Concrete.Verify
import Concrete.ProofCore

namespace Concrete.Reduce

/-! ## Testcase Reducer — syntax-aware shrinking for compiler failure reproduction

The reducer takes a predicate (does this source still trigger the bug?) and
a source file, then applies coarse syntax-aware shrinking passes in a fixpoint
loop until no further reduction is possible.

Shrinking passes (ordered from coarsest to finest):
1. Remove entire top-level items (functions, structs, enums, impl blocks, etc.)
2. Remove statements from function bodies
3. Remove match arms (keeping at least one)
4. Remove else branches
5. Simplify function bodies to a single return/expression
-/

/-- Predicate kinds for the reducer. -/
inductive Predicate where
  | parseError (substr : Option String)
  | resolveError (substr : Option String)
  | checkError (substr : Option String)
  | elabError (substr : Option String)
  | coreCheckError (substr : Option String)
  | monoError (substr : Option String)
  | lowerError (substr : Option String)
  | consistencyViolation
  | verifyWarning
  | crash
  /-- Run an external command on each candidate; the candidate's source
      is written to a temp file and the path is appended as the final
      argument. Exit code 0 means the bug is still present. -/
  | external (cmd : String)

/-- Parse a predicate string like "check-error:expected Int" into a Predicate. -/
def parsePredicate (s : String) : Option Predicate :=
  let (kind, substr) := match s.splitOn ":" with
    | [k] => (k, none)
    | k :: rest => (k, some (":".intercalate rest))
    | [] => (s, none)
  match kind with
  | "parse-error"      => some (.parseError substr)
  | "resolve-error"    => some (.resolveError substr)
  | "check-error"      => some (.checkError substr)
  | "elab-error"       => some (.elabError substr)
  | "core-check-error" => some (.coreCheckError substr)
  | "mono-error"       => some (.monoError substr)
  | "lower-error"      => some (.lowerError substr)
  | "consistency-violation" => some .consistencyViolation
  | "verify-warning"   => some .verifyWarning
  | "crash"            => some .crash
  | "external"         => substr.map .external
  | _ => none

/-- Check if diagnostics contain a substring (if specified). -/
private def diagsMatch (ds : Diagnostics) (substr : Option String) : Bool :=
  match substr with
  | none => true
  | some s => ds.any fun d => (d.message.splitOn s).length > 1

/-- Evaluate a predicate against source code.
    Returns true if the predicate holds (the bug is still present). -/
def evalPredicate (pred : Predicate) (source : String)
    (resolveAllModules : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO Bool := do
  match pred with
  | .parseError substr =>
    match Pipeline.parse source with
    | .error ds => return diagsMatch ds substr
    | .ok _ => return false
  | .crash =>
    -- crash = parse succeeds but anything after can fail
    match Pipeline.parse source with
    | .error _ => return false
    | .ok _ => return true
  | .external cmd => do
    -- Write the candidate to a temp file, then run the command with the
    -- file path appended as the final argument. The command is split on
    -- whitespace so simple wrappers like
    --   "scripts/reduce/expect-error-code.sh E0708"
    -- work without escaping.
    let stamp ← IO.monoMsNow
    let tmpPath := s!"/tmp/concrete-reduce-{stamp}.con"
    IO.FS.writeFile ⟨tmpPath⟩ source
    let parts := cmd.splitOn " " |>.filter (fun p => !p.isEmpty)
    let exitCode ← (do
      match parts with
      | [] => return 1
      | exe :: args =>
        let allArgs := (args ++ [tmpPath]).toArray
        let result ← IO.Process.output { cmd := exe, args := allArgs }
        return result.exitCode)
    try IO.FS.removeFile ⟨tmpPath⟩ catch _ => pure ()
    return exitCode == 0
  | _ =>
    -- All other predicates need at least parsing to succeed
    match Pipeline.parse source with
    | .error _ => return false
    | .ok parsed =>
    let _srcMap : SourceMap := [("reduce.con", source)]
    match ← Pipeline.resolveFiles "." parsed "reduce.con" resolveAllModules with
    | .error ds =>
      match pred with
      | .resolveError substr => return diagsMatch ds substr
      | _ => return false
    | .ok (resolved, _subSrcMap) =>
    let summary := Pipeline.buildSummary resolved
    match Pipeline.resolve resolved summary with
    | .error ds =>
      match pred with
      | .resolveError substr => return diagsMatch ds substr
      | _ => return false
    | .ok resolvedProg =>
    match Pipeline.check resolvedProg summary with
    | .error ds =>
      match pred with
      | .checkError substr => return diagsMatch ds substr
      | _ => return false
    | .ok () =>
    match Pipeline.elaborate resolvedProg summary with
    | .error ds =>
      match pred with
      | .elabError substr => return diagsMatch ds substr
      | _ => return false
    | .ok elabProg =>
    match Pipeline.coreCheck elabProg with
    | .error ds =>
      match pred with
      | .coreCheckError substr => return diagsMatch ds substr
      | _ => return false
    | .ok validCore =>
    -- consistency-violation and verify-warning check here
    match pred with
    | .consistencyViolation =>
      let pc := extractProofCore validCore [] []
      let violations := pc.selfCheck
      return !violations.isEmpty
    | .verifyWarning =>
      let ds := Pipeline.verifyPostElab validCore.coreModules
      return !ds.isEmpty
    | .monoError substr =>
      match Pipeline.monomorphize validCore with
      | .error ds => return diagsMatch ds substr
      | .ok _ => return false
    | .lowerError substr =>
      match Pipeline.monomorphize validCore with
      | .error _ => return false
      | .ok mono =>
        match Pipeline.lower mono with
        | .error ds => return diagsMatch ds substr
        | .ok _ => return false
    | _ => return false

-- ============================================================
-- Shrinking passes
-- ============================================================

/-- Try removing the i-th element from a list. -/
private def removeAt (xs : List α) (i : Nat) : List α :=
  let rec go (j : Nat) : List α → List α
    | [] => []
    | x :: rest => if j == i then rest else x :: go (j + 1) rest
  go 0 xs

/-- Replace the i-th element of a list. -/
private def replaceAt (xs : List α) (i : Nat) (v : α) : List α :=
  let rec go (j : Nat) : List α → List α
    | [] => []
    | x :: rest => if j == i then v :: rest else x :: go (j + 1) rest
  go 0 xs

/-- Get the i-th element of a list. -/
private def getAt? (xs : List α) (i : Nat) : Option α :=
  match xs, i with
  | [], _ => none
  | x :: _, 0 => some x
  | _ :: rest, n + 1 => getAt? rest n

/-- Try each single-element removal from a list, returning the first
    candidate whose formatted source passes the predicate. -/
private def tryShrinkList (items : List α) (rebuild : List α → List Module)
    (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (Option (List α)) := do
  for i in List.range items.length do
    let candidate := removeAt items i
    let source := formatProgram (rebuild candidate)
    if ← evalPredicate pred source resolveAll then
      return some candidate
  return none

/-- Remove top-level items from a module one at a time. -/
partial def shrinkTopLevel (modules : List Module) (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (List Module) := do
  -- For single-module programs, shrink the module's contents
  match modules with
  | [m] => do
    let mut mod := m
    let mut progress := true
    while progress do
      progress := false
      -- Try removing functions
      if let some fns ← tryShrinkList mod.functions (fun fs => [{ mod with functions := fs }]) pred resolveAll then
        mod := { mod with functions := fns }; progress := true; continue
      -- Try removing structs
      if let some ss ← tryShrinkList mod.structs (fun xs => [{ mod with structs := xs }]) pred resolveAll then
        mod := { mod with structs := ss }; progress := true; continue
      -- Try removing enums
      if let some es ← tryShrinkList mod.enums (fun xs => [{ mod with enums := xs }]) pred resolveAll then
        mod := { mod with enums := es }; progress := true; continue
      -- Try removing impl blocks
      if let some ibs ← tryShrinkList mod.implBlocks (fun xs => [{ mod with implBlocks := xs }]) pred resolveAll then
        mod := { mod with implBlocks := ibs }; progress := true; continue
      -- Try removing trait defs
      if let some ts ← tryShrinkList mod.traits (fun xs => [{ mod with traits := xs }]) pred resolveAll then
        mod := { mod with traits := ts }; progress := true; continue
      -- Try removing trait impls
      if let some tis ← tryShrinkList mod.traitImpls (fun xs => [{ mod with traitImpls := xs }]) pred resolveAll then
        mod := { mod with traitImpls := tis }; progress := true; continue
      -- Try removing constants
      if let some cs ← tryShrinkList mod.constants (fun xs => [{ mod with constants := xs }]) pred resolveAll then
        mod := { mod with constants := cs }; progress := true; continue
      -- Try removing type aliases
      if let some tas ← tryShrinkList mod.typeAliases (fun xs => [{ mod with typeAliases := xs }]) pred resolveAll then
        mod := { mod with typeAliases := tas }; progress := true; continue
      -- Try removing extern fns
      if let some efs ← tryShrinkList mod.externFns (fun xs => [{ mod with externFns := xs }]) pred resolveAll then
        mod := { mod with externFns := efs }; progress := true; continue
      -- Try removing newtypes
      if let some nts ← tryShrinkList mod.newtypes (fun xs => [{ mod with newtypes := xs }]) pred resolveAll then
        mod := { mod with newtypes := nts }; progress := true; continue
      -- Try removing imports
      if let some imps ← tryShrinkList mod.imports (fun xs => [{ mod with imports := xs }]) pred resolveAll then
        mod := { mod with imports := imps }; progress := true; continue
      -- Try removing submodules
      if let some subs ← tryShrinkList mod.submodules (fun xs => [{ mod with submodules := xs }]) pred resolveAll then
        mod := { mod with submodules := subs }; progress := true; continue
    return [mod]
  | _ => return modules

/-- Remove statements from function bodies one at a time. -/
partial def shrinkStatements (modules : List Module) (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (List Module) := do
  match modules with
  | [m] => do
    let mut mod := m
    let mut progress := true
    while progress do
      progress := false
      for fi in List.range mod.functions.length do
        if let some fn := getAt? mod.functions fi then
          if fn.body.length <= 1 then continue
          for si in List.range fn.body.length do
            let newBody := removeAt fn.body si
            let newFn := { fn with body := newBody }
            let newFns := replaceAt mod.functions fi newFn
            let candidate := formatProgram [{ mod with functions := newFns }]
            if ← evalPredicate pred candidate resolveAll then
              mod := { mod with functions := newFns }
              progress := true
              break
          if progress then break
    return [mod]
  | _ => return modules

/-- Remove match arms (keeping at least one) from expressions in function bodies. -/
partial def shrinkMatchArms (modules : List Module) (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (List Module) := do
  match modules with
  | [m] => do
    let mut mod := m
    let mut progress := true
    while progress do
      progress := false
      for fi in List.range mod.functions.length do
        if let some fn := getAt? mod.functions fi then
          for si in List.range fn.body.length do
            if let some stmt := getAt? fn.body si then
              let candidates := stmtMatchCandidates stmt
              for newStmt in candidates do
                let newBody := replaceAt fn.body si newStmt
                let newFn := { fn with body := newBody }
                let newFns := replaceAt mod.functions fi newFn
                let candidate := formatProgram [{ mod with functions := newFns }]
                if ← evalPredicate pred candidate resolveAll then
                  mod := { mod with functions := newFns }
                  progress := true
                  break
              if progress then break
          if progress then break
    return [mod]
  | _ => return modules
where
  /-- Generate candidates by removing one match arm from any match in a statement. -/
  stmtMatchCandidates (stmt : Stmt) : List Stmt :=
    match stmt with
    | .expr sp (.match_ msp scrut arms) =>
      if arms.length <= 1 then []
      else (List.range arms.length).filterMap fun i =>
        let newArms := removeAt arms i
        if newArms.isEmpty then none
        else some (.expr sp (.match_ msp scrut newArms))
    | .return_ sp (some (.match_ msp scrut arms)) =>
      if arms.length <= 1 then []
      else (List.range arms.length).filterMap fun i =>
        let newArms := removeAt arms i
        if newArms.isEmpty then none
        else some (.return_ sp (some (.match_ msp scrut newArms)))
    | .letDecl sp name isMut ty (.match_ msp scrut arms) isGhost =>
      if arms.length <= 1 then []
      else (List.range arms.length).filterMap fun i =>
        let newArms := removeAt arms i
        if newArms.isEmpty then none
        else some (.letDecl sp name isMut ty (.match_ msp scrut newArms) isGhost)
    | _ => []

/-- Remove else branches from if statements. -/
partial def shrinkElseBranches (modules : List Module) (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    : IO (List Module) := do
  match modules with
  | [m] => do
    let mut mod := m
    let mut progress := true
    while progress do
      progress := false
      for fi in List.range mod.functions.length do
        if let some fn := getAt? mod.functions fi then
          for si in List.range fn.body.length do
            if let some stmt := getAt? fn.body si then
              match stmt with
              | .ifElse sp cond then_ (some _) =>
                let newStmt := Stmt.ifElse sp cond then_ none
                let newBody := replaceAt fn.body si newStmt
                let newFn := { fn with body := newBody }
                let newFns := replaceAt mod.functions fi newFn
                let candidate := formatProgram [{ mod with functions := newFns }]
                if ← evalPredicate pred candidate resolveAll then
                  mod := { mod with functions := newFns }
                  progress := true
                  break
              | _ => pure ()
          if progress then break
    return [mod]
  | _ => return modules

/-- Line-based reduction for programs that don't parse.
    Tries removing one line at a time. -/
private partial def reduceLines (source : String) (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    (verbose : Bool)
    : IO String := do
  let lines := source.splitOn "\n"
  let mut result := lines
  let mut progress := true
  let mut round := (0 : Nat)
  while progress do
    progress := false
    round := round + 1
    if verbose then IO.eprintln s!"  line-reduce round {round}: {result.length} lines"
    for i in List.range result.length do
      let candidate := removeAt result i
      let candidateSrc := "\n".intercalate candidate
      if ← evalPredicate pred candidateSrc resolveAll then
        result := candidate
        progress := true
        break
  return "\n".intercalate result

/-- Run all shrinking passes in a fixpoint loop.
    Returns the minimized source code. -/
partial def reduce (source : String) (pred : Predicate)
    (resolveAll : String → List Module → String → IO (Except String (List Module × SourceMap)))
    (verbose : Bool := false)
    : IO String := do
  -- First verify the predicate holds on the original source
  if !(← evalPredicate pred source resolveAll) then
    IO.eprintln "error: predicate does not hold on the original source"
    return source

  -- Parse the source to get the AST
  let modules ← match parse source with
    | .ok ms => pure ms
    | .error _ =>
      -- If we can't parse, we can't do syntax-aware reduction
      -- For parse-error predicates, try line-based reduction
      if verbose then IO.eprintln "  (source doesn't parse — trying line-based reduction)"
      return ← reduceLines source pred resolveAll verbose

  let mut mods := modules
  let mut prevSize := (formatProgram mods).length
  let mut round := (0 : Nat)
  let mut cont := true

  while cont do
    round := round + 1
    if verbose then IO.eprintln s!"  round {round}: {prevSize} chars"

    -- Pass 1: Remove top-level items
    mods ← shrinkTopLevel mods pred resolveAll

    -- Pass 2: Remove statements
    mods ← shrinkStatements mods pred resolveAll

    -- Pass 3: Remove match arms
    mods ← shrinkMatchArms mods pred resolveAll

    -- Pass 4: Remove else branches
    mods ← shrinkElseBranches mods pred resolveAll

    let newSize := (formatProgram mods).length
    if newSize < prevSize then
      prevSize := newSize
    else
      cont := false

  return formatProgram mods

end Concrete.Reduce

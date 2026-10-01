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
import Concrete.Semantics.IntArith
import Concrete.Report.TermTranslate
import Concrete.Semantics.Capabilities
-- Obligation collectors need only the contract/VC helper cluster, not the
-- capability/arith/unsafe/layout report renderers (pipeline #34).
import Concrete.Report.ReportVC

namespace Concrete
namespace Report

/-- The NAME an array access is rooted at, peeling `(…)`.

    `a[i]` and `(a)[i]` denote the same access, but bounds discovery matched a BARE `.ident`
    and so generated an obligation only for the first — no VC, no `unproven` marker, nothing
    in the report. Found by `collectIndexUsesE_complete`: writing the completeness predicate
    forced the question "which array expressions are actually recorded?", and the honest
    answer was narrower than anyone had assumed.

    Only `.paren` is peeled. It is meaning- AND type-preserving, so the enclosing
    length lookup (`ScopeDecls.sizes`, keyed by name) stays correct. `.deref`/`.fieldAccess` are NOT
    peeled: they change the type at which the length must be read, so treating them as the
    same name would trade a missing obligation for a WRONG one. -/
def arrayRootName : Expr → Option String
  | .ident _ n => some n
  | .paren _ x => arrayRootName x
  | _ => none

/-- Termination helper for the discovery walkers (R-0455).

    An `else` branch is `Option (List Stmt)`, and the walkers recurse into `el.getD []`. The
    membership fact `attach` supplies is about the DEFAULTED list, so the size bound has to
    climb back out to the `Option` itself; `List.sizeOf_lt_of_mem` alone does not reach. -/
theorem sizeOf_mem_getD {α} [SizeOf α] {o : Option (List α)} {x : α}
    (h : x ∈ o.getD []) : sizeOf x < sizeOf o := by
  cases o with
  | none => simp at h
  | some l =>
    simp only [Option.getD_some] at h
    have := List.sizeOf_lt_of_mem h
    simp +arith
    omega


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
def collectIndexUsesE : Expr → List (Expr × Expr)
  -- EVERY indexed access is recorded, as the array EXPRESSION rather than a name. Requiring
  -- a bare `.ident` here is what made `b.data[i]` invisible to bounds discovery: not an
  -- unproven VC, absent entirely. Whether a length can be resolved is a separate question,
  -- answered by `arrayAccessOf` and reported when the answer is no.
  | .arrayIndex _ a idx => (a, idx) :: (collectIndexUsesE a ++ collectIndexUsesE idx)
  | .binOp _ _ l r => collectIndexUsesE l ++ collectIndexUsesE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .cast _ x _ | .fieldAccess _ x _ => collectIndexUsesE x
  | .arrayLit _ es => es.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesE e)
  | .call _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesE e)
  | .methodCall _ o _ _ args => collectIndexUsesE o ++ args.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesE e)
  | .staticMethodCall _ _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesE e)
  | .structLit _ _ _ fs base => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectIndexUsesE fe) ++ (base.attach.map (fun ⟨e, _⟩ => collectIndexUsesE e)).getD []
  | .enumLit _ _ _ _ fs => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectIndexUsesE fe)
  | .allocCall _ x a => collectIndexUsesE x ++ collectIndexUsesE a
  | .ifExpr _ c t el =>
      collectIndexUsesE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesS e) ++ el.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesS e)
  | .match_ _ s _ => collectIndexUsesE s
  | _ => []
termination_by x => sizeOf x
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
def collectIndexUsesS : Stmt → List (Expr × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectIndexUsesE v
  | .return_ _ (some v) => collectIndexUsesE v
  | .ifElse _ c t el => collectIndexUsesE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesS e) ++ (el.getD []).attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesS e)
  | .while_ _ c b _ => collectIndexUsesE c ++ b.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesS e)
  | .forLoop _ init c step b _ =>
      (init.attach.map (fun ⟨e, _⟩ => collectIndexUsesS e)).getD [] ++ collectIndexUsesE c
        ++ (step.attach.map (fun ⟨e, _⟩ => collectIndexUsesS e)).getD [] ++ b.attach.flatMap (fun ⟨e, _⟩ => collectIndexUsesS e)
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectIndexUsesE o ++ collectIndexUsesE v
  | .arrayIndexAssign _ a idx v =>
      (a, idx) :: (collectIndexUsesE a ++ collectIndexUsesE idx ++ collectIndexUsesE v)
  -- Missing until 2026-08-06, as in the three sibling statement walkers: `assert_`/`assume_`
  -- carry a condition, `borrowIn` a body, and both destructuring forms a scrutinee, so each is a
  -- place an indexed access can hide.
  | .assert_ _ c | .assume_ _ c => collectIndexUsesE c
  | .borrowIn _ _ _ _ _ b => b.attach.flatMap (fun ⟨x, _⟩ => collectIndexUsesS x)
  | .letDestructure _ _ _ _ v _ => collectIndexUsesE v
  | .letStructDestructure _ _ _ v => collectIndexUsesE v
  | _ => []
termination_by x => sizeOf x
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
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

/-- `{fnQual}@{line}#{obl}` — the loop VC id. Duplicated from `loopVCKey` below only
    because that definition appears later in the file than its first use here; the gate
    asserts the two formats stay identical. -/
def loopVCKeyFwd (fnQual : String) (line : Nat) (obl : String) : String :=
  s!"{fnQual}@{line}#{obl}"

/-- The loop VCs an obligation's hypotheses OWE — R-0461, closing H23.

    `loopHypsAt` puts a loop's `#[invariant]` expressions verbatim into scope, so an
    obligation discharged inside that loop was proved ASSUMING them. Whether they hold is a
    separate question answered by that loop's O1 (init) and O2 (preservation), and until
    2026-08-03 nothing related the two: a bounds obligation could read
    `proved_by_multi_kernel` while the invariant it rested on read `unproven` in the same
    report, and the compiled program aborted. See `examples/unsound_hypothesis/`.

    Recovered by MATCHING rather than by threading a second value through `scopedWalk`. The
    walker inserts the invariant `Expr` unchanged, so an obligation owes a loop exactly when
    its hypotheses still contain one of that loop's invariants — and `dropStaleHyps` having
    removed it means the obligation no longer assumes it and correctly owes nothing.

    Compared on `fmtExpr` because `Expr` derives only `Repr`: it carries spans, so structural
    equality would distinguish two occurrences of the same invariant. Over-matching is the
    safe direction here (extra debt is conservative); under-matching would be unsound, which
    is why the comparison is on the normalised form rather than on span-bearing terms.

    Only O1 and O2 are owed. O3 (exit implies post) is about the function's `#[ensures]`, and
    O4/O5 are the variant's termination argument — neither is what makes an in-loop
    hypothesis true at the point the obligation is discharged. -/
def loopInvariantDebt (f : FnDef) (fq : String) (hyps : List Expr) : List String :=
  let hypForms := hyps.map Concrete.fmtExpr
  f.loopContracts.flatMap fun lc =>
    if lc.invariants.any (fun inv => hypForms.contains (Concrete.fmtExpr inv))
    then [loopVCKeyFwd fq lc.line "O1", loopVCKeyFwd fq lc.line "O2"]
    else []

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
/-- Collect every struct in a module tree, submodules included. -/
def allStructsOf (m : Module) : List StructDef :=
  m.structs ++ m.submodules.attach.flatMap (fun ⟨sub, _⟩ => allStructsOf sub)
termination_by sizeOf m
decreasing_by
  all_goals simp_wf
  all_goals
    (rename_i h
     have := List.sizeOf_lt_of_mem h
     cases m
     simp +arith at this ⊢
     omega)

/-- Struct name → its field types. Global, not scoped, so it is passed alongside `ScopeDecls`
    rather than inside it. -/
abbrev StructFieldEnv := List (String × List (String × Ty))

/-- Struct field types for a whole program, keyed by struct name. -/
def structFieldEnv (modules : List Module) : StructFieldEnv :=
  (modules.flatMap allStructsOf).map fun sd =>
    (sd.name, sd.fields.map (fun f => (f.name, f.ty)))

/-- Peel references/pointers to reach a named struct. -/
def namedStructOf : Ty → Option String
  | .named n => some n
  -- A struct whose only arguments are CAPABILITIES has the same fields for every
  -- instantiation, so field paths through it resolve exactly as through `.named n`.
  -- Without this arm, giving a struct a capability parameter would silently remove it
  -- from bounds discovery. (Structs with TYPE arguments stay excluded, as before: their
  -- field types would need substituting.)
  | .generic n [] _ => some n
  | .ref t | .refMut t | .ptrMut t | .ptrConst t | .heap t => namedStructOf t
  | _ => none

/-- Type of a PLACE expression, following field paths through struct definitions.

    `exprIntTy` deliberately stops at integer-typed leaves; this one exists to answer "what
    array is `b.data`?", which is what bounds discovery could not previously ask. -/
def placeTy (structs : StructFieldEnv) (tys : List (String × Ty)) : Expr → Option Ty
  | .ident _ n => tys.lookup n
  | .paren _ e | .borrow _ e | .borrowMut _ e | .deref _ e => placeTy structs tys e
  | .fieldAccess _ o f => do
      let ot ← placeTy structs tys o
      let sname ← namedStructOf ot
      let fields ← structs.lookup sname
      fields.lookup f
  | _ => none

/-- Declarations in scope at a program point: variable types and fixed array lengths.

    Both were previously flat per-function maps (`varTyMap`, `arraySizeMap`) resolved by NAME,
    which meant a shadowed variable was resolved from the wrong binding. For array sizes that
    produced a bound about the wrong array; for types it produced a shift-width obligation
    about the wrong width — `x << 40` on an `i8` was reported PROVED because an earlier
    `x : i64` supplied the width. Threading them per scope is what makes the answer correspond
    to the code.

    Kept as one record rather than two threaded parameters so a future declaration kind cannot
    be added to one and forgotten in the other. -/
structure ScopeDecls where
  /-- Variable → declared type: parameters and ANNOTATED lets. Unannotated lets are absent
      deliberately — inferring a type here risks disagreeing with the checker, and a wrong
      type is exactly the defect being fixed. Their obligations are dropped and named. -/
  tys   : List (String × Ty) := []
  /-- Variable → fixed array length. Includes lengths inferred from an array-literal
      initialiser, which needs no type inference to be certain of. -/
  sizes : List (String × Nat) := []
  deriving Inhabited

/-- Declarations introduced by THIS statement — deliberately not recursive; scoping is the
    walker's job. -/
def declsOwn : Stmt → ScopeDecls
  | .letDecl _ nm _ (some t) _ _ =>
      { tys := [(nm, t)]
      , sizes := match t with | .array _ n => [(nm, n)] | _ => [] }
  | .letDecl _ nm _ none (.arrayLit _ es) _ => { sizes := [(nm, es.length)] }
  | _ => {}

/-- Inner declarations shadow outer ones: prepended, and every lookup takes the first match. -/
def ScopeDecls.extend (inner outer : ScopeDecls) : ScopeDecls :=
  { tys := inner.tys ++ outer.tys, sizes := inner.sizes ++ outer.sizes }

/-- The declarations a function body starts from. -/
def paramDecls (f : FnDef) : ScopeDecls :=
  { tys := f.params.map (fun p => (p.name, p.ty))
  , sizes := f.params.filterMap fun p =>
      match p.ty with | .array _ n => some (p.name, n) | _ => none }

mutual
partial def scopedWalkSizedS {α}
    (leaf : List Expr → ScopeDecls → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) (decls : ScopeDecls) :
    Stmt → List α
  | s@(.ifElse _ c t el) =>
      leaf scope decls s
        ++ scopedWalkSizedB leaf lcs (scope ++ [c]) decls t
        ++ scopedWalkSizedB leaf lcs (scope ++ (negateGuard c).toList) decls (el.getD [])
  | s@(.while_ sp _ b _) =>
      leaf scope decls s
        ++ scopedWalkSizedB leaf lcs (scope ++ loopHypsAt lcs sp.line) decls b
  | s@(.forLoop sp init _ step b _) =>
      -- init, then this statement's own leaves (the loop condition), then step,
      -- then body — the traversal ORDER every family's old walker used, so the
      -- positional `#idx`/`#pre`/… keys are preserved across the migration.
      -- Anything `init` declares is visible to the condition, step and body.
      let decls' := ((init.map declsOwn).getD {}).extend decls
      ((init.map (scopedWalkSizedS leaf lcs scope decls)).getD [])
        ++ leaf scope decls' s
        ++ ((step.map (scopedWalkSizedS leaf lcs scope decls')).getD [])
        ++ scopedWalkSizedB leaf lcs (scope ++ loopHypsAt lcs sp.line) decls' b
  | s@(.borrowIn _ _ _ _ _ b) =>
      leaf scope decls s ++ scopedWalkSizedB leaf lcs scope decls b
  | s => leaf scope decls s
partial def scopedWalkSizedB {α}
    (leaf : List Expr → ScopeDecls → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) (decls : ScopeDecls) :
    List Stmt → List α
  | [] => []
  | s :: rest =>
      let restScope := match s with
        | .ifElse _ c t none => if blockTerminates t then scope ++ (negateGuard c).toList
                                else dropStaleHyps scope (assignedScalarsS s)
        | _ => dropStaleHyps scope (assignedScalarsS s)
      -- A declaration is visible to what FOLLOWS it, and shadows an outer binding of the
      -- same name because it is prepended and lookups take the first match. Bindings made
      -- inside `s` (a block) do not reach `rest`: only `s`'s own declaration does.
      let restDecls := (declsOwn s).extend decls
      scopedWalkSizedS leaf lcs scope decls s
        ++ scopedWalkSizedB leaf lcs restScope restDecls rest
end

/-- Declaration-agnostic walk, for the families that resolve nothing by name. Identical
    traversal, so positional keys are shared. -/
def scopedWalkS {α} (leaf : List Expr → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) (st : Stmt) : List α :=
  scopedWalkSizedS (fun sc _ s => leaf sc s) lcs scope {} st

/-- See `scopedWalkS`. -/
def scopedWalkB {α} (leaf : List Expr → Stmt → List α)
    (lcs : List LoopContract) (scope : List Expr) (body : List Stmt) : List α :=
  scopedWalkSizedB (fun sc _ s => leaf sc s) lcs scope {} body

/-- The array an access refers to: a display name and its fixed length.

    Two routes, and both are needed:

    * the scoped size map, keyed by the root NAME — this is the only route that knows a length
      inferred from an array-literal initialiser (`let a = [0; 16]`, no annotation);
    * `placeTy`, which follows field paths — the only route that can answer `b.data[i]`, which
      previously produced no obligation at all because discovery required a bare `.ident`.

    The name is display-only (`BoundsObl.arrName` is interpolated into messages, never
    resolved), so a field path can carry `b.data` without anything downstream needing to
    parse it. -/
def arrayAccessOf (structs : StructFieldEnv) (decls : ScopeDecls) (a : Expr) :
    Option (String × Nat) :=
  match arrayRootName a with
  | some nm =>
      match decls.sizes.find? (·.1 == nm) with
      | some (_, sz) => some (nm, sz)
      | none => match placeTy structs decls.tys a with
        | some (.array _ n) => some (nm, n)
        | _ => none
  | none =>
      match placeTy structs decls.tys a with
      | some (.array _ n) => some (Concrete.fmtExpr a, n)
      | _ => none

/-- Array-index leaf: the index uses in a statement's OWN expression positions
    (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's index uses).
    A store `a[idx] = v` carries its target bound `(a, idx)` FIRST, matching the
    old walker's ordering exactly. -/
def boundsLeaf (structs : StructFieldEnv) (scope : List Expr) (decls : ScopeDecls) :
    Stmt → List (String × Expr × List Expr × Option Nat) :=
  let mk : (Expr × Expr) → (String × Expr × List Expr × Option Nat) :=
    fun (a, i) => match arrayAccessOf structs decls a with
      | some (nm, sz) => (nm, i, scope, some sz)
      | none => (Concrete.fmtExpr a, i, scope, none)
  fun
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectIndexUsesE v).map mk
  | .return_ _ (some v) => (collectIndexUsesE v).map mk
  | .ifElse _ c _ _ => (collectIndexUsesE c).map mk
  | .while_ _ c _ _ => (collectIndexUsesE c).map mk
  | .forLoop _ _ c _ _ _ => (collectIndexUsesE c).map mk
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectIndexUsesE o ++ collectIndexUsesE v).map mk
  | .arrayIndexAssign _ a idx v =>
      mk (a, idx) :: (collectIndexUsesE a ++ collectIndexUsesE idx ++ collectIndexUsesE v).map mk
  -- as in the other three leaves: the condition of an `assert_`/`assume_` was never examined.
  | .assert_ _ c | .assume_ _ c => (collectIndexUsesE c).map mk
  | _ => []

/-- Index uses paired with the hypotheses in scope at the access (Phase 3 #5 —
    migrated onto the unified `scopedWalk`). Like the call-site migration, the
    collector now threads enclosing `if`-guards (then assumes `c`, else assumes
    `¬c`), early-return fall-through, and loop invariants/guards — strictly more
    sound context than the old bounds walker, so a bounds obligation can only move
    `unproven → proved_by_kernel_decision` (e.g. `if 0 ≤ i && i < n { a[i] }`),
    never the reverse, and a reassigned index still drops its stale guard. -/
def scopedBoundsB (structs : StructFieldEnv) (lcs : List LoopContract) (scope : List Expr)
    (decls : ScopeDecls) (body : List Stmt) :
    List (String × Expr × List Expr × Option Nat) :=
  scopedWalkSizedB (boundsLeaf structs) lcs scope decls body

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
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
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
  hyps          : List Expr := []   -- in-scope #[requires]/guards (for prover-neutral lowering)
  -- Loop VCs these hypotheses owe (R-0461). Non-empty means the obligation was discharged
  -- under an invariant whose own O1/O2 must hold; the ledger caps the status accordingly.
  hypDebt       : List String := []

/-- Array accesses that reach NO bounds obligation, named so coverage cannot be over-read.

    `N VCs, all proved` must not be readable as `every array access is checked`. This is the
    same discipline `--report core-semantics-diff` applies to functions outside the extractable
    fragment, applied to the layer that was quietly exempt from it.

    Two causes reach here, both real gaps rather than errors:

    * **no statically known size** — nothing in scope at the access fixes a length for that
      name (e.g. an array returned from a call, or a `let` with a non-literal initialiser).

    Shadowing is no longer a cause: sizes are resolved per SCOPE, so a redeclared array is
    sized from the binding actually in effect. That replaced a conservative refusal, which had
    in turn replaced a wrong answer.

    A third cause does not reach here and is listed in `DiscoveryComplete.lean`: an array
    reached through a field (`b.data[i]`) is never RECORDED, so there is no name to report.
    Closing that needs type resolution for arbitrary array expressions. -/
def unresolvedBoundsAccesses (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    for (arr, _, _, msize) in scopedBoundsB (structFieldEnv modules) f.loopContracts [] (paramDecls f) f.body do
      if msize.isNone then
        out := out ++ [(fq, arr)]
  return out.eraseDups

/-- Generate array-bounds obligations for every indexed access into a known
    fixed-size array. -/
def boundsObligations (modules : List Module) : List BoundsObl := Id.run do
  let mut out : List BoundsObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (arr, idx, scope, msize) in scopedBoundsB (structFieldEnv modules) f.loopContracts [] (paramDecls f) f.body do
      match msize with
      | none => pure ()   -- named by `unresolvedBoundsAccesses`, which walks identically
      | some n =>
        let key := s!"{fq}#bounds{i}"
        let obHyps := f.requires ++ scope
        let cv : Option Bool × Option String := match cEvalInt idx with
          | some k => (some (decide (0 ≤ k) && decide (k < (Int.ofNat n))), none)
          | none => match toLeanProp idx with
            | none => (none, none)
            | some idxStr =>
              let vars := (collectIdents idx ++ obHyps.flatMap collectIdents).eraseDups
              let reqs := obHyps.filterMap toLeanProp
              let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
              let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
              (none, some s!"{binder}{hyp}(0 ≤ {idxStr} ∧ {idxStr} < {n})")
        out := out ++ [{ fnQual := fq, key, arrName := arr, idxExpr := idx, size := n
                        , closedVerdict := cv.1, leanGoal := cv.2, hyps := obHyps
                        , hypDebt := loopInvariantDebt f fq obHyps }]
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
def collectDivisorsE : Expr → List (Bool × Expr × Expr)
  -- R-0464: the DIVIDEND travels with the divisor now. `divisor ≠ 0` needs only `r`, which
  -- is why `l` was dropped — and dropping it is what made the signed `MIN / -1` quotient
  -- overflow inexpressible at this layer rather than merely unstated.
  | .binOp _ .div l r => (false, l, r) :: (collectDivisorsE l ++ collectDivisorsE r)
  | .binOp _ .mod l r => (true, l, r) :: (collectDivisorsE l ++ collectDivisorsE r)
  | .binOp _ _ l r => collectDivisorsE l ++ collectDivisorsE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .cast _ x _ | .fieldAccess _ x _ => collectDivisorsE x
  | .arrayLit _ es => es.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsE e)
  | .arrayIndex _ a i => collectDivisorsE a ++ collectDivisorsE i
  | .call _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsE e)
  | .methodCall _ o _ _ args => collectDivisorsE o ++ args.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsE e)
  | .staticMethodCall _ _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsE e)
  | .structLit _ _ _ fs base => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectDivisorsE fe) ++ (base.attach.map (fun ⟨e, _⟩ => collectDivisorsE e)).getD []
  | .enumLit _ _ _ _ fs => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectDivisorsE fe)
  | .allocCall _ x a => collectDivisorsE x ++ collectDivisorsE a
  | .ifExpr _ c t el =>
      collectDivisorsE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsS e) ++ el.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsS e)
  | .match_ _ s _ => collectDivisorsE s
  | _ => []
termination_by x => sizeOf x
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
def collectDivisorsS : Stmt → List (Bool × Expr × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectDivisorsE v
  | .return_ _ (some v) => collectDivisorsE v
  | .ifElse _ c t el => collectDivisorsE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsS e) ++ (el.getD []).attach.flatMap (fun ⟨e, _⟩ => collectDivisorsS e)
  | .while_ _ c b _ => collectDivisorsE c ++ b.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsS e)
  | .forLoop _ init c step b _ =>
      (init.attach.map (fun ⟨e, _⟩ => collectDivisorsS e)).getD [] ++ collectDivisorsE c
        ++ (step.attach.map (fun ⟨e, _⟩ => collectDivisorsS e)).getD [] ++ b.attach.flatMap (fun ⟨e, _⟩ => collectDivisorsS e)
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectDivisorsE o ++ collectDivisorsE v
  | .arrayIndexAssign _ a i v => collectDivisorsE a ++ collectDivisorsE i ++ collectDivisorsE v
  -- These five were MISSING from all four statement walkers until 2026-08-06. An obligation
  -- inside `assert(a / b > 0)` was lost entirely -- verified: zero div-by-zero obligations for
  -- exactly that program. `assert_`/`assume_` carry a condition, `borrowIn` a body, and both
  -- destructuring forms a scrutinee, so each is a place obligations can hide.
  | .assert_ _ c | .assume_ _ c => collectDivisorsE c
  | .borrowIn _ _ _ _ _ b => b.attach.flatMap (fun ⟨x, _⟩ => collectDivisorsS x)
  | .letDestructure _ _ _ _ v _ => collectDivisorsE v
  | .letStructDestructure _ _ _ v => collectDivisorsE v
  | _ => []
termination_by x => sizeOf x
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
end

/-- Inclusive value range of a *fixed-width* integer type (none = arbitrary/
    `Int`). The range values come from the arithmetic reference
    (`IntArith.intRange`); this deliberately keeps `Int`/`Uint` as `none` (an
    audit choice — their overflow is profile-dependent, per the note above), so
    it is not a blind alias of `IntArith.intRange`, which does give them ranges. -/
def intRange : Ty → Option (Int × Int)
  | .int | .uint => none
  | ty => IntArith.intRange ty

/-- Best-effort fixed-width int type of an expression, from a var→type map. -/
def exprIntTy (vt : List (String × Ty)) : Expr → Option Ty
  | .ident _ n => vt.lookup n
  | .paren _ e => exprIntTy vt e
  | .unaryOp _ _ e => exprIntTy vt e
  | .cast _ _ t => some t
  | .binOp _ _ l r => match exprIntTy vt l with | some t => some t | none => exprIntTy vt r
  | _ => none

/-- Divisor leaf: the `/`/`%` divisors in a statement's OWN expression positions
    (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's divisors).
    Each item is `(isMod, divisorExpr, scope)`. -/
def divLeaf (scope : List Expr) (decls : ScopeDecls) :
    Stmt → List (Bool × Expr × Expr × List Expr × Option Ty) :=
  let mk : (Bool × Expr × Expr) → (Bool × Expr × Expr × List Expr × Option Ty) :=
    fun (m, n, e) => (m, n, e, scope, exprIntTy decls.tys n)
  fun
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectDivisorsE v).map mk
  | .return_ _ (some v) => (collectDivisorsE v).map mk
  | .ifElse _ c _ _ => (collectDivisorsE c).map mk
  | .while_ _ c _ _ => (collectDivisorsE c).map mk
  | .forLoop _ _ c _ _ _ => (collectDivisorsE c).map mk
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectDivisorsE o ++ collectDivisorsE v).map mk
  | .arrayIndexAssign _ a i v =>
      (collectDivisorsE a ++ collectDivisorsE i ++ collectDivisorsE v).map mk
  -- `assert_`/`assume_` carry a CONDITION, and no leaf examined it until 2026-08-06: a division
  -- in `assert(a / b > 0)` produced no obligation at all. The statement traversal calls these
  -- leaves, so a gap here loses the obligation even when the `…S` walker handles the case.
  | .assert_ _ c | .assume_ _ c => (collectDivisorsE c).map mk
  | _ => []

/-- Divisor uses paired with the hypotheses in scope at the `/`/`%` (Phase 3 #6 —
    migrated onto the unified `scopedWalk`). The collector threads enclosing
    guards / negated guards / fall-through / loop invariants, so a `divisor ≠ 0`
    obligation can only move `unproven → proved` (e.g. `if d != 0 { n / d }`),
    never the reverse. The SOUND division/modulo lowering is unchanged: it still
    flows through `divSound`/`toLeanPropSound`, which lower `/`/`%` to Lean
    E-division ONLY when the dividend is provably non-negative — keeping Concrete's
    truncating semantics from being confused with Lean's floor division. -/
def scopedDivB (lcs : List LoopContract) (scope : List Expr) (decls : ScopeDecls)
    (body : List Stmt) : List (Bool × Expr × Expr × List Expr × Option Ty) :=
  scopedWalkSizedB divLeaf lcs scope decls body

mutual
/-- Every `+`/`-`/`*` binop node in an expression (the whole `a op b`). -/
def collectArithE : Expr → List Expr
  | e@(.binOp _ op l r) =>
    let here := match op with | .add | .sub | .mul => [e] | _ => []
    here ++ collectArithE l ++ collectArithE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .cast _ x _ | .fieldAccess _ x _ => collectArithE x
  | .arrayLit _ es => es.attach.flatMap (fun ⟨e, _⟩ => collectArithE e)
  | .arrayIndex _ a i => collectArithE a ++ collectArithE i
  | .call _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectArithE e)
  | .methodCall _ o _ _ args => collectArithE o ++ args.attach.flatMap (fun ⟨e, _⟩ => collectArithE e)
  | .staticMethodCall _ _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectArithE e)
  | .structLit _ _ _ fs base => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectArithE fe) ++ (base.attach.map (fun ⟨e, _⟩ => collectArithE e)).getD []
  | .enumLit _ _ _ _ fs => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectArithE fe)
  | .allocCall _ x a => collectArithE x ++ collectArithE a
  | .ifExpr _ c t el =>
      collectArithE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectArithS e) ++ el.attach.flatMap (fun ⟨e, _⟩ => collectArithS e)
  | .match_ _ s _ => collectArithE s
  | _ => []
termination_by x => sizeOf x
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
def collectArithS : Stmt → List Expr
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectArithE v
  | .return_ _ (some v) => collectArithE v
  | .ifElse _ c t el => collectArithE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectArithS e) ++ (el.getD []).attach.flatMap (fun ⟨e, _⟩ => collectArithS e)
  | .while_ _ c b _ => collectArithE c ++ b.attach.flatMap (fun ⟨e, _⟩ => collectArithS e)
  | .forLoop _ init c step b _ =>
      (init.attach.map (fun ⟨e, _⟩ => collectArithS e)).getD [] ++ collectArithE c
        ++ (step.attach.map (fun ⟨e, _⟩ => collectArithS e)).getD [] ++ b.attach.flatMap (fun ⟨e, _⟩ => collectArithS e)
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectArithE o ++ collectArithE v
  | .arrayIndexAssign _ a i v => collectArithE a ++ collectArithE i ++ collectArithE v
  -- These five were MISSING from all four statement walkers until 2026-08-06. An obligation
  -- inside `assert(a / b > 0)` was lost entirely -- verified: zero div-by-zero obligations for
  -- exactly that program. `assert_`/`assume_` carry a condition, `borrowIn` a body, and both
  -- destructuring forms a scrutinee, so each is a place obligations can hide.
  | .assert_ _ c | .assume_ _ c => collectArithE c
  | .borrowIn _ _ _ _ _ b => b.attach.flatMap (fun ⟨x, _⟩ => collectArithS x)
  | .letDestructure _ _ _ _ v _ => collectArithE v
  | .letStructDestructure _ _ _ v => collectArithE v
  | _ => []
termination_by x => sizeOf x
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
end

/-- Arithmetic-op leaf: the `+`/`-`/`*` op nodes in a statement's OWN expression
    positions (the walker owns recursion into branches/loops/init/step, so
    `.ifElse`/`.while_`/`.forLoop` contribute only their condition's op nodes). -/
def arithLeaf (scope : List Expr) (decls : ScopeDecls) :
    Stmt → List (Expr × List Expr × Option Ty) :=
  let mk : Expr → (Expr × List Expr × Option Ty) :=
    fun e => (e, scope, exprIntTy decls.tys e)
  fun
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectArithE v).map mk
  | .return_ _ (some v) => (collectArithE v).map mk
  | .ifElse _ c _ _ => (collectArithE c).map mk
  | .while_ _ c _ _ => (collectArithE c).map mk
  | .forLoop _ _ c _ _ _ => (collectArithE c).map mk
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectArithE o ++ collectArithE v).map mk
  | .arrayIndexAssign _ a i v =>
      (collectArithE a ++ collectArithE i ++ collectArithE v).map mk
  -- `assert_`/`assume_` carry a CONDITION, and no leaf examined it until 2026-08-06: a division
  -- in `assert(a / b > 0)` produced no obligation at all. The statement traversal calls these
  -- leaves, so a gap here loses the obligation even when the `…S` walker handles the case.
  | .assert_ _ c | .assume_ _ c => (collectArithE c).map mk
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
def scopedArithB (lcs : List LoopContract) (scope : List Expr) (decls : ScopeDecls)
    (body : List Stmt) : List (Expr × List Expr × Option Ty) :=
  scopedWalkSizedB arithLeaf lcs scope decls body

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
  -- Loop VCs these hypotheses owe (R-0461). Non-empty means the obligation was discharged
  -- under an invariant whose own O1/O2 must hold; the ledger caps the status accordingly.
  hypDebt       : List String := []

/-- One division-by-zero obligation. -/
structure DivObl where
  fnQual        : String
  key           : String
  divExpr       : Expr              -- the DIVISOR (meaning unchanged; 6 consumers rely on it)
  numExpr       : Expr              -- the DIVIDEND (R-0464: needed to state MIN / -1)
  isMod         : Bool
  closedVerdict : Option Bool
  leanGoal      : Option String
  -- R-0464 / H24: the `quotientInRange` half of what `IntArith.trapConditions` says `/` and
  -- `%` owe. `leanGoal` above is the `divisorNonZero` half, and shipping only that half is
  -- why `examples/trap_semantics_gap/` reported `proved_by_kernel_decision` for a division
  -- that aborts. `none` when the operand type is not a known-width integer, in which case
  -- there is no MIN to state and the condition is vacuous.
  quotGoal      : Option String := none
  -- The signed minimum this obligation excludes, for the report. `none` for unsigned and
  -- unknown-width types, which cannot hit the quotient overflow at all.
  quotMin       : Option Int := none
  hyps          : List Expr := []   -- in-scope #[requires]/guards (for prover-neutral lowering)
  -- Loop VCs these hypotheses owe (R-0461). Non-empty means the obligation was discharged
  -- under an invariant whose own O1/O2 must hold; the ledger caps the status accordingly.
  hypDebt       : List String := []

/-- Generate `divisor ≠ 0` obligations for every `/` and `%`. -/
def divObligations (modules : List Module) : List DivObl := Id.run do
  let mut out : List DivObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (isMod, nv, dv, scope, nvTy) in scopedDivB f.loopContracts [] (paramDecls f) f.body do
      let key := s!"{fq}#div{i}"
      let obHyps := f.requires ++ scope
      let cv : Option Bool × Option String := match cEvalInt dv with
        | some k => (some (decide (k ≠ 0)), none)
        | none => match toLeanProp dv with
          | none => (none, none)
          | some dStr =>
            let vars := (collectIdents dv ++ obHyps.flatMap collectIdents).eraseDups
            let reqs := obHyps.filterMap toLeanProp
            let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
            let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
            (none, some s!"{binder}{hyp}({dStr} ≠ 0)")
      -- R-0464: the SECOND condition `IntArith.trapConditions` lists for `/` and `%`.
      -- Signed only, and type-relative: the bound is THIS type's minimum, which is exactly
      -- what a hand-written rule gets wrong at one width or another.
      let quot : Option String × Option Int :=
        match nvTy.bind intRange with
        | some (lo, _) =>
          if !(IntArith.isSignedInt (nvTy.getD .i32)) then (none, none)
          else match toLeanProp nv, toLeanProp dv with
            | some nStr, some dStr =>
              let vars := (collectIdents nv ++ collectIdents dv
                           ++ obHyps.flatMap collectIdents).eraseDups
              let reqs := obHyps.filterMap toLeanProp
              let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
              let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
              (some s!"{binder}{hyp}(¬({nStr} = {lo} ∧ {dStr} = -1))", some lo)
            | _, _ => (none, some lo)
        | none => (none, none)
      out := out ++ [{ fnQual := fq, key, divExpr := dv, numExpr := nv, isMod
                     , closedVerdict := cv.1
                     , leanGoal := cv.2, hyps := obHyps
                     , quotGoal := quot.1, quotMin := quot.2
                     , hypDebt := loopInvariantDebt f fq obHyps }]
      i := i + 1
  return out

/-! ### Shift-amount obligations (R-0464 / H24)

The third condition `IntArith.trapConditions` lists, and the one no family generated. This
was not a weak rule but an ABSENT one: `collectArithE` matches `.add | .sub | .mul`, so `<<`
and `>>` were collected by nothing, `--report vcs` was empty for them, and
`examples/trap_semantics_gap/`'s `1 << 40` aborted with no obligation having been stated.

Worth recording why the gate suite could not see it: `check_vc_bridge_register.sh` asserts
every family GENERATOR has a register row. A missing family has no generator, so there is
nothing for it to notice — the same blind spot mutation coverage has, one level up. What
closes it is the totality lock against `allTrapConditions`, which fails when a condition in
`IntArith` is claimed by no family. -/

mutual
def collectShiftsE : Expr → List (Expr × Expr)
  | .binOp _ .shl l r => (l, r) :: (collectShiftsE l ++ collectShiftsE r)
  | .binOp _ .shr l r => (l, r) :: (collectShiftsE l ++ collectShiftsE r)
  | .binOp _ _ l r => collectShiftsE l ++ collectShiftsE r
  | .unaryOp _ _ x | .paren _ x | .borrow _ x | .borrowMut _ x | .deref _ x
  | .cast _ x _ | .fieldAccess _ x _ => collectShiftsE x
  | .arrayLit _ es => es.attach.flatMap (fun ⟨e, _⟩ => collectShiftsE e)
  | .arrayIndex _ a i => collectShiftsE a ++ collectShiftsE i
  | .call _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectShiftsE e)
  | .methodCall _ o _ _ args =>
      collectShiftsE o ++ args.attach.flatMap (fun ⟨e, _⟩ => collectShiftsE e)
  | .staticMethodCall _ _ _ _ args => args.attach.flatMap (fun ⟨e, _⟩ => collectShiftsE e)
  | .structLit _ _ _ fs base =>
      fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectShiftsE fe)
        ++ (base.attach.map (fun ⟨e, _⟩ => collectShiftsE e)).getD []
  | .enumLit _ _ _ _ fs => fs.attach.flatMap (fun ⟨(_, fe), _⟩ => collectShiftsE fe)
  | .allocCall _ x a => collectShiftsE x ++ collectShiftsE a
  -- `.ifExpr` was MISSING until 2026-08-06: a shift inside an if-EXPRESSION produced no
  -- shift-amount obligation at all. Found by per-walker constructor coverage, not by a test --
  -- all three sibling walkers had this case, which is why a file-granular check could not see it.
  | .ifExpr _ c t el =>
      collectShiftsE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectShiftsS e)
        ++ el.attach.flatMap (fun ⟨e, _⟩ => collectShiftsS e)
  | .match_ _ sc _ => collectShiftsE sc
  | _ => []
termination_by e => sizeOf e
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
def collectShiftsS : Stmt → List (Expr × Expr)
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v => collectShiftsE v
  | .return_ _ (some v) => collectShiftsE v
  | .ifElse _ c t el => collectShiftsE c ++ t.attach.flatMap (fun ⟨e, _⟩ => collectShiftsS e) ++ (el.getD []).attach.flatMap (fun ⟨e, _⟩ => collectShiftsS e)
  | .while_ _ c b _ => collectShiftsE c ++ b.attach.flatMap (fun ⟨e, _⟩ => collectShiftsS e)
  | .forLoop _ init c step b _ =>
      (init.attach.map (fun ⟨e, _⟩ => collectShiftsS e)).getD [] ++ collectShiftsE c
        ++ (step.attach.map (fun ⟨e, _⟩ => collectShiftsS e)).getD [] ++ b.attach.flatMap (fun ⟨e, _⟩ => collectShiftsS e)
  | .fieldAssign _ o _ v | .derefAssign _ o v => collectShiftsE o ++ collectShiftsE v
  | .arrayIndexAssign _ a i v => collectShiftsE a ++ collectShiftsE i ++ collectShiftsE v
  -- These five were MISSING from all four statement walkers until 2026-08-06. An obligation
  -- inside `assert(a / b > 0)` was lost entirely -- verified: zero div-by-zero obligations for
  -- exactly that program. `assert_`/`assume_` carry a condition, `borrowIn` a body, and both
  -- destructuring forms a scrutinee, so each is a place obligations can hide.
  | .assert_ _ c | .assume_ _ c => collectShiftsE c
  | .borrowIn _ _ _ _ _ b => b.attach.flatMap (fun ⟨x, _⟩ => collectShiftsS x)
  | .letDestructure _ _ _ _ v _ => collectShiftsE v
  | .letStructDestructure _ _ _ v => collectShiftsE v
  | _ => []
termination_by e => sizeOf e
decreasing_by
  all_goals simp_wf
  all_goals
    first
      | omega
      | (rename_i h; have := List.sizeOf_lt_of_mem h; simp +arith at this ⊢; omega)
      | (rename_i h; have := sizeOf_mem_getD h; simp +arith at this ⊢; omega)
      | (rename_i h; subst h; simp +arith; omega)
      | (rename_i h; subst h; simp +arith)
end

def shiftLeaf (scope : List Expr) (decls : ScopeDecls) :
    Stmt → List (Expr × Expr × List Expr × Option Ty) :=
  let mk : (Expr × Expr) → (Expr × Expr × List Expr × Option Ty) :=
    fun (l, r) => (l, r, scope, exprIntTy decls.tys l)
  fun
  | .letDecl _ _ _ _ v _ | .assign _ _ v | .expr _ v _ | .defer _ v =>
      (collectShiftsE v).map mk
  | .return_ _ (some v) => (collectShiftsE v).map mk
  | .ifElse _ c _ _ => (collectShiftsE c).map mk
  | .while_ _ c _ _ => (collectShiftsE c).map mk
  | .forLoop _ _ c _ _ _ => (collectShiftsE c).map mk
  | .fieldAssign _ o _ v | .derefAssign _ o v =>
      (collectShiftsE o ++ collectShiftsE v).map mk
  | .arrayIndexAssign _ a i v =>
      (collectShiftsE a ++ collectShiftsE i ++ collectShiftsE v).map mk
  -- `assert_`/`assume_` carry a CONDITION, and no leaf examined it until 2026-08-06: a division
  -- in `assert(a / b > 0)` produced no obligation at all. The statement traversal calls these
  -- leaves, so a gap here loses the obligation even when the `…S` walker handles the case.
  | .assert_ _ c | .assume_ _ c => (collectShiftsE c).map mk
  | _ => []

/-- One shift-amount obligation: `0 ≤ amount < bitWidth(ty)`. -/
structure ShiftObl where
  fnQual        : String
  key           : String
  shiftedExpr   : Expr
  amountExpr    : Expr
  width         : Nat
  closedVerdict : Option Bool
  leanGoal      : Option String
  hyps          : List Expr := []
  hypDebt       : List String := []

/-- Generate `0 ≤ amount < width` obligations for every `<<` and `>>`.

    The width comes from the SHIFTED operand's type, matching
    `IntArith.shiftAmountInRange`, which takes the value's type — not the amount's. Getting
    that backwards would state a condition about the wrong width and pass. -/
def shiftObligations (modules : List Module) : List ShiftObl := Id.run do
  let mut out : List ShiftObl := []
  for (pfx, f) in modules.flatMap allFunctions do
    let fq := pfx ++ f.name
    let mut i := 0
    for (sv, amt, scope, svTy) in
        scopedWalkSizedB shiftLeaf f.loopContracts [] (paramDecls f) f.body do
      match svTy.bind IntArith.intBitWidth with
      | some (w, _) =>
        let key := s!"{fq}#shift{i}"
        let obHyps := f.requires ++ scope
        let cv : Option Bool × Option String := match cEvalInt amt with
          | some k => (some (decide (0 ≤ k ∧ k < Int.ofNat w)), none)
          | none => match toLeanProp amt with
            | none => (none, none)
            | some aStr =>
              let vars := (collectIdents amt ++ obHyps.flatMap collectIdents).eraseDups
              let reqs := obHyps.filterMap toLeanProp
              let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
              let hyp := if reqs.isEmpty then "" else s!"({" ∧ ".intercalate reqs}) → "
              (none, some s!"{binder}{hyp}(0 ≤ {aStr} ∧ {aStr} < {w})")
        out := out ++ [{ fnQual := fq, key, shiftedExpr := sv, amountExpr := amt, width := w
                       , closedVerdict := cv.1, leanGoal := cv.2, hyps := obHyps
                       , hypDebt := loopInvariantDebt f fq obHyps }]
        i := i + 1
      | none => pure ()
  return out

/-- Lean goals for the non-constant shift-amount obligations, for omega discharge. -/
def shiftGoals (modules : List Module) : List (String × String) :=
  (shiftObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- Lean goals for the non-constant divisor obligations, for omega discharge. -/
def divGoals (modules : List Module) : List (String × String) :=
  (divObligations modules).filterMap fun o => o.leanGoal.map (fun g => (o.key, g))

/-- **R-0464: the quotient-overflow goals, keyed separately from `divGoals`.**

    A distinct key (`…#divq{n}` rather than `…#div{n}`) because it is a distinct obligation:
    a division can discharge `divisor ≠ 0` and fail `quotientInRange`, which is exactly the
    case `examples/trap_semantics_gap/` reproduces. Sharing one key would let the stronger
    condition's failure be masked by the weaker one's success — the same collapsing of two
    facts into one status that H23 was. -/
def divQuotGoals (modules : List Module) : List (String × String) :=
  (divObligations modules).filterMap fun o => o.quotGoal.map (fun g => (o.key ++ "q", g))

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
def conjuncts : Expr → List Expr
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
def exprIntervalMax (bounds : List (String × (Int × Int))) : Expr → Option (Int × Int × Nat)
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
def arithToBVW (w : Nat) : Expr → Option String
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
    let mut i := 0
    for (e, scope, eTy) in scopedArithB f.loopContracts [] (paramDecls f) f.body do
      match eTy.bind intRange, toLeanProp e with
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
                       , closedVerdict := cv.1, leanGoal := cv.2, bvGoal, hyps
                       , hypDebt := loopInvariantDebt f fq hyps }]
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
def exprToSmt : Expr → Option String
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
def exprHasNonlinMul : Expr → Bool
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


/-! ## Prover-neutral multi-kernel path — the `proved_by_two_kernels` beachhead

The evidence thesis says independence is the trust value: an obligation a
Lean-hosted compiler discharges with `omega` should ALSO be dischargeable by
OTHER, independently-implemented kernels. We lower the SAME structured obligation
`Expr` through a prover-neutral DRIVER (`ProverLowering`) and hand the script to
that kernel's linear-arithmetic decision procedure:

  * Rocq / `coqc`  — `lia`        (CIC kernel; certification lineage)
  * Isabelle / HOL — `presburger` (HOL kernel; FOUNDATIONAL independence from CIC)

A VC that Lean's `omega` AND ≥1 external kernel independently close on the same
subject graduates `proved_by_kernel_decision` → `proved_by_two_kernels` (or, with
≥2 externals, `proved_by_multi_kernel`). If a kernel is absent, the honest verdict
is "it did not run" — the VC keeps its class, never a fabricated upgrade. Adding a
new prover ("any useful cool language") is a new `ProverLowering` value plus its
binop column in `ReportVC`, not new plumbing. -/

/-- A prover-neutral lowering DRIVER (the Why3 "driver" idea): everything needed to
    turn a structured obligation into a self-contained proof script for one external
    kernel. `binop` is the prover's row of the shared lowering table; `render vars
    hyps concl` wraps the lowered pieces in that prover's goal/quantifier/proof
    syntax. Adding a prover is a new value of this record, not new code paths. -/
structure ProverLowering where
  name  : String
  binop : BinOp → String → String → Option String
  /-- `render quantifiedVars loweredHyps loweredConclusion → full source file`. -/
  render : List String → List String → String → String
  /-- This prover's implication arrow (Rocq `->`, Isabelle `-->`). Needed to build a
      GROUND implication for the lowering-agreement check, which cannot reuse
      `render`'s hypothesis handling because it must also negate the whole thing. -/
  arrow : String := "->"
  /-- This prover's typed quantifier prefix for `vars` (Rocq `forall (a b : Z), `,
      Isabelle `ALL a b::int. `). The agreement check MUST keep this even though its
      instances are ground: Isabelle infers a fresh free type variable per numeral in
      an unquantified proposition (`0 ≤ (100::'b)`), so the lemma stops being about
      integers and `presburger` cannot prove it. Variables are therefore pinned by
      equality hypotheses rather than substituted away. -/
  binder : List String → String := fun _ => ""
  /-- Wrap a proposition in this prover's negation. Both Rocq and Isabelle spell it
      `~`, matching `exprToProver`'s `.not_` case. -/
  negate : String → String := fun p => s!"~ ({p})"
  /-- **R-0455: the tactic is DATA, not a template literal.**

      Ordered attempts, tried in sequence. Measured cost of not having this field:
      `rocqNiaLowering` existed as a full clone of `rocqLowering` — a duplicated
      `ProverLowering` record whose only substantive difference was the word `lia` becoming
      `nia`. Reaching a different tactic cost a whole driver, which is the argument R-0455
      makes for `tactics` being a field.

      Ordered rather than a single value because a real pipeline tries cheap-then-expensive;
      budgeting them is future work and is why this is a list now rather than a `String`. -/
  tactics : List String := []
  /-- This prover's spelling of negation INSIDE an expression (`.not_`). Distinct from
      `negate`, which wraps a whole proposition. Rocq and Isabelle both use `~`; Lean uses
      `¬`, and that single character was what kept Lean's lowering from being a driver —
      and therefore what kept the agreement machinery from being pointable at it. -/
  notSym : String := "~"
  /-- `batchRender closedPropositions → one source file asserting ALL of them`.
      Batching matters: the agreement check emits one lemma per grid assignment, and
      a fresh Isabelle session build per lemma would cost ~30s each. -/
  batchRender : List String → String := fun _ => ""

/-- Lower a contract `Expr` through an arbitrary prover's binop column. Same linear
    fragment as `exprToLeanProp` (int literals/vars, `+`/`-`/`*`, comparisons,
    connectives, unary `-`/`~`); `div`/`mod` dropped as in `toLeanProp`. Structured
    (not a string rewrite), so the emitted script is well-formed by construction.
    Coq and Isabelle share this: both spell unary neg `(- e)`, `not` `~`, and a
    negative literal `(-k)`; only the binary-op column and wrapper differ. -/
def exprToProverU (binop : BinOp → String → String → Option String)
    (notSym : String) : Expr → Option String
  | .intLit _ v => some (if v < 0 then s!"({v})" else s!"{v}")
  | .ident _ n => some n
  | .paren _ e => exprToProverU binop notSym e
  | .unaryOp _ op e => do
    let E ← exprToProverU binop notSym e
    match op with
    | .neg  => some s!"(- {E})"
    | .not_ => some s!"({notSym} {E})"
    | _ => none
  | .binOp _ op l r => do
    let L ← exprToProverU binop notSym l; let R ← exprToProverU binop notSym r
    match op with
    | .div | .mod => none
    | _ => binop op L R
  | _ => none

/-- The linear fragment, lowered for a driver. `~` is the default because Rocq and Isabelle
    both spell negation that way; Lean passes `¬`.

    R-0450 (partial): negation used to be HARD-CODED to `~` here, which is the one thing that
    stopped this function from also being Lean's lowering — so `exprToLeanProp` existed as a
    near-copy, and the fragment was defined twice in two functions that had to agree. It is
    now defined once and `exprToLeanProp` delegates. -/
def exprToProver (binop : BinOp → String → String → Option String) : Expr → Option String :=
  exprToProverU binop "~"

/-- Lean's lowering of the linear fragment, for the replay theorem. Handles unary negation
    (`-30000`) — the signed bounds that make a VC SMT-eligible in the first place.

    **R-0450 (partial): this is now the SAME function as every other driver's**, specialised
    to `leanBinOp` and Lean's negation. It used to be an independent recursion that happened
    to match `exprToProver` case for case, differing only in negative-literal parentheses,
    a space, and `¬` versus `~` — which is exactly the "fragment defined twice, in two
    functions that must agree" that R-0450 exists to remove. Two of those three differences
    were cosmetic; the third is now a parameter.

    Why this matters beyond tidiness: the lowering-agreement check validates a rendering
    against the reference EVALUATOR (`safeOn`/`evalBoolEnv` on the AST), not against Lean.
    The standing justification for Lean being the one kernel whose rendering is unvalidated —
    "its rendering IS the reference, so there is nothing to validate it with" — was therefore
    wrong. The real obstacle was that Lean's lowering was not expressible as a driver, so the
    machinery could not be pointed at it. That obstacle is what this removes. -/
def exprToLeanProp (e : Expr) : Option String :=
  exprToProverU leanBinOp "¬" e

/-! ### Bridge differential-check (feature #1)

An INDEPENDENT concrete evaluator for the contract fragment, used to fuzz proved
obligations: sample variable assignments, and check that no hypothesis-satisfying
assignment refutes the obligation's safety conclusion. A counterexample under a
*proved* obligation would mean the Core→VC bridge (or the discharge) claimed
something concretely false — the one thing "proved" must never do. Independent of
`exprToProver`/omega: it evaluates the arithmetic directly (truncating `/`,`%`,
matching Concrete's `IntArith`), so agreement is a real cross-check, not a tautology. -/

/-! ### The reference evaluator (R-0455: no longer `partial`)

`evalIntEnv`/`evalBoolEnv` are what EVERY lowering-agreement check measures a rendering
against — Rocq's, Isabelle's, the SMT column's and Lean's own. They used to be `partial def`s,
which meant the kernel could not reduce them: the yardstick for every rendering was itself
outside the reach of proof, and no `rfl` example or theorem could say anything about it. That
limitation was hit three times while writing locks elsewhere in this arc.

They are now thin wrappers over `ofExpr` + `TermIR.eval*`, both structural. The behaviour is
the same fragment plus two gains that fall out of the IR:

* **casts evaluate** (as the reference's wrap) where they previously yielded `none`, so
  obligations like `arr[(a / b) as Int]` now have a reference value instead of being skipped;
* `geq`/`gt`/`neq` are canonicalised rather than special-cased, so the relation set is
  minimal without narrowing what can be evaluated.

Spec-function calls still evaluate to `none`: `noSyms` gives uninterpreted symbols no meaning,
which is the honest default — the agreement machinery must not invent a value for a function
it knows nothing about. -/

/-- No oracle for uninterpreted symbols. A spec call has no reference value, so an agreement
    instance mentioning one is SKIPPED rather than answered — the fail-closed direction. -/
def noSyms : TermIR.SymEnv := fun _ _ => none

/-- Integer-valued reference evaluation of a contract expression. -/
def evalIntEnv (env : List (String × Int)) (e : Expr) : Option Int :=
  (ofExpr e).bind (TermIR.evalInt env noSyms)

/-- Boolean-valued reference evaluation of a contract expression. -/
def evalBoolEnv (env : List (String × Int)) (e : Expr)
    (benv : TermIR.BoolEnv := []) : Option Bool :=
  (ofExpr e).bind (TermIR.evalBool env noSyms benv)

/-! #### Behavioural locks on the reference evaluator — **newly possible**

None of these could exist while `evalIntEnv` was `partial`: the kernel could not reduce it, so
its behaviour was only ever assertable by grep. `check_vc_bridge_register.sh` said so in its
own comment. These are the locks that grep stood in for.

The division convention is the one that matters. Lean's `.tdiv`, `/` and `.fdiv` agree on
positives and diverge on negatives, so a plausible "cleanup" would pass every positive test
and silently re-point the reference every rendering is validated against. -/

-- The three spellings, distinguished where they actually differ. Restored here after the
-- evaluator rewrite removed them along with the old `partial` definition: they pin the
-- CONVENTION, while the `evalIntEnv` examples below pin which convention this code uses, and
-- both halves are needed. A swap from `.tdiv` to `/` passes every positive test.
example : (-7 : Int).tdiv 2 = -3 := rfl   -- truncate toward zero — what Concrete means
example : (-7 : Int).fdiv 2 = -4 := rfl   -- floor — a DIFFERENT answer
example : (-7 : Int).tmod 2 = -1 := rfl   -- remainder follows the dividend's sign
example : (-7 : Int).emod 2 =  1 := rfl   -- Euclidean — also different

private def spR : Span := default

-- TRUNCATING division and remainder, pinned on the negative case where the spellings differ.
example : evalIntEnv [("a", -7), ("b", 2)]
    (.binOp spR .div (.ident spR "a") (.ident spR "b")) = some (-3) := rfl
example : evalIntEnv [("a", -7), ("b", 2)]
    (.binOp spR .mod (.ident spR "a") (.ident spR "b")) = some (-1) := rfl
-- Division by zero has NO reference value; it is never guessed.
example : evalIntEnv [("a", -7), ("b", 0)]
    (.binOp spR .div (.ident spR "a") (.ident spR "b")) = none := rfl
-- An unbound variable propagates `none` rather than defaulting to 0.
example : evalIntEnv [] (.binOp spR .add (.ident spR "x") (.intLit spR 1)) = none := rfl
-- A spec-function call has no reference value: `noSyms` refuses to invent one.
example : evalIntEnv [] (.call spR "f" [] [.intLit spR 1]) = none := rfl

-- Relations the old evaluator special-cased are canonicalised, and still evaluate the same.
example : evalBoolEnv [("a", 3), ("b", 5)]
    (.binOp spR .geq (.ident spR "a") (.ident spR "b")) = some false := rfl
example : evalBoolEnv [("a", 5), ("b", 3)]
    (.binOp spR .gt (.ident spR "a") (.ident spR "b")) = some true := rfl
example : evalBoolEnv [("a", 3), ("b", 3)]
    (.binOp spR .neq (.ident spR "a") (.ident spR "b")) = some false := rfl

-- GAINED by going through the IR: a cast now has a reference value, where the old evaluator
-- returned `none` and the agreement instance was skipped. 200 at i8 wraps to -56.
example : evalIntEnv [("x", 200)] (.cast spR (.ident spR "x") .i8) = some (-56) := rfl
example : evalBoolEnv [("x", 200)]
    (.binOp spR .lt (.cast spR (.ident spR "x") .i8) (.intLit spR 0)) = some true := rfl

/-- A family-neutral view of one linear obligation, so every runtime-safety family
    (overflow / array-bounds / div-nonzero, and any future family) flows through the
    SAME prover-neutral lowering instead of each re-implementing it. `mainExpr` is the
    operand/index/divisor to lower; `mkConcl` builds the obligation's conclusion from
    the driver's binop column and the lowered `mainExpr` — so each family states its
    own shape (`lo<=e<=hi`, `0<=i<n`, `d<>0`) in the prover's syntax. `desc` is the
    human display; `hyps` are the in-scope assumptions. Selection is the omega domain:
    non-constant obligations carrying a linear `leanGoal`, matched to omega by `key`. -/
structure MultiKernelObl where
  key      : String
  desc     : String
  hyps     : List Expr
  mainExpr : Expr
  mkConcl  : (BinOp → String → String → Option String) → String → Option String
  -- concrete safety check for the bridge fuzzer (feature #1): does the obligation's
  -- conclusion hold under this environment? `none` if it can't be evaluated.
  safeOn   : List (String × Int) → Option Bool
  -- The obligation's OWN boundary values — the type range for overflow, `0` and the
  -- length for bounds, `0` for a divisor. These are where a comparison-operator error
  -- lives: `<=` rendered as `<` differs from the reference ONLY at a boundary, and
  -- nowhere else. They were previously captured inside `mkConcl`/`safeOn` and invisible
  -- to the grid, so whether the mutation was detected depended on the boundary happening
  -- to appear as a literal in the hypotheses. Measured: `fuzzGrid` contains 3 and 100 but
  -- not 4, so a `[i32; 4]` bounds obligation caught it only incidentally.
  boundaryVals : List Int := []

/-- Collect the linear runtime-safety obligations of every family into the neutral
    view. Adding a family is a new block here, not a new lowering. -/
def multiKernelObligations (modules : List Module) (requireLeanGoal : Bool := true)
    : List MultiKernelObl := Id.run do
  let mut out : List MultiKernelObl := []
  for o in overflowObligations modules do
    if o.closedVerdict.isNone && (!requireLeanGoal || o.leanGoal.isSome) then
      let mk : (BinOp → String → String → Option String) → String → Option String :=
        fun bop e => do let lo ← bop .leq (toString o.lo) e; let hi ← bop .leq e (toString o.hi); bop .and_ lo hi
      let safe : List (String × Int) → Option Bool :=
        fun env => (evalIntEnv env o.opExpr).map (fun v => o.lo ≤ v && v ≤ o.hi)
      out := out ++ [{ boundaryVals := [o.lo, o.hi], key := o.key, desc := s!"{Concrete.fmtExpr o.opExpr} ∈ [{o.lo}, {o.hi}]", hyps := o.hyps, mainExpr := o.opExpr, mkConcl := mk, safeOn := safe }]
  for o in boundsObligations modules do
    if o.closedVerdict.isNone && (!requireLeanGoal || o.leanGoal.isSome) then
      let mk : (BinOp → String → String → Option String) → String → Option String :=
        fun bop e => do let lo ← bop .leq "0" e; let hi ← bop .lt e (toString o.size); bop .and_ lo hi
      let safe : List (String × Int) → Option Bool :=
        fun env => (evalIntEnv env o.idxExpr).map (fun v => 0 ≤ v && v < (Int.ofNat o.size))
      out := out ++ [{ boundaryVals := [0, Int.ofNat o.size], key := o.key, desc := s!"0 ≤ {Concrete.fmtExpr o.idxExpr} < {o.size} (bounds of {o.arrName})", hyps := o.hyps, mainExpr := o.idxExpr, mkConcl := mk, safeOn := safe }]
  for o in divObligations modules do
    if o.closedVerdict.isNone && (!requireLeanGoal || o.leanGoal.isSome) then
      let mk : (BinOp → String → String → Option String) → String → Option String :=
        fun bop e => bop .neq e "0"
      let safe : List (String × Int) → Option Bool :=
        fun env => (evalIntEnv env o.divExpr).map (fun v => v != 0)
      out := out ++ [{ boundaryVals := [0], key := o.key, desc := s!"{Concrete.fmtExpr o.divExpr} ≠ 0 (divisor)", hyps := o.hyps, mainExpr := o.divExpr, mkConcl := mk, safeOn := safe }]
  return out

/-- Result of fuzzing one obligation against the concrete evaluator (feature #1). -/
structure BridgeFuzzResult where
  key            : String
  desc           : String
  hypSat         : Nat                              -- assignments satisfying every hypothesis
  counterexample : Option (List (String × Int))     -- a hyp-satisfying assignment that REFUTES safety

/-- Integer probes, including i32 extremes and 46341 (≈√(2^31), where `a*a`
    overflows i32) so the fuzzer actually finds real violations. -/
def fuzzGrid : List Int := [-2147483648, -100, -3, -1, 0, 1, 3, 100, 46341, 2147483647]

/-- Cartesian product of `vals` over `vars` (all assignments). Callers bound `vars`
    and shrink `vals` to keep this finite. -/
def cartesianEnvs (vars : List String) (vals : List Int) : List (List (String × Int)) :=
  match vars with
  | [] => [[]]
  | v :: rest =>
    let restEnvs := cartesianEnvs rest vals
    vals.flatMap (fun x => restEnvs.map (fun e => (v, x) :: e))

/-- Cartesian product over a PER-VARIABLE grid. `cartesianEnvs` shares one value list across
    every variable, which is wrong whenever the variables have different domains — a `u32`
    parameter and an `i32` parameter do not accept the same literals. -/
def cartesianEnvsPer : List (String × List Int) → List (List (String × Int))
  | [] => [[]]
  | (v, vals) :: rest =>
    let restEnvs := cartesianEnvsPer rest
    vals.flatMap (fun x => restEnvs.map (fun e => (v, x) :: e))

/-- Fuzz every linear obligation against the INDEPENDENT concrete evaluator: over a
    grid of assignments, keep those satisfying all hypotheses, and look for one that
    refutes the safety conclusion. A counterexample under a *proved* obligation is a
    bridge/discharge unsoundness; a counterexample under an *unproved* one validates
    that the fuzzer has teeth. Assignments where a hypothesis can't be evaluated are
    conservatively excluded (never tested against). -/
def bridgeFuzz (modules : List Module) : List BridgeFuzzResult := Id.run do
  let mut out : List BridgeFuzzResult := []
  for o in multiKernelObligations modules do
    let vars := (collectIdents o.mainExpr ++ o.hyps.flatMap collectIdents).eraseDups
    -- keep the grid finite: shrink to extremes when there are many variables.
    let grid := if vars.length ≥ 4 then [(-2147483648 : Int), -1, 0, 1, 46341, 2147483647] else fuzzGrid
    let mut hypSat := 0
    let mut cex : Option (List (String × Int)) := none
    for env in cartesianEnvs vars grid do
      if o.hyps.all (fun h => evalBoolEnv env h == some true) then
        hypSat := hypSat + 1
        if o.safeOn env == some false && cex.isNone then cex := some env
    out := out ++ [{ key := o.key, desc := o.desc, hypSat, counterexample := cex }]
  return out

/-! ### R-0462: fuzz the compiled ARTIFACT against the safety claims

Register A asserts *if the obligation holds, the runtime property holds*. R-0460 discharges
that row by row as theorems. This is the empirical shadow of the same statement, and it tests
something no theorem here reaches: for a function whose runtime-safety obligations all read
`proved`, generate inputs satisfying its `#[requires]`, run the **compiled binary**, and
assert it does not trap.

Distinct from `bridge-check`, and the distinction is the point. `bridge-check` evaluates the
*obligation* on sampled inputs and looks for one that refutes it — obligation against a
model. This runs the *artifact*, so it also crosses surface → Core → SSA → LLVM, which no
register row covers at all.

H23 is the existence proof: a bounds obligation read `proved_by_multi_kernel` and the binary
aborted on its first run. Every kernel-side surface reported success; a fuzzer pointed at the
binary would have caught it in seconds.

This module produces the PLAN — candidate functions and hypothesis-satisfying argument
tuples — and emits a driver program that exercises them. Compiling and running that driver is
the gate's job (`check_artifact_fuzz.sh`), because the compiler cannot re-invoke itself.
Keeping the split here means the plan is deterministic and testable on its own. -/

/-- Can the driver pass a literal of this type? Integers, and fixed-size arrays of them.

    Arrays matter specifically: H23 — the hole this whole task cites as its existence proof —
    lives in `pub fn bad(a: [i32; 4]) -> i32`. An int-only fuzzer excludes the one function it
    most needed to reach, which is how a coverage limit becomes a blind spot. -/
private def fuzzablePassableTy : Ty → Bool
  | .array elem _ => (IntArith.intBitWidth elem).isSome
  | ty            => (IntArith.intBitWidth ty).isSome

/-- Argument spellings for one parameter. An integer contributes its grid; a fixed-size array
    contributes a few UNIFORM fills rather than the cartesian product over its elements —
    `[i32; 4]` over a 12-value grid would be 20736 arrays for one parameter alone. Uniform
    fills at the extremes and zero are what expose an index/bounds fault, which is what the
    array case is here for. -/
private def fuzzArgSpellings (ty : Ty) (grid : List Int) : List String :=
  let lit := fun (v : Int) => if v < 0 then s!"({v})" else s!"{v}"
  match ty with
  | .array elem n =>
    let evs := match IntArith.intRange elem with
      | some (lo, hi) => ([lo, 0, 1, hi].filter (fun v => lo ≤ v && v ≤ hi)).eraseDups
      | none => [0]
    evs.map fun v => "[" ++ ", ".intercalate (List.replicate n (lit v)) ++ "]"
  | _ => grid.map lit

/-- One function the artifact fuzzer can exercise, with the argument rows to try. -/
structure ArtifactFuzzCase where
  /-- How the driver spells the call, e.g. `tg::d`. -/
  callPath : String
  /-- Dotted name, matching obligation keys' `fnQual`. -/
  fnQual   : String
  /-- Argument rows, already rendered as source literals. Strings rather than `Int`s because
      an array argument has no `Int` spelling. -/
  argRows  : List (List String)
  /-- What the obligation layer CLAIMS about this function's runtime safety, which decides
      what a trap means:

      * `"claimed"`  — it has runtime-safety obligations and they all read proved. A trap is a
        counterexample to Register A, to the obligation generator, or to the lowering. This is
        the case the task exists to find.
      * `"unclaimed"` — it has NO runtime-safety obligations at all. A trap means an obligation
        should have existed and did not: the *applicability* half of H24, where the shift family
        was simply absent and the binary aborted with nothing having been stated.
      * `"unproved"`  — at least one obligation is unproved or capped. A trap is EXPECTED and
        is not a finding; the compiler never claimed otherwise.

      Fuzzing without this distinction makes every trap ambiguous. Measured: `sum_all` in
      `examples/error_conventions/` traps on four `i32::MIN`s, and its bounds obligation reads
      `unproven` — reporting that as a Register A counterexample would be a false alarm in the
      first run of the tool. -/
  claim    : String
  deriving Repr

/-- Candidate functions for artifact fuzzing, with contract-satisfying argument rows.

    The filter is narrow, and every clause is a callability requirement rather than a taste:
    * `isPublic` — a private function cannot be called from a generated driver;
    * in the SCOPE of the file the driver is appended to (`scopedValidCore`) — `parsed.modules`
      includes the resolved standard library, and a driver calling into std does not resolve;
    * params are integers or fixed-size integer arrays — the driver passes literals;
    * return type an integer — so the driver can bind and accumulate the result;
    * `capSet` empty and no capability params — a function needing `with(Std)` would force the
      driver to hold capabilities it may not be able to grant;
    * no type params — no instantiation to choose here.

    **A `#[requires]` this layer cannot evaluate excludes the function.** Preconditions are
    checked with `evalBoolEnv` over the integer parameters; if a precondition mentions anything
    else (an array element, a field), it cannot be verified here, and calling anyway would feed
    the function inputs it never promised to handle and then blame the compiler for the trap.
    Fail closed: skip it.

    Every exclusion is counted and printed by `--report artifact-fuzz`, because a fuzzer that
    reports "no traps" while silently testing nothing is worse than no fuzzer. -/
def artifactFuzzCases (modules : List Module) (ownFns : List String)
    (claimOf : String → String) : List ArtifactFuzzCase := Id.run do
  let mut out : List ArtifactFuzzCase := []
  for (pfx, f) in modules.flatMap allFunctions do
    if !f.isPublic then continue
    if f.isTest || f.isEntryPoint then continue
    if !f.typeParams.isEmpty || !f.capParams.isEmpty then continue
    if f.capSet != .empty then continue
    if f.params.isEmpty then continue
    if !(ownFns.contains (pfx ++ f.name)) then continue
    if !f.params.all (fun p => fuzzablePassableTy p.ty) then continue
    if (IntArith.intBitWidth f.retTy).isNone then continue
    let intNames := f.params.filterMap (fun p =>
      if (IntArith.intBitWidth p.ty).isSome then some p.name else none)
    -- Contract must be checkable over the integer params alone, or we do not fuzz it.
    let reqIdents := (f.requires.flatMap collectIdents).eraseDups
    if !reqIdents.all (fun n => intNames.contains n) then continue
    let baseGrid := if f.params.length ≥ 3 then [(-2147483648 : Int), -1, 0, 1, 46341, 2147483647]
                    else fuzzGrid
    -- PER-PARAMETER grids, clamped to each parameter's own type range plus that type's own
    -- boundaries. Not an optimisation: a `u32` parameter handed `-1` produces a driver that
    -- does not COMPILE, so a shared grid yielded a fuzzer that could never run. Seeding each
    -- type's `lo`/`hi` also puts the boundary in the grid by construction rather than by luck.
    let intGrids : List (String × List Int) := f.params.filterMap fun p =>
      match IntArith.intRange p.ty with
      | some (lo, hi) =>
        some (p.name, ((baseGrid ++ [lo, hi]).filter (fun v => lo ≤ v && v ≤ hi)).eraseDups)
      | none => none
    let satEnvs := (cartesianEnvsPer intGrids).filter (fun env =>
      f.requires.all (fun h => evalBoolEnv env h == some true))
    if satEnvs.isEmpty then continue
    -- Build one row per (satisfying integer assignment × array fill), rendering each argument
    -- in parameter order.
    let mut rows : List (List String) := []
    for env in satEnvs do
      let mut acc : List (List String) := [[]]
      for p in f.params do
        let spellings :=
          if (IntArith.intBitWidth p.ty).isSome then
            match env.lookup p.name with
            | some v => [if v < 0 then s!"({v})" else s!"{v}"]
            | none => []
          else fuzzArgSpellings p.ty baseGrid
        acc := acc.flatMap (fun pre => spellings.map (fun sp => pre ++ [sp]))
      rows := rows ++ acc
    if rows.isEmpty then continue
    let fq := pfx ++ f.name
    let callPath := (fq.splitOn ".").intersperse "::" |>.foldl (· ++ ·) ""
    out := out ++ [{ callPath, fnQual := fq, argRows := rows, claim := claimOf fq }]
  return out

/-- Emit a driver for the given cases under the given test-function name.

    Results are accumulated into a value the driver returns, so nothing is dead-code
    eliminated: a call whose result is discarded could in principle be optimized away, and
    then the fuzzer would run a program that never performs the operation it is testing. -/
def artifactFuzzDriverFor (cases : List ArtifactFuzzCase) (fnName : String) : String :=
  -- `unproved` functions are NOT called. A trap there is expected and would drown the signal
  -- the tool exists to produce; the compiler never claimed they were safe.
  let lines := cases.flatMap fun c =>
    c.argRows.zipIdx.map fun (r, i) =>
      s!"    acc = acc + (({c.callPath}({", ".intercalate r})) as Int);   // {c.claim} {c.fnQual}#{i}"
  let body := "\n".intercalate lines
  -- A `#[test]` function inside a module, because that is the shape `--test` accepts and
  -- `--test` COMPILES and runs (via clang) rather than interpreting — which is the whole
  -- point: an interpreted driver would test a model again, not the artifact that ships.
  --
  -- `acc` is returned (as `acc - acc`, so the result is always 0 and the test passes unless
  -- the program TRAPS) specifically so no call can be dead-code eliminated. A driver whose
  -- calls are optimized away runs a program that never performs the operation under test.
  "// GENERATED by `--report artifact-fuzz` (R-0462). Do not edit.\n" ++
  "// Appended to the file under test and run with `--test`, which COMPILES and executes.\n" ++
  "// A TRAP here is a counterexample to Register A, to the obligation generator, or to the\n" ++
  "// lowering — the binary did what no kernel-side surface reported.\n" ++
  -- TOP LEVEL, not inside a wrapper module. A `mod artifact_fuzz { … }` cannot see sibling
  -- top-level modules — `tg::d` fails to resolve from inside it with "call to undeclared
  -- function 'tg_d'" — whereas the file's own `main` calls exactly that path successfully.
  -- The driver therefore lives where the callers it imitates live.
  "#[test]\n" ++
  s!"fn {fnName}() -> i32 \{\n" ++
  "    let mut acc: Int = 0;\n" ++
  body ++ "\n" ++
  "    return (acc - acc) as i32;\n" ++
  "}\n"

/-- The driver that MATTERS: functions the obligation layer claims are safe, plus functions it
    stated nothing about. A trap in the first group is a Register A counterexample; a trap in
    the second means an obligation should have existed and did not. -/
def artifactFuzzDriver (cases : List ArtifactFuzzCase) : String :=
  artifactFuzzDriverFor (cases.filter (fun c => c.claim != "unproved")) "artifact_fuzz_all"

/-- A driver over the `unproved` functions, emitted separately and for ONE purpose: proving the
    harness can detect a trap at all.

    Needed because of an outcome that is good news read carelessly: after R-0461 and R-0464 no
    fixture in this repo claims a proved obligation on a function that traps — which is exactly
    what those fixes accomplished. So the soundness check finds nothing, and a check that finds
    nothing is indistinguishable from a check that cannot find anything. This driver is the
    distinguishing experiment: these functions are KNOWN to trap, the compiler never claimed
    otherwise, and the harness must report it. A trap here is not a defect; its ABSENCE is. -/
def artifactFuzzMechanismDriver (cases : List ArtifactFuzzCase) : String :=
  artifactFuzzDriverFor (cases.filter (fun c => c.claim == "unproved")) "artifact_fuzz_mechanism"

/-! ### Lowering-agreement check — closing the "same proposition?" hole

`proved_by_two_kernels` matches kernels on the obligation KEY, and each driver
re-spells the obligation in its own syntax. Nothing so far checks that those
spellings mean the SAME thing, which the multi-kernel report states outright as a
non-attestation. So two kernels could each close a DIFFERENT (possibly weaker)
proposition and the report would still read `proved_by_two_kernels` — the badge
would be measuring agreement it never established.

This closes that hole without writing a parser per prover syntax: use each prover
as the evaluator of its OWN output. We take the driver's rendering of the obligation
verbatim — the same string the multi-kernel path sends the kernel — and PIN its
variables to one grid assignment with equality hypotheses, giving a proposition
whose truth value is decided (`lia`/`presburger` both decide ground linear
arithmetic either way). We compare that against the reference truth value from
`evalBoolEnv`, the independent concrete evaluator already used by the bridge fuzzer:

  * reference TRUE  → the prover must prove the ground implication
  * reference FALSE → the prover must prove its NEGATION

If every instance checks, the driver's rendering has the same truth table as the
reference on the grid — so precedence bugs, a wrong operator column (`~=` vs `<>`),
and mangled hypotheses all surface. Mapping onto `KernelVerdict` is exact: a
malformed rendering is a syntax `error`, while a well-formed rendering that means
something else is a `refused` — a real DISAGREEMENT.

Honest limits. This is a differential test, not a proof of equivalence: it certifies
agreement on the SAMPLED assignments only. A rendering that differs only at values
the hypotheses make unreachable will (correctly) read as agreeing, and one that
differs only outside the grid will be missed — hence `agreementGrid` seeds the
literals appearing in the obligation and their ±1 neighbours, which is where
comparison-operator errors live. -/

/-- How many ground instances to check per obligation. Bounded because each instance
    is a lemma in the emitted script; hypothesis-satisfying assignments are taken
    first since they are the informative ones (a vacuous instance is trivially true
    for the prover, though it still catches a hypothesis rendered too weakly). -/
def agreementInstanceCap : Nat := 24

/-- Every integer literal occurring in an expression. Used to derive BOUNDARY probe
    values, because an off-by-one operator bug (`<=` rendered as `<`) only shows up
    at a value where the two differ — a fixed grid can miss it entirely. -/
def collectIntLits : Expr → List Int
  | .intLit _ v => [v]
  | .paren _ e => collectIntLits e
  | .unaryOp _ _ e => collectIntLits e
  | .binOp _ _ l r => collectIntLits l ++ collectIntLits r
  | _ => []

/-- Probe values for one obligation: the shared grid plus each literal appearing in
    the obligation or its hypotheses, and each literal ±1. Those neighbours are what
    make a comparison-operator error observable. -/
def agreementGrid (o : MultiKernelObl) (base : List Int) : List Int :=
  -- Seed from the literals appearing in the obligation AND from the obligation's own
  -- boundary values, each with its ±1 neighbours. The boundaries are the load-bearing
  -- addition: a comparison-operator error differs from the reference only there, so a grid
  -- that does not reach them cannot detect one. Relying on the boundary appearing as a
  -- literal made detection incidental.
  let lits := (o.hyps.flatMap collectIntLits ++ collectIntLits o.mainExpr ++ o.boundaryVals)
  let withNeighbours := lits.flatMap (fun v => [v - 1, v, v + 1])
  (base ++ withNeighbours).eraseDups

/-- One `(vcKey, proofScript)` per obligation: a single batched script whose lemmas
    are the ground instances described above. A `refused` verdict on that script
    means this obligation's lowering DISAGREES with the reference evaluator.
    Obligations whose lowering is outside the fragment are skipped (never asked). -/
def loweringAgreementProps (pl : ProverLowering) (modules : List Module)
    : List (String × List String) := Id.run do
  let mut out : List (String × List String) := []
  for o in multiKernelObligations modules do
    let vars := (collectIdents o.mainExpr ++ o.hyps.flatMap collectIdents).eraseDups
    -- Boundary-aware grid. With ≥3 variables the cartesian product explodes, so use
    -- the bare extremes there and reserve the literal-derived neighbours (which is
    -- what catches an off-by-one operator column) for the small-arity cases.
    let base := if vars.length ≥ 4 then [(-2147483648 : Int), -1, 0, 1, 46341, 2147483647]
                else fuzzGrid
    let grid := if vars.length ≥ 3 then base else agreementGrid o base
    -- partition the grid by whether the hypotheses hold, preferring the informative
    -- (hypothesis-satisfying) assignments up to the cap.
    let envs := cartesianEnvs vars grid
    let sat := envs.filter (fun e => o.hyps.all (fun h => evalBoolEnv e h == some true))
    let unsat := envs.filter (fun e => !(o.hyps.all (fun h => evalBoolEnv e h == some true)))
    -- Implication instances want hypothesis-SATISFYING assignments: those are where
    -- the implication is not vacuous.
    let implEnvs := (sat.take agreementInstanceCap)
                    ++ (unsat.take (agreementInstanceCap - min agreementInstanceCap sat.length))
    -- Conclusion-only instances must span the WHOLE grid, hypotheses or not. Taking
    -- only hypothesis-satisfying points here would defeat the purpose: a weakened
    -- conclusion is invisible on the domain the hypotheses carve out (see below), so
    -- half the budget is spent on assignments the hypotheses reject.
    let half := agreementInstanceCap / 2
    let conclEnvs := (sat.take half) ++ (unsat.take half)
    -- The driver's rendering of the obligation itself, rendered ONCE: this is the
    -- very string the multi-kernel path sends the kernel, so we are checking the
    -- production lowering, not a special variant of it.
    let concl := (exprToProverU pl.binop pl.notSym o.mainExpr).bind (o.mkConcl pl.binop)
    let loweredHyps := o.hyps.mapM (exprToProverU pl.binop pl.notSym)
    match concl, loweredHyps with
    | some c, some hs =>
      let body := String.join (hs.map (fun h => s!"({h}) {pl.arrow} ")) ++ c
      -- Pin each variable to an assignment with an equality hypothesis instead of
      -- substituting a literal, so the prover's TYPED binder is retained. Returns
      -- `none` if some variable has no value in this environment.
      let pinnedClaim := fun (env : List (String × Int)) (claim : String) (truth : Bool) =>
        let pins := vars.filterMap (fun v => do
          let value ← env.lookup v
          let lit := if value < 0 then s!"({value})" else s!"{value}"
          pl.binop .eq v lit)
        if pins.length != vars.length then none
        else
          let pinArrows := String.join (pins.map (fun p => s!"({p}) {pl.arrow} "))
          some s!"{pl.binder vars}{pinArrows}{if truth then claim else pl.negate claim}"
      let mut props : List String := []
      -- (a) the WHOLE implication, which exercises the hypothesis rendering.
      for env in implEnvs do
        match o.safeOn env with
        | none => pure ()   -- no reference obtainable; never guess
        | some cv =>
          let hypsHold := o.hyps.all (fun h => evalBoolEnv env h == some true)
          match pinnedClaim env body (!hypsHold || cv) with
          | some p => props := props ++ [p]
          | none => pure ()
      -- (b) the CONCLUSION alone, over a spread of the grid and ignoring whether the
      -- hypotheses hold. This is what catches a WEAKENED conclusion: rendering
      -- `A ∧ B` as `A ∨ B` has the same truth value everywhere the hypotheses hold
      -- (both conjuncts are true there), so (a) alone provably cannot see it. The two
      -- differ only where exactly one side holds — assignments the hypotheses reject,
      -- but still perfectly good tests of the RENDERING itself.
      for env in conclEnvs do
        match o.safeOn env with
        | none => pure ()
        | some cv =>
          match pinnedClaim env c cv with
          | some p => props := props ++ [p]
          | none => pure ()
      if !props.isEmpty then
        out := out ++ [(o.key, props)]
    | _, _ => pure ()   -- outside the fragment: never asked
  return out

/-- One batched source file per obligation, for the drivers that shell out to a prover
    binary. Batching matters: a fresh Isabelle session per lemma costs ~30s. -/
def loweringAgreementScripts (pl : ProverLowering) (modules : List Module)
    : List (String × String) :=
  (loweringAgreementProps pl modules).map (fun (k, ps) => (k, pl.batchRender ps))

/-- `(vcKey, proofScript)` for each linear obligation (ALL families), lowered through
    the given driver. The conclusion is built via the driver's binop column, so `<=` /
    `<` / `<>` / conjunction are spelled the prover's way. Soundness: if the main expr,
    ANY hypothesis, or the conclusion falls outside the fragment we DROP the whole goal
    (never emit a partial one — a dropped goal reads `not-asked`, never a false verdict). -/
def proverReplayGoals (pl : ProverLowering) (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in multiKernelObligations modules do
    let concl : Option String := (exprToProverU pl.binop pl.notSym o.mainExpr).bind (o.mkConcl pl.binop)
    match concl, o.hyps.mapM (exprToProverU pl.binop pl.notSym) with
    | some c, some hyps =>
      let vars := (collectIdents o.mainExpr ++ o.hyps.flatMap collectIdents).eraseDups
      out := out ++ [(o.key, pl.render vars hyps c)]
    | _, _ => pure ()
  return out

/-- **Lean driver — R-0450: the reference kernel's OWN rendering, made checkable.**

    Lean was the one kernel whose lowering was never validated, and the reason on record was
    that its rendering *is* the reference the others are measured against. That was false.
    The agreement check measures a rendering against the reference EVALUATOR
    (`safeOn`/`evalBoolEnv`, which walk the AST and know nothing about Lean); Rocq and
    Isabelle are compared to what the expression MEANS, not to Lean. Lean's rendering was
    simply never put on the same scale, because it was not expressible as a driver.

    It is now. `binop := leanBinOp` and `notSym := "¬"` are exactly what `exprToLeanProp`
    uses — the same function, since `exprToLeanProp` delegates to `exprToProverU` — so this
    validates the PRODUCTION lowering rather than a special variant built for the check.
    That property is the whole point; a driver that rendered differently would prove
    something true about a string nobody sends to a kernel.

    `batchRender` emits a CONJUNCTION rather than a source file: the other drivers shell out
    to `coqc`/`isabelle`, while Lean's instances are discharged in-process by the existing
    omega path (`kernelDischargeLoopVCs`), which wraps a goal string in
    `theorem … := by intros; omega`. One conjunction per obligation means one closed goal
    answers "does this rendering agree with the evaluator on every sampled assignment". -/
def leanLowering : ProverLowering where
  name := "lean"
  binop := leanBinOp
  notSym := "¬"
  arrow := "→"
  negate := fun p => s!"¬ ({p})"
  binder := fun vars => if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
  render := fun vars hyps concl =>
    let binder := if vars.isEmpty then "" else s!"∀ ({" ".intercalate vars} : Int), "
    let arrows := String.join (hyps.map (fun h => s!"({h}) → "))
    s!"theorem vc : {binder}{arrows}{concl} := by intros; omega"
  batchRender := fun props => " ∧ ".intercalate (props.map (fun p => s!"({p})"))

/-- `(obligationKey, goal)` validating that **Lean's own** rendering denotes the obligation.

    Discharged by the ordinary omega path; a key that closes is an obligation whose Lean
    rendering agreed with the reference evaluator on every sampled assignment, and is
    therefore eligible to mint a `LoweringValidated` witness — the same standard every
    external kernel has had to meet since the witness change. -/
def leanAgreementGoals (modules : List Module) : List (String × String) :=
  -- LINEAR obligations only, and the restriction is a soundness point rather than a
  -- convenience. The other drivers get a three-valued answer from their prover — closed,
  -- refused, error — so `refused` genuinely means "this rendering has a different truth
  -- table". The in-process Lean path returns only the CLOSED set, so a goal that fails to
  -- close is indistinguishable from one omega cannot decide. omega decides linear integer
  -- arithmetic; handed a pinned instance of `a * b` it fails on both counts at once.
  --
  -- Measured before this filter existed: 48 of 96 instances "DISAGREED" on
  -- `two_kernel_demo`, all of them the nonlinear `mul_unbounded`, none of them an actual
  -- rendering fault. Reporting those as disagreement would be a false alarm in the one
  -- check whose entire job is to be believed when it cries wolf.
  --
  -- This is not a coverage hole in disguise: the linear fragment is exactly where omega is
  -- the discharging kernel, so it is exactly where Lean's rendering is load-bearing.
  -- Nonlinear obligations are answered by bv_decide or SMT, whose renderings have their own
  -- agreement checks. Widening this needs a Lean discharge that reports refusal distinctly.
  let linearKeys := (multiKernelObligations modules).filterMap (fun o =>
    if exprHasNonlinMul o.mainExpr then none else some o.key)
  (loweringAgreementProps leanLowering modules).flatMap fun (k, ps) =>
    if !linearKeys.contains k then [] else
    ps.zipIdx.map fun (p, i) => (s!"{k}#leanagree{i}", p)

/-- Obligations whose Lean rendering agreed with the reference evaluator on EVERY sampled
    assignment. Mirrors `smtValidatedObligations`: all instances must close, and an
    obligation with no instances is not validated — "we never checked" is not "it agreed". -/
def leanValidatedObligations (goals : List (String × String)) (closed : List String)
    : List String :=
  let keyOf := fun (k : String) => (k.splitOn "#leanagree").headD k
  let obligations := (goals.map (fun (k, _) => keyOf k)).eraseDups
  obligations.filter fun ob =>
    let mine := goals.filter (fun (k, _) => keyOf k == ob)
    !mine.isEmpty && mine.all (fun (k, _) => closed.contains k)

/-- The one Rocq script shape, parameterised by tactic and header comment. Both Rocq drivers
    are now this function plus two fields, instead of two records that had to be kept in step.

    The `Print Assumptions` line is emitted here, once, and that placement is deliberate:
    `coqc` exits 0 on `Admitted.` too, so the exit code cannot distinguish a closed proof from
    a stated one. A per-driver template made this an attestation each clone had to remember —
    both happened to keep it, but "happened to" is the property this removes. -/
def rocqScript (tactic hdr : String) (vars hyps : List String) (concl : String) : String :=
  let binder := if vars.isEmpty then "" else s!"forall ({" ".intercalate vars} : Z), "
  let arrows := String.join (hyps.map (fun h => s!"({h}) -> "))
  "\n".intercalate
    [ hdr,
      "From Stdlib Require Import ZArith.", "From Stdlib Require Import Lia.",
      "Open Scope Z_scope.", s!"Lemma vc : {binder}{arrows}{concl}.",
      s!"Proof. {tactic}. Qed.",
      -- ATTEST: see `rocqScript`'s docstring — the exit code alone cannot tell a Qed-closed
      -- proof from an `Admitted.` one, so the script asserts its own integrity.
      "Print Assumptions vc." ]

/-- Rocq/`coqc` driver: `lia` is Coq's linear-integer-arithmetic decision procedure
    (CIC kernel). -/
def rocqLowering : ProverLowering where
  name := "rocq"
  binop := rocqBinOp
  tactics := ["lia"]
  render := rocqScript "lia"
    "(* second-kernel (Rocq/lia) check of a linear runtime-safety obligation *)"
  arrow := "->"
  binder := fun vars =>
    if vars.isEmpty then "" else s!"forall ({" ".intercalate vars} : Z), "
  -- The AGREEMENT script, and it is a different shape from `render`: N pinned instances of
  -- ONE obligation, each its own lemma with its own `Print Assumptions`. Dropping this field
  -- does not fail the build and does not stop `coqc` closing the proof — it makes the
  -- agreement check have nothing to check, so every Rocq cell reads
  -- "LOWERING DISAGREES" and the badge silently vanishes. Which is exactly what happened
  -- when this refactor first landed: 78/78 became three failures with `rocq:lia = closed`
  -- sitting next to a refused attestation.
  batchRender := fun props =>
    "\n".intercalate
      ([ "(* lowering-agreement check: pinned instances of ONE obligation *)",
         "From Stdlib Require Import ZArith.", "From Stdlib Require Import Lia.",
         "Open Scope Z_scope." ]
       ++ props.zipIdx.map (fun (p, i) =>
            s!"Lemma agree{i} : {p}.\nProof. lia. Qed.\nPrint Assumptions agree{i}."))

/-- Isabelle/HOL driver: `lemma "ALL vars::int. h1 --> ... --> concl" by presburger`.
    `presburger` decides linear integer arithmetic in a HOL kernel — independent of
    CIC, so agreement with Lean/Rocq is FOUNDATIONAL cross-logic independence.

    `tactics` carries `presburger` as data for the same reason the Rocq drivers do; the
    Isabelle scripts are not yet derived from a shared builder because the batch form
    (one theory, N indexed lemmas) differs structurally from the single form, and collapsing
    them without changing emitted output needs the batch/single split handled first. Recorded
    rather than half-done. -/
def isabelleLowering : ProverLowering where
  name := "isabelle"
  binop := isabelleBinOp
  tactics := ["presburger"]
  render := fun vars hyps concl =>
    let binder := if vars.isEmpty then "" else s!"ALL {" ".intercalate vars}::int. "
    let arrows := String.join (hyps.map (fun h => s!"({h}) --> "))
    "\n".intercalate
      [ "theory VC imports Main begin",
        s!"lemma \"{binder}{arrows}{concl}\"", "  by presburger", "end" ]
  arrow := "-->"
  binder := fun vars =>
    if vars.isEmpty then "" else s!"ALL {" ".intercalate vars}::int. "
  batchRender := fun props =>
    -- each lemma needs a distinct name, hence the index
    "\n".intercalate
      ([ "theory VC imports Main begin" ]
       ++ (props.zipIdx.map (fun (p, i) => s!"lemma agree{i}: \"{p}\"\n  by presburger"))
       ++ [ "end" ])

/-- Rocq driver using `nia` (nonlinear integer arithmetic) instead of `lia`. Used to
    CERTIFICATE-CHECK the solver: a nonlinear VC an external SMT solver reports `unsat`
    (`solver_trusted`, solver in the TCB) that Rocq's `nia` ALSO closes graduates to
    `solver_checked` — an independent kernel corroborated the solver, so the solver drops out
    of the trusted base. `nia` ships with `Require Import Lia` (micromega).

    **This is now a two-field override of `rocqLowering`, not a cloned record.** That is the
    whole point of `tactics` being data: before R-0455's slice, reaching `nia` cost a
    duplicated driver that had to be kept in step with its twin. -/
def rocqNiaLowering : ProverLowering :=
  { rocqLowering with
    name := "rocq-nia"
    tactics := ["nia"]
    render := rocqScript "nia"
      "(* certificate-check (Rocq/nia) of a solver-trusted nonlinear obligation *)" }

/-- SMT-eligible overflow obligations (the genuinely NONLINEAR ones the kernel tiers
    left open — same selection as `overflowSmtGoals`), as structured obligations so
    they can be lowered to Rocq `nia` for certificate-checking. -/
def smtEligibleOverflow (modules : List Module) : List OverflowObl :=
  (overflowObligations modules).filter fun o =>
    o.closedVerdict.isNone && o.bvGoal.isNone && exprHasNonlinMul o.opExpr

/-- `(vcKey, coqSource)` lowering each SMT-eligible nonlinear overflow obligation to a
    Rocq `nia` goal — the certificate-check of the solver's `unsat` verdict. -/
def rocqNiaGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in smtEligibleOverflow modules do
    let concl : Option String := do
      let e ← exprToProver rocqBinOp o.opExpr
      let lo ← rocqBinOp .leq (toString o.lo) e
      let hi ← rocqBinOp .leq e (toString o.hi)
      rocqBinOp .and_ lo hi
    match concl, o.hyps.mapM (exprToProver rocqBinOp) with
    | some c, some hyps =>
      let vars := (collectIdents o.opExpr ++ o.hyps.flatMap collectIdents).eraseDups
      out := out ++ [(o.key, rocqNiaLowering.render vars hyps c)]
    | _, _ => pure ()
  return out

/-! ### Certificate REPLAY (as opposed to kernel corroboration)

`solver_checked` means an independent decision procedure (Rocq `nia`) reached the
same verdict as the SMT solver. That removes the solver from the trusted base, but
it is corroboration: nothing checked the solver's own reasoning.

Replay is strictly stronger. Isabelle's `smt` method, with `smt_oracle = false`,
asks a proof-producing solver for its proof and RECONSTRUCTS every inference in the
HOL kernel — the solver's argument itself is checked, not merely its answer. Two
things make that claim auditable rather than aspirational:

  * `smt_oracle = false` is declared explicitly. Under `smt_oracle = true` the
    method stamps the goal instead of checking it, which is precisely the trust leak
    replay exists to close.
  * the emitted theory ASSERTS the resulting theorem carries no oracle
    (`Thm_Deps.all_oracles`), so a stamped proof fails the build rather than passing
    as a replayed one. Verified in both directions: the assertion passes under
    `smt_oracle = false` and fires under `smt_oracle = true`.

Measured limitation, and the reason this does not simply replace `solver_checked`:
reconstruction handles LINEAR integer arithmetic but not nonlinear. Checked against
all three proof-producing solvers reachable here — z3 4.4.0pre (bundled with
Isabelle for exactly this purpose), cvc5, and veriT 2021.06.2 — every one
reconstructs a linear goal and every one fails on `0 ≤ a ⟹ 0 ≤ a * a`, at a 120s
timeout. Since the SMT path exists precisely for the nonlinear VCs the kernel tiers
cannot close, replay currently cannot upgrade them, and `solver_checked` remains
the ceiling for that family. The machinery is wired anyway: it reports the honest
status per goal today, and any goal that becomes replayable graduates on its own. -/

/-- Isabelle driver that REPLAYS a proof-producing solver's proof through the HOL
    kernel, rather than re-deciding the goal with another decision procedure. -/
def isabelleSmtReplayLowering : ProverLowering where
  name := "isabelle-smt-verit"
  binop := isabelleBinOp
  render := fun vars hyps concl =>
    let binder := if vars.isEmpty then "" else s!"ALL {" ".intercalate vars}::int. "
    let arrows := String.join (hyps.map (fun h => s!"({h}) --> "))
    "\n".intercalate
      [ "theory VC imports Main begin",
        "(* certificate REPLAY: the solver's proof is reconstructed in the HOL kernel. *)",
        "declare [[smt_oracle = false, smt_timeout = 60]]",
        s!"lemma replayed: \"{binder}{arrows}{concl}\"",
        "  by (smt (verit))",
        "(* A replayed proof must carry NO oracle. Under smt_oracle = true the method",
        "   stamps the result instead of checking it; that must fail, not pass. *)",
        "ML \\<open>",
        "  val n = length (Thm_Deps.all_oracles [@{thm replayed}]);",
        "  val _ = if n = 0 then writeln \"REPLAY-VERIFIED: no oracle\"",
        "          else error \"ORACLE PRESENT - proof was stamped, not checked\";",
        "\\<close>",
        "end" ]
  arrow := "-->"
  binder := fun vars =>
    if vars.isEmpty then "" else s!"ALL {" ".intercalate vars}::int. "

/-- `(vcKey, isabelleTheorySource)` replaying each SMT-eligible NONLINEAR overflow
    obligation through the HOL kernel. Same selection as `rocqNiaGoals`, so the
    `solver-cert` report can show corroboration and replay side by side on one goal. -/
def isabelleSmtReplayGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  for o in smtEligibleOverflow modules do
    let concl : Option String := do
      let e ← exprToProver isabelleBinOp o.opExpr
      let lo ← isabelleBinOp .leq (toString o.lo) e
      let hi ← isabelleBinOp .leq e (toString o.hi)
      isabelleBinOp .and_ lo hi
    match concl, o.hyps.mapM (exprToProver isabelleBinOp) with
    | some c, some hyps =>
      let vars := (collectIdents o.opExpr ++ o.hyps.flatMap collectIdents).eraseDups
      out := out ++ [(o.key, isabelleSmtReplayLowering.render vars hyps c)]
    | _, _ => pure ()
  return out

/-- `(vcKey, coqSource)` for each linear overflow obligation (Rocq driver). -/
def rocqReplayGoals (modules : List Module) : List (String × String) :=
  proverReplayGoals rocqLowering modules

/-- `(vcKey, isabelleTheorySource)` for each linear overflow obligation (Isabelle). -/
def isabelleReplayGoals (modules : List Module) : List (String × String) :=
  proverReplayGoals isabelleLowering modules

/-! ### SMT rendering validation — the one-printer-many-tools path, finally checked

`exprToSmt` renders an obligation once and feeds z3, cvc5, veriT and any other SMT-LIB
consumer. That makes it the CHEAPEST diversity in the system — one printer, N tools —
and it was the only lowering with no agreement check at all, while its verdict enters the
TCB as `solver_trusted` with no kernel re-deriving it. R-0450 named the gap; this closes
the rendering half of it.

Same method as the prover drivers, adapted to SMT's shape. Variables are PINNED with
equality assertions rather than substituted away, so `exprToSmt` is exercised verbatim —
the point is to test the printer we actually use, not a variant of it. Each instance is
its own query because `(check-sat)` is per-script: batching would ask one question about a
conjunction and lose which instance disagreed.

Truth-table comparison, not provability: for a ground instance the reference evaluator says
TRUE or FALSE, and the solver must AGREE. Reference true → asserting the negation must be
`unsat`. Reference false → asserting the formula itself must be `unsat`. Either way the
expected answer is `unsat`, so `sat` is a genuine disagreement and `unknown` is our problem
rather than the printer's. -/

/-- SMT-LIB operator column, so the family-neutral `mkConcl` can render a conclusion in
    prefix form exactly as `exprToSmt` would. Deliberately partial: an operator with no
    entry yields `none` and the obligation is skipped rather than rendered approximately.
    A wrong column here would make the check pass against a formula the solver never
    sees, which is the failure this whole mechanism exists to detect. -/
def smtBinOpColumn : BinOp → String → String → Option String
  | .leq,  l, r => some s!"(<= {l} {r})"
  | .lt,   l, r => some s!"(< {l} {r})"
  | .geq,  l, r => some s!"(>= {l} {r})"
  | .gt,   l, r => some s!"(> {l} {r})"
  | .eq,   l, r => some s!"(= {l} {r})"
  | .neq,  l, r => some s!"(not (= {l} {r}))"
  | .and_, l, r => some s!"(and {l} {r})"
  | .or_,  l, r => some s!"(or {l} {r})"
  | _,     _, _ => none

/-- `(instanceKey, smtLibQuery)` validating that `exprToSmt`'s rendering of each obligation
    denotes the obligation. Keys are `{obligationKey}#smtagree{i}` so a disagreement names
    the instance; `validatedKeysOf`-style consumers strip the suffix. -/
def smtAgreementGoals (modules : List Module) : List (String × String) := Id.run do
  let mut out : List (String × String) := []
  -- `requireLeanGoal := false` is essential, not incidental. The default view is the OMEGA
  -- domain (linear); the SMT VERDICT path is the complement — it takes only obligations with
  -- a nonlinear multiplication, because omega owns the linear case. Validating the default
  -- view would therefore check `exprToSmt` on exactly the obligations SMT never renders for
  -- a verdict, and the two key sets would be disjoint. Measured before this was fixed.
  for o in multiKernelObligations modules (requireLeanGoal := false) do
    let vars := (collectIdents o.mainExpr ++ o.hyps.flatMap collectIdents).eraseDups
    match exprToSmt o.mainExpr, o.hyps.mapM exprToSmt with
    | some eSmt, some hypSmts =>
      match o.mkConcl smtBinOpColumn eSmt with
      | none => pure ()          -- conclusion outside the SMT column: skip, never guess
      | some conclSmt =>
        let base := if vars.length ≥ 4 then [(-2147483648 : Int), -1, 0, 1, 46341, 2147483647]
                    else fuzzGrid
        let grid := if vars.length ≥ 3 then base else agreementGrid o base
        let mut i := 0
        for env in (cartesianEnvs vars grid).take agreementInstanceCap do
          -- Reference value of the WHOLE obligation under this assignment. Vacuous when
          -- the hypotheses do not hold, which is still a row worth checking: a rendering
          -- that mangles a hypothesis shows up as disagreement exactly there.
          let hypsTrue := o.hyps.all (fun h => evalBoolEnv env h == some true)
          let refVal := if hypsTrue then o.safeOn env else some true
          match refVal with
          | none => pure ()      -- not evaluable: no reference to compare against
          | some rv =>
            let impl := if hypSmts.isEmpty then conclSmt
                        else s!"(=> (and {" ".intercalate hypSmts}) {conclSmt})"
            -- Reference TRUE  => negation must be unsat.
            -- Reference FALSE => the formula itself must be unsat.
            let asserted := if rv then s!"(not {impl})" else impl
            let decls := vars.map (fun v => s!"(declare-const {v} Int)")
            let pins := env.map (fun (v, k) =>
              s!"(assert (= {v} {if k < 0 then s!"(- {-k})" else toString k}))")
            let q := "(set-logic ALL)\n"
              ++ String.intercalate "\n" decls ++ "\n"
              ++ String.intercalate "\n" pins ++ "\n"
              ++ s!"(assert {asserted})\n(check-sat)\n"
            out := out ++ [(s!"{o.key}#smtagree{i}", q)]
            i := i + 1
    | _, _ => pure ()
  return out

/-- `(vcKey, coqSource)` ground-instance agreement scripts for the Rocq driver. -/
def rocqAgreementGoals (modules : List Module) : List (String × String) :=
  loweringAgreementScripts rocqLowering modules

/-- `(vcKey, isabelleTheorySource)` ground-instance agreement scripts (Isabelle). -/
def isabelleAgreementGoals (modules : List Module) : List (String × String) :=
  loweringAgreementScripts isabelleLowering modules

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
    if f.isTrusted || Capabilities.capSetHasUnsafe f.capSet then some (pfx ++ f.name) else none
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

end Report
end Concrete

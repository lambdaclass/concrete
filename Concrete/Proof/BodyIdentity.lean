import Concrete.Proof.Proof
import Concrete.Proof.Digest

/-!
# Canonical body identity

What makes two function bodies THE SAME body: alpha-invariant surface names (`stripAlpha`),
canonical normalization (`normalizePExpr`), and the one digest built from them
(`sourceBodyDigestV1Of`). One producer for that digest, deliberately — a second copy of the formula
is a second answer to "did this body change".

**Placement below `DependencyEdge` is the point, not an accident.** `tableEntryEvidence` (in
`DependencyEdge`) recomputes a body digest from the actual `PFnDef.body` and refuses when it
disagrees with the stored provenance; without that, a body can be replaced while keeping its
callable identity AND its stored digest, and the join still binds, because every other leg compares
metadata against metadata. The producer used to sit in `ProofCore`, which imports `DependencyRoot`,
which imports `DependencyEdge` — so calling it from the consumer was an import cycle.

Imports `Proof` (for `PExpr`/`pexprCanonical`) and `Digest` (for `shortHash`). It must import
neither `ProofCore` nor `DependencyEdge`, and both of those import it.

Named for what it owns rather than how it works: an earlier name, `PExprNormalize`, described the
normalization step and undersold a module that also owns surface-name stripping and the canonical
digest. `shortHash` deliberately does NOT live here — it digests toolchains, workspaces, imports,
tables and theorem artifacts too, so it is general and lives in `Digest`.

`namespace Concrete`, matching `ProofCore`, so nothing was renamed when these moved: `Proof.lean` is
`namespace Concrete.Proof`, and landing them there would have churned every call site.

The three `private` helpers (`pexprFreeIn`, `pexprSortKey`, `isCommutative`) had no users outside
the moved blocks and stay private here.
-/
namespace Concrete

/-- Bug 045: Elab alpha-renames match binders (`value` → `value.b7`).
    Fingerprints and extracted proof expressions must be INVARIANT under
    that renaming — otherwise editing an unrelated earlier match shifts
    the indices of later binders and falsely staleness-marks every proof
    link and spec downstream. Surface identifiers cannot contain '.', so
    stripping at the first '.' recovers the surface name exactly. -/
def stripAlpha (n : String) : String :=
  match n.splitOn "." with
  | base :: _ => base
  | [] => n

-- ============================================================
-- PExpr normalization
-- ============================================================

/-- Check whether a variable name occurs free in a PExpr. -/
private partial def pexprFreeIn (name : String) : Proof.PExpr → Bool
  | .lit _ => false
  | .var n => n == name
  | .binOp _ l r => pexprFreeIn name l || pexprFreeIn name r
  | .letIn n v b => pexprFreeIn name v || (n != name && pexprFreeIn name b)
  | .ifThenElse c t e => pexprFreeIn name c || pexprFreeIn name t || pexprFreeIn name e
  | .call _ args => args.any (pexprFreeIn name)
  -- The BINDING is itself a free occurrence: `f` in `f(x)` refers to the local
  -- `f`, so a normalizer that thinks `f` is unused could eliminate the very let
  -- that supplies it. `.call`'s name denotes a definition and is not free.
  | .applyVar binding args => binding == name || args.any (pexprFreeIn name)
  | .structLit _ fields => fields.any fun (_, fexpr) => pexprFreeIn name fexpr
  | .enumLit _ _ fields => fields.any fun (_, fexpr) => pexprFreeIn name fexpr
  | .fieldAccess obj _ => pexprFreeIn name obj
  | .arrayIndex arr idx => pexprFreeIn name arr || pexprFreeIn name idx
  | .match_ scrutinee arms =>
    pexprFreeIn name scrutinee ||
    arms.any fun (pat, body) =>
      -- An arm's pattern can shadow `name` via its bindings; if so,
      -- the body doesn't free-occur `name` even if textually present.
      let shadows := match pat with
        | .enumPat _ _ bindings => bindings.contains name
        | .varPat binding => binding == name
        | .litPat _ => false
      !shadows && pexprFreeIn name body
  | .cast inner => pexprFreeIn name inner
  | .wrapCast _ _ inner => pexprFreeIn name inner
  | .arrayLit elems => elems.any (pexprFreeIn name)
  | .arraySet arr idx val =>
    pexprFreeIn name arr || pexprFreeIn name idx || pexprFreeIn name val
  | .while_ cond assigns cont =>
    -- An assign rebinds `name` (it does NOT shadow — the variable
    -- pre-existed). The right-hand expression and the cont can
    -- still free-occur `name`. Walk all three.
    pexprFreeIn name cond ||
    assigns.any (fun (_, e) => pexprFreeIn name e) ||
    pexprFreeIn name cont
  | .while_step cond _ step cont =>
    -- carried list documents rebinds but doesn't shadow (per while_).
    -- Walk cond, step, and cont.
    pexprFreeIn name cond ||
    pexprFreeIn name step ||
    pexprFreeIn name cont

/-- Ordering key for commutative canonicalization.
    vars sort before lits; among vars, alphabetical; among lits, by value. -/
private def pexprSortKey : Proof.PExpr → (Nat × String)
  | .var n => (0, n)
  | .lit (.int n) => (1, toString n)
  | .lit (.bool b) => (1, toString b)
  | _ => (2, "")  -- compound exprs stay in place

private def isCommutative : Proof.PBinOp → Bool
  | .add | .mul | .eq | .ne => true
  | _ => false

/-- Normalize a PExpr to canonical form for stable proof attachment.
    Applied once after Core→PExpr extraction, before storage.

    Rewrites (applied bottom-up):
    1. Dead let elimination:  let x = v; body  →  body  (when x ∉ FV(body))
    2. Algebraic identities:  x+0→x, 0+x→x, x*1→x, 1*x→x, x*0→0, 0*x→0, x-0→x
    3. Boolean short-circuit: if true then a else b → a, if false … → b
    4. Let flattening:        let x = (let y=v; e); body → let y=v; let x=e; body
    5. Commutative ordering:  add/mul/eq/ne operands sorted by (kind, name/value) -/
private partial def normalizeCanon : Proof.PExpr → Proof.PExpr
  | .lit v => .lit v
  -- Alpha-canonical names were already chosen by `alphaCanon` (bugs 045/087).
  | .var n => .var n
  | .binOp op lhs rhs =>
    let l := normalizeCanon lhs
    let r := normalizeCanon rhs
    -- Algebraic identities
    match op, l, r with
    | .add, .lit (.int 0), x | .add, x, .lit (.int 0) => x
    | .sub, x, .lit (.int 0) => x
    | .mul, .lit (.int 1), x | .mul, x, .lit (.int 1) => x
    | .mul, .lit (.int 0), _ | .mul, _, .lit (.int 0) => .lit (.int 0)
    | _, _, _ =>
      -- Commutative canonicalization: sort operands
      if isCommutative op then
        let (ln, ls) := pexprSortKey l
        let (rn, rs) := pexprSortKey r
        let swap := ln > rn || (ln == rn && ls > rs)
        if swap then .binOp op r l
        else .binOp op l r
      else .binOp op l r
  | .letIn name val body =>
    let v := normalizeCanon val
    let b := normalizeCanon body
    -- Dead let elimination
    if !pexprFreeIn name b then b
    -- Let flattening: let x = (let y = v'; e); body → let y = v'; let x = e; body
    else match v with
    | .letIn innerName innerVal innerBody =>
      normalizeCanon (.letIn innerName innerVal (.letIn name innerBody b))
    | _ => .letIn name v b
  | .ifThenElse cond thenBr elseBr =>
    let c := normalizeCanon cond
    let t := normalizeCanon thenBr
    let e := normalizeCanon elseBr
    -- Boolean short-circuit
    match c with
    | .lit (.bool true) => t
    | .lit (.bool false) => e
    | _ => .ifThenElse c t e
  | .call fn args =>
    .call fn (args.map normalizeCanon)
  -- The binding's alpha-canonical name was chosen by `alphaCanon`. A `.call`
  -- name is a definition and is never renamed.
  | .applyVar binding args =>
    .applyVar binding (args.map normalizeCanon)
  | .structLit name fields =>
    .structLit name (fields.map fun (fname, fexpr) => (fname, normalizeCanon fexpr))
  | .enumLit enumName variant fields =>
    .enumLit enumName variant (fields.map fun (fname, fexpr) => (fname, normalizeCanon fexpr))
  | .fieldAccess obj field =>
    .fieldAccess (normalizeCanon obj) field
  | .arrayIndex arr idx =>
    .arrayIndex (normalizeCanon arr) (normalizeCanon idx)
  | .match_ scrutinee arms =>
    .match_ (normalizeCanon scrutinee)
      (arms.map fun (pat, body) => (pat, normalizeCanon body))
  | .cast inner => .cast (normalizeCanon inner)
  | .wrapCast w sg inner => .wrapCast w sg (normalizeCanon inner)
  | .arrayLit elems => .arrayLit (elems.map normalizeCanon)
  | .arraySet arr idx val =>
    .arraySet (normalizeCanon arr) (normalizeCanon idx) (normalizeCanon val)
  | .while_ cond assigns cont =>
    .while_ (normalizeCanon cond)
      (assigns.map fun (n, e) => (n, normalizeCanon e))
      (normalizeCanon cont)
  | .while_step cond carried step cont =>
    .while_step (normalizeCanon cond) carried
      (normalizeCanon step)
      (normalizeCanon cont)


/-- Every LOCAL name a PExpr mentions, in binder or reference position: variables,
    `let` binders, pattern bindings, applied bindings, loop-carried names, assigned
    names, and literal field labels (a `LoopStep::Cont` payload labels its fields with
    carried locals; a real field name never contains '.', so including field labels
    can only add names `alphaCanon` leaves alone). Definition names (`.call`) and
    projected fields are not locals and are excluded. -/
private partial def pexprLocalNames : Proof.PExpr → List String
  | .lit _ => []
  | .var n => [n]
  | .binOp _ l r => pexprLocalNames l ++ pexprLocalNames r
  | .letIn n v b => n :: (pexprLocalNames v ++ pexprLocalNames b)
  | .ifThenElse c t e => pexprLocalNames c ++ pexprLocalNames t ++ pexprLocalNames e
  | .call _ args => args.flatMap pexprLocalNames
  | .applyVar binding args => binding :: args.flatMap pexprLocalNames
  | .structLit _ fields => fields.flatMap fun (f, e) => f :: pexprLocalNames e
  | .enumLit _ _ fields => fields.flatMap fun (f, e) => f :: pexprLocalNames e
  | .fieldAccess obj _ => pexprLocalNames obj
  | .arrayIndex arr idx => pexprLocalNames arr ++ pexprLocalNames idx
  | .match_ scrutinee arms =>
    pexprLocalNames scrutinee ++ arms.flatMap fun (pat, body) =>
      (match pat with
        | .enumPat _ _ binds => binds
        | .varPat b => [b]
        | .litPat _ => []) ++ pexprLocalNames body
  | .cast inner => pexprLocalNames inner
  | .wrapCast _ _ inner => pexprLocalNames inner
  | .arrayLit elems => elems.flatMap pexprLocalNames
  | .arraySet arr idx val => pexprLocalNames arr ++ pexprLocalNames idx ++ pexprLocalNames val
  | .while_ cond assigns cont =>
    pexprLocalNames cond ++ assigns.flatMap (fun (n, e) => n :: pexprLocalNames e) ++
      pexprLocalNames cont
  | .while_step cond carried step cont =>
    pexprLocalNames cond ++ carried ++ pexprLocalNames step ++ pexprLocalNames cont

/-- Apply a renaming to every local-name position `pexprLocalNames` enumerates. -/
private partial def pexprRenameLocals (f : String → String) : Proof.PExpr → Proof.PExpr
  | .lit v => .lit v
  | .var n => .var (f n)
  | .binOp op l r => .binOp op (pexprRenameLocals f l) (pexprRenameLocals f r)
  | .letIn n v b => .letIn (f n) (pexprRenameLocals f v) (pexprRenameLocals f b)
  | .ifThenElse c t e =>
    .ifThenElse (pexprRenameLocals f c) (pexprRenameLocals f t) (pexprRenameLocals f e)
  | .call fn args => .call fn (args.map (pexprRenameLocals f))
  | .applyVar binding args => .applyVar (f binding) (args.map (pexprRenameLocals f))
  | .structLit name fields => .structLit name (fields.map fun (k, e) => (f k, pexprRenameLocals f e))
  | .enumLit en v fields => .enumLit en v (fields.map fun (k, e) => (f k, pexprRenameLocals f e))
  | .fieldAccess obj field => .fieldAccess (pexprRenameLocals f obj) field
  | .arrayIndex arr idx => .arrayIndex (pexprRenameLocals f arr) (pexprRenameLocals f idx)
  | .match_ scrutinee arms =>
    .match_ (pexprRenameLocals f scrutinee) (arms.map fun (pat, body) =>
      let pat' := match pat with
        | .enumPat en v binds => .enumPat en v (binds.map f)
        | .varPat b => .varPat (f b)
        | other => other
      (pat', pexprRenameLocals f body))
  | .cast inner => .cast (pexprRenameLocals f inner)
  | .wrapCast w sg inner => .wrapCast w sg (pexprRenameLocals f inner)
  | .arrayLit elems => .arrayLit (elems.map (pexprRenameLocals f))
  | .arraySet arr idx val =>
    .arraySet (pexprRenameLocals f arr) (pexprRenameLocals f idx) (pexprRenameLocals f val)
  | .while_ cond assigns cont =>
    .while_ (pexprRenameLocals f cond) (assigns.map fun (n, e) => (f n, pexprRenameLocals f e))
      (pexprRenameLocals f cont)
  | .while_step cond carried step cont =>
    .while_step (pexprRenameLocals f cond) (carried.map f) (pexprRenameLocals f step)
      (pexprRenameLocals f cont)

/-- Bugs 045/087: the ALPHA-CANONICAL form of an extracted body.

    Elab gives a shadowing binder a fresh Core name (`x` → `x.b3`); committed specs use
    surface names, and the `.bN` index moves when an unrelated earlier binder is added, so
    comparison must not see the suffix. But stripping it is a RENAMING, and a renaming is
    sound only if it is injective on the names in play. The extraction duplicates a
    statement's continuation into each branch (`if c { let x = 100; } return x` becomes
    `if c then (let x.b0 = 100; x) else x`), so a stripped inner binder CAPTURES the outer
    read: blind stripping turned that body back into `let x = 100; x` — the very wrong
    value bug 087 removed from both runtimes — and, worse, the dead-let pass then saw
    `x.b0` unused and deleted a loop body's `let k.b1 = 1000`, rebinding its reads to the
    outer `k`. A model that differs from the program is a false proof.

    So a suffix is stripped only when no OTHER local in the body shares its base name:
    then `x.bN` ↦ `x` is injective and capture-free. Otherwise every same-based name stays
    verbatim, which is still deterministic, and is exactly as alpha-sensitive as the
    program's own shadowing. Surface identifiers cannot contain '.', so names without a
    suffix are never touched. -/
def alphaCanon (e : Proof.PExpr) : Proof.PExpr :=
  let names := (pexprLocalNames e).eraseDups
  let ren := fun (n : String) =>
    let base := stripAlpha n
    if base == n then n
    else if names.any (fun m => m != n && stripAlpha m == base) then n
    else base
  pexprRenameLocals ren e

/-- Normalize a PExpr to canonical form for stable proof attachment: alpha-canonical
    names (`alphaCanon`), then the structural rewrites of `normalizeCanon`. -/
def normalizePExpr (e : Proof.PExpr) : Proof.PExpr := normalizeCanon (alphaCanon e)

/-- THE canonical V1 source-body digest. One producer, and that is the point.

    This formula existed twice — `Report.renderSourceBodyDigest` and inline in
    `implementationManifestOf`. Two copies of a digest are worse than two copies of ordinary
    logic: the whole value of comparing an entry's stored digest against the manifest's is that
    both sides mean the same thing, and a drifted copy makes every comparison quietly answer a
    different question.

    NORMALIZE FIRST, so the digest is invariant under commutative reordering: two bodies differing
    only in the order of `+`'s operands denote the same body, and separating them would report
    drift where there is none.

    LIMIT: this establishes ARTIFACT binding and freshness. It does NOT establish semantic
    equivalence between a source body and a `PFnDef` — that is the source-to-proof-model
    faithfulness question, and matching digests are not an answer to it.

    **Now used by `tableEntryEvidence`**, which is the check that matters most. It used to read
    `PFnDef.sourceBodyDigest` and TRUST it rather than recomputing from `PFnDef.body`, so a
    substituted body retaining old metadata still bound. That needed this producer BELOW
    `DependencyEdge` in the import order, which is why it lives in `BodyIdentity` next to the
    `normalizePExpr` and `shortHash` it is built from, rather than in `ProofCore`. -/
def sourceBodyDigestV1Of (pe : Proof.PExpr) : String :=
  shortHash (Proof.pexprCanonical (normalizePExpr pe))

end Concrete

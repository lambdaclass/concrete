import Lean
import Concrete.Proof.DependencyEdge
import Concrete.Proof.ProofCore

/-! # Deriving a dependency edge from a theorem (R-0004 step 6)

The classifier. It reads the ELABORATED TYPE of a theorem and answers what that
theorem relies on, so the edge is derived from the proof rather than declared by
its author.

## The discriminator

* every `FnTable`-typed subterm is a BOUND VARIABLE → `contract`. The theorem
  holds for any table meeting its hypotheses, so no callee body can affect it.
* the type names concrete table CONSTANTS → `body`. The theorem is about those
  exact entries, and any relevant body change stales it.

Source text cannot answer this. `fns` and `combineFns` are both identifiers on the
page; the difference appears only after elaboration.

## Table-valued expressions, and why a naive test fails OPEN

A first version tested "is this a constant whose TYPE is `FnTable`". That misses a
table built inline — `eval (FnTable.ofGlobals g) …` mentions `FnTable.ofGlobals`,
a constant of FUNCTION type, so the test finds no table constant and answers
`contract` for a theorem that depends on concrete entries. Failing toward
`contract` is the unsafe direction: it says "an implementation change cannot
affect this" about a proof that an implementation change breaks.

So a constant counts as table-valued when its type RETURNS `FnTable` after any
number of arguments, not only when it IS `FnTable`. Measured when this was
written: 11 plain table definitions and 4 table-valued combinators
(`FnTable.mk`, `.ofGlobals`, `.withCallables`, and the flat constructor), and 8
theorems that mention a combinator without naming a table — all of them
metatheory about `FnTable` itself rather than claims about a program. The gap was
latent, not live, and it is closed here before it becomes live.

## What this does NOT decide

`trusted` and `missing` are not properties of the theorem's type: a dependency is
trusted because the callee declares a boundary, and missing because nothing
validated it. Those come from the callee's status, and this classifier
deliberately does not guess them — it reports what the theorem uses, and returns
`none` when the theorem depends on no table at all.
-/

namespace Concrete.Proof

open Lean Meta

/-- Is this constant table-VALUED — either an `FnTable` or a function returning
    one? The second case is what a naive "type is FnTable" test misses. -/
def isTableValued (ci : ConstantInfo) : MetaM Bool := do
  let tableTy : Lean.Expr := mkConst ``Concrete.Proof.FnTable
  if (← isDefEq ci.type tableTy) then return true
  forallTelescope ci.type fun _ b => isDefEq b tableTy

/-- Names of table-valued constants appearing in `e`. -/
def tableConstsIn (e : Lean.Expr) : MetaM (List Name) := do
  let mut out : List Name := []
  for c in e.getUsedConstants do
    match (← try pure (some (← getConstInfo c)) catch _ => pure none) with
    | some ci => if (← isTableValued ci) then out := c :: out
    | none    => pure ()
  return out.eraseDups

/-- Is this constant a SPECIFICATION expression — the thing a `#[spec(...)]` attribute names?

    Specs are `PExpr`-valued, exactly as tables are `FnTable`-valued, so the same shape of test
    answers it. -/
def isSpecValued (ci : ConstantInfo) : MetaM Bool := do
  let specTy : Lean.Expr := mkConst ``Concrete.Proof.PExpr
  if (← isDefEq ci.type specTy) then return true
  forallTelescope ci.type fun _ b => isDefEq b specTy

/-- Names of specification constants appearing in `e`.

    WHY THIS EXISTS. An independent adversarial review bound `main.check_magic` to
    `check_class_correct` — a real theorem the kernel accepts — and the claim reported
    `kernel-replayed, receipt current`. The receipt recorded WHICH theorem was replayed and nothing
    ever asked whether that theorem SAYS anything about this subject: `#[proof_by(...)]` was taken
    as ground truth. Both theorems bind the same table, so binding the table could not tell them
    apart; what distinguishes them is the SPEC each one is about.

    A theorem's statement is the only independent witness of what it concerns, and the specs it
    mentions are extractable from it exactly as its tables are. -/
def specConstsIn (e : Lean.Expr) : MetaM (List Name) := do
  let mut out : List Name := []
  for c in e.getUsedConstants do
    match (← try pure (some (← getConstInfo c)) catch _ => pure none) with
    | some ci => if (← isSpecValued ci) then out := c :: out
    | none    => pure ()
  return out.eraseDups

/-- A digest binding the WHOLE of a table constant, or `none` if it cannot be bound.

    **The under-approximation this exists to prevent.** A `body` edge records which tables a
    theorem names. That is not enough to stale it: the NAME does not move when an entry's
    definition changes, so a proof about `fns` stayed valid-looking after any edit inside
    `fns`. Binding only the entries a static index appears to touch would be worse than
    useless — a dynamic index can reach any entry, so the apparent subset is an
    under-approximation, and an under-approximated dependency fails OPEN.

    So this digests the constant's DEFINING VALUE, not a selection from it. Whole-by-
    construction: there is no subset to guess, and any change anywhere in the table moves it.

    `none` when the constant has no value — an axiom, an opaque, or an unavailable import.
    Refusing is the fail-closed direction and matches `shadowDepsLine`, which refuses rather
    than digesting a partial constant set. A digest over "the parts we could see" is
    indistinguishable from one over the whole thing, which is exactly how a partial
    dependency becomes an invisible one.

    LIMIT, stated because it bounds what a receipt may claim: the digest is over the
    elaborated value's structural rendering, so it is deterministic within a toolchain and
    not across toolchains. That is sufficient here only because toolchain identity is bound
    separately in the receipt envelope (slice 4); if that binding is ever dropped, this
    becomes a cross-machine reproducibility hole rather than a stable key. -/
def tableValueDigest (n : Name) : MetaM (Option String) := do
  match (← try pure (some (← getConstInfo n)) catch _ => pure none) with
  | none => return none
  | some ci =>
    match ci.value? with
    | none => return none
    | some v => return some (Concrete.shortHash ("tableV1:" ++ toString n ++ ":" ++ toString v))

/-- The result of reading a theorem's type: which edge it implies, and the
    concrete tables it names (empty for a `contract` edge).

    The tables are carried, not just the verdict, because a `body` edge has to say
    WHICH entries it binds — a receipt recording only "body" would not let a
    consumer decide whether a given change stales it. -/
structure EdgeEvidence where
  edge   : DependencyEdge
  tables : List Name
  /-- Whole-table digests for `tables`, in the same order, each paired with its name.
      `none` in the second component means the table could not be bound and the evidence
      is INCOMPLETE — see `tablesFullyBound`. Carried alongside the names rather than
      replacing them so a consumer can report which table failed, not just that one did. -/
  tableDigests : List (Name × Option String) := []
  /-- True when the type quantifies over some `FnTable`. Kept alongside `edge` so
      a caller can see the two facts that produced the verdict rather than having
      to trust it. -/
  quantifiesOverTable : Bool
  /-- The SPECIFICATION constants the theorem's statement mentions.

      This is what ties a theorem to the claim it is allowed to justify. Without it a claim could
      name any accepted theorem in `#[proof_by(...)]` and the receipt would record it as evidence —
      the break an independent review found. -/
  specs : List Name := []
deriving Repr, Inhabited

/-- Every named table is bound to a digest. A `body` edge whose evidence is not fully
    bound must not mint a receipt: it names a dependency it cannot detect a change in,
    which reads exactly like a dependency that never changes. -/
def EdgeEvidence.tablesFullyBound (e : EdgeEvidence) : Bool :=
  e.tableDigests.length == e.tables.length && e.tableDigests.all (·.2.isSome)

/-- Derive the edge a theorem implies, or `none` if it depends on no table.

    `none` is deliberately distinct from `missing`: a theorem that never mentions
    a table has no table dependency to classify, whereas `missing` means there IS
    a dependency and nothing validated it. Collapsing them would let "irrelevant"
    read as "unvalidated" and contain claims that are fine. -/
def classifyTheorem (n : Name) : MetaM (Option EdgeEvidence) := do
  let info ← getConstInfo n
  let tableTy : Lean.Expr := mkConst ``Concrete.Proof.FnTable
  forallTelescope info.type fun xs body => do
    let mut bound := false
    for x in xs do
      if (← isDefEq (← inferType x) tableTy) then bound := true
    let mut exprs := [body]
    for x in xs do exprs := (← inferType x) :: exprs
    let mut consts : List Name := []
    let mut specs : List Name := []
    for e in exprs do
      consts := consts ++ (← tableConstsIn e)
      specs := specs ++ (← specConstsIn e)
    let named := consts.eraseDups
    let specsFound := specs.eraseDups
    if named.isEmpty && !bound then return none
    -- Naming a concrete table wins: such a theorem depends on those entries even
    -- if it ALSO quantifies over some other table. Answering `contract` there
    -- would be the fail-open direction.
    if !named.isEmpty then
      -- Bind each named table WHOLE, here rather than at the consumer: the classifier is
      -- the only place that knows which constants are tables, and a consumer re-deriving
      -- that would be a second, weaker answer to a question already answered.
      let mut digs : List (Name × Option String) := []
      for t in named do digs := digs ++ [(t, ← tableValueDigest t)]
      return some { edge := .body, tables := named, tableDigests := digs
                  , quantifiesOverTable := bound, specs := specsFound }
    return some { edge := .contract, tables := [], quantifiesOverTable := true, specs := specsFound }

/-! ## The classification hand-back (R-0004 slice 6, step 1)

The compiler cannot perform this classification: it reads a theorem's ELABORATED TYPE, which
exists only while Lean is elaborating. So the Lean side answers, and the answers cross to the
compiler as data. These are the producers of that data.

**A theorem that cannot be classified yields `unclassified`, never a dropped row.** Dropping is
how a dependency disappears from a root and the root then reports a confident value over
material it never saw — the same failure the migration manifest's `owner=NONE` rows exist to
prevent one layer up. Every requested name appears in the output, exactly once. -/

/-- Classify one theorem, total. `none` from `classifyTheorem` means the theorem mentions no
    table at all, which is a real answer (`contract`-like: nothing about a table can stale it)
    and distinct from "we could not look". An unknown name is `unclassified`. -/
def classifyOrUnclassified (n : Name) : MetaM EdgeEvidence := do
  match (← try classifyTheorem n catch _ => pure none) with
  | some ev => return ev
  | none    =>
    -- Distinguishing "no table dependency" from "could not classify" needs to survive here, and
    -- it does: `classifyTheorem` throwing is caught above and both land as `unclassified`, which
    -- is the fail-closed reading. A theorem that genuinely mentions no table is ALSO reported
    -- unclassified rather than `contract`, because this function cannot tell the two apart from
    -- a `none` — and inventing `contract` would assert independence from an implementation the
    -- theorem might well depend on.
    return { edge := .unclassified, tables := [], tableDigests := [], quantifiesOverTable := false }

/-- Classify every requested theorem. One row per request, in the REQUESTED order — the caller's
    order, not the environment's, so two runs over the same list agree regardless of how the
    constants happen to be stored. -/
def classifyAll (ns : List Name) : MetaM (List (Name × EdgeEvidence)) :=
  ns.mapM fun n => do return (n, ← classifyOrUnclassified n)

/-- Canonical, length-prefixed serialization of a classification table.

    SORTED BY THEOREM NAME, for the reason `mint?` sorts table bindings: the caller's order is
    convenient for reading and must not enter the bytes, or the same classification discovered in
    a different order would serialize differently and compare unequal.

    Tables are rendered with their digests so a consumer can tell a `body` edge bound to specific
    entries from one that named them and bound nothing. An UNBOUND table renders as `?`, which is
    deliberately not a digest-shaped value — a consumer must not be able to mistake it for one. -/
def renderClassification (rows : List (Name × EdgeEvidence)) : String :=
  let lp := fun (t p : String) => t ++ toString p.length ++ ":" ++ p
  let sorted := rows.mergeSort (fun a b => toString a.1 ≤ toString b.1)
  let one := fun (n, ev) =>
    let tbls := ev.tableDigests.mergeSort (fun a b => toString a.1 ≤ toString b.1)
    lp "T" (toString n) ++ lp "e" ev.edge.canonical
      ++ "n" ++ toString tbls.length ++ ":"
      ++ String.join (tbls.map fun (tn, d?) => lp "t" (toString tn) ++ lp "d" (d?.getD "?"))
  "classifyV1:n" ++ toString sorted.length ++ ":" ++ String.join (sorted.map one)

/-- A digest of the theorem ITSELF — its type and its proof term.

    Without it a classification is a label floating free of what it classifies: `("T", "body")`
    stays structurally valid after `T` is reproved, restated, or replaced by a different theorem
    of the same name. The row would still parse, still merge, and still type an edge — while
    describing a theorem that no longer exists.

    The TYPE is what `classifyTheorem` reads, so a type change can change the classification and
    must move this. The VALUE is included because a re-proof with the same statement is a
    different artifact, and a receipt that binds "which proof" cannot rest on a digest that
    ignores the proof.

    Same toolchain-relativity limit as `tableValueDigest`, and acceptable for the same reason:
    the toolchain is separately bound in the receipt. -/
def theoremArtifactDigest (n : Name) : MetaM (Option String) := do
  match (← try pure (some (← getConstInfo n)) catch _ => pure none) with
  | none => return none
  | some ci =>
    -- The VALUE contributes as a structural HASH, not as rendered text. `toString` on a proof
    -- term is O(term size), and proof terms are enormous: rendering 43 of them exceeded a
    -- 28-minute budget and produced nothing. Measured, not predicted — the first version did
    -- exactly that.
    --
    -- `Expr.hash` is a cheap structural hash and keeps the property that matters here: a
    -- re-proof with the same statement moves the digest, gated by a same-type/different-proof
    -- probe.
    --
    -- WHAT THIS IS NOT, stated because the previous wording ("astronomically unlikely
    -- collision") claimed more than a 64-bit structural hash supports. `Expr.hash` is not a
    -- cryptographic identity, and passing it through `shortHash` cannot restore collision
    -- resistance that the input never had — hashing a hash adds no entropy. This is a
    -- toolchain-bound FRESHNESS TRIPWIRE, adequate against drift and accident, and NOT an
    -- adversarially robust receipt identity. Slice 8 should attack it.
    --
    -- A second known weakness: `value? = none` yields the shared marker `<opaque>`, so two
    -- opaque declarations with the same name and type are indistinguishable here. Recorded
    -- rather than papered over — an opaque artifact is exactly where a digest is least able to
    -- tell one proof from another.
    --
    -- The TYPE is still rendered in full: types are small, and the type is what
    -- `classifyTheorem` reads, so it is the part where an exact representation earns its cost.
    let valHash := match ci.value? with
      | some v => toString v.hash
      | none   => "<opaque>"
    return some (Concrete.shortHash ("thmV1:" ++ toString n ++ "|ty:" ++ toString ci.type
                                     ++ "|valh:" ++ valHash))

/-! ## The merge (R-0004 slice 6, step 3)

The compiler emits `unclassified` edges keyed by theorem name; Lean answers; this joins them.
It is the boundary where a partial answer becomes indistinguishable from a complete one, so it
refuses five distinct ways rather than returning a best effort.

Roots are computed only after this succeeds. A merge that silently dropped, defaulted, or
double-counted a row would hand the root builder material it could not tell from complete
material — and the root builder's own fail-closed checks cannot see a row that never arrived. -/

inductive MergeError where
  /-- A requested key came back with no classification. -/
  | unanswered (key : String)
  /-- A key was answered twice. Not "take the first": two answers for one question means the
      classifier disagrees with itself, and picking one makes the result depend on list order. -/
  | duplicateAnswer (key : String)
  /-- An answer arrived for a key nobody asked about. Its presence means the answer set was
      built from a different question set, so the whole batch is suspect — not just this row. -/
  | unknownAnswer (key : String)
  /-- The classifier returned `unclassified`, which is not an answer. Accepting it would put
      exactly the state this merge exists to eliminate into a root. -/
  | stillUnclassified (key : String)
  /-- The classifier returned `missing`: it ran and found nothing validating this dependency.
      A real answer, and not one a current root may be built from. -/
  | classifiedMissing (key : String)
deriving Repr, BEq

def MergeError.explain : MergeError → String
  | .unanswered k        => s!"no classification returned for '{k}'"
  | .duplicateAnswer k   => s!"'{k}' was classified twice — the classifier disagrees with itself"
  | .unknownAnswer k     => s!"classification returned for '{k}', which was never requested"
  | .stillUnclassified k => s!"'{k}' came back unclassified — that is not an answer"
  | .classifiedMissing k => s!"'{k}' classified as `missing`: nothing validates this dependency"

/-- Join requested keys to returned classifications, or refuse.

    EVERY refusal is a distinct constructor because the repairs differ: an unanswered key means
    the hand-back did not run over it; a duplicate means the classifier is inconsistent; an
    unknown answer means the two sides disagree about the question set; `unclassified` means the
    classification did not conclude; `missing` means it concluded there is nothing. Collapsing
    them to `none` would leave every one of those looking like "try again".

    Returns rows in REQUESTED order, so a caller's iteration is stable without depending on how
    the answers happened to be listed. -/
def mergeClassifications
    (requested : List String) (answers : List (String × EdgeEvidence))
    : Except MergeError (List (String × EdgeEvidence)) := do
  -- Unknown answers first: a key nobody asked about means the two sides were built from
  -- different question sets, which makes every OTHER row suspect too. Reporting a downstream
  -- symptom (say, an unanswered key) would send the reader to the wrong place.
  for (k, _) in answers do
    if !requested.contains k then throw (.unknownAnswer k)
  let mut out : List (String × EdgeEvidence) := []
  for k in requested do
    let hits := answers.filter (fun a => a.1 == k)
    match hits with
    | []        => throw (.unanswered k)
    | [(_, ev)] =>
      if ev.edge == DependencyEdge.unclassified then throw (.stillUnclassified k)
      else if ev.edge == DependencyEdge.missing then throw (.classifiedMissing k)
      else out := out ++ [(k, ev)]
    | _         => throw (.duplicateAnswer k)
  return out

/-- Was every requested key answered with a classification a root may use?

    The predicate a caller should consult before building a root. Deliberately derived from
    `mergeClassifications` rather than reimplemented: two definitions of "complete" is how one
    of them ends up weaker. -/
def classificationsComplete (requested : List String)
    (answers : List (String × EdgeEvidence)) : Bool :=
  (mergeClassifications requested answers) matches Except.ok _

end Concrete.Proof

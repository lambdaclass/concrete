import Concrete.Frontend.AST

/-!
# Trust edges — the canonical, DIRECT facts about where rawness enters

Four edge kinds, per function, and nothing else:

  callsTrusted X   this function calls a `trusted` declaration
  callsFFI Y       this function calls an `extern` declaration
  containsRawOp Z  this function performs a raw operation (Z names which)
  assumesUnsafe W  this function declares `with(Unsafe)` — the caller owes an
                   unchecked precondition

WHY DIRECT EDGES AND NOT A SUMMARY. A `usesUnsafe` boolean, or a stored transitive
closure, is a second producer of a fact the call graph already determines, and it drifts:
today 292 public std functions reach a raw root while only 154 declare `Unsafe`, so the
declarations are ALREADY an incomplete provenance system, off by 138. Reports derive paths
and closure from these edges; nothing hand-maintains the answer.

WHAT AN EDGE IS NOT. It is a RECORD of where rawness is, never a LICENSE for it. Derived
provenance does not authorize a raw operation. Every raw operation must be justified by
being inside a `trusted` implementation, or by a declared caller obligation. std does not
satisfy that invariant today — 47 raw roots are neither `pub` nor `trusted` — and these
edges are what make that measurable rather than assumed.

THE THREE FACTS THESE KEEP APART (the same separation as R-0484's requires/carries/performs):
  - operational authority  `with(File, Alloc, ...)` — unchanged, orthogonal, and NEVER
    erased by `trusted`;
  - caller safety obligation  `assumesUnsafe` — the caller must uphold an invariant the
    language cannot establish;
  - trust provenance  the other three edges — the implementation depends on raw
    operations, FFI, or an audited assertion.

An API may be safe to CALL while raw INSIDE (`Vec::push`), which is why provenance must
not propagate as a caller requirement.
-/

namespace Concrete

/-- One direct trust fact about one function. `target` names the callee, the raw-operation
    kind, or the capability, depending on `kind`. -/
inductive TrustEdgeKind where
  | callsTrusted
  | callsFFI
  | containsRawOp
  | assumesUnsafe
  deriving Repr, BEq, DecidableEq

def TrustEdgeKind.tag : TrustEdgeKind → String
  | .callsTrusted   => "calls-trusted"
  | .callsFFI       => "calls-ffi"
  | .containsRawOp  => "contains-raw-op"
  | .assumesUnsafe  => "assumes-unsafe"

structure TrustEdge where
  /-- The function the edge belongs to, as the checker names it. -/
  fn : String
  /-- The module the function was checked in, so a spelling cannot become identity. -/
  modName : String
  kind : TrustEdgeKind
  /-- Callee name, raw-operation kind (`*raw_ptr`, `ptr_arith`, `unsafe_cast`,
      `*raw_ptr=`), or capability name. -/
  target : String
  deriving Repr, BEq

/-- Per-function attributes a consumer needs that are NOT edges.

    THE JUSTIFICATION INVARIANT needs this. "Every raw operation must be justified — by
    sitting inside a trusted implementation, or by a declared caller obligation" is not
    decidable from the edges alone: `callsTrusted` says something about CALLEES, and the
    invariant asks whether the CONTAINING function is trusted. Kept separate from the
    edge list rather than smuggled in as a fifth edge kind, because it is a property of a
    declaration, not a relation between two. -/
structure TrustFnAttr where
  fn : String
  modName : String
  isTrusted : Bool
  isPublic : Bool
  /-- Does this function's capability set AUTHORIZE a raw operation? This is the
      `capsContain` question — the same entailment the checker's raw-operation gate
      asks — deliberately distinct from the `assumesUnsafe` edge, which records
      LITERAL membership. The two agree today: since bug 188 a capability VARIABLE
      entails only itself, never `Unsafe`, so `for_each<cap C>(..) with(C)` authorizes
      a raw operation only by being `trusted`. Reading the checker's own question
      keeps the report from inventing (or hiding) an unjustified operation if the
      entailment rule changes again. -/
  capsAuthorizeUnsafe : Bool := false
  deriving Repr, BEq

/-- Stable ordering key, so a report over these is byte-reproducible. -/
def TrustEdge.key (e : TrustEdge) : String :=
  s!"{e.modName}\t{e.fn}\t{e.kind.tag}\t{e.target}"

/-- Deduplicate and order. A function derefs a pointer in a loop; the FACT is one edge. -/
def TrustEdge.canonical (es : List TrustEdge) : List TrustEdge :=
  let keyed := es.map (fun e => (e.key, e))
  let sorted := keyed.mergeSort (fun a b => a.1 < b.1)
  let rec dedup : List (String × TrustEdge) → List TrustEdge
    | [] => []
    | [x] => [x.2]
    | x :: y :: rest => if x.1 == y.1 then dedup (y :: rest) else x.2 :: dedup (y :: rest)
  dedup sorted

end Concrete

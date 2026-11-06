import Concrete.Check.CoreCheck
import Concrete.Semantics.TrustEdges

/-!
# `--report trust-edges` — the canonical direct trust facts, one line each

Emits only DIRECT edges. Paths and transitive closure are derived by whatever consumes
this, never stored: a stored closure is a second producer of something the call graph
already determines, and it drifts. Today 292 public std functions reach a raw root while
154 declare `Unsafe` — the declarations are already an incomplete provenance system, off
by 138, which is the drift this format exists to stop.

Tab-separated and sorted, so the inventory is byte-reproducible and diffable.

THE `unjustified-raw-op` LINES ARE THE POINT. A raw operation must be justified — by
sitting inside a `trusted` implementation, or by a declared caller obligation. Derived
provenance records where rawness is; it never authorizes it. Any function listed there
performs a raw operation with neither justification.
-/

namespace Concrete
namespace Report

def trustEdgeReport (modules : List CModule) : String :=
  let (edges, attrs) := coreTrustEdges modules
  let isTrusted := fun (m : String) (f : String) =>
    (attrs.find? (fun a => a.modName == m && a.fn == f)).map (·.isTrusted) |>.getD false
  -- AUTHORITY, not literal membership: a `cap C` function satisfies the raw-operation
  -- gate through its variable. Asking the literal question here invented four
  -- unjustified raw operations in `ordered_set` that the checker had authorized.
  let capsOk := fun (m : String) (f : String) =>
    (attrs.find? (fun a => a.modName == m && a.fn == f)).map (·.capsAuthorizeUnsafe) |>.getD false
  let rawOps := edges.filter (fun e => e.kind == .containsRawOp)
  let obliged := edges.filter (fun e => e.kind == .assumesUnsafe)
  let hasObligation := fun (m : String) (f : String) =>
    obliged.any (fun e => e.modName == m && e.fn == f)
  -- Neither inside a trusted implementation nor backed by a declared caller obligation.
  let unjustified := rawOps.filter (fun e =>
    !(isTrusted e.modName e.fn) && !(hasObligation e.modName e.fn) && !(capsOk e.modName e.fn))
  let header := "# module\tfunction\tedge\ttarget\n"
  let body := edges.foldl (fun acc e =>
    acc ++ s!"{e.modName}\t{e.fn}\t{e.kind.tag}\t{e.target}\n") ""
  let unjLines := unjustified.foldl (fun acc e =>
    acc ++ s!"# unjustified-raw-op\t{e.modName}\t{e.fn}\t{e.target}\n") ""
  -- Counts are a READING AID recomputed from the rows above, never a stored fact, so
  -- they cannot disagree with them.
  let nRaw := rawOps.length
  let nFFI := (edges.filter (fun e => e.kind == .callsFFI)).length
  let nTr := (edges.filter (fun e => e.kind == .callsTrusted)).length
  header ++ body ++ unjLines
    ++ s!"# totals: contains-raw-op={nRaw} calls-ffi={nFFI} calls-trusted={nTr}"
    ++ s!" assumes-unsafe={obliged.length} unjustified-raw-op={unjustified.length}\n"

end Report
end Concrete

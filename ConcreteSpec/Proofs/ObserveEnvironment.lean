import ConcreteSpec.Spec.Observe

/-! # Independent environment production

`Responds` specifies admission. Correctness evidence here instead starts with an executable
producer that does not consult that relation or recheck its expected contract.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- Law (subject `ConcreteSpec.Core.Observe.Demo.doublingReply`).
The addition-based producer meets the multiplication-based operation contract for every
request and history, and routes its response to the request's operation kind. No admission
of the produced answer is assumed. -/
theorem doubling_reply_correct (history : List (Event Demo.signature))
    (q : Request Demo.signature) :
    Demo.contract.allows () history q (Demo.doublingReply q) ∧
      RequestResponse q (Demo.doublingReply q) := by
  obtain ⟨kind, n⟩ := q
  cases kind
  change Nat at n
  constructor
  · change n * 2 = n + n
    omega
  · exact RequestResponse.matched (S := Demo.signature) Demo.Op.double n (n + n)

/-- The producer fault increments an otherwise correctly routed response. -/
def offByOneReply (q : Request Demo.signature) : Response Demo.signature :=
  ⟨q.1, q.2 + q.2 + 1⟩

/-- Instance (law `ConcreteSpec.Core.Observe.doubling_reply_correct`, case `double_three`). -/
example : Demo.contract.allows () [Demo.request 3] ⟨.double, 3⟩
    (Demo.doublingReply ⟨.double, 3⟩) ∧
    RequestResponse (S := Demo.signature) ⟨.double, 3⟩ (Demo.doublingReply ⟨.double, 3⟩) :=
  doubling_reply_correct [Demo.request 3] ⟨Demo.Op.double, 3⟩

/-- Planted (law `ConcreteSpec.Core.Observe.doubling_reply_correct`, case `double_three`,
subject `ConcreteSpec.Core.Observe.Demo.doublingReply`, mutant `ConcreteSpec.Core.Observe.offByOneReply`). -/
example : ¬ (Demo.contract.allows () [Demo.request 3] ⟨.double, 3⟩
    (offByOneReply ⟨.double, 3⟩) ∧
    RequestResponse (S := Demo.signature) ⟨.double, 3⟩ (offByOneReply ⟨.double, 3⟩)) := by
  intro invalid
  have arithmetic := invalid.1
  change 3 * 2 = 3 + 3 + 1 at arithmetic
  contradiction

namespace RoutingDemo

inductive Kind where
  | twice
  | fourTimes
  deriving DecidableEq

abbrev signature : EventSignature where
  Kind := Kind
  Request := fun _ => Nat
  Response := fun _ => Nat
  Output := String

/-- Separate operation policy: response routing and multiplication determine correctness. -/
def contract : OperationContract signature where
  Input := Unit
  allows := fun _ _ q r => q.1 = r.1 ∧
    q.2 * (match q.1 with | .twice => 2 | .fourTimes => 4) = r.2
  mayBlock := fun _ _ _ => False

/-- Dispatch implements each operation by addition, without consulting admission or policy. -/
def reply : Request signature → Response signature
  | ⟨.twice, n⟩ => ⟨.twice, n + n⟩
  | ⟨.fourTimes, n⟩ => ⟨.fourTimes, (n + n) + (n + n)⟩

/-- Law (subject `ConcreteSpec.Core.Observe.RoutingDemo.reply`).
Dispatch meets the independently stated arithmetic and routing policy for both operation kinds.
There is no premise admitting the answer, and the request can have either kind. -/
theorem reply_correct (history : List (Event signature)) (q : Request signature) :
    contract.allows () history q (reply q) ∧ RequestResponse q (reply q) := by
  obtain ⟨kind, n⟩ := q
  cases kind with
  | twice =>
    change Nat at n
    refine ⟨⟨rfl, ?_⟩, RequestResponse.matched (S := signature) Kind.twice n (n + n)⟩
    change n * 2 = n + n
    omega
  | fourTimes =>
    change Nat at n
    refine ⟨⟨rfl, ?_⟩,
      RequestResponse.matched (S := signature) Kind.fourTimes n ((n + n) + (n + n))⟩
    change n * 4 = (n + n) + (n + n)
    omega

/-- Fault: arithmetic is unchanged, but a doubling request receives a quadrupling-kind answer. -/
def wrongRoute : Request signature → Response signature
  | ⟨.twice, n⟩ => ⟨.fourTimes, n + n⟩
  | ⟨.fourTimes, n⟩ => ⟨.twice, (n + n) + (n + n)⟩

/-- Instance (law `ConcreteSpec.Core.Observe.RoutingDemo.reply_correct`, case `typed_route`). -/
example : contract.allows () [.request ⟨.twice, 3⟩] ⟨.twice, 3⟩ (reply ⟨.twice, 3⟩) ∧
    RequestResponse (S := signature) ⟨.twice, 3⟩ (reply ⟨.twice, 3⟩) :=
  reply_correct [.request ⟨.twice, 3⟩] ⟨.twice, 3⟩

/-- Planted (law `ConcreteSpec.Core.Observe.RoutingDemo.reply_correct`, case `typed_route`,
subject `ConcreteSpec.Core.Observe.RoutingDemo.reply`, mutant `ConcreteSpec.Core.Observe.RoutingDemo.wrongRoute`). -/
example : ¬ (contract.allows () [.request ⟨.twice, 3⟩] ⟨.twice, 3⟩ (wrongRoute ⟨.twice, 3⟩) ∧
    RequestResponse (S := signature) ⟨.twice, 3⟩ (wrongRoute ⟨.twice, 3⟩)) := by
  intro invalid
  obtain ⟨⟨kinds, _⟩, _⟩ := invalid
  cases kinds

end RoutingDemo

end ConcreteSpec.Core.Observe

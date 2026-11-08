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

end ConcreteSpec.Core.Observe

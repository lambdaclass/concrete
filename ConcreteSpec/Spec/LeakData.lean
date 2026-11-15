import ConcreteSpec.Spec.Leak
import ConcreteSpec.Spec.ReleaseCalls

/-! Monomorphic C7 data adapters. These interpret closed data into the generic execution security
judgments; they neither approximate runs nor define a second security relation.
The integer machine is an instance, not Concrete's scalar or Boolean encoding policy. -/
namespace ConcreteSpec.Core.Leak
open ConcreteSpec.Core Exec

namespace Demo

/-- Values in the example are integers. `.secret` reads the shared secret store; `.masked` adds the
shared secret store to the decoded magnitude of the negative secret local `"hidden"`. -/
inductive Expr where
  | lit (n : Int)
  | secret
  | masked

/-- An explicit public release, a public output, or unobserved variable-time work. -/
inductive Prim where
  | release
  | output
  | work

/-- Decidable equality for the example's primitive answers. -/
instance answerDecidableEq : DecidableEq (Answer Int Int Int) := fun a b =>
  decidable_of_iff (a.event = b.event ∧ a.result = b.result ∧ a.store = b.store) (by
    constructor
    · intro h
      cases a
      cases b
      simp_all
    · intro h
      cases h
      exact ⟨rfl, rfl, rfl⟩)

/-- The Boolean answer contract for each primitive request in a given secret state. -/
def allowed (p : Prim) (vs : List Int) (st : Int) (a : Answer Int Int Int) : Bool :=
  match p, vs with
  | .release, [_] => decide (a = ⟨none, .ok 0, st⟩)
  | .output, [v] => decide (a = ⟨some v, .ok 0, st⟩)
  | .work, [_] => decide (a = ⟨none, .ok 0, st⟩)
  | _, _ => false

/-- The local variable holding the second, unreleased secret component. -/
def hiddenVar : Var := ⟨0⟩

/-- Encode `hidden : Nat` as `Int.negSucc hidden` (`-(Int.ofNat hidden) - 1`), which is always
strictly negative and therefore always classified as secret by `labelling`. -/
def hiddenLocals (hidden : Nat) : AList Var Int :=
  [(hiddenVar, Int.negSucc hidden)]

/-- Decode the non-negative offset `hidden` from a negative secret local encoding. -/
def decodeHidden : Int → Int
  | .negSucc n => Int.ofNat n
  | .ofNat _ => 0

/-- Read the decoded secret local offset from a local environment. -/
def hiddenOffset (locals : AList Var Int) : Int :=
  match locals.lookup hiddenVar with
  | some v => decodeHidden v
  | none => 0

/-- Evaluate a public literal, the raw shared secret, or the masked sum `st + hidden`. -/
def eval (locals : AList Var Int) (st : Int) : Expr → Res (Int × Int)
  | .lit n => .ok (n, st)
  | .secret => .ok (st, st)
  | .masked => .ok (st + hiddenOffset locals, st)

/-- Build the toy C1 operand instance for a given primitive answer oracle. Its nonzero condition
test is local to this example and chooses no Concrete Boolean encoding (ISS-0069). -/
abbrev opsWith (oracle : Prim → List Int → Int → Answer Int Int Int → Bool) : Operands where
  Val := Int
  Expr := Expr
  Ty := Unit
  Store := Int
  Prim := Prim
  Event := Int
  unitVal := 0
  unitTy := ()
  isCopy := fun _ => true
  eval := eval
  life := fun _ st => some st
  test := fun v => some (v ≠ 0)
  allows := oracle

/-- The example C1 operand instance with deterministic primitive answers `allowed`. -/
abbrev ops : Operands := opsWith allowed

/-- Release `released`, then evaluate `result` as the output. -/
def body (released result : Expr) : Stmt ops :=
  .seq (.prim none .release [released] ⟨0⟩) (.prim none .output [result] ⟨1⟩)

/-- The shared empty function table; the examples differ in their initial configurations. -/
def program : Program ops := ⟨[]⟩

/-- Declassify and output the masked sum `secret + hidden` while keeping `secret` and `hidden`
individually secret. Distinct secret pairs with the same sum (such as `(6, 1)` and `(5, 2)`) produce
identical release traces and equal outputs. -/
def start (secret : Int) (hidden : Nat := 0) : Config ops :=
  ⟨.exec (body .masked .masked), hiddenLocals hidden, secret, []⟩

/-- Mutant start configuration that declassifies the masked sum `secret + hidden` (preserving equal
release traces on `(6, 1)` and `(5, 2)`) but outputs the unmasked secret `secret`. -/
def exposingStart (secret : Int) (hidden : Nat := 0) : Config ops :=
  ⟨.exec (body .masked .secret), hiddenLocals hidden, secret, []⟩

/-- The leaky example releases public zero (authorized by `policy`) but outputs its secret. -/
def leakyStart (secret : Int) : Config ops := ⟨.exec (body (.lit 0) .secret), [], secret, []⟩

/-- Public output with secret-dependent variable-time work and no secret-dependent release. -/
def timingBody : Stmt ops := .seq (.prim none .work [.secret] ⟨2⟩) (body (.lit 0) (.lit 7))
def timingStart (secret : Int) : Config ops := ⟨.exec timingBody, [], secret, []⟩

/-- Constant-time start configuration that releases public zero and outputs public seven without
secret-dependent work, inhabiting all `DelimitedReleaseNI`, `ConstantTime`, and
`ReleasePolicy.RunsAuthorized` hypotheses across distinct secrets. -/
def ctStart (secret : Int) : Config ops := ⟨.exec (body (.lit 0) (.lit 7)), [], secret, []⟩

/-- The configuration after sequencing into the release. -/
def afterSeq (released result : Expr) (secret : Int) (locals : AList Var Int := []) : Config ops :=
  ⟨.exec (.prim none .release [released] ⟨0⟩), locals, secret,
    [.seq (.prim none .output [result] ⟨1⟩)]⟩

/-- The configuration after the release answer. -/
def afterRelease (result : Expr) (secret : Int) (locals : AList Var Int := []) : Config ops :=
  ⟨.done, locals, secret, [.seq (.prim none .output [result] ⟨1⟩)]⟩

/-- The configuration before the output answer. -/
def beforeOutput (result : Expr) (secret : Int) (locals : AList Var Int := []) : Config ops :=
  ⟨.exec (.prim none .output [result] ⟨1⟩), locals, secret, []⟩

/-- The configurations specific to sequencing and answering the timing example's work. -/
def timingAfterSeq (secret : Int) : Config ops :=
  ⟨.exec (.prim none .work [.secret] ⟨2⟩), [], secret, [.seq (body (.lit 0) (.lit 7))]⟩
def timingAfterWork (secret : Int) : Config ops :=
  ⟨.done, [], secret, [.seq (body (.lit 0) (.lit 7))]⟩
def timingBeforeBody (secret : Int) : Config ops :=
  ⟨.exec (body (.lit 0) (.lit 7)), [], secret, []⟩

/-- The common final configuration. -/
def finished (secret : Int) (locals : AList Var Int := []) : Config ops := ⟨.done, locals, secret, []⟩

/-- Public observations are output values and the C1 final outcome. -/
inductive Observation where
  | output (v : Int)
  | final (f : Final Int)
  deriving DecidableEq, Repr

/-- The primitive request at a configuration, with its evaluated operands, if that is what C1 is
about to execute. -/
def request : Config ops → Option (Prim × List Int × SiteId)
  | ⟨.exec (.prim _ p args site), locals, st, _⟩ =>
      match evalArgs ops locals st args with
      | .ok (vs, _) => some (p, vs, site)
      | _ => none
  | _ => none

/-- A release record carries the actual evaluated operand, not a constant chosen by the observer. -/
def releaseAt (site : SiteId) (value : Int) : Release Unit Int :=
  ⟨⟨site, ()⟩, value⟩

/-- The site-zero release used by the original local examples. -/
abbrev releaseValue (value : Int) : Release Unit Int := releaseAt ⟨0⟩ value

def releaseZero : Release Unit Int := releaseValue 0

/-- C7 instrumentation of the example. Every primitive is treated as variable-time, so its evaluated
operands are leaked. The external choice records only that a step was taken; answers themselves are
fixed by `allowed`. -/
abbrev instrumentation : Instrumentation ops where
  Address := Unit
  Effect := Unit
  Observation := Observation
  Choice := Unit
  observeStep := fun _ label _ => match label with
    | .silent => []
    | .emit v => [.output v]
  observeFinal := fun c => (c.final?).map .final
  leakage := fun c _ _ => match request c with
    | some (_, vs, site) => [.variableTime site vs]
    | none => []
  releases := fun c _ _ => match request c with
    | some (.release, [v], site) => [releaseAt site v]
    | _ => []
  choices := fun _ _ _ => [()]

/-- Negative local values are secret; nonnegative local values are public. The shared store remains
secret, which is what lets the input configurations be low-equivalent. -/
abbrev labelling : Labelling ops where
  PublicValue := Int
  project := fun v => if v < 0 then none else some v
  lowStore := fun _ _ => True

/-- The policy authorizes exactly the typed release site used by the examples. -/
abbrev policy : ReleasePolicy Unit := ⟨[⟨⟨0⟩, ()⟩]⟩

end Demo

/-- Closed integer-machine statement data. Every C1 statement constructor is represented;
this is a serialization adapter, with all execution delegated to Exec.Step. -/
inductive IntegerStatement where
  | skip
  | bind (x : Var) (e : Demo.Expr) (site : SiteId)
  | assign (x : Var) (e : Demo.Expr) (site : SiteId)
  | seq (s t : IntegerStatement)
  | block (b : BlockId) (s : IntegerStatement)
  | defer (f : FnId) (args : List Demo.Expr) (site : SiteId)
  | cond (c : Demo.Expr) (s t : IntegerStatement) (site : SiteId)
  | loop (l : LoopId) (c : IntegerStatement) (test : Demo.Expr) (body : IntegerStatement) (site : SiteId)
  | brk (l : LoopId)
  | cont (l : LoopId)
  | ret (e : Demo.Expr) (site : SiteId)
  | call (dst : Option Var) (f : FnId) (args : List Demo.Expr) (site : SiteId)
  | prim (dst : Option Var) (p : Demo.Prim) (args : List Demo.Expr) (site : SiteId)
  | abort (site : SiteId)

/-- Decode statement data structurally; no evaluation or admission occurs in this adapter. -/
def IntegerStatement.statement : IntegerStatement → Stmt Demo.ops
  | .skip => .skip
  | .bind x e site => .bind x e site
  | .assign x e site => .assign x e site
  | .seq s t => .seq s.statement t.statement
  | .block b s => .block b s.statement
  | .defer f args site => .defer f args site
  | .cond c s t site => .cond c s.statement t.statement site
  | .loop l c test body site => .loop l c.statement test body.statement site
  | .brk l => .brk l
  | .cont l => .cont l
  | .ret e site => .ret e site
  | .call dst f args site => .call dst f args site
  | .prim dst p args site => .prim dst p args site
  | .abort site => .abort site

/-- Encode any C1 integer-machine statement, so the data interface is not a fixture selector. -/
def IntegerStatement.encode : Stmt Demo.ops → IntegerStatement
  | .skip => .skip
  | .bind x e site => .bind x e site
  | .assign x e site => .assign x e site
  | .seq s t => .seq (encode s) (encode t)
  | .block b s => .block b (encode s)
  | .defer f args site => .defer f args site
  | .cond c s t site => .cond c (encode s) (encode t) site
  | .loop l c test body site => .loop l (encode c) test (encode body) site
  | .brk l => .brk l
  | .cont l => .cont l
  | .ret e site => .ret e site
  | .call dst f args site => .call dst f args site
  | .prim dst p args site => .prim dst p args site
  | .abort site => .abort site

/-- Representation lemma: decoding the data encoding recovers every C1 statement exactly. -/
theorem IntegerStatement.statement_encode (s : Stmt Demo.ops) : (encode s).statement = s := by
  induction s <;> simp_all [encode, statement]

/-- A function in the monomorphic integer machine; each parameter and result has Unit type. -/
structure IntegerFunction where
  params : List Var
  block : BlockId
  body : IntegerStatement

/-- Decode a complete function table rather than choosing a fixture program by name. -/
def integerProgram (fns : List (FnId × IntegerFunction)) : Program Demo.ops :=
  ⟨fns.map fun (f, d) => (f, ⟨d.params.map fun x => (x, ()), (), d.block, d.body.statement⟩)⟩

/-- An arbitrary initial integer-machine statement, local environment and shared store.
The entry starts with no continuations, as a top-level execution. -/
structure IntegerEntry where
  body : IntegerStatement
  locals : AList Var Int
  store : Int

/-- Reconstruct the C1 configuration from closed entry data. -/
def IntegerEntry.config (d : IntegerEntry) : Config Demo.ops :=
  ⟨.exec d.body.statement, d.locals, d.store, []⟩

/-- Complete program and entry data for authorization. -/
structure IntegerExecution where
  functions : List (FnId × IntegerFunction)
  entry : IntegerEntry

/-- Two entries of the same concrete program, observed by the integer machine's baseline
instrumentation and public-state labelling. Generic observers remain in the owning judgments. -/
structure IntegerComparison where
  functions : List (FnId × IntegerFunction)
  left : IntegerEntry
  right : IntegerEntry

/-- Integer-machine instance of complete-run authorization, with concrete policy and entry data.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def IntegerRunsAuthorized (d : ReleasePolicy Unit × IntegerExecution) : Prop :=
  d.1.RunsAuthorized Demo.instrumentation (integerProgram d.2.functions) d.2.entry.config

/-- Integer-machine instance of public-state equivalence.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def IntegerLowEquivalent (d : IntegerComparison) : Prop :=
  LowEquivalent Demo.labelling d.left.config d.right.config

/-- Integer-machine instance of delimited release NI, quantifying over all C1 runs.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def IntegerDelimitedReleaseNI (d : IntegerComparison) : Prop :=
  DelimitedReleaseNI Demo.instrumentation (integerProgram d.functions) Demo.labelling d.left.config d.right.config

/-- Integer-machine instance of CT, quantifying over all C1 runs separately from NI.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def IntegerConstantTime (d : IntegerComparison) : Prop :=
  ConstantTime Demo.instrumentation (integerProgram d.functions) Demo.labelling d.left.config d.right.config

/-- Monomorphic data view of call graph admission; delegates to the owning judgment.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def UnitCallsAdmitted (d : ReleaseInterface Unit × ReleaseCalls Unit) : Prop := d.1.Admitted d.2

/-- Concrete program admission, with the integer primitive interface's fixed release classifier.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def IntegerProgramAdmitted (d : ReleaseInterface Unit × List (FnId × IntegerFunction)) : Prop :=
  d.1.ProgramAdmitted (fun p : Demo.Prim => match p with | .release => some () | _ => none)
    (integerProgram d.2)

/-- Monomorphic data view of typed-site permission; delegates to the owning judgment.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def StringReleasePermitted (d : ReleasePolicy String × Release String Nat) : Prop := d.1.Permits d.2

/-- Monomorphic data view of empty release policy; delegates to the owning judgment.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def UnitReleaseForbidden (d : ReleasePolicy Unit) : Prop := d.ForbidsRelease

/-- Monomorphic data view of ordered release authorization; delegates to the owning judgment.
Authority: docs/decisions.md#decided-2026-11-08-foundation (C7),
docs/decisions.md#decided-project-infoflow-2026-11-06. -/
def UnitTraceAuthorized (d : ReleasePolicy Unit × List (Release Unit Int)) : Prop := d.1.Authorized d.2


end ConcreteSpec.Core.Leak

import ConcreteSpec.Spec.Exec

/-!
# Observations and the environment

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`. Foundation contract C6 (the foundation proposal approved by the
owner on 2026-11-08, "ok all recommended"). `ConcreteSpec.Core.Observe` is the C6 foundation
replacement for the pre-foundation `ConcreteSpec.Spec.Observation`; A2i integration step 12 retires
the old module and moves its remaining observation theorems.

An `EventSignature` is the extension point for external operations. Each operation kind has its
own request and response types; public output has its own type. Requests and responses are separate
events, and `RequestResponse` rules out a response of one kind answering a request of another.
An `OperationContract` relates a response (or explicit blocking) to the immutable launch input and
the shared event history. It may allow several responses; no deterministic or cooperative
environment is assumed. Grant-specific instances, foreign-operation instances and concurrency are
later slices.

A finite trace is a list of events and an infinite trace is a function from event number to event.
A `FullObservation` distinguishes termination, a blocked request, divergence after finitely many
events (including silent divergence), and divergence with infinitely many events. Evaluator fuel,
unsafe stuckness and malformed environment answers are not program observations (Q6.7).

The hosted launcher observations are a normal exit with its status, startup refusal with status
125, and a trap with status 134 (Q7.9–Q7.10, Q8.4–Q8.6). A trap retains its kind and typed site,
and its observation contains the exact decided line. `Span.col` is a 1-based UTF-8 byte column;
a tab is one byte (foundation question 10, 2026-11-08). There is no abbreviated trap format.
-/

namespace ConcreteSpec.Core.Observe
open ConcreteSpec.Core

/-- The types an external-event instance supplies. `Kind` is extensible by choosing a larger type;
each kind keeps its own request and response types. Public output is separate from operations. -/
structure EventSignature where
  Kind : Type
  Request : Kind → Type
  Response : Kind → Type
  Output : Type

variable {S : EventSignature}

/-- A request of some operation kind, with that kind's request payload. -/
abbrev Request (S : EventSignature) := Sigma S.Request

/-- A response of some operation kind, with that kind's response payload. -/
abbrev Response (S : EventSignature) := Sigma S.Response

/-- An externally relevant event. Split request and response events implement Q7.9; output is
kept in the same ordered history. -/
inductive Event (S : EventSignature) where
  | request (q : Request S)
  | response (r : Response S)
  | output (o : S.Output)

/-- A response matches a request only when both carry the same operation kind. The dependent
payloads therefore have the same request/response signature as well. -/
inductive RequestResponse {S : EventSignature} : Request S → Response S → Prop where
  | matched (k : S.Kind) (q : S.Request k) (r : S.Response k) :
      RequestResponse ⟨k, q⟩ ⟨k, r⟩

/-- The environment's explicit answer to a request: a typed response, or a request that remains
blocked and produces no response event. -/
inductive Answer (S : EventSignature) where
  | response (r : Response S)
  | blocked

/-- The contract of every operation kind, interpreted against one immutable launch-environment
snapshot (`Input`, Q7.5) and the ordered events already observed. In `allows` and `mayBlock`, the
history includes the current request as its last event. Error, partial-delivery and blocking cases
are choices of the instance, not universal traps or universal progress assumptions. -/
structure OperationContract (S : EventSignature) where
  Input : Type
  allows : Input → List (Event S) → Request S → Response S → Prop
  mayBlock : Input → List (Event S) → Request S → Prop

variable {C : OperationContract S}

/-- An environment relation. A nondeterministic response must satisfy the operation contract and
match the request; blocking is explicit and permitted only where the contract permits it. -/
inductive Responds (C : OperationContract S) (input : C.Input) :
    List (Event S) → Request S → Answer S → Prop where
  | response {hist q r} : RequestResponse q r → C.allows input hist q r →
      Responds C input hist q (.response r)
  | blocked {hist q} : C.mayBlock input hist q → Responds C input hist q .blocked

/-- A sequential request/response trace. At most one request is outstanding. Output occurs only
while no request is outstanding; a response must match the outstanding request and satisfy its
contract in the history ending in that request. The final state `none` means that no request is
outstanding; `some q` is a prefix waiting for `q`'s response. -/
inductive ValidTrace (C : OperationContract S) (input : C.Input) :
    Option (Request S) → List (Event S) → Prop where
  | idle : ValidTrace C input none []
  | request {hist q} : ValidTrace C input none hist →
      ValidTrace C input (some q) (hist ++ [.request q])
  | response {hist q r} : ValidTrace C input (some q) hist → RequestResponse q r →
      C.allows input hist q r → ValidTrace C input none (hist ++ [.response r])
  | output {hist o} : ValidTrace C input none hist →
      ValidTrace C input none (hist ++ [.output o])

/-- A complete finite trace has no outstanding request. -/
def CompleteTrace (C : OperationContract S) (input : C.Input) (events : List (Event S)) : Prop :=
  ValidTrace C input none events

/-- A finite externally visible history, oldest event first. -/
abbrev FiniteTrace (S : EventSignature) := List (Event S)

/-- An infinite externally visible history, indexed from zero. -/
abbrev InfiniteTrace (S : EventSignature) := Nat → Event S

/-- A program's mapping from typed source sites to source spans. -/
abbrev SiteTable := AList SiteId Span

/-- A well-formed site table gives every resolved site a project-relative, forward-slash path.
Elaboration's stronger normalized-path obligation remains O1. -/
def SiteTable.Valid (sites : SiteTable) : Prop :=
  ∀ site sp, AList.lookup sites site = some sp → sp.PathOk

/-- The character form of the decided trap line (OD101, Q7.10, runtime decision 2026-11-06).
Keeping the append structure explicit also makes the exact delimiter bytes auditable. -/
def trapLineChars (kind : TrapKind) (site : SiteId) (sp : Span) : List Char :=
  "concrete: trap: ".toList ++ kind.name.toList ++ " at ".toList ++ sp.path.toList ++
    ":".toList ++ (toString sp.line).toList ++ ":".toList ++ (toString sp.col).toList ++
    " (site ".toList ++ (toString site.id).toList ++ ")".toList

/-- The exact trap line, with no per-build abbreviation and no trailing newline. -/
def trapLine (kind : TrapKind) (site : SiteId) (sp : Span) : String :=
  String.ofList (trapLineChars kind site sp)

/-- The 1-based byte column after source text `before`: its UTF-8 byte length plus one. -/
def byteColumn (before : String) : Nat := before.utf8ByteSize + 1

/-- Construct an operation's source span from the text preceding it on its source line.
The caller supplies a positive line number, but cannot supply a column: UTF-8 source bytes
establish that column, with a tab contributing its single encoded byte (foundation Q10).
Connecting the elaborator's source offsets to this constructor remains O1. -/
def sourceSpan (path : String) (line : Nat) (linePositive : 0 < line)
    (before : String) : Span :=
  ⟨path, line, before.utf8ByteSize + 1, linePositive, Nat.zero_lt_succ _⟩

/-- A span's column is the byte column at the operation on its source line. -/
def Span.HasByteColumn (sp : Span) (before : String) : Prop := sp.col = byteColumn before

/-- A logical terminal action visible to the parent process. A trapped observation stores the
resolved span and the already rendered line, so later site-table changes cannot rewrite it. -/
inductive Terminal where
  | startupRefused
  | exited (status : Nat)
  | trapped (kind : TrapKind) (site : SiteId) (sp : Span) (line : String)

/-- The terminal observation of a trap at a site present in the table. An absent site is not
assigned an invented source location. -/
def trapTerminal (sites : SiteTable) (kind : TrapKind) (site : SiteId) : Option Terminal :=
  (AList.lookup sites site).map fun sp => .trapped kind site sp (trapLine kind site sp)

/-- The process status (Q7.10): startup refusal 125, a normal exit's own status, trap 134. -/
def Terminal.status : Terminal → Nat
  | .startupRefused => 125
  | .exited status => status
  | .trapped .. => 134

/-- The diagnostic line of a trap; ordinary exits and startup refusal do not acquire a trap line. -/
def Terminal.trapLine? : Terminal → Option String
  | .trapped _ _ _ line => some line
  | _ => none

/-- A finite event history followed by a logical terminal action. -/
structure Observation (S : EventSignature) where
  events : FiniteTrace S
  terminal : Terminal

/-- Every externally visible behavior: finite termination, an explicitly blocked request, silent
or eventually silent divergence with a finite event history, or an infinite event history. In
`blocked events request`, `events` ends in `.request request`: this is the history passed to the
operation contract, and the pending state of a `ValidTrace`. -/
inductive FullObservation (S : EventSignature) where
  | terminal (observation : Observation S)
  | blocked (events : FiniteTrace S) (request : Request S)
  | finiteDivergence (events : FiniteTrace S)
  | infiniteDivergence (events : InfiniteTrace S)

/-- Startup is refused before any program event when a required launch grant is missing. -/
def launch (configured : Bool) (run : Unit → FullObservation S) : FullObservation S :=
  if configured then run () else .terminal ⟨[], .startupRefused⟩

/-! ## Preservation across silent internal steps -/

/-- An execution-step label: internal work, or one externally relevant event. -/
inductive Step (S : EventSignature) where
  | silent
  | emit (event : Event S)

/-- A finite terminated or blocked execution, or a step-indexed infinite execution. The infinite
form retains silent steps, so an empty event history does not make it terminated. -/
inductive Execution (S : EventSignature) where
  | finite (steps : List (Step S)) (terminal : Terminal)
  | blocked (steps : List (Step S)) (request : Request S)
  | infinite (steps : Nat → Step S)

/-- Any finite number of silent steps inserted anywhere in a finite execution. -/
inductive SilentInsertion : List (Step S) → List (Step S) → Prop where
  | nil : SilentInsertion [] []
  | silent {before after} : SilentInsertion before after → SilentInsertion before (.silent :: after)
  | keepSilent {before after} : SilentInsertion before after →
      SilentInsertion (.silent :: before) (.silent :: after)
  | emit {before after} event : SilentInsertion before after →
      SilentInsertion (.emit event :: before) (.emit event :: after)

/-- A strictly increasing map from old step indices to new step indices. -/
def StepIndexIncreasing (f : Nat → Nat) : Prop := ∀ n, f n < f (n + 1)

/-- Infinite stuttering retains old step `n` at new index `f n`; every other new step is silent. -/
def InfiniteStuttering (steps : Nat → Step S) (f : Nat → Nat) (stutter : Nat → Step S) : Prop :=
  StepIndexIncreasing f ∧ (∀ n, stutter (f n) = steps n) ∧
    ∀ m, (∀ n, f n ≠ m) → stutter m = .silent

/-- The event contributed by a step, if any. -/
def Step.event? : Step S → Option (Event S)
  | .silent => none
  | .emit event => some event

/-- The ordered external events of finitely many steps, with silent steps removed. -/
def visible (steps : List (Step S)) : List (Event S) := steps.filterMap Step.event?

/-- The events in the first `n` steps of an infinite execution. -/
def visiblePrefix (steps : Nat → Step S) (n : Nat) : List (Event S) :=
  visible ((List.range n).map steps)

/-- One finite event prefix occurs as a prefix of some, possibly later, prefix of another
execution. This permits every finite amount of internal stuttering. -/
def PrefixEventually (steps₁ steps₂ : Nat → Step S) : Prop :=
  ∀ n, ∃ m rest, visiblePrefix steps₂ m = visiblePrefix steps₁ n ++ rest

/-- Two infinite executions have the same full event history, including the case where both stop
emitting and diverge silently. Each visible prefix of either must eventually occur in the other. -/
def SameInfiniteObservation (steps₁ steps₂ : Nat → Step S) : Prop :=
  PrefixEventually steps₁ steps₂ ∧ PrefixEventually steps₂ steps₁

/-- Full-observation equivalence for executions. Finite runs preserve every event and the exact
terminal action; blocked runs also preserve the pending request. Infinite runs use the eventual
prefix relation. Mixed finite/infinite cases are false, which is what prevents silent divergence
from becoming successful termination. -/
def SameObservation : Execution S → Execution S → Prop
  | .finite xs terminal, .finite ys terminal' =>
      visible xs = visible ys ∧ terminal = terminal'
  | .blocked xs request, .blocked ys request' =>
      visible xs = visible ys ∧ request = request'
  | .infinite xs, .infinite ys => SameInfiniteObservation xs ys
  | _, _ => False

/-- The canonical infinite execution that emits a finite history and then is silent forever. -/
def finiteDivergenceSteps (events : FiniteTrace S) (n : Nat) : Step S :=
  if h : n < events.length then .emit events[n] else .silent

/-- An execution has a particular full observation. Finite and blocked runs project exactly their
visible history and ending. An infinite run with finite divergence has the same history as the
canonical eventually silent execution; infinite divergence retains every event of an infinite
trace. This is a projection relation, not merely a restatement of `SameObservation`. -/
def HasFullObservation : Execution S → FullObservation S → Prop
  | .finite steps terminal, .terminal observation =>
      observation = ⟨visible steps, terminal⟩
  | .blocked steps request, .blocked events request' =>
      visible steps ++ [.request request] = events ∧ request = request'
  | .infinite steps, .finiteDivergence events =>
      SameInfiniteObservation steps (finiteDivergenceSteps events)
  | .infinite steps, .infiniteDivergence events =>
      SameInfiniteObservation steps (fun n => .emit (events n))
  | _, _ => False

/-! ## C1 adapter -/

/-- How a C1 operand instance appears in C6. An answer's C1 event may project to several typed C6
events (for example, the split request and response it records). A pending C1 request must have a
typed C6 request for blocking to be observed. Only a valid `main` result needs a status mapping;
C1's value-less `Final.normal` is not given an invented process status. -/
structure ExecAdapter (O : Exec.Operands) (S : EventSignature) where
  events : O.Event → List (Event S)
  request : O.Prim → List O.Val → SiteId → O.Store → Option (Request S)
  status : O.Val → Option Nat
  sites : SiteTable

variable {O : Exec.Operands}

/-- Project a C1 event history without dropping or reordering any of its events' observations. -/
def ExecAdapter.project (A : ExecAdapter O S) (events : List O.Event) : List (Event S) :=
  events.flatMap A.events

/-- Adapter and oracle well-formedness against a C6 operation contract. An internal answer emits
only output. If a C1 request has typed request `q`, its answer projects to any output, then
`.request q`, then a matching `.response r`, then any output; output cannot intervene while `q` is
pending. The response satisfies `C.allows` in the history ending in that request. An unanswered
typed request is blocking, not an evaluator result, only when `C.mayBlock` permits it. -/
structure ExecAdapter.WellFormed (A : ExecAdapter O S) (C : OperationContract S) (input : C.Input)
    (ρ : Exec.Oracle O) : Prop where
  internal {hist p vs site store a} : ρ hist p vs site store = some a →
    A.request p vs site store = none →
    ∃ outputs : List S.Output, A.project a.event.toList = outputs.map Event.output
  answered {hist p vs site store a q} : ρ hist p vs site store = some a →
    A.request p vs site store = some q →
    ∃ before r after, A.project a.event.toList = before ++ [.request q, .response r] ++ after ∧
      (∃ outputs : List S.Output, before = outputs.map Event.output) ∧
      (∃ outputs : List S.Output, after = outputs.map Event.output) ∧
      RequestResponse q r ∧ C.allows input (A.project hist ++ before ++ [.request q]) q r
  blocked {hist p vs site store q} : ρ hist p vs site store = none →
    A.request p vs site store = some q →
    C.mayBlock input (A.project hist ++ [.request q]) q

/-- Turn a C1 final configuration into a logical terminal observation, if it is a valid hosted
`main` ending with a known trap site. -/
def ExecAdapter.terminal? (A : ExecAdapter O S) : Exec.Final O.Val → Option Terminal
  | .normal => none
  | .returned value => (A.status value).map .exited
  | .trapped kind site => trapTerminal A.sites kind site

/-- The observation of a C1 finite execution. Its event projection is retained in full, including
all output emitted before a trap. -/
def stepsObservation (A : ExecAdapter O S) {P : Exec.Program O} {c c' : Exec.Config O}
    {events : List O.Event} (_steps : Exec.Steps P c events c') : Option (Observation S) :=
  (c'.final?.bind A.terminal?).map fun terminal => ⟨A.project events, terminal⟩

/-- Every `noAnswer` projection in a report is permitted to block. The history ends in the typed
request, exactly as required by `OperationContract.mayBlock` and `ValidTrace`. -/
def ExecAdapter.BlockingPermitted (P : Exec.Program O) (A : ExecAdapter O S)
    (C : OperationContract S) (input : C.Input) (r : Exec.Report O) : Prop :=
  ∀ {c p vs site store k q}, r.ending = .noAnswer c →
    Exec.step P c = some (.request p vs site store k) → A.request p vs site store = some q →
    C.mayBlock input (A.project r.events ++ [.request q]) q

/-- The observation of a C1 evaluator report. The `BlockingPermitted` premise prevents an
unanswered request from becoming a program observation when its operation forbids blocking.
A blocked observation retains the earlier events and ends in its pending request. Unsafe stuckness,
a bad answer and fuel exhaustion remain evaluator/premise reports, not external events. -/
def reportObservation (P : Exec.Program O) (A : ExecAdapter O S) (C : OperationContract S)
    (input : C.Input) (r : Exec.Report O) (_permitted : ExecAdapter.BlockingPermitted P A C input r) :
    Option (FullObservation S) :=
  match r.ending with
  | .final final => (A.terminal? final).map fun terminal =>
      .terminal ⟨A.project r.events, terminal⟩
  | .noAnswer c => match Exec.step P c with
    | some (.request p vs site store _) => (A.request p vs site store).map fun request =>
        .blocked (A.project r.events ++ [.request request]) request
    | _ => none
  | .stuck _ | .badAnswer _ | .outOfFuel _ => none

/-! ## Examples -/

namespace Demo

inductive Op where
  | double

/-- One operation kind: a natural request, its doubled natural response, and text output. -/
abbrev signature : EventSignature where
  Kind := Op
  Request := fun _ => Nat
  Response := fun _ => Nat
  Output := String

/-- Responses double the request after any history; blocking is refused by this operation. -/
abbrev contract : OperationContract signature where
  Input := Unit
  allows := fun _ _ q r => q.2 * 2 = r.2
  mayBlock := fun _ _ _ => False

def request (n : Nat) : Event signature := .request ⟨.double, n⟩
def response (n : Nat) : Event signature := .response ⟨.double, n⟩

/-- Accepted: request `3`, response `6`, then output, with no request outstanding. -/
example : CompleteTrace contract () [request 3, response 6, .output "done"] := by
  exact .output (.response (.request .idle) (RequestResponse.matched (S := signature) Op.double (3 : Nat) (6 : Nat)) rfl)

/-- Refused: `7` is not a response to doubling `3`, so the environment relation excludes it. -/
example : ¬ Responds contract () [request 3] ⟨.double, 3⟩ (.response ⟨.double, 7⟩) := by
  intro h
  cases h with
  | response _ allows => contradiction

/-- Refused: this operation may not remain blocked. -/
example : ¬ Responds contract () [request 3] ⟨.double, 3⟩ .blocked := by
  intro h
  cases h with
  | blocked blocking => exact blocking

/-- A matched response has the operation's own kind and satisfies its contract. -/
example : Responds contract () [request 3] ⟨.double, 3⟩ (.response ⟨.double, 6⟩) :=
  .response (RequestResponse.matched (S := signature) Op.double (3 : Nat) (6 : Nat)) rfl

/-- Infinite traces need no coinductive library: every event in this example is output. -/
example : InfiniteTrace signature := fun _ => .output "tick"

/-- Internal steps before and after an event do not change a finite observation. -/
example : SameObservation (.finite [.silent, .emit (request 1), .silent] (.exited 0))
    (.finite [.emit (request 1)] (.exited 0)) := by
  exact ⟨rfl, rfl⟩

/-- Accepted: an existing silent step is retained while another is inserted. -/
example : SilentInsertion (S := signature) [.silent, .emit (request 1)]
    [.silent, .silent, .emit (request 1)] :=
  .keepSilent (.silent (.emit (request 1) .nil))

/-- An infinite silent execution is not a successful empty termination. -/
example : ¬ SameObservation (S := signature) (.infinite (fun _ => Step.silent))
    (.finite [] (.exited 0)) := by
  simp [SameObservation]

def site : SiteId := ⟨7⟩
def sp : Span := ⟨"src/main.con", 3, 9, by decide, by decide⟩
def sites : SiteTable := [(site, sp)]

/-- The exact decided line, including the byte column and typed site identity. -/
example : trapLine .overflow site sp =
    "concrete: trap: overflow at src/main.con:3:9 (site 7)" := by decide

example : (trapTerminal sites .overflow site).map Terminal.status = some 134 := by decide
example : (launch (S := signature) false (fun _ => .finiteDivergence [])) =
    .terminal ⟨[], .startupRefused⟩ := rfl
example : (Terminal.exited 134).status = 134 ∧ Terminal.exited 134 ≠
    (trapTerminal sites .overflow site).get (by decide) := by
  constructor
  · rfl
  · intro h
    cases h

/-- A tab contributes one byte; `é` contributes its two UTF-8 bytes. The next column is five after
`a`, tab and `é`, not four after three Unicode scalar values. -/
example : byteColumn "a\té" = 5 := by decide

example : Span.HasByteColumn (Span.mk "src/main.con" 1 5 (by decide) (by decide)) "a\té" := rfl

/-- Refusing result binding through lifecycle instrumentation; all other demo operations are
unchanged. -/
abbrev refusingBindOps : Exec.Operands := { Exec.Demo.ops with
  life := fun e st => match e with
    | .bind y => if y = Exec.Demo.x then none else some (Exec.Demo.DState.life e st)
    | _ => some (Exec.Demo.DState.life e st) }

/-- A request whose otherwise valid response would initialize the refused local. -/
def refusedRequest : Exec.Config refusingBindOps :=
  ⟨.exec (.prim (some Exec.Demo.x) .log [.lit 3] Exec.Demo.s), [], .init, []⟩

/-- The answer is allowed by the primitive contract, but its continuation is refused. -/
def refusedReport : Exec.Report refusingBindOps :=
  Exec.run ⟨[]⟩ Exec.Demo.env 1 refusedRequest

/-- If the refused response had been accepted, its C1 event would project to a C6 response. -/
def refusingAdapter : ExecAdapter refusingBindOps signature where
  events := fun n => [response (Int.toNat n)]
  request := fun _ _ _ _ => none
  status := fun _ => none
  sites := []

/-- Helper (computation). Bookkeeping for a stuck report: its `noAnswer` antecedent is
impossible. This is not a contract law or evidence of blocking correctness. (approved: PENDING) -/
theorem refusedReportPermitted :
    ExecAdapter.BlockingPermitted ⟨[]⟩ refusingAdapter contract () refusedReport := by
  intro c p vs site store k q ending pending typed
  rw [show refusedReport.ending = .stuck refusedRequest from rfl] at ending
  cases ending

/-- Refused: result binding rejects the response. The run keeps the requesting configuration as
its stuck ending, records neither the response nor any other event, and has no C6 observation. -/
example : refusedReport.events = [] ∧
    (match refusedReport.ending with | .stuck _ => true | _ => false) = true ∧
    reportObservation ⟨[]⟩ refusingAdapter contract () refusedReport refusedReportPermitted = none := by
  refine ⟨by decide, by decide, rfl⟩

end Demo

end ConcreteSpec.Core.Observe

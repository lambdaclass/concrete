import ConcreteSpec.Spec.Examples

/-!
# Part 7: observations

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Observe
open ConcreteSpec

/-! ## Part 7. What the parent process observes (rows Q7.10, Q8.5; R4 F6)

The launcher of the hosted target turns the outcome of a run of slice 1 into what the parent
process observes (row Q7.10, R4 F6 "Startup and shutdown"):

* if a grant `main` asks for is not configured at launch, the launcher refuses to start, before
  `main` runs, with exit status 125;
* if `main` returns `code : u8`, the exit status is `code`;
* if the run traps, the runtime writes one line naming the trap and its source location to
  descriptor 2 (OD101's content) and the exit status is 134.

These effects belong to the target, not to source code: no function gains a permission from them
(authority rule 12). A status of 134 alone does not tell a trap from `return 134`; the trap
message does, and the observation records which trap occurred.

Not modelled here: the bytes of the trap line (OD101's content) and of the startup refusal, and
the trap's source location, which the decision requires in that line but which needs the
elaborator's source positions (not yet defined, README); the observation records only which trap
occurred. A run
that does not end (here: one that runs out of fuel) has no observation, and neither has a stuck
run, which a checked program never reaches (`checked_never_stuck`).
-/

/-- What the parent process observes. -/
inductive Observation where
  /-- A configured grant was missing: the launcher refused to start, before `main`. -/
  | startupRefused
  /-- `main` returned `code`. -/
  | exited (code : Nat)
  /-- The run trapped with `k`; the runtime wrote `k`'s message to descriptor 2. -/
  | trapped (k : Trap)
  deriving DecidableEq, Repr

/-- The exit status (Q7.10): 125 for a refused start, the returned code, or 134 for a trap. -/
def Observation.status : Observation → Nat
  | .startupRefused => 125
  | .exited code => code
  | .trapped _ => 134

/-- The launcher's map from a run's outcome to the observation. `configured` says that every grant
`main` asks for was configured at launch; if not, `main` never runs. -/
def observe (configured : Bool) (o : Outcome) : Option Observation :=
  if configured then
    match o with
    | .exit code => some (.exited code)
    | .trap k => some (.trapped k)
    | .stuck _ => none
    | .outOfFuel => none
  else some .startupRefused

/-! ### Examples -/

/-- `moveThenConsume` exits with status 42. -/
example : (observe true (run Examples.moveThenConsume 10)).map Observation.status = some 42 := by
  decide

/-- The accepted body `add100` traps on input 200 (it is not statically known): the trap message
goes to descriptor 2 and the status is 134. -/
example : observe true (runBody Examples.decls 10 (Examples.inputEnv 200) Examples.add100
    (.copy Examples.s₂)) = some (.trapped .arithmeticOverflow) := by decide

example : (Observation.trapped .arithmeticOverflow).status = 134 := rfl

/-- A missing configured grant: status 125, and `main` does not run. -/
example : (observe false (run Examples.moveThenConsume 10)).map Observation.status = some 125 := by
  decide

/-- The status of a trap and of `return 134` agree; the observations do not. -/
example : (Observation.trapped .arithmeticOverflow).status = (Observation.exited 134).status ∧
    Observation.trapped .arithmeticOverflow ≠ .exited 134 := by decide

end ConcreteSpec.Observe

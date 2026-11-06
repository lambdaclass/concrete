/-!
# Part 6: authority

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Authority

/-! ## Part 6. Authority: capabilities and grants (authority rules 1–8, 13; Q5.1–Q5.4, Q7.11)

A function's `with(...)` clause lists the permissions it may use. Permissions are capabilities;
they exist only at compile time. The resources a function may reach are grant values, passed to
it as parameters. An operation needs both (rules 1 and 2): a grant of the operation's type, and
every capability the primitive table gives the operation (rule 7). A call needs the callee's
permissions, with the callee's capability variables replaced by the call's explicit capability
arguments (Q5.1, Q5.4: no inference), to be included in the caller's (TRUST-4, rule 7). A trusted
function is checked the same way, and `Unsafe` never stands in for an operational capability
(rule 5). A function's permissions are checked once, for every value of its capability variables
(rule 3), and `sub_iff` (`Proofs/Authority.lean`) proves that the check is exactly inclusion for
every value. `authority_contained` proves what the rules buy: every event a run of `main`
produces, whether or not the run finishes, is an operation on a grant `main` received, through
capabilities `main` declares.

`ProgOk` checks authority under an assumed valid launch: it does not check `main`'s grant
parameters against the target (the hosted table's instance limits, such as at most one `StdOut`,
and the grants and permissions the target offers; row Q7.10). `observe` (part 7) refuses a start
whose configured grants are missing.

Not modelled here: the ownership of grants (that `main` consumes each grant before it returns,
rule 12, and narrowing), checked acquisition, minting and unforgeability of grants and the
identity of trusted wrappers (rule 11), the target's own effects (startup, shutdown, the trap
message), the "may block" table (rule 6), returned values (rule 8), function-pointer values and
their exact capability sets (rule 4), foreign calls, trait dispatch, generic methods and generated
cleanup (rules 5 and 7; this part has direct calls only), and reserving the capability names
against other uses (rule 13). The operations are a representative part of the hosted table (row
Q7.10), one per grant type used here.
-/

/-- The reserved capability names (authority rule 13). `Console` is the permission for operations
on the standard-stream grants `StdIn`, `StdOut` and `StdErr` (row Q7.11); `Device` is for device
grants; `Unsafe` is not an operational capability (rule 5). -/
inductive Cap where
  | fs | network | env | time | alloc | device | console | process | «unsafe»
  deriving DecidableEq, Repr

/-- Every capability, once. -/
def Cap.all : List Cap := [.fs, .network, .env, .time, .alloc, .device, .console, .process, .«unsafe»]

/-- A capability-set expression as written in `with(...)` or as an explicit capability argument
(Q5.2): named capabilities and capability variables, joined by union. A function's capability
variables are numbered by position, `0, 1, …`. `with()` is `⟨[], []⟩`. -/
structure CapExpr where
  caps : List Cap
  vars : List Nat
  deriving DecidableEq, Repr

/-- A written set names no capability and no variable twice (OD108, Q5.3: a repeated name is
refused, not repaired), and uses only the `n` capability variables in scope. -/
def CapExpr.WellFormed (n : Nat) (e : CapExpr) : Prop :=
  e.caps.Nodup ∧ e.vars.Nodup ∧ ∀ v ∈ e.vars, v < n

instance (n : Nat) (e : CapExpr) : Decidable (e.WellFormed n) := by
  unfold CapExpr.WellFormed; infer_instance

/-- What a set expression contains once each variable `v` stands for the set `ρ v`. -/
def CapExpr.Mem (ρ : Nat → List Cap) (e : CapExpr) (c : Cap) : Prop :=
  c ∈ e.caps ∨ ∃ v ∈ e.vars, c ∈ ρ v

instance (ρ : Nat → List Cap) (e : CapExpr) (c : Cap) : Decidable (e.Mem ρ c) := by
  unfold CapExpr.Mem; infer_instance

/-- Inclusion of `e` in `f`, checked once for every value of the variables (rule 3): every
capability `e` names is named by `f`, and every variable of `e` is a variable of `f`, unless `f`
names every capability. `sub_iff` proves this is exactly "`e` ⊆ `f` for every value of the
variables" (Q5.3, semantic equality). -/
def CapExpr.Sub (e f : CapExpr) : Prop :=
  (∀ c ∈ e.caps, c ∈ f.caps) ∧ (∀ v ∈ e.vars, v ∈ f.vars ∨ ∀ c ∈ Cap.all, c ∈ f.caps)

instance (e f : CapExpr) : Decidable (e.Sub f) := by unfold CapExpr.Sub; infer_instance

/-- Two set expressions are the same set when each is included in the other (Q5.3: canonical
semantic equality). By `sub_iff` this is "equal for every value of the variables": the order and
grouping of the names do not matter. `CapExpr`'s derived `=` compares the written form only. -/
def CapExpr.Equiv (e f : CapExpr) : Prop := e.Sub f ∧ f.Sub e

instance (e f : CapExpr) : Decidable (e.Equiv f) := by unfold CapExpr.Equiv; infer_instance

/-- The callee's set `e` at a call whose explicit capability arguments are `σ`: each variable `v`
is replaced by the argument `σ[v]` (Q5.4, substitution checking). -/
def CapExpr.subst (σ : List CapExpr) (e : CapExpr) : CapExpr :=
  ⟨e.caps ++ e.vars.flatMap (fun v => (σ.getD v ⟨[], []⟩).caps),
   e.vars.flatMap (fun v => (σ.getD v ⟨[], []⟩).vars)⟩

/-- The grant types of the hosted target (row Q7.10, R4 F6) that this part uses. -/
inductive GrantTy where
  | stdin | stdout | stderr | directory | allocator | clock | environment | connect
  deriving DecidableEq, Repr

/-- The primitive operations of this part, each on one grant. -/
inductive Op where
  | readStdin | writeStdout | writeStderr | openDir | alloc | now | readEnv | connect
  deriving DecidableEq, Repr

/-- The primitive effect table (rule 7), first column: the grant type each operation acts on. -/
def Op.grant : Op → GrantTy
  | .readStdin => .stdin
  | .writeStdout => .stdout
  | .writeStderr => .stderr
  | .openDir => .directory
  | .alloc => .allocator
  | .now => .clock
  | .readEnv => .environment
  | .connect => .connect

/-- The primitive effect table (rule 7), second column: the capabilities each operation needs.
The standard streams need `Console` (Q7.11). -/
def Op.caps : Op → List Cap
  | .readStdin => [.console]
  | .writeStdout => [.console]
  | .writeStderr => [.console]
  | .openDir => [.fs]
  | .alloc => [.alloc]
  | .now => [.time]
  | .readEnv => [.env]
  | .connect => [.network]

/-- A statement of this part. -/
inductive Stmt where
  /-- Operation `op` on the function's grant parameter at position `g`. -/
  | prim (op : Op) (g : Nat)
  /-- A call of `f` with explicit capability arguments `caps`, one for each capability parameter
  of `f` (Q5.1), and grant arguments `gs`, positions of the caller's grant parameters. -/
  | call (f : String) (caps : List CapExpr) (gs : List Nat)
  deriving DecidableEq, Repr

/-- A function: its capability parameters (a count), its grant parameters, its permissions
`with(...)`, whether it is `trusted`, and its body. -/
structure Fn where
  name : String
  capParams : Nat
  grants : List GrantTy
  perms : CapExpr
  trusted : Bool
  body : List Stmt
  deriving DecidableEq, Repr

abbrev Prog := List Fn

/-- The function named `f`. -/
def Prog.find (P : Prog) (f : String) : Option Fn := P.find? (fun g => g.name == f)

/-- An operation `op` on `f`'s grant parameter `g` is permitted: the parameter has the operation's
grant type, and `f`'s permissions include every capability the table gives the operation (rules 1,
2, 7). -/
def PrimOk (f : Fn) (op : Op) (g : Nat) : Prop :=
  f.grants[g]? = some op.grant ∧ CapExpr.Sub ⟨op.caps, []⟩ f.perms

instance (f : Fn) (op : Op) (g : Nat) : Decidable (PrimOk f op g) := by
  unfold PrimOk; infer_instance

/-- The arguments of a call from `f` to `callee` are permitted: exactly one well-formed capability
argument per capability parameter (Q5.1: none is inferred), grant arguments of the callee's
grant types, and the callee's permissions, with the arguments substituted, included in `f`'s
(Q5.4, TRUST-4). -/
def ArgsOk (f callee : Fn) (caps : List CapExpr) (gs : List Nat) : Prop :=
  caps.length = callee.capParams ∧ (∀ e ∈ caps, e.WellFormed f.capParams) ∧
    gs.map (fun i => f.grants[i]?) = callee.grants.map some ∧
    (callee.perms.subst caps).Sub f.perms

instance (f callee : Fn) (caps : List CapExpr) (gs : List Nat) :
    Decidable (ArgsOk f callee caps gs) := by
  unfold ArgsOk; infer_instance

/-- A call of `h` from `f` is permitted: `h` names a function, and the arguments are permitted. -/
def CallOk (P : Prog) (f : Fn) (h : String) (caps : List CapExpr) (gs : List Nat) : Prop :=
  ∃ callee, P.find h = some callee ∧ ArgsOk f callee caps gs

instance (P : Prog) (f : Fn) (h : String) (caps : List CapExpr) (gs : List Nat) :
    Decidable (CallOk P f h caps gs) :=
  match hf : P.find h with
  | none => isFalse (fun ⟨_, hc, _⟩ => by rw [hf] at hc; cases hc)
  | some c =>
    if hok : ArgsOk f c caps gs then isTrue ⟨c, hf, hok⟩
    else isFalse (fun ⟨_, hc, h'⟩ => by rw [hf] at hc; cases hc; exact hok h')

/-- A statement of `f` is permitted. Whether `f` is `trusted` plays no part (rule 5), and
`Unsafe` in `f`'s permissions satisfies no other capability. -/
def StmtOk (P : Prog) (f : Fn) : Stmt → Prop
  | .prim op g => PrimOk f op g
  | .call h caps gs => CallOk P f h caps gs

instance (P : Prog) (f : Fn) : (s : Stmt) → Decidable (StmtOk P f s)
  | .prim op g => inferInstanceAs (Decidable (PrimOk f op g))
  | .call h caps gs => inferInstanceAs (Decidable (CallOk P f h caps gs))

/-- A function is permitted: its own set is well formed and every statement is permitted. -/
def FnOk (P : Prog) (f : Fn) : Prop := f.perms.WellFormed f.capParams ∧ ∀ s ∈ f.body, StmtOk P f s

instance (P : Prog) (f : Fn) : Decidable (FnOk P f) := by unfold FnOk; infer_instance

/-- A program is permitted: function names are distinct, every function is permitted, and `main`
exists with no capability variables (rule 12: it names its permissions concretely). -/
def ProgOk (P : Prog) : Prop :=
  (P.map Fn.name).Nodup ∧ (∀ f ∈ P, FnOk P f) ∧
    ∃ m, P.find "main" = some m ∧ m.capParams = 0

instance (P : Prog) : Decidable (ProgOk P) := by
  unfold ProgOk
  have : Decidable (∃ m, P.find "main" = some m ∧ m.capParams = 0) :=
    match h : P.find "main" with
    | some m => if h0 : m.capParams = 0 then isTrue ⟨m, rfl, h0⟩
        else isFalse (fun ⟨m', h', h0'⟩ => by cases h'; exact h0 h0')
    | none => isFalse (fun ⟨m', h', _⟩ => by cases h')
  infer_instance

/-- What an outside observer sees of this part: an operation on a grant. The launcher numbers the
grants it mints. -/
structure Event where
  op : Op
  grant : Nat
  deriving DecidableEq, Repr

/-- What a run has produced so far: the events in order, and whether the run is complete. A run
that is not complete stopped early: it ran out of fuel, or met a missing grant or callee (which a
permitted program never meets). Its events are still observable, so the containment theorem
covers them too: it needs no termination premise. -/
structure Trace where
  events : List Event
  complete : Bool
  deriving DecidableEq, Repr

/-- Run a body whose grant parameters are bound, by position, to the launch grants `env`. Fuel
bounds the depth of the run, not its length: each step takes one unit, and a call runs its callee
and then the rest of the body each with the remaining fuel. Capabilities have no run-time representation: the run never
consults them. -/
def exec (P : Prog) : Nat → List Nat → List Stmt → Trace
  | 0, _, _ => ⟨[], false⟩
  | _ + 1, _, [] => ⟨[], true⟩
  | n + 1, env, .prim op g :: rest =>
    match env[g]? with
    | none => ⟨[], false⟩
    | some id =>
      let t := exec P n env rest
      ⟨⟨op, id⟩ :: t.events, t.complete⟩
  | n + 1, env, .call h _ gs :: rest =>
    match P.find h, gs.mapM (fun i => env[i]?) with
    | some callee, some env' =>
      let t₁ := exec P n env' callee.body
      if t₁.complete then
        let t₂ := exec P n env rest
        ⟨t₁.events ++ t₂.events, t₂.complete⟩
      else t₁
    | _, _ => ⟨[], false⟩

/-- Run `main` on the grants `launch` the launcher minted for its parameters. -/
def runMain (P : Prog) (fuel : Nat) (launch : List Nat) : Trace :=
  match P.find "main" with
  | some m => exec P fuel launch m.body
  | none => ⟨[], false⟩

/-! ### Examples -/

/-- `with(Console)`. -/
def console : CapExpr := ⟨[.console], []⟩

/-- Accepted: writing to standard output needs the `StdOut` grant and `Console`.
```
fn log(out: &StdOut) with(Console) { write(out, "hi"); }
``` -/
def logFn : Fn :=
  { name := "log", capParams := 0, grants := [.stdout], perms := console, trusted := false,
    body := [.prim .writeStdout 0] }

example : FnOk [logFn] logFn := by decide

/-- Refused: the grant without the permission (rule 1). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[], []⟩ } := by decide

/-- Refused: the permission without the grant (rule 2): `log` has no `StdIn`. -/
example : ¬ FnOk [logFn] { logFn with body := [.prim .readStdin 0] } := by decide

/-- Refused: `Unsafe` does not stand in for `Console`, and neither does `trusted` (rule 5). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[.«unsafe»], []⟩, trusted := true } := by decide

/-- Refused: `Device` is not the standard streams' permission (Q7.11). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[.device], []⟩ } := by decide

/-- Refused: a capability written twice (OD108). -/
example : ¬ FnOk [logFn] { logFn with perms := ⟨[.console, .console], []⟩ } := by decide

/-- A capability-polymorphic function: `C` is its capability variable `0`.
```
fn emit<cap C>(out: &StdOut) with(C, Console) { write(out, "x"); }
``` -/
def emitFn : Fn :=
  { name := "emit", capParams := 1, grants := [.stdout], perms := ⟨[.console], [0]⟩,
    trusted := false, body := [.prim .writeStdout 0] }

/-- `main` calls `emit` with the explicit capability argument `{}`.
```
fn main(out: StdOut) with(Console) -> u8 { emit<cap {}>(&out); log(&out); … }
``` -/
def mainFn : Fn :=
  { name := "main", capParams := 0, grants := [.stdout], perms := console, trusted := false,
    body := [.call "emit" [⟨[], []⟩] [0], .call "log" [] [0]] }

def prog : Prog := [mainFn, emitFn, logFn]

example : ProgOk prog := by decide

/-- Refused: `emit<cap {Fs}>` needs `Fs`, which `main` does not declare. -/
example : ¬ ProgOk [{ mainFn with body := [.call "emit" [⟨[.fs], []⟩] [0]] }, emitFn, logFn] := by
  decide

/-- Refused: a call without its capability argument; nothing is inferred (Q5.1). -/
example : ¬ ProgOk [{ mainFn with body := [.call "emit" [] [0]] }, emitFn, logFn] := by decide

/-- A function may forward its own capability variable (Q5.2): `relay<cap D>` calls
`emit<cap D>` and declares `D` and `Console`. -/
def relayFn : Fn :=
  { name := "relay", capParams := 1, grants := [.stdout], perms := ⟨[.console], [0]⟩,
    trusted := false, body := [.call "emit" [⟨[], [0]⟩] [0]] }

example : FnOk [emitFn, relayFn] relayFn := by decide

/-- Refused: `relay` may not drop `D` from its permissions while passing it on. -/
example : ¬ FnOk [emitFn, relayFn] { relayFn with perms := console } := by decide

/-- A run of `prog` on launch grant `7`: `emit` writes, then `log` writes, both to grant `7`. -/
example : runMain prog 10 [7] = ⟨[⟨.writeStdout, 7⟩, ⟨.writeStdout, 7⟩], true⟩ := by decide

/-- With too little fuel the run stops early, and the events so far are kept: `emit`'s write. -/
example : runMain prog 2 [7] = ⟨[⟨.writeStdout, 7⟩], false⟩ := by decide

/-- Set equality is semantic (Q5.3): the order of the names does not matter. -/
example : CapExpr.Equiv ⟨[.console, .fs], []⟩ ⟨[.fs, .console], []⟩ := by decide

/-- A variable is a set like any other: `with(C)` is not `with(C, Console)`. -/
example : ¬ CapExpr.Equiv ⟨[], [0]⟩ ⟨[.console], [0]⟩ := by decide

end ConcreteSpec.Authority

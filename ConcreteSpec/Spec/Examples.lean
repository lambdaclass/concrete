import ConcreteSpec.Diagnostics.Types
import ConcreteSpec.Spec.Slice1Checker
import ConcreteSpec.Spec.Rep
import ConcreteSpec.Spec.Observation

/-!
# Slice-1 conformance examples

PROPOSED: not normative until the owner freezes Core (ROADMAP A2c). Part of `ConcreteSpec.Spec`;
see `ConcreteSpec/README.md`.
-/

namespace ConcreteSpec.Examples
open ConcreteSpec

/-! ### Slice-1 conformance examples

Each example gives the surface program in its doc comment, its Core syntax, and what the
specification says about it: accepted (`check p = true`) with an outcome (`run p 10 = …`), or
refused (`check p = false`). By `check_iff` (`Proofs/Checker.lean`), `check p = true` is exactly
the rule `Checked p`, and `bodyOkB … = true` is exactly `BodyOk …`. Locals are written with their
number alone: `t₀` is `⟨0⟩`; source spellings belong in `LocalNames`. A refused program may still have a `run` example:
it shows what the refused code would do if it ran anyway. -/

/-- `struct Token {}` — empty, not `Copy`, so linear (OWN-D1). -/
def token : StructDecl := { name := "Token", copy := false, fields := [] }

/-- `struct Copy Unit0 {}` — empty and declared `Copy` (TYPE-14). -/
def unit0 : StructDecl := { name := "Unit0", copy := true, fields := [] }

/-- `struct Pair { a: u8, b: u8 }` — not `Copy`. -/
def pair : StructDecl := { name := "Pair", copy := false, fields := [("a", .int .u8), ("b", .int .u8)] }

def decls : Decls := [token, unit0, pair]

def u8 : Ty := .scalar (.int .u8)

def lit (n : Int) : Expr := .atom (.int .u8 n)

def mv (x : Var) : Expr := .atom (.move x)

def cp (x : Var) : Expr := .atom (.copy x)

/-- Local 0; for example, `⟨0⟩` denotes this numeric identity. -/
def t₀ : Var := ⟨0⟩

/-- Local 1; for example, `⟨1⟩` denotes this numeric identity. -/
def u₁ : Var := ⟨1⟩

/-- Local 4; for example, `⟨4⟩` denotes this numeric identity. -/
def p₄ : Var := ⟨4⟩

/-- Local 5; for example, `⟨5⟩` denotes this numeric identity. -/
def q₅ : Var := ⟨5⟩

/-- Local 0; for example, `⟨0⟩` denotes this numeric identity. -/
def x₀ : Var := ⟨0⟩

/-- Local 1; for example, `⟨1⟩` denotes this numeric identity. -/
def x₁ : Var := ⟨1⟩

/-- Local 6; for example, `⟨6⟩` denotes this numeric identity. -/
def y₆ : Var := ⟨6⟩

/-- Local 0; for example, `⟨0⟩` denotes this numeric identity. -/
def a₀ : Var := ⟨0⟩

/-- Local 1; for example, `⟨1⟩` denotes this numeric identity. -/
def b₁ : Var := ⟨1⟩

/-- Local 2; for example, `⟨2⟩` denotes this numeric identity. -/
def s₂ : Var := ⟨2⟩

/-- Local 3; for example, `⟨3⟩` denotes this numeric identity. -/
def z₃ : Var := ⟨3⟩

/-- The slice's central program: an empty linear struct is created, moved, and consumed by a
destructuring `let` (OWN-1, OWN-2, PAT-5). It exits with 42.
```
fn main() -> u8 {
    let t: Token = Token {};
    let u: Token = t;
    let Token {} = u;
    return 42;
}
``` -/
def moveThenConsume : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .let_ (.name u₁) (.struct "Token") (mv t₀),
                    .letStruct "Token" [] (mv u₁)],
    ret := .int .u8 42 }

example : check moveThenConsume = true := by decide

example : run moveThenConsume 10 = .exit 42 := by decide

/-- Refused: `t` is used after it was moved into `u` (OWN-1).
```
fn main() -> u8 {
    let t: Token = Token {};
    let u: Token = t;
    let Token {} = t;      // error: t was moved
    let Token {} = u;
    return 42;
}
``` -/
def useAfterMove : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .let_ (.name u₁) (.struct "Token") (mv t₀),
                    .letStruct "Token" [] (mv t₀),
                    .letStruct "Token" [] (mv u₁)],
    ret := .int .u8 42 }

example : check useAfterMove = false := by decide

/-- The refusal is what keeps the program from getting stuck: run anyway, the model reads a moved
slot. This is the behaviour `checked_no_use_after_move` rules out for accepted programs. -/
example : run useAfterMove 10 = .stuck .movedRead := by decide

/-- Refused: a linear value read with `copy` (Q9.1: a linear local is only moved). Run anyway, the
read is in the wrong mode. -/
def copyLinear : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" []),
                    .letStruct "Token" [] (cp t₀)],
    ret := .int .u8 42 }

example : check copyLinear = false := by decide

example : run copyLinear 10 = .stuck .wrongMode := by decide

/-- Refused: a Copy value read with `move` (Q9.1, owner-confirmed 2026-11-05: `move` only for
non-Copy types). Run anyway, the read is in the wrong mode. -/
def moveCopy : Program :=
  { decls, body := [.let_ (.name x₀) u8 (lit 7)],
    ret := .move x₀ }

example : check moveCopy = false := by decide

example : run moveCopy 10 = .stuck .wrongMode := by decide

/-- Refused: the linear `t` is never consumed before `return` (OWN-2).
```
fn main() -> u8 {
    let t: Token = Token {};
    return 42;             // error: t is not consumed
}
``` -/
def notConsumed : Program :=
  { decls, body := [.let_ (.name t₀) (.struct "Token") (.mk "Token" [])], ret := .int .u8 42 }

example : check notConsumed = false := by decide

/-- Accepted: a `Copy` struct is copied, not moved, so it can be read twice (OWN-D1, TYPE-14).
```
fn main() -> u8 {
    let p: Unit0 = Unit0 {};
    let q: Unit0 = p;
    let Unit0 {} = p;
    let Unit0 {} = q;
    return 0;
}
``` -/
def copyTwice : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Unit0") (.mk "Unit0" []),
                    .let_ (.name q₅) (.struct "Unit0") (cp p₄),
                    .letStruct "Unit0" [] (cp p₄),
                    .letStruct "Unit0" [] (cp q₅)],
    ret := .int .u8 0 }

example : check copyTwice = true := by decide

example : run copyTwice 10 = .exit 0 := by decide

/-- Refused: `x` is bound and never used (OWN-5).
```
fn main() -> u8 {
    let x: u8 = 1;         // error: x is never used
    return 42;
}
``` -/
def unusedBinding : Program :=
  { decls, body := [.let_ (.name x₀) u8 (lit 1)], ret := .int .u8 42 }

example : check unusedBinding = false := by decide

/-- Accepted: destructuring binds every field, here under new names (PAT-1, PAT-5), and checked
addition in range (ARITH-3). In A-normal form `return x + y` binds the sum to a temporary first.
Exits with 42.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 40, b: 2 };
    let Pair { a: x, b: y } = p;
    return x + y;
}
``` -/
def destructureAdd : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .int .u8 40), ("b", .int .u8 2)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₆)] (mv p₄),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₆))],
    ret := .copy s₂ }

example : check destructureAdd = true := by decide

example : run destructureAdd 10 = .exit 42 := by decide

/-- Refused: the pattern does not name field `b` (PAT-5).
```
fn main() -> u8 {
    let p: Pair = Pair { a: 40, b: 2 };
    let Pair { a: x } = p;   // error: field b is not named
    return x;
}
``` -/
def missingField : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .int .u8 40), ("b", .int .u8 2)]),
                    .letStruct "Pair" [("a", .name x₁)] (mv p₄)],
    ret := .copy x₁ }

example : check missingField = false := by decide

/-- Accepted with `_` for an ignored field (PAT-5; a scalar field is an eligible wildcard, OWN-5).
```
fn main() -> u8 {
    let p: Pair = Pair { a: 42, b: 0 };
    let Pair { a: x, b: _ } = p;
    return x;
}
``` -/
def wildcardField : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .int .u8 42), ("b", .int .u8 0)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .wild)] (mv p₄)],
    ret := .copy x₁ }

example : check wildcardField = true := by decide

example : run wildcardField 10 = .exit 42 := by decide

/-- Refused (Q6.1, Q6.2, Q6.4; ARITH-13, OD118): `a` and `b` are immutable `let`s of literals, so
they are statically known, and `a + b` is known to overflow `u8`. The refusal is justified: run
anyway, the addition traps.
```
fn main() -> u8 {
    let a: u8 = 200;
    let b: u8 = 100;
    return a + b;          // error: 200 + 100 overflows u8
}
``` -/
def overflowTrap : Program :=
  { decls, body := [.let_ (.name a₀) u8 (lit 200),
                    .let_ (.name b₁) u8 (lit 100),
                    .let_ (.name s₂) u8 (.add (.copy a₀) (.copy b₁))],
    ret := .copy s₂ }

example : check overflowTrap = false := by decide

example : run overflowTrap 10 = .trap .arithmeticOverflow := by decide

/-- Refused for the same reason in a position whose value is never used (Q6.4: all written code
is checked): the known overflow is dropped by `_`, and the program would otherwise exit 42.
```
fn main() -> u8 {
    let _: u8 = 200 + 100;   // error: 200 + 100 overflows u8
    return 42;
}
``` -/
def overflowDropped : Program :=
  { decls, body := [.let_ .wild u8 (.add (.int .u8 200) (.int .u8 100))], ret := .int .u8 42 }

example : check overflowDropped = false := by decide

/-- Refused (Q6.2 as amended 2026-11-05, Q6.4; OD118): known values flow through an immutable
struct. Constructing `p` records that field `a` is `200` and field `b` is `100`; destructuring
hands those facts to `x` and `y`, so `x + y` is known to overflow `u8`, exactly as for two plain
`let`s (`overflowTrap`). The refusal is justified: run anyway, the addition traps.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 200, b: 100 };
    let Pair { a: x, b: y } = p;
    return x + y;          // error: 200 + 100 overflows u8
}
``` -/
def fieldOverflowTrap : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 100)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₆)] (mv p₄),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₆))],
    ret := .copy s₂ }

example : check fieldOverflowTrap = false := by decide

example : run fieldOverflowTrap 10 = .trap .arithmeticOverflow := by decide

/-- Refused: an ownership transfer keeps the facts for the receiving binding (Q6.2 as amended).
`p` is moved into `q`, which may no longer read `p` (OWN-1), and `q` carries `p`'s field facts.
```
fn main() -> u8 {
    let p: Pair = Pair { a: 200, b: 100 };
    let q: Pair = p;
    let Pair { a: x, b: y } = q;
    return x + y;          // error: 200 + 100 overflows u8
}
``` -/
def fieldOverflowMoved : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 100)]),
                    .let_ (.name q₅) (.struct "Pair") (mv p₄),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₆)] (mv q₅),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₆))],
    ret := .copy s₂ }

example : check fieldOverflowMoved = false := by decide

example : run fieldOverflowMoved 10 = .trap .arithmeticOverflow := by decide

/-- Accepted with the facts in range: the known sum fits, and the program exits with it. The
known field values make no program with a fitting sum fail to check. -/
def fieldSumFits : Program :=
  { decls, body := [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .int .u8 200), ("b", .int .u8 55)]),
                    .letStruct "Pair" [("a", .name x₁), ("b", .name y₆)] (mv p₄),
                    .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₆))],
    ret := .copy s₂ }

example : check fieldSumFits = true := by decide

example : run fieldSumFits 10 = .exit 255 := by decide

/-- **An overflow from an input is refused** (no hidden traps, decision of 2026-11-06, Q1). The
checker does not know an input's value, so it bounds `x` by its type, `0 ≤ x ≤ 255`, and `x + 100`
may not fit: the addition has no evidence and is refused. Run anyway, it traps for `x > 155`. Slice 1 has no source of input: `main` takes no parameters (Q8.4), and an input reaches a
program through a capability (Q5), which slice 1 does not model. The witness is therefore a body
checked from one binding `x : u8` with no known value, as the body of a function with a `u8`
parameter would be:
```
fn add100(x: u8) -> u8 {
    return x + 100;        // error: x + 100 may overflow u8 (x ≤ 255)
}
```
In Core the sum is bound to a temporary first (Q9.1). The guarded forms that are accepted need
conditions, which slice 1 does not have; they come with Core `if` (ROADMAP R-0574). -/
def inputCtx : Ctx := [(x₀, { ty := u8, live := true, used := false, known := none })]

def inputEnv (n : Int) : Env := [(x₀, .val (.scalar (.int .u8 n)))]

def add100 : List Stmt := [.let_ (.name s₂) u8 (.add (.copy x₀) (.int .u8 100))]

example : bodyOkB decls inputCtx add100 (.copy s₂) = false := by decide

example : runBody decls 10 (inputEnv 200) add100 (.copy s₂) = .trap .arithmeticOverflow := by decide

example : runBody decls 10 (inputEnv 1) add100 (.copy s₂) = .exit 101 := by decide

/-- Accepted: the type bound of the input is evidence enough when the sum always fits.
```
fn add0(x: u8) -> u8 {
    return x + 0;          // accepted: 0 ≤ x + 0 ≤ 255
}
``` -/
def add0 : List Stmt := [.let_ (.name s₂) u8 (.add (.copy x₀) (.int .u8 0))]

example : bodyOkB decls inputCtx add0 (.copy s₂) = true := by decide

example : runBody decls 10 (inputEnv 255) add0 (.copy s₂) = .exit 255 := by decide

/-- **Partial facts** (Q6.2 as amended): an unknown field does not discard a known sibling. `p`'s
field `a` comes from the input `x` and is unknown; field `b` is `200` and is known. Adding `100` to
the known field is refused; adding the unknown field to the known one is refused too, since the
sum fits only for some inputs.
```
fn f(x: u8) -> u8 {
    let p: Pair = Pair { a: x, b: 200 };
    let Pair { a: c, b: d } = p;
    let s: u8 = d + 100;     // error: 200 + 100 overflows u8
    let z: u8 = c + s;
    return z;
}
``` -/
def partialKnownRefused : List Stmt :=
  [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .copy x₀), ("b", .int .u8 200)]),
   .letStruct "Pair" [("a", .name x₁), ("b", .name y₆)] (mv p₄),
   .let_ (.name s₂) u8 (.add (.copy y₆) (.int .u8 100)),
   .let_ (.name z₃) u8 (.add (.copy x₁) (.copy s₂))]

example : bodyOkB decls inputCtx partialKnownRefused (.copy z₃) = false := by decide

/-- Refused (no hidden traps): the unknown field is bounded by its type, `0 ≤ c ≤ 255`, and the
known one is `200`, so `c + d` may not fit. The known sibling is still used: it is the `200` in
the bound. Run anyway, the sum traps when `x > 55`.
```
fn f(x: u8) -> u8 {
    let p: Pair = Pair { a: x, b: 200 };
    let Pair { a: c, b: d } = p;
    return c + d;          // error: c + 200 may overflow u8 (c ≤ 255)
}
``` -/
def partialUnknownRefused : List Stmt :=
  [.let_ (.name p₄) (.struct "Pair") (.mk "Pair" [("a", .copy x₀), ("b", .int .u8 200)]),
   .letStruct "Pair" [("a", .name x₁), ("b", .name y₆)] (mv p₄),
   .let_ (.name s₂) u8 (.add (.copy x₁) (.copy y₆))]

example : bodyOkB decls inputCtx partialUnknownRefused (.copy s₂) = false := by decide

example : runBody decls 10 (inputEnv 55) partialUnknownRefused (.copy s₂) = .exit 255 := by decide

example : runBody decls 10 (inputEnv 56) partialUnknownRefused (.copy s₂) = .trap .arithmeticOverflow := by
  decide

/-- Refused: `300` does not fit `u8` (ARITH-2). -/
def literalTooBig : Program :=
  { decls, body := [.let_ (.name a₀) u8 (lit 300)], ret := .copy a₀ }

example : check literalTooBig = false := by decide

/-- Refused: `+` takes two operands of one integer type (TYPE-6).
```
fn main() -> u8 {
    let a: u8 = 1;
    let b: i64 = 2;
    return a + b;          // error: u8 + i64
}
``` -/
def mixedAdd : Program :=
  { decls, body := [.let_ (.name a₀) u8 (lit 1),
                    .let_ (.name b₁) (.scalar (.int .i64)) (.atom (.int .i64 2)),
                    .let_ (.name s₂) u8 (.add (.copy a₀) (.copy b₁))],
    ret := .copy s₂ }

example : check mixedAdd = false := by decide

/-- Refused: one identity bound twice (Q9.1: every Core binder is fresh). Elaboration gives two
surface bindings of one name different identities, so this arises only from a faulty elaborator;
the surface rule against rebinding (SCOPE-1) is an elaboration rule. -/
def rebind : Program :=
  { decls, body := [.let_ (.name x₀) u8 (lit 1),
                    .let_ (.name x₀) u8 (lit 41)],
    ret := .copy x₀ }

example : check rebind = false := by decide

/-- Accepted: `_` drops a Copy value (OWN-5).
```
fn main() -> u8 {
    let _: u8 = 1;
    return 42;
}
``` -/
def wildCopy : Program :=
  { decls, body := [.let_ .wild u8 (lit 1)], ret := .int .u8 42 }

example : check wildCopy = true := by decide

example : run wildCopy 10 = .exit 42 := by decide

/-- Refused: `_` never discards a linear value (OWN-5).
```
fn main() -> u8 {
    let _: Token = Token {};   // error: a linear value is discarded
    return 42;
}
``` -/
def wildLinear : Program :=
  { decls, body := [.let_ .wild (.struct "Token") (.mk "Token" [])], ret := .int .u8 42 }

example : check wildLinear = false := by decide

/-- Refused: `main` returns `u8` (STMT-7). -/
def wrongReturnType : Program :=
  { decls, body := [], ret := .int .i64 42 }

example : check wrongReturnType = false := by decide

/-! ### Representation and observations of the examples -/

/-- `Token` has no bytes, and decoding no bytes at type `Token` gives the empty struct (Part 2). -/
example : Rep.encodeVal (.struct "Token" []) = [] ∧
    Rep.decodeVal decls (.struct "Token") [] = some (.struct "Token" []) := by decide

/-- `moveThenConsume` exits with status 42 (Part 7). -/
example : (Observe.observe true (run moveThenConsume 10)).map Observe.Observation.status = some 42 := by
  decide

/-- The refused body `add100`, run anyway on input 200, traps: the trap message goes to descriptor
2 and the status is 134. -/
example : Observe.observe true (runBody decls 10 (inputEnv 200) add100 (.copy s₂)) =
    some (.trapped .arithmeticOverflow) := by decide

/-- A missing configured grant: status 125, and `main` does not run. -/
example : (Observe.observe false (run moveThenConsume 10)).map Observe.Observation.status =
    some 125 := by decide

/-- The identities bound by a pattern; for example, `.name ⟨7⟩` binds local 7. -/
def binderLocals : Binder → List Var
  | .name x => [x]
  | .wild => []

/-- All statement binders, including destructuring; for example, a wildcard adds none. -/
def bodyLocals (ss : List Stmt) : List Var := ss.flatMap fun st =>
  match st with
  | .let_ b _ _ => binderLocals b
  | .letStruct _ pat _ => pat.flatMap fun (_, b) => binderLocals b

/-- Every displayed number is bound in this program; for example, an empty table is valid. -/
def namesBound (names : LocalNames) (bound : List Var) : Bool :=
  names.entries.all fun (x, _) => bound.contains x

/-- Elaboration output for `moveThenConsume`; spellings are scoped to this program. For example, `t₀` displays as `t`. -/
def moveThenConsumeElaborated : Elaborated Program :=
  ⟨moveThenConsume, ⟨[(t₀, "t"), (u₁, "u")]⟩⟩

example : namesBound moveThenConsumeElaborated.names (bodyLocals moveThenConsumeElaborated.code.body) = true := by decide

/-- Elaboration output for `useAfterMove`; spellings are scoped to this program. For example, `t₀` displays as `t`. -/
def useAfterMoveElaborated : Elaborated Program :=
  ⟨useAfterMove, ⟨[(t₀, "t"), (u₁, "u")]⟩⟩

example : namesBound useAfterMoveElaborated.names (bodyLocals useAfterMoveElaborated.code.body) = true := by decide

/-- Elaboration output for `copyLinear`; spellings are scoped to this program. For example, `t₀` displays as `t`. -/
def copyLinearElaborated : Elaborated Program :=
  ⟨copyLinear, ⟨[(t₀, "t")]⟩⟩

example : namesBound copyLinearElaborated.names (bodyLocals copyLinearElaborated.code.body) = true := by decide

/-- Elaboration output for `moveCopy`; spellings are scoped to this program. For example, `x₀` displays as `x`. -/
def moveCopyElaborated : Elaborated Program :=
  ⟨moveCopy, ⟨[(x₀, "x")]⟩⟩

example : namesBound moveCopyElaborated.names (bodyLocals moveCopyElaborated.code.body) = true := by decide

/-- Elaboration output for `notConsumed`; spellings are scoped to this program. For example, `t₀` displays as `t`. -/
def notConsumedElaborated : Elaborated Program :=
  ⟨notConsumed, ⟨[(t₀, "t")]⟩⟩

example : namesBound notConsumedElaborated.names (bodyLocals notConsumedElaborated.code.body) = true := by decide

/-- Elaboration output for `copyTwice`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def copyTwiceElaborated : Elaborated Program :=
  ⟨copyTwice, ⟨[(p₄, "p"), (q₅, "q")]⟩⟩

example : namesBound copyTwiceElaborated.names (bodyLocals copyTwiceElaborated.code.body) = true := by decide

/-- Elaboration output for `unusedBinding`; spellings are scoped to this program. For example, `x₀` displays as `x`. -/
def unusedBindingElaborated : Elaborated Program :=
  ⟨unusedBinding, ⟨[(x₀, "x")]⟩⟩

example : namesBound unusedBindingElaborated.names (bodyLocals unusedBindingElaborated.code.body) = true := by decide

/-- Elaboration output for `destructureAdd`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def destructureAddElaborated : Elaborated Program :=
  ⟨destructureAdd, ⟨[(p₄, "p"), (x₁, "x"), (y₆, "y"), (s₂, "s")]⟩⟩

example : namesBound destructureAddElaborated.names (bodyLocals destructureAddElaborated.code.body) = true := by decide

/-- Elaboration output for `missingField`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def missingFieldElaborated : Elaborated Program :=
  ⟨missingField, ⟨[(p₄, "p"), (x₁, "x")]⟩⟩

example : namesBound missingFieldElaborated.names (bodyLocals missingFieldElaborated.code.body) = true := by decide

/-- Elaboration output for `wildcardField`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def wildcardFieldElaborated : Elaborated Program :=
  ⟨wildcardField, ⟨[(p₄, "p"), (x₁, "x")]⟩⟩

example : namesBound wildcardFieldElaborated.names (bodyLocals wildcardFieldElaborated.code.body) = true := by decide

/-- Elaboration output for `overflowTrap`; spellings are scoped to this program. For example, `a₀` displays as `a`. -/
def overflowTrapElaborated : Elaborated Program :=
  ⟨overflowTrap, ⟨[(a₀, "a"), (b₁, "b"), (s₂, "s")]⟩⟩

example : namesBound overflowTrapElaborated.names (bodyLocals overflowTrapElaborated.code.body) = true := by decide

/-- Elaboration output for `overflowDropped`; spellings are scoped to this program. For example, this table has no named locals. -/
def overflowDroppedElaborated : Elaborated Program :=
  ⟨overflowDropped, ⟨[]⟩⟩

example : namesBound overflowDroppedElaborated.names (bodyLocals overflowDroppedElaborated.code.body) = true := by decide

/-- Elaboration output for `fieldOverflowTrap`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def fieldOverflowTrapElaborated : Elaborated Program :=
  ⟨fieldOverflowTrap, ⟨[(p₄, "p"), (x₁, "x"), (y₆, "y"), (s₂, "s")]⟩⟩

example : namesBound fieldOverflowTrapElaborated.names (bodyLocals fieldOverflowTrapElaborated.code.body) = true := by decide

/-- Elaboration output for `fieldOverflowMoved`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def fieldOverflowMovedElaborated : Elaborated Program :=
  ⟨fieldOverflowMoved, ⟨[(p₄, "p"), (q₅, "q"), (x₁, "x"), (y₆, "y"), (s₂, "s")]⟩⟩

example : namesBound fieldOverflowMovedElaborated.names (bodyLocals fieldOverflowMovedElaborated.code.body) = true := by decide

/-- Elaboration output for `fieldSumFits`; spellings are scoped to this program. For example, `p₄` displays as `p`. -/
def fieldSumFitsElaborated : Elaborated Program :=
  ⟨fieldSumFits, ⟨[(p₄, "p"), (x₁, "x"), (y₆, "y"), (s₂, "s")]⟩⟩

example : namesBound fieldSumFitsElaborated.names (bodyLocals fieldSumFitsElaborated.code.body) = true := by decide

/-- Elaboration output for `literalTooBig`; spellings are scoped to this program. For example, `a₀` displays as `a`. -/
def literalTooBigElaborated : Elaborated Program :=
  ⟨literalTooBig, ⟨[(a₀, "a")]⟩⟩

example : namesBound literalTooBigElaborated.names (bodyLocals literalTooBigElaborated.code.body) = true := by decide

/-- Elaboration output for `mixedAdd`; spellings are scoped to this program. For example, `a₀` displays as `a`. -/
def mixedAddElaborated : Elaborated Program :=
  ⟨mixedAdd, ⟨[(a₀, "a"), (b₁, "b"), (s₂, "s")]⟩⟩

example : namesBound mixedAddElaborated.names (bodyLocals mixedAddElaborated.code.body) = true := by decide

/-- Elaboration output for `rebind`; spellings are scoped to this program. For example, `x₀` displays as `x`. -/
def rebindElaborated : Elaborated Program :=
  ⟨rebind, ⟨[(x₀, "x")]⟩⟩

example : namesBound rebindElaborated.names (bodyLocals rebindElaborated.code.body) = true := by decide

/-- Elaboration output for `wildCopy`; spellings are scoped to this program. For example, this table has no named locals. -/
def wildCopyElaborated : Elaborated Program :=
  ⟨wildCopy, ⟨[]⟩⟩

example : namesBound wildCopyElaborated.names (bodyLocals wildCopyElaborated.code.body) = true := by decide

/-- Elaboration output for `wildLinear`; spellings are scoped to this program. For example, this table has no named locals. -/
def wildLinearElaborated : Elaborated Program :=
  ⟨wildLinear, ⟨[]⟩⟩

example : namesBound wildLinearElaborated.names (bodyLocals wildLinearElaborated.code.body) = true := by decide

/-- Elaboration output for `wrongReturnType`; spellings are scoped to this program. For example, this table has no named locals. -/
def wrongReturnTypeElaborated : Elaborated Program :=
  ⟨wrongReturnType, ⟨[]⟩⟩

example : namesBound wrongReturnTypeElaborated.names (bodyLocals wrongReturnTypeElaborated.code.body) = true := by decide

/-- Elaboration output for the `add100` function body, including its input and result. For example, `x₀` displays as `x`. -/
def add100Elaborated : Elaborated (Ctx × List Stmt × Atom) :=
  ⟨(inputCtx, add100, .copy s₂), ⟨[(x₀, "x"), (s₂, "s")]⟩⟩

example : namesBound add100Elaborated.names (List.map Prod.fst add100Elaborated.code.1 ++ bodyLocals add100Elaborated.code.2.1) = true := by decide

/-- Elaboration output for the `add0` function body. For example, `x₀` displays as `x`. -/
def add0Elaborated : Elaborated (Ctx × List Stmt × Atom) :=
  ⟨(inputCtx, add0, .copy s₂), ⟨[(x₀, "x"), (s₂, "s")]⟩⟩

example : namesBound add0Elaborated.names (List.map Prod.fst add0Elaborated.code.1 ++ bodyLocals add0Elaborated.code.2.1) = true := by decide

/-- In this function the locals numbered 1 and 6 spell `c` and `d`, as in its surface example. For example, `x₀` displays as `x`. -/
def partialKnownRefusedElaborated : Elaborated (Ctx × List Stmt × Atom) :=
  ⟨(inputCtx, partialKnownRefused, .copy z₃),
    ⟨[(x₀, "x"), (p₄, "p"), (x₁, "c"), (y₆, "d"), (s₂, "s"), (z₃, "z")]⟩⟩

example : namesBound partialKnownRefusedElaborated.names (List.map Prod.fst partialKnownRefusedElaborated.code.1 ++ bodyLocals partialKnownRefusedElaborated.code.2.1) = true := by decide

/-- The same function-scoped spellings for the second partial-facts example. For example, `x₀` displays as `x`. -/
def partialUnknownRefusedElaborated : Elaborated (Ctx × List Stmt × Atom) :=
  ⟨(inputCtx, partialUnknownRefused, .copy s₂),
    ⟨[(x₀, "x"), (p₄, "p"), (x₁, "c"), (y₆, "d"), (s₂, "s")]⟩⟩

example : namesBound partialUnknownRefusedElaborated.names (List.map Prod.fst partialUnknownRefusedElaborated.code.1 ++ bodyLocals partialUnknownRefusedElaborated.code.2.1) = true := by decide

/-- A stale table naming unbound local 999 is refused, even for a valid program. -/
example : namesBound ⟨[(⟨999⟩, "stale")]⟩ (bodyLocals moveThenConsume.body) = false := by decide

end ConcreteSpec.Examples

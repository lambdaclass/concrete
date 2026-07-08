# Constructor coverage — the walker checklist

The compiler is a stack of independent recursive walkers over two expression
trees:

- **surface `Expr`** (`Concrete/Frontend/AST.lean`) — what the parser produces;
- **core `CExpr`** (`Concrete/Elab/Core.lean`) — what elaboration produces and
  everything downstream consumes.

Each walker matches on these constructors, and most use a wildcard `| _ =>`
somewhere. The standing risk: you add a constructor, teach the parser and a
couple of passes about it, and **one** walker silently swallows it through a
wildcard — the miss surfaces much later as a wrong answer, not a build error.

`scripts/tests/check_constructor_coverage.sh` turns "every walker that must
handle a constructor does" into a mechanically-checked, self-maintaining
invariant. It reads the constructor lists **from the source** (so a new
constructor is picked up automatically) and asserts each one appears explicitly
(`.ctor`) in every required pass. Adding a constructor without teaching a walker
fails CI.

## The desugaring boundary

Four surface forms never reach Core — Resolve/Elab rewrite them away — so they
exist as `Expr` but not `CExpr`, and are checked only against the frontend
passes:

| surface-only `Expr` | rewritten to | where |
|---|---|---|
| `methodCall` | `call` (resolved receiver + args) | Resolve/Elab |
| `staticMethodCall` | `call` | Resolve/Elab |
| `arrowAccess` (`p->f`) | `deref` + `fieldAccess` | Elab |
| `paren` | inner expression | Elab |

This is why the two matrices below are checked against different constructor
lists — it is a property, not an omission.

## Required coverage

**Surface `Expr`** — every constructor must be handled by:

- checker — `Concrete/Check/Check.lean`
- elaborator — `Concrete/Elab/Elab.lean`
- formatter — `Concrete/Frontend/Format.lean`

**Core `CExpr`** — every constructor must be handled by:

- lowering — `Concrete/IR/Lower.lean`
- monomorphizer — `Concrete/IR/Mono.lean`
- core-checker — `Concrete/Check/CoreCheck.lean`
- interpreter — `Concrete/Interp/Interp.lean`
- proof extraction — `Concrete/Proof/ProofCore.lean`

## Adding a constructor

1. Add it to `Expr` (and `CExpr`, unless it is desugared before Core).
2. Teach every pass above — the gate names any pass you miss.
3. If it is surface-only, add the desugaring and record it in the table above.
4. Add an interp-vs-compiled test (or a documented compile-only reason).

## What this gate does NOT check

Presence of a `.ctor` arm, not its *correctness* — a wrong-but-present arm still
passes here and is the job of the differential tests. It also does not cover
statement (`Stmt`/`CStmt`) or type (`Ty`) constructors yet; extend the same
pattern if a walker miss ever bites there.

# Concrete Style Guide

Status: guidance — ROADMAP Phase 6 #16.

Mechanical layout (indentation, spacing, wrapping, trailing commas) is **not a
matter of taste in Concrete**: it is owned by `concrete fmt`. Run it; do not hand-
format. This guide covers what `fmt` does *not* decide — naming, structure, and
the idioms that keep code readable and audit-friendly. It is advisory; the only
mechanically-enforced part is `concrete fmt` (gated by `check_concrete_fmt.sh`).

## Formatting is `concrete fmt`'s job

- `concrete fmt <file>` (stdout), `--write` (in place), `--check` (CI), `--stdin`.
- Formatting is **semantics-preserving**: `--report fingerprints` is byte-identical
  before and after, so `fmt` never invalidates proof links or manifests.
- Don't argue about brace placement or wrapping in review — `fmt --check` settles it.

## Naming

- **Types** (`struct`, `enum`, newtype, alias): `UpperCamelCase` — `Digest`, `TcpConn`.
- **Functions, variables, fields, modules**: `lower_snake_case` — `read_u32_be`, `block_count`.
- **Enum variants**: `UpperCamelCase` — `Result.Ok`, `State.Ready`.
- **Compile-time / target facts**: `SCREAMING_SNAKE_CASE` — `CONCRETE_OS`.
- Prefer names that state *units and intent*: `len_bytes`, `timeout_ms`,
  `off_in_block` — not bare `len`, `t`, `i` for anything non-local. (Concrete has
  no units-of-measure types; the name is the contract — see ROADMAP #15.)

## Functions

- Keep bodies flat: prefer early `return` / `?` over deep nesting. Extraction into
  `ProvableV1` favors flat, non-nested control flow, so flat code is also more
  provable.
- One responsibility per function; push branching into small helpers.
- Make capabilities visible and minimal: a function takes `with(Console)` only if
  it truly needs it. Narrow capability sets are easier to audit and to prove.

## Modules

- One concept per module; expose the minimum (`pub`) surface.
- Order within a module: types → constants → public API → private helpers → tests.
- Use import aliases / qualified names (ROADMAP #2a) to avoid renaming on name
  clashes rather than inventing awkward local names.

## Pattern matching and error handling

- Prefer exhaustive `match` over chains of `if`; let the non-exhaustiveness error
  catch missing cases.
- Use `Result`/`Option` and `?` for fallible paths; do not encode errors as
  sentinel integers (the checked-arithmetic model will trap on a sentinel that
  overflows — model the error explicitly).
- Keep match arms small; lift complex arm bodies into named helpers so each arm
  reads as one intention.

## Arithmetic (see ARITHMETIC_POLICY.md)

- `a + b` means *checked*: overflow is a bug. Write `wrapping_*` when you intend
  modular arithmetic and `saturating_*` when you intend clamping — the spelling is
  the documentation. A reviewer should never need the build profile to know
  whether an operation wraps.
- `--report arithmetic` shows every site's class; use it to audit a module.

## Contracts and proof-bearing code

- Put `#[requires]`/`#[ensures]` directly above the function; keep clauses small
  and total. A precondition that is impossible to violate is a smell.
- Keep proof-carrying functions in `ProvableV1` shape: flat control flow, no
  nested loops, explicit modular arithmetic where the spec is modular (e.g.
  `wrapping_add` in SHA-256), so extraction and refinement stay stable.
- Name the spec and proof attributes explicitly (`#[spec(...)]`,
  `#[proof_by(...)]`); a formatting-only change must not alter fingerprints, and
  `fmt` guarantees that.

## Examples

- Every example under `examples/` must be classified in
  `scripts/tests/example_manifest.txt` in the same commit it is added.
- Examples are documentation: prefer clarity over cleverness, comment the
  *why*, and keep them runnable and gated.

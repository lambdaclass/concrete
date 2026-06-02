# Concurrency Formal Model

**Status:** Open research direction
**Affects:** language semantics, type system, structured concurrency, Lean mechanization, evidence reports
**Date:** 2026-05-01

## Purpose

This note specifies the formal-model fragment for the structured-concurrency direction. The umbrella direction is in [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md); the worked evidence example is in [concurrency-evidence-example.md](concurrency-evidence-example.md). This note defines the formal grounding that lets evidence claims like "no missing-concurrency deadlock — enforced" carry weight.

The mechanization target is Lean 4, alongside the existing core formalization. The point is that no other production language designing async I/O has shipped a mechanized model of its concurrency fragment. Doing so is a credible move that fits Concrete's audit positioning.

## Scope

This note covers:

1. Syntax of the concurrency fragment.
2. Operational semantics for scope, spawn, join, cancel, race.
3. Typing rules for capabilities and linear handles.
4. Theorems the model should support.
5. Mechanization plan in Lean.

It does not cover:

1. Channel semantics in detail (see [../stdlib-runtime/channel-model.md](../stdlib-runtime/channel-model.md)); the model treats channels as a primitive sync object with a small algebra.
2. Cancellation across FFI (see [../stdlib-runtime/ffi-cancellation-boundary.md](../stdlib-runtime/ffi-cancellation-boundary.md)).
3. Per-task stack analysis (see [../predictable-execution/concurrent-stack-analysis.md](../predictable-execution/concurrent-stack-analysis.md)).
4. Capability polymorphism elaboration (see [../language/capability-polymorphism.md](../language/capability-polymorphism.md)); this note assumes capability sets are checkable.

## Syntax

The concurrency fragment extends the core expression language with:

```
e ::= ... core expressions ...
    | scope x with(C) { e }                  scope introduction
    | x.spawn(e)                             spawn task in scope
    | x.spawn_handle(e)                      spawn task and bind handle
    | x.join(h)                              wait for handle, return result
    | x.cancel(h)                            cancel handle, wait, return result
    | x.race(h_1, h_2)                       race two handles

v ::= ... core values ...
    | scope-token(σ, C)                      runtime scope value
    | handle(σ, t)                           handle into scope σ for task t
```

`σ` ranges over scope identifiers, `t` over task identifiers, `C` over capability sets.

The syntactic distinction between `spawn` (no handle) and `spawn_handle` (with handle) reflects the surface API. Internally, both produce a task identifier; the difference is whether that identifier is bound to a user-visible linear handle or kept private to the scope.

## Runtime Configurations

A configuration during execution:

```
⟨T; S; e⟩
  T : task pool, mapping task id t to its current expression e_t
  S : scope state, mapping scope id σ to (parent task, capability C, child set, status)
  e : the expression of the currently focused task
```

Scope status is one of:

- `Active`: scope is open, accepting spawns
- `Cancelling`: scope received an error or cancel request; remaining tasks must terminate
- `Closed`: scope has exited; no further operations valid

## Operational Semantics

A subset of the reduction rules. Full elaboration is in the mechanization.

**Scope introduction.** Entering a scope creates a fresh scope identifier and an empty child set.

```
σ fresh    S' = S[σ ↦ (current_task, C, ∅, Active)]
─────────────────────────────────────────────────────────
⟨T; S; scope x with(C) { e }⟩  →  ⟨T; S'; e[x := scope-token(σ, C)]⟩
```

**Spawn.** Spawning a task adds it to the scope's child set and the task pool.

```
S(σ) = (parent, C, children, Active)    t fresh    children' = children ∪ {t}
─────────────────────────────────────────────────────────────────────────────
⟨T; S; x.spawn(e_child)⟩
  →
⟨T[t ↦ e_child]; S[σ ↦ (parent, C, children', Active)]; ()⟩

where x = scope-token(σ, C)
```

**Spawn with handle.** Same as above, but produces a handle as the value of the spawn expression.

```
S(σ) = (parent, C, children, Active)    t fresh    children' = children ∪ {t}
─────────────────────────────────────────────────────────────────────────────
⟨T; S; x.spawn_handle(e_child)⟩
  →
⟨T[t ↦ e_child]; S[σ ↦ (parent, C, children', Active)]; handle(σ, t)⟩
```

**Join successful.** If the target task has reduced to a value, the join consumes the handle and returns the value.

```
T(t) = v    h = handle(σ, t)
─────────────────────────────────────────────────────────────
⟨T; S; x.join(h)⟩  →  ⟨T \ {t}; S[σ remove t from children]; v⟩
```

**Join with error propagation.** If the target task reduced to an error, the parent transitions the scope to Cancelling.

```
T(t) = err(e)    h = handle(σ, t)
─────────────────────────────────────────────────────────────
⟨T; S; x.join(h)⟩
  →
⟨T \ {t}; S[σ ↦ (parent, C, children \ {t}, Cancelling)]; err(e)⟩
```

**Cancel.** Cancellation requests termination of the target task. The target is given a chance to reach a checkpoint; once it does, cancel returns its result (or the cancellation outcome).

```
T(t) = v    h = handle(σ, t)
─────────────────────────────────────────────────────────────
⟨T; S; x.cancel(h)⟩  →  ⟨T \ {t}; S[σ remove t from children]; v⟩

T(t) = paused-at-checkpoint    h = handle(σ, t)
─────────────────────────────────────────────────────────────
⟨T; S; x.cancel(h)⟩  →  ⟨T \ {t}; S[σ remove t from children]; err(Cancelled)⟩
```

The "paused-at-checkpoint" state is the runtime's representation of cooperative cancellation. Cancellation only takes effect at a checkpoint; the formal model treats checkpoints as a primitive.

**Scope exit.** Scope exit waits for all children to terminate. If the scope is in Cancelling state, remaining children are sent cancellation requests.

```
S(σ) = (parent, C, ∅, Active)    parent = current_task
─────────────────────────────────────────────────────────────
⟨T; S; close-scope(σ, v)⟩  →  ⟨T; S[σ ↦ Closed]; v⟩

S(σ) = (parent, C, ∅, Cancelling)    parent = current_task
─────────────────────────────────────────────────────────────
⟨T; S; close-scope(σ, err(e))⟩  →  ⟨T; S[σ ↦ Closed]; err(e)⟩
```

The `close-scope` is implicit at the end of a `scope ... { e }` expression. The reduction blocks until the child set is empty.

**Race.** A race over two handles waits until at least one is ready, returns its result, and produces a fresh handle for the loser.

```
T(t_1) = v_1    h_1 = handle(σ, t_1)    h_2 = handle(σ, t_2)
──────────────────────────────────────────────────────────────────────────────
⟨T; S; x.race(h_1, h_2)⟩
  →
⟨T \ {t_1}; S[σ remove t_1]; First(v_1, handle(σ, t_2))⟩
```

(Symmetric rule for the other side.)

## Typing Judgment

Typing is parameterized by a capability context Γ_C in addition to the usual term context Γ.

```
Γ; Γ_C ⊢ e : τ ! C
```

reads as "expression e has type τ and requires capability set C in context Γ; Γ_C".

The capability context Γ_C lists capabilities the surrounding function holds. The required set C must be a subset (after lattice expansion) of Γ_C.

**Important framing.** Γ_C is a static permission context, not a handler stack. Capabilities in Concrete are gates that are checked at the call site; they are never handled, transformed, or reified at the term level. A reader coming from Koka, Effekt, or OCaml 5 should resist the urge to interpret Γ_C as effect handlers in scope. There are none. The only operations on Γ_C are: extend (when entering a scope that introduces capabilities), check subset (at call sites), and expand through the lattice. There is no resume, no continue, no replay. See [../language/capability-polymorphism.md](../language/capability-polymorphism.md) for the broader framing.

**Scope rule.**

```
Γ; Γ_C ⊢ C' valid    C' ⊆ Γ_C
Γ, x : Scope<C'>; Γ_C ⊢ e : τ ! C''    C'' ⊆ C'
─────────────────────────────────────────────────────────────
Γ; Γ_C ⊢ scope x with(C') { e } : τ ! ∅
```

Reading: a scope with capability set C' requires the surrounding context to hold C'. Inside the scope, the body may use any capabilities in C'. The scope expression itself, from the outside, is pure (the inside capabilities do not leak past the scope boundary).

This is the lexical containment rule. Capabilities introduced by a scope are bounded by that scope.

**Spawn rule.**

```
Γ; Γ_C ⊢ x : Scope<C'>
Γ; Γ_C ⊢ e : τ ! C''    C'' ⊆ C'
─────────────────────────────────────────────────────────────
Γ; Γ_C ⊢ x.spawn(e) : Unit ! ∅
```

A spawned task may use capabilities only from the scope's set. The scope is the capability gate.

**Spawn-handle rule.** Same as spawn, but the result is a linear handle:

```
Γ; Γ_C ⊢ x.spawn_handle(e) : Handle<τ, ∅> ! ∅
                              ── (linear)
```

Handles must be consumed by join, cancel, or race before the scope closes. The scope-close rule below enforces this.

**Scope close (implicit, structural).**

```
At scope close, the multiset of unconsumed handles introduced inside the scope must be empty.
This is checked as a linearity invariant on the typing derivation, analogous to the linearity check
on owned values at function exit.
```

A handle that escapes a scope unconsumed is a typing error. This is the no-leaked-tasks property promoted to a static check.

**Capability lattice subsumption.**

```
Γ; Γ_C ⊢ e : τ ! C'    C' ⊆ C
─────────────────────────────────────
Γ; Γ_C ⊢ e : τ ! C
```

Required capabilities can be widened along the lattice (e.g., `with(Async)` is weakened to `with(Concurrent)` if needed by the surrounding context). The reverse is not allowed.

## Theorems

The mechanization should support at least the following:

**T1: Capability containment.**
*If `Γ; Γ_C ⊢ e : τ ! C` and `e → e'`, then `Γ; Γ_C ⊢ e' : τ ! C` and the capabilities used by `e'` are still contained by `C`.*

This is the standard preservation theorem extended to capability sets.

**T2: Handle linearity.**
*A well-typed program never leaves a handle unconsumed at scope close.*

Follows from the linearity discipline applied to handles. This is the no-leaked-tasks property.

**T3: Scope containment.**
*A child task's reduction sequence cannot produce a handle visible outside its parent scope.*

Follows from scope tokens being introduced fresh per scope. Handles refer to a scope identifier, and the typing rules prevent handles from being returned past a scope boundary.

**T4: Async-only programs cannot deadlock from missing concurrency.**
*If `Γ; ∅ ⊢ e : τ ! C` and `Concurrent ∉ C`, then any reduction sequence of `e` either terminates or blocks only on external I/O — never on internal scheduler dependencies.*

This is the static deadlock-freedom theorem for the `Async`-only fragment. It is the formal grounding for the "no missing-concurrency deadlock — enforced" evidence claim.

The proof relies on the fact that `Async` reductions are valid sequentially; if no progress requires concurrent execution, a sequential schedule exists.

**T5: Cancellation termination.**
*If a scope enters Cancelling state and all child tasks reach a checkpoint within bounded steps, the scope reaches Closed within bounded steps.*

Stronger than the basic operational semantics needs, but desired for predictable-execution use. The "bounded steps" condition is the reason long-running CPU code must declare `with(Cancellable)` and contain checkpoints.

**T6: Race linearity.**
*A `race` operation consumes both input handles and produces exactly one fresh handle for the loser. The loser's handle is the same task as the input loser.*

Ensures race does not duplicate or drop tasks.

## Mechanization Plan

The Lean 4 development should be staged.

**Stage A: definitions and statements.**
Define the syntax, operational semantics, and typing rules in Lean. Write the theorem statements without proofs. This alone is valuable: it forces precision and produces an artifact that can be cited and reviewed.

**Stage B: progress and preservation.**
Prove T1 (capability containment) and basic preservation. This is the most standard piece and should not be hard given Lean's existing core formalization.

**Stage C: linearity properties.**
Prove T2 (handle linearity) and T3 (scope containment). These rely on linearity tracking that Concrete already has in some form for value linearity; the extension to handles should be analogous.

**Stage D: deadlock-freedom theorem.**
Prove T4 (async-only deadlock freedom). This is the interesting theorem. It requires defining what "deadlock" means in the operational model and showing that the absence of `Concurrent` rules out the deadlock-causing reductions.

**Stage E: cancellation properties.**
Prove T5 (cancellation termination) under appropriate checkpoint assumptions. This is conditional on the cooperative cancellation discipline; the theorem statement should be honest about its preconditions.

**Stage F: race properties.**
Prove T6 (race linearity).

Stages A and B should ship before any concurrency surface lands in the user-facing language. Stages C-F can follow as the implementation matures.

## What This Replaces

Languages that ship async without a formal model rely on:

- Convention and documentation, which drift over time.
- Test suites, which exercise specific schedules but not all.
- Library implementations that may or may not match the documented model.
- Surprising bugs in features like Pin, async-fn-in-trait, structured concurrency cancellation propagation.

A mechanized model lets future work (compiler changes, new backends, new synchronization primitives) be checked against a stable specification. Discrepancies become Lean proof failures rather than production bugs found after release.

## What Not To Mechanize

- Specific runtime backend behavior (thread pool scheduling, io_uring submission, simulation seed semantics). The model abstracts over backends.
- Channel internals beyond their abstract send/recv/close algebra.
- FFI semantics. Trusted regions are modeled as opaque transitions.
- Performance properties. The model is about correctness, not throughput.

## Connection To Evidence Reports

Evidence claims marked **enforced** in `--report concurrency` should each correspond to a theorem (or theorem instance) in this model:

| Evidence claim | Theorem |
|---|---|
| no leaked tasks | T2 |
| no handles escape scope | T3 |
| no missing-concurrency deadlock | T4 (when `Concurrent ∉ C`) |
| cancellation cleanup terminates | T5 |
| race preserves linearity | T6 |

The audit pipeline can cite the theorem name in the report. A reviewer who wants to verify the claim follows the citation to the Lean proof.

This is what "machine-checked evidence" means concretely.

## One-Line Test

A formal model is good if a future change to the concurrency surface can be evaluated by extending the model and rerunning the proofs, rather than by exhaustive testing and ad hoc reasoning.

## Relationship To Other Notes

- [../stdlib-runtime/async-concurrency-evidence.md](../stdlib-runtime/async-concurrency-evidence.md) — umbrella; this note formalizes the behavior described there
- [concurrency-evidence-example.md](concurrency-evidence-example.md) — worked example; theorems here back the enforced claims there
- [../stdlib-runtime/channel-model.md](../stdlib-runtime/channel-model.md) — channel algebra; this note treats it abstractly
- [../stdlib-runtime/ffi-cancellation-boundary.md](../stdlib-runtime/ffi-cancellation-boundary.md) — trusted-region semantics
- [../predictable-execution/concurrent-stack-analysis.md](../predictable-execution/concurrent-stack-analysis.md) — stack discipline interacts with cancellation termination
- [../language/capability-polymorphism.md](../language/capability-polymorphism.md) — polymorphism over capability sets; this note assumes elaborated sets
- [concrete-to-lean-pipeline.md](concrete-to-lean-pipeline.md) — pipeline for surfacing Lean proofs as evidence
- [proving-concrete-functions-in-lean.md](proving-concrete-functions-in-lean.md) — sibling formalization workflow

# Authority Budgets And Dependency Policy

**Status:** Open

This note develops the idea that capabilities should eventually scale from function-level facts into package-, subsystem-, and binary-level policy.

## Core Idea

Concrete already answers:

- what authority does this function require?

A stronger future system should also answer:

- what authority is this package, dependency, subsystem, or binary allowed to require at all?

That is the authority-budget idea.

## Why This Matters

Without budgets, capability information is descriptive:

- "this code requires `File`"
- "this code transitively requires `Network`"

With budgets, capability information becomes enforceable:

- "this dependency may use only `{File, Alloc}`"
- "this subsystem may not require `Network` or `Process`"
- "this binary must stay within this declared authority set"

That turns capability checking into a project-level control mechanism rather than only a local type/property.

## Example Shapes

Conceptually, the future project/package model could support statements like:

- package `logger` may use only `{File, Alloc}`
- package `config` may use only `{FileRead, Alloc}`
- binary `worker` may not require `Network`
- dependency `fmt_json` must remain pure except for allocation

Then if a dependency grows new authority, the tooling can:

- report it
- fail the build
- require explicit review/approval

## What Problems It Solves

Authority budgets are especially valuable for:

- preventing authority creep in dependencies
- reviewing third-party packages
- keeping least-authority boundaries stable over time
- making subsystem trust boundaries easier to state and enforce
- connecting package management with Concrete's core capability model

This is one of the most natural ways to make the package/dependency model concretely safer instead of merely organized.

## Relation To Reports

Authority budgets only become usable if the reporting story is strong.

The project already has the beginnings of this:

- capability "why" traces
- authority flow reporting
- trust boundary reporting

Future package-level budgeting would naturally build on those facts:

- direct and transitive capability summaries per package
- diffs when authority grows
- explanations of why a package exceeded its budget

## Relation To The Dependency Manager

This is where the idea becomes real.

If Concrete gets a real package/dependency model, it should eventually be able to enforce:

- declared budgets for packages or subsystems
- dependency checks against those budgets
- explicit handling of exceptions or approved budget increases

Without enforcement in the package/project layer, authority budgets remain only a nice report concept.

## What This Should Not Become

Authority budgets should not become:

- a second unrelated effect system
- an unreadable policy language
- a source of hidden ambient rules

The design should stay:

- explicit
- reviewable
- grep-able
- aligned with existing capability names and reports

## Implementation Path

### Step 1: Module-level budgets (no package model needed)

```con
#[authority(Alloc)]
mod Parser {
    // All functions here may only use Alloc
    // Transitive call to Console/File/etc. is a compile error
}
```

**Effort**: ~1 week
- Parser: `#[authority(...)]` attribute on `mod` declarations (1 day)
- AST: `authorityBudget : Option (List String)` on module definitions
- New checking pass: walk module, resolve transitive caps via existing BFS, check containment (2-3 days)
- Integration with `--report authority`: show budget violations (1 day)

**What makes this easy**: `--report authority` already computes the exact transitive capability set per function. Budget checking is just `transitive_caps ⊆ declared_budget`.

### Step 2: Package-level budgets (requires package model)

Deferred until the package model lands (Phase J). Would add budget declarations to package manifests and check cross-dependency authority.

### Difficulty summary

| Component | Effort | Prerequisite |
|-----------|--------|-------------|
| Module-level `#[authority(...)]` | 1 week | None |
| Report integration | 1 day | Module-level budgets |
| Package-level budgets | 2-3 weeks | Package model |
| Cross-dependency enforcement | 1-2 weeks | Package-level budgets |

### Evidence from Phase H programs

The JSON parser already has natural authority boundaries:
- `Lex` module: pure (no capabilities needed)
- `Parser` module: Alloc only
- `Printer` module: Console + Alloc
- `Main` module: Std (everything)

Module-level budgets would formalize and enforce what's already implicit.

## Relation To The Roadmap

This idea spans two phases most directly:

- **Phase F**: authority clarity, stronger safety profiles, explicit wrappers/aliases
- **Phase H**: package/dependency semantics and actual enforcement of package- or subsystem-level authority budgets

It also benefits from:

- **Phase C** audit/report work
- **Phase I** evidence and traceability workflows

## Why This Fits Concrete

This is one of the strongest Concrete-specific ideas because it extends an existing strength instead of inventing a new subsystem:

- capabilities already exist
- reports already explain authority flow
- package management will need a security/trust story anyway

Authority budgets make those parts reinforce each other.

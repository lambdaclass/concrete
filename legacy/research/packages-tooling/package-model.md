# Package Model

Status: exploratory

Concrete will eventually need a real package/project model. This note isolates that work instead of leaving it scattered across roadmap bullets.

## Why This Matters

Once the compiler, stdlib, and proof story become credible, the next serious usability question is simple:

- how do real projects depend on each other?

Without a coherent answer, the language stays repo-local longer than it should.

## What The Package Model Must Eventually Answer

### 1. Package Identity

- what names a package?
- how are versions expressed?
- what counts as compatibility?

### 2. Module And Package Boundaries

- what is the relationship between source modules and packages?
- what is public to downstream users?
- what stays private to a package?

### 3. Dependency Semantics

- how are dependencies declared?
- how are version constraints resolved?
- how are conflicting requirements handled?

### 4. Workspace Model

- how do multi-package repos work?
- how do local path dependencies behave?
- how do shared lockfiles or build graphs work?

### 5. Stdlib And Third-Party Boundary

- what counts as "the stdlib" versus ordinary packages?
- what package capabilities, if any, differ for stdlib code?

The intended answer should be:

- the stdlib is a builtin package dependency
- user projects should not point at std through repo-relative paths
- the resolver should supply std identity/version directly
- lockfiles and package graphs should still record which std version/build identity a project resolved against

## Why Concrete's Package Model Should Be Different

Concrete should not stop at "packages can import packages."

The package model is also the natural place for:

- authority budgets
- trust boundaries
- proof/evidence linkage
- high-integrity review workflows

That means packages may eventually need to carry more than:

- name
- version
- dependency list

They may also need to carry:

- authority constraints
- trust declarations
- evidence/report references

## Authority Budgets Depend On This

The authority-budget idea becomes much more real once the package model exists.

At that point a package or subsystem could eventually say:

- allowed capabilities: `{File, Alloc}`
- forbidden capabilities: `Network`, `Process`

and the package/dependency layer could enforce that budget.

Without a package model, authority budgets remain mostly a report concept.

## What Should Happen First

Concrete should not jump straight to registries or ecosystem scale.

The first package-model milestones should be smaller:

1. local package identity
2. path dependencies
3. builtin std dependency resolution (replace repo-path std manifests)
4. workspaces / multi-package repos
5. explicit public/private boundaries
6. authority-aware dependency metadata later

## Current State (2026-03-16)

`concrete build`, `concrete run`, and `concrete test` all work:

- parses `Concrete.toml` for package name and path dependencies
- resolves `mod X;` stubs from source directory (including `name/mod.con` directory modules)
- loads dependency modules, merges with project modules, runs full pipeline
- `cgrep`, `conhash`, and `project` examples converted to use std imports
- **builtin std resolution**: std is found automatically relative to the compiler binary (or via `CONCRETE_STD` env var). No `std = { path = "..." }` needed in user manifests.
- **`concrete run`**: builds to temp binary, executes with inherited stdio, cleans up. Supports `-- args...`.
- **`concrete test`**: project-aware test mode. Loads dependencies, compiles in test mode, runs test binary. Supports `--module` filter.
- **error messages**: actionable hints for missing Concrete.toml, missing src/main.con, bad dependency paths.

## Roadmap Placement

This belongs mainly in:

- **Phase J**: package and project model (first priority after Phase H)

Phase J remaining work:

- ~~builtin std resolution~~ **DONE**
- ~~`concrete build/test/run`~~ **DONE** (basic versions)
- workspace / multi-package repo support
- recording std identity/version in lockfile without exposing as path dep
- incremental / artifact direction

Also tied to:

- **Phase F**: authority-aware safety workflows
- **Phase I**: operational maturity and evidence workflows

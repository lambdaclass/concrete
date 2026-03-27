# Package Testing Tooling

**Status:** Open research direction

This note covers the testing workflow that should exist once Concrete has a real package/project model.

It is intentionally narrower than [testing-strategy.md](testing-strategy.md).

- `testing-strategy.md` is about confidence, invariants, fuzzing, reports, and differential testing
- this note is about the user-facing package/project testing workflow: `concrete test`, discovery rules, output shape, filtering, and failure UX

The main point is simple:

- better package/project tooling without better testing tooling is incomplete

If Concrete wants real programs and real packages, testing has to stop feeling like a shell-level side path and start feeling like a first-class part of the toolchain.

## What This Should Solve

Concrete already has useful testing ingredients:

- `lean_tests/` for end-to-end language/compiler tests
- `#[test]` support for package-local tests
- `run_tests.sh` for broad regression coverage

The gap is the project-facing workflow.

Right now the package model is becoming real, but the user-facing test experience still needs a clear shape:

- how tests are discovered
- how package tests are run
- how one test is selected
- how failures are reported
- how workspaces behave
- how test mode relates to reports, dependencies, and incremental builds

## UX Goals

The package/project testing UX should be:

- obvious for small projects
- consistent with `concrete build` / `concrete run`
- package-aware
- workspace-aware later
- good enough for daily development, not only CI

The intended user surface should feel like one coherent tool:

```bash
concrete test
concrete test parser
concrete test --filter json
concrete test --workspace
concrete test --report
```

Not all of those flags need to land at once, but the end state should be clear early.

## First User-Facing Shape

### 1. Run package tests

```bash
concrete test
```

Behavior:

- find `Concrete.toml`
- build the current package in test mode
- discover package-local tests
- run them
- return non-zero on failure

This should be the default “does my project still work?” command.

### 2. Run one named test or filtered subset

```bash
concrete test sha256_known_vector
concrete test --filter sha256
```

This matters because once programs are nontrivial, rerunning the entire package for one failing test becomes slow and annoying.

### 3. Show clear failures

A failure should show:

- test name
- package/module path
- failure reason
- source location if available

Bad:

- giant opaque dump
- compiler-internal noise first

Good:

```text
FAIL verifier::sha256_known_vector
  at src/hash_test.con:41
  expected: "9f86d081..."
  got:      "e3b0c442..."
```

### 4. Workspace later

```bash
concrete test --workspace
```

Later, this should:

- run tests across workspace members
- preserve package boundaries in output
- support filtering by package and test name

## Discovery Model

The first version should stay boring.

Likely sources of tests:

- `#[test]` functions inside package modules
- maybe a conventional `tests/` directory later if needed

I would not start with multiple competing test styles.

Best first rule:

- package tests are `#[test]` functions reachable from the package graph in test mode

That keeps the model aligned with the compiler and package graph.

## Error And Failure UX

Testing tooling should improve three different failure classes:

### 1. Build/setup errors

Examples:

- no `Concrete.toml`
- bad dependency
- unresolved import
- malformed workspace

These should fail before test execution, with package-aware messages.

### 2. Compilation errors in test code

These should look like ordinary compilation errors, but clearly say they occurred while building tests.

### 3. Runtime test failures

These should name the failing test cleanly and not drown the user in unrelated output.

## Package-Model Integration

Testing tooling should be part of Phase J package hardening, not an unrelated later feature.

Why:

- package roots determine discovery
- dependency resolution determines what tests can import
- builtin std resolution affects ordinary test code
- incremental compilation matters immediately for test turnaround

So `concrete test` should be designed alongside:

- `concrete build`
- `concrete run`
- builtin `std`
- workspaces
- graph artifacts

## Interaction With Incremental Compilation

Testing is one of the clearest reasons to invest in artifact reuse.

Good outcome:

- unchanged package modules are reused
- only changed test modules recompile
- test-mode artifacts are distinct where needed

Bad outcome:

- every `concrete test` rebuilds the whole package graph from scratch

That means testing tooling should be one of the practical drivers for incremental compilation, not just a consumer of it.

## Interaction With Reports

Eventually, testing and reporting should connect.

Possible future commands:

```bash
concrete test --report authority
concrete test --report proof
```

Or:

```bash
concrete report authority --tests
```

The point is not to add flag clutter early.
The point is to keep the architecture compatible with package-aware report generation and evidence workflows.

## What To Avoid

- shell-script-only testing as the main user story
- many different test conventions too early
- brittle snapshot-heavy output checks as the primary model
- a split world where package mode exists for build/run but test still feels ad hoc

## MVP Order

1. `concrete test` for the current package
2. package-aware error messages
3. single-test / filtered test execution
4. cleaner failure output
5. workspace test mode
6. incremental test rebuilds
7. report-aware test integration

## Recommended Roadmap Placement

This should live in Phase J package/project hardening, immediately after:

- builtin std resolution
- `concrete build`
- `concrete run`

And before:

- richer workspace/package-manager features
- evidence-heavy operational workflow

Because testing is part of “real project usability,” not a polish-only add-on.

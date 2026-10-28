# Bug 316 — Browser.lean, the browser compiler root, did not build: it called the removed Pipeline.desugar

**Status:** FIXED (2026-10-28, branch `wip/bug-316`)
**Found:** 2026-10-28, by the cleanup pass's totality ratchet. Its environment walk tried to import
every executable root, and `lake build Browser` failed on main `ca49fada`. Codex confirmed the bug
as real (read-only review of the source and history): `6269002b` deleted `Pipeline.desugar` and
updated every caller but `Browser.lean`.
**Severity:** loud, in the browser compiler only. The `concrete-browser-native` executable, the
native twin of the site's Wasm compiler, could not be built from source. Neither could the Wasm
compiler, which `scripts/browser/build-wasm.py` builds from the same root. The site kept serving
the committed Wasm snapshot from `341514d0` (2026-10-22), one day older than the break, so
visitors saw no failure. But no change to the compiler could reach the browser compiler. The
native CLI and its outputs were not affected.

<!-- bug-ledger
summary: `Browser.lean`, the root of the `concrete-browser-native` executable (the native twin of the site's Wasm compiler), still called `Pipeline.desugar` after `6269002b` folded desugaring into `Pipeline.resolve`, so the browser compiler could not be built from source; nothing noticed for a week because the root sat outside `lakefile.toml`'s `defaultTargets` and no `lake build`, CI job or hook compiled it. The call is removed (resolve already desugars) and the executable is a default target, run by `check_browser_native.sh`.
coverage: check_browser_native.sh (builds concrete-browser-native, then runs the native adapter on scripts/browser/test-parity.mjs's cases: verdicts, the four stages, diagnostics on refusal)
class: 11 — a build root that nothing built read as a root that builds
silent: no
-->

## Reproducer

On main `ca49fada`, inside `nix develop`:

```
$ lake build Browser
error: Browser.lean:43:18: Unknown identifier `Pipeline.desugar`
Some required targets logged failures:
- Browser
error: build failed
```

`lake build` without a target succeeds, because `concrete-browser-native` is not among
`lakefile.toml`'s `defaultTargets`.

## Observed

`lake build concrete-browser-native` (the first step of `scripts/browser/README.md`'s build) and
`lake build Browser` fail. No CI job, Makefile target or hook step builds either, so main stayed
green.

## Expected

`scripts/browser/README.md` builds the browser compiler from the repository with
`lake build concrete-browser-native`, and `scripts/browser/test-parity.mjs` compares that native
build with the Wasm build. The root has to compile at every commit of main.

## Cause

`Browser.lean`'s `run` restates the front-end stage sequence (parse, summary, resolve, check,
elaborate, core-check, mono, lower, emit) instead of calling a shared driver, because
`Pipeline.runFrontend` does file resolution in `IO` and the browser has no files. Bug 202's fix
(`6269002b`, R-0492 slice 1) made desugaring part of `Pipeline.resolve`, deleted `Pipeline.desugar`
and updated the drivers it could see. `Browser.lean` was not compiled by any of the builds that
change was tested with, so its call survived. Deleting the call is the whole repair:
`Pipeline.resolve` maps `finishResolve`, `resolveModuleLocals (desugarModule …)`, over every module,
which is what `Pipeline.desugar` did.

## Structural cause

Class 11, [absence read as a fact](STRUCTURAL_CLASSES.md#11-absence-read-as-a-fact-fail-open-defaults):
a build root that no build compiled was read as a root that builds. A green `lake build` said
nothing about a target it was never asked to build, in the same way that bug 252's link check that
no workflow step ran passed as coverage. The edit that broke it is an instance of class 12 (one fact
kept in two places): the stage sequence is restated by each driver, and bug 202 is the earlier
instance in which four drivers skipped desugaring.

## Structural owner

For class 11, `lakefile.toml` now makes `concrete-browser-native` a default target. Every
`lake build` builds it: CI's build job (whose warning check runs an untargeted `lake build`),
`make build`, and the pre-push hook's build. `check_browser_native.sh` builds it and runs it.
For the restated sequence (class 12), the owner is the one-resolver work of R-0492/R-0514: a pure
front-end prefix that `runFrontend` and the browser both call would leave nothing to restate.

## Containment while open

Fixed. `check_browser_native.sh` (in CI's Language-surface gates job and the gate inventory) fails
if the root stops building or its adapter stops answering the parity cases.

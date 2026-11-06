### Bug 316: Browser.lean, the browser compiler root, did not build: it called the removed Pipeline.desugar (2026-10-28)

The browser compiler builds from source again: `Browser.lean` no longer calls `Pipeline.desugar`,
which `Pipeline.resolve` absorbed. `concrete-browser-native` is now one of `lakefile.toml`'s
`defaultTargets`, so every `lake build`, including CI's and the hook's, compiles it.
`scripts/tests/check_browser_native.sh` builds it and runs the native adapter on the parity cases.
The root had sat outside every build, so the break went unnoticed for a week
([bug 316](docs/bugs/316_browser_lean_the_browser_compiler_root_did_not_b.md)).

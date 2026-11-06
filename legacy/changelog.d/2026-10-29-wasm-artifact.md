### The browser compiler is built by the site deployment, not committed (2026-10-29)

`site/static/compiler/concrete.wasm` (15 MB), `concrete.mjs` and `manifest.json` are no longer in
git. `scripts/browser/build-site-compiler.sh` fetches the pinned Lean source and Emscripten into
`.cache/browser`, builds the single-threaded Lean runtime, cross-compiles this commit's compiler and
runs the native/Wasm parity, loader and UI tests; the site workflow runs it before Zola, so the
published explorer always matches the published commit. History is not rewritten.

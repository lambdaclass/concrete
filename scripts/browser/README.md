# Browser compiler and landing-page explorer (experimental)

The existing Lean compiler is compiled to Wasm and called from a module worker.
`Browser.lean` exposes the production pipeline and returns JSON containing Core,
specialized Core, SSA, LLVM text, and diagnostics. The landing page loads the
compiler only on demand. It never uploads source. Cancel and the compilation
deadline terminate the worker; editing source invalidates previous results.

This is a **Wasm-hosted compiler**, not a Wasm code-generation backend. LLVM
output currently uses the pipeline's default target. There is no executable
generation, execution, import/file resolution, standard-library loading, or
independent proof verifier. Function selection links Core functions to source
declaration lines (Core and SSA); this is not a complete source-to-IR transformation map.

## Build

The compiler assets in `site/static/compiler/` (`concrete.mjs`, `concrete.wasm`, `manifest.json`)
are build outputs and are not committed. The site deployment (`.github/workflows/book.yml`) builds
them from the commit it publishes, and runs the parity tests first, with one script:

```sh
JOBS=6 bash scripts/browser/build-site-compiler.sh
```

Prerequisites: the repository's pinned Lean toolchain, Git, CMake, Python 3 and Node. The script
fetches the rest into `.cache/browser` (override with `BROWSER_CACHE`) and reuses it on later runs:
the Lean source checkout matching `lean-toolchain` with `lean-runtime.patch` applied, Emscripten
4.0.23, and the single-threaded Lean runtime built with Emscripten. It then builds
`concrete-browser-native`, cross-compiles with `build-wasm.py`, and runs `test-parity.mjs`,
`test-loader.mjs` and `test-ui.mjs`. Upgrading the Lean toolchain requires reviewing the runtime
patch; the parity tests are the check.

The patch removes pthread requirements for a single-worker runtime (so the
site needs no cross-origin-isolation headers), fixes two unsupported libuv
stub signatures, and removes obsolete world arguments from the temporary-file
runtime functions to match the pinned generated stdlib C. Unsupported event-loop
operations still fail; they are not silently implemented as successes.

Generated C comes from `.lake/build/ir` for Concrete and the pinned Lean
checkout's `stage0/stdlib` for dependencies. Objects are cached in `.lake/browser`.
The manifest records the compiler commit, compiled-source digest, toolchain, Wasm digest and
size. A Zola build without the assets still works: the explorer reports that the browser
compiler is unavailable.

## Local preview

```sh
zola --root site build --base-url http://127.0.0.1:18081
python3 -m http.server 18081 --bind 127.0.0.1 --directory site/public
```

Open `http://127.0.0.1:18081/#explorer`. Compile each example; inspect each
representation; select a Core function; edit the source and confirm old results
are disabled; cancel a compilation; retry. Test with a narrow viewport too.
JavaScript modules and Wasm require HTTP with the correct MIME types, not a
`file://` URL. A modern browser with WebAssembly exception handling is required.

Limits: 32 KiB UTF-8 source, 120 seconds to load, 20 seconds to compile, and a
1 GiB maximum Wasm memory. The first prototype is about 14.3 MiB uncompressed.
The loader revalidates the manifest, uses digest-versioned asset URLs for HTTP
caching, reports download progress, and checks the Wasm size and SHA-256 before
instantiation. These are delivery integrity checks, not formal verification.
Completed validated stages remain inspectable when a later pass rejects a program.

The native/Wasm test compares exact JSON across accepted and rejected programs,
including repeated calls in one runtime. This establishes parity for those
cases, not formal compiler correctness or full language coverage.

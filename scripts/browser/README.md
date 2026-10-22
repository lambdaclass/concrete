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

Prerequisites: the repository's pinned Lean toolchain, Python 3, Node, CMake,
Git, and Emscripten 4.0.23. Build in separate scratch directories. Commands
below use paths relative to the repository root; substitute your own paths.
The Lean checkout must match `lean-toolchain`; upgrading requires reviewing
the runtime patch and rerunning parity checks.

```sh
git clone --depth 1 --branch v4.34.1 https://github.com/leanprover/lean4 /tmp/concrete-browser-lean
git clone https://github.com/emscripten-core/emsdk /tmp/concrete-browser-emsdk
/tmp/concrete-browser-emsdk/emsdk install 4.0.23
/tmp/concrete-browser-emsdk/emsdk activate 4.0.23
git -C /tmp/concrete-browser-lean apply --unidiff-zero "$PWD/scripts/browser/lean-runtime.patch"
/tmp/concrete-browser-emsdk/upstream/emscripten/emcmake cmake \
  -S /tmp/concrete-browser-lean/src -B /tmp/concrete-browser-runtime \
  -DSTAGE=0 -DCMAKE_BUILD_TYPE=Release -DUSE_GMP=OFF -DUSE_MIMALLOC=OFF \
  -DMULTI_THREAD=OFF -DAUTO_THREAD_FINALIZATION=OFF \
  -DLEAN_VERSION_PATCH=1 -DUSE_GITHASH=OFF
cmake --build /tmp/concrete-browser-runtime --target leanrt -j 6
lake build concrete-browser-native
python3 scripts/browser/build-wasm.py \
  --lean-source /tmp/concrete-browser-lean \
  --runtime /tmp/concrete-browser-runtime \
  --emsdk /tmp/concrete-browser-emsdk --jobs 6
node scripts/browser/test-parity.mjs
node scripts/browser/test-loader.mjs
node scripts/browser/test-ui.mjs
zola --root site build
```

The patch removes pthread requirements for a single-worker runtime (so the
site needs no cross-origin-isolation headers), fixes two unsupported libuv
stub signatures, and removes obsolete world arguments from the temporary-file
runtime functions to match the pinned generated stdlib C. Unsupported event-loop
operations still fail; they are not silently implemented as successes.

Generated C comes from `.lake/build/ir` for Concrete and the pinned Lean
checkout's `stage0/stdlib` for dependencies. Objects are cached in `.lake/browser`.
The tested compiler snapshot in `site/static/compiler/` is versioned so ordinary
Zola builds and Pages deployments include a working explorer. Rebuild and run
parity tests before updating that snapshot. The manifest
records the baseline commit, compiled-source digest, toolchain, Wasm digest and
size. The baseline commit alone does not identify uncommitted changes.

To update the compiler snapshot, rebuild assets **before** Zola. The existing
deployment workflow has not been changed to download and cross-build these
toolchains. Without assets the explorer reports that the browser compiler is
unavailable. No site is published by these commands.

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
1 GiB maximum Wasm memory. The first prototype is about 13 MiB uncompressed.
The loader revalidates the manifest, uses digest-versioned asset URLs for HTTP
caching, reports download progress, and checks the Wasm size and SHA-256 before
instantiation. These are delivery integrity checks, not formal verification.
Completed validated stages remain inspectable when a later pass rejects a program.

The native/Wasm test compares exact JSON across accepted and rejected programs,
including repeated calls in one runtime. This establishes parity for those
cases, not formal compiler correctness or full language coverage.

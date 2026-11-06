#!/usr/bin/env python3
"""Cross-compile the existing compiler's generated C; no second compiler implementation.

Requires a native `lake build concrete-browser-native`, a pinned Lean source checkout,
and its single-threaded Emscripten runtime build. See scripts/browser/README.md.
"""
import argparse
import concurrent.futures
import hashlib
import json
from pathlib import Path
import re
import subprocess

ROOT = Path(__file__).resolve().parents[2]


def main():
    p = argparse.ArgumentParser()
    p.add_argument('--lean-source', type=Path, required=True)
    p.add_argument('--runtime', type=Path, required=True)
    p.add_argument('--emsdk', type=Path, required=True)
    p.add_argument('--jobs', type=int, default=4)
    args = p.parse_args()
    em = args.emsdk / 'upstream/emscripten'
    out = ROOT / 'site/static/compiler'
    out.mkdir(parents=True, exist_ok=True)
    cache = ROOT / '.lake/browser'
    cache.mkdir(parents=True, exist_ok=True)
    roots = [ROOT / '.lake/build/ir', args.lean_source / 'stage0/stdlib']
    modules = {}

    def visit(name):
        if name in modules:
            return
        rel = Path(name.replace('.', '/') + '.c')
        src = next((r / rel for r in roots if (r / rel).exists()), None)
        if src is None:
            raise RuntimeError(f'Missing generated C for {name}; build the native target first')
        modules[name] = src
        header = src.read_text().splitlines()[2]
        for dep in re.findall(r'\bimport (?:all )?([A-Za-z0-9_.]+)', header):
            visit(dep)

    visit('Browser')
    print(f'Compiling {len(modules)} modules with {args.jobs} workers', flush=True)
    flags = ['-O1', '-fwasm-exceptions', '-DLEAN_EMSCRIPTEN',
             '-I' + str(args.runtime / 'include')]
    inputs = list(modules.items()) + [('bridge', ROOT / 'scripts/browser/bridge.c')]
    toolchain = subprocess.check_output([str(em / 'emcc'), '--version'])
    headers = (args.runtime / 'include/lean/lean.h').read_bytes()

    def compile_one(item):
        name, src = item
        obj = cache / (name + '.o')
        # Flags, headers and toolchain are included, not just source mtime.
        key = hashlib.sha256(src.read_bytes() + repr(flags).encode()
                             + headers + toolchain).hexdigest()
        stamp = obj.with_suffix('.sha256')
        if not obj.exists() or not stamp.exists() or stamp.read_text() != key:
            subprocess.run([str(em / 'emcc'), *flags, '-c', str(src), '-o', str(obj)], check=True)
            stamp.write_text(key)
        return obj

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        objects = list(pool.map(compile_one, inputs))
    print('Linking browser module', flush=True)
    subprocess.run([str(em / 'em++'), *map(str, objects),
                    str(args.runtime / 'lib/lean/libleanrt.a'),
                    str(args.runtime / 'libuv/src/libuv/libuv.a'),
                    '-O1', '-fwasm-exceptions', '--no-entry',
                    '-sMODULARIZE=1', '-sEXPORT_ES6=1', '-sEXPORT_NAME=createConcrete',
                    '-sENVIRONMENT=web,worker,node', '-sALLOW_MEMORY_GROWTH=1',
                    '-sSTACK_SIZE=16777216', '-sINITIAL_MEMORY=67108864',
                    '-sMAXIMUM_MEMORY=1073741824', '-sINVOKE_RUN=0',
                    '-sEXPORTED_FUNCTIONS=["_browser_compile"]',
                    '-sEXPORTED_RUNTIME_METHODS=["ccall"]',
                    '-o', str(out / 'concrete.mjs')], check=True)
    module = out / 'concrete.mjs'
    module.write_text('\n'.join(line.rstrip() for line in module.read_text().splitlines()).rstrip() + '\n')
    wasm = out / 'concrete.wasm'
    manifest = {'schemaVersion': 1,
                'sourceSha256': hashlib.sha256(b''.join(name.encode() + src.read_bytes() for name, src in inputs)).hexdigest(),
                'emscriptenVersion': toolchain.decode().splitlines()[0],
                'compilerCommit': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip(),
                'leanVersion': (ROOT / 'lean-toolchain').read_text().strip(),
                'moduleSha256': hashlib.sha256(module.read_bytes()).hexdigest(),
                'wasmBytes': wasm.stat().st_size,
                'wasmSha256': hashlib.sha256(wasm.read_bytes()).hexdigest(),
                'scope': 'Single-file compilation; no std imports, execution, linking or proof checking.'}
    (out / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    print(json.dumps(manifest), flush=True)


if __name__ == '__main__':
    main()

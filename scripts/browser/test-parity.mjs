// The native adapter and Wasm must return identical IR and diagnostics.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import createConcrete from '../../site/static/compiler/concrete.mjs';

const native = fileURLToPath(new URL('../../.lake/build/bin/concrete-browser-native', import.meta.url));
const runtime = await createConcrete();
// One list for both halves of the parity check: scripts/tests/check_browser_native.sh runs the
// native half of the same cases in CI (bug 316).
const cases = JSON.parse(readFileSync(new URL('./parity_cases.json', import.meta.url), 'utf8'));
for (let round = 0; round < 3; round++) {
  for (const [name, source, ok] of cases) {
    const process = spawnSync(native, [], { input: source, encoding: 'utf8', timeout: 20000 });
    assert.equal(process.status, 0, process.stderr);
    const expected = JSON.parse(process.stdout);
    const actual = JSON.parse(runtime.ccall('browser_compile', 'string', ['string'], [source]));
    assert.equal(actual.target, 'arm64-apple-macosx14.0.0');
    assert.equal(expected.ok, ok, `${name}: unexpected native verdict`);
    assert.deepEqual(actual, expected, `${name}: native/Wasm mismatch`);
    if (ok) assert.deepEqual(actual.stages.map(s => s.name), ['Core', 'Specialized Core', 'SSA', 'LLVM']);
    else assert.ok(actual.diagnostics.length);
  }
}
console.log(`Passed ${cases.length * 3} native/Wasm comparisons, including repeated calls after errors.`);

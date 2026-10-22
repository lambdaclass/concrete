// The native adapter and Wasm must return identical IR and diagnostics.
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import createConcrete from '../../site/static/compiler/concrete.mjs';

const native = fileURLToPath(new URL('../../.lake/build/bin/concrete-browser-native', import.meta.url));
const runtime = await createConcrete();
const cases = [
  ['constant', 'mod demo { fn main() -> Int { return 42; } }', true],
  ['arithmetic', 'mod demo { fn add(a: i64, b: i64) -> i64 { return a + b; } fn main() -> Int { return add(20, 22); } }', true],
  ['generic', 'mod demo { fn identity<T>(x: T) -> T { return x; } fn main() -> Int { return identity::<Int>(42); } }', true],
  ['struct', 'mod demo { struct Copy Point { x: Int, y: Int } fn main() -> Int { let p = Point { x: 20, y: 22 }; return p.x + p.y; } }', true],
  ['capability', 'mod demo { fn report() with(Console) { println("Hello from Concrete"); } fn main() with(Console) -> Int { report(); return 0; } }', true],
  ['ownership error', 'mod demo { struct Point { x: Int } fn main() -> Int { let p = Point { x: 42 }; return p.x; } }', false],
  ['branch', 'mod demo { fn main() -> Int { if true { return 42; } else { return 0; } } }', true],
  ['type error', 'mod demo { fn main() -> Int { return true; } }', false],
  ['parse error', 'mod demo { fn main(', false],
  ['unknown name', 'mod demo { fn main() -> Int { return missing; } }', false],
  ['unicode comment', '// café λ\nmod demo { fn main() -> Int { return 42; } }', true],
];
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

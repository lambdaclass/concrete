// Exercise UI state transitions without adding a browser/framework dependency.
import assert from 'node:assert/strict';
import vm from 'node:vm';
import { readFileSync } from 'node:fs';
const elements = new Map();
function element(id) {
  if (!elements.has(id)) elements.set(id, {
    value: id === 'source' ? 'fn main() -> Int { return 42; }' : '',
    textContent: '', disabled: false, hidden: false, dataset: {}, handlers: {}, options: [],
    addEventListener(type, fn) { this.handlers[type] = fn; },
    replaceChildren(...children) { this.options = children; this.value = children[0]?.value || ''; },
    add(child) { this.options.push(child); }, focus() {}, setSelectionRange() {},
  });
  return elements.get(id);
}
const workers = [];
let workerFailure = false;
class Worker {
  constructor() { if (workerFailure) throw new Error('Workers unavailable'); workers.push(this); }
  postMessage(data) { this.source = data.source; }
  terminate() { this.terminated = true; }
  message(data) { this.onmessage({ data }); }
}
const context = vm.createContext({
  document: { getElementById: id => element(id.replace('explorer-', '')) },
  Worker, URL, TextEncoder, Option: function(text, value) { this.text = text; this.value = value; },
  setTimeout: () => 1, clearTimeout() {},
});
const source = readFileSync(new URL('../../site/static/explorer.mjs', import.meta.url), 'utf8')
  .replaceAll('import.meta.url', '"https://example.test/concrete2/explorer.mjs"');
vm.runInContext(source, context);
const click = id => element(id).handlers.click();
const result = {
  type: 'result', elapsedMs: 4,
  manifest: { wasmSha256: 'a'.repeat(64), leanVersion: 'test', wasmBytes: 100 },
  result: { ok: true, target: 'test-target', diagnosticsText: '', stages: [
    { name: 'Core', text: 'typed program', functions: [] },
  ] },
};
click('compile');
assert.equal(element('compile').disabled, true);
workers[0].message({ type: 'progress', received: 50, total: 100 });
assert.match(element('status').textContent, /50%/);
click('cancel');
assert.equal(workers[0].terminated, true);
workers[0].message(result); // A queued message from a cancelled worker must not revive its result.
assert.equal(element('download').disabled, true);
click('compile');
workers[1].message(result);
assert.equal(element('output').textContent, 'typed program');
assert.equal(element('download').disabled, false);
assert.match(element('build').textContent, /test-target/);
element('source').handlers.input();
assert.equal(element('download').disabled, true);
assert.equal(element('stage').disabled, true);
click('compile');
workers[1].message({ ...result, result: { ...result.result, ok: false, diagnosticsText: 'later pass rejected' } });
assert.equal(element('stage').disabled, false);
assert.equal(element('diagnostics').hidden, false);
assert.equal(element('diagnostics').textContent, 'later pass rejected');
click('compile');
workers[1].message({ type: 'error', message: 'download interrupted' });
assert.equal(element('compile').disabled, false);
assert.equal(workers[1].terminated, true);
workerFailure = true;
click('compile');
assert.equal(element('compile').disabled, false);
assert.match(element('status').textContent, /Workers unavailable/);
console.log('UI passed: progress, cancel/stale-message rejection, results, invalidation, partial stages, failure/retry controls.');

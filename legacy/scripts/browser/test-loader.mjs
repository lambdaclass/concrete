import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { loadCompiler } from '../../site/static/compiler-loader.mjs';

const base = new URL('../../site/static/explorer-worker.mjs', import.meta.url);
const manifest = JSON.parse(await readFile(new URL('./compiler/manifest.json', base)));
const wasm = new Uint8Array(await readFile(new URL('./compiler/concrete.wasm', base)));
let mode = 'ok';
const requests = [];
const originalFetch = globalThis.fetch;
globalThis.fetch = async (url, options) => {
  requests.push({ url: String(url), options });
  if (String(url).endsWith('manifest.json')) {
    if (mode === 'missing') return new Response('', { status: 404 });
    return Response.json(mode === 'bad-manifest' ? { ...manifest, schemaVersion: 999 } : manifest);
  }
  if (mode === 'network') throw new Error('Network interrupted');
  if (mode === 'short') return new Response(wasm.slice(0, 100));
  if (mode === 'corrupt') { const bytes = wasm.slice(); bytes[20] ^= 1; return new Response(bytes); }
  return new Response(new ReadableStream({ start(controller) {
    for (let start = 0; start < wasm.length; start += 1048576) controller.enqueue(wasm.slice(start, start + 1048576));
    controller.close();
  } }));
};
try {
  for (const [failure, message] of [
    ['missing', /unavailable/], ['bad-manifest', /manifest/],
    ['network', /interrupted/], ['short', /incomplete/], ['corrupt', /integrity/],
  ]) {
    mode = failure;
    await assert.rejects(loadCompiler({ base }), message);
  }
  mode = 'ok';
  const progress = [];
  const { runtime } = await loadCompiler({ base, progress: (n, total) => progress.push([n, total]) });
  assert.ok(progress.length > 1);
  assert.deepEqual(progress.at(-1), [wasm.length, wasm.length]);
  assert.ok(progress.every(([n], i) => i === 0 || n >= progress[i - 1][0]));
  const result = JSON.parse(runtime.ccall('browser_compile', 'string', ['string'], ['fn main() -> Int { return 42; }']));
  assert.equal(result.ok, true);
  assert.equal(requests.at(-2).options.cache, 'no-cache');
  assert.equal(requests.at(-1).options.cache, 'force-cache');
  assert.ok(requests.at(-1).url.endsWith(manifest.wasmSha256));
  console.log('Loader passed: download progress, versioned cache requests, five failure modes, and retry with real Wasm.');
} finally { globalThis.fetch = originalFetch; }

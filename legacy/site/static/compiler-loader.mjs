// HTTP caching uses content identities; the manifest is always revalidated.
// Source text never enters this loader or any network request.
export async function loadCompiler({ base = import.meta.url, progress = () => {} } = {}) {
  const response = await fetch(new URL('compiler/manifest.json', base), { cache: 'no-cache' });
  if (!response.ok) throw new Error('The compiler download is unavailable. Please retry.');
  const manifest = await response.json();
  if (manifest.schemaVersion !== 1 || !/^[a-f0-9]{64}$/.test(manifest.wasmSha256)
      || !/^[a-f0-9]{64}$/.test(manifest.moduleSha256)
      || !Number.isSafeInteger(manifest.wasmBytes) || manifest.wasmBytes <= 0
      || manifest.wasmBytes > 64 * 1024 * 1024) {
    throw new Error('Unrecognized compiler manifest. Reload the page and retry.');
  }
  const wasmURL = new URL(`compiler/concrete.wasm?v=${manifest.wasmSha256}`, base);
  const binaryResponse = await fetch(wasmURL, { cache: 'force-cache' });
  if (!binaryResponse.ok) throw new Error('Could not download the compiler. Please retry.');
  const chunks = [];
  let received = 0;
  if (binaryResponse.body) {
    const reader = binaryResponse.body.getReader();
    while (true) {
      const { value, done } = await reader.read();
      if (done) break;
      received += value.byteLength;
      if (received > manifest.wasmBytes) { await reader.cancel(); throw new Error('Compiler download size mismatch. Reload and retry.'); }
      chunks.push(value);
      progress(received, manifest.wasmBytes);
    }
  } else {
    const value = new Uint8Array(await binaryResponse.arrayBuffer());
    chunks.push(value); received = value.byteLength;
  }
  if (received !== manifest.wasmBytes) throw new Error('Compiler download was incomplete. Please retry.');
  const bytes = new Uint8Array(received);
  let offset = 0;
  for (const chunk of chunks) { bytes.set(chunk, offset); offset += chunk.byteLength; }
  const digest = [...new Uint8Array(await crypto.subtle.digest('SHA-256', bytes))]
    .map(byte => byte.toString(16).padStart(2, '0')).join('');
  if (digest !== manifest.wasmSha256) throw new Error('Compiler download failed its integrity check. Reload and retry.');
  progress(received, received);
  const moduleURL = new URL(`compiler/concrete.mjs?v=${manifest.moduleSha256}`, base);
  const { default: createConcrete } = await import(moduleURL.href);
  const runtime = await createConcrete({ wasmBinary: bytes, printErr: text => console.warn(text) });
  return { runtime, manifest };
}

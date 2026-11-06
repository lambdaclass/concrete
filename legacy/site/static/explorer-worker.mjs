import { loadCompiler } from './compiler-loader.mjs';
// One runtime per worker. Terminating the worker cancels loading or compilation.
let compiler;
self.onmessage = async ({ data }) => {
  try {
    if (typeof data.source !== 'string' || new TextEncoder().encode(data.source).length > 32768) {
      throw new Error('Use a single source file no larger than 32 KiB.');
    }
    if (data.source.includes('\0')) throw new Error('Source contains a NUL character. Remove it and compile again.');
    if (!compiler) {
      self.postMessage({ type: 'loading' });
      compiler = await loadCompiler({ progress: (received, total) => self.postMessage({ type: 'progress', received, total }) });
    }
    const { runtime, manifest } = compiler;
    self.postMessage({ type: 'compiling', manifest });
    const start = performance.now();
    const result = JSON.parse(runtime.ccall('browser_compile', 'string', ['string'], [data.source]));
    self.postMessage({ type: 'result', result, manifest, elapsedMs: performance.now() - start });
  } catch (error) {
    compiler = undefined;
    self.postMessage({ type: 'error', message: error.message || String(error) });
  }
};

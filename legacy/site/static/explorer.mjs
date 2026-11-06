const $ = id => document.getElementById(`explorer-${id}`);
const source = $('source');
const examples = {
  arithmetic: source.value,
  generic: 'mod demo {\n    fn identity<T>(x: T) -> T { return x; }\n\n    fn main() -> Int {\n        return identity::<Int>(42);\n    }\n}',
  struct: 'mod demo {\n    struct Copy Point { x: Int, y: Int }\n\n    fn main() -> Int {\n        let p = Point { x: 20, y: 22 };\n        return p.x + p.y;\n    }\n}',
  capability: 'mod demo {\n    fn report() with(Console) {\n        println("Hello from Concrete");\n    }\n\n    fn main() with(Console) -> Int {\n        report();\n        return 0;\n    }\n}',
  error: 'mod demo {\n    fn main() -> Int {\n        return true;\n    }\n}',
};
const stageHelp = {
  Core: 'Core: typed program after elaboration and Core validation.',
  'Specialized Core': 'Specialized Core: generic functions specialized for the concrete types used by this program.',
  SSA: 'SSA: basic blocks and explicit control flow, after lowering, validation, and cleanup.',
  LLVM: 'LLVM: emitted LLVM IR for the target shown below. This is not a runnable WebAssembly program.',
};
let worker, timer, busy = false, artifact = null;
const status = (text, state = '') => {
  $('status').textContent = text;
  $('status').dataset.state = state;
};
function end() {
  clearTimeout(timer);
  busy = false;
  $('compile').disabled = false;
  $('cancel').disabled = true;
}
function invalidate() {
  artifact = null;
  $('diagnostics').hidden = true;
  $('diagnostics').textContent = '';
  $('stage-help').textContent = 'Core → Specialized Core → SSA → LLVM';
  $('download').disabled = true;
  $('stage').disabled = true;
  $('function').disabled = true;
  if (busy) { worker?.terminate(); worker = null; end(); }
  $('output').textContent = 'Source changed. Compile again to see current output.';
  status('Ready to compile locally.');
}
function renderStage() {
  if (!artifact) return;
  const stage = artifact.result.stages[Number($('stage').value)];
  $('function').replaceChildren(new Option('Whole module', ''));
  stage.functions.forEach((fn, i) => $('function').add(new Option(fn.name, String(i))));
  $('function').disabled = !stage.functions.length;
  $('output').textContent = stage.text;
  $('stage-help').textContent = stageHelp[stage.name] || stage.name;
}
function showResult(data, submitted) {
  artifact = { ...data, source: submitted };
  $('download').disabled = false;
  $('build').textContent = `Build ${data.manifest.wasmSha256.slice(0, 12)} · ${data.manifest.leanVersion} · ${(data.manifest.wasmBytes / 1048576).toFixed(1)} MiB Wasm`;
  $('build').textContent += ` · Output target: ${data.result.target}`;
  $('diagnostics').hidden = data.result.ok;
  $('diagnostics').textContent = data.result.diagnosticsText;
  $('stage').replaceChildren(...data.result.stages.map((s, i) => new Option(s.name, String(i))));
  $('stage').disabled = !data.result.stages.length;
  if (data.result.stages.length) renderStage();
  else $('output').textContent = 'No validated intermediate representation was produced. See diagnostics below.';
  status(data.result.ok
    ? `Compilation completed in ${Math.round(data.elapsedMs)} ms. Compiler checks passed; no proof verification was performed.`
    : `Compilation rejected. ${data.result.stages.length ? 'Completed stages remain available above. ' : ''}See diagnostics below.`,
    data.result.ok ? 'success' : 'error');
}
function compile() {
  if (busy) return;
  if (new TextEncoder().encode(source.value).length > 32768) { status('Source exceeds the 32 KiB limit.', 'error'); return; }
  invalidate();
  busy = true;
  $('compile').disabled = true;
  $('cancel').disabled = false;
  const submitted = source.value;
  status('Loading the compiler. The first download can take a moment.');
  try {
    if (!worker) worker = new Worker(new URL('./explorer-worker.mjs', import.meta.url), { type: 'module' });
  } catch (error) { end(); status(`Could not start the compiler: ${error.message}. Please retry.`, 'error'); return; }
  const activeWorker = worker;
  const timeout = ms => {
    clearTimeout(timer);
    timer = setTimeout(() => {
      worker?.terminate(); worker = null; end();
      status('The compiler exceeded its time limit. Try a smaller program or retry the download.', 'error');
    }, ms);
  };
  timeout(120000);
  worker.onmessage = ({ data }) => {
    if (worker !== activeWorker) return;
    if (data.type === 'progress') {
      const percent = Math.floor(100 * data.received / data.total);
      status(percent < 100 ? `Loading compiler: ${percent}% of ${(data.total / 1048576).toFixed(1)} MiB…` : 'Preparing the compiler…');
      return;
    }
    if (data.type === 'loading') return;
    if (data.type === 'compiling') { status('Compiling on your device…'); timeout(20000); return; }
    end();
    if (data.type === 'error') {
      worker.terminate(); worker = null;
      status(data.message, 'error');
    } else showResult(data, submitted);
  };
  worker.onerror = event => {
    if (worker !== activeWorker) return;
    worker?.terminate(); worker = null; end();
    status(`Could not start the browser compiler: ${event.message || 'worker error'}`, 'error');
  };
  worker.postMessage({ source: submitted });
}
$('compile').addEventListener('click', compile);
$('cancel').addEventListener('click', () => { worker?.terminate(); worker = null; end(); status('Compilation cancelled.'); });
source.addEventListener('input', invalidate);
source.addEventListener('keydown', event => {
  if ((event.ctrlKey || event.metaKey) && event.key === 'Enter') { event.preventDefault(); compile(); }
});
$('example').addEventListener('change', () => { source.value = examples[$('example').value]; invalidate(); });
$('stage').addEventListener('change', renderStage);
$('function').addEventListener('change', () => {
  const stage = artifact.result.stages[Number($('stage').value)];
  const fn = stage.functions[$('function').value];
  $('output').textContent = fn ? fn.text : stage.text;
  if (fn?.line > 0) {
    const lines = source.value.split('\n');
    const start = lines.slice(0, fn.line - 1).reduce((n, line) => n + line.length + 1, 0);
    source.focus();
    source.setSelectionRange(start, start + (lines[fn.line - 1]?.length || 0));
  }
});
$('download').addEventListener('click', () => {
  if (!artifact) return;
  const url = URL.createObjectURL(new Blob([JSON.stringify(artifact, null, 2)], { type: 'application/json' }));
  const a = document.createElement('a'); a.href = url; a.download = 'concrete-explorer.json'; a.click();
  setTimeout(() => URL.revokeObjectURL(url), 1000);
});

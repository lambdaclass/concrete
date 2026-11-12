#!/usr/bin/env python3
"""Fresh, isolated Lean census. Also usable on the CI server without git or Lake dependencies."""
import json
import os
from pathlib import Path
import subprocess
import sys
import shutil
import tempfile


def run(args, cwd, env):
    p = subprocess.run(args, cwd=cwd, env=env, text=True, capture_output=True)
    if p.returncode:
        raise RuntimeError(f"{' '.join(map(str, args))}:\n{p.stdout}{p.stderr}")
    return p.stdout


def census(root, exporter, examples=True, regenerate_modules=(), validator=None):
    # Never rewrite caller sources or artifacts, even when instrumentation fails.
    with tempfile.TemporaryDirectory(prefix='landing-census-') as tmp:
        scratch = Path(tmp)
        shutil.copytree(Path(root)/'ConcreteSpec', scratch/'ConcreteSpec',
                        ignore=shutil.ignore_patterns('*.olean', '*.ilean', '*.ir'))
        shutil.copy2(Path(root)/'ConcreteSpec.lean', scratch/'ConcreteSpec.lean')
        return _census(scratch, Path(exporter).resolve(), examples, regenerate_modules, validator)


def _census(root, exporter, include_examples, regenerate_modules, validator=None):
    root = Path(root).resolve()
    env = dict(os.environ, LEAN_PATH=str(root))
    files = sorted(root.glob('ConcreteSpec/**/*.lean')) + [root / 'ConcreteSpec.lean']
    records, imports, modules = [], {}, {}
    rels = [file.relative_to(root).as_posix() for file in files]
    parsed = [json.loads(s) for s in run(
        ['lean', '--run', str(exporter), 'parse', *rels], root, env).splitlines()]
    for rel in rels:
        modules[rel[:-5].replace('/', '.')] = rel
    for r in parsed:
        if r['record'] == 'imports':
            imports[r['file'][:-5].replace('/', '.')] = r['imports']
    records.extend(parsed)
    order, active = [], set()
    def visit(m):
        if m in active:
            raise ValueError(f'import cycle: {m}')
        if m in order:
            return
        active.add(m)
        for dep in imports[m]:
            if dep in modules:
                visit(dep)
            elif dep != 'Lean' and not dep.startswith(('Lean.', 'Init', 'Std')):
                raise ValueError(f'unknown snapshot dependency: {dep}')
        active.remove(m)
        order.append(m)
    for m in modules:
        visit(m)
    def build(instrumented=False):
        # Every module is fresh, including modules outside the umbrella import closure.
        for m in order:
            src = modules[m]
            out = root / (src[:-5] + '.olean')
            print(f'build {m}', file=sys.stderr, flush=True)
            run(['lean', '-DautoImplicit=false', '-DwarningAsError=true',
                 *(['-Dlinter.defProp=false'] if instrumented else []), '-o', str(out), src], root, env)
    module_file = root / 'inventory-modules.txt'
    module_file.write_text('\n'.join(order) + '\n')
    def constants():
        return [json.loads(s) for s in run(
            ['lean', '--run', str(exporter), 'constants', str(module_file)], root, env).splitlines()]
    build()
    if validator is not None: validator(root, parsed)
    original = constants()
    records.extend(original)
    requests = root/'inventory-realizations.txt'
    requests.write_text('\n'.join(sorted(d['name'] for d in original if d['line']==0 and d['kind']=='theorem'))+'\n')
    for m in order:
        requested = str(requests) if m in regenerate_modules else '-'
        records.extend(json.loads(s) for s in run(
            ['lean', '--run', str(exporter), 'origins', modules[m], m, requested], root, env).splitlines())
        if m in regenerate_modules:
            records.append({'record':'command','file':modules[m], 'line':0,'endLine':0,
                            'syntax':'module-realization','example':False})
    if not include_examples:
        return records
    names = {d['name'] for d in original}
    # Examples are erased from the persistent environment. Give each a private name in a
    # disposable second build; Lean still elaborates its exact binders/type/body/context.
    # Keyword positions come from Lean syntax, never a source-text search.
    examples = {}
    for rel in rels:
        cs = [c for c in parsed if c['record']=='command' and c['file']==rel and c['example']]
        body = (root/rel).read_bytes()
        for c in sorted(cs, key=lambda c:c['exampleToken'], reverse=True):
            pos = c['exampleToken']
            if body[pos:pos+7] != b'example':
                raise ValueError(f'example token mismatch: {rel}:{pos}')
            marker = f'__landing_example_{pos}'
            replacement = f'def {marker}'.encode()
            insertion = c['noncomputableInsert'] if c['noncomputableInsert'] is not None else pos
            if not c['noncomputable'] and insertion==pos:
                replacement = b'noncomputable '+replacement
            body = body[:pos] + replacement + body[pos+7:]
            if not c['noncomputable'] and insertion<pos:
                body = body[:insertion]+b'noncomputable '+body[insertion:]
            examples[(rel[:-5].replace('/','.'),marker)] = c
        (root/rel).write_bytes(body)
    build(instrumented=True)
    instrumented = constants()
    byname = {d['name']:d for d in instrumented}
    def used(n, seen):
        if n in names:
            return {n}
        if n in seen or n not in byname:
            return set()
        seen.add(n)
        result = set()
        for dep in byname[n]['uses']:
            result |= used(dep, seen)
        return result
    for d in instrumented:
        marker = d['userName'].rsplit('.',1)[-1]
        key = (d['module'],marker)
        if key in examples:
            c = examples.pop(key)
            records.append({'record':'example', 'file':c['file'], 'line':c['line'],
                            'endLine':c['endLine'], 'uses':sorted(used(d['name'],set()))})
    if examples:
        raise ValueError('instrumented examples absent from environment: '+str(sorted(examples)))
    return records


if __name__ == '__main__':
    try:
        result = census(sys.argv[1], Path(sys.argv[2]).resolve())
        Path(sys.argv[3]).write_text(''.join(json.dumps(r, sort_keys=True) + '\n' for r in result))
    except (RuntimeError, ValueError) as exc:
        sys.exit(str(exc))

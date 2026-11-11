#!/usr/bin/env python3
"""Check exact direct imports, including constants in anonymous examples.

Run inside nix develop after building. --write updates import commands only,
retaining comments and complete declarations. The required trust gate runs this
check and its elaborated negative controls; the root owns population reachability.
"""
import argparse
from pathlib import Path
import subprocess
import sys
import tempfile
from lean_lex import EXAMPLE, lex, module_imports

ROOT = Path(__file__).resolve().parents[2]
MODULES = {'ConcreteSpec'} | {
    '.'.join(p.relative_to(ROOT).with_suffix('').parts)
    for p in (ROOT / 'ConcreteSpec').rglob('*.lean')}


def run(command):
    result = subprocess.run(
        ['bash', str(ROOT / 'scripts/checks/lib/bounded.sh'), '300', *command],
        cwd=ROOT, text=True, capture_output=True)
    if result.returncode:
        print(result.stdout, end='', file=sys.stderr)
        print(result.stderr, end='', file=sys.stderr)
        raise SystemExit(result.returncode)
    return result.stdout


def dependencies(source, temp):
    _, _, end = module_imports(source, MODULES)
    # Rewrite only example command tokens; all declarations and context survive.
    body = source[end:]
    for i, match in reversed(list(enumerate(EXAMPLE.finditer(lex(body, literals=False))))):
        body = body[:match.start()] + f'noncomputable def «⟪import-example⟫{i}»' + body[match.end():]
    temp.write_text('import Lean\n' + source[:end] +
                    'set_option linter.defProp false\n' + body + '\n' +
                    (ROOT / 'scripts/proofs/imports.lean').read_text())
    output = run(['lake', 'env', 'lean', '-DwarningAsError=true',
                  '-DautoImplicit=false', str(temp)])
    deps = sorted(line.removeprefix('IMPORT ') for line in output.splitlines()
                  if line.startswith('IMPORT '))
    if not deps:
        raise SystemExit('imports: empty dependency population')
    return deps


def matches(source, deps):
    actual, _, _ = module_imports(source, MODULES)
    return sorted(actual) == deps


def rewrite(source, deps):
    _, spans, _ = module_imports(source, MODULES)
    if not spans:
        raise SystemExit('imports: missing import header')
    # Retain every byte outside import commands, including interleaved comments.
    for i, (start, end) in reversed(list(enumerate(spans))):
        replacement = '\n'.join(f'import {dep}' for dep in deps) if i == 0 else ''
        source = source[:start] + replacement + source[end:]
    return source


def controls(temp):
    # Brand imports Basics itself: removing the explicit Basics import still
    # elaborates. Both an extra import and a transitive-only dependency must fail.
    good = ('-- leading comment\nimport ConcreteSpec.Spec.Basics -- keep me\n'
            '/- between imports -/\n\nimport ConcreteSpec.Spec.Brand\n'
            'open ConcreteSpec\n'
            'def importControl := Core.AList.lookup (κ := Nat) (α := Nat)\n')
    good += 'def brandControl : Brand.STy := .heap 0\n'
    deps = dependencies(good, temp)
    if not matches(good, deps):
        raise SystemExit('positive direct-import control rejected')
    extra = 'import ConcreteSpec.Spec.Authority\n' + good
    if matches(extra, dependencies(extra, temp)):
        raise SystemExit('unused-import control accepted')
    canonical = rewrite(good, deps)
    if not matches(canonical, dependencies(canonical, temp)):
        raise SystemExit('header rewrite failed')
    if '-- keep me' not in canonical or '/- between imports -/' not in canonical:
        raise SystemExit('header rewrite lost comments')
    missing = good.replace('import ConcreteSpec.Spec.Basics', '')
    # Removing just the command retains its trailing comment and legal header.
    if matches(missing, dependencies(missing, temp)):
        raise SystemExit('missing-direct-import control accepted')
    print('imports: extra, transitive-only, commented-header and rewrite controls pass')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--write', action='store_true')
    args = parser.parse_args()
    run(['lake', 'build', 'ConcreteSpec'])
    failed = False
    with tempfile.TemporaryDirectory(prefix='spec-imports-') as tmp:
        temp = Path(tmp) / 'Probe.lean'
        controls(temp)
        for path in sorted((ROOT / 'ConcreteSpec/Proofs').glob('*.lean')):
            source = path.read_text()
            deps = dependencies(source, temp)
            print(f'{path.relative_to(ROOT)}: {", ".join(deps)}', flush=True)
            if not matches(source, deps):
                if args.write:
                    path.write_text(rewrite(source, deps))
                else:
                    failed = True
                    print('  import header differs from direct constant dependencies', flush=True)
    return int(failed)


if __name__ == '__main__':
    sys.exit(main())

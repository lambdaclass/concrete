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
from lean_lex import module_imports, rename_examples

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


def inspect(module, temp):
    return run(["lake", "env", "env",
                "LEAN_PATH=" + str(temp.parent) + ":" +
                run(["lake", "env", "printenv", "LEAN_PATH"]).strip(),
                "lean", "--run", str(ROOT / "scripts/proofs/imports.lean"), module])


def dependencies(source, temp, built_module=None):
    module_imports(source, MODULES)
    temp.write_text(rename_examples(source, lambda line: f'⟪import-example⟫{line}'))
    search = str(temp.parent) + ':' + run(['lake', 'env', 'printenv', 'LEAN_PATH']).strip()
    run(['lake', 'env', 'env', 'LEAN_PATH=' + search, 'lean', '-DwarningAsError=true',
         '-DautoImplicit=false', '-R', str(temp.parent), '-o', str(temp.with_suffix('.olean')), str(temp)])
    output = inspect(temp.stem, temp)
    if built_module:
        built = inspect(built_module, temp)
        def named(text):
            return sorted(line for line in text.splitlines() if line.startswith('NAMED '))
        if named(output) != named(built):
            raise SystemExit(f'probe/built .olean dependency mismatch: {built_module}\n'
                             f'probe: {named(output)}\nbuilt: {named(built)}')
    deps = sorted(line.removeprefix('IMPORT ') for line in output.splitlines()
                  if line.startswith('IMPORT '))
    if not deps:
        raise SystemExit('imports: empty dependency population')
    return deps


def matches(source, deps):
    actual, _, _ = module_imports(source, MODULES)
    return sorted(actual) == deps


def rewrite(source, deps):
    actual, spans, _ = module_imports(source, MODULES)
    if not spans:
        raise SystemExit('imports: missing import header')
    # Keep each surviving command in its original slot: its trailing comment
    # describes that import, not an arbitrary member of a sorted replacement.
    remaining = set(deps)
    edits = []
    for module, (start, end) in zip(actual, spans):
        if module in remaining:
            remaining.remove(module)
            continue
        line_end = source.find('\n', end)
        line_end = len(source) if line_end < 0 else line_end + 1
        # Remove the newline only for a line with no trailing comment. Otherwise
        # leave the comment exactly where it was, including whitespace and EOL.
        stop = line_end if not source[end:line_end].strip() else end
        edits.append((start, stop, ''))
    newline = '\r\n' if '\r\n' in source else '\n'
    if remaining:
        edits.append((spans[0][0], spans[0][0],
                      ''.join(f'import {dep}{newline}' for dep in sorted(remaining))))
    for start, end, replacement in sorted(edits, reverse=True):
        source = source[:start] + replacement + source[end:]
    return source


def proof_paths(root):
    paths = sorted((root / 'ConcreteSpec/Proofs').rglob('*.lean'))
    if not paths:
        raise SystemExit('imports: empty proof-module population')
    return paths


def declaring_owner_control(temp):
    # Keep both variants in isolated search paths: no stale realized lemmas may
    # leak from the pre-realized defining module into the lazy variant.
    for realized in (False, True):
        with tempfile.TemporaryDirectory(prefix='import-owner-') as directory:
            root = Path(directory)
            module_dir = root / 'ConcreteSpec'
            module_dir.mkdir()
            sources = {
                'ImportOwnerB': 'def ConcreteSpec.zg (n : Nat) := n\n',
                'ImportOwnerA': (
                    'import ConcreteSpec.ImportOwnerB\n'
                    'def ConcreteSpec.zf : Nat → Nat | 0 => ConcreteSpec.zg 0 | n+1 => n\n'
                    + ('theorem zf1 : ConcreteSpec.zf 1 = 0 := by rw [ConcreteSpec.zf.eq_2]\n'
                       if realized else '')),
            }
            synthetic = {f'ConcreteSpec.{name}' for name in sources}
            MODULES.update(synthetic)
            try:
                search = str(root) + ':' + run(['lake', 'env', 'printenv', 'LEAN_PATH']).strip()
                def compile_source(path, source):
                    path.write_text(source)
                    run(['lake', 'env', 'env', 'LEAN_PATH=' + search,
                         'lean', '-DwarningAsError=true', '-DautoImplicit=false',
                         '-R', str(root), '-o', str(path.with_suffix('.olean')), str(path)])
                for name, source in sources.items():
                    compile_source(module_dir / f'{name}.lean', source)
                for declaration in ('theorem zt', 'example'):
                    body = declaration + ' : ConcreteSpec.zf 1 = 0 := by rw [ConcreteSpec.zf.eq_2]\n'
                    minimal = 'import ConcreteSpec.ImportOwnerA\nopen ConcreteSpec\n' + body
                    compile_source(root / 'Built.lean', minimal)
                    probe = root / 'Probe.lean'
                    for source in (minimal, 'import ConcreteSpec.ImportOwnerB\n' + minimal):
                        deps = dependencies(source, probe, 'Built')
                        if deps != ['ConcreteSpec.ImportOwnerA'] or matches(source, deps) != (source == minimal):
                            raise SystemExit(f'declaring-owner control failed: realized={realized}, {declaration}: {deps}')
            finally:
                MODULES.difference_update(synthetic)
    print('imports: declaring-owner controls pass (named/examples, lazy/pre-realized)')


def controls(temp):
    declaring_owner_control(temp)
    # Brand imports Basics itself: removing the explicit Basics import still
    # elaborates. Both an extra import and a transitive-only dependency must fail.
    good = ('-- leading comment\nimport ConcreteSpec.Spec.Basics -- keep me\n'
            '/- between imports -/\n\nimport ConcreteSpec.Spec.Brand\n'
            'open ConcreteSpec\n'
            'def importControl := Core.AList.lookup (κ := Nat) (α := Nat)\n')
    good += 'def brandControl : Brand.STy := .heap 0\n'
    scope = ('import ConcreteSpec.Spec.Slice1Syntax\n'
             'open ConcreteSpec\nnamespace ConcreteSpec\n'
             'def scopeControl : Name := "scope"\n')
    if dependencies(scope, temp) != ['ConcreteSpec.Spec.Slice1Syntax']:
        raise SystemExit('open namespace probe control failed')
    equation = ('open ConcreteSpec\nset_option linter.defProp false in def probeEq := @ConcreteSpec.runBody.eq_2\n')
    minimal = 'import ConcreteSpec.Spec.Slice1Semantics\n' + equation
    extra_equation = 'import ConcreteSpec.Proofs.Slice1\n' + minimal
    eq_deps = dependencies(minimal, temp)
    if dependencies(extra_equation, temp) != eq_deps:
        raise SystemExit('realized equation ownership depends on imports')
    extra_canonical = rewrite(extra_equation, eq_deps + ['ConcreteSpec.Proofs.Slice1'])
    if matches(extra_canonical, dependencies(extra_canonical, temp)):
        raise SystemExit('unneeded realization-module import accepted')
    if not matches(rewrite(minimal, eq_deps), dependencies(rewrite(minimal, eq_deps), temp)):
        raise SystemExit('canonical equation imports rejected')
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
    for newline in ('\n', '\r\n'):
        header = good.replace('import ConcreteSpec.Spec.Brand\n',
                              'import ConcreteSpec.Spec.Brand -- brand comment\n')
        header = header.replace('\n', newline)
        if rewrite(header, deps) != header:
            raise SystemExit('rewrite changed canonical header bytes')
        changed = rewrite('import ConcreteSpec.Spec.Authority' + newline + header,
                          deps + ['ConcreteSpec.Spec.Slice1Syntax'])
        if not matches(changed, deps + ['ConcreteSpec.Spec.Slice1Syntax']):
            raise SystemExit('rewrite produced incorrect imports')
        if rewrite(changed, deps + ['ConcreteSpec.Spec.Slice1Syntax']) != changed:
            raise SystemExit('header rewrite is not idempotent')
        if '-- keep me' + newline not in changed or '-- brand comment' + newline not in changed:
            raise SystemExit('rewrite changed trailing comments or line endings')
    with tempfile.TemporaryDirectory(prefix='proof-path-control-') as directory:
        root = Path(directory)
        try:
            proof_paths(root)
        except SystemExit:
            pass
        else:
            raise SystemExit('empty proof population control accepted')
        nested = root / 'ConcreteSpec/Proofs/Nested/Proof.lean'
        nested.parent.mkdir(parents=True)
        nested.touch()
        if proof_paths(root) != [nested]:
            raise SystemExit('recursive proof population control failed')
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
        for path in proof_paths(ROOT):
            source = path.read_bytes().decode('utf-8')
            deps = dependencies(source, temp, '.'.join(path.relative_to(ROOT).with_suffix('').parts))
            print(f'{path.relative_to(ROOT)}: {", ".join(deps)}', flush=True)
            if not matches(source, deps):
                if args.write:
                    path.write_bytes(rewrite(source, deps).encode('utf-8'))
                else:
                    failed = True
                    print('  import header differs from direct constant dependencies', flush=True)
    return int(failed)


if __name__ == '__main__':
    sys.exit(main())

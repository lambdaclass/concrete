#!/usr/bin/env python3
"""Derive exact direct imports from elaborated constants, including anonymous examples.

Run inside nix develop after lake build ConcreteSpec. --write updates only import headers;
without it, fail on any difference. The root (not a proof file) owns population reachability.
Temporary sources retain complete commands and context. Naming examples preserves their terms
for inspection; generated helpers and types are included rather than guessed from source names.
"""
import argparse
from pathlib import Path
import subprocess
import tempfile
from lean_lex import EXAMPLE, lex

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--write', action='store_true')
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
probe = (root / 'scripts/proofs/imports.lean').read_text()
# Never inspect stale imported declarations.
subprocess.run(['lake', 'build', 'ConcreteSpec'], cwd=root, check=True)
failed = False
with tempfile.TemporaryDirectory(prefix='spec-imports-') as tmp:
    for path in sorted((root / 'ConcreteSpec/Proofs').glob('*.lean')):
        source = path.read_text()
        # Existing headers follow the repository's one-module-per-line grammar,
        # independently enforced by scripts/checks/spec_imports.sh.
        lines = source.splitlines(keepends=True)
        end = 0
        while end < len(lines) and lines[end].startswith('import '):
            end += 1
        if not end:
            raise SystemExit(f'{path}: missing import header')
        named = source
        for i, match in reversed(list(enumerate(EXAMPLE.finditer(lex(source, literals=False))))):
            named = named[:match.start()] + f'noncomputable def «⟪import-example⟫{i}»' + named[match.end():]
        temp = Path(tmp) / path.name
        # The option must follow the entire import header.
        temp.write_text('import Lean\n' + ''.join(lines[:end]) +
                        'set_option linter.defProp false\n' +
                        named[len(''.join(lines[:end])):] + '\n' + probe)
        result = subprocess.run(['lake', 'env', 'lean', '-DwarningAsError=true', '-DautoImplicit=false', str(temp)],
                                cwd=root, text=True, capture_output=True, check=True)
        deps = sorted(line.removeprefix('IMPORT ') for line in result.stdout.splitlines()
                      if line.startswith('IMPORT '))
        if not deps:
            raise SystemExit(f'{path}: empty dependency population')
        expected = ''.join(f'import {dep}\n' for dep in deps)
        actual = ''.join(lines[:end])
        print(f'{path.relative_to(root)}: {", ".join(deps)}', flush=True)
        if expected != actual:
            if args.write:
                path.write_text(expected + ''.join(lines[end:]))
            else:
                failed = True
                print('  import header differs from direct constant dependencies', flush=True)
if failed:
    raise SystemExit(1)

#!/usr/bin/env bash
# No specification or proof module may hide its dependencies behind an umbrella.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
cd "$(dirname "$0")/../.."
python3 -I - <<'PY'
import sys
from pathlib import Path
sys.path.insert(0, str(Path('scripts/proofs').resolve()))
from lean_lex import lex, ModuleHeader, FIRST_COMMAND, LexError

modules = {'ConcreteSpec'} | {
    '.'.join(p.with_suffix('').parts) for p in Path('ConcreteSpec').rglob('*.lean')}
header = ModuleHeader(modules)

def imports(source):
    lines = lex(source, literals=False).splitlines()
    result = []
    k = 0
    while k < len(lines) and (not lines[k].strip() or header.match(lines[k])):
        if lines[k].strip():
            result.append(lines[k][7:].strip())
        k += 1
    if k < len(lines) and not FIRST_COMMAND.match(lines[k]):
        raise ValueError('invalid import header terminator')
    if any(line.lstrip().startswith('import') for line in lines[k:]):
        raise ValueError('import outside header')
    return result

def check(source, basics=False):
    deps = imports(source)
    if any(m in {'ConcreteSpec', 'ConcreteSpec.Spec'} for m in deps):
        raise ValueError('umbrella import')
    if basics and deps != ['ConcreteSpec.Spec.Basics']:
        raise ValueError('Basics must import only ConcreteSpec.Spec.Basics')

suffix = '\nopen ConcreteSpec\n'
check('import ConcreteSpec.Spec.Basics' + suffix, True)
check('import ConcreteSpec.Spec.Basics\n/- nested /- comment -/ -/\n'
      'def literal := r#"/- -- import ConcreteSpec"#\n', True)
for source, basics in [
    ('import ConcreteSpec.Spec' + suffix, False),
    ('import ConcreteSpec' + suffix, False),
    ('import ConcreteSpec.Spec.Basics ConcreteSpec.Spec' + suffix, False),
    ('import\nConcreteSpec.Spec' + suffix, False),
    ('/- nested /- comment -/ -/ import ConcreteSpec.Spec' + suffix, False),
    ('import ConcreteSpec.Spec.Authority' + suffix, True),
]:
    try:
        check(source, basics)
    except (ValueError, LexError):
        pass
    else:
        raise SystemExit('spec_imports: negative control accepted: ' + source)
paths = sorted(Path('ConcreteSpec').rglob('*.lean'))
for path in paths:
    try:
        check(path.read_text(), path == Path('ConcreteSpec/Proofs/Basics.lean'))
        # Exercise the umbrella control against every current module, including future files.
        try:
            check('import ConcreteSpec.Spec\n' + path.read_text())
        except ValueError as e:
            if str(e) != 'umbrella import':
                raise
        else:
            raise ValueError('per-module umbrella control accepted')
    except (ValueError, LexError) as e:
        raise SystemExit(f'spec_imports: {path}: {e}')
print(f'spec_imports: {len(paths)} module headers and negative controls pass')
PY

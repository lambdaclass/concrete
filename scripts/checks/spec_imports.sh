#!/usr/bin/env bash
# No specification or proof module may hide its dependencies behind an umbrella.
# The capability base is a leaf: higher authority/grant/primitive models depend on it.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
cd "$(dirname "$0")/../.."
python3 -I - <<'PY'
import sys
from pathlib import Path
sys.path.insert(0, str(Path('scripts/proofs').resolve()))
from lean_lex import module_imports, LexError

modules = {'ConcreteSpec'} | {
    '.'.join(p.with_suffix('').parts) for p in Path('ConcreteSpec').rglob('*.lean')}

def check(source, basics=False, spec=False, capability_base=False):
    deps, _, _ = module_imports(source, modules)
    if capability_base and deps:
        raise ValueError('capability base must have no imports')
    if spec and any(m.startswith("ConcreteSpec.Proofs.") for m in deps):
        raise ValueError("spec imports proof")
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
# Check the same leaf rule used on the real source against every current module.
# A comment or string mentioning Authority is not a dependency.
check('/- import ConcreteSpec.Spec.Authority -/\n'
      'def literal := "import ConcreteSpec.Spec.Authority"\n', capability_base=True)
for dep in sorted(modules):
    try:
        check('import ' + dep + suffix, capability_base=True)
    except ValueError as e:
        if str(e) != 'capability base must have no imports':
            raise
    else:
        raise SystemExit('spec_imports: capability-base control accepted: ' + dep)
paths = sorted(Path('ConcreteSpec').rglob('*.lean'))
for path in paths:
    try:
        check(path.read_text(), path == Path('ConcreteSpec/Proofs/Basics.lean'),
              path == Path('ConcreteSpec/Spec.lean') or Path('ConcreteSpec/Spec') in path.parents,
              path == Path('ConcreteSpec/Spec/CapabilityBase.lean'))
        if path == Path('ConcreteSpec/Spec.lean') or Path('ConcreteSpec/Spec') in path.parents:
            try:
                check('import ConcreteSpec.Proofs.Basics\n' + path.read_text(), spec=True)
            except ValueError as e:
                if str(e) != 'spec imports proof':
                    raise
            else:
                raise ValueError('spec-to-proof control accepted')
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

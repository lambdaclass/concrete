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
from lean_lex import FIRST_COMMAND_TACTICS, module_imports, LexError

spec_paths = sorted(Path('ConcreteSpec').rglob('*.lean'))
evidence_paths = sorted(Path('ConcreteSpecEvidence').rglob('*.lean'))
modules = {'ConcreteSpec'} | {'.'.join(p.with_suffix('').parts) for p in spec_paths}
evidence_modules = modules | {'ConcreteSpecEvidence'} | {
    '.'.join(p.with_suffix('').parts) for p in evidence_paths}

def check(source, basics=False, spec=False, capability_base=False, evidence=False, tactics=False):
    deps, _, _ = module_imports(
        source,
        evidence_modules if evidence else modules,
        allow_quotations=tactics,
        first_command=FIRST_COMMAND_TACTICS if tactics else None,
    )
    if capability_base and deps:
        raise ValueError('capability base must have no imports')
    if tactics and any(not m.startswith('ConcreteSpec.Tactics.') for m in deps):
        raise ValueError('tactics module may import only ConcreteSpec.Tactics.*')
    if spec and any(m.startswith("ConcreteSpec.Proofs.") for m in deps):
        raise ValueError("spec imports proof")
    if spec and any(m == "ConcreteSpec.Tactics" or m.startswith("ConcreteSpec.Tactics.") for m in deps):
        raise ValueError("spec imports tactics")
    if any(m in {'ConcreteSpec', 'ConcreteSpec.Spec', 'ConcreteSpecEvidence'} for m in deps):
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
# Positive and negative layering controls for ConcreteSpec/Tactics/.
if 'ConcreteSpec.Tactics' in modules:
    tactics_suffix = '\nnamespace ConcreteSpec.Tactics\nend ConcreteSpec.Tactics\n'
    check('import ConcreteSpec.Tactics' + suffix)
    for bad_tactics in (
        'import ConcreteSpec.Spec.Basics' + tactics_suffix,
        'import ConcreteSpec.Proofs.Basics' + tactics_suffix,
        'import ConcreteSpec.Tactics' + tactics_suffix,
    ):
        try:
            check(bad_tactics, tactics=True)
        except ValueError as e:
            if str(e) != 'tactics module may import only ConcreteSpec.Tactics.*':
                raise
        else:
            raise SystemExit('spec_imports: tactics-layer control accepted: ' + bad_tactics)
    for bad_meta_import in ('import Lean.Elab' + tactics_suffix, 'import Lean.Meta' + tactics_suffix):
        try:
            check(bad_meta_import, tactics=True)
        except (ValueError, LexError):
            pass
        else:
            raise SystemExit('spec_imports: tactics meta-import control accepted: ' + bad_meta_import)
    for bad_spec in ('import ConcreteSpec.Tactics' + suffix, 'import ConcreteSpec.Tactics.Option' + suffix):
        if bad_spec.split()[1] in modules:
            try:
                check(bad_spec, spec=True)
            except ValueError as e:
                if str(e) != 'spec imports tactics':
                    raise
            else:
                raise SystemExit('spec_imports: spec-to-tactics control accepted: ' + bad_spec)
paths = spec_paths + evidence_paths
for path in paths:
    evidence = path.parts[0] == 'ConcreteSpecEvidence'
    is_tactics = path == Path('ConcreteSpec/Tactics.lean') or Path('ConcreteSpec/Tactics') in path.parents
    is_spec = path == Path('ConcreteSpec/Spec.lean') or Path('ConcreteSpec/Spec') in path.parents
    try:
        check(path.read_text(), path == Path('ConcreteSpec/Proofs/Basics.lean'),
              is_spec,
              path == Path('ConcreteSpec/Spec/CapabilityBase.lean'),
              evidence=evidence,
              tactics=is_tactics)
        if is_spec:
            try:
                check('import ConcreteSpec.Proofs.Basics\n' + path.read_text(), spec=True)
            except ValueError as e:
                if str(e) != 'spec imports proof':
                    raise
            else:
                raise ValueError('spec-to-proof control accepted')
            if 'ConcreteSpec.Tactics' in modules:
                try:
                    check('import ConcreteSpec.Tactics\n' + path.read_text(), spec=True)
                except ValueError as e:
                    if str(e) != 'spec imports tactics':
                        raise
                else:
                    raise ValueError('spec-to-tactics control accepted')
        # Exercise the umbrella control against every current module, including future files.
        for root in (('ConcreteSpec.Spec', 'ConcreteSpecEvidence') if evidence else ('ConcreteSpec.Spec',)):
            try:
                check('import ' + root + '\n' + path.read_text(), evidence=evidence, tactics=is_tactics)
            except ValueError as e:
                expected = 'tactics module may import only ConcreteSpec.Tactics.*' if is_tactics else 'umbrella import'
                if str(e) != expected:
                    raise
            else:
                raise ValueError('per-module umbrella control accepted')
    except (ValueError, LexError) as e:
        raise SystemExit(f'spec_imports: {path}: {e}')
print(f'spec_imports: {len(paths)} module headers and negative controls pass')
PY

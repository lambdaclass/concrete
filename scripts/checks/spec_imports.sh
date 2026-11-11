#!/usr/bin/env bash
# Basics is shared proof support: its import header must depend only on Spec.Basics.
# Keep this in the root-check glob so broadening it cannot silently couple every proof to the spec.
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib/gate_env.sh" || exit 2
cd "$(dirname "$0")/../.."
python3 -I - <<'PY'
from pathlib import Path


def narrow_basics(source):
    # Strip Lean comments (including nested block comments) before judging the header.
    out = []
    depth = 0
    i = 0
    while i < len(source):
        pair = source[i:i + 2]
        if pair == '/-':
            depth += 1
            out.append(' ')
            i += 2
        elif depth and pair == '-/':
            depth -= 1
            i += 2
        elif depth:
            i += 1
        elif pair == '--':
            end = source.find('\n', i)
            i = len(source) if end < 0 else end
        else:
            out.append(source[i])
            i += 1
    if depth:
        return False
    tokens = ''.join(out).split()
    # The first declaration closes the import header. Requiring it here fails closed on
    # additional imports, including multiple modules on one line or an alternate umbrella.
    return tokens[:4] == ['import', 'ConcreteSpec.Spec.Basics',
                          'namespace', 'ConcreteSpec.Core.AList']


suffix = '\nnamespace ConcreteSpec.Core.AList\nend ConcreteSpec.Core.AList\n'
assert narrow_basics('import ConcreteSpec.Spec.Basics' + suffix)
for header in ('import ConcreteSpec.Spec', 'import ConcreteSpec',
               'import ConcreteSpec.Spec.Basics ConcreteSpec.Spec',
               'import ConcreteSpec.Spec.Basics\nimport ConcreteSpec.Spec.Authority',
               'import\nConcreteSpec.Spec',
               '/- outer /- nested -/ -/ import ConcreteSpec.Spec'):
    if narrow_basics(header + suffix):
        raise SystemExit('spec_imports: negative control accepted: ' + header)
if not narrow_basics(Path('ConcreteSpec/Proofs/Basics.lean').read_text()):
    raise SystemExit('spec_imports: Basics must import only ConcreteSpec.Spec.Basics')
print('spec_imports: narrow Basics header and negative controls pass')
PY

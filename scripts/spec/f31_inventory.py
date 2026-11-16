#!/usr/bin/env python3
"""F31 migration tool; retire with both census tools and the summary when F31 closes.

Regenerate full inventories into a caller-selected directory; compare only their compact summary.
The source registries remain the owners. --update explicitly accepts a reviewed inventory change.
"""
import argparse
from collections import Counter
import hashlib
import json
from pathlib import Path
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--update', action='store_true')
parser.add_argument('--output', type=Path, help='retain full inventories for F31 migration')
args = parser.parse_args()
summary_path = Path('docs/proposals/f31-summary.json')
with tempfile.TemporaryDirectory(prefix='f31-inventory-') as temporary:
    output = args.output or Path(temporary)
    output.mkdir(parents=True, exist_ok=True)
    for path in Path('scripts/spec').glob('f31_*.py'):
        compile(path.read_text(), str(path), 'exec')
    result = subprocess.run(['lake', 'env', 'lean', 'scripts/spec/f31_consumers.lean'],
                            check=True, text=True, capture_output=True)
    # Lean routes #eval IO diagnostics to stdout on some toolchain versions.
    registry = []
    declarations = []
    for line in result.stdout.splitlines():
        (registry if line.startswith('F31-REGISTRY\t') else declarations).append(line)
    registry += [line for line in result.stderr.splitlines() if line.startswith('F31-REGISTRY\t')]
    if not registry:
        raise SystemExit('F31: empty live premise registry')
    (output / 'registry.txt').write_text('\n'.join(registry) + '\n')
    decls = json.loads('\n'.join(declarations))
    (output / 'declarations.json').write_text(json.dumps(decls, indent=2, ensure_ascii=False) + '\n')
    surface = subprocess.run(['python3', '-I', 'scripts/spec/f31_surface.py', str(output / 'registry.txt')],
                             check=True, text=True, capture_output=True)
    rows = json.loads(surface.stdout)
    (output / 'surface.json').write_text(surface.stdout)
    summary = dict(
        declarations=len(decls), modules=len({r['module'] for r in decls}),
        kinds=dict(sorted(Counter(r['kind'] for r in decls).items())),
        roles=dict(sorted(Counter(r['role'] for r in decls).items())),
        surface=len(rows), surface_kinds=dict(sorted(Counter(r['kind'] for r in rows).items())),
        sha256={name: hashlib.sha256((output / name).read_bytes()).hexdigest()
                for name in ('declarations.json', 'surface.json')})
    rendered = json.dumps(summary, indent=2, sort_keys=True) + '\n'
    if args.update:
        summary_path.write_text(rendered)
    elif summary_path.read_text() != rendered:
        raise SystemExit('F31 inventory drift: regenerate with --output, inspect, then accept with --update')
    print(rendered, end='')

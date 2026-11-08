#!/usr/bin/env python3
"""Compare the stage outputs of two independent slice-0 builds, byte for byte.

usage: repro_compare.py [--stages A,B,...] [--inventory] [--required A,B,...]
                       [--label NAME] SIDE_A SIDE_B IDENTITY [MANIFEST]

Each SIDE holds `names` (the example inventory) and `out/NAME/STAGE` for every stage. Every selected
file is read on both sides: a missing file, a differing inventory or one differing byte fails. With
MANIFEST, writes the identity lines followed by one `SHA256  NAME/STAGE` line per artifact.
--inventory recursively compares every file below each program's output directory, and --required
adds assertions for files that must not be missing from both sides. --stages selects gate 8's
original explicit-list mode; its default is `slice0_repro`. --label changes the diagnostic prefix.
"""
import argparse
import hashlib
import sys
from pathlib import Path

STAGES = ['bir.txt', 'llvm.ll', 'lcb_job.txt', 'lcb_source.txt', 'lcb_body.ssa', 'lcb_body.s',
          'lcb_rt.ssa', 'lcb_rt.s', 'llvm.o', 'lcb_body.o', 'lcb_rt.o', 'llvm.exe', 'lcb.exe']


def names(value):
    if value is None:
        return []
    result = value.split(',')
    if any(not name or name.startswith('/') or '..' in Path(name).parts for name in result):
        raise ValueError('file lists must contain relative file paths')
    if len(set(result)) != len(result):
        raise ValueError('file lists contain a duplicate')
    return result


def inventory(root):
    if not root.is_dir():
        sys.exit(f'FAIL output directory missing ({root})')
    result = []
    for path in root.rglob('*'):
        relative = path.relative_to(root).as_posix()
        if path.is_file():
            result.append(relative)
        elif not path.is_dir():
            sys.exit(f'FAIL non-file artifact {relative}')
    return sorted(result)


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('--stages')
    parser.add_argument('--inventory', action='store_true')
    parser.add_argument('--required')
    parser.add_argument('--label', default='slice0_repro')
    parser.add_argument('side_a')
    parser.add_argument('side_b')
    parser.add_argument('identity')
    parser.add_argument('manifest', nargs='?')
    args = parser.parse_args(argv[1:])
    try:
        required = names(args.required)
        stages = names(args.stages) if args.stages is not None else STAGES
    except ValueError as error:
        parser.error(str(error))
    if args.inventory and args.stages is not None:
        parser.error('--inventory and --stages are alternative comparison modes')
    a, b, identity = Path(args.side_a), Path(args.side_b), Path(args.identity)
    names_bytes = (a / 'names').read_bytes()
    if names_bytes != (b / 'names').read_bytes():
        sys.exit(f'FAIL {args.label}: example inventory differs')
    if not names_bytes.strip():
        sys.exit(f'FAIL {args.label}: no examples')
    lines = identity.read_text().splitlines()
    for name in names_bytes.decode().splitlines():
        roots = [d / 'out' / name for d in (a, b)]
        selected = sorted(set(required + ([] if args.inventory else stages)))
        for stage in selected:
            try:
                hashes = [hashlib.sha256((root / stage).read_bytes()).hexdigest() for root in roots]
            except FileNotFoundError as error:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: missing ({error.filename})')
            if not args.inventory and hashes[0] != hashes[1]:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: {hashes[0]} != {hashes[1]}')
        if args.inventory:
            inventories = [inventory(root) for root in roots]
            if inventories[0] != inventories[1]:
                extra = sorted(set(inventories[0]) ^ set(inventories[1]))[0]
                side = 'second' if extra in inventories[1] else 'first'
                sys.exit(f'FAIL {args.label}: {name}: {extra}: inventory differs (only in {side} side)')
            selected = inventories[0]
        for stage in selected:
            hashes = [hashlib.sha256((root / stage).read_bytes()).hexdigest() for root in roots]
            if hashes[0] != hashes[1]:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: {hashes[0]} != {hashes[1]}')
            lines.append(f'{hashes[0]}  {name}/{stage}')
        print(f'ok {args.label}: {name}: all stages byte-identical')
    if args.manifest:
        Path(args.manifest).write_text('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main(sys.argv)

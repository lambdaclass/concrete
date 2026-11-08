#!/usr/bin/env python3
"""Compare the stage outputs of two independent slice-0 builds, byte for byte.

usage: repro_compare.py [--stages A,B,...] [--inventory] [--required A,B,...]
                       [--label NAME] SIDE_A SIDE_B IDENTITY [MANIFEST]

Each SIDE holds `names` (the example inventory) and `out/NAME/STAGE` for every stage. Top-level
`out/` entries are validated against `names` so unexpected top-level files or unlisted program
directories fail closed. Every selected file is read once on both sides in pipeline stage order,
comparing existence and bytes together per stage so the earliest differing stage is always reported
first. With MANIFEST, writes the identity lines followed by one `SHA256  NAME/STAGE` line per
artifact. --inventory recursively compares every file and directory below each program's output
directory (ordered by registered pipeline stage first, then discovered files lexicographically),
and --required adds assertions for files that must not be missing from both sides. --stages selects
gate 8's original explicit-list mode and preserves its declared stage sequence; its default is
`slice0_repro`. --label changes the diagnostic prefix.
"""
import argparse
import hashlib
import sys
from pathlib import Path

STAGES = ['bir.txt', 'llvm.ll', 'lcb_job.txt', 'lcb_source.txt', 'lcb_body.ssa', 'lcb_body.s',
          'lcb_rt.ssa', 'lcb_rt.s', 'llvm.o', 'lcb_body.o', 'lcb_rt.o', 'llvm.exe', 'lcb.exe']

PIPELINE_ORDER = [
    'bir.txt', 'llvm.ll', 'lcb_job.txt', 'lcb_source.txt', 'lcb_body.ssa', 'lcb_body.s',
    'lcb_rt.ssa', 'status', 'stderr',
    'lcb_rt.s', 'llvm.o', 'lcb_body.o', 'lcb_rt.o', 'llvm.exe', 'lcb.exe',
]


def names(value):
    if value is None:
        return []
    result = value.split(',')
    if any(not name or name.startswith('/') or '..' in Path(name).parts for name in result):
        raise ValueError('file lists must contain relative file paths')
    if len(set(result)) != len(result):
        raise ValueError('file lists contain a duplicate')
    return result


def validate_out_root(side_root, side_name, expected_names, label):
    out_dir = side_root / 'out'
    if not out_dir.is_dir():
        sys.exit(f'FAIL {label}: output directory missing ({out_dir})')
    expected_set = set(expected_names)
    for entry in sorted(out_dir.iterdir(), key=lambda p: p.name):
        if entry.is_symlink() or not entry.is_dir():
            sys.exit(
                f'FAIL {label}: {entry.name}: unexpected top-level file in out/ ({side_name} side)')
        if entry.name not in expected_set:
            sys.exit(
                f'FAIL {label}: {entry.name}: unexpected program directory in out/ ({side_name} side)')


def inventory(root, label, name):
    if not root.is_dir():
        sys.exit(f'FAIL {label}: {name}: output directory missing ({root})')
    files = []
    dirs = []
    for path in root.rglob('*'):
        relative = path.relative_to(root).as_posix()
        if path.is_symlink():
            sys.exit(f'FAIL {label}: {name}: {relative}: non-file artifact (symlink)')
        elif path.is_file():
            files.append(relative)
        elif path.is_dir():
            dirs.append(relative)
        else:
            sys.exit(f'FAIL {label}: {name}: {relative}: non-file artifact')
    return sorted(files), sorted(dirs)


def ordered_inventory_stages(required, files_a, files_b):
    discovered = set(files_a) | set(files_b)
    required_set = set(required)
    ordered = []
    seen = set()
    for stage in PIPELINE_ORDER:
        if stage in required_set or stage in discovered:
            ordered.append(stage)
            seen.add(stage)
    for stage in required:
        if stage not in seen:
            ordered.append(stage)
            seen.add(stage)
    for stage in sorted(discovered - seen):
        ordered.append(stage)
    return ordered


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
        stages = names(args.stages) if args.stages is not None else list(STAGES)
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
    example_names = names_bytes.decode().splitlines()
    if (any(not n or '/' in n or n in ('.', '..') for n in example_names)
            or len(set(example_names)) != len(example_names)):
        sys.exit(f'FAIL {args.label}: invalid example inventory')
    validate_out_root(a, 'first', example_names, args.label)
    validate_out_root(b, 'second', example_names, args.label)
    lines = identity.read_text().splitlines()
    required_set = set(required)
    for name in example_names:
        roots = [d / 'out' / name for d in (a, b)]
        if args.inventory:
            (files_a, dirs_a), (files_b, dirs_b) = (
                inventory(roots[0], args.label, name),
                inventory(roots[1], args.label, name),
            )
            if dirs_a != dirs_b:
                extra_dir = sorted(set(dirs_a) ^ set(dirs_b))[0]
                side = 'second' if extra_dir in set(dirs_b) else 'first'
                sys.exit(
                    f'FAIL {args.label}: {name}: {extra_dir}: '
                    f'inventory differs (only in {side} side)')
            selected = ordered_inventory_stages(required, files_a, files_b)
            if not selected:
                sys.exit(f'FAIL {args.label}: {name}: empty artifact inventory')
            present_a, present_b = set(files_a), set(files_b)
        else:
            selected = list(stages) + [s for s in required if s not in set(stages)]
            if not selected:
                sys.exit(f'FAIL {args.label}: {name}: empty artifact inventory')
            present_a = present_b = None

        for stage in selected:
            if args.inventory and stage not in required_set:
                if (stage in present_a) != (stage in present_b):
                    side = 'second' if stage in present_b else 'first'
                    sys.exit(
                        f'FAIL {args.label}: {name}: {stage}: '
                        f'inventory differs (only in {side} side)')
            try:
                hashes = [hashlib.sha256((root / stage).read_bytes()).hexdigest() for root in roots]
            except FileNotFoundError as error:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: missing ({error.filename})')
            if hashes[0] != hashes[1]:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: {hashes[0]} != {hashes[1]}')
            lines.append(f'{hashes[0]}  {name}/{stage}')
        print(f'ok {args.label}: {name}: all stages byte-identical')
    if args.manifest:
        Path(args.manifest).write_text('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main(sys.argv)

#!/usr/bin/env python3
"""Compare the stage outputs of two independent slice-0 builds, byte for byte.

usage: repro_compare.py [--stages A,B,...] [--label NAME] SIDE_A SIDE_B IDENTITY [MANIFEST]

Each SIDE holds `names` (the example inventory) and `out/NAME/STAGE` for every stage. Every file
is read on both sides: a missing file, a differing inventory or one differing byte fails. With
MANIFEST, writes the identity lines followed by one `SHA256  NAME/STAGE` line per artifact.
--stages selects another artifact set and --label changes the diagnostic prefix; their defaults
are gate 8's original artifact set and `slice0_repro`.
"""
import argparse
import hashlib
import sys
from pathlib import Path

STAGES = ['bir.txt', 'llvm.ll', 'lcb_job.txt', 'lcb_source.txt', 'lcb_body.ssa', 'lcb_body.s',
          'lcb_rt.ssa', 'lcb_rt.s', 'llvm.o', 'lcb_body.o', 'lcb_rt.o', 'llvm.exe', 'lcb.exe']


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument('--stages')
    parser.add_argument('--label', default='slice0_repro')
    parser.add_argument('side_a')
    parser.add_argument('side_b')
    parser.add_argument('identity')
    parser.add_argument('manifest', nargs='?')
    args = parser.parse_args(argv[1:])
    stages = args.stages.split(',') if args.stages is not None else STAGES
    if not stages or any(not stage or '/' in stage or stage in ('.', '..') for stage in stages):
        parser.error('--stages must be a comma-separated list of file names')
    if len(set(stages)) != len(stages):
        parser.error('--stages contains a duplicate')
    a, b, identity = Path(args.side_a), Path(args.side_b), Path(args.identity)
    names = (a / 'names').read_bytes()
    if names != (b / 'names').read_bytes():
        sys.exit(f'FAIL {args.label}: example inventory differs')
    if not names.strip():
        sys.exit(f'FAIL {args.label}: no examples')
    lines = identity.read_text().splitlines()
    for name in names.decode().splitlines():
        for stage in stages:
            try:
                hashes = [hashlib.sha256((d / 'out' / name / stage).read_bytes()).hexdigest()
                          for d in (a, b)]
            except FileNotFoundError as e:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: missing ({e.filename})')
            if hashes[0] != hashes[1]:
                sys.exit(f'FAIL {args.label}: {name}: {stage}: {hashes[0]} != {hashes[1]}')
            lines.append(f'{hashes[0]}  {name}/{stage}')
        print(f'ok {args.label}: {name}: all stages byte-identical')
    if args.manifest:
        Path(args.manifest).write_text('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main(sys.argv)

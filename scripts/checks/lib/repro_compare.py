#!/usr/bin/env python3
"""Compare the stage outputs of two independent slice-0 builds, byte for byte.

usage: repro_compare.py SIDE_A SIDE_B IDENTITY [MANIFEST]

Each SIDE holds `names` (the example inventory) and `out/NAME/STAGE` for every stage. Every file
is read on both sides: a missing file, a differing inventory or one differing byte fails. With
MANIFEST, writes the identity lines followed by one `SHA256  NAME/STAGE` line per artifact.
"""
import hashlib
import sys
from pathlib import Path

STAGES = ['bir.txt', 'llvm.ll', 'lcb_job.txt', 'lcb_source.txt', 'lcb_body.ssa', 'lcb_body.s',
          'lcb_rt.ssa', 'lcb_rt.s', 'llvm.o', 'lcb_body.o', 'lcb_rt.o', 'llvm.exe', 'lcb.exe']


def main(argv):
    if len(argv) not in (4, 5):
        sys.exit(__doc__)
    a, b, identity = Path(argv[1]), Path(argv[2]), Path(argv[3])
    names = (a / 'names').read_bytes()
    if names != (b / 'names').read_bytes():
        sys.exit('FAIL slice0_repro: example inventory differs')
    if not names.strip():
        sys.exit('FAIL slice0_repro: no examples')
    lines = identity.read_text().splitlines()
    for name in names.decode().splitlines():
        for stage in STAGES:
            try:
                hashes = [hashlib.sha256((d / 'out' / name / stage).read_bytes()).hexdigest()
                          for d in (a, b)]
            except FileNotFoundError as e:
                sys.exit(f'FAIL slice0_repro: {name}: {stage}: missing ({e.filename})')
            if hashes[0] != hashes[1]:
                sys.exit(f'FAIL slice0_repro: {name}: {stage}: {hashes[0]} != {hashes[1]}')
            lines.append(f'{hashes[0]}  {name}/{stage}')
        print(f'ok slice0_repro: {name}: all stages byte-identical')
    if len(argv) == 5:
        Path(argv[4]).write_text('\n'.join(lines) + '\n')


if __name__ == '__main__':
    main(sys.argv)

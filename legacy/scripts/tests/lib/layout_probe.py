#!/usr/bin/env python3
"""LLVM-vs-compiler layout agreement probe (bug 121).

The compiler computes every field offset itself (Layout.tySize / tyAlign / fieldOffset; field
access is a byte-offset GEP), while aggregate loads, stores and allocas use LLVM's layout of the
type definitions it emits. When the two disagree, each whole-value copy moves fields to where the
compiler does not look: bug 121 (a payload-less enum declared 8 bytes in LLVM and 4 in tySize)
freed a String pointer read out of padding.

This probe asks LLVM ITSELF, not a second model of it. For each program it takes the compiler's
`--report layout` (sizes, alignments, field offsets) and the emitted type definitions, generates a
module that prints `getelementptr`-computed sizes, alignments and field offsets under the same
datalayout and triple, compiles it with the C compiler and compares the numbers.

usage: layout_probe.py <compiler> <cc> <workdir> <file.con>...
Prints one line per program: "ok <file> <n> checks", "skip <file> <why>" (the program has no
concrete struct or enum), "REJECTED <file> <stage>" (the compiler refused `--report layout` or
`--emit-llvm`; the caller pins which programs may be rejected) or
"MISMATCH <file> <type> <what> compiler=<n> llvm=<m>"; exits 1 when any MISMATCH or probe
failure occurs, 2 on usage errors.
"""
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

# `struct Name  #[repr(C)]  #[packed]  (size: N, align: A)` — annotations are optional.
HEADER = re.compile(r'^(struct|enum) (\S+)(?:\s+#\[[^\]]*\])*\s+\(size: (\d+), align: (\d+)')
FIELD = re.compile(r'^\s+offset\s+(\d+)\s+size\s+(\d+)\s+align\s+(\d+)\s+(\S+?):\s')
TYPEDEF = re.compile(r'^%[\w.$-]+ = type ')


def parse_report(text):
    """[(kind, name, size, align, [field offsets])] from `--report layout`."""
    types, cur = [], None
    for line in text.splitlines():
        m = HEADER.match(line)
        if m:
            cur = (m.group(1), m.group(2), int(m.group(3)), int(m.group(4)), [])
            types.append(cur)
            continue
        f = FIELD.match(line)
        if f and cur is not None and cur[0] == 'struct':
            cur[4].append(int(f.group(1)))
        elif not line.startswith(' '):
            cur = None
    return types


def probe_one(compiler, cc, workdir, src, idx):
    def run(args):
        return subprocess.run(args, capture_output=True, text=True, timeout=120)

    rep = run([compiler, src, '--report', 'layout'])
    if rep.returncode != 0:
        return [f'REJECTED {src} report']
    types = parse_report(rep.stdout)
    if not types:
        return [f'skip {src} no-types']
    ll = run([compiler, src, '--emit-llvm'])
    if ll.returncode != 0:
        return [f'REJECTED {src} emit']
    lines = ll.stdout.splitlines()
    header = [l for l in lines if l.startswith('target datalayout') or l.startswith('target triple')]
    typedefs = [l for l in lines if TYPEDEF.match(l)]
    defined = {l.split(' = type ', 1)[0] for l in typedefs}

    checks, body = [], []

    def emit(label, want, expr):
        reg = f'%v{len(checks)}'
        body.append(f'  {reg} = ptrtoint ptr {expr} to i64')
        body.append(f'  call i32 (ptr, ...) @printf(ptr @.fmt, i64 {reg})')
        checks.append((label, want))

    for kind, name, size, align, offsets in types:
        ty = f'%{kind}.{name}'
        if ty not in defined:  # generic declarations are emitted per instantiation
            continue
        emit(f'{name} size', size, f'getelementptr ({ty}, ptr null, i32 1)')
        emit(f'{name} align', align, f'getelementptr ({{ i8, {ty} }}, ptr null, i32 0, i32 1)')
        for k, off in enumerate(offsets):
            emit(f'{name} field#{k} offset', off, f'getelementptr ({ty}, ptr null, i32 0, i32 {k})')
    if not checks:
        return [f'skip {src} no-emitted-types']

    probe = '\n'.join(header + typedefs + [
        '@.fmt = private constant [6 x i8] c"%lld\\0A\\00"',
        'declare i32 @printf(ptr, ...)',
        'define i32 @main() {',
        *body,
        '  ret i32 0',
        '}', ''])
    ll_path = os.path.join(workdir, f'probe{idx}.ll')
    exe_path = os.path.join(workdir, f'probe{idx}')
    with open(ll_path, 'w') as fh:
        fh.write(probe)
    comp = run([cc, '-Wno-override-module', '-w', ll_path, '-o', exe_path])
    if comp.returncode != 0:
        first = (comp.stderr.strip().splitlines() or ['?'])[0]
        return [f'PROBE-FAILED {src} {first}']
    out = run([exe_path])
    got = out.stdout.split()
    if out.returncode != 0 or len(got) != len(checks):
        return [f'PROBE-FAILED {src} probe printed {len(got)} of {len(checks)} values']
    bad = [f'MISMATCH {src} {label} compiler={want} llvm={g}'
           for (label, want), g in zip(checks, got) if int(g) != want]
    return bad or [f'ok {src} {len(checks)} checks']


def main(argv):
    if len(argv) < 5:
        print(__doc__, file=sys.stderr)
        return 2
    compiler, cc, workdir, srcs = argv[1], argv[2], argv[3], argv[4:]
    jobs = int(os.environ.get('LAYOUT_PROBE_JOBS', '4'))
    with ThreadPoolExecutor(max_workers=jobs) as pool:
        results = list(pool.map(lambda p: probe_one(compiler, cc, workdir, p[1], p[0]),
                                enumerate(srcs)))
    failed = False
    for lines in results:
        for line in lines:
            print(line)
            failed |= line.startswith(('MISMATCH', 'PROBE-FAILED'))
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

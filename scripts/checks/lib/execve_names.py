#!/usr/bin/env python3
"""Print basenames of successful execve calls in a strace -f execve log."""

import os
import re
import sys

SUCCESS = re.compile(r'execve\("((?:[^"\\]|\\.)*)".*\)\s+=\s+0\s*$')
UNFINISHED = re.compile(r'execve\("((?:[^"\\]|\\.)*)".*\)\s+<unfinished \.\.\.>\s*$')
RESUMED = re.compile(r'<\.\.\. execve resumed>.*=\s+(-?[0-9]+)\s*$')
PID = re.compile(r'^\s*([0-9]+)\s+')


def parse(lines):
    names = set()
    pending = {}
    for line_number, line in enumerate(lines, 1):
        line = line.rstrip("\n")
        pid_match = PID.match(line)
        pid = pid_match.group(1) if pid_match else None
        match = SUCCESS.search(line)
        if match:
            names.add(os.path.basename(match.group(1)))
            continue
        match = UNFINISHED.search(line)
        if match:
            if pid is None or pid in pending:
                raise ValueError(f"line {line_number}: ambiguous unfinished execve")
            pending[pid] = match.group(1)
            continue
        match = RESUMED.search(line)
        if match:
            if pid is None or pid not in pending:
                raise ValueError(f"line {line_number}: resumed execve has no unfinished call")
            if int(match.group(1)) == 0:
                names.add(os.path.basename(pending[pid]))
            del pending[pid]
            continue
        if "execve" in line and re.search(r"=\s+0\s*$", line):
            raise ValueError(f"line {line_number}: unrecognised successful execve")
    if pending:
        raise ValueError(f"unfinished execve calls have no result: {', '.join(sorted(pending))}")
    return sorted(names)


def main():
    if len(sys.argv) != 2:
        raise ValueError("usage: execve_names.py LOG")
    with open(sys.argv[1], encoding="utf-8", errors="replace") as log:
        for name in parse(log):
            print(name)


if __name__ == "__main__":
    try:
        main()
    except (OSError, UnicodeError, ValueError) as error:
        print(f"execve_names: {error}", file=sys.stderr)
        sys.exit(1)

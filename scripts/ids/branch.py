#!/usr/bin/env python3
"""The worktree-name and canonical reservation branch policy, shared by both tools."""
import subprocess
import sys


def worktree_name(name):
    if not name or '/' in name or name.startswith('.'):
        raise ValueError('worktree name must be a plain directory name')
    if subprocess.run(['git', 'check-ref-format', 'refs/heads/' + name], capture_output=True).returncode:
        raise ValueError('invalid worktree branch name')
    return name


def canonical_branch(branch):
    if branch == 'main':
        return branch
    name = branch.removeprefix('wip/')
    worktree_name(name)
    return 'wip/' + name


if __name__ == '__main__':
    try:
        if len(sys.argv) != 3 or sys.argv[1] != 'worktree-name':
            raise ValueError('usage: branch.py worktree-name NAME')
        print(worktree_name(sys.argv[2]))
    except ValueError as error:
        print(str(error), file=sys.stderr)
        sys.exit(2)

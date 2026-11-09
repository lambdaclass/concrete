"""Authoritative parser for task identity rows in ROADMAP.md.

Only the first cell declares an identity; references in prose and dependencies do not.
The second cell is the immutable subject. Other cells describe mutable obligations.
"""
import re


def read_rows(text):
    rows = {}
    for number, line in enumerate(text.splitlines(), 1):
        if not re.match(r'^\s*\|\s*R-', line):
            continue
        # Markdown escaped pipes belong to the cell, not the table structure.
        cells = re.split(r'(?<!\\)\|', line.strip())
        key = cells[1].strip()
        if not re.fullmatch(r'R-\d{4}', key):
            raise ValueError(f'ROADMAP.md:{number}: invalid task ID {key!r}')
        if len(cells) < 5 or cells[-1].strip():
            raise ValueError(f'ROADMAP.md:{number}: malformed task table row')
        subject = cells[2].strip()
        if not subject or any(c in subject for c in '\t\r'):
            raise ValueError(f'ROADMAP.md:{number}: empty or invalid subject for {key}')
        if key in rows:
            raise ValueError(f'ROADMAP.md:{number}: duplicate {key} (first at line {rows[key][1]})')
        rows[key] = (subject, number)
    return rows

#!/usr/bin/env python3
"""ISSUES.tsv schema and canonical three-way row merge, shared with the gate."""
import re
import sys

HEADER = ['id', 'title', 'date', 'area', 'evidence', 'owner', 'severity', 'closure_condition', 'status']
AREA = {'spec', 'proof', 'compiler', 'launcher', 'ci', 'migration'}
SEVERITY = {'high', 'medium', 'low'}
STATUS = {'open', 'closed'}


LEGACY_HEADERS = [
    ['id', 'date', 'area', 'evidence', 'owner', 'status'],
    [x for x in HEADER if x != 'title'],
]


def read_rows(text, allow_legacy=False):
    """Raw TSV has no quoting convention. Historical schemas are migration inputs only."""
    lines = [line for line in text.splitlines() if line.strip() and not line.startswith('#')]
    header = lines[0].split('\t') if lines else []
    if header != HEADER and not (allow_legacy and header in LEGACY_HEADERS):
        raise ValueError('header is not a supported ISSUES.tsv schema')
    rows = {}
    last = 0
    for n, line in enumerate(lines[1:], 2):
        fields = line.split('\t')
        if len(fields) != len(header) or any(not x.strip() for x in fields):
            raise ValueError(f'row {n}: needs {len(header)} non-empty tab-separated fields')
        row = dict(zip(header, fields))
        key = row['id']
        if not re.fullmatch(r'ISS-\d{4}', key):
            raise ValueError(f'row {n}: id {key!r} is not ISS-NNNN')
        if int(key[4:]) <= last:
            raise ValueError(f'row {n}: id {key} is not greater than the previous id')
        last = int(key[4:])
        if not re.fullmatch(r'\d{4}-\d{2}-\d{2}', row['date']):
            raise ValueError(f'row {n}: date is not YYYY-MM-DD')
        if row['area'] not in AREA:
            raise ValueError(f'row {n}: area {row["area"]!r} is not one of {sorted(AREA)}')
        if 'severity' in row and row['severity'] not in SEVERITY:
            raise ValueError(f'row {n}: invalid severity {row["severity"]!r}')
        if row['status'] not in STATUS:
            raise ValueError(f'row {n}: status {row["status"]!r} is not one of {sorted(STATUS)}')
        rows[key] = row
    return rows


def parse(text):
    rows = read_rows(text)
    comments = tuple(line for line in text.splitlines() if line.startswith('#'))
    return comments, {key: '\t'.join(row[k] for k in HEADER) for key, row in rows.items()}


def choose(base, ours, theirs):
    if ours == theirs or theirs == base:
        return ours
    if ours == base:
        return theirs
    raise ValueError('competing changes to the same issue or comments')


def merge(ours, base, theirs):
    bcomments, b = parse(base.decode('utf-8'))
    ocomments, o = parse(ours.decode('utf-8'))
    tcomments, t = parse(theirs.decode('utf-8'))
    comments = choose(bcomments, ocomments, tcomments)
    rows = {}
    for key in b.keys() | o.keys() | t.keys():
        value = choose(b.get(key), o.get(key), t.get(key))
        if value is not None:
            rows[key] = value
    text = '\n'.join([*comments, '\t'.join(HEADER), *[rows[k] for k in sorted(rows)]]) + '\n'
    parse(text)  # Refuse to stage any result that does not meet the gate's exact schema.
    return text.encode('utf-8')


if __name__ == '__main__':
    try:
        with open(sys.argv[1]) as stream:
            parse(stream.read())
    except (ValueError, OSError) as error:
        print(error)
        sys.exit(1)

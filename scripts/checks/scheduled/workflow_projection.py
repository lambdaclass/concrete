"""Render and verify scheduler workflow projections from materialized owners."""
import re
from pathlib import Path
import sys
# Establish the common adapter import path, also used when invoked directly.
import process_adapter
from scheduler_policy import NOTE_NAMESPACE, POLICY, BUDGET, TIERS, MAIN_BRANCH, MAIN_REF, REMOTE

ROOT = Path(__file__).resolve().parents[3]
TOKEN = re.compile(r'@@([a-z-]+):([a-z_]+)@@')


def values():
    cadence = POLICY['cadence']
    expression = "'replay'"
    scheduled = "''"
    for tier, cron in reversed(tuple(cadence.items())):
        scheduled = f"(github.event.schedule == '{cron}' && '{tier}' || {scheduled})"
    return dict(schedule=dict(title="scheduled-${{ github.event_name == 'schedule' && " + scheduled + " || " + expression + " }}",
                              crons='\n'.join(f"    - cron: '{cron}'" for cron in cadence.values()),
                              publisher_cron=POLICY['publisher_cron'], observer_cron=POLICY['observer_cron']),
                tier=dict(options='[' + ', '.join(TIERS) + ']'),
                identity=dict(main_ref=MAIN_REF, main_branch=MAIN_BRANCH),
                **{'note-ref': dict(remote=REMOTE)},
                retention=dict(days=POLICY['retention_days']), budget=BUDGET)


def project(template):
    owner = values()
    return TOKEN.sub(lambda m: str(owner[m[1]][m[2]]), template)


def check(root=ROOT, *, write=False):
    for template in sorted((root / 'scripts/checks/scheduled/workflows').glob('*.in')):
        source = template.read_text()
        # Templates describe structure; owned values must remain projections.
        for line in source.splitlines():
            if line.lstrip().startswith('#'): continue
            fact = None
            if 'retention-days:' in line and '@@retention:' not in line: fact = 'retention'
            elif ('timeout-minutes:' in line or re.search(r'(?:bounded_run |--timeout )[-r 0-9]', line)) and '@@budget:' not in line: fact = 'budget'
            elif 'cron:' in line and '@@schedule:' not in line: fact = 'schedule'
            elif 'options:' in line and '@@tier:' not in line: fact = 'tier'
            elif NOTE_NAMESPACE in line: fact = 'note-ref'
            elif 'refs/heads/' in line or re.search(r'\bref: main\b', line): fact = 'identity'
            if fact: raise ValueError('FAIL scheduled ownership: ' + fact)
        expected = project(source)
        target = root / '.github/workflows' / template.name.removesuffix('.in')
        if write:
            target.write_text(expected)
            continue
        actual = target.read_text()
        if actual == expected:
            continue
        # Attribute a stale/edited projected line to its source fact. Other
        # workflow edits must also be made in the template and regenerated.
        import difflib
        rendered = []; facts = []
        for line in source.splitlines():
            matches = TOKEN.findall(line)
            lines = project(line).splitlines()
            rendered.extend(lines)
            facts.extend([matches[0][0] if matches else 'schedule'] * len(lines))
        matcher = difflib.SequenceMatcher(a=rendered, b=actual.splitlines(), autojunk=False)
        for tag, i, j, start, stop in matcher.get_opcodes():
            if tag != 'equal':
                if tag == 'replace' and j-i == stop-start == 1:
                    for line in source.splitlines():
                        if project(line) != rendered[i]: continue
                        tokens = list(TOKEN.finditer(line))
                        pattern = ''; cursor = 0
                        for token in tokens:
                            pattern += re.escape(line[cursor:token.start()]) + '(.*?)'
                            cursor = token.end()
                        pattern += re.escape(line[cursor:])
                        match = re.fullmatch(pattern, actual.splitlines()[start])
                        if match:
                            for token, value in zip(tokens, match.groups()):
                                if value != project(token.group()):
                                    raise ValueError('FAIL scheduled ownership: ' + token[1])
                raise ValueError('FAIL scheduled ownership: ' + facts[min(i, len(facts)-1)])
        raise ValueError('FAIL scheduled ownership: schedule')


if __name__ == '__main__':
    try: check(write=sys.argv[1:] == ['--write'])
    except ValueError as error: sys.exit(str(error))

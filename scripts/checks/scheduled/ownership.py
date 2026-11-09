"""Extend the boundary census to refuse independently authored scheduler facts.

Fixtures are independent controls. Production Python, shell adapters and generated
workflow projections are consumers. Numeric checks target policy-sized values and
literal timeout arguments, avoiding unrelated counters and byte-size bounds.
"""
import ast
from pathlib import Path
import sys
import process_adapter
from scheduler_policy import NOTE_NAMESPACE, POLICY, WINDOWS, TIERS, MAIN_BRANCH, MAIN_REF, MAIN_HISTORY
from workflow_projection import check as check_workflows

ROOT = Path(__file__).resolve().parents[3]


def fail(fact):
    raise ValueError('FAIL scheduled ownership: ' + fact)


def check_python(path):
    tree = ast.parse(path.read_text())
    owner = path.name
    for node in ast.walk(tree):
        names = {'order': 'history', 'branch': 'identity', 'main_ref': 'identity',
                 'occurrence': 'occurrence', 'cron': 'schedule', 'tier': 'tier',
                 'window': 'window', 'windows': 'window', 'budget': 'budget',
                 'timeout': 'budget', 'retention_days': 'retention', 'days': 'retention'}
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                if isinstance(target, ast.Name) and target.id in names:
                    if (isinstance(node.value, (ast.Constant, ast.List, ast.Tuple, ast.Set, ast.Dict, ast.BinOp))
                            and not (target.id == "budget" and isinstance(node.value, ast.Constant) and node.value.value == 0)):
                        fail(names[target.id])
        if isinstance(node, ast.Constant):
            value = node.value
            if isinstance(value, str):
                if (MAIN_HISTORY in value or value in ('--first-parent', '--topo-order')) and owner != 'history.py':
                    fail('history')
                if (value == MAIN_BRANCH or MAIN_REF in value) and owner != 'process_coverage.py':
                    fail('identity')
                if NOTE_NAMESPACE in value:
                    fail('note-ref')
                if value in TIERS or any(cron in value for cron in POLICY['cadence'].values()):
                    fail('tier' if value in TIERS else 'schedule')
                if value in (POLICY['publisher_cron'], POLICY['observer_cron']):
                    fail('schedule')
            elif type(value) in (int, float):
                if value in WINDOWS.values(): fail('window')
                if value == POLICY['retention_days']: fail('retention')
                if value == POLICY['queue_age_seconds']: fail('schedule')
                if value in {v for v in POLICY['budgets'].values() if v >= 20}: fail('budget')
        if isinstance(node, ast.keyword):
            # Materialized identities may be copied, but never independently encoded.
            if node.arg == 'occurrence': fail('occurrence')
            if node.arg == 'timeout' and isinstance(node.value, ast.Constant): fail('budget')


def check(root=ROOT):
    for path in (root / 'scripts/checks/scheduled').glob('*.py'):
        if path.name == 'ownership.py' or path.name.startswith('test_') or path.name.endswith('_control.py'):
            continue
        check_python(path)
    check_python(root / 'scripts/checks/lib/gate_manifest.py')
    # Shell adapters cannot carry policy literals either. Generated workflows are
    # checked byte-for-byte against their owners rather than trusting a marker.
    for relative in ('scripts/checks/scheduled/with_slot.sh', 'scripts/checks/scheduled_gates.sh'):
        text = (root / relative).read_text()
        for fact, values in {'note-ref': [POLICY['note_prefix']],
                             'history': [MAIN_HISTORY], 'identity': [MAIN_REF],
                             'schedule': list(POLICY['cadence'].values()),
                             'budget': [str(POLICY['budgets']['slot'])]}.items():
            if any(value in text for value in values): fail(fact)
    check_workflows(root)


if __name__ == '__main__':
    try: check(Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT)
    except ValueError as error: sys.exit(str(error))

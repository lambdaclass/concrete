"""Light exact-diagnostic controls for named, non-aborting audit failures.

Faults are injected into a private compiled consumer, never the production checker.
"""
import functools
import tempfile
from pathlib import Path

from vacuity_controls import BASE, controls
from vacuity_driver import HERE, ROOT, compiled_consumer, run
from vacuity_fixtures import main as fixtures_main


def main():
    source = BASE.replace('end ConcreteSpecControl', '''axiom budgetSupplier : 2 = 2
/-- Instance (law `ConcreteSpecControl.growth`, case `budget`). -/
example : 2 < S 2 := growth 2 budgetSupplier
/-- Law (subject `ConcreteSpecControl.S`). -/
theorem unrelated (n : Nat) : n < S n := Nat.lt_succ_self n
end ConcreteSpecControl''')
    line = next(i for i, text in enumerate(source.splitlines(), 1)
                if 'growth 2 budgetSupplier' in text)
    name = f'ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:{line}»'
    consumer = (HERE / 'vacuity.lean').read_text()
    anchor = 'partial def falseDischarge (e : Expr) : StateRefT DischargeState MetaM Bool := do\n'
    assert consumer.count(anchor) == 1
    for label, fault, reason in [
        ('work-budget', 'throwError "false-discharge-budget"', 'false-discharge-budget'),
        ('analysis-error', 'throwError "planted-analysis-error"', 'planted-analysis-error'),
        ('heartbeat', '''withTheReader Core.Context (fun ctx => { ctx with initHeartbeats := 0, maxHeartbeats := 1 }) do
      Core.checkMaxHeartbeats "planted-heartbeat"
    return false''', '(deterministic) timeout at `«planted-heartbeat»`'),
    ]:
        injection = ('  if e.isConstOf `ConcreteSpecControl.budgetSupplier then\n    ' + fault + '\n')
        with compiled_consumer(consumer_text=consumer.replace(anchor, anchor + injection)) as meta:
            code, report = run(ROOT, '', source, 64, meta_path=meta)
        diagnostics = [row for row in report.splitlines() if row.startswith(('FAIL ', 'INCOMPLETE '))]
        expected = {
            f'FAIL missing-instance ConcreteSpecControl.unrelated ',
            f'INCOMPLETE analysis-budget-or-error {name} stage=instance law=ConcreteSpecControl.growth case=budget {reason}',
        }
        # Runtime errors contain multiline Lean hints; compare the exact diagnostic
        # line and exact declaration/case, and forbid any other finding.
        if label == 'heartbeat':
            expected = {row + ', maximum number of heartbeats (0) has been reached' if row.endswith(reason) else row
                        for row in expected}
        if code != 2 or set(diagnostics) != expected or not report.endswith('N2 INCOMPLETE\n'):
            raise AssertionError(f'{label}: exit={code}\n{report}\nexpected={expected}')
        print(f'PASS reporting {label}: exact named INCOMPLETE and unrelated FAIL retained')
    with compiled_consumer() as meta:
        if controls(functools.partial(run, meta_path=meta), ROOT,
                    labels={'positive', 'false-discharge', 'positive-budget-chain'}):
            return 1
    # The replay path uses this same report validator. Existing server reports
    # must still be rejected, even though missing diagnostics once looked clean.
    import sys
    with tempfile.TemporaryDirectory(prefix='n2-report-rejection-') as temp:
        import json
        rows = json.loads((HERE / 'fixtures/provenance.json').read_text())
        for label in ['c9-old', 'c9-new', 'c6-old', 'c6-current']:
            lines = []
            for row in rows:
                if row['fixture'].startswith(label + '-'):
                    lines.append(f'N2 SOURCE-FILE {row["path"]} {row["source_sha256"]}')
                    if label.endswith('old'):
                        fixture = (HERE / 'fixtures' / row['fixture']).read_text().splitlines()
                        line_no = row['start'] + next(i for i, text in enumerate(fixture) if text.startswith('example '))
                        diagnostic = 'instance-application' if '-instance-' in row['fixture'] else 'planted-proposition'
                        lines.append(f'FAIL {diagnostic} ConcreteSpec.«⟪example⟫{row["path"]}:{line_no}»')
            if label == 'c9-new':
                lines.append(f'INCOMPLETE analysis-budget-or-error {name} stage=instance law=ConcreteSpecControl.growth case=budget false-discharge-budget')
            lines.append('N2 INCOMPLETE' if label == 'c9-new' else 'N2 FAIL')
            (Path(temp) / (label + '.txt')).write_text('\n'.join(lines) + '\n')
        import contextlib
        import io
        saved_argv = sys.argv
        sys.argv = ['vacuity_fixtures.py', '--reports', temp]
        output = io.StringIO()
        try:
            with contextlib.redirect_stdout(output):
                code = fixtures_main()
        finally:
            sys.argv = saved_argv
        assert code == 1 and output.getvalue().splitlines() == ['FAIL report-incomplete c9-new'], output.getvalue()
    print('PASS reporting replay-validator: exactly FAIL report-incomplete c9-new')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

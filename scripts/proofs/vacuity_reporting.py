"""Light exact-diagnostic controls for named, non-aborting audit failures.

Faults are injected into a private compiled consumer, never the production checker.
"""
import functools
import re
import tempfile
from pathlib import Path

from vacuity_controls import BASE, controls
from vacuity_driver import HERE, ROOT, compiled_consumer, run
from vacuity_fixtures import main as fixtures_main


def ownership_controls(meta):
    fixture = BASE.replace('Instance (law `ConcreteSpecControl.growth`',
                           'Instance (law `ConcreteSpecControl.missingLaw`')
    for cap in [None, 100000]:
        code, report = run(ROOT, '', fixture, 64, meta_path=meta, whole_run_cap=cap)
        name = 'ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:8»'
        assert f'FAIL unknown-name {name} ConcreteSpecControl.missingLaw' in report.splitlines(), report
        assert any(r.startswith(f'NOTE analysis-result {name} stage=classification ') and
                   'result=FAIL ' in r for r in report.splitlines()), report
        assert code == (1 if cap is None else 2), report
        print(f'PASS reporting classification-unknown cap={cap}', flush=True)
    # Both duplicate Instance analyses and both repeated Planted analyses must
    # own their failure even when their diagnostics are globally identical.
    fixture = BASE.replace('/-- Instance (law `ConcreteSpecControl.growth`, case `two`). -/',
        '/-- Instance (law `ConcreteSpecControl.growth`, case `two`). '
        'Instance (law `ConcreteSpecControl.growth`, case `two`). -/').replace(
        '/-- Planted (', '/-- Planted (', 1)
    tag = 'Planted (law `ConcreteSpecControl.growth`, case `two`, subject `ConcreteSpecControl.S`, mutant `ConcreteSpecControl.mutant`).'
    fixture = fixture.replace(tag, tag + ' ' + tag.replace('case `two`', 'case `three`')).replace(
        'example : 2 = 2 ∧', 'theorem badPlanted : 2 = 2 ∧')
    code, report = run(ROOT, '', fixture, 64, meta_path=meta)
    rows = report.splitlines()
    for stage, diagnostic, count in [('evidence-tags', 'duplicate-instance', 1),
                                      ('evidence-kind', 'evidence-not-example', 2)]:
        assert any(r.startswith('FAIL ' + diagnostic + ' ') for r in rows), report
        terminals = [r for r in rows if r.startswith('NOTE analysis-result ') and
                     f'stage={stage} ' in r and
                     ('badPlanted' in r if stage == 'evidence-kind' else 'ConcreteSpec.lean:8' in r)]
        assert len(terminals) >= count and all('result=FAIL ' in r for r in terminals), report
    print('PASS reporting repeated-job-findings', flush=True)


def watchdog_controls():
    consumer = (HERE / 'vacuity.lean').read_text()
    anchor = '        journal s!"START\\t{i}"\n        let complete ← isolated deadline job'
    assert consumer.count(anchor) == 1
    injection = anchor.replace('        let complete',
        '        if job.stage == "evidence-tags" then liftM (IO.sleep 15000 : IO Unit)\n        let complete')
    fixture = BASE.replace('Instance (law `ConcreteSpecControl.growth`',
                           'Instance (law `ConcreteSpecControl.missingLaw`')
    with compiled_consumer(consumer_text=consumer.replace(anchor, injection)) as meta:
        code, report = run(ROOT, '', fixture, 64, meta_path=meta, audit_seconds=4)
    rows = report.splitlines()
    assert code == 2 and 'FAIL unknown-name ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:8» ConcreteSpecControl.missingLaw' in rows, report
    assert 'INCOMPLETE analysis-budget-or-error ConcreteSpecControl.growth stage=evidence-tags law=none case=none reason=wall-watchdog heartbeats=unavailable/2000000 work=unavailable/20000 projection=unavailable/4096' in rows, report
    assert 'INCOMPLETE analysis-budget-or-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none reason=wall-watchdog-unrun heartbeats=0/2000000 work=0/20000 projection=0/4096' in rows, report
    terminals = [r for r in rows if r.startswith('NOTE analysis-result ')]
    stats = dict(re.findall(r'(\w[\w-]*)=([^ ]+)', next(r for r in rows if r.startswith('NOTE analysis-summary '))))
    n = int(stats['N'])
    assert len(terminals) == int(stats['reported']) == n, report
    assert int(stats['attempted']) + int(stats['unrun']) == n, report
    assert int(stats['completed']) + int(stats['incomplete']) == n, report
    assert report.endswith('N2 INCOMPLETE\n') and 'INCOMPLETE enumerator' not in report, report
    print('PASS reporting wall-watchdog: earlier exact FAIL, named interrupted/unrun analyses, population equations', flush=True)


def main():
    watchdog_controls()
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
        normalized = {re.sub(r'heartbeats=\d+/', 'heartbeats=MEASURED/', row) for row in diagnostics}
        expected_reason = {'work-budget': 'false-discharge-budget', 'analysis-error': 'analysis-error', 'heartbeat': 'heartbeats'}[label]
        expected = {
            'FAIL missing-instance ConcreteSpecControl.unrelated ',
            f'INCOMPLETE analysis-budget-or-error {name} stage=instance law=ConcreteSpecControl.growth case=budget reason={expected_reason} heartbeats=MEASURED/2000000 work=0/20000 projection=0/4096',
            'INCOMPLETE analysis-budget-or-error ConcreteSpecControl.growth stage=law law=ConcreteSpecControl.growth case=none reason=dependency-incomplete heartbeats=MEASURED/2000000 work=0/20000 projection=0/4096',
        }
        detail = f'DETAIL analysis-error {name} stage=instance law=ConcreteSpecControl.growth case=budget {reason}'
        if label == 'heartbeat':
            detail += ', maximum number of heartbeats (0) has been reached'
        if code != 2 or normalized != expected or detail not in report.splitlines() or not report.endswith('N2 INCOMPLETE\n'):
            raise AssertionError(f'{label}: exit={code}\n{report}\nexpected={expected}')
        print(f'PASS reporting {label}: exact named INCOMPLETE and unrelated FAIL retained')
    # Enumeration failures, including setup before job construction, belong to
    # the scheduler and cannot masquerade as an empty successful population.
    anchor = '    let build : Audit (Array AnalysisJob) := do\n'
    assert consumer.count(anchor) == 1
    with compiled_consumer(consumer_text=consumer.replace(anchor, anchor +
            '      throwError "planted-scheduler-enumeration"\n')) as meta:
        code, report = run(ROOT, '', source, 64, meta_path=meta)
    diagnostics = [re.sub(r'heartbeats=\d+/', 'heartbeats=MEASURED/', row)
                   for row in report.splitlines() if row.startswith(('FAIL ', 'INCOMPLETE '))]
    assert code == 2 and report.endswith('N2 INCOMPLETE\n') and diagnostics == [
        'INCOMPLETE analysis-budget-or-error [anonymous] stage=scheduler law=none case=none reason=analysis-error heartbeats=MEASURED/2000000 work=0/20000 projection=0/4096',
    ], report
    assert 'DETAIL analysis-error [anonymous] planted-scheduler-enumeration' in report.splitlines(), report
    assert not any(row.startswith(('NOTE analysis-population ', 'NOTE analysis-result ')) for row in report.splitlines()), report
    print('PASS reporting scheduler-error: exact named scheduler INCOMPLETE and no partial population')
    # A failed Planted analysis belongs to the Planted declaration and its
    # paired Instance, rather than merely to the enclosing law.
    fixture = BASE.replace('end ConcreteSpecControl',
        '/-- Law (subject `ConcreteSpecControl.S`). -/\n'
        'theorem unrelated (n : Nat) : n < S n := Nat.lt_succ_self n\n'
        'end ConcreteSpecControl')
    planted_line = next(i for i, text in enumerate(fixture.splitlines(), 1) if 'example : 2 = 2 ∧' in text)
    instance_line = next(i for i, text in enumerate(fixture.splitlines(), 1) if 'example : 2 < S 2' in text)
    planted_name = f'ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:{planted_line}»'
    instance_name = f'ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:{instance_line}»'
    anchor = 'def counterexample (law : ConstantInfo) (ic : InstanceCase) (s m : Name) (subjects : Array Name) (budget : Option WorkBudget := none) : MetaM Expr := do\n'
    assert consumer.count(anchor) == 1
    with compiled_consumer(consumer_text=consumer.replace(anchor, anchor +
            '  if ic.key == "two" then throwError "planted-counterexample-error"\n')) as meta:
        code, report = run(ROOT, '', fixture, 64, meta_path=meta)
    diagnostics = {re.sub(r'heartbeats=\d+/', 'heartbeats=MEASURED/', row)
                   for row in report.splitlines() if row.startswith(('FAIL ', 'INCOMPLETE '))}
    assert code == 2 and report.endswith('N2 INCOMPLETE\n') and diagnostics == {
        'FAIL missing-instance ConcreteSpecControl.unrelated ',
        f'INCOMPLETE analysis-budget-or-error {planted_name} stage=planted law=ConcreteSpecControl.growth case=two reason=analysis-error heartbeats=MEASURED/2000000 work=0/20000 projection=0/4096 instance={instance_name} subject=ConcreteSpecControl.S mutant=ConcreteSpecControl.mutant',
    }, report
    assert f'DETAIL analysis-error {planted_name} stage=planted law=ConcreteSpecControl.growth case=two planted-counterexample-error' in report.splitlines(), report
    print('PASS reporting planted-error: exact Planted and paired Instance named; unrelated FAIL retained')
    from vacuity_budget import controls as budget_controls
    with compiled_consumer() as meta:
        ownership_controls(meta)
        if budget_controls(meta):
            return 1
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

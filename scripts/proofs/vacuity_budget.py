"""A3 exact-diagnostic controls for the production job scheduler and fuel helpers."""
import os
import re
import tempfile
from pathlib import Path

from vacuity_driver import command, compiled_consumer

DRIVER = r'''
import N2Meta
open Lean Meta Vacuity
namespace BudgetControls
def subject : Nat := 1
def mutant : Nat := 2
def wrapper : Nat := subject + 1
theorem nextLaw (n : Nat) : n = n := rfl
theorem nextBad : True := True.intro

def bad : AnalysisJob := {
  name := `BudgetControls.nextBad, stage := "instance", law := some `BudgetControls.nextLaw, caseName := "next"
  action := do
    IO.println "BODY bad"
    let env ← getEnv
    let ci ← getConstInfo `BudgetControls.nextBad
    discard <| checkInstance env [] #[]
      { name := `BudgetControls.nextBad, info := ci, doc := "", law := `BudgetControls.nextLaw, key := "next" } #[]
}

def heavy : AnalysisJob := {
  name := `BudgetControls.heavy, stage := "instance", law := some `BudgetControls.heavyLaw, caseName := "heavy"
  action := do
    let mut state : DischargeState := { budget := (← get).budget }
    for n in [:20001] do
      let (_, next) ← (falseDischarge (mkNatLit n)).run state
      state := next
}

def heartbeat : AnalysisJob := {
  name := `BudgetControls.heartbeat, stage := "instance", law := some `BudgetControls.heavyLaw, caseName := "heartbeat"
  action := do
    -- Plant a runtime exception inside a Meta primitive; the production job's
    -- context is still 2,000,000 and must be restored for the following job.
    withTheReader Core.Context (fun ctx => { ctx with initHeartbeats := 0, maxHeartbeats := 1 }) do
      discard <| isDefEq (mkConst ``True) (mkConst ``False)
      Core.checkMaxHeartbeats "planted-meta-heartbeat"
}

def passJob : AnalysisJob := { name := `BudgetControls.ok, stage := "instance", action := IO.println "BODY ok" }
def unrun (key : String) : AnalysisJob := {
  name := Name.str `BudgetControls key, stage := "instance", law := some `BudgetControls.capLaw, caseName := key
  action := do
    IO.println s!"BODY-RAN {key}"
    fail "unexpected-body" (Name.str `BudgetControls key)
}

def nested (label : String) : AnalysisJob := {
  name := Name.str `BudgetControls label, stage := label
  action := do
    let budget := (← get).budget
    for _ in [:20001] do
      match label with
      | "mentions" => discard <| (mentions (mkConst `BudgetControls.wrapper) `BudgetControls.subject budget).run (0, {})
      | "mutate" => discard <| (mutate (mkConst `BudgetControls.wrapper) `BudgetControls.subject `BudgetControls.mutant #[] budget).run 0
      | _ => discard <| withProtected (mkConst `BudgetControls.wrapper) (mkConst `BudgetControls.wrapper) #[`BudgetControls.subject] (fun _ _ => pure true) budget
}

set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let deadline ← readThe Core.Context
  let ticks ← IO.mkRef (0 : Nat)
  let hb ← IO.mkRef (0 : Nat)
  let jobs : Array AnalysisJob := JOBS
  let (_, state) ← withTheReader Core.Context (fun ctx => { ctx with maxHeartbeats := 0 }) do
    (runJobs deadline jobs CAP ticks hb).run {}
  for f in state.findings do IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
  if state.findings.any (·.status == "INCOMPLETE") then IO.println "N2 INCOMPLETE"
  else if state.findings.any (·.status == "FAIL") then IO.println "N2 FAIL"
  else IO.println "N2 PASS"
end BudgetControls
'''

BAD = 'FAIL instance-application BudgetControls.nextBad law=BudgetControls.nextLaw case=next'
GRAMMAR = re.compile(r'INCOMPLETE analysis-budget-or-error BudgetControls\.(\w+) stage=(\w+) law=([\w.]+) case=(\w+) reason=([\w-]+) heartbeats=(\d+)/2000000 work=(\d+)/20000 projection=(\d+)/4096$')


def controls(meta):
    with tempfile.TemporaryDirectory(prefix='n2-budget-') as temp:
        work = Path(temp)

        def probe(label, jobs, cap):
            path = work / 'Budget.lean'
            path.write_text(DRIVER.replace('JOBS', jobs).replace('CAP', str(cap)))
            result = command(['lean', path], work, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
            if result.returncode:
                raise AssertionError(f'{label}: infrastructure failure\n{result.stdout}')
            rows = result.stdout.splitlines()
            assert not any('BODY-RAN' in r for r in rows), result.stdout
            incomplete = [r for r in rows if r.startswith('INCOMPLETE ')]
            for row in incomplete:
                match = GRAMMAR.fullmatch(row)
                assert match, row
                assert 0 <= int(match[6]) and 0 <= int(match[7]) <= 20000 and 0 <= int(match[8]) <= 4096, row
            terminals = [r for r in rows if r.startswith('NOTE analysis-result ')]
            summary = next(r for r in rows if r.startswith('NOTE analysis-summary '))
            stats = dict(re.findall(r'(\w[\w-]*)=([^ ]+)', summary))
            assert len(terminals) == int(stats['N']) == int(stats['reported']), result.stdout
            assert int(stats['attempted']) + int(stats['unrun']) == int(stats['N']), result.stdout
            assert int(stats['completed']) + int(stats['incomplete']) == int(stats['N']), result.stdout
            print(f'PASS budget {label}', flush=True)
            return rows, incomplete, terminals, stats

        single, heavy, _, _ = probe('heavy-alone', '#[heavy]', 20000)
        assert len(heavy) == 1 and int(GRAMMAR.fullmatch(heavy[0])[6]) < 2000000 and 'stage=instance law=BudgetControls.heavyLaw case=heavy reason=false-discharge-budget' in heavy[0] and 'work=20000/20000' in heavy[0], single
        alone, incomplete, _, _ = probe('bad-alone', '#[bad]', 20000)
        assert incomplete == [] and BAD in alone and alone[-1] == 'N2 FAIL', alone
        rows, inc, terminals, _ = probe('repeated-instance-finding', '#[bad, bad]', 40000)
        assert inc == [] and rows.count(BAD) == 1 and len(terminals) == 2 and all('result=FAIL ' in r for r in terminals), rows
        for label, jobs in [('starvation', '#[heavy, bad]'), ('reverse', '#[bad, heavy]')]:
            rows, inc, terminals, stats = probe(label, jobs, 40000)
            assert BAD in rows and len(inc) == 1 and rows[-1] == 'N2 INCOMPLETE', rows
            assert GRAMMAR.fullmatch(inc[0]).groups()[:5] == GRAMMAR.fullmatch(heavy[0]).groups()[:5], rows
            assert GRAMMAR.fullmatch(inc[0]).groups()[6:] == GRAMMAR.fullmatch(heavy[0]).groups()[6:], rows
            if label == 'starvation': assert rows.index(inc[0]) < rows.index(BAD), rows
            else: assert rows.index(BAD) < rows.index(inc[0]), rows
            assert not any('reason=heartbeats' in r and 'nextBad' in r for r in rows), rows
        rows, inc, _, _ = probe('heartbeat-starvation', '#[heartbeat, bad]', 40000)
        assert len(inc) == 1 and 'reason=heartbeats' in inc[0] and BAD in rows and rows.index(inc[0]) < rows.index(BAD) and rows[-1] == 'N2 INCOMPLETE', rows
        uncapped, _, original, _ = probe('cap-baseline', '#[passJob, bad]', 40000)
        rows, inc, terminals, stats = probe('cap-population', '#[passJob, bad, unrun "unrunA", unrun "unrunB"]', 40000)
        assert BAD in rows and rows[-1] == 'N2 INCOMPLETE', rows
        assert terminals[:2] == original, rows
        expected = [f'INCOMPLETE analysis-budget-or-error BudgetControls.{n} stage=instance law=BudgetControls.capLaw case={n} reason=whole-run-cap heartbeats=0/2000000 work=0/20000 projection=0/4096' for n in ['unrunA', 'unrunB']]
        assert inc == expected, rows
        for k, v in {'N': '4', 'attempted': '2', 'unrun': '2', 'completed': '2', 'incomplete': '2', 'reserved': '40000', 'C': '40000'}.items(): assert stats[k] == v, rows
        rows, inc, _, stats = probe('cap-zero', '#[passJob, bad, unrun "unrunA", unrun "unrunB"]', 0)
        zero_expected = [
            'INCOMPLETE analysis-budget-or-error BudgetControls.ok stage=instance law=none case=none reason=whole-run-cap heartbeats=0/2000000 work=0/20000 projection=0/4096',
            'INCOMPLETE analysis-budget-or-error BudgetControls.nextBad stage=instance law=BudgetControls.nextLaw case=next reason=whole-run-cap heartbeats=0/2000000 work=0/20000 projection=0/4096',
            *expected,
        ]
        assert inc == zero_expected, rows
        assert not any(r.startswith(('FAIL ', 'BODY ')) for r in rows) and stats['attempted'] == '0' and stats['reserved'] == '0', rows
        rows, inc, _, stats = probe('empty', '#[]', 0)
        assert inc == [] and stats['N'] == '0' and stats['cap-hit'] == 'none' and rows[-1] == 'N2 PASS', rows
        for label in ['mentions', 'mutate', 'withProtected']:
            rows, inc, _, _ = probe('nested-reset-' + label, f'#[nested "{label}", bad]', 40000)
            assert len(inc) == 1 and inc[0] == f'INCOMPLETE analysis-budget-or-error BudgetControls.{label} stage={label} law=none case=none reason=work-budget heartbeats={GRAMMAR.fullmatch(inc[0])[6]}/2000000 work=20000/20000 projection=0/4096', rows
            assert BAD in rows and rows[-1] == 'N2 INCOMPLETE', rows
    print('Budget controls: 13 PASS, 0 FAIL', flush=True)
    return 0


def dag_controls():
    from vacuity_driver import HERE
    consumer = (HERE / 'vacuity.lean').read_text()
    anchor = '  if let some b := budget then b.modify fun s => { s with constants := s.constants.insert e result }'
    assert consumer.count(anchor) == 1
    driver = r'''import N2Meta
open Lean Meta Vacuity
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  for depth in [10, 15] do
    let mut e := mkConst ``Nat.zero
    for _ in [:depth] do e := mkApp2 (mkConst ``Nat.add) e e
    let budget ← IO.mkRef ({} : AnalysisUsage)
    try
      discard <| workConstants e (some budget)
      let usage ← budget.get
      IO.println s!"DAG depth={depth} work={usage.work} cached={usage.constants.size} root={usage.constants.contains e}"
      unless usage.constants.contains e do throwError "dag-root-not-cached"
      unless usage.work <= 4 * depth + 3 do throwError "dag-work-not-linear"
    catch ex => IO.println s!"CONTROL {← ex.toMessageData.toString}"
'''
    for label, text in [('memoized', consumer), ('early-exit', consumer.replace(anchor,
            '  if e.isApp then return result\n' + anchor))]:
        with compiled_consumer(consumer_text=text) as meta, tempfile.TemporaryDirectory(prefix='n2-dag-') as temp:
            path = Path(temp) / 'Dag.lean'
            path.write_text(driver)
            result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
        assert result.returncode == 0, result.stdout
        if label == 'memoized':
            assert 'DAG depth=15 work=61 cached=32 root=true' in result.stdout, result.stdout
            assert 'CONTROL ' not in result.stdout, result.stdout
        else:
            assert 'DAG depth=10 work=4093 cached=2 root=false' in result.stdout, result.stdout
            assert 'CONTROL dag-root-not-cached' in result.stdout and 'CONTROL work-budget' in result.stdout, result.stdout
        print(f'PASS DAG {label}: {result.stdout.strip()}', flush=True)



def dependency_controls():
    """Completed negative closures are reusable; partial DFS states never are."""
    from vacuity_driver import HERE
    consumer = (HERE / 'vacuity.lean').read_text()
    anchor = '    if !result && completeQuery then'
    assert consumer.count(anchor) == 1
    declarations = 'inductive Outcome where | next : Nat → Outcome | none\ndef subject (n : Nat) : Outcome := .next (n + 1)\ndef dep0 (n : Nat) := n + 2\n'
    declarations += ''.join(f'def dep{i} (n : Nat) := dep{i-1} n\n' for i in range(1, 101))
    declarations += r'''
theorem law (n : Nat) : (match subject n with
    | .next m => m = n + 1 ∧ dep100 n = dep100 n
    | .none => True) := ⟨rfl, rfl⟩
def witness : (match subject 0 with
    | .next m => m = 0 + 1 ∧ dep100 0 = dep100 0
    | .none => True) := law 0
def changed : (match subject 0 with
    | .next m => m = m ∧ dep100 0 = dep100 0
    | .none => True) := law 0
'''
    driver = r'''import N2Meta
open Lean Meta Vacuity
namespace DependencyControls
DECLARATIONS
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let budget ← IO.mkRef ({} : AnalysisUsage)
  try
    for i in [:100] do
      let e := mkApp (mkConst `DependencyControls.dep100) (mkNatLit i)
      let (found, _) ← (mentions e `DependencyControls.subject (some budget)).run (0, {})
      if found then throwError "negative-dependency-became-positive"
    unless (← budget.get).mentions.contains ((← getConstInfo `DependencyControls.dep0).value!.headBeta, `DependencyControls.subject) do
      throwError "negative-closure-not-cached"
    IO.println s!"DEPENDENCIES complete work={(← budget.get).work}"
  catch ex => IO.println s!"CONTROL {← ex.toMessageData.toString} work={(← budget.get).work}"
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  let law := `DependencyControls.law
  let tags := #[{ name := law, info := (← getConstInfo law), doc := "Law (subject `DependencyControls.subject`)." : Tagged }]
  for name in [`DependencyControls.witness, `DependencyControls.changed] do
    let ci ← getConstInfo name
    let job : AnalysisJob := {
      name, stage := "instance", law := some law, caseName := "inline"
      action := do
        discard <| checkInstance env all tags { name, info := ci, doc := "", law, key := "inline" } #[]
    }
    let (_, state) ← (isolated (← readThe Core.Context) job).run {}
    for f in state.findings do IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
end DependencyControls
'''
    driver = driver.replace('DECLARATIONS', declarations)
    for label, text in [('memoized', consumer), ('uncached', consumer.replace(anchor, '    if false then'))]:
        with compiled_consumer(consumer_text=text) as meta, tempfile.TemporaryDirectory(prefix='n2-dependencies-') as temp:
            path = Path(temp) / 'Dependencies.lean'
            path.write_text(driver)
            (Path(temp) / 'ConcreteSpec.lean').write_text(driver)
            result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
        assert result.returncode == 0, result.stdout
        if label == 'memoized':
            assert re.search(r'^DEPENDENCIES complete work=\d+$', result.stdout, re.M), result.stdout
            assert 'CONTROL ' not in result.stdout, result.stdout
        else:
            assert 'CONTROL work-budget work=20000' in result.stdout, result.stdout
        assert 'NOTE analysis-result DependencyControls.witness stage=instance law=DependencyControls.law case=inline result=PASS ' in result.stdout, result.stdout
        assert 'FAIL instance-application DependencyControls.changed law=DependencyControls.law case=inline' in result.stdout, result.stdout
        assert 'INCOMPLETE ' not in result.stdout, result.stdout
        print(f'PASS dependency {label}: {result.stdout.strip()}', flush=True)


def refutation_controls():
    from vacuity_driver import HERE
    consumer = (HERE / 'vacuity.lean').read_text()
    anchor = '  let key := (closure, simplify)'
    assert consumer.count(anchor) == 1
    instrument = '      let result ← refutableContext simplify'
    assert consumer.count(instrument) == 1
    driver = r'''import N2Meta
open Lean Meta Vacuity
axiom suppliedFalse : False
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  try
    let state : DischargeState := {}
    let (first, state) ← withLocalDeclD `first (mkConst ``Nat) fun _ =>
      dischargeRefutableContext.run state
    let (renamed, state) ← withLocalDeclD `renamed (mkConst ``Nat) fun _ =>
      dischargeRefutableContext.run state
    if first || renamed then throwError "context-cache-invented-contradiction"
    unless state.refutedContexts.size == 1 do throwError "context-cache-not-shared"
    let (impossible, state) ← withLocalDeclD `hypothesis (mkConst ``False) fun _ =>
      dischargeRefutableContext.run state
    unless impossible do throwError "context-cache-crossed-context"
    unless state.refutedContexts.size == 2 do throwError "context-cache-lost-context"
    -- A mutable context must never acquire a refutation cache entry.
    let unknown ← mkFreshExprMVar (mkSort Level.zero)
    let (key, _) ← withLocalDeclD `unknown unknown fun _ => do
      let key ← dischargeClosure (mkConst ``False)
      pure (key, ())
    if key.isSome then throwError "context-cache-retained-metavariable"
    IO.println "CONTEXT CACHE complete shared=1 distinct=2 mutable=uncached"
  catch ex => IO.println s!"CONTROL {← ex.toMessageData.toString}"
'''
    instrumented = consumer.replace(instrument, '      IO.println "CONTEXT SEARCH"\n' + instrument)
    for label, text in [('memoized', instrumented), ('context-erased', instrumented.replace(anchor,
            '  let key := (({ closure := mkConst ``False, instances := #[], declarations := #[] } : DischargeKey), simplify)'))]:
        with compiled_consumer(consumer_text=text) as meta, tempfile.TemporaryDirectory(prefix='n2-refutation-') as temp:
            path = Path(temp) / 'Refutation.lean'
            path.write_text(driver)
            result = command(['lean', path], temp, dict(os.environ, LEAN_PATH=str(meta)), seconds=60)
        assert result.returncode == 0, result.stdout
        if label == 'memoized':
            assert 'CONTEXT CACHE complete shared=1 distinct=2 mutable=uncached' in result.stdout, result.stdout
            assert result.stdout.splitlines().count('CONTEXT SEARCH') == 2, result.stdout
            assert 'CONTROL ' not in result.stdout, result.stdout
        else:
            assert 'CONTROL context-cache-crossed-context' in result.stdout, result.stdout
        print(f'PASS refutation {label}: {result.stdout.strip()}', flush=True)

def main():
    dag_controls()
    dependency_controls()
    refutation_controls()
    with compiled_consumer() as meta:
        return controls(meta)


if __name__ == '__main__':
    raise SystemExit(main())

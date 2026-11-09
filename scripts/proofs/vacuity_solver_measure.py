"""Light finite dependency-DAG census, NOT a discharge solver or completion proof.

Uses exact historical law/Instance/Planted identities from provenance.json.
No beta expansion, application substitution, branch feasibility, or verdict cache.
"""
import io
import json
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile

from vacuity_driver import HERE, ROOT, compiled_consumer, run

DRIVER = r'''
open Lean Meta
partial def censusExpr (root : Expr) (seen : Std.HashSet Expr) : Std.HashSet Expr := Id.run do
  let mut seen := seen
  let mut todo := [root]
  while !todo.isEmpty do
    let e := todo.head!
    todo := todo.tail!
    if seen.contains e then continue
    seen := seen.insert e
    match e with
    | .app f a => todo := f :: a :: todo
    | .lam _ t b _ | .forallE _ t b _ => todo := t :: b :: todo
    | .letE _ t v b _ => todo := t :: v :: b :: todo
    | .mdata _ b | .proj _ _ b => todo := b :: todo
    | _ => pure ()
  return seen
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let start ← IO.getNumHeartbeats
  let env ← getEnv
  let roots := (populationConstants env).filter fun (n, _) => ROOT_FILTER
  unless roots.length == ROOT_COUNT do throwError "census-root-identities"
  let mut seen : Std.HashSet Name := {}
  let mut nodes : Std.HashSet Expr := {}
  let mut pending := roots.map Prod.fst
  let mut edges := 0
  while !pending.isEmpty do
    let n := pending.head!
    pending := pending.tail!
    if seen.contains n then continue
    seen := seen.insert n
    let ci ← getConstInfo n
    let terms := [ci.type] ++ (ci.value? (allowOpaque := true)).toList
    for term in terms do
      nodes := censusExpr term nodes
      for dep in term.getUsedConstants do
        edges := edges + 1
        unless seen.contains dep do pending := dep :: pending
  IO.println s!"CENSUS roots={roots.length} declarations={seen.size} uniqueExpr={nodes.size} constantEdges={edges} userHeartbeats={(← IO.getNumHeartbeats) - start |>.div 1000}"
  for (n, _) in roots do IO.println s!"ROOT {n}"
'''


def main():
    records = json.loads((HERE / 'fixtures/provenance.json').read_text())
    with compiled_consumer() as meta:
        for label, law, scopes in [
            ('c9-new', 'ConcreteSpec.Core.KeyConstruct.collectKeysIn_sound', ['Ident']),
            ('c6-current', 'ConcreteSpec.Core.Observe.trapLine_injective', ['Observe2', 'ObserveEvidence']),
        ]:
            rows = [r for r in records if r['fixture'].startswith(label + '-')]
            suffixes = []
            for row in rows:
                fixture = (HERE / 'fixtures' / row['fixture']).read_text().splitlines()
                line = row['start'] + next(i for i, text in enumerate(fixture) if text.startswith('example '))
                suffixes.append(f'{row["path"]}:{line}»')
            selection = 'n == `' + law + ' || ' + ' || '.join(f'(toString n).endsWith "{s}"' for s in suffixes)
            modules = ['ConcreteSpec.Proofs.' + s for s in scopes]
            driver = ''.join('import ' + s + '\n' for s in ['N2Meta', *modules]) + DRIVER.replace('ROOT_FILTER', selection).replace('ROOT_COUNT', str(len(rows) + 1))
            archive = subprocess.run(['bash', str(ROOT / 'scripts/checks/lib/bounded.sh'), '20',
                'git', 'archive', rows[0]['commit'], 'ConcreteSpec', 'ConcreteSpec.lean', 'ROADMAP.md'],
                cwd=ROOT, stdout=subprocess.PIPE, check=True)
            with tempfile.TemporaryDirectory(prefix='n2-census-') as temp:
                with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as tree:
                    tree.extractall(temp, filter='data')
                code, output = run(Path(temp), ','.join(modules), inventory=True,
                                   driver_text=driver, meta_path=meta)
            print(f'MEASURE {label} commit={rows[0]["commit"]}', flush=True)
            print(output, end='', flush=True)
            if code or 'CENSUS ' not in output:
                return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

"""Bounded ONE-Instance reproduction; never enumerates Grant/Own audit jobs.

The caller supplies an archived source tree and one exact anonymous-name suffix.
--profile instruments a private consumer copy; its heartbeat count includes probe
cost and is diagnostic only. Default runs the unchanged production consumer.
"""
import argparse
import os
from pathlib import Path

from vacuity_driver import HERE, compiled_consumer, run

DRIVER = r'''
import N2Meta
import ConcreteSpec.Proofs.Own
import ConcreteSpec.Proofs.Grant
open Lean Meta Vacuity
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  let selected := all.filter fun (n, _) => SELECTION
  unless selected.length == COUNT do throwError "profile-selection-count"
  for (n, ci) in selected do
    let doc := (← findDocString? env n).getD ""
    let some lawText := field doc "law" | throwError "profile-law"
    let some key := field doc "case" | throwError "profile-case"
    let ((some law), _) ← (resolve all n lawText).run {} | throwError "profile-resolution"
    let lawInfo ← getConstInfo law
    let lawDoc := (← findDocString? env law).getD ""
    let deadline ← readThe Core.Context
    let start ← IO.monoMsNow
    let job : AnalysisJob := {
      name := n, stage := "instance", law := some law, caseName := key
      action := do
        discard <| checkInstance env all #[⟨law, lawInfo, lawDoc⟩] { name := n, info := ci, doc := doc, law := law, key := key } #[]
    }
    let (_, state) ← (isolated deadline job).run {}
    IO.println s!"PROFILE elapsed-ms={(← IO.monoMsNow) - start}"
    for f in state.findings do IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
'''


def instrument(text):
    import re
    text = text.replace('  work : Nat := 0',
        '  profileCharges : Std.HashMap String Nat := {}\n  work : Nat := 0', 1)
    label = ''
    lines = []
    for line in text.splitlines():
        match = re.match(r'(?:partial )?def (\w+)', line)
        if match:
            label = match[1]
        if label not in {'chargeWork', 'useWork'}:
            for call in ['useWork budget', 'chargeWork budget']:
                line = line.replace(call, f'profile_{call.split()[0]} "{label}" budget')
        lines.append(line)
    text = '\n'.join(lines)
    helpers = r'''
initialize profileTimes : IO.Ref (Std.HashMap String (Nat × Nat)) ← IO.mkRef {}
def profileTimed (label : String) (action : MetaM α) : MetaM α := do
  let start ← IO.getNumHeartbeats
  try action
  finally
    let hb := (← IO.getNumHeartbeats) - start
    profileTimes.modify fun s =>
      let (calls, total) := s[label]?.getD (0, 0)
      s.insert label (calls + 1, total + hb)

def profile_chargeWork (label : String) (budget : WorkBudget) (reason : String := "work-budget") : MetaM Unit := do
  budget.modify fun s => { s with profileCharges := s.profileCharges.insert label (s.profileCharges[label]?.getD 0 + 1) }
  chargeWork budget reason

def profile_useWork (label : String) (budget : Option WorkBudget) (reason : String := "work-budget") : MetaM Unit := do
  if let some b := budget then profile_chargeWork label b reason
'''
    text = text.replace('structure AuditState where', helpers + '\nstructure AuditState where')
    for name, parameter, argument in [
        ('refutableType', '(domain : Expr)', 'domain'),
        ('refutableContext', '(simplify : Bool := false)', 'simplify'),
    ]:
        anchor = f'def {name} {parameter}'
        assert text.count(anchor) == 1, name
        text = text.replace(anchor, f'def {name}ProfileInner {parameter}')
        # Place the wrapper before the next definition/documentation block.
        start = text.index(f'def {name}ProfileInner ')
        end = text.index('\n/--' if name == 'refutableType' else '\n/- Solver identities', start)
        wrapper = f'\ndef {name} {parameter} : MetaM Bool :=\n  profileTimed "{name}" ({name}ProfileInner {argument})\n'
        text = text[:end] + wrapper + text[end:]
    anchor = '  let usage ← work.get\n  modify'
    assert text.count(anchor) == 1
    text = text.replace(anchor, '  let usage ← work.get\n'
        '  IO.println s!"PROFILE charges={(usage.profileCharges.toList)}"\n'
        '  IO.println s!"PROFILE times-raw-heartbeats={((← profileTimes.get).toList)}"\n  modify')
    return text


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source', type=Path, required=True)
    parser.add_argument('--suffix', required=True, help='e.g. Own.lean:4258»; must select exactly one declaration')
    parser.add_argument('--consumer', type=Path, default=HERE / 'vacuity.lean')
    parser.add_argument('--profile', action='store_true')
    args = parser.parse_args()
    if not args.suffix.endswith('»') or any(c in args.suffix for c in '\n\r"\\'):
        parser.error('suffix must be a literal anonymous declaration suffix ending in »')
    module = 'ConcreteSpec.Proofs.Grant' if args.suffix.startswith('Grant.lean:') else 'ConcreteSpec.Proofs.Own'
    selection = '(toString n).endsWith "' + args.suffix + '"'
    driver = DRIVER.replace('SELECTION', selection).replace('COUNT', '1')
    driver = driver.replace('import ConcreteSpec.Proofs.Own\nimport ConcreteSpec.Proofs.Grant\n', 'import ' + module + '\n')
    text = args.consumer.read_text()
    if args.profile:
        text = instrument(text)
    print('PROFILE one-instance suffix=' + args.suffix + ' instrumentation=' + str(args.profile), flush=True)
    with compiled_consumer(consumer_text=text) as meta:
        code, output = run(args.source, module, inventory=True, driver_text=driver, meta_path=meta)
    print(output, end='', flush=True)
    if code:
        return code
    if 'INCOMPLETE ' in output:
        return 2
    if 'FAIL ' in output:
        return 1
    return 0


if __name__ == '__main__':
    raise SystemExit(main())

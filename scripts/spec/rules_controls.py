#!/usr/bin/env python3
"""Elaborate every fixture with the production exporter, then validate its output.
Transport mutations happen only after successful Lean elaboration.
"""
import json
from pathlib import Path
import subprocess
import sys

root = Path(__file__).resolve().parents[2]
tmp = Path(sys.argv[1])
base = (root / 'ConcreteSpec/Spec/Rules.lean').read_text().replace(
    'import ConcreteSpec.Spec.Ident', 'import ConcreteSpec.Spec.Ident\nimport Lean')
exporter = (root / 'scripts/spec/rules.lean').read_text().split('-- EXPORTER BODY\n')[1]
rows = tmp / 'rows.md'
rows.write_text('- **Q9.1** Decided.\nR-0001 is only mentioned.\n')
rule = '/-- The rule (Q9.1). -/\ndef ok : Bool := true\ndef other : Bool := true\n'
proofs = 'theorem accepts : ok = true := rfl\ntheorem refuses : ok ≠ false := by decide\n'
registry = '[⟨`ConcreteSpec.Core.ok, 1⟩]'
links = '[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts, .accepted⟩, ⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.refuses, .refused⟩]'
count = 0


def fixture(name, *, decl=rule, proof=proofs, reg=registry, evidence=links,
            reason=None, lean_error=False, mutate=None):
    global count
    src = base.replace('def registeredRules : List RuleId := []',
                       'def registeredRules : List RuleId := ' + reg)
    src = src.replace('namespace ConcreteSpec.Core', 'set_option warningAsError true\nnamespace ConcreteSpec.Core\n' + decl + proof)
    src = src.replace('def registeredRuleExamples : List RuleExample := []',
                      'def registeredRuleExamples : List RuleExample := ' + evidence)
    path = tmp / (name + '.lean')
    path.write_text(src + '\nopen Lean Elab Command\n' + exporter)
    lean = subprocess.run(['lake', 'env', 'lean', str(path)], capture_output=True, text=True)
    if lean_error:
        if lean.returncode == 0 or reason not in lean.stdout + lean.stderr:
            raise SystemExit(f'{name}: expected elaboration failure {reason}: {lean.stdout}{lean.stderr}')
    else:
        if lean.returncode:
            raise SystemExit(f'{name}: elaboration failed: {lean.stdout}{lean.stderr}')
        data = lean.stdout
        if mutate:
            data = mutate(data)
        listing = tmp / (name + '.jsonl')
        listing.write_text(data)
        check = subprocess.run(['python3', '-I', str(root / 'scripts/spec/rules.py'),
                                str(listing), str(rows)], capture_output=True, text=True)
        output = check.stdout + check.stderr
        if (reason is None and check.returncode != 0) or (reason is not None and
                (check.returncode == 0 or reason not in output)):
            raise SystemExit(f'{name}: unexpected validator result {check.returncode}: {output}')
    count += 1
    print(f'  ok   {name}: {reason or "passes"}', flush=True)


fixture('good')
fixture('empty', decl='', proof='', reg='[]', evidence='[]')
fixture('cannot-elaborate', proof=proofs.replace('ok = true := rfl', 'False := trivial').replace('ok ≠ false := by decide', 'False := trivial'),
        lean_error=True, reason='Type mismatch')
fixture('anonymous-cannot-elaborate', proof='example : False := trivial\nexample : False := trivial\n' + proofs,
        lean_error=True, reason='Type mismatch')
fixture('wrong-rule', evidence=links.replace('`ConcreteSpec.Core.ok', '`ConcreteSpec.Core.other'),
        reg='[⟨`ConcreteSpec.Core.ok, 1⟩, ⟨`ConcreteSpec.Core.other, 1⟩]', reason='does not use linked rule')
fixture('unregistered', evidence=links.replace('`ConcreteSpec.Core.ok', '`ConcreteSpec.Core.missing'),
        reason='cites unregistered rule')
fixture('missing-proof', evidence=links.replace('Core.accepts', 'Core.missing'),
        reason='not an elaborated trusted theorem')
fixture('not-theorem', proof='set_option linter.defProp false\n' + proofs.replace('theorem accepts', 'def accepts'),
        reason='not an elaborated trusted theorem')
fixture('sorry-proof', proof=proofs.replace(':= rfl', ':= by sorry'),
        lean_error=True, reason="declaration uses `sorry`")
fixture('untrusted-axiom', proof='axiom fake : ok = true\n' + proofs.replace(':= rfl', ':= fake'),
        reason='not an elaborated trusted theorem')
for verdict, rest in [('Accepted', links.split(', ⟨')[1]), ('Refused', links.split(', ⟨')[0][1:])]:
    evidence = '[⟨' + rest if verdict == 'Accepted' else '[' + rest + ']'
    fixture('missing-' + verdict, evidence=evidence, reason='no example marked ' + verdict)
fixture('duplicate-example', evidence=links[:-1] + ', ⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts, .refused⟩]', reason='linked more than once')
fixture('duplicate-rule', reg='[⟨`ConcreteSpec.Core.ok, 1⟩, ⟨`ConcreteSpec.Core.ok, 1⟩]', reason='registered more than once')
fixture('absent-rule', reg=registry.replace('Core.ok', 'Core.missing'), reason='names no declaration')
fixture('revision-zero', reg=registry.replace(', 1', ', 0'), reason='revision 0')
fixture('no-doc', decl=rule.replace('/-- The rule (Q9.1). -/\n', ''), reason='has no doc comment')
fixture('no-row', decl=rule.replace('Q9.1', 'plain words'), reason='cites no decision row')
fixture('unknown-row', decl=rule.replace('Q9.1', 'Q999.999'), reason='no existing decision row')
fixture('mention-only-row', decl=rule.replace('Q9.1', 'R-0001'), reason='no existing decision row')
fixture('truncated', mutate=lambda s: '\n'.join(s.splitlines()[:-1]), reason='no end record')
for i, bad in enumerate([None, [], {}, {'kind': 'unknown'},
                        {'kind': 'rule', 'name': 'x', 'rev': True, 'exists': True, 'doc': 'Q9.1'}]):
    fixture(f'schema-{i}', mutate=lambda s, b=bad: json.dumps(b) + '\n' + s,
            reason='malformed registry record')
for kind in ('rule', 'example'):
    for mutation in ('surplus', 'wrong-type', 'missing'):
        def change(s, kind=kind, mutation=mutation):
            records = [json.loads(line) for line in s.splitlines()]
            record = next(r for r in records if r.get('kind') == kind)
            if mutation == 'surplus': record['surplus'] = 0
            elif mutation == 'missing': del record['name']
            else: record['exists' if kind == 'rule' else 'elaborated'] = 1
            return '\n'.join(json.dumps(r) for r in records)
        fixture(kind + '-' + mutation, mutate=change, reason='malformed registry record')
fixture('duplicate-field', mutate=lambda s: s.replace('"kind":"rule"', '"kind":"rule","kind":"rule"'), reason='duplicate JSON field')
print(f'SPEC-RULES controls: PASS={count}')

#!/usr/bin/env python3
"""Elaborate every fixture with the production exporter, then validate its output.
Transport mutations happen only after successful Lean elaboration.
"""
import json
import re
import os
import threading
import time
from pathlib import Path
import subprocess
import sys

root = Path(__file__).resolve().parents[2]
tmp = Path(sys.argv[1])
base = (root / 'ConcreteSpec/Spec/Rules.lean').read_text().replace(
    'import ConcreteSpec.Spec.Ident', 'import ConcreteSpec.Spec.Ident\nimport Lean')
# Isolate synthetic fixtures from production registrations and conformance declarations.
# Keep the actual registry types and evidence wrappers, so every control exercises them.
base = re.sub(r'def registeredRules : List RuleId :=.*?(?=\n/--)',
              'def registeredRules : List RuleId := []\n', base, flags=re.S)
base = re.sub(r'namespace Own.Registry.*?end Own.Registry\n', '', base, flags=re.S)
base = re.sub(r'def registeredRuleExamples : List RuleExample :=.*?(?=\nend ConcreteSpec.Core)',
              'def registeredRuleExamples : List RuleExample := []\n', base, flags=re.S)
exporter = (root / 'scripts/spec/rules.lean').read_text().split('-- EXPORTER BODY\n')[1]
rows = tmp / 'rows.md'
rows.write_text('- **Q9.1** Decided.\nR-0001 is only mentioned.\n')
rule = '/-- The rule (Q9.1). -/\ndef ok (n : Nat) : Prop := n = 0\ndef other (n : Nat) : Prop := n = 0\n'
proofs = 'theorem accepts : RuleAccepted ok 0 := rfl\ntheorem refuses : RuleRefused ok 1 := by intro h; cases h\n'
registry = '[⟨`ConcreteSpec.Core.ok, 1⟩]'
links = '[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩, ⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.refuses⟩]'
count = 0


def fixture(name, *, decl=rule, proof=proofs, reg=registry, evidence=links,
            reason=None, lean_error=False, mutate=None):
    global count
    src = base.replace('def registeredRules : List RuleId := []',
                       'def registeredRules : List RuleId := ' + reg)
    src = src.replace('namespace ConcreteSpec.Core', 'set_option warningAsError true\nnamespace ConcreteSpec.Core\n' + decl)
    src = src.replace('def registeredRuleExamples : List RuleExample := []',
                      'def registeredRuleExamples : List RuleExample := ' + evidence)
    path = tmp / (name + '.lean')
    path.write_text(src.replace('/-- Complete conformance links', proof + '\n/-- Complete conformance links') + '\nopen Lean Elab Command\n' + exporter)
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
        check = subprocess.run(['bash', str(root / 'scripts/spec/rules_validate.sh'),
                                str(listing), str(rows)], capture_output=True, text=True)
        output = check.stdout + check.stderr
        if (reason is None and check.returncode != 0) or (reason is not None and
                (check.returncode == 0 or reason not in output)):
            raise SystemExit(f'{name}: unexpected validator result {check.returncode}: {output}')
    count += 1
    print(f'  ok   {name}: {reason or "passes"}', flush=True)


fixture('good')
fixture('empty', decl='', proof='', reg='[]', evidence='[]')
fixture('cannot-elaborate', proof=proofs.replace('RuleAccepted ok 0 := rfl', 'False := trivial').replace('RuleRefused ok 1 := by intro h; cases h', 'False := trivial'),
        lean_error=True, reason='Type mismatch')
fixture('anonymous-cannot-elaborate', proof='example : False := trivial\nexample : False := trivial\n' + proofs,
        lean_error=True, reason='Type mismatch')
fixture('tautology', proof='theorem accepts : ok = ok := rfl\ntheorem refuses : ok = ok := rfl\n',
        reason='no concrete rule acceptance/refusal proposition')
fixture('acceptance-as-refusal', proof=proofs.replace('RuleRefused ok 1 := by intro h; cases h', 'RuleAccepted ok 0 := rfl'),
        reason='no example marked Refused')
fixture('nonconcrete-input', proof=proofs.replace('theorem accepts : RuleAccepted ok 0 := rfl',
        'theorem accepts (n : Nat) (h : ok n) : RuleAccepted ok n := h'),
        reason='no concrete rule acceptance/refusal proposition')
fixture('closed-opaque-input', decl=rule.replace('n = 0', 'True').replace('(n : Nat)', '(_n : Nat)') + 'opaque hidden : Nat := 0\n',
        proof='theorem accepts : RuleAccepted ok hidden := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
        reason='no concrete rule acceptance/refusal proposition')
fixture('computed-concrete-input', proof=proofs.replace('RuleAccepted ok 0', 'RuleAccepted ok (1 - 1)'))
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
fixture('untrusted-axiom', proof='axiom fake : RuleAccepted ok 0\n' + proofs.replace(':= rfl', ':= fake'),
        reason='not an elaborated trusted theorem')
for verdict, rest in [('Accepted', links.split(', ⟨')[1]), ('Refused', links.split(', ⟨')[0][1:])]:
    evidence = '[⟨' + rest if verdict == 'Accepted' else '[' + rest + ']'
    fixture('missing-' + verdict, evidence=evidence, reason='no example marked ' + verdict)
fixture('duplicate-example', evidence=links[:-1] + ', ⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]', reason='linked more than once')
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

# An input directory is unreadable as a listing even when the gate runs as root.
check = subprocess.run(['bash', str(root / 'scripts/spec/rules_validate.sh'), str(tmp), str(rows)],
                       capture_output=True, text=True)
assert check.returncode == 3 and 'Is a directory' in check.stderr, check
print('  ok   unreadable-input: input read error')

# Block the actual production validator while opening its input. Both bounded attempts must
# expire; partial output cannot become success, and the wrapper must report INCOMPLETE.
fifo = tmp / 'hung-input'
os.mkfifo(fifo)
env = dict(os.environ, RULES_CHECK_SECONDS='0.1', RULES_CHECK_RETRY_SECONDS='0.2')
check = subprocess.run(['bash', str(root / 'scripts/spec/rules_validate.sh'), str(fifo), str(rows)],
                       env=env, capture_output=True, text=True)
assert check.returncode == 124 and 'INCOMPLETE' in check.stderr and 'incomplete 0.1s+0.2s' in check.stdout, check
assert 'registered rules' not in check.stdout, check
print('  ok   hung-checker: both timeouts are INCOMPLETE (124), no success output')

# Exercise first-timeout recovery too: the retry receives a complete real export.
recovery_fifo = tmp / 'recovering-input'
os.mkfifo(recovery_fifo)
def supply_export():
    time.sleep(1)
    with recovery_fifo.open('w') as stream:
        stream.write((tmp / 'good.jsonl').read_text())
writer = threading.Thread(target=supply_export, daemon=True)
writer.start()
check = subprocess.run(['bash', str(root / 'scripts/spec/rules_validate.sh'), str(recovery_fifo), str(rows)],
                       env=dict(os.environ, RULES_CHECK_SECONDS='0.1', RULES_CHECK_RETRY_SECONDS='5'),
                       capture_output=True, text=True)
writer.join(1)
assert check.returncode == 0 and 'recovered 0.1s' in check.stdout and '1 registered rules' in check.stdout, check
print('  ok   hung-first-attempt: retry validates real export and reports recovered')
print(f'SPEC-RULES controls: PASS={count + 3}')

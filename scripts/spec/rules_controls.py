#!/usr/bin/env python3
"""Elaborate every fixture with the production exporter, then validate its output.
Transport mutations happen only after successful Lean elaboration.
"""
import json
import hashlib
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
base = base.replace('import ConcreteSpec.Spec.Const\n', '').replace('import ConcreteSpec.Spec.Own\n', '')
base = re.sub(r'namespace Leak.Registry.*?end Leak.Registry\n', '', base, flags=re.S)
base = re.sub(r'namespace ConstRegistry.*?end ConstRegistry\n', '', base, flags=re.S)
base = re.sub(r'namespace Own.Registry.*?end Own.Registry\n', '', base, flags=re.S)
base = re.sub(r'def registeredRuleExamples : List RuleExample :=.*?(?=\nend ConcreteSpec.Core)',
              'def registeredRuleExamples : List RuleExample := []\n', base, flags=re.S)
exporter = (root / 'scripts/spec/rules.lean').read_text().split('-- EXPORTER BODY\n')[1]
rows = tmp / 'rows.md'
rows.write_text('- **Q9.1** Decided (owner: approved).\nR-0001 is only mentioned.\n')
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
        # These acceptance-only fixtures deliberately define a universally true rule.
        # Require exactly its missing-refusal diagnostic: any invalid data fails.
        if reason is None and evidence == '[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]':
            expected = '  FAIL rule ConcreteSpec.Core.ok has no example marked Refused (rule `ConcreteSpec.Core.ok`)'
            if check.returncode != 1 or output.strip() != expected.strip():
                raise SystemExit(f'{name}: expected only missing refusal: {output}')
        elif (reason is None and check.returncode != 0) or (reason is not None and
                (check.returncode == 0 or reason not in output)):
            raise SystemExit(f'{name}: unexpected validator result {check.returncode}: {output}')
    count += 1
    print(f'  ok   {name}: {reason or "passes"}', flush=True)


# Authority fixtures use the real decision inventory, including the refused anchors.
# A valid citation must never mask a non-current citation in either order.
production_rows = (root / 'docs/decisions.md').read_text()
# Migration control: SHA-256 of 2362cb403:docs/decisions.md after stripping only
# list markers immediately before explicit anchors, then all Python Unicode whitespace.
# New recorded owner decisions require an explicit update of this migration baseline.
normalized_inventory = re.sub(r'(?m)^\s*- (?=<a id=")', '', production_rows)
normalized_inventory = re.sub(r'\s', '', normalized_inventory)
assert hashlib.sha256(normalized_inventory.encode()).hexdigest() == (
    '2573410e6e76e93b282ea9684a4c6cb6091713221ec409708a5c32743ef28735'
), 'decision inventory wording differs from 2362cb403 (excluding anchor list markers)'
print('  ok   inventory migration: wording and anchors equal 2362cb403')

rows.write_text(production_rows)
valid = 'decided-2026-11-08-foundation'
for anchor in [valid, 'decided-project-memory-2026-11-06',
               'decided-2026-11-14-constant-types', 'recovered-29-18',
               'recovered-29-19', 'recovered-59-26']:
    fixture('authority-current-' + anchor,
            decl=rule.replace('(Q9.1)', '(docs/decisions.md#' + anchor + ')'))
refused = {
    'recovered-59-16': 'ISS-0083',
    'decided-2026-11-13-d4-recovery': 'ISS-0083',
    'OPEN-38': 'ISS-0083',
    'decided-2026-11-08-recovery-queue': 'ISS-0083',
    'decided-2026-11-08-spelling-round': 'open in Waiting for the owner',
    'recovered-29-22': 'disposition in title: partial',
}
for anchor, diagnostic in refused.items():
    assert '<a id="' + anchor + '"></a>' in production_rows
    for order, anchors in [('alone', [anchor]), ('first', [anchor, valid]),
                           ('last', [valid, anchor])]:
        fixture('authority-' + anchor + '-' + order,
                decl=rule.replace('(Q9.1)', '(' + ', '.join(
                    'docs/decisions.md#' + a for a in anchors) + ')'), reason=diagnostic)
for punctuation in ['.', ':', ';', ',']:
    fixture('authority-punctuation-' + str(ord(punctuation)),
            decl=rule.replace('(Q9.1).', 'docs/decisions.md#' + valid + punctuation))
for name, citation in [
    ('unknown', 'docs/decisions.md#decided-missing'),
    ('empty', 'Q9.1, docs/decisions.md#'),
    ('prefix', 'docs/decisions.md#' + valid + '_extra'),
    ('suffix', 'docs/decisions.md#' + valid + '-more'),
    ('two-dots', 'docs/decisions.md#' + valid + '..'),
    ('four-dots', 'docs/decisions.md#' + valid + '....'),
    ('roadmap', 'ROADMAP.md#' + valid),
    ('proposal', 'docs/proposals/FOUNDATION.md#' + valid),
    ('relative', '../docs/decisions.md#' + valid),
    ('bare', '#' + valid),
]:
    fixture('authority-invalid-' + name,
            decl=rule.replace('(Q9.1)', '(' + citation + ')'),
            reason=('wrong authority path' if name in ['roadmap', 'proposal', 'relative', 'bare']
                    else 'missing authority anchor'))
for name, prose in [('attribute', '#[proof(example)]'), ('heading', '\n# Example heading\n')]:
    fixture('authority-noncitation-' + name, decl=rule.replace('(Q9.1)',
            '(docs/decisions.md#' + valid + ') ' + prose))
# Preserve main's row existence rule, including governing rows with revision notes.
for row in ['Q4.7', 'OD47', 'OD42', 'OD8', 'OD137', 'OD94']:
    fixture('authority-row-' + row, decl=rule.replace('Q9.1', row))
fixture('authority-real-roadmap', decl=rule.replace('Q9.1', 'R-0578'), reason='roadmap-only')
for name, entry in [
    ('proposal', '- <a id="control"></a>*Proposal for the owner to review*.\n'),
    ('linear', '- <a id="control"></a>**Rule**. The linear owner releases its slot.\n'),
    ('questions', '## Owner questions\n- <a id="control"></a>*Open item*. Options a or b.\n'),
]:
    rows.write_text(production_rows + '\n' + entry)
    fixture('authority-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#control'), reason='ISS-0083')
# A dated heading alone is not a provenance-bearing decision record.
rows.write_text(production_rows + '\n- <a id="decided-2026-11-15-control"></a>'
                '**Control (2026-11-15).**\n  A decision-shaped heading alone.\n')
fixture('authority-dated-no-provenance',
        decl=rule.replace('Q9.1', 'docs/decisions.md#decided-2026-11-15-control'),
        reason='ISS-0083')
for disposition in ['held', 'queued']:
    rows.write_text(production_rows + '\n- <a id="control"></a>**' + disposition +
                    '** (owner, 2026-11-08: "ok").\n')
    for order, anchors in [('first', ['control', valid]), ('last', [valid, 'control'])]:
        fixture('authority-title-' + disposition + '-' + order,
                decl=rule.replace('Q9.1', ', '.join('docs/decisions.md#' + a for a in anchors)),
                reason='disposition in title: ' + disposition)
# CommonMark boundary and inline regressions from both independent reviewers.
control_anchor = 'decided-2026-11-15-control'
opening = '- <a id="' + control_anchor + '"></a>**Control (2026-11-15).**\n'
for name, entry, anchor, diagnostic in [
    ('level-three-boundary', opening + '\n### Separate evidence log\n  Provenance: unrelated.\n  Owner reply: unrelated.\n', control_anchor, 'ISS-0083'),
    ('nested-bullet-provenance', opening + '  Owner reply: yes.\n\n  - Child\n\n    Provenance: nested.\n', control_anchor, 'ISS-0083'),
    ('nested-numbered-provenance', opening + '  Owner reply: yes.\n\n  1. Child\n\n     Provenance: nested.\n', control_anchor, 'ISS-0083'),
    ('dated-no-owner-reply', opening + '  Provenance: session.\n', control_anchor, 'ISS-0083'),
    ('held-code-title', '- <a id="control"></a>**Rule `held`** (owner, 2026-11-15: "ok").\n', 'control', 'disposition in title: held'),
    ('softbroken-owner-marker', '- <a id="control"></a>**Rule** (owner,\n  2026-11-15: "ok").\n', 'control', 'ISS-0083'),
    ('hardbroken-owner-marker', '- <a id="control"></a>**Rule** (owner,  \n  2026-11-15: "ok").\n', 'control', 'ISS-0083'),
    ('intervening-code', '- <a id="control"></a>**Rule** `intervening` (owner, 2026-11-15: "ok").\n', 'control', 'ISS-0083'),
]:
    rows.write_text(production_rows + '\n' + entry)
    fixture('authority-commonmark-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + anchor), reason=diagnostic)
rows.write_text(production_rows.replace('**29-list item 18: RECOVERED.**',
                                      '**29-list item 18 (*old*): SUPERSEDED.**'))
fixture('authority-commonmark-original-recovered-nested-emphasis',
        decl=rule.replace('Q9.1', 'docs/decisions.md#recovered-29-18'),
        reason='disposition in title: superseded')
rows.write_text(production_rows.replace('- <a id="recovered-29-18"></a>**29-list item 18: RECOVERED.**',
    '- <a id="recovered-29-18"></a>**29-list item 18 (*old*): SUPERSEDED.**'))
fixture('authority-commonmark-nested-emphasis',
        decl=rule.replace('Q9.1', 'docs/decisions.md#recovered-29-18'),
        reason='disposition in title: superseded')
rows.write_text(production_rows.replace('## Waiting for the owner\n',
    '## Waiting for the owner\n\n1. Closed 2026-11-15: complete.\n\n'
    '- [Open control](#' + valid + ')\n\n'))
fixture('authority-commonmark-open-bullet-after-closed-number',
        decl=rule.replace('Q9.1', 'docs/decisions.md#' + valid),
        reason='open in Waiting for the owner')
rows.write_text(production_rows)
for name, prose in [
    ('mismatched-backticks', '``docs/decisions.md#OPEN-38`'),
    ('cross-paragraph-backticks', '`stray\n\ndocs/decisions.md#OPEN-38\n\nstray`'),
    ('same-line-triple-backticks', '\n```inline``` docs/decisions.md#OPEN-38'),
    ('multi-backtick-following', '``code ` inside`` docs/decisions.md#OPEN-38'),
]:
    fixture('authority-commonmark-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + valid + '\n' + prose),
            reason='OPEN-38: authority status not machine-readable')
# All three accepted forms, ordered items and nested items remain usable.
for name, entry in [
    ('dated', opening + '\n  Provenance: session.\n\n  Owner reply: yes.\n\n  ```text\n  yes\n  ```\n'),
    ('ordered', '1. <a id="' + control_anchor + '"></a>*Control* (owner, 2026-11-15: "yes").\n'),
    ('nested', '- Parent\n  - <a id="' + control_anchor + '"></a>**Control** (owner, 2026-11-15: "yes").\n'),
]:
    rows.write_text(production_rows + '\n' + entry)
    fixture('authority-commonmark-accepted-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + control_anchor))
rows.write_text('- <a id="decided-2026-11-13-d4-recovery"></a>**D4**\n\n'
    '  **Already recovered authority, independently checked; existing records retained.**\n\n'
    '  | Report row | Existing record |\n  |---|---|\n'
    '  | 1 | [record](#recovered-control) |\n\n'
    '- <a id="recovered-control"></a>**Recovered**\n')
fixture('authority-commonmark-accepted-retained',
        decl=rule.replace('Q9.1', 'docs/decisions.md#recovered-control'))
# Round 7: every concrete round-5/6 reproducer must name its refusal.
for name, entry, anchor, diagnostic in [
    ('image-held', '- <a id="control"></a>**Rule ![held](x)** (owner, 2026-11-15: "ok").\n', 'control', 'unsupported title construct: image'),
    ('code-provenance', opening + '\n  `Provenance: transcript`\n\n  Owner reply: yes.\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('code-owner-reply', opening + '\n  Provenance: transcript\n\n  `Owner reply: verbatim`\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('negated-owner-reply', opening + '\n  Provenance: audit note.\n\n  Owner reply is missing; held for confirmation.\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('softwrapped-owner-reply', opening + '\n  Provenance: audit note.\n\n  Missing header\n  Owner reply: yes.\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('duplicate-entry', '- <a id="control"></a>**Held**\n- <a id="control"></a>**Rule** (owner, 2026-11-15: "ok").\n', 'control', 'duplicate authority anchor: control'),
    ('qualified-owner-reply', opening + '\n  Provenance: audit note.\n\n  Owner reply, probably yes:\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('linked-provenance', opening + '\n  [Provenance:](x) transcript\n\n  Owner reply:\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('linked-owner-reply', opening + '\n  Provenance: transcript\n\n  [Owner reply:](x)\n\n  ```text\n  yes\n  ```\n', control_anchor, 'ISS-0083'),
    ('duplicate-table', '| Entry |\n|---|\n| <a id="control"></a>Text |\n\n- <a id="control"></a>**Rule** (owner, 2026-11-15: "ok").\n', 'control', 'duplicate authority anchor: control'),
    ('duplicate-navigation', '- <a id="waiting-for-the-owner"></a>**Rule** (owner, 2026-11-15: "ok").\n', 'waiting-for-the-owner', 'duplicate authority anchor'),
]:
    rows.write_text(production_rows + '\n' + entry)
    fixture('authority-closed-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + anchor), reason=diagnostic)
for name, title, diagnostic in [
    ('link', '[held](x)', 'link_open'), ('html', '<span>held</span>', 'html_inline'),
    ('autolink', '<https://example.org>', 'link_open'), ('footnote', '[^held]', 'footnote-like syntax'),
    ('softbreak', 'Rule\n  held', 'softbreak'), ('hardbreak', 'Rule  \n  held', 'hardbreak'),
]:
    rows.write_text(production_rows + '\n- <a id="control"></a>**' + title + '** (owner, 2026-11-15: "ok").\n')
    fixture('authority-title-allowlist-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#control'),
            reason='unsupported title construct: ' + diagnostic)
for prefix in ['`Closed`', '[Closed](x)', 'Not Closed', 'Possibly Closed']:
    rows.write_text(production_rows.replace('## Waiting for the owner\n',
        '## Waiting for the owner\n\n- ' + prefix + ' [control](#' + valid + ')\n\n'))
    fixture('authority-closed-marker-' + str(len(prefix)) + prefix[:1],
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + valid),
            reason='open in Waiting for the owner')
# Round 8: waiting destinations use exactly the citation normalizer.
for name, link, diagnostic in [
    ('review-reproducer', '[control](docs/decisions.md#' + valid + ')', 'open in Waiting for the owner'),
    ('dot', '[control](./docs/decisions.md#' + valid + ')', 'open in Waiting for the owner'),
    ('parent', '[control](docs/../docs/decisions.md#' + valid + ')', 'open in Waiting for the owner'),
    ('reference', '[control][waiting-control]\n\n[waiting-control]: ./docs/decisions.md#' + valid, 'open in Waiting for the owner'),
    ('encoded', '[control](docs/%64ecisions.md#' + valid + ')', 'open in Waiting for the owner'),
    ('wrong-path', '[control](other/decisions.md#' + valid + ')', 'wrong authority path'),
    ('autolink', '<https://example.org/docs/decisions.md#' + valid + '>', 'wrong authority path'),
    ('query', '[control](docs/decisions.md?unsupported#' + valid + ')', 'wrong authority path'),
]:
    rows.write_text(production_rows.replace('## Waiting for the owner\n',
        '## Waiting for the owner\n\n- Still open ' + link + '\n\n'))
    fixture('authority-waiting-destination-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + valid), reason=diagnostic)
rows.write_text(production_rows)
for name, target, diagnostic in [
    ('open', 'docs/decisions.md#OPEN-38', 'ISS-0083'),
    ('waiting', 'docs/decisions.md#decided-2026-11-08-spelling-round', 'open in Waiting'),
    ('partial', 'docs/decisions.md#recovered-29-22', 'disposition in title: partial'),
    ('missing', 'docs/decisions.md#missing', 'missing authority anchor'),
    ('wrong', 'ROADMAP.md#' + valid, 'wrong authority path'),
]:
    for kind in ['inline', 'reference', 'image']:
        linked = ('![label](' + target + ')' if kind == 'image' else
                  '[label](' + target + ')' if kind == 'inline' else
                  '[label][ref]\n\n[ref]: ' + target)
        for order in ['first', 'last']:
            prose = ('docs/decisions.md#' + valid + '\n' + linked if order == 'last'
                     else linked + '\n\ndocs/decisions.md#' + valid)
            fixture('authority-link-' + name + '-' + kind + '-' + order,
                    decl=rule.replace('Q9.1', prose), reason=diagnostic)
for target in ['docs/decisions.md', './docs/decisions.md', 'docs/../docs/decisions.md']:
    fixture('authority-relative-link-' + str(len(target)),
            decl=rule.replace('Q9.1', '[authority](' + target + '#' + valid + ')'))
fixture('authority-reference-beside-row', decl=rule.replace('Q9.1',
        'Q4.7 [authority][ref]\n\n[ref]: docs/decisions.md#OPEN-38'), reason='ISS-0083')
fixture('authority-link-context', decl=rule.replace('Q9.1',
        '[authority](./docs/decisions.md#' + valid + '), #missing'), reason='missing authority anchor')
rows.write_text(production_rows + '\n- <a id="control"></a>**Rule *with* `code`** (owner, 2026-11-15: "ok").\n')
fixture('authority-allowed-title', decl=rule.replace('Q9.1', 'docs/decisions.md#control'))
rows.write_text(production_rows + '\n- <a id="control"></a>**he*ld*** (owner, 2026-11-15: "ok").\n')
fixture('authority-nested-disposition', decl=rule.replace('Q9.1', 'docs/decisions.md#control'),
        reason='disposition in title: held')
rows.write_text(production_rows)
fixture('authority-code-boundary', decl=rule.replace('Q9.1',
        'docs/decisions.md#' + valid + ' `code`#not-a-citation'))
rows.write_text(production_rows)
for name, code in [('inline', '`#eval Q999.1`'),
                   ('fence', '\n```lean\n#eval Q999.1\n```\n')]:
    fixture('authority-code-' + name,
            decl=rule.replace('Q9.1', 'docs/decisions.md#' + valid + ' ' + code))
rows.write_text('- **Q9.1** Decided (owner: approved).\nR-0001 is only mentioned.\n')
fixture('authority-row-only-existing')
fixture('authority-row-only-missing', decl=rule.replace('Q9.1', 'Q999.1'),
        reason='no existing decision row')
fixture('authority-row-only-mixed', decl=rule.replace('Q9.1', 'Q9.1, Q999.1'))
fixture('authority-roadmap-mention', decl=rule.replace('Q9.1', 'R-0001'), reason='roadmap-only')

# C7 adapters keep the strict traversal: each production input shape accepts closed data,
# but an opaque value of exactly that shape cannot certify an inspectable instance.
for name, carrier, value in [
    ('c7-account', 'Leak.Account', '⟨8, 5, 8, 8⟩'),
    ('c7-signed', 'Int × Leak.SignedInterval', '(-3, ⟨-4, -2, .boundEstablished⟩)'),
    ('c7-reset', 'Leak.Account × Leak.Account × Nat', '(⟨8, 5, 8, 8⟩, ⟨8, 0, 8, 8⟩, 5)'),
    ('c7-report', 'LineageId × Leak.Account × Leak.Account × Option Nat × Leak.LineageReport',
     '(⟨0⟩, ⟨8, 5, 8, 8⟩, ⟨8, 0, 8, 8⟩, some 10, '
     '⟨⟨0⟩, ⟨0, .boundEstablished⟩, ⟨-5, -5, .boundEstablished⟩, '
     '⟨3, .boundEstablished⟩, ⟨10, .boundEstablished⟩⟩)'),
    ('c7-bound', 'Nat × Leak.Bound Nat', '(10, ⟨12, .boundEstablished⟩)'),
    ('c7-optional', 'Option Nat × Leak.Bound Nat', '(some 10, ⟨12, .boundEstablished⟩)'),
    ('c7-action', 'List (LineageId × Leak.Account) × Leak.Action',
     '([(⟨0⟩, ⟨100, 5, 12, 9⟩)], .free ⟨0⟩ 5)'),
    ('c7-sequence', 'List (LineageId × Leak.Account) × List Leak.Action',
     '([(⟨0⟩, ⟨100, 5, 12, 9⟩)], [.free ⟨0⟩ 5, .reset ⟨0⟩])'),
]:
    declaration = '/-- The rule (Q9.1). -/\ndef ok (_ : ' + carrier + ') : Prop := True\n'
    fixture(name + '-closed', decl=declaration,
            proof='theorem accepts : RuleAccepted ok (' + value + ') := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]')
    fixture(name + '-opaque', decl='opaque hidden : ' + carrier + ' := ' + value + '\n' + declaration,
            proof='theorem accepts : RuleAccepted ok hidden := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
            reason='no concrete rule acceptance/refusal proposition')

# Each fixture isolates one round-4 traversal bypass.
for name, prelude, carrier, value in [
    ('opaque-data-parameter', 'opaque hidden : Nat := 0\nstructure Carrier (n : Nat) where\n  data : Bool\n', 'Carrier hidden', '⟨true⟩'),
    ('prop-field-family', '', 'PSigma (fun _ : Prop => Bool)', '⟨∀ n : Nat, n = n, true⟩'),
    ('opaque-prop-field', 'opaque hidden : Prop := True\n', 'PSigma (fun _ : Prop => Bool)', '⟨hidden, true⟩'),
    ('type-field-opaque-index', 'opaque hidden : Nat := 0\n', 'PSigma (fun _ : Type => Bool)', '⟨Fin hidden, true⟩'),
    *[(f'opaque-index-in-{name}-parameter',
       'opaque hidden : Nat := 0\ninductive Carrier : Nat → Type where\n  | mk : Carrier hidden\n', carrier, value)
      for name, carrier, value in [
          ('sum', 'Sum Bool (Carrier hidden)', 'Sum.inl true'),
          ('option', 'Option (Carrier hidden)', 'none'),
          ('list', 'List (Carrier hidden)', '[]'),
          ('prod', 'Prod Bool (Option (Carrier hidden))', '⟨true, none⟩')]],
]:
    fixture(name, decl=prelude + '/-- The rule (Q9.1). -/\ndef ok (_p : ' + carrier + ') : Prop := True\n',
            proof='theorem accepts : RuleAccepted ok (' + value + ' : ' + carrier + ') := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
            reason='no concrete rule acceptance/refusal proposition')

# Direct fields isolate the sort-field refusal from PSigma's function parameter.
for name, field_type, field_value, prelude in [
    ('direct-prop-field-family', 'Prop', '∀ n : Nat, n = n', ''),
    ('direct-opaque-prop-field', 'Prop', 'hidden', 'opaque hidden : Prop := True\n'),
    ('direct-type-field-opaque-index', 'Type', 'Fin hidden', 'opaque hidden : Nat := 0\n'),
]:
    fixture(name,
            decl=prelude + 'structure Carrier where\n  runtime : ' + field_type + '\n  data : Bool\n'
                 '/-- The rule (Q9.1). -/\ndef ok (_p : Carrier) : Prop := True\n',
            proof='theorem accepts : RuleAccepted ok ⟨' + field_value + ', true⟩ := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
            reason='no concrete rule acceptance/refusal proposition')
# Concrete type and data parameters remain admissible, including empty containers.
for name, prelude, carrier, value in [
    ('concrete-data-parameter', 'structure Carrier (n : Nat) where\n  data : Bool\n', 'Carrier 0', '⟨true⟩'),
    ('concrete-sum-parameter', '', 'Sum Bool (Fin 2)', 'Sum.inl true'),
    ('concrete-option-parameter', '', 'Option (Fin 2)', 'none'),
    ('concrete-list-parameter', '', 'List (Fin 2)', '[]'),
    ('concrete-prod-parameter', '', 'Prod Bool (Option (Fin 2))', '⟨true, none⟩'),
]:
    fixture(name,
            decl=prelude + '/-- The rule (Q9.1). -/\ndef ok (_p : ' + carrier + ') : Prop := True\n',
            proof='theorem accepts : RuleAccepted ok (' + value + ' : ' + carrier + ') := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]')

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
fixture('irreducible-data',
        decl=rule.replace('n = 0', 'True').replace('(n : Nat)', '(_n : Nat)') +
             '@[irreducible] def hidden : Nat := 0\n',
        proof='theorem accepts : RuleAccepted ok hidden := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
        reason='no concrete rule acceptance/refusal proposition')
# Trusted theorems can still describe proposition inputs or unknown runtime data.
fixture('proposition-input',
        decl='/-- The rule (Q9.1). -/\ndef ok (_p : True) : Prop := True\nopaque hidden : True := True.intro\n',
        proof='theorem accepts : RuleAccepted ok hidden := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
        reason='no concrete rule acceptance/refusal proposition')
for name, carrier, value in [
    ('opaque-constructor-parameter',
     'structure Carrier (p : Prop) : Type where\n  valid : p\n',
     '(⟨rfl⟩ : Carrier (hidden = hidden))'),
    ('opaque-dependent-proof-proposition',
     'structure Carrier : Type where\n  data : Nat\n  valid : hidden = hidden ∧ data = data\n',
     '(⟨0, ⟨rfl, rfl⟩⟩ : Carrier)'),
    ('opaque-constructor-index',
     'inductive Carrier : Nat → Type where\n  | mk : Carrier hidden\n',
     'Carrier.mk'),
]:
    fixture(name,
            decl='opaque hidden : Nat := 0\n' + carrier +
                 '/-- The rule (Q9.1). -/\ndef ok (_p : ' +
                 ('Carrier (hidden = hidden)' if name == 'opaque-constructor-parameter' else
                  'Carrier hidden' if name == 'opaque-constructor-index' else 'Carrier') +
                 ') : Prop := True\n',
            proof='theorem accepts : RuleAccepted ok ' + value + ' := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
            reason=('no concrete rule acceptance/refusal proposition'
                    if name != 'opaque-dependent-proof-proposition' else None))
fixture('aliased-opaque-proof-proposition',
        decl='opaque hidden : Nat := 0\ndef aliasHidden : Nat := hidden\n'
             'structure Carrier : Type where\n  valid : aliasHidden = aliasHidden\n'
             '/-- The rule (Q9.1). -/\ndef ok (_p : Carrier) : Prop := True\n',
        proof='theorem accepts : RuleAccepted ok ⟨rfl⟩ := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]')
fixture('erased-opaque-data',
        decl=rule + 'opaque hidden : Nat := 0\n',
        proof=proofs.replace('RuleAccepted ok 0', 'RuleAccepted ok ((fun _ : Nat => 0) hidden)'))
fixture('irreducible-constructor-parameter',
        decl='@[irreducible] def hidden : Nat := 0\n'
             'structure Carrier (p : Prop) : Type where\n  valid : p\n'
             '/-- The rule (Q9.1). -/\ndef ok (_p : Carrier (hidden = hidden)) : Prop := True\n',
        proof='theorem accepts : RuleAccepted ok ⟨rfl⟩ := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
        reason='no concrete rule acceptance/refusal proposition')
# Closed proof types and erased annotations do not change the reduced data example.
for name, extra, carrier, value in [
    ('theorem-proposition-body', 'theorem witness : True := let _ := hidden; True.intro\n',
     'structure Carrier (p : True) where\n  data : Bool\n', '(⟨true⟩ : Carrier witness)'),
    ('theorem-proof-field-body', 'theorem witness : True := let _ := hidden; True.intro\n',
     'structure Carrier : Type where\n  data : Bool\n  valid : True\n', '(⟨true, witness⟩ : Carrier)'),
    ('constructor-erased-domain', '',
     'inductive Carrier where\n  | mk : (let _ := hidden; Bool) → Carrier\n', '(Carrier.mk true)'),
    ('inductive-proof-domain', '',
     'inductive P : Prop where\n  | mk : (let _ := hidden; False) → P\n'
     'structure Carrier : Type where\n  data : Bool\n  valid : P → True\n', '(⟨true, fun _ => True.intro⟩ : Carrier)'),
    ('let-proof-field-domain', '',
     'structure Carrier : Type where\n  data : Bool\n  valid : (let _ := hidden; True)\n', '(⟨true, True.intro⟩ : Carrier)'),
]:
    fixture(name, decl='opaque hidden : Nat := 0\n' + extra + carrier +
            '/-- The rule (Q9.1). -/\ndef ok (_p : ' +
            ('Carrier witness' if name == 'theorem-proposition-body' else 'Carrier') + ') : Prop := True\n',
            proof='theorem accepts : RuleAccepted ok ' + value + ' := True.intro\n',
            evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]')
fixture('nested-definition-signature',
        decl=rule + 'opaque hidden : Nat := 0\ndef aliasHidden : (let _ := hidden; Nat) := 0\n',
        proof=proofs.replace('RuleAccepted ok 0', 'RuleAccepted ok (aliasHidden + 0)'))
fixture('wrapper-erased-type', decl=rule + 'opaque hidden : Nat := 0\n',
        proof=proofs.replace('RuleAccepted ok 0', '@RuleAccepted (let _ := hidden; Nat) ok 0'))
# Beta-erasure leaves a closed True proof field and concrete Bool data.
fixture('beta-erased-proof-field-domain',
        decl='opaque hidden : Nat := 0\n'
             'structure Carrier : Type where\n  data : Bool\n'
             '  valid : (fun _ : Nat => True) hidden\n'
             '/-- The rule (Q9.1). -/\ndef ok (_p : Carrier) : Prop := True\n',
        proof='theorem accepts : RuleAccepted ok (⟨true, True.intro⟩ : Carrier) := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]')

# ModuleId keeps its nonempty-path proof while all identity data remains inspectable.
fixture('module-id-proof-field',
        decl='/-- The rule (Q9.1). -/\ndef ok (_m : ModuleId) : Prop := True\n',
        proof='theorem accepts : RuleAccepted ok (⟨⟨⟨"pkg"⟩, ⟨⟨"v1"⟩⟩⟩, [⟨"main"⟩], by decide⟩ : ModuleId) := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]')
# A dictionary parameter has no special exemption for its function fields.
fixture('function-dictionary-parameter',
        decl='structure Dictionary where\n  f : Nat → Nat\n'
             'structure Carrier (d : Dictionary) where\n  data : Bool\n'
             '/-- The rule (Q9.1). -/\ndef ok (_p : Carrier ⟨id⟩) : Prop := True\n',
        proof='theorem accepts : RuleAccepted ok ⟨true⟩ := True.intro\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
        reason='no concrete rule acceptance/refusal proposition')

fixture('computed-concrete-input', proof=proofs.replace('RuleAccepted ok 0', 'RuleAccepted ok (1 - 1)'))
# Constant identities carry closed path-validity proofs. Proof irrelevance must not
# exempt the adjacent runtime data from constructor/literal normalization.
proof_carrier = ('structure ProofCarrier where\n  data : Nat\n  valid : data = data\n'
                 '/-- The rule (Q9.1). -/\ndef ok (p : ProofCarrier) : Prop := p.data = 0\n')
fixture('closed-proof-field', decl=proof_carrier,
        proof='theorem accepts : RuleAccepted ok ⟨0, by decide⟩ := rfl\n'
              'theorem refuses : RuleRefused ok ⟨1, by decide⟩ := by intro h; cases h\n')
fixture('transparent-theorem-proof-field', decl=proof_carrier,
        proof='theorem zeroWitness : 0 = 0 := rfl\n'
              'theorem oneWitness : 1 = 1 := rfl\n'
              'theorem accepts : RuleAccepted ok ⟨0, zeroWitness⟩ := rfl\n'
              'theorem refuses : RuleRefused ok ⟨1, oneWitness⟩ := by intro h; cases h\n')
fixture('proof-field-with-opaque-data',
        decl=proof_carrier + 'opaque hidden : Nat := 0\n',
        proof='theorem accepts : RuleAccepted ok ⟨hidden - hidden, by rfl⟩ := Nat.sub_self hidden\n',
        evidence='[⟨`ConcreteSpec.Core.ok, `ConcreteSpec.Core.accepts⟩]',
        reason='no concrete rule acceptance/refusal proposition')
fixture('opaque-core-proof',
        decl='structure Carrier : Type where\n  data : Nat\n  valid : "a" ++ "b" = "ab"\n'
             '/-- The rule (Q9.1). -/\ndef ok (p : Carrier) : Prop := p.data = 0\n',
        proof='opaque witness : "a" ++ "b" = "ab" := rfl\n'
              'theorem accepts : RuleAccepted ok ⟨0, witness⟩ := rfl\n'
              'theorem refuses : RuleRefused ok ⟨1, witness⟩ := by intro h; cases h\n')
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
fixture('mention-only-row', decl=rule.replace('Q9.1', 'R-0001'), reason='roadmap-only')
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

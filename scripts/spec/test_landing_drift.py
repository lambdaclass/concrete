#!/usr/bin/env python3
"""Dependency drift must be complete, actionable, and validated by required CI."""
import copy
import json
from pathlib import Path
import yaml
import landing_manifest as inventory


def check_ci(workflow, required):
    # This check stays Lean-free so removal of the expensive gate cannot remove its guard.
    spec = workflow['jobs']['spec']
    if spec.get('continue-on-error', False):
        raise ValueError('inventory CI job may not allow failure')
    if spec['name'] not in required.splitlines():
        raise ValueError('inventory CI job is not required')
    steps = [s for s in spec['steps'] if s.get('run') ==
             'nix develop -c bash scripts/spec/landing_inventory.sh']
    if len(steps) != 1 or 'if' in steps[0] or steps[0].get('continue-on-error', False):
        raise ValueError('inventory gate must run unconditionally in Spec build')
    events = workflow.get('on', workflow.get(True))
    push = events.get('push', 'missing')
    main_covered = push is None
    if isinstance(push, dict):
        branches = push.get('branches')
        # GitHub tag-only filters do not trigger branch pushes:
        # https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax
        main_covered = (not any(k in push for k in ('tags', 'tags-ignore')) if branches is None
                        else any(b in branches for b in ('main', '*', '**')))
        # Positive additional branches and tag triggers do not narrow main coverage.
        # Ordered negative branch patterns require explicit coverage analysis.
        main_covered &= not any(b.startswith('!') for b in (branches or []))
        main_covered &= not any(k in push for k in ('paths', 'paths-ignore', 'branches-ignore'))
    if not main_covered or events.get('pull_request', 'missing') not in (None, {}) or 'workflow_dispatch' not in events:
        raise ValueError('inventory must validate every main, PR and train change')
    if spec['if'] != "${{ !cancelled() && needs.landing-record.outputs.recorded != 'true' }}":
        raise ValueError('inventory job may skip only a recorded exact-SHA landing')


def main():
    old = [inventory.row(record='command', name='a:1:2', kind='example', digest='same', dependencies='["old"]'),
           inventory.row(record='declaration', name='Generated.recOn', line=0, digest='same', dependencies='["rec"]'),
           inventory.row(record='declaration', name='Moved', line=2, digest='before', dependencies='["old"]')]
    new = [dict(r, dependencies=json.dumps([name])) for r, name in zip(old, ['instDecidable', 'casesOn', 'new'])]
    new[2]['digest'] = 'after'
    try:
        inventory.census_drift(old, new)
    except ValueError as exc:
        message = str(exc)
        for fragment in ['a:1:2', 'Generated.recOn', 'Moved', 'added=["instDecidable"]',
                         'removed=["old"]', 'added=["casesOn"]', 'removed=["rec"]',
                         'elaboration-or-extraction', 'generated-term-or-extraction', 'source-change']:
            if fragment not in message:
                raise ValueError('incomplete dependency drift diagnostic: '+fragment) from exc
    else:
        raise ValueError('changed dependencies accepted')
    inventory.census_drift(old, old)
    marker = [inventory.row(record='snapshot', name='toolchain', source='old-lean')]
    inventory.check_census_toolchain(marker, 'old-lean')
    for rows, actual in ((marker, 'new-lean'), ([], 'old-lean')):
        try: inventory.check_census_toolchain(rows, actual)
        except ValueError as exc:
            if 'census toolchain changed:' not in str(exc): raise
        else: raise ValueError('unclassified toolchain change accepted')
    root = Path(__file__).resolve().parents[2]
    workflow = yaml.safe_load((root/'.github/workflows/lean_action_ci.yml').read_text())
    required = (root/'required-checks.txt').read_text()
    check_ci(workflow, required)
    expanded = copy.deepcopy(workflow)
    expanded.get('on', expanded.get(True))['push']['branches'].append('ci/*')
    check_ci(expanded, required)
    for push in (None, {}, {'branches': ['**']}, {'branches': ['main'], 'tags': ['v*']}):
        expanded.get('on', expanded.get(True))['push'] = push
        check_ci(expanded, required)
    for mutation in ('remove', 'conditional', 'allowed-failure', 'not-required', 'path-filter', 'pr-path-filter', 'job-allowed-failure', 'main-excluded', 'tags-only'):
        bad = copy.deepcopy(workflow)
        step = next(s for s in bad['jobs']['spec']['steps'] if 'landing_inventory.sh' in s.get('run', ''))
        req = required
        if mutation == 'remove': bad['jobs']['spec']['steps'].remove(step)
        elif mutation == 'conditional': step['if'] = 'false'
        elif mutation == 'allowed-failure': step['continue-on-error'] = True
        elif mutation == 'not-required': req = req.replace('Spec build\n', '')
        elif mutation == 'path-filter': bad.get('on', bad.get(True))['push']['paths'] = ['ConcreteSpec/**']
        elif mutation == 'pr-path-filter': bad.get('on', bad.get(True))['pull_request'] = {'paths': ['ConcreteSpec/**']}
        elif mutation == 'job-allowed-failure': bad['jobs']['spec']['continue-on-error'] = True
        elif mutation == 'main-excluded': bad.get('on', bad.get(True))['push']['branches'] = ['main', '!main']
        else: bad.get('on', bad.get(True))['push'] = {'tags': ['v*']}
        try: check_ci(bad, req)
        except ValueError: pass
        else: raise ValueError('CI guard accepted '+mutation)
    print('control PASS: complete dependency deltas and required unfiltered inventory CI')


if __name__ == '__main__':
    main()

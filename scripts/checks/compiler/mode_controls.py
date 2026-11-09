"""Exercise the real shell entry point without starting a compiler build."""
import os
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[3]


def main(environment):
    env = {k: v for k, v in environment.items() if not k.startswith(('GITHUB_', 'SELECTED_', 'SCHEDULED_', 'RUNNER_'))}
    sha = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True, timeout=10).strip()
    cases = [
        ('pr', 'pull_request', 'refs/pull/123/merge', 'github-hosted', None),
        ('qualification', 'push', 'refs/heads/wip/candidate', 'self-hosted', None),
        ('qualification', 'workflow_dispatch', 'refs/heads/ci/train', 'self-hosted', None),
        ('daily', 'schedule', 'refs/heads/main', 'self-hosted', None),
        ('daily', 'workflow_dispatch', 'refs/heads/wip/candidate', 'self-hosted', None),
        ('pr', 'pull_request', 'refs/pull/123/merge', 'self-hosted', 'FAIL scaling: hosted PR requires github-hosted runner'),
        ('pr', 'push', 'refs/pull/123/merge', 'github-hosted', 'FAIL scaling: hosted PR requires pull_request event'),
        ('pr', 'pull_request', 'refs/heads/main', 'github-hosted', 'FAIL scaling: hosted PR requires refs/pull/N/merge ref'),
        ('qualification', 'pull_request', 'refs/pull/123/merge', 'self-hosted', 'FAIL scaling: unauthorized event pull_request for self-hosted qualification'),
        ('qualification', 'push', 'refs/pull/123/merge', 'self-hosted', 'FAIL scaling: unauthorized ref refs/pull/123/merge for self-hosted qualification'),
        ('daily', 'pull_request', 'refs/heads/main', 'self-hosted', 'FAIL scaling: unauthorized event pull_request for scheduled daily'),
        ('daily', 'schedule', 'refs/tags/v1', 'self-hosted', 'FAIL scaling: unauthorized ref refs/tags/v1 for scheduled daily'),
    ]
    for tier in ('qualification', 'daily'):
        cases.append((tier, 'workflow_dispatch', 'refs/heads/main', 'github-hosted', 'forged runner: expected RUNNER_ENVIRONMENT=self-hosted'))
    for number, (tier, event, ref, runner, diagnostic) in enumerate(cases):
        selected = dict(env, SELECTED_TIER=tier, SELECTED_SHA=sha, GITHUB_EVENT_NAME=event,
                        GITHUB_REF=ref, RUNNER_ENVIRONMENT=runner, CAMPAIGN_EXPECT_FAILURE='1')
        command = ['bash', 'scripts/checks/compiler/scaling.sh', '--check-execution-policy']
        result = subprocess.run(command, cwd=ROOT, env=selected, capture_output=True, text=True, timeout=15)
        want = (1, diagnostic) if diagnostic else (0, 'PASS scaling execution policy: '+tier)
        assert (result.returncode, (result.stdout+result.stderr).strip()) == want, (number, result.returncode, result.stdout, result.stderr)
        # The checkout binding is mandatory in every mode, including policy probes.
        selected['SELECTED_SHA'] = '0'*40
        result = subprocess.run(command, cwd=ROOT, env=selected, capture_output=True, text=True, timeout=15)
        assert (result.returncode, (result.stdout+result.stderr).strip()) == (1, 'FAIL scaling: checked-out HEAD does not match authorized expected SHA '+'0'*40)
        print(f'ok C entry policy {number}: {tier} {event} {ref} {runner}; exact SHA bound')
    return 0


if __name__ == '__main__':
    sys.exit(main(dict(os.environ)))

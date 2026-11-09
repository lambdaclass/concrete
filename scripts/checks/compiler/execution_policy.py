"""Scaling authorization: the registry-selected tier is the sole mode selector."""
import re
import sys


def authorize(tier, event, ref, runner):
    if tier == 'pr':
        if runner != 'github-hosted':
            raise ValueError('FAIL scaling: hosted PR requires github-hosted runner')
        if event != 'pull_request':
            raise ValueError('FAIL scaling: hosted PR requires pull_request event')
        if not re.fullmatch(r'refs/pull/[1-9][0-9]*/merge', ref):
            raise ValueError('FAIL scaling: hosted PR requires refs/pull/N/merge ref')
    elif tier in ('qualification', 'daily'):
        mode = 'self-hosted qualification' if tier == 'qualification' else 'scheduled daily'
        if runner != 'self-hosted':
            raise ValueError('forged runner: expected RUNNER_ENVIRONMENT=self-hosted')
        allowed = ('push', 'workflow_dispatch') if tier == 'qualification' else ('schedule', 'workflow_dispatch')
        if event not in allowed:
            raise ValueError(f'FAIL scaling: unauthorized event {event} for {mode}')
        if not re.fullmatch(r'refs/heads/(main|(?:ci|wip)/.+)', ref):
            raise ValueError(f'FAIL scaling: unauthorized ref {ref} for {mode}')
    else:
        raise ValueError('scaling: invalid SELECTED_TIER')


if __name__ == '__main__':
    try:
        authorize(*sys.argv[1:])
    except ValueError as error:
        sys.exit(str(error))

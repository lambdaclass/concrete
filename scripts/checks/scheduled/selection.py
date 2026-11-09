"""Trusted Actions selection adapter; event SHA and history tip remain distinct."""
import os
import sys
from history import authorize
from scheduler_policy import POLICY, TIERS, note_ref


def main(environment, args):
    if args == ['--note-refs']:
        print('\n'.join(note_ref(tier) for tier in TIERS))
        return
    event, cron, requested_sha, requested_tier, event_sha = args
    if event == 'schedule':
        tiers = [tier for tier, scheduled_cron in POLICY['cadence'].items() if scheduled_cron == cron]
        if len(tiers) != 1:
            raise ValueError('invalid schedule cadence')
        tier, = tiers
        sha = event_sha
    else:
        tier = requested_tier
        if tier not in TIERS:
            raise ValueError('invalid scheduled tier')
        sha = authorize(requested_sha, environment)
    print('SCHEDULE_SELECTION', sha, tier)
    with open(environment['GITHUB_OUTPUT'], 'a') as stream:
        stream.write(f'sha={sha}\ntier={tier}\n')


if __name__ == '__main__':
    try: main(dict(os.environ), sys.argv[1:])
    except ValueError as error: sys.exit(str(error))

"""Publisher-side observer census; never trusts observer-produced heartbeats.

Actions run creation is external evidence. Each due cron occurrence must have
its own successful scheduled run; manual runs cannot fill gaps. A full cadence
period is allowed for scheduler delay/completion. Live qualification is R-0690.
"""
import datetime as dt
from scheduler_policy import POLICY, MAIN_BRANCH

DIAGNOSTIC = 'FAIL scheduled observer: missed occurrence'


def verify_occurrences(runs, now):
    minute, hours, day, month, weekday = POLICY['observer_cron'].split()
    if (day, month, weekday) != ('*', '*', '*') or not hours.startswith('*/'):
        raise ValueError('INCOMPLETE scheduled observer: unsupported cadence')
    period = dt.timedelta(hours=int(hours[2:]))
    anchor = now.replace(hour=0, minute=int(minute), second=0, microsecond=0)
    due = anchor + ((now - period - anchor) // period) * period
    # Check all occurrences in the publisher's retained observation horizon.
    start = now - dt.timedelta(seconds=2 * POLICY['queue_age_seconds'])
    while due >= start:
        found = False
        for run in runs:
            created = dt.datetime.fromisoformat(run['created_at'].replace('Z', '+00:00'))
            if (run.get('event') == 'schedule' and run.get('status') == 'completed'
                    and run.get('conclusion') == 'success' and due <= created < due + period):
                found = True
                break
        if not found:
            raise ValueError(DIAGNOSTIC)
        due -= period


def check(repository, token, now, pages):
    cutoff = (now - dt.timedelta(seconds=2 * POLICY['queue_age_seconds'])).strftime('%Y-%m-%dT%H:%M:%SZ')
    runs = pages(f'/repos/{repository}/actions/workflows/gate_freshness.yml/runs?branch={MAIN_BRANCH}&created=>={cutoff}',
                 'workflow_runs', token=token)
    verify_occurrences(runs, now)

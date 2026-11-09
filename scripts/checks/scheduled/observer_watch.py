"""Publisher-side observer census; never trusts observer-produced heartbeats.

Actions run creation is external evidence. Each due cron occurrence must have
its own completed observation step; manual runs cannot fill gaps. A full cadence
period is allowed for scheduler delay/completion. Live qualification is R-0690.
"""
import datetime as dt
import urllib.parse
from scheduler_policy import POLICY, MAIN_BRANCH

COMPLETION_STEP = 'Observer execution completed'


def query(path, **parameters):
    parts = urllib.parse.urlsplit(path)
    values = dict(urllib.parse.parse_qsl(parts.query))
    values.update(parameters)
    return urllib.parse.urlunsplit(parts._replace(query=urllib.parse.urlencode(values)))


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
                    and run.get('observation_complete') is True and due <= created < due + period):
                found = True
                break
        if not found:
            raise ValueError(DIAGNOSTIC)
        due -= period


def check(repository, token, now, pages):
    cutoff = (now - dt.timedelta(seconds=2 * POLICY['queue_age_seconds'])).strftime('%Y-%m-%dT%H:%M:%SZ')
    runs = pages(query(f'/repos/{repository}/actions/workflows/gate_freshness.yml/runs',
                       branch=MAIN_BRANCH, created='>=' + cutoff), 'workflow_runs', token=token)
    observed = []
    for run in runs:
        jobs = pages(f'/repos/{repository}/actions/runs/{run["id"]}/attempts/{run.get("run_attempt", 1)}/jobs',
                     'jobs', token=token)
        complete = any(step.get('name') == COMPLETION_STEP and step.get('status') == 'completed'
                       and step.get('conclusion') == 'success'
                       for job in jobs for step in job.get('steps', []))
        observed.append(dict(run, observation_complete=complete))
    verify_occurrences(observed, now)

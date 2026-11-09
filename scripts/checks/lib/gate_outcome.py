#!/usr/bin/env python3
"""Structured nested-gate outcomes. Never infer watchdog state from log text or rc 124."""
import json
import os
from pathlib import Path
import sys

STATUSES = {'PASS', 'FAIL', 'INCOMPLETE'}

def emit(status, diagnostic, path, sha):
    if status not in STATUSES or not diagnostic:
        raise ValueError('FAIL scheduled outcome: invalid event')
    if path:
        event = dict(status=status, diagnostic=diagnostic, sha=sha)
        # One append write per event: parallel nested watchdogs cannot interleave JSON lines.
        fd = os.open(path, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o600)
        try:
            os.write(fd, (json.dumps(event, sort_keys=True) + '\n').encode())
        finally:
            os.close(fd)

def result(path, rc, sha):
    try:
        events = [json.loads(line) for line in Path(path).read_text().splitlines()]
        for event in events:
            if not isinstance(event,dict) or set(event) != {'status','diagnostic','sha'} or not isinstance(event['status'],str) or event['status'] not in STATUSES or not isinstance(event['diagnostic'],str):
                raise ValueError('invalid event')
            if event['sha'] != sha:
                print('FAIL scheduled outcome: selected SHA mismatch', file=sys.stderr)
                return 1
    except (OSError, ValueError, TypeError):
        print('FAIL scheduled outcome: invalid event stream', file=sys.stderr)
        return 1
    if any(e['status']=='FAIL' for e in events):
        print('FAIL scheduled gate: explicit failure outcome', file=sys.stderr)
        return 1
    if any(e['status']=='INCOMPLETE' for e in events):
        print('INCOMPLETE scheduled gate: nested watchdog or gate incomplete', file=sys.stderr)
        return 125
    return 1 if rc == 125 else rc

if __name__ == '__main__':
    environment = dict(os.environ)
    if len(sys.argv)==5 and sys.argv[1]=='result':
        sys.exit(result(sys.argv[2], int(sys.argv[3]), sys.argv[4] or None))
    if len(sys.argv)==3:
        emit(sys.argv[1],sys.argv[2],environment.get('SCHEDULED_OUTCOME_FILE'),environment.get('SELECTED_SHA'))
    else:
        sys.exit('usage: gate_outcome.py STATUS DIAGNOSTIC | result FILE RC SHA')

"""Materialized current-controller policy; selected evidence is verified separately.

Gate inventory, tiers and windows remain owned by gates.json. The registry
reader supplies the bootstrap document; all evidence still uses its validator.
"""
from dataclasses import dataclass
import json
import math
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
POLICY = json.loads((ROOT / 'scripts/checks/scheduled/policy.json').read_text())
from gate_manifest import read_document
REGISTRY = read_document()
WINDOWS = REGISTRY['windows']
TIERS = tuple(POLICY['cadence'])
ALL_TIERS = frozenset(TIERS) | frozenset(POLICY['candidate_tiers'])
MAIN_BRANCH = POLICY['main_branch']
REMOTE = POLICY['remote']
MAIN_REF = 'refs/heads/' + MAIN_BRANCH
MAIN_HISTORY = 'refs/remotes/' + REMOTE + '/' + MAIN_BRANCH
BUDGET = POLICY['budgets']
NOTE_NAMESPACE = POLICY['note_prefix'].rsplit('/', 1)[0] + '/'


def note_ref(tier):
    if tier not in TIERS:
        raise ValueError('invalid scheduled tier')
    return POLICY['note_prefix'] + tier


def occurrence(event, run_id):
    """Schedule occurrence is independent of retry; attempt identifies execution."""
    return str(event) + '/' + str(run_id)


def attempt(run_id, run_attempt):
    return str(run_id) + '/' + str(run_attempt)


@dataclass(frozen=True)
class Identity:
    repository: str
    sha: str
    tier: str
    attempt: str
    occurrence: str

    @classmethod
    def actions(cls, repository, sha, tier, run_id, run_attempt, event):
        if tier not in TIERS:
            raise ValueError('invalid scheduled tier')
        return cls(repository, sha, tier, attempt(run_id, run_attempt), occurrence(event, run_id))

    def fields(self):
        return dict(repository=self.repository, sha=self.sha, tier=self.tier,
                    attempt=self.attempt, occurrence=self.occurrence)


def gate_seconds(seconds, *, scheduled=True):
    return seconds * BUDGET['attempt_factor'] + BUDGET['gate_reserve' if scheduled else 'ci_reserve']


def job_minutes(seconds):
    return math.ceil(seconds * BUDGET['attempt_factor'] / 60) + BUDGET['job_reserve_minutes']


def allocate_deadline(end, count=1, *, monotonic, reserve=None):
    """One clock sample composes a child deadline within its parent's remainder."""
    if count < 1:
        raise ValueError('FAIL scheduled suite: invalid allocation')
    now = monotonic()
    if end <= now:
        raise ValueError('INCOMPLETE scheduled suite: deadline exhausted')
    return now + (end - now) * (1 - (BUDGET['suite_reserve'] if reserve is None else reserve)) / count


# Bootstrap resolves names only. Validate the complete current registry before
# exposing any policy value to an adapter; selected revisions are validated anew.
from gate_manifest import validate_manifest
validate_manifest(REGISTRY)

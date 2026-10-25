#!/usr/bin/env python3
"""Bug 263: completed work, rather than machine speed, defines campaign coverage."""
import contextlib
import importlib.util
import io
from pathlib import Path
from types import SimpleNamespace
import tempfile
import unittest
from unittest.mock import Mock, patch

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("farm", ROOT / "scripts/fuzz/farm/farm.py")
farm = importlib.util.module_from_spec(spec)
spec.loader.exec_module(farm)


class Batches(unittest.TestCase):
    def run_campaign(self, *, batches=3, cost=1, deadline=100, pause_midway=False):
        clock = [0.0]
        generated = []

        def make(gen, seed, depth):
            generated.append((gen, seed, depth))
            return [{"oracle": gen, "src": "test", "params": {}}]

        def classify(*_):
            clock[0] += cost
            return SimpleNamespace(cls=None, outcome="agree")

        def paused(_):
            if pause_midway and len(generated) == 1:
                clock[0] = deadline
                return True
            return False

        args = SimpleNamespace(seed=7, worker_id=0, sha="test", batch=2, batches=batches,
                               compiler="unused")
        stats = Mock(data={"new": {}, "seconds": 0, "drift_batches": 0})
        stderr = io.StringIO()
        with tempfile.TemporaryDirectory() as tmp, \
                patch.object(farm.time, "time", side_effect=lambda: clock[0]), \
                patch.object(farm, "paused", side_effect=paused), \
                patch.object(farm, "classify", side_effect=classify), \
                patch.object(farm, "drift_clear"), contextlib.redirect_stderr(stderr):
            rc = farm._campaign_loop(args, {"state": tmp}, SimpleNamespace(make=make),
                                     ["diff", "lin", "mut"], {}, tmp, stats, deadline)
        return rc, generated, stderr.getvalue()

    def test_same_work_at_different_speeds(self):
        fast = self.run_campaign(cost=0.001)
        slow = self.run_campaign(cost=10)
        self.assertEqual(fast, slow)
        self.assertEqual(fast[0], 0)
        self.assertEqual([g for g, _, _ in fast[1]], ["diff", "diff", "lin", "lin", "mut", "mut"])

    def test_deadline_does_not_count_a_partial_batch(self):
        rc, cases, error = self.run_campaign(cost=10, deadline=25)
        self.assertEqual(rc, 2)
        self.assertEqual(len(cases), 3)
        self.assertIn("completed 1/3 batches", error)

    def test_pause_does_not_count_a_partial_batch(self):
        rc, cases, error = self.run_campaign(pause_midway=True)
        self.assertEqual(rc, 2)
        self.assertEqual(len(cases), 1)
        self.assertIn("completed 0/3 batches", error)

    def test_zero_time_is_incomplete(self):
        rc, cases, error = self.run_campaign(deadline=0)
        self.assertEqual(rc, 2)
        self.assertEqual(cases, [])
        self.assertIn("incomplete", error)

    def test_default_timed_campaign_keeps_its_behavior(self):
        rc, cases, error = self.run_campaign(batches=None, deadline=5)
        self.assertEqual(rc, 0)
        self.assertEqual(len(cases), 5)
        self.assertEqual(error, "")


if __name__ == "__main__":
    unittest.main()

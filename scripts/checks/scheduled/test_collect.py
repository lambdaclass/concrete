"""Trusted collection controls, including empty real-world MISSING payloads."""
import copy
import unittest
from unittest.mock import patch
import collect
from protocol import EvidenceError
from test_protocol import SHA, RUNNERS, record

class Collector(unittest.TestCase):
    def setUp(self):
        self.payload = record()
        self.run = dict(id=1, run_attempt=1, event="schedule", head_sha=SHA,
                        created_at=self.payload["started"], updated_at=self.payload["completed"])
        self.job = dict(name="campaign", status="completed", conclusion="success", runner_name="ci2-heavy",
                        started_at=self.payload["started"], completed_at=self.payload["completed"])

    def test_api_identity_overrides_forged_worker(self):
        self.payload["runner_name"] = "forged"
        r = collect.record_for(self.run, self.job, (SHA, "daily"), self.payload, "artifact", "example/repo", RUNNERS)
        self.assertEqual(r["runner_name"], "ci2-heavy")
        self.job["runner_name"] = "other-host"
        with self.assertRaisesRegex(EvidenceError, "^forged runner: not inventoried concrete-ci-2$"):
            collect.record_for(self.run, self.job, (SHA, "daily"), self.payload, "artifact", "example/repo", RUNNERS)

    def test_repository_not_self_asserted(self):
        with self.assertRaisesRegex(EvidenceError, "^wrong repository$"):
            collect.record_for(self.run, self.job, (SHA, "daily"), self.payload, "artifact", "other/repo", RUNNERS)

    def test_all_missing_shapes(self):
        for conclusion in ("cancelled", "skipped", "queued-too-long", "absent"):
            job = copy.deepcopy(self.job)
            job.update(conclusion=conclusion, runner_name=None)
            if conclusion == "queued-too-long":
                job.update(status="queued", completed_at=None)
            if conclusion == "absent":
                job = None
            r = collect.record_for(self.run, job, (SHA, "daily"), None, None, "example/repo", RUNNERS)
            self.assertEqual((r["status"], r["conclusion"], r["gates"], r["artifact"]), ("MISSING", conclusion, [], None))

    def test_job_failure_cannot_be_worker_pass(self):
        self.job["conclusion"] = "failure"
        r = collect.record_for(self.run, self.job, (SHA, "daily"), self.payload, "artifact", "example/repo", RUNNERS)
        self.assertEqual((r["status"], r["conclusion"]), ("INCOMPLETE", "incomplete"))
        self.assertEqual(r["gates"][-1]["id"], "actions-job")

    def test_authoritative_selection(self):
        jobs = [dict(name="select", conclusion="success", id=42)]
        with patch.object(collect, "request", return_value=("2026-11-08T00:00:00Z SCHEDULE_SELECTION " + SHA + " weekly\n").encode()):
            self.assertEqual(collect.selected("/base", jobs, self.run), [(SHA, "weekly")])
        with patch.object(collect, "request", return_value=b"some other failure"):
            with self.assertRaisesRegex(EvidenceError, "^INCOMPLETE: missing trusted selection diagnostic$"):
                collect.selected("/base", jobs, self.run)

if __name__ == "__main__":
    unittest.main()

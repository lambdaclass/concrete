from test_protocol import RegistryFixture
"""Trusted collection controls, including empty real-world MISSING payloads."""
import copy
import datetime as dt
from process_adapter import operation_clock
import unittest
from unittest.mock import patch
import collect
from protocol import EvidenceError
from test_protocol import SHA, RUNNERS, record

class Collector(RegistryFixture, unittest.TestCase):
    def setUp(self):
        super().setUp()
        self.payload = record()
        clock = patch.object(collect, "operation_clock", return_value=operation_clock(observation=dt.datetime.fromisoformat(self.payload["completed"])))
        clock.start(); self.addCleanup(clock.stop)
        self.run = dict(id=1, run_attempt=1, event="schedule", head_sha=SHA,
                        created_at=self.payload["started"], updated_at=self.payload["completed"], head_branch="main", display_title="scheduled-daily")
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
        self.assertEqual(r["execution_error"], "Actions job metadata")

    def test_unstarted_workflow_publishes_real_missing_notes(self):
        with patch.object(collect, "pages", return_value=[]), patch.object(collect, "publish") as publish:
            collect.collect(self.run, "example/repo", RUNNERS, {"GH_TOKEN": "test-token"})
            self.assertEqual(publish.call_count, 1)
            for call in publish.call_args_list:
                r = call.args[0]
                self.assertEqual((r["sha"], r["status"], r["conclusion"], r["gates"], r["artifact"], r["runner_name"]),
                                 (SHA, "MISSING", "absent", [], None, None))

    def test_queued_job_collection_needs_no_worker_artifact(self):
        self.job.update(status="queued", conclusion=None, runner_name=None, completed_at=None, created_at="2027-01-01T00:00:00+00:00")
        jobs = [dict(name="select", conclusion="success", id=42), self.job]
        with patch.object(collect, "pages", return_value=jobs) as pages, patch.object(collect, "request", return_value=("SCHEDULE_SELECTION " + SHA + " daily\n").encode()), patch.object(collect, "publish") as publish:
            collect.collect(self.run, "example/repo", RUNNERS, {"GH_TOKEN": "test-token"})
            self.assertEqual(pages.call_count, 1)
            r = publish.call_args.args[0]
            self.assertEqual((r["status"], r["conclusion"], r["gates"], r["artifact"]),
                             ("MISSING", "queued-too-long", [], None))

    def test_rejected_dispatch_does_not_poison_main(self):
        self.run["event"] = "workflow_dispatch"
        self.assertEqual(collect.selected("/base", [], self.run, token="test-token"), [])

    def test_active_execution_is_not_overdue_queue(self):
        self.job["status"] = "in_progress"
        with patch.object(collect, "pages", return_value=[self.job]), patch.object(collect, "publish") as publish:
            collect.collect(self.run, "example/repo", RUNNERS, {"GH_TOKEN": "test-token"})
            publish.assert_not_called()

    def test_overdue_observation_time(self):
        self.job.update(status="queued", completed_at=None)
        with patch.object(collect, "operation_clock", return_value=operation_clock(observation=dt.datetime(2027, 1, 16, tzinfo=dt.timezone.utc))):
            r = collect.record_for(self.run, self.job, (SHA, "daily"), None, None, "example/repo", {})
        self.assertEqual(r["completed"], "2027-01-16T00:00:00+00:00")
        from protocol import observe, timestamp
        with self.assertRaisesRegex(EvidenceError, f"^MISSING daily {SHA}: recorded attempt$"):
            observe([r], SHA, "daily", timestamp(r["completed"]), RUNNERS, "example/repo")

    def test_crashed_execution_without_index(self):
        self.job["conclusion"] = "failure"
        r = collect.record_for(self.run, self.job, (SHA, "daily"), None, None, "example/repo", {})
        self.assertEqual((r["status"], r["conclusion"]), ("INCOMPLETE", "incomplete"))
        self.assertEqual(r["execution_error"], "Actions execution completed without index.json")
        from protocol import observe
        from test_protocol import NOW
        with self.assertRaisesRegex(EvidenceError, f"^INCOMPLETE daily {SHA}: INCOMPLETE registry: missing gate sample$"):
            observe([r], SHA, "daily", NOW, RUNNERS, "example/repo")

    def test_decompressed_index_bound(self):
        import io
        from unittest.mock import Mock
        archive = Mock()
        archive.open.return_value = io.BytesIO(b"x" * (1024 * 1024 + 1))
        with self.assertRaisesRegex(EvidenceError, "^index too large$"):
            collect.read_index(archive)
        for data in (b"null", b"[]", b'{"gates":[null]}'):
            archive.open.return_value = io.BytesIO(data)
            with self.assertRaisesRegex(EvidenceError, "^invalid worker index shape$"):
                collect.read_index(archive)

    def test_absent_inventory_still_collects(self):
        import tempfile
        import json
        self.run["status"] = "completed"
        with tempfile.NamedTemporaryFile(mode="w") as event:
            json.dump({"workflow_run": self.run}, event); event.flush()
            with patch.object(collect, "inventory", side_effect=EvidenceError("planted absent inventory")), patch.object(collect, "pages", return_value=[]), patch.object(collect, "publish") as publish:
                self.assertEqual(collect.collect_event({"GH_TOKEN": "test-token", "GITHUB_REPOSITORY": "example/repo", "GITHUB_EVENT_PATH": event.name}), 0)
                self.assertEqual(publish.call_args.args[0]["status"], "MISSING")

    def test_sweep_injects_one_observation_into_every_record(self):
        import json
        import tempfile
        from unittest.mock import Mock
        observation = dt.datetime.fromisoformat(self.payload['completed'])
        wall = Mock(return_value=observation)
        clock = operation_clock(wall=wall, monotonic=lambda: 100)
        runs = [dict(self.run, id=n, status='queued', created_at='2027-01-01T00:00:00+00:00')
                for n in (1, 2)]
        job = dict(self.job, status='queued', completed_at=None, created_at=runs[0]['created_at'])
        def pages(path, key, **kwargs):
            if key == 'jobs':
                return [job]
            return runs if '&created=' in path else []
        with tempfile.NamedTemporaryFile(mode='w') as event:
            json.dump({}, event); event.flush()
            environment = dict(GH_TOKEN='test-token', GITHUB_REPOSITORY='example/repo', GITHUB_EVENT_PATH=event.name)
            with patch.object(collect, 'pages', side_effect=pages), \
                 patch.object(collect, 'inventory', return_value=RUNNERS), \
                 patch.object(collect, 'publish') as publish, \
                 patch.object(collect, 'operation_clock', side_effect=AssertionError('unexpected clock capture')):
                self.assertEqual(collect.collect_event(environment, clock=clock), 0)
            self.assertEqual([call.args[0]['completed'] for call in publish.call_args_list],
                             [observation.isoformat(), observation.isoformat()])
        wall.assert_called_once_with()

    def test_authoritative_selection(self):
        jobs = [dict(name="select", conclusion="success", id=42)]
        with patch.object(collect, "request", return_value=("2026-11-08T00:00:00Z SCHEDULE_SELECTION " + SHA + " weekly\n").encode()):
            self.assertEqual(collect.selected("/base", jobs, self.run, token="test-token"), [(SHA, "weekly")])
        self.run["display_title"] = "unknown"
        with patch.object(collect, "request", return_value=b"some other failure"):
            with self.assertRaisesRegex(EvidenceError, "^INCOMPLETE: missing trusted schedule tier$"):
                collect.selected("/base", jobs, self.run, token="test-token")



if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()

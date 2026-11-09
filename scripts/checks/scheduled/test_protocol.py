import copy
import unittest
from protocol import append, observe, EvidenceError

SHA = "a" * 40
RUNNERS = {"example/repo": {"ci2-heavy"}}
NOW = 1800000000

def fixture_registry():
    return dict(schema=1, windows=dict(daily=129600, weekly=691200), gates=[dict(
        id="sample", tiers=["daily", "weekly"], command=["true"], budget_seconds=1,
        runner_class="concrete-ci-2", tools={}, seed=1, workflow=None,
        controls=[dict(id="refusal", command=["false"], exit_code=1, diagnostic="planted refusal")],
        artifacts=[])])

class RegistryFixture:
    def setUp(self):
        from unittest.mock import patch
        registry = patch("protocol.registry_at", side_effect=lambda sha: fixture_registry())
        registry.start()
        self.addCleanup(registry.stop)
        super().setUp()

def record():
    import datetime as dt
    stamp = dt.datetime.fromtimestamp(NOW, dt.timezone.utc).isoformat()
    return dict(sha=SHA, tier="daily", runner_environment="self-hosted",
                runner_name="ci2-heavy", repository="example/repo", attempt="1/1",
                status="PASS", conclusion="success", started=stamp, completed=stamp,
                occurrence="daily/2027-01-15", artifact="https://example/artifact/1",
                gate_revision=SHA, gates=[dict(id="sample", status="PASS", command=["true"],
                tools={}, seed=1, resources={}, controls=[dict(id="refusal", status="PASS", diagnostic="planted refusal")], log="sample.log")])

class Controls(RegistryFixture, unittest.TestCase):
    def rejected(self, diagnostic, fn):
        with self.assertRaises(EvidenceError) as caught:
            fn()
        self.assertEqual(str(caught.exception), diagnostic)

    def test_operational_recovery(self):
        for status, conclusion in (("MISSING", "cancelled"), ("INCOMPLETE", "incomplete")):
            old = record(); old.update(status=status, conclusion=conclusion)
            if status == "MISSING": old.update(gates=[], runner_name=None)
            else: old["gates"][0]["status"] = status
            self.rejected(f"{status} daily {SHA}: recorded attempt", lambda: observe([old], SHA, "daily", NOW, RUNNERS, "example/repo"))
            new = record(); new["attempt"] = "1/2"
            self.assertEqual(observe([old, new], SHA, "daily", NOW, RUNNERS, "example/repo"), f"PASS daily {SHA}")
            new["attempt"] = "1/0"
            self.rejected(f"{status} daily {SHA}: recorded attempt", lambda: observe([old, new], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_malformed_shapes(self):
        for value in (None, []):
            self.rejected("invalid record shape", lambda: observe([value], SHA, "daily", NOW, RUNNERS, "example/repo"))
        for gates, diagnostic in (([None], "invalid gate shape"), ([dict(record()["gates"][0], id=[])], "invalid gate id"), ([dict(record()["gates"][0], controls=[None])], "invalid controls shape")):
            r = record(); r["gates"] = gates
            self.rejected(diagnostic, lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_missing_contradictions(self):
        r = record(); r.update(status="MISSING", conclusion="cancelled")
        self.rejected("MISSING must have empty gates", lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))
        r.update(gates=[], runner_name="forged")
        self.rejected("MISSING has uninventoried runner", lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_repository_inventory_required(self):
        r = record(); r["repository"] = "invented/repo"
        self.rejected("wrong repository", lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_pass(self):
        self.assertEqual(observe([record()], SHA, "daily", NOW, RUNNERS, "example/repo"), "PASS daily " + SHA)

    def test_stale(self):
        self.rejected("STALE daily " + SHA + ": no fresh PASS",
                      lambda: observe([record()], SHA, "daily", NOW + 36*3600 + 1, RUNNERS, "example/repo"))

    def test_missing(self):
        self.rejected("MISSING daily " + SHA + ": absent run",
                      lambda: observe([], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_wrong_sha(self):
        r = record(); r["sha"] = "b" * 40
        self.rejected("wrong SHA", lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_cancelled(self):
        r = record(); r.update(status="MISSING", conclusion="cancelled", gates=[], artifact=None, runner_name=None)
        self.rejected("MISSING daily " + SHA + ": recorded attempt",
                      lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))
        r = record(); r["conclusion"] = "cancelled"
        self.rejected("cancelled/skipped/overdue/absent must be MISSING",
                      lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_replay(self):
        self.rejected("replay would overwrite attempt",
                      lambda: append([record()], record(), SHA, "daily", RUNNERS, "example/repo"))
        r = record(); r["attempt"] = "1/2"
        self.assertEqual(len(append([record()], r, SHA, "daily", RUNNERS, "example/repo")), 2)

    def test_forged(self):
        r = record(); r["runner_name"] = "other-host"
        self.rejected("forged runner: not inventoried concrete-ci-2",
                      lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_control_and_artifact(self):
        r = record(); r["gates"][0]["controls"] = []
        self.rejected("INCOMPLETE registry: missing control sample/refusal", lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))
        r = record(); r["artifact"] = ""
        self.rejected("missing result-index artifact", lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_failed_and_incomplete(self):
        for status, conclusion in (("FAIL", "failure"), ("INCOMPLETE", "incomplete")):
            r = record(); r.update(status=status, conclusion=conclusion); r["gates"][0]["status"] = status
            self.rejected(status + " daily " + SHA + ": recorded attempt",
                          lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_missing_conclusions(self):
        for conclusion in ("skipped", "queued-too-long", "absent"):
            r = record(); r.update(status="MISSING", conclusion=conclusion, gates=[], artifact=None, runner_name=None)
            self.rejected("MISSING daily " + SHA + ": recorded attempt",
                          lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_disagreement(self):
        r = record(); r.update(attempt="1/2", status="FAIL", conclusion="failure"); r["gates"][0]["status"] = "FAIL"
        history = append([r], record(), SHA, "daily", RUNNERS, "example/repo")
        self.rejected("FAIL daily " + SHA + ": recorded attempt", lambda: observe(history, SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_contradictory_statuses(self):
        for status, conclusion in (("FAIL", "failure"), ("INCOMPLETE", "incomplete")):
            r = record(); r.update(status=status, conclusion=conclusion)
            self.rejected("status/conclusion contradicts evidence",
                          lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))
        for status in ("FAIL", "INCOMPLETE"):
            r = record(); r["gates"][0]["status"] = status
            self.rejected("status/conclusion contradicts evidence",
                          lambda: observe([r], SHA, "daily", NOW, RUNNERS, "example/repo"))

    def test_old_failure_cannot_age_out(self):
        r = record(); r.update(status="FAIL", conclusion="failure")
        r["gates"][0]["status"] = "FAIL"
        fresh = record(); fresh["attempt"] = "2/1"
        import datetime as dt
        later = NOW + 36*3600 + 1
        fresh["completed"] = dt.datetime.fromtimestamp(later, dt.timezone.utc).isoformat()
        self.rejected("FAIL daily " + SHA + ": recorded attempt",
                      lambda: observe([r, fresh], SHA, "daily", later, RUNNERS, "example/repo"))

    def test_inventory_never_falls_back_to_a_copied_file(self):
        import tempfile
        from pathlib import Path
        from protocol import _binding
        with tempfile.TemporaryDirectory() as work:
            (Path(work)/'ci2_runners.txt').write_text('example/repo copied-runner\n')
            with self.assertRaises(_binding.BindingError) as caught:
                _binding.read_inventory(work)
            self.assertEqual(str(caught.exception),'INCOMPLETE dependency: ISS-0029 runner binding unavailable')

if __name__ == "__main__":
    unittest.main()

class ExplicitIdentity(RegistryFixture, unittest.TestCase):
    def test_repository_is_target_not_inventory_membership(self):
        from protocol import validate
        runners = dict(RUNNERS, **{"foreign/repo": {"ci2-heavy"}})
        r = record(); r["repository"] = "foreign/repo"
        with self.assertRaisesRegex(EvidenceError, "^wrong repository$"):
            validate(r, SHA, "daily", runners, "example/repo", "1/1")

    def test_expected_attempt(self):
        from protocol import validate
        with self.assertRaisesRegex(EvidenceError, "^wrong attempt$"):
            validate(record(), SHA, "daily", RUNNERS, "example/repo", "999/1")

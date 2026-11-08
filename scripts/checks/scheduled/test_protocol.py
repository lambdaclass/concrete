import copy
import unittest
from protocol import append, observe, EvidenceError

SHA = "a" * 40
RUNNERS = {"example/repo": {"ci2-heavy"}}
NOW = 1800000000

def record():
    import datetime as dt
    stamp = dt.datetime.fromtimestamp(NOW, dt.timezone.utc).isoformat()
    return dict(sha=SHA, tier="daily", runner_environment="self-hosted",
                runner_name="ci2-heavy", repository="example/repo", attempt="1/1",
                status="PASS", conclusion="success", started=stamp, completed=stamp,
                occurrence="daily/2027-01-15", artifact="https://example/artifact/1",
                gate_revision=SHA, gates=[dict(id="sample", status="PASS", command=["true"],
                tools={}, seed=1, resources={}, controls=[dict(status="PASS")], log="sample.log")])

class Controls(unittest.TestCase):
    def rejected(self, diagnostic, fn):
        with self.assertRaises(EvidenceError) as caught:
            fn()
        self.assertEqual(str(caught.exception), diagnostic)

    def test_pass(self):
        self.assertEqual(observe([record()], SHA, "daily", NOW, RUNNERS), "PASS daily " + SHA)

    def test_stale(self):
        self.rejected("STALE daily " + SHA + ": no fresh PASS",
                      lambda: observe([record()], SHA, "daily", NOW + 36*3600 + 1, RUNNERS))

    def test_missing(self):
        self.rejected("MISSING daily " + SHA + ": absent run",
                      lambda: observe([], SHA, "daily", NOW, RUNNERS))

    def test_wrong_sha(self):
        r = record(); r["sha"] = "b" * 40
        self.rejected("wrong SHA", lambda: observe([r], SHA, "daily", NOW, RUNNERS))

    def test_cancelled(self):
        r = record(); r.update(status="MISSING", conclusion="cancelled")
        self.rejected("MISSING daily " + SHA + ": recorded attempt",
                      lambda: observe([r], SHA, "daily", NOW, RUNNERS))
        r["status"] = "PASS"
        self.rejected("cancelled/skipped/overdue/absent must be MISSING",
                      lambda: observe([r], SHA, "daily", NOW, RUNNERS))

    def test_replay(self):
        self.rejected("replay would overwrite attempt",
                      lambda: append([record()], record(), SHA, "daily", RUNNERS))
        r = record(); r["attempt"] = "1/2"
        self.assertEqual(len(append([record()], r, SHA, "daily", RUNNERS)), 2)

    def test_forged(self):
        r = record(); r["runner_name"] = "other-host"
        self.rejected("forged runner: not inventoried concrete-ci-2",
                      lambda: observe([r], SHA, "daily", NOW, RUNNERS))

    def test_control_and_artifact(self):
        r = record(); r["gates"][0]["controls"] = []
        self.rejected("missing/pending control evidence", lambda: observe([r], SHA, "daily", NOW, RUNNERS))
        r = record(); r["artifact"] = ""
        self.rejected("missing result-index artifact", lambda: observe([r], SHA, "daily", NOW, RUNNERS))

    def test_disagreement(self):
        r = record(); r.update(attempt="1/2", status="FAIL", conclusion="failure")
        history = append([r], record(), SHA, "daily", RUNNERS)
        self.rejected("FAIL daily " + SHA + ": recorded attempt", lambda: observe(history, SHA, "daily", NOW, RUNNERS))

if __name__ == "__main__":
    unittest.main()

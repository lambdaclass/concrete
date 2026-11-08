"""History controls: main advancing cannot hide defects or manufacture absence."""
import unittest
from unittest.mock import patch
import subprocess
import observe
from test_protocol import SHA, NOW, RUNNERS, record

TIP = "b" * 40

class History(unittest.TestCase):
    def test_tip_exact_pass(self):
        r = record(); r.update(sha=TIP, gate_revision=TIP)
        self.assertEqual(observe.observe_main([TIP, SHA], {TIP: [r]}, "daily", NOW, RUNNERS), [f"PASS daily {TIP}"])

    def test_new_tip_recovers_operational_ancestor(self):
        for status, conclusion in (("MISSING", "cancelled"), ("INCOMPLETE", "incomplete")):
            old = record(); old.update(status=status, conclusion=conclusion)
            if status == "MISSING": old.update(gates=[], runner_name=None)
            else: old["gates"][0]["status"] = status
            tip = record(); tip.update(sha=TIP, gate_revision=TIP)
            self.assertEqual(observe.observe_main([TIP, SHA], {TIP: [tip], SHA: [old]}, "daily", NOW, RUNNERS), [f"PASS daily {TIP}"])

    def test_ancestor_coverage(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [record()]}, "daily", NOW, RUNNERS),
                         [f"PASS daily {SHA}: ancestor coverage; tip {TIP} requires its own evidence"])

    def test_ancestor_fail_never_ages_out(self):
        r = record(); r.update(status="FAIL", conclusion="failure"); r["gates"][0]["status"] = "FAIL"
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [r]}, "daily", NOW + 999999, RUNNERS),
                         [f"FAIL daily {SHA}: recorded attempt"])

    def test_stale_ancestor(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {SHA: [record()]}, "daily", NOW + 999999, RUNNERS),
                         [f"STALE daily {SHA}: no fresh PASS"])

    def test_missing_history(self):
        self.assertEqual(observe.observe_main([TIP, SHA], {}, "daily", NOW, {}),
                         [f"MISSING daily {TIP}: absent run"])

    def test_corrupt_ancestor_does_not_hide_failure(self):
        r = record(); r.update(status="FAIL", conclusion="failure"); r["gates"][0]["status"] = "FAIL"
        corrupt = record(); corrupt.pop("completed"); corrupt["sha"] = TIP
        messages = observe.observe_main([TIP, SHA], {SHA: [r], TIP: [corrupt]}, "daily", NOW, RUNNERS)
        self.assertEqual(messages, [f"FAIL daily {SHA}: recorded attempt", f"INCOMPLETE daily {TIP}: 'completed'"])

    def test_git_timeout_explicit(self):
        with patch.object(observe, "git", side_effect=subprocess.TimeoutExpired("git", 60)), patch("builtins.print") as output:
            self.assertEqual(observe.main(), 1)
            output.assert_called_once_with("INCOMPLETE observer: Command 'git' timed out after 60 seconds")

if __name__ == "__main__":
    unittest.main()

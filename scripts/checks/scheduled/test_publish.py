import runpy
from pathlib import Path
runpy.run_path(str(Path(__file__).with_name('suite_bootstrap.py')))

from test_protocol import RegistryFixture
"""Publication controls do not contact remotes."""
import subprocess
import unittest
from unittest.mock import patch
import publish
from protocol import EvidenceError
from test_protocol import SHA, RUNNERS, record

class Publication(RegistryFixture, unittest.TestCase):
    def test_idempotent_attempt(self):
        r = record()
        with patch.object(publish, "git", side_effect=["remote ref", "", "remote"]), patch.object(publish, "read_note", return_value=[r]), patch.object(publish, "process_run") as push:
            publish.publish(r, RUNNERS, "example/repo", {})
            push.assert_not_called()

    def test_overdue_attempt_closes_idempotently(self):
        old = record(); old.update(status="MISSING", conclusion="queued-too-long", gates=[], runner_name=None)
        with patch.object(publish, "git", side_effect=["remote ref", "", "remote"]), patch.object(publish, "read_note", return_value=[old]), patch.object(publish, "process_run") as push:
            publish.publish(record(), RUNNERS, "example/repo", {})
            push.assert_not_called()

    def test_collision_has_exact_diagnostic(self):
        r = record(); old = record(); old["occurrence"] = "different"
        with patch.object(publish, "git", side_effect=["remote ref", "", "remote"]), patch.object(publish, "read_note", return_value=[old]):
            with self.assertRaisesRegex(EvidenceError, "^replay would overwrite attempt$"):
                publish.publish(r, RUNNERS, "example/repo", {})

    def test_lost_response_retries_identical_attempt(self):
        r = record()
        with patch.object(publish, "git", side_effect=["old ref", "", "old", "new", "old ref", "new ref", "", "new"]), patch.object(publish, "read_note", side_effect=[[], [r]]), patch.object(publish, "process_run", side_effect=[subprocess.CompletedProcess([], 0), subprocess.CompletedProcess([], 1)]) as execute:
            publish.publish(r, RUNNERS, "example/repo", {})
            self.assertEqual(execute.call_count, 2)

    def test_dropped_response_readback_and_lease(self):
        ref = "refs/notes/concrete-gates-daily"
        with patch.object(publish, "git", side_effect=["old ref", "", "old", "new", "new ref"]), patch.object(publish, "read_note", return_value=[]), patch.object(publish, "process_run", side_effect=[subprocess.CompletedProcess([], 0), subprocess.TimeoutExpired("git push", 120)]) as execute:
            publish.publish(record(), RUNNERS, "example/repo", {})
            self.assertIn("--force-with-lease=" + ref + ":old", execute.call_args_list[-1].args[0])



if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()

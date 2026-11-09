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

    def test_overdue_cannot_discard_late_fail(self):
        import json
        from protocol import observe
        from test_protocol import NOW
        from outcome_control import preserves_failure
        old = record(); old.update(status='MISSING', conclusion='queued-too-long', gates=[], runner_name=None)
        fail = record(); fail.update(status='FAIL', conclusion='failure')
        fail['gates'][0]['status'] = 'FAIL'
        with patch.object(publish, 'git', side_effect=['old ref', '', 'old', 'new', 'new ref']), \
             patch.object(publish, 'read_note', return_value=[old]), \
             patch.object(publish, 'process_run', return_value=subprocess.CompletedProcess([], 0)) as execute:
            publish.publish(fail, RUNNERS, 'example/repo', {})
        history = [json.loads(line) for line in execute.call_args_list[0].kwargs['input'].splitlines()]
        self.assertEqual(history[0], old)
        self.assertEqual(history[1]['qualification_status'], 'FAIL')
        with self.assertRaises(EvidenceError) as caught:
            observe(history, SHA, 'daily', NOW, RUNNERS, 'example/repo')
        self.assertEqual(str(caught.exception), f'FAIL daily {SHA}: recorded attempt')
        # A lost-response retry finds the appended failure, without rewriting history.
        with patch.object(publish, 'git', side_effect=['new ref', '', 'new']), \
             patch.object(publish, 'read_note', return_value=history), \
             patch.object(publish, 'process_run') as execute:
            publish.publish(fail, RUNNERS, 'example/repo', {})
            execute.assert_not_called()
        import types
        source = Path(publish.__file__).read_text().replace(
            'if record["qualification_status"] != "FAIL":', 'if True:')
        plant = types.ModuleType('planted_publish'); plant.__file__ = publish.__file__
        exec(compile(source, publish.__file__, 'exec'), plant.__dict__)
        with patch.object(plant, 'git', side_effect=['old ref', '', 'old']), \
             patch.object(plant, 'read_note', return_value=[old]), \
             patch.object(plant, 'process_run') as execute:
            plant.publish(fail, RUNNERS, 'example/repo', {})
            self.assertEqual(execute.call_count, 0)
        with self.assertRaises(ValueError) as refused:
            preserves_failure('MISSING', 'overdue publication')
        self.assertEqual(str(refused.exception), 'FAIL scheduled monotonicity: overdue publication masked FAIL')

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

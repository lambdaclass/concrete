"""Light execution boundary controls; no campaigns, Nix builds or remote mutations."""
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import run
from protocol import EvidenceError
from test_protocol import SHA, RUNNERS

class Execution(unittest.TestCase):
    def environment(self):
        return dict(RUNNER_ENVIRONMENT="self-hosted", RUNNER_NAME="ci2-heavy",
                    GITHUB_REPOSITORY="example/repo", GITHUB_RUN_ID="1",
                    GITHUB_RUN_ATTEMPT="1", GITHUB_SERVER_URL="https://github.com")

    def test_missing_registry_is_incomplete(self):
        with tempfile.TemporaryDirectory() as work:
            def fake_call(argv, **kwargs):
                if argv[:2] == ["git", "clone"]:
                    Path(argv[-1]).mkdir()
            with patch.dict(os.environ, self.environment()), patch.object(run, "inventory", return_value=RUNNERS), patch.object(run, "git", return_value=SHA), patch.object(run, "call", side_effect=fake_call):
                self.assertEqual(run.run(SHA, "daily", work), 1)
            result = json.loads((Path(work) / "index.json").read_text())
            self.assertEqual(result["status"], "INCOMPLETE")
            self.assertEqual((Path(work) / "incomplete.log").read_text(),
                             "INCOMPLETE dependency: G1 tier registry unavailable\n")

    def test_forged_runner_never_executes(self):
        env = self.environment(); env["RUNNER_NAME"] = "other-host"
        with tempfile.TemporaryDirectory() as work, patch.dict(os.environ, env), patch.object(run, "inventory", return_value=RUNNERS), patch.object(run, "git", return_value=SHA), patch.object(run, "call") as execute:
            with self.assertRaises(EvidenceError) as caught:
                run.run(SHA, "daily", work)
            self.assertEqual(str(caught.exception), "forged runner: not inventoried concrete-ci-2")
            execute.assert_not_called()

if __name__ == "__main__":
    unittest.main()

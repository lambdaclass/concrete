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
            self.assertEqual(run.run(SHA, "daily", work), 1)
            self.assertEqual(json.loads((Path(work) / "index.json").read_text())["status"], "INCOMPLETE")
            self.assertEqual((Path(work) / "incomplete.log").read_text(), "forged runner: not inventoried concrete-ci-2\n")
            execute.assert_not_called()

    def test_missing_inventory_writes_evidence(self):
        with tempfile.TemporaryDirectory() as work, patch.dict(os.environ, self.environment()), patch.object(run, "inventory", side_effect=EvidenceError("INCOMPLETE dependency: ISS-0029 runner binding unavailable")), patch.object(run, "git", return_value=SHA):
            self.assertEqual(run.run(SHA, "daily", work), 1)
            result = json.loads((Path(work) / "index.json").read_text())
            self.assertEqual(result["status"], "INCOMPLETE")
            self.assertEqual((Path(work) / "incomplete.log").read_text(), "INCOMPLETE dependency: ISS-0029 runner binding unavailable\n")

    def test_wrong_checkout_revision_never_executes(self):
        def fake_call(argv, **kwargs):
            if argv[:2] == ["git", "clone"]:
                Path(argv[-1]).mkdir()
        with tempfile.TemporaryDirectory() as work, patch.dict(os.environ, self.environment()), patch.object(run, "inventory", return_value=RUNNERS), patch.object(run, "git", side_effect=[SHA, SHA, "b" * 40]), patch.object(run, "call", side_effect=fake_call) as execute:
            self.assertEqual(run.run(SHA, "daily", work), 1)
            result = json.loads((Path(work) / "index.json").read_text())
            self.assertEqual(result["status"], "INCOMPLETE")
            self.assertIsNone(result["gate_revision"])
            self.assertEqual(result["checkout_revision"], "b" * 40)
            self.assertEqual((Path(work) / "incomplete.log").read_text(), "wrong checked-out revision\n")
            self.assertEqual(execute.call_count, 2)

    def test_failure_survives_later_timeout_and_loop_continues(self):
        import subprocess
        rows = [dict(id=str(i), tier="daily", command=["gate"], budget_seconds=1,
                     tools={}, seed=1, controls=[dict(id="control", command=["control"], exit_code=1, diagnostic="planted")]) for i in range(3)]
        executed = []
        def fake_call(argv, **kwargs):
            if argv[:2] == ["git", "clone"]:
                root = Path(argv[-1]); (root / "scripts/checks/lib").mkdir(parents=True)
                (root / "scripts/checks/lib/gate_manifest.py").touch()
            if argv[0] == "python3":
                return subprocess.CompletedProcess(argv, 0, json.dumps(rows), "")
            if argv[0] == "nix":
                if argv[-1] == "control":
                    return subprocess.CompletedProcess(argv, 1, "planted", "")
                executed.append(argv)
                if len(executed) == 2:
                    raise subprocess.TimeoutExpired(argv, 1)
                return subprocess.CompletedProcess(argv, 1 if len(executed) == 1 else 0)
        with tempfile.TemporaryDirectory() as work, patch.dict(os.environ, self.environment()), patch.object(run, "inventory", return_value=RUNNERS), patch.object(run, "git", return_value=SHA), patch.object(run, "call", side_effect=fake_call), patch.object(Path, "is_file", return_value=True):
            self.assertEqual(run.run(SHA, "daily", work), 1)
            result = json.loads((Path(work) / "index.json").read_text())
            self.assertEqual(result["status"], "FAIL")
            self.assertEqual([g["status"] for g in result["gates"]], ["FAIL", "INCOMPLETE", "PASS"])
            self.assertEqual(result["gates"][1]["error"], str(subprocess.TimeoutExpired(executed[1], 1)))

if __name__ == "__main__":
    unittest.main()

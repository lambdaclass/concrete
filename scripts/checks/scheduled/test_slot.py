"""Exercise the real slot wrapper and bounded runner in a private controller tree."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
import run
from protocol import clean_env, git

ROOT = Path(__file__).resolve().parents[3]

class Slot(unittest.TestCase):
    def invoke(self, command, acquire=None, budget="1"):
        with tempfile.TemporaryDirectory() as work:
            root = Path(work) / "controller"
            lib = root / "scripts/checks/lib"; lib.mkdir(parents=True)
            scheduled = root / "scripts/checks/scheduled"; scheduled.mkdir()
            for name in ("bounded.sh", "bounded_exec.py", "heavy_slots.sh", "gate_outcome.py"):
                shutil.copy(ROOT / "scripts/checks/lib" / name, lib / name)
            (lib / "gate_env.sh").write_text(":\n")
            if acquire is not None:
                (lib / "heavy_slots.sh").write_text(f"heavy_slot_acquire() {{ {acquire}; }}\nheavy_slot_release() {{ :; }}\n")
            pool = Path(work) / "pool"; pool.mkdir()
            for number in (1, 2):
                (pool / f"slot-{number}").touch()
            env = clean_env(); env["CONCRETE_HEAVY_SLOT_DIR"] = str(pool)
            shutil.copy(ROOT / "scripts/checks/scheduled/with_slot.sh", scheduled)
            # Sourcing replayed libraries would produce this exact planted diagnostic.
            replay = Path(work) / "replay/scripts/checks/lib"; replay.mkdir(parents=True)
            (replay / "gate_env.sh").write_text("echo 'untrusted replay controller sourced'; exit 77\n")
            return subprocess.run(["bash", str(scheduled / "with_slot.sh"), budget, *command],
                                  cwd=replay.parents[2], capture_output=True, text=True, timeout=15, env=env)

    def test_replayed_libraries_not_sourced(self):
        result = self.invoke(["sh", "-c", "echo trusted-command"])
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, "trusted-command\n", ""))

    def test_slot_unavailable(self):
        result = self.invoke(["sh", "-c", "echo must-not-run"], acquire="echo 'slot unavailable' >&2; return 1")
        self.assertEqual((result.returncode, result.stdout, result.stderr), (125, "", "slot unavailable\n"))

    def test_completed_124_is_not_incomplete(self):
        result = self.invoke(["sh", "-c", "echo completed-124; exit 124"])
        self.assertEqual((result.returncode, result.stdout, result.stderr), (124, "completed-124\n", ""))

    def test_double_timeout(self):
        result = self.invoke(["python3", "-c", "import time; time.sleep(2)"], budget="0.05")
        self.assertEqual((result.returncode, result.stdout, result.stderr), (125, "", "INCOMPLETE scheduled gate: nested watchdog or gate incomplete\n"))

    def test_real_pool_missing(self):
        result = subprocess.run(["bash", "-c", 'source "$1"; heavy_slot_acquire', "bash",
                                 str(ROOT / "scripts/checks/lib/heavy_slots.sh")],
                                env={**clean_env(), "CONCRETE_HEAVY_SLOT_DIR": "/nonexistent/concrete-slot-control"},
                                capture_output=True, text=True, timeout=5)
        self.assertEqual((result.returncode, result.stderr),
                         (1, "INCOMPLETE heavy slots: host pool is not provisioned\n"))

    def test_real_pool_cross_process_and_release(self):
        import fcntl
        with tempfile.TemporaryDirectory() as work:
            pool = Path(work)
            handles = [(pool / f"slot-{n}").open("w+") for n in (1, 2)]
            for handle in handles:
                fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
            argv = ["bash", "-c", 'source "$1"; heavy_slot_acquire || exit 125; heavy_slot_release',
                    "bash", str(ROOT / "scripts/checks/lib/heavy_slots.sh")]
            env = {**clean_env(), "CONCRETE_HEAVY_SLOT_DIR": work}
            result = subprocess.run(argv, env=env, capture_output=True, text=True, timeout=5)
            self.assertEqual((result.returncode, result.stderr),
                             (125, "INCOMPLETE heavy slots: both host slots occupied\n"))
            handles[0].close()
            for _ in range(2):
                result = subprocess.run(argv, env=env, capture_output=True, text=True, timeout=5)
                self.assertEqual((result.returncode, result.stderr), (0, ""))
            handles[1].close()

    def test_malformed_structured_outcome(self):
        result=self.invoke(['bash','-c', 'printf "[]\\n" >> "$SCHEDULED_OUTCOME_FILE"'])
        self.assertEqual((result.returncode,result.stdout,result.stderr),
                         (1,'','FAIL scheduled outcome: invalid event stream\n'))

    def test_git_environment_sanitized(self):
        from unittest.mock import patch
        with patch.dict(os.environ, {"GIT_DIR": "/nonexistent/planted-git-dir", "GIT_WORK_TREE": "/nonexistent/planted-work-tree"}):
            self.assertNotIn("GIT_DIR", clean_env())
            self.assertRegex(git("-C", str(ROOT), "rev-parse", "HEAD"), r"^[0-9a-f]{40}$")
            result = run.call(["git", "-C", str(ROOT), "rev-parse", "HEAD"], capture_output=True, text=True, check=True)
            self.assertRegex(result.stdout.strip(), r"^[0-9a-f]{40}$")

if __name__ == "__main__":
    unittest.main()

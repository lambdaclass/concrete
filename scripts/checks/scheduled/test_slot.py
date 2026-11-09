"""Exercise the real slot wrapper and bounded runner in a private controller tree."""
from process_adapter import process_run
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from runtime_support import runtime_environment
import run
from protocol import clean_env, git

ROOT = Path(__file__).resolve().parents[3]

class Slot(unittest.TestCase):
    def invoke(self, command, acquire=None, budget="1"):
        with tempfile.TemporaryDirectory() as work:
            root = Path(work) / "controller"
            lib = root / "scripts/checks/lib"; lib.mkdir(parents=True)
            scheduled = root / "scripts/checks/scheduled"; scheduled.mkdir()
            for name in ("bounded.sh", "bounded_exec.py", "scheduled_process.py", "scheduled_process.sh", "heavy_slots.sh", "gate_outcome.py"):
                shutil.copy(ROOT / "scripts/checks/lib" / name, lib / name)
            (lib / "gate_env.sh").write_text(":\n")
            if acquire is not None:
                (lib / "heavy_slots.sh").write_text(f"heavy_slot_acquire() {{ {acquire}; }}\nheavy_slot_release() {{ :; }}\n")
            pool = Path(work) / "pool"; pool.mkdir()
            for number in (1, 2):
                (pool / f"slot-{number}").touch()
            env = runtime_environment(); env["CONCRETE_HEAVY_SLOT_DIR"] = str(pool)
            shutil.copy(ROOT / "scripts/checks/scheduled/with_slot.sh", scheduled)
            # Sourcing replayed libraries would produce this exact planted diagnostic.
            replay = Path(work) / "replay/scripts/checks/lib"; replay.mkdir(parents=True)
            (replay / "gate_env.sh").write_text("echo 'untrusted replay controller sourced'; exit 77\n")
            return process_run(["bash", str(scheduled / "with_slot.sh"), budget, *command],
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
        result = process_run(["bash", "-c", 'source "$1"; heavy_slot_acquire', "bash",
                                 str(ROOT / "scripts/checks/lib/heavy_slots.sh")],
                                env={**runtime_environment(), "CONCRETE_HEAVY_SLOT_DIR": "/nonexistent/concrete-slot-control"},
                                capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
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
            env = {**runtime_environment(), "CONCRETE_HEAVY_SLOT_DIR": work}
            result = process_run(argv, env=env, capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
            self.assertEqual((result.returncode, result.stderr),
                             (125, "INCOMPLETE heavy slots: both host slots occupied\n"))
            handles[0].close()
            for _ in range(2):
                result = process_run(argv, env=env, capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
                self.assertEqual((result.returncode, result.stderr), (0, ""))
            handles[1].close()

    def test_malformed_structured_outcome(self):
        result=self.invoke(['bash','-c', 'printf "[]\\n" >> "$SCHEDULED_OUTCOME_FILE"'])
        self.assertEqual((result.returncode,result.stdout,result.stderr),
                         (1,'','FAIL scheduled outcome: invalid event stream\n'))

    def test_expected_inner_timeout_is_not_gate_outcome(self):
        command = ['bash', '-c', 'source "$1"; bounded_run 0.02 -- sleep 1; rc=$?; bounded_is_incomplete "$rc" || exit 7; echo expected-timeout', 'bash', str(ROOT / 'scripts/checks/lib/bounded.sh')]
        result = self.invoke(command)
        self.assertEqual((result.returncode, result.stdout, result.stderr),
                         (0, 'expected-timeout\n', ''))

    def test_outer_retry_discards_abandoned_outcome(self):
        with tempfile.TemporaryDirectory() as work:
            marker = str(Path(work) / 'attempted')
            result = self.invoke(['bash', '-c',
                'if [ ! -e "$1" ]; then touch "$1"; python3 "$SCHEDULED_OUTCOME_HELPER" INCOMPLETE "abandoned attempt"; exec sleep 2; fi; echo recovered',
                'bash', marker], budget='0.1')
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'recovered\n', ''))

    def test_child_reuses_parent_slot(self):
        script = r'''source "$1"
heavy_slot_acquire || exit 7
parent_fd=$CONCRETE_HEAVY_SLOT_FD
bash -c 'source "$1"; heavy_slot_acquire || exit 8; [ "$CONCRETE_HEAVY_SLOT_FD" = "$2" ] || { echo "child acquired another slot"; exit 9; }; heavy_slot_release' bash "$1" "$parent_fd" || exit $?
# Child release must leave the parent's lock held.
flock -n "$CONCRETE_HEAVY_SLOT_DIR/slot-1" true && { echo "parent lock lost"; exit 10; }
heavy_slot_release
echo inherited-slot
'''
        with tempfile.TemporaryDirectory() as work:
            for n in (1, 2): (Path(work) / f'slot-{n}').touch()
            result = process_run(['bash', '-c', script, 'bash', str(ROOT / 'scripts/checks/lib/heavy_slots.sh')],
                env={**runtime_environment(), 'CONCRETE_HEAVY_SLOT_DIR': work}, capture_output=True, text=True, timeout=5, cwd=Path(__file__).resolve().parents[3])
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'inherited-slot\n', ''))

    def test_watchdog_child_reuses_slot(self):
        result = self.invoke(['bash', '-c', 'source "$1"; heavy_slot_acquire || exit 8; echo inherited-slot; heavy_slot_release',
            'bash', str(ROOT / 'scripts/checks/lib/heavy_slots.sh')])
        self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'inherited-slot\n', ''))

    def test_invalid_inherited_slot_fails_closed(self):
        result = self.invoke(['bash', '-c', 'export CONCRETE_HEAVY_SLOT_FD=9999; source "$1"; heavy_slot_acquire',
            'bash', str(ROOT / 'scripts/checks/lib/heavy_slots.sh')])
        self.assertEqual((result.returncode, result.stdout, result.stderr),
            (1, '', 'INCOMPLETE heavy slots: invalid inherited slot descriptor\n'))

    def test_outer_timeout_and_cancel_reap_gate_and_release_slot(self):
        import fcntl
        import signal
        from bounded_exec import supervise
        for cancel in (False, True):
            with self.subTest(cancel=cancel), tempfile.TemporaryDirectory() as work:
                pool = Path(work) / 'pool'; pool.mkdir()
                for n in (1, 2): (pool / f'slot-{n}').touch()
                marker = Path(work) / 'members.json'
                program = ('import json,os,sys,time; child=os.fork(); '
                           'open(sys.argv[1],"w").write(json.dumps([os.getpid(),child])) if child else None; '
                           'time.sleep(20)')
                command = ['bash', str(ROOT / 'scripts/checks/scheduled/with_slot.sh'), '10',
                           os.sys.executable, '-c', program, str(marker)]
                environment = dict(runtime_environment(), CONCRETE_HEAVY_SLOT_DIR=str(pool))
                if cancel:
                    # The adapter receives cancellation itself, from a separate
                    # bounded test driver, rather than from an expired gate budget.
                    driver = r"""
import os, signal, sys, threading, time
sys.path.insert(0, sys.argv[1])
from process_adapter import process_run
from bounded_exec import Cancelled
from pathlib import Path
def cancel():
    end = time.monotonic() + 2
    while time.monotonic() < end:
        try:
            import json
            if len(json.loads(Path(sys.argv[2]).read_text())) == 2: break
        except (OSError, ValueError): pass
        time.sleep(.005)
    os.kill(os.getpid(), signal.SIGTERM)
threading.Thread(target=cancel).start()
try:
    process_run(sys.argv[3:], cwd=sys.argv[4], env={k:v for k,v in os.environ.items() if not k.startswith('BOUNDED_TREE_')}, timeout=5, grace=.2)
except Cancelled:
    print('cancelled and reaped')
"""
                    # cwd is an explicit argument independent of the command argv.
                    driver = driver.replace('cwd=sys.argv[4]', 'cwd=os.getcwd()')
                    result = process_run([os.sys.executable, '-c', driver,
                                          str(ROOT / 'scripts/checks/scheduled'), str(marker), *command],
                                         cwd=ROOT, env=environment, capture_output=True, text=True, timeout=5)
                    self.assertEqual((result.returncode, result.stdout, result.stderr), (0, 'cancelled and reaped\n', ''))
                else:
                    with self.assertRaises(subprocess.TimeoutExpired):
                        process_run(command, cwd=ROOT, env=environment, timeout=.4, grace=.2,
                                    capture_output=True, text=True)
                import json
                members = json.loads(marker.read_text())
                self.assertTrue(all(pid > 0 for pid in members))
                self.assertTrue(all(not Path(f'/proc/{pid}').exists() for pid in members), members)
                for n in (1, 2):
                    with (pool / f'slot-{n}').open() as handle:
                        fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)

    def test_git_environment_sanitized(self):
        from unittest.mock import patch
        with patch.dict(os.environ, {"GIT_DIR": "/nonexistent/planted-git-dir", "GIT_WORK_TREE": "/nonexistent/planted-work-tree"}):
            self.assertNotIn("GIT_DIR", clean_env(dict(os.environ)))
            self.assertRegex(git("-C", str(ROOT), "rev-parse", "HEAD", environment=dict(os.environ)), r"^[0-9a-f]{40}$")
            result = run.call(["git", "-C", str(ROOT), "rev-parse", "HEAD"], capture_output=True, text=True, check=True, env=clean_env(dict(os.environ)), cwd=Path(__file__).resolve().parents[3], timeout=120)
            self.assertRegex(result.stdout.strip(), r"^[0-9a-f]{40}$")



if __name__ == "__main__":
    from suite_adapter import direct_main
    direct_main()

"""Execute the publisher timeout path with short real bounded attempts."""
from pathlib import Path
import re
import subprocess
import tempfile
import unittest
from test_support import runtime_environment

ROOT = Path(__file__).resolve().parents[3]

def publisher_script():
    text = (ROOT / ".github/workflows/gate_publish.yml").read_text()
    return "\n".join(line[10:] for line in text.split("        run: |\n", 1)[1].splitlines()) + "\n"

def check_budget(script, job_seconds=1200):
    match = re.search(r"bounded_run -r ([0-9]+) ([0-9]+) -- python3", script)
    if not match or sum(map(int, match.groups())) + 160 >= job_seconds:
        raise ValueError("publisher retry budgets exceed job timeout reserve")

class Workflows(unittest.TestCase):
    def test_real_timeout_is_explicit_and_cleaned(self):
        script = publisher_script()
        check_budget(script)
        script = script.replace("source scripts/checks/lib/bounded.sh", f"source {ROOT}/scripts/checks/lib/bounded.sh")
        script = re.sub(r"bounded_run 20 -- git config[^\n]+", "true", script)
        script = script.replace("bounded_run -r 300 300 -- python3 scripts/checks/scheduled/collect.py", "bounded_run -r 0.05 0.05 -- python3 -c 'import time; time.sleep(5)'")
        with tempfile.TemporaryDirectory() as scratch:
            import os
            env = dict(runtime_environment(), TMPDIR=scratch)
            result = subprocess.run(["bash", "-c", script], cwd=scratch, env=env, capture_output=True, text=True, timeout=10)
            self.assertEqual(result.returncode, 1)
            self.assertEqual(result.stdout.strip(), "INCOMPLETE scheduled collection: bounded attempts exhausted")
            self.assertEqual(result.stderr, "")
            self.assertEqual(list(Path(scratch).iterdir()), [])

    def test_planted_retry_budget(self):
        with self.assertRaisesRegex(ValueError, "^publisher retry budgets exceed job timeout reserve$"):
            check_budget(publisher_script().replace("-r 300 300", "-r 2200 1100"))

    def test_execution_uploads_partial_logs_without_unused_bounded_state(self):
        text = (ROOT / ".github/workflows/gate_schedule.yml").read_text()
        execution = text.split("      - name: Run exact-SHA tier", 1)[1].split("      - name: Upload", 1)[0]
        self.assertNotIn("source scripts/checks/lib/bounded.sh", execution)
        self.assertIn("--timeout 80000", execution)
        self.assertIn("path: result-index/", text)
        self.assertEqual(text.count("if: always()"), 2)

if __name__ == "__main__":
    unittest.main()

"""Run a tier in a private exact-SHA clone; registry and CI2 binding are dependencies."""
import datetime as dt
import json
import os
import signal
from pathlib import Path
import subprocess
import sys
import tempfile
import time
from protocol import EvidenceError, git, inventory, require, clean_env

ROOT = Path(__file__).resolve().parents[3]

def now():
    return dt.datetime.now(dt.timezone.utc).isoformat()

def call(argv, **kwargs):
    budget = kwargs.pop("timeout", 120)
    check = kwargs.pop("check", False)
    if kwargs.pop("capture_output", False):
        kwargs["stdout"] = subprocess.PIPE
        kwargs["stderr"] = subprocess.PIPE
    kwargs.setdefault("env", clean_env())
    process = subprocess.Popen(argv, start_new_session=True, **kwargs)
    try:
        stdout, stderr = process.communicate(timeout=budget)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGTERM)
        try:
            process.communicate(timeout=10)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.communicate(timeout=10)
        raise
    result = subprocess.CompletedProcess(argv, process.returncode, stdout, stderr)
    if check:
        result.check_returncode()
    return result

def run(sha, tier, output):
    output = Path(output).resolve(); output.mkdir(parents=True, exist_ok=True)
    attempt = os.environ["GITHUB_RUN_ID"] + "/" + os.environ["GITHUB_RUN_ATTEMPT"]
    record = dict(sha=sha, tier=tier, repository=os.environ["GITHUB_REPOSITORY"],
                  runner_name=os.environ["RUNNER_NAME"], runner_environment="self-hosted",
                  attempt=attempt, occurrence=os.environ.get("OCCURRENCE", tier) + "/" + os.environ["GITHUB_RUN_ID"],
                  controller_revision=None,
                  gate_revision=None, started=now(), status="INCOMPLETE", conclusion="incomplete",
                  artifact=os.environ["GITHUB_SERVER_URL"] + "/" + os.environ["GITHUB_REPOSITORY"] +
                  "/actions/runs/" + os.environ["GITHUB_RUN_ID"], gates=[])
    with tempfile.TemporaryDirectory(prefix="concrete-scheduled-") as scratch:
        checkout = Path(scratch) / "source"
        try:
            record["controller_revision"] = git("rev-parse", "HEAD")
            require(git("rev-parse", sha + "^{commit}") == sha, "wrong SHA")
            runners = inventory()
            require(os.environ.get("RUNNER_NAME") in runners.get(os.environ.get("GITHUB_REPOSITORY"), set()),
                    "forged runner: not inventoried concrete-ci-2")
            call(["git", "clone", "--shared", "--no-checkout", str(ROOT), str(checkout)], check=True)
            call(["git", "-C", str(checkout), "checkout", "--detach", sha], check=True)
            record["checkout_revision"] = git("-C", str(checkout), "rev-parse", "HEAD")
            require(record["checkout_revision"] == sha, "wrong checked-out revision")
            record["gate_revision"] = record["checkout_revision"]
            exporter = checkout / "scripts/checks/lib/gate_manifest.py"
            slots = ROOT / "scripts/checks/lib/heavy_slots.sh"
            require(exporter.is_file(), "INCOMPLETE dependency: G1 tier registry unavailable")
            require(slots.is_file(), "INCOMPLETE dependency: shared host-wide heavy slots unavailable")
            result = call(["python3", str(exporter), "scheduled", "--tier", tier],
                          cwd=checkout, check=True, capture_output=True, text=True)
            rows = json.loads(result.stdout)
            require(isinstance(rows, list) and rows, "INCOMPLETE dependency: tier has no registered gates")
            require(len({r["id"] for r in rows}) == len(rows), "duplicate registry gate")
            for row in rows:
                require(row["tier"] == tier and isinstance(row["command"], list) and row["command"], "invalid scheduled registry row")
                require(isinstance(row["budget_seconds"], int) and row["budget_seconds"] > 0, "invalid registry budget")
                require(row["controls"], "missing registered controls")
            for number, row in enumerate(rows):
                log = output / f"gate-{number}.log"
                gate = dict(id=row["id"], status="INCOMPLETE", command=row["command"],
                            tools=row["tools"], seed=row["seed"], resources={}, controls=[], log=log.name)
                record["gates"].append(gate)
                try:
                    began = time.monotonic()
                    # The shared helper owns acquisition/release; gate commands inherit the held slot.
                    command = ["nix", "develop", "--command", "bash", str(ROOT / "scripts/checks/scheduled/with_slot.sh"),
                               str(row["budget_seconds"]), *row["command"]]
                    gate_env = clean_env()
                    gate_env.update(
                        SELECTED_SHA=sha,
                        SELECTED_TIER=tier,
                        CONCRETE_SCALING_EXPECTED_SHA=sha,
                        CONCRETE_SCALING_GITHUB_REPOSITORY=os.environ.get("GITHUB_REPOSITORY", ""),
                        CONCRETE_SCALING_RUNNER_ENVIRONMENT=os.environ.get("RUNNER_ENVIRONMENT", "self-hosted"),
                        CONCRETE_SCALING_RUNNER_NAME=os.environ.get("RUNNER_NAME", ""),
                    )
                    with log.open("w") as stream:
                        outcome = call(command, cwd=checkout, stdout=stream, stderr=subprocess.STDOUT,
                                       timeout=row["budget_seconds"] * 3 + 180, env=gate_env)
                    gate["resources"] = {"wall_seconds": time.monotonic() - began, "exit_code": outcome.returncode}
                    gate["status"] = "PASS" if outcome.returncode == 0 else "INCOMPLETE" if outcome.returncode == 125 else "FAIL"
                    for control in row["controls"]:
                        require(isinstance(control["command"], list) and control["command"], "invalid control command")
                        require(isinstance(control["diagnostic"], str) and control["diagnostic"], "missing exact control diagnostic")
                        control_run = call(["nix", "develop", "--command", "bash", str(ROOT / "scripts/checks/scheduled/with_slot.sh"),
                                            str(row["budget_seconds"]), *control["command"]], cwd=checkout,
                                           capture_output=True, text=True, timeout=row["budget_seconds"] * 3 + 180)
                        diagnostic = (control_run.stdout + control_run.stderr).strip()
                        passed = control_run.returncode == control["exit_code"] and control_run.returncode not in (0, 125) and diagnostic == control["diagnostic"]
                        status = "PASS" if passed else "INCOMPLETE" if control_run.returncode == 125 else "FAIL"
                        gate["controls"].append(dict(id=control["id"], status=status, diagnostic=diagnostic))
                        if status == "FAIL" or (status == "INCOMPLETE" and gate["status"] != "FAIL"):
                            gate["status"] = status
                except (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
                    gate["error"] = str(error)
                    if gate["status"] != "FAIL":
                        gate["status"] = "INCOMPLETE"
        except (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
            (output / "incomplete.log").write_text(str(error) + "\n")
            print(error, file=sys.stderr)
            # A dependency failure is evidence about availability, never a dummy passed gate.
            record["gates"].append(dict(id="scheduler-dependencies", status="INCOMPLETE", command=[],
                                       tools={}, seed=None, resources={}, controls=[], log="incomplete.log"))
    statuses = {g["status"] for g in record["gates"]}
    record["status"] = "FAIL" if "FAIL" in statuses else "INCOMPLETE" if "INCOMPLETE" in statuses else "PASS"
    record["conclusion"] = {"PASS": "success", "FAIL": "failure", "INCOMPLETE": "incomplete"}[record["status"]]
    record["completed"] = now()
    (output / "index.json").write_text(json.dumps(record, sort_keys=True) + "\n")
    return 0 if record["status"] == "PASS" else 1

if __name__ == "__main__":
    try:
        sys.exit(run(*sys.argv[1:]))
    except EvidenceError as error:
        sys.exit(str(error))

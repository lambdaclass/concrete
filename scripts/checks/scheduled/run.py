"""Run a tier in a private exact-SHA clone; registry and CI2 binding are dependencies."""
import hashlib
import json
import os
import signal
import shutil
import stat
from pathlib import Path
import subprocess
import sys
import tempfile
from protocol import EvidenceError, git, inventory, require, clean_env, _registry, _binding

ROOT = Path(__file__).resolve().parents[3]

from process_adapter import operation_clock, process_run as call

def preserve_artifacts(row, checkout, output, number, sha, required, saved):
    """Preserve regular report trees before private-checkout cleanup, including partial runs."""
    for declared in row["artifacts"]:
        source = checkout / declared["path"]
        if not source.exists():
            require(not required, f"FAIL scheduled artifact: missing {declared['path']}")
            continue
        paths = [source, *source.rglob("*")] if source.is_dir() else [source]
        for path in paths:
            require(not path.is_symlink() and (path.is_dir() or stat.S_ISREG(path.stat().st_mode)),
                    f"FAIL scheduled artifact: unsafe path {path.relative_to(checkout)}")
        # Also refuse symlinked parents that escape the checkout.
        require(all(not parent.is_symlink() for parent in [source, *source.parents] if parent != checkout and checkout in parent.parents),
                f"FAIL scheduled artifact: unsafe path {declared['path']}")
        target = output / f"gate-{number}-artifacts" / declared["path"]
        target.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir(): shutil.copytree(source, target)
        else: shutil.copy2(source,target)
        entries=[]
        for path in sorted(target.rglob("*")) if target.is_dir() else [target]:
            if path.is_file():
                with path.open("rb") as stream:
                    digest=hashlib.file_digest(stream,"sha256").hexdigest()
                entries.append(dict(path=str(path.relative_to(output)), bytes=path.stat().st_size, sha256=digest))
        artifact = dict(path=str(target.relative_to(output)), files=entries, sha=None,
                        binding_status="INCOMPLETE")
        saved.append(artifact)
        binding=checkout / declared["sha_file"]
        require(not binding.is_symlink(), f"FAIL scheduled artifact: unsafe path {declared['sha_file']}")
        if not binding.exists():
            diagnostic = f"INCOMPLETE scheduled artifact: missing SHA binding {declared['sha_file']}"
            artifact["diagnostic"] = diagnostic
            require(not required, diagnostic)
            continue
        require(binding.is_file(), f"FAIL scheduled artifact: unsafe path {declared['sha_file']}")
        require(binding.stat().st_size <= 41 and binding.read_bytes() in (sha.encode(), (sha + "\n").encode()), f"FAIL scheduled artifact: selected SHA mismatch {declared['path']}")
        artifact.update(sha=sha, binding_status="PASS")
    return saved

def gate_environment(sha, tier, attempt, environment):
    from scheduled_process import role_environment
    env = role_environment(clean_env(environment), 'gate')
    env.update(SELECTED_SHA=sha, SELECTED_TIER=tier, SCHEDULED_ATTEMPT_ID=attempt)
    return env

def run(sha, tier, output, environment, *, clock=None):
    clock = clock or operation_clock()
    environment = dict(environment)
    output = Path(output).resolve(); output.mkdir(parents=True, exist_ok=True)
    attempt = environment["GITHUB_RUN_ID"] + "/" + environment["GITHUB_RUN_ATTEMPT"]
    record = dict(sha=sha, tier=tier, repository=environment["GITHUB_REPOSITORY"],
                  runner_name=environment.get("RUNNER_NAME"), runner_environment=environment.get("RUNNER_ENVIRONMENT"),
                  attempt=attempt, occurrence=environment.get("OCCURRENCE", tier) + "/" + environment["GITHUB_RUN_ID"],
                  controller_revision=None,
                  gate_revision=None, started=clock.observation.isoformat(), status="INCOMPLETE", conclusion="incomplete",
                  artifact=environment["GITHUB_SERVER_URL"] + "/" + environment["GITHUB_REPOSITORY"] +
                  "/actions/runs/" + environment["GITHUB_RUN_ID"], gates=[])
    with tempfile.TemporaryDirectory(prefix="concrete-scheduled-") as scratch:
        checkout = Path(scratch) / "source"
        try:
            record["controller_revision"] = git("-C", str(ROOT), "rev-parse", "HEAD", environment=environment)
            require(git("-C", str(ROOT), "rev-parse", sha + "^{commit}", environment=environment) == sha, "wrong SHA")
            require(environment.get("RUNNER_ENVIRONMENT") == "self-hosted",
                    "forged runner: expected RUNNER_ENVIRONMENT=self-hosted")
            runners = inventory()
            _binding.verify(environment.get("GITHUB_REPOSITORY"), environment.get("RUNNER_ENVIRONMENT"),
                            environment.get("RUNNER_NAME"), runners)
            call(["git", "clone", "--shared", "--no-checkout", str(ROOT), str(checkout)], check=True, env=clean_env(environment))
            call(["git", "-C", str(checkout), "checkout", "--detach", sha], check=True, env=clean_env(environment))
            record["checkout_revision"] = git("-C", str(checkout), "rev-parse", "HEAD", environment=environment)
            require(record["checkout_revision"] == sha, "wrong checked-out revision")
            record["gate_revision"] = record["checkout_revision"]
            registry_path = checkout / "scripts/checks/gates.json"
            require(registry_path.is_file(), "INCOMPLETE dependency: G1 tier registry unavailable")
            require((ROOT / "scripts/checks/lib/heavy_slots.sh").is_file(),
                    "INCOMPLETE dependency: shared host-wide heavy slots unavailable")
            # Parse selected data using trusted controller code, never execute its validator.
            rows = _registry.scheduled_rows(tier, _registry.load(registry_path))
            require(rows, "INCOMPLETE dependency: tier has no registered gates")
            gate_env = gate_environment(sha, tier, attempt, environment)
            for number, row in enumerate(rows):
                log = output / f"gate-{number}.log"
                gate = dict(id=row["id"], status="INCOMPLETE", command=row["command"],
                            tools=row["tools"], seed=row["seed"], resources={}, controls=[], log=log.name, artifacts=[])
                record["gates"].append(gate)
                artifacts_attempted = False
                try:
                    began = clock.monotonic()
                    # The shared helper owns acquisition/release; gate commands inherit the held slot.
                    command = ["nix", "develop", "--command", "bash", str(ROOT / "scripts/checks/scheduled/with_slot.sh"),
                               str(row["budget_seconds"]), *row["command"]]
                    with log.open("w") as stream:
                        outcome = call(command, cwd=checkout, stdout=stream, stderr=subprocess.STDOUT,
                                       timeout=row["budget_seconds"] * 3 + 180, env=gate_env)
                    gate["resources"] = {"wall_seconds": clock.monotonic() - began, "exit_code": outcome.returncode}
                    gate["status"] = "PASS" if outcome.returncode == 0 else "INCOMPLETE" if outcome.returncode == 125 else "FAIL"
                    artifacts_attempted = True
                    gate["artifacts"] = preserve_artifacts(row, checkout, output, number, sha, gate["status"] == "PASS", gate["artifacts"])
                    for control in row["controls"]:
                        require(isinstance(control["command"], list) and control["command"], "invalid control command")
                        require(isinstance(control["diagnostic"], str) and control["diagnostic"], "missing exact control diagnostic")
                        control_run = call(["nix", "develop", "--command", "bash", str(ROOT / "scripts/checks/scheduled/with_slot.sh"),
                                            str(row["budget_seconds"]), *control["command"]], cwd=checkout,
                                           capture_output=True, text=True, timeout=row["budget_seconds"] * 3 + 180, env=gate_env)
                        diagnostic = (control_run.stdout + control_run.stderr).strip()
                        passed = control_run.returncode == control["exit_code"] and control_run.returncode not in (0, 125) and diagnostic == control["diagnostic"]
                        status = "PASS" if passed else "INCOMPLETE" if control_run.returncode == 125 else "FAIL"
                        gate["controls"].append(dict(id=control["id"], status=status, diagnostic=diagnostic))
                        if status == "FAIL" or (status == "INCOMPLETE" and gate["status"] != "FAIL"):
                            gate["status"] = status
                except (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
                    gate["error"] = str(error)
                    if str(error).startswith("FAIL scheduled artifact:"):
                        gate["status"] = "FAIL"
                    elif gate["status"] != "FAIL":
                        gate["status"] = "INCOMPLETE"
                finally:
                    if not artifacts_attempted:
                        try:
                            preserve_artifacts(row, checkout, output, number, sha, False, gate["artifacts"])
                        except (EvidenceError, OSError, ValueError) as error:
                            gate["artifact_error"] = str(error)
                            if str(error).startswith("FAIL scheduled artifact:"):
                                gate["status"] = "FAIL"
        except (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
            (output / "incomplete.log").write_text(str(error) + "\n")
            print(error, file=sys.stderr)
            # A dependency failure is evidence about availability, never a dummy passed gate.
            record["execution_error"] = str(error)
    statuses = {g["status"] for g in record["gates"]}
    record["status"] = "FAIL" if "FAIL" in statuses else "INCOMPLETE" if "INCOMPLETE" in statuses else "INCOMPLETE" if record.get("execution_error") else "PASS"
    record["conclusion"] = {"PASS": "success", "FAIL": "failure", "INCOMPLETE": "incomplete"}[record["status"]]
    record["completed"] = clock.instant().isoformat()
    (output / "index.json").write_text(json.dumps(record, sort_keys=True) + "\n")
    return 0 if record["status"] == "PASS" else 1

if __name__ == "__main__":
    try:
        sys.exit(run(*sys.argv[1:], dict(os.environ)))
    except EvidenceError as error:
        sys.exit(str(error))

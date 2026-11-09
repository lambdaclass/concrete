"""Hosted-only collector. Worker JSON is data; Actions API supplies job provenance.

The completion trigger handles whole-workflow cancellation. The hourly sweep also
records queued/unstarted attempts after 26 hours, independently of worker capacity.
"""
import datetime as dt
import io
import json
import os
import re
import subprocess
import sys
import urllib.request
import urllib.parse
import zipfile
from protocol import EvidenceError, inventory, require, validate
from publish import publish

API = "https://api.github.com"
MAX_AGE = 26 * 3600

class SafeRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        require(urllib.parse.urlparse(newurl).scheme == "https", "insecure API redirect")
        redirected = super().redirect_request(req, fp, code, msg, headers, newurl)
        redirected.remove_header("Authorization")
        return redirected

def request(path, binary=False, *, token):
    url = path if path.startswith("https://") else API + path
    require(url.startswith(API + "/"), "untrusted API URL")
    req = urllib.request.Request(url, headers={"Authorization": "Bearer " + token,
                                                "Accept": "application/vnd.github+json"})
    with urllib.request.build_opener(SafeRedirect()).open(req, timeout=60) as response:
        body = response.read(16 * 1024 * 1024 + 1)
    require(len(body) <= 16 * 1024 * 1024, "collector response too large")
    return body if binary else json.loads(body)

def pages(path, key, *, token):
    result = []
    for page in range(1, 1001):
        data = request(path + ("&" if "?" in path else "?") + f"per_page=100&page={page}", token=token)[key]
        result.extend(data)
        if len(data) < 100:
            return result
    raise EvidenceError("INCOMPLETE: API pagination exceeded")

def stamp():
    return dt.datetime.now(dt.timezone.utc).isoformat()

def selected(base, jobs, run, *, token):
    selectors = [j for j in jobs if j["name"] == "select"]
    matches = []
    if selectors and selectors[0].get("conclusion") == "success":
        logs = request(base + f'/jobs/{selectors[0]["id"]}/logs', binary=True, token=token).decode()
        matches = re.findall(r"(?m)^(?:\d{4}-\d\d-\d\dT\S+ )?SCHEDULE_SELECTION ([0-9a-f]{40}) (daily|weekly)\s*$", logs)
    # Rejected manual inputs have no accepted selection and must not annotate main.
    if not matches and run["event"] == "workflow_dispatch":
        return []
    if not matches and run["event"] == "schedule":
        tier = run.get("display_title", "").removeprefix("scheduled-")
        require(tier in {"daily", "weekly"}, "INCOMPLETE: missing trusted schedule tier")
        return [(run["head_sha"], tier)]
    require(len(matches) == 1, "INCOMPLETE: missing trusted selection diagnostic")
    return matches

def record_for(run, job, selection, payload, artifact, repository, runners):
    sha, tier = selection
    missing = ("queued-too-long" if (job or run).get("status") in {"queued", "waiting", "pending", "requested"} else
               job.get("conclusion") if job and job.get("conclusion") in {"cancelled", "skipped"} else
               "absent" if not job else None)
    identity = dict(sha=sha, tier=tier, repository=repository,
                    attempt=f'{run["id"]}/{run["run_attempt"]}', occurrence=f'{run["event"]}/{run["id"]}',
                    runner_name=job.get("runner_name") if job else None,
                    runner_environment="self-hosted" if job and job.get("runner_name") else None)
    if missing:
        identity.update(runner_name=None, runner_environment=None)
        record = dict(identity, status="MISSING", conclusion=missing, gates=[], artifact=None,
                      gate_revision=None, started=run["created_at"], completed=(job.get("completed_at") if job else None) or (stamp() if missing == "queued-too-long" else run.get("updated_at", stamp())))
    else:
        if payload is None:
            payload = dict(identity, gate_revision=None, status="INCOMPLETE", conclusion="incomplete",
                           gates=[dict(id="execution-index", status="INCOMPLETE", command=[], tools={}, seed=None,
                                       resources={}, controls=[], log="Actions execution completed without index.json")])
            artifact = artifact or f'https://github.com/{repository}/actions/runs/{run["id"]}'
        require(isinstance(payload, dict), "invalid worker index shape")
        require(payload.get("sha") == sha and payload.get("tier") == tier, "wrong worker selection")
        require(payload.get("repository") == repository, "wrong repository")
        require(payload.get("attempt") == identity["attempt"], "wrong worker attempt")
        record = dict(payload)
        record.update(identity, artifact=artifact, started=job["started_at"], completed=job["completed_at"])
        # A worker cannot claim success if Actions did not observe successful execution.
        if job["conclusion"] != "success" and record["status"] == "PASS":
            record["gates"].append(dict(id="actions-job", status="INCOMPLETE", command=[], tools={},
                                        seed=None, resources={}, controls=[], log="Actions job metadata"))
            record.update(status="INCOMPLETE", conclusion="incomplete")
    validate(record, sha, tier, runners, repository, attempt=identity["attempt"])
    return record

def read_index(archive):
    with archive.open("index.json") as stream:
        data = stream.read(1024 * 1024 + 1)
    require(len(data) <= 1024 * 1024, "index too large")
    payload = json.loads(data)
    require(isinstance(payload, dict) and isinstance(payload.get("gates"), list) and
            all(isinstance(g, dict) for g in payload["gates"]), "invalid worker index shape")
    return payload

def collect(run, repository, runners, environment):
    token = environment["GH_TOKEN"]
    require(run["head_branch"] == "main" and run["event"] in {"schedule", "workflow_dispatch"}, "untrusted scheduled run")
    base = f"/repos/{repository}/actions"
    jobs = pages(base + f'/runs/{run["id"]}/attempts/{run["run_attempt"]}/jobs', "jobs", token=token)
    campaigns = [j for j in jobs if j["name"] == "campaign"]
    require(len(campaigns) <= 1, "ambiguous campaign job")
    job = campaigns[0] if campaigns else None
    if job and job["status"] in {"queued", "waiting", "pending", "requested"}:
        queued_at = job.get("created_at", run["created_at"])
        if (dt.datetime.fromisoformat(stamp()) - dt.datetime.fromisoformat(queued_at.replace("Z", "+00:00"))).total_seconds() < MAX_AGE:
            return
    if job and job["status"] == "in_progress":
        return  # Queue age is not execution age; let the execution timeout decide.
    artifacts = pages(base + f'/runs/{run["id"]}/artifacts', "artifacts", token=token) if job and job["status"] == "completed" else []
    for selection in selected(base, jobs, run, token=token):
        name = f'scheduled-index-{selection[1]}-{run["id"]}-{run["run_attempt"]}'
        found = [a for a in artifacts if a["name"] == name and not a["expired"]]
        require(len(found) <= 1, "ambiguous result artifact")
        payload = None; url = None
        if found:
            data = request(base + f'/artifacts/{found[0]["id"]}/zip', binary=True, token=token)
            with zipfile.ZipFile(io.BytesIO(data)) as archive:
                if "index.json" in archive.namelist():
                    payload = read_index(archive)
            logs = [a for a in artifacts if a["name"] == name.replace("scheduled-index-", "scheduled-", 1) and not a["expired"]]
            require(len(logs) <= 1, "ambiguous logs artifact")
            if not logs and payload is not None:
                payload["gates"].append(dict(id="artifact-upload", status="INCOMPLETE", command=[], tools={},
                                            seed=None, resources={}, controls=[], log="Actions upload metadata"))
                if payload["status"] != "FAIL":
                    payload.update(status="INCOMPLETE", conclusion="incomplete")
            artifact = logs[0] if logs else found[0]
            url = f'https://github.com/{repository}/actions/runs/{run["id"]}/artifacts/{artifact["id"]}'
        publish(record_for(run, job, selection, payload, url, repository, runners), runners, repository, environment)

def collect_event(environment):
    repository = environment["GITHUB_REPOSITORY"]
    token = environment["GH_TOKEN"]
    with open(environment["GITHUB_EVENT_PATH"]) as source:
        event = json.load(source)
    if "workflow_run" in event:
        runs = [event["workflow_run"]]
    else:
        path = f"/repos/{repository}/actions/workflows/gate_schedule.yml/runs?branch=main"
        cutoff = (dt.datetime.now(dt.timezone.utc) - dt.timedelta(seconds=2 * MAX_AGE)).strftime("%Y-%m-%dT%H:%M:%SZ")
        candidates = pages(path + "&created=" + urllib.parse.quote(">=" + cutoff), "workflow_runs", token=token)
        # Old live runs must remain observable even when the hosted sweep was absent.
        for status in ("queued", "in_progress", "waiting", "pending", "requested"):
            candidates.extend(pages(path + "&status=" + status, "workflow_runs", token=token))
        runs = list({run["id"]: run for run in candidates}.values())
    failed = False
    try:
        runners = inventory()
    except (EvidenceError, OSError, ValueError, subprocess.SubprocessError) as error:
        print(f"INCOMPLETE collector inventory: {error}")
        runners = {repository: set()}
    for run in runs:
        try:
            age = (dt.datetime.now(dt.timezone.utc) - dt.datetime.fromisoformat(run["created_at"].replace("Z", "+00:00"))).total_seconds()
            # Sweep only overdue live jobs and recent completions; completion events cover old replays.
            if run["status"] != "completed" and age < MAX_AGE:
                continue
            if "workflow_run" not in event and run["status"] == "completed" and age > 2 * MAX_AGE:
                continue
            collect(run, repository, runners, environment)
        except (EvidenceError, OSError, ValueError, KeyError, TypeError, zipfile.BadZipFile, subprocess.SubprocessError) as error:
            print(f'INCOMPLETE collection {run["id"]}: {error}'); failed = True
    return int(failed)

def main():
    try:
        return collect_event(dict(os.environ))
    except (EvidenceError, OSError, ValueError, KeyError, TypeError, zipfile.BadZipFile, subprocess.SubprocessError) as error:
        print(f"INCOMPLETE collector: {error}")
        return 1

if __name__ == "__main__":
    sys.exit(main())

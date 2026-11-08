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

def request(path, binary=False):
    url = path if path.startswith("https://") else API + path
    require(url.startswith(API + "/"), "untrusted API URL")
    req = urllib.request.Request(url, headers={"Authorization": "Bearer " + os.environ["GH_TOKEN"],
                                                "Accept": "application/vnd.github+json"})
    with urllib.request.build_opener(SafeRedirect()).open(req, timeout=60) as response:
        body = response.read(16 * 1024 * 1024 + 1)
    require(len(body) <= 16 * 1024 * 1024, "collector response too large")
    return body if binary else json.loads(body)

def pages(path, key):
    result = []
    for page in range(1, 1001):
        data = request(path + ("&" if "?" in path else "?") + f"per_page=100&page={page}")[key]
        result.extend(data)
        if len(data) < 100:
            return result
    raise EvidenceError("INCOMPLETE: API pagination exceeded")

def stamp():
    return dt.datetime.now(dt.timezone.utc).isoformat()

def selected(base, jobs, run):
    selectors = [j for j in jobs if j["name"] == "select" and j["conclusion"] == "success"]
    if not selectors:
        # No selected execution exists; report absence for both tiers at the trusted event SHA.
        return [(run["head_sha"], tier) for tier in ("daily", "weekly")]
    logs = request(base + f'/jobs/{selectors[0]["id"]}/logs', binary=True).decode()
    matches = re.findall(r"(?m)^(?:\d{4}-\d\d-\d\dT\S+ )?SCHEDULE_SELECTION ([0-9a-f]{40}) (daily|weekly)\s*$", logs)
    require(len(matches) == 1, "INCOMPLETE: missing trusted selection diagnostic")
    return matches

def record_for(run, job, selection, payload, artifact, repository, runners):
    sha, tier = selection
    missing = ("queued-too-long" if job and job["status"] != "completed" else
               job.get("conclusion") if job and job.get("conclusion") in {"cancelled", "skipped"} else
               "absent" if not job or payload is None else None)
    identity = dict(sha=sha, tier=tier, repository=repository,
                    attempt=f'{run["id"]}/{run["run_attempt"]}', occurrence=f'{run["event"]}/{run["id"]}',
                    runner_name=job.get("runner_name") if job else None,
                    runner_environment="self-hosted" if job and job.get("runner_name") else None)
    if missing:
        record = dict(identity, status="MISSING", conclusion=missing, gates=[], artifact=None,
                      gate_revision=None, started=run["created_at"], completed=(job.get("completed_at") if job else None) or run["created_at"])
    else:
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
    validate(record, sha, tier, runners, repository)
    return record

def collect(run, repository, runners):
    require(run["head_branch"] == "main" and run["event"] in {"schedule", "workflow_dispatch"}, "untrusted scheduled run")
    base = f"/repos/{repository}/actions"
    jobs = pages(base + f'/runs/{run["id"]}/attempts/{run["run_attempt"]}/jobs', "jobs")
    campaigns = [j for j in jobs if j["name"] == "campaign"]
    require(len(campaigns) <= 1, "ambiguous campaign job")
    job = campaigns[0] if campaigns else None
    artifacts = pages(base + f'/runs/{run["id"]}/artifacts', "artifacts") if job and job["status"] == "completed" else []
    for selection in selected(base, jobs, run):
        name = f'scheduled-{selection[1]}-{run["id"]}-{run["run_attempt"]}'
        found = [a for a in artifacts if a["name"] == name and not a["expired"]]
        require(len(found) <= 1, "ambiguous result artifact")
        payload = None; url = None
        if found:
            data = request(base + f'/artifacts/{found[0]["id"]}/zip', binary=True)
            with zipfile.ZipFile(io.BytesIO(data)) as archive:
                info = archive.getinfo("index.json")
                require(info.file_size <= 1024 * 1024, "index too large")
                payload = json.loads(archive.read(info))
            url = f'https://github.com/{repository}/actions/runs/{run["id"]}/artifacts/{found[0]["id"]}'
        publish(record_for(run, job, selection, payload, url, repository, runners), runners)

def main():
    repository = os.environ["GITHUB_REPOSITORY"]
    event = json.load(open(os.environ["GITHUB_EVENT_PATH"]))
    runs = ([event["workflow_run"]] if "workflow_run" in event else
            pages(f"/repos/{repository}/actions/workflows/gate_schedule.yml/runs?branch=main", "workflow_runs"))
    failed = False
    runners = inventory()
    for run in runs:
        try:
            age = (dt.datetime.now(dt.timezone.utc) - dt.datetime.fromisoformat(run["created_at"].replace("Z", "+00:00"))).total_seconds()
            # Sweep only overdue live jobs and recent completions; completion events cover old replays.
            if run["status"] != "completed" and age < MAX_AGE:
                continue
            if "workflow_run" not in event and run["status"] == "completed" and age > 2 * MAX_AGE:
                continue
            collect(run, repository, runners)
        except (EvidenceError, OSError, ValueError, KeyError, TypeError, subprocess.SubprocessError) as error:
            print(f'INCOMPLETE collection {run["id"]}: {error}'); failed = True
    return int(failed)

if __name__ == "__main__":
    sys.exit(main())

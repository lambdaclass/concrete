"""Single registration authority for shell and standalone Lean differential workers."""
import fcntl
import hashlib
import json
import os
import tempfile
import pathlib
import sys
import uuid


def register(directory, leg, seed, index):
    directory = pathlib.Path(directory)
    directory.mkdir(parents=True, exist_ok=True)
    safe = "seed_" + hashlib.sha256(seed.encode("utf-8")).hexdigest()
    values = {"leg": leg, "seed": seed, "index": str(int(index)),
              "safe-seed.txt": safe,
              "regression-name.txt": f"regression_{leg}_{safe}_{int(index)}"}
    for name, value in values.items():
        path = directory / name
        if path.exists() and path.read_text() != value + "\n":
            raise ValueError(f"FAIL candidate registration conflict: {path}")
    for name, value in values.items():
        (directory / name).write_text(value + "\n")
    owner = directory / "execution-id"
    registration = directory / "registration-path"
    location = str(directory.resolve()) + "\n"
    if not registration.exists() or registration.read_text() != location:
        # Copied plants/baselines are executions in their own right.
        for name in ("execution-id", "execution-path", "bundle-path"):
            (directory / name).unlink(missing_ok=True)
        registration.write_text(location)
    if not owner.exists():
        owner.write_text(uuid.uuid4().hex + "\n")


def candidate_claim(directory):
    claim = {name: (directory / name).read_text() for name in ("leg", "seed", "index")}
    claim["scenario"] = ((directory / "control-replay").read_text()
                         if (directory / "control-replay").exists() else "")
    for name in ("program.lean.txt", "program.con"):
        if (directory / name).exists():
            claim[name] = hashlib.sha256((directory / name).read_bytes()).hexdigest()
    return claim


def allocate(candidate, directory):
    """Validate the deterministic candidate claim; allocate a private execution snapshot.

    Source may not yet exist after a generator exit. Each published source claim is
    monotone: missing content may be learned later, but existing content cannot change.
    Observations and diagnostics belong to executions, never candidate identity.
    """
    candidate, directory = pathlib.Path(candidate), pathlib.Path(directory)
    candidate.mkdir(parents=True, exist_ok=True)
    claim = candidate_claim(directory)
    # Serialize only the short claim transaction, including concurrent first writers.
    with (candidate / ".identity-lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        identity = candidate / "candidate.json"
        if identity.exists():
            previous = json.loads(identity.read_text())
        elif (candidate / "leg").exists():
            # Pre-execution-layout bundles remain intact and participate in the
            # same collision contract when replayed after upgrading the harness.
            previous = candidate_claim(candidate)
        else:
            previous = {}
        if any(key in previous and previous[key] != value for key, value in claim.items()):
            raise ValueError(f"FAIL regression candidate identity collision: {candidate}")
        previous.update(claim)
        fd, name = tempfile.mkstemp(dir=candidate, prefix=".identity-")
        with os.fdopen(fd, "w") as out:
            json.dump(previous, out, sort_keys=True)
            out.write("\n")
        os.replace(name, identity)
    execution = candidate / "executions" / (directory / "execution-id").read_text().strip()
    # First allocation must claim a fresh directory atomically. Later publications
    # may append snapshots only to the execution allocated by this registration.
    allocation = directory / "execution-path"
    if allocation.exists() and allocation.read_text() != str(execution) + "\n":
        # A compound plant changes the scenario and therefore the candidate root.
        allocation.unlink()
        execution = candidate / "executions" / uuid.uuid4().hex
        (directory / "execution-id").write_text(execution.name + "\n")
    if not allocation.exists():
        execution.parent.mkdir(parents=True, exist_ok=True)
        try:
            execution.mkdir()
        except FileExistsError:
            # Even an accidental random-ID collision never shares an execution.
            execution = pathlib.Path(tempfile.mkdtemp(prefix="execution-", dir=execution.parent))
            (directory / "execution-id").write_text(execution.name + "\n")
        allocation.write_text(str(execution) + "\n")
    snapshots = execution / "snapshots"
    snapshots.mkdir(exist_ok=True)
    print(tempfile.mkdtemp(prefix="snapshot-", dir=snapshots))


def digest(directory):
    """Fingerprint the whole published snapshot, including artifact names and bytes."""
    directory = pathlib.Path(directory)
    result = hashlib.sha256()
    for path in sorted(directory.rglob("*")):
        if path.is_file():
            result.update(str(path.relative_to(directory)).encode())
            result.update(b"\0")
            content = hashlib.sha256()
            with path.open("rb") as source:
                for chunk in iter(lambda: source.read(1024 * 1024), b""):
                    content.update(chunk)
            result.update(content.digest())
    return result.hexdigest()


if __name__ == "__main__":
    try:
        if sys.argv[1] == "digest":
            print(digest(sys.argv[2]))
        elif sys.argv[1] == "allocate":
            allocate(*sys.argv[2:])
        else:
            register(*sys.argv[1:])
    except ValueError as error:
        sys.exit(str(error))

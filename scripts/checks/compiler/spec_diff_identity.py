"""Single registration authority for shell and standalone Lean differential workers."""
import hashlib
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
    owner = directory / "bundle-owner"
    if not owner.exists():
        owner.write_text(uuid.uuid4().hex + "\n")


if __name__ == "__main__":
    try:
        register(*sys.argv[1:])
    except ValueError as error:
        sys.exit(str(error))

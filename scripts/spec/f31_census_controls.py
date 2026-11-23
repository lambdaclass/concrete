"""Controls for command boundaries, tracked inputs and whole-inventory publication."""
from concurrent.futures import ThreadPoolExecutor
from hashlib import sha256
import json
import os
from pathlib import Path
import shutil
import tempfile
from threading import Barrier
from unittest.mock import patch

from f31_census import generate, publish, run, snapshot, write_jsonl


def command_control(root, work):
    repo = work / "frontend"
    repo.mkdir()
    run(["git", "init", "-q"], repo)
    shutil.copyfile(root / "lean-toolchain", repo / "lean-toolchain")
    (repo / "lakefile.toml").write_text(
        'name = "f31-control"\n[[lean_lib]]\nname = "ConcreteSpec"\n'
        '[[lean_lib]]\nname = "Concrete"\n', encoding="utf-8")
    (repo / "Concrete.lean").write_text(
        "import ConcreteSpec\ndef f31Consumer : F31Alias := Nat.zero\n", encoding="utf-8")
    exporter = Path("scripts/spec/f31_census.lean")
    (repo / exporter).parent.mkdir(parents=True)
    shutil.copyfile(root / exporter, repo / exporter)
    for filename, typ in (("Legacy", "F31Alias"), ("Clean", "Nat")):
        (repo / "scripts/spec" / f"{filename}.lean").write_text(
            f"import ConcreteSpec\ndef probeValue : {typ} := Nat.zero\n#check probeValue\n",
            encoding="utf-8")
    source = repo / "ConcreteSpec.lean"
    source.write_text("""import Lean
open Lean Elab Command
namespace F31Scope
scoped elab "#f31_scoped" : command => pure ()
end F31Scope
open scoped F31Scope in
with_weak_namespace F31Scope
  #f31_scoped
elab (name := F31Named.quot) "#f31_named" : command => pure ()
#f31_named
def f31Quotation : CommandElabM (TSyntax `command) := `(command| #f31_named)
def ConcreteSpec.IntTy := Nat
abbrev F31Alias := ConcreteSpec.IntTy
open scoped F31Scope in
with_weak_namespace F31Scope
  #check F31Alias
example : F31Alias := Nat.zero
""", encoding="utf-8")
    run(["git", "add", "--", "."], repo)
    destination = work / "frontend-inventory"
    publish(destination, lambda source, result: generate(repo, source, result))
    rows = [json.loads(line) for line in (destination / "lean.jsonl").read_text().splitlines()]
    control_rows = [row for row in rows if row["file"] == "ConcreteSpec.lean"]
    commands = [(row["line"], row["kind"]) for row in control_rows if row["record"] == "command"]
    assert commands == [
        (2, "Lean.Parser.Command.open"), (3, "Lean.Parser.Command.namespace"),
        (4, "Lean.Parser.Command.elab"), (5, "Lean.Parser.Command.end"),
        (6, "Lean.Parser.Command.in"), (7, "Lean.Parser.Command.withWeakNamespace"),
        (8, "F31Scope.«command#f31_scoped»"), (9, "Lean.Parser.Command.elab"),
        (10, "F31Named.quot"), (11, "Lean.Parser.Command.declaration"),
        (12, "Lean.Parser.Command.declaration"), (13, "Lean.Parser.Command.declaration"),
        (14, "Lean.Parser.Command.in"), (15, "Lean.Parser.Command.withWeakNamespace"),
        (16, "Lean.Parser.Command.check"), (17, "Lean.Parser.Command.declaration"),
    ], commands
    alias = next(row for row in rows if row.get("name") == "F31Alias")
    assert alias["direct"] == ["ConcreteSpec.IntTy"], alias
    for line in (16, 17):
        row = next(row for row in control_rows if row["record"] == "command" and row["line"] == line)
        assert "F31Alias" in row["via"], row
    consumer = next(row for row in rows if row.get("name") == "f31Consumer")
    assert consumer["via"] == ["F31Alias"], consumer
    for filename, uses in (("Legacy", ["probeValue"]), ("Clean", [])):
        probe = next(row for row in rows if row["file"] == f"scripts/spec/{filename}.lean"
                     and row.get("kind") == "Lean.Parser.Command.check")
        assert probe["via"] == uses, probe
    print("ISS-0092 and ISS-0093: scoped descendant and .quot invocation counted, quotation excluded")


def tracked_control(work):
    repo = work / "repo"
    repo.mkdir()
    run(["git", "init", "-q"], repo)
    paths = ("scripts/checks/compiler/draft.sh", "scripts/proofs/draft.lean",
             "tests/draft.con", "tests/golden/draft.txt", "Concrete/draft.lean")
    for path in paths:
        file = repo / path
        file.parent.mkdir(parents=True, exist_ok=True)
        file.write_text("draft\n", encoding="utf-8")
    assert snapshot(repo, work / "untracked") == []
    run(["git", "add", "--", *paths], repo)
    rows = snapshot(repo, work / "tracked")
    assert {row["file"] for row in rows} == set(paths)
    assert {row["role"] for row in rows} == {"tool", "lean-probe", "test", "golden", "module"}
    print("ISS-0096: draft files are ignored until staged")


def publication_control(work):
    ready = Barrier(2)
    destination = work / "published"

    def contender(label):
        def produce(source, result):
            assert source.parent == result.parent
            write_jsonl(result / "lean.jsonl", [{"run": label}])
            ready.wait()
            assert not destination.exists()
            write_jsonl(result / "surface.jsonl", [{"run": label}])
            ready.wait()

        try:
            publish(destination, produce)
        except FileExistsError:
            return None
        return label

    with ThreadPoolExecutor(max_workers=2) as workers:
        winners = [label for label in workers.map(contender, ["first", "second"]) if label]
    assert len(winners) == 1, winners
    digest_file = destination / "sha256.json"
    expected = {"lean.jsonl", "surface.jsonl"}
    assert {p.name for p in destination.iterdir()} == expected | {digest_file.name}
    digests = json.loads(digest_file.read_text())
    assert set(digests) == expected
    for name in expected:
        contents = (destination / name).read_bytes()
        assert json.loads(contents)["run"] == winners[0]
        assert sha256(contents).hexdigest() == digests[name]
    print("ISS-0096: one competing run publishes every payload and its digest")


def git_environment_control(work):
    caller = work / "caller"
    caller.mkdir()
    run(["git", "init", "-q"], caller)
    (caller / "Concrete.lean").write_text("-- caller\n", encoding="utf-8")
    run(["git", "add", "--", "Concrete.lean"], caller)
    git_dir = caller / ".git"
    alternate_index = work / "caller-index"
    shutil.copyfile(git_dir / "index", alternate_index)
    caller_files = {path: path.read_bytes() for path in git_dir.rglob("*") if path.is_file()}
    index_contents = alternate_index.read_bytes()

    repo = work / "isolated"
    repo.mkdir()
    with patch.dict(os.environ, {"GIT_DIR": str(git_dir), "GIT_INDEX_FILE": str(alternate_index)}):
        run(["git", "init", "-q"], repo)
        assert (repo / ".git" / "HEAD").is_file()
        (repo / "ConcreteSpec.lean").write_text("-- isolated\n", encoding="utf-8")
        run(["git", "add", "--", "ConcreteSpec.lean"], repo)
        rows = snapshot(repo, work / "isolated-snapshot")
        assert [row["file"] for row in rows] == ["ConcreteSpec.lean"], rows

    assert {path: path.read_bytes() for path in git_dir.rglob("*") if path.is_file()} == caller_files
    assert alternate_index.read_bytes() == index_contents
    print("Inherited Git paths leave the caller repository and index untouched")


if __name__ == "__main__":
    with tempfile.TemporaryDirectory(prefix="f31-controls-") as scratch:
        work = Path(scratch)
        command_control(Path(__file__).resolve().parents[2], work)
        tracked_control(work)
        git_environment_control(work)
        publication_control(work)

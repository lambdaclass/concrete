"""Private source elaboration and the shared environment consumer for N2.

No inventory is reconstructed here: theorems.lean is the sole population authority.
The lexical rewrite only retains anonymous examples, exactly as for the trust gate.
"""
import argparse
import contextlib
import io
import os
from pathlib import Path
import subprocess
import sys
import tempfile

from lean_lex import examples, lex

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
BOUND = ROOT / "scripts/checks/lib/bounded.sh"


def command(args, cwd, env, seconds=120):
    return subprocess.run(["bash", str(BOUND), str(seconds), *map(str, args)],
                          cwd=cwd, env=env, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT)


def run(source, scope, fixture=None, budget=64, inventory=False, *, driver_text=None, proof_mode=None):
    with tempfile.TemporaryDirectory(prefix="vacuity-") as temp:
        work = Path(temp)
        if fixture is None:
            paths = ["ConcreteSpec.lean", *sorted(str(p.relative_to(source))
                     for p in (source / "ConcreteSpec").rglob("*.lean"))]
            with contextlib.redirect_stdout(io.StringIO()):
                examples(source, work, paths)
        else:
            (work / "ConcreteSpec.lean").write_text(fixture)
            with contextlib.redirect_stdout(io.StringIO()):
                examples(work, work, ["ConcreteSpec.lean"])
        (work / "ROADMAP.md").write_text((source / "ROADMAP.md").read_text())
        env = dict(os.environ, LEAN_PATH=str(work), VACUITY_SCOPE=scope,
                   VACUITY_BUDGET=str(budget), PROOF_MODE=proof_mode or ("theorems" if inventory else "vacuity"))
        visited = set()

        def build(module):
            if module in visited or not module.startswith("ConcreteSpec"):
                return
            visited.add(module)
            path = work / (module.replace(".", "/") + ".lean")
            # Imports are code tokens, never comments or strings. All header tokens until the
            # first non-import line belong to import commands, including continued imports.
            header = True
            importing = False
            for line in lex(path.read_text(), literals=False).splitlines():
                words = line.split()
                if not words:
                    continue
                if words[0] == "import":
                    importing = True
                    words = words[1:]
                elif not importing or not all(w.startswith("ConcreteSpec") for w in words):
                    header = False
                if not header:
                    break
                for dependency in words:
                    build(dependency)
            result = command(["lean", "-DautoImplicit=false", "-DwarningAsError=true",
                              "-Dlinter.defProp=false", "-o", path.with_suffix(".olean"), path], work, env)
            if result.returncode:
                raise RuntimeError(f"INCOMPLETE elaboration {module}\n{result.stdout}")

        build(scope or "ConcreteSpec")
        driver = driver_text if driver_text is not None else (HERE / "theorems.lean").read_text()
        if scope:
            driver = driver.replace("import ConcreteSpec\n", f"import {scope}\n")
        if not inventory:
            consumer = (HERE / "vacuity.lean").read_text().replace("import Lean\n", "", 1)
            driver += "\n" + consumer + """
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  try
    let findings ← Vacuity.check (populationConstants (← getEnv))
    for f in findings do
      IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
    if findings.any (·.status == "INCOMPLETE") then IO.println "N2 INCOMPLETE"
    else if findings.any (·.status == "FAIL") then IO.println "N2 FAIL"
    else IO.println "N2 PASS"
  catch e =>
    IO.println s!"INCOMPLETE meta-analysis {← e.toMessageData.toString}"
    IO.println "N2 INCOMPLETE"
"""
        path = work / "audit.lean"
        path.write_text(driver)
        result = command(["lean", path], work, env)
        if result.returncode:
            return 2, "INCOMPLETE enumerator\n" + result.stdout
        output = result.stdout
        if inventory:
            return 0, output
        if "N2 INCOMPLETE\n" in output:
            return 2, output
        if "N2 FAIL\n" in output:
            return 1, output
        if "N2 PASS\n" in output:
            return 0, output
        return 2, "INCOMPLETE missing-verdict\n" + output


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--scope", default="")
    parser.add_argument("--controls", action="store_true")
    args = parser.parse_args()
    if args.scope and not (args.source / (args.scope.replace(".", "/") + ".lean")).is_file():
        print("FAIL scope-not-found " + args.scope)
        return 1
    if args.controls:
        from vacuity_controls import controls
        return controls(run, args.source)
    print("N2 SCOPE " + (args.scope or "ALL ConcreteSpec authored theorems"))
    code, output = run(args.source, args.scope)
    print(output, end="")
    return code


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, RuntimeError) as error:
        print("INCOMPLETE " + str(error))
        sys.exit(2)

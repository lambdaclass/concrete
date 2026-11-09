"""Private source elaboration and the shared environment consumer for N2.

No inventory is reconstructed here: theorems.lean is the sole population authority.
The lexical rewrite only retains anonymous examples, exactly as for the trust gate.
"""
import argparse
import contextlib
import io
import functools
import hashlib
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import time

from lean_lex import examples, lex, LexError, ExampleProvenanceError

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
BOUND = ROOT / "scripts/checks/lib/bounded.sh"


def command(args, cwd, env, seconds=120):
    return subprocess.run(["bash", str(BOUND), str(seconds), *map(str, args)],
                          cwd=cwd, env=env, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT)


@contextlib.contextmanager
def compiled_consumer(*, consumer_text=None):
    """Compile the generic consumer once in a fresh private control-run directory.

    There is no persistent cache and no specification import: each fixture still
    compiles its own source and supplies its own environment population at runtime.
    """
    with tempfile.TemporaryDirectory(prefix="vacuity-meta-") as temp:
        work = Path(temp)
        population = (HERE / "theorems.lean").read_text().replace("import ConcreteSpec\n", "", 1)
        consumer = (consumer_text if consumer_text is not None else (HERE / "vacuity.lean").read_text()).replace("import Lean\n", "", 1)
        path = work / "N2Meta.lean"
        path.write_text(population + "\n" + consumer)
        env = dict(os.environ, LEAN_PATH=str(work), PROOF_MODE="vacuity")
        result = command(["lean", "-o", path.with_suffix(".olean"), path], work, env)
        if result.returncode:
            raise RuntimeError("INCOMPLETE checker-elaboration\n" + result.stdout)
        yield work


def _run(source, scope, fixture=None, budget=4096, inventory=False, *, driver_text=None, proof_mode=None, meta_path=None, audit_seconds=120, whole_run_cap=None):
    with tempfile.TemporaryDirectory(prefix="vacuity-") as temp:
        work = Path(temp)
        if fixture is None:
            paths = ["ConcreteSpec.lean", *sorted(str(p.relative_to(source))
                     for p in (source / "ConcreteSpec").rglob("*.lean"))]
            with contextlib.redirect_stdout(io.StringIO()):
                examples(source, work, paths)
        elif isinstance(fixture, dict):
            for relative, text in fixture.items():
                path = work / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text)
            with contextlib.redirect_stdout(io.StringIO()):
                examples(work, work, sorted(fixture))
        else:
            (work / "ConcreteSpec.lean").write_text(fixture)
            with contextlib.redirect_stdout(io.StringIO()):
                examples(work, work, ["ConcreteSpec.lean"])
        (work / "ROADMAP.md").write_text((source / "ROADMAP.md").read_text())
        env = dict(os.environ, LEAN_PATH=os.pathsep.join(map(str, [work, meta_path] if meta_path else [work])), VACUITY_SCOPE=scope,
                   VACUITY_TRACE=str(work / "audit-trace.tsv"),
                   VACUITY_BUDGET=str(budget), PROOF_MODE=proof_mode or ("theorems" if inventory else "vacuity"))
        if whole_run_cap is not None:
            env["VACUITY_WHOLE_RUN_CAP"] = str(whole_run_cap)
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

        scopes = scope.split(",") if scope else ["ConcreteSpec"]
        for module in scopes:
            build(module)
        if not scope and (fixture is None or isinstance(fixture, dict)):
            disk = {str(p.relative_to(work).with_suffix("")).replace("/", ".")
                    for p in (work / "ConcreteSpec").rglob("*.lean")}
            gap = sorted(disk - visited)
            if gap:
                return 1, "FAIL closure-gap " + ",".join(gap) + "\nN2 FAIL\n"
        driver = driver_text if driver_text is not None else (HERE / "theorems.lean").read_text()
        if scope:
            driver = driver.replace("import ConcreteSpec\n", "".join(f"import {module}\n" for module in scopes))
        if not inventory:
            if meta_path is not None and driver_text is None:
                driver = "import N2Meta\n" + "".join(f"import {module}\n" for module in scopes) + "open Lean Meta\n"
            else:
                consumer = (HERE / "vacuity.lean").read_text().replace("import Lean\n", "", 1)
                driver += "\n" + consumer
            driver += """
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  try
    checkRowSafety (← getEnv)
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
        started = time.monotonic()
        result = command(["lean", path], work, env, seconds=audit_seconds)
        wall = f"N2 WALL audit-seconds={audit_seconds} retry-seconds={audit_seconds * 2} elapsed-seconds={time.monotonic() - started:.3f}\n"
        result.stdout = wall + result.stdout
        if result.returncode:
            trace = work / "audit-trace.tsv"
            tail = "\n".join(trace.read_text().splitlines()[-20:]) if trace.exists() else "no audit progress"
            return 2, "INCOMPLETE enumerator\n" + result.stdout + "\nN2 LAST PROGRESS\n" + tail + "\n"
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


def run(*args, **kwargs):
    try:
        return _run(*args, **kwargs)
    except ExampleProvenanceError as error:
        return 1, "FAIL example-provenance [anonymous] " + str(error) + "\nN2 FAIL\n"
    except LexError as error:
        return 2, "INCOMPLETE lexical-error [anonymous] " + str(error) + "\nN2 INCOMPLETE\n"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, default=ROOT)
    parser.add_argument("--scope", action="append", default=[])
    parser.add_argument("--controls", action="store_true")
    parser.add_argument("--audit-seconds", type=int, default=120, help="wall watchdog per audit attempt; retry uses twice this bound")
    parser.add_argument("--whole-run-cap", type=int, help="lower the frozen population reservation cap (work units)")
    args = parser.parse_args()
    if args.audit_seconds <= 0 or (args.whole_run_cap is not None and args.whole_run_cap < 0):
        parser.error("audit seconds must be positive and the reservation cap nonnegative")
    args.scope = ",".join(args.scope)
    for module in filter(None, args.scope.split(",")):
        if not (args.source / (module.replace(".", "/") + ".lean")).is_file():
            print("FAIL scope-not-found " + module)
            return 1
    if args.controls:
        from vacuity_controls import controls
        with compiled_consumer() as meta_path:
            result = controls(functools.partial(run, meta_path=meta_path), args.source)
        if result:
            return result
        from vacuity_reporting import main as reporting_controls
        return reporting_controls()
    print("N2 SCOPE " + (args.scope or "ALL ConcreteSpec authored theorems"))
    print("N2 CHECKER " + hashlib.sha256((HERE / "vacuity.lean").read_bytes()).hexdigest())
    paths = ([args.source / (module.replace(".", "/") + ".lean")
              for module in args.scope.split(",")] if args.scope else
             [args.source / "ConcreteSpec.lean", *sorted((args.source / "ConcreteSpec").rglob("*.lean"))])
    for path in paths:
        print(f"N2 SOURCE-FILE {path.relative_to(args.source)} {hashlib.sha256(path.read_bytes()).hexdigest()}")
    with compiled_consumer() as meta_path:
        code, output = run(args.source, args.scope, meta_path=meta_path, audit_seconds=args.audit_seconds, whole_run_cap=args.whole_run_cap)
    print(output, end="")
    return code


if __name__ == "__main__":
    try:
        sys.exit(main())
    except LexError as error:
        print("INCOMPLETE lexical-error " + str(error))
        sys.exit(2)
    except (OSError, RuntimeError) as error:
        print("INCOMPLETE " + str(error))
        sys.exit(2)

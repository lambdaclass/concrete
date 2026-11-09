"""Light exact-source helper probes; not a full-module or promotion audit."""
import argparse
import functools
import io
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile

from vacuity_driver import ROOT, HERE, command, compiled_consumer, run

CASES = {
    "c1": ("ccca4abaa", "Exec", [
        "_private.ConcreteSpec.Proofs.Exec.0.ConcreteSpec.Core.Exec.CleanupEvidence.logSegment",
        "ConcreteSpec.Core.Exec.boundary_of_performed"]),
    "c2": ("9cb0ef7fe", "Const", [
        "_private.ConcreteSpec.Proofs.Const.0.ConcreteSpec.Core.acyclic_of_dependencyOrder"]),
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("case", choices=[*CASES, "controls"])
    parser.add_argument("--baseline", action="store_true")
    args = parser.parse_args()
    if args.case == "controls":
        if args.baseline:
            parser.error("--baseline is only for exact-source helper probes")
        from vacuity_controls import controls
        labels = set("""joint-branch-decide-repro joint-hypothetical-positive
            joint-supplied-negative joint-open-supplied-negative
            reachable-open-impossible-negative reachable-joint-helper-negative
            false-discharge absurd-discharge nomatch-discharge empty-elimination
            pempty-elimination helper-nomatch helper-false false-cases
            open-helper-false open-helper-empty open-helper-pempty
            hypothetical-false-positive hypothetical-empty-positive
            hypothetical-pempty-positive decide-hypothesis-positive
            nested-decide-discharge-1 nested-decide-discharge-4
            nested-decide-discharge-12 unreachable-closed-supplier-positive
            context-cache-supplied-negative data-subtype-discharge
            data-psigma-discharge data-pprod-discharge
            finite-collection-decide-positive finite-collection-simp-positive
            finite-collection-cases-positive""".split())
        with compiled_consumer() as meta:
            return controls(functools.partial(run, meta_path=meta), ROOT, labels=labels)
    commit, module, names = CASES[args.case]
    archive = subprocess.run(["bash", str(ROOT / "scripts/checks/lib/bounded.sh"), "30",
        "git", "archive", commit, "ConcreteSpec", "ConcreteSpec.lean", "ROADMAP.md"],
        cwd=ROOT, stdout=subprocess.PIPE, check=True)
    driver = "import ConcreteSpec\nimport N2Meta\nopen Lean Meta\n"
    driver += "set_option maxHeartbeats 2000000 in\n#eval show MetaM Unit from do\n"
    for name in names:
        driver += f'  let some name := ((← getEnv).constants.toList.map Prod.fst).find? (fun n => toString n == "{name}") | throwError "missing-name"\n'
        driver += "  let ci ← getConstInfo name\n"
        driver += '  let some proof := ci.value? (allowOpaque := true) | throwError "missing-helper"\n'
        driver += "  let (bad, _) ← (Vacuity.falseDischarge proof).run {}\n"
        driver += '  IO.println s!"HELPER {name} falseDischarge={bad}"\n'
        driver += f'  unless bad == {str(args.baseline).lower()} do throwError "helper-verdict"\n'
    with tempfile.TemporaryDirectory(prefix="n2-r5-source-") as temp:
        with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as tree:
            tree.extractall(temp, filter="data")
        if args.baseline:
            with tempfile.TemporaryDirectory(prefix="n2-r5-baseline-") as meta:
                meta = Path(meta)
                original = subprocess.run(["bash", str(ROOT / "scripts/checks/lib/bounded.sh"),
                    "20", "git", "show", "550fab85e:scripts/proofs/vacuity.lean"],
                    cwd=ROOT, stdout=subprocess.PIPE, text=True, check=True).stdout
                # Own diagnostic: print both the eliminator and its entire local context.
                original = original.replace("if ← refutableType (← inferType e) then return true",
                    'if ← refutableType (← inferType e) then\n    IO.println s!"TRACE refuted {← ppExpr e}"\n'
                    '    for l in (← getLCtx) do IO.println s!"CTX {l.userName}: {← ppExpr l.type}"\n'
                    '    return true')
                original = original.replace("if r.rules.isEmpty then return true",
                    'if r.rules.isEmpty then\n        IO.println s!"TRACE empty-rec {← ppExpr e}"\n'
                    '        for l in (← getLCtx) do IO.println s!"CTX {l.userName}: {← ppExpr l.type}"\n'
                    '        return true')
                original = original.replace("if matcher.numAlts == 0 then return true",
                    'if matcher.numAlts == 0 then\n        IO.println s!"TRACE empty-matcher {← ppExpr e}"\n'
                    '        for l in (← getLCtx) do IO.println s!"CTX {l.userName}: {← ppExpr l.type}"\n'
                    '        return true')
                original = original.replace("if ← falseDischarge reduced then return true",
                    'if ← falseDischarge reduced then\n            IO.println s!"TRACE via {n}"\n            return true')
                path = meta / "N2Meta.lean"
                path.write_text((HERE / "theorems.lean").read_text().replace("import ConcreteSpec\n", "", 1)
                    + "\n" + original.replace("import Lean\n", "", 1))
                result = command(["lean", "-o", path.with_suffix(".olean"), path], meta,
                    dict(os.environ, LEAN_PATH=str(meta)))
                if result.returncode:
                    raise RuntimeError(result.stdout)
                code, output = run(Path(temp), "ConcreteSpec.Proofs." + module,
                    inventory=True, driver_text=driver, meta_path=meta)
        else:
            with compiled_consumer() as meta:
                code, output = run(Path(temp), "ConcreteSpec.Proofs." + module,
                    inventory=True, driver_text=driver, meta_path=meta)
    print(output, end="")
    expected = [f"HELPER {name} falseDischarge={str(args.baseline).lower()}" for name in names]
    if code != 0 or [line for line in output.splitlines() if line.startswith("HELPER ")] != expected:
        return 1
    print("PASS exact-source helper probes " + args.case)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

"""Seven bounded single-contract probes; no scope census or source mutation."""
import io
from contextlib import nullcontext
import subprocess
import tarfile
import tempfile
from pathlib import Path

from vacuity_driver import ROOT, HERE, compiled_consumer, run

SOURCE = "6efdce6136752d51fc74265d61278d90937f24cd"
DRIVER = r'''import N2Meta
import ConcreteSpec.Proofs.Own
open Lean Meta Vacuity
DECLARATIONS
set_option maxRecDepth 4096 in
set_option maxHeartbeats 2000000 in
#eval show MetaM Unit from do
  let env ← getEnv
  let all := env.constants.map₁.toList ++ env.constants.map₂.toList
  for name in [`Confirm.inferredOk, `Confirm.restatedOk, `Confirm.inferredTrap,
      `Confirm.restatedTrap, `Confirm.changedStoreOk, `Confirm.unselectedRestatedOk,
      `Confirm.unselectedChangedStoreOk] do
    let ci ← getConstInfo name
    let some value := ci.value? | throwError "typed-control-body"
    let .const law _ := (cleanSpine value).getAppFn' | throwError "typed-control-head"
    let lawInfo ← getConstInfo law
    let lawDoc := (← findDocString? env law).getD ""
    let job : AnalysisJob := {
      name, stage := "instance", law := some law, caseName := "typed"
      action := do
        discard <| checkInstance env all #[⟨law, lawInfo, lawDoc⟩]
          { name, info := ci, doc := "", law, key := "typed" } #[]
    }
    let start ← IO.monoMsNow
    let (_, state) ← (isolated (← readThe Core.Context) job).run {}
    IO.println s!"TYPED {name} elapsed-ms={(← IO.monoMsNow) - start}"
    for f in state.findings do IO.println s!"{f.status} {f.code} {f.name} {f.detail}"
'''


def main(meta=None):
    archive = subprocess.run(["bash", str(ROOT / "scripts/checks/lib/bounded.sh"), "30",
        "git", "archive", SOURCE, "ConcreteSpec", "ConcreteSpec.lean", "ROADMAP.md"],
        cwd=ROOT, stdout=subprocess.PIPE, check=True)
    driver = DRIVER.replace("DECLARATIONS", (HERE / "fixtures/N2-Typed-Contracts.lean").read_text())
    with tempfile.TemporaryDirectory(prefix="n2-typed-") as temp:
        with tarfile.open(fileobj=io.BytesIO(archive.stdout)) as tree:
            tree.extractall(temp, filter="data")
        with (compiled_consumer() if meta is None else nullcontext(meta)) as meta:
            code, output = run(Path(temp), "ConcreteSpec.Proofs.Own", inventory=True,
                               driver_text=driver, meta_path=meta)
    print(output, end="", flush=True)
    assert code == 0, (code, output)
    findings = [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))]
    assert findings == [f"FAIL instance-application Confirm.{name} law=ConcreteSpec.Core.Own.evalArgs_ok_erasure case=typed" for name in ["changedStoreOk", "unselectedChangedStoreOk"]], findings
    for name in ["inferredOk", "restatedOk", "inferredTrap", "restatedTrap", "unselectedRestatedOk"]:
        assert any(r.startswith(f"NOTE analysis-result Confirm.{name} ") and "result=PASS " in r
                   for r in output.splitlines()), output
    print("Typed contract controls: 7 PASS, 0 FAIL", flush=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

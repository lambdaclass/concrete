"""Light semantic controls; run inside nix develop after building Concrete.Premises."""
from pathlib import Path
import os
import subprocess
import tempfile

DRIVER = "scripts/checks/compiler/slice0_examples.lean"
def lean(path, driver=False):
    return subprocess.run(["lake", "env", "lean", *(["--run", DRIVER] if driver else []), str(path)],
        env={**os.environ, "LEAN_NUM_THREADS": "2"}, text=True, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, timeout=60)

def require(result, ok, diagnostic):
    assert (result.returncode == 0) == ok and diagnostic in result.stdout, result.stdout

with tempfile.TemporaryDirectory() as d:
    d = Path(d)
    p = d / "examples.lean"
    p.write_text('import Lean\nexample : 1 = 1 := by rfl\n')
    require(lean(p, True), True, '"axioms":[]')
    p.write_text('import Lean\nexample : 2 + 2 = 4 := by decide +native\n')
    require(lean(p, True), False, f'._native.decide.ax_1 in {p}:2')
    p.write_text('import Lean\n#eval (dbg_trace "{-- \"\n() }" ())\nexample : 2 + 2 = 4 := by decide +native\n-- "\n')
    require(lean(p, True), False, f'._native.decide.ax_1 in {p}:4')
    p.write_text('import Lean\npartial def loops : IO Unit := do while true do pure ()\n')
    require(lean(p, True), False, 'FAIL unlisted partial declaration loops')
    p = d / "declarations.lean"
    p.write_text('''import Concrete.Premises
import Lean.Elab.Command
namespace Concrete.Control
 def wf (n : Nat) : Nat := if h : n = 0 then 0 else wf (n - 1) + 1
 termination_by n
 mutual
 def even : Nat → Bool | 0 => true | n + 1 => odd n
 def odd : Nat → Bool | 0 => false | n + 1 => even n
 end
 unsafe def fake._unsafe_rec (n : Nat) := n
 unsafe inductive U where | mk
 def original (n : Nat) := n + 0
 def replacement (n : Nat) := n
 @[csimp] theorem replace : original = replacement := by funext n; simp [original, replacement]
end Concrete.Control
open Lean Elab Command in
#eval show CommandElabM Unit from do
 let env ← getEnv
 for parent in [`Concrete.Control.wf, `Concrete.Control.even, `Concrete.Control.odd] do
   unless env.contains (Compiler.mkUnsafeRecName parent) && Meta.recExt.isTagged env parent do
     throwError "safe recursion fixture did not produce a recorded runtime twin"
 let errors := Concrete.Premises.trustedDeclarationErrors env (some `Concrete.Control)
 for error in errors do IO.println s!"FAIL {error}"
 if errors.any (fun s => (s.splitOn "Concrete.Control.wf").length > 1 ||
     (s.splitOn "Concrete.Control.even").length > 1 || (s.splitOn "Concrete.Control.odd").length > 1) then
   throwError "safe recursion rejected"
 unless errors.contains "unlisted trusted declaration Concrete.Control.fake._unsafe_rec [unsafe]" do
   throwError "hand-written twin escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.U [unsafe]" do
   throwError "unsafe inductive escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.replace [csimp]" do
   throwError "csimp escaped"
 IO.println "ok safe recursion and unsafe controls"
''')
    require(lean(p), True, "ok safe recursion and unsafe controls")
    p = d / "ownership.lean"
    p.write_text('''import Concrete.Premises
import Lean.Elab.Command
@[extern "control"] opaque Concrete.Control.f : Nat → Nat
open Lean Elab Command in
#eval show CommandElabM Unit from do
 let env ← getEnv
 for kind in [Concrete.Premises.ComponentKind.gateTool, .notTrusted, .system, .axiom] do
   let cs := Concrete.Premises.components.map fun c =>
     if c.id == "gate-analysis-tools" then { c with kind := kind, declarations := ["Concrete.Control.f"] } else c
   let errors := Concrete.Premises.trustedDeclarationErrors env (some `Concrete.Control) cs
   unless errors.contains "trusted declaration Concrete.Control.f belongs to non-code component gate-analysis-tools" do
     throwError "non-code owner escaped"
 let cs := Concrete.Premises.components.map fun c =>
   if c.id == "lexer" then { c with declarations := ["Concrete.Control.missing"] } else c
 unless (Concrete.Premises.trustedDeclarationErrors env (some `Concrete.Control) cs).contains
     "stale trusted declaration Concrete.Control.missing in component lexer" do
   throwError "stale declaration escaped"
 IO.println "ok non-code owners and stale declarations rejected"
''')
    require(lean(p), True, "ok non-code owners and stale declarations rejected")
print("PASS focused trusted-base semantic controls")

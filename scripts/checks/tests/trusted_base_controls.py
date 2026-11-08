"""Light semantic controls; run inside nix develop after building Concrete.Premises."""
from pathlib import Path
import os
import re
import subprocess
import tempfile

DRIVER = "scripts/checks/compiler/slice0_examples.lean"
def lean(path, driver=False, extra_env=None):
    env = {**os.environ, "LEAN_NUM_THREADS": "2"}
    if extra_env: env.update(extra_env)
    return subprocess.run(["lake", "env", "lean", *(["--run", DRIVER] if driver else []), str(path)],
        env=env, text=True, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, timeout=60)

def require(result, ok, diagnostic):
    assert (result.returncode == 0) == ok and diagnostic in result.stdout, (result.returncode, result.stdout)

with tempfile.TemporaryDirectory() as d:
    d = Path(d)
    empty = d / "empty.rows"
    empty.write_text("no json rows\n")
    r_empty = subprocess.run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(empty)],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
    require(r_empty, False, "FAIL examples: no example-json rows observed")
    p = d / "examples.lean"
    p.write_text('import Lean\nexample : 1 = 1 := by rfl\n')
    require(lean(p, True), True, '"axioms":[]')
    p.write_text('import Lean\nexample : 2 + 2 = 4 := by decide +native\n')
    r = lean(p, True)
    require(r, False, 'FAIL unlisted example axiom ')
    assert re.search(r'^FAIL unlisted example axiom .*\._native\.decide\.ax_1(?:\._@\.[^ ]+)? in ' + re.escape(str(p)) + r':2$', r.stdout, re.M), r.stdout
    p.write_text('import Lean\n#eval (dbg_trace "{-- \"\n() }" ())\nexample : 2 + 2 = 4 := by decide +native\n-- "\n')
    r = lean(p, True)
    require(r, False, 'FAIL unlisted example axiom ')
    assert re.search(r'^FAIL unlisted example axiom .*\._native\.decide\.ax_1(?:\._@\.[^ ]+)? in ' + re.escape(str(p)) + r':4$', r.stdout, re.M), r.stdout
    p.write_text('import Lean\nexample : IO Unit := do while true do pure ()\n')
    require(lean(p, True), False, 'FAIL unlisted example trusted declaration ')
    no_kw = d / "loop_no_kw.lean"
    benign = d / "benign.lean"
    benign.write_text('import Lean\ndef benignVal : Nat := 1\n')
    for loop_src, expected_decl in [
        ('import Lean\ndef whileOnly : IO Unit := do\n  while true do\n    pure ()\n',
         'FAIL unlisted example trusted declaration whileOnly [partial-loop]'),
        ('import Lean\ndef repeatOnly : IO Unit := do\n  repeat\n    break\n',
         'FAIL unlisted example trusted declaration repeatOnly [partial-loop]'),
    ]:
        assert "example" not in loop_src and "partial" not in loop_src
        no_kw.write_text(loop_src)
        require(lean(no_kw, True), False, expected_decl)
        r_multi = subprocess.run(
            ["lake", "env", "lean", "--run", DRIVER, str(benign), str(no_kw)],
            env={**os.environ, "LEAN_NUM_THREADS": "2"},
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=60,
        )
        require(r_multi, False, expected_decl)
    p.write_text('import Lean\npartial def loops : IO Unit := do while true do pure ()\npartial def nonrec : Nat := 0\n')
    require(lean(p, True), False, 'FAIL unlisted partial declaration loops')
    require(lean(p, True, {"CONCRETE_TRUSTED_DECLARATIONS": "lexer:loops,nonrec"}), True, '"name":"loops"')
    require(lean(p, True, {"CONCRETE_TRUSTED_DECLARATIONS": "bir-lcb-adapter:loops,nonrec"}), False,
        'FAIL trusted declaration loops belongs to non-code component bir-lcb-adapter')
    p = d / "declarations.lean"
    p.write_text('''import Concrete.Premises
import Lean.Elab.Command
private def hiddenLoop : IO Unit := do
  while true do
    pure ()
@[extern "c_hidden"] private opaque hiddenExtern : Nat → Nat
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
 def whileDef : IO Unit := do
   while true do
     pure ()
 def repeatDef : IO Unit := do
   repeat
     break
 def nestedLoop : IO Unit := hiddenLoop
 def nestedExtern (n : Nat) : Nat := hiddenExtern n
end Concrete.Control
open Lean Elab Command in
#eval show CommandElabM Unit from do
 let env ← getEnv
 for parent in [`Concrete.Control.wf, `Concrete.Control.even, `Concrete.Control.odd] do
   unless env.contains (Compiler.mkUnsafeRecName parent) && Meta.recExt.isTagged env parent do
     throwError "safe recursion fixture did not produce a recorded runtime twin"
 let errors ← liftCoreM <| Concrete.Premises.trustedDeclarationErrors env (some `Concrete.Control)
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
 unless errors.contains "unlisted trusted declaration Concrete.Control.whileDef [partial-loop]" do
   throwError "whileDef escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.repeatDef [partial-loop]" do
   throwError "repeatDef escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.nestedLoop [partial-loop]" do
   throwError "nestedLoop escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.nestedExtern [extern, opaque]" do
   throwError "nestedExtern escaped"
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
   let errors ← liftCoreM <| Concrete.Premises.trustedDeclarationErrors env (some `Concrete.Control) cs
   unless errors.contains "trusted declaration Concrete.Control.f belongs to non-code component gate-analysis-tools" do
     throwError "non-code owner escaped"
 let cs := Concrete.Premises.components.map fun c =>
   if c.id == "lexer" then { c with declarations := ["Concrete.Control.missing"] } else c
 unless (← liftCoreM <| Concrete.Premises.trustedDeclarationErrors env (some `Concrete.Control) cs).contains
     "stale trusted declaration Concrete.Control.missing in component lexer" do
   throwError "stale declaration escaped"
 IO.println "ok non-code owners and stale declarations rejected"
''')
    require(lean(p), True, "ok non-code owners and stale declarations rejected")
print("PASS focused trusted-base semantic controls")

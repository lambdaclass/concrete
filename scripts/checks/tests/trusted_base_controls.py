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
    for ex_src, extra, kind_tag in [
        ('import Lean\nunsafe example : Nat := 0\n', None, 'unsafe'),
        ('import Lean\nunsafe def unsafeHelper : Nat := 0\nunsafe example : Nat := unsafeHelper\n',
         {"CONCRETE_TRUSTED_DECLARATIONS": "lexer:unsafeHelper"}, 'unsafe'),
        ('import Lean\npartial example : Nat :=\n  let rec loop (n : Nat) : Nat := loop n\n  loop 0\n',
         None, 'partial'),
        ('import Lean\n@[extern "c_example"] example : Nat := 0\n', None, 'extern'),
        ('import Lean\ndef implTarget : Nat := 0\n@[implemented_by implTarget] example : Nat := 1\n',
         None, 'implemented_by'),
        ('import Lean\ndef csimpOrig (n : Nat) := n + 0\ndef csimpRepl (n : Nat) := n\n'
         '@[csimp] example : csimpOrig = csimpRepl := by funext n; simp [csimpOrig, csimpRepl]\n',
         None, 'csimp'),
    ]:
        p.write_text(ex_src)
        r_ex = lean(p, True, extra)
        require(r_ex, False, f'FAIL unlisted example trusted declaration ')
        assert re.search(
            r'^FAIL unlisted example trusted declaration «?⟪example⟫'
            + re.escape(str(p)) + r':\d+:\d+»? \[' + re.escape(kind_tag) + r'\]$',
            r_ex.stdout, re.M
        ), r_ex.stdout
    no_kw = d / "loop_no_kw.lean"
    benign = d / "benign.lean"
    benign.write_text('import Lean\ndef benignVal : Nat := 1\n')
    r_benign = lean(benign, True)
    assert r_benign.returncode == 0 and "example-json" not in r_benign.stdout, (r_benign.returncode, r_benign.stdout)
    benign_rows = d / "benign.rows"
    benign_rows.write_text(r_benign.stdout)
    r_benign_rows = subprocess.run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(benign_rows)],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
    require(r_benign_rows, False, "FAIL examples: no example-json rows observed")
    non_ex_rows = d / "non_example.rows"
    non_ex_rows.write_text('example-json {"name":"benignVal","axioms":[]}\n')
    r_non_ex = subprocess.run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(non_ex_rows)],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
    require(r_non_ex, False, "FAIL examples: invalid observation schema")
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
    p.write_text(
        'import Lean\n'
        'macro "quotedPartial" : command => `(partial def neverCalled (n : Nat) : Nat := neverCalled n)\n'
        'partial def loops : IO Unit := do while true do pure ()\n'
        'partial def recPartial (n : Nat) : Nat := recPartial n\n'
        'example : 1 = 1 := rfl\n'
    )
    require(lean(p, True), False, 'FAIL unlisted example trusted declaration loops [partial-loop]')
    require(lean(p, True, {"CONCRETE_TRUSTED_DECLARATIONS": "lexer:loops"}), False,
        'FAIL unlisted example trusted declaration recPartial [partial]')
    require(lean(p, True, {"CONCRETE_TRUSTED_DECLARATIONS": "lexer:loops,recPartial"}), True, '⟪example⟫')
    require(lean(p, True, {"CONCRETE_TRUSTED_DECLARATIONS": "bir-lcb-adapter:loops,recPartial"}), False,
        'FAIL trusted declaration loops belongs to non-code component bir-lcb-adapter')
    p.write_text(
        'import Lean\n'
        'macro "localHygPartial" : command => `(partial def hygP (n : Nat) : Nat := hygP n)\n'
        'localHygPartial\n'
    )
    r_hyg = lean(p, True)
    require(r_hyg, False, 'FAIL unlisted example trusted declaration ')
    assert re.search(r'^FAIL unlisted example trusted declaration hygP\._@\.[^ ]+ \[partial\]$', r_hyg.stdout, re.M), r_hyg.stdout
    for late_src, extra_env, expected_diag in [
        # Separately applied extern, implemented_by and csimp attributes.
        ('import Lean\ndef late : Nat := 0\nattribute [extern "c_late"] late\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration late [extern]'),
        ('import Lean\ndef implTarget : Nat := 0\ndef lateImpl : Nat := 1\n'
         'attribute [implemented_by implTarget] lateImpl\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration lateImpl [implemented_by]'),
        ('import Lean\ndef csimpOrig (n : Nat) := n + 0\ndef csimpRepl (n : Nat) := n\n'
         'theorem lateCsimp : csimpOrig = csimpRepl := by funext n; simp [csimpOrig, csimpRepl]\n'
         'attribute [csimp] lateCsimp\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration lateCsimp [csimp]'),
        # Macro-generated commands applying extern, implemented_by and csimp attributes.
        ('import Lean\ndef macroExternTarget : Nat := 0\n'
         'macro "applyExtern" : command => `(attribute [extern "c_macro_late"] $(Lean.mkIdent `macroExternTarget))\n'
         'applyExtern\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroExternTarget [extern]'),
        ('import Lean\ndef macroImplRepl : Nat := 0\ndef macroImplTarget : Nat := 1\n'
         'macro "applyImpl" : command => `(attribute [implemented_by $(Lean.mkIdent `macroImplRepl)] $(Lean.mkIdent `macroImplTarget))\n'
         'applyImpl\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroImplTarget [implemented_by]'),
        ('import Lean\ndef mOrig (n : Nat) := n + 0\ndef mRepl (n : Nat) := n\n'
         'theorem macroCsimpThm : mOrig = mRepl := by funext n; simp [mOrig, mRepl]\n'
         'macro "applyCsimp" : command => `(attribute [csimp] $(Lean.mkIdent `macroCsimpThm))\n'
         'applyCsimp\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroCsimpThm [csimp]'),
        # Attributes applied in a later section/namespace (including scoped/local csimp and pre-attribute callers).
        ('import Lean\nnamespace Earlier\ndef nsExtern : Nat := 0\nend Earlier\n'
         'section LaterSec\nnamespace LaterNS\nattribute [extern "c_ns"] Earlier.nsExtern\nend LaterNS\nend LaterSec\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration Earlier.nsExtern [extern]'),
        ('import Lean\nnamespace Earlier\ndef nsImplRepl : Nat := 0\ndef nsImpl : Nat := 1\nend Earlier\n'
         'section LaterSec\nnamespace LaterNS\nattribute [implemented_by Earlier.nsImplRepl] Earlier.nsImpl\nend LaterNS\nend LaterSec\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration Earlier.nsImpl [implemented_by]'),
        ('import Lean\nnamespace Earlier\ndef nsOrig (n : Nat) := n + 0\ndef nsRepl (n : Nat) := n\n'
         'theorem nsCsimp : nsOrig = nsRepl := by funext n; simp [nsOrig, nsRepl]\nend Earlier\n'
         'section LaterSec\nnamespace LaterNS\nattribute [csimp] Earlier.nsCsimp\nend LaterNS\nend LaterSec\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration Earlier.nsCsimp [csimp]'),
        ('import Lean\nnamespace Earlier\ndef sOrig (n : Nat) := n + 0\ndef sRepl (n : Nat) := n\n'
         'theorem scopedCsimp : sOrig = sRepl := by funext n; simp [sOrig, sRepl]\n'
         'attribute [scoped csimp] scopedCsimp\nend Earlier\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration Earlier.scopedCsimp [csimp]'),
        ('import Lean\nsection LocalSec\ndef lOrig (n : Nat) := n + 0\ndef lRepl (n : Nat) := n\n'
         'theorem localCsimp : lOrig = lRepl := by funext n; simp [lOrig, lRepl]\n'
         'attribute [local csimp] localCsimp\nend LocalSec\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration localCsimp [csimp]'),
        ('import Lean\nprivate def privLate : Nat := 0\ndef callerBeforeAttr : Nat := privLate\n'
         'section LaterSec\nattribute [extern "c_priv_late"] privLate\nend LaterSec\n'
         'example : True := True.intro\n',
         {"CONCRETE_TRUSTED_DECLARATIONS": "lexer:privLate"},
         'FAIL unlisted example trusted declaration callerBeforeAttr [extern]'),
    ]:
        p.write_text(late_src)
        r_late = lean(p, True, extra_env)
        require(r_late, False, expected_diag)
        assert expected_diag in r_late.stdout.splitlines(), r_late.stdout
    # Positive control: a harmless later attribute (simp) in a later section/namespace passes.
    p.write_text(
        'import Lean\n'
        'def harmlessFn (n : Nat) : Nat := n\n'
        'theorem harmlessEq (n : Nat) : harmlessFn n = n := rfl\n'
        'section LaterSec\n'
        'namespace LaterNS\n'
        'attribute [simp] harmlessFn harmlessEq\n'
        'end LaterNS\n'
        'end LaterSec\n'
        'example : harmlessFn 1 = 1 := by simp\n'
    )
    r_harmless = lean(p, True)
    require(r_harmless, True, 'example-json {"axioms":["propext"],')
    assert "⟪example⟫" in r_harmless.stdout, r_harmless.stdout
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
 def initEffect : IO Unit := pure ()
 attribute [init] initEffect
 def exportedFn (n : Nat) : Nat := n
 attribute [export c_exported_fn] exportedFn
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
 unless errors.contains "unlisted trusted declaration Concrete.Control.initEffect [init]" do
   throwError "initEffect escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.exportedFn [export]" do
   throwError "exportedFn escaped"
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

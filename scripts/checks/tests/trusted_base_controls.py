"""Light semantic controls; run inside nix develop after building Concrete.Premises."""
from pathlib import Path
import json
import os
import re
import subprocess
import tempfile
import sys
import time
import signal

import importlib.util
_spec = importlib.util.spec_from_file_location("trusted_control_run", Path(__file__).resolve().parents[1] / "lib/trusted_control_run.py")
_runner = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_runner)
control_run = _runner.control_run

DRIVER = "scripts/checks/compiler/slice0_examples.lean"
# Lake must never fetch while running controls. Only consume existing local artifacts.
BASE_ENV = dict(os.environ)
paths = [Path(".lake/build/lib/lean"), Path(".lake/packages/lean-compiler-backend/.lake/build/lib/lean")]
if not (paths[0] / "Concrete/Premises.olean").is_file():
    sys.exit("INCOMPLETE trusted-base control: missing prebuilt Concrete.Premises")
BASE_ENV["LEAN_PATH"] = os.pathsep.join(str(p.resolve()) for p in paths)
BASE_ENV["LEAN_NUM_THREADS"] = "2"

TEST_DRIVER = "scripts/checks/tests/trusted_examples_entry.lean"

def lean(path, driver=False, extra_env=None, production=False):
    env = dict(BASE_ENV)
    if extra_env: env.update(extra_env)
    if driver and extra_env and not production:
        env["TRUSTED_TEST_FILE"] = str(path)
        args = ["lean", TEST_DRIVER]
    else:
        args = ["lean", *(["--run", DRIVER] if driver else []), str(path)]
    return control_run(args,
        env=env, text=True, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, timeout={"ownership.lean": 180, "declarations.lean": 45}.get(Path(path).name, 15))

def lean_batch(paths):
    r = control_run(["lean", "--run", DRIVER, "--per-file", *[str(p) for p in paths]],
        env=dict(BASE_ENV), text=True, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, timeout=15)
    assert r.returncode == 0, r.stdout
    out, cur, measured = {}, [], set()
    for line in r.stdout.splitlines(keepends=True):
        if line.startswith("__FILE_MILLISECONDS__ "):
            path_s, ms = line[len("__FILE_MILLISECONDS__ "):].strip().rsplit(" ", 1)
            assert path_s not in measured, path_s
            measured.add(path_s)
            print("control-timing " + json.dumps({"control": "batch-file " + Path(path_s).name,
                "seconds": int(ms) / 1000, "bound_seconds": 15}), file=sys.stderr)
        elif line.startswith("__FILE_RC__ "):
            _, path_s, rc_s = line.strip().split(" ", 2)
            out[path_s] = subprocess.CompletedProcess(
                args=[path_s], returncode=int(rc_s), stdout="".join(cur))
            cur = []
        else:
            cur.append(line)
    assert set(out) == measured == {str(p) for p in paths}, (out.keys(), measured)
    return out

def require(result, ok, diagnostic):
    assert (result.returncode == 0) == ok and diagnostic in result.stdout, (result.returncode, result.stdout)

with tempfile.TemporaryDirectory() as d:
    d = Path(d)
    module = d / "scripts/checks/compiler/slice0_examples.olean"
    module.parent.mkdir(parents=True)
    control_run(["lean", "-o", str(module), DRIVER], env=BASE_ENV, check=True, timeout=15)
    BASE_ENV["LEAN_PATH"] = str(d) + os.pathsep + BASE_ENV.get("LEAN_PATH", "")
    ambient = d / "ambient.lean"
    ambient.write_text("import Lean\nunsafe def ambientAuthority : Nat := 0\n")
    ambient_result = lean(ambient, True, {
        "CONCRETE_TRUSTED_DECLARATIONS": "lexer:ambientAuthority",
        "CONCRETE_TRUSTED_BASE_WITHOUT": "lexer",
    }, production=True)
    ambient_diagnostic = "FAIL unlisted example trusted declaration ambientAuthority [unsafe]"
    require(ambient_result, False, ambient_diagnostic)
    assert ambient_diagnostic in ambient_result.stdout.splitlines(), ambient_result.stdout
    empty = d / "empty.rows"
    empty.write_text("no json rows\n")
    r_empty = control_run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(empty)],
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
    ex_cases = [
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
        ('import Lean\n@[cpass] meta example : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\n',
         None, 'cpass'),
    ]
    ex_paths = []
    for idx, (ex_src, extra, kind_tag) in enumerate(ex_cases):
        ex_p = d / f"ex_{idx}.lean"
        ex_p.write_text(ex_src)
        ex_paths.append((ex_p, extra, kind_tag))
    ex_batch = lean_batch([ex_p for ex_p, extra, _ in ex_paths if extra is None])
    for ex_p, extra, kind_tag in ex_paths:
        r_ex = ex_batch[str(ex_p)] if extra is None else lean(ex_p, True, extra)
        require(r_ex, False, f'FAIL unlisted example trusted declaration ')
        assert re.search(
            r'^FAIL unlisted example trusted declaration «?⟪example⟫'
            + re.escape(str(ex_p)) + r':\d+:\d+»? \[' + re.escape(kind_tag) + r'\]$',
            r_ex.stdout, re.M
        ), r_ex.stdout
    no_kw = d / "loop_no_kw.lean"
    benign = d / "benign.lean"
    benign.write_text('import Lean\ndef benignVal : Nat := 1\n')
    r_benign = lean(benign, True)
    assert r_benign.returncode == 0 and "example-json" not in r_benign.stdout, (r_benign.returncode, r_benign.stdout)
    benign_rows = d / "benign.rows"
    benign_rows.write_text(r_benign.stdout)
    r_benign_rows = control_run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(benign_rows)],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
    require(r_benign_rows, False, "FAIL examples: no example-json rows observed")
    valid_rows = d / "valid_example.rows"
    valid_rows.write_text(
        'example-json {"name":"⟪example⟫f.lean:1:0","axioms":[]}\n'
        'example-json {"name":"Concrete.BIR.Examples.«⟪example⟫Concrete/BIR.lean:542:0»","axioms":["propext"]}\n'
    )
    r_valid_rows = control_run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(valid_rows)],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
    assert r_valid_rows.returncode == 0, (r_valid_rows.returncode, r_valid_rows.stdout)
    non_ex_rows = d / "non_example.rows"
    for bad_rows_text in [
        'example-json {"name":"benignVal","axioms":[]}\n',
        'example-json {"name":"⟪example⟫missing_pos","axioms":[]}\n',
        'example-json {invalid json\n',
        'unparsed line\nexample-json {"name":"⟪example⟫f.lean:1:0","axioms":[]}\n',
    ]:
        non_ex_rows.write_text(bad_rows_text)
        r_non_ex = control_run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(non_ex_rows)],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
        require(r_non_ex, False, "FAIL examples: invalid observation schema")
    spoof = d / "spoof_example_json.lean"
    spoof.write_text('import Lean\n#eval IO.println "example-json {\\"name\\":\\"⟪example⟫spoof.lean:1:0\\",\\"axioms\\":[]}"\n')
    r_spoof = control_run(["lean", "--run", DRIVER, str(spoof)],
        env=dict(BASE_ENV), text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=15)
    assert r_spoof.returncode == 0 and r_spoof.stdout == "", (r_spoof.returncode, r_spoof.stdout, r_spoof.stderr)
    spoof_rows = d / "spoof.rows"
    spoof_rows.write_text(r_spoof.stdout)
    r_spoof_rows = control_run(["python3", "-I", "scripts/checks/lib/example_rows.py", str(spoof_rows)],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15)
    require(r_spoof_rows, False, "FAIL examples: no example-json rows observed")
    for loop_src, expected_decl in [
        ('import Lean\ndef whileOnly : IO Unit := do\n  while true do\n    pure ()\n',
         'FAIL unlisted example trusted declaration whileOnly [partial-loop]'),
        ('import Lean\ndef repeatOnly : IO Unit := do\n  repeat\n    break\n',
         'FAIL unlisted example trusted declaration repeatOnly [partial-loop]'),
    ]:
        assert "example" not in loop_src and "partial" not in loop_src
        no_kw.write_text(loop_src)
        require(lean(no_kw, True), False, expected_decl)
        r_multi = control_run(
            ["lean", "--run", DRIVER, str(benign), str(no_kw)],
            env=dict(BASE_ENV),
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=15,
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
    late_cases = [
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
        ('import Lean\n@[cpass] public meta def declCPass : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration declCPass [cpass]'),
        ('import Lean\npublic meta def lateCPass : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\n'
         'attribute [cpass] lateCPass\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration lateCPass [cpass]'),
        # Macro-generated commands applying extern, implemented_by, csimp and cpass attributes.
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
        ('import Lean\npublic meta def macroCPassTarget : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\n'
         'macro "applyCPass" : command => `(attribute [cpass] $(Lean.mkIdent `macroCPassTarget))\n'
         'applyCPass\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroCPassTarget [cpass]'),
        # Attributes applied in a later section/namespace (including scoped/local csimp, cpass and pre-attribute callers).
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
        ('import Lean\nnamespace Earlier\npublic meta def nsCPass : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\nend Earlier\n'
         'section LaterSec\nnamespace LaterNS\nattribute [cpass] Earlier.nsCPass\nend LaterNS\nend LaterSec\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration Earlier.nsCPass [cpass]'),
        ('import Lean\nnamespace Earlier\ndef sOrig (n : Nat) := n + 0\ndef sRepl (n : Nat) := n\n'
         'theorem scopedCsimp : sOrig = sRepl := by funext n; simp [sOrig, sRepl]\n'
         'attribute [scoped csimp] scopedCsimp\nend Earlier\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration Earlier.scopedCsimp [csimp]'),
        ('import Lean\nsection LocalSec\ndef lOrig (n : Nat) := n + 0\ndef lRepl (n : Nat) := n\n'
         'theorem localCsimp : lOrig = lRepl := by funext n; simp [lOrig, lRepl]\n'
         'attribute [local csimp] localCsimp\nend LocalSec\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration localCsimp [csimp]'),
        # Command-local attributes via `attribute [local ...] ... in` and `open ... in attribute [...]`.
        ('import Lean\ndef inOrig (n : Nat) := n + 0\ndef inRepl (n : Nat) := n\n'
         'theorem localCsimpIn : inOrig = inRepl := by funext n; simp [inOrig, inRepl]\n'
         'attribute [local csimp] localCsimpIn in\n'
         'def useLocal (n : Nat) : Nat := inOrig n\n'
         'example : useLocal 1 = 1 := rfl\n',
         None, 'FAIL unlisted example trusted declaration localCsimpIn [csimp]'),
        ('import Lean\nnamespace InScope\ndef inScopeExtern : Nat := 0\nend InScope\n'
         'open InScope in\nattribute [extern "c_open_in"] inScopeExtern\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration InScope.inScopeExtern [extern]'),
        ('import Lean\nnamespace InScope\ndef inScopeRepl : Nat := 0\ndef inScopeImpl : Nat := 1\nend InScope\n'
         'open InScope in\nattribute [implemented_by InScope.inScopeRepl] inScopeImpl\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration InScope.inScopeImpl [implemented_by]'),
        ('import Lean\nnamespace InScope\npublic meta def inScopeCPass : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\nend InScope\n'
         'open InScope in\nattribute [cpass] inScopeCPass\n'
         'example : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration InScope.inScopeCPass [cpass]'),
        # Macros that open and close their own section/namespace scope inside one command.
        ('import Lean\ndef scOrig (n : Nat) := n + 0\ndef scRepl (n : Nat) := n\n'
         'theorem macroScopeLocalCsimp : scOrig = scRepl := by funext n; simp [scOrig, scRepl]\n'
         'macro "closedScopeCsimp" : command => `(section\n'
         '  attribute [local csimp] $(Lean.mkIdent `macroScopeLocalCsimp)\n'
         '  def $(Lean.mkIdent `closedScopeUse) (n : Nat) : Nat := $(Lean.mkIdent `scOrig) n\n'
         'end)\n'
         'closedScopeCsimp\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroScopeLocalCsimp [csimp]'),
        ('import Lean\ndef macroScopeExtern : Nat := 0\n'
         'macro "closedScopeExtern" : command => `(section\n'
         '  attribute [extern "c_closed_scope"] $(Lean.mkIdent `macroScopeExtern)\n'
         'end)\n'
         'closedScopeExtern\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroScopeExtern [extern]'),
        ('import Lean\ndef macroScopeRepl : Nat := 0\ndef macroScopeImpl : Nat := 1\n'
         'macro "closedScopeImpl" : command => `(section\n'
         '  attribute [implemented_by $(Lean.mkIdent `macroScopeRepl)] $(Lean.mkIdent `macroScopeImpl)\n'
         'end)\n'
         'closedScopeImpl\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroScopeImpl [implemented_by]'),
        ('import Lean\npublic meta def macroScopeCPass : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\n'
         'macro "closedScopeCPass" : command => `(section\n'
         '  attribute [cpass] $(Lean.mkIdent `macroScopeCPass)\n'
         'end)\n'
         'closedScopeCPass\nexample : True := True.intro\n',
         None, 'FAIL unlisted example trusted declaration macroScopeCPass [cpass]'),
        ('import Lean\nprivate def privLate : Nat := 0\ndef callerBeforeAttr : Nat := privLate\n'
         'section LaterSec\nattribute [extern "c_priv_late"] privLate\nend LaterSec\n'
         'example : True := True.intro\n',
         {"CONCRETE_TRUSTED_DECLARATIONS": "lexer:privLate"},
         'FAIL unlisted example trusted declaration callerBeforeAttr [extern]'),
    ]
    late_paths = []
    for idx, (late_src, extra_env, expected_diag) in enumerate(late_cases):
        lp = d / f"late_{idx}.lean"
        lp.write_text(late_src)
        late_paths.append((lp, extra_env, expected_diag))
    local_in_cases = [
        ('import Lean\ndef localExternIn : Nat := 0\n'
         'attribute [local extern "c_local_in"] localExternIn in\n'
         'def useLocalExtern : Nat := localExternIn\n',
         "Invalid attribute scope: Attribute `[extern]` must be global, not `local`"),
        ('import Lean\ndef localImplRepl : Nat := 0\ndef localImplIn : Nat := 1\n'
         'attribute [local implemented_by localImplRepl] localImplIn in\n'
         'def useLocalImpl : Nat := localImplIn\n',
         "Invalid attribute scope: Attribute `[implemented_by]` must be global, not `local`"),
        ('import Lean\npublic meta def localCPassIn : Lean.Compiler.LCNF.PassInstaller :=\n'
         '  { phase := .base, install := pure }\n'
         'attribute [local cpass] localCPassIn in\n'
         'def useLocalCPass : Nat := 0\n',
         "Invalid attribute scope: Attribute `[cpass]` must be global, not `local`"),
    ]
    local_in_paths = []
    for idx, (local_in_src, scope_err) in enumerate(local_in_cases):
        lip = d / f"local_in_{idx}.lean"
        lip.write_text(local_in_src)
        local_in_paths.append((lip, scope_err))
    batch_res = lean_batch(
        [lp for lp, extra_env, _ in late_paths if extra_env is None] +
        [lip for lip, _ in local_in_paths]
    )
    for lp, extra_env, expected_diag in late_paths:
        r_late = batch_res[str(lp)] if extra_env is None else lean(lp, True, extra_env)
        require(r_late, False, expected_diag)
        assert expected_diag in r_late.stdout.splitlines(), r_late.stdout
    for lip, scope_err in local_in_paths:
        r_local_in = batch_res[str(lip)]
        require(r_local_in, False, scope_err)
    # Positive control: harmless later/local/macro-scoped attributes (simp) pass.
    p.write_text(
        'import Lean\n'
        'def harmlessFn (n : Nat) : Nat := n\n'
        'theorem harmlessEq (n : Nat) : harmlessFn n = n := rfl\n'
        'section LaterSec\n'
        'namespace LaterNS\n'
        'attribute [simp] harmlessFn\n'
        'end LaterNS\n'
        'end LaterSec\n'
        'attribute [local simp] harmlessEq in\n'
        'def harmlessLocalUse (n : Nat) : Nat := harmlessFn n\n'
        'macro "harmlessClosedScope" : command => `(section attribute [local simp] harmlessEq end)\n'
        'harmlessClosedScope\n'
        'example : harmlessFn 1 = 1 := by simp [harmlessEq]\n'
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
 @[cpass] public meta def declPass : Lean.Compiler.LCNF.PassInstaller := { phase := .base, install := pure }
 public meta def latePass : Lean.Compiler.LCNF.PassInstaller := { phase := .base, install := pure }
 attribute [cpass] latePass
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
 unless errors.contains "unlisted trusted declaration Concrete.Control.declPass [cpass]" do
   throwError "declPass escaped"
 unless errors.contains "unlisted trusted declaration Concrete.Control.latePass [cpass]" do
   throwError "latePass escaped"
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

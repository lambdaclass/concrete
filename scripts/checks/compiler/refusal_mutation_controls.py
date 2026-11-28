"""Qualify the pipeline audit and the independent unreachable-statement fixture.

The compiler gate checks the working audit. For differential qualification, pass
audit files from other revisions as positional arguments to the same command.
"""

from contextlib import contextmanager
import json
import os
from pathlib import Path
import subprocess
import sys
from tempfile import TemporaryDirectory


PIPELINE = Path("Concrete/Interfaces/Pipeline.lean")
AUDIT = Path("scripts/checks/compiler/stage_end_to_end_audit.lean")
LEXER = (
    "def PassFunctions.lexer (passes : PassFunctions grammar language lang profile) : Stage Source LexRefusal TokenStream where\n"
    "  run := passes.lex\n"
    "  kind := .directProof\n"
    "  inputInvariant := Source.WF\n"
    "  outputInvariant stream := stream.parserView.WF ∧ TokensOrdered stream.parserView\n"
    "  success := Lexes\n"
    "  refusal := LexRefuses"
)
LLVM_EMIT = (
    "  | ⟨.llvm, target, maxSlots, maxStack⟩, passes =>\n"
    "    { run := emitGuarded (language := language) passes.emit\n"
    "      kind := .namedPremise \"P_print\"\n"
    "      inputInvariant := BIR.WF (language := language)\n"
    "      outputInvariant := fun output => ∃ bir : BIR lang ⟨.llvm, target, maxSlots, maxStack⟩, LLVMMetadataMatches bir output\n"
    "      success := fun bir output =>\n"
    "        firstUnsupported (language := language) bir = none ∧\n"
    "        LLVMMetadataMatches bir output\n"
    "      refusal := fun bir failure =>\n"
    "        ∃ cause, firstUnsupported (language := language) bir = some cause ∧\n"
    "          failure = .unsupportedFeature ⟨.llvm, target, maxSlots, maxStack⟩ cause\n"
    "      trust := P_print (language := language) }"
)
PRINT = "  let source := BIR.Execution (language := language)"
PPRINT_DECL = "def P_print (bir : BIR lang profile) (output : LLVMInput profile) : Prop :="
PPRINT_INLINE_EXAMPLE = (
    "-- Two-module LLVM `P_print` binds `output.packageBytes` (both `.component` and `.launcher`).\n"
    "example {profile : Profile} (bir : BIR (sliceLoweringLanguage slice0) profile)\n"
    "    (out : LLVMInput profile) :\n"
    "    out.packageBytes.component = out.bytesAt .component ∧\n"
    "    out.packageBytes.launcher = out.bytesAt .launcher ∧\n"
    "    (P_print (language := sliceLanguage slice0) bir out →\n"
    "      ProgramSafe (BIR.Execution (language := sliceLanguage slice0)) bir →\n"
    "      ¬ llvmBehaviors (E := (sliceLanguage slice0).operands.Event) out.packageBytes .wrong) :=\n"
    "  ⟨rfl, rfl, fun hp hsafe hwrong => hp hsafe .wrong hwrong⟩"
)
DECODER = "ByteArray → Option UInt8"
WITNESS = f"(fun (decode : {DECODER}) => decode ByteArray.empty ≠ none)"
VIA_ID = f"¬ (id Exists) {WITNESS}"
EXISTS = f"∃ decode : {DECODER}, decode ByteArray.empty ≠ none"
W_U64 = "∃ decode : ByteArray → UInt64, decode output.packageBytes.component = 0"


def lexer(field, value):
    if field == "inputInvariant":
        return LEXER.replace("  inputInvariant := Source.WF", f"  inputInvariant := {value}")
    if field == "success":
        return LEXER.replace("  success := Lexes", f"  success := {value}")
    if field == "trust":
        return LEXER + f"\n  trust := {value}"
    raise ValueError(f"unsupported lexer field {field}")


def llvm_emit_trust(value, kind=None):
    out = LLVM_EMIT.replace("      trust := P_print (language := language) }",
                            f"      trust := {value} }}")
    if kind is not None:
        out = out.replace('      kind := .namedPremise "P_print"', f"      kind := {kind}")
    return out


# The print conjunct makes trust impossible, while the implication makes it vacuous.
# An existential over a relation or model can also hide in a success field.
# Includes live shadow-module controls for ISS-0131 / Codex round-2 counterexamples (4a, 4b, 4c, 5a, 5b).
MUTATIONS = [
    ("ready-id", LEXER, lexer("inputInvariant", f"fun x => Source.WF x ∧ ({VIA_ID})")),
    ("print-conjunct", PRINT, f"  ({VIA_ID}) ∧\n" + PRINT),
    ("print-guard", PRINT, f"  ({VIA_ID}) →\n" + PRINT),
    ("llvm-final-true", LLVM_EMIT, llvm_emit_trust("fun _ _ => True", kind=".directProof")),
    ("success-id-relation", LEXER, lexer("success",
     "fun x y => ¬ (id Exists) (fun (meaning : Source → TokenStream → Prop) => meaning x y)")),
    ("success-id-model", LEXER, lexer("success",
     "fun _ _ => ¬ (id Exists) (fun (model : CoreLanguage) => model = model)")),
    ("ready-option-decoder", LEXER, lexer("inputInvariant",
     f"fun x => Source.WF x ∧ Nonempty ((some ((decode : {DECODER}) → "
     "PLift (decode ByteArray.empty = none))).getD PUnit)")),
    ("ready-exists", LEXER, lexer("inputInvariant",
     f"fun x => Source.WF x ∧ ¬ ({EXISTS})")),
    ("print-exists", PRINT, f"  (({EXISTS}) → False) →\n" + PRINT),
    ("print-let-exists", PRINT, f"  let witness := {EXISTS}\n  (¬ witness) →\n" + PRINT),
    ("print-neg-exists-prop", PRINT,
     f"  let W := {W_U64}\n  ¬ (∃ _ : W, True) →\n" + PRINT),
    ("print-match-decide", PRINT,
     f"  open Classical in\n  let W := {W_U64}\n"
     "  (match decide W with | true => False | false => True) →\n" + PRINT),
    ("print-fn-eq", PRINT,
     "  ((fun d : ByteArray → UInt64 => d output.packageBytes.component) =\n"
     "    (fun _ : ByteArray → UInt64 => (0 : UInt64))) →\n" + PRINT),
    ("print-local-lcb-ns", PPRINT_DECL,
     "private def _root_.LeanCompilerBackend.EvilPrintGuard (bs : ByteArray) : Prop :=\n"
     "  ¬ ∃ decode : ByteArray → UInt64, decode bs = 0\n"
     + PPRINT_DECL + "\n"
     "  _root_.LeanCompilerBackend.EvilPrintGuard output.packageBytes.component →"),
    ("print-custom-lt", PPRINT_DECL,
     "@[instance_reducible] private def evilPrintLT (bs : ByteArray) : LT Unit :=\n"
     "  ⟨fun _ _ => ¬ ∃ decode : ByteArray → UInt64, decode bs = 0⟩\n"
     + PPRINT_DECL + "\n"
     "  @LT.lt Unit (evilPrintLT output.packageBytes.component) () () →"),
    ("success-exists", LEXER, lexer("success",
     "fun x y => (∃ meaning : Source → TokenStream → Prop, meaning x y) → False")),
    ("trust-id", LLVM_EMIT, llvm_emit_trust(f"fun _ _ => {VIA_ID}")),
    ("trust-let", LLVM_EMIT, llvm_emit_trust(f"fun _ _ => let witness := {EXISTS}; ¬ witness")),
    ("trust-projection", LLVM_EMIT, llvm_emit_trust(f"fun _ _ => (True, ¬ ({EXISTS})).2")),
]


def verdict(audit, env):
    result = subprocess.run(["lean", "--json", "-DautoImplicit=false", "-DwarningAsError=true", audit],
                            env=env, capture_output=True, text=True)
    if result.returncode == 0:
        return "accepted"
    diagnostics = [json.loads(line) for line in result.stdout.splitlines()]
    errors = [row["data"] for row in diagnostics if row.get("severity") == "error"]
    if result.returncode != 1 or not errors or not all(
            "open semantic binder" in error or
            ("unsolved goals" in error and ("FixedTrust" in error or "AllowedTrust" in error))
            for error in errors):
        raise SystemExit(f"{audit}: unexpected Lean failure\n{result.stdout}{result.stderr}")
    return "rejected"


@contextmanager
def shadow_module(path):
    search = os.environ["LEAN_PATH"].split(os.pathsep)
    artifact = path.with_suffix(".olean")
    library = next(Path(entry).resolve() for entry in search if (Path(entry) / artifact).is_file())
    with TemporaryDirectory() as temp:
        shadow = Path(temp)
        module = shadow / path
        module.parent.mkdir(parents=True)
        # Lean selects a search root by module prefix. Supply its other imports.
        for olean in (library / path.parts[0]).rglob("*.olean"):
            dest = shadow / olean.relative_to(library)
            if dest != shadow / artifact:
                dest.parent.mkdir(parents=True, exist_ok=True)
                dest.symlink_to(olean)
        env = dict(os.environ, LEAN_PATH=os.pathsep.join([temp, *search]))
        yield module, env


def compile_variant(module, env, source, old, new):
    if source.count(old) != 1:
        raise SystemExit(f"{module}: expected one occurrence of {old!r}")
    module.write_text(source.replace(old, new))
    shadow = env["LEAN_PATH"].split(os.pathsep)[0]
    subprocess.run(["lean", f"--root={shadow}", "-o", module.with_suffix(".olean"), module],
                   env=env, check=True)


def compare_audits(audits):
    source = PIPELINE.read_text()
    if source.count(PPRINT_INLINE_EXAMPLE) != 1:
        raise SystemExit(f"{PIPELINE}: expected one occurrence of PPRINT_INLINE_EXAMPLE")
    with shadow_module(PIPELINE) as (module, env):
        print("mutation", *(audit.name for audit in audits), sep="\t")
        passing = [("pristine", LEXER, LEXER),
                   ("wrapped-true", LEXER, lexer("trust", "fun _ _ => id True"))]
        for label, old, new in [*passing, *MUTATIONS]:
            variant_src = source
            if old in (PRINT, PPRINT_DECL):
                variant_src = variant_src.replace(PPRINT_INLINE_EXAMPLE, "")
            compile_variant(module, env, variant_src, old, new)
            outcomes = [verdict(audit, env) for audit in audits]
            print(label, *outcomes, sep="\t", flush=True)
            if (label, old, new) in passing:
                if any(result != "accepted" for result in outcomes):
                    raise SystemExit(f"an audit failed on the {label} pipeline")
            elif outcomes[-1] != "rejected":
                raise SystemExit(f"working audit accepted {label}")


def check_unreachable_fixture():
    spec = Path("ConcreteSpec/Spec/Surface0.lean")
    source = spec.read_text()
    emitter = Path("scripts/checks/compiler/slice0_bir.lean")
    rows = Path("tests/manifest.tsv").read_text().splitlines()
    [row] = [line for line in rows if line.startswith("source_after_return\t")]
    _, path, expectation, _ = row.split("\t")
    path = path.removeprefix("con:")
    old = "| .ret _ _ :: next :: _ => .error (.unreachable next.span)"
    operand = "| .ret _ _ :: .ret (.integer _ _ operand) _ :: _ => .error (.unreachable operand)"
    with shadow_module(spec) as (module, env):
        for label, replacement in [("pristine", old), ("operand", operand + "\n    " + old)]:
            compile_variant(module, env, source, old, replacement)
            result = subprocess.run(["lean", "--run", emitter, "source", path, path,
                                     module.parent, "x86_64-linux", *expectation.split()],
                                    env=env, capture_output=True, text=True)
            expected = 0 if label == "pristine" else 14
            if result.returncode != expected or (expected and "spec reports" not in result.stderr):
                raise SystemExit(f"{label}: unexpected fixture result {result.returncode}\n"
                                 f"{result.stdout}{result.stderr}")
            print(f"unreachable fixture ({label}): {result.returncode} {result.stderr.strip()}")


def check_backend_policy_revision_controls():
    policy = Path("ConcreteSpec/Spec/BackendPolicy.lean")
    rules = Path("ConcreteSpec/Spec/Rules.lean")
    policy_source = policy.read_text()
    rules_source = rules.read_text()
    targets = [
        (Path("ConcreteChecks/StageInterfaces/BackendPolicyAndLCBJob.lean"), 2),
        (Path("scripts/checks/compiler/rule_registry.lean"), 1),
    ]
    mutations = [
        ("pristine", None, None, None),
        ("intWidthRule-rev-3", "def intWidthRule : RuleId := ⟨``IntWidthSupported, 1⟩",
         "def intWidthRule : RuleId := ⟨``IntWidthSupported, 3⟩", "BackendPolicy.IntWidthSupported"),
        ("operationRule-rev-3", "def operationRule : RuleId := ⟨``OperationSupported, 1⟩",
         "def operationRule : RuleId := ⟨``OperationSupported, 3⟩", "BackendPolicy.OperationSupported"),
        ("storageFormRule-rev-3", "def storageFormRule : RuleId := ⟨``StorageFormSupported, 1⟩",
         "def storageFormRule : RuleId := ⟨``StorageFormSupported, 3⟩", "BackendPolicy.StorageFormSupported"),
        ("callRule-rev-3", "def callRule : RuleId := ⟨``CallSupported, 1⟩",
         "def callRule : RuleId := ⟨``CallSupported, 3⟩", "BackendPolicy.CallSupported"),
    ]
    with shadow_module(policy) as (policy_mod, env):
        shadow = Path(env["LEAN_PATH"].split(os.pathsep)[0])
        rules_mod = shadow / rules
        rules_olean = rules_mod.with_suffix(".olean")
        if rules_olean.is_symlink() or rules_olean.exists():
            rules_olean.unlink()
        rules_mod.write_text(rules_source)
        for label, old, new, expected_rule in mutations:
            if old is None:
                policy_mod.write_text(policy_source)
                subprocess.run(["lean", f"--root={shadow}", "-DautoImplicit=false",
                                "-o", policy_mod.with_suffix(".olean"), policy_mod],
                               env=env, check=True)
            else:
                compile_variant(policy_mod, env, policy_source, old, new)
            subprocess.run(["lean", f"--root={shadow}", "-DautoImplicit=false",
                            "-o", rules_olean, rules_mod],
                           env=env, check=True)
            for target, min_errors in targets:
                result = subprocess.run(
                    ["lean", "--json", "-DautoImplicit=false", "-DwarningAsError=true", target],
                    env=env, capture_output=True, text=True)
                if old is None:
                    if result.returncode != 0:
                        raise SystemExit(f"{label} ({target}): pristine check failed\n"
                                         f"{result.stdout}{result.stderr}")
                    print(f"backend policy revision ({label}, {target.name}): accepted", flush=True)
                else:
                    diagnostics = [json.loads(line) for line in result.stdout.splitlines()
                                   if line.startswith("{")]
                    errors = [row["data"] for row in diagnostics if row.get("severity") == "error"]
                    if (result.returncode != 1 or len(errors) < min_errors
                            or not all("Tactic `decide` proved that the proposition" in error
                                       and expected_rule in error for error in errors)):
                        raise SystemExit(f"{label} ({target}): expected {min_errors}+ decide failures "
                                         f"(rc={result.returncode})\n{result.stdout}{result.stderr}")
                    print(f"backend policy revision ({label}, {target.name}): rejected ({len(errors)} controls)",
                          flush=True)


if __name__ == "__main__":
    compare_audits([Path(path).resolve() for path in sys.argv[1:]] + [AUDIT.resolve()])
    check_unreachable_fixture()
    check_backend_policy_revision_controls()

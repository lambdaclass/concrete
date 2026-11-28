"""Qualify exact LLVM output and the BIR oracle against isolated printer mutations.

Run with `lake env python3 -B` after building Concrete, inside the Nix devshell
and under the compiler test gate's time and memory bounds.
"""

from pathlib import Path
import os
import subprocess
import tempfile
import time


LLVM = Path("Concrete/Backend/LLVM.lean")
IR = Path("Concrete/Backend/LLVMIR.lean")
RUNTIME = Path("Concrete/Backend/Runtime.lean")
PROVISIONAL_RUNTIME = Path("Concrete/Backend/ProvisionalRuntime.lean")
RETURN_LOCAL = "(.integer .u8, operandAt 8 a) (.integer .u32)"
TOKEN = "def Immediate.token (v : Immediate) : String := decimal v.value"
PIECE = "  | .immediate v before after => String.singleton before.char ++ v.token ++ String.singleton after.char"
MUTATIONS = (
    (
        "equal-width copy zero",
        LLVM,
        "(operandAt (Width.ofCore target).bits a) (.immediate (.natural 0))",
        "(operandAt (Width.ofCore target).bits a) (.immediate (.natural 1))",
    ),
    (
        "checked call immediate moved to the local slot",
        LLVM,
        "[(i, operandAt width.bits a), (i, operandAt width.bits b)]",
        "(match a, b with\n"
        "       | .imm .., .reg _ => [(i, operandAt width.bits b), (i, operandAt width.bits a)]\n"
        "       | _, _ => [(i, operandAt width.bits a), (i, operandAt width.bits b)])",
    ),
    (
        "arithmetic encoded at the immediate tag width",
        LLVM,
        "(operandAt width.bits a) (operandAt width.bits b)",
        "(operandAt (match a with | .imm t _ => (toCore t).bits | _ => width.bits) a) "
        "(operandAt (match b with | .imm t _ => (toCore t).bits | _ => width.bits) b)",
    ),
    (
        "trap result word left wider than i32",
        LLVM,
        "(.natural (toBits .u32 (Int.ofNat (256 + s))))",
        "(.natural (256 + s))",
    ),
    (
        "right immediate moved between sibling additions",
        LLVM,
        "phis p j b ++ b.insts.flatMap inst ++ term j b.term) ++\n  [.endFunction]",
        "phis p j b ++ (match b.insts.flatMap inst with\n"
        "      | .instruction (.bin d .add ty lhs (.immediate value)) ::\n"
        "        .instruction (.bin e .add ty' lhs' (.local source)) :: rest =>\n"
        "        if ty == ty' then\n"
        "          .instruction (.bin d .add ty lhs (.local source)) ::\n"
        "          .instruction (.bin e .add ty' lhs' (.immediate value)) :: rest\n"
        "        else b.insts.flatMap inst\n"
        "      | items => items) ++ term j b.term) ++\n  [.endFunction]",
    ),
    (
        "phi predecessor plus one",
        LLVM,
        "(operandAt ((ty pty).integerWidth.getD 1) (args.getD k (.imm .u8 0)), Label.block pred)\n    .instruction",
        "(operandAt ((ty pty).integerWidth.getD 1) (args.getD k (.imm .u8 0)), Label.block (pred + 1))\n    .instruction",
    ),
    (
        "extract indices swapped",
        LLVM,
        "pair (.overflow d) 0),\n     .instruction (.extract (.register f) pair (.overflow d) 1)",
        "pair (.overflow d) 1),\n     .instruction (.extract (.register f) pair (.overflow d) 0)",
    ),
    (
        "trap result word off by one",
        LLVM,
        "(.natural (toBits .u32 (Int.ofNat (256 + s))))",
        "(.natural (toBits .u32 (Int.ofNat (257 + s))))",
    ),
    (
        "return zero-extension target changed from i32 to i64",
        LLVM,
        RETURN_LOCAL,
        "(.integer .u8, operandAt 8 a) (.integer .u64)",
    ),
    (
        "operand encoding plus one",
        LLVM,
        ".immediate (.natural (toBitsAtWidth width v))",
        ".immediate (.natural (toBitsAtWidth width v + 1))",
    ),
    (
        "write descriptor changed to 3",
        RUNTIME,
        "[(i32, .immediate (.natural 2)), (.ptr",
        "[(i32, .immediate (.natural 3)), (.ptr",
    ),
    (
        "switch destinations rotated",
        RUNTIME,
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite i)",
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite (i + 1))",
    ),
    (
        "first two switch destinations swapped",
        RUNTIME,
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite i)",
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), "
        "Label.trapSite (if diagnostics.length < 2 then i else if i = 0 then 1 else if i = 1 then 0 else i))",
    ),
    (
        "Core u64 mapped to LLVM i32",
        IR,
        "| .u64 | .i64 => .u64",
        "| .u64 => .u32\n  | .i64 => .u64",
    ),
    (
        "Core i64 mapped to LLVM i32",
        IR,
        "| .u64 | .i64 => .u64",
        "| .u64 => .u64\n  | .i64 => .u32",
    ),
    (
        "render i32 as i64",
        IR,
        "  | .integer ty => numbered .integer ty.bits",
        "  | .integer .u32 => numbered .integer 64\n  | .integer ty => numbered .integer ty.bits",
    ),
    ("%ret prefix becomes 0", IR, '| .result => "%ret"', '| .result => "0"'),
    (
        "%ret result becomes bare 0",
        IR,
        "| .numbered p n => p.text ++ decimal n",
        '| .numbered .result _ => "0"\n  | .numbered p n => p.text ++ decimal n',
    ),
    *(
        (
            f"render immediate {value} as {spelling}",
            IR,
            TOKEN,
            TOKEN.replace(":= decimal v.value", f':= if v.value = {value} then "{spelling}" else decimal v.value'),
        )
        for value, spelling in [(0, "zeroinitializer"), (0, "-0"), (17, "1_7")]
    ),
    (
        "drop the space before a negative operand",
        IR,
        PIECE,
        '  | .immediate v before after => if v.value = 17 then "-17" ++ String.singleton after.char\n'
        '      else String.singleton before.char ++ v.token ++ String.singleton after.char',
    ),
    (
        "underscore replaces operand comma",
        IR,
        PIECE,
        '  | .immediate v before .comma => String.singleton before.char ++ v.token ++ "_"\n' + PIECE,
    ),
    (
        "left operand duplicated",
        IR,
        "  | .bin dst op ty lhs rhs =>\n    assign dst ++ (arithmetic op ++ text \" \" ++ "
        "typeCode ty ++ arg lhs .comma ++ arg rhs .newline)",
        "  | .bin dst op ty lhs _ =>\n    assign dst ++ (arithmetic op ++ text \" \" ++ "
        "typeCode ty ++ arg lhs .comma ++ arg lhs .newline)",
    ),
)


BENIGN = (
    "double reversal of BIR instructions",
    LLVM,
    "phis p j b ++ b.insts.flatMap inst ++ term j b.term) ++\n  [.endFunction]",
    "phis p j b ++ b.insts.reverse.reverse.flatMap inst ++ term j b.term) ++\n  [.endFunction]",
)

FIXTURES = "scripts/checks/compiler/llvm_rendering.lean"
GOLDEN = Path("tests/golden/slice0_llvm.txt")
NATIVE = "scripts/checks/lib/slice0_native.sh"
MODULES = (IR, RUNTIME, PROVISIONAL_RUNTIME, LLVM)
LEAN = ["lean", "-DwarningAsError=true", "-j", os.environ.get("LEAN_NUM_THREADS", "2"), "-M", "2048"]


def emit(*args, env=None):
    result = subprocess.run(
        [*LEAN, "--run", FIXTURES, *args], env=env, stdout=subprocess.PIPE, check=True
    )
    return result.stdout


def compare_output(actual, expected):
    if actual == expected:
        return True
    # Find the first differing byte, including a missing or surplus suffix.
    offset = next((i for i, (a, b) in enumerate(zip(actual, expected)) if a != b),
                  min(len(actual), len(expected)))
    print(f"LLVM exact output differs at byte {offset}: "
          f"expected {expected[offset:offset + 24]!r}, got {actual[offset:offset + 24]!r}")
    return False


def build_mutant(directory, path, before, after):
    source = path.read_text(encoding="utf-8")
    if source.count(before) != 1 or before == after:
        raise SystemExit(f"{path}: mutation must change exactly one definition site")

    env = os.environ.copy()
    search_path = env["LEAN_PATH"].split(os.pathsep)
    # Lean resolves modules under the first search entry that owns Concrete.
    # Supply the unchanged imports there as well as the rebuilt modules.
    library = next(Path(entry) for entry in search_path
                   if (Path(entry) / "Concrete/BIR.olean").is_file())
    env["LEAN_PATH"] = os.pathsep.join((str(directory), *search_path))
    split = MODULES.index(path)
    for module in (Path("Concrete/BIR.lean"), *MODULES[:split]):
        artifact = module.with_suffix(".olean")
        link = directory / artifact
        link.parent.mkdir(parents=True, exist_ok=True)
        link.symlink_to((library / artifact).resolve())

    # Re-elaborate downstream backend modules against the changed definitions.
    # Compilation failures abort the gate and cannot count as mutation kills.
    for module in MODULES[split:]:
        shadow = directory / module
        shadow.parent.mkdir(parents=True, exist_ok=True)
        shadow.write_text(source.replace(before, after) if module == path
                          else module.read_text(encoding="utf-8"), encoding="utf-8")
        subprocess.run([*LEAN, f"--root={directory}", "-DautoImplicit=false", "-o", shadow.with_suffix(".olean"), shadow],
                       env=env, check=True)
    return env


def observe_native(directory, fixture, env=None):
    (directory / "llvm.ll").write_bytes(emit(f"{fixture}-module", env=env))
    subprocess.run(["bash", NATIVE, directory, "-", "llvm"], check=True)
    result = subprocess.run([directory / "llvm.exe"], capture_output=True, check=False)
    return result.returncode, result.stdout, result.stderr


# Require exact changed observations from valid executables.
DIFFERENTIAL = {
    "operand encoding plus one": ("return", (18, b"", b"")),
    "first two switch destinations swapped": (
        "trap", (134, b"", 'concrete: trap: overflow at src/ñandú "x".con:1:1 (site 0)\n'.encode("utf-8"))
    ),
}


def check_mutation(directory, expected, mutation, oracles, *, benign=False):
    description, path, before, after = mutation
    start = time.monotonic()
    with tempfile.TemporaryDirectory(dir=directory) as scratch:
        shadow = Path(scratch)
        env = build_mutant(shadow, path, before, after)
        matches = compare_output(emit(env=env), expected)
        if matches != benign:
            raise SystemExit(f"{description}: unexpected exact-output result")
        if description in DIFFERENTIAL:
            fixture, planted = DIFFERENTIAL[description]
            observation = observe_native(shadow, fixture, env)
            if observation == oracles[fixture] or observation != planted:
                raise SystemExit(f"{description}: unexpected native observation {observation!r}")
            print(f"ok   differential control: {fixture} observation disagrees with the BIR oracle")
    verdict = "preserves exact output" if benign else "rejected by exact output"
    print(f"ok   {description}: {verdict} ({time.monotonic() - start:.2f}s)", flush=True)


def main():
    missing = DIFFERENTIAL.keys() - {description for description, *_ in MUTATIONS}
    if missing:
        raise SystemExit(f"differential controls without mutations: {', '.join(sorted(missing))}")
    expected = GOLDEN.read_bytes()
    if not compare_output(emit(), expected):
        raise SystemExit(f"LLVM printer drifted from {GOLDEN}")
    print("ok   LLVM full-module and byte-constant exact output", flush=True)
    with tempfile.TemporaryDirectory() as scratch:
        directory = Path(scratch)
        oracles = {}
        for fixture in ("return", "trap", "widths"):
            status, stderr = emit(f"{fixture}-observation").split(b"\n", 1)
            oracles[fixture] = (int(status), b"", stderr)
            native = directory / fixture
            native.mkdir()
            if observe_native(native, fixture) != oracles[fixture]:
                raise SystemExit(f"unmodified LLVM disagrees with the BIR oracle on {fixture}")
            print(f"ok   unmodified LLVM agrees with the BIR oracle on {fixture}", flush=True)
        for mutation in MUTATIONS:
            check_mutation(directory, expected, mutation, oracles)
        check_mutation(directory, expected, BENIGN, oracles, benign=True)


if __name__ == "__main__":
    main()

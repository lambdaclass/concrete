"""Re-elaborate the real producer or renderer with one planted change per private copy."""

from pathlib import Path
import subprocess
import tempfile
import time


LLVM = Path("Concrete/Backend/LLVM.lean")
IR = Path("Concrete/Backend/LLVMIR.lean")
RUNTIME = Path("Concrete/Backend/Runtime.lean")
TOKEN = "def Immediate.token (v : Immediate) : String := decimal v.value"
PIECE = "  | .immediate v before after => String.singleton before.char ++ v.token ++ String.singleton after.char"
MUTATIONS = (
    (
        "equal-width copy zero",
        LLVM,
        "(operand a) (.immediate (.natural 0))",
        "(operand a) (.immediate (.natural 1))",
        "inst_inventory",
    ),
    (
        "extract indices swapped",
        LLVM,
        "pair (.overflow d) 0),\n     .instruction (.extract (.register f) pair (.overflow d) 1)",
        "pair (.overflow d) 1),\n     .instruction (.extract (.register f) pair (.overflow d) 0)",
        "inst_inventory",
    ),
    (
        "trap result word off by one",
        LLVM,
        "(.natural (256 + s))",
        "(.natural (257 + s))",
        "term_inventory",
    ),
    (
        "operand encoding plus one",
        LLVM,
        ".immediate (.natural (toBits (toCore t) v))",
        ".immediate (.natural (toBits (toCore t) v + 1))",
        "operand_field",
    ),
    (
        "write descriptor changed to 3",
        RUNTIME,
        "[(i32, .immediate (.natural 2)), (.ptr",
        "[(i32, .immediate (.natural 3)), (.ptr",
        "llvmItems_inventory",
    ),
    ("%ret prefix becomes 0", IR, '| .result => "%ret"', '| .result => "0"', "parse_render_i32_result"),
    (
        "%ret result becomes bare 0",
        IR,
        "| .numbered p n => p.text ++ decimal n",
        '| .numbered .result _ => "0"\n  | .numbered p n => p.text ++ decimal n',
        "parse_render_i32_result",
    ),
    *(
        (
            f"render immediate {value} as {spelling}",
            IR,
            TOKEN,
            TOKEN.replace(":= decimal v.value", f':= if v.value = {value} then "{spelling}" else decimal v.value'),
            "render_immediate",
        )
        for value, spelling in [(0, "zeroinitializer"), (0, "-0"), (17, "1_7")]
    ),
    (
        "drop the space before a negative operand",
        IR,
        PIECE,
        '  | .immediate v before after => if v.value = 17 then "-17" ++ String.singleton after.char\n'
        '      else String.singleton before.char ++ v.token ++ String.singleton after.char',
        "render_immediate",
    ),
    (
        "underscore replaces operand comma",
        IR,
        PIECE,
        '  | .immediate v before .comma => String.singleton before.char ++ v.token ++ "_"\n' + PIECE,
        "render_immediate",
    ),
    (
        "left operand duplicated",
        IR,
        "  | .bin dst op ty lhs rhs =>\n    assign dst ++ arithmetic op ++ text \" \" ++ "
        "typeCode ty ++ arg lhs .comma ++ arg rhs .newline",
        "  | .bin dst op ty lhs _ =>\n    assign dst ++ arithmetic op ++ text \" \" ++ "
        "typeCode ty ++ arg lhs .comma ++ arg lhs .newline",
        "immediates_instruction",
    ),
)


def check_mutation(directory, description, path, before, after, theorem):
    source = path.read_text(encoding="utf-8")
    if source.count(before) != 1:
        raise SystemExit(f"{description}: mutation must match exactly one source site")
    source = source.replace(before, after)
    mutant = directory / path.name
    mutant.write_text(source, encoding="utf-8")
    lines = source.splitlines()
    first = next(number for number, line in enumerate(lines, 1) if f"theorem {theorem} " in line)
    last = next((number for number, line in enumerate(lines, 1) if number > first and not line), len(lines))

    start = time.monotonic()
    result = subprocess.run(
        ["lake", "env", "lean", "-M", "2048", str(mutant)], capture_output=True, text=True, check=False
    )
    output = result.stdout + result.stderr
    failure = any(
        line.startswith(f"{mutant}:{number}:")
        and any(marker in line for marker in (": error: unsolved goals", ": error: Tactic", ": error: Type mismatch"))
        for line in output.splitlines()
        for number in range(first, last + 1)
    )
    if result.returncode != 1 or not failure:
        raise SystemExit(f"{description}: expected a proof error in {theorem}\n{output}")
    print(f"ok   {description} rejected by {theorem} ({time.monotonic() - start:.2f}s)")


if __name__ == "__main__":
    with tempfile.TemporaryDirectory() as scratch:
        for mutation in MUTATIONS:
            check_mutation(Path(scratch), *mutation)

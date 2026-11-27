"""Check the citation printed by the installed driver on mixed failures."""
from pathlib import Path
from subprocess import run
from tempfile import TemporaryDirectory

driver = Path(".lake/build/bin/concrete").resolve()
cases = [
    ("main.con", "return ; @", "Slice0Rules.Parsed@1", "file 0", "25..26", ""),
    ("main.con", "let Option: u8 = 0; return ;", "Slice0Rules.UnreservedLocal@2", "file 0", "22..28", ""),
    ("main.con", "let x: u8 = 1; let x {} = x; return 0;", "Slice0Rules.AdmittedProduction@2", "file 0", "39..40", ""),
    ("main.con", "let Option {} = 1; return 0;", "Slice0Rules.AdmittedProduction@2", "file 0", "29..30", ""),
    ("main.con", "let main {} = 1; return 0;", "Slice0Rules.AdmittedProduction@2", "file 0", "27..28", ""),
    ("main.con", "return 01;", "Slice0Rules.Lexed@2", "file 0", "25..26", "leading zero"),
    ("main.con", "return 1z;", "Slice0Rules.Lexed@2", "file 0", "25..26", "invalid integer suffix"),
    ("main.con", r'return "a\q";', "Slice0Rules.Lexed@2", "file 0", "25..26", "invalid string escape (113)"),
    ("main.con", r"return '\q';", "Slice0Rules.Lexed@2", "file 0", "25..26", "invalid byte escape (113)"),
    ("main.con", "return ; \0", "Slice0Rules.Parsed@1", "file 0", "25..26", ""),
    ("main.con", "return '!\0", "Slice0Rules.Lexed@2", "file 0", "27..28", "invalid source"),
    ("main.con", "return 1i\0", "Slice0Rules.Lexed@2", "file 0", "27..28", "invalid source"),
    ("main.con", "let Option: i8 = 1; return 0;", "Slice0Rules.UnreservedLocal@2", "file 0", "22..28", ""),
    ("main.con", "return 0x2a", "Slice0Rules.DecimalLiteral@1", "file 0", "25..29", ""),
    ("main.con", "if", "Slice0Rules.AdmittedProduction@2", "file 0", "18..20", ""),
    ("main.con", "return 42u8;", "Slice0Rules.ExpectedTypeLiteral@1", "file 0", "25..29", ""),
    ("main.con", "return (256 + ;", "Slice0Rules.LiteralTyped@1", "file 0", "26..29", ""),
    ("a b.con", "return 0;", "SourcePath.NoSpaces@1", "path", "1..2", ""),
    ("con.con", "return 0;", "SourcePath.NoReservedStem@1", "path", "0..1", ""),
    ("main.con", "return 0xff;", "Slice0Rules.EntryName@1", "file 0", "3..7", ""),
]

with TemporaryDirectory() as work:
    failures = []
    for path, body, rule, domain, span, detail in cases:
        name = "oops" if rule == "Slice0Rules.EntryName@1" else "main"
        (Path(work) / path).write_text(f"fn {name}() -> u8 {{ {body} }}")
        result = run([driver, "build", path, "-o", "out"], cwd=work, capture_output=True, text=True)
        citation = f"[rule ConcreteSpec.Core.{rule}, {domain}, bytes {span}]"
        if (result.returncode != 1 or result.stdout or result.stderr.count("[rule ") != 1
                or not result.stderr.rstrip().endswith(citation) or detail not in result.stderr):
            failures.append(f"{path} ({body}): expected {detail} {citation}, got {result.stderr.strip()}")
    if failures:
        raise SystemExit("\n".join(failures))

"""Check the citation printed by the installed driver on mixed failures."""
from pathlib import Path
from subprocess import run
from tempfile import TemporaryDirectory

driver = Path(".lake/build/bin/concrete").resolve()
cases = [
    ("main.con", "let x: u8 = 1; let x {} = x; return 0;", "Slice0Rules.AdmittedProduction@2", "file 0", "39..40", ""),
    ("main.con", "let Option {} = 1; return 0;", "Slice0Rules.AdmittedProduction@2", "file 0", "29..30", ""),
    ("main.con", "let main {} = 1; return 0;", "Slice0Rules.AdmittedProduction@2", "file 0", "27..28", ""),
    ("main.con", "return 01;", "Slice0Rules.Lexed@2", "file 0", "26..27", "leading zero"),
    ("main.con", "return 1z;", "Slice0Rules.Lexed@2", "file 0", "26..27", "invalid integer suffix"),
    ("main.con", r'return "a\q";', "Slice0Rules.Lexed@2", "file 0", "28..29", "invalid string escape (113)"),
    ("main.con", r"return '\q';", "Slice0Rules.Lexed@2", "file 0", "27..28", "invalid byte escape (113)"),
    ("main.con", "return '!\0", "Slice0Rules.Lexed@2", "file 0", "27..28", "invalid source"),
    ("main.con", "return 1i\0", "Slice0Rules.Lexed@2", "file 0", "27..28", "invalid source"),
    ("main.con", "return 42u8;", "Slice0Rules.ExpectedTypeLiteral@1", "file 0", "25..29", ""),
    ("main.con", "return 0x;", "Slice0Rules.Lexed@2", "file 0", "27..28", "missing digits"),
    ("main.con", "return '!x';", "Slice0Rules.Lexed@2", "file 0", "27..28", "unterminated byte literal"),
    ("main.con", "return 'é';", "Slice0Rules.Lexed@2", "file 0", "26..28", "forbidden byte"),
    ("main.con", 'return "é\u2028";', "Slice0Rules.Lexed@2", "file 0", "28..31", "forbidden string"),
    ("main.con", r'return "é\n\q";', "Slice0Rules.Lexed@2", "file 0", "31..32", "invalid string escape"),
    ("main.con", "return 1unknown;", "Slice0Rules.Lexed@2", "file 0", "26..33", "invalid integer suffix"),
    ("main.con", "return " + "1" * 1025 + ";", "Slice0Rules.WithinLimit@1", "file 0", "1049..1050", "exceeds limit"),
    ("a b.con", "return 0;", "SourcePath.NoSpaces@1", "path", "1..2", ""),
    ("con.con", "return 0;", "SourcePath.NoReservedStem@1", "path", "0..1", ""),
]

with TemporaryDirectory() as work:
    failures = []
    for path, body, rule, domain, span, detail in cases:
        (Path(work) / path).write_text(f"fn main() -> u8 {{ {body} }}")
        result = run([driver, "build", path, "-o", "out"], cwd=work, capture_output=True, text=True)
        citation = f"[rule ConcreteSpec.Core.{rule}, {domain}, bytes {span}]"
        expected_exit = 2 if rule == "Slice0Rules.WithinLimit@1" else 1
        if (result.returncode != expected_exit or result.stdout or result.stderr.count("[rule ") != 1
                or not result.stderr.rstrip().endswith(citation) or detail not in result.stderr):
            failures.append(f"{path} ({body}): expected {detail} {citation}, got {result.stderr.strip()}")
    if failures:
        raise SystemExit("\n".join(failures))

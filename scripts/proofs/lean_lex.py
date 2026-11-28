#!/usr/bin/env python3
"""A lexer for Lean sources, shared by the specification and compiler trust checks.

`lex(src)` returns `src` with every comment (nested `/- -/`, `--`) blanked to spaces, newlines kept,
so offsets and line numbers are the source's. Everything else is kept, string literals included.
It follows Lean's literal forms, so no comment delimiter inside a literal starts a comment: a
string with its escapes, raw strings r"…" and r#"…"#, character literals and quoted names «…».
Whether a string interpolates depends on the parser, not the lexer, so interpolation is admitted
only after `s!`, `m!` or `f!`, allowing Lean whitespace and comments. Their `{ }` code,
with its own strings and comments, is lexed recursively; and any other string holding `{` is an error. An unterminated string,
comment, interpolation or name is an error too, and so is a syntax quotation `(…) (whose comments are
syntax data; the library runs no meta code): the lexer fails closed, never skips.
`comment_spans(src)` gives the top-level comments (the owner view, scripts/spec/lean_site.py, takes
its doc blocks from them, so a doc comment inside an ordinary comment or a string is not one).

Commands (exit 3 when a source cannot be read or lexed):

  scan RE FILE...          `file:line: text` for every match of RE (over the whole lexed text,
                           reported at the line where it starts) and for every `import` outside
                           the header, which must be lines `import ConcreteSpec[.M…]`, one
                           module each, ended by a command at column 0
  examples SRC DST FILE... copies each FILE (a path relative to SRC) to DST with every `example`
                           command turned into `noncomputable abbrev «⟪example⟫FILE:LINE»` (no space,
                           so listings split on spaces stay sound), so the kernel's view of it
                           stays in the environment; prints the count. Every
                           `example` token must start a line.
"""
import os, re, sys, subprocess
from pathlib import Path

EXAMPLE_MARK = "⟪example⟫"


class LexError(Exception):
    pass


def is_tactics_path(path):
    """True for tactic-family modules under `ConcreteSpec/Tactics/` (not the root barrel)."""
    parts = Path(path).parts
    return len(parts) >= 3 and parts[0] == "ConcreteSpec" and parts[1] == "Tactics"


def lex(src, spans=None, literals=True, allow_quotations=False):
    """`src` with its comments blanked; when `spans` is a list, each top-level comment's
    (start, end) offsets are appended to it (a comment nested in another is part of that one).
    With `literals=False`, string, character and name literals are blanked too (code tokens only).
    With `allow_quotations=True` (used only for `ConcreteSpec/Tactics/`), balanced syntax
    quotations `` `(…) `` and ``` ``(…) ``` are lexed with their code tokens kept visible and
    comments inside quotations refused fail-closed."""
    n, out = len(src), []

    def emit(text, literal=False):
        if literal and not literals:
            text = "".join("\n" if ch == "\n" else " " for ch in text)
        out.append(text)

    def ident(i):                                     # src[i] continues an identifier
        if i < 0: return False
        c, v = src[i], ord(src[i])
        # Init.Meta.Defs.isIdRest; Python's Unicode isalnum is broader than Lean's.
        return (c.isascii() and c.isalnum()) or c in "_'!?." or (
            0x3b1 <= v <= 0x3c9 and v != 0x3bb or
            0x391 <= v <= 0x3a9 and v not in (0x3a0, 0x3a3) or
            any(a <= v <= b for a, b in ((0x3ca, 0x3fb), (0x1f00, 0x1ffe),
                (0x2100, 0x214f), (0x1d49c, 0x1d59f), (0x100, 0x17f),
                (0x2080, 0x2089), (0x2090, 0x209c), (0x1d62, 0x1d6a))) or
            0xc0 <= v <= 0xff and v not in (0xd7, 0xf7) or v == 0x2c7c)

    def comment(i):                                   # at "/-": blank to the matching "-/"
        start, depth = i, 0
        while i < n:
            if src.startswith("/-", i): depth += 1; out.append("  "); i += 2
            elif src.startswith("-/", i):
                depth -= 1; out.append("  "); i += 2
                if depth == 0:
                    if spans is not None: spans.append((start, i))
                    return i
            else: out.append("\n" if src[i] == "\n" else " "); i += 1
        raise LexError("unterminated comment")

    def escape(i, in_string, interp=False):
        # Lean.Parser.Basic.quotedCharCoreFn and stringGapFn (pinned toolchain).
        j = i + 1
        if j >= n: raise LexError("unterminated escape")
        c = src[j]
        if c in "\\\"'rnt" or (interp and c == "{"): return j + 1
        width = {"x": 2, "u": 4}.get(c)
        if width is not None:
            end = j + 1 + width
            if end <= n and re.fullmatch(r"[0-9a-fA-F]+", src[j + 1:end]): return end
            raise LexError("invalid hexadecimal escape")
        if in_string and c == "\n":
            j += 1
            while j < n and src[j] in " \t\r": j += 1
            if j < n and src[j] == "\n": raise LexError("additional newline in string gap")
            return j
        raise LexError("invalid escape")

    def string(i, interp=False):                      # at '"'
        emit('"', True); i += 1
        while i < n:
            c = src[i]
            if c == "\\":
                j = escape(i, True, interp); emit(src[i:j], True); i = j
            elif c == '"': emit(c, True); return i + 1
            elif c == "{" and interp: emit(c, True); i = code(i + 1, True)
            elif c == "{": raise LexError("ambiguous interpolation in an ordinary string; use a raw string")
            else: emit(c, True); i += 1
        raise LexError("unterminated string")

    def raw(i):                                       # at 'r' of r#*"
        j = i + 1
        while j < n and src[j] == "#": j += 1
        close = '"' + "#" * (j - i - 1)
        k = src.find(close, j + 1)
        if k < 0: raise LexError("unterminated raw string")
        emit(src[i:k + len(close)], True); return k + len(close)

    def quotation(i):                                 # at "`(" or "``(": balanced parens, no comments
        start_line = src.count("\n", 0, i) + 1
        prefix_len = 2 if src.startswith("`(", i) else 3
        out.append(src[i:i + prefix_len]); i += prefix_len
        depth, prefix = 1, False
        while i < n and depth > 0:
            c = src[i]
            if not (c in " \t\r\n\"" or src.startswith(("s!", "m!", "f!"), i)):
                prefix = False
            if src.startswith("/-", i) or src.startswith("--", i):
                raise LexError(f"line {src.count(chr(10), 0, i) + 1}: comment inside syntax quotation")
            elif c in " \t\r\n": out.append(c); i += 1
            elif src[i:i + 2] in ("s!", "m!", "f!") and not ident(i - 1):
                out.append(src[i:i + 2]); i += 2; prefix = True
            elif c == '"': i = string(i, prefix); prefix = False
            elif c == "«":
                k = src.find("»", i + 1)
                if k < 0: raise LexError("unterminated «name»")
                emit(src[i:k + 1], True); i = k + 1
            elif c == "r" and not ident(i - 1) and re.match(r'r#*"', src[i:]): i = raw(i)
            elif src.startswith("'(", i) and not src.startswith("'('", i):
                out.append(c); i += 1; prefix = False
            elif c == "'" and not ident(i - 1):
                j = escape(i + 1, False) if i + 1 < n and src[i + 1] == "\\" else i + 2
                if j >= n or src[j] != "'": raise LexError(f"unclassified character literal at line {src.count(chr(10), 0, i) + 1}")
                emit(src[i:j + 1], True); i = j + 1; prefix = False
            elif c == "(": depth += 1; out.append(c); i += 1
            elif c == ")": depth -= 1; out.append(c); i += 1
            else: out.append(c); i += 1
        if depth != 0: raise LexError(f"line {start_line}: unterminated syntax quotation")
        return i

    def code(i, inner):                               # inner: inside an interpolation, stop at its '}'
        depth, prefix = 0, False
        while i < n:
            c = src[i]
            if not (c in " \t\r\n\"" or src.startswith(("/-", "--", "s!", "m!", "f!"), i)):
                prefix = False
            if src.startswith("/-", i): i = comment(i)
            elif src.startswith("--", i):
                j = src.find("\n", i); j = n if j < 0 else j; out.append(" " * (j - i)); i = j
            elif c in " \t\r\n": out.append(c); i += 1
            elif src[i:i + 2] in ("s!", "m!", "f!") and not ident(i - 1):
                out.append(src[i:i + 2]); i += 2; prefix = True
            elif c == '"': i = string(i, prefix); prefix = False
            elif c == "«":                            # a quoted name part: no comment inside it
                k = src.find("»", i + 1)
                if k < 0: raise LexError("unterminated «name»")
                emit(src[i:k + 1], True); i = k + 1
            elif src.startswith("`(", i) or src.startswith("``(", i):
                if allow_quotations:
                    i = quotation(i)
                else:
                    # A syntax quotation: its comments are syntax data, not comments of the source,
                    # and the library outside ConcreteSpec/Tactics/ runs no meta code, so refuse it.
                    raise LexError(f"line {src.count(chr(10), 0, i) + 1}: a syntax quotation")
            elif c == "r" and not ident(i - 1) and re.match(r'r#*"', src[i:]): i = raw(i)
            elif src.startswith("'(", i) and not src.startswith("'('", i):
                # Explicit getElem proof: xs[i]'(by ...), not a character.
                out.append(c); i += 1; prefix = False
            elif c == "'" and not ident(i - 1):
                j = escape(i + 1, False) if i + 1 < n and src[i + 1] == "\\" else i + 2
                if j >= n or src[j] != "'": raise LexError(f"unclassified character literal at line {src.count(chr(10), 0, i) + 1}")
                emit(src[i:j + 1], True); i = j + 1; prefix = False
            elif inner and c == "{": depth += 1; out.append(c); i += 1
            elif inner and c == "}":
                out.append(c); i += 1
                if depth == 0: return i
                depth -= 1
            else: out.append(c); i += 1
        if inner: raise LexError("unterminated interpolation")
        return i

    code(0, False)
    return "".join(out)


def comment_spans(src, allow_quotations=False):
    """The (start, end) offsets of every top-level comment of `src`, in order."""
    spans = []
    lex(src, spans, allow_quotations=allow_quotations)
    return spans


HEADER = re.compile(r"import ConcreteSpec(\.[A-Za-z0-9_]+)*\s*$")
TACTICS_HEADER = re.compile(r"import ConcreteSpec\.Tactics(\.[A-Za-z0-9_]+)*\s*$")
EVIDENCE_HEADER = re.compile(r"import ConcreteSpec(Evidence)?(\.[A-Za-z0-9_]+)*\s*$")
class ModuleHeader:
    """One import per line, drawn from the approved roots' actual module graph.

    No arbitrary module on LEAN_PATH, nor a nonexistent descendant of an approved
    root, is authorized. Closure coverage is independently checked in Lean.
    """
    def __init__(self, modules):
        self.modules = frozenset(modules)

    def match(self, line):
        return line.startswith("import ") and line[7:].rstrip() in self.modules


def module_imports(source, modules, allow_quotations=False, first_command=None):
    """Return (module names, import-line spans, header end) using source offsets.

    Comments and blank lines are legal anywhere in the header. Keep their bytes
    separate from import lines so header rewrites preserve documentation.
    """
    header = ModuleHeader(modules)
    cmd = FIRST_COMMAND if first_command is None else first_command
    lines = lex(source, literals=False, allow_quotations=allow_quotations).splitlines(keepends=True)
    deps, spans = [], []
    k = offset = 0
    while k < len(lines) and (not lines[k].strip() or header.match(lines[k])):
        if lines[k].strip():
            deps.append(lines[k][7:].strip())
            # Replace only code, retaining trailing comments and line endings.
            end = len(lines[k].rstrip())
            spans.append((offset, offset + end))
        offset += len(lines[k])
        k += 1
    if k < len(lines) and not cmd.match(lines[k]):
        raise LexError('invalid import header terminator')
    if any(re.search(r"\bimport\b", line) for line in lines[k:]):
        raise LexError('import outside header')
    return deps, spans, offset


def compiler_header():
    prefix = Path(subprocess.check_output(["lean", "--print-prefix"], text=True).strip())
    modules = set()
    for base, root in ((Path("."), "ConcreteSpec"), (Path("."), "Concrete"),
                       (prefix / "src/lean", "Lean"),
                       (Path(".lake/packages/lean-compiler-backend"), "LeanCompilerBackend")):
        if not (base / (root + ".lean")).is_file() or not (base / root).is_dir():
            raise LexError(f"missing approved module graph: {base / root}")
        modules.add(root)
        modules.update(".".join(p.relative_to(base).with_suffix("").parts)
                       for p in (base / root).rglob("*.lean"))
    # SSA's indexed slot correspondence uses this proof-bearing container from
    # the pinned toolchain. Permit this module in compiler sources only.
    hash_map_lemmas = "Std.Data.HashMap.Lemmas"
    source = prefix / "src/lean" / (hash_map_lemmas.replace(".", "/") + ".lean")
    if not source.is_file():
        raise LexError(f"missing approved module: {source}")
    modules.add(hash_map_lemmas)
    return ModuleHeader(modules)


FIRST_COMMAND = re.compile(r"(namespace|open|section|def|theorem|structure|inductive|abbrev)\b")
FIRST_COMMAND_TACTICS = re.compile(
    r"(namespace|open|section|def|theorem|structure|inductive|abbrev|syntax|macro|macro_rules|elab|elab_rules)\b")


def scan(pattern, paths, header=HEADER, code_pattern=None, allow_quotations=False, first_command=FIRST_COMMAND):
    word, rc = re.compile(pattern, re.M), 0
    code_word = re.compile(code_pattern, re.M) if code_pattern else None
    for path in paths:
        try:
            source = Path(path).read_text(encoding="utf-8")
            text = lex(source, allow_quotations=allow_quotations)
            code_text = lex(source, literals=False, allow_quotations=allow_quotations)
            code_lines = code_text.split("\n")
        except (OSError, UnicodeDecodeError, LexError) as e:
            print(f"error: {path}: {e}", file=sys.stderr); rc = 3; continue
        lines = text.split("\n")
        # Lean lets an import list continue (`import A B`, or B on the next line), so the first
        # line after the header must be a command at column 0.
        k = 0
        while k < len(lines) and (not lines[k].strip() or header.match(lines[k])): k += 1
        if k < len(lines) and not first_command.match(lines[k]):
            print(f"{path}:{k + 1}: {lines[k].strip()} (the import header must end in a command at column 0)")
        hits = {text.count("\n", 0, m.start()) + 1 for m in word.finditer(text)}
        if code_word is not None:
            hits |= {code_text.count("\n", 0, m.start()) + 1 for m in code_word.finditer(code_text)}
        # Banned trust words include literal payloads. Imports and keyword bans are Lean code tokens.
        hits |= {j for j, line in enumerate(code_lines[k:], k + 1) if re.search(r"\bimport\b", line)}
        for j in sorted(hits):
            print(f"{path}:{j}: {lines[j - 1].strip()}")
    return rc


# `example` as a command token: not part of a longer identifier (`example'`, `my.example`, `examples`)
EXAMPLE = re.compile(r"(?<![\w.'!?])example(?![\w'!?])")


def count_examples(src, allow_quotations=False):
    """The number of `example` commands in a source (code tokens only, not literal contents)."""
    return len(EXAMPLE.findall(lex(src, literals=False, allow_quotations=allow_quotations)))


def rename_examples(src, name, allow_quotations=False):
    """Retain examples as abbreviations; name maps each source line to a fresh name."""
    text = lex(src, literals=False, allow_quotations=allow_quotations)
    out, last = [], 0
    for m in EXAMPLE.finditer(text):
        line = text.count("\n", 0, m.start()) + 1
        if m.start() > 0 and text[m.start() - 1] != "\n":
            raise LexError(f"{line}: an `example` that does not start its line")
        out.append(src[last:m.start()])
        out.append(f"noncomputable abbrev «{name(line)}»")
        last = m.end()
    out.append(src[last:])
    return "".join(out)


def examples(src_dir, dst_dir, paths):
    count = 0
    for rel in paths:
        src = (Path(src_dir) / rel).read_text(encoding="utf-8")
        allow_q = is_tactics_path(rel)
        try:
            renamed = rename_examples(src, lambda line: f"{EXAMPLE_MARK}{rel}:{line}",
                                      allow_quotations=allow_q)
        except LexError as e:
            raise LexError(f"{rel}:{e}") from e
        count += count_examples(src, allow_quotations=allow_q)
        dst = os.path.join(dst_dir, rel)
        os.makedirs(os.path.dirname(dst) or ".", exist_ok=True)
        Path(dst).write_text(renamed, encoding="utf-8")
    print(count)


def _parse_scan_args(argv):
    code_re = None
    i = 2
    if i + 1 < len(argv) and argv[i] == "--code-re":
        code_re = argv[i + 1]
        i += 2
    return code_re, argv[i], argv[i + 1:]


if __name__ == "__main__":
    try:
        if sys.argv[1] == "scan-compiler":
            code_re, pat, files = _parse_scan_args(sys.argv)
            sys.exit(scan(pat, files, compiler_header(), code_pattern=code_re))
        if sys.argv[1] == "scan-evidence":
            code_re, pat, files = _parse_scan_args(sys.argv)
            sys.exit(scan(pat, files, EVIDENCE_HEADER, code_pattern=code_re))
        if sys.argv[1] == "scan-tactics":
            code_re, pat, files = _parse_scan_args(sys.argv)
            sys.exit(scan(pat, files, TACTICS_HEADER, code_pattern=code_re,
                          allow_quotations=True, first_command=FIRST_COMMAND_TACTICS))
        if sys.argv[1] == "scan":
            code_re, pat, files = _parse_scan_args(sys.argv)
            sys.exit(scan(pat, files, code_pattern=code_re))
        if sys.argv[1] == "examples":
            examples(sys.argv[2], sys.argv[3], sys.argv[4:]); sys.exit(0)
    except (OSError, UnicodeDecodeError, LexError, subprocess.SubprocessError) as e:
        print(f"error: {e}", file=sys.stderr); sys.exit(3)
    print(f"usage: {sys.argv[0]} scan [--code-re RE] RE FILE... | scan-tactics [--code-re RE] RE FILE... | examples SRC DST FILE...", file=sys.stderr)
    sys.exit(2)

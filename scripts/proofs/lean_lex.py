#!/usr/bin/env python3
"""A lexer for the Lean sources of ConcreteSpec, shared by the proof-integrity checks.

`lex(src)` returns `src` with every comment (nested `/- -/`, `--`) blanked to spaces, newlines kept,
so offsets and line numbers are the source's. Everything else is kept, string literals included.
It follows Lean's literal forms, so no comment delimiter inside a literal starts a comment: a
string with its escapes, raw strings r"…" and r#"…"#, character literals and quoted names «…».
Whether a string interpolates depends on the parser, not the lexer, so interpolation is admitted
only directly after `s!`, `m!` or `f!` (its `{ }` code, with its own strings and comments, is
lexed recursively), and any other string holding `{` is an error. An unterminated string,
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
                           command turned into `noncomputable def «⟪example⟫FILE:LINE»` (no space,
                           so listings split on spaces stay sound), so the kernel's view of it
                           stays in the environment; prints the count. Every
                           `example` token must start a line.
  compiler-examples ...    the same rewrite for Concrete, whose IR printer strings may contain
                           ordinary `{` and `}` characters. Spec scanning/rewriting is unchanged.
"""
import os, re, sys

EXAMPLE_MARK = "⟪example⟫"


class LexError(Exception):
    pass


def lex(src, spans=None, literals=True, ordinary_braces=False):
    """`src` with its comments blanked; when `spans` is a list, each top-level comment's
    (start, end) offsets are appended to it (a comment nested in another is part of that one).
    With `literals=False`, string, character and name literals are blanked too (code tokens only).
    `ordinary_braces` admits braces in non-interpolated strings, needed by the compiler's IR printers."""
    n, out = len(src), []

    def emit(text, literal=False):
        if literal and not literals:
            text = "".join("\n" if ch == "\n" else " " for ch in text)
        out.append(text)

    def ident(i):                                     # src[i] continues an identifier
        return i >= 0 and (src[i].isalnum() or src[i] in "_'!?." or ord(src[i]) > 127)

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

    def string(i):                                    # at '"'
        interp = src[i - 2:i] in ("s!", "m!", "f!") and not ident(i - 3)
        emit('"', True); i += 1
        while i < n:
            c = src[i]
            if c == "\\": emit(src[i:i + 2], True); i += 2
            elif c == '"': emit(c, True); return i + 1
            elif c == "{" and interp: emit(c, True); i = code(i + 1, True)
            elif c == "{" and not ordinary_braces:
                line = src.count("\n", 0, i) + 1
                raise LexError(f"line {line}: a string holding '{{' without s!, m! or f! before it")
            else: emit(c, True); i += 1
        raise LexError("unterminated string")

    def raw(i):                                       # at 'r' of r#*"
        j = i + 1
        while j < n and src[j] == "#": j += 1
        close = '"' + "#" * (j - i - 1)
        k = src.find(close, j + 1)
        if k < 0: raise LexError("unterminated raw string")
        emit(src[i:k + len(close)], True); return k + len(close)

    def code(i, inner):                               # inner: inside an interpolation, stop at its '}'
        depth = 0
        while i < n:
            c = src[i]
            if src.startswith("/-", i): i = comment(i)
            elif src.startswith("--", i):
                j = src.find("\n", i); j = n if j < 0 else j; out.append(" " * (j - i)); i = j
            elif c == '"': i = string(i)
            elif c == "«":                            # a quoted name part: no comment inside it
                k = src.find("»", i + 1)
                if k < 0: raise LexError("unterminated «name»")
                emit(src[i:k + 1], True); i = k + 1
            elif src.startswith("`(", i) or src.startswith("``(", i):
                # A syntax quotation: its comments are syntax data, not comments of the source,
                # and the library runs no meta code, so the lexer refuses it.
                raise LexError(f"line {src.count(chr(10), 0, i) + 1}: a syntax quotation")
            elif c == "r" and not ident(i - 1) and re.match(r'r#*"', src[i:]): i = raw(i)
            elif c == "'" and not ident(i - 1):
                m = re.match(r"'(\\(x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|.)|[^\\'\n])'", src[i:])
                k = len(m.group(0)) if m else 1
                emit(src[i:i + k], m is not None); i += k
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


def comment_spans(src):
    """The (start, end) offsets of every top-level comment of `src`, in order."""
    spans = []
    lex(src, spans)
    return spans


HEADER = re.compile(r"import ConcreteSpec(\.[A-Za-z0-9_]+)*\s*$")
FIRST_COMMAND = re.compile(r"(namespace|open|section|def|theorem|structure|inductive|abbrev)\b")


def scan(pattern, paths):
    word, rc = re.compile(pattern, re.M), 0
    for path in paths:
        try:
            text = lex(open(path, encoding="utf-8").read())
        except (OSError, UnicodeDecodeError, LexError) as e:
            print(f"error: {path}: {e}", file=sys.stderr); rc = 3; continue
        lines = text.split("\n")
        # Lean lets an import list continue (`import A B`, or B on the next line), so the first
        # line after the header must be a command at column 0.
        k = 0
        while k < len(lines) and (not lines[k].strip() or HEADER.match(lines[k])): k += 1
        if k < len(lines) and not FIRST_COMMAND.match(lines[k]):
            print(f"{path}:{k + 1}: {lines[k].strip()} (the import header must end in a command at column 0)")
        hits = {text.count("\n", 0, m.start()) + 1 for m in word.finditer(text)}
        hits |= {j for j, line in enumerate(lines, 1)
                 if re.search(r"\bimport\b", line) and not (j <= k and HEADER.match(line))}
        for j in sorted(hits):
            print(f"{path}:{j}: {lines[j - 1].strip()}")
    return rc


# `example` as a command token: not part of a longer identifier (`example'`, `my.example`, `examples`)
EXAMPLE = re.compile(r"(?<![\w.'!?])example(?![\w'!?])")


def count_examples(src):
    """The number of `example` commands in a source (code tokens only, not literal contents)."""
    return len(EXAMPLE.findall(lex(src, literals=False)))


def examples(src_dir, dst_dir, paths, ordinary_braces=False):
    count = 0
    for rel in paths:
        src = open(os.path.join(src_dir, rel), encoding="utf-8").read()
        text = lex(src, literals=False, ordinary_braces=ordinary_braces)
        out, last = [], 0
        for m in EXAMPLE.finditer(text):
            line = text.count("\n", 0, m.start()) + 1
            if m.start() > 0 and text[m.start() - 1] != "\n":
                raise LexError(f"{rel}:{line}: an `example` that does not start its line")
            out.append(src[last:m.start()])
            out.append(f"noncomputable def «{EXAMPLE_MARK}{rel}:{line}»")
            last = m.end(); count += 1
        out.append(src[last:])
        dst = os.path.join(dst_dir, rel)
        os.makedirs(os.path.dirname(dst) or ".", exist_ok=True)
        open(dst, "w", encoding="utf-8").write("".join(out))
    print(count)


if __name__ == "__main__":
    try:
        if sys.argv[1] == "scan":
            sys.exit(scan(sys.argv[2], sys.argv[3:]))
        if sys.argv[1] in ("examples", "compiler-examples"):
            examples(sys.argv[2], sys.argv[3], sys.argv[4:],
                     ordinary_braces=sys.argv[1] == "compiler-examples"); sys.exit(0)
    except (OSError, UnicodeDecodeError, LexError) as e:
        print(f"error: {e}", file=sys.stderr); sys.exit(3)
    print(f"usage: {sys.argv[0]} scan RE FILE... | (compiler-)examples SRC DST FILE...", file=sys.stderr)
    sys.exit(2)

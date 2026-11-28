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


def _is_lean_ident_rest(ch):
    """Init.Meta.Defs.isIdRest; Python's Unicode isalnum is broader than Lean's."""
    v = ord(ch)
    return (ch.isascii() and ch.isalnum()) or ch in "_'!?." or (
        0x3b1 <= v <= 0x3c9 and v != 0x3bb or
        0x391 <= v <= 0x3a9 and v not in (0x3a0, 0x3a3) or
        any(a <= v <= b for a, b in ((0x3ca, 0x3fb), (0x1f00, 0x1ffe),
            (0x2100, 0x214f), (0x1d49c, 0x1d59f), (0x100, 0x17f),
            (0x2080, 0x2089), (0x2090, 0x209c), (0x1d62, 0x1d6a))) or
        0xc0 <= v <= 0xff and v not in (0xd7, 0xf7) or v == 0x2c7c)


def lex(src, spans=None, literals=True, allow_quotations=False, quot_spans=None):
    """`src` with its comments blanked; when `spans` is a list, each top-level comment's
    (start, end) offsets are appended to it (a comment nested in another is part of that one).
    With `literals=False`, string, character and name literals are blanked too (code tokens only).
    With `allow_quotations=True` (used only for `ConcreteSpec/Tactics/`), balanced syntax
    quotations `` `(…) `` and ``` ``(…) ``` are lexed with their code tokens kept visible and
    comments inside quotations refused fail-closed; when `quot_spans` is a list, each top-level
    quotation's exact `(start, end)` offsets are appended to it."""
    n, out = len(src), []

    def emit(text, literal=False):
        if literal and not literals:
            text = "".join("\n" if ch == "\n" else " " for ch in text)
        out.append(text)

    def ident(i):                                     # src[i] continues an identifier
        if i < 0: return False
        return _is_lean_ident_rest(src[i])

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
                    q_start = i
                    i = quotation(i)
                    if not inner and quot_spans is not None:
                        quot_spans.append((q_start, i))
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
TACTICS_HEADER = re.compile(r"import ConcreteSpec\.Tactics(\.[A-Za-z0-9_]+)+\s*$")
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
FIRST_COMMAND_TACTICS = re.compile(r"(namespace|syntax|macro|macro_rules)\b")

# Whitelist grammar for `ConcreteSpec/Tactics/*.lean` (pure tactic templates only):
#
#   tactics_file     ::= import_line* top_item* EOF
#   import_line      ::= "import" "ConcreteSpec.Tactics." mod_path EOL
#   mod_path         ::= simple_ident ("." simple_ident)*
#   top_item         ::= "namespace" mod_path top_item* "end" mod_path
#                      | syntax_decl
#                      | macro_decl
#                      | macro_rules_decl
#   syntax_decl      ::= "syntax" named_name? str_lit syntax_item* ":" "tactic"
#   named_name       ::= "(" "name" ":=" simple_ident ")"
#   macro_decl       ::= "macro" str_lit macro_item* ":" "tactic" "=>" tactic_quot
#   macro_rules_decl ::= "macro_rules" macro_rule_arm+
#   macro_rule_arm   ::= "|" tactic_quot "=>" tactic_quot
#   syntax_item      ::= syntax_atom ("<|>" syntax_atom)*
#   syntax_atom      ::= syntax_primary syntax_quant?
#   syntax_primary   ::= str_lit | allowed_arg_cat | "(" syntax_item+ ")"
#   syntax_quant     ::= "?" | "*" | "+" | ",*" | ",+"
#   macro_item       ::= simple_ident ":" syntax_atom   -- bound parameter (no space before ':')
#                      | syntax_item
#   tactic_quot      ::= "`(" "tactic" "|" tactic_body ")"
#
# Inside `tactic_quot`:
#   - single backtick `` `(tactic| ...) `` only (no ```` ``( ````, no non-`tactic` category);
#   - no comments (already rejected by `lex`), no nested `` `( `` / ```` ``( ````,
#     no `$(...)` term antiquotations, no `%$` token antiquotations, no `$$`,
#     no interpolated strings (`s!"`, `m!"`, `f!"`), no `#<ident>` commands;
#   - only simple variable antiquotations `$x`, `$x:cat`, `$x:cat*`, `$x:cat,*`, `$x:cat,+`
#     (plus `$_` on LHS patterns only) and splice scopes `$[...]*`, `$[...],*`, `$[...];*`;
#   - every RHS `$x` must be a simple identifier bound on the corresponding LHS;
#   - no banned keywords/meta/syntax-construction tokens anywhere inside the quotation.

TACTICS_ALLOWED_ARG_CATS = frozenset({
    "ident", "term", "tactic", "tacticSeq", "num", "str",
    "colGt", "ppSpace", "location", "simpArg", "rwRule",
    "binderIdent", "rcasesPat",
})

TACTICS_QUOT_FORBIDDEN_WORDS = frozenset({
    "set_option", "run_tac", "run_cmd", "run_elab", "run_meta",
    "native_decide", "sorry", "sorryAx", "admit",
    "do", "elab", "elab_rules", "macro", "macro_rules", "syntax",
    "axiom", "implemented_by", "extern", "unsafe",
    "import", "namespace", "end", "open", "section", "attribute",
    "initialize", "builtin_initialize", "declare_syntax_cat",
    "notation", "infix", "infixl", "infixr", "prefix", "postfix",
    "dbg_trace", "bv_decide", "stop", "sleep", "eval_tactic", "evalTactic",
    "mkIdent", "mkAtom", "mkNode", "mkNullNode", "mkIdentFrom",
    "mkCIdent", "mkNumLit", "mkStrLit",
})

TACTICS_FORBIDDEN_QUALIFIED_SUFFIXES = frozenset({
    "Syntax.node", "Syntax.atom", "Syntax.ident", "Syntax.missing",
    "Syntax.mkNumLit", "Syntax.mkStrLit", "Syntax.mkCharLit",
    "Syntax.mkNameLit", "Syntax.mkApp",
    "Name.mkStr", "Name.mkSimple", "Name.mkNum",
    "Decide.Config",
})

TACTICS_FORBIDDEN_QUALIFIED_PREFIXES = (
    "Lean.Elab", "Lean.Meta", "Lean.Syntax", "Lean.Name",
    "Lean.Macro", "Lean.MacroM", "Lean.TSyntax",
)

TACTICS_FORBIDDEN_LAST_SEGMENTS = frozenset({
    "sorryAx", "ofReduceBool", "ofReduceNat", "trustCompiler", "skipKernelTC",
    "mkIdent", "mkAtom", "mkNode", "mkNullNode", "mkIdentFrom",
    "mkCIdent", "mkNumLit", "mkStrLit", "eval_tactic", "evalTactic",
})

_SIMPLE_IDENT_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_']*")
_MULTI_PUNCT = (":=", "=>", "<|>", ",*", ",+")
_SINGLE_PUNCT = frozenset("():|?*+.")


class TacticsWhitelistError(Exception):
    def __init__(self, line, message):
        super().__init__(message)
        self.line = line
        self.message = message


def _skip_string_lit(s, i):
    """Advance past a validated `"..."` string literal starting at `s[i] == '"'`."""
    n = len(s)
    i += 1
    while i < n:
        c = s[i]
        if c == "\\":
            i += 2
        elif c == '"':
            return i + 1
        else:
            i += 1
    return n


def _skip_raw_string_lit(s, i):
    """Advance past a validated `r#*"..."#*` raw string literal starting at `s[i] == 'r'`."""
    j = i + 1
    n = len(s)
    while j < n and s[j] == "#":
        j += 1
    close = '"' + "#" * (j - i - 1)
    k = s.find(close, j + 1)
    return n if k < 0 else k + len(close)


def _skip_char_lit(s, i):
    """Advance past a validated `'...'` char literal starting at `s[i] == "'"`."""
    n = len(s)
    if i + 1 < n and s[i + 1] == "\\":
        c = s[i + 2] if i + 2 < n else ""
        if c == "x":
            return min(n, i + 6)
        if c == "u":
            return min(n, i + 8)
        return min(n, i + 4)
    return min(n, i + 3)


def _tokenize_tactics(text, start_offset, start_line, quot_span_map):
    """Tokenize comment-stripped Tactics body into `(kind, value, line, space_before)` tuples.
    Quotation boundaries come directly from `lex(..., quot_spans=...)` so the lexer and
    whitelist parser can never disagree on quotation spans."""
    tokens = []
    n = len(text)
    i = start_offset
    line = start_line
    space_before = True
    while i < n:
        c = text[i]
        if c in " \t\r\n":
            if c == "\n":
                line += 1
            space_before = True
            i += 1
            continue
        if i in quot_span_map:
            end = quot_span_map[i]
            tok_text = text[i:end]
            tokens.append(("QUOT", tok_text, line, space_before))
            line += tok_text.count("\n")
            i = end
            space_before = False
            continue
        if c == '"':
            end = _skip_string_lit(text, i)
            tok_text = text[i:end]
            tokens.append(("STR", tok_text, line, space_before))
            line += tok_text.count("\n")
            i = end
            space_before = False
            continue
        matched_op = None
        for op in _MULTI_PUNCT:
            if text.startswith(op, i):
                matched_op = op
                break
        if matched_op is not None:
            tokens.append(("PUNCT", matched_op, line, space_before))
            i += len(matched_op)
            space_before = False
            continue
        if c in _SINGLE_PUNCT:
            tokens.append(("PUNCT", c, line, space_before))
            i += 1
            space_before = False
            continue
        m = _SIMPLE_IDENT_RE.match(text, i)
        if m is not None:
            tokens.append(("IDENT", m.group(0), line, space_before))
            i = m.end()
            space_before = False
            continue
        # Any other character/token (`#eval`, `@[`, `«...»`, digits, etc.) is rejected by the whitelist.
        j = i + 1
        while j < n and text[j] not in " \t\r\n()\"":
            j += 1
        tok_text = text[i:j]
        tokens.append(("OTHER", tok_text, line, space_before))
        line += tok_text.count("\n")
        i = j
        space_before = False
    tokens.append(("EOF", "", line, True))
    return tokens


def _check_dotted_ident_in_quot(ident_text, line):
    """Reject forbidden keywords, syntax/name constructors, and Lean.Elab/Lean.Meta in quotations."""
    segs = ident_text.split(".")
    if segs[0] in TACTICS_QUOT_FORBIDDEN_WORDS:
        raise TacticsWhitelistError(line, f"forbidden keyword/token '{segs[0]}' inside tactic quotation")
    if segs[-1] in TACTICS_FORBIDDEN_LAST_SEGMENTS:
        raise TacticsWhitelistError(line, f"forbidden meta/syntax token '{ident_text}' inside tactic quotation")
    for pref in TACTICS_FORBIDDEN_QUALIFIED_PREFIXES:
        if ident_text == pref or ident_text.startswith(pref + "."):
            raise TacticsWhitelistError(line, f"forbidden Lean meta reference '{ident_text}' inside tactic quotation")
    for k in range(len(segs) - 1):
        pair = segs[k] + "." + segs[k + 1]
        if pair in TACTICS_FORBIDDEN_QUALIFIED_SUFFIXES:
            raise TacticsWhitelistError(line, f"forbidden syntax/name constructor '{ident_text}' inside tactic quotation")


def _validate_tactic_quotation(quot_text, start_line, *, is_lhs, bound_vars=None):
    """Validate a single `` `(tactic| ...) `` quotation and return bound pattern vars (if `is_lhs`)."""
    if quot_text.startswith("``("):
        raise TacticsWhitelistError(start_line, "double-backtick quotation ``(...) is not allowed in Tactics")
    if not (quot_text.startswith("`(") and quot_text.endswith(")")):
        raise TacticsWhitelistError(start_line, "malformed syntax quotation in Tactics")

    inner = quot_text[2:-1]
    i, n = 0, len(inner)
    line = start_line
    while i < n and inner[i] in " \t\r\n":
        if inner[i] == "\n":
            line += 1
        i += 1
    cat_m = re.match(r"([A-Za-z_«][A-Za-z0-9_'»]*)\s*\|", inner[i:])
    if cat_m is None:
        raise TacticsWhitelistError(line, "syntax quotation must specify `(tactic| ...)")
    cat = cat_m.group(1)
    if cat != "tactic":
        raise TacticsWhitelistError(line, f"non-tactic quotation category '{cat}'; only `(tactic| ...) is allowed")
    line += cat_m.group(0).count("\n")
    i += cat_m.end()

    lhs_vars = set()
    rhs_allowed = set() if bound_vars is None else set(bound_vars)
    bracket_stack = []  # True for `$[...]` splice scopes, False for ordinary `[...]`

    while i < n:
        c = inner[i]
        if c == "\n":
            line += 1
            i += 1
            continue
        if c in " \t\r":
            i += 1
            continue
        # Disallow interpolated strings inside tactic quotations.
        if inner[i:i + 2] in ("s!", "m!", "f!") and (i == 0 or not _is_lean_ident_rest(inner[i - 1])):
            k = i + 2
            while k < n and inner[k] in " \t\r\n":
                k += 1
            if k < n and inner[k] == '"':
                raise TacticsWhitelistError(line, f"interpolated string '{inner[i:i + 2]}\"...\"' is not allowed inside `(tactic| ...)")
        if c == '"':
            end = _skip_string_lit(inner, i)
            line += inner[i:end].count("\n")
            i = end
            continue
        if c == "«":
            k = inner.find("»", i + 1)
            end = n if k < 0 else k + 1
            line += inner[i:end].count("\n")
            i = end
            continue
        if c == "r" and (i == 0 or not _is_lean_ident_rest(inner[i - 1])) and re.match(r'r#*"', inner[i:]):
            end = _skip_raw_string_lit(inner, i)
            line += inner[i:end].count("\n")
            i = end
            continue
        if inner.startswith("'(", i) and not inner.startswith("'('", i):
            i += 1
            continue
        if c == "'" and (i == 0 or not _is_lean_ident_rest(inner[i - 1])):
            end = _skip_char_lit(inner, i)
            line += inner[i:end].count("\n")
            i = end
            continue
        if inner.startswith("``(", i) or inner.startswith("`(", i):
            raise TacticsWhitelistError(line, "nested syntax quotation is not allowed inside `(tactic| ...)")
        if inner.startswith("%$", i):
            raise TacticsWhitelistError(line, "token antiquotation '%$' is not allowed in Tactics")
        if c == "$":
            if inner.startswith("$$", i):
                raise TacticsWhitelistError(line, "escaped antiquotation '$$' is not allowed in Tactics")
            if inner.startswith("$(", i):
                raise TacticsWhitelistError(line, "term antiquotation '$(...)' is not allowed in Tactics")
            if inner.startswith("$[", i):
                bracket_stack.append(True)
                i += 2
                continue
            m = _SIMPLE_IDENT_RE.match(inner, i + 1)
            if m is None:
                raise TacticsWhitelistError(line, "only simple variable antiquotations ($x, $x:cat) and splices ($[...]) are allowed in Tactics")
            var_name = m.group(0)
            j = m.end()
            if j < n and inner[j] == ".":
                raise TacticsWhitelistError(line, f"qualified antiquotation '${var_name}.*' is not allowed in Tactics")
            if var_name == "_":
                if not is_lhs:
                    raise TacticsWhitelistError(line, "wildcard antiquotation '$_' is not allowed on macro RHS")
            else:
                if var_name in TACTICS_QUOT_FORBIDDEN_WORDS:
                    raise TacticsWhitelistError(line, f"forbidden antiquotation identifier '${var_name}'")
                if is_lhs:
                    lhs_vars.add(var_name)
                elif var_name not in rhs_allowed:
                    raise TacticsWhitelistError(line, f"unbound antiquotation '${var_name}' on macro RHS")
            if j < n and inner[j] == ":":
                cat_ant = _SIMPLE_IDENT_RE.match(inner, j + 1)
                if cat_ant is None:
                    raise TacticsWhitelistError(line, f"malformed antiquotation category after '${var_name}:'")
                ant_cat = cat_ant.group(0)
                if ant_cat not in TACTICS_ALLOWED_ARG_CATS:
                    raise TacticsWhitelistError(line, f"disallowed antiquotation category '{ant_cat}'")
                j = cat_ant.end()
                for suf in (",*", ",+", "*", "+", "?"):
                    if inner.startswith(suf, j):
                        j += len(suf)
                        break
            i = j
            continue
        if c == "[":
            bracket_stack.append(False)
            i += 1
            continue
        if c == "]":
            is_splice = bracket_stack.pop() if bracket_stack else False
            i += 1
            if is_splice:
                matched_suf = None
                for suf in (",*", ";*", "*", "?"):
                    if inner.startswith(suf, i):
                        matched_suf = suf
                        break
                if matched_suf is None:
                    raise TacticsWhitelistError(line, "antiquotation splice '$[...]' must end with '*', ',*', ';*' or '?'")
                i += len(matched_suf)
            continue
        if c == "#":
            m = _SIMPLE_IDENT_RE.match(inner, i + 1)
            if m is not None:
                raise TacticsWhitelistError(line, f"command '#{m.group(0)}' is not allowed inside `(tactic| ...)")
            i += 1
            continue
        m = re.match(r"[A-Za-z_][A-Za-z0-9_']*(?:\.[A-Za-z_][A-Za-z0-9_']*)*", inner[i:])
        if m is not None:
            _check_dotted_ident_in_quot(m.group(0), line)
            i += m.end()
            continue
        i += 1

    if bracket_stack:
        raise TacticsWhitelistError(line, "unclosed '[' or '$[' inside `(tactic| ...)")
    return lhs_vars


class _TacticsWhitelistParser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def peek(self, offset=0):
        idx = self.pos + offset
        if idx >= len(self.tokens):
            return self.tokens[-1]
        return self.tokens[idx]

    def advance(self):
        tok = self.peek()
        if tok[0] != "EOF":
            self.pos += 1
        return tok

    def expect(self, kind, value=None, msg=None):
        tok = self.peek()
        if tok[0] != kind or (value is not None and tok[1] != value):
            desc = msg or f"expected {value!r}" if value is not None else f"expected {kind}"
            got = tok[1] if tok[0] != "EOF" else "end of file"
            raise TacticsWhitelistError(tok[2], f"{desc}, got {got!r}")
        return self.advance()

    def parse_mod_path(self):
        first = self.expect("IDENT", msg="expected namespace identifier")
        parts = [first[1]]
        while self.peek()[0] == "PUNCT" and self.peek()[1] == "." and not self.peek()[3]:
            self.advance()
            nxt = self.peek()
            if nxt[0] != "IDENT" or nxt[3]:
                raise TacticsWhitelistError(nxt[2], f"expected identifier after '.', got {nxt[1]!r}")
            parts.append(self.advance()[1])
        return ".".join(parts)

    def parse_str_lit(self, require_nonempty=False):
        tok = self.expect("STR", msg="expected string literal")
        if require_nonempty and not tok[1][1:-1].strip():
            raise TacticsWhitelistError(tok[2], "leading tactic syntax string literal must be non-empty")
        return tok

    def parse_syntax_primary(self):
        tok = self.peek()
        if tok[0] == "STR":
            self.advance()
            return
        if tok[0] == "IDENT":
            if tok[1] not in TACTICS_ALLOWED_ARG_CATS:
                raise TacticsWhitelistError(tok[2], f"disallowed syntax argument category '{tok[1]}'")
            self.advance()
            return
        if tok[0] == "PUNCT" and tok[1] == "(":
            self.advance()
            if self.peek()[0] == "PUNCT" and self.peek()[1] == ")":
                raise TacticsWhitelistError(self.peek()[2], "empty group '()' in syntax declaration")
            while not (self.peek()[0] == "PUNCT" and self.peek()[1] == ")"):
                if self.peek()[0] == "EOF":
                    raise TacticsWhitelistError(tok[2], "unclosed '(' in syntax declaration")
                self.parse_syntax_item()
            self.expect("PUNCT", ")")
            return
        got = tok[1] if tok[0] != "EOF" else "end of file"
        raise TacticsWhitelistError(tok[2], f"unexpected token {got!r} in syntax item")

    def parse_syntax_atom(self):
        self.parse_syntax_primary()
        if self.peek()[0] == "PUNCT" and self.peek()[1] in ("?", "*", "+", ",*", ",+"):
            self.advance()

    def parse_syntax_item(self):
        self.parse_syntax_atom()
        while self.peek()[0] == "PUNCT" and self.peek()[1] == "<|>":
            self.advance()
            self.parse_syntax_atom()

    def parse_syntax_decl(self):
        self.expect("IDENT", "syntax")
        if self.peek()[0] == "PUNCT" and self.peek()[1] == "(":
            open_tok = self.advance()
            self.expect("IDENT", "name", msg="only '(name := <ident>)' option is allowed after 'syntax'")
            self.expect("PUNCT", ":=", msg="expected ':=' in '(name := <ident>)'")
            name_tok = self.expect("IDENT", msg="expected simple identifier in '(name := <ident>)'")
            if self.peek()[0] == "PUNCT" and self.peek()[1] == ".":
                raise TacticsWhitelistError(self.peek()[2], f"qualified syntax name '{name_tok[1]}.*' is not allowed in Tactics")
            if name_tok[1] in TACTICS_QUOT_FORBIDDEN_WORDS:
                raise TacticsWhitelistError(name_tok[2], f"forbidden syntax name '{name_tok[1]}'")
            self.expect("PUNCT", ")", msg=f"expected ')' closing '(name := ...)' started at line {open_tok[2]}")
        self.parse_str_lit(require_nonempty=True)
        while not (self.peek()[0] == "PUNCT" and self.peek()[1] == ":"):
            if self.peek()[0] == "EOF":
                raise TacticsWhitelistError(self.peek()[2], "expected ': tactic' to finish syntax declaration")
            self.parse_syntax_item()
        self.expect("PUNCT", ":")
        cat_tok = self.expect("IDENT", msg="expected 'tactic' category after ':'")
        if cat_tok[1] != "tactic":
            raise TacticsWhitelistError(cat_tok[2], f"non-tactic syntax category '{cat_tok[1]}'; only ': tactic' is allowed")

    def parse_macro_decl(self):
        self.expect("IDENT", "macro")
        self.parse_str_lit(require_nonempty=True)
        bound_vars = set()
        while not (self.peek()[0] == "PUNCT" and self.peek()[1] == ":"):
            if self.peek()[0] == "EOF":
                raise TacticsWhitelistError(self.peek()[2], "expected ': tactic => `(tactic| ...)' to finish macro declaration")
            t0, t1, t2, t3 = self.peek(0), self.peek(1), self.peek(2), self.peek(3)
            if (t0[0] == "IDENT" and t1[0] == "PUNCT" and t1[1] == ":" and not t1[3]
                    and not (t2[0] == "IDENT" and t3[0] == "PUNCT" and t3[1] == "=>")):
                var_tok = self.advance()
                var_name = var_tok[1]
                if var_name in TACTICS_QUOT_FORBIDDEN_WORDS:
                    raise TacticsWhitelistError(var_tok[2], f"forbidden macro parameter name '{var_name}'")
                self.advance()  # ':'
                bound_vars.add(var_name)
                self.parse_syntax_atom()
            else:
                self.parse_syntax_item()
        self.expect("PUNCT", ":")
        cat_tok = self.expect("IDENT", msg="expected 'tactic' category after ':'")
        if cat_tok[1] != "tactic":
            raise TacticsWhitelistError(cat_tok[2], f"non-tactic macro category '{cat_tok[1]}'; only ': tactic' is allowed")
        self.expect("PUNCT", "=>", msg="expected '=>' after ': tactic'")
        rhs = self.peek()
        if rhs[0] != "QUOT":
            got = rhs[1] if rhs[0] != "EOF" else "end of file"
            raise TacticsWhitelistError(rhs[2], f"macro RHS must be a single `(tactic| ...) quotation, got {got!r}")
        self.advance()
        _validate_tactic_quotation(rhs[1], rhs[2], is_lhs=False, bound_vars=bound_vars)

    def parse_macro_rules_decl(self):
        kw = self.expect("IDENT", "macro_rules")
        arms = 0
        while self.peek()[0] == "PUNCT" and self.peek()[1] == "|":
            self.advance()
            lhs = self.peek()
            if lhs[0] != "QUOT":
                got = lhs[1] if lhs[0] != "EOF" else "end of file"
                raise TacticsWhitelistError(lhs[2], f"macro_rules pattern must be a `(tactic| ...) quotation, got {got!r}")
            self.advance()
            bound_vars = _validate_tactic_quotation(lhs[1], lhs[2], is_lhs=True)
            self.expect("PUNCT", "=>", msg="expected '=>' after macro_rules pattern")
            rhs = self.peek()
            if rhs[0] != "QUOT":
                got = rhs[1] if rhs[0] != "EOF" else "end of file"
                raise TacticsWhitelistError(rhs[2], f"macro_rules RHS must be a single `(tactic| ...) quotation, got {got!r}")
            self.advance()
            _validate_tactic_quotation(rhs[1], rhs[2], is_lhs=False, bound_vars=bound_vars)
            arms += 1
        if arms == 0:
            got = self.peek()[1] if self.peek()[0] != "EOF" else "end of file"
            raise TacticsWhitelistError(self.peek()[2], f"macro_rules must start with '| `(tactic| ...)' (at line {kw[2]}), got {got!r}")

    def parse_top_items(self, ns_depth=0):
        while self.peek()[0] != "EOF":
            tok = self.peek()
            if tok[0] == "IDENT" and tok[1] == "end":
                if ns_depth > 0:
                    return
                raise TacticsWhitelistError(tok[2], "unexpected 'end' without matching 'namespace'")
            if tok[0] == "IDENT" and tok[1] == "namespace":
                self.advance()
                ns_tok = self.peek()
                ns_name = self.parse_mod_path()
                if ns_depth == 0:
                    if not (ns_name == "ConcreteSpec.Tactics" or ns_name.startswith("ConcreteSpec.Tactics.")):
                        raise TacticsWhitelistError(
                            ns_tok[2],
                            f"top-level namespace '{ns_name}' is not allowed in Tactics; use 'ConcreteSpec.Tactics' or a sub-namespace",
                        )
                elif ns_name == "_root_" or ns_name.startswith("_root_.") or ns_name == "Lean" or ns_name.startswith("Lean."):
                    raise TacticsWhitelistError(ns_tok[2], f"namespace '{ns_name}' is not allowed in Tactics")
                self.parse_top_items(ns_depth=ns_depth + 1)
                self.expect("IDENT", "end", msg=f"unclosed 'namespace {ns_name}'")
                end_name = self.parse_mod_path()
                if end_name != ns_name:
                    raise TacticsWhitelistError(tok[2], f"namespace '{ns_name}' closed by mismatched 'end {end_name}'")
                continue
            if tok[0] == "IDENT" and tok[1] == "syntax":
                self.parse_syntax_decl()
                continue
            if tok[0] == "IDENT" and tok[1] == "macro":
                self.parse_macro_decl()
                continue
            if tok[0] == "IDENT" and tok[1] == "macro_rules":
                self.parse_macro_rules_decl()
                continue
            got = tok[1] if tok[0] != "EOF" else "end of file"
            raise TacticsWhitelistError(
                tok[2],
                f"disallowed declaration/token {got!r} in ConcreteSpec/Tactics (only namespace/end, syntax : tactic, macro : tactic => `(tactic| ...), and macro_rules | `(tactic| ...) => `(tactic| ...) are permitted)",
            )


def check_tactics_whitelist(text, header_end_line, quot_spans):
    """Validate that `text` (comment-stripped via `lex(..., allow_quotations=True, quot_spans=...)`)
    after `header_end_line` (0-indexed `\n`-delimited line) obeys the pure tactic-template whitelist.
    Returns `(line, message)` on violation, or `None` when valid."""
    lines = text.split("\n")
    start_offset = sum(len(l) + 1 for l in lines[:header_end_line])
    tokens = _tokenize_tactics(text, start_offset, header_end_line + 1, dict(quot_spans))
    try:
        _TacticsWhitelistParser(tokens).parse_top_items(ns_depth=0)
    except TacticsWhitelistError as e:
        return (e.line, e.message)
    return None


def scan(pattern, paths, header=HEADER, code_pattern=None, allow_quotations=False,
         first_command=FIRST_COMMAND, tactics_whitelist=False):
    word, rc = re.compile(pattern, re.M), 0
    code_word = re.compile(code_pattern, re.M) if code_pattern else None
    for path in paths:
        try:
            source = Path(path).read_text(encoding="utf-8")
            quot_spans = [] if tactics_whitelist else None
            text = lex(source, allow_quotations=allow_quotations, quot_spans=quot_spans)
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
        if tactics_whitelist:
            violation = check_tactics_whitelist(text, k, quot_spans)
            if violation is not None:
                vline, vmsg = violation
                vtext = lines[vline - 1].strip() if 1 <= vline <= len(lines) else ""
                print(f"{path}:{vline}: {vtext} (tactics whitelist: {vmsg})")
    return rc


def scan_tactics(pattern, paths, code_pattern=None):
    return scan(
        pattern, paths,
        header=TACTICS_HEADER,
        code_pattern=code_pattern,
        allow_quotations=True,
        first_command=FIRST_COMMAND_TACTICS,
        tactics_whitelist=True,
    )


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
            sys.exit(scan_tactics(pat, files, code_pattern=code_re))
        if sys.argv[1] == "scan":
            code_re, pat, files = _parse_scan_args(sys.argv)
            sys.exit(scan(pat, files, code_pattern=code_re))
        if sys.argv[1] == "examples":
            examples(sys.argv[2], sys.argv[3], sys.argv[4:]); sys.exit(0)
    except (OSError, UnicodeDecodeError, LexError, subprocess.SubprocessError) as e:
        print(f"error: {e}", file=sys.stderr); sys.exit(3)
    print(f"usage: {sys.argv[0]} scan [--code-re RE] RE FILE... | scan-tactics [--code-re RE] RE FILE... | examples SRC DST FILE...", file=sys.stderr)
    sys.exit(2)

"""SHELL CONTEXT for the impact selector (select_gates.py): is a name in a script TEXT or a USE?

The selector derives a gate's dependencies from the paths its scripts spell. A path spelled in a
note, a reason or a message is not a dependency: bug 237 was a corpus note naming the docs-drift
gate, read as a call, which made the corpus runners appear to read the documentation tree. This module reads enough bash
to tell the contexts apart: shell words (quotes, `$'...'`, escapes, `$(...)`, `${...}`,
backquotes), simple commands and the role of each word in one, keyed array literals, and how the
values of a keyed array are used.

USE LEVELS. A keyed array's values (or keys) are used at the highest level any expansion of them
reaches, directly or through a variable or another array assigned from them (followed to a fixed
point):
  RUN   the expansion is a command word, an argument of a command not known to only read or print
        (a function, a wrapper, an interpreter), an assignment prefixed to a command, or on a line
        that names an interpreter, `source`, `eval` or `xargs`;
  READ  an argument of a command that reads files (`cat`, `grep`, `diff`, a `[[ -f ... ]]` test),
        or a redirection target;
  TEXT  an argument of `echo`/`printf` and the gates' message helpers, a string test
        (`[[ -n ... ]]`, `==`), a `case` subject, a `for` list, or an assignment whose variable is
        itself only TEXT.
Every unknown context is RUN, so a misjudgment over-selects; only uses proven to be text are TEXT.
"""
import re

TEXT, READ, RUN = 0, 1, 2

WORD_STOP = " \t\n;()|&<>"
# Reserved words: dropped; the next word is still in command position.
KEYWORDS = {"then", "do", "else", "elif", "if", "while", "until", "!", "{", "}", "done", "fi",
            "esac", "time", "function", "in"}
# Commands that run their operands as a command: the next word is still in command position.
WRAPPERS = {"exec", "command", "builtin", "nohup", "env", "timeout", "sudo", "nice", "xargs",
            "stdbuf", "setsid"}
# Commands whose arguments are only printed (to the terminal, or captured by a substitution). The
# gate helpers (ok, no, fail, ...) are the words DATA_RE in select_gates.py treats as print-only.
PRINT_CMDS = {"echo", "printf", "ok", "no", "fail", "die", "note", "warn", ":"}
INERT_CMDS = {"return", "exit", "shift", "true", "false", "unset", "case"}
# Commands that read the files their arguments name and cannot run a command. awk, sed, rg and
# sort can (`system()`, `s///e`, `--pre`, `--compress-program`) and are not here.
READ_CMDS = {"cat", "grep", "egrep", "fgrep", "diff", "cmp", "wc", "head", "tail", "uniq", "cut", "tr", "ls", "stat", "basename", "dirname", "realpath", "readlink",
             "sha256sum", "md5sum", "file", "jq"}
# Filters a printed value may be piped through and stay text (the pipeline's output is the
# value's text, captured or shown).
PURE_FILTERS = {"grep", "egrep", "fgrep", "cut", "tr", "sort", "uniq", "head", "tail", "wc", "rev",
                "fold", "paste", "column"}
# Output of a whole compound command (a group, a loop, a function call) or of the rest of the
# script (`exec >file`) sent to a file or a pipe, a function's output captured, a substitution
# spanning lines, a process substitution: a print inside it is not known to reach only the
# terminal or its own line's capture, so no print in such a script is TEXT.
COMPOUND_OUT_RE = re.compile(r"(^|[;&|\s])(\}|done|fi|esac|\))\s*([0-9]*>(?!&)|&>|\|(?!\|))"
                             r"|\bexec\s+[0-9]*>(?!&)")
FUNC_DEF_RE = re.compile(r"^\s*(?:function\s+([A-Za-z_][\w-]*)|([A-Za-z_][\w-]*)\s*\(\))")
AWK_UNSAFE_RE = re.compile(r"system|getline|\||>|ENVIRON")
DECLARE_CMDS = {"local", "declare", "typeset", "readonly"}
# Anywhere in a script these reach an array without spelling `$NAME`: a nameref, an indirect
# expansion `${!var}`, eval. A script that uses one has every keyed array judged RUN.
INDIRECT_RE = re.compile(r"\b(declare|local|typeset)\s+(-\w+\s+)*-\w*n|\$\{![A-Za-z_]\w*[}:#%/^,-]|\beval\b")
TEST_CMDS = {"[[", "[", "test"}
FILE_TESTS = {"-a", "-b", "-c", "-d", "-e", "-f", "-g", "-h", "-k", "-p", "-r", "-s", "-t", "-u",
              "-w", "-x", "-G", "-L", "-N", "-O", "-S", "-nt", "-ot", "-ef"}
ASSIGN_RE = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)(\[[^\]]*\])?\+?=(.*)$", re.S)
DURATION_RE = re.compile(r"\d+(\.\d+)?[smhd]?$")


def _sq_end(s, i):
    """s[i] is `'`: the index after the closing quote, or None."""
    j = s.find("'", i + 1)
    return None if j < 0 else j + 1


def _ansi_end(s, i):
    """s[i:] starts `$'`: index after the closing quote (backslash escapes honored), or None."""
    j, n = i + 2, len(s)
    while j < n:
        if s[j] == "\\":
            j += 2
        elif s[j] == "'":
            return j + 1
        else:
            j += 1
    return None


def _dq_end(s, i):
    """s[i] is `"`: the index after its closing quote, or None."""
    j, n = i + 1, len(s)
    while j < n:
        c = s[j]
        if c == "\\":
            j += 2
        elif c == '"':
            return j + 1
        elif c == "`" or (c == "$" and s[j + 1:j + 2] in ("(", "{")):
            j = subst_end(s, j)
            if j is None:
                return None
        else:
            j += 1
    return None


def subst_end(s, i):
    """s[i:] starts `$(`, `${` or a backquote: the index after its close, or None."""
    n = len(s)
    if s[i] == "`":
        j = i + 1
        while j < n and s[j] != "`":
            j += 2 if s[j] == "\\" else 1
        return None if j >= n else j + 1
    opener = s[i + 1]
    close = ")" if opener == "(" else "}"
    j, depth = i + 2, 1
    while j < n:
        c = s[j]
        if c == "\\":
            j += 2
        elif c == "$" and s[j + 1:j + 2] == "'":
            j = _ansi_end(s, j)
        elif c == "'":
            j = _sq_end(s, j)
        elif c == '"':
            j = _dq_end(s, j)
        elif c == "`" or (c == "$" and s[j + 1:j + 2] in ("(", "{")):
            j = subst_end(s, j)
        else:
            if c == opener:
                depth += 1
            elif c == close:
                depth -= 1
                if depth == 0:
                    return j + 1
            j += 1
            continue
        if j is None:
            return None
    return None


def word_end(s, i):
    """End of the shell word starting at s[i], or None when a quote or substitution is still open
    at the end of s."""
    n = len(s)
    while i < n and s[i] not in WORD_STOP:
        c = s[i]
        if c == "\\":
            i += 2
        elif c == "$" and s[i + 1:i + 2] == "'":
            i = _ansi_end(s, i)
        elif c == "'":
            i = _sq_end(s, i)
        elif c == '"':
            i = _dq_end(s, i)
        elif c == "`" or (c == "$" and s[i + 1:i + 2] in ("(", "{")):
            i = subst_end(s, i)
        else:
            i += 1
            continue
        if i is None:
            return None
    return min(i, n)


def substitutions(w):
    """-> the command texts of the `$(...)` and backquote substitutions in word w, inside double
    quotes and `${...}` too (their own nested substitutions are found by simple_commands)."""
    out, j, n, dq = [], 0, len(w), False
    while j < n:
        c = w[j]
        if c == "\\":
            j += 2
        elif c == "$" and w[j + 1:j + 2] == "'" and not dq:
            j = _ansi_end(w, j) or n
        elif c == "'" and not dq:
            j = _sq_end(w, j) or n
        elif c == '"':
            dq, j = not dq, j + 1
        elif c == "`" or (c == "$" and w[j + 1:j + 2] in ("(", "{")):
            k = subst_end(w, j) or n
            inner = w[j + (1 if c == "`" else 2):max(k - 1, j)]
            if c == "$" and w[j + 1] == "{":
                out += substitutions(inner)
            else:
                out.append(inner)
            j = k
        else:
            j += 1
    return out


def simple_commands(s):
    """-> the simple commands of s, each a list of (word, role); role is "cmd" (a command word or a
    wrapper), "arg", "assign" (NAME=value before the command), "redir" (a file redirection
    target), "fd" (a descriptor duplication, `>&2`) or "here" (a here-string). A command whose
    output is piped carries a last pseudo-word ("|", "pipe"). The commands of substitutions are
    listed too, after their host."""
    cmds, cur, i, n = [], [], 0, len(s)
    at_cmd, wrapped, redirect, in_test = True, False, None, False
    nested = []
    while i < n:
        c = s[i]
        if c in " \t":
            i += 1
        elif in_test and c in "&|()<>":
            i += 1  # an operator of a [[ ... ]] expression, not a separator
        elif c in ";&|()\n":
            if c == "&" and s[i + 1:i + 2] == ">":
                redirect, i = "redir", i + 2
                continue
            if c == "|" and s[i + 1:i + 2] != "|" and (i == 0 or s[i - 1] != "|"):
                cur.append(("|", "pipe"))
            if cur:
                cmds.append(cur)
            cur, at_cmd, wrapped, redirect, in_test = [], True, False, None, False
            i += 1
        elif c in "<>":
            if s.startswith("<<<", i):
                redirect, i = "here", i + 3
            else:
                redirect, i = "redir", i + 1
                while i < n and s[i] in "<>":
                    i += 1
                if i < n and s[i] == "&":
                    redirect, i = "fd", i + 1
        else:
            j = word_end(s, i)
            j = n if j is None else max(j, i + 1)
            w = s[i:j]
            for inner in substitutions(w):
                nested += simple_commands(inner)
            if redirect:
                cur.append((w, redirect))
                redirect = None
            elif at_cmd and not wrapped and ASSIGN_RE.match(w):
                cur.append((w, "assign"))
            elif at_cmd and w in KEYWORDS:
                pass
            elif at_cmd and wrapped and (w.startswith("-") or DURATION_RE.match(w)
                                         or ASSIGN_RE.match(w)):
                cur.append((w, "arg"))  # a wrapper's option: the command is still to come
            elif at_cmd:
                cur.append((w, "cmd"))
                in_test = w == "[["
                if w in WRAPPERS:
                    wrapped = True
                else:
                    at_cmd = False
            else:
                cur.append((w, "arg"))
                if w == "]]":
                    in_test = False
            i = j
    if cur:
        cmds.append(cur)
    return cmds + nested


def _bracket_end(s, i):
    """s[i] is the `[` of a key: the index of its `]` (quotes and substitutions in the key
    honored), or None."""
    j, n = i + 1, len(s)
    while j is not None and j < n and s[j] != "]":
        c = s[j]
        if c == "\\":
            j += 2
        elif c == "$" and s[j + 1:j + 2] == "'":
            j = _ansi_end(s, j)
        elif c == "'":
            j = _sq_end(s, j)
        elif c == '"':
            j = _dq_end(s, j)
        elif c == "`" or (c == "$" and s[j + 1:j + 2] in ("(", "{")):
            j = subst_end(s, j)
        elif c == "\n":
            return None
        else:
            j += 1
    return j if j is not None and j < n else None


def array_literal(body):
    """body: the text after `NAME=(`. -> ([(key or None, value)], text after the closing paren),
    or None when the literal does not close within body."""
    elems, i, n = [], 0, len(body)
    while i < n:
        c = body[i]
        if c.isspace():
            i += 1
        elif c == "#":
            j = body.find("\n", i)
            i = n if j < 0 else j
        elif c == ")":
            rest = body[i + 1:]
            if not re.match(r"[ \t]*($|[;#\n]|&&|\|\|)", rest):
                return None  # not where a literal ends: a misread quote; leave it to the line rule
            return elems, rest
        else:
            key = None
            if c == "[":
                close = _bracket_end(body, i)
                if close is not None and (body.startswith("=", close + 1)
                                          or body.startswith("+=", close + 1)):
                    key = body[i + 1:close]
                    i = close + (3 if body[close + 1] == "+" else 2)
            j = word_end(body, i)
            if j is None:
                return None
            if j == i and key is None:
                i += 1  # a stray separator: not a word
                continue
            elems.append((key, body[i:j]))
            i = j
    return None


def _value_pat(name):
    # `$NAME`, `${NAME...}` (an element, all elements, a default) — not `${#NAME[@]}` (a count)
    # nor `${!NAME[@]}` (the keys)
    return r"\$\{?" + re.escape(name) + r"(?![A-Za-z0-9_])"


def _keys_pat(name):
    return r"\$\{!" + re.escape(name) + r"(?![A-Za-z0-9_])"


def _command_name(cmd):
    """The command a simple command runs, past its wrappers; None when it has none."""
    names = [w for w, r in cmd if r == "cmd"]
    for w in names:
        if w not in WRAPPERS:
            return w.strip("\"'")
    return names[-1] if names else None


def _open_subst(line):
    """A substitution a print's output may flow into beyond this line: a `$(` or backquote left
    open (the command goes on over the next lines), or a process substitution `<(` / `>(`."""
    if re.search(r"[<>]\(", line):
        return True
    return any(subst_end(line, m.start()) is None for m in re.finditer(r"\$\(|`", line))


def _pure(cmd):
    if any(r == "redir" for _, r in cmd):
        return False  # the filtered text goes to a file
    name = _command_name(cmd)
    if name in PURE_FILTERS:
        return True
    return name == "awk" and not any(AWK_UNSAFE_RE.search(w) for w, r in cmd if r == "arg")


def _printed_only(cmds, k):
    """cmds[k] prints: is its output only shown, captured, or piped through pure filters?"""
    while cmds[k] and cmds[k][-1] == ("|", "pipe"):
        k += 1
        if k >= len(cmds) or not _pure(cmds[k]):
            return False
    return True


def use_level(seeds, code_lines, elements, exec_re):
    """The highest use level of anything matching a seed pattern (see USE LEVELS).

    code_lines: the script's code lines; elements: [(NAME, "key"|"value", text)] for every keyed
    element. Taint flows into a variable assigned from it (`v=...`, `local v=...`, `printf -v v`,
    `for v in ...`, `read v <<< ...`) and into a keyed array whose element holds it. Only the
    contexts below are TEXT or READ; everything else is RUN."""
    if any(INDIRECT_RE.search(t) for t in list(code_lines) + [t for _, _, t in elements]):
        return RUN
    pats, level, grew = set(seeds), TEXT, True
    parsed = [(line, simple_commands(line)) for line in code_lines]
    funcs = {m.group(1) or m.group(2) for line in code_lines for m in [FUNC_DEF_RE.match(line)] if m}
    prints_escape = any(COMPOUND_OUT_RE.search(line) or _open_subst(line) for line in code_lines) \
        or any(_command_name(cmd) in funcs and (cmd[-1] == ("|", "pipe")
                                                or any(r == "redir" for _, r in cmd))
               for _, cmds in parsed for cmd in cmds) \
        or any(_command_name(c) in funcs
               for _, cmds in parsed for cmd in cmds for w, _ in cmd
               for inner in substitutions(w) for c in simple_commands(inner))

    def ident(v):
        return re.match(r"[A-Za-z_]\w*$", v) is not None

    while grew:
        grew = False
        rx = [re.compile(p) for p in pats]

        def hit(w):
            return any(r.search(w) for r in rx)

        def taint(v):
            nonlocal grew
            p = _value_pat(v) if ident(v) else v
            if p not in pats:
                pats.add(p)
                grew = True

        for name, part, text in elements:
            if hit(text):
                taint(_value_pat(name) if part == "value" else _keys_pat(name))
        for line, cmds in parsed:
            if not hit(line):
                continue
            if exec_re.search(line) or re.search(r"\bxargs\b|[<>]\(", line):
                return RUN  # an interpreter, or a process substitution: a consumer not modeled
            for k, cmd in enumerate(cmds):
                if not any(hit(w) for w, _ in cmd):
                    continue
                name = _command_name(cmd)
                words = [w for w, r in cmd if r != "pipe"]
                files = [w for w, r in cmd if r == "redir"]
                for w, role in cmd:
                    if not hit(w):
                        continue
                    if role == "assign":
                        if name is not None:
                            return RUN  # an environment value the command it prefixes reads
                        taint(ASSIGN_RE.match(w).group(1))
                    elif role == "redir":
                        level = max(level, READ)
                    elif role == "here":
                        if name != "read":
                            return RUN
                        taint("REPLY")  # read's destination when it names none
                        for v in words[1:]:
                            if ident(v):
                                taint(v)
                    elif role != "arg":
                        return RUN  # a command word, or anything this reader has no rule for
                    elif name in PRINT_CMDS:
                        opt = [i for i, x in enumerate(words) if x.startswith("-v")]
                        if name == "printf" and opt:
                            i = opt[0]
                            var = words[i][2:] or (words[i + 1] if i + 1 < len(words) else "")
                            if not ident(var.split("[")[0]):
                                return RUN
                            taint(var.split("[")[0])
                        elif files or prints_escape or not _printed_only(cmds, k):
                            return RUN  # printed into a file or a consumer this reader cannot see
                    elif name in INERT_CMDS:
                        pass
                    elif name in DECLARE_CMDS:
                        m = ASSIGN_RE.match(w)
                        if not m:
                            return RUN
                        taint(m.group(1))
                    elif name in ("for", "select"):
                        if len(words) < 2 or not ident(words[1]):
                            return RUN
                        taint(words[1])
                    elif name in TEST_CMDS:
                        if any(x in FILE_TESTS for x in words):
                            level = max(level, READ)
                    elif name in READ_CMDS:
                        level = max(level, READ)
                    else:
                        return RUN
    return level

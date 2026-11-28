"""Check the workflow inventory, cache policy, and landing gate verdict."""

import copy
import json
from pathlib import Path
import re
import sys


INVENTORY = Path("required-checks.txt")
WORKFLOW = Path(".github/workflows/lean_action_ci.yml")
ACTIONS_DIR = Path(".github/actions")
LAKE_PACKAGES_ACTION_DIR = ".github/actions/lake-packages"
LAKE_PACKAGES_ACTION_USE = f"./{LAKE_PACKAGES_ACTION_DIR}"
LAKE_PACKAGES_ACTION_PATH = ACTIONS_DIR / "lake-packages/action.yml"
FETCH_HELPER = "scripts/checks/lib/fetch_lake_packages.sh"
EXPECTED_FETCH_RUN = (
    "nix develop .#gates --command bash --noprofile --norc -eo pipefail "
    f"{FETCH_HELPER}"
)
EXTERNAL_CACHE_PATH = "~/.cache/concrete-ext"
LAKE_PACKAGES_CACHE_PATH = ".lake/packages"
LAKE_PACKAGES_HASH_INPUTS = frozenset({"lake-manifest.json", "lean-toolchain"})
AGGREGATE = "landing-gates"
AGGREGATE_NAME = "Landing gates"

GHA_EXPR_RE = re.compile(r"\$\{\{(.*?)\}\}", re.DOTALL)
RUNNER_OS_EXPR_RE = re.compile(r"^\s*runner\.os\s*$", re.IGNORECASE)
PURE_HASHFILES_RE = re.compile(
    r"^\s*hashFiles\(\s*('[^']*'(?:\s*,\s*'[^']*')*)\s*\)\s*$",
    re.IGNORECASE,
)
LAKE_PACKAGES_KEY_RE = re.compile(
    r"^\$\{\{\s*runner\.os\s*\}\}-lake-packages-sources(?:-[A-Za-z0-9._]+)?-"
    r"\$\{\{\s*hashFiles\(\s*'[^']*'\s*,\s*'[^']*'\s*\)\s*\}\}$",
    re.IGNORECASE,
)
QUOTED_LITERAL = re.compile(r"'([^']*)'")
ENV_ASSIGN_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*=")
LAKE_SCRIPT_RE = re.compile(r"(?:^|/)scripts/(?:checks/compiler|proofs|spec)/")
SHELL_KEYWORDS = frozenset({
    "if", "then", "elif", "else", "do", "while", "until", "!", "{", "}", "time", "coproc",
})
SHELL_BINARIES = frozenset({"bash", "sh", "dash", "zsh", "ksh"})
GIT_INVALIDATING_SUBCOMMANDS = frozenset({
    "checkout", "switch", "reset", "clean", "pull", "restore",
    "merge", "rebase", "cherry-pick", "am", "apply",
})
SENSITIVE_PATHS = frozenset({
    "lake-manifest.json", "lean-toolchain", ".lake", ".lake/packages",
})
MANIFEST_FILENAMES = frozenset({"lake-manifest.json", "lean-toolchain"})
OUTPUT_REDIR_OPS = frozenset({">", ">>", ">|", "<>", "&>", "&>>"})


def gate_names():
    names = [
        line.strip()
        for line in INVENTORY.read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    ]
    if not names or names[0] != AGGREGATE_NAME:
        raise ValueError("required-checks.txt must list Landing gates first")
    return names[1:]


def check_inventory(workflow, names):
    jobs = workflow["jobs"]
    aggregate = jobs[AGGREGATE]
    gate_ids = set(jobs) - {AGGREGATE}
    if not gate_ids or set(aggregate["needs"]) != gate_ids:
        raise ValueError("Landing gates must depend on every gate job")
    if aggregate["name"] != AGGREGATE_NAME or aggregate["if"] != "${{ always() }}":
        raise ValueError("Landing gates must always run under its required check name")
    if any(key in aggregate for key in ("strategy", "continue-on-error")) or any(
        key in step for step in aggregate["steps"] for key in ("if", "continue-on-error")
    ):
        raise ValueError("Landing gates and its steps must run and report their own verdict")
    if workflow["on"]["merge_group"]["types"] != ["checks_requested"]:
        raise ValueError("CI must run for merge-group candidates")

    expanded = []
    for job_id in sorted(gate_ids):
        job = jobs[job_id]
        matrix = job.get("strategy", {}).get("matrix")
        if matrix is not None and (set(matrix) != {"include"} or not matrix["include"]):
            raise ValueError(f"{job_id}: use a nonempty include-only matrix")
        rows = [{}] if matrix is None else matrix["include"]
        for row in rows:
            name = re.sub(r"\$\{\{\s*matrix\.(\w+)\s*\}\}",
                          lambda match: row[match[1]], job["name"])
            if "${{" in name:
                raise ValueError(f"{job_id}: unsupported expression in check name")
            expanded.append(name)
    for labels in (expanded, names):
        if AGGREGATE_NAME in labels or len(set(labels)) != len(labels):
            raise ValueError("gate names must be unique and distinct from Landing gates")
    unlisted = sorted(set(expanded) - set(names))
    stale = sorted(set(names) - set(expanded))
    if unlisted or stale:
        raise ValueError(f"required-checks.txt mismatch: unlisted={unlisted}, stale={stale}")


def require_success(names, pages):
    conclusions = {}
    for page in pages:
        for job in page["jobs"]:
            name = job["name"]
            if name == AGGREGATE_NAME:
                continue
            if name in conclusions:
                raise ValueError(f"duplicate job result: {name}")
            conclusions[name] = job["conclusion"]

    problems = [
        f"{name}: {conclusions.get(name, 'missing')}"
        for name in names if conclusions.get(name) != "success"
    ]
    problems.extend(f"{name}: unlisted" for name in sorted(set(conclusions) - set(names)))
    if problems:
        raise ValueError("\n".join(problems))


def expect_failure(check, diagnostic, *args, label=None, **kwargs):
    try:
        check(*args, **kwargs)
    except ValueError as error:
        if diagnostic in str(error):
            print(f"  ok   control rejects {label or diagnostic}")
            return
        raise
    raise ValueError(f"control unexpectedly accepted {label or diagnostic}")


def parse_hashfiles_args(key, scope):
    """Return ordered list of literal arguments from pure ${{ hashFiles(...) }} expressions.

    Rejects any exclusion pattern ('!...') and any empty or whitespace-padded pattern.
    Compound expressions that wrap hashFiles(...) in conditionals or fallbacks are not
    accepted as pure hashFiles(...) expressions.
    """
    if not isinstance(key, str):
        return []
    args = []
    for match in GHA_EXPR_RE.finditer(key):
        expr = match.group(1)
        pure = PURE_HASHFILES_RE.fullmatch(expr)
        if not pure:
            continue
        call_args = QUOTED_LITERAL.findall(pure.group(1))
        for pattern in call_args:
            if not pattern or pattern.strip() != pattern:
                raise ValueError(
                    f"{scope}: hashFiles(...) pattern must be non-empty and trimmed (got {pattern!r})"
                )
            if pattern.startswith("!"):
                raise ValueError(
                    f"{scope}: hashFiles(...) must not contain exclusion pattern {pattern!r}"
                )
            args.append(pattern)
    return args


def normalize_local_action_use(uses, scope):
    if not isinstance(uses, str) or not uses.startswith("./"):
        return None
    if "@" in uses:
        raise ValueError(f"{scope}: local action {uses!r} must not include a ref")
    raw = uses.removeprefix("./")
    parts = []
    for part in raw.split("/"):
        if part in {"", "."}:
            continue
        if part == "..":
            if not parts:
                raise ValueError(f"{scope}: local action {uses!r} escapes repository root")
            parts.pop()
        else:
            parts.append(part)
    if not parts:
        raise ValueError(f"{scope}: local action {uses!r} must name a directory")
    return "/".join(parts)


def step_action(step):
    return step.get("uses", "").partition("@")[0].lower()


def normalize_rel_path(path):
    if not isinstance(path, str):
        return ""
    norm = path.strip()
    while norm.startswith("./"):
        norm = norm[2:]
    return norm.rstrip("/")


def is_sensitive_path(path):
    norm = normalize_rel_path(path)
    if norm in SENSITIVE_PATHS:
        return True
    return norm.startswith(".lake/packages/")


def _read_cmd_subst(text, start):
    """Read balanced $(...) body starting immediately after '$('."""
    n = len(text)
    i = start
    depth = 1
    in_squote = False
    in_dquote = False
    at_boundary = True
    while i < n:
        ch = text[i]
        if in_squote:
            if ch == "'":
                in_squote = False
            i += 1
            continue
        if in_dquote:
            if ch == "\\" and i + 1 < n:
                i += 2
                continue
            if ch == '"':
                in_dquote = False
                i += 1
                continue
            if ch == "$" and i + 1 < n and text[i + 1] == "(":
                _, i = _read_cmd_subst(text, i + 2)
                continue
            if ch == "`":
                _, i = _read_backtick(text, i + 1)
                continue
            i += 1
            continue
        if ch == "$" and text.startswith("${{", i):
            end = text.find("}}", i + 3)
            i = n if end == -1 else end + 2
            at_boundary = False
            continue
        if ch == "\\" and i + 1 < n:
            i += 2
            at_boundary = False
            continue
        if ch == "'":
            in_squote = True
            at_boundary = False
            i += 1
            continue
        if ch == '"':
            in_dquote = True
            at_boundary = False
            i += 1
            continue
        if ch == "#" and at_boundary:
            nl = text.find("\n", i)
            i = n if nl == -1 else nl
            at_boundary = True
            continue
        if ch == "(":
            depth += 1
            at_boundary = True
            i += 1
            continue
        if ch == ")":
            depth -= 1
            if depth == 0:
                return text[start:i], i + 1
            at_boundary = True
            i += 1
            continue
        if ch == "`":
            _, i = _read_backtick(text, i + 1)
            at_boundary = False
            continue
        at_boundary = ch in " \t\r\n;|&()"
        i += 1
    return text[start:], n


def _read_backtick(text, start):
    """Read `...` body starting immediately after opening backtick."""
    n = len(text)
    i = start
    buf = []
    while i < n:
        ch = text[i]
        if ch == "\\" and i + 1 < n and text[i + 1] in {"$", "`", "\\", "\n"}:
            if text[i + 1] != "\n":
                buf.append(text[i + 1])
            i += 2
            continue
        if ch == "`":
            return "".join(buf), i + 1
        buf.append(ch)
        i += 1
    return "".join(buf), n


def parse_shell_commands(text):
    """Parse a shell script into ordered simple commands.

    Each item is (words, redirections, heredocs, substitutions) so command execution
    is distinguished from filename mentions, command wrappers and subshells are
    inspected in order, and heredocs piped to shells are analyzed.
    """
    commands = []
    words = []
    redirections = []
    heredocs = []
    substitutions = []
    pending_redir = None
    heredoc_queue = []
    cur = []
    in_word = False

    def flush_word():
        nonlocal cur, in_word, pending_redir
        if not in_word:
            return
        tok = "".join(cur)
        cur = []
        in_word = False
        if pending_redir is not None:
            op = pending_redir
            pending_redir = None
            if op in {"<<", "<<-"}:
                heredoc_queue.append((tok, op == "<<-", heredocs))
            else:
                redirections.append((op, tok))
        else:
            words.append(tok)

    def flush_command():
        nonlocal words, redirections, heredocs, substitutions, pending_redir
        flush_word()
        pending_redir = None
        if words or redirections or heredocs or substitutions:
            commands.append((words, redirections, heredocs, substitutions))
            words = []
            redirections = []
            heredocs = []
            substitutions = []

    n = len(text)
    i = 0
    while i < n:
        ch = text[i]

        if ch == "\n":
            flush_command()
            i += 1
            if heredoc_queue:
                for delim, strip_tabs, target_heredocs in heredoc_queue:
                    body_lines = []
                    while i < n:
                        nl = text.find("\n", i)
                        if nl == -1:
                            line = text[i:]
                            i = n
                        else:
                            line = text[i:nl]
                            i = nl + 1
                        cmp_line = line.lstrip("\t") if strip_tabs else line
                        if cmp_line == delim:
                            break
                        body_lines.append(line)
                    target_heredocs.append("\n".join(body_lines))
                heredoc_queue = []
            continue

        if ch in " \t\r":
            flush_word()
            i += 1
            continue

        if ch == "#" and not in_word:
            nl = text.find("\n", i)
            i = n if nl == -1 else nl
            continue

        # Multi-character shell operators and redirections.
        three = text[i:i + 3]
        two = text[i:i + 2]
        if three in {"<<-", "<<<", "&>>"}:
            if in_word and "".join(cur).isdigit():
                cur = []
                in_word = False
            else:
                flush_word()
            pending_redir = three
            i += 3
            continue
        if two in {"&&", "||", "|&", ";;"}:
            flush_command()
            i += 2
            continue
        if two in {"<<", ">>", ">|", "<>", "&>", ">&", "<&"}:
            if in_word and "".join(cur).isdigit():
                cur = []
                in_word = False
            else:
                flush_word()
            pending_redir = two
            i += 2
            continue
        if ch in ";|&()":
            flush_command()
            i += 1
            continue
        if ch in "<>":
            if in_word and "".join(cur).isdigit():
                cur = []
                in_word = False
            else:
                flush_word()
            pending_redir = ch
            i += 1
            continue

        # Word character / quoting / expansion.
        in_word = True
        if ch == "$" and text.startswith("${{", i):
            end = text.find("}}", i + 3)
            if end == -1:
                cur.append(text[i:])
                i = n
            else:
                cur.append(text[i:end + 2])
                i = end + 2
            continue
        if ch == "\\" and i + 1 < n:
            if text[i + 1] == "\n":
                i += 2
                continue
            cur.append(text[i + 1])
            i += 2
            continue
        if ch == "'":
            end = text.find("'", i + 1)
            if end == -1:
                cur.append(text[i + 1:])
                i = n
            else:
                cur.append(text[i + 1:end])
                i = end + 1
            continue
        if ch == '"':
            i += 1
            while i < n and text[i] != '"':
                if text[i] == "\\" and i + 1 < n:
                    if text[i + 1] == "\n":
                        i += 2
                        continue
                    cur.append(text[i + 1])
                    i += 2
                    continue
                if text[i] == "$" and text.startswith("${{", i):
                    end = text.find("}}", i + 3)
                    if end == -1:
                        cur.append(text[i:])
                        i = n
                    else:
                        cur.append(text[i:end + 2])
                        i = end + 2
                    continue
                if text[i] == "$" and i + 1 < n and text[i + 1] == "(":
                    inner, i = _read_cmd_subst(text, i + 2)
                    substitutions.append(inner)
                    continue
                if text[i] == "`":
                    inner, i = _read_backtick(text, i + 1)
                    substitutions.append(inner)
                    continue
                cur.append(text[i])
                i += 1
            if i < n and text[i] == '"':
                i += 1
            continue
        if ch == "$" and i + 1 < n and text[i + 1] == "(":
            inner, i = _read_cmd_subst(text, i + 2)
            substitutions.append(inner)
            continue
        if ch == "`":
            inner, i = _read_backtick(text, i + 1)
            substitutions.append(inner)
            continue

        cur.append(ch)
        i += 1

    flush_command()
    return commands


def _analyze_nix_develop(args, events):
    """Inspect `nix develop ...` arguments. Return inner command words after -c/--command."""
    opts = []
    inner = []
    i = 0
    while i < len(args):
        tok = args[i]
        if tok in {"-c", "--command"}:
            inner = args[i + 1:]
            break
        opts.append(tok)
        i += 1

    # Extract positional installable(s) from nix develop options.
    two_arg_flags = {"--option", "--override-input"}
    one_arg_flags = {
        "-I", "-k", "--keep", "-u", "--unset", "--inputs-from", "--profile",
        "--redirect", "--eval-store", "--store", "--extra-experimental-features",
        "--experimental-features", "--override-flake", "--update-input",
        "--builders", "--max-jobs", "-j", "--cores", "--log-format",
    }
    installables = []
    j = 0
    while j < len(opts):
        tok = opts[j]
        if tok == "--":
            installables.extend(opts[j + 1:])
            break
        if tok in two_arg_flags:
            j += 3
            continue
        if tok in one_arg_flags:
            j += 2
            continue
        if tok.startswith("-"):
            j += 1
            continue
        installables.append(tok)
        j += 1

    if not installables or any(inst not in {".#gates", ".#extcheck"} for inst in installables):
        events.append(("lake", "default nix develop shell"))
    return inner


def _git_subcommand(args):
    one_arg = {
        "-C", "-c", "--git-dir", "--work-tree", "--namespace",
        "--super-prefix", "--config-env", "--list-cmds",
    }
    i = 0
    while i < len(args):
        tok = args[i]
        if tok == "--":
            return args[i + 1] if i + 1 < len(args) else ""
        if tok in one_arg:
            i += 2
            continue
        if tok.startswith("-"):
            i += 1
            continue
        return tok
    return ""


def _lake_subcommand(args):
    one_arg = {"-d", "--dir", "-f", "--file", "-K", "-o", "--out", "--log-level"}
    i = 0
    while i < len(args):
        tok = args[i]
        if tok == "--":
            return args[i + 1] if i + 1 < len(args) else ""
        if tok in one_arg:
            i += 2
            continue
        if tok.startswith("-"):
            i += 1
            continue
        return tok
    return ""


def _analyze_simple_command(words, redirections, heredocs, substitutions, events, depth):
    if depth > 8:
        return

    for sub in substitutions:
        _collect_shell_events(sub, events, depth + 1)

    for op, target in redirections:
        if op in OUTPUT_REDIR_OPS and is_sensitive_path(target):
            events.append(("invalidate", f"write to {target}"))

    idx = 0
    while idx < len(words) and words[idx] in SHELL_KEYWORDS:
        idx += 1
    while idx < len(words) and ENV_ASSIGN_RE.match(words[idx]):
        idx += 1
    words = words[idx:]
    if not words:
        return

    for _ in range(16):
        if not words:
            return
        cmd = words[0]
        base = cmd.rsplit("/", 1)[-1]
        rest = words[1:]

        if base == "env":
            i = 0
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok in {"-u", "--unset", "-C", "--chdir"}:
                    i += 2
                    continue
                if tok == "-S" and i + 1 < len(rest):
                    _collect_shell_events(rest[i + 1], events, depth + 1)
                    i += 2
                    continue
                if tok.startswith("-"):
                    i += 1
                    continue
                if ENV_ASSIGN_RE.match(tok):
                    i += 1
                    continue
                break
            words = rest[i:]
            continue

        if base == "command":
            i = 0
            query_only = False
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok.startswith("-") and len(tok) > 1:
                    if "v" in tok[1:] or "V" in tok[1:]:
                        query_only = True
                    i += 1
                    continue
                break
            if query_only:
                return
            words = rest[i:]
            continue

        if base in {"exec", "nohup", "builtin"}:
            i = 0
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if base == "exec" and tok == "-a":
                    i += 2
                    continue
                if tok.startswith("-"):
                    i += 1
                    continue
                break
            words = rest[i:]
            continue

        if base == "timeout":
            i = 0
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok in {"-k", "--kill-after", "-s", "--signal"}:
                    i += 2
                    continue
                if tok.startswith("-"):
                    i += 1
                    continue
                break
            # Skip the duration operand.
            words = rest[i + 1:] if i < len(rest) else []
            continue

        if base == "nice":
            i = 0
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok in {"-n", "--adjustment"}:
                    i += 2
                    continue
                if tok.startswith("-"):
                    i += 1
                    continue
                break
            words = rest[i:]
            continue

        if base == "sudo":
            one_arg = {"-u", "-g", "-C", "-h", "-p", "-r", "-t", "-U", "-D"}
            i = 0
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok in one_arg:
                    i += 2
                    continue
                if tok.startswith("-") or ENV_ASSIGN_RE.match(tok):
                    i += 1
                    continue
                break
            words = rest[i:]
            continue

        if base == "xargs":
            one_arg = {"-I", "-L", "-n", "-P", "-d", "-E", "-s", "-a", "-R"}
            i = 0
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok in one_arg:
                    i += 2
                    continue
                if tok.startswith("-"):
                    i += 1
                    continue
                break
            words = rest[i:]
            continue

        if base == "nix" and rest and rest[0] == "develop":
            words = _analyze_nix_develop(rest[1:], events)
            continue

        if base in SHELL_BINARIES:
            i = 0
            seen_c = False
            while i < len(rest):
                tok = rest[i]
                if tok == "--":
                    i += 1
                    break
                if tok in {"-o", "+o"}:
                    i += 2
                    continue
                if tok.startswith("--"):
                    i += 1
                    continue
                if tok.startswith("-") and len(tok) > 1:
                    if "c" in tok[1:]:
                        seen_c = True
                    i += 1
                    continue
                if tok.startswith("+") and len(tok) > 1:
                    i += 1
                    continue
                break
            if seen_c:
                if i < len(rest):
                    _collect_shell_events(rest[i], events, depth + 1)
                return
            if i < len(rest):
                if LAKE_SCRIPT_RE.search(rest[i]):
                    events.append(("lake", f"script {rest[i]}"))
                return
            for body in heredocs:
                _collect_shell_events(body, events, depth + 1)
            return

        if base == "eval":
            if rest:
                _collect_shell_events(" ".join(rest), events, depth + 1)
            return

        break

    if not words:
        return
    cmd = words[0]
    base = cmd.rsplit("/", 1)[-1]
    rest = words[1:]

    if base == "lake":
        events.append(("lake", "lake command"))
        if _lake_subcommand(rest) == "update":
            events.append(("invalidate", "lake update"))
        return

    if LAKE_SCRIPT_RE.search(cmd):
        events.append(("lake", f"script {cmd}"))
        return

    if base == "git":
        sub = _git_subcommand(rest)
        if sub in GIT_INVALIDATING_SUBCOMMANDS:
            events.append(("invalidate", f"git {sub}"))
        return

    if base in {"rm", "unlink", "truncate", "tee", "touch"}:
        one_arg = {"-s", "--size", "-r", "--reference"} if base == "truncate" else set()
        i = 0
        end_opts = False
        while i < len(rest):
            tok = rest[i]
            if not end_opts and tok == "--":
                end_opts = True
                i += 1
                continue
            if not end_opts and tok in one_arg:
                i += 2
                continue
            if not end_opts and tok.startswith("-") and tok != "-":
                i += 1
                continue
            if is_sensitive_path(tok):
                events.append(("invalidate", f"{base} {tok}"))
            i += 1
        return

    if base in {"cp", "mv", "ln", "install"}:
        one_arg = {"-S", "--suffix", "-m", "-o", "-g", "-b"}
        target_dir = None
        positionals = []
        i = 0
        end_opts = False
        while i < len(rest):
            tok = rest[i]
            if not end_opts and tok == "--":
                end_opts = True
                i += 1
                continue
            if not end_opts and tok in {"-t", "--target-directory"} and i + 1 < len(rest):
                target_dir = rest[i + 1]
                i += 2
                continue
            if not end_opts and tok.startswith("--target-directory="):
                target_dir = tok.split("=", 1)[1]
                i += 1
                continue
            if not end_opts and tok in one_arg:
                i += 2
                continue
            if not end_opts and tok.startswith("-") and tok != "-":
                i += 1
                continue
            positionals.append(tok)
            i += 1

        if base == "mv":
            for arg in positionals:
                if is_sensitive_path(arg):
                    events.append(("invalidate", f"mv {arg}"))
            if target_dir is not None and (
                is_sensitive_path(target_dir)
                or (
                    normalize_rel_path(target_dir) in {"", "."}
                    and any(p.rstrip("/").rsplit("/", 1)[-1] in MANIFEST_FILENAMES for p in positionals)
                )
            ):
                events.append(("invalidate", f"mv to {target_dir}"))
        else:
            dest = target_dir if target_dir is not None else (
                positionals[-1] if (len(positionals) >= 2 or (base == "ln" and positionals)) else None
            )
            sources = positionals if target_dir is not None else positionals[:-1]
            if dest is not None and (
                is_sensitive_path(dest)
                or (
                    normalize_rel_path(dest) in {"", "."}
                    and any(p.rstrip("/").rsplit("/", 1)[-1] in MANIFEST_FILENAMES for p in sources)
                )
            ):
                events.append(("invalidate", f"{base} {dest}"))
        return

    if base in {"sed", "perl"}:
        in_place = False
        positionals = []
        i = 0
        end_opts = False
        while i < len(rest):
            tok = rest[i]
            if not end_opts and tok == "--":
                end_opts = True
                i += 1
                continue
            if not end_opts and (
                tok == "-i"
                or tok.startswith("-i")
                or tok == "--in-place"
                or tok.startswith("--in-place=")
                or (base == "perl" and tok.startswith("-") and not tok.startswith("--") and "i" in tok[1:])
            ):
                in_place = True
                i += 1
                continue
            if not end_opts and tok in {"-e", "-f", "-E", "-M", "-I"}:
                i += 2
                continue
            if not end_opts and tok.startswith("-"):
                i += 1
                continue
            positionals.append(tok)
            i += 1
        if in_place:
            for arg in positionals:
                if is_sensitive_path(arg):
                    events.append(("invalidate", f"{base} -i {arg}"))
        return

    if base == "patch":
        events.append(("invalidate", "patch"))


def _collect_shell_events(text, events, depth=0):
    if not isinstance(text, str) or not text.strip() or depth > 8:
        return
    for words, redirections, heredocs, substitutions in parse_shell_commands(text):
        _analyze_simple_command(words, redirections, heredocs, substitutions, events, depth)


def step_shell_events(step, *, default_shell=""):
    """Return ordered (kind, detail) events for a step, treating effective shell as part of the command."""
    action = step_action(step)
    events = []
    if action == "actions/checkout":
        events.append(("invalidate", "actions/checkout"))
    if action == "leanprover/lean-action":
        events.append(("lake", "leanprover/lean-action"))
    if "run" in step:
        run_cmd = step.get("run")
        effective_shell = step.get("shell") or default_shell
        # GitHub Actions invokes the effective shell before executing the step body.
        if isinstance(effective_shell, str) and effective_shell.strip():
            _collect_shell_events(effective_shell, events)
        if isinstance(run_cmd, str) and run_cmd.strip():
            _collect_shell_events(run_cmd, events)
    return events


def step_uses_lake(step, *, default_shell=""):
    return any(kind == "lake" for kind, _ in step_shell_events(step, default_shell=default_shell))


def check_step_cache_policy(scope, step, *, allow_lake_packages_cache=False):
    action = step_action(step)
    inputs = step.get("with", {})
    if action == "actions/checkout" and inputs.get("clean", "true") != "true":
        raise ValueError(f"{scope}: checkout must clean the workspace")
    if action == "leanprover/lean-action":
        for option in ("use-github-cache", "use-mathlib-cache"):
            if inputs.get(option) != "false":
                raise ValueError(f"{scope}: lean-action requires {option} disabled")
    if action in {"actions/cache", "actions/cache/restore", "actions/cache/save"}:
        cache_path = inputs.get("path")
        # A path or glob rooted above .lake (or including .lake/build) can restore repo outputs.
        if cache_path not in {EXTERNAL_CACHE_PATH, LAKE_PACKAGES_CACHE_PATH}:
            raise ValueError(
                f"{scope}: cache paths must contain only pinned external tools or "
                f"{LAKE_PACKAGES_CACHE_PATH} (got {cache_path!r})"
            )
        if "restore-keys" in inputs:
            raise ValueError(f"{scope}: cache step must not use restore-keys fallback")
        key = inputs.get("key")
        if not isinstance(key, str) or not key.strip():
            raise ValueError(f"{scope}: cache step requires an exact key")
        hashed_args = parse_hashfiles_args(key, scope)
        if cache_path == LAKE_PACKAGES_CACHE_PATH:
            if action == "actions/cache":
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache must use split restore/save before build"
                )
            if "lake-manifest.json" not in hashed_args:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash lake-manifest.json"
                )
            if "lean-toolchain" not in hashed_args:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash lean-toolchain"
                )
            if set(hashed_args) != LAKE_PACKAGES_HASH_INPUTS or len(hashed_args) != len(LAKE_PACKAGES_HASH_INPUTS):
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must hash only literal "
                    f"lake-manifest.json and lean-toolchain (got {hashed_args!r})"
                )
            exprs = [match.group(1) for match in GHA_EXPR_RE.finditer(key)]
            if not any(RUNNER_OS_EXPR_RE.fullmatch(expr) for expr in exprs):
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must include ${{{{ runner.os }}}}"
                )
            unexpected_exprs = [
                expr.strip()
                for expr in exprs
                if not RUNNER_OS_EXPR_RE.fullmatch(expr) and not PURE_HASHFILES_RE.fullmatch(expr)
            ]
            if unexpected_exprs:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must not contain dynamic or "
                    f"extra expressions {unexpected_exprs!r}"
                )
            if not LAKE_PACKAGES_KEY_RE.fullmatch(key):
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache key must match "
                    f"${{{{ runner.os }}}}-lake-packages-sources[-<tag>]-"
                    f"${{{{ hashFiles('lake-manifest.json', 'lean-toolchain') }}}} (got {key!r})"
                )
            if not allow_lake_packages_cache:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache must live only in {LAKE_PACKAGES_ACTION_USE}"
                )
            extra_with = set(inputs) - {"path", "key"}
            if extra_with:
                raise ValueError(
                    f"{scope}: {LAKE_PACKAGES_CACHE_PATH} cache step sets unexpected inputs {sorted(extra_with)}"
                )
        elif not hashed_args:
            raise ValueError(f"{scope}: external tool cache key must use hashFiles(...)")


def check_lake_packages_action(action_doc, scope=str(LAKE_PACKAGES_ACTION_PATH)):
    runs = action_doc.get("runs")
    if not isinstance(runs, dict) or runs.get("using") != "composite":
        raise ValueError(f"{scope}: must be a composite action")
    steps = runs.get("steps")
    if not isinstance(steps, list) or not steps:
        raise ValueError(f"{scope}: composite action must define steps")

    restore_steps = []
    fetch_steps = []
    save_steps = []
    for index, step in enumerate(steps):
        if not isinstance(step, dict):
            raise ValueError(f"{scope}: step {index} must be a mapping")
        check_step_cache_policy(scope, step, allow_lake_packages_cache=True)
        if step_uses_lake(step):
            raise ValueError(f"{scope}: dependency source action must not run Lake or lean-action")
        action = step_action(step)
        inputs = step.get("with", {})
        if action == "actions/cache/restore" and inputs.get("path") == LAKE_PACKAGES_CACHE_PATH:
            restore_steps.append((index, step))
        elif action == "actions/cache/save" and inputs.get("path") == LAKE_PACKAGES_CACHE_PATH:
            save_steps.append((index, step))
        elif "run" in step:
            fetch_steps.append((index, step))
        else:
            raise ValueError(f"{scope}: unexpected step {index} in dependency source action")

    if len(restore_steps) != 1 or len(fetch_steps) != 1 or len(save_steps) != 1 or len(steps) != 3:
        raise ValueError(
            f"{scope}: must contain exactly one restore, one fetch/verify ({FETCH_HELPER}) and one save step"
        )
    (restore_idx, restore_step), (fetch_idx, fetch_step), (save_idx, save_step) = (
        restore_steps[0], fetch_steps[0], save_steps[0]
    )
    if not (restore_idx < fetch_idx < save_idx):
        raise ValueError(f"{scope}: steps must run in strict restore, fetch, save order")

    restore_id = restore_step.get("id")
    if not isinstance(restore_id, str) or not restore_id.strip():
        raise ValueError(f"{scope}: restore step must declare an id")
    if "if" in restore_step:
        raise ValueError(f"{scope}: restore step must not be conditional")
    extra_restore = set(restore_step) - {"name", "id", "continue-on-error", "uses", "with"}
    if extra_restore:
        raise ValueError(f"{scope}: restore step sets unexpected keys {sorted(extra_restore)}")

    if "if" in fetch_step or "continue-on-error" in fetch_step:
        raise ValueError(f"{scope}: fetch/verify step must run unconditionally and fail closed")
    extra_fetch = set(fetch_step) - {"name", "shell", "run"}
    if extra_fetch:
        raise ValueError(f"{scope}: fetch/verify step sets unexpected keys {sorted(extra_fetch)}")
    if fetch_step.get("shell") != "bash":
        raise ValueError(
            f"{scope}: fetch/verify step must use shell 'bash' (got {fetch_step.get('shell')!r})"
        )
    run_cmd = fetch_step.get("run")
    if not isinstance(run_cmd, str) or run_cmd.strip() != EXPECTED_FETCH_RUN:
        raise ValueError(
            f"{scope}: fetch/verify step must run exact fail-closed command "
            f"{EXPECTED_FETCH_RUN!r} (got {run_cmd!r})"
        )

    if restore_step["with"]["key"] != save_step["with"]["key"]:
        raise ValueError(f"{scope}: restore and save steps must use the same key")
    expected_save_if = f"steps.{restore_id}.outputs.cache-hit != 'true'"
    if save_step.get("if") != expected_save_if:
        raise ValueError(f"{scope}: save step must be gated by {expected_save_if!r}")
    extra_save = set(save_step) - {"name", "id", "if", "continue-on-error", "uses", "with"}
    if extra_save:
        raise ValueError(f"{scope}: save step sets unexpected keys {sorted(extra_save)}")


def load_local_action(action_dir, scope, local_actions):
    if action_dir in local_actions:
        doc = local_actions[action_dir]
    else:
        import yaml

        base = Path(action_dir)
        action_file = None
        for candidate in (base / "action.yml", base / "action.yaml"):
            if candidate.is_file():
                action_file = candidate
                break
        if action_file is None:
            raise ValueError(f"{scope}: missing local action ./{action_dir}")
        doc = yaml.load(action_file.read_text(), Loader=yaml.BaseLoader)
        local_actions[action_dir] = doc

    if not isinstance(doc, dict):
        raise ValueError(f"{scope}: local action ./{action_dir} must be a mapping")
    runs = doc.get("runs")
    if not isinstance(runs, dict) or runs.get("using") != "composite":
        raise ValueError(f"{scope}: local action ./{action_dir} must be a composite action")
    steps = runs.get("steps")
    if not isinstance(steps, list):
        raise ValueError(f"{scope}: local action ./{action_dir} must define steps")
    return doc


def expand_steps(
    steps,
    *,
    scope,
    base_env,
    default_shell,
    default_workdir,
    conditional_or_soft,
    local_actions,
    visiting,
    events,
):
    for index, step in enumerate(steps):
        if not isinstance(step, dict):
            raise ValueError(f"{scope}: step {index} must be a mapping")
        step_scope = f"{scope}[{index}]"
        step_env = base_env | (step.get("env") or {})
        step_cond = conditional_or_soft or ("if" in step) or ("continue-on-error" in step)

        check_step_cache_policy(step_scope, step, allow_lake_packages_cache=False)

        run_cmd = step.get("run", "")
        effective_shell = step.get("shell") or default_shell
        if isinstance(run_cmd, str) and FETCH_HELPER in run_cmd:
            raise ValueError(
                f"{step_scope}: call {LAKE_PACKAGES_ACTION_USE} instead of inlining {FETCH_HELPER}"
            )
        if isinstance(effective_shell, str) and FETCH_HELPER in effective_shell:
            raise ValueError(
                f"{step_scope}: call {LAKE_PACKAGES_ACTION_USE} instead of inlining {FETCH_HELPER}"
            )

        uses = step.get("uses", "")
        local_dir = normalize_local_action_use(uses, step_scope) if isinstance(uses, str) else None
        if local_dir is not None:
            if local_dir == LAKE_PACKAGES_ACTION_DIR:
                if step_cond:
                    raise ValueError(
                        f"{step_scope}: {LAKE_PACKAGES_ACTION_USE} must run unconditionally and fail closed"
                    )
                events.append(("helper", step_scope, step_env, LAKE_PACKAGES_ACTION_USE))
                continue
            if local_dir in visiting:
                raise ValueError(f"{step_scope}: cyclic local action reference ./{local_dir}")
            action_doc = load_local_action(local_dir, step_scope, local_actions)
            expand_steps(
                action_doc["runs"]["steps"],
                scope=f"{step_scope}->./{local_dir}",
                base_env=step_env,
                default_shell="",
                default_workdir="",
                conditional_or_soft=step_cond,
                local_actions=local_actions,
                visiting=visiting | {local_dir},
                events=events,
            )
            continue

        effective_workdir = step.get("working-directory") or default_workdir
        subevents = step_shell_events(step, default_shell=default_shell)
        if any(kind == "lake" for kind, _ in subevents):
            norm_wd = normalize_rel_path(effective_workdir) if isinstance(effective_workdir, str) else ""
            if norm_wd and norm_wd != ".":
                raise ValueError(
                    f"{step_scope}: Lake step must run in repository root "
                    f"(got working-directory {effective_workdir!r})"
                )
        if subevents:
            for kind, detail in subevents:
                events.append((kind, step_scope, step_env, detail))
        else:
            events.append(("other", step_scope, step_env, ""))


def check_cache_steps(
    workflow,
    *,
    local_actions=None,
    require_all_jobs_no_artifact_cache=False,
    workflow_scope="<workflow>",
):
    actions_map = {} if local_actions is None else dict(local_actions)
    wf_env = workflow.get("env") or {}
    wf_run_defaults = (workflow.get("defaults") or {}).get("run") or {}
    wf_shell = wf_run_defaults.get("shell") or ""
    wf_workdir = wf_run_defaults.get("working-directory") or ""
    jobs = workflow.get("jobs")
    if not isinstance(jobs, dict) or not jobs:
        raise ValueError(f"{workflow_scope}: workflow must define jobs")

    for job_id, job in jobs.items():
        job_scope = f"{workflow_scope}:{job_id}"
        if not isinstance(job, dict):
            raise ValueError(f"{job_scope}: job must be a mapping")
        if "steps" not in job:
            if "uses" in job:
                raise ValueError(
                    f"{job_scope}: reusable workflow call {job['uses']!r} cannot bypass step checks"
                )
            continue
        job_env = wf_env | (job.get("env") or {})
        job_run_defaults = (job.get("defaults") or {}).get("run") or {}
        job_shell = job_run_defaults.get("shell") or wf_shell
        job_workdir = job_run_defaults.get("working-directory") or wf_workdir
        events = []
        expand_steps(
            job["steps"],
            scope=job_scope,
            base_env=job_env,
            default_shell=job_shell,
            default_workdir=job_workdir,
            conditional_or_soft=False,
            local_actions=actions_map,
            visiting=frozenset(),
            events=events,
        )

        # Invariant: every Lake invocation must be preceded by an unconditional
        # ./.github/actions/lake-packages verification AFTER the most recent checkout
        # or manifest/toolchain/packages mutation in that job.
        verified = False
        seen_helper = False
        has_any_helper = any(kind == "helper" for kind, _, _, _ in events)
        last_invalidation = None

        for kind, step_scope, env, detail in events:
            if kind == "helper":
                verified = True
                seen_helper = True
                last_invalidation = None
            elif kind == "invalidate":
                verified = False
                last_invalidation = (detail, step_scope)
            elif kind == "lake":
                if not verified:
                    if not has_any_helper:
                        raise ValueError(
                            f"{job_scope}: Lake-using job must invoke {LAKE_PACKAGES_ACTION_USE} "
                            f"before Lake step {step_scope}"
                        )
                    if seen_helper and last_invalidation is not None:
                        inv_reason, inv_scope = last_invalidation
                        raise ValueError(
                            f"{job_scope}: {LAKE_PACKAGES_ACTION_USE} must run after {inv_reason} "
                            f"({inv_scope}) before Lake step {step_scope}"
                        )
                    if not seen_helper:
                        raise ValueError(
                            f"{job_scope}: {LAKE_PACKAGES_ACTION_USE} must run before any Lake step "
                            f"(unverified Lake step {step_scope})"
                        )
                    raise ValueError(
                        f"{job_scope}: {LAKE_PACKAGES_ACTION_USE} must verify dependencies "
                        f"before Lake step {step_scope}"
                    )
                if env.get("LAKE_NO_CACHE") != "true" or env.get("LAKE_ARTIFACT_CACHE") != "false":
                    raise ValueError(
                        f"{step_scope}: build Lake packages locally with artifact caching disabled"
                    )

        if require_all_jobs_no_artifact_cache:
            for _kind, step_scope, env, _detail in events:
                if env.get("LAKE_NO_CACHE") != "true" or env.get("LAKE_ARTIFACT_CACHE") != "false":
                    raise ValueError(
                        f"{step_scope}: build Lake packages locally with artifact caching disabled"
                    )


def check_workflows(names):
    import yaml

    if not Path(FETCH_HELPER).is_file():
        raise ValueError(f"missing dependency fetch helper {FETCH_HELPER}")
    if not LAKE_PACKAGES_ACTION_PATH.is_file():
        raise ValueError(f"missing dependency cache action {LAKE_PACKAGES_ACTION_PATH}")

    action_paths = []
    if ACTIONS_DIR.is_dir():
        action_paths = sorted(
            p for p in ACTIONS_DIR.rglob("*")
            if p.is_file() and p.name in {"action.yml", "action.yaml"}
        )
    if LAKE_PACKAGES_ACTION_PATH not in action_paths:
        raise ValueError(f"missing {LAKE_PACKAGES_ACTION_PATH}")

    local_actions = {}
    for path in action_paths:
        parsed = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
        action_dir = path.parent.as_posix()
        local_actions[action_dir] = parsed
        if path == LAKE_PACKAGES_ACTION_PATH:
            check_lake_packages_action(parsed, str(path))
        else:
            runs = parsed.get("runs")
            if not isinstance(runs, dict) or runs.get("using") != "composite":
                raise ValueError(f"{path}: local action must be a composite action")
            steps = runs.get("steps")
            if not isinstance(steps, list):
                raise ValueError(f"{path}: local composite action must define steps")
            for step in steps:
                check_step_cache_policy(str(path), step, allow_lake_packages_cache=False)
                run_cmd = step.get("run", "")
                step_shell = step.get("shell", "")
                if (isinstance(run_cmd, str) and FETCH_HELPER in run_cmd) or (
                    isinstance(step_shell, str) and FETCH_HELPER in step_shell
                ):
                    raise ValueError(
                        f"{path}: call {LAKE_PACKAGES_ACTION_USE} instead of inlining {FETCH_HELPER}"
                    )
        print(f"  ok   {path}")

    lake_action_doc = local_actions[LAKE_PACKAGES_ACTION_DIR]

    for path in sorted(WORKFLOW.parent.iterdir()):
        if path.suffix in {".yml", ".yaml"}:
            # BaseLoader keeps the GitHub trigger key "on" as a string.
            parsed = yaml.load(path.read_text(), Loader=yaml.BaseLoader)
            check_cache_steps(
                parsed,
                local_actions=local_actions,
                require_all_jobs_no_artifact_cache=(path == WORKFLOW),
                workflow_scope=str(path),
            )
            print(f"  ok   {path}")

    workflow = yaml.load(WORKFLOW.read_text(), Loader=yaml.BaseLoader)
    check_inventory(workflow, names)

    # Controls on lean-action, checkout cleanliness, repository build paths, and restore-keys.
    retry_cache = copy.deepcopy(workflow)
    retry = next(step for step in retry_cache["jobs"]["spec"]["steps"]
                 if step.get("uses", "").startswith("leanprover/lean-action@") and "if" in step)
    retry["with"] = dict(retry["with"])
    del retry["with"]["use-github-cache"]  # The action defaults to restoring a previous build.
    expect_failure(
        check_cache_steps,
        "use-github-cache disabled",
        retry_cache,
        local_actions=local_actions,
    )

    dirty_checkout = copy.deepcopy(workflow)
    dirty_checkout["jobs"]["tests"]["steps"][0]["with"] = {"clean": "false"}
    expect_failure(
        check_cache_steps,
        "checkout must clean the workspace",
        dirty_checkout,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="actions/checkout with clean: false",
    )

    native_cache = copy.deepcopy(workflow)
    native_cache["jobs"]["tests"]["steps"].insert(1, {
        "uses": "actions/cache/restore@v4", "with": {"path": ".lake", "restore-keys": "native-"},
    })
    expect_failure(
        check_cache_steps,
        "cache paths",
        native_cache,
        local_actions=local_actions,
        label="workflow cache path .lake",
    )

    for bad_path, label in (
        (".lake", "cached path .lake (repo root cache dir)"),
        (".lake/build", "cached path .lake/build (repo build outputs)"),
        (".lake/packages\n.lake/build", "multiline cache path smuggling .lake/build"),
    ):
        bad_build_cache = copy.deepcopy(lake_action_doc)
        bad_build_cache["runs"]["steps"][0]["with"]["path"] = bad_path
        expect_failure(check_lake_packages_action, "cache paths", bad_build_cache, label=label)

    # Controls on exact cache key (exact two literal inputs, no exclusion patterns or bypasses).
    for step_indices, key_val, diag, label in (
        (
            (0,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "restore key without lake-manifest.json hash",
        ),
        (
            (2,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "save key without lake-manifest.json hash",
        ),
        (
            (0,),
            "${{ runner.os }}-lake-manifest.json-${{ hashFiles('lean-toolchain') }}",
            "must hash lake-manifest.json",
            "literal lake-manifest.json outside hashFiles(...)",
        ),
        (
            (0,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('!lake-manifest.json', 'lean-toolchain') }}",
            "exclusion pattern",
            "negated !lake-manifest.json in hashFiles(...)",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', '!lake-manifest.json') }}",
            "exclusion pattern",
            "trailing negated !lake-manifest.json excluding manifest from both keys",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', '!*.json') }}",
            "exclusion pattern",
            "trailing wildcard exclusion !*.json in hashFiles(...)",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', '!lean-toolchain') }}",
            "exclusion pattern",
            "trailing negated !lean-toolchain in hashFiles(...)",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain', 'README.md') }}",
            "must hash only literal lake-manifest.json and lean-toolchain",
            "extra unapproved input in .lake/packages hashFiles(...)",
        ),
        (
            (0,),
            "${{ false && hashFiles('lake-manifest.json', 'lean-toolchain') || 'static' }}",
            "must hash lake-manifest.json",
            "conditional expression bypassing hashFiles(...)",
        ),
        (
            (0,),
            "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json') }}",
            "must hash lean-toolchain",
            "cache key without lean-toolchain hash",
        ),
        (
            (0, 2),
            "lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}",
            "must include ${{ runner.os }}",
            "matching restore/save keys omitting runner.os",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ github.run_id }}-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}",
            "dynamic or extra expressions",
            "matching restore/save keys containing unique github.run_id",
        ),
        (
            (0, 2),
            "${{ runner.os }}-lake-packages-sources-${{ github.sha }}-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}",
            "dynamic or extra expressions",
            "matching restore/save keys containing per-commit github.sha",
        ),
        (
            (0, 2),
            "${{ runner.os }}-other-prefix-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}",
            "cache key must match",
            "matching restore/save keys with non-standard prefix",
        ),
    ):
        mutated = copy.deepcopy(lake_action_doc)
        for step_idx in step_indices:
            mutated["runs"]["steps"][step_idx]["with"]["key"] = key_val
        expect_failure(check_lake_packages_action, diag, mutated, label=label)

    # Control: exclusion pattern in external tool cache key is also rejected.
    excluded_ext = copy.deepcopy(workflow)
    ext_cache = next(step for step in excluded_ext["jobs"]["external-recheck"]["steps"]
                     if step.get("uses", "").startswith("actions/cache@"))
    ext_cache["with"]["key"] = (
        "${{ runner.os }}-concrete-ext-${{ hashFiles('scripts/proofs/external.pins', '!scripts/proofs/external.pins') }}"
    )
    expect_failure(
        check_cache_steps,
        "exclusion pattern",
        excluded_ext,
        local_actions=local_actions,
        label="exclusion pattern in ~/.cache/concrete-ext hashFiles(...)",
    )

    # Controls on restore-keys fallback (on both .lake/packages and ~/.cache/concrete-ext).
    restore_keys_action = copy.deepcopy(lake_action_doc)
    restore_keys_action["runs"]["steps"][0]["with"]["restore-keys"] = "${{ runner.os }}-lake-packages-sources-"
    expect_failure(
        check_lake_packages_action,
        "restore-keys fallback",
        restore_keys_action,
        label="restore-keys fallback on .lake/packages cache",
    )

    lookup_only_action = copy.deepcopy(lake_action_doc)
    lookup_only_action["runs"]["steps"][0]["with"]["lookup-only"] = "true"
    expect_failure(
        check_lake_packages_action,
        "unexpected inputs",
        lookup_only_action,
        label="lookup-only input on .lake/packages cache restore",
    )

    restore_keys_ext = copy.deepcopy(workflow)
    ext_cache = next(step for step in restore_keys_ext["jobs"]["external-recheck"]["steps"]
                     if step.get("uses", "").startswith("actions/cache@"))
    ext_cache["with"]["restore-keys"] = "${{ runner.os }}-concrete-ext-"
    expect_failure(
        check_cache_steps,
        "restore-keys fallback",
        restore_keys_ext,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="restore-keys fallback on ~/.cache/concrete-ext cache",
    )

    # Controls on sources-only save discipline, exact fail-closed helper invocation, and step ordering.
    combined_cache_action = copy.deepcopy(lake_action_doc)
    combined_cache_action["runs"]["steps"][0]["uses"] = "actions/cache@v4"
    expect_failure(
        check_lake_packages_action,
        "split restore/save before build",
        combined_cache_action,
        label="combined post-build actions/cache@v4 on .lake/packages",
    )

    mismatched_keys_action = copy.deepcopy(lake_action_doc)
    mismatched_keys_action["runs"]["steps"][2]["with"]["key"] = (
        "${{ runner.os }}-lake-packages-sources-v2-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}"
    )
    expect_failure(
        check_lake_packages_action,
        "must use the same key",
        mismatched_keys_action,
        label="mismatched restore and save cache keys",
    )

    unconditional_save_action = copy.deepcopy(lake_action_doc)
    del unconditional_save_action["runs"]["steps"][2]["if"]
    expect_failure(
        check_lake_packages_action,
        "save step must be gated",
        unconditional_save_action,
        label="save step without cache-hit guard",
    )

    reordered_action = copy.deepcopy(lake_action_doc)
    reordered_action["runs"]["steps"][1], reordered_action["runs"]["steps"][2] = (
        reordered_action["runs"]["steps"][2], reordered_action["runs"]["steps"][1]
    )
    expect_failure(
        check_lake_packages_action,
        "strict restore, fetch, save order",
        reordered_action,
        label="save step placed before fetch/verify step",
    )

    missing_fetch_action = copy.deepcopy(lake_action_doc)
    del missing_fetch_action["runs"]["steps"][1]
    expect_failure(
        check_lake_packages_action,
        "must contain exactly one restore, one fetch/verify",
        missing_fetch_action,
        label="action omitting fetch_lake_packages.sh",
    )

    soft_fetch_action = copy.deepcopy(lake_action_doc)
    soft_fetch_action["runs"]["steps"][1]["continue-on-error"] = "true"
    expect_failure(
        check_lake_packages_action,
        "fail closed",
        soft_fetch_action,
        label="fetch/verify step with continue-on-error",
    )

    for bad_run, label in (
        (f"bash -n {FETCH_HELPER}", "bash -n syntax check instead of executing helper"),
        (f"{EXPECTED_FETCH_RUN} || true", "helper command swallowing failure with || true"),
        (f"echo {FETCH_HELPER}", "no-op command merely mentioning helper path"),
        (f"# {FETCH_HELPER}\ntrue", "comment mentioning helper path followed by true"),
    ):
        noop_fetch = copy.deepcopy(lake_action_doc)
        noop_fetch["runs"]["steps"][1]["run"] = bad_run
        expect_failure(
            check_lake_packages_action,
            "exact fail-closed command",
            noop_fetch,
            label=label,
        )

    bad_shell_fetch = copy.deepcopy(lake_action_doc)
    bad_shell_fetch["runs"]["steps"][1]["shell"] = "bash -n {0}"
    expect_failure(
        check_lake_packages_action,
        "must use shell 'bash'",
        bad_shell_fetch,
        label="fetch/verify step overriding shell to syntax-check only",
    )

    bad_workdir_fetch = copy.deepcopy(lake_action_doc)
    bad_workdir_fetch["runs"]["steps"][1]["working-directory"] = "subdir"
    expect_failure(
        check_lake_packages_action,
        "unexpected keys",
        bad_workdir_fetch,
        label="fetch/verify step changing working-directory",
    )

    bad_env_fetch = copy.deepcopy(lake_action_doc)
    bad_env_fetch["runs"]["steps"][1]["env"] = {"PATH": "/tmp/fake"}
    expect_failure(
        check_lake_packages_action,
        "unexpected keys",
        bad_env_fetch,
        label="fetch/verify step injecting env overrides",
    )

    extra_step_action = copy.deepcopy(lake_action_doc)
    extra_step_action["runs"]["steps"].append({"shell": "bash", "run": "echo extra"})
    expect_failure(
        check_lake_packages_action,
        "must contain exactly one restore, one fetch/verify",
        extra_step_action,
        label="extra step smuggled into dependency source action",
    )

    lake_in_action = copy.deepcopy(lake_action_doc)
    lake_in_action["runs"]["steps"].insert(2, {"shell": "bash", "run": "lake build"})
    expect_failure(
        check_lake_packages_action,
        "must not run Lake",
        lake_in_action,
        label="lake build inside dependency source action before save",
    )

    # Controls on workflow job wiring (every Lake job across ALL workflows and local composite actions).
    missing_helper = copy.deepcopy(workflow)
    missing_helper["jobs"]["tests"]["steps"] = [
        step for step in missing_helper["jobs"]["tests"]["steps"]
        if step.get("uses") != LAKE_PACKAGES_ACTION_USE
    ]
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        missing_helper,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="Lake-using job omitting ./.github/actions/lake-packages",
    )

    conditional_helper = copy.deepcopy(workflow)
    cond_step = next(step for step in conditional_helper["jobs"]["tests"]["steps"]
                     if step.get("uses") == LAKE_PACKAGES_ACTION_USE)
    cond_step["if"] = "false"
    expect_failure(
        check_cache_steps,
        "must run unconditionally",
        conditional_helper,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="conditional ./.github/actions/lake-packages step",
    )

    soft_helper = copy.deepcopy(workflow)
    soft_step = next(step for step in soft_helper["jobs"]["tests"]["steps"]
                     if step.get("uses") == LAKE_PACKAGES_ACTION_USE)
    soft_step["continue-on-error"] = "true"
    expect_failure(
        check_cache_steps,
        "must run unconditionally",
        soft_helper,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="continue-on-error ./.github/actions/lake-packages step",
    )

    late_helper = copy.deepcopy(workflow)
    spec_steps = late_helper["jobs"]["spec"]["steps"]
    helper_step = next(step for step in spec_steps if step.get("uses") == LAKE_PACKAGES_ACTION_USE)
    spec_steps.remove(helper_step)
    spec_steps.append(helper_step)
    expect_failure(
        check_cache_steps,
        "must run before any Lake step",
        late_helper,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="./.github/actions/lake-packages placed after lean-action",
    )

    # Control: second workflow (not lean_action_ci.yml) with checkout + direct lake build and no helper.
    second_workflow_unwired = {
        "name": "Extra",
        "on": "workflow_dispatch",
        "jobs": {
            "extra-lake": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        second_workflow_unwired,
        local_actions=local_actions,
        workflow_scope=".github/workflows/extra.yml",
        label="second workflow running lake build without ./.github/actions/lake-packages",
    )

    # Control: second workflow with helper but without LAKE_NO_CACHE / LAKE_ARTIFACT_CACHE.
    second_workflow_no_env = {
        "name": "Extra",
        "on": "workflow_dispatch",
        "jobs": {
            "extra-lake": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": LAKE_PACKAGES_ACTION_USE},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "artifact caching disabled",
        second_workflow_no_env,
        local_actions=local_actions,
        workflow_scope=".github/workflows/extra.yml",
        label="second workflow running lake build without disabling Lake artifact cache",
    )

    # Positive control: properly wired second workflow passes.
    second_workflow_valid = copy.deepcopy(second_workflow_no_env)
    second_workflow_valid["env"] = {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"}
    check_cache_steps(
        second_workflow_valid,
        local_actions=local_actions,
        workflow_scope=".github/workflows/extra.yml",
    )
    print("  ok   control accepts properly wired second workflow")

    # Controls: local composite action hiding a Lake build (direct, nested, late helper, conditional wrapper).
    actions_with_lake = dict(local_actions)
    actions_with_lake[".github/actions/run-lake"] = {
        "runs": {
            "using": "composite",
            "steps": [{"shell": "bash", "run": "lake build"}],
        },
    }
    actions_with_lake[".github/actions/outer-lake"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": "./.github/actions/run-lake"}],
        },
    }
    actions_with_lake[".github/actions/wrap-helper"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": LAKE_PACKAGES_ACTION_USE}],
        },
    }
    actions_with_lake[".github/actions/cycle-a"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": "./.github/actions/cycle-b"}],
        },
    }
    actions_with_lake[".github/actions/cycle-b"] = {
        "runs": {
            "using": "composite",
            "steps": [{"uses": "./.github/actions/cycle-a"}],
        },
    }

    composite_lake_unwired = {
        "name": "CompositeLake",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": "./.github/actions/run-lake"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        composite_lake_unwired,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="Lake build hidden inside local composite action without helper",
    )

    nested_composite_unwired = copy.deepcopy(composite_lake_unwired)
    nested_composite_unwired["jobs"]["build"]["steps"][1] = {"uses": "./.github/actions/outer-lake"}
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        nested_composite_unwired,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="Lake build hidden inside nested local composite action without helper",
    )

    composite_lake_before_helper = copy.deepcopy(composite_lake_unwired)
    composite_lake_before_helper["jobs"]["build"]["steps"].append({"uses": LAKE_PACKAGES_ACTION_USE})
    expect_failure(
        check_cache_steps,
        "must run before any Lake step",
        composite_lake_before_helper,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="local composite action running Lake before ./.github/actions/lake-packages",
    )

    conditional_wrapper_helper = {
        "name": "CondWrap",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"if": "false", "uses": "./.github/actions/wrap-helper"},
                    {"uses": "./.github/actions/run-lake"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "must run unconditionally",
        conditional_wrapper_helper,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/composite.yml",
        label="conditional local composite wrapper around ./.github/actions/lake-packages",
    )

    checkout_after_helper = {
        "name": "LateCheckout",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": LAKE_PACKAGES_ACTION_USE},
                    {"uses": "actions/checkout@v4"},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "must run after actions/checkout",
        checkout_after_helper,
        local_actions=local_actions,
        workflow_scope=".github/workflows/late-checkout.yml",
        label="actions/checkout wiping .lake/packages after ./.github/actions/lake-packages",
    )

    # Finding 4 controls: Lake invoked via job defaults.run.shell or step shell:.
    shell_lake_default = {
        "name": "ShellLake",
        "on": "workflow_dispatch",
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "defaults": {"run": {"shell": "lake env bash {0}"}},
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"run": "lean ConcreteSpec.lean"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        shell_lake_default,
        local_actions=local_actions,
        workflow_scope=".github/workflows/shell-lake.yml",
        label="defaults.run.shell invoking lake env bash without helper",
    )

    shell_lake_default_no_env = copy.deepcopy(shell_lake_default)
    shell_lake_default_no_env["jobs"]["build"]["steps"].insert(1, {"uses": LAKE_PACKAGES_ACTION_USE})
    expect_failure(
        check_cache_steps,
        "artifact caching disabled",
        shell_lake_default_no_env,
        local_actions=local_actions,
        workflow_scope=".github/workflows/shell-lake.yml",
        label="defaults.run.shell invoking lake env bash without disabling artifact cache",
    )

    shell_lake_step = {
        "name": "StepShellLake",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"shell": "lake env bash {0}", "run": "lean ConcreteSpec.lean"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        f"must invoke {LAKE_PACKAGES_ACTION_USE}",
        shell_lake_step,
        local_actions=local_actions,
        workflow_scope=".github/workflows/step-shell-lake.yml",
        label="step shell invoking lake env bash without helper",
    )

    shell_lake_valid = copy.deepcopy(shell_lake_default_no_env)
    shell_lake_valid["env"] = {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"}
    check_cache_steps(
        shell_lake_valid,
        local_actions=local_actions,
        workflow_scope=".github/workflows/shell-lake.yml",
    )
    print("  ok   control accepts properly wired defaults.run.shell lake env bash job")

    # Finding 5 controls: second checkout or manifest/git mutation invalidates verification
    # before a subsequent Lake build; re-verifying after the second checkout is accepted.
    second_checkout_unverified = {
        "name": "SecondCheckout",
        "on": "workflow_dispatch",
        "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
        "jobs": {
            "compare": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"uses": LAKE_PACKAGES_ACTION_USE},
                    {"run": "lake build"},
                    {"uses": "actions/checkout@v4", "with": {"ref": "main"}},
                    {"run": "lake build"},
                ],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "must run after actions/checkout",
        second_checkout_unverified,
        local_actions=local_actions,
        workflow_scope=".github/workflows/second-checkout.yml",
        label="second actions/checkout between two Lake builds without re-verification",
    )

    second_checkout_reverified = copy.deepcopy(second_checkout_unverified)
    second_checkout_reverified["jobs"]["compare"]["steps"].insert(4, {"uses": LAKE_PACKAGES_ACTION_USE})
    check_cache_steps(
        second_checkout_reverified,
        local_actions=local_actions,
        workflow_scope=".github/workflows/second-checkout.yml",
    )
    print("  ok   control accepts second actions/checkout when followed by re-verification")

    for bad_steps, diag, label in (
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "lake build"},
                {"run": "git checkout main"},
                {"run": "lake build"},
            ],
            "must run after git checkout",
            "git checkout between two Lake builds without re-verification",
        ),
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "git checkout main && lake build"},
            ],
            "must run after git checkout",
            "git checkout followed by lake build in the same step",
        ),
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "cp other-manifest.json lake-manifest.json"},
                {"run": "lake build"},
            ],
            "must run after cp lake-manifest.json",
            "overwriting lake-manifest.json before Lake build without re-verification",
        ),
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "printf 'leanprover/lean4:v4.35.0\\n' > lean-toolchain"},
                {"run": "lake build"},
            ],
            "must run after write to lean-toolchain",
            "rewriting lean-toolchain before Lake build without re-verification",
        ),
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "lake build"},
                {"run": "rm -rf .lake/packages"},
                {"run": "lake build"},
            ],
            "must run after rm .lake/packages",
            "deleting .lake/packages before second Lake build without re-verification",
        ),
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "lake update"},
                {"run": "lake build"},
            ],
            "must run after lake update",
            "lake update mutating manifest/packages before subsequent Lake build",
        ),
        (
            [
                {"uses": "actions/checkout@v4"},
                {"uses": LAKE_PACKAGES_ACTION_USE},
                {"run": "lake build"},
                {"if": "always()", "uses": "actions/checkout@v4"},
                {"run": "lake build"},
            ],
            "must run after actions/checkout",
            "conditional second checkout invalidating verification before later Lake build",
        ),
    ):
        mutated_wf = {
            "name": "Mutated",
            "on": "workflow_dispatch",
            "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
            "jobs": {"build": {"runs-on": "ubuntu-latest", "steps": bad_steps}},
        }
        expect_failure(
            check_cache_steps,
            diag,
            mutated_wf,
            local_actions=local_actions,
            workflow_scope=".github/workflows/mutated.yml",
            label=label,
        )

    # Non-blocking 3 controls: filename mentions (sha256sum lake-manifest.json) are not
    # classified as Lake builds, while real indirect Lake invocations still are.
    manifest_only_job = {
        "name": "ManifestHash",
        "on": "workflow_dispatch",
        "jobs": {
            "checksum": {
                "runs-on": "ubuntu-latest",
                "steps": [
                    {"uses": "actions/checkout@v4"},
                    {"run": "sha256sum lake-manifest.json"},
                    {"run": "cat lake-manifest.json && echo lake"},
                ],
            },
        },
    }
    check_cache_steps(
        manifest_only_job,
        local_actions=local_actions,
        workflow_scope=".github/workflows/manifest-only.yml",
    )
    print("  ok   control accepts manifest-only checksum job without requiring Lake package fetch")

    for hidden_cmd, label in (
        ("bash -c 'lake build'", "bash -c running lake build without helper"),
        ("env FOO=1 /usr/local/bin/lake build", "env-wrapped absolute /usr/local/bin/lake build without helper"),
        ("bash <<'EOF'\nlake build\nEOF", "bash heredoc running lake build without helper"),
    ):
        hidden_lake_wf = {
            "name": "HiddenLake",
            "on": "workflow_dispatch",
            "env": {"LAKE_NO_CACHE": "true", "LAKE_ARTIFACT_CACHE": "false"},
            "jobs": {
                "build": {
                    "runs-on": "ubuntu-latest",
                    "steps": [
                        {"uses": "actions/checkout@v4"},
                        {"run": hidden_cmd},
                    ],
                },
            },
        }
        expect_failure(
            check_cache_steps,
            f"must invoke {LAKE_PACKAGES_ACTION_USE}",
            hidden_lake_wf,
            local_actions=local_actions,
            workflow_scope=".github/workflows/hidden-lake.yml",
            label=label,
        )

    cyclic_composite = {
        "name": "Cyclic",
        "on": "workflow_dispatch",
        "jobs": {
            "build": {
                "runs-on": "ubuntu-latest",
                "steps": [{"uses": "./.github/actions/cycle-a"}],
            },
        },
    }
    expect_failure(
        check_cache_steps,
        "cyclic local action reference",
        cyclic_composite,
        local_actions=actions_with_lake,
        workflow_scope=".github/workflows/cyclic.yml",
        label="cyclic local composite action reference",
    )

    inlined_cache = copy.deepcopy(workflow)
    inlined_cache["jobs"]["tests"]["steps"].insert(1, {
        "uses": "actions/cache/restore@v4",
        "with": {
            "path": ".lake/packages",
            "key": "${{ runner.os }}-lake-packages-sources-${{ hashFiles('lake-manifest.json', 'lean-toolchain') }}",
        },
    })
    expect_failure(
        check_cache_steps,
        f"must live only in {LAKE_PACKAGES_ACTION_USE}",
        inlined_cache,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="inlined .lake/packages cache step in workflow job",
    )

    inlined_fetch = copy.deepcopy(workflow)
    inlined_fetch["jobs"]["grammar"]["steps"].insert(2, {
        "shell": "bash",
        "run": f"bash {FETCH_HELPER}",
    })
    expect_failure(
        check_cache_steps,
        f"instead of inlining {FETCH_HELPER}",
        inlined_fetch,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="inlined fetch_lake_packages.sh in workflow job",
    )

    artifact_cache_on = copy.deepcopy(workflow)
    artifact_cache_on["env"]["LAKE_ARTIFACT_CACHE"] = "true"
    expect_failure(
        check_cache_steps,
        "artifact caching disabled",
        artifact_cache_on,
        local_actions=local_actions,
        require_all_jobs_no_artifact_cache=True,
        label="LAKE_ARTIFACT_CACHE enabled",
    )

    # Inventory and verdict controls.
    extra_leg = copy.deepcopy(workflow)
    extra_leg["jobs"]["inventory-control"] = {
        "name": "Inventory control (${{ matrix.arch }})",
        "strategy": {"matrix": {"include": [{"arch": "listed"}, {"arch": "unlisted"}]}},
    }
    extra_leg["jobs"][AGGREGATE]["needs"].append("inventory-control")
    expect_failure(check_inventory, "Inventory control (unlisted)", extra_leg,
                   [*names, "Inventory control (listed)"])
    expect_failure(check_inventory, "Retired gate", workflow, [*names, "Retired gate"])
    missing_dependency = copy.deepcopy(workflow)
    missing_dependency["jobs"][AGGREGATE]["needs"].pop()
    expect_failure(check_inventory, "must depend on every gate job", missing_dependency, names)
    skipped_aggregate = copy.deepcopy(workflow)
    skipped_aggregate["jobs"][AGGREGATE]["if"] = "${{ success() }}"
    expect_failure(check_inventory, "must always run", skipped_aggregate, names)

    jobs = [{"name": name, "conclusion": "success"} for name in names]
    require_success(names, [{"jobs": jobs[:1]}, {"jobs": jobs[1:]}])
    for name, conclusion in ((names[0], "skipped"), (names[-1], "neutral")):
        changed = [dict(job, conclusion=conclusion) if job["name"] == name else job for job in jobs]
        expect_failure(require_success, f"{name}: {conclusion}", names, [{"jobs": changed}])
    expect_failure(require_success, f"{names[-1]}: missing", names, [{"jobs": jobs[:-1]}])


def main():
    names = gate_names()
    if sys.argv[1:] == ["verdict"]:
        require_success(names, json.load(sys.stdin))
        print(f"All {len(names)} landing gate jobs succeeded.")
    elif not sys.argv[1:]:
        check_workflows(names)
    else:
        raise ValueError("usage: workflow_checks.py [verdict]")


if __name__ == "__main__":
    try:
        main()
    except (OSError, KeyError, TypeError, ValueError) as error:
        sys.exit(f"Workflow checks: {error}")

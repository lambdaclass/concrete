#!/usr/bin/env python3
"""IMPACT SELECTION: which workflow gate COMMANDS can a set of changed files break?

Usage: select_gates.py --inventory FILE [--changed FILE] [--explain] [--durations TSV]
  --inventory  the runner's resolved command list (`run_ci_gates_local.sh --plan`), one per line
  --changed    changed paths, one per line (default: stdin)
  --explain    print `command<TAB>reason` instead of the bare command
  --durations  `command<TAB>seconds` table; prints an estimate to stderr

The unit is a workflow COMMAND, not a script: `run_tests.sh` and `run_tests.sh --trust-gate`
are two gates, and selecting the script selects both.

THE MAPPING IS DERIVED, NOT CURATED. A hand list of "this area -> these gates" (the hook's area
filters) rots: four gates broke in CI on 2026-10-22 that the area filters did not select. Here
every edge is read out of the tree on each run:

  1. script dependencies — every tracked path a gate script names (outside full-line comments),
     resolved from the repository root and from the script's own directory; a glob such as
     `tests/programs/*.con` is a dependency on the directory. Scripts a gate names (sourced
     libraries, helper .py files, other gates it runs) are followed transitively, so a change
     to scripts/tests/lib/X selects every gate that reaches X. Text is not a reference: a heredoc
     no interpreter reads names nothing, nor does a keyed array value the script uses only as
     text (a corpus note, an exemption reason; bug 237, shell_text.py); a name on a line that
     only greps or prints it is a read, not a run.
  2. compiler dependencies — a gate whose closure runs the compiler binary
     (`.lake/build/bin/concrete`, `require_fresh_binary`, `run_tests.sh`) or Lean (`lake env
     lean`, `lake build`) is selected by any change to a Lean module in the import closure of
     Main.lean, the toolchain, or the bundled stdlib (the compiler reads std/ at run time).
     A Lean module outside that closure (proofs/, PipelineTest) selects only the Lean-running
     gates and the gates that name it.
  3. derived artifacts — the curated ALWAYS_IF table of gates_for_diff.py (one owner), for gates
     that check a derivation of a whole tree without naming the changed file; and its
     EVERY_CHANGE list, cheap gates over the whole tree that any non-empty diff selects (a
     selection that maps no file, so it never satisfies rule 4).
  4. FAIL-SAFE — a changed file that no rule maps and that is not known-inert documentation
     selects EVERY gate. "Nothing names it" must never read as "nothing can break".

NOT a proof of completeness: a gate can depend on a file through a path it computes at run
time. Rule 4 and the compiler rule exist so that the error is always over-selection.
"""
import argparse
import os
import re
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import gates_for_diff as gfd  # noqa: E402  (ALWAYS_IF: the one owner of the derived-artifact table)
import shell_text as st  # noqa: E402  (which contexts of a script are text)

SCRIPT_EXT = (".sh", ".py")
GATE_NAME_RE = re.compile(r"(check|run|test)_[A-Za-z0-9_]+\.sh$")
DATA_EXT = (".txt", ".toml", ".tsv", ".json", ".list", ".csv")
# Directory references that are NOT a dependency on every file inside: gate scripts reach their
# helpers as "$ROOT/scripts/tests/$name", which would make every gate depend on every script. A
# script-to-script edge is taken from the script's NAME instead; a real `scripts/tests/check_*.sh`
# glob (a gate over all gates) is still kept.
# Data files whose rows are path PREFIXES used to classify paths, never paths a gate reads: indirecting
# through them would make every gate that reaches their reader depend on the whole tree (a mention
# read as a use, class 19). docs/bugs/area_map.tsv maps the paths fix commits touched to compiler
# areas for scripts/gen/bug_areas.py; what it classifies is git history, not files on disk. That
# history dependence (a commit naming a bug can make docs/bugs/AREAS.md stale whatever it touches)
# is one no file-based rule can express: it is covered by check_docs_drift.sh, which the pre-push
# hook runs on EVERY push (alone for a docs-only push, inside run_fast_surface_gates.sh otherwise)
# and CI runs in the fast surface gates. The table itself is still a file the ledger gates name.
PATH_CLASSIFIERS = {"docs/bugs/area_map.tsv"}
SCRIPT_TREES = {"scripts", "scripts/tests", "scripts/tests/lib", "scripts/ci", "scripts/tools",
                "scripts/gen", "scripts/reduce"}
COMPILER_RE = re.compile(
    r"\.lake/build/bin/concrete|require_fresh_binary|lake build|lake exe|\bmake (build|test)")
# Lean run on something other than the compiler. A bare `lake build` (every default target,
# require_fresh_binary's rebuild) is not here: the pre-push hook and CI both build every default
# target before any gate, so a proof that does not build fails there, not in a gate.
LEAN_RE = re.compile(r"lake env\b|lake lean\b|lake build[ \t]+[A-Za-z\"'$]")
# A line that only reads or prints a script's text, as opposed to running it.
DATA_RE = re.compile(r"\b(grep|egrep|rg|sed|awk|echo|printf|cat|diff|cmp|wc|head|tail|ok|no|fail|die|note|warn)\b"
                     r"|\bopen\(|read_text\(|\.read\(\)|^\s*for\s+\w+\s+in\b")
EXEC_RE = re.compile(r"(?<![\w./-])(bash|sh|python3|python|source|exec|timeout|env)\b|^\s*\.\s|\$\(\s*\"?\$")
TOKEN_RE = re.compile(r"[A-Za-z0-9_.][A-Za-z0-9_./-]*")
# Changes that select the compiler/Lean gates regardless of module.
TOOLCHAIN = {"lakefile.toml", "lake-manifest.json", "lean-toolchain", "flake.nix", "flake.lock",
             "Makefile"}
# Files nothing executes. A change here that no gate names selects nothing.
INERT_RE = re.compile(r"(^|/)[^/]*\.md$|^(docs|research|paper|site|editor|bench)/|^LICENSE$|\.png$")

# KEYED ELEMENTS: `declare -A NAME=( [key]="value" ... )` and `NAME[key]="value"`. A value is
# usually TEXT — the bug corpus's per-bug notes, a gate's exemption reasons — and a script name
# in a note is not a call: naming the docs-drift gate in a corpus note made run_tests.sh, which
# runs the corpus audit, look as if it ran that gate and read docs/ (bug 237). Each array's
# values, and separately its keys, are judged by how the script uses them (shell_text.use_level):
# used only as text, a value names nothing; used as a file to read, it is a read and never a run;
# anything else, including every context the reader does not know, makes it code as before. A key
# is at least a read (a lookup string matched against paths the script enumerates). An element
# holding a command substitution, or spanning lines, is always code. A literal with no keyed
# element (a list of gates to run), or one whose end the reader cannot place, is judged line by
# line as before.
ARRAY_START_RE = re.compile(
    r"\s*(?:(?:declare|local|typeset|readonly)\s+(?:-[A-Za-z]+\s+)*)?([A-Za-z_][A-Za-z0-9_]*)\+?=\(")
ELEM_ASSIGN_RE = re.compile(r"\s*([A-Za-z_][A-Za-z0-9_]*)\[([^\]\n]*)\]\+?=(\S.*)$")


def _array_elements(name, elems):
    out = []
    for key, value in elems:
        if key is None:
            out.append((value, "code"))
            continue
        for part, text in (("key", key), ("value", value)):
            # A substitution runs at assignment. A value over several lines is code too: the
            # corpus notes are one line each, and a quote misread across lines must not turn the
            # code it swallowed into text.
            if "$(" in text or "`" in text:
                out.append((text, "subst"))  # runs at assignment: a call, whatever it spells
            else:
                out.append((text, "code" if "\n" in text else (part, name)))
    return out


# The estimate scales the selection's share of CI serial time to the measured local full-set run
# (.githooks/pre-push header: 23 minutes on an 18-core machine at JOBS=12, nothing else running).
# It ignores the critical path, so a selection dominated by one long gate is underestimated.
FULL_SET_LOCAL_MIN = 23


WORKFLOW = ".github/workflows/lean_action_ci.yml"
# A replica of run_ci_gates_local.sh's `_extract_cmds` pattern and `_norm_cmd`. It is used only to
# tell which commands a workflow edit ADDED OR CHANGED; if it ever disagrees with the runner, the
# disagreement shows up as "new" commands, i.e. as over-selection.
CMD_RE = re.compile(
    r"(?:[A-Z_][A-Z0-9_]*=[^ ;|&]+\s+)*(?:(?:bash|python3|sh)\s+)?(?:\./)?scripts/[^ ;|&]*\.(?:sh|py)[^;|&\n]*")


def norm_cmd(cmd):
    cmd = re.sub(r"\s[0-9]*[<>].*$", "", cmd).rstrip()
    return cmd.replace("bash ./", "bash ", 1)


def workflow_commands(text):
    return {norm_cmd(m) for m in CMD_RE.findall(text)}


def git_lines(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True, check=True).stdout.split("\n")


class Repo:
    def __init__(self, extra_files):
        self.files = {f for f in git_lines("ls-files") if f} | {f for f in extra_files if f}
        self.dirs = set()
        for f in self.files:
            d = os.path.dirname(f)
            while d and d not in self.dirs:
                self.dirs.add(d)
                d = os.path.dirname(d)
        self._scripts = {}
        self._data = {}
        self.lean_closure = self._main_closure()

    # ---- Lean import graph -------------------------------------------------------------
    def _module_file(self, mod):
        rel = mod.replace(".", "/") + ".lean"
        for cand in (rel, "proofs/" + rel):
            if cand in self.files:
                return cand
        return None

    def _main_closure(self):
        seen, stack = set(), ["Main.lean"]
        while stack:
            f = stack.pop()
            if f in seen or not os.path.isfile(f):
                continue
            seen.add(f)
            for line in open(f, errors="ignore"):
                m = re.match(r"\s*(?:public\s+)?import\s+(.*)", line)
                if m:
                    for mod in m.group(1).split():
                        p = self._module_file(mod)
                        if p:
                            stack.append(p)
        return seen

    # ---- script dependency extraction ----------------------------------------------------
    def _resolve(self, tok, base, glob_follows):
        """-> ('file'|'dir', path) or None.

        Leading components that name no directory (`ROOT_DIR/` from "$ROOT_DIR/...") are
        dropped; the first suffix that starts at a real directory (from the root or the script's
        own directory) is the reference, resolved there or not at all, so `scripts/tests/$g`
        never degrades into the unrelated top-level `tests/`."""
        tok = tok.rstrip(".,;:")
        while tok.startswith("./"):
            tok = tok[2:]
        parts = tok.split("/")
        for i in range(len(parts)):
            s = "/".join(parts[i:])
            if not s:
                continue
            for cand in (s, os.path.normpath(os.path.join(base, s))):
                c = cand.rstrip("/")
                if c in self.files:
                    return ("file", c)
                if "/" in s and c in self.dirs:
                    if c in SCRIPT_TREES and glob_follows != "*":
                        return None
                    return ("dir", c)
            if glob_follows and "/" in s:
                parent = os.path.dirname(s.rstrip("/")) if not s.endswith("/") else s.rstrip("/")
                for cand in (parent, os.path.normpath(os.path.join(base, parent))):
                    if cand in self.dirs:
                        if cand in SCRIPT_TREES and glob_follows != "*":
                            return None
                        return ("dir", cand)
            head = parts[i]
            if "/" in s and (head in self.dirs or os.path.normpath(os.path.join(base, head)) in self.dirs):
                return None  # anchored at a real directory and unresolved: a stale or computed path
        return None

    def deps_of_text(self, text, base):
        files, dirs = set(), set()
        for m in TOKEN_RE.finditer(text):
            nxt = text[m.end():m.end() + 1]
            glob = nxt if nxt in ("*", "{", "$", "[") else ""
            hit = self._resolve(m.group(0), base, glob)
            if hit:
                (files if hit[0] == "file" else dirs).add(hit[1])
        return files, dirs

    @staticmethod
    def _logical_lines(raw):
        """-> [(line, kind)]: full-line comments dropped, backslash continuations joined.
        kind is "code"; "data" for the body of a heredoc that no interpreter reads (`cat <<EOF`:
        a fixture list, a generated program); or ("key"|"value", NAME) for one element of a keyed
        array assignment (see KEYED ELEMENTS below). A heredoc fed to python3/bash is code, and
        its lines are judged like any other."""
        out, buf, heredoc, hkind, hexpand = [], "", None, "code", False
        raws = raw.split("\n")
        i = 0
        while i < len(raws):
            line = raws[i]
            i += 1
            if heredoc is not None:
                if line.strip() == heredoc:
                    heredoc = None
                elif hkind == "data":
                    # "xdata": the delimiter is unquoted, so the shell expands the body
                    out.append((line, "xdata" if hexpand else "data"))
                elif not line.lstrip().startswith("#"):
                    out.append((line, "code"))
                continue
            if line.lstrip().startswith("#"):
                continue
            if line.endswith("\\"):
                buf += line[:-1] + " "
                continue
            line, buf = buf + line, ""
            m = ARRAY_START_RE.match(line)
            if m:
                # A literal with no keyed element is left to the line rule below, line by line.
                body, j, done = line[m.end():], i, st.array_literal(line[m.end():])
                while done is None and j < len(raws):
                    body += "\n" + raws[j]
                    j += 1
                    done = st.array_literal(body)
                if done is not None and any(k is not None for k, _ in done[0]):
                    out.append((line[:m.end()], "code"))
                    out.extend(_array_elements(m.group(1), done[0]))
                    i, line = j, done[1]
                    if not line.strip():
                        continue
            m = ELEM_ASSIGN_RE.match(line)
            if m and st.word_end(line, m.start(3)) == len(line.rstrip()):
                out.extend(_array_elements(m.group(1), [(m.group(2), m.group(3).rstrip())]))
                continue
            out.append((line, "code"))
            m = re.search(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)['\"]?", line)
            if m:
                heredoc, hexpand = m.group(2), not m.group(1)
                hkind = "code" if EXEC_RE.search(line[:m.start()]) else "data"
        return out

    @staticmethod
    def _map_levels(lines):
        """-> {(NAME, "key"|"value"): use level} for the keyed arrays of a script (KEYED ELEMENTS)."""
        code = [l for l, k in lines if k in ("code", "subst")]
        expanded = [l for l, k in lines if k == "xdata"]
        elements = [(k[1], k[0], l) for l, k in lines if isinstance(k, tuple)]
        levels = {}
        for name in {n for n, _, _ in elements}:
            levels[(name, "value")] = st.use_level({st._value_pat(name)}, code, elements, EXEC_RE,
                                                   expanded)
            levels[(name, "key")] = max(st.READ, st.use_level({st._keys_pat(name)}, code, elements,
                                                              EXEC_RE, expanded))
        return levels

    def script(self, path):
        """-> (files, dirs, uses_compiler, uses_lean, runs) for one script, not transitive.

        `runs` is the subset of named scripts this one may EXECUTE. A gate that greps another
        gate's text (`grep -q MARKER scripts/tests/test_mutation.sh`) depends on that file, but not
        on everything that file runs; following such mentions transitively made the gate-hygiene
        gate "depend" on every fixture in the repository. A name counts as run unless its line is
        plainly data — a grep/sed/echo/printf line with no interpreter on it, or a heredoc body.
        Misjudging data as a run only over-selects."""
        if path in self._scripts:
            return self._scripts[path]
        try:
            raw = open(path, errors="ignore").read()
        except OSError:
            raw = ""
        files, dirs, runs = set(), set(), set()
        base = os.path.dirname(path)
        lines = self._logical_lines(raw)
        levels = self._map_levels(lines)

        def level(kind):
            return levels[(kind[1], kind[0])] if isinstance(kind, tuple) else st.RUN

        for line, kind in lines:
            if kind in ("data", "xdata") or level(kind) == st.TEXT:
                continue  # text: a note, a reason, a message — names in it are not used
            f, d = self.deps_of_text(line, base)
            if level(kind) == st.READ:
                files |= f  # a path the script reads, or a lookup key: never a run
                dirs |= d
                continue
            files |= f
            dirs |= d
            # An element judged RUN is a call whatever its text spells (a helper named grep-x.sh).
            if isinstance(kind, tuple) or kind == "subst" \
                    or not (DATA_RE.search(line) and not EXEC_RE.search(line)):
                runs |= {x for x in f if x.endswith(SCRIPT_EXT)}
        text = "\n".join(l for l, k in lines if not (isinstance(k, tuple) and level(k) == st.TEXT))
        if path.endswith(".py"):
            for m in re.finditer(r"^\s*(?:from|import)\s+([A-Za-z_][A-Za-z0-9_]*)", text, re.M):
                sib = os.path.join(base, m.group(1) + ".py")
                if sib in self.files:
                    files.add(sib)
                    runs.add(sib)
        r = (files, dirs, bool(COMPILER_RE.search(text)), bool(LEAN_RE.search(text)), runs)
        self._scripts[path] = r
        return r

    def closure(self, cmd):
        """Transitive dependencies of one workflow command."""
        files, dirs = self.deps_of_text(cmd, "")
        compiler = lean = False
        stack = [f for f in files if f.endswith(SCRIPT_EXT)]
        roots = set(stack)
        seen = set()
        while stack:
            s = stack.pop()
            if s in seen:
                continue
            seen.add(s)
            f, d, c, l, runs = self.script(s)
            compiler |= c
            lean |= l
            dirs |= d
            files |= f
            # The whole-suite drivers name every gate. A gate that invokes one of them does so in a
            # bounded mode (`--plan`, a single mutation family), so it depends on the driver and the
            # helpers the driver runs, but not on every gate the driver could run. The drivers' own
            # workflow commands are roots and are traversed in full.
            if s not in roots and os.path.basename(s) in gfd.NOT_AUTO_RUN:
                runs = {x for x in runs if not GATE_NAME_RE.match(os.path.basename(x))}
            stack.extend(x for x in runs if x not in seen)
        # running the corpus runner is running the compiler, whatever its text says
        if any(os.path.basename(s) in ("run_tests.sh", "test_ssa.sh") for s in seen):
            compiler = True
        # ONE LEVEL OF DATA INDIRECTION. A gate that reads a manifest (example_manifest.txt,
        # test_manifest.toml, a .tsv ledger) depends on every path the manifest names, though the
        # gate's own text names none of them.
        for f in [f for f in files if f.endswith(DATA_EXT) and f not in PATH_CLASSIFIERS]:
            df, dd = self.data(f)
            files |= df
            dirs |= dd
        return files, dirs, compiler, lean, seen

    def data(self, path):
        if path not in self._data:
            try:
                text = open(path, errors="ignore").read() if os.path.getsize(path) < 2_000_000 else ""
            except OSError:
                text = ""
            self._data[path] = self.deps_of_text(text, os.path.dirname(path))
        return self._data[path]


def select(repo, inventory, changed, base=None):
    """-> ({command: reason}, [unmapped changed files])

    `base` is the commit the change is measured from; it is needed only when the workflow itself
    changed, to tell which commands the edit added or re-spelled (new arguments, a new env prefix).
    Without it, a workflow change selects every command."""
    info = {cmd: repo.closure(cmd) for cmd in inventory}
    chosen, unmapped = {}, []

    def pick(cmd, why):
        chosen.setdefault(cmd, why)

    if WORKFLOW in changed:
        old = None
        if base:
            r = subprocess.run(["git", "show", f"{base}:{WORKFLOW}"], capture_output=True, text=True)
            if r.returncode == 0:
                old = workflow_commands(r.stdout)
        for cmd in inventory:
            if old is None:
                pick(cmd, "the workflow changed and no base is known to compare its commands with")
            elif cmd not in old:
                pick(cmd, "the workflow adds or re-spells this command")

    for ch in changed:
        hit = False
        is_lean_src = ch.endswith(".lean")
        compiler_change = ch in TOOLCHAIN or ch in repo.lean_closure or ch.startswith("std/") \
            or (ch.startswith("Concrete/") and is_lean_src and not os.path.exists(ch))
        lean_change = is_lean_src and not compiler_change
        for cmd, (files, dirs, compiler, lean, scripts) in info.items():
            if ch in files or ch in scripts:
                pick(cmd, f"reads {ch}"); hit = True
            elif any(ch.startswith(d + "/") for d in dirs):
                d = next(d for d in dirs if ch.startswith(d + "/"))
                pick(cmd, f"reads {d}/"); hit = True
            elif compiler_change and (compiler or lean):
                pick(cmd, f"runs the compiler/Lean, which {ch} builds or feeds"); hit = True
            elif lean_change and lean:
                pick(cmd, f"runs Lean over modules that may import {ch}"); hit = True
        for prefix, gates in gfd.ALWAYS_IF.items():
            if ch.startswith(prefix):
                for g, why in gates:
                    for cmd in inventory:
                        if re.search(r"(^|[ /])" + re.escape(g) + r"( |$)", cmd):
                            pick(cmd, f"derived from {prefix}: {why}"); hit = True
        if compiler_change or lean_change:
            hit = True
        if not hit and not INERT_RE.search(ch):
            unmapped.append(ch)
    if changed:  # selected, never mapped: these do not count against the fail-safe below
        for g, why in gfd.EVERY_CHANGE:
            for cmd in inventory:
                if re.search(r"(^|[ /])" + re.escape(g) + r"( |$)", cmd):
                    pick(cmd, f"any change: {why}")
    for ch in unmapped:  # FAIL-SAFE: an unmapped executable-looking change selects everything
        for cmd in inventory:
            pick(cmd, f"fail-safe: no rule maps {ch}")
    return chosen, unmapped


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--inventory", required=True)
    ap.add_argument("--changed")
    ap.add_argument("--explain", action="store_true")
    ap.add_argument("--durations")
    ap.add_argument("--base", help="commit the change is measured from (for workflow edits)")
    a = ap.parse_args()
    inventory = [l.rstrip("\n") for l in open(a.inventory) if l.strip()]
    src = open(a.changed) if a.changed else sys.stdin
    changed = sorted({l.strip() for l in src if l.strip()})
    repo = Repo(changed)
    chosen, unmapped = select(repo, inventory, changed, a.base)
    for cmd in inventory:  # inventory order, so output is stable
        if cmd in chosen:
            print(f"{cmd}\t{chosen[cmd]}" if a.explain else cmd)
    for u in unmapped:
        print(f"select_gates: no rule maps {u} — fail-safe selects every gate", file=sys.stderr)
    if a.durations:
        dur = {}
        for l in open(a.durations):
            if l.startswith("#") or "\t" not in l:
                continue
            c, s = l.rstrip("\n").rsplit("\t", 1)
            dur[c] = float(s)
        known = [dur[c] for c in chosen if c in dur]
        unknown = len(chosen) - len(known)
        total = sum(known)
        whole = sum(dur.get(c, 0.0) for c in inventory) or 1.0
        local = total / whole * FULL_SET_LOCAL_MIN
        print(f"select_gates: {len(chosen)} of {len(inventory)} gate commands selected; "
              f"CI serial time {total/60:.1f} of {whole/60:.0f} min; "
              f"estimated local run ~{local:.0f} min (full set = {FULL_SET_LOCAL_MIN} min, JOBS=12)"
              + (f"; {unknown} selected without a CI timing" if unknown else ""), file=sys.stderr)


if __name__ == "__main__":
    main()

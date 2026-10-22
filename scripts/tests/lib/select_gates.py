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
     to scripts/tests/lib/X selects every gate that reaches X.
  2. compiler dependencies — a gate whose closure runs the compiler binary
     (`.lake/build/bin/concrete`, `require_fresh_binary`, `run_tests.sh`) or Lean (`lake env
     lean`, `lake build`) is selected by any change to a Lean module in the import closure of
     Main.lean, the toolchain, or the bundled stdlib (the compiler reads std/ at run time).
     A Lean module outside that closure (proofs/, PipelineTest) selects only the Lean-running
     gates and the gates that name it.
  3. derived artifacts — the curated ALWAYS_IF table of gates_for_diff.py (one owner), for gates
     that check a derivation of a whole tree without naming the changed file.
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

SCRIPT_EXT = (".sh", ".py")
# Directory references that are NOT a dependency on every file inside: gate scripts reach their
# helpers as "$ROOT/scripts/tests/$name", which would make every gate depend on every script. A
# script-to-script edge is taken from the script's NAME instead; a real `scripts/tests/check_*.sh`
# glob (a gate over all gates) is still kept.
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


# The estimate scales the selection's share of CI serial time to the measured local full-set run
# (.githooks/pre-push header: 23 minutes on an 18-core machine at JOBS=12, nothing else running).
# It ignores the critical path, so a selection dominated by one long gate is underestimated.
FULL_SET_LOCAL_MIN = 23


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
        kind is "code", or "data" for the body of a heredoc that no interpreter reads
        (`cat <<EOF`: a fixture list, a generated program). A heredoc fed to python3/bash is
        code, and its lines are judged like any other."""
        out, buf, heredoc, hkind = [], "", None, "code"
        for line in raw.split("\n"):
            if heredoc is not None:
                if line.strip() == heredoc:
                    heredoc = None
                elif hkind == "data":
                    out.append((line, "data"))
                elif not line.lstrip().startswith("#"):
                    out.append((line, "code"))
                continue
            if line.lstrip().startswith("#"):
                continue
            if line.endswith("\\"):
                buf += line[:-1] + " "
                continue
            line, buf = buf + line, ""
            out.append((line, "code"))
            m = re.search(r"<<-?\s*['\"]?([A-Za-z_][A-Za-z0-9_]*)['\"]?", line)
            if m:
                heredoc = m.group(1)
                hkind = "code" if EXEC_RE.search(line[:m.start()]) else "data"
        return out

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
        for line, kind in lines:
            if kind == "data":
                continue
            f, d = self.deps_of_text(line, base)
            files |= f
            dirs |= d
            if not (DATA_RE.search(line) and not EXEC_RE.search(line)):
                runs |= {x for x in f if x.endswith(SCRIPT_EXT)}
        text = "\n".join(l for l, _ in lines)
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
            # The whole-suite drivers name every gate. A gate that invokes one of them does so in a
            # bounded mode (`--plan`, a single family), so its run is not "every gate's inputs".
            # The drivers' own commands are roots and are traversed.
            if s not in roots and os.path.basename(s) in gfd.NOT_AUTO_RUN:
                continue
            f, d, c, l, runs = self.script(s)
            compiler |= c
            lean |= l
            dirs |= d
            files |= f
            stack.extend(x for x in runs if x not in seen)
        # running the corpus runner is running the compiler, whatever its text says
        if any(os.path.basename(s) in ("run_tests.sh", "test_ssa.sh") for s in seen):
            compiler = True
        return files, dirs, compiler, lean, seen


def select(repo, inventory, changed):
    """-> ({command: reason}, [unmapped changed files])"""
    info = {cmd: repo.closure(cmd) for cmd in inventory}
    chosen, unmapped = {}, []

    def pick(cmd, why):
        chosen.setdefault(cmd, why)

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
    a = ap.parse_args()
    inventory = [l.rstrip("\n") for l in open(a.inventory) if l.strip()]
    src = open(a.changed) if a.changed else sys.stdin
    changed = sorted({l.strip() for l in src if l.strip()})
    repo = Repo(changed)
    chosen, unmapped = select(repo, inventory, changed)
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

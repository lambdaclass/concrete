#!/usr/bin/env python3
"""The totality ratchet: compare the compiler's non-total declarations with the baseline.

Input is the inventory scripts/tools/totality_inventory.lean prints from the elaborated
environment (kind, declaration, module, recursion, recursive argument types, line). Data:

  docs/compiler/totality_baseline.tsv  every known non-total declaration, with its stage, reason
                                       and owner; it may only shrink
  docs/compiler/totality_stages.tsv    file prefix -> pipeline stage, proof-relevant, owner
  docs/compiler/totality_pinned.tsv    declarations a stated theorem needs total; never listable
  scripts/tests/axiom_native_owners.txt  the lemmas allowed to mint a native-trust axiom

Commands:
  check    --inventory FILE [--base-baseline FILE]
                              exit 1 on any finding (the gate). With --base-baseline (the baseline
                              at the base revision), a row the current baseline has and the base
                              does not is a finding: the baseline only SHRINKS, so admitting a new
                              non-total declaration by adding its row fails too.
  generate --inventory FILE   rewrite the baseline from the inventory; a row already in the
                              baseline keeps its reason and owner, a new row gets the reason the
                              source heuristic below infers. Use it to seed or to SHRINK the
                              baseline, never to admit a new entry silently: the diff is reviewed.
  summary  --inventory FILE   counts per stage, kind, recursion and reason (the research note)

A `sorry` row is never admissible, and neither is an axiom other than a native-trust axiom minted
by a lemma in axiom_native_owners.txt. When every row of a proof-relevant stage is gone, nothing
here changes: an empty stage is a stage in which any non-total declaration is new, so the gate is
then the hard "no non-total declarations here" check by construction.
"""
import argparse
import collections
import os
import re
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
BASELINE = "docs/compiler/totality_baseline.tsv"
STAGES = "docs/compiler/totality_stages.tsv"
PINNED = "docs/compiler/totality_pinned.tsv"
NATIVE_OWNERS = "scripts/tests/axiom_native_owners.txt"
HEADER = ["kind", "decl", "file", "stage", "recursion", "argtypes", "reason", "owner"]

# The reason vocabulary. A row's reason is one of these codes; the research note
# (research/compiler/partial-def-inventory-2026-10.md) gives the conversion for each.
REASONS = {
    "derived-instance": "a deriving handler (BEq, Repr, ...) on a nested inductive emitted a partial instance",
    "token-consumption": "consumes input tokens or characters; terminates because the position advances, which is not structural",
    "worklist-fixpoint": "worklist or fixpoint iteration; needs fuel or a decreasing measure",
    "index-loop": "recursion on an index, counter or shrinking slice; needs termination_by on the remaining length",
    "mutual-walker": "mutual recursion over the AST or IR (expressions, statements, arms); needs a mutual structural block or termination_by sizeOf",
    "nested-inductive": "recursion through a nested inductive (a List, Array or Option of the type itself) inside map/fold/for; needs recursion over the nested type or attach with termination_by sizeOf",
    "direct-recursion": "recursion only on direct subterms as far as the source shows; removing `partial`, or adding termination_by sizeOf, is the first thing to try",
    "object-divergence": "evaluates the object language, whose programs may diverge (while, recursion); no measure exists, so the conversion is a clock (fuel) argument, as the Core semantics plan states",
    "host-io": "host I/O: an implemented_by bridge to an unsafe implementation (the interpreter's file and environment access)",
    "simp-attr-extension": "register_simp_attr declares an opaque environment extension",
}

DERIVED = {"beq", "repr", "hash", "decEq", "toJson", "fromJson", "compare", "ord"}
DECL_START = re.compile(
    r"^(@\[|/--|private |protected |partial |def |theorem |lemma |instance |structure |inductive |"
    r"class |mutual\b|end\b|namespace |section |open |abbrev |noncomputable |unsafe |macro|syntax|"
    r"set_option|#|deriving |opaque |axiom |attribute )")


def read_tsv(path, ncols):
    rows = []
    with open(os.path.join(ROOT, path)) as f:
        for lineno, line in enumerate(f, 1):
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            cols = line.split("\t")
            if len(cols) != ncols:
                raise SystemExit(f"{path}:{lineno}: expected {ncols} tab-separated columns, got {len(cols)}")
            rows.append(cols)
    return rows


def read_tsv_abs(path, ncols):
    return read_tsv(os.path.abspath(path), ncols)


def module_file(module):
    return module.replace(".", "/") + ".lean"


def load_stages():
    return [(p, s, pr == "yes", o) for p, s, pr, o in read_tsv(STAGES, 4)]


def stage_of(stages, path):
    best = None
    for prefix, stage, proof, owner in stages:
        if path.startswith(prefix) and (best is None or len(prefix) > len(best[0])):
            best = (prefix, stage, proof, owner)
    return best


def load_inventory(path):
    rows = []
    with open(path) as f:
        for line in f:
            line = line.rstrip("\n")
            if not line:
                continue
            cols = line.split("\t")
            if len(cols) != 6:
                raise SystemExit(f"inventory: malformed row {line!r}")
            kind, decl, module, recursion, argtypes, lineno = cols
            rows.append({"kind": kind, "decl": decl, "file": module_file(module), "module": module,
                         "recursion": recursion, "argtypes": argtypes, "line": int(lineno)})
    return rows


def key(kind, decl, path):
    return (kind, decl, path)


_SOURCE = {}


def source_lines(path):
    if path not in _SOURCE:
        try:
            with open(os.path.join(ROOT, path)) as f:
                _SOURCE[path] = f.read().split("\n")
        except OSError:
            _SOURCE[path] = []
    return _SOURCE[path]


def body_text(path, line):
    lines = source_lines(path)
    if not lines or line <= 0:
        return ""
    # The declaration range starts at its docstring or attributes; the body starts at `def`.
    start = line - 1
    while start < min(len(lines), line + 40) and not re.search(r"\b(def|instance|let rec)\b", lines[start]):
        start += 1
    if start >= len(lines):
        return ""
    out = [lines[start]]
    for l in lines[start + 1:]:
        if DECL_START.match(l):
            break
        out.append(l)
    return "\n".join(out)


def infer_reason(row):
    """The source heuristic behind `generate`. It seeds reasons; a reviewer can correct one by
    editing the baseline, and `generate` keeps an existing row's reason."""
    kind, decl, path = row["kind"], row["decl"], row["file"]
    short = decl.split(".")[-1]
    if kind in ("implemented_by", "unsafe") and path == "Concrete/Interp/Interp.lean" and short.startswith("host"):
        return "host-io"
    if kind == "opaque" and path == "Concrete/Proof/SimpAttr.lean":
        return "simp-attr-extension"
    if kind != "partial":
        return "-"
    parts = decl.split(".")
    if short in DERIVED and any(p.startswith("inst") for p in parts[:-1]):
        return "derived-instance"
    if path == "Concrete/Interp/Interp.lean" and re.match(r"(eval|runDeferred|callFn|execute)", short):
        return "object-divergence"
    text = body_text(path, row["line"])
    if path in ("Concrete/Frontend/Parser.lean", "Concrete/Frontend/Lexer.lean") or \
            re.search(r"\b(parse|lex|skip|scan)", short, re.I) and re.search(r"\b(pos|peek|advance|cursor|rest)\b", text):
        return "token-consumption"
    if row["argtypes"] != "-":
        # Its arguments include a nested or mutual inductive (Expr, Stmt, Ty, CModule, ...): an AST
        # or IR walker, whatever else its body mentions.
        return "mutual-walker" if row["recursion"] == "mutual" else "nested-inductive"
    if re.search(r"\b(worklist|fixpoint|fixPoint|changed|queue|todo|pending|frontier|untilStable|iterate|"
                 r"visited|seen|reachable|stack)\b", text) or re.search(r"(Fixpoint|Dfs|dfs|Reachable)", short):
        return "worklist-fixpoint"
    call = re.compile(r"\b" + re.escape(short) + r"\b")
    rec_lines = [l for l in text.split("\n")[1:] if call.search(l)]
    if any(re.search(r"(\+\s*1\b|-\s*1\b|\.drop\b|\.extract\b|\.tail\b|\bfuel\b)", l) for l in rec_lines):
        return "index-loop"
    if row["recursion"] == "mutual":
        return "mutual-walker"
    if any(re.search(r"(\.map|\.mapM|\.all|\.any|\.foldl|\.foldr|\.foldlM|\.filterMap|\.forM|\.flatMap|\.attach|\bfor\b)", l)
           for l in rec_lines):
        return "nested-inductive"
    return "direct-recursion"


def load_baseline():
    return read_tsv(BASELINE, len(HEADER))


def native_owner(decl):
    m = re.match(r"^(.*)\._native\.(bv_decide|native_decide)\.ax_[0-9_]+$", decl)
    return m.group(1) if m else None


def native_owners():
    out = set()
    with open(os.path.join(ROOT, NATIVE_OWNERS)) as f:
        for line in f:
            line = line.strip()
            if line and not line.startswith("#"):
                out.add(line)
    return out


def tracked_sources():
    out = subprocess.run(["git", "ls-files", "Concrete.lean", "Concrete/*.lean", "Main.lean"],
                         cwd=ROOT,
                         capture_output=True, text=True, check=True).stdout
    return [l for l in out.split("\n") if l]


def cmd_check(args):
    stages = load_stages()
    inv = load_inventory(args.inventory)
    base = load_baseline()
    pinned = read_tsv(PINNED, 3)
    owners = native_owners()
    fails = []

    for path in tracked_sources():
        if stage_of(stages, path) is None:
            fails.append(f"unmapped source: {path} matches no prefix in {STAGES}")

    base_keys = {}
    for row in base:
        kind, decl, path, stage, recursion, argtypes, reason, owner = row
        k = key(kind, decl, path)
        if k in base_keys:
            fails.append(f"baseline lists {kind} {decl} ({path}) twice")
        base_keys[k] = row
        st = stage_of(stages, path)
        if st is None:
            fails.append(f"baseline row {decl}: {path} has no stage")
            continue
        if stage != st[1]:
            fails.append(f"baseline row {decl}: stage {stage!r}, but {path} is in stage {st[1]!r}")
        if not re.fullmatch(r"R-\d{4}", owner):
            fails.append(f"baseline row {decl}: owner {owner!r} is not a roadmap id (R-NNNN)")
        if reason not in REASONS and (st[2] or reason != "-"):
            fails.append(f"baseline row {decl} ({st[1]}, proof-relevant={'yes' if st[2] else 'no'}): "
                         f"reason {reason!r} is not one of {', '.join(sorted(REASONS))}")
        if kind in ("sorry", "axiom"):
            fails.append(f"baseline row {decl}: a {kind} row is never admissible in the baseline")

    seen = set()
    inv_keys = collections.Counter(key(r["kind"], r["decl"], r["file"]) for r in inv)
    for k, n in inv_keys.items():
        if n > 1:
            fails.append(f"{n} {k[0]} declarations in {k[2]} share the name {k[1]} once private prefixes and "
                         f"macro scopes are removed; the baseline cannot tell them apart. Rename one.")
    for row in inv:
        kind, decl, path = row["kind"], row["decl"], row["file"]
        where = f"{path}:{row['line']}"
        if kind == "sorry":
            fails.append(f"SORRY reaches a compiler declaration: {decl} ({where})")
            continue
        if kind == "axiom":
            owner = native_owner(decl)
            if owner is None:
                fails.append(f"AXIOM declared in the compiler: {decl} ({where})")
            elif owner not in owners:
                fails.append(f"native-trust axiom {decl} ({where}) is minted by {owner}, "
                             f"which {NATIVE_OWNERS} does not list")
            continue
        k = key(kind, decl, path)
        seen.add(k)
        if k not in base_keys:
            st = stage_of(stages, path)
            stage = st[1] if st else "?"
            fails.append(f"NEW non-total declaration: {kind} {decl} ({where}, stage {stage}"
                         f"{', proof-relevant' if st and st[2] else ''}). Make it total; the baseline only shrinks.")
        elif base_keys[k][4:6] != [row["recursion"], row["argtypes"]]:
            fails.append(f"baseline row {decl}: recursion/argtypes {base_keys[k][4:6]}, "
                         f"inventory says {[row['recursion'], row['argtypes']]}")

    for k, row in base_keys.items():
        if k not in seen:
            fails.append(f"baseline row {row[0]} {row[1]} ({row[2]}) is no longer non-total (or no longer exists): "
                         f"delete the row, so the baseline only shrinks")

    if args.base_baseline:
        base_rows = read_tsv_abs(args.base_baseline, len(HEADER)) if os.path.getsize(args.base_baseline) else []
        before = {key(r[0], r[1], r[2]) for r in base_rows}
        for k, row in base_keys.items():
            if k not in before:
                fails.append(f"baseline row ADDED: {row[0]} {row[1]} ({row[2]}) is not in the base revision's "
                             f"baseline. The baseline only shrinks; make the declaration total instead")

    for path, short, why in pinned:
        if not re.search(r"\bdef " + re.escape(short) + r"\b", "\n".join(source_lines(path))):
            fails.append(f"PINNED {short} is not defined in {path}: a pin on a renamed or deleted "
                         f"declaration holds nothing; update {PINNED}")
        for row in inv:
            if row["file"] == path and row["decl"].split(".")[-1] == short:
                fails.append(f"PINNED total, now {row['kind']}: {row['decl']} ({path}:{row['line']}) — {why}")
        for row in base:
            if row[2] == path and row[1].split(".")[-1] == short:
                fails.append(f"PINNED declaration {row[1]} is listed in the baseline — {why}")

    by_stage = collections.Counter(r[3] for r in base)
    proof_stages = sorted({s for _, s, p, _ in stages if p})
    for f in fails:
        print(f"  FAIL {f}")
    summary = ", ".join(f"{s} {by_stage.get(s, 0)}" for s in proof_stages)
    print(f"TOTALITY: inventory {len(inv)} rows, baseline {len(base)} rows; proof-relevant stages: {summary}")
    return 1 if fails else 0


def cmd_generate(args):
    stages = load_stages()
    inv = load_inventory(args.inventory)
    old = {}
    if os.path.exists(os.path.join(ROOT, BASELINE)):
        for row in load_baseline():
            old[key(row[0], row[1], row[2])] = row
    header_lines = []
    with open(os.path.join(ROOT, BASELINE)) as f:
        for line in f:
            if line.startswith("#"):
                header_lines.append(line.rstrip("\n"))
            else:
                break
    if not inv:
        raise SystemExit("refusing to generate from an empty inventory (did the inventory tool fail?)")
    rows = []
    for row in inv:
        if row["kind"] in ("sorry", "axiom"):
            continue
        st = stage_of(stages, row["file"])
        if st is None:
            raise SystemExit(f"unmapped source {row['file']}: add a prefix to {STAGES}")
        k = key(row["kind"], row["decl"], row["file"])
        if k in old:
            reason, owner = old[k][6], old[k][7]
        else:
            reason, owner = infer_reason(row), st[3]
        rows.append([row["kind"], row["decl"], row["file"], st[1], row["recursion"], row["argtypes"], reason, owner])
    rows.sort(key=lambda r: (r[2], r[1], r[0]))
    with open(os.path.join(ROOT, BASELINE), "w") as f:
        for line in header_lines:
            f.write(line + "\n")
        for r in rows:
            f.write("\t".join(r) + "\n")
    print(f"wrote {BASELINE}: {len(rows)} rows ({len(rows) - len(set(old) & {key(r[0], r[1], r[2]) for r in rows})} new)")
    return 0


def cmd_summary(args):
    stages = load_stages()
    base = load_baseline()
    proof = {s: p for _, s, p, _ in stages}
    by = collections.defaultdict(collections.Counter)
    for kind, decl, path, stage, recursion, argtypes, reason, owner in base:
        by[stage][reason] += 1
    for stage in sorted(by, key=lambda s: (not proof.get(s, False), s)):
        total = sum(by[stage].values())
        parts = ", ".join(f"{r} {n}" for r, n in by[stage].most_common())
        print(f"{stage}\t{'proof' if proof.get(stage) else '-'}\t{total}\t{parts}")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("command", choices=["check", "generate", "summary"])
    ap.add_argument("--inventory")
    ap.add_argument("--base-baseline")
    args = ap.parse_args()
    if args.command in ("check", "generate") and not args.inventory:
        ap.error("--inventory is required")
    return {"check": cmd_check, "generate": cmd_generate, "summary": cmd_summary}[args.command](args)


if __name__ == "__main__":
    sys.exit(main())

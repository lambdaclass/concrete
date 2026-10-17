#!/usr/bin/env bash
# IDENTITY MIGRATION MAP — R-0482's total, typed old-to-new map (IDENTITY_MIGRATION_PLAN.md section 5).
#
# Pairs every row of the RAW pre-migration baseline with a row of a raw tree by CONTENT KEY, never by
# digest — digests are exactly what the migration changes. One typed output row per baseline row:
#
#   mapped  <kind> <old> -> <new>        the row exists on both sides; its bytes moved (or did not)
#   shared  <kind> <old> -> <new>        distinct old values legitimately became ONE new value
#   refused <kind> <old> <reason>        no counterpart, an ambiguous key, or a changed implementation
#
# Refuses (exit 1, rows still printed) when the map is not total, a new row has no baseline row, a
# pairing key matches more than one row on either side, or a paired row's implementation digest
# differs. `shared` rows are printed and counted; the gate requires 0 in this corpus (none expected:
# scopes stay distinct), so any that appear must be justified where they are accepted.
#
# Pairing keys (per kind):
#   fixture     label
#   subject     (label, module, decl, theorem, tables, refusal)            impl compared after pairing
#   dependency  (label, consumer, table, disposition, kind, module, decl)  impl compared after pairing
#   reference   (table prefix, label of its scope, module, decl)           impl compared after pairing
#   selection   (file, holding declaration, model, reference content key) -> the mapped symbol
#   link        (file, fn, fingerprint) — must be byte-unchanged
#
# AMENDMENTS (optional third argument, docs/verification/identity-rows-amendments.tsv): a row
# legitimately re-frozen after the baseline is listed there as its baseline value (`-`) and its new
# value (`+`). Each `-` row must appear exactly once in the baseline or the tool refuses; it is
# replaced before pairing and reported as an `amended` line. Anything not listed still has to pair.
# Without the argument nothing is amended, which is how the committed R-0482 map was produced.
#
# Usage: scripts/tools/identity_migration_map.sh BASELINE_RAW NEW_RAW [AMENDMENTS]
set -uo pipefail
{ [ "$#" -eq 2 ] || [ "$#" -eq 3 ]; } || { echo "usage: $0 BASELINE_RAW NEW_RAW [AMENDMENTS]" >&2; exit 2; }
for f in "$@"; do [ -r "$f" ] || { echo "FATAL: cannot read $f" >&2; exit 2; }; done
export LC_ALL=C
python3 -I - "$@" <<'PY'
import sys

def load(path):
    rows = {}
    for line in open(path, encoding="utf-8"):
        if line.startswith("#") or not line.strip():
            continue
        f = line.rstrip("\n").split("\t")
        rows.setdefault(f[0], []).append(f[1:])
    return rows

old, new = load(sys.argv[1]), load(sys.argv[2])
out, refused, shared = [], 0, 0
amended_lines = []
if len(sys.argv) > 3:
    minus, plus = [], []
    for line in open(sys.argv[3], encoding="utf-8"):
        line = line.rstrip("\n")
        if line.startswith("-\t"): minus.append(line[2:].split("\t"))
        elif line.startswith("+\t"): plus.append(line[2:].split("\t"))
    if len(minus) != len(plus):
        print("FATAL: amendments must pair each - row with a + row", file=sys.stderr); sys.exit(2)
    for o, n in zip(minus, plus):
        if o[0] != n[0] or old.get(o[0], []).count(o[1:]) != 1:
            print("FATAL: amendment is not exactly one baseline row: " + "\t".join(o), file=sys.stderr); sys.exit(2)
        rows = old[o[0]]
        rows[rows.index(o[1:])] = n[1:]
        amended_lines.append("\t".join(["amended", o[0], "\t".join(o[1:]), "->", "\t".join(n[1:])]))

def emit(kind, verdict, *parts):
    out.append("\t".join((verdict, kind) + parts))

def labels(rows):
    lab = {}
    for label, pkg in rows.get("fixture", []):
        lab[pkg] = label
    return lab

lab_old, lab_new = labels(old), labels(new)

def index(kind, rows, keyf):
    idx, amb = {}, set()
    for r in rows.get(kind, []):
        k = keyf(r)
        if k in idx:
            amb.add(k)
        idx[k] = r
    return idx, amb

def pair(kind, keyf, describe, impl_at=None, digests=()):
    """Pair `kind` rows by keyf; return {old_row_tuple: new_row}. `digests` are the column indexes
    whose old->new values are reported (and checked for sharing)."""
    global refused, shared
    io, ao = index(kind, old, keyf(lab_old))
    inn, an = index(kind, new, keyf(lab_new))
    paired = {}
    for k in sorted(io):
        o = io[k]
        if k in ao or k in an:
            emit(kind, "refused", describe(o), "ambiguous-key"); refused += 1; continue
        n = inn.get(k)
        if n is None:
            emit(kind, "refused", describe(o), "no-counterpart"); refused += 1; continue
        if impl_at is not None and o[impl_at] != n[impl_at]:
            emit(kind, "refused", describe(o), f"implementation-changed {o[impl_at]} -> {n[impl_at]}")
            refused += 1; continue
        paired[k] = (o, n)
        moved = " ".join(f"{o[i]}->{n[i]}" for i in digests) or "unchanged"
        emit(kind, "mapped", describe(o), moved)
    for k in sorted(set(inn) - set(io)):
        emit(kind, "refused", describe(inn[k]), "new-row-without-baseline"); refused += 1
    return paired

def check_injective(kind, paired, col):
    """Distinct old values at `col` must stay distinct; a collapse is `shared`."""
    global shared
    seen = {}
    for o, n in paired.values():
        if o[col] in ("-", "NO-IDENTITY"):
            continue
        seen.setdefault(n[col], set()).add(o[col])
    for nv, ovs in sorted(seen.items()):
        if len(ovs) > 1:
            for ov in sorted(ovs):
                emit(kind, "shared", ov, nv); shared += 1

# fixture: label -> package (scope)
fx = pair("fixture", lambda lab: (lambda r: r[0]), lambda r: r[0], digests=(1,))
check_injective("fixture", fx, 1)
# subject<label,definition,package,module,decl,impl,theorem,tables,refusal>
sj = pair("subject", lambda lab: (lambda r: (r[0], r[3], r[4], r[6], r[7], r[8])),
          lambda r: f"{r[0]}:{r[3]}.{r[4]}@{r[6]}", impl_at=5, digests=(1, 2))
check_injective("subject", sj, 1)
# dependency<label,consumer-definition,consumer,table,disposition,kind,definition,package,module,decl,impl>
dp = pair("dependency", lambda lab: (lambda r: (r[0], r[2], r[3], r[4], r[5], r[8], r[9])),
          lambda r: f"{r[0]}:{r[2]}->{r[8]}.{r[9]}@{r[3]}", impl_at=10, digests=(1, 6, 7))
# reference<symbol,package,module,decl,impl>
def ref_key(lab):
    def k(r):
        sym, pkg = r[0], r[1]
        return (sym.split("_" + pkg[:8] + "_")[0], lab.get(pkg, "?" + pkg), r[2], r[3])
    return k
rf = pair("reference", ref_key, lambda r: r[0], impl_at=4, digests=(0, 1))
check_injective("reference", rf, 0)
symmap = {o[0]: n[0] for o, n in rf.values()}
# selection<file,holding-decl,model,symbol>: keyed by (file, holding declaration, model, CONTENT KEY of
# the selected reference) — one model is legitimately selected once per consuming scope (`check_nonce`
# in two packages), so (file, holding, model) alone is ambiguous. The new symbol must be exactly the
# mapped reference. Identical rows are compared as a multiset, never collapsed.
okey = {r[0]: ref_key(lab_old)(r) for r in old.get("reference", [])}
nkey = {r[0]: ref_key(lab_new)(r) for r in new.get("reference", [])}
def sel_index(rows, keys):
    idx = {}
    for r in rows.get("selection", []):
        idx.setdefault((r[0], r[1], r[2], keys.get(r[3], ("?", r[3]))), []).append(r)
    return idx
io, inn = sel_index(old, okey), sel_index(new, nkey)
for k in sorted(io, key=str):
    os_, ns = io[k], inn.get(k, [])
    for o in os_:
        d = f"{o[0]}:{o[1]}:{o[2]}"
        if len(os_) != len(ns):
            emit("selection", "refused", d, f"multiplicity {len(os_)} -> {len(ns)}"); refused += 1
        elif symmap.get(o[3]) != ns[0][3]:
            emit("selection", "refused", d, f"selects {ns[0][3]}, but {o[3]} maps to {symmap.get(o[3], 'nothing')}")
            refused += 1
        else:
            emit("selection", "mapped", d, f"{o[3]}->{ns[0][3]}")
for k in sorted(set(inn) - set(io), key=str):
    for n in inn[k]:
        emit("selection", "refused", f"{n[0]}:{n[1]}:{n[2]}", "new-row-without-baseline"); refused += 1
# link<file,fn,fingerprint>: byte-unchanged
pair("link", lambda lab: (lambda r: tuple(r)), lambda r: f"{r[0]}:{r[1]}")

print("# identity-migration-map v1 — scripts/tools/identity_migration_map.sh; do not edit by hand")
print("# verdict<TAB>kind<TAB>row<TAB>old->new (or refusal reason)")
for line in amended_lines + out:
    print(line)
counts = {}
for line in out:
    v = line.split("\t", 1)[0]; counts[v] = counts.get(v, 0) + 1
total = sum(len(v) for v in old.values())
print(f"# totals: baseline-rows={total} mapped={counts.get('mapped', 0)} "
      f"shared={counts.get('shared', 0)} refused={counts.get('refused', 0)}"
      + (f" amended={len(amended_lines)}" if amended_lines else ""))
if refused or counts.get("mapped", 0) != total:
    print(f"REFUSE: the map is not total ({counts.get('mapped', 0)} of {total} mapped, {refused} refused)",
          file=sys.stderr)
    sys.exit(1)
PY

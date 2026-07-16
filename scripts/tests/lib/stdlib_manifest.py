#!/usr/bin/env python3
"""Phase 7 items 2a + 22a: derive the stdlib API snapshot for every public item.

For each `pub fn` in std/src/*.con, derive from the SIGNATURE (+ attributes):
  allocates   yes | no                  (with(...) contains Alloc)
  ownership   borrows | mut | consumes | constructs   (receiver mode; no receiver => constructs)
  fails       infallible | option | result            (return type head)
  capability  none | comma-set                        (the with(...) clause)
  proof-class pure-core | deterministic | hosted-effect | trusted-boundary
              (trusted fn => trusted-boundary; caps!=none => hosted-effect;
               pure-core modules (option,result,bytes,numeric,math,ascii,slice,
               hash,bitset,sort) => pure-core; else deterministic)
  evidence    proved | gated            (item 22a: `proved` iff the fn carries a
              #[proof_by(...)] ATTRIBUTE — kernel-checked link, exact marking,
              no spillover; everything else is `gated`: manifest facts + oracle
              tests + mutation/capability/error gates. Deprecation status and
              per-API doc links are deliberately ABSENT until the language has
              a deprecation mechanism / per-API docs — no decorative columns.)
  signature   canonical one-line signature (item 22a: a param/return type
              change that keeps the same fact profile still fails the diff)

Emits TSV: module fn allocates ownership fails capability proof-class evidence signature
Used by check_stdlib_manifest.sh both to GENERATE the committed manifest and to
VERIFY the committed manifest still agrees with the signatures (the
"stale hand-written string disagrees with the compiler fact" negative).
"""
import re, sys, os, glob

PURE_CORE = {"option","result","bytes","numeric","math","ascii","slice","hash","bitset","sort"}

def preceding_attr_lines(src, start):
    """Contiguous attribute (`#[...]`) / comment lines directly above the fn.
    Only real attribute lines count for proof-link detection — a `//` comment
    that MENTIONS #[proof_by] (parse_hex's honesty note) must not match."""
    lines = src[:start].splitlines()
    attrs = []
    for line in reversed(lines):
        t = line.strip()
        if t.startswith("#[") or t.startswith("//") or t == "":
            if t.startswith("#["):
                attrs.append(t)
            if t == "" and attrs:
                break
            continue
        break
    return attrs

def canon_sig(gen, params, caps, ret):
    p = re.sub(r"\s+", " ", (params or "").strip())
    g = re.sub(r"\s+", " ", (gen or "").strip())
    sig = f"{g}({p})"
    if caps is not None:
        c = ",".join(x.strip() for x in caps.split(",") if x.strip())
        sig += f" with({c})"
    r = re.sub(r"\s+", " ", (ret or "").strip())
    if r:
        sig += f" -> {r}"
    return sig

def rows():
    out = []
    for path in sorted(glob.glob("std/src/*.con")):
        mod = os.path.basename(path)[:-4]
        src = open(path, encoding="utf-8").read()
        # join signatures split across lines: capture from 'pub fn' to '{' or ';'
        for m in re.finditer(r'pub\s+(trusted\s+)?fn\s+(\w+)\s*(<[^>]*>)?\s*\(([^)]*)\)\s*(with\s*\(([^)]*)\))?\s*(->\s*([^\{;]+))?', src):
            trusted, name, gen, params, w, caps, _r, ret = m.groups()
            params = (params or "").strip()
            ret = (ret or "").strip()
            caps_set = [c.strip() for c in (caps or "").split(",") if c.strip()]
            alloc = "yes" if any(c.startswith("Alloc") for c in caps_set) else "no"
            first = params.split(",")[0].strip() if params else ""
            if first.startswith("&mut self"): own = "mut"
            elif first.startswith("&self"): own = "borrows"
            elif first == "self" or first.startswith("self:") or first.startswith("mut self"): own = "consumes"
            else: own = "constructs"
            if ret.startswith("Result<") or ret.startswith("Result <"): fails = "result"
            elif ret.startswith("Option<") or ret.startswith("Option <"): fails = "option"
            else: fails = "infallible"
            cap = ",".join(sorted(caps_set)) if caps_set else "none"
            if trusted: pc = "trusted-boundary"
            elif caps_set: pc = "hosted-effect"
            elif mod in PURE_CORE: pc = "pure-core"
            else: pc = "deterministic"
            attrs = preceding_attr_lines(src, m.start())
            evidence = "proved" if any(a.startswith("#[proof_by(") for a in attrs) else "gated"
            sig = canon_sig(gen, params, caps if w else None, ret)
            out.append((mod, name, alloc, own, fails, cap, pc, evidence, sig))
    return out

if __name__ == "__main__":
    for r in rows():
        print("\t".join(r))

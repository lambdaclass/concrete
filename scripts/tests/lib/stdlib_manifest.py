#!/usr/bin/env python3
"""Phase 7 item 2a: derive the five-fact manifest for every public stdlib item.

For each `pub fn` in std/src/*.con, derive from the SIGNATURE:
  allocates   yes | no                  (with(...) contains Alloc)
  ownership   borrows | mut | consumes | constructs   (receiver mode; no receiver => constructs)
  fails       infallible | option | result            (return type head)
  capability  none | comma-set                        (the with(...) clause)
  proof-class pure-core | deterministic | hosted-effect | trusted-boundary
              (trusted fn => trusted-boundary; caps!=none => hosted-effect;
               pure-core modules (option,result,bytes,numeric,math,ascii,slice,
               hash,bitset,sort) => pure-core; else deterministic)

Emits TSV: module<TAB>function<TAB>allocates<TAB>ownership<TAB>fails<TAB>capability<TAB>proof-class
Used by check_stdlib_manifest.sh both to GENERATE the committed manifest and to
VERIFY the committed manifest still agrees with the signatures (the
"stale hand-written string disagrees with the compiler fact" negative).
"""
import re, sys, os, glob

PURE_CORE = {"option","result","bytes","numeric","math","ascii","slice","hash","bitset","sort"}

def rows():
    out = []
    for path in sorted(glob.glob("std/src/*.con")):
        mod = os.path.basename(path)[:-4]
        src = open(path, encoding="utf-8").read()
        # join signatures split across lines: capture from 'pub fn' to '{' or ';'
        for m in re.finditer(r'pub\s+(trusted\s+)?fn\s+(\w+)\s*(<[^>]*>)?\s*\(([^)]*)\)\s*(with\s*\(([^)]*)\))?\s*(->\s*([^\{;]+))?', src):
            trusted, name, _gen, params, _w, caps, _r, ret = m.groups()
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
            out.append((mod, name, alloc, own, fails, cap, pc))
    return out

if __name__ == "__main__":
    for r in rows():
        print("\t".join(r))

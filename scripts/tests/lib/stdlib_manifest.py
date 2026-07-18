#!/usr/bin/env python3
"""Phase 7 items 2a + 22a: derive the stdlib API snapshot for every public item.

For each `pub fn` in std/src/*.con, derive from the SIGNATURE (+ attributes):
  allocates   yes | no                  (with(...) contains Alloc)
  ownership   borrows | mut | consumes | constructs   (receiver mode; no receiver => constructs)
  fails       infallible | option | result            (return type head)
  capability  none | comma-set                        (the with(...) clause)
  proof-class pure-core | deterministic | hosted-effect | trusted-boundary
              (trusted fn OR fn inside a trusted impl => trusted-boundary;
               caps!=none => hosted-effect; pure-core modules => pure-core;
               else deterministic)
  evidence    proved | gated            (item 22a: `proved` iff the fn carries a
              #[proof_by(...)] ATTRIBUTE — kernel-checked link, exact marking,
              no spillover; everything else is `gated`.)
  signature   canonical one-line signature (item 22a: a param/return type
              change that keeps the same fact profile still fails the diff)

Emits TSV: module fn allocates ownership fails capability proof-class evidence signature

PARSER (defect-queue 0a, replaced the single-regex derivation): a scanner
that strips comments/strings, tracks brace depth and the ENCLOSING impl's
trusted-ness, and reads signatures with BALANCED parentheses and BALANCED
generic angles — the regex truncated every fn-pointer parameter at the
first nested `)` (39 corrupted rows), missed `pub [trusted] extern fn`,
and classified trust only at fn level. The self-test fixture in
check_stdlib_manifest.sh pins every previously-misparsed shape.
"""
import re, sys, os, glob

PURE_CORE = {"option","result","bytes","numeric","math","ascii","slice","hash","bitset","sort"}


def strip_comments_strings(src):
    """Blank out comments and string/char literals (preserving byte offsets
    and newlines) so structural scanning never trips on `)` or `{` inside
    them. Attribute lines (`#[...]`) are preserved."""
    out = list(src)
    i, n = 0, len(src)
    while i < n:
        c = src[i]
        if c == "/" and i + 1 < n and src[i+1] == "/":
            j = i
            while j < n and src[j] != "\n":
                # keep attribute detection working: comments blank to spaces
                out[j] = " "
                j += 1
            i = j
        elif c == '"':
            out[i] = '"'
            j = i + 1
            while j < n and src[j] != '"':
                if src[j] == "\\" and j + 1 < n:
                    out[j] = " "; out[j+1] = " "
                    j += 2
                    continue
                if src[j] != "\n":
                    out[j] = " "
                j += 1
            i = j + 1
        elif c == "'":
            # char literal: 'x' or '\n' — short, bounded scan
            j = i + 1
            if j < n and src[j] == "\\":
                j += 1
            j += 1
            if j < n and src[j] == "'":
                for k in range(i + 1, j):
                    if src[k] != "\n":
                        out[k] = " "
                i = j + 1
            else:
                i += 1
        else:
            i += 1
    return "".join(out)


def scan_balanced(s, i, open_ch, close_ch):
    """s[i] == open_ch; return (content, index-after-close). Balanced."""
    assert s[i] == open_ch
    depth, j = 1, i + 1
    start = j
    while j < len(s) and depth > 0:
        if s[j] == open_ch:
            depth += 1
        elif s[j] == close_ch:
            depth -= 1
        j += 1
    return s[start:j-1], j


def scan_generics(s, i):
    """Balanced <...> starting at s[i] == '<'. `->` never appears in a
    generic parameter list, so a bare '>' always closes a level."""
    depth, j = 1, i + 1
    start = j
    while j < len(s) and depth > 0:
        if s[j] == "<":
            depth += 1
        elif s[j] == ">":
            depth -= 1
        j += 1
    return s[start:j-1], j


FN_HEAD = re.compile(r"\bpub\s+(trusted\s+)?(extern\s+)?fn\s+(\w+)")
IMPL_HEAD = re.compile(r"\b(trusted\s+)?impl\b[^{]*\{")


def preceding_attr_lines(src, start):
    """Contiguous attribute (`#[...]`) / comment lines directly above the fn.
    Only real attribute lines count for proof-link detection — a `//` comment
    that MENTIONS #[proof_by] (parse_hex's honesty note) must not match.
    Runs over the ORIGINAL source (comments intact)."""
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
    g = f"<{re.sub(r'\\s+', ' ', gen.strip())}>" if gen else ""
    sig = f"{g}({p})"
    if caps is not None:
        c = ",".join(x.strip() for x in caps.split(",") if x.strip())
        sig += f" with({c})"
    r = re.sub(r"\s+", " ", (ret or "").strip())
    if r:
        sig += f" -> {r}"
    return sig


def parse_fns(src):
    """Yield dicts for every `pub [trusted] [extern] fn` with balanced
    signature parts and the enclosing impl's trusted-ness."""
    clean = strip_comments_strings(src)
    # impl regions: (start_brace_index, end_index, trusted?)
    impl_regions = []
    for im in IMPL_HEAD.finditer(clean):
        brace = clean.index("{", im.start())
        _, end = scan_balanced(clean, brace, "{", "}")
        impl_regions.append((brace, end, bool(im.group(1))))

    for m in FN_HEAD.finditer(clean):
        fn_trusted, fn_extern, name = bool(m.group(1)), bool(m.group(2)), m.group(3)
        i = m.end()
        while i < len(clean) and clean[i].isspace():
            i += 1
        gen = None
        if i < len(clean) and clean[i] == "<":
            gen, i = scan_generics(clean, i)
            while i < len(clean) and clean[i].isspace():
                i += 1
        if i >= len(clean) or clean[i] != "(":
            continue  # not a declaration (defensive)
        params, i = scan_balanced(clean, i, "(", ")")
        rest = clean[i:]
        # optional with(...)
        caps = None
        wm = re.match(r"\s*with\s*\(", rest)
        if wm:
            caps, j = scan_balanced(rest, wm.end() - 1, "(", ")")
            rest = rest[j:]
        # optional -> ret, up to body '{' or decl ';'
        ret = ""
        rm = re.match(r"\s*->\s*([^\{;]+)", rest)
        if rm:
            ret = rm.group(1)
        in_trusted_impl = any(b <= m.start() < e and t for (b, e, t) in impl_regions)
        yield {
            "start": m.start(),
            "name": name,
            "trusted": fn_trusted or in_trusted_impl,
            "extern": fn_extern,
            "gen": gen,
            "params": params,
            "caps": caps,
            "ret": ret,
        }


def rows(paths=None):
    out = []
    for path in paths if paths is not None else sorted(glob.glob("std/src/*.con")):
        mod = os.path.basename(path)[:-4]
        src = open(path, encoding="utf-8").read()
        for fn in parse_fns(src):
            params = re.sub(r"\s+", " ", fn["params"].strip())
            ret = fn["ret"].strip()
            caps_set = [c.strip() for c in (fn["caps"] or "").split(",") if c.strip()]
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
            if fn["trusted"]: pc = "trusted-boundary"
            elif caps_set: pc = "hosted-effect"
            elif mod in PURE_CORE: pc = "pure-core"
            else: pc = "deterministic"
            attrs = preceding_attr_lines(src, fn["start"])
            evidence = "proved" if any(a.startswith("#[proof_by(") for a in attrs) else "gated"
            sig = canon_sig(fn["gen"], params, fn["caps"], ret)
            out.append((mod, fn["name"], alloc, own, fails, cap, pc, evidence, sig))
    return out


if __name__ == "__main__":
    # optional explicit file args (the gate's parser self-test fixture)
    paths = sys.argv[1:] or None
    for r in rows(paths):
        print("\t".join(r))

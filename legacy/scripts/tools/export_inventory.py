#!/usr/bin/env python3
"""Read a lean4export NDJSON stream (format 3.1.0) for the external-checker gate.

  export_inventory.py meta FILE            lean version, exporter version, format version
  export_inventory.py theorems FILE        every exported theorem's full name, one per line
  export_inventory.py axioms FILE          every exported axiom's full name, one per line
  export_inventory.py statements FILE      every exported theorem as `<name>\t<universe params>\t<type>`,
                                           in the canonical form proof_layer_theorems.lean renders
  export_inventory.py swap FILE A B OUT    copy FILE to OUT with the later of A/B given the earlier's
                                           proof; prints "RECEIVER DONOR"

`swap` is the hostile control: the receiver keeps its statement, the proof it is given proves the
donor's. A checker that accepts the result is not checking the proofs it is handed.
"""
import json
import sys


def records(path):
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.strip()
            if line:
                yield json.loads(line)


def name_table(path):
    names = {0: ""}
    for r in records(path):
        if "str" in r and "in" in r:
            pre, s = r["str"]["pre"], r["str"]["str"]
            names[r["in"]] = s if names.get(pre, "") == "" else names[pre] + "." + s
        elif "num" in r and "in" in r:
            pre, i = r["num"]["pre"], r["num"]["i"]
            names[r["in"]] = str(i) if names.get(pre, "") == "" else f"{names[pre]}.{i}"
    return names


def lstr(s):
    return f"{len(s.encode('utf-8'))}:{s}"


def statements(path):
    """Canonical rendering of every exported theorem's statement, in the form proof_layer_theorems.lean
    renders from the kernel's own Expr (change both or neither). It is the kernel's view: binder
    names, binder annotations and the let `nondep` flag are erased, because the kernel ignores them
    and lean4export does not preserve them — it shares one index among expressions equal under
    Lean's `Expr` BEq, which ignores exactly those, so the first spelling written wins."""
    names, lvrec, exrec, thms = {0: ""}, {0: None}, {}, []
    for r in records(path):
        if "in" in r:
            k = "str" if "str" in r else "num"
            pre = names.get(r[k]["pre"], "")
            part = r[k]["str"] if k == "str" else str(r[k]["i"])
            names[r["in"]] = part if pre == "" else pre + "." + part
        elif "il" in r:
            lvrec[r["il"]] = r
        elif "ie" in r:
            exrec[r["ie"]] = r
        elif "thm" in r:
            thms.append(r["thm"])
    lv = {0: "0"}

    def level(i):
        if i in lv:
            return lv[i]
        r = lvrec[i]
        if "succ" in r: out = f"(s {level(r['succ'])})"
        elif "max" in r: out = f"(max {level(r['max'][0])} {level(r['max'][1])})"
        elif "imax" in r: out = f"(imax {level(r['imax'][0])} {level(r['imax'][1])})"
        elif "param" in r: out = f"(p {lstr(names[r['param']])})"
        else: raise ValueError(f"unknown level record {r}")
        lv[i] = out
        return out

    def children(r):
        if "app" in r: return [r["app"]["fn"], r["app"]["arg"]]
        for k in ("lam", "forallE"):
            if k in r: return [r[k]["type"], r[k]["body"]]
        if "letE" in r: return [r["letE"]["type"], r["letE"]["value"], r["letE"]["body"]]
        if "mdata" in r: return [r["mdata"]["expr"]]
        if "proj" in r: return [r["proj"]["struct"]]
        return []

    ex = {}

    def expr(root):
        # iterative post-order: statements nest far deeper than Python's recursion limit
        stack = [root]
        while stack:
            i = stack[-1]
            if i in ex:
                stack.pop()
                continue
            r = exrec[i]
            pending = [c for c in children(r) if c not in ex]
            if pending:
                stack.extend(pending)
                continue
            stack.pop()
            if "bvar" in r: out = f"(b {r['bvar']})"
            elif "sort" in r: out = f"(S {level(r['sort'])})"
            elif "const" in r:
                c = r["const"]
                out = f"(c {lstr(names[c['name']])} [{' '.join(level(u) for u in c['us'])}])"
            elif "app" in r: out = f"(a {ex[r['app']['fn']]} {ex[r['app']['arg']]})"
            elif "lam" in r: out = f"(l {ex[r['lam']['type']]} {ex[r['lam']['body']]})"
            elif "forallE" in r: out = f"(f {ex[r['forallE']['type']]} {ex[r['forallE']['body']]})"
            elif "letE" in r:
                b = r["letE"]
                out = f"(let {ex[b['type']]} {ex[b['value']]} {ex[b['body']]})"
            elif "natVal" in r: out = f"(n {r['natVal']})"
            elif "strVal" in r: out = f"(str {lstr(r['strVal'])})"
            elif "mdata" in r: out = ex[r["mdata"]["expr"]]
            elif "proj" in r:
                b = r["proj"]
                out = f"(j {lstr(names[b['typeName']])} {b['idx']} {ex[b['struct']]})"
            else: raise ValueError(f"unknown expr record {r}")
            ex[i] = out
        return ex[root]

    rows = [f"{names[t['name']]}\t{' '.join(lstr(names[u]) for u in t['levelParams'])}\t{expr(t['type'])}"
            for t in thms]
    return sorted(rows)


def decl_names(path, kind):
    names = name_table(path)
    return sorted(names[r[kind]["name"]] for r in records(path) if kind in r)


def main(argv):
    if len(argv) < 3:
        print(__doc__, file=sys.stderr)
        return 2
    cmd, path = argv[1], argv[2]
    if cmd == "meta":
        m = next(records(path))["meta"]
        print(m["lean"]["version"], m["exporter"]["version"], m["format"]["version"])
    elif cmd == "theorems":
        print("\n".join(decl_names(path, "thm")))
    elif cmd == "statements":
        print("\n".join(statements(path)))
    elif cmd == "axioms":
        print("\n".join(decl_names(path, "axiom")))
    elif cmd == "swap" and len(argv) == 6:
        a, b, out = argv[3], argv[4], argv[5]
        names = name_table(path)
        value, line = {}, {}
        for i, r in enumerate(records(path)):
            if "thm" in r:
                n = names[r["thm"]["name"]]
                value[n], line[n] = r["thm"]["value"], i
        if a not in value or b not in value:
            print(f"swap: theorem not exported: {a if a not in value else b}", file=sys.stderr)
            return 2
        # The stream defines every expression before its first use. The LATER theorem receives the
        # earlier one's proof, so the doctored stream stays well-formed and the only defect left is
        # the one under test: a proof of the wrong statement. (The other way round, the checker
        # refuses a forward reference and never reaches the type.)
        receiver, donor = (a, b) if line[a] > line[b] else (b, a)
        with open(out, "w", encoding="utf-8") as o:
            for r in records(path):
                if "thm" in r and names[r["thm"]["name"]] == receiver:
                    r["thm"]["value"] = value[donor]
                o.write(json.dumps(r, separators=(",", ":")) + "\n")
        print(receiver, donor)
    else:
        print(__doc__, file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

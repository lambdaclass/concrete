#!/usr/bin/env python3
"""Read a lean4export NDJSON stream (format 3.1.0) for scripts/proofs/external.sh.

  export_inventory.py meta FILE            lean version, exporter version, format version
  export_inventory.py theorems FILE        every exported theorem's name (encoded), one per line
  export_inventory.py axioms FILE          every exported axiom's name (dotted), one per line
  export_inventory.py names FILE           every exported declaration's name (encoded), one per line
  export_inventory.py encode FILE          every name of the stream's name table (encoded), one per line
  export_inventory.py decls FILE           every exported declaration in the canonical declaration
                                           form theorems.lean renders (PROOF_MODE=decls)
  export_inventory.py swap FILE A B OUT    copy FILE to OUT with the later of theorems A/B (encoded)
                                           given the earlier's proof; prints "RECEIVER DONOR" dotted,
                                           then both encoded

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


def name_table(path, encoded=False):
    """Every name of the stream, by index: dotted for display (`a.b.0`), or with encoded=True in the
    injective encoding theorems.lean's `enc` uses (each component as `s<utf-8 length>:<bytes>` or
    `n<digits>:`), which is what every comparison uses: dotted names of different structure can agree."""
    names = {0: ""}
    for r in records(path):
        if "str" in r and "in" in r:
            pre, s = r["str"]["pre"], r["str"]["str"]
            if encoded: names[r["in"]] = names[pre] + "s" + lstr(s)
            else: names[r["in"]] = s if names.get(pre, "") == "" else names[pre] + "." + s
        elif "num" in r and "in" in r:
            pre, i = r["num"]["pre"], r["num"]["i"]
            if encoded: names[r["in"]] = names[pre] + f"n{i}:"
            else: names[r["in"]] = str(i) if names.get(pre, "") == "" else f"{names[pre]}.{i}"
    return names


def lstr(s):
    return f"{len(s.encode('utf-8'))}:{s}"


def decls(path):
    """Every declaration of the stream in the canonical declaration form theorems.lean renders from
    Lean's own environment (PROOF_MODE=decls; change both or neither): declarations in name order,
    each field's expression before the next field and an expression's children left to right; each
    distinct node numbered when first met and written as `<number>\t<node>` naming its children by
    number; then the line `D\t<name>\t<kind>\t<fields>`. Binder names, binder annotations, the let
    `nondep` flag and metadata are erased: the kernel ignores them, and lean4export shares one index
    among expressions equal under Lean's `Expr` BEq, which ignores exactly those."""
    names, lvrec, exrec, consts = name_table(path, encoded=True), {0: None}, {}, {}
    for r in records(path):
        if "in" in r:
            continue                    # names: name_table above
        elif "il" in r:
            lvrec[r["il"]] = r
        elif "ie" in r:
            exrec[r["ie"]] = r
        elif "inductive" in r:
            ind = r["inductive"]
            for t in ind["types"]: consts.setdefault(names[t["name"]], []).append(("inductive", t))
            for c in ind["ctors"]: consts.setdefault(names[c["name"]], []).append(("ctor", c))
            for c in ind["recs"]: consts.setdefault(names[c["name"]], []).append(("rec", c))
        else:
            for k in ("axiom", "def", "thm", "opaque", "quot"):
                if k in r:
                    consts.setdefault(names[r[k]["name"]], []).append((k, r[k]))
    lv = {0: "0"}

    def level(i):
        if i in lv:
            return lv[i]
        r = lvrec[i]
        if "succ" in r: out = f"(s {level(r['succ'])})"
        elif "max" in r: out = f"(max {level(r['max'][0])} {level(r['max'][1])})"
        elif "imax" in r: out = f"(imax {level(r['imax'][0])} {level(r['imax'][1])})"
        elif "param" in r: out = f"(p {names[r['param']]})"
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

    ids, num, out = {}, {}, []

    def node(text):
        if text not in ids:
            ids[text] = len(ids)
            out.append(f"{ids[text]}\t{text}")
        return ids[text]

    def dag(root):
        # iterative post-order, children left to right: terms nest far deeper than Python's recursion
        stack = [(root, False)]
        while stack:
            i, expanded = stack.pop()
            if i in num:
                continue
            r = exrec[i]
            if not expanded:
                stack.append((i, True))
                stack.extend((c, False) for c in reversed(children(r)))
                continue
            c = [num[j] for j in children(r)]
            if "bvar" in r: num[i] = node(f"(b {r['bvar']})")
            elif "sort" in r: num[i] = node(f"(S {level(r['sort'])})")
            elif "const" in r:
                k = r["const"]
                num[i] = node(f"(c {names[k['name']]} [{' '.join(level(u) for u in k['us'])}])")
            elif "app" in r: num[i] = node(f"(a {c[0]} {c[1]})")
            elif "lam" in r: num[i] = node(f"(l {c[0]} {c[1]})")
            elif "forallE" in r: num[i] = node(f"(f {c[0]} {c[1]})")
            elif "letE" in r: num[i] = node(f"(let {c[0]} {c[1]} {c[2]})")
            elif "natVal" in r: num[i] = node(f"(n {r['natVal']})")
            elif "strVal" in r: num[i] = node(f"(str {lstr(r['strVal'])})")
            elif "mdata" in r: num[i] = c[0]
            elif "proj" in r:
                k = r["proj"]
                num[i] = node(f"(j {names[k['typeName']]} {k['idx']} {c[0]})")
            else: raise ValueError(f"unknown expr record {r}")
        return num[root]

    def nms(xs):
        return "".join(names[x] + ";" for x in xs)      # `;`-terminated: [] and [anonymous] differ

    def flag(b):
        return "1" if b else "0"

    for n in sorted(consts):
        if len(consts[n]) != 1:
            raise ValueError(f"the stream declares {n} {len(consts[n])} times")
        kind, d = consts[n][0]
        lp = nms(d["levelParams"])
        t = dag(d["type"])
        head = f"D\t{n}\t{{}}\t[{lp}]\tT={t}"
        if kind == "axiom":
            line = head.format("axiom") + f" unsafe={flag(d['isUnsafe'])}"
        elif kind == "def":
            v = dag(d["value"])
            h = d["hints"]
            hints = h if isinstance(h, str) else f"regular {h['regular']}"
            line = head.format("def") + f" V={v} hints={hints} safety={d['safety']} all=[{nms(d['all'])}]"
        elif kind == "thm":
            line = head.format("thm") + f" V={dag(d['value'])} all=[{nms(d['all'])}]"
        elif kind == "opaque":
            v = dag(d["value"])
            line = head.format("opaque") + f" V={v} unsafe={flag(d['isUnsafe'])} all=[{nms(d['all'])}]"
        elif kind == "quot":
            line = head.format("quot") + f" kind={d['kind']}"
        elif kind == "inductive":
            line = head.format("inductive") + (
                f" params={d['numParams']} indices={d['numIndices']} all=[{nms(d['all'])}]"
                f" ctors=[{nms(d['ctors'])}] rec={flag(d['isRec'])} unsafe={flag(d['isUnsafe'])}"
                f" reflexive={flag(d['isReflexive'])} nested={d['numNested']}")
        elif kind == "ctor":
            line = head.format("ctor") + (
                f" induct={names[d['induct']]} cidx={d['cidx']} params={d['numParams']}"
                f" fields={d['numFields']} unsafe={flag(d['isUnsafe'])}")
        else:
            rules = "; ".join(f"{names[x['ctor']]} {x['nfields']} {dag(x['rhs'])}" for x in d["rules"])
            line = head.format("rec") + (
                f" all=[{nms(d['all'])}] params={d['numParams']} indices={d['numIndices']}"
                f" motives={d['numMotives']} minors={d['numMinors']} k={flag(d['k'])}"
                f" unsafe={flag(d['isUnsafe'])} rules=[{rules}]")
        out.append(line)
    return out


def decl_names_all(path):
    names = name_table(path, encoded=True)
    out = []
    for r in records(path):
        if "inductive" in r:
            for k in ("types", "ctors", "recs"):
                out.extend(names[x["name"]] for x in r["inductive"][k])
        else:
            out.extend(names[r[k]["name"]] for k in ("axiom", "def", "thm", "opaque", "quot") if k in r)
    return sorted(out)


def decl_names(path, kind, encoded=False):
    names = name_table(path, encoded)
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
        print("\n".join(decl_names(path, "thm", encoded=True)))
    elif cmd == "decls":
        print("\n".join(decls(path)))
    elif cmd == "encode":
        print("\n".join(v for k, v in sorted(name_table(path, encoded=True).items()) if k))
    elif cmd == "names":
        print("\n".join(decl_names_all(path)))
    elif cmd == "axioms":
        print("\n".join(decl_names(path, "axiom")))
    elif cmd == "swap" and len(argv) == 6:
        a, b, out = argv[3], argv[4], argv[5]
        names, shown = name_table(path, encoded=True), name_table(path)
        value, line, show = {}, {}, {}
        for i, r in enumerate(records(path)):
            if "thm" in r:
                n = names[r["thm"]["name"]]
                value[n], line[n], show[n] = r["thm"]["value"], i, shown[r["thm"]["name"]]
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
        print(show[receiver], show[donor], receiver, donor)
    else:
        print(__doc__, file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

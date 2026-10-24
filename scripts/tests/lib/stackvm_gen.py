#!/usr/bin/env python3
"""Seeded program generator for check_stackvm_differential.sh.

stackvm_gen.py <outdir> <seed> <n>: writes n assembly programs:
  gen_NNN.sasm   structured valid programs: arithmetic over random constants (overflow and
                 division by zero arise naturally), counted loops over locals, acyclic calls with
                 parameters, a bounded recursion; every one terminates well inside the step limit;
  mut_NNN.sasm   one random line-level mutation of a valid program (deleted, duplicated or
                 swapped lines, a corrupted token), so the assembler's diagnostics and their
                 order are compared too.
"""
import random
import sys

OPS = ["add", "sub", "mul", "div", "mod", "eq", "lt", "gt"]


def expr(r, depth, nlocals):
    """Lines that push exactly one value."""
    k = r.random()
    if depth <= 0 or k < 0.3:
        if nlocals and r.random() < 0.4:
            return ["load %d" % r.randrange(nlocals)]
        if r.random() < 0.06:
            c = 0
        elif r.random() < 0.08:
            c = r.choice([2**62, -(2**62), 2**63 - 1, -(2**63 - 1), 3037000499])
        else:
            c = r.choice([1, 2, 3, 7, -1, -5, 100, 65536, r.randrange(1, 1000), -r.randrange(1, 1000)])
        return ["push %d" % c]
    if k < 0.45:
        return expr(r, depth - 1, nlocals) + [r.choice(["neg", "not"])]
    return expr(r, depth - 1, nlocals) + expr(r, depth - 1, nlocals) + [r.choice(OPS)]


def body(r, name, nparams, nlocals, callees, label_seq):
    out = []
    for _ in range(r.randrange(1, 5)):
        k = r.random()
        if k < 0.35:
            out += expr(r, 3, nlocals) + ["print"]
        elif k < 0.55 and nlocals:
            out += expr(r, 2, nlocals) + ["store %d" % r.randrange(nlocals)]
        elif k < 0.75 and nlocals:
            i = r.randrange(nlocals)
            top, done = "L%d" % next(label_seq), "L%d" % next(label_seq)
            out += ["push %d" % r.randrange(0, 6), "store %d" % i,
                    "%s:" % top, "load %d" % i, "jz %s" % done]
            out += expr(r, 2, nlocals) + ["print"]
            out += ["load %d" % i, "push 1", "sub", "store %d" % i, "jmp %s" % top,
                    "%s:" % done]
        elif callees:
            g, gp = r.choice(callees)
            for _ in range(gp):
                out += expr(r, 1, nlocals)
            out += ["call %s" % g, "print"]
        else:
            out += expr(r, 2, nlocals) + [r.choice(["dup", "pop"]) if r.random() < 0.5 else "print"]
            if out[-1] == "dup":
                out += ["print", "print"]
            elif out[-1] == "pop":
                pass
    out += expr(r, 2, nlocals) + ["ret"]
    return out


def program(r):
    counter = iter(range(10**6))
    funcs, text = [], []
    for fi in range(r.randrange(0, 4)):
        name = "f%d" % fi
        np_ = r.randrange(0, 3)
        nl_ = np_ + r.randrange(0, 3)
        lines = body(r, name, np_, nl_, list(funcs), counter)
        text += ["fn %s %d %d" % (name, np_, nl_)] + ["    " + l for l in lines] + ["end", ""]
        funcs.append((name, np_))
    # a bounded recursion: countdown(n) = n == 0 ? 0 : n + countdown(n - 1)
    text += ["fn countdown 1 1", "    load 0", "    jnz go", "    push 0", "    ret", "go:",
             "    load 0", "    load 0", "    push 1", "    sub", "    call countdown", "    add",
             "    ret", "end", ""]
    nl_ = r.randrange(0, 4)
    lines = body(r, "main", 0, nl_, funcs + [("countdown", 1)], counter)
    if r.random() < 0.5:
        lines = ["push %d" % r.choice([r.randrange(0, 200), r.randrange(0, 200), 300]), "call countdown", "print"] + lines
    text += ["fn main 0 %d" % nl_] + ["    " + l for l in lines] + ["end", ""]
    return text


def mutate(r, lines):
    lines = list(lines)
    k = r.randrange(6)
    i = r.randrange(len(lines))
    if k == 0:
        del lines[i]
    elif k == 1:
        lines.insert(i, lines[i])
    elif k == 2 and len(lines) > 1:
        j = r.randrange(len(lines))
        lines[i], lines[j] = lines[j], lines[i]
    elif k == 3:
        lines[i] = lines[i] + " " + r.choice(["x", "1", "main", ";c"])
    elif k == 4:
        toks = lines[i].split()
        if toks:
            toks[-1] = r.choice(["999", "-", "9x", "Lnope", "_ok", "", "300", "-9223372036854775808"])
            lines[i] = " ".join(toks)
    else:
        lines.insert(i, r.choice(["bogus", "fn", "end", "lbl:", "push", "jmp", "fn main 0 0",
                                  "fn f0 1 0", "load 7", "call nowhere"]))
    return lines


def main():
    out, seed, n = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
    r = random.Random(seed)
    for i in range(n):
        lines = program(r)
        with open("%s/gen_%03d.sasm" % (out, i), "w") as fh:
            fh.write("\n".join(lines) + "\n")
        with open("%s/mut_%03d.sasm" % (out, i), "w") as fh:
            fh.write("\n".join(mutate(r, lines)) + "\n")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Reference implementation of examples/stackvm (Phase 7 workload 10).

Written from the specifications in the header comments of examples/stackvm/src/{main,asm,codec,
vm,dis}.con, not from their code, so check_stackvm_differential.sh can compare every output of the
Concrete program (assembled bytes, listings, program output, diagnostics, exit codes) with it.

Usage: stackvm_ref.py asm <in.sasm> <out.svm> | dis <in.svm> | run <in.svm> | exec <in.sasm>
                      | check <in.sasm>
"""
import re
import sys

I64_MIN, I64_MAX = -(1 << 63), (1 << 63) - 1
SIMPLE = ["halt", "pop", "dup", "swap", "add", "sub", "mul", "div", "mod", "neg", "eq", "lt",
          "gt", "not", "ret", "print"]
OPCODE = {"halt": 0, "push": 1, "pop": 2, "dup": 3, "swap": 4, "add": 5, "sub": 6, "mul": 7,
          "div": 8, "mod": 9, "neg": 10, "eq": 11, "lt": 12, "gt": 13, "not": 14, "jmp": 15,
          "jz": 16, "jnz": 17, "load": 18, "store": 19, "call": 20, "ret": 21, "print": 22}
MNEMONIC = {v: k for k, v in OPCODE.items()}
NAME = re.compile(rb"[A-Za-z_][A-Za-z0-9_]*")


def valid_name(b):
    return 0 < len(b) <= 32 and NAME.fullmatch(b) is not None


class Fail(Exception):
    def __init__(self, line, msg):
        super().__init__(msg)
        self.line, self.msg = line, msg


class Program:
    def __init__(self):
        self.code = []    # [mnemonic, operand-or-None]
        self.funcs = []   # [entry, len, nparams, nlocals]
        self.names = []   # bytes


def parse_uint(t):
    if not re.fullmatch(rb"[0-9]+", t):
        return None
    v = int(t)
    return v if v < (1 << 64) else None


def parse_int(t):
    if not re.fullmatch(rb"-?[0-9]+", t):
        return None
    v = int(t)
    return v if -I64_MAX <= v <= I64_MAX else None


def small(t):
    v = parse_uint(t)
    return v if v is not None and v <= 255 else None


def q(t):
    return "'" + t.decode("latin-1") + "'"


MAX_FIXUPS = 1000000   # unresolved references per worklist (a function's jumps; all calls)


def assemble(src):
    p = Program()
    in_fn = False
    labels, jumps, calls = {}, [], []   # labels: name -> (pos, line), in definition order
    fn_line = entry = nparams = nlocals = 0
    lines = src.split(b"\n")
    for lineno, raw in enumerate(lines, 1):
        body = raw.split(b";", 1)[0]
        toks = [t for t in re.split(rb"[ \t\r]+", body) if t]
        if not toks:
            continue
        t0 = toks[0]
        if t0 == b"fn":
            if in_fn:
                raise Fail(lineno, "nested fn")
            if len(toks) != 4:
                raise Fail(lineno, "bad fn header")
            if not valid_name(toks[1]):
                raise Fail(lineno, "bad name " + q(toks[1]))
            np_, nl_ = small(toks[2]), small(toks[3])
            np_ = 256 if np_ is None else np_
            nl_ = 256 if nl_ is None else nl_
            if np_ > 255 or nl_ > 255 or np_ > nl_:
                raise Fail(lineno, "bad fn header")
            if toks[1] in p.names:
                raise Fail(lineno, "duplicate function " + q(toks[1]))
            p.names.append(toks[1])
            in_fn, fn_line, entry, nparams, nlocals = True, lineno, len(p.code), np_, nl_
            labels, jumps = {}, []
            continue
        if t0 == b"end":
            if not in_fn:
                raise Fail(lineno, "end outside a function")
            if len(toks) != 1:
                raise Fail(lineno, "unexpected operand")
            for name, (pos, lline) in labels.items():
                if pos == len(p.code):
                    raise Fail(lline, "label " + q(name) + " has no instruction")
            for name, at, line in jumps:
                if name not in labels:
                    raise Fail(line, "undefined label " + q(name))
                p.code[at][1] = labels[name][0]
            if len(p.code) == entry:
                raise Fail(lineno, "empty function " + q(p.names[-1]))
            p.funcs.append([entry, len(p.code) - entry, nparams, nlocals])
            in_fn = False
            continue
        if not in_fn:
            raise Fail(lineno, "instruction outside a function")
        if t0.endswith(b":"):
            if len(toks) != 1:
                raise Fail(lineno, "unexpected operand")
            name = t0[:-1]
            if not valid_name(name):
                raise Fail(lineno, "bad name " + q(name))
            if name in labels:
                raise Fail(lineno, "duplicate label " + q(name))
            labels[name] = (len(p.code), lineno)
            continue
        m = t0.decode("latin-1")
        if m in SIMPLE:
            if len(toks) != 1:
                raise Fail(lineno, "unexpected operand")
            p.code.append([m, None])
            continue
        if m not in ("push", "jmp", "jz", "jnz", "load", "store", "call"):
            raise Fail(lineno, "unknown instruction " + q(t0))
        if len(toks) == 1:
            raise Fail(lineno, "expected operand")
        if len(toks) > 2:
            raise Fail(lineno, "unexpected operand")
        t1 = toks[1]
        if m == "push":
            v = parse_int(t1)
            if v is None:
                raise Fail(lineno, "bad integer " + q(t1))
            p.code.append([m, v])
        elif m in ("load", "store"):
            v = small(t1)
            if v is None:
                raise Fail(lineno, "bad slot " + q(t1))
            if v >= nlocals:
                raise Fail(lineno, "slot out of range " + q(t1))
            p.code.append([m, v])
        else:
            if not valid_name(t1):
                raise Fail(lineno, "bad name " + q(t1))
            work = calls if m == "call" else jumps
            if len(work) >= MAX_FIXUPS:
                raise Fail(lineno, "too many references")
            work.append((t1, len(p.code), lineno))
            p.code.append([m, 0])
    if in_fn:
        raise Fail(fn_line, "fn " + q(p.names[-1]) + " has no end")
    for name, at, line in calls:
        if name not in p.names:
            raise Fail(line, "undefined function " + q(name))
        p.code[at][1] = p.names.index(name)
    if b"main" not in p.names:
        raise Fail(0, "no function 'main'")
    if p.funcs[p.names.index(b"main")][2] != 0:
        raise Fail(0, "main takes no parameters")
    return p


def le(v, n):
    return (v % (1 << (8 * n))).to_bytes(n, "little")


def encode(p):
    out = bytearray(b"SVM1") + le(len(p.funcs), 4)
    for (entry, ln, np_, nl_), name in zip(p.funcs, p.names):
        out += le(entry, 4) + le(ln, 4) + le(np_, 1) + le(nl_, 1) + le(len(name), 1) + name
    out += le(len(p.code), 4)
    for m, x in p.code:
        out.append(OPCODE[m])
        if m == "push":
            out += le(x, 8)
        elif m in ("jmp", "jz", "jnz", "call"):
            out += le(x, 4)
        elif m in ("load", "store"):
            out += le(x, 1)
    return bytes(out)


class Truncated(Exception):
    pass


def load(b):
    if len(b) < 4 or b[:4] != b"SVM1":
        raise Fail(0, "bad magic")
    pos = [4]

    def rd(n):
        if pos[0] + n > len(b):
            raise Truncated()
        v = int.from_bytes(b[pos[0]:pos[0] + n], "little")
        pos[0] += n
        return v

    p = Program()
    try:
        nfuncs = rd(4)
        for _ in range(nfuncs):
            entry, ln, np_, nl_, nlen = rd(4), rd(4), rd(1), rd(1), rd(1)
            if pos[0] + nlen > len(b):
                raise Truncated()
            name = b[pos[0]:pos[0] + nlen]
            if not valid_name(name):
                raise Fail(0, "bad function name")
            pos[0] += nlen
            if nl_ < np_:
                raise Fail(0, "bad function table")
            p.funcs.append([entry, ln, np_, nl_])
            p.names.append(name)
        ncode = rd(4)
        for c in range(ncode):
            op = rd(1)
            if op not in MNEMONIC:
                raise Fail(0, "unknown opcode %d at %d" % (op, c))
            m = MNEMONIC[op]
            if m == "push":
                v = rd(8)
                p.code.append([m, v - (1 << 64) if v >= (1 << 63) else v])
            elif m in ("jmp", "jz", "jnz", "call"):
                p.code.append([m, rd(4)])
            elif m in ("load", "store"):
                p.code.append([m, rd(1)])
            else:
                p.code.append([m, None])
    except Truncated:
        raise Fail(0, "truncated")
    if pos[0] != len(b):
        raise Fail(0, "trailing bytes")
    nxt = 0
    for entry, ln, _, _ in p.funcs:
        if entry != nxt or ln == 0:
            raise Fail(0, "bad function table")
        nxt += ln
    if nxt != len(p.code):
        raise Fail(0, "bad function table")
    if len(set(p.names)) != len(p.names):
        raise Fail(0, "duplicate function name")
    for entry, ln, _, nl_ in p.funcs:
        for pc in range(entry, entry + ln):
            m, x = p.code[pc]
            if m in ("jmp", "jz", "jnz") and not (entry <= x < entry + ln):
                raise Fail(0, "jump out of function at %d" % pc)
            if m in ("load", "store") and x >= nl_:
                raise Fail(0, "bad slot at %d" % pc)
            if m == "call" and x >= len(p.funcs):
                raise Fail(0, "bad call at %d" % pc)
    if b"main" not in p.names:
        raise Fail(0, "no function 'main'")
    if p.funcs[p.names.index(b"main")][2] != 0:
        raise Fail(0, "main takes no parameters")
    return p


def disassemble(p):
    out = []
    for (entry, ln, np_, nl_), name in zip(p.funcs, p.names):
        out.append("fn %s params=%d locals=%d\n" % (name.decode("latin-1"), np_, nl_))
        for pc in range(entry, entry + ln):
            m, x = p.code[pc]
            if m in ("jmp", "jz", "jnz"):
                opd = " %04d" % x
            elif m == "call":
                opd = " " + p.names[x].decode("latin-1")
            elif x is not None:
                opd = " %d" % x
            else:
                opd = ""
            out.append("  %04d  %s%s\n" % (pc, m, opd))
        out.append("end\n")
    return "".join(out)


class VmFail(Exception):
    pass


def tdiv(a, b):
    qq = abs(a) // abs(b)
    return qq if (a < 0) == (b < 0) else -qq


def run(p, out):
    """Runs main; appends printed lines to `out`; returns None or (func, pc, msg)."""
    ops, locals_, frames = [], [], []
    state = {}

    def pop():
        if not ops:
            raise VmFail("stack underflow")
        return ops.pop()

    def push(v):
        if len(ops) >= 1024:
            raise VmFail("stack overflow")
        ops.append(v)

    def call(g, ret_pc):
        entry, _, np_, nl_ = p.funcs[g]
        if len(ops) < np_:
            raise VmFail("stack underflow")
        if len(frames) >= 256:
            raise VmFail("call stack overflow")
        base = len(locals_)
        locals_.extend([0] * nl_)
        for j in reversed(range(np_)):
            locals_[base + j] = ops.pop()
        frames.append((g, ret_pc, base))
        state.update(func=g, base=base, pc=entry)

    call(p.names.index(b"main"), 0)
    steps = 0
    while True:
        f, pc = state["func"], state["pc"]
        entry, ln, _, _ = p.funcs[f]
        if pc >= entry + ln:
            return (f, pc, "fell off the end of the function")
        if steps >= 1000000:
            return (f, pc, "step limit exceeded")
        steps += 1
        m, x = p.code[pc]
        nxt = pc + 1
        try:
            if m == "halt":
                return None
            elif m == "push":
                state["pc"] = nxt
                push(x)
            elif m == "pop":
                pop()
                state["pc"] = nxt
            elif m == "dup":
                if not ops:
                    raise VmFail("stack underflow")
                state["pc"] = nxt
                push(ops[-1])
            elif m == "swap":
                if len(ops) < 2:
                    raise VmFail("stack underflow")
                b_, a_ = ops.pop(), ops.pop()
                ops.extend([b_, a_])
                state["pc"] = nxt
            elif m == "neg":
                a_ = pop()
                if a_ == I64_MIN:
                    raise VmFail("integer overflow")
                state["pc"] = nxt
                push(-a_)
            elif m == "not":
                a_ = pop()
                state["pc"] = nxt
                push(1 if a_ == 0 else 0)
            elif m == "jmp":
                state["pc"] = x
            elif m in ("jz", "jnz"):
                a_ = pop()
                state["pc"] = x if (a_ == 0) == (m == "jz") else nxt
            elif m == "load":
                state["pc"] = nxt
                push(locals_[state["base"] + x])
            elif m == "store":
                v = pop()
                locals_[state["base"] + x] = v
                state["pc"] = nxt
            elif m == "call":
                call(x, nxt)
            elif m == "ret":
                v = pop()
                g, ret_pc, base = frames.pop()
                if not frames:
                    return None
                cf, _, cbase = frames[-1]
                del locals_[base:]
                state.update(func=cf, base=cbase, pc=ret_pc)
                push(v)
            elif m == "print":
                out.append("%d\n" % pop())
                state["pc"] = nxt
            else:
                b_ = pop()
                a_ = pop()
                if m == "add":
                    r = a_ + b_
                elif m == "sub":
                    r = a_ - b_
                elif m == "mul":
                    r = a_ * b_
                elif m in ("div", "mod"):
                    if b_ == 0:
                        raise VmFail("division by zero")
                    if a_ == I64_MIN and b_ == -1:
                        raise VmFail("integer overflow")
                    r = tdiv(a_, b_) if m == "div" else a_ - b_ * tdiv(a_, b_)
                elif m == "eq":
                    r = int(a_ == b_)
                elif m == "lt":
                    r = int(a_ < b_)
                else:
                    r = int(a_ > b_)
                if not I64_MIN <= r <= I64_MAX:
                    raise VmFail("integer overflow")
                push(r)
                state["pc"] = nxt
        except VmFail as e:
            return (f, pc, str(e))


def asm_error(e):
    return "asm error: " + ("line %d: " % e.line if e.line else "") + e.msg


def execute(p):
    out = []
    err = run(p, out)
    sys.stdout.write("".join(out))
    if err:
        f, pc, msg = err
        print("runtime error: in '%s' at %d: %s" % (p.names[f].decode("latin-1"), pc, msg),
              file=sys.stderr)
        return 1
    return 0


def summary(p, prefix):
    return "%s%d functions, %d instructions" % (prefix, len(p.funcs), len(p.code))


def main(argv):
    usage = ("usage: stackvm asm <in.sasm> <out.svm> | dis <in.svm> | run <in.svm> | "
             "exec <in.sasm> | check <in.sasm>")
    cmd = argv[1] if len(argv) > 1 else ""
    if not ((len(argv) == 4 and cmd == "asm") or
            (len(argv) == 3 and cmd in ("dis", "run", "exec", "check"))):
        print(usage, file=sys.stderr)
        return 2
    try:
        with open(argv[2], "rb") as fh:
            data = fh.read()
    except OSError:
        print("stackvm: cannot read input", file=sys.stderr)
        return 1
    try:
        if cmd in ("asm", "exec", "check"):
            try:
                p = assemble(data)
            except Fail as e:
                print(asm_error(e), file=sys.stderr)
                return 1
            if cmd == "exec":
                return execute(p)
            b = encode(p)
            if cmd == "asm":
                try:
                    with open(argv[3], "wb") as fh:
                        fh.write(b)
                except OSError:
                    print("stackvm: cannot write output", file=sys.stderr)
                    return 1
                print(summary(p, "assembled "))
                return 0
            back = load(b)
            if disassemble(back) != disassemble(p):
                print("stackvm: round trip changed the program", file=sys.stderr)
                return 1
            print(summary(p, "ok ") + ", %d bytes" % len(b))
            return 0
        p = load(data)
    except Fail as e:
        print("load error: " + e.msg, file=sys.stderr)
        return 1
    if cmd == "dis":
        sys.stdout.write(disassemble(p))
        return 0
    return execute(p)


if __name__ == "__main__":
    sys.exit(main(sys.argv))

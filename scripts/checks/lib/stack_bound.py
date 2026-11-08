#!/usr/bin/env python3
"""Stack bound of a slice-0 executable, from its disassembly (README gate 12). PROVISIONAL.

usage: stack_bound.py ARCH DISASM             -> prints `bound N` and one `external SYM DEPTH` per call
       stack_bound.py ARCH DISASM --ranges    -> also prints `range SYM LO HI` per Concrete function
ARCH is x86_64 or aarch64; DISASM is `llvm-objdump -d --no-show-raw-insn` output of the LINKED
executable (x86-64 in Intel syntax).

The Concrete-owned code is `main` (the runtime entry), `concrete_rt_write_all` and the body
(`concrete_body`), with or without the Mach-O underscore. From `main`'s entry, every path through
them is followed over the control-flow graph, tracking the stack pointer's depth below its value
at `main`'s entry (and the frame pointer's). The bound is the deepest point reached, by the stack
pointer or by any byte addressed relative to it or the frame pointer (the red zone below the
stack pointer is used without moving it), counting each
internal callee's own bound and the depth at which each external function (libc's `write`,
`_exit`, `abort`) is entered; the external functions' own use is outside it (a premise, listed),
and so is that `_exit` and `abort` do not return (a path ends at a call to one).

Stack addresses never leave the stack and frame pointers: taking one as a value, storing or
loading a saved frame pointer other than through the modelled frame save and restore, touching
bytes at or above the entry stack pointer (the return address, the caller's frame) and returning
through a link register that does not hold the return address are all refused (St, x86_inst,
a64_inst).

Fail closed: an instruction outside the modelled set (X86_PLAIN, A64_PLAIN and the control
transfers), any instruction that writes the stack pointer in a way not modelled here, an indirect
branch or call, a branch leaving its function, two different depths at one address, a return at a
nonzero depth, recursion, or a call to a function that is neither Concrete-owned nor a PLT/stub
symbol is an error (exit 1), never a bound.
"""
import re
import sys

OWNED = ("main", "concrete_body", "concrete_rt_write_all")
# External functions that do not return (part of the libc premise): a path ends at a call to one.
NORETURN = ("exit", "_exit", "_Exit", "abort")


class Refuse(Exception):
    pass


def owned_name(sym):
    s = sym[1:] if sym.startswith("_") and sym[1:] in OWNED else sym
    return s if s in OWNED else None


def parse(path):
    """Functions: name -> list of (addr, mnemonic, operands). Also the set of stub symbols."""
    funcs, cur, stubs, section = {}, None, set(), ""
    head = re.compile(r"^([0-9a-f]+) <([^>]+)>:$")
    inst = re.compile(r"^\s*([0-9a-f]+):\s*(\S+)\s*(.*)$")
    for line in open(path, encoding="utf-8", errors="replace"):
        line = line.rstrip("\n")
        if line.startswith("Disassembly of section"):
            section = line.split()[-1].rstrip(":")
            cur = None
            continue
        m = head.match(line)
        if m:
            name = m.group(2)
            if section in (".plt", ".plt.sec", ".plt.got", "__stubs") or name.endswith("@plt"):
                stubs.add(name)
                cur = None
            else:
                cur = name
                funcs.setdefault(name, [])
            continue
        m = inst.match(line)
        if m and cur is not None:
            ops = m.group(3).split("#", 1)[0] if ARCH == "x86_64" else m.group(3).split("//")[0]
            funcs[cur].append((int(m.group(1), 16), m.group(2).lower(), ops.strip()))
    return funcs, stubs


def target_of(ops):
    m = re.search(r"0x([0-9a-f]+)\s*<([^>]+)>", ops)
    if not m:
        m2 = re.search(r"\b([0-9a-f]+)\s*<([^>]+)>", ops)
        if not m2:
            return None, None
        m = m2
    return int(m.group(1), 16), m.group(2)


def imm(s):
    t = s.strip().lstrip("#")
    try:
        return int(t, 16) if t.lower().startswith(("0x", "-0x")) else int(t)
    except ValueError:
        raise Refuse(f"not an immediate: {s}") from None


X86_SP = ("rsp", "esp", "sp", "spl")
X86_FP = ("rbp", "ebp", "bp", "bpl")
X86_STACK = X86_SP + X86_FP
X86_GPR64 = re.compile(r"^(r[abcd]x|r[sd]i|rbp|r(?:8|9|1[0-5]))$")
X86_SIZE = {"byte": 1, "word": 2, "dword": 4, "qword": 8, "tbyte": 10, "xmmword": 16,
            "ymmword": 32, "zmmword": 64}
MEM = re.compile(r"\[([^\]]*)\]")


class St:
    """The abstract state at an instruction: `d`, the stack pointer's depth below the function's
    entry; `fp`, the frame pointer's depth (None: unknown); `saved`, the stack slots holding a
    saved frame pointer or link register, as (lowest, highest) byte depths with a tag ("fp", or
    "lr" with whether it holds the function's own return address); `lr`, whether the AArch64 link
    register holds the function's own return address. A byte at depth n lies n bytes below the
    entry stack pointer; the function's own bytes have depth >= 1."""

    __slots__ = ("d", "fp", "saved", "lr")

    def __init__(self, d=0, fp=None, saved=frozenset(), lr=True):
        self.d, self.fp, self.saved, self.lr = d, fp, saved, lr

    def key(self):
        return (self.d, self.fp, self.saved, self.lr)

    def but(self, **kw):
        s = St(self.d, self.fp, self.saved, self.lr)
        for k, v in kw.items():
            setattr(s, k, v)
        # Slots wholly below the stack pointer are free memory: drop them.
        s.saved = frozenset(x for x in s.saved if x[0] <= s.d)
        return s


def check_bytes(st, lo, hi, what, allowed=()):
    """The bytes [lo, hi] (depths) are the function's own and overlap no saved slot, except the
    slots `allowed` (the frame-record save/restore itself)."""
    if lo < 1:
        raise Refuse(f"touches the return address or the caller's frame: {what}")
    for slot in st.saved:
        if slot not in allowed and slot[0] <= hi and lo <= slot[1]:
            raise Refuse(f"touches a saved frame pointer or link register: {what}")


def x86_inst(mn, ops, st):
    """(new state, deepest byte touched or None) after a non-control x86 instruction. Fails closed:
    a stack or frame pointer read as a value (other than the modelled frame setup), a stack
    address taken (`lea`), an indexed or partial-register stack address, an access of unknown size,
    an access at or above the entry stack pointer or into a saved frame pointer, a stack-pointer
    write that is not modelled, a push or pop of other than a 64-bit general register."""
    what = f"{mn} {ops}"
    args = [a.strip() for a in ops.split(",")] if ops else []
    dst = args[0] if args else ""
    deepest = None
    for m in MEM.finditer(ops):
        inner = m.group(1)
        terms = re.findall(r"[+-]?\s*[^+\-\s]+", inner)
        regs = [t.strip(" +-") for t in terms if not re.fullmatch(r"[+-]?\s*(0x[0-9a-f]+|\d+)", t)]
        base = [r for r in regs if r in X86_STACK]
        if not base:
            continue
        if base[0] not in ("rsp", "rbp"):
            raise Refuse(f"stack address through a partial register: {what}")
        if len(regs) != 1:
            raise Refuse(f"indexed stack address: {what}")
        if mn == "lea":
            raise Refuse(f"stack address taken as a value: {what}")
        size = re.search(r"(\w+) ptr\s*(?:\w+:)?\s*$", ops[:m.start()])
        if not size or size.group(1) not in X86_SIZE:
            raise Refuse(f"stack access of unknown size: {what}")
        k = sum(int(t.replace(" ", ""), 0) for t in terms if t.strip(" +-") not in regs)
        if base[0] == "rbp":
            if st.fp is None:
                raise Refuse(f"frame-relative address with an unknown frame pointer: {what}")
            top = st.fp - k
        else:
            top = st.d - k
        check_bytes(st, top - X86_SIZE[size.group(1)] + 1, top, what)
        deepest = top if deepest is None else max(deepest, top)
    outside = MEM.sub("", ops)
    modelled = (mn, ops) in (("push", "rbp"), ("pop", "rbp"), ("mov", "rbp, rsp"),
                             ("mov", "rsp, rbp"), ("leave", "")) or (
        mn in ("sub", "add") and dst == "rsp" and re.fullmatch(r"rsp, (0x[0-9a-f]+|\d+)", ops))
    if not modelled and any(re.search(rf"\b{r}\b", outside) for r in X86_STACK):
        raise Refuse(f"stack or frame pointer used as a value: {what}")
    if mn in ("push", "pop"):
        if not X86_GPR64.match(dst):
            raise Refuse(f"push/pop of other than a 64-bit general register: {what}")
        if mn == "push":
            d = st.d + 8
            saved = st.saved | {(d - 7, d, "fp")} if dst == "rbp" else st.saved
            return st.but(d=d, saved=saved), d
        slot = (st.d - 7, st.d, "fp")
        if slot in st.saved and dst != "rbp":
            raise Refuse(f"a saved frame pointer popped into {dst}: {what}")
        check_bytes(st, st.d - 7, st.d, what, allowed=(slot,))
        return st.but(d=st.d - 8, fp=None if dst == "rbp" else st.fp, saved=st.saved - {slot}), None
    if mn == "leave":
        if st.fp is None:
            raise Refuse(f"leave with an unknown frame pointer: {what}")
        slot = (st.fp - 7, st.fp, "fp")
        check_bytes(st, st.fp - 7, st.fp, what, allowed=(slot,))
        return st.but(d=st.fp - 8, fp=None, saved=st.saved - {slot}), None
    if mn in ("sub", "add") and dst == "rsp":
        n = imm(args[1])
        return st.but(d=st.d + n if mn == "sub" else st.d - n), deepest
    if (mn, ops) == ("mov", "rbp, rsp"):
        return st.but(fp=st.d), deepest
    if (mn, ops) == ("mov", "rsp, rbp"):
        if st.fp is None:
            raise Refuse(f"mov rsp, rbp with an unknown frame pointer: {what}")
        return st.but(d=st.fp), deepest
    if dst in X86_SP:
        raise Refuse(f"unmodelled stack-pointer write: {what}")
    if dst in X86_FP:
        return st.but(fp=None), deepest
    return st, deepest


A64_SP = ("sp", "wsp")
A64_FP = ("x29", "w29")
A64_LR = ("x30", "w30")
A64_REGSIZE = {"x": 8, "w": 4, "q": 16, "d": 8, "s": 4, "h": 2, "b": 1}


def a64_inst(mn, ops, st):
    """(new state, deepest byte touched or None) after a non-control AArch64 instruction. Fails
    closed as x86_inst does, and on: writeback on x29; a register offset from sp or x29; sp or
    x29 stored or read as a value other than the frame record (`stp x29, x30` / `ldp x29, x30`
    with base sp) and the modelled frame setup; a write to x30 other than the frame-record
    restore."""
    what = f"{mn} {ops}"
    args = [a.strip() for a in re.split(r",(?![^\[]*\])", ops)] if ops else []
    first_mem = next((i for i, a in enumerate(args) if a.startswith("[")), len(args))
    data = args[:first_mem]
    is_load, is_store = mn.startswith("ld"), mn.startswith("st")
    record = mn in ("stp", "ldp") and data == ["x29", "x30"]
    d, deepest, saved, fp, lr = st.d, None, st.saved, st.fp, st.lr
    if first_mem < len(args):
        mem = args[first_mem]
        post = args[first_mem + 1] if first_mem + 1 < len(args) else None
        m = re.fullmatch(r"\[(\w+)(?:,\s*(#?-?(?:0x)?[0-9a-f]+))?\](!?)", mem)
        if not m:
            if re.search(r"\b(sp|x29|w29|wsp)\b", mem):
                raise Refuse(f"register-offset stack address: {what}")
            base = None
        else:
            base, off, pre = m.group(1), m.group(2), m.group(3) == "!"
        if base in A64_FP and (pre or post is not None):
            raise Refuse(f"writeback on the frame pointer: {what}")
        if base in ("sp", "x29"):
            if mn in ("ldrb", "strb", "ldrsb", "ldurb", "sturb", "ldursb"):
                size = 1
            elif mn in ("ldrh", "strh", "ldrsh", "ldurh", "sturh", "ldursh"):
                size = 2
            elif mn in ("ldrsw", "ldursw", "ldpsw"):
                size = 4 * len(data)
            else:
                if not data or data[0][0] not in A64_REGSIZE:
                    raise Refuse(f"stack access of unknown size: {what}")
                size = A64_REGSIZE[data[0][0]] * len(data)
            k = imm(off) if off else 0
            if base == "x29":
                if fp is None:
                    raise Refuse(f"frame-relative address with an unknown frame pointer: {what}")
                top = fp - k
            elif pre:
                d = d - k
                top = d
            else:
                top = d - k
            if post is not None:
                if base != "sp":
                    raise Refuse(f"unmodelled writeback: {what}")
                top = st.d
                d = d - imm(post)
            lo = top - size + 1
            if record and base == "sp":
                # x29 at the lower address (the deeper half), x30 above it.
                rec = ((lo + 8, lo + 15, "fp"), (lo, lo + 7, ("lr", lr)))
                if is_store:
                    check_bytes(st, lo, top, what)
                    saved = saved | set(rec)
                else:
                    lr_slot = next((s for s in saved if s[:2] == (lo, lo + 7)), None)
                    check_bytes(st, lo, top, what, allowed=tuple(s for s in saved if s[0] >= lo and s[1] <= top))
                    lr = bool(lr_slot and lr_slot[2] == ("lr", True))
                    saved = frozenset(s for s in saved if not (s[0] >= lo and s[1] <= top))
                    fp = None
            else:
                check_bytes(st, lo, top, what)
            deepest = top
        elif base is not None and (pre or post is not None) and base in A64_SP:
            raise Refuse(f"unmodelled stack writeback: {what}")
    if is_store and not record and any(a in A64_SP + A64_FP for a in data):
        raise Refuse(f"stack or frame pointer stored as a value: {what}")
    if is_load:
        if any(a in A64_SP for a in data):
            raise Refuse(f"stack pointer loaded: {what}")
        if any(a in A64_LR for a in data) and not record:
            raise Refuse(f"link register overwritten: {what}")
        if any(a in A64_FP for a in data):
            fp = None
        return st.but(d=d, fp=fp, saved=frozenset(saved), lr=lr), deepest
    if is_store:
        return st.but(d=d, fp=fp, saved=frozenset(saved), lr=lr), deepest
    # Not a memory instruction.
    dst, srcs = (args[0] if args else ""), args[1:]
    if dst in A64_SP:
        if mn in ("sub", "add") and len(args) == 3 and args[1] == "sp" and args[2].startswith("#"):
            n = imm(args[2])
            return st.but(d=d + n if mn == "sub" else d - n), None
        if (mn, args[1:]) == ("mov", ["x29"]):
            if fp is None:
                raise Refuse(f"mov sp, x29 with an unknown frame pointer: {what}")
            return st.but(d=fp), None
        if mn in ("sub", "add") and len(args) == 3 and args[1] == "x29" and args[2].startswith("#"):
            if fp is None:
                raise Refuse(f"sp from an unknown frame pointer: {what}")
            n = imm(args[2])
            return st.but(d=fp + n if mn == "sub" else fp - n), None
        raise Refuse(f"unmodelled stack-pointer write: {what}")
    if dst in A64_FP:
        if (mn, srcs) == ("mov", ["sp"]):
            return st.but(fp=d), None
        if mn == "add" and len(args) == 3 and args[1] == "sp" and args[2].startswith("#"):
            return st.but(fp=d - imm(args[2])), None
        if any(a in A64_SP + A64_FP for a in srcs):
            raise Refuse(f"stack or frame pointer used as a value: {what}")
        return st.but(fp=None), None
    if dst in A64_LR:
        raise Refuse(f"link register overwritten: {what}")
    if any(a in A64_SP + A64_FP for a in srcs):
        raise Refuse(f"stack or frame pointer used as a value: {what}")
    return st, None


X86_JCC = re.compile(r"^j(?!mp$)[a-z]+$")
A64_COND = re.compile(r"^b\.[a-z]+$")

# The data-processing instructions whose only effect on the stack is what x86_inst and a64_inst
# model (memory operands, and writes to the stack, frame and link registers). Any other
# instruction on a reachable path is refused: an unknown one may move the stack pointer or
# transfer control (x86 `loop`, a `rep` prefix), so the analysis never treats it as a fallthrough.
X86_PLAIN = re.compile(
    r"^(mov|movabs|movzx|movsx|movsxd|lea|add|sub|adc|sbb|imul|mul|and|or|xor|not|neg|inc|dec"
    r"|cmp|test|shl|shr|sar|sal|rol|ror|cdq|cqo|cdqe|cwde|cbw|cwd|nop|endbr64"
    r"|push|pop|leave|set[a-z]+|cmov[a-z]+)$")
A64_PLAIN = re.compile(
    r"^(mov|movz|movk|movn|add|adds|sub|subs|neg|negs|mul|madd|msub|mneg|smull|umull|smulh|umulh"
    r"|smaddl|umaddl|sdiv|udiv|and|ands|orr|orn|eor|eon|bic|bics|mvn|lsl|lsr|asr|ror|lslv|lsrv"
    r"|asrv|cmp|cmn|tst|ccmp|ccmn|cset|csetm|csel|csinc|csinv|csneg|cinc|cneg|sxtb|sxth|sxtw"
    r"|uxtb|uxth|ubfx|sbfx|ubfm|sbfm|bfi|bfxil|extr|adr|adrp|nop"
    r"|ldr|ldrb|ldrh|ldrsb|ldrsh|ldrsw|ldur|ldurb|ldurh|ldursb|ldursh|ldursw"
    r"|str|strb|strh|stur|sturb|sturh|ldp|stp|ldpsw)$")


def control(mn, ops):
    """(kind, target) for a control instruction, or (None, None)."""
    if ARCH == "x86_64":
        if mn == "call":
            return "call", ops
        if mn == "jmp":
            return "jmp", ops
        if X86_JCC.match(mn):
            return "jcc", ops
        if mn == "ret":
            if ops:
                return "indirect", ops
            return "ret", None
        if mn in ("ud2", "hlt", "int3"):
            return "stop", None
        return None, None
    if mn == "bl":
        return "call", ops
    if mn == "b":
        return "jmp", ops
    if A64_COND.match(mn) or mn in ("cbz", "cbnz", "tbz", "tbnz"):
        return "jcc", ops
    if mn in ("blr", "br", "braa", "blraa", "braaz", "blraaz"):
        return "indirect", ops
    if mn == "ret" and not ops:
        return "ret", None
    if mn in ("ret", "retaa", "retab"):
        return "indirect", ops
    if mn in ("brk", "udf", "hlt"):
        return "stop", None
    return None, None


def analyse(funcs, stubs):
    """(bound, externals, owned): main's bound, each external's deepest entry depth, both from
    main's entry, and the owned functions' names."""
    owned = {owned_name(n): n for n in funcs if owned_name(n)}
    if "main" not in owned:
        raise Refuse("no main")
    memo, active = {}, set()
    ret_push = 8 if ARCH == "x86_64" else 0
    inst = x86_inst if ARCH == "x86_64" else a64_inst
    plain = X86_PLAIN if ARCH == "x86_64" else A64_PLAIN

    def add_ext(ext, sub, base):
        for name, depth in sub.items():
            ext[name] = max(ext.get(name, 0), base + depth)

    def fn_bound(key):
        """(deepest, externals) of owned function `key`, relative to its own entry."""
        if key in memo:
            return memo[key]
        if key in active:
            raise Refuse(f"recursion through {key}")
        active.add(key)
        code = funcs[owned[key]]
        if not code:
            raise Refuse(f"{key}: no instructions")
        index = {a: i for i, (a, _, _) in enumerate(code)}
        seen, work, deepest, ext = {}, [(0, St())], 0, {}
        while work:
            i, st = work.pop()
            while True:
                if i >= len(code):
                    raise Refuse(f"{key}: runs off its end")
                addr, mn, ops = code[i]
                if addr in seen:
                    if seen[addr] != st.key():
                        raise Refuse(f"{key}+{addr:#x}: states {seen[addr]} and {st.key()} meet")
                    break
                seen[addr] = st.key()
                kind, arg = control(mn, ops)
                if kind is None:
                    if not plain.match(mn):
                        raise Refuse(f"{key}+{addr:#x}: unmodelled instruction: {mn} {ops}")
                    try:
                        st, touched = inst(mn, ops, st)
                    except Refuse as e:
                        raise Refuse(f"{key}+{addr:#x}: {e}") from None
                    if st.d < 0:
                        raise Refuse(f"{key}+{addr:#x}: stack above the entry")
                    deepest = max(deepest, st.d, touched or 0)
                    i += 1
                    continue
                if kind == "indirect":
                    raise Refuse(f"{key}+{addr:#x}: indirect branch {mn} {ops}")
                if kind == "ret":
                    if st.d != 0:
                        raise Refuse(f"{key}+{addr:#x}: return at depth {st.d}")
                    if not st.lr:
                        raise Refuse(f"{key}+{addr:#x}: return through a link register not restored")
                    break
                if kind == "stop":
                    break
                tgt, sym = target_of(arg)
                if tgt is None:
                    raise Refuse(f"{key}+{addr:#x}: indirect {mn} {ops}")
                if kind == "call":
                    base = sym.split("+")[0]
                    callee = owned_name(base)
                    d = st.d
                    if callee and "+" not in sym:
                        sub_deep, sub_ext = fn_bound(callee)
                        deepest = max(deepest, d + ret_push + sub_deep)
                        add_ext(ext, sub_ext, d + ret_push)
                    elif base in stubs or base.endswith("@plt"):
                        # ELF `write@plt`; Mach-O stubs carry one extra leading underscore.
                        name = base.removesuffix("@plt") if base.endswith("@plt") else base[1:]
                        add_ext(ext, {name: 0}, d + ret_push)
                        deepest = max(deepest, d + ret_push)
                        if name in NORETURN:
                            break
                    else:
                        raise Refuse(f"{key}+{addr:#x}: call to {sym}, neither owned nor a stub")
                    if ARCH == "aarch64":
                        st = st.but(lr=False)  # bl wrote x30: this function's return address is gone
                    i += 1
                    continue
                if tgt not in index:
                    raise Refuse(f"{key}+{addr:#x}: branch leaves the function: {mn} {ops}")
                if kind == "jmp":
                    i = index[tgt]
                    continue
                work.append((index[tgt], st))
                i += 1
        active.discard(key)
        memo[key] = (deepest, ext)
        return memo[key]

    total, ext = fn_bound("main")
    return total, ext, owned


if __name__ == "__main__":
    if len(sys.argv) not in (3, 4) or sys.argv[1] not in ("x86_64", "aarch64"):
        sys.exit("usage: stack_bound.py x86_64|aarch64 DISASM [--ranges]")
    ARCH = sys.argv[1]
    try:
        funcs, stubs = parse(sys.argv[2])
        total, ext, owned = analyse(funcs, stubs)
    except Refuse as e:
        print(f"refused: {e}")
        sys.exit(1)
    print(f"bound {total}")
    for name, depth in sorted(ext.items()):
        print(f"external {name} {depth}")
    if len(sys.argv) == 4 and sys.argv[3] == "--ranges":
        for key, name in sorted(owned.items()):
            code = funcs[name]
            print(f"range {key} {code[0][0]:#x} {code[-1][0]:#x}")

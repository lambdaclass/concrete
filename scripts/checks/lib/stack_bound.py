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


def x86_step(mn, ops, d, fp):
    """New (d, fp) after a non-control x86 instruction."""
    args = [a.strip() for a in ops.split(",")] if ops else []
    dst = args[0] if args else ""
    if mn == "push":
        return d + 8, fp
    if mn == "pop":
        return d - 8, (None if dst == "rbp" else fp)
    if mn == "leave":
        if fp is None:
            raise Refuse("leave with an unknown frame pointer")
        return fp - 8, None
    if mn in ("sub", "add") and dst == "rsp":
        n = imm(args[1])
        return (d + n if mn == "sub" else d - n), fp
    if mn == "mov" and dst == "rbp" and args[1:] == ["rsp"]:
        return d, d
    if mn == "mov" and dst == "rsp" and args[1:] == ["rbp"]:
        if fp is None:
            raise Refuse("mov rsp, rbp with an unknown frame pointer")
        return fp, fp
    if mn in ("enter", "pushf", "pushfq", "popf", "popfq") or dst in ("rsp", "esp", "sp", "spl"):
        raise Refuse(f"unmodelled stack-pointer write: {mn} {ops}")
    if dst in ("rbp", "ebp"):
        return d, None
    return d, fp


MEM = re.compile(r"\[([^\]]*)\]")
X86_STACK = ("rsp", "rbp", "esp", "ebp")
# x86 instructions whose stack-register operands the depth tracking itself models.
X86_MODELLED = {("push", "rbp"), ("pop", "rbp"), ("mov", "rbp, rsp"), ("mov", "rsp, rbp"),
                ("leave", "")}


def x86_access(mn, ops, d, fp):
    """The deepest byte an x86 instruction touches or addresses through the stack or frame
    pointer (a memory operand or `lea`), or None. The red zone below the stack pointer is used
    without moving it, so this, not only the stack pointer, is what the bound must cover.
    Refuses an indexed stack address and a stack or frame pointer copied out as a value."""
    deepest = None
    for inner in MEM.findall(ops):
        terms = re.findall(r"[+-]?\s*[^+\-\s]+", inner)
        regs = [t.strip(" +-") for t in terms if not re.fullmatch(r"[+-]?\s*(0x[0-9a-f]+|\d+)", t)]
        base = [r for r in regs if r in X86_STACK]
        if not base:
            continue
        if len(regs) != 1:
            raise Refuse(f"indexed stack address: {mn} {ops}")
        k = sum(int(t.replace(" ", ""), 0) for t in terms if t.strip(" +-") not in regs)
        if base[0] in ("rbp", "ebp"):
            if fp is None:
                raise Refuse(f"frame-relative address with an unknown frame pointer: {mn} {ops}")
            depth = fp - k
        else:
            depth = d - k
        deepest = depth if deepest is None else max(deepest, depth)
    outside = MEM.sub("", ops)
    if (mn, ops) not in X86_MODELLED and not (mn in ("sub", "add") and ops.startswith("rsp,")):
        if any(re.search(rf"\b{r}\b", outside) for r in X86_STACK):
            raise Refuse(f"stack or frame pointer used as a value: {mn} {ops}")
    return deepest


def a64_access(mn, ops, d, fp):
    """As x86_access, for AArch64: `[sp, #k]` and `[x29, #k]` operands (writeback on sp is the
    depth tracking's); refuses a register offset from sp or x29, writeback on x29, and sp or x29
    copied out as a value (other than the frame setup the depth tracking models)."""
    deepest = None
    for inner in MEM.findall(ops):
        parts = [p.strip() for p in inner.split(",")]
        if parts[0] not in ("sp", "x29"):
            continue
        if len(parts) > 2 or (len(parts) == 2 and not parts[1].startswith("#")):
            raise Refuse(f"register-offset stack address: {mn} {ops}")
        k = imm(parts[1]) if len(parts) == 2 else 0
        if parts[0] == "x29":
            if fp is None:
                raise Refuse(f"frame-relative address with an unknown frame pointer: {mn} {ops}")
            depth = fp - k
        else:
            depth = d - k
        deepest = depth if deepest is None else max(deepest, depth)
    args = [a.strip() for a in re.split(r",(?![^\[]*\])", MEM.sub("[]", ops))] if ops else []
    modelled = (args[:1] in (["sp"], ["x29"]) or (mn.startswith(("stp", "ldp")) and "x29" in args[:2])
                or (mn.startswith(("str", "ldr")) and args[:1] == ["x29"]))
    if not modelled and any(a in ("sp", "x29", "wsp", "w29") for a in args[1:]):
        raise Refuse(f"stack or frame pointer used as a value: {mn} {ops}")
    return deepest


SPWB_PRE = re.compile(r"\[sp(?:,\s*#(-?(?:0x)?[0-9a-f]+))?\]!")
SPWB_POST = re.compile(r"\[sp\],\s*#(-?(?:0x)?[0-9a-f]+)")


def a64_step(mn, ops, d, fp):
    """New (d, fp) after a non-control AArch64 instruction."""
    args = [a.strip() for a in re.split(r",(?![^\[]*\])", ops)] if ops else []
    dst = args[0] if args else ""
    if re.search(r"\[x29\],|\[x29[^\]]*\]!", ops):
        raise Refuse(f"writeback on the frame pointer: {mn} {ops}")
    m = SPWB_PRE.search(ops)
    if m:
        d = d - (imm(m.group(1)) if m.group(1) else 0)
    m = SPWB_POST.search(ops)
    if m:
        d = d - imm(m.group(1))
    if SPWB_PRE.search(ops) or SPWB_POST.search(ops):
        return d, (None if "x29" in args[:2] and mn.startswith("ld") else fp)
    if dst in ("sp", "wsp"):
        if mn in ("sub", "add") and len(args) == 3 and args[1] == "sp":
            n = imm(args[2])
            return (d + n if mn == "sub" else d - n), fp
        if mn == "mov" and args[1:] == ["x29"]:
            if fp is None:
                raise Refuse("mov sp, x29 with an unknown frame pointer")
            return fp, fp
        if mn in ("sub", "add") and len(args) == 3 and args[1] == "x29":
            if fp is None:
                raise Refuse("sp from an unknown frame pointer")
            n = imm(args[2])
            return (fp + n if mn == "sub" else fp - n), fp
        raise Refuse(f"unmodelled stack-pointer write: {mn} {ops}")
    if dst in ("x29", "w29"):
        if mn == "mov" and args[1:] == ["sp"]:
            return d, d
        if mn == "add" and len(args) == 3 and args[1] == "sp":
            return d, d - imm(args[2])
        return d, None
    if mn.startswith("ldp") and "x29" in args[:2]:
        return d, None
    return d, fp


X86_JCC = re.compile(r"^j(?!mp$)[a-z]+$")
A64_COND = re.compile(r"^b\.[a-z]+$")

# The data-processing instructions whose only effect on the stack is what x86_access/x86_step and
# a64_access/a64_step model (memory operands, and writes to the stack or frame pointer). Any other
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
    if mn in ("ret", "retaa", "retab"):
        return "ret", None
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
    step = x86_step if ARCH == "x86_64" else a64_step
    access = x86_access if ARCH == "x86_64" else a64_access
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
        seen, work, deepest, ext = {}, [(0, 0, None)], 0, {}
        while work:
            i, d, fp = work.pop()
            while True:
                if i >= len(code):
                    raise Refuse(f"{key}: runs off its end")
                addr, mn, ops = code[i]
                if addr in seen:
                    if seen[addr] != (d, fp):
                        raise Refuse(f"{key}+{addr:#x}: depths {seen[addr]} and {(d, fp)} meet")
                    break
                seen[addr] = (d, fp)
                kind, arg = control(mn, ops)
                if kind is None:
                    if not plain.match(mn):
                        raise Refuse(f"{key}+{addr:#x}: unmodelled instruction: {mn} {ops}")
                    touched = access(mn, ops, d, fp)
                    if touched is not None:
                        deepest = max(deepest, touched)
                    d, fp = step(mn, ops, d, fp)
                    if d < 0:
                        raise Refuse(f"{key}+{addr:#x}: stack above the entry")
                    deepest = max(deepest, d)
                    i += 1
                    continue
                if kind == "indirect":
                    raise Refuse(f"{key}+{addr:#x}: indirect branch {mn} {ops}")
                if kind == "ret":
                    if d != 0:
                        raise Refuse(f"{key}+{addr:#x}: return at depth {d}")
                    break
                if kind == "stop":
                    break
                tgt, sym = target_of(arg)
                if tgt is None:
                    raise Refuse(f"{key}+{addr:#x}: indirect {mn} {ops}")
                if kind == "call":
                    base = sym.split("+")[0]
                    callee = owned_name(base)
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
                    i += 1
                    continue
                if tgt not in index:
                    raise Refuse(f"{key}+{addr:#x}: branch leaves the function: {mn} {ops}")
                if kind == "jmp":
                    i = index[tgt]
                    continue
                work.append((index[tgt], d, fp))
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

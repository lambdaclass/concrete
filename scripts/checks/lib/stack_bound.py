#!/usr/bin/env python3
"""Stack bound of a slice-0 executable, from its disassembly (README gate 12). PROVISIONAL.

usage: stack_bound.py ARCH DISASM             -> prints `bound N` and one `external SYM DEPTH` per call
       stack_bound.py ARCH DISASM --ranges    -> also prints `range SYM LO HI` per Concrete function
ARCH is x86_64 or aarch64; DISASM is `llvm-objdump -d --no-show-raw-insn` output of the LINKED
executable (x86-64 in Intel syntax).

The Concrete-owned code is `main` (the runtime entry), `concrete_rt_write_all` and the body
(`concrete_body`), with or without the Mach-O underscore. Each owned function is analysed over its
control-flow graph, from its entry, as a dataflow fixpoint over an abstract state (St): the stack
pointer's depth below the entry stack pointer, the frame pointer's value, the saved slots and the
link register (equal on every path into an instruction), and the stack bytes the function has
written (the intersection over those paths). A byte at depth n lies n bytes below the entry stack
pointer.

The bound is the deepest byte the function's stack pointer reaches or any of its instructions
addresses through the stack or frame pointer (so the red zone, used below the stack pointer without
moving it, is covered), with each owned callee's own bound added at its call, and each external
function (libc's `write`, `_exit`, `abort`) counted at its entry depth.

Why addresses cannot escape this accounting: memory is addressed only through the stack or frame
pointer (accounted), PC-relatively within a fixed reach of the code (x86 `rip` with a displacement
up to PC_REACH; on AArch64 a register holding an adrp/adr result within PC_REACH, plus at most one
add immediate, on every path since the last call, with a bounded offset and no writeback) or
through the x86 thread-pointer segments. Any other register may hold a stack address (main's argv
and envp, a value an external function left), so no access goes through one. The analysis also refuses taking a stack address as a value (`lea` of one, the stack or frame
pointer copied, stored or used in arithmetic, except the modelled frame setup), so only the frame
save (`push rbp`, `stp x29, x30`) puts one in memory. Saved slots may be read only
by the modelled restore (`pop rbp`, `leave`, `ldp x29, x30` with base sp). Any other stack read
must be of bytes the function itself wrote, on every path, since its last call: so no stale value
(a callee's or the caller's saved frame pointer, libc's scratch) is ever loaded. Accesses must lie
below the entry stack pointer (never the return address or the caller's frame), returns must be at
depth 0 with the caller's frame pointer restored and, on AArch64, with the link register holding
the return address.

The premises (outside the bound, listed in the gate's report): the stack lies farther than
PC_REACH + 4095 + A64_OFF_REACH bytes from the image's code, and the thread-pointer block (fs/gs)
is not on the stack; the external functions' own stack use; that `_exit` and `abort` do not return (a path ends at a call to one); that external
functions follow the ABI's callee-saved rule (the frame pointer and stack pointer preserved).

Fail closed: an instruction outside the modelled set (X86_PLAIN, A64_PLAIN and the control
transfers), any of the refusals above, an unmodelled stack-pointer write, an indirect branch, call
or return, a branch leaving its function, two different states meeting, recursion, or a call to a
function that is neither Concrete-owned nor a PLT/stub symbol is an error (exit 1), never a bound.
"""
import re
import sys

OWNED = ("main", "concrete_body", "concrete_rt_write_all")
# External functions that do not return (part of the libc premise): a path ends at a call to one.
NORETURN = ("exit", "_exit", "_Exit", "abort")
ENTRY = "entry"


class Refuse(Exception):
    pass


def owned_name(sym):
    s = sym[1:] if sym.startswith("_") and sym[1:] in OWNED else sym
    return s if s in OWNED else None


def parse(path):
    """Functions: name -> list of (addr, mnemonic, operands). Also the stubs: ELF PLT symbol names
    (`write@plt`), and Mach-O stub entries, address -> imported symbol, from `stub ADDR SYMBOL`
    lines the gate appends (from `llvm-objdump --macho --indirect-symbols`, since the Mach-O
    disassembly names only the whole `__stubs` section)."""
    funcs, cur, stubs, section = {}, None, {"names": set(), "addrs": {}}, ""
    head = re.compile(r"^([0-9a-f]+) <([^>]+)>:$")
    inst = re.compile(r"^\s*([0-9a-f]+):\s*(\S+)\s*(.*)$")
    stub = re.compile(r"^stub 0x([0-9a-f]+) (\S+)$")
    for line in open(path, encoding="utf-8", errors="replace"):
        line = line.rstrip("\n")
        m = stub.match(line)
        if m:
            addr = int(m.group(1), 16)
            if addr in stubs["addrs"]:
                raise Refuse(f"two stub entries at {addr:#x}")
            stubs["addrs"][addr] = m.group(2)
            cur = None
            continue
        if line.startswith("Disassembly of section"):
            section = line.split()[-1].rstrip(":").split(",")[-1]
            cur = None
            continue
        m = head.match(line)
        if m:
            name = m.group(2)
            if section in (".plt", ".plt.sec", ".plt.got", "__stubs") or name.endswith("@plt"):
                stubs["names"].add(name)
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
    m = re.search(r"0x([0-9a-f]+)\s*<([^>]+)>", ops) or re.search(r"\b([0-9a-f]+)\s*<([^>]+)>", ops)
    return (int(m.group(1), 16), m.group(2)) if m else (None, None)


def imm(s):
    t = s.strip().lstrip("#")
    try:
        return int(t, 16) if t.lower().startswith(("0x", "-0x")) else int(t)
    except ValueError:
        raise Refuse(f"not an immediate: {s}") from None


class St:
    """`d`: the stack pointer's depth. `fp`: the frame pointer's value: a depth (int), ENTRY (the
    caller's, which every return must restore) or None (unknown). `saved`: slots holding a saved
    frame pointer or link register, (lowest, highest, tag) in byte depths, tag ("fp", value) or
    ("lr", whether it is this function's return address); kept after the stack pointer moves past
    them and after a restore (memory keeps them) until an exact overwrite. `lr`: whether the
    AArch64 link register holds this function's return address on every path (not part of the
    key; joins take the conjunction). `w`: the stack bytes this
    function has written since its last call, on every path (not part of the equality key). `g`
    (AArch64): the registers holding a PC-relative address (adrp/adr, plus an immediate), on every
    path since the last call (not part of the key): the only registers memory may be addressed
    through besides the stack and frame pointers."""

    __slots__ = ("d", "fp", "saved", "lr", "w", "g")

    def __init__(self, d=0, fp=ENTRY, saved=frozenset(), lr=True, w=frozenset(), g=frozenset()):
        self.d, self.fp, self.saved, self.lr, self.w, self.g = d, fp, saved, lr, w, g

    def key(self):
        return (self.d, self.fp, self.saved)

    def but(self, **kw):
        s = St(self.d, self.fp, self.saved, self.lr, self.w, self.g)
        for k, v in kw.items():
            setattr(s, k, v)
        return s


def frame_depth(fp, what):
    """The frame pointer as a depth, for addressing; refused unless it is this frame's."""
    if not isinstance(fp, int):
        raise Refuse(f"frame-relative use of a frame pointer that is not this frame's: {what}")
    return fp


def slot_at(saved, lo, hi):
    return next((x for x in saved if x[:2] == (lo, hi)), None)


def touch(st, lo, hi, mode, what, restore=()):
    """Check an access of the bytes [lo, hi] (depths) in `mode` ("r", "w" or "rw") and return the
    new written set. The bytes must be the function's own (depth >= 1). A saved slot may be read
    only by a restore (`restore`: the slots being restored), and written only by an exact
    overwrite (which clears it; the caller removes it from `saved`). A read needs every byte
    written by this function on every path since its last call."""
    if lo < 1:
        raise Refuse(f"touches the return address or the caller's frame: {what}")
    for slot in st.saved:
        if slot[0] <= hi and lo <= slot[1] and slot not in restore:
            exact = (slot[0], slot[1]) == (lo, hi) or (lo <= slot[0] and slot[1] <= hi and mode == "w")
            if "r" in mode or not exact:
                raise Refuse(f"touches a saved frame pointer or link register: {what}")
    if "r" in mode and not all(b in st.w for b in range(lo, hi + 1)):
        raise Refuse(f"reads stack bytes this function has not written: {what}")
    return st.w | frozenset(range(lo, hi + 1)) if "w" in mode else st.w


def cleared(saved, lo, hi):
    """`saved` without the slots a write of [lo, hi] overwrote."""
    return frozenset(x for x in saved if not (lo <= x[0] and x[1] <= hi))


X86_SP = ("rsp", "esp", "sp", "spl")
X86_FP = ("rbp", "ebp", "bp", "bpl")
X86_STACK = X86_SP + X86_FP
X86_GPR64 = re.compile(r"^(r[abcd]x|r[sd]i|rbp|r(?:8|9|1[0-5]))$")
X86_SIZE = {"byte": 1, "word": 2, "dword": 4, "qword": 8, "tbyte": 10, "xmmword": 16,
            "ymmword": 32, "zmmword": 64}
MEM = re.compile(r"\[([^\]]*)\]")


# PC-relative addressing reaches at most PC_REACH bytes from the instruction (x86 rip
# displacement; AArch64 adrp/adr target) plus, on AArch64, one add immediate (at most 4095) and
# the access's own offset (at most A64_OFF_REACH): the premise that the stack lies farther than
# that from the image's code makes such an access never a stack access.
PC_REACH = 16 << 20
A64_OFF_REACH = 32760


def x86_inst(mn, ops, st, addr=0):
    """(new state, deepest byte touched or None) after a non-control x86 instruction."""
    what = f"{mn} {ops}"
    args = [a.strip() for a in ops.split(",")] if ops else []
    dst = args[0] if args else ""
    deepest, w, saved = None, st.w, st.saved
    for m in MEM.finditer(ops):
        terms = re.findall(r"[+-]?\s*[^+\-\s]+", m.group(1))
        regs = [t.strip(" +-") for t in terms if not re.fullmatch(r"[+-]?\s*(0x[0-9a-f]+|\d+)", t)]
        base = [r for r in regs if r in X86_STACK]
        if not base:
            # Any other register may hold a stack address (main's argv and envp, a value an
            # external function left), so memory is addressed only PC-relative or absolutely
            # (a segment offset: the thread pointer's block).
            k = sum(int(t.replace(" ", ""), 0) for t in terms if t.strip(" +-") not in regs)
            if regs == ["rip"]:
                if abs(k) > PC_REACH:
                    raise Refuse(f"PC-relative access beyond the premise's reach: {what}")
            elif regs or not re.search(r"\b[fg]s:\s*$", ops[:m.start()]):
                raise Refuse(f"memory addressed through a register that may hold a stack address, "
                             f"or absolutely: {what}")
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
        top = (frame_depth(st.fp, what) if base[0] == "rbp" else st.d) - k
        lo = top - X86_SIZE[size.group(1)] + 1
        # The memory operand is the destination when it comes first: written only by mov and
        # setcc, read and written by the others, read only by cmp and test.
        first = ops.lstrip().find("[") < ops.find(",") if "," in ops else True
        if not first or mn in ("cmp", "test"):
            mode = "r"
        elif mn in ("mov", "movabs") or mn.startswith("set"):
            mode = "w"
        else:
            mode = "rw"
        w = touch(st.but(w=w, saved=saved), lo, top, mode, what)
        if "w" in mode:
            saved = cleared(saved, lo, top)
        deepest = top if deepest is None else max(deepest, top)
    st = st.but(w=w, saved=saved)
    outside = MEM.sub("", ops)
    modelled = (mn, ops) in (("push", "rbp"), ("pop", "rbp"), ("mov", "rbp, rsp"),
                             ("mov", "rsp, rbp"), ("leave", "")) or (
        mn in ("sub", "add") and re.fullmatch(r"rsp, (0x[0-9a-f]+|\d+)", ops))
    if not modelled and any(re.search(rf"\b{r}\b", outside) for r in X86_STACK):
        raise Refuse(f"stack or frame pointer used as a value: {what}")
    if mn == "push":
        if not X86_GPR64.match(dst):
            raise Refuse(f"push of other than a 64-bit general register: {what}")
        d = st.d + 8
        w = touch(st, d - 7, d, "w", what)
        saved = cleared(st.saved, d - 7, d)
        if dst == "rbp":
            saved = saved | {(d - 7, d, ("fp", st.fp))}
        return st.but(d=d, saved=saved, w=w), d
    if mn == "pop":
        if not X86_GPR64.match(dst):
            raise Refuse(f"pop of other than a 64-bit general register: {what}")
        return x86_pop(st, dst, st.d, what), None
    if mn == "leave":
        return x86_pop(st, "rbp", frame_depth(st.fp, what), what), None
    if mn in ("sub", "add") and dst == "rsp":
        n = imm(args[1])
        return st.but(d=st.d + n if mn == "sub" else st.d - n), deepest
    if (mn, ops) == ("mov", "rbp, rsp"):
        return st.but(fp=st.d), deepest
    if (mn, ops) == ("mov", "rsp, rbp"):
        return st.but(d=frame_depth(st.fp, what)), deepest
    if dst in X86_SP:
        raise Refuse(f"unmodelled stack-pointer write: {what}")
    if dst in X86_FP:
        return st.but(fp=None), deepest
    return st, deepest


def x86_pop(st, dst, at, what):
    """Pop the 8 bytes whose deepest is at depth `at` into `dst`: a saved frame pointer only into
    rbp, which then holds the saved value again (any other pop into rbp makes it unknown)."""
    slot = slot_at(st.saved, at - 7, at)
    if slot and dst != "rbp":
        raise Refuse(f"a saved frame pointer popped into {dst}: {what}")
    touch(st, at - 7, at, "r", what, restore=(slot,) if slot else ())
    if dst != "rbp":
        return st.but(d=at - 8)
    return st.but(d=at - 8, fp=slot[2][1] if slot and slot[2][0] == "fp" else None)


A64_SP = ("sp", "wsp")
A64_FP = ("x29", "w29")
A64_LR = ("x30", "w30")
A64_REGSIZE = {"x": 8, "w": 4, "q": 16, "d": 8, "s": 4, "h": 2, "b": 1}


def a64_size(mn, data, what):
    if mn in ("ldrb", "strb", "ldrsb", "ldurb", "sturb", "ldursb"):
        return 1
    if mn in ("ldrh", "strh", "ldrsh", "ldurh", "sturh", "ldursh"):
        return 2
    if mn in ("ldrsw", "ldursw", "ldpsw"):
        return 4 * len(data)
    if not data or data[0][0] not in A64_REGSIZE:
        raise Refuse(f"stack access of unknown size: {what}")
    return A64_REGSIZE[data[0][0]] * len(data)


def a64_reg(r):
    """The 64-bit name of a general register (w8 -> x8)."""
    return "x" + r[1:] if re.fullmatch(r"w([0-9]|[12][0-9]|30)", r) else r


A64_FLAGS_ONLY = ("cmp", "cmn", "tst", "ccmp", "ccmn", "nop")


def a64_inst(mn, ops, st, addr=0):
    """(new state, deepest byte touched or None) after a non-control AArch64 instruction, with `g`
    (the PC-relative address registers, as (register, level) pairs) updated: every register the
    instruction writes leaves it; adrp/adr with a target within PC_REACH of the instruction puts
    its destination in at level 0, and one add of an immediate up to 4095 to a level-0 register
    puts its destination in at level 1 (so the address stays within a fixed reach of the code)."""
    new, deepest = a64_core(mn, ops, st)
    args = [a.strip() for a in re.split(r",(?![^\[]*\])", ops)] if ops else []
    first_mem = next((i for i, a in enumerate(args) if a.startswith("[")), len(args))
    if mn.startswith(("ld", "st")):
        written = list(args[:first_mem]) if mn.startswith("ld") else []
        if first_mem < len(args) and (args[first_mem].endswith("!") or first_mem + 1 < len(args)):
            written.append(re.match(r"\[(\w+)", args[first_mem]).group(1))
    else:
        written = [] if mn in A64_FLAGS_ONLY or not args else [args[0]]
    out = {a64_reg(r) for r in written}
    g = {x for x in new.g if x[0] not in out}
    if mn in ("adrp", "adr") and len(args) == 2 and args[0].startswith("x"):
        t = re.fullmatch(r"(0x[0-9a-f]+)(?:\s*<[^>]*>)?", args[1])
        if t and abs(int(t.group(1), 16) - addr) <= PC_REACH:
            g.add((args[0], 0))
    elif (mn == "add" and len(args) == 3 and args[0].startswith("x") and (args[1], 0) in st.g
          and re.fullmatch(r"#(0x[0-9a-f]+|\d+)", args[2]) and imm(args[2]) <= 4095):
        g.add((args[0], 1))
    return new.but(g=frozenset(g)), deepest


def a64_core(mn, ops, st):
    """(new state, deepest byte touched or None) after a non-control AArch64 instruction."""
    what = f"{mn} {ops}"
    args = [a.strip() for a in re.split(r",(?![^\[]*\])", ops)] if ops else []
    first_mem = next((i for i, a in enumerate(args) if a.startswith("[")), len(args))
    data = args[:first_mem]
    is_load, is_store = mn.startswith("ld"), mn.startswith("st")
    deepest = None
    record = restored = single = False
    if first_mem < len(args):
        mem = args[first_mem]
        post = args[first_mem + 1] if first_mem + 1 < len(args) else None
        m = re.fullmatch(r"\[(\w+)(?:,\s*(#?-?(?:0x)?[0-9a-f]+))?\](!?)", mem)
        if not m:
            raise Refuse(f"unmodelled address form: {what}")
        else:
            base, off, pre = m.group(1), m.group(2), m.group(3) == "!"
            if base in A64_FP and (pre or post is not None):
                raise Refuse(f"writeback on the frame pointer: {what}")
            if base in A64_LR and (pre or post is not None):
                raise Refuse(f"writeback on the link register: {what}")
            if base not in ("sp", "x29") + A64_SP + A64_FP:
                # Any other register may hold a stack address (main's argv, envp and apple
                # vector, a value an external function left): only a PC-relative one is used.
                if base not in {x[0] for x in st.g}:
                    raise Refuse(f"memory addressed through a register that may hold a stack address: {what}")
                if pre or post is not None or (off and abs(imm(off)) > A64_OFF_REACH):
                    raise Refuse(f"PC-relative access beyond the premise's reach: {what}")
        if base in ("sp", "x29"):
            record = mn in ("stp", "ldp") and data == ["x29", "x30"] and base == "sp"
            size = a64_size(mn, data, what)
            k = imm(off) if off else 0
            d = st.d
            if base == "x29":
                top = frame_depth(st.fp, what) - k
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
            saved, fp, lr = st.saved, st.fp, st.lr
            if record and is_store:
                # The frame record: x29 at the lower address (the deeper half), x30 above it.
                w = touch(st, lo, top, "w", what)
                saved = cleared(saved, lo, top) | {(lo + 8, lo + 15, ("fp", fp)), (lo, lo + 7, ("lr", lr))}
            elif record:
                fp_slot, lr_slot = slot_at(saved, lo + 8, lo + 15), slot_at(saved, lo, lo + 7)
                w = touch(st, lo, top, "r", what, restore=tuple(x for x in (fp_slot, lr_slot) if x))
                lr = bool(lr_slot and lr_slot[2] == ("lr", True))
                fp = fp_slot[2][1] if fp_slot and fp_slot[2][0] == "fp" else None
                restored = True
            elif data in (["x30"], ["x29"]) and top - lo == 7:
                # A single 8-byte save or restore of the link register or frame pointer (LCB's
                # `str x30, [sp, #k]`): a saved slot, as in the frame record.
                tag = "lr" if data == ["x30"] else "fp"
                if is_store:
                    w = touch(st, lo, top, "w", what)
                    saved = cleared(saved, lo, top) | {(lo, top, (tag, lr if tag == "lr" else fp))}
                    single = True
                elif is_load:
                    slot = slot_at(saved, lo, top)
                    w = touch(st, lo, top, "r", what, restore=(slot,) if slot else ())
                    if not slot or slot[2][0] != tag:
                        raise Refuse(f"{data[0]} loaded from other than its saved slot: {what}")
                    if tag == "lr":
                        lr = slot[2][1]
                    else:
                        fp = slot[2][1]
                    restored = True
                else:
                    raise Refuse(f"unmodelled stack memory instruction: {what}")
            elif is_load or is_store:
                w = touch(st, lo, top, "r" if is_load else "w", what)
                if is_store:
                    saved = cleared(saved, lo, top)
            else:
                raise Refuse(f"unmodelled stack memory instruction: {what}")
            st = st.but(d=d, saved=saved, fp=fp, lr=lr, w=w)
            deepest = top
        elif base in A64_SP + A64_FP:
            raise Refuse(f"unmodelled stack address: {what}")
    if is_store:
        if not (record or single) and any(a in A64_SP + A64_FP for a in data):
            raise Refuse(f"stack or frame pointer stored as a value: {what}")
        return st, deepest
    if is_load:
        if any(a in A64_SP for a in data):
            raise Refuse(f"stack pointer loaded: {what}")
        if any(a in A64_LR for a in data) and not (record or restored):
            raise Refuse(f"link register overwritten: {what}")
        if any(a in A64_FP for a in data) and not restored:
            st = st.but(fp=None)
        return st, deepest
    # Not a memory instruction.
    dst, srcs = (args[0] if args else ""), args[1:]
    if dst in A64_SP:
        if mn in ("sub", "add") and len(args) == 3 and args[1] == "sp" and args[2].startswith("#"):
            n = imm(args[2])
            return st.but(d=st.d + n if mn == "sub" else st.d - n), None
        if (mn, srcs) == ("mov", ["x29"]):
            return st.but(d=frame_depth(st.fp, what)), None
        if mn in ("sub", "add") and len(args) == 3 and args[1] == "x29" and args[2].startswith("#"):
            n, f = imm(args[2]), frame_depth(st.fp, what)
            return st.but(d=f + n if mn == "sub" else f - n), None
        raise Refuse(f"unmodelled stack-pointer write: {what}")
    if dst in A64_FP:
        if (mn, srcs) == ("mov", ["sp"]):
            return st.but(fp=st.d), None
        if mn == "add" and len(args) == 3 and args[1] == "sp" and args[2].startswith("#"):
            return st.but(fp=st.d - imm(args[2])), None
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
            return ("indirect", ops) if ops else ("ret", None)
        if mn in ("ud2", "hlt", "int3"):
            return "stop", None
        return None, None
    if mn == "bl":
        return "call", ops
    if mn == "b":
        return "jmp", ops
    if A64_COND.match(mn) or mn in ("cbz", "cbnz", "tbz", "tbnz"):
        return "jcc", ops
    if mn in ("blr", "br", "braa", "blraa", "braaz", "blraaz", "retaa", "retab"):
        return "indirect", ops
    if mn == "ret":
        return ("indirect", ops) if ops else ("ret", None)
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
        states, work, deepest, ext = {0: St()}, [0], 0, {}

        def flow(j, st, addr):
            """Propagate `st` into instruction j: the keys must agree, the written sets meet."""
            if j >= len(code):
                raise Refuse(f"{key}: runs off its end")
            if j not in states:
                states[j] = st
                work.append(j)
                return
            old = states[j]
            if old.key() != st.key():
                raise Refuse(f"{key}+{addr:#x}: states {old.key()} and {st.key()} meet")
            if not (old.w <= st.w and old.g <= st.g and (st.lr or not old.lr)):
                states[j] = old.but(w=old.w & st.w, g=old.g & st.g, lr=old.lr and st.lr)
                work.append(j)

        while work:
            i = work.pop()
            st = states[i]
            addr, mn, ops = code[i]
            kind, arg = control(mn, ops)
            if kind is None:
                if not plain.match(mn):
                    raise Refuse(f"{key}+{addr:#x}: unmodelled instruction: {mn} {ops}")
                try:
                    st, touched = inst(mn, ops, st, addr)
                except Refuse as e:
                    raise Refuse(f"{key}+{addr:#x}: {e}") from None
                if st.d < 0:
                    raise Refuse(f"{key}+{addr:#x}: stack above the entry")
                deepest = max(deepest, st.d, touched or 0)
                flow(i + 1, st, addr)
                continue
            if kind == "indirect":
                raise Refuse(f"{key}+{addr:#x}: indirect control transfer {mn} {ops}")
            if kind == "ret":
                if st.d != 0:
                    raise Refuse(f"{key}+{addr:#x}: return at depth {st.d}")
                if not st.lr:
                    raise Refuse(f"{key}+{addr:#x}: return through a link register not restored")
                if st.fp != ENTRY:
                    raise Refuse(f"{key}+{addr:#x}: return without the caller's frame pointer restored")
                continue
            if kind == "stop":
                continue
            tgt, sym = target_of(arg)
            if tgt is None:
                raise Refuse(f"{key}+{addr:#x}: indirect {mn} {ops}")
            if kind == "call":
                base = sym.split("+")[0]
                callee, d = owned_name(base), st.d
                if tgt in stubs["addrs"]:
                    # A Mach-O stub entry: the imported symbol carries one leading underscore.
                    imported = stubs["addrs"][tgt]
                    name = imported[1:] if imported.startswith("_") else imported
                    add_ext(ext, {name: 0}, d + ret_push)
                    deepest = max(deepest, d + ret_push)
                    if name in NORETURN:
                        continue
                elif callee and "+" not in sym:
                    sub_deep, sub_ext = fn_bound(callee)
                    deepest = max(deepest, d + ret_push + sub_deep)
                    add_ext(ext, sub_ext, d + ret_push)
                elif base in stubs["names"] and base.endswith("@plt"):
                    # An ELF PLT entry, `write@plt`.
                    name = base.removesuffix("@plt")
                    add_ext(ext, {name: 0}, d + ret_push)
                    deepest = max(deepest, d + ret_push)
                    if name in NORETURN:
                        continue
                else:
                    raise Refuse(f"{key}+{addr:#x}: call to {sym}, neither owned nor a stub")
                # The callee may have written anything below the stack pointer; on AArch64, bl
                # replaced the link register.
                st = st.but(w=frozenset(b for b in st.w if b <= d), g=frozenset(),
                           lr=st.lr if ARCH == "x86_64" else False)
                flow(i + 1, st, addr)
                continue
            if tgt not in index:
                raise Refuse(f"{key}+{addr:#x}: branch leaves the function: {mn} {ops}")
            flow(index[tgt], st, addr)
            if kind == "jcc":
                flow(i + 1, st, addr)
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

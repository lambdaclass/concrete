#!/usr/bin/env python3
"""Stack bound of a slice-0 executable, from its disassembly (README gate 12). PROVISIONAL.

usage: stack_bound.py TARGET DISASM           -> prints `bound N` and one `external SYM DEPTH` per call
       stack_bound.py TARGET DISASM --ranges  -> also prints `redzone N` (the target's) and
                                                `range SYM LO HI` per Concrete function
       stack_bound.py --elf-tls HEADERS_JSON  -> prints `tls_memsz 0xM` from `llvm-readobj
                                                --program-headers --elf-output-style=JSON`
TARGET is a key of TARGETS (x86_64-linux, aarch64-macos, aarch64-linux); DISASM is
`llvm-objdump -d --no-show-raw-insn` output of the LINKED executable (x86-64 in Intel syntax).
For x86-64 the gate appends `tls_memsz 0xM`, the Variant II static-TLS reservation derived by
`--elf-tls` from the linked executable's `llvm-readobj --program-headers --elf-output-style=JSON`
output (requiring at least one PT_LOAD segment and at most one PT_TLS segment); absent, malformed
or duplicate metadata is an error.

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

Why addresses cannot escape this accounting: instructions and operands are parsed by the shared
fail-closed grammar in `stack_access.py`, which consumes every byte of every operand against an
explicit allowlist of mnemonics and operand shapes. Memory is addressed only through the stack or
frame pointer (accounted), PC-relatively within a fixed reach of the code (x86 `rip` with a
displacement up to PC_REACH; on AArch64 a register holding an adrp/adr result within PC_REACH, plus
at most one add immediate, on every path since the last call, with a bounded offset and no
writeback) or through an absolute x86 `fs:` operand whose complete byte interval lies in the
modelled TCB or static-TLS interval. `gs:` is not a thread-pointer base on x86-64 Linux and is
refused outright, as is every register-relative segment override, every other segment override,
any segment-register write or read, and any instruction with two memory operands. Any other
register may hold a stack address (main's argv and envp, a value an external function left), so no
access goes through one. The analysis also refuses taking a stack address as a value (`lea` of one,
the stack or frame pointer copied, stored or used in arithmetic, except the modelled frame setup),
so only the frame save (`push rbp`, `stp x29, x30`) puts one in memory. Saved slots may be read
only by the modelled restore (`pop rbp`, `leave`, `ldp x29, x30` with base sp). Any other stack
read must be of bytes the function itself wrote, on every path, since its last call: so no stale
value (a callee's or the caller's saved frame pointer, libc's scratch) is ever loaded. Accesses
must lie below the entry stack pointer (never the return address or the caller's frame) and at most
the target's red zone below the current stack pointer (TARGETS: 128 bytes on x86-64 System V and on
Apple arm64, none under AAPCS64 on Linux; deeper bytes may be overwritten asynchronously, by a
signal handler, so code using them has no bound), returns must be at depth 0 with the caller's
frame pointer restored and, on AArch64, with the link register holding the return address.

The premises (outside the bound, listed in the gate's report): the stack lies farther than
PC_REACH + 4095 + A64_OFF_REACH bytes from the image's code. On x86-64 Linux, let M be the linked
ELF's Variant II static-TLS reservation below FS.base: zero without PT_TLS or when p_memsz = 0, and
otherwise roundup(p_memsz + ((p_vaddr - p_memsz) & (a - 1)), a) with a = max(p_align, 1). FS.base
is the Variant II thread pointer and remains fixed throughout Concrete-owned execution, including
across external calls. Static TLS occupies offsets [-M, 0), and the x86-64 Linux TCB header through
__private_tm occupies [0, 0x70), relative to FS.base. The runtime-separation obligation is that no
byte of the thread's stack has an address in { (FS.base + o) mod 2^64 | -M <= o < 0x70 }; this
covers every byte of every accepted access and address wrap-around. The other premises are the
external functions' own stack use; that `_exit` and `abort` do not return (a path ends at a call to
one); and that external functions follow the ABI's callee-saved rule (the frame pointer and stack
pointer preserved).

Fail closed: an instruction or operand outside the modelled grammar in `stack_access.py`, any of
the refusals above, an unmodelled stack-pointer write, an indirect branch, call or return, a branch
leaving its function, two different states meeting, recursion, or a call to a function that is
neither Concrete-owned nor a PLT/stub symbol is an error (exit 1), never a bound.
"""
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import stack_access
from stack_access import (
    A64_FLAGS_ONLY,
    A64_FP,
    A64_LR,
    A64_OFF_REACH,
    A64_REGSIZE,
    A64_SP,
    PC_REACH,
    TARGETS,
    TCB_SIZE,
    X86_FP,
    X86_GPR64,
    X86_SP,
    X86_STACK,
    byte_interval,
)

OWNED = ("main", "concrete_body", "concrete_rt_write_all")
# External functions that do not return (part of the libc premise): a path ends at a call to one.
NORETURN = ("exit", "_exit", "_Exit", "abort")
ENTRY = "entry"
TLS_MEMSZ = None


class Refuse(Exception):
    pass


def owned_name(sym):
    s = sym[1:] if sym.startswith("_") and sym[1:] in OWNED else sym
    return s if s in OWNED else None


def elf_tls_line(path):
    """Derive `tls_memsz 0xM` (the Variant II static-TLS reservation M below FS.base) from
    `llvm-readobj --program-headers --elf-output-style=JSON` output. Requires at least one PT_LOAD
    segment and at most one PT_TLS segment."""
    try:
        with open(path, encoding="utf-8") as f:
            data = json.load(f)
    except Exception as e:
        raise Refuse(f"malformed program-header JSON: {e}") from None
    if not isinstance(data, list) or len(data) != 1 or not isinstance(data[0], dict):
        raise Refuse("expected a single ELF object in program-header JSON")
    phdrs = data[0].get("ProgramHeaders")
    if not isinstance(phdrs, list):
        raise Refuse("missing ProgramHeaders list in JSON")
    loads = 0
    tls = None
    for entry in phdrs:
        ph = entry.get("ProgramHeader") if isinstance(entry, dict) else None
        if not isinstance(ph, dict):
            raise Refuse("malformed ProgramHeader entry")
        kind = ph.get("Type", {}).get("Name") if isinstance(ph.get("Type"), dict) else None
        if kind == "PT_LOAD":
            loads += 1
        elif kind == "PT_TLS":
            if tls is not None:
                raise Refuse("multiple PT_TLS program headers")
            tls = ph
    if loads < 1:
        raise Refuse("ELF has no PT_LOAD program header")
    if tls is None:
        return "tls_memsz 0x0"
    memsz, filesz = tls.get("MemSize"), tls.get("FileSize")
    align, vaddr = tls.get("Alignment"), tls.get("VirtualAddress")
    if not all(isinstance(x, int) and not isinstance(x, bool) for x in (memsz, filesz, align, vaddr)):
        raise Refuse("non-integer PT_TLS program-header field")
    if filesz < 0 or filesz > memsz:
        raise Refuse(f"invalid PT_TLS sizes: p_filesz {filesz} > p_memsz {memsz}")
    try:
        res = stack_access.tls_reservation(memsz, align, vaddr)
    except ValueError as e:
        raise Refuse(str(e)) from None
    return f"tls_memsz {res:#x}"


def parse(path):
    """Functions, stubs and the ELF Variant II static-TLS reservation recorded in the disassembly.

    Functions map names to (addr, mnemonic, operands). Stubs are ELF PLT symbol names
    (`write@plt`), and Mach-O stub entries, address -> imported symbol, from `stub ADDR SYMBOL`
    lines the gate appends (from `llvm-objdump --macho --indirect-symbols`, since the Mach-O
    disassembly names only the whole `__stubs` section). On x86 the gate also appends exactly one
    `tls_memsz M [ALIGN VADDR]` line from the linked executable's program headers (zero with no
    PT_TLS)."""
    funcs, cur, stubs, section = {}, None, {"names": set(), "addrs": {}}, ""
    tls_memsz = None
    head = re.compile(r"^([0-9a-f]+) <([^>]+)>:$")
    inst = re.compile(r"^\s*([0-9a-f]+):\s*(\S+)\s*(.*)$")
    stub = re.compile(r"^stub 0x([0-9a-f]+) (\S+)$")
    for line in open(path, encoding="utf-8", errors="replace"):
        line = line.rstrip("\n")
        if line.startswith("tls_memsz"):
            m = re.fullmatch(
                r"tls_memsz (0x[0-9a-f]+|\d+)(?:\s+(0x[0-9a-f]+|\d+)\s+(0x[0-9a-f]+|\d+))?",
                line)
            if not m or tls_memsz is not None:
                raise Refuse(f"malformed or duplicate PT_TLS metadata: {line}")
            memsz = int(m.group(1), 0)
            align = int(m.group(2), 0) if m.group(2) is not None else 1
            vaddr = int(m.group(3), 0) if m.group(3) is not None else 0
            try:
                tls_memsz = stack_access.tls_reservation(memsz, align, vaddr)
            except ValueError as e:
                raise Refuse(f"{e}: {line}") from None
            cur = None
            continue
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
                # Stub code is never analysed. Only an ELF PLT entry's name (`write@plt`) makes a
                # call external; a Mach-O stub needs an exact `stub` address entry.
                if name.endswith("@plt"):
                    stubs["names"].add(name)
                cur = None
            else:
                cur = name
                funcs.setdefault(name, [])
            continue
        m = inst.match(line)
        if m and cur is not None:
            ops = (
                m.group(3).split("#", 1)[0]
                if ARCH == "x86_64"
                else m.group(3).split("//", 1)[0].split(";", 1)[0]
            )
            funcs[cur].append((int(m.group(1), 16), m.group(2).lower(), ops.strip()))
    return funcs, stubs, tls_memsz


def target_of(ops):
    m = re.fullmatch(r"#?(?:0x)?([0-9a-f]+)\s*<([^>\[\]]+)>", ops)
    return (int(m.group(1), 16), m.group(2)) if m else (None, None)


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


def x86_inst(mn, ops, st, addr=0):
    """(new state, deepest byte touched or None) after a non-control x86 instruction."""
    what = f"{mn} {ops}".strip()
    try:
        parsed_ops, memory = stack_access.parse_x86_plain(mn, ops)
    except ValueError as e:
        raise Refuse(str(e)) from None

    deepest, w, saved = None, st.w, st.saved
    for mem_idx, segment, width, _, regs, scaled, k in memory:
        if mn == "lea":
            if any(r in X86_STACK for r in regs):
                raise Refuse(f"stack address taken as a value: {what}")
            elif regs != ["rip"] or scaled:
                raise Refuse(f"LEA through a register that may hold a stack address, or absolutely: {what}")
            lo, _ = byte_interval(k, 1)
            if abs(lo) > PC_REACH:
                raise Refuse(f"PC-relative LEA beyond the premise's reach: {what}")
            continue

        base = [r for r in regs if r in X86_STACK]
        if not base:
            # Any other register may hold a stack address (main's argv and envp, a value an
            # external function left). The complete byte interval, not only its first byte, must
            # lie within the relevant separation premise.
            lo, hi = byte_interval(k, width)
            if segment == "fs":
                in_tcb = 0 <= lo and hi < TCB_SIZE
                in_tls = -TLS_MEMSZ <= lo and hi < 0
                if not (in_tcb or in_tls):
                    raise Refuse(f"thread-pointer-relative access beyond the premise's reach: {what}")
            elif regs == ["rip"] and not scaled:
                if lo < -PC_REACH or hi > PC_REACH:
                    raise Refuse(f"PC-relative access beyond the premise's reach: {what}")
            else:
                raise Refuse(f"memory addressed through a register that may hold a stack address, "
                             f"or absolutely: {what}")
            continue
        if base[0] not in ("rsp", "rbp"):
            raise Refuse(f"stack address through a partial register: {what}")
        if len(regs) != 1 or scaled:
            raise Refuse(f"indexed stack address: {what}")
        top = (frame_depth(st.fp, what) if base[0] == "rbp" else st.d) - k
        lo = top - width + 1
        # The memory operand is the destination when it is the first operand (mem_idx == 0):
        # written only by mov, movabs and setcc, read and written by the others, read only by cmp and test.
        first = mem_idx == 0
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
    dst = parsed_ops[0][1] if parsed_ops and parsed_ops[0][0] == "reg" else ""
    if mn == "push":
        if dst != "rbp" and dst in X86_STACK:
            raise Refuse(f"stack or frame pointer used as a value: {what}")
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
    if (mn in ("sub", "add") and dst == "rsp" and len(parsed_ops) == 2
            and parsed_ops[1][0] == "imm"):
        n = parsed_ops[1][1]
        return st.but(d=st.d + n if mn == "sub" else st.d - n), deepest
    if (mn, ops) == ("mov", "rbp, rsp"):
        return st.but(fp=st.d), deepest
    if (mn, ops) == ("mov", "rsp, rbp"):
        return st.but(d=frame_depth(st.fp, what)), deepest
    srcs = [val for kind, val in (parsed_ops if mn in ("cmp", "test") else parsed_ops[1:])
            if kind == "reg"]
    if any(r in X86_STACK for r in srcs):
        raise Refuse(f"stack or frame pointer used as a value: {what}")
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


def a64_size(mn, data):
    if mn in ("ldrb", "strb", "ldrsb", "ldurb", "sturb", "ldursb"):
        return 1
    if mn in ("ldrh", "strh", "ldrsh", "ldurh", "sturh", "ldursh"):
        return 2
    if mn in ("ldrsw", "ldursw", "ldpsw"):
        return 4 * len(data)
    return A64_REGSIZE[data[0][0]] * len(data)


def a64_reg(r):
    """The 64-bit name of a general register (w8 -> x8, fp -> x29, lr -> x30)."""
    if r == "fp":
        return "x29"
    if r == "lr":
        return "x30"
    return "x" + r[1:] if re.fullmatch(r"w([0-9]|[12][0-9]|30)", r) else r


def a64_inst(mn, ops, st, addr=0):
    """(new state, deepest byte touched or None) after a non-control AArch64 instruction, with `g`
    (the PC-relative address registers, as (register, level) pairs) updated: every register the
    instruction writes leaves it; adrp/adr with a target within PC_REACH of the instruction puts
    its destination in at level 0, and one add of an immediate up to 4095 to a level-0 register
    puts its destination in at level 1 (so the address stays within a fixed reach of the code)."""
    try:
        parsed_ops, mem = stack_access.parse_a64_plain(mn, ops)
    except ValueError as e:
        raise Refuse(str(e)) from None
    new, deepest = a64_core(mn, ops, parsed_ops, mem, st)
    if mem is not None:
        _, data, base, _, _, writeback = mem
        written = list(data) if mn.startswith("ld") else []
        if writeback:
            written.append(base)
    else:
        written = [] if mn in A64_FLAGS_ONLY or not parsed_ops else [parsed_ops[0][1]]
    out = {a64_reg(r) for r in written}
    g = {x for x in new.g if x[0] not in out}
    dst = parsed_ops[0][1] if parsed_ops and parsed_ops[0][0] == "reg" else ""
    if mn in ("adrp", "adr") and len(parsed_ops) == 2 and dst.startswith("x"):
        tgt = parsed_ops[1][1]
        if tgt is not None and abs(tgt - addr) <= PC_REACH:
            g.add((dst, 0))
    elif (mn == "add" and len(parsed_ops) == 3 and dst.startswith("x")
          and (parsed_ops[1][1], 0) in st.g and parsed_ops[2][0] == "imm"
          and 0 <= parsed_ops[2][1] <= 4095):
        g.add((dst, 1))
    return new.but(g=frozenset(g)), deepest


def a64_core(mn, ops, parsed_ops, mem, st):
    """(new state, deepest byte touched or None) after a parsed non-control AArch64 instruction."""
    what = f"{mn} {ops}".strip()
    is_load, is_store = mn.startswith("ld"), mn.startswith("st")
    deepest = None
    record = restored = single = False
    if mem is not None:
        mode, data, base, k, _, writeback = mem
        if base in A64_FP and writeback:
            raise Refuse(f"writeback on the frame pointer: {what}")
        if base in A64_LR and writeback:
            raise Refuse(f"writeback on the link register: {what}")
        if base not in ("sp", "x29", "fp"):
            # Any other register may hold a stack address (main's argv, envp and apple
            # vector, a value an external function left): only a PC-relative one is used.
            if base not in {x[0] for x in st.g}:
                raise Refuse(f"memory addressed through a register that may hold a stack address: {what}")
            if writeback or mode == "index" or abs(k) > A64_OFF_REACH:
                raise Refuse(f"PC-relative access beyond the premise's reach: {what}")
        else:
            if mode == "index":
                raise Refuse(f"indexed stack address: {what}")
            record = mn in ("stp", "ldp") and data == ["x29", "x30"] and base == "sp"
            size = a64_size(mn, data)
            d = st.d
            if base in A64_FP:
                top = frame_depth(st.fp, what) - k
            elif mode == "pre":
                d = d - k
                top = d
            elif mode == "post":
                top = st.d
                d = d - k
            else:
                top = d - k
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
                else:
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
                w = touch(st, lo, top, "r" if is_load else "w", what)
                if is_store:
                    saved = cleared(saved, lo, top)
            st = st.but(d=d, saved=saved, fp=fp, lr=lr, w=w)
            deepest = top
        if is_store:
            if not (record or single) and any(a in A64_SP + A64_FP for a in data):
                raise Refuse(f"stack or frame pointer stored as a value: {what}")
            return st, deepest
        if any(a in A64_LR for a in data) and not (record or restored):
            raise Refuse(f"link register overwritten: {what}")
        if any(a in A64_FP for a in data) and not restored:
            st = st.but(fp=None)
        return st, deepest
    # Not a memory instruction.
    dst = parsed_ops[0][1] if parsed_ops and mn not in A64_FLAGS_ONLY and parsed_ops[0][0] == "reg" else ""
    srcs = [val for kind, val in (parsed_ops if mn in A64_FLAGS_ONLY else parsed_ops[1:]) if kind == "reg"]
    if dst in A64_SP:
        if mn in ("sub", "add") and len(parsed_ops) == 3 and parsed_ops[1] == ("reg", "sp") and parsed_ops[2][0] == "imm":
            n = parsed_ops[2][1]
            return st.but(d=st.d + n if mn == "sub" else st.d - n), None
        if (mn, srcs) == ("mov", ["x29"]):
            return st.but(d=frame_depth(st.fp, what)), None
        if mn in ("sub", "add") and len(parsed_ops) == 3 and parsed_ops[1] == ("reg", "x29") and parsed_ops[2][0] == "imm":
            n, f = parsed_ops[2][1], frame_depth(st.fp, what)
            return st.but(d=f + n if mn == "sub" else f - n), None
        raise Refuse(f"unmodelled stack-pointer write: {what}")
    if dst in A64_FP:
        if (mn, srcs) == ("mov", ["sp"]):
            return st.but(fp=st.d), None
        if mn == "add" and len(parsed_ops) == 3 and parsed_ops[1] == ("reg", "sp") and parsed_ops[2][0] == "imm":
            return st.but(fp=st.d - parsed_ops[2][1]), None
        if any(a in A64_SP + A64_FP for a in srcs):
            raise Refuse(f"stack or frame pointer used as a value: {what}")
        return st.but(fp=None), None
    if dst in A64_LR:
        raise Refuse(f"link register overwritten: {what}")
    if any(a in A64_SP + A64_FP for a in srcs):
        raise Refuse(f"stack or frame pointer used as a value: {what}")
    return st, None


def control(mn, ops):
    return stack_access.control(ARCH, mn, ops)


def analyse(funcs, stubs):
    """(bound, externals, owned): main's bound, each external's deepest entry depth, both from
    main's entry, and the owned functions' names."""
    if ARCH == "x86_64" and TLS_MEMSZ is None:
        raise Refuse("x86 analysis without the linked executable's PT_TLS")
    owned = {owned_name(n): n for n in funcs if owned_name(n)}
    if "main" not in owned:
        raise Refuse("no main")
    memo, active = {}, set()
    ret_push = 8 if ARCH == "x86_64" else 0
    inst = x86_inst if ARCH == "x86_64" else a64_inst

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
                before = st.d
                try:
                    st, touched = inst(mn, ops, st, addr)
                except Refuse as e:
                    raise Refuse(f"{key}+{addr:#x}: {e}") from None
                # The stack pointer either side of the instruction (a pre-indexed or pushing access
                # is at the new one, a post-indexed one at the old).
                if touched is not None and touched > max(before, st.d) + RED_ZONE:
                    raise Refuse(f"{key}+{addr:#x}: an access {touched - max(before, st.d)} bytes below the "
                                 f"stack pointer, beyond the {RED_ZONE}-byte red zone of {TARGET}: {mn} {ops}")
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
                elif base.endswith("@plt") and "+" not in sym:
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
    if len(sys.argv) == 3 and sys.argv[1] == "--elf-tls":
        try:
            print(elf_tls_line(sys.argv[2]))
        except Refuse as e:
            print(f"refused: {e}", file=sys.stderr)
            sys.exit(1)
        sys.exit(0)
    if len(sys.argv) not in (3, 4) or sys.argv[1] not in TARGETS:
        sys.exit(f"usage: stack_bound.py {'|'.join(TARGETS)} DISASM [--ranges] | --elf-tls HEADERS_JSON")
    TARGET = sys.argv[1]
    ARCH, RED_ZONE = TARGETS[TARGET]
    try:
        funcs, stubs, TLS_MEMSZ = parse(sys.argv[2])
        total, ext, owned = analyse(funcs, stubs)
    except Refuse as e:
        print(f"refused: {e}")
        sys.exit(1)
    print(f"bound {total}")
    for name, depth in sorted(ext.items()):
        print(f"external {name} {depth}")
    if len(sys.argv) == 4 and sys.argv[3] == "--ranges":
        print(f"redzone {RED_ZONE}")
        for key, name in sorted(owned.items()):
            code = funcs[name]
            print(f"range {key} {code[0][0]:#x} {code[-1][0]:#x}")

# Shared instruction, operand and premise model for slice-0 gate 12 (stack_bound.py static analysis,
# stack_measure.py under gdb, stack_measure_lldb.py under LLDB; Concrete/README.md gate 12).
# PROVISIONAL. Plain Python 3.9 (Apple's LLDB runs its own), no imports beyond re.
#
# Both the static bound and the run check consume instructions through this module's grammar and
# allowlists. Every byte of every operand must be consumed by an explicit operand shape; anything
# outside the modelled mnemonics and operand forms is an error (fail closed), never "no access".
import re

MASK = (1 << 64) - 1
NUM = re.compile(r"(?:0x[0-9a-f]+|\d+)$")

# Premise parameters (shared by stack_bound.py and the run checks):
# PC-relative addressing reaches at most PC_REACH bytes from the instruction (x86 rip displacement;
# AArch64 adrp/adr target) plus, on AArch64, one add immediate (at most 4095) and the access's own
# offset (at most A64_OFF_REACH).
PC_REACH = 16 << 20
A64_OFF_REACH = 32760
# x86-64 Linux glibc's tcbhead_t through __private_tm occupies [fs:0, fs:0x70). Static TLS uses
# Variant II: the linked PT_TLS reservation of M bytes occupies [fs:-M, fs:0).
TCB_SIZE = 0x70
# TARGET -> (architecture, red zone in bytes): x86-64 System V ABI section 3.2.2 (128); Apple,
# "Writing ARM64 code for Apple platforms" (128); AAPCS64 has none (0).
TARGETS = {
    "x86_64-linux": ("x86_64", 128),
    "aarch64-macos": ("aarch64", 128),
    "aarch64-linux": ("aarch64", 0),
}

X86_SP = ("rsp", "esp", "sp", "spl")
X86_FP = ("rbp", "ebp", "bp", "bpl")
X86_STACK = X86_SP + X86_FP
X86_SEGMENTS = ("cs", "ds", "es", "fs", "gs", "ss")
X86_GPR64 = re.compile(r"^(r[abcd]x|r[sd]i|rbp|r(?:8|9|1[0-5]))$")
# Register names an operand or effective address may use. Anything else is refused before it
# reaches the debugger (gdb reads `$name` as a convenience variable when it is not a register).
X86_OP_REG = re.compile(
    r"^(?:r(?:[abcd]x|[sd]i|[sb]p|(?:[89]|1[0-5])[dwb]?)"
    r"|e(?:[abcd]x|[sd]i|[sb]p)|[abcd][xhl]|[sd]il?|[sb]pl?)$"
)
X86_REG = re.compile(
    r"^(?:r(?:[abcd]x|[sd]i|[sb]p|(?:[89]|1[0-5])[dwb]?)|e(?:[abcd]x|[sd]i|[sb]p)|r?ip)$"
)
X86_OP_IMM = re.compile(r"^[+-]?(?:0x[0-9a-f]+|\d+)$")
X86_SIZE = {
    "byte": 1, "word": 2, "dword": 4, "qword": 8, "tbyte": 10,
    "xmmword": 16, "ymmword": 32, "zmmword": 64,
}
X86_ACC_SIZE = {"al": 1, "ax": 2, "eax": 4, "rax": 8}

X86_JCC = re.compile(r"^j(?!mp$)[a-z]+$")
# The data-processing instructions whose only effect on the stack is what x86_inst and
# x86_addresses model. Any other instruction (string ops, rep prefixes, xlat, enter, loop, etc.)
# is refused by both the static analysis and the run check.
X86_PLAIN = re.compile(
    r"^(mov|movabs|movzx|movsx|movsxd|lea|add|sub|adc|sbb|imul|mul|and|or|xor|not|neg|inc|dec"
    r"|cmp|test|shl|shr|sar|sal|rol|ror|cdq|cqo|cdqe|cwde|cbw|cwd|nop|endbr64"
    r"|push|pop|leave|set[a-z]+|cmov[a-z]+)$"
)
X86_ZERO_OP = ("cdq", "cqo", "cdqe", "cwde", "cbw", "cwd", "nop", "endbr64", "leave")
X86_ONE_OP = ("push", "pop", "not", "neg", "inc", "dec", "mul")
X86_TWO_OP = (
    "mov", "movabs", "movzx", "movsx", "movsxd", "lea", "add", "sub", "adc", "sbb",
    "and", "or", "xor", "cmp", "test",
)
X86_SHIFT_OP = ("shl", "shr", "sar", "sal", "rol", "ror")

A64_SP = ("sp", "wsp")
A64_FP = ("x29", "w29")
A64_LR = ("x30", "w30")
A64_REGSIZE = {"x": 8, "w": 4, "q": 16, "d": 8, "s": 4, "h": 2, "b": 1}
A64_FLAGS_ONLY = ("cmp", "cmn", "tst", "ccmp", "ccmn", "nop")
A64_COND = re.compile(r"^b\.[a-z]+$")
A64_PLAIN = re.compile(
    r"^(mov|movz|movk|movn|add|adds|sub|subs|neg|negs|mul|madd|msub|mneg|smull|umull|smulh|umulh"
    r"|smaddl|umaddl|sdiv|udiv|and|ands|orr|orn|eor|eon|bic|bics|mvn|lsl|lsr|asr|ror|lslv|lsrv"
    r"|asrv|cmp|cmn|tst|ccmp|ccmn|cset|csetm|csel|csinc|csinv|csneg|cinc|cneg|sxtb|sxth|sxtw"
    r"|uxtb|uxth|ubfx|sbfx|ubfm|sbfm|bfi|bfxil|extr|adr|adrp|nop"
    r"|ldr|ldrb|ldrh|ldrsb|ldrsh|ldrsw|ldur|ldurb|ldurh|ldursb|ldursh|ldursw"
    r"|str|strb|strh|stur|sturb|sturh|ldp|stp|ldpsw)$"
)
A64_REG = re.compile(r"(?:[xw](?:[12]?\d|30)|sp|fp|lr)$")
A64_DATA_REG = re.compile(r"^(?:[xw](?:[12]?\d|30)|[xw]zr|sp|wsp|[bhsdq](?:[12]?\d|3[01]))$")

_BRACKET_MEM = re.compile(
    r"^(?:(?P<seg1>[cdefg]s|ss):\s*)?"
    r"(?:(?P<size>\w+)\s+ptr\s+)?"
    r"(?:(?P<seg2>[cdefg]s|ss):\s*)?"
    r"\[(?P<inner>[^\[\]]*)\]$"
)
_ABS_SEG_MEM = re.compile(
    r"^(?:(?P<seg1>[cdefg]s|ss):\s*)?"
    r"(?:(?P<size>\w+)\s+ptr\s+)?"
    r"(?:(?P<seg2>[cdefg]s|ss):\s*)?"
    r"(?P<inner>[+-]?\s*(?:0x[0-9a-f]+|\d+))$"
)
_TERM = re.compile(r"\s*([+-]?)\s*([^\s+\-*]+(?:\s*\*\s*[^\s+\-*]+)?)\s*")


def _reg(reg, name, pattern):
    if not pattern.match(name):
        raise ValueError(f"unmodelled register {name!r} in an address")
    return reg(name)


def _num(text):
    return int(text, 0)


def byte_interval(k, size):
    """The inclusive signed offsets of an access, with its displacement reduced modulo 2^64."""
    lo = k & MASK
    if lo & (1 << 63):
        lo -= 1 << 64
    return lo, lo + size - 1


def tls_reservation(memsz, align=1, vaddr=0):
    """Variant II static-TLS reservation M below FS.base for PT_TLS (p_memsz, p_align, p_vaddr):
    0 when p_memsz == 0, else roundup(p_memsz + ((p_vaddr - p_memsz) & (a - 1)), a) for
    a = max(p_align, 1)."""
    if memsz < 0 or memsz > MASK or vaddr < 0 or vaddr > MASK or align < 0 or align > MASK:
        raise ValueError("PT_TLS field out of 64-bit unsigned range")
    a = max(align, 1)
    if a & (a - 1):
        raise ValueError(f"PT_TLS p_align {align:#x} is not a power of two")
    if memsz == 0:
        return 0
    raw = memsz + ((vaddr - memsz) & (a - 1))
    res = (raw + a - 1) & ~(a - 1)
    if res > MASK:
        raise ValueError("PT_TLS reservation overflows 64 bits")
    return res


def control(arch, mn, ops):
    """(kind, target) for a control instruction, or (None, None)."""
    if arch == "x86_64":
        if mn == "call":
            return "call", ops
        if mn == "jmp":
            return "jmp", ops
        if X86_JCC.match(mn):
            return "jcc", ops
        if mn == "ret":
            return ("indirect", ops) if ops else ("ret", None)
        if mn in ("ud2", "hlt", "int3"):
            return ("indirect", ops) if ops else ("stop", None)
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


def _parse_x86_inner(inner, what):
    """Parse the inner displacement/register sum of an x86 memory operand, consuming every byte."""
    if not inner or not inner.strip():
        raise ValueError(f"empty memory address: {what}")
    pos = 0
    idx = 0
    terms = []
    regs = []
    scaled = False
    disp = 0
    while pos < len(inner):
        tm = _TERM.match(inner, pos)
        if not tm or (idx > 0 and tm.group(1) not in ("+", "-")):
            raise ValueError(f"malformed memory address: {what}")
        sign = -1 if tm.group(1) == "-" else 1
        atom = tm.group(2)
        if "*" in atom:
            a, b = [p.strip() for p in atom.split("*", 1)]
            r, scale_s = (b, a) if NUM.match(a) else (a, b)
            if sign < 0 or not NUM.match(scale_s) or _num(scale_s) not in (1, 2, 4, 8):
                raise ValueError(f"unparsed scaled index {atom} in {what}")
            if not X86_REG.match(r) or r in ("rip", "eip"):
                raise ValueError(f"unmodelled register {r!r} in {what}")
            scale = _num(scale_s)
            terms.append(("reg", r, scale))
            regs.append(r)
            scaled = True
        elif NUM.match(atom):
            v = sign * _num(atom)
            terms.append(("imm", None, v))
            disp += v
        elif X86_REG.match(atom):
            if sign < 0:
                raise ValueError(f"subtracted register {atom!r} in {what}")
            terms.append(("reg", atom, 1))
            regs.append(atom)
        else:
            raise ValueError(f"unmodelled register {atom!r} in {what}")
        pos = tm.end()
        idx += 1
    return terms, regs, scaled, disp


def parse_x86_plain(mn, ops, allow_syscall=False):
    """Parse a non-control x86-64 instruction in Intel syntax (llvm-objdump or gdb).

    Consumes every byte of every operand against the explicit allowlist of mnemonics and operand
    shapes (register, immediate, bracketed memory, or segment-qualified unbracketed displacement).
    Returns (parsed_ops, memory) where each memory item is
    (index, segment, width, terms, regs, scaled, disp)."""
    what = f"{mn} {ops}".strip()
    if allow_syscall and mn == "syscall":
        if ops:
            raise ValueError(f"unexpected operand: {what}")
        return [], []
    if not X86_PLAIN.match(mn):
        raise ValueError(f"unmodelled instruction: {what}")
    if not ops:
        raw_ops = []
    else:
        raw_ops = [a.strip() for a in ops.split(",")]
        if any(not a for a in raw_ops):
            raise ValueError(f"malformed operand list: {what}")
    if mn in X86_ZERO_OP:
        if raw_ops:
            raise ValueError(f"unexpected operand: {what}")
    elif mn in X86_ONE_OP or mn.startswith("set"):
        if len(raw_ops) != 1:
            raise ValueError(f"wrong operand count: {what}")
    elif mn in X86_TWO_OP or mn.startswith("cmov"):
        if len(raw_ops) != 2:
            raise ValueError(f"wrong operand count: {what}")
    elif mn in X86_SHIFT_OP:
        if len(raw_ops) not in (1, 2):
            raise ValueError(f"wrong operand count: {what}")
    elif mn == "imul":
        if len(raw_ops) not in (1, 2, 3):
            raise ValueError(f"wrong operand count: {what}")

    parsed_ops = []
    memory = []
    for i, op in enumerate(raw_ops):
        if op in X86_SEGMENTS:
            if i == 0 and mn not in ("cmp", "test"):
                raise ValueError(f"write to a segment register: {what}")
            raise ValueError(f"segment register used as a value: {what}")
        if X86_OP_REG.fullmatch(op):
            if mn == "lea" and i == 1:
                raise ValueError(f"LEA without memory operand: {what}")
            parsed_ops.append(("reg", op))
            continue
        if X86_OP_IMM.fullmatch(op):
            if (i == 0 and mn != "push") or mn in ("lea", "pop") or mn.startswith(("set", "cmov")):
                raise ValueError(f"invalid immediate operand: {what}")
            parsed_ops.append(("imm", _num(op.replace(" ", ""))))
            continue
        bm = _BRACKET_MEM.fullmatch(op)
        am = None if bm is not None else _ABS_SEG_MEM.fullmatch(op)
        if bm is None and (am is None or not (am.group("seg1") or am.group("seg2"))):
            raise ValueError(f"unmodelled operand {op!r}: {what}")
        m = bm or am
        bracketed = bm is not None
        seg1, seg2, size_name, inner = (
            m.group("seg1"), m.group("seg2"), m.group("size"), m.group("inner")
        )
        if seg1 and seg2:
            raise ValueError(f"duplicate segment override: {what}")
        segment = seg1 or seg2
        terms, regs, scaled, disp = _parse_x86_inner(inner, what)
        if mn == "lea":
            if i != 1 or not bracketed or size_name is not None:
                raise ValueError(f"malformed LEA operand: {what}")
            if segment:
                raise ValueError(f"segment override on LEA: {what}")
            width = None
        else:
            if size_name is not None:
                if size_name not in X86_SIZE:
                    raise ValueError(f"memory access of unknown size: {what}")
                width = X86_SIZE[size_name]
            elif mn == "movabs" and not bracketed and len(raw_ops) == 2 and raw_ops[1 - i] in X86_ACC_SIZE:
                width = X86_ACC_SIZE[raw_ops[1 - i]]
            else:
                raise ValueError(f"memory access of unknown size: {what}")
            if segment == "gs":
                raise ValueError(f"gs segment override on x86_64-linux: {what}")
            if segment and segment != "fs":
                raise ValueError(f"unmodelled {segment} segment override: {what}")
            if segment == "fs" and regs:
                raise ValueError(f"thread-pointer-relative access through a register: {what}")
        mem_info = (i, segment, width, terms, regs, scaled, disp)
        parsed_ops.append(("mem", mem_info))
        memory.append(mem_info)

    if mn == "lea" and len(memory) != 1:
        raise ValueError(f"LEA without memory operand: {what}")
    if len(memory) > 1:
        raise ValueError(f"instruction with more than one memory operand: {what}")
    return parsed_ops, memory


def x86_addresses(asm, reg, next_pc):
    """Effective addresses of the memory operands of one instruction in gdb's Intel syntax.

    Uses the shared fail-closed instruction and operand grammar (parse_x86_plain). GDB prints
    register-relative operands bracketed (`QWORD PTR [rsp-0x80]`) and absolute segment operands
    unbracketed (`QWORD PTR fs:0x28`, `BYTE PTR fs:0x100000`, `movabs rax,fs:0x100`). Any
    instruction outside the allowlist (rep/string instructions, xlat, enter, etc.), any segment
    override other than absolute fs:, or any unparsed operand is an error."""
    clean = asm.split("#", 1)[0].strip().lower()
    if not clean:
        raise ValueError("empty instruction")
    parts = clean.split(None, 1)
    mn, ops = parts[0], (parts[1].strip() if len(parts) > 1 else "")
    kind, arg = control("x86_64", mn, ops)
    if kind is not None:
        if kind == "indirect":
            raise ValueError(f"indirect control transfer: {asm}")
        if kind in ("call", "jmp", "jcc") and not re.fullmatch(
            r"(?:0x)?[0-9a-f]+(?:\s*<[^>\[\]:]+>)?", arg
        ):
            raise ValueError(f"indirect or unparsed control target: {asm}")
        return []
    _, memory = parse_x86_plain(mn, ops, allow_syscall=True)
    if mn == "lea" or not memory:
        return []
    _, segment, _, terms, _, _, _ = memory[0]
    addr = reg("fs_base") if segment == "fs" else 0
    for term_kind, r, val in terms:
        if term_kind == "imm":
            addr += val
        elif r == "rip":
            addr += next_pc * val
        else:
            addr += _reg(reg, r, X86_REG) * val
    return [addr & MASK]


def _a64_index(text, reg):
    """Value of an AArch64 index operand: `x9`, `x9, lsl #3`, `w9, sxtw #2`, `w9, uxtw`."""
    parts = [p.strip() for p in text.split(",")]
    r = parts[0]
    v = _reg(reg, "x" + r[1:], A64_REG) if r.startswith("w") else _reg(reg, r, A64_REG)
    if r.startswith("w"):
        v &= 0xFFFFFFFF
    shift = 0
    if len(parts) > 1:
        m = re.fullmatch(r"(lsl|sxtw|uxtw|sxtx|uxtx)(?:\s+#(\d+))?", parts[1])
        if not m:
            raise ValueError(f"unparsed index extension {parts[1]}")
        if m.group(1) == "sxtw" and v & 0x80000000:
            v -= 1 << 32
        shift = int(m.group(2) or 0)
    if len(parts) > 2:
        raise ValueError(f"unparsed index {text}")
    return v << shift


def a64_addresses(mnemonic, operands, reg):
    """Effective addresses of one AArch64 instruction in LLDB's syntax (`stp x29, x30, [sp,
    #-0x10]!`, `ldr x30, [sp], #0x10`, `ldr w8, [x9, x10, lsl #2]`). A post-indexed access is at the
    base; a pre-indexed or offset one at base plus offset. Use a64_new_sp for the stack pointer a
    pre-indexed access is judged against."""
    mn, ops = mnemonic.lower().strip(), operands.lower().split("//", 1)[0].strip()
    kind, _ = control("aarch64", mn, ops)
    if kind is None and not A64_PLAIN.match(mn):
        raise ValueError(f"unmodelled instruction: {mn} {operands}")
    if "[" not in ops and "]" not in ops:
        return []
    m = re.search(r"\[([^\[\]]*)\](!?)", ops)
    if not m or "[" in ops[:m.start()] or "]" in ops[:m.start()] or "[" in ops[m.end():] or "]" in ops[m.end():]:
        raise ValueError(f"unparsed memory operand in {mn} {operands}")
    parts = [p.strip() for p in m.group(1).split(",", 1)]
    base = _reg(reg, parts[0], A64_REG)
    post = ops[m.end():].strip()
    if post:
        if m.group(2) == "!" or not re.fullmatch(r",\s*(#-?(0x[0-9a-f]+|\d+)|x\d+)", post) or len(parts) > 1:
            raise ValueError(f"unparsed post-index in {mn} {operands}")
        return [base & MASK]
    if len(parts) == 1:
        return [base & MASK]
    off = parts[1]
    if off.startswith("#"):
        return [(base + _num(off[1:])) & MASK]
    return [(base + _a64_index(off, reg)) & MASK]


def a64_new_sp(mnemonic, operands, reg, sp):
    """The stack pointer an access of this instruction is judged against: the written-back one for
    a pre-indexed sp access (`stp x29, x30, [sp, #-0x90]!` allocates the frame first, then stores
    at the new sp, as stack_bound.py treats it), else `sp`."""
    ops = operands.lower().split("//", 1)[0]
    m = re.search(r"\[\s*sp\s*(?:,\s*#(-?(?:0x[0-9a-f]+|\d+)))?\s*\]!", ops)
    if not m:
        return sp
    return min(sp, (sp + _num(m.group(1) or "0")) & MASK)


def lowest_stack(addresses, sp, sp0, red_zone, span):
    """The lowest of `addresses` that lies on the stack below main's entry stack pointer sp0
    (within `span` bytes of it; the analysis' premise puts the image farther away), or None. An
    access deeper than the red zone below `sp` is an error: no bound holds for bytes a signal
    handler may overwrite. `sp` is the lower of the stack pointer before and after the access (a
    pre-indexed access is at the new one; a64_new_sp)."""
    low = None
    for a in addresses:
        if sp0 - span <= a < sp0:
            if a < sp - red_zone:
                raise ValueError(f"an access {sp - a} bytes below the stack pointer, beyond the "
                                 f"{red_zone}-byte red zone")
            low = a if low is None else min(low, a)
    return low

# The memory addresses one instruction accesses, for the run checks (stack_measure.py under gdb,
# stack_measure_lldb.py under LLDB; README gate 12). PROVISIONAL. Plain Python 3.9 (Apple's LLDB
# runs its own), no imports beyond re.
#
# The run check counts the lowest byte each stepped instruction of the Concrete-owned code
# addresses, not only the stack pointer: code may use the target's red zone, below the stack
# pointer, without moving it (stack_bound.py counts the same bytes). An access's lowest byte is its
# effective address, whatever its size, so only the address is computed. Fail closed: an operand
# this module cannot evaluate is an error, never "no access".
import re

MASK = (1 << 64) - 1
NUM = re.compile(r"(?:0x[0-9a-f]+|\d+)$")
# Register names an address may use. Anything else is refused before it reaches the debugger (gdb
# reads `$name` as a convenience variable when it is not a register).
X86_REG = re.compile(r"(?:r(?:[abcd]x|[sd]i|[sb]p|\d{1,2}[dwb]?)|e(?:[abcd]x|[sd]i|[sb]p)|r?ip)$")
A64_REG = re.compile(r"(?:[xw](?:[12]?\d|30)|sp|fp|lr)$")


def _reg(reg, name, pattern):
    if not pattern.match(name):
        raise ValueError(f"unmodelled register {name!r} in an address")
    return reg(name)


def _num(text):
    return int(text, 0)


def x86_addresses(asm, reg, next_pc):
    """Effective addresses of the memory operands of one instruction in gdb's Intel syntax
    (`mov QWORD PTR [rsp-0x80],0x0`). reg(name) reads a register. lea and nop compute an address
    without accessing it; fs:/gs: operands address the thread block, not the stack."""
    asm = asm.split("#", 1)[0].strip().lower()
    mn = asm.split(None, 1)[0] if asm else ""
    if mn == "lea" or mn.startswith("nop") or mn.startswith("prefetch"):
        return []
    out = []
    for m in re.finditer(r"(?:(\w\w):)?\[([^\]]*)\]", asm):
        if m.group(1) in ("fs", "gs"):
            continue
        inner = m.group(2).replace(" ", "")
        if not inner or inner[0] not in "+-":
            inner = "+" + inner
        terms = re.findall(r"([+-])([^+-]+)", inner)
        if "".join(s + t for s, t in terms) != inner:
            raise ValueError(f"unparsed memory operand [{m.group(2)}] in {asm}")
        addr = 0
        for sign, term in terms:
            if "*" in term:
                a, b = term.split("*", 1)
                r, scale = (b, a) if NUM.match(a) else (a, b)
                if not NUM.match(scale) or _num(scale) not in (1, 2, 4, 8):
                    raise ValueError(f"unparsed scaled index {term} in {asm}")
                v = _reg(reg, r, X86_REG) * _num(scale)
            elif NUM.match(term):
                v = _num(term)
            elif term == "rip":
                v = next_pc
            else:
                v = _reg(reg, term, X86_REG)
            addr += v if sign == "+" else -v
        out.append(addr & MASK)
    if not out and re.search(r"\b(movs|stos|lods|cmps|scas)", mn):
        raise ValueError(f"string instruction with implicit memory operands: {asm}")
    return out


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
    base; a pre-indexed or offset one at base plus offset. prfm/prfum only hint. Use
    a64_new_sp for the stack pointer a pre-indexed access is judged against."""
    mn, ops = mnemonic.lower(), operands.lower().split("//", 1)[0]
    if mn.startswith("prf"):
        return []
    m = re.search(r"\[([^\]]*)\](!?)", ops)
    if not m:
        return []
    parts = [p.strip() for p in m.group(1).split(",", 1)]
    base = _reg(reg, parts[0], A64_REG)
    post = ops[m.end():].strip()
    if post:
        if not re.fullmatch(r",\s*(#-?(0x[0-9a-f]+|\d+)|x\d+)", post) or len(parts) > 1:
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
    if not m or mnemonic.lower().startswith("prf"):
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

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
A64_FP = ("x29", "w29", "fp")
A64_LR = ("x30", "w30", "lr")
A64_REGSIZE = {"x": 8, "w": 4, "q": 16, "d": 8, "s": 4, "h": 2, "b": 1, "f": 8, "l": 8}
A64_FLAGS_ONLY = ("cmp", "cmn", "tst", "ccmp", "ccmn", "fcmp", "nop")
A64_COND = re.compile(r"^b\.(?:eq|ne|cs|hs|cc|lo|mi|pl|vs|vc|hi|ls|ge|lt|gt|le|al|nv)$")
_A64_CC = re.compile(r"^(?:eq|ne|cs|hs|cc|lo|mi|pl|vs|vc|hi|ls|ge|lt|gt|le|al|nv)$")
A64_PLAIN = re.compile(
    r"^(mov|movz|movk|movn|add|adds|sub|subs|neg|negs|mul|madd|msub|mneg|smull|umull|smulh|umulh"
    r"|smaddl|umaddl|sdiv|udiv|and|ands|orr|orn|eor|eon|bic|bics|mvn|lsl|lsr|asr|ror|lslv|lsrv"
    r"|asrv|rorv|cmp|cmn|tst|ccmp|ccmn|cset|csetm|csel|csinc|csinv|csneg|cinc|cneg|sxtb|sxth|sxtw"
    r"|uxtb|uxth|ubfx|sbfx|ubfm|sbfm|bfi|bfxil|extr|adr|adrp|nop"
    r"|fmov|fcvt|fneg|fabs|fsqrt|fcvtzs|fcvtzu|scvtf|ucvtf|fadd|fsub|fmul|fdiv|fcmp|fcsel"
    r"|ldr|ldrb|ldrh|ldrsb|ldrsh|ldrsw|ldur|ldurb|ldurh|ldursb|ldursh|ldursw"
    r"|str|strb|strh|stur|sturb|sturh|ldp|stp|ldpsw)$"
)
A64_SINGLE_LDST = (
    "ldr", "ldrb", "ldrh", "ldrsb", "ldrsh", "ldrsw",
    "ldur", "ldurb", "ldurh", "ldursb", "ldursh", "ldursw",
    "str", "strb", "strh", "stur", "sturb", "sturh",
)
A64_UNSCALED_LDST = (
    "ldur", "ldurb", "ldurh", "ldursb", "ldursh", "ldursw",
    "stur", "sturb", "sturh",
)
A64_PAIR_LDST = ("ldp", "stp", "ldpsw")
A64_LDST = A64_SINGLE_LDST + A64_PAIR_LDST
A64_ZERO_OP = ("nop",)
A64_TWO_OP = (
    "mov", "adr", "adrp", "cset", "csetm", "sxtb", "sxth", "sxtw", "uxtb", "uxth",
    "fmov", "fcvt", "fneg", "fabs", "fsqrt", "fcvtzs", "fcvtzu", "scvtf", "ucvtf", "fcmp",
)
A64_TWO_OR_THREE_OP = ("movz", "movk", "movn", "mvn", "neg", "negs", "cmp", "cmn", "tst")
A64_THREE_OP = (
    "mul", "mneg", "smull", "umull", "smulh", "umulh", "sdiv", "udiv",
    "lsl", "lsr", "asr", "ror", "lslv", "lsrv", "asrv", "rorv",
    "cinc", "cneg", "fadd", "fsub", "fmul", "fdiv",
)
A64_THREE_OR_FOUR_OP = (
    "add", "adds", "sub", "subs", "and", "ands", "orr", "orn", "eor", "eon", "bic", "bics",
)
A64_FOUR_OP = (
    "madd", "msub", "smaddl", "umaddl", "ccmp", "ccmn", "csel", "csinc", "csinv", "csneg",
    "ubfx", "sbfx", "ubfm", "sbfm", "bfi", "bfxil", "extr", "fcsel",
)
A64_ARITY = {
    **{m: (2,) for m in A64_TWO_OP},
    **{m: (2, 3) for m in A64_TWO_OR_THREE_OP},
    **{m: (3,) for m in A64_THREE_OP},
    **{m: (3, 4) for m in A64_THREE_OR_FOUR_OP},
    **{m: (4,) for m in A64_FOUR_OP},
}
A64_COND_OP = (
    "cset", "csetm", "cinc", "cneg", "csel", "csinc", "csinv", "csneg", "ccmp", "ccmn", "fcsel",
)
A64_MOD_OP = A64_TWO_OR_THREE_OP + A64_THREE_OR_FOUR_OP
A64_IMM_OK = {
    ("mov", 1), ("movz", 1), ("movk", 1), ("movn", 1),
    ("cmp", 1), ("cmn", 1), ("tst", 1), ("ccmp", 1), ("ccmn", 1), ("ccmp", 2), ("ccmn", 2),
    ("add", 2), ("adds", 2), ("sub", 2), ("subs", 2),
    ("and", 2), ("ands", 2), ("orr", 2), ("eor", 2),
    ("lsl", 2), ("lsr", 2), ("asr", 2), ("ror", 2),
    ("ubfx", 2), ("ubfx", 3), ("sbfx", 2), ("sbfx", 3),
    ("ubfm", 2), ("ubfm", 3), ("sbfm", 2), ("sbfm", 3),
    ("bfi", 2), ("bfi", 3), ("bfxil", 2), ("bfxil", 3), ("extr", 3),
    ("fmov", 1), ("fcmp", 1),
}
A64_IMM_REQ = {
    ("movz", 1), ("movk", 1), ("movn", 1), ("ccmp", 2), ("ccmn", 2),
    ("ubfx", 2), ("ubfx", 3), ("sbfx", 2), ("sbfx", 3),
    ("ubfm", 2), ("ubfm", 3), ("sbfm", 2), ("sbfm", 3),
    ("bfi", 2), ("bfi", 3), ("bfxil", 2), ("bfxil", 3), ("extr", 3),
}
A64_REG = re.compile(r"^(?:[xw](?:[12]?\d|30)|sp|fp|lr)$")
A64_ADDR_BASE = re.compile(r"^(?:x(?:[12]?\d|30)|sp|fp|lr)$")
A64_GPR = re.compile(r"^(?:[xw](?:[12]?\d|30)|[xw]zr|sp|wsp|fp|lr)$")
A64_GPR_NO_SP = re.compile(r"^(?:[xw](?:[12]?\d|30)|[xw]zr|fp|lr)$")
A64_FP_REG = re.compile(r"^[bhsdq](?:[12]?\d|3[01])$")
A64_DATA_REG = re.compile(r"^(?:[xw](?:[12]?\d|30)|[xw]zr|sp|wsp|fp|lr|[bhsdq](?:[12]?\d|3[01]))$")
_A64_IMM = re.compile(r"^#-?(?:0x[0-9a-f]+|\d+)$")
_A64_UIMM = re.compile(r"^#(?:0x[0-9a-f]+|\d+)$")
_A64_TARGET = re.compile(r"^#?-?(?:0x)?[0-9a-f]+(?:\s*<[^>\[\]]+>)?$")
_A64_SHIFT = re.compile(r"^(?:lsl|lsr|asr|ror)\s+#(?:0x[0-9a-f]+|\d+)$")
_A64_ADD_SHIFT = re.compile(r"^(?:lsl|lsr|asr)\s+#(?:0x[0-9a-f]+|\d+)$")
_A64_EXTEND = re.compile(r"^(?:uxtb|uxth|uxtw|uxtx|sxtb|sxth|sxtw|sxtx)(?:\s+#[0-4])?$")
_A64_INDEX_EXT = re.compile(r"^(lsl|uxtw|sxtw|uxtx|sxtx)(?:\s+#([0-4]))?$")

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
    if A64_COND.match(mn):
        return "jcc", ops
    if mn in ("cbz", "cbnz"):
        parts = [p.strip() for p in ops.split(",")]
        if len(parts) != 2 or not A64_GPR_NO_SP.fullmatch(parts[0]):
            return "indirect", ops
        return "jcc", parts[1]
    if mn in ("tbz", "tbnz"):
        parts = [p.strip() for p in ops.split(",")]
        if (len(parts) != 3 or not A64_GPR_NO_SP.fullmatch(parts[0])
                or not _A64_UIMM.fullmatch(parts[1]) or _num(parts[1][1:]) > 63):
            return "indirect", ops
        return "jcc", parts[2]
    if mn in ("blr", "br", "braa", "blraa", "braaz", "blraaz", "retaa", "retab"):
        return "indirect", ops
    if mn == "ret":
        return ("indirect", ops) if ops else ("ret", None)
    if mn in ("brk", "udf", "hlt"):
        if ops and not _A64_UIMM.fullmatch(ops):
            return "indirect", ops
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


def _strip_a64_comment(text):
    return text.split("//", 1)[0].split(";", 1)[0].strip()


def _parse_a64_index(parts):
    """Parse the index register and optional shift/extend of `[base, xm[, ext #n]]`, or None."""
    if len(parts) not in (1, 2) or not A64_GPR_NO_SP.fullmatch(parts[0]):
        return None
    idx_reg = parts[0]
    if len(parts) == 1:
        return None if idx_reg.startswith("w") else (idx_reg, "lsl", 0)
    em = _A64_INDEX_EXT.fullmatch(parts[1])
    if not em or (em.group(1) in ("uxtw", "sxtw")) != idx_reg.startswith("w"):
        return None
    return idx_reg, em.group(1), int(em.group(2) or 0)


def _valid_a64_mod(mn, prev_kind, op):
    if mn in ("movz", "movk", "movn"):
        return bool(re.fullmatch(r"lsl\s+#(0|16|32|48)", op))
    if prev_kind == "imm":
        return mn in ("add", "adds", "sub", "subs", "cmp", "cmn") and bool(re.fullmatch(r"lsl\s+#(0|12)", op))
    if mn in ("add", "adds", "sub", "subs", "cmp", "cmn"):
        return bool(_A64_ADD_SHIFT.fullmatch(op) or _A64_EXTEND.fullmatch(op))
    return bool(_A64_SHIFT.fullmatch(op))


def _valid_a64_reg(mn, op):
    if mn.startswith(("f", "scvt", "ucvt")):
        return bool(
            A64_FP_REG.fullmatch(op)
            or (mn in ("fmov", "fcvtzs", "fcvtzu", "scvtf", "ucvtf") and A64_GPR_NO_SP.fullmatch(op))
        )
    return bool(A64_GPR.fullmatch(op))


def parse_a64_plain(mn, ops):
    """Parse a non-control AArch64 instruction in llvm-objdump or LLDB syntax.

    Consumes every byte of every operand against the explicit allowlist of mnemonics and operand
    shapes (general and SIMD/FP scalar registers, immediates, shift/extend modifiers, condition
    codes, PC-relative targets, and bracketed memory operands in offset, pre-indexed, post-indexed,
    or register-offset addressing modes). Returns (parsed_ops, mem) where mem is None or
    (mode, data_regs, base, disp, index_info, writeback)."""
    what = f"{mn} {ops}".strip()
    if not A64_PLAIN.match(mn):
        raise ValueError(f"unmodelled instruction: {what}")
    raw_ops = [a.strip() for a in re.split(r",(?![^\[\]]*\])", ops)] if ops else []
    if any(not a for a in raw_ops):
        raise ValueError(f"malformed operand list: {what}")
    if mn in A64_ZERO_OP:
        if raw_ops:
            raise ValueError(f"unexpected operand: {what}")
        return [], None
    if mn in A64_LDST:
        n_data = 2 if mn in A64_PAIR_LDST else 1
        if len(raw_ops) not in (n_data + 1, n_data + 2):
            raise ValueError(f"wrong operand count: {what}")
        data = raw_ops[:n_data]
        if not all(A64_DATA_REG.fullmatch(r) for r in data):
            raise ValueError(f"unmodelled transfer register: {what}")
        if mn.startswith("ld") and any(r in A64_SP for r in data):
            raise ValueError(f"stack pointer loaded: {what}")
        m = re.fullmatch(r"\[([^\[\]]*)\](!?)", raw_ops[n_data])
        if not m:
            raise ValueError(f"unbracketed or malformed memory operand: {what}")
        inner = [p.strip() for p in m.group(1).split(",")] if m.group(1).strip() else []
        if not inner or any(not p for p in inner):
            raise ValueError(f"malformed memory address: {what}")
        base, pre = inner[0], m.group(2) == "!"
        post_op = raw_ops[n_data + 1] if len(raw_ops) == n_data + 2 else None
        if base in ("wsp", "w29"):
            raise ValueError(f"unmodelled stack address: {what}")
        if not A64_ADDR_BASE.fullmatch(base):
            raise ValueError(f"unmodelled base register {base!r}: {what}")
        if post_op is not None and not pre and mn not in A64_UNSCALED_LDST and len(inner) == 1 and _A64_IMM.fullmatch(post_op):
            mem = ("post", data, base, _num(post_op[1:]), None, True)
        elif post_op is None and len(inner) == 1 and not pre:
            mem = ("offset", data, base, 0, None, False)
        elif post_op is None and len(inner) == 2 and _A64_IMM.fullmatch(inner[1]) and not (pre and mn in A64_UNSCALED_LDST):
            mem = ("pre" if pre else "offset", data, base, _num(inner[1][1:]), None, pre)
        elif post_op is None and not pre and mn not in A64_PAIR_LDST and mn not in A64_UNSCALED_LDST and _parse_a64_index(inner[1:]) is not None:
            mem = ("index", data, base, 0, _parse_a64_index(inner[1:]), False)
        else:
            raise ValueError(f"unsupported addressing mode: {what}")
        return [("reg", r) for r in data] + [("mem", mem)], mem

    if len(raw_ops) not in A64_ARITY[mn]:
        raise ValueError(f"wrong operand count: {what}")
    parsed_ops = []
    for i, op in enumerate(raw_ops):
        if mn in ("adr", "adrp") and i == 1:
            if not _A64_TARGET.fullmatch(op):
                raise ValueError(f"unmodelled address target {op!r}: {what}")
            tm = re.fullmatch(r"#?(0x[0-9a-f]+)(?:\s*<[^>\[\]]*>)?", op)
            parsed_ops.append(("target", int(tm.group(1), 16) if tm else None))
            continue
        if mn in A64_COND_OP and i == len(raw_ops) - 1:
            if not _A64_CC.fullmatch(op):
                raise ValueError(f"unmodelled condition {op!r}: {what}")
            parsed_ops.append(("cond", op))
            continue
        if mn in A64_MOD_OP and len(raw_ops) == max(A64_ARITY[mn]) and i == len(raw_ops) - 1:
            if not _valid_a64_mod(mn, parsed_ops[-1][0], op):
                raise ValueError(f"unmodelled shift or extend {op!r}: {what}")
            parsed_ops.append(("mod", op))
            continue
        if _A64_IMM.fullmatch(op) or (mn in ("fmov", "fcmp") and i == 1 and re.fullmatch(r"#-?\d+(?:\.\d+)?", op)):
            if (mn, i) not in A64_IMM_OK:
                raise ValueError(f"invalid immediate operand: {what}")
            parsed_ops.append(("imm", _num(op[1:]) if _A64_IMM.fullmatch(op) else 0))
            continue
        if (mn, i) not in A64_IMM_REQ and _valid_a64_reg(mn, op):
            parsed_ops.append(("reg", op))
            continue
        raise ValueError(f"unmodelled operand {op!r}: {what}")
    return parsed_ops, None


def a64_addresses(mnemonic, operands, reg):
    """Effective addresses of one AArch64 instruction in LLDB's or llvm-objdump's syntax
    (`stp x29, x30, [sp, #-0x10]!`, `ldr x30, [sp], #0x10`, `ldr w8, [x9, x10, lsl #2]`).
    Uses the shared fail-closed instruction and operand grammar (parse_a64_plain). A post-indexed
    access is at the base; a pre-indexed or offset one at base plus offset. Use a64_new_sp for the
    stack pointer a pre-indexed access is judged against."""
    mn, ops = mnemonic.lower().strip(), _strip_a64_comment(operands.lower())
    what = f"{mn} {ops}".strip()
    if not mn:
        raise ValueError("empty instruction")
    kind, arg = control("aarch64", mn, ops)
    if kind is not None:
        if kind == "indirect":
            raise ValueError(f"indirect control transfer: {what}")
        if kind in ("call", "jmp", "jcc") and not _A64_TARGET.fullmatch(arg):
            raise ValueError(f"indirect or unparsed control target: {what}")
        return []
    _, mem = parse_a64_plain(mn, ops)
    if mem is None:
        return []
    mode, _, base_name, disp, index_info, _ = mem
    base = _reg(reg, base_name, A64_ADDR_BASE)
    if mode == "post":
        return [base & MASK]
    if mode in ("offset", "pre"):
        return [(base + disp) & MASK]
    idx_reg, ext_kind, ext_shift = index_info
    v = _reg(reg, "x" + idx_reg[1:] if idx_reg.startswith("w") else idx_reg, A64_REG)
    if idx_reg.startswith("w") or ext_kind in ("uxtw", "sxtw"):
        v &= 0xFFFFFFFF
        if ext_kind == "sxtw" and (v & 0x80000000):
            v -= 1 << 32
    return [(base + (v << ext_shift)) & MASK]


def a64_new_sp(mnemonic, operands, reg, sp):
    """The stack pointer an access of this instruction is judged against: the written-back one for
    a pre-indexed sp access (`stp x29, x30, [sp, #-0x90]!` allocates the frame first, then stores
    at the new sp, as stack_bound.py treats it), else `sp`."""
    mn, ops = mnemonic.lower().strip(), _strip_a64_comment(operands.lower())
    kind, _ = control("aarch64", mn, ops)
    if kind is not None:
        return sp
    _, mem = parse_a64_plain(mn, ops)
    if mem is None:
        return sp
    mode, _, base_name, disp, _, _ = mem
    if mode != "pre" or base_name != "sp":
        return sp
    return min(sp, (sp + disp) & MASK)


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

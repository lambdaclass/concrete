# gdb -batch -nx -x stack_measure.py EXE (README gate 12, x86-64 Linux). PROVISIONAL.
#
# Measures the stack depth one run of EXE reaches, below the stack pointer at `main`'s entry, by
# single-stepping every instruction of the Concrete-owned functions (STACK_RANGES: "LO:HI,..." in
# the executable's link-time addresses, from stack_bound.py --ranges; STACK_MAIN: main's LO).
# An external function (entered through a call, so its return address is at the stack pointer)
# runs at full speed to its return; its own use is outside the bound, as in stack_bound.py, but
# the depth at its entry is measured. Once main has returned, the rest runs freely. Prints
# `measured N` and `steps N`, or `error ...` (the gate fails on it).
import os

import gdb

LIMIT = 2_000_000
ranges = [tuple(int(x, 16) for x in r.split(":")) for r in os.environ["STACK_RANGES"].split(",")]
main_lo = int(os.environ["STACK_MAIN"], 16)
exited = [False]
gdb.events.exited.connect(lambda _e: exited.__setitem__(0, True))
gdb.execute("set pagination off")
gdb.execute("set confirm off")
gdb.execute("set disable-randomization on")


def reg(name):
    return int(gdb.parse_and_eval(f"(unsigned long) ${name}"))


try:
    gdb.execute("break *main", to_string=True)
    gdb.execute("run </dev/null >/dev/null 2>/dev/null", to_string=True)
    if exited[0]:
        raise RuntimeError("exited before main")
    offset = reg("pc") - main_lo
    sp0 = reg("sp")
    low, steps = sp0, 0
    inferior = gdb.selected_inferior()
    while not exited[0]:
        pc, sp = reg("pc"), reg("sp")
        if sp > sp0:  # main has returned
            gdb.execute("continue", to_string=True)
            continue
        low = min(low, sp)
        if any(lo + offset <= pc <= hi + offset for lo, hi in ranges):
            gdb.execute("stepi", to_string=True)
            steps += 1
            if steps > LIMIT:
                raise RuntimeError(f"more than {LIMIT} steps")
        else:
            ret = int.from_bytes(inferior.read_memory(sp, 8).tobytes(), "little")
            gdb.execute("delete", to_string=True)
            gdb.execute(f"tbreak *{ret:#x}", to_string=True)
            gdb.execute("continue", to_string=True)
    print(f"measured {sp0 - low}")
    print(f"steps {steps}")
except Exception as e:  # noqa: BLE001 - every failure is reported, the gate fails closed
    print(f"error {e}")

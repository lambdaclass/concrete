# gdb -batch -nx -x stack_measure.py EXE (README gate 12, x86-64 Linux). PROVISIONAL.
#
# Measures the stack depth one complete run of EXE reaches, below the stack pointer at `main`'s
# entry, by single-stepping every instruction of the Concrete-owned functions (STACK_RANGES:
# "LO:HI,..." in the executable's link-time addresses, from stack_bound.py --ranges; STACK_MAIN:
# main's LO). An external function (entered through a call, so its return address is at the stack
# pointer) runs at full speed to that return address; its own use is outside the bound, as in
# stack_bound.py, but the depth at its entry is measured. Once main has returned, the rest runs
# freely.
#
# Fail closed: the measurement counts only for a run that stopped first at main's runtime address,
# stopped after each external call exactly at its return address (or exited inside it), received
# no signal, and exited normally with status STACK_EXPECT. Prints `measured N` and `steps N`, or
# `error ...` (the gate fails on anything but a measurement).
import os

import gdb

LIMIT = 2_000_000
ranges = [tuple(int(x, 16) for x in r.split(":")) for r in os.environ["STACK_RANGES"].split(",")]
main_lo = int(os.environ["STACK_MAIN"], 16)
expect = int(os.environ["STACK_EXPECT"])
state = {"exit": None, "signal": None}


def on_exit(event):
    state["exit"] = event.exit_code if hasattr(event, "exit_code") else "unknown"


def on_stop(event):
    if isinstance(event, gdb.SignalEvent):
        state["signal"] = event.stop_signal


gdb.events.exited.connect(on_exit)
gdb.events.stop.connect(on_stop)
gdb.execute("set pagination off")
gdb.execute("set confirm off")
gdb.execute("set disable-randomization on")


def reg(name):
    return int(gdb.parse_and_eval(f"(unsigned long) ${name}"))


def live():
    if state["signal"] is not None:
        raise RuntimeError(f"signal {state['signal']}")
    return state["exit"] is None


try:
    gdb.execute("break *main", to_string=True)
    gdb.execute("run </dev/null >/dev/null 2>/dev/null", to_string=True)
    if not live():
        raise RuntimeError("exited before main")
    main_rt = int(gdb.parse_and_eval("(unsigned long) &main"))
    if reg("pc") != main_rt:
        raise RuntimeError(f"first stop at {reg('pc'):#x}, not main ({main_rt:#x})")
    offset = main_rt - main_lo
    sp0 = reg("sp")
    low, steps = sp0, 0
    inferior = gdb.selected_inferior()
    gdb.execute("delete", to_string=True)
    while live():
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
            if not any(lo + offset <= ret <= hi + offset for lo, hi in ranges):
                raise RuntimeError(f"external code at {pc:#x} returns to {ret:#x}, outside the owned code")
            gdb.execute(f"tbreak *{ret:#x}", to_string=True)
            gdb.execute("continue", to_string=True)
            if live() and reg("pc") != ret:
                raise RuntimeError(f"stopped at {reg('pc'):#x}, not the return address {ret:#x}")
    if state["exit"] != expect:
        raise RuntimeError(f"exit status {state['exit']}, expected {expect}")
    if steps == 0:
        raise RuntimeError("no instruction stepped")
    print(f"measured {sp0 - low}")
    print(f"steps {steps}")
except Exception as e:  # noqa: BLE001 - every failure is reported, the gate fails closed
    print(f"error {e}")

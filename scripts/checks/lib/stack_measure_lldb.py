# lldb --batch -o 'command script import stack_measure_lldb.py' (README gate 12, aarch64-macos).
# PROVISIONAL.
#
# The LLDB counterpart of stack_measure.py (same contract, same environment: STACK_EXE, the
# executable; STACK_RANGES "LO:HI,..." in link-time addresses, from stack_bound.py --ranges;
# STACK_MAIN, main's LO; STACK_EXPECT, the expected exit status; STACK_RED_ZONE; STACK_LIB, the
# directory of stack_access.py). It measures the stack depth one complete run reaches, below the
# stack pointer at `main`'s entry: the lowest of the stack pointer and every stack byte an
# instruction addresses (so the red zone), by single-stepping every instruction of the
# Concrete-owned functions. An external function (entered by `bl`, so its
# return address is in the link register) runs at full speed to that return address; its own use
# is outside the bound, as in stack_bound.py, but the depth at its entry is measured. Once main has
# returned, the rest runs freely.
#
# Fail closed: the measurement counts only for a run that stopped first at main's runtime address,
# stopped after each external call exactly at its return address (or exited inside it), received
# no signal, exited normally with status STACK_EXPECT, and addressed no stack byte deeper than the
# red zone below the stack pointer. Prints `measured N` and `steps N`, or
# `error ...` (the gate fails on anything but a measurement).
import os
import sys

import lldb

sys.path.insert(0, os.environ["STACK_LIB"])
import stack_access  # noqa: E402 - the path is the gate's lib directory

LIMIT = 2_000_000
SPAN = 8 << 20  # stack bytes counted below main's entry (stack_access.lowest_stack)


def measure(debugger):
    exe = os.environ["STACK_EXE"]
    ranges = [tuple(int(x, 16) for x in r.split(":")) for r in os.environ["STACK_RANGES"].split(",")]
    main_lo = int(os.environ["STACK_MAIN"], 16)
    expect = int(os.environ["STACK_EXPECT"])
    debugger.SetAsync(False)
    target = debugger.CreateTarget(exe)
    if not target.IsValid():
        raise RuntimeError(f"cannot load {exe}")
    # Break at main's exact first instruction (a name breakpoint would skip its prologue, after
    # the frame is allocated, and misplace both sp0 and the slide).
    syms = [c.GetSymbol() for c in target.FindSymbols("main") if c.GetSymbol().IsValid()]
    syms = [s for s in syms if s.GetType() == lldb.eSymbolTypeCode]
    if len(syms) != 1:
        raise RuntimeError(f"{len(syms)} code symbols named main")
    entry = syms[0].GetStartAddress()
    bp = target.BreakpointCreateBySBAddress(entry)
    if bp.GetNumLocations() != 1:
        raise RuntimeError(f"main has {bp.GetNumLocations()} breakpoint locations")
    info = lldb.SBLaunchInfo([])
    info.SetLaunchFlags(lldb.eLaunchFlagDisableASLR)
    for fd, flags in ((0, os.O_RDONLY), (1, os.O_WRONLY), (2, os.O_WRONLY)):
        info.AddOpenFileAction(fd, "/dev/null", flags == os.O_RDONLY, flags != os.O_RDONLY)
    err = lldb.SBError()
    process = target.Launch(info, err)
    if not err.Success() or not process.IsValid():
        raise RuntimeError(f"launch failed: {err}")
    try:
        # Load addresses exist only once the process is loaded.
        main_rt = entry.GetLoadAddress(target)
        if main_rt == lldb.LLDB_INVALID_ADDRESS:
            raise RuntimeError("main has no load address")
        return run(process, target, bp, ranges, main_lo, main_rt, expect)
    finally:
        if process.GetState() not in (lldb.eStateExited, lldb.eStateDetached, lldb.eStateInvalid):
            process.Kill()


def check_live(process):
    """True while the process is stopped (not exited); raises on a signal or another stop."""
    state = process.GetState()
    if state == lldb.eStateExited:
        return False
    if state != lldb.eStateStopped:
        raise RuntimeError(f"unexpected process state {state}")
    thread = process.GetSelectedThread()
    if thread.GetStopReason() not in (lldb.eStopReasonBreakpoint, lldb.eStopReasonPlanComplete,
                                      lldb.eStopReasonTrace):
        raise RuntimeError(f"unexpected stop ({thread.GetStopReason()}): {thread.GetStopDescription(200)}")
    return True


def register(frame, name):
    """A general register's value (`sp`, `x0`..`x30`, `fp`, `lr`); unknown names fail closed."""
    v = frame.FindRegister(name)
    if not v.IsValid() and name in ("x29", "x30"):
        v = frame.FindRegister({"x29": "fp", "x30": "lr"}[name])
    if not v.IsValid():
        raise ValueError(f"no register {name}")
    return v.GetValueAsUnsigned()


def run(process, target, bp, ranges, main_lo, main_rt, expect):
    red_zone = int(os.environ["STACK_RED_ZONE"])
    if not check_live(process):
        raise RuntimeError("exited before main")
    thread = process.GetSelectedThread()
    frame = thread.GetFrameAtIndex(0)
    if frame.GetPC() != main_rt:
        raise RuntimeError(f"first stop at {frame.GetPC():#x}, not main ({main_rt:#x})")
    target.BreakpointDelete(bp.GetID())
    offset = main_rt - main_lo
    sp0 = frame.GetSP()
    # main's own return address: `bl` leaves it in the link register and the stack pointer unchanged,
    # so main has returned when the pc reaches it with the stack pointer back at its entry value.
    caller = frame.FindRegister("lr").GetValueAsUnsigned()
    low, steps, returned = sp0, 0, False

    def owned(pc):
        return any(lo + offset <= pc <= hi + offset for lo, hi in ranges)

    while check_live(process):
        thread = process.GetSelectedThread()
        frame = thread.GetFrameAtIndex(0)
        pc, sp = frame.GetPC(), frame.GetSP()
        if returned or sp > sp0 or (pc == caller and sp == sp0):  # main has returned
            returned = True
            process.Continue()
            continue
        low = min(low, sp)
        if owned(pc):
            insn = target.ReadInstructions(lldb.SBAddress(pc, target), 1).GetInstructionAtIndex(0)
            mn, ops = insn.GetMnemonic(target), insn.GetOperands(target)
            try:
                touched = stack_access.lowest_stack(
                    stack_access.a64_addresses(mn, ops, lambda r: register(frame, r)),
                    sp, sp0, red_zone, SPAN)
            except ValueError as e:
                raise RuntimeError(f"at {pc:#x} ({mn} {ops}): {e}") from None
            if touched is not None:
                low = min(low, touched)
            thread.StepInstruction(False)
            steps += 1
            if steps > LIMIT:
                raise RuntimeError(f"more than {LIMIT} steps")
        else:
            ret = frame.FindRegister("lr").GetValueAsUnsigned()
            if not owned(ret):
                raise RuntimeError(f"external code at {pc:#x} returns to {ret:#x}, outside the owned code")
            tb = target.BreakpointCreateByAddress(ret)
            tb.SetOneShot(True)
            process.Continue()
            if check_live(process) and process.GetSelectedThread().GetFrameAtIndex(0).GetPC() != ret:
                raise RuntimeError(f"stopped at {process.GetSelectedThread().GetFrameAtIndex(0).GetPC():#x}, "
                                   f"not the return address {ret:#x}")
            target.BreakpointDelete(tb.GetID())
    status = process.GetExitStatus()
    if status != expect:
        raise RuntimeError(f"exit status {status}, expected {expect}")
    if steps == 0:
        raise RuntimeError("no instruction stepped")
    return sp0 - low, steps


def __lldb_init_module(debugger, _dict):
    try:
        measured, steps = measure(debugger)
        print(f"measured {measured}")
        print(f"steps {steps}")
    except Exception as e:  # noqa: BLE001 - every failure is reported, the gate fails closed
        print(f"error {e}")

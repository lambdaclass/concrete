# Interrupt and Signal Model

**Status:** Open

This note defines the future direction for interrupts, signals, and other externally triggered control transfers in Concrete.

## Why This Matters

Concrete currently targets hosted POSIX-like environments. That already implies interaction with:

1. process signals
2. hardware traps surfaced by the OS
3. later, possibly interrupts on freestanding targets

If Concrete wants to support high-integrity, embedded, or predictable-execution profiles, these boundaries need an explicit design.

## The Core Problem

Interrupts and signals break simple local reasoning because they introduce:

1. externally triggered control flow
2. reentrancy hazards
3. shared-state visibility issues
4. timing and scheduling interference

That makes them different from ordinary function calls, even if the surface syntax is small.

## Hosted Reality

Today, Concrete already lives with several externally triggered failure/control boundaries:

1. segmentation faults
2. stack overflow traps
3. abort-driven process termination
4. process signals at the OS level

These exist whether or not the language gives them surface syntax.

## First Design Direction

Concrete should probably start with a deliberately narrow rule:

1. no language-level signal or interrupt handlers in the predictable/high-integrity profile
2. hosted signals are treated as outside the ordinary language model
3. if signal support exists for hosted code, it should remain explicitly unsafe and operationally restricted

This matches the existing "small explicit core first" philosophy.

## Freestanding Direction

If Concrete later targets freestanding or embedded systems, it will need a more explicit interrupt story.

The likely design questions are:

1. are interrupt handlers ordinary functions under a special profile?
2. what capabilities are allowed inside them?
3. is allocation forbidden?
4. is blocking forbidden?
5. what shared-state discipline is required?

The predictable-execution direction strongly suggests the answer will need to be restrictive.

## Relationship To Other Notes

This note connects to:

1. [high-integrity-profile.md](high-integrity-profile.md)
2. [../predictable-execution/predictable-execution.md](../predictable-execution/predictable-execution.md)
3. [../predictable-execution/analyzable-concurrency.md](../predictable-execution/analyzable-concurrency.md)
4. [../stdlib-runtime/no-std-freestanding.md](../stdlib-runtime/no-std-freestanding.md)

## Bottom Line

Concrete does not need a full interrupt model now. It does need an explicit design stance:

1. hosted signals are outside the normal language reasoning model
2. predictable/high-integrity profiles should forbid handler-style complexity at first
3. a later freestanding interrupt model must be designed as a restricted execution environment, not as ordinary callback programming

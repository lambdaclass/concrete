### Bug 263: deterministic fuzz-farm gate coverage

Add opt-in complete-batch campaigns with a nonzero incomplete result when the deadline expires.
The fuzz-farm gate now requests fixed work instead of assuming that 2–8 seconds cover every
generator and the drift threshold. Preserve timed production campaigns and all existing
end-to-end assertions; add virtual-clock controls for speed independence and partial completion.

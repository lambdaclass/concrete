"""Fake linearity generator for check_fuzz_farm.sh (same interface as
scripts/tests/fuzz_linearity.py: gen_cases(rng, steps) -> (kind, src, must_compile)).
A chain threads one Res through `let`/`pass` steps with Copy noise; the fake compiler
misses a leak whenever the chain contains `pass(` (the planted unsoundness)."""

HDR = "struct Res { tag: Int }\n"


def gen_cases(rng, steps):
    lines, n = ["    let v1: Res = mk(1);"], 1
    for _ in range(steps + 2):
        n += 1
        if rng.random() < 0.5:
            lines.append(f"    let v{n}: Res = pass(v{n - 1});")
        else:
            lines.append(f"    let v{n}: Res = v{n - 1};")
        if rng.random() < 0.4:
            lines.append(f"    let k{n}: Int = {rng.randrange(9)};")
    live = f"v{n}"
    prog = lambda term: HDR + "fn main() -> Int {\n" + "\n".join(lines + term) + "\n}\n"
    yield ("consume-once", prog([f"    return use_res({live});"]), True)
    yield ("leak", prog(["    return 0;"]), False)

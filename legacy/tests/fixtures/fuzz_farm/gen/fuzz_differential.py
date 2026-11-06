"""Fake differential generator for check_fuzz_farm.sh (same interface as
scripts/tests/fuzz_differential.py: Gen(rng, depth).program()).

FAKE_GEN_MODE selects what it plants: 'bugs' (BUG/CRASHME lines in some programs),
'drift' (most programs carry REJECTME), 'clean' (nothing)."""
import os


class Gen:
    def __init__(self, rng, depth):
        self.rng = rng
        self.depth = depth

    def program(self):
        r = self.rng
        mode = os.environ.get("FAKE_GEN_MODE", "bugs")
        lines = ["mod m {", "    fn main() with(Console) {"]
        for _ in range(r.randint(4, 9)):
            lines.append("        " + r.choice(["inc;", "nop;", "inc; nop;", "if c { inc; } else { nop; }"]))
        roll = r.random()
        if mode == "bugs" and roll < 0.3:
            lines.insert(r.randint(2, len(lines)), "        BUG;")
        elif mode == "bugs" and roll < 0.4:
            lines.insert(r.randint(2, len(lines)), "        CRASHME;")
        elif mode == "drift" and roll < 0.8:
            lines.insert(r.randint(2, len(lines)), "        REJECTME;")
        lines += ["    }", "}"]
        return "\n".join(lines) + "\n"

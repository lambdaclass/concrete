#!/usr/bin/env python3
"""Continuous fuzz farm: campaigns, classification, minimization, dedup, re-check.

This is the engine behind scripts/fuzz/farm/farm.sh. It REUSES the repository's
generators (scripts/tests/fuzz_differential.py `Gen`, scripts/tests/fuzz_linearity.py
`gen_cases`), loaded by file path from a snapshot of origin/main, but owns its own
execution, oracles and bookkeeping, so it keeps running when a gate changes shape and
never stops at the first failure.

Generators (rotated per batch):
  diff  random well-typed program; oracle = `--interp` vs the compiled binary
  lin   linearity chain with ground truth (must compile / must reject)
  mut   a diff program with 1-3 token mutations; oracle = the compiler must not crash
        or hang (any clean diagnostic is fine)

Classes (priority order):
  silent    interp and native both ran and disagree (value/value or value/trap)
  crash     compiler or interpreter panic, signal, internal error, SSA-verify (E07xx)
  unsound   the checker accepted a program the oracle says must be rejected
  timeout   a side hung (re-run at 4x the budget before it is recorded)
  refusal   a program the generator believes valid was rejected (fail-closed). Batches
            whose refusal rate exceeds the drift threshold are reported as a
            generator-drift alert instead of as findings.

Signature = class | oracle | normalized shape | structure hash of the minimized program.
Findings live in $FARM_HOME/findings/<sig>/ with repro.con, minimized.con, meta.json, log,
indexed by $FARM_HOME/findings/findings.json.

Subcommands: campaign, classify, recheck, summary, status, promote, predicate, repro.
"""
import argparse, contextlib, datetime, fcntl, glob, hashlib, importlib.util, json, os
import random, re, resource, shutil, signal, subprocess, sys, tempfile, time

HERE = os.path.dirname(os.path.abspath(__file__))
FARM_HOME = os.environ.get("FARM_HOME", "/home/fuzz")
CLASSES = ["silent", "crash", "unsound", "timeout", "refusal"]
NOTIFY_CLASSES = {"silent", "crash", "unsound"}
DRIFT_THRESHOLD = float(os.environ.get("FARM_DRIFT_THRESHOLD", "0.30"))
DRIFT_MIN_CASES = int(os.environ.get("FARM_DRIFT_MIN_CASES", "20"))
# timeouts (seconds); a timeout is re-run once at TIMEOUT_RETRY_FACTOR x before it counts
T_COMPILE = float(os.environ.get("FARM_T_COMPILE", "60"))
T_RUN = float(os.environ.get("FARM_T_RUN", "10"))
T_INTERP = float(os.environ.get("FARM_T_INTERP", "30"))
TIMEOUT_RETRY_FACTOR = 4
MAX_VARIANTS_PER_PRESIG = 5     # distinct minimized structures kept per pre-signature
RESAMPLE_EVERY = 25             # re-minimize every Nth repeat of a known pre-signature
MAX_OCCURRENCES = 50            # seeds remembered per signature
LINEARITY_CODES = ["E0205", "E0206", "E0207", "E0208", "E0209", "E0210", "E0211",
                   "E0212", "E0213", "E0219", "E0287", "E0288", "E0290", "E0291",
                   "E0292", "E0301"]


def now_iso():
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def today():
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d")


def paths(home=None):
    h = home or FARM_HOME
    return {"home": h, "findings": os.path.join(h, "findings"),
            "index": os.path.join(h, "findings", "findings.json"),
            "lock": os.path.join(h, "findings", ".lock"),
            "state": os.path.join(h, "state"), "stats": os.path.join(h, "state", "stats"),
            "drift": os.path.join(h, "state", "drift.json"),
            "pause": os.path.join(h, "state", "paused"),
            "reports": os.path.join(h, "reports"), "tmp": os.path.join(h, "tmp"),
            "public": os.environ.get("FARM_PUBLIC", os.path.join(h, "public"))}


def ensure_dirs(p):
    for k in ("findings", "state", "stats", "reports", "tmp", "public"):
        os.makedirs(p[k], exist_ok=True)


@contextlib.contextmanager
def locked(p):
    os.makedirs(p["findings"], exist_ok=True)
    with open(p["lock"], "a+") as fh:
        fcntl.flock(fh, fcntl.LOCK_EX)
        try:
            yield
        finally:
            fcntl.flock(fh, fcntl.LOCK_UN)


def load_json(path, default):
    try:
        with open(path) as fh:
            return json.load(fh)
    except (OSError, ValueError):
        return default


def save_json(path, obj):
    tmp = f"{path}.tmp.{os.getpid()}"
    with open(tmp, "w") as fh:
        json.dump(obj, fh, indent=1, sort_keys=True)
        fh.write("\n")
    os.replace(tmp, path)


def empty_index():
    return {"version": 1, "signatures": {}, "presig": {}, "pending": {}}


# --------------------------------------------------------------------------- generators

def load_module(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class Generators:
    """The repo's generators, loaded from a snapshot's scripts/tests directory."""

    def __init__(self, gen_dir):
        self.gen_dir = gen_dir
        self.diff = load_module(os.path.join(gen_dir, "fuzz_differential.py"), "farm_gen_diff")
        self.lin = load_module(os.path.join(gen_dir, "fuzz_linearity.py"), "farm_gen_lin")

    def make(self, generator, seed, depth=3):
        """Return a list of cases (dict oracle/src/params) for one seed. Deterministic."""
        if generator == "diff":
            src = self.diff.Gen(random.Random(seed), depth).program()
            return [{"oracle": "diff", "src": src, "params": {}}]
        if generator == "lin":
            rng = random.Random(seed)
            steps = rng.randrange(1, 7)
            cases = list(self.lin.gen_cases(rng, steps))
            ok_src = next(s for k, s, m in cases if k == "consume-once")
            return [{"oracle": "lin", "src": s,
                     "params": {"kind": k, "must_compile": m, "ok_src": ok_src}}
                    for k, s, m in cases]
        if generator == "mut":
            rng = random.Random(seed)
            src = self.diff.Gen(random.Random(rng.getrandbits(64)), depth).program()
            return [{"oracle": "mut", "src": mutate(src, rng), "params": {}}]
        raise ValueError(f"unknown generator {generator}")


TOKEN_RE = re.compile(r"[A-Za-z_][A-Za-z0-9_]*|\d+|\"(?:[^\"\\]|\\.)*\"|==|!=|<=|>=|=>|->|::|&&|\|\||\S")


def mutate(src, rng):
    """1-3 token edits on one line; the other lines keep their layout (readable repros)."""
    lines = src.split("\n")
    cand = [i for i, l in enumerate(lines) if l.strip()]
    li = rng.choice(cand)
    indent = lines[li][:len(lines[li]) - len(lines[li].lstrip())]
    toks = TOKEN_RE.findall(lines[li])
    for _ in range(rng.randint(1, 3)):
        if not toks:
            break
        i = rng.randrange(len(toks))
        op = rng.randrange(4)
        if op == 0:
            del toks[i]
        elif op == 1:
            toks.insert(i, toks[i])
        elif op == 2:
            j = rng.randrange(len(toks))
            toks[i], toks[j] = toks[j], toks[i]
        else:
            toks[i] = rng.choice(["{", "}", "(", ")", ";", "&mut", "if", "match", "0",
                                  "as", "i8", "u32", "while", "defer", "=", "<", "-1"])
    lines[li] = indent + " ".join(toks)
    return "\n".join(lines)


# --------------------------------------------------------------------------- execution

class InfraError(Exception):
    """The farm cannot run the compiler/binary at all (missing file, exec format...)."""


def _limits():
    os.setsid()
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))


def run_cmd(argv, timeout, cwd):
    """(rc, stdout, stderr, timed_out). rc < 0 means killed by a signal."""
    try:
        p = subprocess.Popen(argv, cwd=cwd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                             stdin=subprocess.DEVNULL, preexec_fn=_limits)
    except OSError as e:  # the farm's own environment is broken: never a finding
        raise InfraError(f"cannot run {argv[0]}: {e}")
    try:
        out, err = p.communicate(timeout=timeout)
        return (p.returncode, out.decode("utf-8", "replace"), err.decode("utf-8", "replace"), False)
    except subprocess.TimeoutExpired:
        with contextlib.suppress(ProcessLookupError):
            os.killpg(p.pid, signal.SIGKILL)
        out, err = p.communicate()
        return (None, out.decode("utf-8", "replace"), err.decode("utf-8", "replace"), True)


def run_retry(argv, timeout, cwd):
    r = run_cmd(argv, timeout, cwd)
    if r[3]:  # a hang under load is re-run with a larger budget before it counts
        r = run_cmd(argv, timeout * TIMEOUT_RETRY_FACTOR, cwd)
    return r


# A Lean `panic!` prints "PANIC at ..."; an escaped IO error prints "uncaught exception";
# E07xx are SSA-verifier refusals of the compiler's OWN output (never a user error).
CRASH_MARKERS = ["PANIC", "internal error", "uncaught exception", "Stack overflow",
                 "stack overflow", "ssa-verify", "Segmentation fault"]
E07_RE = re.compile(r"\(E07\d\d\)")
CODE_RE = re.compile(r"\((E\d{4})\)")


def crash_line(text):
    """The first line that marks a compiler/interpreter crash, or None."""
    for line in text.splitlines():
        if any(m in line for m in CRASH_MARKERS) or E07_RE.search(line):
            return line
    return None


def norm(s):
    """Normalize a diagnostic / trap message for signatures."""
    s = re.sub(r"(/[^\s:]+)+", "<path>", s)
    s = re.sub(r"\b[\w.-]+\.con\b", "<file>", s)
    s = re.sub(r":\d+:\d+", "", s)
    s = re.sub(r"'[^']*'|`[^`]*`|\"[^\"]*\"", "'X'", s)
    s = re.sub(r"\b(v|k|a|s|e|f|w|str)\d+(_in\d+)?\b", "ID", s)
    s = re.sub(r"\b0x[0-9a-fA-F]+\b", "N", s)
    s = re.sub(r"\b\d+\b", "N", s)
    return re.sub(r"\s+", " ", s).strip()[:200]


def first_error(text):
    for line in text.splitlines():
        if "error" in line.lower():
            return line
    lines = [l for l in text.splitlines() if l.strip()]
    return lines[0] if lines else ""


class Result:
    """Outcome of classifying one program. cls None = uninteresting (agree / expected)."""

    def __init__(self, cls, shape, detail, outcome):
        self.cls, self.shape, self.detail, self.outcome = cls, shape, detail, outcome

    def presig(self, oracle):
        # A compiler crash is the same bug whichever generator reached it.
        if self.cls == "crash":
            return f"{self.cls}|{self.shape}"
        return f"{self.cls}|{oracle}|{self.shape}"


def compile_prog(compiler, src, wd):
    path = os.path.join(wd, "case.con")
    with open(path, "w") as fh:
        fh.write(src)
    binp = os.path.join(wd, "case.bin")
    with contextlib.suppress(OSError):
        os.remove(binp)
    rc, out, err, to = run_retry([compiler, path, "-o", binp], T_COMPILE, wd)
    return path, binp, rc, out, err, to


def classify(oracle, src, params, compiler, wd):
    os.makedirs(wd, exist_ok=True)
    path, binp, rc, out, err, to = compile_prog(compiler, src, wd)
    both = out + "\n" + err
    codes = sorted(set(CODE_RE.findall(both)))
    d = {"compile": {"rc": rc, "stdout": out[-4000:], "stderr": err[-4000:], "timeout": to},
         "codes": codes}
    if to:
        return Result("timeout", "compile", d, "timeout")
    cl = crash_line(both)
    if (rc is not None and rc < 0) or cl:
        what = norm(cl) if cl else f"signal {-rc}"
        return Result("crash", f"compile:{what}", d, "crash")
    accepted = rc == 0
    if oracle == "mut":
        return Result(None, "", d, "ok")
    if oracle == "lin":
        must = params["must_compile"]
        kind = params["kind"]
        if must and not accepted:
            return Result("refusal", f"lin-overreject:{kind}:{','.join(codes)}:{norm(first_error(both))}",
                          d, "refusal")
        if not must and accepted:
            return Result("unsound", f"lin-accepted:{kind}", d, "unsound")
        if not must and not any(c in LINEARITY_CODES for c in codes):
            return Result("refusal", f"lin-offtarget:{kind}:{','.join(codes)}", d, "refusal")
        return Result(None, "", d, "ok")
    # diff
    if not accepted:
        return Result("refusal", f"diff-reject:{','.join(codes)}:{norm(first_error(both))}",
                      d, "refusal")
    rrc, rout, rerr, rto = run_retry([binp], T_RUN, wd)
    irc, iout, ierr, ito = run_retry([compiler, path, "--interp"], T_INTERP, wd)
    d["native"] = {"rc": rrc, "stdout": rout[-4000:], "stderr": rerr[-2000:], "timeout": rto}
    d["interp"] = {"rc": irc, "stdout": iout[-4000:], "stderr": ierr[-2000:], "timeout": ito}
    icl = crash_line(iout + "\n" + ierr)
    if (irc is not None and irc < 0) or icl:
        return Result("crash", f"interp:{norm(icl) if icl else f'signal {-irc}'}", d, "crash")
    iall = iout + "\n" + ierr
    if any(u in iall for u in ("not yet supported", "unsupported", "not supported")):
        return Result(None, "", d, "pending")
    if rto or ito:
        if rto and ito:  # both hang: agreement (e.g. a genuine infinite loop)
            return Result(None, "", d, "agree")
        return Result("timeout", "native" if rto else "interp", d, "timeout")
    nk = "value" if rrc == 0 else "trap"
    ik = "value" if irc == 0 else "trap"
    if nk == ik == "trap":
        return Result(None, "", d, "agree")
    if nk == ik == "value":
        if rout.strip() == iout.strip():
            return Result(None, "", d, "agree")
        a, b = rout.strip().splitlines(), iout.strip().splitlines()
        diff = [i for i in range(max(len(a), len(b)))
                if (a[i] if i < len(a) else None) != (b[i] if i < len(b) else None)]
        return Result("silent", f"value/value:lines={','.join(map(str, diff[:4]))}", d, "silent")
    if nk == "trap":
        return Result("silent", f"native=trap(rc{rrc})/interp=value", d, "silent")
    msg = next((l for l in ierr.splitlines() if "interp:" in l), first_error(ierr))
    return Result("silent", f"native=value/interp=trap:{norm(msg)}", d, "silent")


# --------------------------------------------------------------------------- structure hash

KEYWORDS = set("""mod fn let mut if else match while for return struct enum impl Copy true false
as defer with break continue self Self Int i8 i16 i32 i64 u8 u16 u32 u64 String Res Wrap Holder
Console println print_int string_push_char string_length drop_string use_res pass mk addv bump
Has Empty""".split())


def structure_hash(src):
    """Hash of the token shape: literals erased, identifiers renamed by first appearance."""
    names, out = {}, []
    for t in TOKEN_RE.findall(re.sub(r"//[^\n]*", "", src)):
        if t[0].isdigit():
            out.append("N")
        elif t[0] == '"':
            out.append("S")
        elif re.match(r"[A-Za-z_]", t) and t not in KEYWORDS:
            out.append(names.setdefault(t, f"x{len(names)}"))
        else:
            out.append(t)
    return hashlib.sha256(" ".join(out).encode()).hexdigest()[:12]


def signature(presig, min_src):
    h = hashlib.sha256(f"{presig}|{structure_hash(min_src)}".encode()).hexdigest()
    return f"{presig.split('|')[0]}-{h[:12]}"


# --------------------------------------------------------------------------- minimizer

def split_top(text, sep=";"):
    """Split `text` at top-level `sep` (outside (), {}, [] and strings), keeping separators."""
    parts, depth, cur, instr = [], 0, "", False
    for ch in text:
        cur += ch
        if ch == '"':
            instr = not instr
        elif not instr and ch in "({[":
            depth += 1
        elif not instr and ch in ")}]":
            depth -= 1
        elif not instr and ch == sep and depth == 0:
            parts.append(cur)
            cur = ""
    if cur:
        parts.append(cur)
    return parts


def balanced_end(s, i, open_ch, close_ch):
    depth = 0
    for j in range(i, len(s)):
        if s[j] == open_ch:
            depth += 1
        elif s[j] == close_ch:
            depth -= 1
            if depth == 0:
                return j
    return -1


def line_variants(line):
    """Smaller rewrites of one line: drop a statement, take one if-arm, shrink a group."""
    out = []
    indent = line[:len(line) - len(line.lstrip())]
    body = line.strip()
    stmts = split_top(body)
    if len(stmts) > 1:
        for i in range(len(stmts)):
            out.append(indent + "".join(stmts[:i] + stmts[i + 1:]).strip())
    for m in re.finditer(r"\bif\b", body):
        a0 = body.find("{", m.end())
        if a0 < 0:
            continue
        a1 = balanced_end(body, a0, "{", "}")
        if a1 < 0:
            continue
        rest = body[a1 + 1:]
        em = re.match(r"\s*else\s*", rest)
        if not em:
            continue
        b0 = a1 + 1 + em.end()
        if b0 >= len(body) or body[b0] != "{":
            continue
        b1 = balanced_end(body, b0, "{", "}")
        if b1 < 0:
            continue
        for arm in (body[a0 + 1:a1].strip(), body[b0 + 1:b1].strip()):
            out.append(indent + (body[:m.start()] + arm + body[b1 + 1:]).strip())
    for m in re.finditer(r"\(", body):
        e = balanced_end(body, m.start(), "(", ")")
        if e < 0:
            continue
        inner = body[m.start() + 1:e]
        ops = split_top(inner, "%")
        if len(ops) == 2:  # `(X % B)` -> `X`
            out.append(indent + body[:m.start()] + "(" + ops[0][:-1].strip() + ")" + body[e + 1:])
        for op in "+-*":
            parts = split_top(inner, op)
            if len(parts) == 2 and parts[0].strip() and parts[1].strip():
                for keep in (parts[0][:-1], parts[1]):
                    out.append(indent + body[:m.start()] + "(" + keep.strip() + ")" + body[e + 1:])
    for m in re.finditer(r"\b[2-9]\d*\b", body):  # literals -> 1
        out.append(indent + body[:m.start()] + "1" + body[m.end():])
    toks = TOKEN_RE.findall(body)
    if len(toks) <= 60:  # last resort: drop single tokens (mutated/unparseable lines)
        for i in range(len(toks)):
            out.append(indent + " ".join(toks[:i] + toks[i + 1:]))
    seen, uniq = set(), []
    for v in out:
        if v != line and v not in seen:
            seen.add(v)
            uniq.append(v)
    return uniq


class Budget:
    def __init__(self, seconds, max_tests):
        self.deadline = time.time() + seconds
        self.left = max_tests
        self.tests = 0

    def ok(self):
        return time.time() < self.deadline and self.left > 0

    def spend(self):
        self.left -= 1
        self.tests += 1


def ddmin_lines(units, test, budget):
    """Remove chunks of units (halving chunk size) while test(units) holds."""
    n = 2
    while len(units) >= 1 and budget.ok():
        chunk = max(1, len(units) // n)
        removed = False
        i = 0
        while i < len(units) and budget.ok():
            cand = units[:i] + units[i + chunk:]
            budget.spend()
            if cand != units and test(cand):
                units = cand
                removed = True
            else:
                i += chunk
        if not removed:
            if chunk == 1:
                break
            n *= 2
    return units


def minimize_text(src, holds, budget):
    """Generic delta reduction of a program: lines, then intra-line rewrites, to fixpoint."""
    units = src.split("\n")
    test = lambda us: holds("\n".join(us))
    changed = True
    while changed and budget.ok():
        changed = False
        before = list(units)
        units = ddmin_lines(units, test, budget)
        for i in range(len(units)):
            progress = True
            while progress and budget.ok() and i < len(units):
                progress = False
                for v in line_variants(units[i]):
                    if not budget.ok():
                        break
                    cand = units[:i] + ([v] if v.strip() else []) + units[i + 1:]
                    budget.spend()
                    if test(cand):
                        units = cand
                        progress = True
                        break
        changed = units != before
    return "\n".join(units)


LET_RES = re.compile(r"^\s*let (?:mut )?(\w+): (\w+) = (.*);\s*$")


def lin_split(bug_src, ok_src):
    """Split a linearity pair into header template, shared chain lines, and terminals."""
    def parts(s):
        head, _, rest = s.partition("fn main() -> Int {\n")
        body = rest.rsplit("\n}", 1)[0]
        return head, body.split("\n")
    bh, bl = parts(bug_src)
    oh, ol = parts(ok_src)
    k = 0
    while k < min(len(bl), len(ol)) and bl[k] == ol[k]:
        k += 1
    return {"bug_head": bh, "ok_head": oh, "chain": bl[:k], "bug_term": bl[k:], "ok_term": ol[k:]}


def lin_render(head, chain, term):
    return head + "fn main() -> Int {\n" + "\n".join(chain + term) + "\n}\n"


def minimize_lin(split, holds_bug, holds_ok, budget):
    """Pair minimization: the bug program must keep its class AND the consume-once twin over
    the same chain must still compile (so the chain is still a valid linear flow and the
    terminal is still a real violation). Moves: drop a line, or collapse a `let NEW = f(OLD)`
    step by renaming NEW to OLD downstream."""
    chain, bt, ot = list(split["chain"]), list(split["bug_term"]), list(split["ok_term"])

    def test(c, b, o):
        budget.spend()
        return (holds_bug(lin_render(split["bug_head"], c, b))
                and holds_ok(lin_render(split["ok_head"], c, o)))

    progress = True
    while progress and budget.ok():
        progress = False
        for i in range(len(chain) - 1, 0, -1):
            if not budget.ok():
                break
            if i >= len(chain):
                continue
            cand = chain[:i] + chain[i + 1:]
            if test(cand, bt, ot):
                chain, progress = cand, True
                continue
            m = LET_RES.match(chain[i])
            if not m:
                continue
            new = m.group(1)
            for old in re.findall(r"\b[a-z]\w*\b", m.group(3)):
                if not any(re.match(rf"^\s*let (?:mut )?{re.escape(old)}\b", l) for l in chain[:i]):
                    continue
                sub = lambda l: re.sub(rf"\b{re.escape(new)}\b", old, l)
                c2 = chain[:i] + [sub(l) for l in chain[i + 1:]]
                b2, o2 = [sub(l) for l in bt], [sub(l) for l in ot]
                if test(c2, b2, o2):
                    chain, bt, ot, progress = c2, b2, o2, True
                    break
    return lin_render(split["bug_head"], chain, bt), lin_render(split["ok_head"], chain, ot)


def lean_reduce(compiler, src, meta_path, wd, seconds):
    """Run the in-repo AST reducer (`concrete reduce`) with this farm as external predicate."""
    inp = os.path.join(wd, "lean_in.con")
    outp = os.path.join(wd, "lean_out.con")
    with open(inp, "w") as fh:
        fh.write(src)
    pred = f"external:{os.path.join(HERE, 'predicate.sh')} {meta_path}"
    rc, _, _, to = run_cmd([compiler, "reduce", inp, "--predicate", pred, "-o", outp], seconds, wd)
    if rc == 0 and not to and os.path.exists(outp):
        with open(outp) as fh:
            return fh.read()
    return None


def minimize(case, res, compiler, wd, seconds=900, max_tests=600, use_lean=True):
    """Return (minimized_src, tests_run). Every accepted step preserves cls+shape."""
    oracle, params = case["oracle"], case["params"]
    target = res.presig(oracle)
    budget = Budget(seconds, max_tests)
    cwd = os.path.join(wd, "min")

    def holds(src):
        if not src.strip():
            return False
        r = classify(oracle, src, params, compiler, cwd)
        return r.cls is not None and r.presig(oracle) == target

    if oracle == "lin":
        if res.cls == "unsound":
            split = lin_split(case["src"], params["ok_src"])
            okp = dict(params, must_compile=True, kind="consume-once")

            def holds_ok(src):
                return classify("lin", src, okp, compiler, cwd).cls is None

            out, _ = minimize_lin(split, holds, holds_ok, budget)
            return out, budget.tests
        out = minimize_text(case["src"], holds, budget)
        return out, budget.tests
    src = case["src"]
    if use_lean and oracle == "diff":
        meta = os.path.join(wd, "pred_meta.json")
        save_json(meta, {"oracle": oracle, "params": params, "presig": target,
                         "compiler": compiler})
        red = lean_reduce(compiler, src, meta, wd, min(seconds / 2, 600))
        if red and len(red) < len(src) and holds(red):
            src = red
    return minimize_text(src, holds, budget), budget.tests


# --------------------------------------------------------------------------- findings store

def notify(snapshot, cls, sig, meta, log):
    script = os.path.join(snapshot or "", "scripts", "land", "notify.sh")
    if not snapshot or not os.access(script, os.X_OK):
        return "notify.sh absent"
    subject = f"fuzz-farm: NEW {cls} finding {sig}"
    body = (f"class={cls} oracle={meta['oracle']} generator={meta['generator']} "
            f"seed={meta['seed']} sha={meta['compiler_sha']}\nshape={meta['shape']}\n"
            f"dir={meta['dir']}")
    rc, out, err, _ = run_cmd([script, subject, body], 60, snapshot)
    log.append(f"notify rc={rc} {out.strip()[:200]} {err.strip()[:200]}")
    return f"notify rc={rc}"


def record(p, case, res, ctx, wd, log):
    """Dedup + minimize + store one interesting result. Returns (sig, is_new)."""
    oracle = case["oracle"]
    pre = res.presig(oracle)
    occ = {"seed": case["seed"], "generator": case["generator"], "sha": ctx["sha"], "at": now_iso()}
    with locked(p):
        idx = load_json(p["index"], empty_index())
        known = idx["presig"].get(pre, [])
        pend = idx["pending"].get(pre)
        hits = sum(idx["signatures"][s]["count"] for s in known) if known else 0
        resample = (known and len(known) < MAX_VARIANTS_PER_PRESIG
                    and hits % RESAMPLE_EVERY == 0)
        if (known and not resample) or (pend and time.time() - pend < 3600):
            sig = known[0] if known else None
            if sig:
                bump(idx, sig, occ)
                save_json(p["index"], idx)
            return sig, False
        idx["pending"][pre] = time.time()
        save_json(p["index"], idx)
    t0 = time.time()
    try:
        min_src, ntests = minimize(case, res, ctx["compiler"], wd,
                                   seconds=ctx.get("min_seconds", 900),
                                   use_lean=ctx.get("lean_reduce", True))
    except Exception as e:  # a minimizer bug must never lose the finding
        log.append(f"minimizer error: {e!r}")
        min_src, ntests = case["src"], 0
    sig = signature(pre, min_src)
    cls = res.cls
    with locked(p):
        idx = load_json(p["index"], empty_index())
        idx["pending"].pop(pre, None)
        if sig in idx["signatures"]:
            bump(idx, sig, occ)
            save_json(p["index"], idx)
            return sig, False
        d = os.path.join(p["findings"], sig)
        os.makedirs(d, exist_ok=True)
        meta = {"signature": sig, "class": res.cls, "oracle": oracle, "shape": res.shape,
                "presig": pre, "generator": case["generator"], "seed": case["seed"],
                "depth": case.get("depth"), "params": {k: v for k, v in case["params"].items()},
                "compiler_sha": ctx["sha"], "first_seen": now_iso(),
                "structure_hash": structure_hash(min_src), "detail": res.detail,
                "minimize": {"tests": ntests, "seconds": round(time.time() - t0, 1),
                             "orig_lines": case["src"].count("\n"),
                             "min_lines": min_src.count("\n")},
                "dir": d,
                "repro_cmd": (f"python3 scripts/fuzz/farm/farm.py repro --generator "
                              f"{case['generator']} --seed {case['seed']} --depth {case.get('depth', 3)}"
                              f" --gen-dir scripts/tests")}
        with open(os.path.join(d, "repro.con"), "w") as fh:
            fh.write(case["src"])
        with open(os.path.join(d, "minimized.con"), "w") as fh:
            fh.write(min_src)
        save_json(os.path.join(d, "meta.json"), meta)
        with open(os.path.join(d, "log"), "a") as fh:
            fh.write("\n".join(log + [f"{now_iso()} recorded {sig} at {ctx['sha']}"]) + "\n")
        idx["signatures"][sig] = {"class": res.cls, "oracle": oracle, "shape": res.shape,
                                  "presig": pre, "generator": case["generator"],
                                  "status": "open", "first_seen": meta["first_seen"],
                                  "last_seen": meta["first_seen"], "count": 1,
                                  "first_sha": ctx["sha"], "last_sha": ctx["sha"],
                                  "occurrences": [occ], "dir": d}
        idx["presig"].setdefault(pre, []).append(sig)
        save_json(p["index"], idx)
    if res.cls in NOTIFY_CLASSES and ctx.get("notify", True):
        meta["dir"] = d
        notify(ctx.get("snapshot"), res.cls, sig, meta, log)
    return sig, True


def bump(idx, sig, occ):
    e = idx["signatures"][sig]
    e["count"] += 1
    e["last_seen"] = occ["at"]
    e["last_sha"] = occ["sha"]
    e["occurrences"] = (e.get("occurrences", []) + [occ])[-MAX_OCCURRENCES:]
    if e["status"].startswith("fixed-on-main"):
        e["status"] = "open"
        e["reopened"] = occ["at"]


# --------------------------------------------------------------------------- campaign

class Stats:
    def __init__(self, p, worker):
        self.path = os.path.join(p["stats"], today(), f"w{worker}-{os.getpid()}.json")
        os.makedirs(os.path.dirname(self.path), exist_ok=True)
        self.data = load_json(self.path, {"cases": {}, "outcomes": {}, "new": {}, "seconds": 0.0,
                                          "drift_batches": 0})

    def add(self, gen, outcome):
        self.data["cases"][gen] = self.data["cases"].get(gen, 0) + 1
        k = f"{gen}:{outcome}"
        self.data["outcomes"][k] = self.data["outcomes"].get(k, 0) + 1

    def flush(self):
        save_json(self.path, self.data)


def paused(p):
    return os.path.exists(p["pause"])


def campaign(args):
    p = paths(args.home)
    ensure_dirs(p)
    args.compiler = os.path.abspath(args.compiler)
    gens = Generators(args.gen_dir)
    order = [g for g in args.generators.split(",") if g]
    ctx = {"compiler": args.compiler, "sha": args.sha, "snapshot": args.snapshot,
           "min_seconds": args.min_seconds, "lean_reduce": not args.no_lean_reduce,
           "notify": not args.no_notify}
    wd = os.path.join(p["tmp"], f"w{args.worker_id}")
    shutil.rmtree(wd, ignore_errors=True)
    os.makedirs(wd)
    stats = Stats(p, args.worker_id)
    deadline = time.time() + args.seconds
    master = random.Random(args.seed if args.seed is not None else
                           int.from_bytes(os.urandom(8), "big"))
    gi = args.worker_id
    seeds_log = os.path.join(p["state"], "seeds.log")
    while time.time() < deadline:
        if paused(p):
            time.sleep(10)
            continue
        gen = order[gi % len(order)]
        gi += 1
        depth = master.choice([3, 3, 3, 4])
        batch_seed = master.getrandbits(63)
        brng = random.Random(batch_seed)
        with open(seeds_log, "a") as fh:
            fh.write(f"{now_iso()} sha={args.sha} worker={args.worker_id} gen={gen} "
                     f"depth={depth} batch_seed={batch_seed} n={args.batch}\n")
        t0 = time.time()
        refusals, compared = [], 0
        for _ in range(args.batch):
            if time.time() >= deadline or paused(p):
                break
            seed = brng.getrandbits(63)
            for case in gens.make(gen, seed, depth):
                case.update({"seed": seed, "generator": gen, "depth": depth})
                res = classify(case["oracle"], case["src"], case["params"], args.compiler,
                               os.path.join(wd, "c"))
                stats.add(gen, res.outcome)
                compared += 1
                if res.cls == "refusal":
                    refusals.append((case, res))
                elif res.cls:
                    sig, new = record(p, case, res, ctx, wd, [])
                    if new:
                        stats.data["new"][res.cls] = stats.data["new"].get(res.cls, 0) + 1
                    print(f"[farm] {res.cls} {'NEW ' if new else ''}{sig} gen={gen} seed={seed}",
                          flush=True)
        if gen != "mut" and compared:
            rate = len(refusals) / compared
            if compared >= DRIFT_MIN_CASES and rate > DRIFT_THRESHOLD:
                stats.data["drift_batches"] += 1
                drift_alert(p, gen, args.sha, compared, refusals)
                refusals = refusals[:1]  # one sample is kept as a finding, not every case
                print(f"[farm] DRIFT gen={gen} refusal rate {rate:.0%} ({len(refusals)} kept)",
                      flush=True)
            else:
                drift_clear(p, gen, args.sha, compared, rate)
            for case, res in refusals:
                record(p, case, res, ctx, wd, [])
        stats.data["seconds"] += time.time() - t0
        stats.flush()
    stats.flush()
    return 0


def drift_alert(p, gen, sha, n, refusals):
    codes = {}
    for _, r in refusals:
        k = ",".join(r.detail.get("codes", [])) or "no-code"
        codes[k] = codes.get(k, 0) + 1
    with locked(p):
        d = load_json(p["drift"], {})
        d[gen] = {"alert": True, "rate": round(len(refusals) / n, 3), "cases": n,
                  "refusals": len(refusals), "sha": sha, "at": now_iso(),
                  "top_codes": sorted(codes.items(), key=lambda kv: -kv[1])[:5],
                  "sample_shape": refusals[0][1].shape}
        save_json(p["drift"], d)


def drift_clear(p, gen, sha, n, rate):
    with locked(p):
        d = load_json(p["drift"], {})
        prev = d.get(gen, {})
        if prev.get("alert") and prev.get("sha") == sha and n < DRIFT_MIN_CASES:
            return
        d[gen] = {"alert": False, "rate": round(rate, 3), "cases": n, "sha": sha, "at": now_iso()}
        save_json(p["drift"], d)


# --------------------------------------------------------------------------- recheck

def recheck(args):
    """Re-run every open finding's minimized program (and repro) against a new compiler."""
    p = paths(args.home)
    ensure_dirs(p)
    args.compiler = os.path.abspath(args.compiler)
    wd = os.path.join(p["tmp"], "recheck")
    with locked(p):
        idx = load_json(p["index"], empty_index())
        sigs = [s for s, e in idx["signatures"].items() if e["status"] == "open"]
    fixed = still = 0
    for sig in sigs:
        d = os.path.join(p["findings"], sig)
        meta = load_json(os.path.join(d, "meta.json"), None)
        if not meta:
            continue
        repro = False
        for name in ("minimized.con", "repro.con"):
            fp = os.path.join(d, name)
            if not os.path.exists(fp):
                continue
            with open(fp) as fh:
                src = fh.read()
            r = classify(meta["oracle"], src, meta["params"], args.compiler, wd)
            if r.cls is not None and r.presig(meta["oracle"]) == meta["presig"]:
                repro = True
                break
        with locked(p):
            idx = load_json(p["index"], empty_index())
            e = idx["signatures"].get(sig)
            if not e:
                continue
            e["last_recheck"] = {"sha": args.sha, "at": now_iso(), "reproduces": repro}
            if not repro:
                e["status"] = f"fixed-on-main {args.sha}"
                e["fixed_at"] = now_iso()
                fixed += 1
            else:
                still += 1
            save_json(p["index"], idx)
        with open(os.path.join(d, "log"), "a") as fh:
            fh.write(f"{now_iso()} recheck at {args.sha}: "
                     f"{'reproduces' if repro else 'FIXED (no longer reproduces)'}\n")
    print(f"[farm] recheck at {args.sha}: {still} still open, {fixed} fixed-on-main")
    return 0


# --------------------------------------------------------------------------- reporting

def aggregate_stats(p, day):
    agg = {"cases": {}, "outcomes": {}, "new": {}, "seconds": 0.0, "drift_batches": 0}
    for f in glob.glob(os.path.join(p["stats"], day, "*.json")):
        s = load_json(f, {})
        for k in ("cases", "outcomes", "new"):
            for kk, v in s.get(k, {}).items():
                agg[k][kk] = agg[k].get(kk, 0) + v
        agg["seconds"] += s.get("seconds", 0.0)
        agg["drift_batches"] += s.get("drift_batches", 0)
    return agg


def status_obj(p):
    idx = load_json(p["index"], empty_index())
    day = today()
    by = {c: {"open": 0, "fixed": 0, "new_today": 0, "occurrences": 0} for c in CLASSES}
    for e in idx["signatures"].values():
        c = by.setdefault(e["class"], {"open": 0, "fixed": 0, "new_today": 0, "occurrences": 0})
        c["open" if e["status"] == "open" else "fixed"] += 1
        c["occurrences"] += e["count"]
        if e["first_seen"].startswith(day):
            c["new_today"] += 1
    agg = aggregate_stats(p, day)
    drift = load_json(p["drift"], {})
    state = load_json(os.path.join(p["state"], "farm.json"), {})
    return {"schema": "fuzz-farm-status/1", "generated_at": now_iso(),
            "compiler_sha": state.get("sha"), "farm_state": state.get("state"),
            "classes": by,
            "totals": {"open": sum(v["open"] for v in by.values()),
                       "fixed": sum(v["fixed"] for v in by.values()),
                       "new_today": sum(v["new_today"] for v in by.values()),
                       "signatures": len(idx["signatures"])},
            "today": {"cases": sum(agg["cases"].values()), "per_generator": agg["cases"],
                      "outcomes": agg["outcomes"], "busy_seconds": round(agg["seconds"])},
            "drift_alerts": {g: v for g, v in drift.items() if v.get("alert")},
            "paused": os.path.exists(p["pause"])}


def status(args):
    p = paths(args.home)
    ensure_dirs(p)
    st = status_obj(p)
    out = args.out or os.path.join(p["public"], "fuzz-status.json")
    save_json(out, st)
    with contextlib.suppress(OSError):
        os.chmod(out, 0o644)
    if args.print:
        print(json.dumps(st, indent=1, sort_keys=True))
    return 0


def summary(args):
    p = paths(args.home)
    ensure_dirs(p)
    day = args.day or today()
    idx = load_json(p["index"], empty_index())
    agg = aggregate_stats(p, day)
    st = status_obj(p)
    lines = [f"# fuzz farm summary {day}", "",
             f"compiler sha: {st['compiler_sha']}  state: {st['farm_state']}",
             f"cases: {sum(agg['cases'].values())} {agg['cases']}  busy: {agg['seconds']/3600:.1f} core-h",
             f"outcomes: {agg['outcomes']}", ""]
    if st["drift_alerts"]:
        lines.append("## GENERATOR DRIFT (refusals reported as drift, not bugs)")
        for g, v in st["drift_alerts"].items():
            lines.append(f"- {g}: refusal rate {v['rate']:.0%} over {v['cases']} cases at "
                         f"{v['sha']}; top codes {v['top_codes']}")
        lines.append("")
    lines.append("## signatures")
    for sig, e in sorted(idx["signatures"].items(),
                         key=lambda kv: (CLASSES.index(kv[1]["class"]) if kv[1]["class"] in CLASSES else 9,
                                         kv[0])):
        new = " NEW" if e["first_seen"].startswith(day) else ""
        lines.append(f"- {sig} [{e['status']}]{new} x{e['count']} {e['generator']}: {e['shape']}")
    text = "\n".join(lines) + "\n"
    out = os.path.join(p["reports"], f"summary-{day}.md")
    with open(out, "w") as fh:
        fh.write(text)
    print(text if args.print else out)
    return 0


# --------------------------------------------------------------------------- promote

def promote(args):
    p = paths(args.home)
    d = os.path.join(p["findings"], args.signature)
    meta = load_json(os.path.join(d, "meta.json"), None)
    if not meta:
        print(f"error: no finding {args.signature} under {p['findings']}", file=sys.stderr)
        return 2
    with open(os.path.join(d, "minimized.con")) as fh:
        src = fh.read()
    slug = f"fuzz_{meta['class']}_{args.signature.split('-')[-1][:8]}"
    rel = f"tests/regressions/{slug}/repro.con"
    expect = {
        "silent": "compiled stdout/exit == --interp stdout/exit (they disagree on this program)",
        "crash": "the compiler/interpreter must not crash (it panics/aborts on this program)",
        "unsound": f"the checker must REJECT this program (linearity: {meta['params'].get('kind')})",
        "timeout": f"no side may hang ({meta['shape']} hangs)",
        "refusal": "the checker should ACCEPT this program (it is refused)",
    }[meta["class"]]
    d0 = meta["detail"]
    observed = []
    for side in ("compile", "native", "interp"):
        if side in d0:
            s = d0[side]
            txt = (s.get("stdout", "") + s.get("stderr", "")).strip().splitlines()
            observed.append(f"{side}: rc={s.get('rc')} {' / '.join(txt[:2])[:160]}")
    header = [f"// Fuzz-farm finding {args.signature} ({meta['class']}): {meta['shape']}",
              f"// Expected: {expect}.",
              f"// Found by generator '{meta['generator']}' seed {meta['seed']} at compiler "
              f"{meta['compiler_sha'][:12]}; minimized from {meta['minimize']['orig_lines']} "
              f"to {meta['minimize']['min_lines']} lines."]
    header += [f"// Observed {o}" for o in observed]
    header.append("// Replay: python3 scripts/fuzz/farm/farm.py classify --oracle "
                  f"{meta['oracle']} {rel}")
    fixture = "\n".join(header) + "\n" + src.rstrip("\n") + "\n"
    out_dir = os.path.join(args.repo, "tests", "regressions", slug) if args.repo else None
    if out_dir and args.write:
        os.makedirs(out_dir, exist_ok=True)
        with open(os.path.join(out_dir, "repro.con"), "w") as fh:
            fh.write(fixture)
    title = {"silent": "silent wrong code", "crash": "compiler crash",
             "unsound": "checker accepts a program it must reject",
             "timeout": "hang", "refusal": "valid program refused"}[meta["class"]]
    print(f"== fixture: {rel}{' (written)' if out_dir and args.write else ''}")
    print(fixture)
    print("== file it (scripts/gen/new_bug.sh, from wip/playbooks):")
    print(f"scripts/gen/new_bug.sh --slug {slug} --class {meta['class']} "
          f"--title {json.dumps('fuzz: ' + title + ' (' + meta['shape'][:60] + ')')} "
          f"--repro {rel}")
    print("then rename the directory to bug<NNN>_<slug> with the number new_bug.sh assigns.")
    return 0


# --------------------------------------------------------------------------- small commands

def cmd_classify(args):
    with open(args.file) as fh:
        src = fh.read()
    params = json.loads(args.params) if args.params else {}
    if args.oracle == "lin":
        params.setdefault("must_compile", False)
        params.setdefault("kind", "unknown")
    wd = tempfile.mkdtemp(prefix="farm_classify_")
    try:
        r = classify(args.oracle, src, params, os.path.abspath(args.compiler), wd)
    finally:
        shutil.rmtree(wd, ignore_errors=True)
    print(json.dumps({"class": r.cls, "shape": r.shape, "outcome": r.outcome,
                      "presig": r.presig(args.oracle) if r.cls else None}, sort_keys=True))
    return 0 if r.cls is None else 1


def cmd_predicate(args):
    """External predicate for `concrete reduce`: exit 0 iff the candidate keeps the presig."""
    meta = load_json(args.meta, None)
    with open(args.candidate) as fh:
        src = fh.read()
    wd = tempfile.mkdtemp(prefix="farm_pred_")
    try:
        r = classify(meta["oracle"], src, meta["params"], meta["compiler"], wd)
    finally:
        shutil.rmtree(wd, ignore_errors=True)
    return 0 if r.cls is not None and r.presig(meta["oracle"]) == meta["presig"] else 1


def cmd_repro(args):
    gens = Generators(args.gen_dir)
    for c in gens.make(args.generator, args.seed, args.depth):
        tag = f"// oracle={c['oracle']} {json.dumps({k: v for k, v in c['params'].items() if k != 'ok_src'})}"
        print(tag)
        print(c["src"])
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--home", default=None)
    sub = ap.add_subparsers(dest="cmd", required=True)
    c = sub.add_parser("campaign")
    c.add_argument("--compiler", required=True)
    c.add_argument("--gen-dir", required=True)
    c.add_argument("--sha", required=True)
    c.add_argument("--snapshot", default=None)
    c.add_argument("--seconds", type=float, default=900)
    c.add_argument("--worker-id", type=int, default=0)
    c.add_argument("--generators", default="diff,lin,mut")
    c.add_argument("--batch", type=int, default=40)
    c.add_argument("--seed", type=int, default=None)
    c.add_argument("--min-seconds", type=float, default=900)
    c.add_argument("--no-lean-reduce", action="store_true")
    c.add_argument("--no-notify", action="store_true")
    r = sub.add_parser("recheck")
    r.add_argument("--compiler", required=True)
    r.add_argument("--sha", required=True)
    s = sub.add_parser("summary")
    s.add_argument("--day", default=None)
    s.add_argument("--print", action="store_true")
    st = sub.add_parser("status")
    st.add_argument("--out", default=None)
    st.add_argument("--print", action="store_true")
    pr = sub.add_parser("promote")
    pr.add_argument("signature")
    pr.add_argument("--repo", default=None)
    pr.add_argument("--write", action="store_true")
    cl = sub.add_parser("classify")
    cl.add_argument("--oracle", choices=["diff", "lin", "mut"], default="diff")
    cl.add_argument("--params", default=None)
    cl.add_argument("--compiler", default=os.path.join(HERE, "..", "..", "..", ".lake", "build",
                                                        "bin", "concrete"))
    cl.add_argument("file")
    pd = sub.add_parser("predicate")
    pd.add_argument("meta")
    pd.add_argument("candidate")
    rp = sub.add_parser("repro")
    rp.add_argument("--generator", required=True)
    rp.add_argument("--seed", type=int, required=True)
    rp.add_argument("--depth", type=int, default=3)
    rp.add_argument("--gen-dir", default=os.path.join(HERE, "..", "..", "tests"))
    args = ap.parse_args()
    fn = {"campaign": campaign, "recheck": recheck, "summary": summary, "status": status,
          "promote": promote, "classify": cmd_classify, "predicate": cmd_predicate,
          "repro": cmd_repro}[args.cmd]
    sys.exit(fn(args))


if __name__ == "__main__":
    main()

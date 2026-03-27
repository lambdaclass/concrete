#!/usr/bin/env python3
"""Minimal MAL interpreter in Python — same architecture as the Concrete version.
   Tagged values, flat binding pool with linked-list envs, cons cell pool."""
import time

# --- Types ---
NIL, INT, SYM, LIST, BOOL, CLOSURE, BUILTIN, ERROR = range(8)

def mk(tag, data=0): return (tag, data)

# --- Pools ---
cells = []  # (car_tag, car_data, cdr_tag, cdr_data)
def alloc_cell(car, cdr):
    idx = len(cells)
    cells.append((car[0], car[1], cdr[0], cdr[1]))
    return idx

def get_car(idx):
    c = cells[idx]
    return (c[0], c[1])

def get_cdr(idx):
    c = cells[idx]
    return (c[2], c[3])

def list_len(v):
    if v[0] != LIST or v[1] < 0: return 0
    n, cur = 0, v
    while cur[0] == LIST and cur[1] >= 0:
        n += 1
        cur = get_cdr(cur[1])
    return n

def list_nth(v, n):
    cur = v
    for _ in range(n):
        if cur[0] != LIST or cur[1] < 0: return mk(ERROR)
        cur = get_cdr(cur[1])
    if cur[0] != LIST or cur[1] < 0: return mk(ERROR)
    return get_car(cur[1])

# --- Symbols ---
sym_table = {}
sym_names = []
def intern(s):
    if s in sym_table: return sym_table[s]
    idx = len(sym_names)
    sym_table[s] = idx
    sym_names.append(s)
    return idx

SYM_DEF = intern("def!")
SYM_LET = intern("let*")
SYM_FN = intern("fn*")
SYM_IF = intern("if")
SYM_DO = intern("do")
SYM_TRUE = intern("true")
SYM_FALSE = intern("false")
SYM_NIL = intern("nil")

# --- Environments (linked-list per env, flat binding pool) ---
env_metas = []  # (parent, last_binding_idx)
bindings = []   # (sym_id, val, prev_in_env)

def env_new(parent):
    eid = len(env_metas)
    env_metas.append([parent, -1])
    return eid

def env_set(eid, sym_id, val):
    idx = len(bindings)
    bindings.append((sym_id, val, env_metas[eid][1]))
    env_metas[eid][1] = idx

def env_get(eid, sym_id):
    cur = eid
    for _ in range(200):
        if cur < 0: return mk(ERROR)
        idx = env_metas[cur][1]
        while idx >= 0:
            b = bindings[idx]
            if b[0] == sym_id: return b[1]
            idx = b[2]
        cur = env_metas[cur][0]
    return mk(ERROR)

# --- Closures ---
closures = []  # (params_list_idx, body_val, env_id)

# --- Reader ---
def read_str(s):
    val, _ = read_form(s, 0)
    return val

def skip_ws(s, p):
    while p < len(s) and s[p] in ' \t\n\r': p += 1
    return p

def read_form(s, pos):
    p = skip_ws(s, pos)
    if p >= len(s): return mk(NIL), p
    if s[p] == '(':
        return read_list(s, p+1)
    return read_atom(s, p)

def read_atom(s, pos):
    p = pos
    if s[p].isdigit() or (s[p] == '-' and p+1 < len(s) and s[p+1].isdigit()):
        neg = 1
        if s[p] == '-': neg = -1; p += 1
        n = 0
        while p < len(s) and s[p].isdigit():
            n = n * 10 + int(s[p]); p += 1
        return mk(INT, n * neg), p
    # symbol
    sym_chars = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!*+-/<=>?")
    start = p
    while p < len(s) and s[p] in sym_chars: p += 1
    name = s[start:p]
    sid = intern(name)
    if sid == SYM_TRUE: return mk(BOOL, 1), p
    if sid == SYM_FALSE: return mk(BOOL, 0), p
    if sid == SYM_NIL: return mk(NIL), p
    return mk(SYM, sid), p

def read_list(s, pos):
    p = skip_ws(s, pos)
    if p >= len(s): return mk(ERROR), p
    if s[p] == ')': return mk(LIST, -1), p+1
    first, p = read_form(s, p)
    if first[0] == ERROR: return first, p
    rest, p = read_list(s, p)
    if rest[0] == ERROR: return rest, p
    idx = alloc_cell(first, rest)
    return mk(LIST, idx), p

# --- Eval ---
def eval_mal(v, eid):
    if v[0] in (NIL, INT, BOOL, ERROR): return v
    if v[0] == SYM: return env_get(eid, v[1])
    if v[0] != LIST: return mk(ERROR)
    if v[1] < 0: return v

    head = get_car(v[1])
    rest = get_cdr(v[1])

    if head[0] == SYM:
        if head[1] == SYM_DEF:
            sym = list_nth(rest, 0)
            val = eval_mal(list_nth(rest, 1), eid)
            if val[0] == ERROR: return val
            env_set(eid, sym[1], val)
            return val
        if head[1] == SYM_LET:
            bl = list_nth(rest, 0)
            body = list_nth(rest, 1)
            le = env_new(eid)
            cur = bl
            while cur[0] == LIST and cur[1] >= 0:
                s = get_car(cur[1])
                nxt = get_cdr(cur[1])
                if nxt[0] != LIST or nxt[1] < 0: return mk(ERROR)
                val = eval_mal(get_car(nxt[1]), le)
                if val[0] == ERROR: return val
                env_set(le, s[1], val)
                cur = get_cdr(nxt[1])
            return eval_mal(body, le)
        if head[1] == SYM_FN:
            params = list_nth(rest, 0)
            body = list_nth(rest, 1)
            ci = len(closures)
            closures.append((params[1], body, eid))
            return mk(CLOSURE, ci)
        if head[1] == SYM_IF:
            cond = eval_mal(list_nth(rest, 0), eid)
            if cond[0] == ERROR: return cond
            truthy = not (cond[0] == NIL or (cond[0] == BOOL and cond[1] == 0))
            if truthy: return eval_mal(list_nth(rest, 1), eid)
            if list_len(rest) >= 3: return eval_mal(list_nth(rest, 2), eid)
            return mk(NIL)
        if head[1] == SYM_DO:
            result = mk(NIL)
            cur = rest
            while cur[0] == LIST and cur[1] >= 0:
                result = eval_mal(get_car(cur[1]), eid)
                if result[0] == ERROR: return result
                cur = get_cdr(cur[1])
            return result

    func = eval_mal(head, eid)
    if func[0] == ERROR: return func
    args = eval_list(rest, eid)
    if args[0] == ERROR: return args

    if func[0] == BUILTIN:
        return apply_builtin(func[1], args)

    if func[0] == CLOSURE:
        cl = closures[func[1]]
        fe = env_new(cl[2])
        pc, ac = cl[0], args
        while pc >= 0 and ac[0] == LIST and ac[1] >= 0:
            ps = get_car(pc)[1]
            env_set(fe, ps, get_car(ac[1]))
            cdr = get_cdr(pc)
            pc = cdr[1] if cdr[0] == LIST else -1
            ac = get_cdr(ac[1])
        return eval_mal(cl[1], fe)

    return mk(ERROR)

def eval_list(v, eid):
    if v[0] != LIST or v[1] < 0: return mk(LIST, -1)
    car = eval_mal(get_car(v[1]), eid)
    if car[0] == ERROR: return car
    cdr = eval_list(get_cdr(v[1]), eid)
    if cdr[0] == ERROR: return cdr
    return mk(LIST, alloc_cell(car, cdr))

def apply_builtin(bid, args):
    if bid <= 3:
        a, b = list_nth(args, 0), list_nth(args, 1)
        if a[0] != INT or b[0] != INT: return mk(ERROR)
        if bid == 0: return mk(INT, a[1] + b[1])
        if bid == 1: return mk(INT, a[1] - b[1])
        if bid == 2: return mk(INT, a[1] * b[1])
        if bid == 3:
            if b[1] == 0: return mk(ERROR)
            return mk(INT, int(a[1] / b[1]))
    if bid == 4:
        a, b = list_nth(args, 0), list_nth(args, 1)
        return mk(BOOL, 1 if a == b else 0)
    if bid == 5:
        a, b = list_nth(args, 0), list_nth(args, 1)
        return mk(BOOL, 1 if a[1] < b[1] else 0)
    if bid == 6:
        a, b = list_nth(args, 0), list_nth(args, 1)
        return mk(BOOL, 1 if a[1] > b[1] else 0)
    if bid == 7: return args
    if bid == 8:
        a = list_nth(args, 0)
        if a[0] == NIL: return mk(INT, 0)
        return mk(INT, list_len(a))
    if bid == 9:
        a = list_nth(args, 0)
        if a[0] == NIL: return mk(BOOL, 1)
        if a[0] == LIST and (a[1] < 0 or list_len(a) == 0): return mk(BOOL, 1)
        return mk(BOOL, 0)
    return mk(ERROR)

# --- Setup ---
import sys
sys.setrecursionlimit(100000)

root = env_new(-1)
for name, bid in [("+",0),("-",1),("*",2),("/",3),("=",4),("<",5),(">",6),
                   ("list",7),("count",8),("empty?",9)]:
    env_set(root, intern(name), mk(BUILTIN, bid))

def run(code):
    return eval_mal(read_str(code), root)

def run_int(code):
    r = run(code)
    assert r[0] == INT, f"expected int, got {r}"
    return r[1]

# Define functions
run("(def! fib (fn* (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))")
run("(def! sum-loop (fn* (n acc) (if (< n 1) acc (sum-loop (- n 1) (+ acc n)))))")
run("(def! ack (fn* (m n) (if (= m 0) (+ n 1) (if (= n 0) (ack (- m 1) 1) (ack (- m 1) (ack m (- n 1)))))))")
run("(def! make-counter (fn* (n) (fn* (x) (+ n x))))")
run("(def! sum-closures (fn* (n) (if (< n 1) 0 (+ ((make-counter n) 0) (sum-closures (- n 1))))))")
run("(def! compose (fn* (f g) (fn* (x) (f (g x)))))")
run("(def! inc (fn* (x) (+ x 1)))")
run("(def! compose-n (fn* (f n) (if (< n 2) f (compose f (compose-n f (- n 1))))))")

# Verify
assert run_int("(fib 10)") == 55
assert run_int("(sum-loop 100 0)") == 5050
assert run_int("(ack 3 4)") == 125
assert run_int("(sum-closures 10)") == 55
assert run_int("((compose-n inc 10) 0)") == 10

# Benchmark
t0 = time.perf_counter()
assert run_int("(fib 30)") == 832040
t1 = time.perf_counter()
assert run_int("(sum-loop 10000 0)") == 50005000
t2 = time.perf_counter()
assert run_int("(ack 3 4)") == 125
t3 = time.perf_counter()
assert run_int("(sum-closures 1000)") == 500500
t4 = time.perf_counter()
assert run_int("((compose-n inc 100) 0)") == 100
t5 = time.perf_counter()

print(f"fib(30)          {(t1-t0)*1000:8.1f} ms")
print(f"sum-loop(10000)  {(t2-t1)*1000:8.1f} ms")
print(f"ack(3,4)         {(t3-t2)*1000:8.1f} ms")
print(f"sum-closures(1k) {(t4-t3)*1000:8.1f} ms")
print(f"compose-n(100)   {(t5-t4)*1000:8.1f} ms")
print(f"TOTAL            {(t5-t0)*1000:8.1f} ms")

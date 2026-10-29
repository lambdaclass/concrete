#!/usr/bin/env python3
"""Grammar conformance: does the hand-written parser accept exactly grammar/concrete.ebnf?

scripts/tests/check_grammar_ll1.sh proves the grammar is LL(1). This tool checks the other half
of SYN-1 (docs/spec/syntax.md): that Concrete/Frontend/Parser.lean accepts a program exactly when
the grammar derives it. It reads the EBNF with check_ll1.py's parser and works in three steps:

  generate  derivations of `program`, from a fixed seed, steered toward every alternative,
            every optional (present and absent) and every repetition (zero and some) of every
            rule, until each is covered or the budget runs out. Depth is bounded: past it, the
            cheapest alternative is taken. The output is one token sequence per program.
  mutate    each generated program, with one token deleted, inserted or swapped with its
            neighbour, mostly at the positions where an alternative was chosen.
  recognize a reference recognizer built from the same EBNF decides grammar membership
            exactly. It computes the set of end positions every grammar node can reach from
            every start position, memoized; the grammar has no left recursion, so this is a
            complete recognizer for it, ambiguities included.

The parser's verdicts come from scripts/tools/parse_batch.lean. Disagreements are reduced to
SIGNATURES that do not depend on the random program:
  accept-gap  the grammar derives the program and the parser rejects it:
              `accept-gap <normalized parser message>`
  reject-gap  the grammar rejects the program and the parser accepts it:
              `reject-gap <rule>: <token>`, the rule and token where the recognizer got
              furthest before failing.
docs/compiler/grammar_conformance_known.tsv lists the signatures known today, each with a
reason; the gate fails on a signature that is not listed, and on a listed one that no longer
occurs (the list only shrinks).

One lexical side rule of SYN-1 is not expressible in the EBNF and is implemented here, from the
grammar's own comment on `ident_tail` (bug 212): an identifier immediately followed by `{` in
`ident_tail` begins a struct literal exactly when the identifier starts with an uppercase letter.

Usage:
  grammar_conformance.py generate --out DIR [--seed S] [--budget N] [--depth D]
      writes DIR/programs.tsv (id, kind, tokens), DIR/sources.txt (for parse_batch.lean),
      DIR/coverage.txt
  grammar_conformance.py judge --dir DIR --verdicts FILE [--known TSV]
      prints the findings and exits 1 on any unlisted signature or stale listed one
"""
import argparse
import collections
import importlib.util
import os
import random
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
GRAMMAR = os.path.join(ROOT, "grammar", "concrete.ebnf")
SEP = "\n\x1e\n"

# The lexer's reserved words (Concrete/Frontend/Lexer.lean, lookupKeyword). A grammar terminal
# that is not one of these, such as 'self' or 'Copy', is spelled as an identifier.
KEYWORDS = {
    "fn", "let", "mut", "if", "else", "while", "for", "return", "assert", "assume", "true",
    "false", "mod", "struct", "enum", "match", "pub", "import", "as", "impl", "trait", "const",
    "type", "extern", "with", "cap", "break", "continue", "defer", "borrow", "in", "newtype",
    "trusted",
}
CLASS_SAMPLES = {
    "INT": ["0", "1", "42", "0x1F"],
    "FLOAT": ["1.5", "0.25"],
    "STRING": ['"s"', '"hello"'],
    "CHAR": ["'c'"],
    "LABEL": ["'outer"],
}
LOWER_IDENTS = ["x", "y", "foo", "bar_1"]
UPPER_IDENTS = ["Foo", "Bar", "T"]
IDENT_RE = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")


def load_ll1():
    spec = importlib.util.spec_from_file_location("check_ll1", os.path.join(ROOT, "scripts", "check_ll1.py"))
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


LL1 = load_ll1()
T, NT, SEQ, ALT, OPT, REP, EPS = (LL1.Terminal, LL1.Nonterminal, LL1.Sequence, LL1.Alternation,
                                   LL1.Optional, LL1.Repetition, LL1.EPSILON)


def load_rules():
    with open(GRAMMAR) as f:
        return LL1.parse_ebnf(f.read())


# ── Tokens ────────────────────────────────────────────────────────────────────────────────────
# A token is (text, cls): cls is a class name (IDENT, INT, ...) for a literal, or None for a
# fixed spelling. A token matches a grammar terminal 'kw' when its text is kw, and IDENT when it
# is an identifier that is not reserved.

def term_matches(term, tok):
    name = term.name
    text, cls = tok
    if name.startswith("'"):
        return cls is None and text == name[1:-1]
    if name == "IDENT":
        return cls == "IDENT" or (cls is None and IDENT_RE.match(text) is not None and text not in KEYWORDS)
    return cls == name


# ── Reference recognizer ──────────────────────────────────────────────────────────────────────

class Recognizer:
    def __init__(self, rules):
        self.rules = rules
        self.fail_pos = -1
        self.fail_rule = None

    def run(self, toks):
        self.toks = toks
        self.memo = {}
        self.active = set()
        self.fail_pos = -1
        self.fail_rule = "program"
        self.stack = ["program"]
        ends = self.ends(self.rules["program"], 0)
        return len(toks) in ends

    def note_fail(self, pos):
        if pos > self.fail_pos:
            self.fail_pos = pos
            self.fail_rule = self.stack[-1]

    def ends(self, node, pos):
        key = (id(node), pos)
        if key in self.memo:
            return self.memo[key]
        if key in self.active:
            return frozenset()
        self.active.add(key)
        out = self._ends(node, pos)
        self.active.discard(key)
        self.memo[key] = out
        return out

    def _ends(self, node, pos):
        toks = self.toks
        if node == EPS:
            return frozenset([pos])
        if isinstance(node, T):
            if pos < len(toks) and term_matches(node, toks[pos]):
                return frozenset([pos + 1])
            self.note_fail(pos)
            return frozenset()
        if isinstance(node, NT):
            self.stack.append(node.name)
            try:
                if node.name == "ident_tail":
                    return self._ident_tail(pos)
                return self.ends(self.rules[node.name], pos)
            finally:
                self.stack.pop()
        if isinstance(node, SEQ):
            cur = frozenset([pos])
            for item in node.items:
                nxt = set()
                for p in cur:
                    nxt |= self.ends(item, p)
                cur = frozenset(nxt)
                if not cur:
                    break
            return cur
        if isinstance(node, ALT):
            out = set()
            for alt in node.alts:
                out |= self.ends(alt, pos)
            return frozenset(out)
        if isinstance(node, OPT):
            return frozenset({pos} | set(self.ends(node.body, pos)))
        if isinstance(node, REP):
            seen = {pos}
            frontier = [pos]
            while frontier:
                p = frontier.pop()
                for q in self.ends(node.body, p):
                    if q not in seen:
                        seen.add(q)
                        frontier.append(q)
            return frozenset(seen)
        raise TypeError(node)

    def _ident_tail(self, pos):
        # SYN-1's side rule (bug 212): after an identifier, `{` starts a struct literal exactly
        # when the identifier is uppercase. So the `{` alternative needs an uppercase identifier,
        # and an uppercase identifier followed by `{` cannot take the empty alternative.
        body = self.rules["ident_tail"]
        prev = self.toks[pos - 1][0] if pos > 0 else ""
        upper = prev[:1].isupper()
        nxt_brace = pos < len(self.toks) and self.toks[pos] == ("{", None)
        out = set()
        for alt in body.alts:
            first_is_brace = isinstance(alt, SEQ) and isinstance(alt.items[0], T) and alt.items[0].name == "'{'"
            is_empty = alt == EPS or (isinstance(alt, SEQ) and alt.items == [EPS])
            if first_is_brace and not upper:
                continue
            if is_empty and upper and nxt_brace:
                continue
            out |= self.ends(alt, pos)
        if not out:
            self.note_fail(pos)
        return frozenset(out)


# ── Generator ─────────────────────────────────────────────────────────────────────────────────

def node_label(rule, path):
    return f"{rule}:{path}"


class Generator:
    def __init__(self, rules, seed, depth):
        self.rules = rules
        self.rng = random.Random(seed)
        self.max_depth = depth
        self.cost = self._costs()
        self.items = {}       # coverage item -> label
        self.covered = set()
        self._index("program")
        for name in rules:
            self._index(name)
        self._subtrees()

    def _costs(self):
        # Fewest tokens each node derives (fixpoint); steers generation home past the depth bound.
        cost = {name: float("inf") for name in self.rules}

        def c(node):
            if node == EPS:
                return 0
            if isinstance(node, T):
                return 1
            if isinstance(node, NT):
                return cost.get(node.name, float("inf"))
            if isinstance(node, SEQ):
                return sum(c(i) for i in node.items)
            if isinstance(node, ALT):
                return min(c(a) for a in node.alts)
            if isinstance(node, (OPT, REP)):
                return 0
            raise TypeError(node)
        self._c = c
        changed = True
        while changed:
            changed = False
            for name, body in self.rules.items():
                v = c(body)
                if v < cost[name]:
                    cost[name] = v
                    changed = True
        return cost

    def _index(self, rule):
        # Coverage items: each alternative of each alternation, each optional present/absent,
        # each repetition empty/non-empty, identified by rule and path inside the rule.
        if getattr(self, "_indexed", None) is None:
            self._indexed = set()
        if rule in self._indexed:
            return
        self._indexed.add(rule)
        self.paths = getattr(self, "paths", {})

        def walk(node, path):
            self.paths[id(node)] = (rule, path)
            if isinstance(node, ALT):
                for i, a in enumerate(node.alts):
                    self.items[(rule, path, "alt", i)] = f"{rule}{path} alternative {i + 1}"
                    walk(a, f"{path}|{i + 1}")
            elif isinstance(node, SEQ):
                for i, a in enumerate(node.items):
                    walk(a, f"{path}.{i + 1}")
            elif isinstance(node, OPT):
                self.items[(rule, path, "opt", 0)] = f"{rule}{path} [ ] absent"
                self.items[(rule, path, "opt", 1)] = f"{rule}{path} [ ] present"
                walk(node.body, f"{path}[?]")
            elif isinstance(node, REP):
                self.items[(rule, path, "rep", 0)] = f"{rule}{path} {{ }} empty"
                self.items[(rule, path, "rep", 1)] = f"{rule}{path} {{ }} repeated"
                walk(node.body, f"{path}{{*}}")
        walk(self.rules[rule], "")

    def _subtrees(self):
        # For every node: the coverage items inside it (within its rule) and the nonterminals it
        # names directly; and for every rule, the rules it reaches. Steering toward an uncovered
        # item anywhere below a choice is what makes deep rules (array bodies, field values)
        # reachable at all: uniform choices almost never get there.
        self.local = {}
        self.direct = {}

        def walk(node):
            items, nts = set(), set()
            rule, path = self.paths[id(node)]
            if isinstance(node, ALT):
                items |= {(rule, path, "alt", i) for i in range(len(node.alts))}
                kids = node.alts
            elif isinstance(node, SEQ):
                kids = node.items
            elif isinstance(node, OPT):
                items |= {(rule, path, "opt", 0), (rule, path, "opt", 1)}
                kids = [node.body]
            elif isinstance(node, REP):
                items |= {(rule, path, "rep", 0), (rule, path, "rep", 1)}
                kids = [node.body]
            elif isinstance(node, NT):
                nts.add(node.name)
                kids = []
            else:
                kids = []
            for k in kids:
                ki, kn = walk(k)
                items |= ki
                nts |= kn
            self.local[id(node)] = items
            self.direct[id(node)] = nts
            return items, nts
        for name, body in self.rules.items():
            walk(body)
        self.reach = {}
        for name, body in self.rules.items():
            seen, todo = set(), [name]
            while todo:
                r = todo.pop()
                if r in seen or r not in self.rules:
                    continue
                seen.add(r)
                todo += list(self.direct[id(self.rules[r])])
            self.reach[name] = seen

    def needs(self, node):
        """Does an uncovered item lie inside `node`, or inside a rule it reaches?"""
        if any(k not in self.covered for k in self.local[id(node)]):
            return True
        for nt in self.direct[id(node)]:
            for r in self.reach.get(nt, ()):
                if any(k not in self.covered for k in self.local[id(self.rules[r])]):
                    return True
        return False

    def _mark(self, node, kind, i):
        rule, path = self.paths[id(node)]
        k = (rule, path, kind, i)
        new = k not in self.covered
        self.covered.add(k)
        return new

    def _uncovered(self, node, kind, i):
        rule, path = self.paths[id(node)]
        return (rule, path, kind, i) not in self.covered

    def program(self):
        self.out = []        # tokens: [text, cls, tag]
        self.choices = []    # token indices where a choice was made
        self.gen(self.rules["program"], 0)
        self._resolve_idents()
        return [(t, c) for t, c, _ in self.out], sorted(set(self.choices))

    def gen(self, node, depth):
        over = depth > self.max_depth
        if node == EPS:
            return
        if isinstance(node, T):
            name = node.name
            if name.startswith("'"):
                self.out.append([name[1:-1], None, None])
            elif name == "IDENT":
                self.out.append(["?ident", "IDENT", None])
            else:
                self.out.append([self.rng.choice(CLASS_SAMPLES[name]), name, None])
            return
        if isinstance(node, NT):
            if node.name == "ident_tail":
                start = len(self.out)
                self.gen_alt(self.rules["ident_tail"], depth + 1, over)
                # Tag the identifier this tail follows with the alternative taken.
                if start > 0 and self.out[start - 1][1] == "IDENT":
                    took_brace = len(self.out) > start and self.out[start][0] == "{" and self.out[start][1] is None
                    self.out[start - 1][2] = "brace" if took_brace else ("empty" if len(self.out) == start else "other")
                return
            body = self.rules[node.name]
            if isinstance(body, ALT):
                self.gen_alt(body, depth + 1, over)
            else:
                self.gen(body, depth + 1)
            return
        if isinstance(node, SEQ):
            for item in node.items:
                self.gen(item, depth)
            return
        if isinstance(node, ALT):
            self.gen_alt(node, depth, over)
            return
        if isinstance(node, OPT):
            if over:
                take = False
            elif self._uncovered(node, "opt", 1) or self.needs(node.body):
                take = True
            elif self._uncovered(node, "opt", 0):
                take = False
            else:
                take = self.rng.random() < 0.5
            self.choices.append(len(self.out))
            self._mark(node, "opt", 1 if take else 0)
            if take:
                self.gen(node.body, depth + 1)
            return
        if isinstance(node, REP):
            if over:
                n = 0
            elif self._uncovered(node, "rep", 1) or self.needs(node.body):
                n = self.rng.randint(1, 2)
            elif self._uncovered(node, "rep", 0):
                n = 0
            else:
                n = self.rng.choice([0, 0, 1, 2])
            self.choices.append(len(self.out))
            self._mark(node, "rep", 1 if n else 0)
            for _ in range(n):
                self.gen(node.body, depth + 1)
            return
        raise TypeError(node)

    def gen_alt(self, node, depth, over):
        if not isinstance(node, ALT):
            self.gen(node, depth)
            return
        alts = list(range(len(node.alts)))
        if over:
            best = min(self._c(node.alts[i]) for i in alts)
            pick = self.rng.choice([i for i in alts if self._c(node.alts[i]) == best])
        else:
            fresh = [i for i in alts if self._uncovered(node, "alt", i)]
            leads = [i for i in alts if self.needs(node.alts[i])]
            pick = self.rng.choice(fresh or leads or alts)
        self.choices.append(len(self.out))
        self._mark(node, "alt", pick)
        self.gen(node.alts[pick], depth + 1)

    def _resolve_idents(self):
        # SYN-1's side rule, from the generator's side: an identifier whose tail took `{` is
        # uppercase; one whose tail was empty and is followed by `{` is lowercase; others either.
        for i, tok in enumerate(self.out):
            if tok[0] != "?ident":
                continue
            nxt = self.out[i + 1] if i + 1 < len(self.out) else None
            if tok[2] == "brace":
                tok[0] = self.rng.choice(UPPER_IDENTS)
            elif tok[2] == "empty" and nxt is not None and nxt[0] == "{" and nxt[1] is None:
                tok[0] = self.rng.choice(LOWER_IDENTS)
            else:
                tok[0] = self.rng.choice(LOWER_IDENTS + UPPER_IDENTS)


def render(tokens):
    return " ".join(t for t, _ in tokens) + "\n"


def mutants(tokens, choices, rng, vocabulary, per_program):
    """One-token edits: delete, insert (a terminal of the grammar), swap with the next token."""
    out = []
    n = len(tokens)
    if n == 0:
        return out
    sites = [c for c in choices if c < n] or list(range(n))
    for _ in range(per_program):
        site = rng.choice(sites) if rng.random() < 0.8 else rng.randrange(n)
        op = rng.choice(["delete", "insert", "swap"])
        if op == "delete":
            m = tokens[:site] + tokens[site + 1:]
            desc = f"delete {tokens[site][0]}"
        elif op == "insert":
            tok = rng.choice(vocabulary)
            m = tokens[:site] + [tok] + tokens[site:]
            desc = f"insert {tok[0]}"
        else:
            if site + 1 >= n or tokens[site] == tokens[site + 1]:
                continue
            m = tokens[:site] + [tokens[site + 1], tokens[site]] + tokens[site + 2:]
            desc = f"swap {tokens[site][0]} {tokens[site + 1][0]}"
        out.append((m, desc))
    return out


def vocabulary_of(rules):
    terms = set()

    def walk(node):
        if isinstance(node, T):
            terms.add(node.name)
        elif isinstance(node, (SEQ,)):
            for i in node.items:
                walk(i)
        elif isinstance(node, ALT):
            for a in node.alts:
                walk(a)
        elif isinstance(node, (OPT, REP)):
            walk(node.body)
    for body in rules.values():
        walk(body)
    vocab = []
    for t in sorted(terms):
        if t.startswith("'"):
            vocab.append((t[1:-1], None))
        elif t == "IDENT":
            vocab += [("x", "IDENT"), ("Foo", "IDENT")]
        elif t != "ε":
            vocab.append((CLASS_SAMPLES[t][0], t))
    return vocab, terms


def encode(tokens):
    return " ".join(f"{t}\x1f{c or ''}" for t, c in tokens)


def decode(s):
    out = []
    if not s:
        return out
    for part in s.split(" "):
        t, c = part.split("\x1f")
        out.append((t, c or None))
    return out


def cmd_generate(args):
    rules = load_rules()
    rec = Recognizer(rules)
    vocab, _ = vocabulary_of(rules)
    mrng = random.Random(args.seed + 1)
    os.makedirs(args.out, exist_ok=True)
    # The empty program is a derivation too (`program = { top_decl }`, zero times).
    rows = [("derived", "-", [])]
    n_gen = 0
    # Pass 1, SMALL programs: a shallow depth bound, so every difference also shows up in a short
    # program, which the judge keeps as its example. Pass 2: the full depth, run until every
    # alternative, optional and repetition of the grammar is covered (coverage is pass 2's).
    small = Generator(rules, args.seed + 2, args.small_depth)
    for _ in range(args.small):
        toks, choices = small.program()
        if not rec.run(toks):
            raise SystemExit(f"generator bug: the recognizer rejects a generated program: {render(toks)}")
        rows.append(("derived", "-", toks))
        for m, desc in mutants(toks, choices, mrng, vocab, args.mutants):
            rows.append(("mutant", desc, m))
    gen = Generator(rules, args.seed, args.depth)
    gen._mark(rules["program"], "rep", 0)
    while n_gen < args.budget:
        toks, choices = gen.program()
        n_gen += 1
        if not rec.run(toks):
            raise SystemExit(f"generator bug: the recognizer rejects a generated program: {render(toks)}")
        rows.append(("derived", "-", toks))
        for m, desc in mutants(toks, choices, mrng, vocab, args.mutants):
            rows.append(("mutant", desc, m))
        if len(gen.covered) == len(gen.items) and n_gen >= args.min_programs:
            break
    with open(os.path.join(args.out, "programs.tsv"), "w") as f:
        for i, (kind, desc, toks) in enumerate(rows):
            member = "1" if (kind == "derived" or rec.run(toks)) else "0"
            extra = "" if member == "1" else f"{rec.fail_rule}\x1d{toks[rec.fail_pos][0] if rec.fail_pos < len(toks) else '<eof>'}"
            f.write(f"{i}\t{kind}\t{member}\t{desc}\t{extra}\t{encode(toks)}\n")
    with open(os.path.join(args.out, "sources.txt"), "w") as f:
        f.write(SEP.join(render(toks) for _, _, toks in rows))
    missing = [gen.items[k] for k in sorted(gen.items) if k not in gen.covered]
    with open(os.path.join(args.out, "coverage.txt"), "w") as f:
        f.write(f"programs {n_gen}\nitems {len(gen.items)}\ncovered {len(gen.covered)}\n")
        for m in missing:
            f.write(f"uncovered {m}\n")
    n_derived = sum(1 for r in rows if r[0] == "derived")
    print(f"generated {n_derived} programs ({n_gen} in the coverage pass) and {len(rows) - n_derived} mutants; "
          f"coverage {len(gen.covered)}/{len(gen.items)} items")
    return 0


def normalize_message(msg):
    msg = re.sub(r"\bgot (str|float|int|char)\([^)]*\)", r"got \1", msg)
    msg = re.sub(r'"[^"]*"', '"…"', msg)
    msg = re.sub(r"\b\d+\b", "N", msg)
    msg = re.sub(r"\bident [A-Za-z_][A-Za-z0-9_]*", "ident …", msg)
    msg = re.sub(r"\b(x|y|foo|bar_1|Foo|Bar|T)\b", "…", msg)
    return msg.strip()[:160]


def load_known(path):
    known = {}
    if path and os.path.exists(path):
        with open(path) as f:
            for line in f:
                line = line.rstrip("\n")
                if not line or line.startswith("#"):
                    continue
                sig, reason = line.split("\t", 1)
                known[sig] = reason
    return known


def cmd_judge(args):
    rows = []
    with open(os.path.join(args.dir, "programs.tsv")) as f:
        for line in f:
            i, kind, member, desc, extra, toks = line.rstrip("\n").split("\t")
            toks = toks if toks else ""
            rows.append((int(i), kind, member == "1", desc, extra, toks))
    with open(args.verdicts) as f:
        verdicts = [l.rstrip("\n") for l in f if l.strip()]
    if len(verdicts) != len(rows):
        print(f"FAIL the parser gave {len(verdicts)} verdicts for {len(rows)} programs")
        return 1
    seen = collections.OrderedDict()
    stats = collections.Counter()
    for (i, kind, member, desc, extra, toks), v in zip(rows, verdicts):
        accepted = v == "ACCEPT"
        stats[(kind, member, accepted)] += 1
        if member and not accepted:
            sig = "accept-gap " + normalize_message(v.split("\t", 1)[1] if "\t" in v else v)
        elif not member and accepted:
            rule, tok = extra.split("\x1d")
            sig = f"reject-gap {rule}: {tok if not IDENT_RE.match(tok) or tok in KEYWORDS else 'IDENT'}"
        else:
            continue
        src = " ".join(t for t, _ in decode(toks)) if toks else ""
        if sig not in seen or len(src) < len(seen[sig][3]):
            seen[sig] = (i, kind, desc, src)
    known = load_known(args.known)
    fails = 0
    if args.examples:
        with open(args.examples, "w") as f:
            for sig, (i, kind, desc, src) in sorted(seen.items()):
                f.write(f"{sig}\t{kind}\t{desc}\t{src}\n")
    for sig, (i, kind, desc, src) in seen.items():
        if sig in known:
            print(f"  known {sig}")
        else:
            fails += 1
            print(f"  FAIL new difference: {sig}\n       example #{i} ({kind}{', ' + desc if desc != '-' else ''}): {src[:300]}")
    for sig in known:
        if sig not in seen:
            fails += 1
            print(f"  FAIL listed difference no longer occurs, delete it: {sig}")
    d_ok = stats[("derived", True, True)]
    d_all = sum(v for (k, _, _), v in stats.items() if k == "derived")
    m_rej = sum(v for (k, m, _), v in stats.items() if k == "mutant" and not m)
    m_rej_both = stats[("mutant", False, False)]
    print(f"derived programs accepted by the parser: {d_ok}/{d_all}; grammar-rejected mutants also "
          f"rejected by the parser: {m_rej_both}/{m_rej}; distinct differences: {len(seen)}")
    return 1 if fails else 0


def signature_of(rec, toks, verdict):
    """The difference signature of one program, or None when grammar and parser agree."""
    member = rec.run(toks)
    accepted = verdict == "ACCEPT"
    if member and not accepted:
        return "accept-gap " + normalize_message(verdict.split("\t", 1)[1] if "\t" in verdict else verdict)
    if not member and accepted:
        tok = toks[rec.fail_pos][0] if rec.fail_pos < len(toks) else "<eof>"
        return f"reject-gap {rec.fail_rule}: {tok if not IDENT_RE.match(tok) or tok in KEYWORDS else 'IDENT'}"
    return None


def parse_batch(sources, workdir):
    """The parser's verdict on each source, from one run of scripts/tools/parse_batch.lean."""
    import subprocess
    path = os.path.join(workdir, "reduce_sources.txt")
    with open(path, "w") as f:
        f.write(SEP.join(sources))
    out = subprocess.run(["lake", "env", "lean", "--run", os.path.join(ROOT, "scripts", "tools", "parse_batch.lean"), path],
                         cwd=ROOT, capture_output=True, text=True, check=True).stdout
    verdicts = [l for l in out.split("\n") if l.strip()]
    if len(verdicts) != len(sources):
        raise SystemExit(f"parse_batch gave {len(verdicts)} verdicts for {len(sources)} sources")
    return verdicts


def cmd_reduce(args):
    """Shrink each difference's example while it keeps its signature (delta debugging over token
    spans, one parser batch per round for every difference at once)."""
    rules = load_rules()
    rec = Recognizer(rules)
    cur = {}
    with open(args.examples) as f:
        for line in f:
            sig, kind, desc, src = line.rstrip("\n").split("\t")
            cur[sig] = [tok_of(t) for t in src.split(" ")] if src else []
    for rnd in range(args.rounds):
        cands = []
        for sig, toks in cur.items():
            n = len(toks)
            size = max(1, n // 2)
            while True:
                for start in range(0, n, max(1, size // 2) if size > 1 else 1):
                    c = toks[:start] + toks[start + size:]
                    if len(c) < n:
                        cands.append((sig, c))
                if size == 1:
                    break
                size = max(1, size // 2)
        if not cands:
            break
        verdicts = parse_batch([render(c) for _, c in cands], args.workdir)
        improved = 0
        best = {}
        for (sig, c), v in zip(cands, verdicts):
            if signature_of(rec, c, v) == sig and len(c) < len(best.get(sig, cur[sig])):
                best[sig] = c
        for sig, c in best.items():
            cur[sig] = c
            improved += 1
        print(f"round {rnd + 1}: {len(cands)} candidates, {improved} examples shrunk")
        if not improved:
            break
    with open(args.examples, "w") as f:
        for sig in sorted(cur):
            f.write(f"{sig}\treduced\t-\t{' '.join(t for t, _ in cur[sig])}\n")
    for sig in sorted(cur):
        print(f"{sig}\n    {' '.join(t for t, _ in cur[sig])}")
    return 0


def tok_of(text):
    """A token back from its spelling (reduction works on rendered examples)."""
    if text.startswith('"'):
        return (text, "STRING")
    if text.startswith("'") and text.endswith("'") and len(text) > 2:
        return (text, "CHAR")
    if text.startswith("'"):
        return (text, "LABEL")
    if re.match(r"^\d+\.\d+$", text):
        return (text, "FLOAT")
    if re.match(r"^(0x[0-9A-Fa-f]+|\d+)$", text):
        return (text, "INT")
    if IDENT_RE.match(text) and text not in KEYWORDS:
        # A spelling that is also a grammar terminal (self, Copy, C, repr, ...) stays fixed text;
        # term_matches lets it match IDENT too.
        return (text, None)
    return (text, None)


# ── Lookahead: the parser decides with one token ───────────────────────────────────────────────

PARSER = os.path.join(ROOT, "Concrete", "Frontend", "Parser.lean")
DEF_RE = re.compile(r"^\s*(?:@\[[^\]]*\]\s*)?(?:private |protected )*(?:partial )?(?:def|abbrev) ([^\s(:]+)")
LOOKAHEAD_KINDS = [
    # kind, pattern, what it means
    ("peek2", re.compile(r"\bpeek2\b"), "a second token of lookahead"),
    ("token-index", re.compile(r"\.tokens\[|\btokens\[[^\]]*\]\?"), "reads the token array directly, not through peek"),
    ("try-catch", re.compile(r"\btry\b"), "catches a parse failure (a backtracking or recovery point)"),
    ("pos-write", re.compile(r"\bpos\s*:="), "writes the token position"),
    ("state-restore", re.compile(r"^\s*set\s"), "restores a saved parser state"),
]
# The token primitives themselves: one token of lookahead is implemented here.
LOOKAHEAD_PRIMITIVES = {"peek", "peekSpan", "peek2", "advance", "expect", "mkParserState"}


def lookahead_sites(path=PARSER):
    """Every place in Parser.lean that looks further than one token, or can undo a decision,
    outside the token primitives: (function, kind) -> count."""
    sites = collections.Counter()
    fn = None
    in_block_comment = False
    with open(path) as f:
        for line in f:
            stripped = line.strip()
            if in_block_comment:
                if "-/" in stripped:
                    in_block_comment = False
                continue
            if stripped.startswith("/-"):
                in_block_comment = "-/" not in stripped
                continue
            code = line.split("--", 1)[0]
            m = DEF_RE.match(code)
            if m:
                fn = m.group(1)
            if fn in LOOKAHEAD_PRIMITIVES:
                continue
            for kind, pat, _ in LOOKAHEAD_KINDS:
                if pat.search(code):
                    sites[(fn or "<top>", kind)] += 1
    return sites


def cmd_lookahead(args):
    sites = lookahead_sites(args.parser)
    known = {}
    with open(args.known) as f:
        for line in f:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            fn, kind, count, reason = line.split("\t")
            known[(fn, kind)] = (int(count), reason)
    fails = 0
    for key in sorted(set(sites) | set(known)):
        got = sites.get(key, 0)
        want = known.get(key, (0, ""))[0]
        if got > want:
            fails += 1
            print(f"  FAIL {key[0]}: {got} '{key[1]}' site(s), {want} allowed — new lookahead beyond one token "
                  f"or a new backtracking point; the parser must decide with one token (SYN-1)")
        elif got < want:
            fails += 1
            print(f"  FAIL {key[0]}: {got} '{key[1]}' site(s), {want} listed — the list only shrinks; lower it")
    print(f"lookahead sites beyond one token: {sum(sites.values())} in {len(sites)} (function, kind) pairs, "
          f"{len(known)} listed")
    if args.print:
        for (fn, kind), n in sorted(sites.items()):
            print(f"{fn}\t{kind}\t{n}")
    return 1 if fails else 0


def cmd_keywords(args):
    """Every reserved word of the lexer appears in the grammar, unless listed (a word the parser
    reads that no production mentions is syntax the grammar does not describe)."""
    _, terms = vocabulary_of(load_rules())
    known = set()
    with open(args.known) as f:
        for line in f:
            line = line.rstrip("\n")
            if line and not line.startswith("#"):
                known.add(line.split("\t")[0])
    missing = sorted(k for k in KEYWORDS if f"'{k}'" not in terms)
    fails = 0
    for k in missing:
        if k not in known:
            fails += 1
            print(f"  FAIL the lexer reserves '{k}', and no grammar production mentions it")
    for k in sorted(known):
        if k not in missing:
            fails += 1
            print(f"  FAIL '{k}' is listed as missing from the grammar but the grammar mentions it now; delete the row")
    print(f"reserved words missing from the grammar: {len(missing)} ({', '.join(missing) or 'none'}), {len(known)} listed")
    return 1 if fails else 0


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    g = sub.add_parser("generate")
    g.add_argument("--out", required=True)
    g.add_argument("--seed", type=int, default=20261029)
    g.add_argument("--budget", type=int, default=1200)
    g.add_argument("--min-programs", type=int, default=150)
    g.add_argument("--depth", type=int, default=20)
    g.add_argument("--mutants", type=int, default=6)
    g.add_argument("--small", type=int, default=1500)
    g.add_argument("--small-depth", type=int, default=7)
    j = sub.add_parser("judge")
    j.add_argument("--dir", required=True)
    j.add_argument("--verdicts", required=True)
    j.add_argument("--known")
    j.add_argument("--examples", help="write each difference's shortest example here")
    r = sub.add_parser("reduce")
    r.add_argument("--examples", required=True, help="examples.tsv from judge; rewritten in place")
    r.add_argument("--workdir", required=True)
    r.add_argument("--rounds", type=int, default=25)
    la = sub.add_parser("lookahead")
    la.add_argument("--known", required=True)
    la.add_argument("--parser", default=PARSER)
    la.add_argument("--print", action="store_true")
    kw = sub.add_parser("keywords")
    kw.add_argument("--known", required=True)
    args = ap.parse_args()
    return {"generate": cmd_generate, "judge": cmd_judge, "reduce": cmd_reduce,
            "lookahead": cmd_lookahead, "keywords": cmd_keywords}[args.cmd](args)


if __name__ == "__main__":
    sys.exit(main())

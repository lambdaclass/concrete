#!/usr/bin/env python3
"""
LL(1) grammar checker for Concrete.

Parses grammar/concrete.ebnf and verifies the LL(1) property:
for every alternation (A | B | C), the FIRST sets of A, B, C
must be pairwise disjoint (after accounting for nullable rules).

Usage:
    python3 scripts/check_ll1.py [--canonical] [grammar/concrete.ebnf]

Exit code 0 = LL(1) clean, 1 = conflicts found, 2 = bad input.

--canonical prints the cross-check form shared by check_ll1.c and check_ll1.rs (one of three
deliberately independent implementations; scripts/tests/check_grammar_ll1.sh diffs them):
    rule <name> <nullable|-> <FIRST tokens, sorted bytewise, space-separated>
    conflict <path> <alt a> <alt b> <shared tokens, sorted bytewise, comma-separated>
"""

import re
import sys
from collections import defaultdict
from pathlib import Path


# ── EBNF Parser ──────────────────────────────────────────────────

class Terminal:
    """A terminal symbol like 'fn' or INT."""
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return self.name
    def __eq__(self, other):
        return isinstance(other, Terminal) and self.name == other.name
    def __hash__(self):
        return hash(self.name)

class Nonterminal:
    """A nonterminal reference like expr or type."""
    def __init__(self, name):
        self.name = name
    def __repr__(self):
        return f"<{self.name}>"
    def __eq__(self, other):
        return isinstance(other, Nonterminal) and self.name == other.name
    def __hash__(self):
        return hash(("NT", self.name))

class Sequence:
    """A sequence of symbols: A B C."""
    def __init__(self, items):
        self.items = items
    def __repr__(self):
        return " ".join(str(i) for i in self.items)

class Alternation:
    """A choice: A | B | C."""
    def __init__(self, alts):
        self.alts = alts
    def __repr__(self):
        return " | ".join(str(a) for a in self.alts)

class Optional:
    """Zero or one: [ A ]."""
    def __init__(self, body):
        self.body = body
    def __repr__(self):
        return f"[ {self.body} ]"

class Repetition:
    """Zero or more: { A }."""
    def __init__(self, body):
        self.body = body
    def __repr__(self):
        return f"{{ {self.body} }}"

# Token classes for the EBNF parser itself
EPSILON = Terminal("ε")

# Known terminal token classes (not quoted)
TOKEN_CLASSES = {"IDENT", "INT", "FLOAT", "STRING", "CHAR", "LABEL"}


def tokenize_ebnf(text):
    """Tokenize EBNF text into a list of tokens."""
    tokens = []
    i = 0
    while i < len(text):
        # Skip whitespace
        if text[i] in " \t\n\r":
            i += 1
            continue
        # Skip comments (* ... *)
        if text[i:i+2] == "(*":
            end = text.find("*)", i + 2)
            if end == -1:
                raise ValueError(f"Unterminated comment at position {i}")
            i = end + 2
            continue
        # Single-char delimiters
        if text[i] in "=;|[]{}().,+":
            tokens.append(text[i])
            i += 1
            continue
        # Quoted terminal: 'xxx'
        if text[i] == "'":
            j = text.index("'", i + 1)
            tokens.append(("TERM", text[i+1:j]))
            i = j + 1
            continue
        # Identifier (nonterminal or token class)
        if text[i].isalpha() or text[i] == "_":
            j = i
            while j < len(text) and (text[j].isalnum() or text[j] == "_"):
                j += 1
            tokens.append(("ID", text[i:j]))
            i = j
            continue
        raise ValueError(f"Unexpected character {text[i]!r} at position {i}")
    return tokens


def parse_ebnf(text):
    """Parse EBNF text into a dict of {name: body} rules."""
    tokens = tokenize_ebnf(text)
    rules = {}
    i = 0

    def peek():
        nonlocal i
        return tokens[i] if i < len(tokens) else None

    def advance():
        nonlocal i
        t = tokens[i]
        i += 1
        return t

    def expect(val):
        t = advance()
        if t != val:
            raise ValueError(f"Expected {val!r}, got {t!r} at token {i-1}")

    def parse_alternation():
        alts = [parse_sequence()]
        while peek() == "|":
            advance()
            alts.append(parse_sequence())
        if len(alts) == 1:
            return alts[0]
        return Alternation(alts)

    def parse_sequence():
        items = []
        while True:
            p = peek()
            if p is None or p in ("|", ";", "]", "}", ")"):
                break
            items.append(parse_atom())
        if len(items) == 0:
            return Sequence([EPSILON])
        if len(items) == 1:
            return items[0]
        return Sequence(items)

    def parse_atom():
        p = peek()
        if p == "[":
            advance()
            body = parse_alternation()
            expect("]")
            return Optional(body)
        if p == "{":
            advance()
            body = parse_alternation()
            expect("}")
            return Repetition(body)
        if p == "(":
            advance()
            body = parse_alternation()
            expect(")")
            return body
        if isinstance(p, tuple):
            advance()
            kind, val = p
            if kind == "TERM":
                return Terminal(f"'{val}'")
            if kind == "ID":
                if val in TOKEN_CLASSES:
                    return Terminal(val)
                return Nonterminal(val)
        raise ValueError(f"Unexpected token {p!r} at position {i}")

    while i < len(tokens):
        name_tok = advance()
        if not isinstance(name_tok, tuple) or name_tok[0] != "ID":
            raise ValueError(f"Expected rule name, got {name_tok!r}")
        name = name_tok[1]
        expect("=")
        body = parse_alternation()
        expect(";")
        rules[name] = body

    return rules


# ── FIRST / FOLLOW / Nullable ────────────────────────────────────

def compute_nullable(rules):
    """Compute which nonterminals can derive ε."""
    nullable = set()
    changed = True
    while changed:
        changed = False
        for name, body in rules.items():
            if name not in nullable and is_nullable(body, nullable):
                nullable.add(name)
                changed = True
    return nullable


def is_nullable(node, nullable):
    """Check if a grammar node can derive ε."""
    if node == EPSILON:
        return True
    if isinstance(node, Terminal):
        return False
    if isinstance(node, Nonterminal):
        return node.name in nullable
    if isinstance(node, Sequence):
        return all(is_nullable(item, nullable) for item in node.items)
    if isinstance(node, Alternation):
        return any(is_nullable(alt, nullable) for alt in node.alts)
    if isinstance(node, Optional):
        return True
    if isinstance(node, Repetition):
        return True
    return False


def compute_first_table(rules, nullable):
    """FIRST set of every nonterminal, by fixpoint iteration.

    The textbook algorithm, deliberately unlike the depth-first walks of check_ll1.c and
    check_ll1.rs: start every set empty and grow them until nothing changes. A recursive rule
    (`a = b 'x' ; b = a | 'y' ;`) reaches its full set because the iteration revisits it,
    where a memo filled in mid-recursion would keep a partial one."""
    table = {name: set() for name in rules}
    changed = True
    while changed:
        changed = False
        for name, body in rules.items():
            new = compute_first(body, table, nullable)
            if not new <= table[name]:
                table[name] |= new
                changed = True
    return table


def compute_first(node, table, nullable):
    """FIRST set of a grammar node (set of terminals), reading nonterminals from `table`."""
    result = set()
    if node == EPSILON:
        pass
    elif isinstance(node, Terminal):
        result.add(node)
    elif isinstance(node, Nonterminal):
        result |= table.get(node.name, set())
    elif isinstance(node, Sequence):
        for item in node.items:
            result |= compute_first(item, table, nullable)
            if not is_nullable(item, nullable):
                break
    elif isinstance(node, Alternation):
        for alt in node.alts:
            result |= compute_first(alt, table, nullable)
    elif isinstance(node, (Optional, Repetition)):
        result |= compute_first(node.body, table, nullable)
    return result


# ── FOLLOW ───────────────────────────────────────────────────────

EOF_TOKEN = "<eof>"


def first_names(node, table, nullable):
    return {str(t) for t in compute_first(node, table, nullable)}


def seq_follows(items, fol, table, nullable):
    """For each item of a sequence, the tokens that can follow it: the FIRST of the items after
    it up to the first non-nullable one, and the sequence's own FOLLOW if all of those can
    vanish."""
    out = []
    for i in range(len(items)):
        f = set()
        rest_nullable = True
        for nxt in items[i + 1:]:
            f |= first_names(nxt, table, nullable)
            if not is_nullable(nxt, nullable):
                rest_nullable = False
                break
        if rest_nullable:
            f |= fol
        out.append(f)
    return out


def compute_follow_table(rules, nullable, table):
    """FOLLOW set of every nonterminal, by fixpoint iteration. The first rule is the start
    symbol, followed by end of input."""
    follow = {name: set() for name in rules}
    if rules:
        follow[next(iter(rules))].add(EOF_TOKEN)

    def walk(node, fol):
        changed = False
        if isinstance(node, Nonterminal):
            if node.name in follow and not fol <= follow[node.name]:
                follow[node.name] |= fol
                changed = True
        elif isinstance(node, Sequence):
            for item, f in zip(node.items, seq_follows(node.items, fol, table, nullable)):
                changed |= walk(item, f)
        elif isinstance(node, Alternation):
            for alt in node.alts:
                changed |= walk(alt, fol)
        elif isinstance(node, Optional):
            changed |= walk(node.body, fol)
        elif isinstance(node, Repetition):
            changed |= walk(node.body, fol | first_names(node.body, table, nullable))
        return changed

    changed = True
    while changed:
        changed = False
        for name, body in rules.items():
            changed |= walk(body, set(follow[name]))
    return follow


# ── LL(1) Conflict Detection ─────────────────────────────────────

def check_ll1(rules, canonical=None):
    """Check every choice point for LL(1) conflicts. Returns list of issues.

    FIRST/FIRST: two alternatives of one alternation start with a common token.
    FIRST/FOLLOW: a choice that may derive nothing (an optional, a repetition, an alternation
    with a nullable alternative) can be entered on a token that may also FOLLOW it, so one
    token of lookahead cannot decide whether to enter it (bug 329: until then no checker
    computed FOLLOW, and these conflicts passed).

    `canonical`, when a list, receives one `conflict ...` / `conflict-follow ...` line per issue."""
    nullable = compute_nullable(rules)
    table = compute_first_table(rules, nullable)
    follow = compute_follow_table(rules, nullable, table)
    issues = []

    def follow_conflict(path, kind, overlap):
        toks = sorted(overlap)
        if canonical is not None:
            canonical.append(f"conflict-follow {path} {kind} " + ",".join(toks))
        issues.append(f"  {path}: {kind} can be entered on a token that may follow it: "
                      f"{{{', '.join(toks)}}}")

    def check_node(node, path, fol):
        if isinstance(node, Alternation):
            # Check pairwise FIRST set disjointness
            alt_firsts = []
            for j, alt in enumerate(node.alts):
                first = compute_first(alt, table, nullable)
                alt_firsts.append((j, alt, first))

            for a_idx in range(len(alt_firsts)):
                for b_idx in range(a_idx + 1, len(alt_firsts)):
                    j_a, alt_a, first_a = alt_firsts[a_idx]
                    j_b, alt_b, first_b = alt_firsts[b_idx]
                    overlap = first_a & first_b
                    if overlap:
                        overlap_str = ", ".join(str(t) for t in sorted(overlap, key=str))
                        if canonical is not None:
                            canonical.append(
                                f"conflict {path} {j_a+1} {j_b+1} "
                                + ",".join(sorted(str(t) for t in overlap)))
                        issues.append(
                            f"  {path}: alternatives {j_a+1} and {j_b+1} "
                            f"share FIRST tokens: {{{overlap_str}}}\n"
                            f"    alt {j_a+1}: {alt_a}\n"
                            f"    alt {j_b+1}: {alt_b}"
                        )

            # An alternation that can derive nothing: no alternative may start with a token
            # that can follow the alternation.
            if any(is_nullable(alt, nullable) for alt in node.alts):
                for j, alt in enumerate(node.alts):
                    overlap = first_names(alt, table, nullable) & fol
                    if overlap:
                        follow_conflict(path, f"alt{j+1}", overlap)

            # Recurse into each alternative
            for j, alt in enumerate(node.alts):
                check_node(alt, f"{path}|{j+1}", fol)

        elif isinstance(node, Sequence):
            for j, (item, f) in enumerate(zip(node.items, seq_follows(node.items, fol, table, nullable))):
                check_node(item, f"{path}.{j+1}", f)

        elif isinstance(node, Optional):
            overlap = first_names(node.body, table, nullable) & fol
            if overlap:
                follow_conflict(path, "opt", overlap)
            check_node(node.body, f"{path}[?]", fol)

        elif isinstance(node, Repetition):
            overlap = first_names(node.body, table, nullable) & fol
            if overlap:
                follow_conflict(path, "rep", overlap)
            check_node(node.body, f"{path}{{*}}", fol | first_names(node.body, table, nullable))

    for name, body in rules.items():
        check_node(body, name, set(follow[name]))

    return issues


# ── Main ─────────────────────────────────────────────────────────

def canonical_report(rules):
    """The --canonical form: every rule's nullability and FIRST set, every rule's FOLLOW set,
    then every conflict."""
    nullable = compute_nullable(rules)
    table = compute_first_table(rules, nullable)
    follow = compute_follow_table(rules, nullable, table)
    lines = []
    for name in rules:
        first = sorted(str(t) for t in table[name])
        flag = "nullable" if name in nullable else "-"
        lines.append(" ".join([f"rule {name} {flag}"] + first))
    for name in rules:
        lines.append(" ".join([f"follow {name}"] + sorted(follow[name])))
    conflicts = []
    check_ll1(rules, conflicts)
    return lines + conflicts


def main():
    args = sys.argv[1:]
    canonical = "--canonical" in args
    args = [a for a in args if a != "--canonical"]
    grammar_path = Path(args[0]) if args else Path("grammar/concrete.ebnf")
    if not grammar_path.exists():
        print(f"error: grammar file not found: {grammar_path}", file=sys.stderr)
        sys.exit(2)

    text = grammar_path.read_text()
    try:
        rules = parse_ebnf(text)
    except ValueError as e:
        print(f"error: failed to parse grammar: {e}", file=sys.stderr)
        sys.exit(2)

    if canonical:
        lines = canonical_report(rules)
        for line in lines:
            print(line)
        sys.exit(1 if any(l.startswith(("conflict ", "conflict-follow ")) for l in lines) else 0)

    print(f"Parsed {len(rules)} grammar rules from {grammar_path}")

    issues = check_ll1(rules)

    if issues:
        print(f"\nFound {len(issues)} LL(1) conflict(s):\n")
        for issue in issues:
            print(issue)
            print()
        sys.exit(1)
    else:
        print("LL(1) check passed: no FIRST/FIRST or FIRST/FOLLOW conflicts found.")
        sys.exit(0)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
LL(1) grammar checker for Concrete.

Parses grammar/concrete.ebnf and verifies the LL(1) property:
for every alternation (A | B | C), the FIRST sets of A, B, C
must be pairwise disjoint (after accounting for nullable rules).

Usage:
    python3 scripts/check_ll1.py [grammar/concrete.ebnf]

Exit code 0 = LL(1) clean, 1 = conflicts found.
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


def compute_first(node, rules, nullable, memo=None):
    """Compute the FIRST set of a grammar node (set of terminals)."""
    if memo is None:
        memo = {}

    key = id(node)
    if key in memo:
        return memo[key]
    # Mark as being computed (avoid infinite recursion)
    memo[key] = set()
    result = set()

    if node == EPSILON:
        pass
    elif isinstance(node, Terminal):
        result.add(node)
    elif isinstance(node, Nonterminal):
        if node.name in rules:
            result |= compute_first(rules[node.name], rules, nullable, memo)
    elif isinstance(node, Sequence):
        for item in node.items:
            result |= compute_first(item, rules, nullable, memo)
            if not is_nullable(item, nullable):
                break
    elif isinstance(node, Alternation):
        for alt in node.alts:
            result |= compute_first(alt, rules, nullable, memo)
    elif isinstance(node, Optional):
        result |= compute_first(node.body, rules, nullable, memo)
    elif isinstance(node, Repetition):
        result |= compute_first(node.body, rules, nullable, memo)

    memo[key] = result
    return result


# ── LL(1) Conflict Detection ─────────────────────────────────────

def check_ll1(rules):
    """Check all alternations for LL(1) conflicts. Returns list of issues."""
    nullable = compute_nullable(rules)
    issues = []
    memo = {}

    def check_node(node, path):
        if isinstance(node, Alternation):
            # Check pairwise FIRST set disjointness
            alt_firsts = []
            for j, alt in enumerate(node.alts):
                first = compute_first(alt, rules, nullable, memo)
                alt_firsts.append((j, alt, first))

            for a_idx in range(len(alt_firsts)):
                for b_idx in range(a_idx + 1, len(alt_firsts)):
                    j_a, alt_a, first_a = alt_firsts[a_idx]
                    j_b, alt_b, first_b = alt_firsts[b_idx]
                    overlap = first_a & first_b
                    if overlap:
                        overlap_str = ", ".join(str(t) for t in sorted(overlap, key=str))
                        issues.append(
                            f"  {path}: alternatives {j_a+1} and {j_b+1} "
                            f"share FIRST tokens: {{{overlap_str}}}\n"
                            f"    alt {j_a+1}: {alt_a}\n"
                            f"    alt {j_b+1}: {alt_b}"
                        )

            # Recurse into each alternative
            for j, alt in enumerate(node.alts):
                check_node(alt, f"{path}|{j+1}")

        elif isinstance(node, Sequence):
            for j, item in enumerate(node.items):
                check_node(item, f"{path}.{j+1}")

        elif isinstance(node, Optional):
            # Optional: FIRST(body) must not overlap with FOLLOW
            # (We approximate: just recurse into body)
            check_node(node.body, f"{path}[?]")

        elif isinstance(node, Repetition):
            check_node(node.body, f"{path}{{*}}")

    for name, body in rules.items():
        check_node(body, name)

    return issues


# ── Main ─────────────────────────────────────────────────────────

def main():
    grammar_path = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("grammar/concrete.ebnf")
    if not grammar_path.exists():
        print(f"error: grammar file not found: {grammar_path}", file=sys.stderr)
        sys.exit(1)

    text = grammar_path.read_text()
    try:
        rules = parse_ebnf(text)
    except ValueError as e:
        print(f"error: failed to parse grammar: {e}", file=sys.stderr)
        sys.exit(1)

    print(f"Parsed {len(rules)} grammar rules from {grammar_path}")

    issues = check_ll1(rules)

    if issues:
        print(f"\nFound {len(issues)} LL(1) conflict(s):\n")
        for issue in issues:
            print(issue)
            print()
        sys.exit(1)
    else:
        print("LL(1) check passed: no FIRST/FIRST conflicts found.")
        sys.exit(0)


if __name__ == "__main__":
    main()

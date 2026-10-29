///! LL(1) grammar checker for Concrete (Rust implementation)
///!
///! Parses grammar/concrete.ebnf and verifies the LL(1) property:
///! for every alternation, FIRST sets of alternatives must be disjoint.
///!
///! Build:  rustc -O -o scripts/check_ll1_rs scripts/check_ll1.rs
///! Usage:  ./scripts/check_ll1_rs [--canonical] [grammar/concrete.ebnf]
///!
///! Exit 0 = LL(1) clean, 1 = conflicts found, 2 = bad input.
///!
///! One of three deliberately independent implementations (check_ll1.py, check_ll1.c);
///! scripts/tests/check_grammar_ll1.sh diffs their --canonical output, whose format is
///! documented in check_ll1.py.

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::process;

// ── AST ──────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
enum Node {
    Terminal(String),
    Nonterminal(String),
    Sequence(Vec<Node>),
    Alternation(Vec<Node>),
    Optional(Box<Node>),
    Repetition(Box<Node>),
    Epsilon,
}

// ── EBNF Tokenizer ──────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Tok {
    Term(String),
    Id(String),
    Eq,
    Semi,
    Pipe,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Comma,
    Plus,
    Dot,
    End,
}

const TOKEN_CLASSES: &[&str] = &["IDENT", "INT", "FLOAT", "STRING", "CHAR", "LABEL"];

fn is_token_class(s: &str) -> bool {
    TOKEN_CLASSES.contains(&s)
}

fn tokenize(text: &str) -> Vec<Tok> {
    let bytes = text.as_bytes();
    let mut toks = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        let ch = bytes[i] as char;
        // Skip whitespace
        if ch.is_ascii_whitespace() {
            i += 1;
            continue;
        }
        // Skip comments (* ... *)
        if i + 1 < bytes.len() && bytes[i] == b'(' && bytes[i + 1] == b'*' {
            match text[i + 2..].find("*)") {
                Some(pos) => {
                    i = i + 2 + pos + 2;
                    continue;
                }
                None => {
                    eprintln!("error: unterminated comment at position {i}");
                    process::exit(2);
                }
            }
        }
        // Single-char tokens
        let tok = match ch {
            '=' => Some(Tok::Eq),
            ';' => Some(Tok::Semi),
            '|' => Some(Tok::Pipe),
            '[' => Some(Tok::LBrack),
            ']' => Some(Tok::RBrack),
            '{' => Some(Tok::LBrace),
            '}' => Some(Tok::RBrace),
            '(' => Some(Tok::LParen),
            ')' => Some(Tok::RParen),
            ',' => Some(Tok::Comma),
            '+' => Some(Tok::Plus),
            '.' => Some(Tok::Dot),
            _ => None,
        };
        if let Some(t) = tok {
            toks.push(t);
            i += 1;
            continue;
        }
        // Quoted terminal
        if ch == '\'' {
            let j = text[i + 1..]
                .find('\'')
                .unwrap_or_else(|| {
                    eprintln!("error: unterminated terminal at position {i}");
                    process::exit(2);
                })
                + i
                + 1;
            let val = format!("'{}'", &text[i + 1..j]);
            toks.push(Tok::Term(val));
            i = j + 1;
            continue;
        }
        // Identifier
        if ch.is_ascii_alphabetic() || ch == '_' {
            let j = text[i..]
                .find(|c: char| !c.is_ascii_alphanumeric() && c != '_')
                .map(|p| p + i)
                .unwrap_or(bytes.len());
            let val = &text[i..j];
            toks.push(Tok::Id(val.to_string()));
            i = j;
            continue;
        }
        eprintln!("error: unexpected character {ch:?} at position {i}");
        process::exit(2);
    }
    toks.push(Tok::End);
    toks
}

// ── EBNF Parser ─────────────────────────────────────────────────

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
}

impl Parser {
    fn new(toks: Vec<Tok>) -> Self {
        Self { toks, pos: 0 }
    }

    fn peek(&self) -> &Tok {
        &self.toks[self.pos]
    }

    fn advance(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        self.pos += 1;
        t
    }

    fn expect(&mut self, expected: &Tok) {
        let got = self.advance();
        if std::mem::discriminant(&got) != std::mem::discriminant(expected) {
            eprintln!("error: expected {expected:?}, got {got:?} at token {}", self.pos - 1);
            process::exit(2);
        }
    }

    fn parse_alternation(&mut self) -> Node {
        let mut alts = vec![self.parse_sequence()];
        while *self.peek() == Tok::Pipe {
            self.advance();
            alts.push(self.parse_sequence());
        }
        if alts.len() == 1 {
            alts.pop().unwrap()
        } else {
            Node::Alternation(alts)
        }
    }

    fn parse_sequence(&mut self) -> Node {
        let mut items = Vec::new();
        loop {
            match self.peek() {
                Tok::Pipe | Tok::Semi | Tok::RBrack | Tok::RBrace | Tok::RParen | Tok::End => {
                    break
                }
                _ => items.push(self.parse_atom()),
            }
        }
        match items.len() {
            0 => Node::Epsilon,
            1 => items.pop().unwrap(),
            _ => Node::Sequence(items),
        }
    }

    fn parse_atom(&mut self) -> Node {
        match self.peek().clone() {
            Tok::LBrack => {
                self.advance();
                let body = self.parse_alternation();
                self.expect(&Tok::RBrack);
                Node::Optional(Box::new(body))
            }
            Tok::LBrace => {
                self.advance();
                let body = self.parse_alternation();
                self.expect(&Tok::RBrace);
                Node::Repetition(Box::new(body))
            }
            Tok::LParen => {
                self.advance();
                let body = self.parse_alternation();
                self.expect(&Tok::RParen);
                body
            }
            Tok::Term(val) => {
                self.advance();
                Node::Terminal(val)
            }
            Tok::Id(val) => {
                self.advance();
                if is_token_class(&val) {
                    Node::Terminal(val)
                } else {
                    Node::Nonterminal(val)
                }
            }
            other => {
                eprintln!("error: unexpected token {other:?} at position {}", self.pos);
                process::exit(2);
            }
        }
    }

    fn parse_rules(&mut self) -> Vec<(String, Node)> {
        let mut rules = Vec::new();
        while *self.peek() != Tok::End {
            let name = match self.advance() {
                Tok::Id(n) => n,
                other => {
                    eprintln!("error: expected rule name, got {other:?}");
                    process::exit(2);
                }
            };
            self.expect(&Tok::Eq);
            let body = self.parse_alternation();
            self.expect(&Tok::Semi);
            rules.push((name, body));
        }
        rules
    }
}

// ── Nullable ────────────────────────────────────────────────────

fn compute_nullable(rules: &[(String, Node)]) -> HashSet<String> {
    let mut nullable = HashSet::new();
    let mut changed = true;
    while changed {
        changed = false;
        for (name, body) in rules {
            if !nullable.contains(name) && is_nullable(body, &nullable) {
                nullable.insert(name.clone());
                changed = true;
            }
        }
    }
    nullable
}

fn is_nullable(node: &Node, nullable: &HashSet<String>) -> bool {
    match node {
        Node::Epsilon => true,
        Node::Terminal(_) => false,
        Node::Nonterminal(name) => nullable.contains(name),
        Node::Sequence(items) => items.iter().all(|i| is_nullable(i, nullable)),
        Node::Alternation(alts) => alts.iter().any(|a| is_nullable(a, nullable)),
        Node::Optional(_) | Node::Repetition(_) => true,
    }
}

// ── FIRST set ───────────────────────────────────────────────────

fn compute_first(
    node: &Node,
    rules: &HashMap<String, &Node>,
    nullable: &HashSet<String>,
    visiting: &mut HashSet<String>,
) -> HashSet<String> {
    match node {
        Node::Epsilon => HashSet::new(),
        Node::Terminal(name) => {
            let mut s = HashSet::new();
            s.insert(name.clone());
            s
        }
        Node::Nonterminal(name) => {
            if visiting.contains(name) {
                return HashSet::new(); // cycle guard
            }
            if let Some(body) = rules.get(name) {
                visiting.insert(name.clone());
                let result = compute_first(body, rules, nullable, visiting);
                visiting.remove(name);
                result
            } else {
                HashSet::new()
            }
        }
        Node::Sequence(items) => {
            let mut result = HashSet::new();
            for item in items {
                result.extend(compute_first(item, rules, nullable, visiting));
                if !is_nullable(item, nullable) {
                    break;
                }
            }
            result
        }
        Node::Alternation(alts) => {
            let mut result = HashSet::new();
            for alt in alts {
                result.extend(compute_first(alt, rules, nullable, visiting));
            }
            result
        }
        Node::Optional(body) | Node::Repetition(body) => {
            compute_first(body, rules, nullable, visiting)
        }
    }
}

// ── FOLLOW set ──────────────────────────────────────────────────

const EOF_TOKEN: &str = "<eof>";

fn first_of(node: &Node, rules: &HashMap<String, &Node>, nullable: &HashSet<String>) -> HashSet<String> {
    let mut visiting = HashSet::new();
    compute_first(node, rules, nullable, &mut visiting)
}

/// What can follow item `i` of a sequence: FIRST of the items after it up to the first
/// non-nullable one, plus the sequence's own FOLLOW if all of those can vanish.
fn seq_follow(
    items: &[Node],
    i: usize,
    fol: &HashSet<String>,
    rules: &HashMap<String, &Node>,
    nullable: &HashSet<String>,
) -> HashSet<String> {
    let mut out = HashSet::new();
    let mut rest_nullable = true;
    for nxt in &items[i + 1..] {
        out.extend(first_of(nxt, rules, nullable));
        if !is_nullable(nxt, nullable) {
            rest_nullable = false;
            break;
        }
    }
    if rest_nullable {
        out.extend(fol.iter().cloned());
    }
    out
}

fn follow_walk(
    node: &Node,
    fol: &HashSet<String>,
    follow: &mut HashMap<String, HashSet<String>>,
    rules: &HashMap<String, &Node>,
    nullable: &HashSet<String>,
) -> bool {
    match node {
        Node::Nonterminal(name) => match follow.get_mut(name) {
            Some(set) => {
                let before = set.len();
                set.extend(fol.iter().cloned());
                set.len() != before
            }
            None => false,
        },
        Node::Sequence(items) => {
            let mut changed = false;
            for (i, item) in items.iter().enumerate() {
                let f = seq_follow(items, i, fol, rules, nullable);
                changed |= follow_walk(item, &f, follow, rules, nullable);
            }
            changed
        }
        Node::Alternation(alts) => {
            let mut changed = false;
            for alt in alts {
                changed |= follow_walk(alt, fol, follow, rules, nullable);
            }
            changed
        }
        Node::Optional(body) => follow_walk(body, fol, follow, rules, nullable),
        Node::Repetition(body) => {
            let mut f = fol.clone();
            f.extend(first_of(body, rules, nullable));
            follow_walk(body, &f, follow, rules, nullable)
        }
        _ => false,
    }
}

/// FOLLOW of every nonterminal, by fixpoint; the first rule is the start symbol.
fn compute_follow(
    rule_list: &[(String, Node)],
    rules: &HashMap<String, &Node>,
    nullable: &HashSet<String>,
) -> HashMap<String, HashSet<String>> {
    let mut follow: HashMap<String, HashSet<String>> =
        rule_list.iter().map(|(n, _)| (n.clone(), HashSet::new())).collect();
    if let Some((start, _)) = rule_list.first() {
        follow.get_mut(start).unwrap().insert(EOF_TOKEN.to_string());
    }
    let mut changed = true;
    while changed {
        changed = false;
        for (name, body) in rule_list {
            let f = follow[name].clone();
            changed |= follow_walk(body, &f, &mut follow, rules, nullable);
        }
    }
    follow
}

// ── LL(1) check ─────────────────────────────────────────────────

struct Checker<'a> {
    rules: &'a HashMap<String, &'a Node>,
    nullable: &'a HashSet<String>,
    issues: Vec<String>,
    canonical: bool,
}

impl<'a> Checker<'a> {
    /// A FIRST/FOLLOW conflict: a choice that may derive nothing can be entered on a token that
    /// may also follow it (bug 329: until then no checker computed FOLLOW).
    fn follow_conflict(&mut self, path: &str, kind: &str, overlap: &HashSet<String>) {
        let mut sorted: Vec<&str> = overlap.iter().map(|s| s.as_str()).collect();
        sorted.sort();
        if self.canonical {
            self.issues.push(format!("conflict-follow {path} {kind} {}", sorted.join(",")));
        } else {
            self.issues.push(format!(
                "  {path}: {kind} can be entered on a token that may follow it: {{{}}}",
                sorted.join(", ")
            ));
        }
    }

    fn check_node(&mut self, node: &Node, path: &str, fol: &HashSet<String>) {
        let (rules, nullable) = (self.rules, self.nullable);
        match node {
            Node::Alternation(alts) => {
                let firsts: Vec<HashSet<String>> =
                    alts.iter().map(|alt| first_of(alt, rules, nullable)).collect();
                for a in 0..firsts.len() {
                    for b in (a + 1)..firsts.len() {
                        let overlap: Vec<&String> = firsts[a].intersection(&firsts[b]).collect();
                        if !overlap.is_empty() {
                            let mut sorted: Vec<&str> = overlap.iter().map(|s| s.as_str()).collect();
                            sorted.sort();
                            if self.canonical {
                                self.issues.push(format!(
                                    "conflict {path} {} {} {}",
                                    a + 1,
                                    b + 1,
                                    sorted.join(",")
                                ));
                            } else {
                                self.issues.push(format!(
                                    "  {path}: alternatives {} and {} share FIRST tokens: {{{}}}",
                                    a + 1,
                                    b + 1,
                                    sorted.join(", ")
                                ));
                            }
                        }
                    }
                }
                // An alternation that can derive nothing: no alternative may start with a token
                // that can follow the alternation.
                if alts.iter().any(|alt| is_nullable(alt, nullable)) {
                    for (i, fi) in firsts.iter().enumerate() {
                        let overlap: HashSet<String> = fi.intersection(fol).cloned().collect();
                        if !overlap.is_empty() {
                            self.follow_conflict(path, &format!("alt{}", i + 1), &overlap);
                        }
                    }
                }
                for (i, alt) in alts.iter().enumerate() {
                    self.check_node(alt, &format!("{path}|{}", i + 1), fol);
                }
            }
            Node::Sequence(items) => {
                for (i, item) in items.iter().enumerate() {
                    let f = seq_follow(items, i, fol, rules, nullable);
                    self.check_node(item, &format!("{path}.{}", i + 1), &f);
                }
            }
            // The same path spelling as check_ll1.py: `[?]` into an optional, `{*}` into a repetition.
            Node::Optional(body) | Node::Repetition(body) => {
                let is_opt = matches!(node, Node::Optional(_));
                let fb = first_of(body, rules, nullable);
                let overlap: HashSet<String> = fb.intersection(fol).cloned().collect();
                if !overlap.is_empty() {
                    self.follow_conflict(path, if is_opt { "opt" } else { "rep" }, &overlap);
                }
                let mut inner = fol.clone();
                if !is_opt {
                    inner.extend(fb);
                }
                let sub = if is_opt { format!("{path}[?]") } else { format!("{path}{{*}}") };
                self.check_node(body, &sub, &inner);
            }
            _ => {}
        }
    }
}

fn check_all(
    rule_list: &[(String, Node)],
    rules: &HashMap<String, &Node>,
    nullable: &HashSet<String>,
    follow: &HashMap<String, HashSet<String>>,
    canonical: bool,
) -> Vec<String> {
    let mut checker = Checker { rules, nullable, issues: Vec::new(), canonical };
    for (name, body) in rule_list {
        checker.check_node(body, name, &follow[name]);
    }
    checker.issues
}

// ── Main ────────────────────────────────────────────────────────

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let canonical = args.iter().any(|a| a == "--canonical");
    let path = args
        .iter()
        .find(|a| a.as_str() != "--canonical")
        .map(|a| a.as_str())
        .unwrap_or("grammar/concrete.ebnf");

    let text = match fs::read_to_string(path) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("error: cannot open {path}: {e}");
            process::exit(2);
        }
    };

    let toks = tokenize(&text);
    let mut parser = Parser::new(toks);
    let rule_list = parser.parse_rules();

    let nullable = compute_nullable(&rule_list);

    let rule_map: HashMap<String, &Node> =
        rule_list.iter().map(|(n, b)| (n.clone(), b)).collect();
    let follow = compute_follow(&rule_list, &rule_map, &nullable);

    if canonical {
        for (name, body) in &rule_list {
            let mut first: Vec<String> = first_of(body, &rule_map, &nullable).into_iter().collect();
            first.sort();
            let flag = if nullable.contains(name) { "nullable" } else { "-" };
            let mut line = format!("rule {name} {flag}");
            for t in &first {
                line.push(' ');
                line.push_str(t);
            }
            println!("{line}");
        }
        for (name, _) in &rule_list {
            let mut fol: Vec<&String> = follow[name].iter().collect();
            fol.sort();
            let mut line = format!("follow {name}");
            for t in fol {
                line.push(' ');
                line.push_str(t);
            }
            println!("{line}");
        }
        let issues = check_all(&rule_list, &rule_map, &nullable, &follow, true);
        for issue in &issues {
            println!("{issue}");
        }
        process::exit(if issues.is_empty() { 0 } else { 1 });
    }

    println!("Parsed {} grammar rules from {path}", rule_list.len());

    let issues = check_all(&rule_list, &rule_map, &nullable, &follow, false);

    if !issues.is_empty() {
        println!("\nFound {} LL(1) conflict(s):\n", issues.len());
        for issue in &issues {
            println!("{issue}\n");
        }
        process::exit(1);
    }

    println!("LL(1) check passed: no FIRST/FIRST or FIRST/FOLLOW conflicts found.");
}

# Semantic Query Interface

**Status:** Open

Concrete should eventually expose compiler-produced facts through a query interface that humans, scripts, editors, CI, and LLM agents can use without grepping source code.

Inspired by: Dmitri Sotnikov, ["Giving LLMs a Formal Reasoning Engine for Code Analysis"](https://yogthos.net/posts/2026-04-08-neurosymbolic-mcp.html), April 8, 2026.

## The Lesson To Copy

LLM coding agents are weak at transitive structural analysis if the only tool is grep.

Reachability, impact, call cycles, dead code, authority paths, allocation paths, proof impact, and trust drift are graph / logic questions. They should be answered by querying a fact graph, not by making an LLM iteratively search files and reconstruct partial call chains in its context window.

## Concrete's Advantage

The post describes a tree-sitter -> Prolog path for languages where the external tool must reconstruct code structure.

Concrete does not need to start there.

The compiler already knows or is learning:

1. qualified function definitions
2. function bodies and checked Core
3. call graph
4. recursion / SCCs
5. capabilities and transitive authority
6. allocation status
7. loop-boundedness status
8. FFI / trusted / host boundaries
9. proof/evidence status
10. body fingerprints
11. predictable-profile results
12. source locations, once threaded through reports

The first query engine should query compiler facts and artifacts.

## Queries To Support Eventually

Start with structural/evidence questions:

1. callers of `main.decode_header`
2. transitive callers of `main.decode_header`
3. callees reachable from `main.main`
4. path from `main.main` to `Network`, `File`, `Alloc`, FFI, trusted, blocking, or a specific function
5. functions that can reach `Alloc`
6. functions that fail `--check predictable`, grouped by gate
7. proved functions affected by a source change
8. proof-stale functions and the body fingerprint mismatch
9. dead functions after identifying configured entry points
10. direct and mutual call cycles
11. functions that depend on trusted assumptions
12. function-level evidence summary plus derivation/path trace

## Possible CLI Shape

Keep it boring first:

```text
concrete query callers main.decode_header
concrete query reaches main.main main.decode_header
concrete query path main.main --to-cap Network
concrete query authority-path main.main File
concrete query proof-impact main.parse_byte
concrete query predictable-failures
concrete query dead-functions
```

Every answer should have a small human version and a machine-readable version.

## Agent / MCP Direction

After machine-readable reports and the CLI query engine are useful, expose the same facts to agent tools.

An agent should ask:

```text
which functions transitively call decode_header?
can the packet parser core reach Alloc?
why is report_result not predictable?
which proofs became stale in this change?
```

and receive small structured answers with a derivation or path, not 500 lines of grep output.

## Non-Goals For Now

- do not add tree-sitter for Concrete before trying compiler-produced facts
- do not require Prolog/Z3 before ordinary graph queries have earned their keep
- do not start an MCP server before machine-readable reports / query CLI exist
- do not let query answers imply more evidence than the compiler checked

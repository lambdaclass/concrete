#!/usr/bin/env python3
"""Select the gates a set of changed files can break.

Reads changed paths on stdin (one per line), writes `gate<TAB>reason` on stdout.

THE MAPPING IS READ OUT OF THE GATES, not hand-maintained. Gates name the files they
check — `check_byte_view.sh` contains `std/src/numeric.con` — so a curated list would
drift and this cannot. Gate-to-gate references come along for free and matter:
`check_mutation_anchors.sh` names `check_gate_mutation_coverage.sh`, so editing that
driver implicates the anchor gate.

IT IS NOT A PROOF OF COMPLETENESS. A gate can be broken by a change it never names — it
may compile a whole corpus, or assert a count derived from everything. Believing
otherwise would repeat the defect R-0483 removed from `ByteView`, where a check that
caught some substitutions read at the call site like one that caught all of them.
"""
import os
import re
import sys
import glob

PATH_RE = re.compile(
    r'(?:std|Concrete|examples|scripts|tests|docs|proofs|grammar|site)/[A-Za-z0-9_./-]+'
)

# Nearly every gate mentions `scripts/tests/` to reach a helper. Treating that as "this
# gate checks that tree" selects most of the suite, and a list that is always long is
# ignored exactly as fast as one that is always empty. Broad trees are covered by
# ALWAYS_IF instead, which is curated and carries a reason per entry.
UMBRELLA = {
    "scripts", "scripts/tests", "scripts/tests/lib", "scripts/gen", "scripts/ci",
    "docs", "tests", "tests/programs", "examples", "std", "std/src",
    "Concrete", "proofs", "site",
}

# A change anywhere in these trees moves something a derived artifact is built from, so
# the freshness gates run whether or not they name the file. This set is what four
# separate CI rounds of R-0483 cost to learn: the stdlib surface manifest, the
# shadow-body extraction coupling, six inert mutation anchors, and the ByteView gate.
ALWAYS_IF = {
    "std/": [
        ("check_stdlib_manifest.sh", "the public stdlib surface is a checked-in derivation"),
        ("check_attestation_manifest.sh", "package content moves attestation identities"),
        ("check_attestation_refs_order.sh", "generated references are compared byte for byte"),
        ("check_classification_freshness.sh", "the classification table is derived from it"),
        ("check_build_identity_freshness.sh", "the embedded identity covers the sources"),
        ("check_std_compiled_coverage.sh", "one compiled probe per std module"),
    ],
    "Concrete/": [
        ("check_build_identity_freshness.sh", "the embedded identity covers these sources"),
        ("check_attestation_manifest.sh", "generated attestations derive from compiler output"),
        ("check_attestation_refs_order.sh", "generated references are compared byte for byte"),
        ("check_classification_freshness.sh", "the classification table is derived from it"),
        ("check_shadow_body_v2.sh", "structural body digests and their coverage ratchet"),
        ("check_mutation_anchors.sh", "renames make mutation anchors INERT, and silently"),
        # A compiler change can alter ANY program's behaviour or report text, and
        # `run_tests.sh` names no compiler source because it compiles the whole corpus —
        # the "broken by a change it never names" case this file warns about, made
        # concrete. Its `--trust-gate` form is listed separately because it runs the
        # self-consistency section the plain form does not, and that is what caught a
        # DIAG-STATUS disagreement introduced by an R-0484 edit to Report.lean.
        ("run_tests.sh", "a compiler change can alter any program's behaviour or output"),
    ],
    "proofs/": [
        ("check_attestation_manifest.sh", "proof links name generated attestation symbols"),
        ("check_proof_freshness.sh", "proof-to-body binding"),
    ],
    "scripts/tests/": [
        ("check_gate_hygiene.sh", "every gate must be pipe-safe and fail loudly"),
        ("check_gate_registration.sh", "a gate nothing runs proves nothing"),
        ("check_mutation_anchors.sh", "driver edits move anchor text"),
    ],
}

# A change to ANY path can break these, so every non-empty diff selects them. They are kept
# apart from ALWAYS_IF because selecting a gate is not mapping a file: a path no rule maps
# still triggers the fail-safe (every gate), which an empty-prefix ALWAYS_IF entry would
# silently switch off. A gate belongs here only if it is cheap (seconds) and its input is
# the whole tree. Bug 252: the Markdown-link gate checks links into every tracked path and
# anchors into every Markdown heading, so a docs-only edit, a renamed heading or a deleted
# file can each break it, and before it was listed nothing selected it.
EVERY_CHANGE = [
    ("check_doc_links.sh", "a Markdown link can name any path, and its anchor any heading"),
]


def tokens(text, exists=os.path.exists):
    """Path tokens a gate names, keeping only those specific enough to mean something."""
    out = set()
    for m in PATH_RE.findall(text):
        t = m.rstrip('.,;:)"\'')
        if not t or t in UMBRELLA or t.rstrip("/") in UMBRELLA:
            continue
        # A token naming nothing is a stale reference, not a signal.
        if os.path.isfile(t) or os.path.isdir(t.rstrip("/")):
            out.add(t)
    return out


def covers(token, changed_path):
    """Does `token`, as written in a gate, cover `changed_path`?"""
    if token == changed_path:
        return True
    if token.endswith("/") and changed_path.startswith(token):
        return True
    return changed_path.startswith(token.rstrip("/") + "/")


# NOT AUTO-RUN, but still listed — the point is to say what a change touches, and
# silently dropping something from the list would be the same lie as a short list read as
# "nothing can break".
#
#   run_ci_gates_local.sh          executes most of the suite itself, so running it from a
#                                  SELECTIVE runner defeats the selection, and it holds the
#                                  repository lock that exclusive gates need — which makes
#                                  those refuse and look like failures rather than like the
#                                  correct fail-closed refusal they are.
#   check_gate_mutation_coverage.sh  bare, this is a full campaign: hours, exclusive access,
#                                  and it mutates a disposable copy of the tree. It takes
#                                  FAMILY_ID/FAMILY_SPEC for a single-family run, which is
#                                  a deliberate act, not something to trigger by editing a
#                                  file it happens to name.
# A SCRIPT IS NOT A GATE — an invocation is. CI runs `run_tests.sh` plain AND
# `run_tests.sh --trust-gate`, which exercise different sections; the trust gate caught a
# DIAG-STATUS inconsistency that the plain run does not check. Selecting the script while
# suggesting only its bare form is a mapping that looks complete and is not, so the
# flagged forms are named here and printed alongside.
EXTRA_INVOCATIONS = {
    "run_tests.sh": ["run_tests.sh --trust-gate"],
    "run_fast_surface_gates.sh": ["run_fast_surface_gates.sh --mutate"],
}

NOT_AUTO_RUN = {
    "run_ci_gates_local.sh": "runs most of the suite itself; run it directly",
    "check_gate_mutation_coverage.sh": "full campaign, exclusive and hours; use FAMILY_ID for one family",
}


def select(changed, gate_paths=None):
    """-> {gate basename: [reasons]}"""
    if gate_paths is None:
        gate_paths = sorted(
            glob.glob("scripts/tests/check_*.sh") + glob.glob("scripts/tests/run_*.sh")
        )
    selected = {}
    for gate in gate_paths:
        name = os.path.basename(gate)
        try:
            toks = tokens(open(gate, errors="ignore").read())
        except OSError:
            continue
        for ch in changed:
            hit = next((t for t in toks if covers(t, ch)), None)
            if hit:
                selected.setdefault(name, []).append(f"names {hit}")
                break
    for prefix, gates in ALWAYS_IF.items():
        if any(c.startswith(prefix) for c in changed):
            for g, why in gates:
                if os.path.exists(f"scripts/tests/{g}"):
                    selected.setdefault(g, []).append(why)
    if changed:
        for g, why in EVERY_CHANGE:
            if os.path.exists(f"scripts/tests/{g}"):
                selected.setdefault(g, []).append(why)
    return selected


def main():
    changed = [l.strip() for l in sys.stdin if l.strip()]
    for gate, reasons in sorted(select(changed).items()):
        # One reason is enough to act on; more would bury the gate name.
        why = sorted(set(reasons))[0]
        if gate in NOT_AUTO_RUN:
            why += f"  [not auto-run: {NOT_AUTO_RUN[gate]}]"
        print(f"{gate}\t{why}")
        for extra in EXTRA_INVOCATIONS.get(gate, []):
            print(f"{extra}\tCI also runs this flagged form, which checks different sections")


if __name__ == "__main__":
    main()

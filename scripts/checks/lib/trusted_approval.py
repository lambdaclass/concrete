"""Verify owner authority over the trusted-base registry, theorem axiom closure and toolchain pins.

Owner decisions live in docs/decisions.md (guarded by the single-writer rule and
scripts/review/review.sh's owner-decision checks, ISS-0011 and ISS-0013). The
approval record in tests/golden/slice0_trusted_base_approval.json binds the
verbatim owner decision records in docs/decisions.md to the content digests of:
  - the trusted-component and premise registry rows in the report (registry_sha256);
  - the headline theorem axiom closure rows in the report (theorems_sha256);
  - the toolchain and dependency pins (flake.nix, flake.lock, lean-toolchain,
    lake-manifest.json).

Because the record binds content digests rather than a git ancestor SHA or a
mutable CI variable, rebasing a branch never invalidates an unchanged baseline,
while any unapproved change to a component, premise, theorem axiom closure,
toolchain pin, or cited decision record in docs/decisions.md fails closed.
"""
import hashlib
import json
from pathlib import Path
import re
import sys

INPUT_FILES = ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json")
DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")


def registry_rows(text):
    return "".join(line + "\n" for line in text.splitlines()
                   if line.startswith(("component ", "premise "))).encode()


def theorem_rows(text):
    return "".join(line + "\n" for line in text.splitlines()
                   if line.startswith(("theorem ", "axiom "))).encode()


def input_digests(root=Path(".")):
    root = Path(root)
    return {
        source: hashlib.sha256((root / source).read_bytes()).hexdigest()
        for source in INPUT_FILES
    }


def _decision_window(decisions_text, frag):
    marker = f'<a id="{frag}"></a>'
    idx = decisions_text.find(marker)
    if idx < 0:
        return None
    tail = decisions_text[idx + len(marker):]
    end = len(tail)
    for boundary in ('\n- <a id="decided-', "\n## "):
        pos = tail.find(boundary)
        if pos >= 0 and pos < end:
            end = pos
    return " ".join((marker + tail[:end]).split())


def verify_decisions(decisions_text, entries):
    if not isinstance(entries, list) or not entries:
        raise ValueError("owner approval must cite at least one decision record")
    for entry in entries:
        if not isinstance(entry, dict) or set(entry) != {"anchor", "date", "quote", "scope"}:
            raise ValueError("invalid owner decision entry schema")
        anchor = entry["anchor"]
        date = entry["date"]
        quote = entry["quote"]
        scope = entry["scope"]
        if not isinstance(anchor, str) or not anchor.startswith("docs/decisions.md#decided-"):
            raise ValueError(f"invalid owner decision anchor: {anchor}")
        frag = anchor.split("#", 1)[1]
        if not isinstance(date, str) or not DATE_RE.match(date) or date not in frag:
            raise ValueError(f"invalid owner decision date for {anchor}: {date}")
        if not isinstance(quote, str) or not quote.strip():
            raise ValueError(f"empty owner decision quote for {anchor}")
        if not isinstance(scope, str) or not scope.strip():
            raise ValueError(f"empty owner decision scope for {anchor}")
        window = _decision_window(decisions_text, frag)
        if window is None:
            raise ValueError(f"owner decision anchor missing in docs/decisions.md: {anchor}")
        if f"(owner, {date}:" not in window or f'"{quote}"' not in window:
            raise ValueError(f"owner decision record in docs/decisions.md does not match {anchor}")


def verify(report, record, decisions_file="docs/decisions.md"):
    record = Path(record)
    if not record.is_file():
        raise ValueError("owner approval record missing")
    approval = json.loads(record.read_bytes())
    expected_keys = {"schema", "decisions", "registry_sha256", "theorems_sha256", "inputs"}
    if not isinstance(approval, dict) or set(approval) != expected_keys or approval["schema"] != 1:
        raise ValueError("invalid owner approval schema")

    decisions_path = Path(decisions_file)
    if not decisions_path.is_file():
        raise ValueError("docs/decisions.md missing")
    report_text = Path(report).read_text()
    verify_decisions(decisions_path.read_text(), approval["decisions"])

    reg_bytes = registry_rows(report_text)
    if not reg_bytes:
        raise ValueError("trusted-base report has no registry rows")
    reg_digest = hashlib.sha256(reg_bytes).hexdigest()
    if reg_digest != approval["registry_sha256"]:
        raise ValueError("registry differs from owner-approved digest")

    thm_bytes = theorem_rows(report_text)
    if not thm_bytes:
        raise ValueError("trusted-base report has no theorem/axiom rows")
    thm_digest = hashlib.sha256(thm_bytes).hexdigest()
    if thm_digest != approval["theorems_sha256"]:
        raise ValueError("theorem axiom closure differs from owner-approved digest")

    inputs = approval["inputs"]
    if not isinstance(inputs, dict) or set(inputs) != set(INPUT_FILES):
        raise ValueError("invalid owner approval inputs schema")
    for source in INPUT_FILES:
        actual = hashlib.sha256(Path(source).read_bytes()).hexdigest()
        if actual != inputs[source]:
            raise ValueError(f"trust input {source} differs from owner-approved digest")

    print(f"ok   owner-approved trusted-base registry ({reg_digest[:12]}) and theorem closure ({thm_digest[:12]})")


if __name__ == "__main__":
    try:
        if len(sys.argv) not in (3, 4):
            raise ValueError("usage: trusted_approval.py REPORT APPROVAL_JSON [DECISIONS_MD]")
        verify(sys.argv[1], sys.argv[2], *(sys.argv[3:] if len(sys.argv) == 4 else ()))
    except (ValueError, OSError, json.JSONDecodeError) as e:
        sys.exit("FAIL approval: " + str(e))

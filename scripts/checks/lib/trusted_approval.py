"""Compute candidate trust digests; owner authorization is not yet established.

Historical citations are context, never authentication. The owner must choose
an authority boundary and approve the initial canonical digest (R-0678/R-0509).
"""
import hashlib
import json
from pathlib import Path
import re
import sys

INPUT_FILES = ("flake.nix", "flake.lock", "lean-toolchain", "lake-manifest.json")
DATE_RE = re.compile(r"^\d{4}-\d{2}-\d{2}$")
REPORT_PREFIXES = ("component ", "premise ", "theorem ", "axiom ", "lean-toolchain ", "pin ")


def validate_report_lines(text):
    for line_number, line in enumerate(text.splitlines(), 1):
        if not line.startswith(REPORT_PREFIXES):
            raise ValueError(f"line {line_number}: unrecognised trusted-base report row")


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
    if decisions_text.count(marker) != 1:
        return None
    idx = decisions_text.find(marker)
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


def verify_candidate(report, record, decisions_file="docs/decisions.md"):
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
    validate_report_lines(report_text)
    verify_decisions(decisions_path.read_text(), approval["decisions"])

    reg_bytes = registry_rows(report_text)
    if not reg_bytes:
        raise ValueError("trusted-base report has no registry rows")
    reg_digest = hashlib.sha256(reg_bytes).hexdigest()
    if reg_digest != approval["registry_sha256"]:
        raise ValueError("registry differs from candidate digest")

    thm_bytes = theorem_rows(report_text)
    if not thm_bytes:
        raise ValueError("trusted-base report has no theorem/axiom rows")
    thm_digest = hashlib.sha256(thm_bytes).hexdigest()
    if thm_digest != approval["theorems_sha256"]:
        raise ValueError("theorem axiom closure differs from candidate digest")

    inputs = approval["inputs"]
    if not isinstance(inputs, dict) or set(inputs) != set(INPUT_FILES):
        raise ValueError("invalid owner approval inputs schema")
    for source in INPUT_FILES:
        actual = hashlib.sha256(Path(source).read_bytes()).hexdigest()
        if actual != inputs[source]:
            raise ValueError(f"trust input {source} differs from candidate digest")

    return {"registry_sha256": reg_digest, "theorems_sha256": thm_digest,
            "inputs": inputs}


def canonical_digest(candidate):
    """One versioned, deterministic input for the future owner authenticator."""
    payload = {"schema": 1, **candidate}
    return hashlib.sha256(json.dumps(payload, sort_keys=True, separators=(",", ":")).encode()).hexdigest()


def authenticate_owner(digest):
    """Single integration point for the OWNER-CHOSEN authenticator.

    Do not accept branch metadata, citations, environment variables or an agent's
    choice of authentication policy here. No mechanism has been authorized yet.
    """
    raise ValueError("owner authorization is not established (T1): authenticator and initial digest await owner decision")


def verify(report, record, decisions_file="docs/decisions.md"):
    candidate = verify_candidate(report, record, decisions_file)
    digest = canonical_digest(candidate)
    print(f"trust-candidate sha256:{digest}")
    authenticate_owner(digest)


if __name__ == "__main__":
    try:
        if len(sys.argv) not in (3, 4):
            raise ValueError("usage: trusted_approval.py REPORT APPROVAL_JSON [DECISIONS_MD]")
        verify(sys.argv[1], sys.argv[2], *(sys.argv[3:] if len(sys.argv) == 4 else ()))
    except (ValueError, OSError, json.JSONDecodeError) as e:
        sys.exit("FAIL approval: " + str(e))

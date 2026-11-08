#!/usr/bin/env python3
"""Single source of truth for Gate C tier declarations and scheduled-tier export.

Usage:
  python3 scripts/checks/lib/gate_manifest.py scheduled --tier daily|weekly
  python3 scripts/checks/lib/gate_manifest.py tier --tier pr|qualification|daily|weekly
  python3 scripts/checks/lib/gate_manifest.py validate
  python3 scripts/checks/lib/gate_manifest.py self-test
"""

import copy
import json
import sys

VALID_TIERS = ("pr", "qualification", "daily", "weekly")
SCHEDULED_TIERS = ("daily", "weekly")

GATES = [
    {
        "id": "scaling",
        "tier": "pr",
        "command": [
            "env",
            "SCALING_TIER=pr",
            "bash",
            "scripts/checks/compiler/scaling.sh",
            "scaling-report",
        ],
        "budget_seconds": 2400,
        "tools": {
            "lean": "lean-toolchain",
            "lcb": "lake-manifest.json",
            "nix": "flake.lock",
        },
        "seed": 0,
        "controls": [],
    },
    {
        "id": "scaling",
        "tier": "qualification",
        "command": [
            "env",
            "SCALING_TIER=qualification",
            "bash",
            "scripts/checks/compiler/scaling.sh",
            "scaling-report",
        ],
        "budget_seconds": 600,
        "tools": {
            "lean": "lean-toolchain",
            "lcb": "lake-manifest.json",
            "nix": "flake.lock",
        },
        "seed": 0,
        "controls": [],
    },
    {
        "id": "scaling",
        "tier": "daily",
        "command": [
            "env",
            "SCALING_TIER=daily",
            "bash",
            "scripts/checks/compiler/scaling.sh",
            "scaling-report",
        ],
        "budget_seconds": 3600,
        "tools": {
            "lean": "lean-toolchain",
            "lcb": "lake-manifest.json",
            "nix": "flake.lock",
        },
        "seed": 0,
        "controls": [
            {
                "id": "refusal-unauthorised-plant",
                "command": [
                    "bash",
                    "scripts/checks/compiler/scaling.sh",
                    "--plant",
                    "quadratic",
                ],
                "exit_code": 2,
                "diagnostic": "scaling: plants require --allow-planted-failure --plant linear|quadratic",
            },
            {
                "id": "refusal-invalid-tier",
                "command": [
                    "env",
                    "SCALING_TIER=invalid",
                    "bash",
                    "scripts/checks/compiler/scaling.sh",
                ],
                "exit_code": 2,
                "diagnostic": "scaling: invalid SCALING_TIER",
            },
            {
                "id": "refusal-forged-runner",
                "command": [
                    "env",
                    "CONCRETE_SCALING_GITHUB_REPOSITORY=unbalancedparentheses/concrete2",
                    "CONCRETE_SCALING_RUNNER_ENVIRONMENT=github-hosted",
                    "CONCRETE_SCALING_RUNNER_NAME=ubuntu-latest",
                    "bash",
                    "scripts/checks/compiler/scaling.sh",
                ],
                "exit_code": 1,
                "diagnostic": "FAIL scaling: expected runner_environment=self-hosted, got github-hosted",
            },
            {
                "id": "refusal-unauthorized-sha",
                "command": [
                    "env",
                    "CONCRETE_SCALING_EXPECTED_SHA=not-a-sha",
                    "bash",
                    "scripts/checks/compiler/scaling.sh",
                ],
                "exit_code": 1,
                "diagnostic": "FAIL scaling: unauthorized or invalid expected SHA: not-a-sha",
            },
            {
                "id": "refusal-mismatched-sha",
                "command": [
                    "env",
                    "CONCRETE_SCALING_EXPECTED_SHA=0000000000000000000000000000000000000000",
                    "bash",
                    "scripts/checks/compiler/scaling.sh",
                ],
                "exit_code": 1,
                "diagnostic": "FAIL scaling: checked-out HEAD does not match authorized expected SHA 0000000000000000000000000000000000000000",
            },
        ],
    },
]

REQUIRED_DAILY_SCALING_CONTROLS = (
    "refusal-unauthorised-plant",
    "refusal-invalid-tier",
    "refusal-forged-runner",
    "refusal-unauthorized-sha",
    "refusal-mismatched-sha",
)


class GateManifestError(ValueError):
    pass


def require(condition, message):
    if not condition:
        raise GateManifestError(message)


def validate_manifest(gates=None):
    rows = GATES if gates is None else gates
    require(isinstance(rows, list) and bool(rows), "gate manifest is empty")
    seen_tier_ids = set()
    for row in rows:
        require(isinstance(row, dict), "invalid gate manifest row shape")
        gate_id = row.get("id")
        tier = row.get("tier")
        command = row.get("command")
        budget = row.get("budget_seconds")
        tools = row.get("tools")
        controls = row.get("controls")
        require(isinstance(gate_id, str) and bool(gate_id), "invalid gate id")
        require(isinstance(tier, str) and tier in VALID_TIERS, f"invalid gate tier {tier!r}")
        if gate_id == "scaling" and tier not in ("pr", "qualification", "daily"):
            raise GateManifestError(f"scaling daily gate misrouted to tier {tier!r}")
        require((tier, gate_id) not in seen_tier_ids, f"duplicate gate {gate_id!r} in tier {tier!r}")
        seen_tier_ids.add((tier, gate_id))
        require(
            isinstance(command, list) and bool(command) and all(isinstance(arg, str) and bool(arg) for arg in command),
            f"invalid command for gate {gate_id!r} in tier {tier!r}",
        )
        require(isinstance(budget, int) and budget > 0, f"invalid budget_seconds for gate {gate_id!r}")
        require(
            isinstance(tools, dict) and all(isinstance(k, str) and isinstance(v, str) for k, v in tools.items()),
            f"invalid tools map for gate {gate_id!r}",
        )
        require("seed" in row, f"missing seed for gate {gate_id!r}")
        require(isinstance(controls, list), f"invalid controls list for gate {gate_id!r}")
        if tier in SCHEDULED_TIERS:
            require(bool(controls), f"scheduled gate {gate_id!r} in tier {tier!r} has no controls")
        seen_controls = set()
        for ctl in controls:
            require(isinstance(ctl, dict), f"invalid control shape in gate {gate_id!r}")
            cid = ctl.get("id")
            ccmd = ctl.get("command")
            code = ctl.get("exit_code")
            diag = ctl.get("diagnostic")
            require(isinstance(cid, str) and bool(cid), f"invalid control id in gate {gate_id!r}")
            require(cid not in seen_controls, f"duplicate control {cid!r} in gate {gate_id!r}")
            seen_controls.add(cid)
            require(
                isinstance(ccmd, list) and bool(ccmd) and all(isinstance(a, str) and bool(a) for a in ccmd),
                f"invalid control command for {cid!r}",
            )
            require(isinstance(code, int) and code not in (0, 125), f"invalid control exit_code for {cid!r}")
            require(isinstance(diag, str) and bool(diag.strip()), f"missing exact control diagnostic for {cid!r}")

    daily_scaling = [r for r in rows if r.get("id") == "scaling" and r.get("tier") == "daily"]
    require(len(daily_scaling) == 1, "daily tier manifest missing scaling gate")
    daily_cmd = daily_scaling[0]["command"]
    scaling_tier_args = [arg for arg in daily_cmd if arg.startswith("SCALING_TIER=")]
    require(
        scaling_tier_args == ["SCALING_TIER=daily"]
        and daily_cmd == [
            "env",
            "SCALING_TIER=daily",
            "bash",
            "scripts/checks/compiler/scaling.sh",
            "scaling-report",
        ],
        "daily scaling gate must run with SCALING_TIER=daily",
    )
    control_ids = tuple(c["id"] for c in daily_scaling[0]["controls"])
    require(
        all(req in control_ids for req in REQUIRED_DAILY_SCALING_CONTROLS),
        "daily scaling gate is missing required exact-diagnostic controls",
    )

    for expected_tier in ("pr", "qualification"):
        matching = [r for r in rows if r.get("id") == "scaling" and r.get("tier") == expected_tier]
        require(len(matching) == 1, f"{expected_tier} tier manifest missing scaling gate")
        expected_cmd = [
            "env",
            f"SCALING_TIER={expected_tier}",
            "bash",
            "scripts/checks/compiler/scaling.sh",
            "scaling-report",
        ]
        require(
            matching[0]["command"] == expected_cmd,
            f"{expected_tier} scaling gate must run with SCALING_TIER={expected_tier}",
        )
    return rows


def scheduled_rows(tier, gates=None):
    require(tier in SCHEDULED_TIERS, f"invalid scheduled tier {tier!r}")
    rows = validate_manifest(gates)
    return [copy.deepcopy(row) for row in rows if row["tier"] == tier]


def tier_rows(tier, gates=None):
    require(tier in VALID_TIERS, f"invalid gate tier {tier!r}")
    rows = validate_manifest(gates)
    return [copy.deepcopy(row) for row in rows if row["tier"] == tier]


def self_test():
    validate_manifest()
    assert len(scheduled_rows("daily")) == 1
    assert scheduled_rows("weekly") == []

    # Control 1: daily scaling invocation removed
    removed = [copy.deepcopy(r) for r in GATES if not (r["id"] == "scaling" and r["tier"] == "daily")]
    try:
        validate_manifest(removed)
    except GateManifestError as exc:
        assert str(exc) == "daily tier manifest missing scaling gate", str(exc)
    else:
        raise AssertionError("removed daily scaling gate was not refused")

    # Control 2: daily scaling invocation misrouted to weekly tier
    misrouted = copy.deepcopy(GATES)
    for row in misrouted:
        if row["id"] == "scaling" and row["tier"] == "daily":
            row["tier"] = "weekly"
    try:
        validate_manifest(misrouted)
    except GateManifestError as exc:
        assert str(exc) == "scaling daily gate misrouted to tier 'weekly'", str(exc)
    else:
        raise AssertionError("misrouted daily scaling gate was not refused")

    # Control 3: daily scaling invocation run with wrong SCALING_TIER
    wrong_tier = copy.deepcopy(GATES)
    for row in wrong_tier:
        if row["id"] == "scaling" and row["tier"] == "daily":
            row["command"] = [
                "env",
                "SCALING_TIER=qualification",
                "bash",
                "scripts/checks/compiler/scaling.sh",
                "scaling-report",
            ]
    try:
        validate_manifest(wrong_tier)
    except GateManifestError as exc:
        assert str(exc) == "daily scaling gate must run with SCALING_TIER=daily", str(exc)
    else:
        raise AssertionError("wrong SCALING_TIER in daily scaling gate was not refused")

    print("gate_manifest self-test: ok")


def main(argv):
    if len(argv) == 2 and argv[1] == "validate":
        validate_manifest()
        print("gate_manifest: valid")
        return 0
    if len(argv) == 2 and argv[1] == "self-test":
        self_test()
        return 0
    if len(argv) == 4 and argv[1] in ("scheduled", "tier") and argv[2] == "--tier":
        rows = scheduled_rows(argv[3]) if argv[1] == "scheduled" else tier_rows(argv[3])
        print(json.dumps(rows, sort_keys=True))
        return 0
    raise GateManifestError(
        "usage: gate_manifest.py (scheduled --tier daily|weekly | tier --tier pr|qualification|daily|weekly | validate | self-test)"
    )


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv))
    except GateManifestError as error:
        print(f"gate_manifest: {error}", file=sys.stderr)
        sys.exit(1)

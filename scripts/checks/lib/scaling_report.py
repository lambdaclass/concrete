#!/usr/bin/env python3
"""Summarize interleaved scaling samples and enforce inventory, exponent and resolution limits."""

import argparse
import math
import sys
from collections import defaultdict

CORE = ("llvm_module", "lcb_emit", "lcb_assemble")


def exponent(small, large):
    if small <= 0 or large <= 0:
        raise ValueError("stage times must be positive")
    return math.log(large / small) / math.log(10)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("samples")
    parser.add_argument("report")
    parser.add_argument("--k0", type=int, required=True)
    parser.add_argument("--repetitions", type=int, required=True)
    parser.add_argument("--max-exponent", type=float, required=True)
    parser.add_argument("--min-stage-ns", type=int, required=True)
    parser.add_argument("--min-native-ns", type=int, required=True)
    parser.add_argument("--mode", choices=["normal", "in-process", "linear", "quadratic"],
                        default="normal")
    args = parser.parse_args()
    if (args.k0 <= 0 or args.repetitions <= 0 or args.max_exponent <= 0
            or args.min_stage_ns <= 0 or args.min_native_ns <= 0):
        parser.error("k0/repetitions/exponent and both resolution floors must be positive")

    samples = defaultdict(dict)
    with open(args.samples, encoding="utf-8") as stream:
        for line_number, line in enumerate(stream, 1):
            fields = line.rstrip("\n").split("\t")
            if not fields or fields[0] != "sample":
                continue
            if len(fields) != 6:
                sys.exit(f"FAIL scaling: malformed sample at line {line_number}")
            _, rep, k, stage, nanos, _ = fields
            try:
                key = (int(rep), int(k))
                nanos = int(nanos)
            except ValueError:
                sys.exit(f"FAIL scaling: non-numeric sample at line {line_number}")
            if key in samples[stage]:
                sys.exit(f"FAIL scaling: duplicate sample for {stage} at line {line_number}")
            samples[stage][key] = nanos

    sizes = [args.k0, 10 * args.k0, 100 * args.k0]
    expected_keys = {(rep, k) for rep in range(args.repetitions) for k in sizes}
    failures = []
    if args.mode in ("linear", "quadratic"):
        expected_inventory = {"plant_" + args.mode}
    else:
        raw_expected = set(CORE)
        if args.mode == "normal":
            raw_expected.add("native")
        if set(samples) != raw_expected:
            missing = sorted(raw_expected - set(samples))
            unexpected = sorted(set(samples) - raw_expected)
            failures.append(f"FAIL scaling: raw stage inventory differs (missing {missing}, unexpected {unexpected})")
        per_rep = defaultdict(dict)
        for stage, values in samples.items():
            for key, nanos in values.items():
                per_rep[key][stage] = nanos
        if args.mode in ("normal", "in-process"):
            samples["in_process_total"] = {
                key: sum(stages[stage] for stage in CORE)
                for key, stages in per_rep.items() if all(stage in stages for stage in CORE)
            }
        if args.mode == "normal":
            samples["total"] = {
                key: stages["native"] + samples["in_process_total"][key]
                for key, stages in per_rep.items()
                if "native" in stages and key in samples["in_process_total"]
            }
        expected_inventory = set(CORE) | {"in_process_total"}
        if args.mode == "normal":
            expected_inventory.update(("native", "total"))
    if set(samples) != expected_inventory:
        missing = sorted(expected_inventory - set(samples))
        unexpected = sorted(set(samples) - expected_inventory)
        failures.append(f"FAIL scaling: stage inventory differs (missing {missing}, unexpected {unexpected})")

    lines = [
        f"# mode={args.mode}; k0={args.k0}; sizes={sizes[0]},{sizes[1]},{sizes[2]}; repetitions={args.repetitions}",
        f"# minimum per size; max_exponent={args.max_exponent:.3f}; "
        f"min_100x_stage_ns={args.min_stage_ns}; min_100x_native_ns={args.min_native_ns}",
        "stage\t1x_ms\t10x_ms\t100x_ms\texponent_1x_10x\texponent_10x_100x",
    ]
    for stage in sorted(expected_inventory):
        if stage not in samples:
            continue
        values = samples[stage]
        if values.keys() != expected_keys:
            missing = len(expected_keys - values.keys())
            unexpected = len(values.keys() - expected_keys)
            failures.append(
                f"FAIL scaling: stage {stage}: sample inventory differs "
                f"({missing} missing, {unexpected} unexpected)")
            continue
        minima = [min(values[(rep, k)] for rep in range(args.repetitions)) for k in sizes]
        try:
            exponents = [exponent(minima[i], minima[i + 1]) for i in range(2)]
        except ValueError as error:
            failures.append(f"FAIL scaling: stage {stage}: {error}")
            continue
        lines.append("{}\t{:.3f}\t{:.3f}\t{:.3f}\t{:.3f}\t{:.3f}".format(
            stage, *(nanos / 1_000_000 for nanos in minima), *exponents))
        floor = args.min_native_ns if stage == "native" else args.min_stage_ns
        if minima[2] < floor:
            failures.append(
                f"FAIL scaling: stage {stage}: 100x minimum {minima[2]} ns is below resolution "
                f"{floor} ns; increase SCALING_K0")
        if exponents[1] > args.max_exponent:
            failures.append(
                f"FAIL scaling: stage {stage} exponent 10x->100x = {exponents[1]:.3f} "
                f"exceeds {args.max_exponent:.3f}")
    text = "\n".join(lines) + "\n"
    with open(args.report, "w", encoding="utf-8") as stream:
        stream.write(text)
    sys.stdout.write(text)
    for failure in failures:
        print(failure)
    if failures:
        sys.exit(1)
    print(f"PASS scaling: all 10x->100x exponents are at most {args.max_exponent:.3f}")


if __name__ == "__main__":
    main()

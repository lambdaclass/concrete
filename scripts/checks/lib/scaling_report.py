#!/usr/bin/env python3
"""Summarize interleaved scaling samples and enforce the exponent/resolution limits."""

import argparse
import math
import sys
from collections import defaultdict

CORE = ["llvm_module", "lcb_emit", "lcb_assemble"]


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
    parser.add_argument("--only-plant", choices=["linear", "quadratic"])
    args = parser.parse_args()
    if args.k0 <= 0 or args.repetitions <= 0 or args.max_exponent <= 0 or args.min_stage_ns < 0:
        parser.error("k0/repetitions/max-exponent must be positive and min-stage-ns nonnegative")

    samples = defaultdict(dict)
    with open(args.samples, encoding="utf-8") as stream:
        for line_number, line in enumerate(stream, 1):
            fields = line.rstrip("\n").split("\t")
            if not fields or fields[0] != "sample":
                continue
            if len(fields) != 6:
                sys.exit(f"FAIL scaling: malformed sample at line {line_number}")
            _, rep, k, stage, nanos, _ = fields
            samples[stage][(int(rep), int(k))] = int(nanos)
    if args.only_plant:
        wanted = "plant_" + args.only_plant
        samples = {wanted: samples[wanted]} if wanted in samples else {}
    else:
        per_rep = defaultdict(dict)
        for stage, values in samples.items():
            for key, nanos in values.items():
                per_rep[key][stage] = nanos
        derived = {"in_process_total": {}, "total": {}}
        for key, stages in per_rep.items():
            if all(stage in stages for stage in CORE):
                derived["in_process_total"][key] = sum(stages[stage] for stage in CORE)
                if "native" in stages:
                    derived["total"][key] = stages["native"] + derived["in_process_total"][key]
        samples.update({stage: values for stage, values in derived.items() if values})
    if not samples:
        sys.exit("FAIL scaling: no samples")

    sizes = [args.k0, 10 * args.k0, 100 * args.k0]
    expected_keys = {(rep, k) for rep in range(args.repetitions) for k in sizes}
    lines = [
        f"# k0={args.k0}; sizes={sizes[0]},{sizes[1]},{sizes[2]}; repetitions={args.repetitions}",
        f"# minimum per size; max_exponent={args.max_exponent:.3f}; min_100x_stage_ns={args.min_stage_ns}",
        "stage\t1x_ms\t10x_ms\t100x_ms\texponent_1x_10x\texponent_10x_100x",
    ]
    failures = []
    for stage in sorted(samples):
        values = samples[stage]
        missing = expected_keys - values.keys()
        if missing:
            failures.append(f"FAIL scaling: stage {stage}: incomplete samples ({len(missing)} missing)")
            continue
        minima = [min(values[(rep, k)] for rep in range(args.repetitions)) for k in sizes]
        try:
            exponents = [exponent(minima[i], minima[i + 1]) for i in range(2)]
        except ValueError as error:
            failures.append(f"FAIL scaling: stage {stage}: {error}")
            continue
        lines.append("{}\t{:.3f}\t{:.3f}\t{:.3f}\t{:.3f}\t{:.3f}".format(
            stage, *(nanos / 1_000_000 for nanos in minima), *exponents))
        if stage != "native" and minima[2] < args.min_stage_ns:
            failures.append(
                f"FAIL scaling: stage {stage}: 100x minimum {minima[2]} ns is below resolution "
                f"{args.min_stage_ns} ns; increase SCALING_K0")
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

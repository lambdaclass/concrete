"""Light exact-diagnostic controls for protected inline-match comparison."""
import functools

from vacuity_controls import controls
from vacuity_driver import ROOT, compiled_consumer, run


def main():
    with compiled_consumer() as meta:
        return controls(functools.partial(run, meta_path=meta), ROOT,
                        labels={"inline-match-positive", "inline-match-changed-branch"})


if __name__ == "__main__":
    raise SystemExit(main())

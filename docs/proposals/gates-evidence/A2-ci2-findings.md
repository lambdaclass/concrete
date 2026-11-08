ci-2 run A2 at 83fb12da (fresh clone, warm .lake/packages only), 2026-11-08T15:35Z:
- spec_diff.sh rc=1: `lake env lean --run scripts/checks/compiler/spec_diff.lean gen-spec ...` fails with
  "object file .lake/build/lib/lean/ConcreteSpec/Spec/Slice0.olean of module ConcreteSpec.Spec.Slice0 does not exist".
  The gate does not build every module its driver imports (after the generators moved out of Concrete/). A gate must build its own complete import closure in a fresh checkout (e.g. a lake target/lib for the test generators that depends on ConcreteSpec and Concrete, built by the gate), never rely on a previous job's build.
- root checks: all 12 pass.

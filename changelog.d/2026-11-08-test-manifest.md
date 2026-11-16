### One manifest-driven test format

The slice-0 BIR programs and their expected outcomes now have one inventory in `tests/manifest.tsv`.
A shared fail-closed reader feeds the test, artifact and reproducibility gates. The compiler test job
is now `Tests`; root checks validate the manifest, source-file ownership and planted defects.

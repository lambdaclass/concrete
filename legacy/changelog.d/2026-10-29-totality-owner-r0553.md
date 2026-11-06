### Totality baseline: R-0553 owns the stages no pass proof needs (2026-10-29)

The 301 baseline rows in Frontend, Resolve, Backend, Proof, ProofKit, Report and Pipeline, the
stages no verified-pipeline pass proof needs, named R-0506 as their owner. R-0506 states which code
must be total; R-0553 converts the baseline, so those rows and their stages in
`docs/compiler/totality_stages.tsv` now name R-0553. Rows in proof-relevant stages keep the owner
whose proof needs them.

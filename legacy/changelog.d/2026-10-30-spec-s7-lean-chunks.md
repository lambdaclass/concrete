### Spec gate: the S7 Lean name check runs in chunks

`check_spec.sh` S7 resolves every `Enforced by:` name in Lean through one generated file. It held a
single `#eval` whose do-block nests one `match` per name, and past about 240 names Lean stops with
"maximum recursion depth has been reached", so the check resolved none of them and failed. The
generator (`scripts/spec/spec.py sites`) now opens a new `#eval` every 50 names; the result is read
as before, and the N7c negative control still fails on an unresolved name.

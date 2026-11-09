#!/usr/bin/env bash
set -eu
m1=manifests/reproducibility-linux-first/manifest.txt
m2=manifests/reproducibility-linux-independent/manifest.txt
# Both manifests carry the identity and at least one artifact hash.
for m in "$m1" "$m2"; do grep -q '^compiler_commit=' "$m"; grep -q '/lcb.exe$' "$m"; done
diff -u "$m1" "$m2"

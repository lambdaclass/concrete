TMP=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/tb
ok(){ echo "ok $*"; }; no(){ echo "NO $*"; }
has_match(){ [ "$1" = -F ] && grep -qF -- "$2"; }
lake_options() {
  python3 - "$1" <<'EOF'
import sys, tomllib
try:
    cfg = tomllib.load(open(sys.argv[1], "rb"))
except Exception as e:
    print(f"unreadable: {e}"); sys.exit(0)
bad = []
import os
if os.path.exists(os.path.join(os.path.dirname(sys.argv[1]) or ".", "lakefile.lean")):
    bad.append("a lakefile.lean, which Lake reads instead of lakefile.toml")
if cfg.get("leanOptions", {}).get("warningAsError") is not True:
    bad.append("[leanOptions] warningAsError is not true")
# Allow-lists, not deny-lists: a key not named here (srcDir, roots, globs, moreLeanArgs, …) could
# point the build at other sources or pass options, so it is refused.
extra = set(cfg) - {"name", "version", "defaultTargets", "leanOptions", "lean_lib", "require"}
if extra: bad.append(f"the package sets {sorted(extra)}")
if set(cfg.get("leanOptions", {})) != {"warningAsError"}:
    bad.append(f"[leanOptions] sets {sorted(set(cfg.get('leanOptions', {})) - {'warningAsError'})}")
import json, re
reqs = cfg.get("require", [])
if reqs:
    try:
        man = json.load(open(os.path.join(os.path.dirname(sys.argv[1]) or ".", "lake-manifest.json")))
        pkgs = {p.get("name", "").strip("«»"): p for p in man.get("packages", [])}
    except Exception as e:
        bad.append(f"lake-manifest.json unreadable: {e}"); pkgs = {}
    for r in reqs:
        n = r.get("name")
        if set(r) != {"name", "git", "rev"}:
            bad.append(f"the require {n} sets {sorted(set(r) ^ {'name', 'git', 'rev'})}"); continue
        if not re.fullmatch(r"[0-9a-f]{40}", str(r["rev"])):
            bad.append(f"the require {n} is not pinned to a full commit ({r['rev']})")
        p = pkgs.get(n)
        if not p or p.get("type") != "git" or p.get("url") != r["git"] or p.get("rev") != r["rev"] or p.get("inputRev") != r["rev"]:
            bad.append(f"lake-manifest.json does not record the require {n} at {r['git']} {r['rev']}")
libs = cfg.get("lean_lib", [])
if [l.get("name") for l in libs] != ["ConcreteSpec", "Concrete"]: bad.append(f"libraries {[l.get('name') for l in libs]}")
for l in libs:
    extra = set(l) - {"name", "leanOptions"}
    if extra: bad.append(f"the library sets {sorted(extra)}")
    opts = set(l.get("leanOptions", {})) - {"autoImplicit"}
    if opts: bad.append(f"the library sets options {sorted(opts)}")
print("; ".join(bad))
EOF
}
# The dependency controls run on a copy of this lakefile, beside a manifest, so each varies one thing.
mkdir -p "$TMP/lk"
lake_variant() { # NAME PYTHON-EDIT: lakefile.toml and lake-manifest.json copied to $TMP/lk/NAME, then edited
  mkdir -p "$TMP/lk/$1"; cp lakefile.toml lake-manifest.json "$TMP/lk/$1/"
  (cd "$TMP/lk/$1" && python3 -I -c "$2")
}
lake_variant short 'import re; p="lakefile.toml"; s=open(p).read(); r=re.search("[0-9a-f]{40}", s).group(0); open(p,"w").write(s.replace(r, r[:12]))'
lake_variant branch 'import re; p="lakefile.toml"; s=open(p).read(); open(p,"w").write(re.sub(r"rev = \"[0-9a-f]+\"", "rev = \"main\"", s))'
lake_variant drift 'import json; p="lake-manifest.json"; m=json.load(open(p)); m["packages"][0]["rev"]="0"*40; json.dump(m, open(p,"w"))'
lake_variant subdir 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s.replace("[[require]]\n", "[[require]]\nsubDir = \"x\"\n", 1))'
lake_variant thirdlib 'p="lakefile.toml"; s=open(p).read(); open(p,"w").write(s + "\n[[lean_lib]]\nname = \"Other\"\n")'
for v in short:"not pinned to a full commit" branch:"not pinned to a full commit" drift:"does not record the require" subdir:"sets ['subDir']" thirdlib:"libraries ['ConcreteSpec', 'Concrete', 'Other']"; do
  L=$(lake_options "$TMP/lk/${v%%:*}/lakefile.toml")
  case "$L" in *"${v#*:}"*) ok "control: a lakefile whose dependency or library set is changed (${v%%:*}) is refused ($L)" ;;
    *) no "control: the ${v%%:*} lakefile was not refused for '${v#*:}': '$L'" ;; esac
done

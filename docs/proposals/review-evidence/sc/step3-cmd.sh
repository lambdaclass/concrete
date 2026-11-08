set -u
echo "head=$(git rev-parse HEAD)"
r=0
run(){ echo "##### $*"; s=$(date +%s); "$@"; c=$?; echo "##### rc=$c ($(( $(date +%s)-s ))s): $*"; [ $c = 0 ] || r=1; }
run lake build ConcreteSpec
run bash scripts/spec/rules.sh
run bash scripts/proofs/axioms.sh
run bash scripts/proofs/trust_ban.sh
run bash scripts/proofs/kernel_replay.sh
run bash scripts/proofs/external.sh
run bash scripts/checks/run.sh
echo "ALL rc=$r"; exit $r

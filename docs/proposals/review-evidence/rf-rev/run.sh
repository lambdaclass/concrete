set -u
S=/tmp/claude-30035/-home-unbalancedparen-projects-concrete2/e2c54e65-31b5-4145-8101-c2d7b071b97e/scratchpad/rf-rev
cd $S/repo && timeout 1800 kimi -p "$(cat $S/prompt.txt)" < /dev/null > $S/kimi.out 2>&1 &
K=$!
P="Do not use any tools and do not run commands: review only the text in this message. $(sed 's/^Review REVIEW.diff/Review the diff below/' $S/prompt.txt) This message holds one part of the diff; other parts go to separate runs, so report only what this part shows."
cd $S/agyempty
timeout 1800 agy -p "$P

=== PENDING_RECORDS.md
$(cat $S/repo/PENDING_RECORDS.md)

=== diff part A (decisions.md, changelog, ISSUES.tsv, AGENTS.md, README.md, PHILOSOPHY.md), zero context
$(cat $S/agyA.diff)" < /dev/null > $S/agyA.out 2>&1
timeout 1800 agy -p "$P

=== PENDING_RECORDS.md
$(cat $S/repo/PENDING_RECORDS.md)

=== diff part B1 (ROADMAP.md), zero context
$(cat $S/agyB1.diff)" < /dev/null > $S/agyB1.out 2>&1
timeout 1800 agy -p "$P

=== approved FOUNDATION_PROPOSAL (final)
$(cat $S/repo/FOUNDATION_PROPOSAL_final.md)

=== diff part B2 (docs/proposals/FOUNDATION.md), zero context
$(cat $S/agyB2.diff)" < /dev/null > $S/agyB2.out 2>&1
timeout 1800 agy -p "$P

=== diff part B3 (docs/proposals/SPEC_UNIFICATION.md), zero context
$(cat $S/agyB3.diff)" < /dev/null > $S/agyB3.out 2>&1
wait $K
echo all-done

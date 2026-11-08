# Server-side protection of main (proposal, 2026-11-08)

Status: a draft for the owner to apply or approve. Nothing here has been applied to GitHub: these
settings change the repository, so only the owner applies them. The owner approved the redesign
that calls for them ("yes redesign", 2026-11-08): shrink the landing scripts, switch to the one
repository, then make the server the boundary that protects main, with a much smaller local guard.

Read on 2026-11-08 (read-only `gh api`):
- classic branch protection on main: force pushes and deletions blocked, conversation resolution on,
  no required status checks, admins NOT enforced;
- ruleset 19117072 "PR required for non-admins" (~DEFAULT_BRANCH): deletion, non_fast_forward,
  pull_request (1 approval); bypass: OrganizationAdmin and RepositoryRole 5 (admin), "always".
So today any admin, including the account the train pushes with, can update main with anything.

Goal: the normal way to move main is a train's fast-forward push of a commit whose required checks
are green, using one landing identity. The rulesets block deletion and non-fast-forwards and stop
accidental and unauthorised pushes by everyone else. They do not prove that `train.sh` ran.
Repository admins (and any custom role with "edit repository rules") can edit or disable rulesets;
that is the emergency path. They can then push main by hand, so the protection is not absolute
against an admin.

## Ruleset 1 "main: required checks, no rewrite" (no bypass actors)

Requires each listed check context to pass on the commit being pushed. For a required status check,
GitHub accepts the conclusions `success`, `skipped` and `neutral` as passing: "passed" does not
necessarily mean that the job ran successfully. The train's stricter check remains authoritative
(see below).

    gh api -X POST repos/lambdaclass/concrete/rulesets --input - <<'JSON'
    {
      "name": "main: required checks, no rewrite",
      "target": "branch",
      "enforcement": "active",
      "conditions": {"ref_name": {"include": ["~DEFAULT_BRANCH"], "exclude": []}},
      "bypass_actors": [],
      "rules": [
        {"type": "deletion"},
        {"type": "non_fast_forward"},
        {"type": "required_status_checks", "parameters": {
          "strict_required_status_checks_policy": false,
          "do_not_enforce_on_create": false,
          "required_status_checks": [
            {"context": "Root checks", "integration_id": 15368},
            {"context": "Spec build", "integration_id": 15368},
            {"context": "Oracle (GitHub-hosted fetch + smoke)", "integration_id": 15368},
            {"context": "Proof axioms", "integration_id": 15368},
            {"context": "Proof trust ban (sorry, native_decide)", "integration_id": 15368},
            {"context": "Kernel replay (leanchecker)", "integration_id": 15368},
            {"context": "External re-check (con-leche, nanoda)", "integration_id": 15368}
          ]}}
      ]
    }
    JSON

`integration_id` 15368 identifies the GitHub Actions app, not this workflow, an event, a branch, a
base or a particular run. Another Actions workflow can produce checks with the same names and app.
The ruleset therefore binds only the check names, app and GitHub's passing conclusions.

The list is `required-checks.txt` minus **Landing record**, deliberately. On the train's `ci/*`
workflow dispatch, every step of that job has a `push`-to-main condition, so the job runs no steps
and adds no provenance before main moves. On main's later push run it looks up and links the
qualifying dispatch, allowing duplicate jobs to be skipped; that post-landing record is not a
pre-landing check. A follow-up gate can compare the lists with this one deliberate exclusion.

## What only train.sh establishes

`train.sh ci` dispatches CI on `ci/<name>` with the train's base and records that workflow run's id.
`watch` and `land` require the recorded run to be completed at the exact candidate SHA, every
landing-tier job to conclude `success`, and every job required by `required-checks.txt` and the
transition rules to be present and conclude `success`; skipped and neutral jobs are not accepted.
`land` also requires origin/main still to be the train's base and the head to descend from it, and
separately checks review provenance and any required acceptance rehearsal.

Those workflow/event/branch/base associations come from the run that `train.sh ci` dispatched and
recorded; they are not inferred from check names, and `watch`/`land` do not reconstruct them from a
ruleset. The success-only job rule, exact-run check, moved-main refusal, review and rehearsal policy
remain train.sh's enforcement, not GitHub ruleset guarantees.

## Ruleset 2 "main: updates only by the landing key"

Restricts who may update main. The `DeployKey` bypass is class-wide: it covers every current and
future write deploy key, not one named key. Its bypass applies only to this ruleset; ruleset 1's
checks, deletion ban and non-fast-forward rule still apply. A deploy key authenticates a credential,
not execution of `train.sh`: anyone or any process holding a write deploy key can hand-push a
qualifying fast-forward without running the train.

    gh api -X POST repos/lambdaclass/concrete/rulesets --input - <<'JSON'
    {
      "name": "main: updates only by the landing key",
      "target": "branch",
      "enforcement": "active",
      "conditions": {"ref_name": {"include": ["~DEFAULT_BRANCH"], "exclude": []}},
      "bypass_actors": [{"actor_id": null, "actor_type": "DeployKey", "bypass_mode": "always"}],
      "rules": [{"type": "update", "parameters": {"update_allows_fetch_and_merge": false}}]
    }
    JSON

Prerequisites (owner): a dedicated write deploy key for the landing clone (`gh repo deploy-key add
<pub> --repo lambdaclass/concrete --title landing-train --allow-write`), the landing clone's
lambdaclass remote using it (an ssh host alias), and no other write deploy key on the repository.
Confine the key to a dedicated landing host or service that exposes only the train operation, not a
general-purpose shell. The trusted authorities are that credential's custodian and host, plus the
repository admins and custom roles that can edit rulesets.

A dedicated GitHub App is a narrower bypass actor: the REST actor type `Integration` identifies a
particular app, whereas `DeployKey` covers the whole write-key class. Prefer an App installed only
on this repository, with only the permissions landing needs and its installation credentials confined
to the landing service, if that operational change is feasible. Its credential still would not prove
that `train.sh` ran. The [repository ruleset REST API](https://docs.github.com/en/rest/repos/rules)
lists `Integration`, `OrganizationAdmin`, `RepositoryRole`, `Team`, `DeployKey` and `User` actor
types. Whichever identity is chosen, verify the saved bypass actor read-only before relying on it.

Audit write deploy keys and every repository ruleset's bypass actors (all read-only):

    gh api repos/lambdaclass/concrete/keys --paginate \
      --jq '.[] | select(.read_only == false) | {id,title,read_only,last_used}'
    for id in $(gh api repos/lambdaclass/concrete/rulesets --paginate --jq '.[].id'); do
      gh api "repos/lambdaclass/concrete/rulesets/$id" \
        --jq '{id,name,enforcement,bypass_actors}'
    done

## Cutover, test and rollback

Apply after B lands (lambdaclass is then the one repository) and before the small local guard. Record
the ids returned by the two POST commands above.

1. Create both replacements as active while ruleset 19117072 is still active. This temporarily
   blocks direct main pushes, including the train's; do not test a landing yet.
2. Disable the superseded PR ruleset, removing its enforcement but retaining it for rollback:

       gh api -X PATCH repos/lambdaclass/concrete/rulesets/19117072 -f enforcement=disabled

   This order is necessary because all active rulesets and classic branch protection stack. A bypass
   in ruleset 2 does not bypass ruleset 19117072, so leaving its pull_request rule active would
   deadlock the deploy key's direct train push. See GitHub's [rule layering
   documentation](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-rulesets/about-rulesets#about-rule-layering).
3. Test with a real train landing; it must succeed. A hand push from a non-landing identity must be
   refused. Inspect the effective rules read-only:

       gh api repos/lambdaclass/concrete/rules/branches/main

4. Only after that landing succeeds, delete the disabled PR ruleset:

       gh api -X DELETE repos/lambdaclass/concrete/rulesets/19117072

   Classic protection can stay (it duplicates deletion/force-push blocking) or be removed:
   `gh api -X DELETE repos/lambdaclass/concrete/branches/main/protection`. Recommended: remove it
   after the successful test, so one place holds the rules.

If the test fails, and before deleting ruleset 19117072, restore the previous behaviour by
reactivating it first and then disabling both replacements (substituting their returned ids):

    gh api -X PATCH repos/lambdaclass/concrete/rulesets/19117072 -f enforcement=active
    gh api -X PATCH repos/lambdaclass/concrete/rulesets/<ruleset-1-id> -f enforcement=disabled
    gh api -X PATCH repos/lambdaclass/concrete/rulesets/<ruleset-2-id> -f enforcement=disabled

Risks to weigh: admins retain the emergency path above; the landing credential's holder can bypass
the train but not ruleset 1; and a renamed required check blocks every landing until the ruleset is
updated (fail closed). `strict_required_status_checks_policy` is false because the train itself
refuses a moved main and checks ancestry, rather than relying on GitHub's up-to-date-branch policy.

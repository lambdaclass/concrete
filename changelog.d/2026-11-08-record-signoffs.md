### Sign-offs, the landing redesign and the legacy sweeps (2026-11-08)

`docs/decisions.md` records:
- **The owner's sign-offs at the review cap** ("sign off ea5181b5, 1547734f8, a27fbb842, yes
  redesign"): `wip/slice0` at `ea5181b5`, `wip/record-language` at `1547734f8` and
  `wip/one-repository` at `a27fbb842`, each with its three verdicts.
- **The landing protection redesign** ("yes redesign"). The landing scripts shrink first. Then one
  repository lands, before the foundation freeze. Then server-side protection of `main` becomes the
  real boundary (R-0628), and a much smaller local guard follows as a convenience (R-0629).
  `wip/remote-guard` and `wip/acceptance-shim` are superseded.
- **Commit and push everything all the time** ("commit and push everything all the time"):
  every piece is committed when it builds and its checks pass, and pushed at once to the push remotes
  in force. Today those are `origin` and `lambdaclass`; after one repository lands, only
  `lambdaclass/concrete`.
- **Codex, Kimi and agy write; Claude reviews** ("can you have kimi, codex and agy implement things and
  we just review it"): Codex, Kimi and agy implement. Claude briefs, reviews, runs the gates and
  lands.
- **Byte-for-byte sweeps into `legacy/`** ("ok to 1"): an addition under `legacy/` is accepted only
  if it is byte-identical to the same root path at a named commit and is gone from the root. P5 and
  the restart row are revised.
- **All of `docs/archive/` moves under `legacy/`** ("ok to 2"), by that sweep (R-0631). P21 is
  revised. The archive-move answer settles the destination of docs/archive/; it does not decide a separate relocation policy for docs/proposals/. No such relocation is recorded here.

ROADMAP adds R-0630: gate 2 for `Concrete/`, the axioms, trust, totality and replay checks over the
compiler, which are not built yet. ISSUES.tsv files ISS-0018, review.sh reviewing changes outside a
train candidate's diff.

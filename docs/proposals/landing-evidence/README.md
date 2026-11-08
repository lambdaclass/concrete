# Landing evidence, 2026-11-08

These are verbatim copies of review prompts and raw reviewer outputs, kept as the durable record behind
the owner sign-offs and landing decisions. The owner's rule is to commit and push everything, because the
machine can crash. Before this commit the files lived only in a session scratchpad. Nothing here was
edited. They were scanned for secrets (tokens, keys, passwords) before copying, and none was found.

Each review ran in a git-archive copy of the exact commit named. `<x>/tree/REVIEW.diff` is the diff
the reviewer was given. The rest of each archive is the commit itself and is not copied: rebuild it with
`git archive <sha>`.

## codexfull/: Codex with full access (`codex exec -s danger-full-access`), owner-approved method

`<sha>-request.txt` is the prompt and `<sha>.txt` the raw output. `<sha>.agy.txt` is agy's output for the
same request, which was sent to agy with the diff inline and no file reads.

- 119381451a57: B (wip/one-repository-2), the full branch over ea212942. Codex REJECT, two doc defects.
- e997548d9348: B fix delta 11938145..e997548d. Codex ACCEPT, agy ACCEPT. The owner signed off e997548d.
- 61be4bed3179: launcher (wip/agent-launcher-2), the full branch over ea212942. Codex REJECT, one defect.
- 00feab7a87f3: launcher fix delta 61be4bed..00feab7a. Codex ACCEPT, agy ACCEPT.
- launcher.ctx and shrink.ctx are context files used to build the requests. The landing-shrink review
  (e9b6096a6) was dropped before it started, because the slice-0 worker owns it, so no output exists.

## iss21/: re-reviews under ISS-0021, run where the reviewer can read the repository

`<tag>-request.txt` is the prompt and `<tag>.txt` the raw output. Each output opens with `REPOSITORY:` and
lists `FILES READ:`.

- agy-B: e997548d, full branch over ea212942. REPOSITORY: readable. ACCEPT.
- agy-launcher: 00feab7a8, full branch over ea212942. REPOSITORY: readable. ACCEPT.
- agy-sweep: 35b489e4 (wip/legacy-sweep-2), full branch over 3bd7bc41. REPOSITORY: readable. ACCEPT.
- kimi-sweep: 35b489e4, the same request. REPOSITORY: readable. REJECT with two real findings: the
  doc_links.py swept-file lookup fails open, and ROADMAP/decisions rows went stale. The owner's sign-off
  of 35b489e4 lapses, and the fix goes to a new SHA.

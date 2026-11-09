- ISS-0040: the shared reviewer-output contract blocks unresolved source anchors,
  invalid line ranges, missing files and absent positive-verdict identifiers at
  the reviewed commit. Review prompts require exact named source links; pinned
  external declarations and proposed FINDINGS names have explicit handling.
  Exact-diagnostic controls run in the review-tool gate.

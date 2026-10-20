/-!
# Generated compiler build identity — do not edit

Emitted by `scripts/gen/build_identity.sh` from the compiler's own sources. A receipt binds this to
name the compiler that produced it.

EMBEDDED AT BUILD TIME on purpose. Identities derived from the repository at REPORT time —
`git status`, `git rev-parse HEAD` — describe the tree the binary is later pointed at rather than
the binary, and are wrong in both directions: a binary built from one commit claims another once the
checkout moves, and a binary built from uncommitted sources claims whatever is checked out. This
value travels with the artifact.

A run-time digest of the executable would be strictly stronger and is not portable: neither
`/proc/self/exe` nor GNU `sha256sum` exists on macOS, which is an active CI platform, and hashing
a 178 MB binary in-process is not viable with the in-repo SHA-256. **Known limit: a binary patched
after build keeps the constant its sources produced.** This identifies the BUILD, not the bytes.

Staleness is a gate failure rather than a silent wrong answer:
`check_build_identity_freshness.sh` re-derives this and fails on disagreement.
-/

namespace Concrete

/-- 128-bit digest over the compiler's own sources at build time. -/
def buildIdentity : String := "97d56171fe06dc694a28aa854c7b6e96"
||||||| parent of 5237c5fc (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
def buildIdentity : String := "5d5d808f2f47938102ab6f40a3f487b8"
>>>>>>> 932afdfa (fix: bug 091 Codex round 3 review — one place rule for explicit and implicit borrows)
=======
def buildIdentity : String := "5d5d808f2f47938102ab6f40a3f487b8"
||||||| parent of d8c7870e (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
def buildIdentity : String := "60bb29dc8b6e66c9427b2ad0064f2626"
>>>>>>> 3396afcf (fix: bug 091 Codex round 3 review — one place rule for explicit and implicit borrows)
=======
def buildIdentity : String := "60bb29dc8b6e66c9427b2ad0064f2626"
=======
>>>>>>> 492014c2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
||||||| parent of 60b36a02 (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
def buildIdentity : String := "d1addc41cade4a9efad3828b8a716848"
=======
def buildIdentity : String := "dedd8cefb7a556960714c9693144a00a"
>>>>>>> 60b36a02 (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
>>>>>>> d8c7870e (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
||||||| parent of ae598662 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
=======
||||||| parent of eef1b0a1 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
>>>>>>> c33f5d2b (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
=======
||||||| parent of 492014c2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
=======
||||||| parent of b2298367 (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
||||||| parent of 60b36a02 (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
def buildIdentity : String := "d1addc41cade4a9efad3828b8a716848"
=======
def buildIdentity : String := "dedd8cefb7a556960714c9693144a00a"
>>>>>>> 60b36a02 (fix: bug 091 Codex round 3 review — by-value derefs, paren arguments, root provenance)
=======
>>>>>>> b2298367 (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
||||||| parent of e06d05e2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
def buildIdentity : String := "dedd8cefb7a556960714c9693144a00a"
=======
def buildIdentity : String := "c970505bec99e70152d01cbddbeae8d2"
>>>>>>> e06d05e2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
>>>>>>> eef1b0a1 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
||||||| parent of d92fb92f (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
=======
||||||| parent of 90cd6db8 (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
>>>>>>> 492014c2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
=======
||||||| parent of b2298367 (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
=======
||||||| parent of 87c4131e (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
||||||| parent of e06d05e2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
def buildIdentity : String := "dedd8cefb7a556960714c9693144a00a"
=======
def buildIdentity : String := "c970505bec99e70152d01cbddbeae8d2"
>>>>>>> e06d05e2 (fix: bug 091 Codex round 3 review — deref through borrows, match *r, traced provenance)
=======
>>>>>>> 87c4131e (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
||||||| parent of d105bc7c (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
def buildIdentity : String := "c970505bec99e70152d01cbddbeae8d2"
=======
def buildIdentity : String := "6a84c4f099a6d9cae06cad445e9c465b"
>>>>>>> d105bc7c (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
>>>>>>> 90cd6db8 (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
||||||| parent of b6b40a8e (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
=======
||||||| parent of a2bb78a4 (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
>>>>>>> b2298367 (fix: bug 091 Codex round 3 review — exclusive references, reassignment provenance)
=======
||||||| parent of 87c4131e (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
=======
||||||| parent of ab41e479 (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
def buildIdentity : String := "6a84c4f099a6d9cae06cad445e9c465b"
=======
def buildIdentity : String := "9941464b15dfa750c2ceae8ac82fcaa9"
>>>>>>> ab41e479 (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
>>>>>>> 87c4131e (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
>>>>>>> a2bb78a4 (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)
>>>>>>> b6b40a8e (fix: bug 091 Codex round 3 review — no re-pointed references, argument borrows through parens and *)

/-- How many source files the identity above was computed over.

    Carried so that a SHRINKING inventory is detectable. The content digest moves when a file is
    removed, but it moves the same way it does for an ordinary edit — this value distinguishes the
    two, and a digest computed over fewer files is a weaker claim wearing the same shape. -/
def buildIdentitySourceCount : Nat := 105

/-- Digest of the inventory's FILE LIST, independent of file contents.

    Moves only when files are added, removed, or renamed. Together with the count, this makes
    "the inventory changed" distinguishable from "a source changed". -/
def buildIdentityInventoryDigest : String := "5575a687796b6810869d28ec2cbe0aa4"

end Concrete

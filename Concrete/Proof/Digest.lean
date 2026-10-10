import Concrete.Proof.Sha256Fast

/-!
# Content hashing

`shortHash` is GENERAL — it digests toolchain identifiers, workspace identifiers, import sets,
table values, theorem artifacts, dependency and assumption sets, and body fingerprints. It is not
body-specific, so it does not belong in `BodyIdentity` even though the body digest is built from
it; naming a general helper after one of its callers is how a "canonical producer" ends up with a
second copy elsewhere.

Imports `Sha256Fast` (and through it `Sha256Spec`) and nothing else, so it sits below everything
that hashes.
-/

namespace Concrete

/-- Two-digit lowercase hex of a byte. -/
private def byteToHex (b : UInt8) : String :=
  let digits := "0123456789abcdef".toList
  let n := b.toNat
  String.ofList [digits.getD (n / 16) '0', digits.getD (n % 16) '0']

/-- Compact, stable hex hash of a body fingerprint, for the in-source
    `#[proof_fingerprint("…")]` attribute. The full PExpr string is grotesque in
    source, so we store a digest. SHA-256 truncated to 128 bits: the previous
    64-bit non-cryptographic `String.hash` defended against accidental drift but
    not against a crafted body that collides with the recorded fingerprint —
    a silent stale→proved upgrade. Computed by `Sha256Fast`, which build-time
    equivalence tests compare with the in-repo FIPS 180-4 spec
    (`Concrete.Sha256Spec`) on fixed inputs (tests, not a proof); running the
    spec itself cost ~97% of a report invocation, because package identity
    hashes every module's source. -/
def shortHash (fingerprint : String) : String :=
  let digest := (Sha256Fast.hash fingerprint.toUTF8).toList.take 16
  String.join (digest.map byteToHex)

-- The truncated digest is the spec's, byte for byte.
#guard shortHash "abc" == "ba7816bf8f01cfea414140de5dae2223"

end Concrete

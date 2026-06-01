/-
  Concrete.Sha256Spec — a mathematical SHA-256 specification.

  This module is DELIBERATELY independent of the Concrete evaluator,
  PExpr, and proof model. It is pure `BitVec`-valued mathematics that
  reads as FIPS 180-4, so that flagship proofs can show the *extracted*
  source `refines this spec*, rather than merely "evaluates to these
  bytes." (The HACL*/seL4 spec/implementation split — see
  docs/PROOF_LADDER.md.)

  Scope (task #16): the per-block SHA-256 pipeline — Boolean functions,
  rotations/sigmas, constants, big-endian word packing, message-schedule
  expansion, the compression round, and block compression. The
  multi-block padded `hash` and the `hmac` construction are intentionally
  left as TODOs for the proof pass that needs them (#21), where the
  padding details are pinned alongside the proof. Nothing here is proved;
  these are the refinement targets.

  BitVec-valued throughout so that `bv_decide` (kernel-checked) discharges
  the word-level identities.
-/

namespace Concrete.Sha256Spec

/-- A SHA-256 word. -/
abbrev W := BitVec 32

/-- A byte. -/
abbrev Byte := BitVec 8

-- ------------------------------------------------------------------
-- Boolean functions (FIPS 180-4 § 4.1.2)
-- ------------------------------------------------------------------

/-- `Ch(x,y,z) = (x AND y) XOR ((NOT x) AND z)`. -/
def ch (x y z : W) : W := (x &&& y) ^^^ ((~~~x) &&& z)

/-- `Maj(x,y,z) = (x AND y) XOR (x AND z) XOR (y AND z)`. -/
def maj (x y z : W) : W := (x &&& y) ^^^ (x &&& z) ^^^ (y &&& z)

-- ------------------------------------------------------------------
-- Rotations / shifts and the Sigma functions (FIPS 180-4 § 4.1.2)
-- ------------------------------------------------------------------

/-- Circular right rotation by `n` (`0 < n < 32`). -/
def rotr (x : W) (n : Nat) : W := (x >>> n) ||| (x <<< (32 - n))

def bigSigma0 (x : W) : W := rotr x 2 ^^^ rotr x 13 ^^^ rotr x 22
def bigSigma1 (x : W) : W := rotr x 6 ^^^ rotr x 11 ^^^ rotr x 25
def smallSigma0 (x : W) : W := rotr x 7 ^^^ rotr x 18 ^^^ (x >>> 3)
def smallSigma1 (x : W) : W := rotr x 17 ^^^ rotr x 19 ^^^ (x >>> 10)

-- ------------------------------------------------------------------
-- Constants
-- ------------------------------------------------------------------

/-- SHA-256 initial hash value H(0) (FIPS 180-4 § 5.3.3). -/
def initState : List W :=
  [0x6a09e667#32, 0xbb67ae85#32, 0x3c6ef372#32, 0xa54ff53a#32,
   0x510e527f#32, 0x9b05688c#32, 0x1f83d9ab#32, 0x5be0cd19#32]

/-- SHA-256 round constants K[0..63] (FIPS 180-4 § 4.2.2). -/
def k : List W :=
  [0x428a2f98#32, 0x71374491#32, 0xb5c0fbcf#32, 0xe9b5dba5#32,
   0x3956c25b#32, 0x59f111f1#32, 0x923f82a4#32, 0xab1c5ed5#32,
   0xd807aa98#32, 0x12835b01#32, 0x243185be#32, 0x550c7dc3#32,
   0x72be5d74#32, 0x80deb1fe#32, 0x9bdc06a7#32, 0xc19bf174#32,
   0xe49b69c1#32, 0xefbe4786#32, 0x0fc19dc6#32, 0x240ca1cc#32,
   0x2de92c6f#32, 0x4a7484aa#32, 0x5cb0a9dc#32, 0x76f988da#32,
   0x983e5152#32, 0xa831c66d#32, 0xb00327c8#32, 0xbf597fc7#32,
   0xc6e00bf3#32, 0xd5a79147#32, 0x06ca6351#32, 0x14292967#32,
   0x27b70a85#32, 0x2e1b2138#32, 0x4d2c6dfc#32, 0x53380d13#32,
   0x650a7354#32, 0x766a0abb#32, 0x81c2c92e#32, 0x92722c85#32,
   0xa2bfe8a1#32, 0xa81a664b#32, 0xc24b8b70#32, 0xc76c51a3#32,
   0xd192e819#32, 0xd6990624#32, 0xf40e3585#32, 0x106aa070#32,
   0x19a4c116#32, 0x1e376c08#32, 0x2748774c#32, 0x34b0bcb5#32,
   0x391c0cb3#32, 0x4ed8aa4a#32, 0x5b9cca4f#32, 0x682e6ff3#32,
   0x748f82ee#32, 0x78a5636f#32, 0x84c87814#32, 0x8cc70208#32,
   0x90befffa#32, 0xa4506ceb#32, 0xbef9a3f7#32, 0xc67178f2#32]

-- ------------------------------------------------------------------
-- Big-endian byte/word packing
-- ------------------------------------------------------------------

/-- Pack four big-endian bytes into a 32-bit word. -/
def packWord (b0 b1 b2 b3 : Byte) : W :=
  ((b0.setWidth 32) <<< 24) ||| ((b1.setWidth 32) <<< 16) |||
  ((b2.setWidth 32) <<< 8) ||| (b3.setWidth 32)

/-- The 16 words of a 64-byte block, big-endian. `block` is indexed
    `block.getD k 0`; for an in-range block this is `block[k]`. -/
def blockToWords (block : List Byte) : List W :=
  (List.range 16).map (fun j =>
    packWord (block.getD (4 * j) 0) (block.getD (4 * j + 1) 0)
             (block.getD (4 * j + 2) 0) (block.getD (4 * j + 3) 0))

-- ------------------------------------------------------------------
-- Message schedule and compression (FIPS 180-4 § 6.2.2)
-- ------------------------------------------------------------------

/-- Expand the 16 block words to the full 64-word message schedule:
    `W[i] = sigma1(W[i-2]) + W[i-7] + sigma0(W[i-15]) + W[i-16]`,
    all additions mod 2^32 (BitVec `+`). -/
def expandSchedule (w16 : List W) : List W := Id.run do
  let mut w := w16
  for i in [16:64] do
    w := w ++ [smallSigma1 (w.getD (i - 2) 0) + w.getD (i - 7) 0
               + smallSigma0 (w.getD (i - 15) 0) + w.getD (i - 16) 0]
  return w

/-- One compression round on the working state `[a,b,c,d,e,f,g,h]`
    with round constant `kc` and schedule word `wc`. -/
def round (s : List W) (kc wc : W) : List W :=
  let a := s.getD 0 0; let b := s.getD 1 0; let c := s.getD 2 0
  let d := s.getD 3 0; let e := s.getD 4 0; let f := s.getD 5 0
  let g := s.getD 6 0; let h := s.getD 7 0
  let t1 := h + bigSigma1 e + ch e f g + kc + wc
  let t2 := bigSigma0 a + maj a b c
  [t1 + t2, a, b, c, d + t1, e, f, g]

/-- Compress one 64-byte block into the 8-word state: 64 rounds over
    the expanded schedule, then the Davies-Meyer feed-forward (add the
    working state back into the input state, mod 2^32). -/
def compress (state : List W) (block : List Byte) : List W := Id.run do
  let w := expandSchedule (blockToWords block)
  let mut s := state
  for i in [0:64] do
    s := round s (k.getD i 0) (w.getD i 0)
  return (List.range 8).map (fun j => state.getD j 0 + s.getD j 0)

-- ------------------------------------------------------------------
-- Multi-block hash (FIPS 180-4 § 5.1.1 padding + § 6.2 chaining)
-- ------------------------------------------------------------------

/-- The 8-byte big-endian encoding of a 64-bit count. -/
def be64 (n : Nat) : List Byte :=
  (List.range 8).map (fun i => ((BitVec.ofNat 64 n) >>> (8 * (7 - i))).setWidth 8)

/-- FIPS 180-4 § 5.1.1 padding: append `0x80`, then zeros, then the
    64-bit big-endian message bit-length, to the next 64-byte multiple. -/
def padMessage (message : List Byte) : List Byte :=
  let len := message.length
  let paddedLen := ((len + 9 + 63) / 64) * 64
  message ++ [128] ++ List.replicate (paddedLen - len - 9) 0 ++ be64 (len * 8)

/-- The 64-byte block at index `blk`. -/
def blockAt (bytes : List Byte) (blk : Nat) : List Byte :=
  (bytes.drop (blk * 64)).take 64

/-- The 8-word state after compressing every padded block, from H(0). -/
def hashState (message : List Byte) : List W :=
  let padded := padMessage message
  (List.range (padded.length / 64)).foldl
    (fun st blk => compress st (blockAt padded blk)) initState

/-- Big-endian unpack of a word into 4 bytes. -/
def wordToBytes (x : W) : List Byte :=
  [(x >>> 24).setWidth 8, (x >>> 16).setWidth 8, (x >>> 8).setWidth 8, x.setWidth 8]

/-- Big-endian unpack of the 8-word state into the 32-byte digest. -/
def stateToBytes (state : List W) : List Byte := (state.take 8).flatMap wordToBytes

/-- SHA-256 of a byte message (FIPS 180-4). -/
def hash (message : List Byte) : List Byte := stateToBytes (hashState message)

-- ------------------------------------------------------------------
-- HMAC-SHA256 (RFC 2104)
-- ------------------------------------------------------------------

/-- Normalize a key to a 64-byte block `K'`: hash it first if it is
    longer than the block, then zero-pad to 64 bytes. -/
def keyPrep (key : List Byte) : List Byte :=
  let k0 := if key.length > 64 then hash key else key
  k0 ++ List.replicate (64 - k0.length) 0

/-- `HMAC(key, m) = H((K' ⊕ opad) ‖ H((K' ⊕ ipad) ‖ m))`,
    ipad = `0x36`×64, opad = `0x5c`×64 (RFC 2104). -/
def hmac (key message : List Byte) : List Byte :=
  let kp := keyPrep key
  hash ((kp.map (fun b => b ^^^ 92)) ++ hash ((kp.map (fun b => b ^^^ 54)) ++ message))

-- ------------------------------------------------------------------
-- Vector validation (FIPS 180-4 App. B / RFC 4231) — checks the spec
-- is the right mathematics before any refinement proof relies on it.
-- ------------------------------------------------------------------

/-- SHA-256("abc") = ba7816bf…f20015ad (FIPS 180-4 Appendix B.1). -/
example : hash [0x61, 0x62, 0x63] =
    [0xba,0x78,0x16,0xbf,0x8f,0x01,0xcf,0xea,0x41,0x41,0x40,0xde,0x5d,0xae,0x22,0x23,
     0xb0,0x03,0x61,0xa3,0x96,0x17,0x7a,0x9c,0xb4,0x10,0xff,0x61,0xf2,0x00,0x15,0xad] := by
  native_decide

/-- SHA-256("") = e3b0c442…7852b855. -/
example : hash [] =
    [0xe3,0xb0,0xc4,0x42,0x98,0xfc,0x1c,0x14,0x9a,0xfb,0xf4,0xc8,0x99,0x6f,0xb9,0x24,
     0x27,0xae,0x41,0xe4,0x64,0x9b,0x93,0x4c,0xa4,0x95,0x99,0x1b,0x78,0x52,0xb8,0x55] := by
  native_decide

/-- HMAC-SHA256 RFC 4231 Test Case 1: key = 0x0b×20, data = "Hi There". -/
example : hmac (List.replicate 20 0x0b)
    [0x48,0x69,0x20,0x54,0x68,0x65,0x72,0x65] =
    [0xb0,0x34,0x4c,0x61,0xd8,0xdb,0x38,0x53,0x5c,0xa8,0xaf,0xce,0xaf,0x0b,0xf1,0x2b,
     0x88,0x1d,0xc2,0x00,0xc9,0x83,0x3d,0xa7,0x26,0xe9,0x37,0x6c,0x2e,0x32,0xcf,0xf7] := by
  native_decide

/-- HMAC-SHA256 RFC 4231 Test Case 2: key = "Jefe", data = "what do ya want for nothing?". -/
example : hmac [0x4a,0x65,0x66,0x65]
    [0x77,0x68,0x61,0x74,0x20,0x64,0x6f,0x20,0x79,0x61,0x20,0x77,0x61,0x6e,0x74,0x20,
     0x66,0x6f,0x72,0x20,0x6e,0x6f,0x74,0x68,0x69,0x6e,0x67,0x3f] =
    [0x5b,0xdc,0xc1,0x46,0xbf,0x60,0x75,0x4e,0x6a,0x04,0x24,0x26,0x08,0x95,0x75,0xc7,
     0x5a,0x00,0x3f,0x08,0x9d,0x27,0x39,0x83,0x9d,0xec,0x58,0xb9,0x64,0xec,0x38,0x43] := by
  native_decide

end Concrete.Sha256Spec

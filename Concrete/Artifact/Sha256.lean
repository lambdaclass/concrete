/-!
# SHA-256 for artifact byte identities

This total Lean implementation ports the UInt32 compression loop from the frozen compiler.
Its normal driver execution relies on the Lean compiler and runtime. Reference vectors provide
regression evidence for the implementation. SHA-256 identities carry no semantic correspondence.
-/

namespace Concrete.Artifact.Sha256

private def k : Array UInt32 := #[
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

@[inline] private def rotr (x : UInt32) (n : UInt32) : UInt32 := (x >>> n) ||| (x <<< (32 - n))

/-- FIPS 180-4 § 5.1.1 padding, as bytes. -/
private def pad (msg : ByteArray) : ByteArray := Id.run do
  let len := msg.size
  let paddedLen := ((len + 9 + 63) / 64) * 64
  let mut out := msg.push 0x80
  for _ in [0:paddedLen - len - 9] do
    out := out.push 0
  let bits := len * 8
  for i in [0:8] do
    out := out.push ((bits >>> (8 * (7 - i))) % 256).toUInt8
  return out

/-- Compress the 64-byte block at `off` into `h`. -/
private def compress (h : Array UInt32) (p : ByteArray) (off : Nat) : Array UInt32 := Id.run do
  let mut w : Array UInt32 := Array.replicate 64 0
  for j in [0:16] do
    let b i := (p.get! (off + 4 * j + i)).toUInt32
    w := w.set! j ((b 0 <<< 24) ||| (b 1 <<< 16) ||| (b 2 <<< 8) ||| b 3)
  for i in [16:64] do
    let x := w[i - 15]!
    let y := w[i - 2]!
    let s0 := rotr x 7 ^^^ rotr x 18 ^^^ (x >>> 3)
    let s1 := rotr y 17 ^^^ rotr y 19 ^^^ (y >>> 10)
    w := w.set! i (s1 + w[i - 7]! + s0 + w[i - 16]!)
  let mut a := h[0]!
  let mut b := h[1]!
  let mut c := h[2]!
  let mut d := h[3]!
  let mut e := h[4]!
  let mut f := h[5]!
  let mut g := h[6]!
  let mut hh := h[7]!
  for i in [0:64] do
    let t1 := hh + (rotr e 6 ^^^ rotr e 11 ^^^ rotr e 25) + ((e &&& f) ^^^ ((~~~e) &&& g))
              + k[i]! + w[i]!
    let t2 := (rotr a 2 ^^^ rotr a 13 ^^^ rotr a 22) + ((a &&& b) ^^^ (a &&& c) ^^^ (b &&& c))
    hh := g
    g := f
    f := e
    e := d + t1
    d := c
    c := b
    b := a
    a := t1 + t2
  return #[h[0]! + a, h[1]! + b, h[2]! + c, h[3]! + d,
           h[4]! + e, h[5]! + f, h[6]! + g, h[7]! + hh]

/-- SHA-256 of a byte message. Refuse lengths outside the FIPS 180-4 64-bit bit length. -/
def hash (msg : ByteArray) : Option ByteArray := Id.run do
  if msg.size ≥ 2^61 then return none
  let p := pad msg
  let mut h : Array UInt32 := #[0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                                0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]
  for blk in [0:p.size / 64] do
    h := compress h p (64 * blk)
  let mut out := ByteArray.emptyWithCapacity 32
  for x in h do
    out := out.push (x >>> 24).toUInt8 |>.push (x >>> 16).toUInt8
             |>.push (x >>> 8).toUInt8 |>.push x.toUInt8
  return some out

end Concrete.Artifact.Sha256

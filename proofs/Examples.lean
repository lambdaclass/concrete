-- Root of the `Examples` Lake library: the flagship / example CORRECTNESS proofs.
--
-- These modules were moved out of the compiler library (`Concrete.lean`) so the
-- compiler's size, module graph, and trusted surface reflect implementation only.
-- They still `import Concrete.Proof.Proof` (they build ON the compiler's proof
-- infrastructure) and are kernel-checked by building this library; the compiler
-- itself references their theorems by name/fingerprint, never by import.
--
-- Namespace convention: `Examples.<Ex>.Proofs` (guarded by check_proof_namespace.sh).
import Examples.HmacSha256.Proofs
import Examples.LoopInvariant.Proofs
import Examples.ParseValidate.Proofs
import Examples.CryptoVerify.Proofs
import Examples.FixedCapacity.Proofs
import Examples.ConstantTimeTag.Proofs
import Examples.ElfHeader.Proofs
import Examples.ProofPatterns.Proofs

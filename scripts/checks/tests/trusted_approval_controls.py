"""T1 SSH controls. All throwaway keys and signers live only in this temp dir."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile

spec = importlib.util.spec_from_file_location("approval", Path(__file__).resolve().parents[1] / "lib/trusted_approval.py")
a = importlib.util.module_from_spec(spec)
spec.loader.exec_module(a)

def reject(fn, expected):
    try:
        fn()
    except ValueError as error:
        assert str(error) == expected, str(error)
    else:
        raise AssertionError("accepted planted control: " + expected)

# Check the production file itself, including when it is still awaiting the key.
try:
    a.validate_owner_signers(a.OWNER_SIGNERS)
except ValueError as error:
    assert str(error) == "awaiting owner key", str(error)

with tempfile.TemporaryDirectory() as tmp:
    d = Path(tmp)
    keys = [d / "owner", d / "wrong-signer"]
    for key in keys:
        subprocess.run(["ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-f", str(key)], check=True, timeout=15)
    signers = d / "allowed_signers"
    signers.write_text(a.OWNER_PRINCIPAL + " " + keys[0].with_suffix(".pub").read_text())
    digest = "a" * 64
    def sign(key, namespace):
        message = d / "digest"
        message.write_text(digest + "\n")
        signature = d / "digest.sig"
        signature.unlink(missing_ok=True)
        subprocess.run(["ssh-keygen", "-Y", "sign", "-f", str(key), "-n", namespace, str(message)],
                       check=True, capture_output=True, timeout=15)
        return signature
    signature = sign(keys[0], a.OWNER_NAMESPACE)
    a.verify_owner_signature(digest, signature, signers)
    reject(lambda: a.verify_owner_signature("b" * 64, signature, signers), "owner signature verification failed")
    signature = sign(keys[1], a.OWNER_NAMESPACE)
    reject(lambda: a.verify_owner_signature(digest, signature, signers), "owner signature verification failed")
    signature = sign(keys[0], "wrong-namespace")
    reject(lambda: a.verify_owner_signature(digest, signature, signers), "owner signature verification failed")
    # Plant only in a private copy, never in the production trust root.
    signers.write_text(a.OWNER_SIGNERS.read_text() + "\nintruder " + keys[1].with_suffix(".pub").read_text())
    reject(lambda: a.validate_owner_signers(signers), "allowed-signers contains a non-owner entry")
print("PASS T1 owner signature, wrong signer, digest tamper, namespace and production principal controls")

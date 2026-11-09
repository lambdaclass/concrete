"""Bounded projection/carrier and dependent-contract regression probes."""
import argparse
from vacuity_controls import BASE
from vacuity_driver import ROOT, compiled_consumer, run


PROJECTIONS = {
    "subtype-proj": "(⟨0, h.1⟩ : {_k : Nat // n < n+1}).2",
    "packed-and-refl": "(⟨h.1, Nat.le.refl⟩ : n < n+1 ∧ n ≤ n).1",
    "pprod-proj": "(⟨0, h.1⟩ : PProd Nat (n < n+1)).2",
    "psigma-proj": "(⟨0, h.1⟩ : PSigma (fun _ : Nat => n < n+1)).2",
    "cases-bool": "Bool.casesOn true h.1 h.1",
    "cases-nat": "Nat.casesOn (motive := fun _ => n < n+1) n h.1 (fun _ => h.1)",
    "matcher": "match n == 0 with | true => h.1 | false => h.1",
    "nested-matcher": "match n == 0 with | true => h.1 | false => match n == 1 with | true => h.1 | false => h.1",
    "mixed-minors": "Bool.casesOn (n == 0) (Nat.lt_succ_self n) h.1",
    "recursor-function": "(Bool.casesOn (motive := fun _ => (n < n+1) → n < n+1) (n == 0) (fun hp => hp) (fun hp => hp)) h.1",
    "match-carrier": "(match n == 0 with | true => (⟨0, h.1⟩ : {_k : Nat // n < n+1}) | false => ⟨0, h.1⟩).2",
    "cast-let": "let carrier : {_k : Nat // n < n+1} := ⟨0, h.1⟩; Eq.mp rfl carrier.2",
    "iff-field": "((Iff.intro (fun _ : True => h.1) (fun _ => True.intro)).mp True.intro)",
    "exists-carrier": "Exists.elim (⟨0, h.1⟩ : ∃ _ : Nat, n < n+1) (fun _ hp => hp)",
    "sigma-carrier": "(⟨0, (⟨h.1⟩ : PLift (n < n+1))⟩ : Sigma (fun _ : Nat => PLift (n < n+1))).2.down",
}


def projection(body):
    helper = ("/-- Helper (projection). Carrier control (approved: independent:confirm). -/\n"
              f"theorem helper (n : Nat) (h : n < n+1 ∧ True) : n < n+1 := {body}\n")
    return (BASE.replace("/-- Law", helper + "/-- Law", 1)
            .replace("(_h : n = 2)", "(h : n < n+1 ∧ True)")
            .replace(":= Nat.lt_succ_self n", ":= helper n h")
            .replace("growth 2 rfl", "growth 2 ⟨by decide, trivial⟩")
            .replace("2 = 2 ∧ ¬", "(2 < 2+1 ∧ True) ∧ ¬"))


DEPENDENT = BASE.replace(
    "(n : Nat) (_h : n = 2) : n < S n := Nat.lt_succ_self n",
    "(n : Nat) : ∃ (x : {v : Nat // v ≤ S n}), x.1 < S x.1 := ⟨⟨0, Nat.zero_le _⟩, Nat.lt_succ_self _⟩"
).replace("2 < S 2 := growth 2 rfl", "(∃ (x : {v : Nat // v ≤ S 2}), x.1 < S x.1) := growth 2").replace(
    "2 = 2 ∧ ¬ 2 < mutant 2", "¬ (∃ (x : {v : Nat // v ≤ mutant 2}), x.1 < mutant x.1)").replace(":= by decide", ":= by intro ⟨x, h⟩; exact Nat.lt_irrefl _ h")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", action="store_true")
    parser.add_argument("--group", choices=["fields", "eliminators", "dependent", "all"], default="all")
    args = parser.parse_args()
    eliminators = {"cases-bool", "cases-nat", "matcher", "nested-matcher", "mixed-minors", "recursor-function", "match-carrier"}
    with compiled_consumer() as meta:
        for label, body in PROJECTIONS.items():
            if args.group == "dependent" or (args.group == "fields" and label in eliminators) or (args.group == "eliminators" and label not in eliminators):
                continue
            code, output = run(ROOT, "", projection(body), 4096, meta_path=meta)
            print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
            if not args.baseline:
                assert code == 1, output
                assert [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))] == [
                    "FAIL projection-law ConcreteSpecControl.growth "], output
        if args.group in {"fields", "all"}:
            code, output = run(ROOT, "", projection("(⟨h.1, Nat.lt_succ_self n⟩ : n < n+1 ∧ n < n+1).2"), 4096, meta_path=meta)
            print(f"ROUND5 independent-selected exit={code}\n{output}", flush=True)
            assert code == 0 and "N2 PASS" in output, output
        if args.group in {"eliminators", "all"}:
            for label, body in {
                "match-independent-field": "(match n == 0 with | true => (⟨h.1, Nat.lt_succ_self n⟩ : n < n+1 ∧ n < n+1) | false => ⟨h.1, Nat.lt_succ_self n⟩).2",
                "minor-independent-origin": "Subtype.rec (motive := fun _ => n < n+1) (fun _ hp => hp) (if n == 0 then (⟨0, Nat.lt_succ_self n⟩ : {_k : Nat // n < n+1}) else ⟨0, Nat.lt_succ_self n⟩)",
            }.items():
                fixture = projection(body)
                if label == "minor-independent-origin":
                    fixture = fixture.replace("theorem helper (n : Nat) (h :", "theorem helper (n : Nat) (_h :")
                code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
                assert code == 0 and "N2 PASS" in output, output
        if args.group in {"dependent", "all"}:
            code, output = run(ROOT, "", DEPENDENT, 4096, meta_path=meta)
            print(f"ROUND5 dependent exit={code}\n{output}", flush=True)
            if not args.baseline:
                assert code == 0 and "N2 PASS" in output, output
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

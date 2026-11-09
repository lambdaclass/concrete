"""Bounded projection/carrier and dependent-contract regression probes."""
import argparse
import re
from vacuity_controls import BASE
from vacuity_driver import ROOT, compiled_consumer, run

NESTED_EXISTS = "(if n == 0 then (⟨0, ⟨h.1, Nat.lt_succ_self n⟩⟩ : ∃ _ : Nat, n < n+1 ∧ n < n+1) else ⟨0, ⟨h.1, Nat.lt_succ_self n⟩⟩)"
FUNCTION_EXISTS = "(if n == 0 then (⟨0, fun hp => hp⟩ : ∃ _ : Nat, (n < n+1) → n < n+1) else ⟨0, fun hp => hp⟩)"

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
    "psum-carrier": "PSum.casesOn (motive := fun _ : PSum Nat (n < n+1) => n < n+1) (if n == 0 then PSum.inl n else PSum.inr h.1) (fun _ => Nat.lt_succ_self n) (fun hp => hp)",
    "nested-exists-field": "Exists.elim " + NESTED_EXISTS + " (fun _ hp => hp.1)",
    "minor-function-application": "Exists.elim " + FUNCTION_EXISTS + " (fun _ f => f h.1)",
    "recursive-ih-transfer": "Nat.rec (motive := fun _ => (n < n+1) → n < n+1) (fun hp => hp) (fun _ ih _ => ih h.1) n (Nat.lt_succ_self n)",
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


def premise_carriers():
    for label, domain, body, argument in [
        ("function-carrier", "Nat → {_k : Nat // n < n+1}", "(f 0).2", "(fun _ => ⟨0, by decide⟩)"),
        ("nested-carrier", "Sigma (fun _ : Nat => PLift (n < n+1))", "f.2.down", "⟨0, ⟨by decide⟩⟩"),
    ]:
        yield label, (BASE.replace("(n : Nat) (_h : n = 2)", f"(n : Nat) (f : {domain})")
                      .replace(":= Nat.lt_succ_self n", f":= {body}")
                      .replace("growth 2 rfl", "growth 2 " + argument).replace("2 = 2 ∧ ¬", "¬"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--group", choices=["fields", "eliminators", "dependent", "all"], default="all")
    args = parser.parse_args()
    eliminators = {"cases-bool", "cases-nat", "matcher", "nested-matcher", "mixed-minors", "recursor-function", "match-carrier", "psum-carrier", "nested-exists-field", "minor-function-application", "recursive-ih-transfer"}
    with compiled_consumer() as meta:
        for label, body in PROJECTIONS.items():
            if args.group == "dependent" or (args.group == "fields" and label in eliminators) or (args.group == "eliminators" and label not in eliminators):
                continue
            code, output = run(ROOT, "", projection(body), 4096, meta_path=meta)
            print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
            assert code == 1, output
            assert [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))] == [
                "FAIL projection-law ConcreteSpecControl.growth "], output
        if args.group in {"fields", "all"}:
            for kind in ["def", "opaque"]:
                for label, domain, value, body in [
                    ("data-projection", "{_k : Nat // n < n+1}", "⟨0, h.1⟩", "(carrierPack n h).2"),
                    ("data-elimination", "PSum Nat (n < n+1)", "if n == 0 then PSum.inl n else PSum.inr h.1", "PSum.casesOn (motive := fun _ => n < n+1) (carrierPack n h) (fun _ => Nat.lt_succ_self n) (fun hp => hp)"),
                ]:
                    declaration = kind + " carrierPack (n : Nat) (h : n < n+1 ∧ True) : " + domain + " := " + value
                    fixture = projection(body).replace("/-- Helper", declaration + "\n/-- Helper", 1)
                    code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                    print(f"ROUND5 {kind}-{label} exit={code}\n{output}", flush=True)
                    assert code == 1 and [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))] == ["FAIL projection-law ConcreteSpecControl.growth "], output
            for label, declaration, body in [
                ("unavailable-carrier", "axiom unavailableCarrier (n : Nat) (h : n < n+1 ∧ True) : {_k : Nat // n < n+1}", "(unavailableCarrier n h).2"),
                ("unavailable-sum", "axiom unavailableCarrier (n : Nat) (h : n < n+1 ∧ True) : PSum Nat (n < n+1)", "PSum.casesOn (motive := fun _ => n < n+1) (unavailableCarrier n h) (fun _ => Nat.lt_succ_self n) (fun hp => hp)"),
            ]:
                fixture = projection(body).replace("/-- Helper", declaration + "\n/-- Helper", 1)
                code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
                findings = [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))]
                assert code == 2 and len(findings) == 1 and re.fullmatch(
                    r"INCOMPLETE analysis-budget-or-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none reason=analysis-error heartbeats=\d+/2000000 work=0/20000 projection=\d+/4096",
                    findings[0]), output
                assert "DETAIL analysis-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none projection-body-unavailable ConcreteSpecControl.unavailableCarrier" in output.splitlines(), output
            for label, fixture in premise_carriers():
                code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
                assert code == 1 and [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))] == ["FAIL projection-law ConcreteSpecControl.growth "], output
            code, output = run(ROOT, "", projection("(⟨h.1, Nat.lt_succ_self n⟩ : n < n+1 ∧ n < n+1).2"), 4096, meta_path=meta)
            print(f"ROUND5 independent-selected exit={code}\n{output}", flush=True)
            assert code == 0 and "N2 PASS" in output, output
        if args.group in {"eliminators", "all"}:
            for label, body in {
                "match-independent-field": "(match n == 0 with | true => (⟨h.1, Nat.lt_succ_self n⟩ : n < n+1 ∧ n < n+1) | false => ⟨h.1, Nat.lt_succ_self n⟩).2",
                "minor-independent-origin": "Subtype.rec (motive := fun _ => n < n+1) (fun _ hp => hp) (if n == 0 then (⟨n, Nat.lt_succ_self n⟩ : {_k : Nat // n < n+1}) else ⟨n, Nat.lt_succ_self n⟩)",
                "psum-independent-origin": "PSum.casesOn (motive := fun _ : PSum Nat (n < n+1) => n < n+1) (if n == 0 then PSum.inl n else PSum.inr (Nat.lt_succ_self n)) (fun _ => Nat.lt_succ_self n) (fun hp => hp)",
                "nested-exists-independent-field": "Exists.elim " + NESTED_EXISTS + " (fun _ hp => hp.2)",
                "minor-function-independent-argument": "Exists.elim " + FUNCTION_EXISTS + " (fun _ f => f (Nat.lt_succ_self n))",
                "recursive-independent-base": "Nat.rec (motive := fun _ => (n < n+1) → n < n+1) (fun _ => Nat.lt_succ_self n) (fun _ ih _ => ih h.1) n (Nat.lt_succ_self n)",
            }.items():
                fixture = projection(body)
                if label in {"minor-independent-origin", "psum-independent-origin", "minor-function-independent-argument"}:
                    fixture = fixture.replace("theorem helper (n : Nat) (h :", "theorem helper (n : Nat) (_h :")
                code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
                assert code == 0 and "N2 PASS" in output, output
            tree = "inductive CarrierTree (p : Prop) : Type where\n | leaf : p → CarrierTree p\n | node : CarrierTree p → CarrierTree p → CarrierTree p\n"
            for label, leaf, branch, expected in [
                ("two-ih-projection", "fun hp => hp", "fun _ _ leftIH _ => leftIH", 1),
                ("two-ih-independent", "fun _ => Nat.lt_succ_self n", "fun _ _ _ rightIH => rightIH", 0),
            ]:
                body = ("CarrierTree.rec (motive := fun _ => n < n+1) (" + leaf + ") (" + branch + ") "
                        "(if n == 0 then CarrierTree.leaf (Nat.lt_succ_self n) else CarrierTree.node (CarrierTree.leaf h.1) (CarrierTree.leaf (Nat.lt_succ_self n)))")
                fixture = projection(body).replace("/-- Helper", tree + "/-- Helper", 1)
                code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                print(f"ROUND5 {label} exit={code}\n{output}", flush=True)
                assert code == expected, output
                assert [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))] == (
                    ["FAIL projection-law ConcreteSpecControl.growth "] if expected else []), output
            for field, expected in [("1", 1), ("2", 0)]:
                body = "((Eq.ndrec (motive := fun k => PProd Nat (k < k+1 ∧ k < k+1)) (⟨n, ⟨h.1, Nat.lt_succ_self n⟩⟩ : PProd Nat (n < n+1 ∧ n < n+1)) h.2).2)." + field
                fixture = (projection(body).replace("n < n+1 ∧ True", "n < n+1 ∧ n = n")
                           .replace("⟨by decide, trivial⟩", "⟨by decide, rfl⟩")
                           .replace("2 < 2+1 ∧ True", "2 < 2+1 ∧ 2 = 2"))
                code, output = run(ROOT, "", fixture, 4096, meta_path=meta)
                print(f"ROUND5 cast-field-{field} exit={code}\n{output}", flush=True)
                assert code == expected, output
                assert [r for r in output.splitlines() if r.startswith(("FAIL ", "INCOMPLETE "))] == (
                    ["FAIL projection-law ConcreteSpecControl.growth "] if expected else []), output
        if args.group in {"dependent", "all"}:
            code, output = run(ROOT, "", DEPENDENT, 4096, meta_path=meta)
            print(f"ROUND5 dependent exit={code}\n{output}", flush=True)
            assert code == 0 and "N2 PASS" in output, output
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

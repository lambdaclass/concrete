"""Regression fixtures for the round-two structural checker review."""

def extend(add, base, extra):
    example = "EXAMPLE"
    def bad(code, name=example):
        return {("FAIL", code, name)}
    def law(decl, absent=True):
        findings = bad("projection-law", "ConcreteSpecControl.projection") | bad("missing-instance", "ConcreteSpecControl.projection")
        if absent:
            findings |= bad("subject-absent", "ConcreteSpecControl.projection")
        return extra("/-- Law (subject `ConcreteSpecControl.S`). -/", decl), findings
    for label, decl in [
        ("obtain", "theorem projection (p q : Prop) (h : p ∧ q) : p := by obtain ⟨hp, _⟩ := h; exact hp"),
        ("cases", "theorem projection (p q : Prop) (h : p ∧ q) : q := by cases h with | intro _ hq => exact hq"),
        ("id", "theorem projection (p : Prop) (h : p) : p := id h"),
        ("true-intro", "theorem projection : True := True.intro"),
        ("unfolded-hypothesis", "theorem projection (n : Nat) (h : n < n + 1) : n < S n := Eq.mp rfl h"),
        ("wrapped-rfl", "theorem projection (n : Nat) : S n = n + 1 := Eq.symm rfl"),
    ]:
        text, expected = law(decl, label not in {"unfolded-hypothesis", "wrapped-rfl"})
        add(label, text, expected)
    tag = "/-- Instance (law `ConcreteSpecControl.growth`, case `bad`). -/"
    for label, helper in [
        ("helper-false", "private def discharge (h : False) : 2 = 2 := False.elim h"),
        ("false-cases", "private def discharge (h : False) : 2 = 2 := False.casesOn (fun _ => 2 = 2) h"),
    ]:
        # A proof-valued def is itself authored evidence and needs classification.
        helper = "/-- Helper (computation). Test elimination wrapper (approved: independent:r2). -/\n" + helper
        text = extra("", "axiom impossible : False\n" + helper + "\n" + tag + "\nexample : 2 < S 2 := growth 2 (discharge impossible)")
        add(label, text, bad("instance-false-discharge"))
    add("unclassified-def", extra("", "def proofDef : True := trivial"), bad("classification-count", "ConcreteSpecControl.proofDef"))
    add("duplicate-field", base.replace("case `two`).", "case `two`, law `ConcreteSpecControl.growth`).", 1),
        bad("evidence-tag") | bad("unpaired-planted") | bad("missing-instance", "ConcreteSpecControl.growth"))
    add("obligation-format", extra("/-- Obligation (slice 9, R-0219). -/", "def owed : Prop := True"), bad("obligation-format", "ConcreteSpecControl.owed"))
    add("annotated-only", extra("/-- Accepted (rule `not_a_declaration`). prose `.ret` and `localValue`. -/", "example : True := trivial"), set(), status=0)
    positive = """
namespace ConcreteSpecControl
def S (n : Nat) : Nat := n + 1
def mutant (n : Nat) : Nat := n
/-- Law (subject `S`). -/
theorem nonzero (n : Nat) : S n ≠ 0 := Nat.succ_ne_zero n
/-- Instance (law `nonzero`, case `zero`). -/
example : S 0 ≠ 0 := nonzero 0
/-- Planted (law `nonzero`, case `zero`, subject `S`, mutant `mutant`). -/
example : ¬ (mutant 0 ≠ 0) := by decide
end ConcreteSpecControl
"""
    add("inequality-positive", positive, set(), status=0)
    add("negation-positive", positive.replace("S n ≠ 0", "¬ S n = 0").replace("S 0 ≠ 0", "¬ S 0 = 0").replace("mutant 0 ≠ 0", "¬ mutant 0 = 0"), set(), status=0)
    equal = base.replace("def mutant (n : Nat) : Nat := n", "def mutant (n : Nat) : Nat := S n").replace("example : 2 = 2 ∧ ¬ 2 < mutant 2 := by decide", "example : 2 = 2 ∧ ¬ 2 < mutant 2 := counterfeit").replace("/-- Planted", "axiom counterfeit : 2 = 2 ∧ ¬ 2 < mutant 2\n/-- Planted")
    add("defeq-mutant", equal, bad("mutant-defeq"))
    chain = ""
    for i in range(1, 6):
        body = "Nat.lt_succ_self n" if i == 1 else f"d{i-1} n"
        chain += f"/-- Helper (computation). Test budget wrapper (approved: independent:r2). -/\ndef d{i} (n : Nat) : n < n + 1 := {body}\n"
    chain_fixture = base.replace("/-- Law", chain + "/-- Law", 1).replace(":= Nat.lt_succ_self n", ":= d5 n")
    # Restore d1 after replacing the law's proof.
    chain_fixture = chain_fixture.replace("def d1 (n : Nat) : n < n + 1 := d5 n", "def d1 (n : Nat) : n < n + 1 := Nat.lt_succ_self n")
    add("positive-budget-chain", chain_fixture, {("INCOMPLETE", "projection-budget", "ConcreteSpecControl.growth")}, budget=3, status=2)

    add("missing-close", base.replace("case `two`).", "case `two`.", 1), bad("evidence-tag") | bad("unpaired-planted") | bad("missing-instance", "ConcreteSpecControl.growth"))
    add("ancestor-namespace", extra("", "namespace Inner\n/-- Helper (computation). Mentions `S` in the parent namespace (approved: independent:r2). -/\ntheorem parent : True := trivial\nend Inner"), set(), status=0)
    add("private-module-boundary", {
        "ConcreteSpec.lean": "import ConcreteSpec.A\nimport ConcreteSpec.B\n",
        "ConcreteSpec/A.lean": "namespace ConcreteSpec.A\n/-- Helper (computation). Private support (approved: independent:r2). -/\nprivate theorem secret : True := trivial\nend ConcreteSpec.A\n",
        "ConcreteSpec/B.lean": "import ConcreteSpec.A\nnamespace ConcreteSpec.A\n/-- Helper (computation). Cannot see private `secret` (approved: independent:r2). -/\ntheorem marker : True := trivial\nend ConcreteSpec.A\n",
    }, bad("unknown-name", "ConcreteSpec.A.marker"))
    add("closure-gap", {"ConcreteSpec.lean": base, "ConcreteSpec/Unimported.lean": "def lost : Nat := 0\n"}, bad("closure-gap", "ConcreteSpec.Unimported"))

    wrapper = """
namespace ConcreteSpecControl
def S (n : Nat) : Option Nat := some (n + 1)
def mutant (_n : Nat) : Option Nat := none
def Correct (n : Nat) : Option Nat → Prop
  | none => False
  | some result => n < result
/-- Law (subject `S`). -/
theorem wrapped (n : Nat) : Correct n (S n) := Nat.lt_succ_self n
/-- Instance (law `wrapped`, case `two`). -/
example : Correct 2 (S 2) := wrapped 2
/-- Planted (law `wrapped`, case `two`, subject `S`, mutant `mutant`). -/
example : ¬ Correct 2 (mutant 2) := by intro h; exact h
end ConcreteSpecControl
"""
    add("wrapper-positive", wrapper, set(), status=0)
    add("wrapper-fresh-argument", wrapper.replace("¬ Correct 2 (mutant 2)", "¬ Correct 2 (mutant 999)"), bad("planted-proposition"))
    add("wrapper-erased-mutant", wrapper.replace("¬ Correct 2 (mutant 2)", "¬ False"), bad("planted-proposition"))
    add("wrapper-erased-instance", wrapper.replace("example : Correct 2 (S 2)", "example : 2 < 3"), bad("instance-application") | bad("missing-instance", "ConcreteSpecControl.wrapped") | bad("unpaired-planted"))
    add("vacuous-wrapper-branch", wrapper.replace("| none => False", "| none => True").replace("¬ Correct 2 (mutant 2)", "¬ False"), bad("planted-proposition"))

    for label, domain, body in [
        ("empty-elimination", "Empty", "Empty.elim h"),
        ("pempty-elimination", "PEmpty.{0}", "PEmpty.elim h"),
        ("helper-nomatch", "False", "by nomatch h"),
    ]:
        helper = f"/-- Helper (computation). Test elimination wrapper (approved: independent:r2). -/\nprivate def discharge (h : {domain}) : 2 = 2 := {body}"
        add(label, extra("", f"axiom impossible : {domain}\n" + helper + "\n" + tag + "\nexample : 2 < S 2 := growth 2 (discharge impossible)"), bad("instance-false-discharge"))

    decision = base.replace("theorem growth (n : Nat)", "theorem growth (_decision : DecidableEq Nat) (n : Nat)").replace("growth 2 rfl", "growth inferInstance 2 rfl")
    add("non-proof-decidable-argument", decision, set(), status=0)
    add("missing-provenance", base, {("INCOMPLETE", "missing-provenance", "ConcreteSpecControl.missingMetadata")}, status=2)

    add("lexical-error", "/-- unterminated", {("INCOMPLETE", "lexical-error", "[anonymous]")}, status=2)
    add("scoped-obligation-only", {
        "ConcreteSpec.lean": "import ConcreteSpec.Owed\n",
        "ConcreteSpec/Owed.lean": "/-- Obligation (discharged at slice 9, R-0219). -/\ndef ConcreteSpec.owed : Prop := True\n",
    }, set(), status=0)

    for label, opening, reference, expected in [
        ("open-namespace", "open ConcreteSpecControl", "S", set()),
        ("open-only", "open ConcreteSpecControl (S)", "S", set()),
        ("open-renaming", "open ConcreteSpecControl renaming S -> Operation", "Operation", set()),
        ("open-hiding", "open ConcreteSpecControl hiding S", "S", bad("unknown-name", "Other.marker")),
    ]:
        text = base + f"namespace Other\n{opening}\n/-- Helper (computation). References `{reference}` (approved: independent:r2). -/\ntheorem marker : True := trivial\nend Other\n"
        add(label, text, expected, status=1 if expected else 0)
    text = base + "namespace Other\nsection\nopen ConcreteSpecControl\nend\n/-- Helper (computation). References `S` after the open ended (approved: independent:r2). -/\ntheorem marker : True := trivial\nend Other\n"
    add("open-section-boundary", text, bad("unknown-name", "Other.marker"))
    text = base + "open ConcreteSpecControl in\n/-- Helper (computation). References `S` for one command (approved: independent:r2). -/\ntheorem Other.marker : True := trivial\n"
    add("open-in", text, set(), status=0)
    add("imported-matcher-provenance", {
        "ConcreteSpec.lean": "import ConcreteSpec.B\n",
        "ConcreteSpec/A.lean": "namespace ConcreteSpec\ndef Predicate : Option Nat → Prop\n  | none => False\n  | some n => 0 ≤ n\nend ConcreteSpec\n",
        "ConcreteSpec/B.lean": "import ConcreteSpec.A\nnamespace ConcreteSpec\n/-- Helper (computation). Evaluate the predicate (approved: independent:r2). -/\ntheorem helper (n : Nat) : Predicate (some n) := by simp [Predicate]\nend ConcreteSpec\n",
    }, set(), status=0)

    add("self-approved-exemption", base.replace("Law (subject `ConcreteSpecControl.S`).", "Law (subject `ConcreteSpecControl.S`). Exemption: bypass request (approved: self)."), bad("self-approved", "ConcreteSpecControl.growth"))
    add("independent-exemption", base.replace("Law (subject `ConcreteSpecControl.S`).", "Law (subject `ConcreteSpecControl.S`). Exemption: review note (approved: independent:r2)."), set(), status=0)
    structure_projection = "structure Box (p : Prop) : Prop where\n  proof : p\n/-- Law (subject `ConcreteSpecControl.S`). -/\ntheorem projection (p : Prop) (h : Box p) : p := by cases h with | mk hp => exact hp"
    add("structure-cases", extra("", structure_projection), bad("projection-law", "ConcreteSpecControl.projection") | bad("missing-instance", "ConcreteSpecControl.projection") | bad("subject-absent", "ConcreteSpecControl.projection"))

    partial = extra("/-- Law (subject `ConcreteSpecControl.S`). -/", "theorem negationPartial (n : Nat) (h : S n = 0) : False := Nat.succ_ne_zero n h\n/-- Instance (law `ConcreteSpecControl.negationPartial`, case `bad`). -/\nexample : S 0 ≠ 0 := negationPartial 0")
    add("partial-negation", partial, bad("instance-application") | bad("missing-instance", "ConcreteSpecControl.negationPartial"))
    alias = positive.replace("/-- Law", "def Nonzero (n : Nat) : Prop := S n ≠ 0\n/-- Law", 1).replace("theorem nonzero (n : Nat) : S n ≠ 0", "theorem nonzero (n : Nat) : Nonzero n").replace("example : S 0 ≠ 0", "example : Nonzero 0")
    add("negation-alias-positive", alias, set(), status=0)

    add("helper-reason-anchor", extra("/-- Earlier (context). Helper (computation). (approved: independent:r2). -/", "theorem helper : True := trivial"), bad("classification-reason", "ConcreteSpecControl.helper"))
    add("approval-missing-close", extra("/-- Helper (computation). Reason (approved: independent:r2 -/", "theorem helper : True := trivial"), bad("missing-approval", "ConcreteSpecControl.helper"))
    ambiguous = base + "namespace Left\ndef shared : Nat := 0\nend Left\nnamespace Right\ndef shared : Nat := 1\nend Right\nopen Left Right\n/-- Helper (computation). References `shared` (approved: independent:r2). -/\ntheorem Other.marker : True := trivial\n"
    add("ambiguous-open", ambiguous, bad("ambiguous-name", "Other.marker"))
    nested = base + "open ConcreteSpecControl in\nopen Nat in\n/-- Helper (computation). References `S` and `succ` in nested opens (approved: independent:r2). -/\ntheorem Other.marker : True := trivial\n"
    add("nested-open-in", nested, set(), status=0)

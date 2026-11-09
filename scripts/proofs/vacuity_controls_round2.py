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
    long_chain = ""
    for i in range(1, 97):
        body = "Nat.lt_succ_self n" if i == 1 else f"long{i-1} n"
        long_chain += ("/-- Helper (computation). Long projection traversal control "
                       "(approved: independent:r4). -/\n"
                       f"def long{i} (n : Nat) : n < n + 1 := {body}\n")
    long_fixture = base.replace("/-- Law", long_chain + "/-- Law", 1)
    long_fixture = long_fixture.replace("theorem growth (n : Nat) (_h : n = 2) : n < S n := Nat.lt_succ_self n",
                                        "theorem growth (n : Nat) (_h : n = 2) : n < S n := long96 n")
    add("projection-long-complete", long_fixture, set(), budget=4096, status=0)
    add("projection-long-incomplete", long_fixture,
        {("INCOMPLETE", "projection-budget", "ConcreteSpecControl.growth")}, budget=64, status=2)


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

    add("unclassified-opaque", extra("", "opaque proofOpaque : True := trivial"), bad("classification-count", "ConcreteSpecControl.proofOpaque"))
    add("opaque-helper-positive", extra("/-- Helper (computation). Opaque support proof (approved: independent:r2). -/", "opaque proofOpaque : True := trivial"), set(), status=0)

    add("forged-example-provenance", extra("", "def «⟪example⟫spoof» : True := trivial"), bad("example-provenance", "[anonymous]"))
    add("example-marker-literals-positive", extra("", 'def markerText : String := "⟪example⟫"\n-- ⟪example⟫ is permitted in comments.'), set(), status=0)

    dependent = base.replace("def mutant (n : Nat) : Nat := n", "def mutant (_n : Nat) : Nat := S 0")
    add("dependent-mutant-positive", dependent, set(), status=0)
    add("dependent-mutant-fresh-argument", dependent.replace("2 = 2 ∧ ¬ 2 < mutant 2", "2 = 2 ∧ ¬ 2 < mutant 999"), bad("planted-proposition"))
    add("dependent-mutant-erased-boundary", dependent.replace("2 = 2 ∧ ¬ 2 < mutant 2", "2 = 2 ∧ ¬ 2 < S 0"), bad("planted-proposition"))

    rooted = base.replace("Law (subject `ConcreteSpecControl.S`)", "Law (subject `_root_.S`)")
    add("rooted-name-no-local-fallback", rooted, bad("unknown-name", "ConcreteSpecControl.growth") | bad("planted-subject"))
    rooted = base.replace("Law (subject `ConcreteSpecControl.S`)", "Law (subject `_root_.ConcreteSpecControl.S`)")
    add("rooted-name-positive", rooted, set(), status=0)

    universal = positive.replace("/-- Law", "def Nonzero (n : Nat) : Prop := ∀ _k : Nat, S n ≠ 0\n/-- Law", 1).replace("theorem nonzero (n : Nat) : S n ≠ 0 := Nat.succ_ne_zero n", "theorem nonzero (n : Nat) : Nonzero n := fun _ => Nat.succ_ne_zero n").replace("example : S 0 ≠ 0", "example : Nonzero 0").replace("¬ (mutant 0 ≠ 0) := by decide", "¬ (∀ _k : Nat, mutant 0 ≠ 0) := by intro h; exact h 0 rfl")
    add("forall-alias-full-application", universal, set(), status=0)
    add("decide-hypothesis-positive", base.replace("growth 2 rfl", "growth 2 (by decide)"), set(), status=0)
    # Opaque discharging lemmas must reduce their closed proof arguments, at
    # arbitrary nesting depth, before unreachable branches are inspected.
    for depth in [1, 4, 12]:
        helpers = ""
        proof = "(by decide)"
        for i in range(depth):
            helpers += ("/-- Helper (computation). Closed discharge wrapper "
                        "(approved: independent:r4). -/\n"
                        f"theorem discharge{i} (h : 2 = 2) : 2 = 2 := by cases h; decide\n")
            proof = f"(discharge{i} {proof})"
        fixture = base.replace("/-- Law", helpers + "/-- Law", 1).replace("growth 2 rfl", f"growth 2 {proof}")
        add(f"nested-decide-discharge-{depth}", fixture, set(), status=0)




    for label, domain, elimination in [
        ("hypothetical-false-positive", "False", "False.elim h"),
        ("hypothetical-empty-positive", "Empty", "Empty.elim h"),
        ("hypothetical-pempty-positive", "PEmpty.{0}", "PEmpty.elim h"),
    ]:
        fixture = base.replace("_h : n = 2", f"_h : {domain} → n = 2")
        fixture = fixture.replace("growth 2 rfl", f"growth 2 (fun h => {elimination})")
        fixture = fixture.replace("2 = 2 ∧ ¬ 2 < mutant 2 := by decide",
            f"({domain} → 2 = 2) ∧ ¬ 2 < mutant 2 := by constructor; exact fun _ => rfl; decide")
        add(label, fixture, set(), status=0)

    universal_hyp = base.replace("_h : n = 2", "_h : ∀ _k : Nat, n = 2")
    universal_hyp = universal_hyp.replace("growth 2 rfl", "growth 2 (fun _ => rfl)")
    universal_hyp = universal_hyp.replace("2 = 2 ∧ ¬ 2 < mutant 2 := by decide",
        "(∀ _k : Nat, 2 = 2) ∧ ¬ 2 < mutant 2 := by constructor; exact fun _ => rfl; decide")
    for label, domain, body in [
        ("open-helper-false", "False", "False.elim h"),
        ("open-helper-empty", "Empty", "Empty.elim h"),
        ("open-helper-pempty", "PEmpty.{0}", "PEmpty.elim h"),
    ]:
        helper = (f"axiom impossible : {domain}\n"
                  "/-- Helper (computation). Generic discharge control (approved: independent:r4). -/\n"
                  f"theorem genericDischarge (_k : Nat) (h : {domain}) : 2 = 2 := {body}\n")
        fixture = universal_hyp.replace("/-- Law", helper + "/-- Law", 1)
        fixture = fixture.replace("end ConcreteSpecControl",
            "/-- Instance (law `ConcreteSpecControl.growth`, case `bad`). -/\n"
            "example : 2 < S 2 := growth 2 (fun k => genericDischarge k impossible)\n"
            "end ConcreteSpecControl")
        add(label, fixture, bad("instance-false-discharge"))

    # N4a F1: carry the impossible supplier through a non-Prop data argument.
    # The original paired case remains, so only the bad Instance may fail.
    for label, carrier, result, proof in [
        ("data-subtype-discharge", "{ m : Nat // m = 2 }", "s.val = 2", "s.property"),
        ("data-psigma-discharge", "PSigma (fun m : Nat => m = 2)", "s.fst = 2", "s.snd"),
        ("data-pprod-discharge", "PProd Nat (2 = 2)", "2 = 2", "s.snd"),
    ]:
        helper = ("axiom impossible : False\n"
                  "/-- Helper (computation). Data-carried supplier control (approved: independent:r6). -/\n"
                  f"theorem dataDischarge (_k : Nat) (s : {carrier}) : {result} := {proof}\n")
        fixture = universal_hyp.replace("/-- Law", helper + "/-- Law", 1)
        fixture = fixture.replace("end ConcreteSpecControl", tag +
            "\nexample : 2 < S 2 := growth 2 (fun k => dataDischarge k ⟨2, False.elim impossible⟩)\n"
            "end ConcreteSpecControl")
        add(label, fixture, bad("instance-false-discharge"))

    # N4a F1: legitimate closed proofs for an inhabited finite collection.
    for label, proof in [
        ("finite-collection-decide-positive", "by decide"),
        ("finite-collection-simp-positive", "by simp"),
        ("finite-collection-cases-positive", "by intro x hx; cases hx with | head => rfl | tail _ hnil => nomatch hnil"),
    ]:
        fixture = base.replace("_h : n = 2", "_h : ∀ x, x ∈ [n] → x = n")
        fixture = fixture.replace("growth 2 rfl", f"growth 2 ({proof})")
        fixture = fixture.replace("2 = 2 ∧ ¬ 2 < mutant 2 := by decide",
            "(∀ x, x ∈ [2] → x = 2) ∧ ¬ 2 < mutant 2 := by constructor; simp; decide")
        add(label, fixture, set(), status=0)

    # Jointly contradictory *hypothetical* binders are an unreachable branch.
    # Supplied inhabitants of the same two domains are actual discharges.
    joint_hyp = base.replace("_h : n = 2", "_h : ∀ p : Prop, p → ¬ p → n = 2")
    joint_hyp = joint_hyp.replace("growth 2 rfl", "growth 2 (fun _ hp hn => False.elim (hn hp))")
    joint_hyp = joint_hyp.replace("2 = 2 ∧ ¬ 2 < mutant 2 := by decide",
        "(∀ p : Prop, p → ¬ p → 2 = 2) ∧ ¬ 2 < mutant 2 := by constructor; exact fun _ _ _ => rfl; decide")
    add("joint-hypothetical-positive", joint_hyp, set(), status=0)
    helper_tag = "/-- Helper (computation). Supplied contradiction (approved: independent:r5). -/\n"
    joint = ("axiom opaqueP : Prop\naxiom hp : opaqueP\naxiom hn : ¬ opaqueP\n" + helper_tag +
        "theorem jointDischarge (_k : Nat) (p : Prop) (hp : p) (hn : ¬ p) : 2 = 2 := False.elim (hn hp)\n")
    add("joint-supplied-negative", extra("", joint + tag +
        "\nexample : 2 < S 2 := growth 2 (jointDischarge 0 opaqueP hp hn)"), bad("instance-false-discharge"))
    add("joint-open-supplied-negative", universal_hyp.replace("/-- Law", joint + "/-- Law", 1).replace(
        "end ConcreteSpecControl", tag +
        "\nexample : 2 < S 2 := growth 2 (fun k => jointDischarge k opaqueP hp hn)\nend ConcreteSpecControl"), bad("instance-false-discharge"))
    reachable = ("axiom impossibleAt : Nat → False\n" + helper_tag +
        "theorem reachableDischarge (k : Nat) : 2 = 2 := False.elim (impossibleAt k)\n")
    add("reachable-open-impossible-negative", universal_hyp.replace("/-- Law", reachable + "/-- Law", 1).replace(
        "end ConcreteSpecControl", tag +
        "\nexample : 2 < S 2 := growth 2 (fun k => reachableDischarge k)\nend ConcreteSpecControl"), bad("instance-false-discharge"))

    branching = ("axiom opaqueP : Prop\naxiom hp : opaqueP\naxiom hn : ¬ opaqueP\n" + helper_tag +
        "theorem branchDischarge (_k : Nat) (bad : Bool) (p : Prop) (hp : p) (hn : ¬ p) : 2 = 2 := by\n"
        "  cases bad with\n  | false => rfl\n  | true => exact False.elim (hn hp)\n")
    add("reachable-joint-helper-negative", universal_hyp.replace("/-- Law", branching + "/-- Law", 1).replace(
        "end ConcreteSpecControl", tag +
        "\nexample : 2 < S 2 := growth 2 (fun k => Eq.trans (branchDischarge k false opaqueP hp hn) "
        "(branchDischarge k true opaqueP hp hn))\nend ConcreteSpecControl"), bad("instance-false-discharge"))

    # Reusing a closed subterm after an unreachable context must inspect it
    # again. The contradiction here needs simp on decide, beyond the fast pass.
    contextual = base.replace("_h : n = 2",
        "_h : (∀ (p : Prop) [Decidable p], decide p = true → ¬p → n = 2) ∧ n = 2")
    contextual = contextual.replace("growth 2 rfl",
        "growth 2 ⟨(fun _ _ _ _ => rfl), rfl⟩")
    contextual = contextual.replace("2 = 2 ∧ ¬ 2 < mutant 2 := by decide",
        "((∀ (p : Prop) [Decidable p], decide p = true → ¬p → 2 = 2) ∧ 2 = 2) ∧ ¬ 2 < mutant 2 := "
        "⟨⟨(fun _ _ _ _ => rfl), rfl⟩, by decide⟩")
    contextual = contextual.replace("/-- Law", "axiom impossible : False\n/-- Law", 1)
    for label, right, expected in [
        ("unreachable-closed-supplier-positive", "rfl", set()),
        ("context-cache-supplied-negative", "False.elim impossible", bad("instance-false-discharge")),
    ]:
        proof = "growth 2 ⟨(fun _ _ _ _ => False.elim impossible), " + right + "⟩"
        if expected:
            fixture = contextual.replace("end ConcreteSpecControl", tag +
                "\nexample : 2 < S 2 := " + proof + "\nend ConcreteSpecControl")
        else:
            fixture = contextual.replace("growth 2 ⟨(fun _ _ _ _ => rfl), rfl⟩", proof)
        add(label, fixture, expected, status=1 if expected else 0)

    add("recursive-law-positive", extra("/-- Law (subject `ConcreteSpecControl.S`). -/", "theorem recursiveLaw (n : Nat) : n < S n := match n with\n  | 0 => Nat.zero_lt_succ 0\n  | n + 1 => Nat.succ_lt_succ (recursiveLaw n)"), bad("missing-instance", "ConcreteSpecControl.recursiveLaw"))
    add("namespace-components", base + "namespace Left.Right\nopen ConcreteSpecControl\n/-- Helper (computation). References `S` (approved: independent:r3). -/\ntheorem marker : True := trivial\nend Left.Right\n", set(), status=0)
    add("root-and-open-ambiguous", base + "def S : Nat := 0\nopen ConcreteSpecControl\n/-- Helper (computation). Ambiguous reference `S` (approved: independent:r3). -/\ntheorem Other.marker : True := trivial\n", bad("ambiguous-name", "Other.marker"))

    add("generated-recursor-provenance", base, set(), status=0)

    multiple = """
namespace ConcreteSpecControl
def S (n : Nat) := n + 1
def T (n : Nat) := S n + 1
def badS (n : Nat) := n
def badT (n : Nat) := n
/-- Law (subject `S`, `T`). -/
theorem together (n : Nat) : n < S n ∧ n < T n :=
  ⟨Nat.lt_succ_self n, Nat.lt_trans (Nat.lt_succ_self n) (Nat.lt_succ_self _)⟩
/-- Instance (law `together`, case `zero`). -/
example : 0 < S 0 ∧ 0 < T 0 := together 0
/-- Planted (law `together`, case `zero`, subject `S`, mutant `badS`). -/
example : ¬ (0 < badS 0 ∧ 0 < T 0) := by decide
/-- Planted (law `together`, case `zero`, subject `T`, mutant `badT`). -/
example : ¬ (0 < S 0 ∧ 0 < badT 0) := by decide
end ConcreteSpecControl
"""
    add("multiple-subjects-positive", multiple, set(), status=0)
    add("sibling-subject-erased", multiple.replace("0 < badS 0 ∧ 0 < T 0", "0 < badS 0 ∧ 0 < (badS 0 + 1)"), bad("planted-proposition"))

    partial_pair = base.replace("example : 2 < S 2 := growth 2 rfl", "example : ∀ _h : 2 = 2, 2 < S 2 := growth 2").replace("example : 2 = 2 ∧ ¬ 2 < mutant 2 := by decide", "example : ¬ (∀ _h : 2 = 2, 2 < mutant 2) := by intro h; exact Nat.lt_irrefl 2 (h rfl)")
    add("partial-paired", partial_pair, bad("instance-application") | bad("planted-proposition") | bad("unpaired-planted") | bad("missing-instance", "ConcreteSpecControl.growth"))

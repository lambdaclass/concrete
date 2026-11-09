"""Each throwaway fixture has an exact finding set, not merely a nonzero exit."""
BASE = """
namespace ConcreteSpecControl
def S (n : Nat) : Nat := n + 1
def mutant (n : Nat) : Nat := n
/-- Law (subject `ConcreteSpecControl.S`). -/
theorem growth (n : Nat) (_h : n = 2) : n < S n := Nat.lt_succ_self n
/-- Instance (law `ConcreteSpecControl.growth`, case `two`). -/
example : 2 < S 2 := growth 2 rfl
/-- Planted (law `ConcreteSpecControl.growth`, case `two`, subject `ConcreteSpecControl.S`, mutant `ConcreteSpecControl.mutant`). -/
example : 2 = 2 ∧ ¬ 2 < mutant 2 := by decide
end ConcreteSpecControl
"""


def controls(run, source, labels=None):
    cases = [("positive", BASE, 64, set(), 0)]

    def add(label, text, expected, budget=64, status=1):
        cases.append((label, text, budget, set(expected), status))

    # Same proof and telescope; declaration opacity must not hide projection.
    for kind in ["def", "theorem", "opaque"]:
        helper = ("/-- Helper (projection). Returns the premise. (approved: independent:confirm). -/\n"
                  + kind + " helper (n : Nat) (h : n < n+1 ∧ True) : n < n+1 := h.1\n")
        fixture = (BASE.replace("/-- Law", helper + "/-- Law", 1)
                   .replace("(_h : n = 2)", "(h : n < n+1 ∧ True)")
                   .replace(":= Nat.lt_succ_self n", ":= helper n h")
                   .replace("growth 2 rfl", "growth 2 ⟨by decide, trivial⟩")
                   .replace("2 = 2 ∧ ¬", "(2 < 2+1 ∧ True) ∧ ¬"))
        add("projection-helper-" + kind, fixture,
            {("FAIL", "projection-law", "ConcreteSpecControl.growth")})

    # Unavailable proof evidence is unknown, never a completed negative.
    unavailable = BASE.replace("/-- Law", "axiom unavailable (n : Nat) : n < n+1\n/-- Law", 1).replace(":= Nat.lt_succ_self n", ":= unavailable n")
    add("projection-unavailable", unavailable,
        {("INCOMPLETE", "analysis-budget-or-error", "ConcreteSpecControl.growth")}, status=2)

    # Observations for an OPEN owner contract, not enforcement of a new policy.
    hidden_law = BASE.replace("/-- Law", "axiom impossible : False\n/-- Helper (computation). Supplied proof. (approved: independent:confirm). -/\ntheorem hidden (n : Nat) : n < S n := False.elim impossible\n/-- Law", 1).replace(":= Nat.lt_succ_self n", ":= hidden n")
    add("hidden-law-body", hidden_law, set(), status=0)
    hidden_planted = BASE.replace("/-- Law", "axiom impossible : False\n/-- Helper (computation). Supplied proof. (approved: independent:confirm). -/\ntheorem hidden : False := impossible\n/-- Law", 1).replace(":= by decide", ":= False.elim hidden")
    add("hidden-planted-body", hidden_planted, set(), status=0)

    from pathlib import Path
    repro = Path(__file__).with_name("fixtures") / "F3b-Repro.lean"
    inline_match = (Path(__file__).with_name("fixtures") / "N2-Inline-Match-Repro.lean").read_text()
    add("inline-match-positive", inline_match, set(), status=0)
    # Kernel conversion at the concrete operation result hides this changed
    # branch, but protected conversion must retain the law's actual contract.
    negative = inline_match.replace("end ConcreteSpec.Repro", """/-- Instance (law `ConcreteSpec.Repro.growth`, case `changed-branch`). -/
example : (match S 2 with
    | .ok (y, n) => y = y ∧ 0 < n
    | _ => True) := growth 2

end ConcreteSpec.Repro""")
    add("inline-match-changed-branch", negative,
        {("FAIL", "instance-application", "EXAMPLE")})
    add("joint-branch-decide-repro", {
        "ConcreteSpec.lean": "import ConcreteSpec.Repro\n",
        "ConcreteSpec/Repro.lean": repro.read_text(),
    }, {("INCOMPLETE", "analysis-budget-or-error", "ConcreteSpec.Repro.find_absent")}, status=2)

    add("unclassified", BASE.replace("/-- Law (subject `ConcreteSpecControl.S`). -/", ""),
        {("FAIL", "classification-count", "ConcreteSpecControl.growth"), ("FAIL", "evidence-law-kind", "EXAMPLE")})
    add("double", BASE.replace("Law (subject", "Helper (computation). reason (approved: independent:r1) (writer: writer) (reviewer: reviewer). Law (subject"),
        {("FAIL", "classification-count", "ConcreteSpecControl.growth")})
    add("unknown", BASE.replace("ConcreteSpecControl.S`", "ConcreteSpecControl.missing`", 1),
        {("FAIL", "unknown-name", "ConcreteSpecControl.growth"), ("FAIL", "planted-subject", "EXAMPLE")})
    # These keep a valid paired case; the isolated bad example must add only its own diagnostic.
    prefix, suffix = BASE.rsplit("end ConcreteSpecControl", 1)
    def extra(doc, decl):
        return prefix + doc + "\n" + decl + "\nend ConcreteSpecControl\n"
    tag = "/-- Instance (law `ConcreteSpecControl.growth`, case `bad`). -/"
    add("partial", extra(tag, "example : ∀ _h : 2 = 2, 2 < S 2 := growth 2"),
        {("FAIL", "instance-application", "EXAMPLE")})
    add("restated", extra(tag, "example : 2 < S 2 := Nat.lt_succ_self 2"),
        {("FAIL", "instance-application", "EXAMPLE")})
    add("negated-premise", BASE.replace("2 = 2 ∧ ¬ 2 < mutant 2", "¬ 2 = 3"),
        {("FAIL", "planted-proposition", "EXAMPLE")})
    add("fresh-arguments", BASE.replace("2 = 2 ∧ ¬ 2 < mutant 2", "3 = 3 ∧ ¬ 3 < mutant 3"),
        {("FAIL", "planted-proposition", "EXAMPLE")})
    add("wrong-mutant-type", BASE.replace("def mutant (n : Nat) : Nat := n", "def mutant (n : Nat) : Int := n").replace("2 = 2 ∧ ¬ 2 < mutant 2", "2 = 2 ∧ ¬ 2 < mutant 2"),
        {("FAIL", "mutant-type", "EXAMPLE")})
    for label, declaration in [
        ("returned-hypothesis", "theorem projection (p : Prop) (h : p) : p := h"),
        ("applied-hypothesis", "theorem projection (p : Nat → Prop) (h : ∀ n, p n) : p 0 := h 0"),
        ("repacked-witness", "theorem projection (p : Nat → Prop) (h : p 0) : ∃ n, p n := ⟨0, h⟩"),
        ("and-left", "theorem projection (p q : Prop) (h : p ∧ q) : p := And.left h"),
        ("and-right", "theorem projection (p q : Prop) (h : p ∧ q) : q := And.right h"),
        ("expr-proj", "structure Box (p : Prop) : Prop where\n  proof : p\n/-- Law (subject `ConcreteSpecControl.S`). -/\ntheorem projection (p : Prop) (h : Box p) : p := h.proof"),
        ("repackaging", "theorem projection (p q : Prop) (h : p ∧ q) : p ∧ q := ⟨h.1, h.2⟩"),
        ("rfl", "theorem projection (n : Nat) : S n = S n := rfl"),
    ]:
        # The projection deliberately has no evidence: exact missing-instance is expected too.
        doc = "" if label == "expr-proj" else "/-- Law (subject `ConcreteSpecControl.S`). -/"
        add(label, extra(doc, declaration),
            {("FAIL", "projection-law", "ConcreteSpecControl.projection"),
             ("FAIL", "missing-instance", "ConcreteSpecControl.projection"),
             ("FAIL", "subject-absent", "ConcreteSpecControl.projection")} if label != "rfl" else
            {("FAIL", "projection-law", "ConcreteSpecControl.projection"),
             ("FAIL", "missing-instance", "ConcreteSpecControl.projection")}, status=1)
    add("obligation-theorem", extra("/-- Obligation (discharged at slice 9, R-0219). -/", "theorem owed : True := trivial"),
        {("FAIL", "obligation-kind", "ConcreteSpecControl.owed")})
    add("obligation-row", extra("/-- Obligation (discharged at slice 9, R-9999). -/", "def owed : Prop := True"),
        {("FAIL", "obligation-roadmap", "ConcreteSpecControl.owed")})
    add("self-approved", extra("/-- Helper (computation). reason (approved: r1) (writer: alice) (reviewer: alice). -/", "theorem helper : True := trivial"),
        {("FAIL", "self-approved", "ConcreteSpecControl.helper")})
    add("authored-generated-suffix", extra("", "theorem authored.eq_1 : True := trivial"),
        {("FAIL", "classification-count", "ConcreteSpecControl.authored.eq_1")})
    add("generated-provenance", extra("", "def recursive : Nat → Nat\n  | 0 => 0\n  | n + 1 => recursive n + 1"), set(), status=0)
    add("hidden-partial", extra("", "def OpenCase : Prop := ∀ _h : 2 = 2, 2 < S 2\n/-- Instance (law `ConcreteSpecControl.growth`, case `hidden`). -/\nexample : OpenCase := growth 2"),
        {("FAIL", "instance-application", "EXAMPLE")})
    add("false-discharge", extra("", "axiom impossible : False\n/-- Instance (law `ConcreteSpecControl.growth`, case `exfalso`). -/\nexample : 2 < S 2 := growth 2 (False.elim impossible)"),
        {("FAIL", "instance-false-discharge", "EXAMPLE")})
    add("missing-planted", BASE.replace("/-- Planted (law `ConcreteSpecControl.growth`, case `two`, subject `ConcreteSpecControl.S`, mutant `ConcreteSpecControl.mutant`). -/", ""),
        {("FAIL", "missing-planted", "ConcreteSpecControl.growth")})
    approved = "(approved: independent:r1) (writer: writer) (reviewer: reviewer)."
    positive = extra("/-- Helper (computation). Computes reflexivity. " + approved + " -/", "private theorem helper : 1 = 1 := rfl")
    positive = positive.replace("end ConcreteSpecControl", "/-- Representation lemma. Encoding reflexivity. " + approved + " -/\ntheorem representation : 2 = 2 := rfl\n/-- Obligation (discharged at slice 9, R-0219). -/\ndef owed (n : Nat) : Prop := n = n\nend ConcreteSpecControl")
    positive = positive.replace("end ConcreteSpecControl", "structure OwedFields : Prop where\n  /-- Obligation (discharged at slice 9, R-0219). -/\n  checked : 1 = 1\nend ConcreteSpecControl")
    add("non-law-positive", positive, set(), status=0)
    for label, proof in [("absurd-discharge", "absurd (rfl : 2 = 2) (fun _ => impossible)"),
                         ("nomatch-discharge", "by nomatch impossible")]:
        add(label, extra("", "axiom impossible : False\n/-- Instance (law `ConcreteSpecControl.growth`, case `exfalso`). -/\nexample : 2 < S 2 := growth 2 (" + proof + ")"),
            {("FAIL", "instance-false-discharge", "EXAMPLE")})
    add("final-carrier-positive", extra("/-- Helper (computation). Computes reflexivity. (approved: independent-review:r5). -/", "theorem helper : 1 = 1 := rfl"), set(), status=0)
    add("literal-self-approval", extra("/-- Helper (computation). Computes reflexivity. (approved: self). -/", "theorem helper : 1 = 1 := rfl"),
        {("FAIL", "self-approved", "ConcreteSpecControl.helper")})
    add("proof-definition-obligation", extra("/-- Obligation (discharged at slice 9, R-0219). -/", "def proofDef : True := trivial"),
        {("FAIL", "obligation-kind", "ConcreteSpecControl.proofDef")})
    add("law-definition", extra("/-- Law (subject `ConcreteSpecControl.S`). -/", "def falseLaw : 1 = 1 := rfl"),
        {("FAIL", "law-kind", "ConcreteSpecControl.falseLaw")})
    relative = BASE.replace("Law (subject `ConcreteSpecControl.S`)", "Law (subject `ConcreteSpecControl.«S»`)").replace("Instance (law `ConcreteSpecControl.growth`", "Instance (law `growth`").replace("Planted (law `ConcreteSpecControl.growth`", "Planted (law `ConcreteSpecControl.«growth»`").replace("subject `ConcreteSpecControl.S`, mutant `ConcreteSpecControl.mutant`", "subject `S`, mutant `mutant`")
    add("resolved-name-identity", relative, set(), status=0)
    shared = BASE.replace("end ConcreteSpecControl", "/-- Law (subject `ConcreteSpecControl.S`). -/\ntheorem growth2 (n : Nat) (_h : n = 2) : n < S n := Nat.lt_succ_self n\n/-- Instance (law `ConcreteSpecControl.growth2`, case `two`). -/\nexample : 2 < S 2 := growth2 2 rfl\nend ConcreteSpecControl").replace("mutant `ConcreteSpecControl.mutant`). -/", "mutant `ConcreteSpecControl.mutant`).\nPlanted (law `ConcreteSpecControl.growth2`, case `two`, subject `ConcreteSpecControl.S`, mutant `ConcreteSpecControl.mutant`). -/")
    add("shared-planted-example", shared, set(), status=0)
    from vacuity_controls_round2 import extend
    extend(add, BASE, extra)
    for index, (label, fixture, budget, expected, status) in enumerate(cases):
        if label == "unclassified":
            expected.discard(("FAIL", "evidence-law-kind", "EXAMPLE"))
            lines = fixture.splitlines()
            for line, text in enumerate(lines, 1):
                if text.startswith("example :"):
                    expected.add(("FAIL", "evidence-law-kind", f"ConcreteSpecControl.«⟪example⟫ConcreteSpec.lean:{line}»"))
    for label, _, _, expected, _ in cases:
        if label == "missing-provenance":
            expected.update({
                ("INCOMPLETE", "analysis-budget-or-error", "[anonymous]"),
                ("INCOMPLETE", "analysis-budget-or-error", "ConcreteSpecControl.missingMetadata"),
            })
    for _, _, _, expected, _ in cases:
        if ("FAIL", "unpaired-planted", "EXAMPLE") in expected and any(
            code in {"instance-application", "instance-false-discharge"}
            for status, code, name in expected):
            expected.add(("FAIL", "planted-proposition", "EXAMPLE"))
    # Pending owner question 8: generic unavailable-body analysis, no transfer rule.
    unavailable_laws = {
        "propext-hypothesis": "ConcreteSpecControl.projection",
        "inequality-positive": "ConcreteSpecControl.nonzero",
        "negation-positive": "ConcreteSpecControl.nonzero",
        "partial-negation": "ConcreteSpecControl.negationPartial",
        "negation-alias-positive": "ConcreteSpecControl.nonzero",
        "forall-alias-full-application": "ConcreteSpecControl.nonzero",
    }
    cases = [(label, fixture, budget,
              expected | {("INCOMPLETE", "analysis-budget-or-error", unavailable_laws[label])}, 2)
             if label in unavailable_laws else (label, fixture, budget, expected, status)
             for label, fixture, budget, expected, status in cases]
    if labels is not None:
        cases = [case for case in cases if case[0] in labels]
    failed = 0
    for label, fixture, budget, expected, status in cases:
        options = {}
        if label in {"missing-provenance", "generated-recursor-provenance"}:
            from pathlib import Path
            driver = Path(__file__).with_name("theorems.lean").read_text()
            driver += """
#eval show MetaM Unit from do
  addDecl <| .thmDecl {
    name := `ConcreteSpecControl.missingMetadata
    levelParams := []
    type := mkConst ``True
    value := mkConst ``True.intro
  }
"""
            if label == "generated-recursor-provenance":
                driver = driver.replace("`ConcreteSpecControl.missingMetadata", "((Name.num `_private.ConcreteSpec 0) ++ `PSigma.casesOn.generatedChild)")
                driver = driver.replace("  match env.getModuleIdxFor? n with", "  if privateToUserName n == `PSigma.casesOn.generatedChild then true else\n  match env.getModuleIdxFor? n with", 1)
            options["driver_text"] = driver
        scope = "ConcreteSpec.Owed" if label == "scoped-obligation-only" else ""
        try:
            code, output = run(source, scope, fixture, budget, **options)
        except RuntimeError as error:
            code, output = 2, "INCOMPLETE fixture-elaboration [anonymous] " + str(error)

        found = set()
        identities = {}
        for line in output.splitlines():
            words = line.split()
            if len(words) >= 3 and words[0] in {"FAIL", "INCOMPLETE"}:
                name = "EXAMPLE" if "⟪example⟫" in words[2] else words[2]
                if label == "unclassified" and name == "EXAMPLE":
                    name = words[2]
                key = (words[0], words[1], name)
                identities.setdefault(key, set()).add(words[2])
                found.add(key)
        report_ok = True
        if label == "projection-unavailable":
            report_ok = ("DETAIL analysis-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none projection-body-unavailable ConcreteSpecControl.unavailable" in output)
        if label in unavailable_laws:
            law = unavailable_laws[label]
            report_ok = (f"DETAIL analysis-error {law} stage=projection law={law} case=none projection-body-unavailable propext"
                         in output.splitlines())
        if label == "joint-branch-decide-repro":
            report_ok = ("DETAIL analysis-error ConcreteSpec.Repro.find_absent stage=projection law=ConcreteSpec.Repro.find_absent case=none projection-body-unavailable propext"
                         in output.splitlines())
            report_ok = report_ok and all(any(
                row.startswith(f"NOTE analysis-result ConcreteSpec.Repro.{law} stage=projection ")
                and "result=PASS " in row for row in output.splitlines())
                for law in ["len_pos", "sum_zero"])
        if label == "missing-provenance":
            report_ok = {row for row in output.splitlines() if row.startswith(("FAIL ", "INCOMPLETE "))} == {
                'INCOMPLETE missing-provenance ConcreteSpecControl.missingMetadata ',
                'INCOMPLETE analysis-budget-or-error [anonymous] stage=scope law=none case=none reason=dependency-incomplete heartbeats=0/2000000 work=0/20000 projection=0/4096',
                'INCOMPLETE analysis-budget-or-error ConcreteSpecControl.missingMetadata stage=evidence-tags law=none case=none reason=dependency-incomplete heartbeats=0/2000000 work=0/20000 projection=0/4096',
            }
        if label in {"positive-budget-chain", "projection-long-incomplete"}:
            import re
            records = [row for row in output.splitlines() if row.startswith(("FAIL ", "INCOMPLETE "))]
            report_ok = len(records) == 1 and re.fullmatch(
                rf"INCOMPLETE analysis-budget-or-error ConcreteSpecControl.growth stage=projection law=ConcreteSpecControl.growth case=none reason=projection-budget heartbeats=\d+/2000000 work=0/20000 projection={budget}/4096",
                records[0]) is not None
        if label == "inline-match-changed-branch":
            line = next(i for i, text in enumerate(fixture.splitlines(), 1)
                        if text.startswith("example :")
                        and "case `changed-branch`" in fixture.splitlines()[i - 2])
            exact = (f"FAIL instance-application ConcreteSpec.Repro.«⟪example⟫ConcreteSpec.lean:{line}» "
                     "law=ConcreteSpec.Repro.growth case=changed-branch")
            report_ok = [row for row in output.splitlines() if row.startswith(("FAIL ", "INCOMPLETE "))] == [exact]
        if label == "non-law-positive":
            report_ok = all(row in output for row in [
                "REPORT Helper ( _private.ConcreteSpec.0.ConcreteSpecControl.helper ",
                "REPORT Representation lemma. ConcreteSpecControl.representation ",
                "REPORT Obligation ConcreteSpecControl.owed ",
                "REPORT Obligation ConcreteSpecControl.OwedFields.checked ",
            ])
        if code == status and found == expected and report_ok and all(len(v) == 1 for k, v in identities.items() if k[2] == "EXAMPLE"):
            print("PASS control " + label)
        else:
            failed += 1
            print(f"FAIL control {label}: exit={code}, expected={status}; findings={found}, expected={expected}\n{output}")
    selection = " selected" if labels is not None else ""
    print(f"N2{selection} controls: {len(cases) - failed} PASS, {failed} FAIL")
    return bool(failed)

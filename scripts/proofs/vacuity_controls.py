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


def controls(run, source):
    cases = [("positive", BASE, 64, set(), 0)]

    def add(label, text, expected, budget=64, status=1):
        cases.append((label, text, budget, set(expected), status))

    add("unclassified", BASE.replace("/-- Law (subject `ConcreteSpecControl.S`). -/", ""),
        {("FAIL", "classification-count", "ConcreteSpecControl.growth")})
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
    add("budget", BASE, {("INCOMPLETE", "projection-budget", "ConcreteSpecControl.growth")}, budget=0, status=2)
    add("obligation-theorem", extra("/-- Obligation (discharged at slice 9, R-0219). -/", "theorem owed : True := trivial"),
        {("FAIL", "obligation-kind", "ConcreteSpecControl.owed")})
    add("obligation-row", extra("/-- Obligation (discharged at slice 9, R-999999). -/", "def owed : Prop := True"),
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
    failed = 0
    for label, fixture, budget, expected, status in cases:
        code, output = run(source, "", fixture, budget)
        found = set()
        for line in output.splitlines():
            words = line.split()
            if len(words) >= 3 and words[0] in {"FAIL", "INCOMPLETE"}:
                name = "EXAMPLE" if "⟪example⟫" in words[2] else words[2]
                found.add((words[0], words[1], name))
        report_ok = True
        if label == "non-law-positive":
            report_ok = all(row in output for row in [
                "REPORT Helper ( _private.ConcreteSpec.0.ConcreteSpecControl.helper ",
                "REPORT Representation lemma. ConcreteSpecControl.representation ",
                "REPORT Obligation ConcreteSpecControl.owed ",
                "REPORT Obligation ConcreteSpecControl.OwedFields.checked ",
            ])
        if code == status and found == expected and report_ok:
            print("PASS control " + label)
        else:
            failed += 1
            print(f"FAIL control {label}: exit={code}, expected={status}; findings={found}, expected={expected}\n{output}")
    print(f"N2 controls: {len(cases) - failed} PASS, {failed} FAIL")
    return bool(failed)

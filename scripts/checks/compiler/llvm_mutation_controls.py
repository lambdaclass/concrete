"""Kernel-check concrete counterexamples against real mutated definitions."""

from pathlib import Path
import json
import os
import re
import subprocess
import tempfile
import time


LLVM = Path("Concrete/Backend/LLVM.lean")
IR = Path("Concrete/Backend/LLVMIR.lean")
RUNTIME = Path("Concrete/Backend/Runtime.lean")
# The boundaries follow the executable definitions and independent expectations.
# Keep the proofs outside the copy so a broken tactic cannot affect the witness.
PROOF_SECTION = {
    LLVM: "/-! ## Producer and text laws -/",
    IR: "/-! ## Assembly reader laws -/",
    RUNTIME: "/-! ## Runtime inventory laws -/",
}

RETURN_LOCAL = "(.integer .u8, operandAt 8 a) (.integer .u32)"
SIBLINGS = """⟨[⟨[], [
  .bin .add .u8 0 (.imm .u8 2) (.imm .u8 56),
  .bin .add .u8 1 (.reg 0) (.reg 0)], .ret (.reg 1)⟩], []⟩"""

TOKEN = "def Immediate.token (v : Immediate) : String := decimal v.value"
PIECE = "  | .immediate v before after => String.singleton before.char ++ v.token ++ String.singleton after.char"
MUTATIONS = (
    (
        "equal-width copy zero",
        LLVM,
        "(operandAt (Width.ofCore target).bits a) (.immediate (.natural 0))",
        "(operandAt (Width.ofCore target).bits a) (.immediate (.natural 1))",
        "inst_inventory",
        ".trunc .u8 .i8 0 (.reg 1)",
    ),
    (
        "checked call immediate moved to the local slot",
        LLVM,
        "[(i, operandAt width.bits a), (i, operandAt width.bits b)]",
        "(match a, b with\n"
        "       | .imm .., .reg _ => [(i, operandAt width.bits b), (i, operandAt width.bits a)]\n"
        "       | _, _ => [(i, operandAt width.bits a), (i, operandAt width.bits b)])",
        "inst_inventory",
        ".binOvf .add .u8 0 1 (.imm .u8 17) (.reg 2)",
    ),
    (
        "arithmetic encoded at the immediate tag width",
        LLVM,
        "(operandAt width.bits a) (operandAt width.bits b)",
        "(operand a) (operand b)",
        "inst_inventory",
        ".bin .add .u8 0 (.imm .u16 256) (.imm .u8 1)",
    ),
    (
        "trap result word left wider than i32",
        LLVM,
        "(.natural (toBits .u32 (Int.ofNat (256 + s))))",
        "(.natural (256 + s))",
        "term_inventory",
        ".trap 4294967040",
    ),
    (
        "right immediate moved between sibling additions",
        LLVM,
        "phis p j b ++ b.insts.flatMap inst ++ term j b.term) ++\n  [.endFunction]",
        "phis p j b ++ (match b.insts.flatMap inst with\n"
        "      | .instruction (.bin d .add ty lhs (.immediate value)) ::\n"
        "        .instruction (.bin e .add ty' lhs' (.local source)) :: rest =>\n"
        "        if ty == ty' then\n"
        "          .instruction (.bin d .add ty lhs (.local source)) ::\n"
        "          .instruction (.bin e .add ty' lhs' (.immediate value)) :: rest\n"
        "        else b.insts.flatMap inst\n"
        "      | items => items) ++ term j b.term) ++\n  [.endFunction]",
        "moduleItems_inventory",
        SIBLINGS,
    ),
    (
        "phi predecessor plus one",
        LLVM,
        "(operandAt ((ty pty).integerWidth.getD 1) (args.getD k (.imm .u8 0)), Label.block pred)\n    .instruction",
        "(operandAt ((ty pty).integerWidth.getD 1) (args.getD k (.imm .u8 0)), Label.block (pred + 1))\n    .instruction",
        "phi_inventory",
        "phiProgram 1 phiBlock",
    ),
    (
        "extract indices swapped",
        LLVM,
        "pair (.overflow d) 0),\n     .instruction (.extract (.register f) pair (.overflow d) 1)",
        "pair (.overflow d) 1),\n     .instruction (.extract (.register f) pair (.overflow d) 0)",
        "inst_inventory",
        ".binOvf .add .u8 0 1 (.reg 2) (.reg 3)",
    ),
    (
        "trap result word off by one",
        LLVM,
        "(.natural (toBits .u32 (Int.ofNat (256 + s))))",
        "(.natural (toBits .u32 (Int.ofNat (257 + s))))",
        "term_inventory",
        ".trap 17",
    ),
    (
        "return zero-extension target changed from i32 to i64",
        LLVM,
        RETURN_LOCAL,
        "(.integer .u8, operandAt 8 a) (.integer .u64)",
        "moduleItems_inventory",
        SIBLINGS,
    ),
    (
        "operand encoding plus one",
        LLVM,
        ".immediate (.natural (toBitsAtWidth width v))",
        ".immediate (.natural (toBitsAtWidth width v + 1))",
        "operand_field",
        ".u8 17",
    ),
    (
        "write descriptor changed to 3",
        RUNTIME,
        "[(i32, .immediate (.natural 2)), (.ptr",
        "[(i32, .immediate (.natural 3)), (.ptr",
        "llvmItems_inventory",
        "twoSites",
    ),
    (
        "switch destinations rotated",
        RUNTIME,
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite i)",
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite (i + 1))",
        "llvmItems_inventory",
        "twoSites",
    ),
    (
        "first two switch destinations swapped",
        RUNTIME,
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite i)",
        "(Immediate.natural (toBits .u32 (Int.ofNat i)), Label.trapSite (if i = 0 then 1 else if i = 1 then 0 else i))",
        "llvmItems_inventory",
        "twoSites",
    ),
    (
        "render i32 as i64",
        IR,
        "  | .integer ty => numbered .integer ty.bits",
        "  | .integer .u32 => numbered .integer 64\n  | .integer ty => numbered .integer ty.bits",
        "inventory_render",
        "returnLocal 2",
    ),
    ("%ret prefix becomes 0", IR, '| .result => "%ret"', '| .result => "0"', "inventory_render", "returnLocal 2"),
    (
        "%ret result becomes bare 0",
        IR,
        "| .numbered p n => p.text ++ decimal n",
        '| .numbered .result _ => "0"\n  | .numbered p n => p.text ++ decimal n',
        "inventory_render",
        "returnLocal 0",
    ),
    *(
        (
            f"render immediate {value} as {spelling}",
            IR,
            TOKEN,
            TOKEN.replace(":= decimal v.value", f':= if v.value = {value} then "{spelling}" else decimal v.value'),
            "inventory_render",
            f"returnImmediate {value}",
        )
        for value, spelling in [(0, "zeroinitializer"), (0, "-0"), (17, "1_7")]
    ),
    (
        "drop the space before a negative operand",
        IR,
        PIECE,
        '  | .immediate v before after => if v.value = 17 then "-17" ++ String.singleton after.char\n'
        '      else String.singleton before.char ++ v.token ++ String.singleton after.char',
        "inventory_render",
        "addition (.local (.register 1)) 17",
    ),
    (
        "underscore replaces operand comma",
        IR,
        PIECE,
        '  | .immediate v before .comma => String.singleton before.char ++ v.token ++ "_"\n' + PIECE,
        "inventory_render",
        "addition (.immediate (.natural 2)) 56",
    ),
    (
        "left operand duplicated",
        IR,
        "  | .bin dst op ty lhs rhs =>\n    assign dst ++ (arithmetic op ++ text \" \" ++ "
        "typeCode ty ++ arg lhs .comma ++ arg rhs .newline)",
        "  | .bin dst op ty lhs _ =>\n    assign dst ++ (arithmetic op ++ text \" \" ++ "
        "typeCode ty ++ arg lhs .comma ++ arg lhs .newline)",
        "inventory_render",
        "addition (.immediate (.natural 2)) 56",
    ),
)


# Each pair is the concrete instance of the indicated law. The expected side
# comes from the same independent definition used in that law.
WITNESS = {
    "inst_inventory": ("(inst ({x})).map Item.inventory", "expectedInst ({x})"),
    "moduleItems_inventory": ("(moduleItems ({x})).map Item.inventory", "expectedInventory ({x})"),
    "phi_inventory": (
        "(phis {x}).map Item.inventory",
        "(indexed phiBlock.params).map (fun (k, _, t) => expectedPhi phiProgram 1 k t)",
    ),
    "term_inventory": ("(term 0 ({x})).map Item.inventory", "expectedTerm ({x})"),
    "operand_field": (
        "(operand (.imm {x})).immediateValue", "some (toBits (toCore .u8) 17)",
    ),
    "llvmItems_inventory": ("(llvmItems {x}).map Item.inventory", "expectedInventory {x}"),
    "inventory_render": ("inventory (render ({x}))", "some (({x}).map Item.inventory)"),
}

WITNESS_DEFINITIONS = {
    "phi_inventory": """
private def phiBlock : Block := ⟨[(0, .int .u8)], [], .ret (.reg 0)⟩
private def phiProgram : Program := ⟨[⟨[], [], .br 1 [.imm .u8 17]⟩, phiBlock], []⟩
""",
    "llvmItems_inventory": """
private def twoSites : SiteTable :=
  [⟨.arithmeticOverflow, ⟨0⟩, ⟨0, 1⟩, ⟨"a", 1, 1⟩⟩,
   ⟨.arithmeticOverflow, ⟨1⟩, ⟨1, 2⟩, ⟨"b", 1, 1⟩⟩]
""",
    "inventory_render": """
private def returnLocal (n : Nat) : List Item :=
  [.terminator (.ret (.integer .u32, .local (.result n)))]
private def returnImmediate (n : Nat) : List Item :=
  [.terminator (.ret (.integer .u32, .immediate (.natural n)))]
private def addition (a : Argument) (b : Nat) : List Item :=
  [.instruction (.bin (.register 0) .add (.integer .u8) a (.immediate (.natural b)))]
""",
}

# State each concrete spelling and its single parsed item independently of
# the mutated renderer. Split rendering, parsing and comparison so the kernel
# only reduces one small step at a time.
RENDERED = {
    "render i32 as i64": ("  ret i64 %ret2\n", "some (.terminator (.ret (.integer .u64, .local (.result 2))), [])"),
    "%ret prefix becomes 0": ("  ret i32 02\n", "none"),
    "%ret result becomes bare 0": ("  ret i32 0\n", "some (.terminator (.ret (.integer .u32, .immediate (.natural 0))), [])"),
    "render immediate 0 as zeroinitializer": ("  ret i32 zeroinitializer\n", "none"),
    "render immediate 0 as -0": ("  ret i32 -0\n", "none"),
    "render immediate 17 as 1_7": ("  ret i32 1_7\n", "none"),
    "drop the space before a negative operand": ("  %r0 = add i8 %r1,-17\n", "none"),
    "underscore replaces operand comma": ("  %r0 = add i8 2_ 56\n", "none"),
    "left operand duplicated": ("  %r0 = add i8 2, 2\n", "some (.instruction (.bin (.register 0) .add (.integer .u8) (.immediate (.natural 2)) (.immediate (.natural 2))), [])"),
}


def renderer_proof(expression, witness, rendered, parsed):
    left, right = expression
    # No theorem about the unmutated renderer participates in these equalities.
    spelling = json.dumps(rendered)
    chars = "[" + ", ".join(" '" + json.dumps(c)[1:-1] + "'" for c in rendered[1:]) + "]"
    return f"""set_option linter.unusedSimpArgs false in
theorem witness : ({left}) ≠ ({right}) := by
  have spelling : (render ({witness})).toList = {spelling}.toList := by
    simp [render, renderPieces, Item.pieces, terminatorPieces, instructionPieces,
      LLVM.assign, LLVM.typed, LLVM.typeCode, LLVM.arg, LLVM.localName, LLVM.arithmetic,
      framed, numbered, text, imm, Piece.render, Opening.char, Separator.char, NumberPrefix.text,
      Immediate.token, Immediate.natural, Width.bits, decimal] <;> rfl
  have parsed : item (' ' :: {chars}) = {parsed} := by rfl
  have observed : inventory {spelling} =
      (match ({parsed} : Option (Item × List Char)) with
       | none => none
       | some (i, _) => some [Item.inventory i]) := by
    unfold inventory parse
    rw [show {spelling}.toList = ' ' :: {chars} from rfl, List.length_cons, moduleItems.eq_3]
    change ((item (' ' :: {chars})).bind _).map _ = _
    rw [parsed]
    rfl
  intro unchanged
  rw [String.toList_injective spelling, observed] at unchanged
  cases unchanged"""


# Project the differing row before reduction so a proof does not have to
# decide equality across a whole runtime inventory.
WITNESS_ROWS = {
    "right immediate moved between sibling additions": 3,
    "return zero-extension target changed from i32 to i64": 5,
    "write descriptor changed to 3": 16,
    "switch destinations rotated": 36,
    "first two switch destinations swapped": 36,
}

BENIGN = (
    "double reversal of BIR instructions",
    LLVM,
    "phis p j b ++ b.insts.flatMap inst ++ term j b.term) ++\n  [.endFunction]",
    "phis p j b ++ b.insts.reverse.reverse.flatMap inst ++ term j b.term) ++\n  [.endFunction]",
    "moduleItems_inventory",
    SIBLINGS,
)


def elaborate(mutant, prefix, theorem, expression, row=None, equal=False, render_case=None):
    left, right = expression
    if equal:
        proof = f"""theorem witness : ({left}) = ({right}) := by
  simp only [moduleItems, body, List.reverse_reverse, List.map_append, Runtime.llvmItems_inventory]
  rfl"""
    elif render_case is not None:
        proof = renderer_proof(expression, *render_case)
    elif row is None:
        proof = f"theorem witness : ({left}) ≠ ({right}) := by decide"
    else:
        proof = f"""theorem witness : ({left}) ≠ ({right}) := by
  intro h
  have different : ({left})[{row}]? ≠ ({right})[{row}]? := by decide
  exact different (congrArg (fun rows => rows[{row}]?) h)"""
    mutant.write_text(
        prefix + "\n" + WITNESS_DEFINITIONS.get(theorem, "")
        + "\n" + proof + "\n#print axioms witness\n",
        encoding="utf-8",
    )
    result = subprocess.run(
        ["lake", "env", "lean", "-DwarningAsError=true",
         "-j", os.environ.get("LEAN_NUM_THREADS", "2"), "-M", "2048", str(mutant)],
        capture_output=True,
        text=True,
        check=False,
    )
    output = result.stdout + result.stderr
    report = re.search(
        r"^'[\w.]+[.]witness' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])$",
        output,
        re.MULTILINE,
    )
    standard_axioms = {"propext", "Classical.choice", "Quot.sound"}
    axioms = set(report[2].split(", ")) if report and report[2] else set()
    return result.returncode == 0 and report is not None and axioms <= standard_axioms, output


def check_mutation(directory, description, path, before, after, theorem, witness, *, benign=False):
    source = path.read_text(encoding="utf-8")
    prefix, boundary, _ = source.partition(PROOF_SECTION[path])
    if not boundary or prefix.count(before) != 1 or source.count(before) != 1:
        raise SystemExit(f"{description}: mutation must match exactly one definition site")
    prefix = prefix.replace(before, after)
    expression = tuple(side.format(x=witness) for side in WITNESS[theorem])
    mutant = directory / path.name
    start = time.monotonic()
    row = 3 if benign else WITNESS_ROWS.get(description)
    render_case = (witness, *RENDERED[description]) if theorem == "inventory_render" else None
    killed, output = elaborate(mutant, prefix, theorem, expression, row, render_case=render_case)
    if benign:
        if killed:
            raise SystemExit(f"{description}: checker accepted a counterexample to the benign edit")
        equal, output = elaborate(mutant, prefix, theorem, expression, equal=True)
        if not equal:
            raise SystemExit(f"{description}: expected a kernel-checked equality\n{output}")
        verdict = "survived with a kernel-checked equality"
    elif killed:
        verdict = f"refutes {theorem} on the concrete witness"
    else:
        raise SystemExit(f"{description}: no kernel-checked counterexample to {theorem}\n{output}")
    print(f"ok   {description}: {verdict} ({time.monotonic() - start:.2f}s)", flush=True)


if __name__ == "__main__":
    with tempfile.TemporaryDirectory() as scratch:
        for mutation in MUTATIONS:
            check_mutation(Path(scratch), *mutation)
        check_mutation(Path(scratch), *BENIGN, benign=True)

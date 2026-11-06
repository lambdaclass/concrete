import ConcreteSpec.Spec.Semantics

/-!
# BIR: the backend IR, the contract both backends read

PROVISIONAL (slice 0; see `Concrete/README.md`, "BIR is a complete backend contract").

BIR is the one boundary that the LLVM printer and the LCB adapter read. It is block-argument SSA over
the spec's integer types (`ConcreteSpec.IntTy`, all eight widths and signednesses, so that the
integer slices extend it without reshaping) and `bool` flags. Integer values are mathematical
integers inside their type's range (`IntTy.inRange`), as in the spec; each backend chooses its bit
representation.

* `bin op t d a b` is the wrapping operation (the result reduced into `t`'s range);
* `binOvf op t d f a b` also sets the flag `f` exactly when the mathematical result is outside `t`;
* `trap s` ends the run at trap site `s`, an index into the program's site table, which gives the
  trap's kind, its typed source site and its span.

A run ends by returning a `u8` from `main`'s body, by trapping, or by getting stuck (only possible
for a program that is not well formed). `Behaviour` is the fuel-free meaning; `encode`/`decode` is
the result word every backend's body returns, decoded by the runtime entry.
-/

namespace Concrete.BIR
open ConcreteSpec (IntTy Trap)

/-! ## Sites -/

/-- A typed source-site identity, minted by the frontend. -/
structure SiteId where
  id : Nat
  deriving DecidableEq, Repr

/-- A byte span in the source. -/
structure Span where
  start : Nat
  stop : Nat
  deriving DecidableEq, Repr

/-- One trap site: what traps there, where it came from. -/
structure SiteEntry where
  kind : Trap
  site : SiteId
  span : Span
  deriving DecidableEq, Repr

abbrev SiteTable := List SiteEntry

/-! ## Syntax -/

/-- BIR value types. -/
inductive Ty where
  | int (t : IntTy)
  | bool
  deriving DecidableEq, Repr

abbrev Reg := Nat

/-- An operand: a register, or an integer immediate of a type. -/
inductive Operand where
  | reg (r : Reg)
  | imm (t : IntTy) (v : Int)
  deriving DecidableEq, Repr

/-- The arithmetic operations. Slice 0 uses `add`; the others are here so the integer slices
extend BIR without reshaping it. -/
inductive BinOp where
  | add | sub | mul
  deriving DecidableEq, Repr

inductive Inst where
  /-- `d := wrap_t (a op b)`. Emitted only where a discharged-evidence token shows the result is
  in range, or where the source asks for wrapping. -/
  | bin (op : BinOp) (t : IntTy) (d : Reg) (a b : Operand)
  /-- `d := wrap_t (a op b)` and `f := (a op b) ∉ t`. -/
  | binOvf (op : BinOp) (t : IntTy) (d f : Reg) (a b : Operand)
  deriving DecidableEq, Repr

inductive Term where
  /-- Return the `u8` operand from `main`'s body. -/
  | ret (a : Operand)
  | br (target : Nat) (args : List Operand)
  | condbr (c : Reg) (yes : Nat) (yesArgs : List Operand) (no : Nat) (noArgs : List Operand)
  /-- Trap at site `s` (an index into the program's site table). -/
  | trap (s : Nat)
  deriving DecidableEq, Repr

structure Block where
  params : List (Reg × Ty)
  insts : List Inst
  term : Term
  deriving DecidableEq, Repr

/-- `main`'s body: blocks (block 0 is the entry) and the trap-site table. -/
structure Program where
  blocks : List Block
  sites : SiteTable
  deriving DecidableEq, Repr

/-! ## Values and operations -/

inductive Val where
  | int (t : IntTy) (v : Int)
  | bool (b : Bool)
  deriving DecidableEq, Repr

/-- The mathematical result of an operation. -/
def BinOp.exact : BinOp → Int → Int → Int
  | .add, a, b => a + b
  | .sub, a, b => a - b
  | .mul, a, b => a * b

/-- `n` reduced into `t`'s range, modulo `2^bits` (two's complement for a signed type). -/
def wrap (t : IntTy) (n : Int) : Int := (n - t.min) % ((2 : Int) ^ t.bits) + t.min

abbrev Env := List (Reg × Val)

def lookup (env : Env) (r : Reg) : Option Val :=
  match env with
  | [] => none
  | (r', v) :: rest => if r' = r then some v else lookup rest r

def evalOp (env : Env) : Operand → Option Val
  | .reg r => lookup env r
  | .imm t v => if t.inRange v then some (.int t v) else none

/-- Two integer operands of type `t`. -/
def intArgs (env : Env) (t : IntTy) (a b : Operand) : Option (Int × Int) :=
  match evalOp env a, evalOp env b with
  | some (.int ta x), some (.int tb y) => if ta = t ∧ tb = t then some (x, y) else none
  | _, _ => none

def execInst (env : Env) : Inst → Option Env
  | .bin op t d a b => do
    let (x, y) ← intArgs env t a b
    pure ((d, .int t (wrap t (op.exact x y))) :: env)
  | .binOvf op t d f a b => do
    let (x, y) ← intArgs env t a b
    let r := op.exact x y
    pure ((f, .bool (!t.inRange r)) :: (d, .int t (wrap t r)) :: env)

def execInsts (env : Env) : List Inst → Option Env
  | [] => some env
  | i :: is => match execInst env i with
    | some env' => execInsts env' is
    | none => none

/-- Enter block `target` with arguments evaluated in `env` (all read before any is bound). -/
def enter (p : Program) (target : Nat) (args : List Operand) (env : Env) : Option (Block × Env) := do
  let blk ← p.blocks[target]?
  if blk.params.length ≠ args.length then none
  let vals ← args.mapM (evalOp env)
  pure (blk, (blk.params.map Prod.fst).zip vals ++ env)

/-! ## Semantics -/

/-- How a run ends, or that the fuel ran out. -/
inductive Outcome where
  | exited (c : Nat)
  | trapped (s : Nat)
  | stuck
  | outOfFuel
  deriving DecidableEq, Repr

def Outcome.final : Outcome → Bool
  | .outOfFuel => false
  | _ => true

/-- Run block `blk` and what follows, with `fuel` block executions. -/
def runBlock (p : Program) : Nat → Block → Env → Outcome
  | 0, _, _ => .outOfFuel
  | fuel + 1, blk, env =>
    match execInsts env blk.insts with
    | none => .stuck
    | some env' =>
      match blk.term with
      | .ret a => match evalOp env' a with
        | some (.int .u8 c) => .exited c.toNat
        | _ => .stuck
      | .trap s => if s < p.sites.length then .trapped s else .stuck
      | .br t args => match enter p t args env' with
        | some (blk', env'') => runBlock p fuel blk' env''
        | none => .stuck
      | .condbr c t ta e ea => match lookup env' c with
        | some (.bool true) => match enter p t ta env' with
          | some (blk', env'') => runBlock p fuel blk' env''
          | none => .stuck
        | some (.bool false) => match enter p e ea env' with
          | some (blk', env'') => runBlock p fuel blk' env''
          | none => .stuck
        | _ => .stuck

/-- Run `main`'s body with `fuel` block executions. -/
def run (p : Program) (fuel : Nat) : Outcome :=
  match p.blocks[0]? with
  | some blk => runBlock p fuel blk []
  | none => .stuck

/-- More fuel never changes a final outcome. -/
theorem runBlock_mono (p : Program) :
    ∀ (n k : Nat) (blk : Block) (env : Env), (runBlock p n blk env).final = true →
      runBlock p (n + k) blk env = runBlock p n blk env := by
  intro n
  induction n with
  | zero => intro k blk env h; simp [runBlock, Outcome.final] at h
  | succ n ih =>
    intro k blk env h
    rw [show n + 1 + k = (n + k) + 1 by omega]
    simp only [runBlock] at h ⊢
    split
    · rfl
    · rename_i env' _
      split
      · rfl
      · rfl
      · split
        · rename_i blk' env'' _
          simp_all only []
        · rfl
      · split
        · split
          · rename_i blk' env'' _
            simp_all only []
          · rfl
        · split
          · rename_i blk' env'' _
            simp_all only []
          · rfl
        · rfl

theorem run_mono {p : Program} {n m : Nat} (h : (run p n).final = true) (hm : n ≤ m) :
    run p m = run p n := by
  obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le hm
  unfold run at h ⊢
  split
  · rename_i blk _
    simp_all only []
    exact runBlock_mono p n k blk [] h
  · rfl

/-- Two final outcomes of the same program agree. -/
theorem run_final_unique {p : Program} {n m : Nat}
    (hn : (run p n).final = true) (hm : (run p m).final = true) : run p n = run p m := by
  rw [← run_mono hn (Nat.le_max_left n m), ← run_mono hm (Nat.le_max_right n m)]

/-! ## Behaviour: the fuel-free meaning -/

/-- A run's behaviour as the parent process sees it, up to the trap's kind. `refused` (a startup
refusal) and `killed` arise only for native runs. -/
inductive Behaviour where
  | exited (c : Nat)
  | trapped (k : Trap)
  | diverges
  | stuck
  | refused
  | killed
  deriving DecidableEq, Repr

def Outcome.toBehaviour (tbl : SiteTable) : Outcome → Behaviour
  | .exited c => .exited c
  | .trapped s => match tbl[s]? with
    | some e => .trapped e.kind
    | none => .stuck
  | .stuck => .stuck
  | .outOfFuel => .diverges

open Classical in
/-- The behaviour: the final outcome if some fuel reaches one, otherwise divergence. -/
noncomputable def behaviour (p : Program) : Behaviour :=
  if h : ∃ n, (run p n).final = true then (run p (Classical.choose h)).toBehaviour p.sites
  else .diverges

theorem behaviour_of_run {p : Program} {n : Nat} (h : (run p n).final = true) :
    behaviour p = (run p n).toBehaviour p.sites := by
  have hex : ∃ n, (run p n).final = true := ⟨n, h⟩
  unfold behaviour
  split
  · rename_i h'
    rw [run_final_unique (Classical.choose_spec h') h]
  · exact absurd hex ‹_›

/-! ## The result word

Every backend emits `main`'s body as a function returning a 32-bit word that encodes how the run
ended; one runtime entry decodes it (`Concrete/Backend/Runtime.lean`). Exiting with `c` returns
`c`; trapping at site `s` returns `256 + s`. The site table gives the trap's kind and site, so a
trap is never confused with `return 134`. -/

/-- The largest site table the encoding supports. -/
def maxSites : Nat := 2 ^ 31 - 256

/-- The word a final outcome encodes to. -/
def encode : Outcome → Option Nat
  | .exited c => some c
  | .trapped s => some (256 + s)
  | _ => none

/-- An outcome the encoding represents: an exit status below 256, or a trap at a site of the
table. -/
def Outcome.Encodable (tbl : SiteTable) : Outcome → Prop
  | .exited c => c < 256
  | .trapped s => s < tbl.length ∧ tbl.length ≤ maxSites
  | _ => False

/-- The runtime's reading of a result word. A word that encodes nothing is `stuck`. -/
def decode (tbl : SiteTable) (w : BitVec 32) : Behaviour :=
  if w.toNat < 256 then .exited w.toNat
  else match tbl[w.toNat - 256]? with
    | some e => .trapped e.kind
    | none => .stuck

/-- The round trip: decoding the word an outcome encodes to gives the outcome's behaviour. -/
theorem decode_encode (tbl : SiteTable) (o : Outcome) (n : Nat) (he : encode o = some n)
    (hv : o.Encodable tbl) : decode tbl (BitVec.ofNat 32 n) = o.toBehaviour tbl := by
  cases o with
  | exited c =>
    simp only [encode, Option.some.injEq] at he
    subst he
    simp only [Outcome.Encodable] at hv
    have h32 : c < 2 ^ 32 := by omega
    simp [decode, Outcome.toBehaviour, BitVec.toNat_ofNat, Nat.mod_eq_of_lt h32, hv]
  | trapped s =>
    simp only [encode, Option.some.injEq] at he
    subst he
    simp only [Outcome.Encodable, maxSites] at hv
    have h32 : 256 + s < 2 ^ 32 := by omega
    have hn : ¬ 256 + s < 256 := by omega
    simp [decode, Outcome.toBehaviour, BitVec.toNat_ofNat, Nat.mod_eq_of_lt h32, hn]
  | stuck => simp [encode] at he
  | outOfFuel => simp [encode] at he

/-- What a backend's body function produces. -/
inductive BodyResult where
  | returns (w : BitVec 32)
  | diverges
  | stuck
  deriving DecidableEq, Repr

def BodyResult.behaviour (tbl : SiteTable) : BodyResult → Behaviour
  | .returns w => decode tbl w
  | .diverges => .diverges
  | .stuck => .stuck

/-! ## The overflow-checked operations -/

/-- `binOvf` computes the wrapped result and flags exactly the out-of-range results, for every
operation, every integer type and all in-range operands: the check is total, not an
approximation. -/
theorem binOvf_spec (env : Env) (op : BinOp) (t : IntTy) (d f : Reg) (a b : Operand) (x y : Int)
    (h : intArgs env t a b = some (x, y)) :
    execInst env (.binOvf op t d f a b) =
      some ((f, .bool (!t.inRange (op.exact x y))) :: (d, .int t (wrap t (op.exact x y))) :: env) := by
  simp [execInst, h]

/-- Wrapping leaves an in-range result unchanged. -/
theorem wrap_of_inRange (t : IntTy) (n : Int) (h : t.inRange n = true) : wrap t n = n := by
  cases t <;>
    simp_all [wrap, IntTy.inRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] <;>
    omega

/-- The wrapped result is always in range. -/
theorem wrap_inRange (t : IntTy) (n : Int) : t.inRange (wrap t n) = true := by
  cases t <;>
    simp [wrap, IntTy.inRange, IntTy.min, IntTy.max, IntTy.signed, IntTy.bits] <;>
    omega

/-! ## Well-formedness

A decidable check of the program's shape: types, arities, targets, sites and single definition of
every register. It does not yet check that every use is dominated by its definition (a follow-up),
so a well-formed program can still get stuck; the backends then get stuck too. -/

def lookupTy (types : List (Reg × Ty)) (r : Reg) : Option Ty :=
  match types with
  | [] => none
  | (r', t) :: rest => if r' = r then some t else lookupTy rest r

def Inst.defs : Inst → List (Reg × Ty)
  | .bin _ t d _ _ => [(d, .int t)]
  | .binOvf _ t d f _ _ => [(d, .int t), (f, .bool)]

def Block.defs (b : Block) : List (Reg × Ty) := b.params ++ b.insts.flatMap Inst.defs

/-- Every register the program defines, with its type. -/
def Program.defs (p : Program) : List (Reg × Ty) := p.blocks.flatMap Block.defs

/-- The frame-accounting handoff: how many registers the body defines. Native stack bytes never
come from this number (they come from the final artifact and the launcher's reservation). -/
def Program.frameSlots (p : Program) : Nat := p.defs.length

def operandHas (types : List (Reg × Ty)) (ty : Ty) : Operand → Bool
  | .reg r => lookupTy types r == some ty
  | .imm t v => ty == .int t && t.inRange v

def Inst.wf (types : List (Reg × Ty)) : Inst → Bool
  | .bin _ t _ a b => operandHas types (.int t) a && operandHas types (.int t) b
  | .binOvf _ t _ _ a b => operandHas types (.int t) a && operandHas types (.int t) b

/-- A branch to `target` with `args`: the block exists, is not the entry, and the arguments
match its parameters' types. -/
def edgeWf (p : Program) (types : List (Reg × Ty)) (target : Nat) (args : List Operand) : Bool :=
  target != 0 &&
  match p.blocks[target]? with
  | none => false
  | some blk => blk.params.length == args.length &&
      (blk.params.zip args).all (fun (pr, a) => operandHas types pr.2 a)

def Term.wf (p : Program) (types : List (Reg × Ty)) : Term → Bool
  | .ret a => operandHas types (.int .u8) a
  | .br t args => edgeWf p types t args
  | .condbr c t ta e ea =>
    lookupTy types c == some .bool && t != e && edgeWf p types t ta && edgeWf p types e ea
  | .trap s => s < p.sites.length

def wf (p : Program) : Bool :=
  let types := p.defs
  let regs := types.map Prod.fst
  (match p.blocks[0]? with | some b => b.params.isEmpty | none => false) &&
  regs.eraseDups.length == regs.length &&
  p.sites.length ≤ maxSites &&
  p.blocks.all (fun b => b.insts.all (Inst.wf types) && b.term.wf p types)

/-- A well-formed BIR program. -/
def WF (p : Program) : Prop := wf p = true

instance (p : Program) : Decidable (WF p) := inferInstanceAs (Decidable (wf p = true))

/-! ## Hand-written examples (slice 0) -/

namespace Examples

/-- The one trap site of the examples: the checked `+`. -/
def plusSite : SiteEntry := ⟨.arithmeticOverflow, ⟨0⟩, ⟨59, 64⟩⟩

/-- `a + b` with the overflow check: `binOvf`, then branch to the trap site or return. -/
def checkedSum (a b : Int) : Program :=
  { blocks := [
      { params := [], insts := [.binOvf .add .u8 0 1 (.imm .u8 a) (.imm .u8 b)],
        term := .condbr 1 1 [] 2 [.reg 0] },
      { params := [], insts := [], term := .trap 0 },
      { params := [(2, .int .u8)], insts := [], term := .ret (.reg 2) }],
    sites := [plusSite] }

/-- `20 + 22` exits with 42. -/
def answer : Program := checkedSum 20 22
/-- `200 + 55` exits with 255, the boundary. -/
def boundary : Program := checkedSum 200 55
/-- `200 + 56` traps at the `+` (as BIR: the checker refuses this source, but BIR must still trap). -/
def overflow : Program := checkedSum 200 56

/-- The discharged form: a `+` whose evidence token shows it cannot overflow has no check. -/
def answerDischarged : Program :=
  { blocks := [
      { params := [], insts := [.bin .add .u8 0 (.imm .u8 20) (.imm .u8 22)],
        term := .ret (.reg 0) }],
    sites := [] }

/-- `return 134` exits with 134, which the result word keeps apart from a trap. -/
def return134 : Program :=
  { blocks := [{ params := [], insts := [], term := .ret (.imm .u8 134) }], sites := [] }

example : WF answer := by decide
example : WF overflow := by decide
example : WF answerDischarged := by decide
example : WF return134 := by decide
example : run answer 3 = .exited 42 := by decide
example : run boundary 3 = .exited 255 := by decide
example : run overflow 3 = .trapped 0 := by decide
example : run answerDischarged 1 = .exited 42 := by decide
example : run return134 1 = .exited 134 := by decide
example : encode (.trapped 0) = some 256 := rfl

/-- `a op b` at type `t` with the overflow check, exiting 0 when it is in range: exercises the
other widths, signedness and operations BIR already has. -/
def checkedOp (op : BinOp) (t : IntTy) (a b : Int) : Program :=
  { blocks := [
      { params := [], insts := [.binOvf op t 0 1 (.imm t a) (.imm t b)],
        term := .condbr 1 1 [] 2 [] },
      { params := [], insts := [], term := .trap 0 },
      { params := [], insts := [], term := .ret (.imm .u8 0) }],
    sites := [plusSite] }

example : run (checkedOp .add .i8 (-100) (-28)) 3 = .exited 0 := by decide
example : run (checkedOp .add .i8 (-100) (-29)) 3 = .trapped 0 := by decide

/-- The examples, by name, with the outcome each must show (`scripts/checks/slice0_backends.sh`). -/
def all : List (String × Program × Outcome) :=
  [("answer", answer, .exited 42), ("boundary", boundary, .exited 255),
   ("overflow", overflow, .trapped 0), ("answer_discharged", answerDischarged, .exited 42),
   ("return134", return134, .exited 134),
   ("i8_add_ok", checkedOp .add .i8 (-100) (-28), .exited 0),
   ("i8_add_ovf", checkedOp .add .i8 (-100) (-29), .trapped 0),
   ("u16_mul_ok", checkedOp .mul .u16 300 218, .exited 0),
   ("u16_mul_ovf", checkedOp .mul .u16 300 219, .trapped 0),
   ("i16_sub_ovf", checkedOp .sub .i16 (-32768) 1, .trapped 0),
   ("u8_sub_ovf", checkedOp .sub .u8 0 1, .trapped 0),
   ("i8_mul_ok", checkedOp .mul .i8 (-16) 8, .exited 0),
   ("i8_mul_ovf", checkedOp .mul .i8 (-16) (-8), .trapped 0)]

end Examples

end Concrete.BIR

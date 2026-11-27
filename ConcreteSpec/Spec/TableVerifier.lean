import ConcreteSpec.StageGrammar

/-! Certificate checker for an erased CFG. All input addresses are static. Each
positive Horn fact carries a height. The checker tests every implication for
closure and requires a strictly shorter justification for every positive head.
Suffix facts share work between adjacent positions of the same production. -/

namespace ConcreteSpec.GrammarVerification
open Concrete.LL1 ConcreteSpec.StageGrammar

structure RawGrammar where
  nonterminals : Nat
  terminals : Nat
  cfg : Grammar Nat Nat

def RawGrammar.columns (g : RawGrammar) := g.terminals + 1

/-- A position exists at each symbol and at the end of every RHS. -/
def RawGrammar.positions (g : RawGrammar) : Nat :=
  (g.cfg.prods.map fun p => p.2.length + 1).sum

def RawGrammar.budget (g : RawGrammar) := 256 * (g.positions + 1) * g.columns

def RawGrammar.valid (g : RawGrammar) : Bool :=
  g.cfg.start < g.nonterminals && g.nonterminals ≤ g.positions &&
  g.cfg.prods.all fun (lhs, rhs) => lhs < g.nonterminals && rhs.all fun
    | .t t => t < g.terminals
    | .n n => n < g.nonterminals

/-- Row cells, then the rank used by the cursor, then nonterminal and suffix
fact ranks. Column zero denotes physical EOF. -/
structure Layout where
  n : Nat
  c : Nat
  positions : Nat

def RawGrammar.layout (g : RawGrammar) : Layout := ⟨g.nonterminals, g.columns, g.positions⟩

namespace Layout
def cell (l : Layout) (n col : Nat) := n*l.c + col
def rank (l : Layout) (n : Nat) := l.n*l.c + n
def ntBase (l : Layout) := l.rank l.n
def nullNt (l : Layout) (n : Nat) := l.ntBase + n
def productive (l : Layout) (n : Nat) := l.nullNt l.n + n
def firstNt (l : Layout) (n col : Nat) := l.productive l.n + n*l.c + col
def follow (l : Layout) (n col : Nat) := l.firstNt l.n 0 + n*l.c + col
def prefixNull (l : Layout) (p : Nat) := l.follow l.n 0 + p
def suffixNull (l : Layout) (p : Nat) := l.prefixNull l.positions + p
def suffixProductive (l : Layout) (p : Nat) := l.suffixNull l.positions + p
def suffixFirst (l : Layout) (p col : Nat) := l.suffixProductive l.positions + p*l.c + col
def scratch (l : Layout) := l.suffixFirst l.positions 0
end Layout

/-- A fact is represented by a nonzero height. Horn rules have at most two
premises. Even mutually recursive, unproductive cycles have a least model. -/
inductive Fact where
  | nullable (n : Nat)
  | productive (n : Nat)
  | first (n col : Nat)
  | follow (n col : Nat)
  | prefixNull (pos : Nat)
  | suffixNull (pos : Nat)
  | suffixProductive (pos : Nat)
  | suffixFirst (pos col : Nat)
  deriving DecidableEq, Repr

def Fact.address (l : Layout) : Fact → Nat
  | .nullable n => l.nullNt n
  | .productive n => l.productive n
  | .first n c => l.firstNt n c
  | .follow n c => l.follow n c
  | .prefixNull p => l.prefixNull p
  | .suffixNull p => l.suffixNull p
  | .suffixProductive p => l.suffixProductive p
  | .suffixFirst p c => l.suffixFirst p c

def Fact.valid (l : Layout) : Fact → Bool
  | .nullable n | .productive n => n < l.n
  | .first n c | .follow n c => n < l.n && c < l.c
  | .prefixNull p | .suffixNull p | .suffixProductive p => p < l.positions
  | .suffixFirst p c => p < l.positions && c < l.c

structure Rule (α : Type) where
  head : α
  premises : List α

abbrev Clause := Rule Nat

def Rule.map {α β : Type} (f : α → β) (r : Rule α) : Rule β :=
  ⟨f r.head, r.premises.map f⟩

/-- Least consequence of the indexed deductions, independently of ranks and memory. -/
inductive Provable {α : Type} (rules : List (Rule α)) : α → Prop
  | step (r : Rule α) : r ∈ rules → (∀ p ∈ r.premises, Provable rules p) → Provable rules r.head

def Closed (rules : List Clause) (height : Nat → Nat) : Prop :=
  ∀ r ∈ rules, (∀ p ∈ r.premises, 0 < height p) → 0 < height r.head

def Justified (rules : List Clause) (height : Nat → Nat) : Prop :=
  ∀ a, 0 < height a → ∃ r ∈ rules, r.head = a ∧
    ∀ p ∈ r.premises, 0 < height p ∧ height p < height a

inductive Test where
  | literal (value : Bool)
  | word (address : Nat) (value : UInt64)
  | less (left right : Nat)
  | neg (p : Test)
  | both (p q : Test)
  | either (p q : Test)
  deriving Inhabited

namespace Test
def all (tests : List Test) : Test :=
  match tests with
  | [] => .literal true
  | p :: ps => ps.foldl both p

def any (tests : List Test) : Test :=
  match tests with
  | [] => .literal false
  | p :: ps => ps.foldl either p

def implies (p q : Test) : Test := .either (.neg p) q

def positive (a : Nat) : Test := .neg (.word a 0)

def eval (read : Nat → UInt64) : Test → Bool
  | .literal b => b
  | .word a v => read a == v
  | .less a b => read a < read b
  | .neg p => !(eval read p)
  | .both p q => eval read p && eval read q
  | .either p q => eval read p || eval read q

/-- All jumps are forward. A comparison with a constant initializes the sole
scratch register before reading it. Control flow is independent of the initial scratch word. -/
def width : Test → Nat
  | .literal _ | .less .. => 1
  | .word .. => 2
  | .neg p => p.width
  | .both p q | .either p q => p.width + q.width

def code (scratch pc yes no : Nat) : Test → List VerifyInstruction
  | .literal b => [.ifEqual scratch scratch (if b then yes else no) (if b then yes else no)]
  | .word a v => [.constant scratch v, .ifEqual a scratch yes no]
  | .less a b => [.ifLess a b yes no]
  | .neg p => p.code scratch pc no yes
  | .both p q =>
    let mid := pc + p.width
    p.code scratch pc mid no ++ q.code scratch mid yes no
  | .either p q =>
    let mid := pc + p.width
    p.code scratch pc yes mid ++ q.code scratch mid yes no

def inBounds (limit : Nat) : Test → Bool
  | .literal _ => true
  | .word a _ => a < limit
  | .less a b => a < limit && b < limit
  | .neg p => p.inBounds limit
  | .both p q | .either p q => p.inBounds limit && q.inBounds limit
end Test

/-- Production occurrences retain a separate index even when two rows are equal. -/
structure Row where
  index : Nat
  base : Nat
  lhs : Nat
  rhs : List (Sym Nat Nat)
  deriving DecidableEq

def indexRows (index base : Nat) : List (Nat × List (Sym Nat Nat)) → List Row
  | [] => []
  | (lhs,rhs) :: rest =>
    ⟨index,base,lhs,rhs⟩ :: indexRows (index+1) (base+rhs.length+1) rest

def RawGrammar.rows (g : RawGrammar) := indexRows 0 0 g.cfg.prods

/-- A finite expansion exists, with any terminal yield. -/
inductive Productive (cfg : Grammar Nat Nat) : Nat → Prop
  | row {n rhs} : (n, rhs) ∈ cfg.prods →
      (∀ child, Sym.n child ∈ rhs → Productive cfg child) → Productive cfg n

def ProductiveSeq (cfg : Grammar Nat Nat) (rhs : List (Sym Nat Nat)) : Prop :=
  ∀ child, Sym.n child ∈ rhs → Productive cfg child

/-- Independent interpretation of an occurrence address in the original CFG. -/
def positionParts (base pos : Nat) : List (Nat × List (Sym Nat Nat)) →
    List (Sym Nat Nat) × List (Sym Nat Nat)
  | [] => ([],[])
  | (_,rhs) :: rs =>
    if pos < base + rhs.length + 1 then (rhs.take (pos-base), rhs.drop (pos-base))
    else positionParts (base+rhs.length+1) pos rs

def lookahead : Nat → Option Nat
  | 0 => none
  | n+1 => some n

def Fact.Meaning (g : RawGrammar) (f : Fact) : Prop :=
  let parts := fun p => positionParts 0 p g.cfg.prods
  match f with
  | .nullable n => Nullable g.cfg n
  | .productive n => Productive g.cfg n
  | .first n c => ∃ t, c = t+1 ∧ First g.cfg n t
  | .follow n c => Follow g.cfg n (lookahead c)
  | .prefixNull p => NullableSeq g.cfg (parts p).1
  | .suffixNull p => NullableSeq g.cfg (parts p).2
  | .suffixProductive p => ProductiveSeq g.cfg (parts p).2
  | .suffixFirst p c => ∃ t, c = t+1 ∧ FirstSeq g.cfg (parts p).2 t

def Layout.symbolRules (l : Layout) (lhs pos : Nat) : Sym Nat Nat → List (Rule Fact)
  | .t t => [⟨Fact.suffixFirst pos (t+1), []⟩,
      ⟨Fact.suffixProductive pos, [Fact.suffixProductive (pos+1)]⟩]
  | .n n =>
    let next := pos + 1
    [⟨Fact.prefixNull next, [Fact.prefixNull pos, Fact.nullable n]⟩,
     ⟨Fact.suffixNull pos, [Fact.nullable n, Fact.suffixNull next]⟩,
     ⟨Fact.suffixProductive pos, [Fact.productive n, Fact.suffixProductive next]⟩] ++
    (List.range l.c).flatMap fun col =>
      [⟨Fact.follow n col, [Fact.suffixNull next, Fact.follow lhs col]⟩] ++
      (if col = 0 then [] else
       [⟨Fact.suffixFirst pos col, [Fact.first n col]⟩,
        ⟨Fact.suffixFirst pos col, [Fact.nullable n, Fact.suffixFirst next col]⟩,
        ⟨Fact.follow n col, [Fact.suffixFirst next col]⟩])

def Layout.rowRules (l : Layout) (r : Row) : List (Rule Fact) :=
  let stop := r.base + r.rhs.length
  [⟨Fact.prefixNull r.base, []⟩, ⟨Fact.suffixNull stop, []⟩,
   ⟨Fact.suffixProductive stop, []⟩, ⟨Fact.nullable r.lhs, [Fact.suffixNull r.base]⟩,
   ⟨Fact.productive r.lhs, [Fact.suffixProductive r.base]⟩] ++
  ((List.range (l.c-1)).map fun t => ⟨Fact.first r.lhs (t+1), [Fact.suffixFirst r.base (t+1)]⟩) ++
  (r.rhs.zipIdx.flatMap fun (sym,i) => l.symbolRules r.lhs (r.base+i) sym)

def grammarRules (g : RawGrammar) : List (Rule Fact) :=
  ⟨.follow g.cfg.start 0, []⟩ :: g.rows.flatMap g.layout.rowRules

def grammarClauses (g : RawGrammar) : Array Clause :=
  ((grammarRules g).map (Rule.map (Fact.address g.layout))).toArray

def groupByAddress {α : Type} (key : α → Nat) (size : Nat) (items : List α) :
    Array (List α) :=
  items.foldl (fun bins item => bins.modify (key item) (item :: ·)) (Array.replicate size [])

def Clause.closedTest (r : Clause) : Test :=
  (Test.all (r.premises.map Test.positive)).implies (.positive r.head)

def Clause.justification (r : Clause) : Test :=
  .both (Test.all (r.premises.map Test.positive))
    (Test.all (r.premises.map fun p => .less p r.head))

def Layout.factAddresses (l : Layout) : List Nat :=
  (List.range (l.scratch - l.ntBase)).map (l.ntBase + ·)

/-- For each head, check both closure and one well-founded local explanation.
Addresses with no defining rule can only hold zero. -/
def factChecks (l : Layout) (rules : Array Clause) : Array Test :=
  let groups := groupByAddress Rule.head l.scratch rules.toList
  rules.map Clause.closedTest ++ l.factAddresses.toArray.map fun a =>
    (Test.positive a).implies (Test.any (groups[a]!.map Clause.justification))

def Layout.isFact (l : Layout) (a : Nat) : Bool := l.ntBase ≤ a && a < l.scratch

def clausesInBounds (g : RawGrammar) : Bool :=
  (grammarClauses g).all fun r => g.layout.isFact r.head && r.premises.all g.layout.isFact

def Layout.prediction (l : Layout) (r : Row) (col : Nat) : Test :=
  .either (.positive (l.suffixFirst r.base col))
    (.both (.positive (l.suffixNull r.base)) (.positive (l.follow r.lhs col)))

def Layout.cellChecks (l : Layout) (rows : List Row) : List Test :=
  let groups := groupByAddress Row.lhs l.n rows
  (List.range l.n).flatMap fun n => (List.range l.c).flatMap fun col =>
    let sameLhs := groups[n]!
    let cell := l.cell n col
    ((Test.positive cell).implies (Test.any (sameLhs.map (l.prediction · col)))) ::
      sameLhs.map fun r => (l.prediction r col).implies (.word cell (r.index+1).toUInt64)

def Layout.descentChecks (l : Layout) (r : Row) : List Test :=
  r.rhs.zipIdx.filterMap fun (sym,i) => match sym with
    | .t _ => none
    | .n child => some ((Test.positive (l.prefixNull (r.base+i))).implies
        (.less (l.rank child) (l.rank r.lhs)))

/-- The declarative obligations on initial certificate memory. Rules are
occurrence-indexed and interpreted by their least finite derivations. -/
def Layout.height (l : Layout) (read : Nat → UInt64) (address : Nat) : Nat :=
  if l.isFact address then (read address).toNat else 0

def Layout.PredictsAt (l : Layout) (read : Nat → UInt64) (r : Row) (col : Nat) : Prop :=
  0 < (read (l.suffixFirst r.base col)).toNat ∨
    (0 < (read (l.suffixNull r.base)).toNat ∧ 0 < (read (l.follow r.lhs col)).toNat)

def ValidCertificate (g : RawGrammar) (read : Nat → UInt64) : Prop :=
  let l := g.layout
  let rs := (grammarClauses g).toList
  Closed rs (l.height read) ∧ Justified rs (l.height read) ∧
    (∀ n < l.n, ∀ col < l.c,
      ((0 < (read (l.cell n col)).toNat → ∃ r ∈ g.rows, r.lhs = n ∧ l.PredictsAt read r col) ∧
       ∀ r ∈ g.rows, r.lhs = n → l.PredictsAt read r col → read (l.cell n col) = (r.index+1).toUInt64)) ∧
    (∀ r ∈ g.rows, ∀ child i, (Sym.n child, i) ∈ r.rhs.zipIdx →
      0 < (read (l.prefixNull (r.base+i))).toNat → read (l.rank child) < read (l.rank r.lhs)) ∧
    ∀ n < l.n, 0 < (read (l.productive n)).toNat

def predictionChecks (g : RawGrammar) : Array Test :=
  (g.layout.cellChecks g.rows ++ g.rows.flatMap g.layout.descentChecks ++
   (List.range g.nonterminals).map (fun n => Test.positive (g.layout.productive n))).toArray

def checks (g : RawGrammar) := factChecks g.layout (grammarClauses g) ++ predictionChecks g

/-- Compile each obligation to either continue with the next one or jump to the
same rejecting halt. Instruction width is an upper bound even on bad input. -/
def totalWidth (tests : List Test) := (tests.map Test.width).sum

def emitChecks (scratch reject : Nat) (initial : Array VerifyInstruction) (tests : List Test) :=
  tests.foldl (fun code p =>
    code ++ (p.code scratch code.size (code.size + p.width) reject).toArray) initial

def compileTests (scratch : Nat) (tests : Array Test) : Array VerifyInstruction :=
  let reject := totalWidth tests.toList + 1
  emitChecks scratch reject #[] tests.toList ++ #[.halt true, .halt false]

structure Compiled where
  words : Nat
  program : Array VerifyInstruction

/-- Representability depends only on the input grammar and its word-address layout. -/
def representable (g : RawGrammar) : Bool :=
  g.valid && g.layout.scratch + 1 < UInt64.size && g.cfg.prods.length < UInt64.size

def compileTableVerifier (g : RawGrammar) : Compiled :=
  if representable g then ⟨g.layout.scratch + 1, compileTests g.layout.scratch (checks g)⟩
  else ⟨0, #[.halt false]⟩

def Compiled.accepts (v : Compiled) (memory : Array UInt64) (budget : Nat) : Bool :=
  memory.size == v.words && (verifyRun v.program ⟨0, memory⟩ budget).2 == some true

def decodeCells (l : Layout) (memory : Array UInt64) : Option (Array (Option Nat)) := do
  guard (memory.size == l.scratch + 1)
  return (memory.extract 0 (l.n*l.c)).map fun w => if w == 0 then none else some (w.toNat - 1)

def decodeRank (l : Layout) (memory : Array UInt64) (n : Fin l.n) : Option Nat := do
  guard (memory.size == l.scratch + 1)
  return (← memory[l.rank n.val]?).toNat

end ConcreteSpec.GrammarVerification

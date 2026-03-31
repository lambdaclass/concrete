import Concrete.SSA

namespace Concrete

/-! ## SSACleanup — SSA optimization passes

Runs after SSAVerify. Simple cleanup passes:
- Dead block elimination (blocks with no predecessors except entry)
- Trivial phi elimination (phi with one incoming value → replace with that value)
- Empty block folding (block that just branches → redirect predecessors)
- Constant folding (evaluate constant binary ops at compile time)
- Algebraic simplifications (identity, annihilation, self-cancellation)
- Identity cast elimination (remove casts where source type == target type)
- Strength reduction (multiply/divide by power-of-2 → shift)
-/

-- ============================================================
-- Helpers
-- ============================================================

/-- Get successor labels from a terminator. -/
private def termSuccessors : STerm → List String
  | .br lbl => [lbl]
  | .condBr _ tl el => [tl, el]
  | .ret _ => []
  | .unreachable => []

/-- Replace a label in a terminator. -/
private def replaceLabelInTerm (t : STerm) (oldLabel newLabel : String) : STerm :=
  match t with
  | .br lbl => .br (if lbl == oldLabel then newLabel else lbl)
  | .condBr cond tl el =>
    .condBr cond
      (if tl == oldLabel then newLabel else tl)
      (if el == oldLabel then newLabel else el)
  | other => other

/-- Replace a label in phi node incoming entries. -/
private def replaceLabelInInst (inst : SInst) (oldLabel newLabel : String) : SInst :=
  match inst with
  | .phi dst incoming ty =>
    .phi dst (incoming.map fun (v, lbl) =>
      (v, if lbl == oldLabel then newLabel else lbl)) ty
  | other => other

/-- Replace an SVal in an instruction (for trivial phi elimination). -/
private def replaceRegInSVal (v : SVal) (oldReg : String) (replacement : SVal) : SVal :=
  match v with
  | .reg name _ => if name == oldReg then replacement else v
  | other => other

private def replaceRegInInst (inst : SInst) (oldReg : String) (replacement : SVal) : SInst :=
  let r := fun v => replaceRegInSVal v oldReg replacement
  match inst with
  | .binOp dst op lhs rhs ty => .binOp dst op (r lhs) (r rhs) ty
  | .unaryOp dst op operand ty => .unaryOp dst op (r operand) ty
  | .call dst fn args retTy =>
    -- Also handle indirect calls where fn is a register name (e.g., "%phi.6")
    let fn' := if fn == "%" ++ oldReg then
      match replacement with
      | .reg name _ => "%" ++ name
      | _ => fn
    else fn
    .call dst fn' (args.map r) retTy
  | .alloca dst ty => .alloca dst ty
  | .load dst ptr ty => .load dst (r ptr) ty
  | .store val ptr => .store (r val) (r ptr)
  | .gep dst base indices ty => .gep dst (r base) (indices.map r) ty
  | .phi dst incoming ty => .phi dst (incoming.map fun (v, lbl) => (r v, lbl)) ty
  | .cast dst val tgt => .cast dst (r val) tgt
  | .memcpy dst src size => .memcpy (r dst) (r src) size

private def replaceRegInTerm (t : STerm) (oldReg : String) (replacement : SVal) : STerm :=
  let r := fun v => replaceRegInSVal v oldReg replacement
  match t with
  | .ret (some v) => .ret (some (r v))
  | .condBr cond tl el => .condBr (r cond) tl el
  | other => other

-- ============================================================
-- Pass 1: Dead block elimination
-- ============================================================

/-- Find reachable blocks via BFS from an entry label. -/
private partial def findReachable (worklist : List String) (visited : List String) (blocks : List SBlock) : List String :=
  match worklist with
  | [] => visited
  | lbl :: wl =>
    if visited.contains lbl then findReachable wl visited blocks
    else
      let visited := lbl :: visited
      let succs := match blocks.find? fun b => b.label == lbl with
        | some b => termSuccessors b.term
        | none => []
      findReachable (wl ++ succs) visited blocks

/-- Remove phi entries that reference labels in the given dead set. -/
private def stripDeadPhiEntries (blocks : List SBlock) (deadLabels : List String) : List SBlock :=
  if deadLabels.isEmpty then blocks
  else blocks.map fun b =>
    { b with insts := b.insts.map fun inst =>
      match inst with
      | .phi dst incoming ty =>
        .phi dst (incoming.filter fun (_, lbl) => !deadLabels.contains lbl) ty
      | other => other }

/-- Remove blocks with no predecessors (except the entry block).
    Also strips phi entries referencing removed blocks. -/
private def eliminateDeadBlocks (blocks : List SBlock) : List SBlock :=
  match blocks with
  | [] => []
  | entry :: rest =>
    let reachable := findReachable [entry.label] [] (entry :: rest)
    let deadLabels := (entry :: rest).filter (fun b => !reachable.contains b.label) |>.map (·.label)
    let live := (entry :: rest).filter fun b => reachable.contains b.label
    stripDeadPhiEntries live deadLabels

-- ============================================================
-- Pass 2: Trivial phi elimination
-- ============================================================

/-- Find trivial phis: phi with all incoming values being the same (ignoring self-references).
    Returns list of (dst_reg, replacement_val). -/
private def findTrivialPhis (blocks : List SBlock) : List (String × SVal) :=
  blocks.foldl (fun acc b =>
    b.insts.foldl (fun acc inst =>
      match inst with
      | .phi dst incoming _ =>
        -- Filter out self-references
        let nonSelf := incoming.filter fun (v, _) =>
          match v with
          | .reg name _ => name != dst
          | _ => true
        -- Check if all remaining values are the same
        match nonSelf with
        | [] => acc
        | (v, _) :: rest =>
          let allSame := rest.all fun (v', _) =>
            match v, v' with
            | .reg n1 _, .reg n2 _ => n1 == n2
            | .intConst v1 _, .intConst v2 _ => v1 == v2
            | .boolConst b1, .boolConst b2 => b1 == b2
            | _, _ => false
          if allSame then (dst, v) :: acc else acc
      | _ => acc
    ) acc
  ) []

/-- Apply register replacements across all blocks. -/
private def applyReplacements (blocks : List SBlock)
    (replacements : List (String × SVal)) : List SBlock :=
  if replacements.isEmpty then blocks
  else
    blocks.map fun b =>
      let insts := b.insts.filter fun inst =>
        match inst with
        | .phi dst _ _ => !(replacements.any fun (d, _) => d == dst)
        | _ => true
      let insts := replacements.foldl (fun insts (oldReg, newVal) =>
        insts.map fun inst => replaceRegInInst inst oldReg newVal
      ) insts
      let term := replacements.foldl (fun t (oldReg, newVal) =>
        replaceRegInTerm t oldReg newVal
      ) b.term
      { b with insts := insts, term := term }

/-- Repeatedly eliminate trivial phis until fixpoint. -/
private partial def eliminateTrivialPhis (blocks : List SBlock) : List SBlock :=
  let trivials := findTrivialPhis blocks
  if trivials.isEmpty then blocks
  else eliminateTrivialPhis (applyReplacements blocks trivials)

-- ============================================================
-- Pass 3: Empty block folding
-- ============================================================

/-- Check if a label is referenced in any phi incoming of a block. -/
private def isPhiSource (b : SBlock) (label : String) : Bool :=
  b.insts.any fun inst =>
    match inst with
    | .phi _ incoming _ => incoming.any fun (_, lbl) => lbl == label
    | _ => false

/-- Find blocks that only contain a br (no instructions, just jump).
    Returns list of (emptyLabel, targetLabel).
    Skips the entry block and blocks whose target has phis referencing them
    (folding those requires complex phi predecessor updates). -/
private def findEmptyBlocks (blocks : List SBlock) : List (String × String) :=
  let nonEntry := blocks.drop 1
  nonEntry.filterMap fun b =>
    match b.insts, b.term with
    | [], .br target =>
      -- Skip folding if the target block has phis that reference this block
      let targetBlock := blocks.find? fun tb => tb.label == target
      match targetBlock with
      | some tb => if isPhiSource tb b.label then none else some (b.label, target)
      | none => some (b.label, target)
    | _, _ => none

/-- Redirect branches from empty blocks to their targets. -/
private def foldEmptyBlocks (blocks : List SBlock) : List SBlock :=
  let empties := findEmptyBlocks blocks
  if empties.isEmpty then blocks
  else
    -- For each empty block, redirect predecessors
    let blocks := empties.foldl (fun blocks (emptyLabel, targetLabel) =>
      blocks.map fun b =>
        let term := replaceLabelInTerm b.term emptyLabel targetLabel
        let insts := b.insts.map fun inst => replaceLabelInInst inst emptyLabel targetLabel
        { b with term := term, insts := insts }
    ) blocks
    -- Remove the now-unreferenced empty blocks
    eliminateDeadBlocks blocks

-- ============================================================
-- Pass 4: Dead instruction elimination
-- ============================================================

/-- Get the destination register of an instruction, if any. -/
private def instDst : SInst → Option String
  | .binOp dst _ _ _ _ => some dst
  | .unaryOp dst _ _ _ => some dst
  | .call dst _ _ _ => dst
  | .alloca dst _ => some dst
  | .load dst _ _ => some dst
  | .store _ _ => none
  | .gep dst _ _ _ => some dst
  | .phi dst _ _ => some dst
  | .cast dst _ _ => some dst
  | .memcpy _ _ _ => none

/-- Collect all register uses in an SVal. -/
private def svalUses : SVal → List String
  | .reg name _ => [name]
  | _ => []

/-- Collect all register uses in an instruction. -/
private def instUses : SInst → List String
  | .binOp _ _ lhs rhs _ => svalUses lhs ++ svalUses rhs
  | .unaryOp _ _ operand _ => svalUses operand
  | .call _ fn args _ =>
    let argUses := args.foldl (fun acc a => acc ++ svalUses a) []
    -- If the call target is a %-prefixed register (indirect fn-pointer call),
    -- include the register name as a use so DCE preserves the producing instruction.
    if fn.startsWith "%" then (fn.drop 1).toString :: argUses
    else argUses
  | .alloca _ _ => []
  | .load _ ptr _ => svalUses ptr
  | .store val ptr => svalUses val ++ svalUses ptr
  | .gep _ base indices _ => svalUses base ++ indices.foldl (fun acc i => acc ++ svalUses i) []
  | .phi _ incoming _ => incoming.foldl (fun acc (v, _) => acc ++ svalUses v) []
  | .cast _ val _ => svalUses val
  | .memcpy dst src _ => svalUses dst ++ svalUses src

/-- Collect register uses in a terminator. -/
private def termUses : STerm → List String
  | .ret (some v) => svalUses v
  | .ret none => []
  | .br _ => []
  | .condBr cond _ _ => svalUses cond
  | .unreachable => []

/-- Collect all used registers across all blocks. -/
private def collectAllUses (blocks : List SBlock) : List String :=
  blocks.foldl (fun acc b =>
    let instU := b.insts.foldl (fun acc i => acc ++ instUses i) []
    let termU := termUses b.term
    acc ++ instU ++ termU) []

/-- Is an instruction side-effecting (must be kept even if result unused)? -/
private def isSideEffecting : SInst → Bool
  | .call _ _ _ _ => true
  | .store _ _ => true
  | .memcpy _ _ _ => true
  | _ => false

/-- Eliminate instructions whose dst is never used. Iterate until fixpoint. -/
private partial def eliminateDeadInstsFixpoint (blocks : List SBlock) : List SBlock :=
  let allUses := collectAllUses blocks
  let changed := blocks.any fun b =>
    b.insts.any fun inst =>
      match instDst inst with
      | some dst => !allUses.contains dst && !isSideEffecting inst
      | none => false
  if !changed then blocks
  else
    let blocks := blocks.map fun b =>
      { b with insts := b.insts.filter fun inst =>
        match instDst inst with
        | some dst => allUses.contains dst || isSideEffecting inst
        | none => true }
    eliminateDeadInstsFixpoint blocks

-- ============================================================
-- Pass 5: Constant folding + algebraic simplifications
-- ============================================================

/-- Try to fold a binary operation on two constant operands. -/
private def foldBinOpConst (op : BinOp) (lhs rhs : SVal) (ty : Ty) : Option SVal :=
  match lhs, rhs with
  | .intConst a _, .intConst b _ =>
    match op with
    | .add => some (.intConst (a + b) ty)
    | .sub => some (.intConst (a - b) ty)
    | .mul => some (.intConst (a * b) ty)
    | .div => if b != 0 then some (.intConst (a / b) ty) else none
    | .mod => if b != 0 then some (.intConst (a % b) ty) else none
    | .eq => some (.boolConst (a == b))
    | .neq => some (.boolConst (a != b))
    | .lt => some (.boolConst (a < b))
    | .gt => some (.boolConst (a > b))
    | .leq => some (.boolConst (a <= b))
    | .geq => some (.boolConst (a >= b))
    | _ => none
  | .boolConst a, .boolConst b =>
    match op with
    | .and_ => some (.boolConst (a && b))
    | .or_ => some (.boolConst (a || b))
    | .eq => some (.boolConst (a == b))
    | .neq => some (.boolConst (a != b))
    | _ => none
  | _, _ => none

/-- Check if two SVal are the same register. -/
private def sameReg (a b : SVal) : Bool :=
  match a, b with
  | .reg n1 _, .reg n2 _ => n1 == n2
  | _, _ => false

private def isIntZero : SVal → Bool
  | .intConst 0 _ => true
  | _ => false

private def isIntOne : SVal → Bool
  | .intConst 1 _ => true
  | _ => false

private def isBoolTrue : SVal → Bool
  | .boolConst true => true
  | _ => false

private def isBoolFalse : SVal → Bool
  | .boolConst false => true
  | _ => false

/-- Check if a type is non-floating (safe for self-equality/cancellation rules). -/
private def isNonFloatTy : Ty → Bool
  | .float32 => false
  | .float64 => false
  | _ => true

/-- Algebraic identity simplifications for binary ops.
    Returns the simplified SVal (may be a register, not just a constant). -/
private def foldBinOpAlgebraic (op : BinOp) (lhs rhs : SVal) (ty : Ty) : Option SVal :=
  -- Additive identity
  if op == .add && isIntZero rhs then some lhs
  else if op == .add && isIntZero lhs then some rhs
  else if op == .sub && isIntZero rhs then some lhs
  -- Multiplicative identity
  else if op == .mul && isIntOne rhs then some lhs
  else if op == .mul && isIntOne lhs then some rhs
  -- Multiplicative annihilation
  else if op == .mul && isIntZero rhs then some (.intConst 0 ty)
  else if op == .mul && isIntZero lhs then some (.intConst 0 ty)
  -- Self-cancellation (not valid for floats: NaN - NaN = NaN, Inf - Inf = NaN)
  else if op == .sub && sameReg lhs rhs && isNonFloatTy ty then some (.intConst 0 ty)
  else if op == .bitxor && sameReg lhs rhs then some (.intConst 0 ty)
  -- Bitwise identity (same register)
  else if op == .bitand && sameReg lhs rhs then some lhs
  else if op == .bitor && sameReg lhs rhs then some lhs
  -- Shift by zero
  else if (op == .shl || op == .shr) && isIntZero rhs then some lhs
  -- Bitwise with zero
  else if op == .bitor && isIntZero rhs then some lhs
  else if op == .bitor && isIntZero lhs then some rhs
  else if op == .bitand && isIntZero rhs then some (.intConst 0 ty)
  else if op == .bitand && isIntZero lhs then some (.intConst 0 ty)
  else if op == .bitxor && isIntZero rhs then some lhs
  else if op == .bitxor && isIntZero lhs then some rhs
  -- Boolean identity
  else if op == .and_ && isBoolTrue rhs then some lhs
  else if op == .and_ && isBoolTrue lhs then some rhs
  else if op == .and_ && isBoolFalse rhs then some (.boolConst false)
  else if op == .and_ && isBoolFalse lhs then some (.boolConst false)
  else if op == .or_ && isBoolFalse rhs then some lhs
  else if op == .or_ && isBoolFalse lhs then some rhs
  else if op == .or_ && isBoolTrue rhs then some (.boolConst true)
  else if op == .or_ && isBoolTrue lhs then some (.boolConst true)
  -- Self-comparison (not valid for floats: NaN == NaN is false, NaN != NaN is true)
  else if op == .eq && sameReg lhs rhs && isNonFloatTy ty then some (.boolConst true)
  else if op == .neq && sameReg lhs rhs && isNonFloatTy ty then some (.boolConst false)
  else if (op == .leq || op == .geq) && sameReg lhs rhs && isNonFloatTy ty then some (.boolConst true)
  else if (op == .lt || op == .gt) && sameReg lhs rhs && isNonFloatTy ty then some (.boolConst false)
  else none

/-- Combined binary op folding: try constant folding first, then algebraic. -/
private def foldBinOp (op : BinOp) (lhs rhs : SVal) (ty : Ty) : Option SVal :=
  match foldBinOpConst op lhs rhs ty with
  | some v => some v
  | none => foldBinOpAlgebraic op lhs rhs ty

/-- Check if an integer is a power of 2. Returns the exponent if so. -/
private def isPowerOfTwo (n : Int) : Option Nat :=
  if n > 0 then
    let m := n.toNat
    if m &&& (m - 1) == 0 then some (Nat.log2 m) else none
  else none

/-- Check if a type is unsigned. -/
private def isUnsignedTy : Ty → Bool
  | .uint => true
  | .u8 => true
  | .u16 => true
  | .u32 => true
  | _ => false

/-- Fold constant expressions and algebraic identities in all blocks.
    Also performs strength reduction (mul/div by power-of-2 → shift). -/
private def foldConstants (blocks : List SBlock) : List SBlock :=
  -- Collect replacements (dst → value) and instruction rewrites (dst → new inst)
  let (replacements, rewrites) := blocks.foldl (fun (acc : List (String × SVal) × List (String × SInst)) b =>
    b.insts.foldl (fun (acc : List (String × SVal) × List (String × SInst)) inst =>
      match inst with
      | .binOp dst op lhs rhs ty =>
        match foldBinOp op lhs rhs ty with
        | some val => (acc.1 ++ [(dst, val)], acc.2)
        | none =>
          -- Strength reduction: mul by power-of-2 → shl
          if op == .mul then
            match rhs with
            | .intConst n _ =>
              match isPowerOfTwo n with
              | some exp => (acc.1, acc.2 ++ [(dst, SInst.binOp dst .shl lhs (.intConst exp ty) ty)])
              | none => acc
            | _ =>
              match lhs with
              | .intConst n _ =>
                match isPowerOfTwo n with
                | some exp => (acc.1, acc.2 ++ [(dst, SInst.binOp dst .shl rhs (.intConst exp ty) ty)])
                | none => acc
              | _ => acc
          -- Strength reduction: unsigned div by power-of-2 → shr
          else if op == .div && isUnsignedTy ty then
            match rhs with
            | .intConst n _ =>
              match isPowerOfTwo n with
              | some exp => (acc.1, acc.2 ++ [(dst, SInst.binOp dst .shr lhs (.intConst exp ty) ty)])
              | none => acc
            | _ => acc
          else acc
      | _ => acc
    ) acc) ([], [])
  -- Apply value replacements
  let blocks :=
    if replacements.isEmpty then blocks
    else applyReplacements blocks replacements
  -- Apply instruction rewrites (replace instruction in-place)
  if rewrites.isEmpty then blocks
  else blocks.map fun b =>
    { b with insts := b.insts.map fun inst =>
      match instDst inst with
      | some dst =>
        match rewrites.find? fun (d, _) => d == dst with
        | some (_, newInst) => newInst
        | none => inst
      | none => inst }

-- ============================================================
-- Pass 6: GEP-zero elimination
-- ============================================================

/-- Eliminate `gep dst base [intConst 0 _]` (single zero-index GEP is identity). -/
private def eliminateGepZero (blocks : List SBlock) : List SBlock :=
  let replacements := blocks.foldl (fun acc b =>
    b.insts.foldl (fun acc inst =>
      match inst with
      | .gep dst base [.intConst 0 _] _ => (dst, base) :: acc
      | _ => acc) acc) []
  if replacements.isEmpty then blocks
  else applyReplacements blocks replacements

-- ============================================================
-- Pass 7: Store-load forwarding (block-local)
-- ============================================================

/-- Get the register name from an SVal, if it is a register. -/
private def svalRegName : SVal → Option String
  | .reg name _ => some name
  | _ => none

/-- Forward store values to subsequent loads within each block.
    Conservatively invalidates on any call or memcpy. -/
private def forwardStoreToLoad (blocks : List SBlock) : List SBlock :=
  let replacements := blocks.foldl (fun acc b =>
    let (_, blockRepls) := b.insts.foldl
      (fun (state : List (String × SVal) × List (String × SVal)) inst =>
        let (storeMap, repls) := state
        match inst with
        | .store val ptr =>
          match svalRegName ptr with
          | some ptrName => ([(ptrName, val)], repls)
          | none => ([], repls)
        | .load dst ptr ty =>
          match svalRegName ptr with
          | some ptrName =>
            match storeMap.find? fun (k, _) => k == ptrName with
            | some (_, storedVal) =>
              if storedVal.ty == ty then (storeMap, (dst, storedVal) :: repls)
              else (storeMap, repls)
            | none => (storeMap, repls)
          | none => (storeMap, repls)
        | .call .. | .memcpy .. => ([], repls)
        | _ => (storeMap, repls))
      ([], [])
    acc ++ blockRepls) []
  if replacements.isEmpty then blocks
  else applyReplacements blocks replacements

-- ============================================================
-- Pass 8: Redundant load elimination (block-local)
-- ============================================================

/-- Eliminate redundant loads: if two loads from the same pointer (same type)
    occur in the same block with no intervening store/call/memcpy,
    replace the second load's result with the first's. -/
private def eliminateRedundantLoads (blocks : List SBlock) : List SBlock :=
  let replacements := blocks.foldl (fun acc b =>
    let (_, blockRepls) := b.insts.foldl
      (fun (state : List (String × Ty × String) × List (String × SVal)) inst =>
        let (loadMap, repls) := state
        match inst with
        | .load dst ptr ty =>
          match svalRegName ptr with
          | some ptrName =>
            match loadMap.find? fun (k, t, _) => k == ptrName && t == ty with
            | some (_, _, prevDst) => (loadMap, (dst, SVal.reg prevDst ty) :: repls)
            | none => ((ptrName, ty, dst) :: loadMap, repls)
          | none => (loadMap, repls)
        | .store .. | .call .. | .memcpy .. => ([], repls)
        | _ => (loadMap, repls))
      ([], [])
    acc ++ blockRepls) []
  if replacements.isEmpty then blocks
  else applyReplacements blocks replacements

-- ============================================================
-- Pass 9: Identity cast elimination
-- ============================================================

/-- Eliminate `cast dst val tgt` where `val.ty == tgt` (no-op cast). -/
private def eliminateIdentityCasts (blocks : List SBlock) : List SBlock :=
  let replacements := blocks.foldl (fun acc b =>
    b.insts.foldl (fun acc inst =>
      match inst with
      | .cast dst val tgt =>
        if val.ty == tgt then (dst, val) :: acc else acc
      | _ => acc) acc) []
  if replacements.isEmpty then blocks
  else applyReplacements blocks replacements

-- ============================================================
-- Pass 10: Constant branch elimination
-- ============================================================

/-- Replace `condBr (boolConst true) t e` → `br t` and
    `condBr (boolConst false) t e` → `br e`. -/
private def eliminateConstantBranches (blocks : List SBlock) : List SBlock :=
  blocks.map fun b =>
    match b.term with
    | .condBr (.boolConst true) thenLabel _ =>
      { b with term := .br thenLabel }
    | .condBr (.boolConst false) _ elseLabel =>
      { b with term := .br elseLabel }
    | _ => b

-- ============================================================
-- Pass 11: Stale PHI entry cleanup
-- ============================================================

/-- For each block, remove PHI incoming entries whose source label is not
    an actual predecessor (i.e., no block with that label has a terminator
    targeting this block). This is needed after constant branch elimination
    removes edges without updating PHIs in the target blocks. -/
private def stripStalePhiEntries (blocks : List SBlock) : List SBlock :=
  blocks.map fun b =>
    let predLabels := blocks.filter (fun pred =>
      (termSuccessors pred.term).contains b.label) |>.map (·.label)
    { b with insts := b.insts.map fun inst =>
      match inst with
      | .phi dst incoming ty =>
        .phi dst (incoming.filter fun (_, lbl) => predLabels.contains lbl) ty
      | other => other }

-- ============================================================
-- Combined cleanup
-- ============================================================

/-- Cheap fingerprint: total blocks + total instructions.
    If this doesn't change between iterations, the pass sequence has converged. -/
private def blocksFingerprint (blocks : List SBlock) : Nat :=
  blocks.foldl (fun acc b => acc + 1 + b.insts.length) 0

/-- One full pass of all cleanup transformations. -/
private def cleanupBlocks (blocks : List SBlock) : List SBlock :=
  let blocks := eliminateDeadBlocks blocks
  let blocks := eliminateTrivialPhis blocks
  let blocks := foldConstants blocks              -- constant folding + algebraic + strength reduction
  let blocks := eliminateGepZero blocks           -- clean up gep-zero before memory opts
  let blocks := forwardStoreToLoad blocks         -- local store-to-load forwarding
  let blocks := eliminateRedundantLoads blocks    -- local redundant load elimination
  let blocks := eliminateIdentityCasts blocks     -- remove no-op casts
  let blocks := eliminateConstantBranches blocks
  let blocks := stripStalePhiEntries blocks
  let blocks := eliminateTrivialPhis blocks       -- re-run after folding may expose new trivial phis
  let blocks := eliminateDeadInstsFixpoint blocks
  let blocks := foldEmptyBlocks blocks
  eliminateDeadBlocks blocks                      -- re-run after branch elimination

/-- Run the full pass sequence to fixpoint (converge when fingerprint stabilizes). -/
private partial def cleanupFixpoint (blocks : List SBlock) : List SBlock :=
  let before := blocksFingerprint blocks
  let blocks := cleanupBlocks blocks
  if blocksFingerprint blocks == before then blocks
  else cleanupFixpoint blocks

private def cleanupFn (f : SFnDef) : SFnDef :=
  { f with blocks := cleanupFixpoint f.blocks }

def ssaCleanupModule (m : SModule) : SModule :=
  { m with functions := m.functions.map cleanupFn }

def ssaCleanupProgram (modules : List SModule) : List SModule :=
  modules.map ssaCleanupModule

end Concrete

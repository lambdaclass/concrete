pub enum Instruction {
    /// Push a value from the scope into the stack.
    GetScope(String),
    /// Pop a value from the stack and insert it into the scope.
    SetScope(String),

    /// Construct a data structure (struct, enum, union, tuple, array...).
    DataPack,
    /// Destructure a data structure (struct, enum, union, tuple, array...).
    DataUnpack,

    /// Call a function.
    Invoke(String, usize),

    OpAdd,
    OpAnd,
    OpBitAnd,
    OpBitNot,
    OpBitOr,
    OpBitXor,
    OpCmpEq,
    OpCmpGe,
    OpCmpGt,
    OpCmpLe,
    OpCmpLt,
    OpCmpNe,
    OpDiv,
    OpMul,
    OpNeg,
    OpNot,
    OpOr,
    OpRem,
    OpSar,
    OpSll,
    OpSlr,
    OpSub,

    /// Invalid operation.
    ///
    /// This is a placeholder for compiler errors. It is treated as a block terminator that
    /// disregards the remaining stack and scope items.
    OpInval,
}

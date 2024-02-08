use std::collections::{BTreeMap, HashMap, HashSet};

use concrete_ast::common::{Ident, Span};

pub mod lowering;

pub type LocalIndex = usize;
pub type BlockIndex = usize;
pub type TypeIndex = usize;
pub type FieldIndex = usize;

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
    pub symbols: HashMap<DefId, String>,
    pub modules: HashMap<String, DefId>,
    pub functions: HashMap<String, DefId>,
    pub constants: HashMap<String, DefId>,
    pub structs: HashMap<String, DefId>,
    pub types: HashMap<String, DefId>,
}

#[derive(Debug, Clone, Default)]
pub struct ProgramBody {
    pub top_level_module_names: BTreeMap<String, DefId>,
    /// The top level modules.
    pub top_level_modules: Vec<DefId>,
    /// All the modules in a flat map.
    pub modules: BTreeMap<DefId, ModuleBody>,
    /// This stores all the functions from all modules
    pub functions: BTreeMap<DefId, FnBody>,
    /// The function signatures.
    pub function_signatures: HashMap<DefId, (Vec<Ty>, Ty)>,
}

#[derive(Debug, Clone)]
pub struct ModuleBody {
    pub id: DefId,
    pub parent_ids: Vec<DefId>,
    pub symbols: SymbolTable,
    /// Functions defined in this module.
    pub functions: HashSet<DefId>,
    /// Structs defined in this module.
    pub structs: HashSet<DefId>,
    /// Types defined in this module.
    pub types: HashSet<DefId>,
    /// Constants defined in this module.
    pub constants: HashSet<DefId>,
    /// Submodules defined in this module.
    pub modules: HashSet<DefId>,
    /// Imported items. symbol -> id
    pub imports: HashMap<String, DefId>,
}

/// Function body
#[derive(Debug, Clone)]
pub struct FnBody {
    pub id: DefId,
    pub name: String,
    pub basic_blocks: Vec<BasicBlock>,
    pub locals: Vec<Local>,
}

impl FnBody {
    pub fn get_params(&self) -> Vec<&Local> {
        self.locals
            .iter()
            .filter(|x| matches!(x.kind, LocalKind::Arg))
            .collect()
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub statements: Vec<Statement>,
    pub terminator: Box<Terminator>,
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub span: Option<Span>,
    pub kind: StatementKind,
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Assign(Place, Rvalue),
    StorageLive(LocalIndex),
    StorageDead(LocalIndex),
}

#[derive(Debug, Clone)]
pub struct Terminator {
    pub span: Option<Span>,
    pub kind: TerminatorKind,
}

/// The kind of terminator for a basic block.
#[derive(Debug, Clone)]
pub enum TerminatorKind {
    /// Unconditional branch to the given target block.
    Goto { target: BlockIndex },
    /// Function return
    Return,
    /// Unreachable terminator.
    Unreachable,
    /// Function call
    Call {
        /// The function to call.
        func: DefId,
        /// The arguments.
        args: Vec<Rvalue>,
        /// The place in memory to store the return value of the function call.
        destination: Place,
        /// What basic block to jump to after the function call, if the function is non-diverging (i.e it returns control back).
        target: Option<BlockIndex>,
    },
    /// Conditional branching, used in ifs, while
    SwitchInt {
        /// The value to check.
        discriminator: Operand,
        /// The targets that match against the value.
        targets: SwitchTargets,
    },
}

/// Used for ifs, match
#[derive(Debug, Clone)]
pub struct SwitchTargets {
    /// The values to match the discriminator against.
    /// Each value has a target with the same index.
    pub values: Vec<ValueTree>,
    /// The targets where to jump into if the value with the same index matches.
    /// There is always 1 more extra target, the "otherwise" block for the case where no value matched.
    pub targets: Vec<BlockIndex>,
}

/// A right-side value. The computed value of a right hand side statement such an assignment.
///
/// Binary operations can't be nested, so complex expressions
/// are made by storing temporaries in newly created temp locals (with their given place).
#[derive(Debug, Clone)]
pub enum Rvalue {
    /// Use the operand as-is.
    Use(Operand),
    /// The result of the logical operation.
    LogicOp(LogOp, (Operand, Operand)), // separate due to short-circuit
    /// The result of a binary operation.
    BinaryOp(BinOp, (Operand, Operand)),
    /// The result of a unary operation.
    UnaryOp(UnOp, Operand),
    /// A reference to a place.
    Ref(Mutability, Place),
}

/// A operand is a value, either from a place in memory or constant data.
#[derive(Debug, Clone)]
pub enum Operand {
    Place(Place),
    Const(ConstData),
}

/// A place in memory, defined by the given local and it's projection (deref, field, index, etc).
#[derive(Debug, Clone)]
pub struct Place {
    pub local: LocalIndex,
    pub projection: Vec<PlaceElem>,
}

/// A element of the place projection.
#[derive(Debug, Clone)]
pub enum PlaceElem {
    /// Dereference
    Deref,
    /// Get a field
    Field(FieldIndex, TypeIndex),
    /// array index
    Index(LocalIndex),
}

/// A local, akin to a variable, it can be user defined or compiler-introduced.
#[derive(Debug, Clone)]
pub struct Local {
    /// A span exists for user-defined variables.
    pub span: Option<Span>,
    /// A name exists for user-defined variables.
    pub debug_name: Option<String>,
    /// The type of the local.
    pub ty: Ty,
    /// The type of local.
    pub kind: LocalKind,
}

impl Local {
    pub fn new(span: Option<Span>, kind: LocalKind, ty: Ty, debug_name: Option<String>) -> Self {
        Self {
            span,
            kind,
            ty,
            debug_name,
        }
    }

    pub const fn temp(ty: Ty) -> Self {
        Self {
            span: None,
            ty,
            kind: LocalKind::Temp,
            debug_name: None,
        }
    }
}

/// The type of local.
#[derive(Debug, Clone, Copy)]
pub enum LocalKind {
    /// User-declared variable binding or compiler-introduced temporary.
    Temp,
    /// Function argument.
    Arg,
    /// Location of function's return value.
    ReturnPointer,
}

/// Aggregate data type: struct, enum, tuple..
pub struct AdtBody {
    pub name: DefId,
    pub variants: Vec<VariantDef>,
}

// Definition of a variant, a struct field or enum variant.
pub struct VariantDef {
    pub id: DefId,
    // The relative position in the aggregate structure.
    pub discriminant: usize,
}

// A definition id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct DefId {
    // The program id, like a crate in rust.
    pub program_id: usize,
    pub id: usize,
}

/// A type
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Ty {
    pub span: Option<Span>,
    pub kind: TyKind,
}

impl Ty {
    pub fn new(span: &Span, kind: TyKind) -> Self {
        Self {
            span: Some(*span),
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum TyKind {
    Unit, // ()
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    String,
    Array(Box<Ty>, Box<ConstData>),
    Ref(Box<Self>, Mutability),
    // Type param <T>
    Param {
        index: usize,
        name: String, // todo: change me?
    },
}

impl TyKind {
    pub fn get_falsy_value(&self) -> ValueTree {
        match self {
            TyKind::Unit => unreachable!(),
            TyKind::Bool => ValueTree::Leaf(ConstValue::Bool(false)),
            TyKind::Char => todo!(),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => ValueTree::Leaf(ConstValue::I8(0)),
                IntTy::I16 => ValueTree::Leaf(ConstValue::I16(0)),
                IntTy::I32 => ValueTree::Leaf(ConstValue::I32(0)),
                IntTy::I64 => ValueTree::Leaf(ConstValue::I64(0)),
                IntTy::I128 => ValueTree::Leaf(ConstValue::I128(0)),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => ValueTree::Leaf(ConstValue::U8(0)),
                UintTy::U16 => ValueTree::Leaf(ConstValue::U16(0)),
                UintTy::U32 => ValueTree::Leaf(ConstValue::U32(0)),
                UintTy::U64 => ValueTree::Leaf(ConstValue::U64(0)),
                UintTy::U128 => ValueTree::Leaf(ConstValue::U128(0)),
            },
            TyKind::Float(_) => todo!(),
            TyKind::String => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Ref(_, _) => todo!(),
            TyKind::Param { .. } => todo!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mutability {
    Not,
    Mut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    U128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ConstData {
    pub ty: TyKind,
    pub data: ConstKind,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ConstKind {
    /// A generic parameter constant.
    Param(ParamConst),
    /// The value of the constant.
    Value(ValueTree),
    /// A constant expression: todo.
    Expr(Box<ConstExpr>),
}

/// A generic constant
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParamConst {
    pub index: usize,
    // todo: change me
    pub ident: Ident,
}

/// Constant data, in case the data is complex such as an array it will be a branch with leaf values.
#[derive(Debug, Clone, PartialEq, PartialOrd)]
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/consts/valtree/enum.ValTree.html
pub enum ValueTree {
    Leaf(ConstValue),
    Branch(Vec<Self>),
}

/// Constant expression
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ConstExpr {
    Binop(BinOp, ConstData, ConstData),
    UnOp(UnOp, ConstData),
    FunctionCall(ConstData, Vec<ConstData>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LogOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitXor,
    BitAnd,
    BitOr,
    Shl,
    Shr,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ConstValue {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    F32(f32),
    F64(f64),
}

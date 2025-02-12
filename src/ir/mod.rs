use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
    path::PathBuf,
    sync::Arc,
};

pub mod lowering;

pub type LocalIndex = usize;
pub type BlockIndex = usize;
pub type FieldIndex = usize;

pub type ModuleIndex = SmallSlabIndex<ModuleBody>;
pub type StructIndex = SmallSlabIndex<Option<AdtBody>>;
pub type FnIndex = SmallSlabIndex<Option<FnBody>>;
pub type TypeIndex = SmallSlabIndex<Option<TyKind>>;
pub type ConstIndex = SmallSlabIndex<Option<ConstBody>>;

pub type Types = SmallSlab<Option<TyKind>>;
pub type Functions = SmallSlab<Option<FnBody>>;
pub type Structs = SmallSlab<Option<AdtBody>>;
pub type Constants = SmallSlab<Option<ConstBody>>;
pub type Modules = SmallSlab<ModuleBody>;

pub use crate::ast::common::Span;
use typed_generational_arena::{SmallSlab, SmallSlabIndex};

#[derive(Debug, Clone)]
pub struct ProgramBody {
    pub types: Types,
    pub functions: Functions,
    pub structs: Structs,
    pub constants: Constants,
    pub modules: Modules,
    pub top_level_modules: Vec<ModuleIndex>,
    pub builtin_types: HashMap<TyKind, TypeIndex>,
}

impl ProgramBody {
    pub fn get_bool_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::Bool).unwrap()
    }

    pub fn get_char_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::Char).unwrap()
    }

    pub fn get_i32_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::Int(IntTy::I32)).unwrap()
    }

    pub fn get_i64_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::Int(IntTy::I64)).unwrap()
    }

    pub fn get_u64_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::Uint(UintTy::U64)).unwrap()
    }

    pub fn get_f64_ty(&self) -> TypeIndex {
        *self
            .builtin_types
            .get(&TyKind::Float(FloatTy::F64))
            .unwrap()
    }

    pub fn get_string_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::String).unwrap()
    }

    pub fn get_unit_ty(&self) -> TypeIndex {
        *self.builtin_types.get(&TyKind::Unit).unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct ModuleBody {
    pub name: String,
    pub parents: Vec<ModuleIndex>,
    /// Functions in this module.
    pub functions: HashSet<FnIndex>,
    /// Structs in this module.
    pub structs: HashSet<StructIndex>,
    /// Types in this module.
    pub types: HashSet<TypeIndex>,
    /// Constants in this module.
    pub constants: HashSet<ConstIndex>,
    /// Submodules in this module.
    pub modules: HashMap<String, ModuleIndex>,
    pub span: Span,
    pub file_path: PathBuf,
}

/// Function body
#[derive(Debug, Clone)]
pub struct FnBody {
    pub name: String,
    pub args: Vec<TypeIndex>,
    pub ret_ty: TypeIndex,
    pub is_extern: bool,
    pub is_intrinsic: Option<ConcreteIntrinsic>,
    pub basic_blocks: Vec<BasicBlock>,
    pub module_idx: ModuleIndex,
    pub locals: Vec<Local>,
}

impl FnBody {
    pub fn get_params(&self) -> Vec<&Local> {
        self.locals
            .iter()
            .filter(|x| matches!(x.kind, LocalKind::Arg))
            .collect()
    }

    pub fn get_mangled_name(&self) -> String {
        self.name.clone()
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
        func: FnIndex,
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
    /// A cast.
    Cast(Operand, TypeIndex, Span),
}

impl Rvalue {
    pub fn get_local(&self) -> Option<usize> {
        match self {
            Rvalue::Use(op) => op.get_local(),
            Rvalue::Ref(_, op) => Some(op.local),
            Rvalue::Cast(op, _, _) => op.get_local(),
            _ => None,
        }
    }
}

/// A operand is a value, either from a place in memory or constant data.
#[derive(Debug, Clone)]
pub enum Operand {
    Place(Place),
    Const(ConstData),
}

impl Operand {
    pub fn get_local(&self) -> Option<usize> {
        match self {
            Operand::Place(place) => Some(place.local),
            Operand::Const(_) => None,
        }
    }
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
    Field(FieldIndex),
    /// array index
    Index(LocalIndex),
    /// constant array index
    ConstantIndex(u64),
}

/// A local, akin to a variable, it can be user defined or compiler-introduced.
#[derive(Debug, Clone)]
pub struct Local {
    /// A span exists for user-defined variables.
    pub span: Option<Span>,
    /// A name exists for user-defined variables.
    pub debug_name: Option<String>,
    /// The type of the local.
    pub ty: TypeIndex,
    /// The type of local.
    pub kind: LocalKind,
    /// Whether this local is declared mutable.
    pub mutable: bool,
}

impl Local {
    pub fn new(
        span: Option<Span>,
        kind: LocalKind,
        ty: TypeIndex,
        debug_name: Option<String>,
        mutable: bool,
    ) -> Self {
        Self {
            span,
            kind,
            ty,
            debug_name,
            mutable,
        }
    }

    pub const fn temp(ty: TypeIndex) -> Self {
        Self {
            span: None,
            ty,
            kind: LocalKind::Temp,
            debug_name: None,
            mutable: false,
        }
    }

    pub fn is_mutable(&self, types: &Types) -> bool {
        if self.mutable {
            return true;
        }

        match types[self.ty].as_ref().unwrap() {
            TyKind::Ptr(_, is_mut) => matches!(is_mut, Mutability::Mut),
            TyKind::Ref(_, is_mut) => matches!(is_mut, Mutability::Mut),
            _ => false,
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
#[derive(Debug, Clone)]
pub struct AdtBody {
    pub is_pub: bool,
    pub name: String,
    pub variants: Vec<VariantDef>,
    pub name_to_variant_idx: HashMap<String, usize>,
    pub span: Span,
}

/// Definition of a variant, a struct field or enum variant.
#[derive(Debug, Clone)]
pub struct VariantDef {
    // The relative position in the aggregate structure.
    pub name: String,
    pub discriminant: usize,
    pub ty: TypeIndex,
}

#[derive(Debug, Clone)]
pub struct ConstBody {
    pub name: String,
    pub value: ConstData,
    pub span: Span,
}

/// A type kind, cheaply clonable.
#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub enum TyKind {
    Unit, // ()
    Bool,
    Char,
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    String,
    Array(TypeIndex, Arc<ConstData>),
    Ref(TypeIndex, Mutability),
    Ptr(TypeIndex, Mutability),
    Struct(StructIndex),
}

impl TyKind {
    // checks if a type equals another
    pub fn is_equal(&self, other: &TyKind, ir: &ProgramBody) -> bool {
        match self {
            TyKind::Unit
            | TyKind::Bool
            | TyKind::Char
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::String => self == other,
            TyKind::Array(index, const_data) => {
                if let TyKind::Array(other_index, other_const_data) = other {
                    let self_ty = ir.types[*index].as_ref().unwrap();
                    let other_ty = ir.types[*other_index].as_ref().unwrap();
                    const_data.data == other_const_data.data && self_ty.is_equal(other_ty, ir)
                } else {
                    false
                }
            }
            TyKind::Ref(index, mutability) => {
                if let TyKind::Ref(other_index, other_mutability) = other {
                    let self_ty = ir.types[*index].as_ref().unwrap();
                    let other_ty = ir.types[*other_index].as_ref().unwrap();
                    mutability == other_mutability && self_ty.is_equal(other_ty, ir)
                } else {
                    false
                }
            }
            TyKind::Ptr(index, mutability) => {
                if let TyKind::Ptr(other_index, other_mutability) = other {
                    let self_ty = ir.types[*index].as_ref().unwrap();
                    let other_ty = ir.types[*other_index].as_ref().unwrap();
                    mutability == other_mutability && self_ty.is_equal(other_ty, ir)
                } else {
                    false
                }
            }
            TyKind::Struct(index) => {
                if let TyKind::Struct(other_index) = other {
                    index == other_index
                } else {
                    false
                }
            }
        }
    }

    pub fn is_ptr_like(&self) -> bool {
        matches!(self, TyKind::Ptr(_, _) | TyKind::Ref(_, _))
    }

    pub fn get_inner_type(&self) -> Option<TypeIndex> {
        match self {
            TyKind::Unit => None,
            TyKind::Bool => None,
            TyKind::Char => None,
            TyKind::Int(_) => None,
            TyKind::Uint(_) => None,
            TyKind::Float(_) => None,
            TyKind::String => None,
            TyKind::Array(index, _) => Some(*index),
            TyKind::Ref(index, _) => Some(*index),
            TyKind::Ptr(index, _) => Some(*index),
            TyKind::Struct { .. } => None,
        }
    }

    pub fn is_array(&self) -> bool {
        matches!(self, TyKind::Array(_, _))
    }

    pub fn is_int(&self) -> bool {
        matches!(self, TyKind::Int(_) | TyKind::Uint(_))
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, TyKind::Int(_))
    }

    pub fn is_float(&self) -> bool {
        matches!(self, TyKind::Float(_))
    }

    /// Returns the type bit width, None if unsized.
    ///
    /// Meant for use in casts.
    pub fn get_bit_width(&self) -> Option<usize> {
        match self {
            TyKind::Unit => None,
            TyKind::Bool => Some(1),
            TyKind::Char => Some(8),
            TyKind::Int(ty) => match ty {
                IntTy::I8 => Some(8),
                IntTy::I16 => Some(16),
                IntTy::I32 => Some(32),
                IntTy::I64 => Some(64),
                IntTy::I128 => Some(128),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U8 => Some(8),
                UintTy::U16 => Some(16),
                UintTy::U32 => Some(32),
                UintTy::U64 => Some(64),
                UintTy::U128 => Some(128),
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F32 => Some(32),
                FloatTy::F64 => Some(64),
            },
            TyKind::String => todo!(),
            TyKind::Array(_, _) => todo!(),
            TyKind::Ref(_, _) => todo!(),
            TyKind::Ptr(_, _) => todo!(),
            TyKind::Struct { .. } => todo!(),
        }
    }
}

impl TyKind {
    pub fn display(&self, ir: &ProgramBody) -> Result<String, std::fmt::Error> {
        let mut f = String::new();
        match self {
            TyKind::Unit => write!(f, "()"),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Char => write!(f, "char"),
            TyKind::Int(ty) => match ty {
                IntTy::I128 => write!(f, "i128"),
                IntTy::I64 => write!(f, "i64"),
                IntTy::I32 => write!(f, "i32"),
                IntTy::I16 => write!(f, "i16"),
                IntTy::I8 => write!(f, "i8"),
            },
            TyKind::Uint(ty) => match ty {
                UintTy::U128 => write!(f, "u128"),
                UintTy::U64 => write!(f, "u64"),
                UintTy::U32 => write!(f, "u32"),
                UintTy::U16 => write!(f, "u16"),
                UintTy::U8 => write!(f, "u8"),
            },
            TyKind::Float(ty) => match ty {
                FloatTy::F32 => write!(f, "f64"),
                FloatTy::F64 => write!(f, "f32"),
            },
            TyKind::String => write!(f, "string"),
            TyKind::Array(inner, size) => {
                let value =
                    if let ConstKind::Value(ValueTree::Leaf(ConstValue::U64(x))) = &size.data {
                        *x
                    } else {
                        unreachable!("const data for array sizes should always be u64")
                    };
                write!(
                    f,
                    "[{}; {:?}]",
                    ir.types[*inner].as_ref().unwrap().display(ir)?,
                    value
                )
            }
            TyKind::Ref(inner, is_mut) => {
                let word = if let Mutability::Mut = is_mut {
                    "mut"
                } else {
                    "const"
                };

                write!(
                    f,
                    "&{word} {}",
                    ir.types[*inner].as_ref().unwrap().display(ir)?
                )
            }
            TyKind::Ptr(inner, is_mut) => {
                let word = if let Mutability::Mut = is_mut {
                    "mut"
                } else {
                    "const"
                };

                write!(
                    f,
                    "*{word} {}",
                    ir.types[*inner].as_ref().unwrap().display(ir)?
                )
            }
            TyKind::Struct(index) => {
                let body = ir.structs[*index].as_ref().unwrap();
                writeln!(f, "{} {{", body.name)?;

                for var in &body.variants {
                    let ty = ir.types[var.ty].as_ref().unwrap().display(ir)?;
                    writeln!(f, "\t{}: {},", var.name, ty)?;
                }
                write!(f, "}}")?;

                Ok(())
            }
        }?;

        Ok(f)
    }
}

impl TyKind {
    pub fn get_falsy_value(&self) -> ValueTree {
        match self {
            TyKind::Unit => unreachable!(),
            TyKind::Bool => ValueTree::Leaf(ConstValue::Bool(false)),
            TyKind::Char => ValueTree::Leaf(ConstValue::Char(0)),
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
            TyKind::Struct { .. } => todo!(),
            TyKind::Ptr(_, _) => todo!(),
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct ConstData {
    pub ty: TypeIndex,
    pub span: Span,
    pub data: ConstKind,
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ConstKind {
    /// The value of the constant.
    Value(ValueTree),
    /// A constant expression: todo.
    Expr(Box<ConstExpr>),
}

/// Constant data, in case the data is complex such as an array it will be a branch with leaf values.
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
// https://doc.rust-lang.org/nightly/nightly-rustc/rustc_middle/ty/consts/valtree/enum.ValTree.html
pub enum ValueTree {
    Leaf(ConstValue),
    Branch(Vec<Self>),
}

/// Constant expression
#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ConstValue {
    Bool(bool),
    Char(u8),
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
    F32(String),
    F64(String),
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum ConcreteIntrinsic {
    // Todo: Add intrinsics here
}

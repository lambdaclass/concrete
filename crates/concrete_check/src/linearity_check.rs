use std::collections::HashMap;

use concrete_session::Session;


use self::errors::LinearityError;
pub mod errors;


use concrete_ir::ProgramBody;



#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VarState {
    Unconsumed,
    Consumed,
    Borrowed,
    BorrowedMut,
}

#[derive(Debug, Clone, PartialEq)]
enum CountResult {
    Zero,
    One,
    MoreThanOne,
}

#[derive(Debug, Clone, Copy)]
struct Appearances {
    consumed: u32,
    write: u32,
    read: u32,
    path: u32,
}

#[allow(dead_code)]
enum Expr {
    NilConstant,
    BoolConstant(bool),
    IntConstant(i32),
    FloatConstant(f64),
    StringConstant(String),
    ConstVar,
    ParamVar(String),
    LocalVar(String),
    FunVar,
    Funcall(Box<Expr>, Vec<Expr>),
    MethodCall(Box<Expr>, Vec<Expr>),
    VarMethodCall(Vec<Expr>),
    FptrCall(Box<Expr>, Vec<Expr>),
    Cast(Box<Expr>, String),
    Comparison(Box<Expr>, Box<Expr>),
    Conjunction(Box<Expr>, Box<Expr>),
    Disjunction(Box<Expr>, Box<Expr>),
    Negation(Box<Expr>),
    IfExpression(Box<Expr>, Box<Expr>, Box<Expr>),
    RecordConstructor(Vec<Expr>),
    UnionConstructor(Vec<Expr>),
    Path { head: Box<Expr>, elems: Vec<Expr> },
    Embed(Vec<Expr>),
    Deref(Box<Expr>),
    SizeOf,
    BorrowExpr(BorrowMode, String),
    ArrayIndex(Box<Expr>),
}

#[allow(dead_code)]
enum BorrowMode {
    ReadBorrow,
    WriteBorrow,
}

#[allow(dead_code)]
impl Appearances {
    fn new(consumed: u32, write: u32, read: u32, path: u32) -> Self {
        Appearances { consumed, write, read, path }
    }

    fn partition(count: u32) -> CountResult {
        match count {
            0 => CountResult::Zero,
            1 => CountResult::One,
            _ => CountResult::MoreThanOne,
        }
    }

    fn zero() -> Self {
        Self::new(0, 0, 0, 0)
    }

    fn consumed_once() -> Self {
        Self::new(1, 0, 0, 0)
    }

    fn read_once() -> Self {
        Self::new(0, 0, 1, 0)
    }

    fn write_once() -> Self {
        Self::new(0, 1, 0, 0)
    }

    fn path_once() -> Self {
        Self::new(0, 0, 0, 1)
    }

    fn merge(&self, other: &Appearances) -> Self {
        Appearances {
            consumed: self.consumed + other.consumed,
            write: self.write + other.write,
            read: self.read + other.read,
            path: self.path + other.path,
        }
    }

    fn merge_list(appearances: Vec<Appearances>) -> Self {
        appearances.into_iter().fold(Self::zero(), |acc, x| acc.merge(&x))
    }
}


#[derive(Debug, Clone)]
struct StateTbl {
    vars: HashMap<String, VarState>,
}

impl StateTbl {
    // Initialize with an empty state table
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    // Example of updating the state table
    fn update_state(&mut self, var: & str, state: VarState) {
        self.vars.insert(var.to_string(), state);
    }

    // Remove a variable from the state table
    fn remove_entry(&mut self, var: &str) {
        self.vars.remove(var);
    }

    // Retrieve a variable's state
    fn get_state(&self, var: &str) -> Option<&VarState> {
        self.vars.get(var)
    }
}

/* 
// Placeholder function signatures (implementation required)
fn check_expr(expr: &str, state_tbl: &mut StateTbl) {
    // Implementation needed 
}

fn count(vars: &[String], state_tbl: &StateTbl) -> usize {
    // Implementation needed 
    0
}

*/



#[allow(dead_code)]
#[allow(unused_variables)]
/* 
fn count(name: &str, expr: &Expr) -> Appearances {
    match expr {
        Expr::NilConstant | Expr::BoolConstant(_) | Expr::IntConstant(_) | Expr::FloatConstant(_) | Expr::StringConstant(_) | Expr::ConstVar | Expr::FunVar | Expr::SizeOf => 
            Appearances::zero(),
        Expr::ParamVar(var_name) | Expr::LocalVar(var_name) => 
            if var_name == name { Appearances::consumed_once() } else { Appearances::zero() },
        Expr::Funcall(func, args) | Expr::MethodCall(func, args) | Expr::VarMethodCall(args) | Expr::FptrCall(func, args) | Expr::Embed(args) => 
            args.iter().map(|arg| count(name, arg)).collect::<Vec<_>>().into_iter().fold(Appearances::zero(), |acc, x| acc.merge(&x)),
        Expr::Cast(e, _) | Expr::Negation(e) | Expr::Deref(e) => 
            count(name, e),
        Expr::Comparison(lhs, rhs) | Expr::Conjunction(lhs, rhs) | Expr::Disjunction(lhs, rhs) => 
            count(name, lhs).merge(&count(name, rhs)),
        Expr::IfExpression(cond, then_expr, else_expr) => 
            count(name, cond).merge(&count(name, then_expr)).merge(&count(name, else_expr)),
        Expr::RecordConstructor(args) | Expr::UnionConstructor(args) => 
            args.iter().map(|arg| count(name, arg)).collect::<Vec<_>>().into_iter().fold(Appearances::zero(), |acc, x| acc.merge(&x)),
        Expr::Path { head, elems } => {
            let head_apps = count(name, head);
            let elems_apps = elems.iter().map(|elem| count(name, elem)).collect::<Vec<_>>().into_iter().fold(Appearances::zero(), |acc, x| acc.merge(&x));
            head_apps.merge(&elems_apps)
        },
        Expr::BorrowExpr(mode, var_name) => 
            if var_name == name {
                match mode {
                    BorrowMode::ReadBorrow => Appearances::read_once(),
                    BorrowMode::WriteBorrow => Appearances::write_once(),
                }
            } else {
                Appearances::zero()
            },
        Expr::ArrayIndex(e) => 
            count(name, e),
    }
}
*/
fn count(name: &str, expr: &Expr) -> Appearances {
    match expr {
        Expr::NilConstant | Expr::BoolConstant(_) | Expr::IntConstant(_) | Expr::FloatConstant(_) | Expr::StringConstant(_) | Expr::ConstVar | Expr::FunVar | Expr::SizeOf => 
            Appearances::zero(),

        Expr::ParamVar(var_name) | Expr::LocalVar(var_name) => 
            if var_name == name { Appearances::consumed_once() } else { Appearances::zero() },

        Expr::Funcall(func, args) | Expr::MethodCall(func, args) | Expr::FptrCall(func, args) => 
            args.iter().map(|arg| count(name, arg)).fold(Appearances::zero(), |acc, x| acc.merge(&x)),

        Expr::VarMethodCall(args) | Expr::Embed(args) => 
            args.iter().map(|arg| count(name, arg)).fold(Appearances::zero(), |acc, x| acc.merge(&x)),

        Expr::Cast(e, _) | Expr::Negation(e) | Expr::Deref(e) => 
            count(name, e),

        Expr::Comparison(lhs, rhs) | Expr::Conjunction(lhs, rhs) | Expr::Disjunction(lhs, rhs) => 
            count(name, lhs).merge(&count(name, rhs)),

        Expr::IfExpression(cond, then_expr, else_expr) => 
            count(name, cond).merge(&count(name, then_expr)).merge(&count(name, else_expr)),

        Expr::RecordConstructor(args) | Expr::UnionConstructor(args) => 
            args.iter().map(|arg| count(name, arg)).fold(Appearances::zero(), |acc, x| acc.merge(&x)),

        Expr::Path { head, elems } => {
            let head_apps = count(name, head);
            let elems_apps = elems.iter().map(|elem| count(name, elem)).fold(Appearances::zero(), |acc, x| acc.merge(&x));
            head_apps.merge(&elems_apps)
        },

        Expr::BorrowExpr(mode, var_name) => 
            if var_name == name {
                match mode {
                    BorrowMode::ReadBorrow => Appearances::read_once(),
                    BorrowMode::WriteBorrow => Appearances::write_once(),
                }
            } else {
                Appearances::zero()
            },

        Expr::ArrayIndex(e) => 
            count(name, e),
    }
}







#[allow(dead_code)]
#[allow(unreachable_patterns)]
//fn check_var_in_expr(state_tbl: &mut StateTbl, depth: u32, name: &str, expr: &str) -> Result<StateTbl, String> {
fn check_var_in_expr(state_tbl: &mut StateTbl, _depth: u32, name: &str, expr: &Expr) -> Result<StateTbl, LinearityError> {
    let apps = count(name, expr); // Assume count function implementation
    let Appearances { consumed, write, read, path } = apps;

    let state = state_tbl.get_state(name).unwrap_or(&VarState::Unconsumed); // Assume default state

    match (state, Appearances::partition(consumed), Appearances::partition(write), Appearances::partition(read), Appearances::partition(path)) {
        /*(        State            Consumed           WBorrow             RBorrow           Path      )
        (* ------------------|-------------------|-----------------|------------------|----------------)*/
        (VarState::Unconsumed, CountResult::Zero, CountResult::Zero,                _,                 _) => Ok(state_tbl.clone()),
        (VarState::Unconsumed, CountResult::Zero, CountResult::One, CountResult::Zero, CountResult::Zero) => Ok(state_tbl.clone()),
        (VarState::Unconsumed, CountResult::Zero, CountResult::MoreThanOne,         _,                 _) =>
            Err(LinearityError::BorrowedMutMoreThanOnce { variable: name.to_string() }),
        (VarState::Unconsumed, CountResult::One,                         _,         _,                 _) =>
            Err(LinearityError::ConsumedAndUsed { variable: name.to_string() }),
        (VarState::Unconsumed, CountResult::MoreThanOne,                 _,          _,                 _) =>
            Err(LinearityError::ConsumedMoreThanOnce { variable: name.to_string() }),
        (VarState::Borrowed,                  _,                         _,          _,                 _) =>
            Err(LinearityError::ReadBorrowedAndUsed { variable: name.to_string() }),
        (VarState::BorrowedMut,               _,                         _,          _,                 _) =>
            Err(LinearityError::WriteBorrowedAndUsed { variable: name.to_string() }),
        (VarState::Consumed,                  _,                         _,          _,                 _) =>
            Err(LinearityError::AlreadyConsumedAndUsed { variable: name.to_string() }),
        _ => Err(LinearityError::UnhandledStateOrCount { variable: name.to_string() }),
    }
}


#[allow(dead_code)]
#[allow(unused_variables)]
fn consume_once(state_tbl: &mut StateTbl, depth: u32, name: &str) -> Result<StateTbl, String> {
    // TODO Implement the logic to consume a variable once, updating the state table and handling depth
    state_tbl.update_state(name, VarState::Consumed);
    Ok(state_tbl.clone())
}


// Do nothing implementation of linearity check
#[allow(unused_variables)]
pub fn linearity_check_program(program_ir: &ProgramBody, session: &Session) ->  Result<String, LinearityError> {
    let mut linearity_table = StateTbl::new();
    linearity_table.update_state("x", VarState::Unconsumed);
    linearity_table.update_state("y", VarState::Consumed);
    linearity_table.update_state("z", VarState::Borrowed);
    linearity_table.update_state("w", VarState::BorrowedMut);
    
    linearity_table.remove_entry("x");
    let state = linearity_table.get_state("y");
    Ok("OK".to_string())
}


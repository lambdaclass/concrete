use std::collections::HashMap;

use concrete_session::Session;


use self::errors::LinearityError;
pub mod errors;


use concrete_ir::ProgramBody;

#[derive(Debug, Clone, Copy)]
struct Appearances {
    consumed: u32,
    write: u32,
    read: u32,
    path: u32,
}


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

#[allow(dead_code)]
impl Appearances {
    fn partition(count: u32) -> CountResult {
        match count {
            0 => CountResult::Zero,
            1 => CountResult::One,
            _ => CountResult::MoreThanOne,
        }
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
fn count(name: &str, expr: &str) -> Appearances {
    // TODO implement
    Appearances { consumed: 0, write: 0, read: 0, path: 0 }
}



#[allow(dead_code)]
#[allow(unreachable_patterns)]
//fn check_var_in_expr(state_tbl: &mut StateTbl, depth: u32, name: &str, expr: &str) -> Result<StateTbl, String> {
fn check_var_in_expr(state_tbl: &mut StateTbl, _depth: u32, name: &str, expr: &str) -> Result<StateTbl, LinearityError> {
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


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
    fn update_state(&mut self, var: String, state: VarState) {
        self.vars.insert(var, state);
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
fn check_var_in_expr(state_tbl: &mut StateTbl, depth: u32, name: &str, expr: &str) -> Result<StateTbl, String> {
    let apps = count(name, expr); // Assume count function implementation
    let Appearances { consumed, write, read, path } = apps;

    let state = state_tbl.get_state(name).unwrap_or(&VarState::Unconsumed); // Assume default state

    match (state, Appearances::partition(consumed), Appearances::partition(write), Appearances::partition(read), Appearances::partition(path)) {
        (VarState::Unconsumed, CountResult::Zero, CountResult::Zero, _, _) => Ok(state_tbl.clone()),
        (VarState::Unconsumed, CountResult::Zero, CountResult::One, CountResult::Zero, CountResult::Zero) => Ok(state_tbl.clone()),
        (VarState::Unconsumed, CountResult::Zero, CountResult::One, _, _) => Err("Error: Borrowed mutably and used".to_string()),
        (VarState::Unconsumed, CountResult::Zero, CountResult::MoreThanOne, _, _) => Err("Error: Borrowed mutably more than once".to_string()),
        (VarState::Unconsumed, CountResult::One, CountResult::Zero, CountResult::Zero, CountResult::Zero) => consume_once(state_tbl, depth, name),
        (VarState::Unconsumed, CountResult::One, _, _, _) => Err("Error: Consumed and something else".to_string()),
        (VarState::Unconsumed, CountResult::MoreThanOne, _, _, _) => Err("Error: Consumed more than once".to_string()),
        (VarState::Borrowed, CountResult::Zero, CountResult::Zero, CountResult::Zero, _) => Ok(state_tbl.clone()),
        (VarState::Borrowed, _, _, _, _) => Err("Error: Read borrowed and something else".to_string()),
        (VarState::BorrowedMut, CountResult::Zero, CountResult::Zero, CountResult::Zero, CountResult::Zero) => Ok(state_tbl.clone()),
        (VarState::BorrowedMut, _, _, _, _) => Err("Error: Write borrowed and used".to_string()),
        (VarState::Consumed, CountResult::Zero, CountResult::Zero, CountResult::Zero, CountResult::Zero) => Ok(state_tbl.clone()),
        (VarState::Consumed, _, _, _, _) => Err("Error: Already consumed".to_string()),
        _ => Err("Unhandled state or appearance count".to_string()),
    }
}


#[allow(unused_variables)]
fn consume_once(state_tbl: &mut StateTbl, depth: u32, name: &str) -> Result<StateTbl, String> {
    // TODO Implement the logic to consume a variable once, updating the state table and handling depth
    state_tbl.update_state(name.to_string(), VarState::Consumed);
    Ok(state_tbl.clone())
}


// Do nothing implementation of linearity check
#[allow(unused_variables)]
pub fn linearity_check_program(program_ir: &ProgramBody, session: &Session) ->  Result<String, LinearityError> {
    let mut linearity_table = StateTbl::new();
    linearity_table.update_state("x".to_string(), VarState::Unconsumed);
    linearity_table.update_state("y".to_string(), VarState::Consumed);
    linearity_table.update_state("z".to_string(), VarState::Borrowed);
    linearity_table.update_state("w".to_string(), VarState::BorrowedMut);
    
    linearity_table.remove_entry("x");
    let state = linearity_table.get_state("y");
    Ok("OK".to_string())
}


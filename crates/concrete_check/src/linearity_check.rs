use std::collections::HashMap;

use concrete_session::Session;
//pub mod linearityError;


use self::errors::LinearityError;
pub mod errors;


use concrete_ir::ProgramBody;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VarState {
    Available,
    Consumed,
    Borrowed,
    BorrowedMut,
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
    // Implementation needed based on OCaml logic
}

fn count(vars: &[String], state_tbl: &StateTbl) -> usize {
    // Implementation needed based on OCaml logic
    0
}

*/

// Do nothing implementation of linearity check
#[allow(unused_variables)]
pub fn linearity_check_program(program_ir: &ProgramBody, session: &Session) ->  Result<String, LinearityError> {
    let mut linearity_table = StateTbl::new();
    linearity_table.update_state("x".to_string(), VarState::Available);
    linearity_table.update_state("y".to_string(), VarState::Consumed);
    linearity_table.update_state("z".to_string(), VarState::Borrowed);
    linearity_table.update_state("w".to_string(), VarState::BorrowedMut);
    
    linearity_table.remove_entry("x");
    let state = linearity_table.get_state("y");
    Ok("OK".to_string())
}


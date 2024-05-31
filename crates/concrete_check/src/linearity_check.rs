use std::collections::HashMap;

use concrete_session::Session;

use self::errors::LinearityError;
pub mod errors;

use concrete_ast::expressions::{Expression, PathOp, StructInitField, ValueExpr};
use concrete_ast::functions::{FunctionDecl, FunctionDef, Param};
use concrete_ast::modules::ModuleDefItem;
use concrete_ast::statements::{AssignStmt, Binding, LetStmt, LetStmtTarget, Statement};
use concrete_ast::Program;
use std::path::PathBuf;

use concrete_ast::types::TypeSpec;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum VarState {
    Unconsumed,
    Consumed,
    _Borrowed,
    _BorrowedMut,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarInfo {
    ty: String, //TODO Define 'Type' as a struct or enum?
    depth: usize,
    state: VarState,
}

#[derive(Debug, Clone, Copy, PartialEq)]
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

// TODO remove. This are structures translated from Austral
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
    Borrow(BorrowMode, String),
    ArrayIndex(Box<Expr>),
}

#[allow(dead_code)]
enum BorrowMode {
    ReadBorrow,
    WriteBorrow,
}

impl Appearances {
    fn new(consumed: u32, write: u32, read: u32, path: u32) -> Self {
        Appearances {
            consumed,
            write,
            read,
            path,
        }
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

    // When borrowed implemented
    fn _read_once() -> Self {
        Self::new(0, 0, 1, 0)
    }

    fn _write_once() -> Self {
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

    fn _merge_list(appearances: Vec<Appearances>) -> Self {
        appearances
            .into_iter()
            .fold(Self::zero(), |acc, x| acc.merge(&x))
    }
}

#[derive(Debug, Clone)]
struct StateTbl {
    vars: HashMap<String, VarInfo>,
}

/// StateTbl is a table that keeps track of the state of variables in a program for doing linearityCheck. The core of algorithm is decision table
impl StateTbl {
    // Initialize with an empty state table
    fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn init(&mut self, vars: Vec<(String, String)>, depth: usize) {
        for (name, ty) in vars {
            self.vars.insert(
                name.to_string(),
                VarInfo {
                    ty: ty.to_string(),
                    depth,
                    state: VarState::Unconsumed,
                },
            );
        }
    }
    // Example of updating the state table
    fn update_info(&mut self, var: &str, info: VarInfo) {
        self.vars.insert(var.to_string(), info);
    }

    // Remove a variable from the state table
    fn _remove_entry(&mut self, var: &str) {
        self.vars.remove(var);
    }

    fn remove_entries(&mut self, vars: Vec<String>) {
        for var in vars {
            self.vars.remove(&var);
        }
    }

    fn get_info_mut(&mut self, var: &str) -> Option<&mut VarInfo> {
        if !self.vars.contains_key(var) {
            self.vars.insert(
                var.to_string(),
                VarInfo {
                    ty: "".to_string(),
                    depth: 0,
                    state: VarState::Unconsumed,
                },
            );
            tracing::debug!(
                "Variable {} not found in state table. Inserting with default state",
                var
            );
        }
        self.vars.get_mut(var)
    }

    fn get_info(&self, var: &str) -> Option<&VarInfo> {
        self.vars.get(var)
    }

    // Retrieve a variable's state
    fn _get_state(&mut self, var: &str) -> Option<&VarState> {
        if let Some(info) = self.get_info(var) {
            Some(&info.state)
        } else {
            None
        }
    }

    // Retrieve a variable's state
    fn update_state(&mut self, var: &str, new_state: &VarState) {
        let info = self.get_info_mut(var);
        if let Some(info) = info {
            //info.state = new_state.clone();
            info.state = *new_state;
        }
    }

    fn get_loop_depth(&mut self, name: &str) -> usize {
        let state = self.get_info(name);
        if let Some(state) = state {
            state.depth
        } else {
            0
        }
    }

    pub fn _tables_are_consistent(&self, other: &StateTbl) -> Result<(), LinearityError> {
        for (key, value) in &self.vars {
            if let Some(other_value) = other.vars.get(key) {
                if value != other_value {
                    return Err(LinearityError::StateInconsistency {
                        message: format!(
                            "Variable '{}' state mismatch: {:?} != {:?}",
                            key, value, other_value
                        ),
                    });
                }
            } else {
                return Err(LinearityError::StateInconsistency {
                    message: format!("Variable '{}' not found in the other table", key),
                });
            }
        }
        // Also check for variables in `other` not present in `self`
        for key in other.vars.keys() {
            if !self.vars.contains_key(key) {
                return Err(LinearityError::StateInconsistency {
                    message: format!("Variable '{}' not found in the first table", key),
                });
            }
        }

        Ok(())
    }
}

struct LinearityChecker {}

#[allow(dead_code)]
#[allow(unused_variables)]
impl LinearityChecker {
    fn new() -> Self {
        LinearityChecker {}
    }

    fn consume_once(
        &self,
        mut state_tbl: StateTbl,
        depth: usize,
        name: &str,
    ) -> Result<StateTbl, LinearityError> {
        let loop_depth = state_tbl.get_loop_depth(name);
        tracing::debug!(
            "Consuming variable: {} depth {} loop_depth {}",
            name,
            depth,
            loop_depth
        );
        if depth == state_tbl.get_loop_depth(name) {
            // Consumed when a variable defined inside the loop is consumed
            state_tbl.update_state(name, &VarState::Consumed);
            tracing::debug!("Consumed variable: {}", name);
            Ok(state_tbl)
        } else {
            Err(LinearityError::ConsumedVariableInLoop {
                variable: name.to_string(),
            })
        }
    }

    fn check_expr(
        &self,
        mut state_tbl: StateTbl,
        depth: usize,
        expr: &Expression,
        context: &str,
    ) -> Result<StateTbl, LinearityError> {
        // Assuming you have a method to get all variable names and types
        //let vars = &mut self.state_tbl.vars;
        //TODO check if we can avoid cloning
        let vars = state_tbl.vars.clone();
        println!("Check_expr vars {:?}", vars);
        for (name, _info) in vars.iter() {
            //self.check_var_in_expr(depth, &name, &info.ty, expr)?;
            state_tbl = self
                .check_var_in_expr(state_tbl, depth, name, expr, context)
                .unwrap();
        }
        Ok(state_tbl)
    }

    fn count_in_statements(&self, name: &str, statements: &[Statement]) -> Appearances {
        statements
            .iter()
            .map(|stmt| self.count_in_statement(name, stmt))
            .fold(Appearances::zero(), |acc, x| acc.merge(&x))
    }

    fn count_in_statement(&self, name: &str, statement: &Statement) -> Appearances {
        match statement {
            Statement::Let(binding) => {
                // Handle let bindings, possibly involving pattern matching
                self.count_in_expression(name, &binding.value)
            }
            Statement::If(if_stmt) => {
                // Process all components of an if expression
                let cond_apps = self.count_in_expression(name, &if_stmt.value);
                let then_apps = self.count_in_statements(name, &if_stmt.contents);
                let else_apps;
                let else_statements = &if_stmt.r#else;
                if let Some(else_statements) = else_statements {
                    else_apps = self.count_in_statements(name, else_statements);
                } else {
                    else_apps = Appearances::zero();
                }
                cond_apps.merge(&then_apps).merge(&else_apps)
            }
            Statement::While(while_expr) => {
                let cond = &while_expr.value;
                let block = &while_expr.contents;
                // Handle while loops
                self.count_in_expression(name, cond)
                    .merge(&self.count_in_statements(name, block))
            }
            Statement::For(for_expr) => {
                // Handle for loops
                //init, cond, post, block
                let init = &for_expr.init;
                let cond = &for_expr.condition;
                let post = &for_expr.post;
                let block = &for_expr.contents;
                let mut apps = Appearances::zero();
                if let Some(init) = init {
                    if let Some(cond) = cond {
                        if let Some(post) = post {
                            apps = self
                                .count_in_let_statements(name, init)
                                .merge(&self.count_in_expression(name, cond))
                                .merge(&self.count_in_assign_statement(name, post))
                                .merge(&self.count_in_statements(name, block))
                        }
                    }
                }
                apps
            }
            Statement::Assign(assign_stmt) => {
                // Handle assignments
                self.count_in_assign_statement(name, assign_stmt)
            }
            Statement::Return(return_stmt) => {
                // Handle return statements
                if let Some(value) = &return_stmt.value {
                    self.count_in_expression(name, value)
                } else {
                    Appearances::zero()
                }
            }
            Statement::FnCall(fn_call_op) => {
                // Process function call arguments
                //fn_call_op.target.iter().map(|arg| self.count_in_path_op(name, arg)).fold(Appearances::zero(), |acc, x| acc.merge(&x));
                fn_call_op
                    .args
                    .iter()
                    .map(|arg| self.count_in_expression(name, arg))
                    .fold(Appearances::zero(), |acc, x| acc.merge(&x))
            }
            Statement::Match(_) => {
                todo!("do not support match statement")
            } //_ => Appearances::zero(),
        }
    }

    fn count_in_assign_statement(&self, name: &str, assign_stmt: &AssignStmt) -> Appearances {
        let AssignStmt {
            target,
            derefs,
            value,
            span,
        } = assign_stmt;
        // Handle assignments
        let ret = self.count_in_path_op(name, target, true);
        ret.merge(&self.count_in_expression(name, value));
        ret
    }

    fn count_in_path_op(&self, name: &str, path_op: &PathOp, lvalue: bool) -> Appearances {
        if name == path_op.first.name {
            if lvalue {
                Appearances::consumed_once()
            } else {
                Appearances::path_once()
            }
        } else {
            Appearances::zero()
        }
    }

    fn count_in_let_statements(&self, name: &str, let_stmt: &LetStmt) -> Appearances {
        let LetStmt {
            is_mutable,
            target,
            value,
            span,
        } = let_stmt;
        self.count_in_expression(name, value)
    }

    fn count_in_expression(&self, name: &str, expr: &Expression) -> Appearances {
        match expr {
            Expression::Value(value_expr, _) => {
                // Handle value expressions, typically constant or simple values
                match value_expr {
                    ValueExpr::Path(path) => {
                        if name == path.first.name {
                            Appearances::path_once()
                            // FIXME
                            // This is a stub implementation. The only call with path is as an lvalue
                            // But we should have a clever implementation.
                            // An approach is to difference when a path is an lvalue or an rvalue, contained in the Path itself
                            //Appearances::consumed_once()
                        } else {
                            Appearances::zero()
                        }
                    }
                    ValueExpr::ConstBool(_, _)
                    | ValueExpr::ConstChar(_, _)
                    | ValueExpr::ConstInt(_, _)
                    | ValueExpr::ConstFloat(_, _)
                    | ValueExpr::ConstStr(_, _) => Appearances::zero(),
                }
            }
            Expression::FnCall(fn_call_op) => {
                // Process function call arguments
                fn_call_op
                    .args
                    .iter()
                    .map(|arg| self.count_in_expression(name, arg))
                    .fold(Appearances::zero(), |acc, x| acc.merge(&x))
            }
            Expression::Match(match_expr) => todo!("do not support match expression"),
            //Match expressions should be implemented as an extension of if expressions
            Expression::If(if_expr) => {
                // Process all components of an if expression
                // TODO review this code. If expressions should be processed counting both branches and comparing them
                let cond_apps = self.count_in_expression(name, &if_expr.value);
                let then_apps = self.count_in_statements(name, &if_expr.contents);
                cond_apps.merge(&then_apps);
                if let Some(else_block) = &if_expr.r#else {
                    let else_apps = self.count_in_statements(name, else_block);
                    cond_apps.merge(&then_apps).merge(&else_apps);
                }
                cond_apps
            }
            Expression::UnaryOp(_, expr) => {
                // Unary operations likely don't change the count but process the inner expression
                self.count_in_expression(name, expr)
            }
            Expression::BinaryOp(left, _, right) => {
                // Handle binary operations by processing both sides
                self.count_in_expression(name, left)
                    .merge(&self.count_in_expression(name, right))
            }
            Expression::StructInit(struct_init_expr) => {
                // Handle struct initialization
                struct_init_expr
                    .fields
                    .values()
                    .map(|expr| self.count_struct_init(name, expr))
                    .fold(Appearances::zero(), |acc, x| acc.merge(&x))
            }
            Expression::ArrayInit(array_init_expr) => {
                // Handle array initializations
                array_init_expr
                    .values
                    .iter()
                    .map(|expr| self.count_in_expression(name, expr))
                    .fold(Appearances::zero(), |acc, x| acc.merge(&x))
            }
            Expression::Deref(expr, _)
            | Expression::AsRef(expr, _, _)
            | Expression::Cast(expr, _, _) => {
                // Deref, AsRef, and Cast are handled by just checking the inner expression
                self.count_in_expression(name, expr)
            } // Add more cases as necessary based on the Expression types you expect
        }
    }

    fn count_struct_init(&self, name: &str, struct_init: &StructInitField) -> Appearances {
        tracing::debug!("Checking struct init: {:?}", struct_init);
        self.count_in_expression(name, &struct_init.value)
    }

    fn check_stmt_let(
        &self,
        mut state_tbl: StateTbl,
        depth: usize,
        binding: &LetStmt,
        context: &str,
    ) -> Result<StateTbl, LinearityError> {
        // Handle let bindings, possibly involving pattern matching
        let LetStmt {
            is_mutable,
            target,
            value,
            span,
        } = binding;
        match target {
            LetStmtTarget::Simple { name, r#type } => {
                match r#type {
                    TypeSpec::Simple {
                        name: variable_type,
                        qualifiers,
                        span,
                    } => {
                        state_tbl.update_info(
                            &name.name,
                            VarInfo {
                                ty: variable_type.name.clone(),
                                depth,
                                state: VarState::Unconsumed,
                            },
                        );
                    }
                    TypeSpec::Generic {
                        name: variable_type,
                        qualifiers,
                        type_params,
                        span,
                    } => {
                        state_tbl.update_info(
                            &name.name,
                            VarInfo {
                                ty: variable_type.name.clone(),
                                depth,
                                state: VarState::Unconsumed,
                            },
                        );
                    }
                    TypeSpec::Array {
                        of_type,
                        size,
                        qualifiers,
                        span,
                    } => {
                        let array_type = "Array<".to_string() + &of_type.get_name() + ">";
                        state_tbl.update_info(
                            &name.name,
                            VarInfo {
                                ty: array_type,
                                depth,
                                state: VarState::Unconsumed,
                            },
                        );
                    }
                }
                self.check_var_in_expr(state_tbl, depth, &name.name, value, context)
            }
            LetStmtTarget::Destructure(bindings) => {
                for binding in bindings {
                    let new_state_tbl = self.check_bindings(&state_tbl, depth, binding);
                    if let Ok(new_state_tbl) = new_state_tbl {
                        state_tbl = new_state_tbl;
                    }
                }
                Ok(state_tbl)
            }
        }
    }

    fn check_bindings(
        &self,
        state_tbl: &StateTbl,
        depth: usize,
        binding: &Binding,
    ) -> Result<StateTbl, LinearityError> {
        // TODO Do something with the bindings
        tracing::debug!("TODO implement Checking bindings: {:?}", binding);
        Ok(state_tbl.clone())
    }

    fn check_function(
        &self,
        mut state_tbl: StateTbl,
        depth: usize,
        function_def: &FunctionDef,
    ) -> Result<StateTbl, LinearityError> {
        let decl = &function_def.decl;
        // Handle function declarations
        let FunctionDecl {
            doc_string,
            generic_params,
            name,
            params,
            ret_type,
            is_extern,
            is_pub,
            attributes,
            span,
        } = decl;
        let errors: Vec<LinearityError> = Vec::new();
        tracing::debug!("Checking function declaration: {:?}", decl);
        let mut params_vec: Vec<(String, String)> = Vec::new();
        let mut params_clean_vec: Vec<String> = Vec::new();
        for param in params {
            let Param { name, r#type } = param;
            let name = name.name.clone();
            let r#type = match r#type {
                TypeSpec::Simple {
                    name: variable_type,
                    qualifiers,
                    span,
                } => variable_type.name.clone(),
                TypeSpec::Generic {
                    name: variable_type,
                    qualifiers,
                    type_params,
                    span,
                } => variable_type.name.clone(),
                TypeSpec::Array {
                    of_type,
                    size,
                    qualifiers,
                    span,
                } => "Array<".to_string() + &of_type.get_name() + ">",
            };
            params_vec.push((name.clone(), r#type));
            let var_clean = name.clone();
            params_clean_vec.push(var_clean);
        }
        state_tbl.init(params_vec, depth);

        //function.decl
        for statement in &function_def.body {
            //tracing::debug!("Checking linearity for function body: {:?}", function.body);
            let stmt_context = format!("{:?}", statement);
            state_tbl = self.check_stmt(state_tbl, 0, statement, &stmt_context)?;
        }
        tracing::debug!(
            "Finished checking linearity for function: {} {:?}",
            function_def.decl.name.name,
            state_tbl
        );
        state_tbl.remove_entries(params_clean_vec);
        //if errors.len() > 0 {
        if !errors.is_empty() {
            //FIXME replace with Vec<LinearityError>
            Err(errors[0].clone())
        } else {
            Ok(state_tbl)
        }
    }

    fn check_stmts(
        &self,
        mut state_tbl: StateTbl,
        depth: usize,
        stmts: &Vec<Statement>,
        context: &str,
    ) -> Result<StateTbl, LinearityError> {
        for stmt in stmts {
            state_tbl = self.check_stmt(state_tbl, depth, stmt, context)?;
        }
        Ok(state_tbl)
    }

    fn check_stmt(
        &self,
        mut state_tbl: StateTbl,
        depth: usize,
        stmt: &Statement,
        context: &str,
    ) -> Result<StateTbl, LinearityError> {
        let mut errors: Vec<LinearityError> = Vec::new();
        match stmt {
            Statement::Let(binding) => {
                // Handle let bindings, possibly involving pattern matching
                self.check_stmt_let(state_tbl, depth, binding, context)
            }
            //Statement::If(cond, then_block, else_block) => {
            Statement::If(if_stmt) => {
                // Handle conditional statements
                state_tbl = self.check_expr(state_tbl, depth, &if_stmt.value, context)?;
                state_tbl = self.check_stmts(state_tbl, depth + 1, &if_stmt.contents, context)?;
                if let Some(else_block) = &if_stmt.r#else {
                    state_tbl = self.check_stmts(state_tbl, depth + 1, else_block, context)?;
                }
                Ok(state_tbl)
            }
            //Statement::While(cond, block) => {
            Statement::While(while_stmt) => {
                // Handle while loops
                state_tbl = self.check_expr(state_tbl, depth, &while_stmt.value, context)?;
                state_tbl =
                    self.check_stmts(state_tbl, depth + 1, &while_stmt.contents, context)?;
                Ok(state_tbl)
            }
            //Statement::For(init, cond, post, block) => {
            Statement::For(for_stmt) => {
                // Handle for loops
                if let Some(init) = &for_stmt.init {
                    state_tbl = self.check_stmt_let(state_tbl, depth, init, context)?;
                }
                if let Some(condition) = &for_stmt.condition {
                    state_tbl = self.check_expr(state_tbl, depth, condition, context)?;
                }
                if let Some(post) = &for_stmt.post {
                    //TODO check assign statement
                    //self.check_stmt_assign(depth, post)?;
                }
                state_tbl = self.check_stmts(state_tbl, depth + 1, &for_stmt.contents, context)?;
                Ok(state_tbl)
            }
            Statement::Assign(assign_stmt) => {
                // Handle assignments
                let AssignStmt {
                    target,
                    derefs,
                    value,
                    span,
                } = assign_stmt;
                tracing::debug!("Checking assignment: {:?}", assign_stmt);
                let target_context = format!("target <{:?}>", target);
                state_tbl = self.check_path_opt(state_tbl, depth, target, &target_context)?;
                let value_context = format!("value <{:?}>", value);
                state_tbl = self.check_expr(state_tbl, depth, value, &value_context)?;

                //state_tbl = self.check_var_in_expr(state_tbl, depth, name, expr, "");
                Ok(state_tbl)
            }
            Statement::Return(return_stmt) => {
                if let Some(return_stmt) = &return_stmt.value {
                    state_tbl = self.check_expr(state_tbl, depth, return_stmt, "return")?;
                }
                // Ensure that all variables are properly consumed
                for (name, var_info) in state_tbl.vars.iter() {
                    match var_info.state {
                        VarState::Consumed => (), // If consumed, no action needed
                        _ => match var_info.ty {
                            // Type::WriteRef(_) | Type::SpanMut(_) => (),  // These can be dropped implicitly
                            _ if self.is_universe_linear_ish(&var_info.ty) => {
                                // Collect error if a variable that needs to be consumed hasn't been
                                errors.push(LinearityError::VariableNotConsumed {
                                    variable: name.clone(),
                                });
                            }
                            _ => (),
                        },
                    }
                }
                if !errors.is_empty() {
                    Err(errors[0].clone())
                } else {
                    Ok(state_tbl)
                }
            }
            Statement::FnCall(fn_call_op) => {
                // Process function call arguments
                for arg in &fn_call_op.args {
                    state_tbl = self.check_expr(state_tbl, depth, arg, context)?;
                }
                Ok(state_tbl)
            }
            Statement::Match(_) => {
                tracing::debug!("Skipping linearity check for statement type: \n{:?}", stmt);
                todo!("Implement linearity checkt for match statement")
            }
        }
    }

    fn check_path_opt(
        &self,
        state_tbl: StateTbl,
        depth: usize,
        path_op: &PathOp,
        context: &str,
    ) -> Result<StateTbl, LinearityError> {
        tracing::debug!("Checking path: {:?}", path_op);
        let var_expression = Expression::Value(
            concrete_ast::expressions::ValueExpr::Path(path_op.clone()),
            path_op.span,
        );
        self.check_var_in_expr(
            state_tbl,
            depth,
            &path_op.first.name,
            &var_expression,
            context,
        )
    }

    fn is_universe_linear_ish(&self, ty: &str) -> bool {
        *ty == *"Linear".to_string()
    }

    fn check_var_in_expr(
        &self,
        state_tbl: StateTbl,
        depth: usize,
        name: &str,
        expr: &Expression,
        context: &str,
    ) -> Result<StateTbl, LinearityError> {
        let info = state_tbl.get_info(name); // Assume default state
        if let Some(info) = info {
            //Only checks Linearity for types of name Linear
            // TODO improve this approach
            if self.is_universe_linear_ish(&info.ty) {
                let state = &info.state;
                let apps = self.count_in_expression(name, expr); // Assume count function implementation
                let Appearances {
                    consumed,
                    write,
                    read,
                    path,
                } = apps;
                //tracing::debug!("Checking variable: {} with state: {:?} and appearances: {:?} in expression {:?}", name, state, apps, expr);
                tracing::debug!(
                    "Checking state_tbl variable: {}: {:?} {:?} \n <<< context {:?} >>> \n << expression {:?} >>",
                    name, info, apps, context, expr
                );
                match (
                    state,
                    Appearances::partition(consumed),
                    Appearances::partition(write),
                    Appearances::partition(read),
                    Appearances::partition(path),
                ) {
                    /*(        State            Consumed           WBorrow             RBorrow           Path      )
                    (* ------------------|-------------------|-----------------|------------------|----------------)*/
                    // Not yet consumed, and at most used through immutable borrows or path reads.
                    (VarState::Unconsumed, CountResult::Zero, CountResult::Zero, _, _) => {
                        Ok(state_tbl)
                    }
                    // Not yet consumed, borrowed mutably once, and nothing else.
                    (
                        VarState::Unconsumed,
                        CountResult::Zero,
                        CountResult::One,
                        CountResult::Zero,
                        CountResult::Zero,
                    ) => Ok(state_tbl),
                    // Not yet consumed, borrowed mutably, then either borrowed immutably or accessed through a path.
                    (VarState::Unconsumed, CountResult::Zero, CountResult::One, _, _) => {
                        Err(LinearityError::BorrowedMutUsed {
                            variable: name.to_string(),
                        })
                    }
                    // Not yet consumed, borrowed mutably more than once.
                    (VarState::Unconsumed, CountResult::Zero, CountResult::MoreThanOne, _, _) => {
                        Err(LinearityError::BorrowedMutMoreThanOnce {
                            variable: name.to_string(),
                        })
                    }
                    // Not yet consumed, consumed once, and nothing else. Valid IF the loop depth matches.
                    (
                        VarState::Unconsumed,
                        CountResult::One,
                        CountResult::Zero,
                        CountResult::Zero,
                        CountResult::Zero,
                    ) => self.consume_once(state_tbl, depth, name),
                    // Not yet consumed, consumed once, then either borrowed or accessed through a path.
                    (VarState::Unconsumed, CountResult::One, _, _, _) => {
                        Err(LinearityError::ConsumedAndUsed {
                            variable: name.to_string(),
                        })
                    }
                    // Not yet consumed, consumed more than once.
                    (VarState::Unconsumed, CountResult::MoreThanOne, _, _, _) => {
                        Err(LinearityError::ConsumedMoreThanOnce {
                            variable: name.to_string(),
                        })
                    }
                    // Read borrowed, and at most accessed through a path.
                    (
                        VarState::_Borrowed,
                        CountResult::Zero,
                        CountResult::Zero,
                        CountResult::Zero,
                        _,
                    ) => Ok(state_tbl),
                    // Read borrowed, and either consumed or borrowed again.
                    (VarState::_Borrowed, _, _, _, _) => Err(LinearityError::ReadBorrowedAndUsed {
                        variable: name.to_string(),
                    }),
                    // Write borrowed, unused.
                    (
                        VarState::_BorrowedMut,
                        CountResult::Zero,
                        CountResult::Zero,
                        CountResult::Zero,
                        CountResult::Zero,
                    ) => Ok(state_tbl),
                    // Write borrowed, used in some way.
                    (VarState::_BorrowedMut, _, _, _, _) => {
                        Err(LinearityError::WriteBorrowedAndUsed {
                            variable: name.to_string(),
                        })
                    }
                    // Already consumed, and unused.
                    (
                        VarState::Consumed,
                        CountResult::Zero,
                        CountResult::Zero,
                        CountResult::Zero,
                        CountResult::Zero,
                    ) => Ok(state_tbl),
                    // Already consumed, and used in some way.
                    (VarState::Consumed, _, _, _, _) => {
                        Err(LinearityError::AlreadyConsumedAndUsed {
                            variable: name.to_string(),
                        })
                    }
                }
            } else {
                //Only checks Linearity for types of name Linear
                Ok(state_tbl)
            }
        } else {
            Err(LinearityError::VariableNotFound {
                variable: name.to_string(),
            })
        }
    }
}

// This is because there is no warranty check_function returned state_tbl is readed once
//#[allow(unused_assignments)]
pub fn linearity_check_program(
    programs: &Vec<(PathBuf, String, Program)>,
    _session: &Session,
) -> Result<String, LinearityError> {
    tracing::debug!("Starting linearity check");
    let checker = LinearityChecker::new();
    for (_path, name, program) in programs {
        tracing::debug!("Checking linearity for program: {}", name);
        for module in &program.modules {
            tracing::debug!("Checking linearity for module: {}", module.name.name);
            for module_content in &module.contents {
                let mut state_tbl = StateTbl::new();
                match module_content {
                    ModuleDefItem::Function(function) => {
                        //tracing::debug!("Checking linearity for function: {:?}", function);
                        //checker.check_function(&function)?;
                        //FIXME check function function.decl
                        state_tbl = checker.check_function(state_tbl, 0, function)?;
                        //checker.linearity_check(&function)?;
                    }
                    ModuleDefItem::FunctionDecl(function_decl) => {
                        tracing::debug!(
                            "Skipping linearity check for FunctionDecl: {:?}",
                            function_decl
                        );
                    }
                    ModuleDefItem::Module(module) => {
                        tracing::debug!("Skipping linearity check for Module: {:?}", module);
                    }
                    ModuleDefItem::Struct(struc) => {
                        //tracing::debug!("Skipping linearity check for Struct: {:?}", module_content);
                        //checker.
                        state_tbl.update_info(
                            &struc.name.name,
                            VarInfo {
                                ty: "Struct".to_string(),
                                depth: 0,
                                state: VarState::Unconsumed,
                            },
                        );
                    }
                    ModuleDefItem::Enum(_) => {
                        tracing::debug!("Skipping linearity check for Enum: {:?}", module_content);
                    }
                    ModuleDefItem::Constant(_) => {
                        tracing::debug!(
                            "Skipping linearity check for Constant: {:?}",
                            module_content
                        );
                    }
                    ModuleDefItem::Union(_) => {
                        tracing::debug!("Skipping linearity check for Uinon: {:?}", module_content);
                    }
                    ModuleDefItem::Type(_) => {
                        tracing::debug!(
                            "Skipping linearity check for module content: {:?}",
                            module_content
                        );
                    } /*_ =>
                      {
                          tracing::debug!("Skipping linearity check for module content: {:?}", module_content);
                          ()
                      },*/
                }
                tracing::debug!(
                    "Finished linearity check for module {} with resulting state_tbl {:?}",
                    module.name.name,
                    state_tbl
                );
            }
        }
    }
    Ok("OK".to_string())
}

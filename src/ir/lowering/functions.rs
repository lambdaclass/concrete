use std::collections::HashMap;

use tracing::{debug, instrument};

use crate::{
    ast::{
        expressions::FnCallOp,
        functions::{FunctionDecl, FunctionDef},
        statements::{self, LetStmtTarget},
        types::TypeDescriptor,
    },
    ir::{
        lowering::{expressions::lower_expression, types::lower_type, Symbol},
        BasicBlock, ConcreteIntrinsic, FnBody, Local, LocalKind, Operand, Place, Span, Terminator,
        TerminatorKind, TyKind,
    },
};

use super::{
    errors::LoweringError,
    ir::{FnIndex, Rvalue, TypeIndex},
    statements::lower_statement,
    FnIrBuilder, IRBuilder,
};

/// Lowers a function if its not yet lowered.
///
/// If the function is generic, `builder.current_generics_map` should contain types for the generics.
#[instrument(level = "debug", skip_all, fields(name = ?func.decl.name.name))]
pub(crate) fn lower_func(
    builder: &mut IRBuilder,
    func: &FunctionDef,
    method_of: Option<TypeIndex>,
) -> Result<FnIndex, LoweringError> {
    debug!("lowering function {:?}", func.decl.name.name);
    let is_intrinsic: Option<ConcreteIntrinsic> = None;
    let module_idx = if let Some(id) = method_of {
        builder
            .type_module_idx
            .get(&id)
            .copied()
            .expect("should exist")
    } else {
        builder.local_module.expect("should exist")
    };

    let mut generic_types = Vec::new();

    // Initially, this is the polymorphic symbol, if its a generic function, the symbol changes to the monomorphic version after
    // id resolution
    let mut symbol = Symbol {
        name: func.decl.name.name.clone(),
        method_of,
        generics: Vec::new(),
    };

    // Find the function id, and if its generic, the monormorphic function id.
    let (poly_fn_id, mono_fn_id) = {
        let symbols = builder.symbols.get(&module_idx).unwrap();

        if let Some(poly_id) = symbols.functions.get(&symbol).copied() {
            if !func.decl.generic_params.is_empty() {
                debug!(
                    "function is generic over {} parameters",
                    func.decl.generic_params.len()
                );

                for generic_param in &func.decl.generic_params {
                    if let Some(ty) = builder.current_generics_map.get(&generic_param.name.name) {
                        generic_types.push(*ty);
                    } else {
                        panic!()
                    }
                }

                // Construct the monomorphized function symbol.
                symbol = Symbol {
                    name: func.decl.name.name.clone(),
                    method_of,
                    generics: generic_types,
                };

                let symbols = builder.symbols.get(&module_idx).unwrap(); // needed for borrowck

                let mono_id = if let Some(id) = symbols.functions.get(&symbol) {
                    *id
                } else {
                    builder.ir.functions.insert(None)
                };
                (poly_id, Some(mono_id))
            } else {
                (poly_id, None)
            }
        } else {
            panic!("fn not found")
        }
    };

    let fn_id = mono_fn_id.unwrap_or(poly_fn_id);

    // Check if this function is already lowered.
    if builder.ir.functions[fn_id].is_some() {
        debug!(
            "function '{}' already lowered",
            &builder.ir.functions[fn_id].as_ref().unwrap().name
        );
        return Ok(fn_id);
    }

    let mut args_ty = Vec::new();

    for arg in &func.decl.params {
        let ty = lower_type(builder, &arg.r#type)?;
        args_ty.push(ty);
    }

    let ret_ty = func
        .decl
        .ret_type
        .as_ref()
        .map(|x| lower_type(builder, x))
        .unwrap_or(Ok(builder.ir.get_unit_ty()))?;

    let mut fn_builder = FnIrBuilder {
        body: FnBody {
            name: if !func.decl.is_extern && func.decl.name.name != "main" {
                builder
                    .get_mangled_name(module_idx, &func.decl.name.name, fn_id)
                    .expect("should get mangled name")
            } else {
                func.decl.name.name.clone()
            },
            args: args_ty.clone(),
            ret_ty,
            is_extern: func.decl.is_extern,
            is_intrinsic,
            basic_blocks: Vec::new(),
            module_idx,
            locals: Vec::new(),
        },
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        ret_local: 0,
        builder,
        local_exists: Default::default(),
    };

    // A extern fn cannot have a body.
    if !func.body.is_empty() && func.decl.is_extern {
        return Err(LoweringError::ExternFnWithBody {
            span: func.span,
            name: func.decl.name.name.clone(),
            path: fn_builder.get_file_path().clone(),
        });
    }

    fn_builder.ret_local = fn_builder.body.locals.len();
    fn_builder.body.locals.push(Local::new(
        None,
        LocalKind::ReturnPointer,
        ret_ty,
        None,
        false,
    ));

    // Add argument locals.
    for (arg, ty) in func.decl.params.iter().zip(args_ty) {
        fn_builder
            .name_to_local
            .insert(arg.name.name.clone(), fn_builder.body.locals.len());
        fn_builder.local_exists.insert(fn_builder.body.locals.len());
        fn_builder.body.locals.push(Local::new(
            Some(arg.name.span),
            LocalKind::Arg,
            ty,
            Some(arg.name.name.clone()),
            false,
        ));
    }

    // Get all top level locals
    for stmt in &func.body {
        get_locals(&mut fn_builder, stmt)?;
    }

    for stmt in &func.body {
        lower_statement(&mut fn_builder, stmt, ret_ty)?;
    }

    let statements = std::mem::take(&mut fn_builder.statements);
    fn_builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: None,
            kind: TerminatorKind::Return,
        }),
    });

    fn_builder.builder.ir.functions[fn_id] = Some(fn_builder.body);
    builder.ir.modules[module_idx].functions.insert(fn_id);

    Ok(fn_id)
}

#[instrument(level = "debug", skip_all)]
pub(crate) fn lower_fn_call(
    fn_builder: &mut FnIrBuilder,
    info: &FnCallOp,
    self_value: Option<(Place, TypeIndex)>,
    method_idx: Option<TypeIndex>, // in case its a method
) -> Result<(Rvalue, TypeIndex, Span), LoweringError> {
    debug!("lowering fn call");
    let (poly_fn_id, mono_fn_id) = fn_builder.get_id_for_fn_call(info, method_idx)?;

    let target_fn_decl = fn_builder
        .builder
        .bodies
        .functions
        .get(&poly_fn_id)
        .map(|x| &x.decl)
        .or_else(|| fn_builder.builder.bodies.functions_decls.get(&poly_fn_id))
        .unwrap()
        .clone();

    let old_generic_map = fn_builder.builder.current_generics_map.clone();

    for (generic_ty, generic_param) in info
        .generics
        .iter()
        .zip(target_fn_decl.generic_params.iter())
    {
        let ty = lower_type(
            fn_builder.builder,
            &TypeDescriptor::Type {
                name: generic_ty.clone(),
                span: generic_ty.span,
            },
        )?;
        fn_builder
            .builder
            .current_generics_map
            .insert(generic_param.name.name.clone(), ty);
    }

    if info.generics.len() != target_fn_decl.generic_params.len() {
        // todo: this check will be removed/refactored when we have inference for generics from the arguments used.
        return Err(LoweringError::GenericCountMismatch {
            span: info.span,
            found: info.generics.len(),
            needs: target_fn_decl.generic_params.len(),
            path: fn_builder.get_file_path().clone(),
        });
    }

    let mut args_ty = Vec::new();

    if let Some((_, self_ty)) = self_value {
        fn_builder.builder.self_ty = Some(self_ty);
    }

    for param in &target_fn_decl.params {
        let ty = lower_type(fn_builder.builder, &param.r#type)?;
        args_ty.push(ty);
    }

    let return_ty = if let Some(ret_ty) = target_fn_decl.ret_type {
        lower_type(fn_builder.builder, &ret_ty)?
    } else {
        fn_builder.builder.ir.get_unit_ty()
    };

    if args_ty.len()
        != info.args.len() + {
            if self_value.is_some() {
                1
            } else {
                0
            }
        }
    {
        return Err(LoweringError::CallParamCountMismatch {
            span: info.span,
            found: info.args.len(),
            needs: args_ty.len(),
            path: fn_builder.get_file_path().clone(),
        });
    }

    let mut args = Vec::new();

    let mut args_ty_iter = args_ty.into_iter();

    // Add the self value if there is one.
    if let Some((arg, _arg_ty)) = self_value {
        // Here arg_ty is the type without references.
        // We should use the type from the fn sig to know if it needs a reference.
        let expected_type_idx = args_ty_iter.next().expect("self ty should be there");
        let expected_ty = fn_builder.builder.get_type(expected_type_idx);
        match expected_ty {
            TyKind::Ref(_, mutability) => {
                args.push(Rvalue::Ref(*mutability, arg.clone()));
            }
            _ => {
                args.push(Rvalue::Use(Operand::Place(arg.clone())));
            }
        }
    }

    for (arg, arg_type_idx) in info.args.iter().zip(args_ty_iter) {
        let (rvalue, rvalue_type_idx, rvalue_span) =
            lower_expression(fn_builder, arg, Some(arg_type_idx))?;
        let arg_ty = fn_builder.builder.get_type(arg_type_idx);
        let rvalue_ty = fn_builder.builder.get_type(rvalue_type_idx);

        if !rvalue_ty.is_equal(arg_ty, &fn_builder.builder.ir) {
            return Err(LoweringError::UnexpectedType {
                found_span: rvalue_span,
                found: rvalue_ty.display(&fn_builder.builder.ir).unwrap(),
                expected: arg_ty.display(&fn_builder.builder.ir).unwrap(),
                expected_span: Some(rvalue_span),
                path: fn_builder.get_file_path().clone(),
            });
        }

        args.push(rvalue);
    }

    let dest_local = fn_builder.add_local(Local::temp(return_ty));

    let dest_place = Place {
        local: dest_local,
        projection: Vec::new(),
    };

    let target_block = fn_builder.body.basic_blocks.len() + 1;

    // todo: check if function is diverging such as exit().
    let kind = TerminatorKind::Call {
        func: mono_fn_id.unwrap_or(poly_fn_id),
        args,
        destination: dest_place.clone(),
        target: Some(target_block),
    };

    let statements = std::mem::take(&mut fn_builder.statements);
    fn_builder.body.basic_blocks.push(BasicBlock {
        statements,
        terminator: Box::new(Terminator {
            span: Some(info.target.span),
            kind,
        }),
    });

    fn_builder.builder.self_ty = None;

    fn_builder.builder.current_generics_map = old_generic_map;

    Ok((
        Rvalue::Use(Operand::Place(dest_place)),
        return_ty,
        info.span,
    ))
}

#[instrument(level = "debug", skip_all)]
pub(crate) fn lower_func_decl(
    builder: &mut IRBuilder,
    func: &FunctionDecl,
) -> Result<FnIndex, LoweringError> {
    let is_intrinsic: Option<ConcreteIntrinsic> = None;
    let module_idx = builder.local_module.expect("should exist");

    let mut generic_types = Vec::new();

    // Initially, this is the polymorphic symbol, if its a generic function, the symbol changes to the monomorphic version after
    // id resolution
    let mut symbol = Symbol {
        name: func.name.name.clone(),
        method_of: None,
        generics: Vec::new(),
    };

    // Find the function id, and if its generic, the monormorphic function id.
    let (poly_fn_id, mono_fn_id) = {
        let symbols = builder.symbols.get(&module_idx).unwrap();

        if let Some(poly_id) = symbols.functions.get(&symbol).copied() {
            if !func.generic_params.is_empty() {
                debug!(
                    "function is generic over {} parameters",
                    func.generic_params.len()
                );

                for generic_param in &func.generic_params {
                    if let Some(ty) = builder
                        .current_generics_map
                        .get(&generic_param.name.name)
                        .copied()
                    {
                        generic_types.push(ty);
                    } else {
                        // todo: should error?
                    }
                }

                // Construct the monomorphized function symbol.
                symbol = Symbol {
                    name: func.name.name.clone(),
                    method_of: None,
                    generics: generic_types,
                };

                let symbols = builder.symbols.get(&module_idx).unwrap(); // needed for borrowck

                let mono_id = if let Some(id) = symbols.functions.get(&symbol) {
                    *id
                } else {
                    builder.ir.functions.insert(None)
                };
                (poly_id, Some(mono_id))
            } else {
                (poly_id, None)
            }
        } else {
            panic!("fn not found")
        }
    };

    let fn_id = mono_fn_id.unwrap_or(poly_fn_id);

    // Check if this function is already lowered.
    if builder.ir.functions[fn_id].is_some() {
        debug!(
            "function '{}' already lowered",
            &builder.ir.functions[fn_id].as_ref().unwrap().name
        );
        return Ok(fn_id);
    }

    let mut args_ty = Vec::new();

    for arg in &func.params {
        let ty = lower_type(builder, &arg.r#type)?;
        args_ty.push(ty);
    }

    let ret_ty = func
        .ret_type
        .as_ref()
        .map(|x| lower_type(builder, x))
        .unwrap_or(Ok(builder.ir.get_unit_ty()))?;

    let fn_builder = FnIrBuilder {
        body: FnBody {
            name: if !func.is_extern && func.name.name != "main" {
                builder
                    .get_mangled_name(module_idx, &func.name.name, fn_id)
                    .expect("should get mangled name")
            } else {
                func.name.name.clone()
            },
            args: args_ty.clone(),
            ret_ty,
            is_extern: func.is_extern,
            is_intrinsic,
            basic_blocks: Vec::new(),
            module_idx,
            locals: Vec::new(),
        },
        name_to_local: HashMap::new(),
        statements: Vec::new(),
        ret_local: 0,
        builder,
        local_exists: Default::default(),
    };

    fn_builder.builder.ir.functions[fn_id] = Some(fn_builder.body);
    builder.ir.modules[module_idx].functions.insert(fn_id);

    Ok(fn_id)
}

/// Get and map names to locals.
///
/// Should be called on each new scope.
pub(crate) fn get_locals(
    builder: &mut FnIrBuilder,
    stmt: &crate::ast::statements::Statement,
) -> Result<(), LoweringError> {
    match stmt {
        statements::Statement::Assign(_assign_stmt) => {}
        statements::Statement::Match(_match_expr) => todo!(),
        statements::Statement::For(info) => {
            if let Some(info) = &info.init {
                match &info.target {
                    LetStmtTarget::Simple { id: name, r#type } => {
                        let ty = lower_type(builder.builder, r#type)?;
                        builder
                            .name_to_local
                            .insert(name.name.clone(), builder.body.locals.len());
                        builder.body.locals.push(Local::new(
                            Some(name.span),
                            LocalKind::Temp,
                            ty,
                            Some(name.name.clone()),
                            info.is_mutable,
                        ));
                    }
                    LetStmtTarget::Destructure(_) => todo!(),
                }
            }
        }
        // handled in the lower function
        statements::Statement::If(_info) => {}
        statements::Statement::Let(info) => match &info.target {
            LetStmtTarget::Simple { id: name, r#type } => {
                let ty = lower_type(builder.builder, r#type)?;
                builder
                    .name_to_local
                    .insert(name.name.clone(), builder.body.locals.len());
                builder.body.locals.push(Local::new(
                    Some(name.span),
                    LocalKind::Temp,
                    ty,
                    Some(name.name.clone()),
                    info.is_mutable,
                ));
            }
            LetStmtTarget::Destructure(_) => todo!(),
        },
        statements::Statement::Return(_return_stmt) => {}
        statements::Statement::While(_while_stmt) => {}
        statements::Statement::FnCall(_fn_call_op) => {}
        statements::Statement::PathOp(_path_op) => {}
    }

    Ok(())
}

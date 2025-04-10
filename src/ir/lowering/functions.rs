use std::collections::HashMap;

use tracing::{debug, instrument};

use crate::{
    ast::{
        common::GenericParam,
        expressions::FnCallOp,
        functions::{FunctionDecl, FunctionDef},
        statements::{self, LetStmtTarget},
    },
    ir::{
        BasicBlock, ConcreteIntrinsic, Function, Local, LocalKind, ModuleIndex, Operand, Place,
        Span, Terminator, TerminatorKind, Type,
        lowering::{
            expressions::{find_expression_type, lower_expression},
            types::lower_type,
        },
    },
};

use super::{
    FnIrBuilder, IRBuilder,
    errors::LoweringError,
    ir::{FnIndex, Rvalue, TypeIndex},
    statements::lower_statement,
    symbols::FnSymbol,
    traits::TraitIdx,
};

/// Lowers a function or method if its not yet lowered.
///
/// If the function is generic, `builder.context.generics_mapping` should contain types for the generics.
#[instrument(level = "debug", skip_all, fields(name = ?func.decl.name.name, mod_id))]
pub(crate) fn lower_func(
    builder: &mut IRBuilder,
    func: &FunctionDef,
    method_of: Option<TypeIndex>,
    trait_method_of: Option<TraitIdx>,
) -> Result<FnIndex, LoweringError> {
    if !func.decl.generic_params.is_empty() || !builder.context.generics_mapping.is_empty() {
        return lower_generic_func(builder, func, method_of, trait_method_of);
    }

    debug!("lowering function {:?}", func.decl.name.name);

    // Get the module id of this function/method
    // This is needed incase this is a method in a impl block, if its imported the `lower_import` doesn't
    // bring them into the target module functions struct,
    // rather we have to get the module and find the method in the original module.

    let old_self_ty = builder.context.self_ty;
    let module_idx = if let Some(id) = method_of {
        builder.context.self_ty = Some(id);
        builder
            .type_to_module
            .get(&id)
            .copied()
            .expect("should exist")
    } else {
        builder.get_current_module_idx()
    };

    tracing::span::Span::current().record("mod_id", module_idx.to_idx());

    let symbol = FnSymbol {
        name: func.decl.name.name.clone(),
        method_of,
        trait_method_of,
    };

    let (fn_id, fn_module_idx) = builder
        .symbols
        .get(&module_idx)
        .unwrap()
        .functions
        .get(&symbol)
        .copied()
        .unwrap();

    // Check it it's already lowered.
    if builder.ir.functions[fn_id].is_some() {
        debug!(
            "function '{}' already lowered",
            &builder.ir.functions[fn_id].as_ref().unwrap().name
        );
        builder.context.self_ty = old_self_ty;
        return Ok(fn_id);
    }

    builder.enter_module_context(fn_module_idx);

    lower_func_body(builder, fn_id, func)?;

    builder.leave_module_context();

    builder.context.self_ty = old_self_ty;

    Ok(fn_id)
}

pub(crate) fn lower_generic_func(
    builder: &mut IRBuilder,
    func: &FunctionDef,
    method_of: Option<TypeIndex>,
    trait_method_of: Option<TraitIdx>,
) -> Result<FnIndex, LoweringError> {
    debug!(
        "lowering generic function {:?}, generic map: {:?}",
        func.decl.name.name, builder.context.generics_mapping
    );

    let old_self_ty = builder.context.self_ty;

    let module_idx = if let Some(id) = method_of {
        builder.context.self_ty = Some(id);
        builder
            .type_to_module
            .get(&id)
            .copied()
            .expect("should exist")
    } else {
        builder.get_current_module_idx()
    };

    let (mono_id, fn_module_idx) = get_or_create_function_mono_idx(
        builder,
        &func.decl.name.name,
        &func.decl.generic_params,
        method_of,
        trait_method_of,
        module_idx,
        None,
    )?;

    if builder.ir.functions[mono_id].is_some() {
        debug!(
            "function '{}' already lowered",
            &builder.ir.functions[mono_id].as_ref().unwrap().name
        );
        builder.context.self_ty = old_self_ty;
        return Ok(mono_id);
    }

    builder.enter_module_context(fn_module_idx);

    lower_func_body(builder, mono_id, func)?;

    builder.leave_module_context();

    builder.context.self_ty = old_self_ty;

    Ok(mono_id)
}

pub(crate) fn lower_func_body(
    builder: &mut IRBuilder,
    fn_id: FnIndex,
    func: &FunctionDef,
) -> Result<(), LoweringError> {
    let is_intrinsic: Option<ConcreteIntrinsic> = None;

    let module_idx = builder.get_current_module_idx();

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
        body: Function {
            name: if !func.decl.is_extern && func.decl.name.name != "main" {
                builder
                    .get_mangled_name(module_idx, &func.decl.name.name, fn_id)
                    .expect("should get mangled name")
            } else {
                func.decl.name.name.clone()
            },
            debug_name: if !func.decl.is_extern && func.decl.name.name != "main" {
                builder.get_debug_name(module_idx, &func.decl.name.name)
            } else {
                Some(func.decl.name.name.clone())
            },
            args: args_ty.clone(),
            ret_ty,
            is_extern: func.decl.is_extern,
            is_intrinsic,
            basic_blocks: Vec::new(),
            module_idx,
            locals: Vec::new(),
        },
        fn_id,
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

    for attr in &func.decl.attributes {
        if attr.name.as_str() == "test" {
            // TODO: check its a valid test function, i.e: no arguments, returns a i32.
            if builder.context.add_tests {
                builder.ir.tests.push(fn_id);
            }
            break;
        }
    }

    Ok(())
}

/// Gets or creates a monomorphized function id for the given adt.
pub(crate) fn get_or_create_function_mono_idx(
    builder: &mut IRBuilder,
    name: &str,
    generics: &[GenericParam],
    method_of: Option<TypeIndex>,
    trait_method_of: Option<TraitIdx>,
    module_idx: ModuleIndex,
    // In case its already lowered (needed for fn calls)
    generic_types: Option<Vec<TypeIndex>>,
) -> Result<(FnIndex, ModuleIndex), LoweringError> {
    // If the method of type is generic this may be the mono id, so we need the poly id to find the methods.
    let mut poly_method_of = method_of;
    if let Some(inner_method_of) = method_of {
        if let Some(x) = builder.mono_type_to_poly.get(&inner_method_of) {
            poly_method_of = Some(*x)
        }
    }

    let sym = FnSymbol {
        name: name.to_string(),
        method_of: poly_method_of,
        trait_method_of,
    };

    let generic_types = if let Some(generic_types) = generic_types {
        generic_types
    } else {
        builder.lower_generic_params(generics)?
    };
    let mut mono_sym = sym.monomorphize(&generic_types);
    mono_sym.method_of = method_of;

    // Already lowered.
    if let Some(id) = builder.symbols[&module_idx]
        .monomorphized_functions
        .get(&mono_sym)
    {
        return Ok(*id);
    }

    let (poly_id, fn_module_idx) = *builder.symbols[&module_idx]
        .functions
        .get(&sym)
        .expect("function not found");

    let id = builder.ir.functions.insert(None);

    if let Some(func_decl) = builder.bodies.functions_decls.get(&poly_id).cloned() {
        builder.bodies.functions_decls.insert(id, func_decl);
    }

    if let Some(func_def) = builder.bodies.functions.get(&poly_id).cloned() {
        builder.bodies.functions.insert(id, func_def);
    }

    builder
        .symbols
        .get_mut(&fn_module_idx)
        .unwrap()
        .monomorphized_functions
        .insert(mono_sym, (id, fn_module_idx));

    Ok((id, fn_module_idx))
}

/// Lowers a function or method call.
///
/// If the function is generic, and hasn't been monomorphized yet, it gets lowered with the given generic types.
#[instrument(level = "debug", skip_all, fields(name = ?info.target.name))]
pub(crate) fn lower_fn_call(
    fn_builder: &mut FnIrBuilder,
    info: &FnCallOp,
    self_value: Option<(Place, TypeIndex)>,
    method_idx: Option<TypeIndex>, // in case its a method
) -> Result<(Rvalue, TypeIndex, Span), LoweringError> {
    debug!("lowering fn call");

    let module_idx = fn_builder.builder.get_path_module_idx(&info.path)?;

    // Temporarly set the local module to the import module in case the function is not yet
    // lowered and needs to be.
    fn_builder.enter_module_context(module_idx);

    let fn_id = fn_builder.get_id_for_fn_call(info, method_idx)?;

    fn_builder.leave_module_context();

    // Get the function declaration to inspect its types.
    let target_fn_decl = fn_builder
        .builder
        .bodies
        .functions
        .get(&fn_id)
        .map(|x| x.decl.clone())
        .or_else(|| {
            fn_builder
                .builder
                .bodies
                .functions_decls
                .get(&fn_id)
                .cloned()
        })
        .unwrap()
        .clone();

    // Enter a new scope for generics.
    let old_generic_map = fn_builder.builder.context.generics_mapping.clone();

    // Check parameter count is correct first, makes things simpler.
    if target_fn_decl.params.len() != info.args.len() + { if self_value.is_some() { 1 } else { 0 } }
    {
        return Err(LoweringError::CallParamCountMismatch {
            span: info.span,
            found: info.args.len(),
            needs: target_fn_decl.params.len() - if self_value.is_some() { 1 } else { 0 },
            path: fn_builder.get_file_path().clone(),
        });
    }

    // Save the generics info.
    if !info.generics.is_empty() {
        fn_builder
            .builder
            .add_generic_params(&info.generics, &target_fn_decl.generic_params)?;

        if info.generics.len() != target_fn_decl.generic_params.len() {
            return Err(LoweringError::GenericCountMismatch {
                span: info.span,
                found: info.generics.len(),
                needs: target_fn_decl.generic_params.len(),
                path: fn_builder.get_file_path().clone(),
            });
        }
    }

    // TODO: bounds checks

    let mut args_ty = Vec::new();

    // Set self_ty if there is one.
    // Used in lower_type.
    if let Some((_, self_ty)) = self_value {
        fn_builder.builder.context.self_ty = Some(self_ty);
    }

    if info.generics.is_empty() {
        // Guess the generic parameter types based on the arguments
        // We already know args count match with the target declaration.

        let generics: HashMap<String, GenericParam> = target_fn_decl
            .generic_params
            .iter()
            .map(|x| (x.name.name.clone(), x.clone()))
            .collect();

        let target_fn_param_start_idx = if self_value.is_some() { 1 } else { 0 };

        for (i, param) in info.args.iter().enumerate() {
            if let Some(name) = target_fn_decl.params[target_fn_param_start_idx + i]
                .r#type
                .get_name()
            {
                if let Some(generic) = generics.get(&name) {
                    let infer_ty = find_expression_type(fn_builder, param)?.expect("need to infer");
                    fn_builder
                        .builder
                        .context
                        .generics_mapping
                        .insert(name.clone(), infer_ty);

                    for bound in &generic.bounds {
                        if let Some(check_trait) = fn_builder
                            .builder
                            .trait_db
                            .get_trait_by_name(&bound.name.name, module_idx)
                        {
                            let trait_generics = Vec::new(); // TODO: implement trait generics here
                            if !fn_builder.builder.trait_db.type_implements_trait(
                                infer_ty,
                                check_trait,
                                &trait_generics,
                            ) {
                                todo!("type doesnt implement trait error here")
                            }
                        } else {
                            todo!("trait not found error here")
                        }
                    }
                }
            }
            let ty = lower_type(
                fn_builder.builder,
                &target_fn_decl.params[target_fn_param_start_idx + i].r#type,
            )?;
            args_ty.push(ty);
        }
    } else {
        // Lower the param types, skipping self.
        for param in target_fn_decl
            .params
            .iter()
            .skip(if self_value.is_some() { 1 } else { 0 })
        {
            let ty = lower_type(fn_builder.builder, &param.r#type)?;
            args_ty.push(ty);
        }
    }

    // Lower the return type.
    let return_ty = if let Some(ret_ty) = &target_fn_decl.ret_type {
        lower_type(fn_builder.builder, ret_ty)?
    } else {
        fn_builder.builder.ir.get_unit_ty()
    };

    let mut args = Vec::new();

    // Add the self value if there is one.
    if let Some((arg, _self_arg_ty)) = self_value {
        // Here self_arg_ty is the type without references.
        // We should use the type from the fn sig to know if it needs a reference.
        let expected_self_arg_ty_idx =
            lower_type(fn_builder.builder, &target_fn_decl.params[0].r#type)?;
        let expected_ty = fn_builder.builder.get_type(expected_self_arg_ty_idx);
        match expected_ty {
            Type::Ref(_, mutability) => {
                args.push(Rvalue::Ref(*mutability, arg.clone()));
            }
            _ => {
                args.push(Rvalue::Use(Operand::Place(arg.clone())));
            }
        }
    }

    // Lower the argument expressions.
    for (arg, arg_type_idx) in info.args.iter().zip(args_ty.into_iter()) {
        let (rvalue, rvalue_type_idx, rvalue_span) =
            lower_expression(fn_builder, arg, Some(arg_type_idx))?;
        let arg_ty = fn_builder.builder.get_type(arg_type_idx);
        let rvalue_ty = fn_builder.builder.get_type(rvalue_type_idx);

        if !rvalue_ty.is_equal(arg_ty, &fn_builder.builder.ir) {
            return Err(LoweringError::UnexpectedType {
                found_span: rvalue_span,
                found: fn_builder.builder.display_typename(rvalue_type_idx),
                expected: fn_builder.builder.display_typename(arg_type_idx),
                expected_span: Some(rvalue_span),
                path: fn_builder.get_file_path().clone(),
            });
        }

        args.push(rvalue);
    }

    // Add a local for the returned value.
    let dest_local = fn_builder.add_local(Local::temp(return_ty));

    let dest_place = Place {
        local: dest_local,
        projection: Vec::new(),
    };

    let target_block = fn_builder.body.basic_blocks.len() + 1;

    // todo: check if function is diverging such as exit().
    let kind = TerminatorKind::Call {
        func: fn_id,
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

    fn_builder.builder.context.self_ty = None;

    fn_builder.builder.context.generics_mapping = old_generic_map;

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
    let mut is_intrinsic: Option<ConcreteIntrinsic> = None;

    let module_idx = builder.get_current_module_idx();

    let generic_types = builder.lower_generic_params(&func.generic_params)?;

    for attr in &func.attributes {
        match attr.name.as_str() {
            "intrinsic" => {
                let value = attr.value.as_ref().unwrap();
                match value.as_str() {
                    "sizeof" => {
                        is_intrinsic =
                            Some(ConcreteIntrinsic::SizeOf(*generic_types.first().unwrap()));
                    }
                    "alignof" => {
                        is_intrinsic =
                            Some(ConcreteIntrinsic::AlignOf(*generic_types.first().unwrap()));
                    }
                    _ => {
                        debug!("Unknown intrinsic attribute {:?}", attr);
                    }
                }
            }
            _ => {
                debug!("Unknown attribute {:?}", attr);
            }
        }
    }

    let fn_id = if func.generic_params.is_empty() {
        let symbol = FnSymbol {
            name: func.name.name.clone(),
            method_of: None,
            trait_method_of: None,
        };

        builder
            .symbols
            .get(&module_idx)
            .unwrap()
            .functions
            .get(&symbol)
            .unwrap()
            .0
    } else {
        let (mono_id, _) = get_or_create_function_mono_idx(
            builder,
            &func.name.name,
            &func.generic_params,
            None,
            None,
            module_idx,
            None,
        )?;

        mono_id
    };

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

    let function = Function {
        name: if !func.is_extern && func.name.name != "main" {
            builder
                .get_mangled_name(module_idx, &func.name.name, fn_id)
                .expect("should get mangled name")
        } else {
            func.name.name.clone()
        },
        debug_name: if !func.is_extern && func.name.name != "main" {
            builder.get_debug_name(module_idx, &func.name.name)
        } else {
            Some(func.name.name.clone())
        },
        args: args_ty.clone(),
        ret_ty,
        is_extern: func.is_extern,
        is_intrinsic,
        basic_blocks: Vec::new(),
        module_idx,
        locals: Vec::new(),
    };

    builder.ir.functions[fn_id] = Some(function);
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
        statements::Statement::Match(_match_expr) => {}
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

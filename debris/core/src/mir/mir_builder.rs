use std::{collections::HashSet, iter, rc::Rc};

use debris_common::{CodeRef, Ident, Span, SpecialIdent};
use itertools::{EitherOrBoth, Itertools};
use rustc_hash::FxHashMap;

use crate::{
    class::{Class, ClassKind, ClassRef},
    debris_object::ValidPayload,
    error::{LangError, LangErrorKind, Result},
    hir::{
        hir_nodes::{
            HirBlock, HirConditionalBranch, HirConstValue, HirControlFlow, HirDeclarationMode,
            HirExpression, HirFormatStringMember, HirFunction, HirFunctionCall, HirImport,
            HirInfiniteLoop, HirModule, HirObject, HirStatement, HirStruct,
            HirStructInitialization, HirTupleInitialization, HirTypePattern,
            HirVariableInitialization, HirVariablePattern, HirVariableUpdate,
        },
        IdentifierPath,
    },
    llir::utils::ItemId,
    namespace::NamespaceEntry,
    objects::{
        obj_bool_static::ObjStaticBool,
        obj_class::HasClass,
        obj_format_string::{FormatStringComponent, ObjFormatString},
        obj_function::{CompilerFunction, FunctionFlags, FunctionParameters, ObjFunction},
        obj_int_static::ObjStaticInt,
        obj_module::{ModuleFactory, ObjModule},
        obj_native_function::{
            FunctionParameterDefinition, ObjNativeFunction, ObjNativeFunctionSignature,
        },
        obj_null::ObjNull,
        obj_string::ObjString,
        obj_struct::{ObjStruct, Struct},
        obj_struct_object::ObjStructObject,
        obj_tuple_object::{ObjTupleObject, Tuple, TupleRef},
    },
    CompileContext, Namespace, ObjectRef, TypePattern,
};

use super::{
    mir_context::AccessedProperty,
    mir_nodes::{MirBranchIf, MirJumpLocation, MirReturnValue},
    ContextId, ContextKind, ControlFlowMode, Mir, MirContext, MirContextInfo, MirGotoContext,
    MirNode, MirUpdateValue, MirValue, NamespaceArena,
};

#[derive(Debug)]
pub struct CachedFunctionSignature {
    pub parameters: Vec<FunctionParameterDefinition>,
    pub return_type: TypePattern,
    block_id: usize,
}

/// Visits the hir and creates a mir from it
#[derive(Debug)]
pub struct MirBuilder<'a, 'ctx> {
    mir: &'a mut Mir<'ctx>,
    compile_context: &'ctx CompileContext,
    context_stack: ContextStack,
    /// A list of all modules that are imported
    hir_modules: &'a [HirModule],
    /// Cache for all function definitions that were already visited
    visited_functions: FxHashMap<Span, Rc<CachedFunctionSignature>>,
    hir_function_blocks: Vec<&'a HirBlock>,
    /// The first visited function gets marked as the main function
    pub main_context: Option<ContextId>,
    /// Contexts which got scheduled to run every tick
    pub ticking_contexts: Vec<ContextId>,
    /// The global, empty context which only contains modules
    global_context: ContextId,
}

impl<'a> MirBuilder<'a, '_> {
    /// Entry point for visiting the hir
    pub fn visit(&mut self, block: &'a HirBlock) -> Result<MirValue> {
        self.visit_block(block)
    }

    fn visit_object(&mut self, object: &'a HirObject) -> Result<MirValue> {
        match object {
            HirObject::Function(func) => self.visit_function(func),
            HirObject::Struct(val) => self.visit_struct(val),
            HirObject::Module(module) => self.visit_module(module),
        }
    }

    fn visit_struct(&mut self, hir_struct: &'a HirStruct) -> Result<MirValue> {
        let ident = self.context().get_ident(&hir_struct.ident);

        let fields = hir_struct
            .properties
            .iter()
            .map(|decl| {
                Ok((
                    self.context().get_ident(&decl.ident),
                    self.get_type_pattern(&decl.datatype)?,
                ))
            })
            .collect::<Result<_>>()?;

        let context_id = self.add_context(hir_struct.span, ContextKind::Struct);
        let obj = {
            let strukt = Struct {
                ident: ident.clone(),
                fields,
                properties: context_id.as_inner(),
            };
            let obj_struct = ObjStruct::new(Rc::new(strukt));

            let obj = MirValue::from(obj_struct.into_object(self.compile_context));
            // Adds the struct to its own context, so that methods can access it
            self.context_info()
                .add_unique_value(ident.clone(), obj.clone(), hir_struct.span)?;

            for obj in &hir_struct.objects {
                self.visit_object(obj)?;
            }
            self.pop_context();
            obj
        };
        let context = self.mir.contexts.get(context_id);
        assert!(
            context.nodes.is_empty(),
            "Struct bodies must be evaluated at compile time!"
        );

        self.context_info()
            .add_unique_value(ident, obj.clone(), hir_struct.span)?;

        Ok(obj)
    }

    fn visit_module(&mut self, module: &'a HirModule) -> Result<MirValue> {
        let context_id =
            self.add_context_after(self.global_context, module.block.span, ContextKind::Block);
        self.visit_block_local(&module.block)?;
        self.pop_context();

        // Modules are mainly intended to provide objects,
        // but they *are* allowed to emit nodes.
        // If that is the case here, then call that context
        if !self.mir.contexts.get(context_id).nodes.is_empty() {
            self.call_context(context_id, module.span);
        }

        // ToDo: intern idents
        let object_map = self
            .mir
            .contexts
            .get(context_id)
            .namespace(&self.mir.namespaces)
            .iter()
            .filter_map(|(ident, value)| match value {
                MirValue::Concrete(obj) => Some((ident.clone(), obj.clone())),
                MirValue::Template { .. } => None,
            })
            .collect();

        let ident = self.context().get_ident(&module.ident);
        let module_obj = ObjModule::with_members(ident.clone(), object_map);
        let object = MirValue::Concrete(module_obj.into_object(self.compile_context));
        self.context_info()
            .add_unique_value(ident, object, module.ident.span)?;

        Ok(MirValue::null(self.compile_context))
    }

    fn visit_block(&mut self, block: &'a HirBlock) -> Result<MirValue> {
        let context_id = self.add_context(block.span, ContextKind::Block);
        let result = self.visit_block_local(block)?;
        self.pop_context();

        self.call_context(context_id, block.span);

        Ok(result)
    }

    fn visit_import(&mut self, import: &'a HirImport) -> Result<MirValue> {
        let id = import.id;
        let module = &self.hir_modules[id];
        self.visit_module(module)
    }

    fn visit_control_flow(&mut self, control_flow: &'a HirControlFlow) -> Result<MirValue> {
        let control_mode = control_flow.kind.into();

        let jump_location = self.context_stack.jump_location_for(control_mode);
        if jump_location.is_none() {
            return Err(LangError::new(
                LangErrorKind::InvalidControlFlow {
                    mode: control_flow.kind.into(),
                },
                control_flow.span,
            )
            .into());
        }

        // Update return values
        let (value, span) = if let Some(expr) = &control_flow.expression {
            let span = expr.span();
            (self.visit_expression(&expr)?, span)
        } else {
            (MirValue::null(self.compile_context), control_flow.span)
        };

        let context_id = self
            .context_stack
            .iter()
            .find(|(id, _span)| {
                self.mir
                    .contexts
                    .get(*id)
                    .kind
                    .matches_control_flow(control_flow.kind)
            })
            .unwrap()
            .0;

        // Special case for continue, because that statement should not return anything
        if !control_mode.exits_block() {
            value.assert_type(
                TypePattern::Class(self.compile_context.type_ctx().null().class.clone()),
                control_flow.span,
                None,
            )?;
        } else {
            // Otherwise the control flow exits the block, so update the return value
            let return_index = self.update_return_value(context_id, value, span)?;
            self.push(MirNode::ReturnValue(MirReturnValue {
                return_index,
                context_id,
            }));
        }

        self.context_mut().control_flow = ControlFlowMode::from(control_flow.kind);
        Ok(MirValue::null(self.compile_context))
    }

    fn visit_function(&mut self, function: &'a HirFunction) -> Result<MirValue> {
        // Get the data about this function
        let visited_function = match self.visited_functions.get(&function.span) {
            Some(visited_function) => visited_function,
            None => {
                // The pattern of values that are allowed to be returned from this
                let result_pattern = match &function.return_type {
                    // ToDo: enable actual paths instead of truncating to the first element
                    Some(return_type) => self.get_type_pattern(return_type)?,
                    None => ObjNull::class(self.compile_context).into(),
                };

                // Converts the hir parameters into a vector of `FunctionParameterDefinition`
                let parameters = function
                    .parameters
                    .iter()
                    .map(|decl| {
                        Ok(FunctionParameterDefinition {
                            expected_type: self.get_type_pattern(&decl.typ)?,
                            name: self.context().get_ident(&decl.ident),
                            span: decl.span,
                        })
                    })
                    .collect::<Result<Vec<_>>>()?;

                self.hir_function_blocks.push(&function.block);
                self.visited_functions.insert(
                    function.span,
                    Rc::new(CachedFunctionSignature {
                        parameters,
                        return_type: result_pattern,
                        block_id: self.hir_function_blocks.len() - 1,
                    }),
                );
                self.visited_functions.get(&function.span).unwrap()
            }
        };

        let function_value = MirValue::Concrete(
            ObjNativeFunctionSignature::new(
                self.compile_context.get_unique_id(),
                function.span,
                function.return_type_span(),
                self.current_context(), // ToDo: Separate into const and non-const scope
                visited_function.parameters.as_slice(),
                visited_function.return_type.clone(),
            )
            .into_object(self.compile_context),
        );

        let ident = self.context().get_ident(&function.ident);
        // Add the final function to the namespace
        self.context_info()
            .add_unique_value(ident, function_value, function.span)?;

        Ok(MirValue::null(self.compile_context))
    }

    fn visit_conditional_branch(&mut self, branch: &'a HirConditionalBranch) -> Result<MirValue> {
        // Get the condition, whose value is not yet known - just the type
        let condition = self.visit_expression(&branch.condition)?;
        if !condition.class().kind.is_bool() {
            return Err(LangError::new(
                LangErrorKind::ExpectedBoolean {
                    got: condition.class().clone(),
                },
                branch.condition.span(),
            )
            .into());
        }

        let context_kind = if condition.class().kind.runtime_encodable() {
            ContextKind::RuntimeConditionalBlock
        } else {
            ContextKind::ComptimeConditionalBlock
        };

        // Visit the positive block
        let pos_branch = self.add_context(branch.block_positive.span, context_kind);
        let mut pos_value = self.visit_block_local(&branch.block_positive)?;

        // If the condition is not comptime, the return_value must not be comptime either
        if condition.class().kind.runtime_encodable() && !pos_value.class().kind.runtime_encodable()
        {
            pos_value = self.promote_runtime(pos_value, branch.block_positive.last_item_span())?;
        }

        // // Copy the value, since we don't want to alias another variable
        // if let Some((class, id)) = pos_value.template() {
        //     pos_value = self.try_clone(class, id, branch.block_positive.last_item_span())?;
        // };
        self.pop_context();

        // Visit the negative block
        let (neg_branch, neg_value) = if let Some(neg_branch) = branch.block_negative.as_deref() {
            let context_id = self.add_context(neg_branch.span, context_kind);

            let mut else_result = self.visit_block_local(neg_branch)?;
            // If the condition is not comptime, the alternative return_value must not be comptime either
            if condition.class().kind.runtime_encodable()
                && !else_result.class().kind.runtime_encodable()
            {
                else_result =
                    self.promote_runtime(else_result, branch.block_positive.last_item_span())?;
            }
            self.pop_context();

            (context_id, else_result)
        } else {
            let context_id = self.add_context(Span::empty(), context_kind);
            self.pop_context();
            (context_id, MirValue::null(self.compile_context))
        };

        // Asserts that both blocks have the same type
        pos_value.assert_type_exact(
            neg_value.class(),
            branch.block_positive.last_item_span(),
            branch
                .block_negative
                .as_deref()
                .map(|block| block.last_item_span()),
        )?;

        // Since both the negative and the positive result must unify, copy the target address from the positive result
        // To the positive result of the negative result
        let pos_template = self
            .mir
            .contexts
            .get(pos_branch)
            .return_values
            .template
            .clone();
        let result = match &pos_template {
            Some((value, _)) => value.clone(),
            None => MirValue::null(self.compile_context),
        };
        self.mir.contexts.get_mut(neg_branch).return_values.template = pos_template;

        self.push(MirNode::BranchIf(MirBranchIf {
            span: branch.span,
            pos_branch,
            neg_branch,
            condition,
        }));

        // Set a jump location here, don't automatically run since that
        // is done in both of the branches
        let jump_location = self.context_mut().next_jump_location();
        self.push(MirNode::JumpLocation(MirJumpLocation {
            index: jump_location,
        }));

        let default_next = (self.context().id, jump_location);

        // Set the successor contexts for both blocks. If none, then normal control flow
        // is used
        for branch_id in [pos_branch, neg_branch].iter() {
            let branch_next = self.get_jump_target(*branch_id, default_next).unwrap();

            let context = self.mir.context_info(*branch_id).context;
            context.nodes.push(MirNode::GotoContext(MirGotoContext {
                context_id: branch_next.0,
                block_id: branch_next.1,
                span: branch.span,
            }));
        }

        // The result should now be equivalent to the result_else, because of the mem_copy
        Ok(result)
    }

    fn visit_struct_initialization(
        &mut self,
        struct_instantiation: &'a HirStructInitialization,
    ) -> Result<MirValue> {
        let strukt_obj = self
            .context_info()
            .resolve_path(&struct_instantiation.accessor)?
            .value;
        strukt_obj.assert_type(
            TypePattern::Class(ObjStruct::class(self.compile_context)),
            struct_instantiation.accessor.span(),
            None,
        )?;
        let strukt_obj = strukt_obj
            .expect_concrete("Structs are always concrete")
            .clone();
        let strukt = strukt_obj
            .downcast_payload::<ObjStruct>()
            .expect("It was already verified that this is a struct");
        let namespace = self
            .arena_mut()
            .insert_with(|index| Namespace::new(index.into(), None));

        let mut required_parameters: HashSet<&Ident> = strukt.fields.keys().collect();
        for (ident, value) in &struct_instantiation.values {
            let span = ident.span.until(value.span());
            let ident = self.context().get_ident(ident);

            let did_exist = required_parameters.remove(&ident);
            if !did_exist {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedStructInitializer {
                        ident,
                        strukt: self
                            .context()
                            .get_ident(struct_instantiation.accessor.last()),
                        available: required_parameters.into_iter().cloned().collect(),
                    },
                    span,
                )
                .into());
            }

            let mut value = self.visit_expression(value)?;
            let required_type = strukt.fields.get(&ident).expect("Verified to exist");
            let is_correct_type = required_type.matches(&value.class());
            // If the type is incorrect, try to promote it, otherwise error out
            if !is_correct_type {
                if let Ok(new_value) = self.promote_runtime(value.clone(), span) {
                    value = new_value
                }

                let is_correct_type = required_type.matches(&value.class());
                // If the type is still wrong, throw an error
                if !is_correct_type {
                    return Err(LangError::new(
                        LangErrorKind::UnexpectedType {
                            got: value.class().clone(),
                            expected: required_type.clone(),
                            declared: None, // Could be some actually
                        },
                        span,
                    )
                    .into());
                }
            }

            // Struct values should be templates
            let value = if let Some(concrete) = value.concrete() {
                if !concrete.class.kind.typ().should_be_const() {
                    self.add_mutable_value(concrete)
                } else {
                    value
                }
            } else {
                value
            };

            // Pass variables by value
            let value = self.try_clone_if_variable(value, span)?;

            // Make sure that the value gets treated like any other variable
            if let MirValue::Template { class: _, id } = &value {
                self.context_info().declare_as_variable(*id, span);
            }
            self.arena_mut()
                .get_mut(namespace)
                .add_object(ident, NamespaceEntry::Variable { span, value });
        }

        if !required_parameters.is_empty() {
            return Err(LangError::new(
                LangErrorKind::MissingStructInitializer {
                    missing: required_parameters.into_iter().cloned().collect(),
                    strukt: self
                        .context()
                        .get_ident(&struct_instantiation.accessor.last()),
                },
                struct_instantiation.span,
            )
            .into());
        }

        let struct_object =
            ObjStructObject::new(self.arena_mut(), strukt.struct_ref.clone(), namespace);
        let obj = struct_object.into_object(self.compile_context);

        Ok(obj.into())
    }

    fn visit_tuple_initialization(
        &mut self,
        tuple_initialization: &'a HirTupleInitialization,
    ) -> Result<MirValue> {
        let namespace_idx = self
            .arena_mut()
            .insert_with(|index| Namespace::new(index.into(), None));

        let mut types = Vec::with_capacity(tuple_initialization.values.len());
        for (index, expression) in tuple_initialization.values.iter().enumerate() {
            let value = self.visit_expression(expression)?;
            // Pass tuple values into the tuple by value, not by reference
            let value = self.try_clone_if_variable(value, expression.span())?;

            types.push(TypePattern::Class(value.class().clone()));
            self.arena_mut().get_mut(namespace_idx).add_object(
                Ident::Index(index),
                NamespaceEntry::Variable {
                    span: expression.span(),
                    value,
                },
            );
        }

        let tuple = TupleRef::from(Tuple::from(types));
        let tuple_obj = ObjTupleObject::new(self.arena_mut(), tuple, namespace_idx);
        Ok(tuple_obj.into_object(self.compile_context).into())
    }

    fn visit_infinite_loop(&mut self, infinite_loop: &'a HirInfiniteLoop) -> Result<MirValue> {
        // Set a jump target for when to break from the loop
        let after_loop_id = self.context_mut().next_jump_location();
        self.context_stack.push_jump_location(
            ControlFlowMode::Break,
            self.context().id,
            after_loop_id,
        );

        // Evaluate loop body
        let context_id = self.add_context(infinite_loop.span, ContextKind::Loop);
        self.context_stack
            .push_jump_location(ControlFlowMode::Continue, context_id, 0);
        let result = self.visit_block_local(&infinite_loop.block)?;

        // Either jump back to the top or break from the loop
        let (next_context, next_block) = if self.context().control_flow.is_normal() {
            (self.context().id, 0)
        } else {
            self.context_stack
                .jump_location_for(self.context().control_flow)
                .unwrap()
        };

        self.push(MirNode::GotoContext(MirGotoContext {
            context_id: next_context,
            block_id: next_block,
            span: infinite_loop.span,
        }));
        self.context_stack
            .pop_jump_location(ControlFlowMode::Continue);
        self.pop_context();

        self.context_stack.pop_jump_location(ControlFlowMode::Break);
        self.call_context(context_id, infinite_loop.span);
        self.push(MirNode::JumpLocation(MirJumpLocation {
            index: after_loop_id,
        }));

        Ok(result)
    }

    fn visit_statement(&mut self, statement: &'a HirStatement) -> Result<MirValue> {
        // If a return target is already set, then this statement
        // comes after a control-flow statement, which is illegal.
        if !self.context().control_flow.is_normal() {
            return Err(LangError::new(LangErrorKind::UnreachableCode, statement.span()).into());
        }

        match statement {
            HirStatement::VariableDecl(declaration) => self.visit_variable_declaration(declaration),
            HirStatement::VariableUpdate(update) => self.visit_variable_update(update),
            HirStatement::FunctionCall(call) => self.visit_function_call(call),
            HirStatement::Import(import) => self.visit_import(import),
            HirStatement::ControlFlow(control_flow) => self.visit_control_flow(control_flow),
            HirStatement::InfiniteLoop(inf_loop) => self.visit_infinite_loop(inf_loop),
            HirStatement::Block(block) => {
                self.visit_block(block)?;
                Ok(MirValue::null(self.compile_context))
            }
            HirStatement::ConditonalBranch(branch) => {
                self.visit_conditional_branch(branch)?;
                Ok(MirValue::null(self.compile_context))
            }
        }
    }

    fn visit_expression(&mut self, expression: &'a HirExpression) -> Result<MirValue> {
        match expression {
            HirExpression::Value(const_value) => self.visit_const_value(const_value),
            HirExpression::Variable(spanned_ident) => self
                .context_info()
                .get_from_spanned_ident(spanned_ident)
                .map(|value| value.clone()),
            HirExpression::Path(path) => Ok(self.context_info().resolve_path(path)?.value),
            HirExpression::BinaryOperation {
                lhs,
                rhs,
                operation,
            } => {
                // Get the correct function object
                // The Type is solely determined by the lhs value
                let lhs = self.visit_expression(lhs)?;
                let rhs = self.visit_expression(rhs)?;

                let object = lhs
                    .get_property(self.arena(), &operation.operator.get_special_ident().into())
                    .ok_or_else(|| {
                        LangError::new(
                            LangErrorKind::UnexpectedOperator {
                                operator: operation.operator.get_special_ident(),
                                lhs: lhs.class().clone(),
                                rhs: rhs.class().clone(),
                            },
                            operation.span,
                        )
                    })?
                    .expect_concrete("Functions are always concrete")
                    .clone();

                let (return_value, node) = self.context_info().register_function_call(
                    object,
                    vec![lhs, rhs],
                    None,
                    operation.span,
                )?;

                self.push(node);
                Ok(return_value)
            }
            HirExpression::UnaryOperation {
                operation,
                value: _,
            } => {
                // Note: this feature is blocked by the precedence handler.
                // Right now, unary operators are parsed with the wrong precedence
                Err(LangError::new(
                    LangErrorKind::NotYetImplemented {
                        msg: "Unary operations are not yet implemented".to_string(),
                    },
                    operation.span,
                )
                .into())
            }
            HirExpression::Block(block) => self.visit_block(block),
            HirExpression::FunctionCall(function_call) => self.visit_function_call(function_call),
            HirExpression::ConditionalBranch(branch) => self.visit_conditional_branch(branch),
            HirExpression::StructInitialization(struct_initialization) => {
                self.visit_struct_initialization(struct_initialization)
            }
            HirExpression::TupleInitialization(tuple_initialization) => {
                self.visit_tuple_initialization(tuple_initialization)
            }
            HirExpression::InfiniteLoop(inf_loop) => self.visit_infinite_loop(inf_loop),
        }
    }

    fn visit_function_call(&mut self, function_call: &'a HirFunctionCall) -> Result<MirValue> {
        let parent = function_call
            .accessor
            .as_ref()
            .map(|expr| self.visit_expression(&expr))
            .transpose()?;

        let ident = self.context().get_ident(&function_call.ident);
        let value = match &parent {
            Some(parent) => parent.get_property(self.arena(), &ident).ok_or_else(|| {
                println!("{:?}", parent.class());
                LangError::new(
                    LangErrorKind::MissingProperty {
                        parent: parent.class().clone(),
                        property: ident,
                        similar: vec![],
                    },
                    function_call.ident.span,
                )
            })?,
            None => self
                .context_info()
                .get_from_spanned_ident(&function_call.ident)?
                .clone(),
        };

        // If the object is not yet known, there is no way to call it
        let object = match value {
            MirValue::Concrete(function) => function,
            MirValue::Template { id: _, class } => {
                if class.kind.is_function() {
                    return Err(LangError::new(
                        LangErrorKind::NotYetImplemented {
                            msg: "Dependent functions".to_string(),
                        },
                        function_call.ident.span,
                    )
                    .into());
                }
                return Err(LangError::new(
                    LangErrorKind::UnexpectedType {
                        declared: None,
                        expected: ObjFunction::class(self.compile_context).into(),
                        got: class,
                    },
                    function_call.parameters_span,
                )
                .into());
            }
        };

        let parameters = function_call
            .parameters
            .iter()
            .map(|expr| {
                let value = self.visit_expression(expr)?;
                // Copy the value because functions should not mutate the passed
                // arguments if they are cloneable
                let cloned_value = self.try_clone_if_variable(value, expr.span())?;
                Ok(cloned_value)
            })
            .collect::<Result<Vec<_>>>()?;
        self.call_function(object, parent, parameters, function_call.span)
    }

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &'a HirVariableInitialization,
    ) -> Result<MirValue> {
        let mut value = self.visit_expression(&variable_declaration.value)?;
        value = self.try_clone_if_variable(value, variable_declaration.span)?;

        self.assign_to_pattern(
            &variable_declaration.pattern,
            value,
            variable_declaration.value.span(),
            PatternAssignmentMode::Assignment(variable_declaration.mode),
        )?;
        Ok(MirValue::null(&self.compile_context))
    }

    fn visit_variable_update(
        &mut self,
        variable_update: &'a HirVariableUpdate,
    ) -> Result<MirValue> {
        let value = self.visit_expression(&variable_update.value)?;
        let value = self.try_clone_if_variable(value, variable_update.span)?;
        self.assign_to_pattern(
            &variable_update.pattern,
            value,
            variable_update.pattern.span(),
            PatternAssignmentMode::Update,
        )?;
        Ok(MirValue::null(self.compile_context))
    }

    fn visit_const_value(&mut self, const_value: &'a HirConstValue) -> Result<MirValue> {
        Ok(match const_value {
            HirConstValue::Integer { span: _, value } => ObjStaticInt::new(*value)
                .into_object(self.compile_context)
                .into(),
            HirConstValue::Bool { value, .. } => ObjStaticBool::from(*value)
                .into_object(self.compile_context)
                .into(),
            HirConstValue::Fixed { span: _, value: _ } => unimplemented!("Fixed point numbers"),
            HirConstValue::String { span: _, value } => ObjString::new(value.clone())
                .into_object(self.compile_context)
                .into(),
            HirConstValue::FormatString { span: _, value } => ObjFormatString::new(
                value
                    .iter()
                    .map(|member| match member {
                        HirFormatStringMember::String(string) => {
                            Ok(FormatStringComponent::String(string.clone()))
                        }
                        HirFormatStringMember::Variable(var) => {
                            let value = self.context_info().get_from_spanned_ident(var)?;
                            Ok(FormatStringComponent::Value(value.clone()))
                        }
                    })
                    .collect::<Result<Vec<_>>>()?,
            )
            .into_object(self.compile_context)
            .into(),
        })
    }
}

impl<'a, 'ctx> MirBuilder<'a, 'ctx> {
    pub fn new(
        mir: &'a mut Mir<'ctx>,
        hir_modules: &'a [HirModule],
        modules: &[ModuleFactory],
        compile_context: &'ctx CompileContext,
        code: CodeRef<'ctx>,
    ) -> Self {
        // The global context which contains the imported modules
        let mut global_context = MirContext::new(
            compile_context,
            &mut mir.namespaces,
            None,
            code.get_span(),
            ContextKind::Block,
        );

        for module_factory in modules {
            let module = module_factory.call(&compile_context);
            if module_factory.import_members() {
                global_context.register_members(&mut mir.namespaces, module);
            } else {
                global_context.register(&mut mir.namespaces, module);
            }
        }

        let global_context_id = global_context.id;
        mir.add_context(global_context);
        let context_stack = ContextStack {
            current_context: global_context_id,
            current_span: Span::empty(),
            stack: Vec::new(),
            jump_locations: Vec::new(),
        };

        MirBuilder {
            mir,
            compile_context,
            context_stack,
            hir_modules,
            visited_functions: FxHashMap::default(),
            hir_function_blocks: Vec::new(),
            main_context: None,
            ticking_contexts: Vec::new(),
            global_context: global_context_id,
        }
    }

    #[inline]
    fn current_context(&self) -> ContextId {
        self.context_stack.current_context
    }

    /// Visits a block without creating a new context
    fn visit_block_local(&mut self, block: &'a HirBlock) -> Result<MirValue> {
        for object in &block.objects {
            self.visit_object(object)?;
        }

        for statement in &block.statements {
            self.visit_statement(statement)?;
        }

        // Todo: If this approach works out, unify result and return_value.
        // Add an implicite null return to blocks which can implicitely return and don't have any other
        // return value specified.
        let result = if let Some(return_expr) = &block.return_value {
            Some(self.visit_expression(&return_expr)?)
        } else if self.context().return_values.get_template().is_none() {
            Some(self.context().return_values.default_return.clone().into())
        } else {
            None
        };

        if let Some(result) = result {
            let return_index =
                self.update_return_value(self.context().id, result, block.last_item_span())?;

            self.push(MirNode::ReturnValue(MirReturnValue {
                return_index,
                context_id: self.context().id,
            }));
        }

        // If the return values only consist of one possible value, return that
        // (since it could be concrete and thus more useful).
        // Otherwise, get the template which will be concrete at the llir stage.
        // If the template does not exist (because no value is returned), return the blocks default
        let return_value = self
            .context()
            .return_values
            .get_single()
            .unwrap_or_else(|| self.context().return_values.get_template_or_default());

        Ok(return_value)
    }

    fn push_existing_context(&mut self, id: ContextId) {
        if self.main_context.is_none() {
            self.main_context = Some(id);
        }
        self.context_stack.push(id, self.mir.contexts.get(id).span);
    }

    /// Creates a new context and pushes it to the top
    fn add_context(&mut self, span: Span, kind: ContextKind) -> ContextId {
        self.add_context_after(self.context().id, span, kind)
    }

    /// Creates a new context that is not successor of the current,
    /// but successor of a specific context
    fn add_context_after(
        &mut self,
        ancestor: ContextId,
        span: Span,
        kind: ContextKind,
    ) -> ContextId {
        let context: MirContext<'ctx> = MirContext::new(
            self.compile_context,
            &mut self.mir.namespaces,
            Some(ancestor),
            span,
            kind,
        );
        let id = context.id;
        self.mir.add_context(context);
        self.push_existing_context(id);
        id
    }

    fn pop_context(&mut self) -> &mut MirContext<'ctx> {
        let prev_context = self.current_context();
        self.context_stack.pop();

        self.mir.contexts.get_mut(prev_context)
    }

    /// Calculates the next jump target for `context_id`
    fn get_jump_target(
        &self,
        context_id: ContextId,
        default: (ContextId, usize),
    ) -> Option<(ContextId, usize)> {
        let mode = self.mir.contexts.get(context_id).control_flow;
        if mode.is_normal() {
            Some(default)
        } else {
            self.context_stack.jump_location_for(mode)
        }
    }

    /// Adds a mir node to the current context
    fn push(&mut self, mir_node: MirNode) {
        self.context_mut().nodes.push(mir_node)
    }

    /// Adds a mutable concrete value to the current namespace.
    /// Returns the template
    fn add_mutable_value(&mut self, value: ObjectRef) -> MirValue {
        let template = self
            .context_info()
            .add_anonymous_template(value.class.clone());
        let id = template.expect_template("Must be a template").1;
        self.push(MirNode::UpdateValue(MirUpdateValue {
            id,
            new_value: value.into(),
        }));

        template
    }

    /// Promotes a comptime value to its runtime variant
    fn promote_runtime(&mut self, value: MirValue, span: Span) -> Result<MirValue> {
        if value.class().kind.runtime_encodable() {
            return Ok(value);
        }

        let function = value
            .class()
            .get_property(self.arena(), &SpecialIdent::PromoteRuntime.into())
            .and_then(|value| value.concrete())
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::UnpromotableType {
                        got: value.class().clone(),
                    },
                    span,
                )
            })?;

        let (result, node) =
            self.context_info()
                .register_function_call(function, vec![value], None, span)?;
        self.push(node);
        Ok(result)
    }

    /// Generates a function call to that context.
    /// After the other context was executed, the current context
    /// will continue to run normally.
    fn call_context(&mut self, context_id: ContextId, span: Span) {
        let node = MirNode::GotoContext(MirGotoContext {
            context_id,
            block_id: 0,
            span,
        });
        self.push(node);
    }

    /// Returns whether a variable declared in `stopping_context` is
    /// considered comptime. A variable is considered comptime, if
    /// every context in the `context_stack` from where the variable is declared
    /// to the current context is comptime.
    fn runtime_context(&self, stopping_context_id: ContextId) -> Option<(ContextId, Span)> {
        for (context_id, span) in self.context_stack.iter() {
            let context = self.mir.contexts.get(context_id);
            if context.kind.is_dynamic() {
                return Some((context_id, span));
            }
            if context_id == stopping_context_id {
                break;
            }
        }
        None
    }

    /// Calls a function in the current context
    fn call_function(
        &mut self,
        object: ObjectRef,
        parent: Option<MirValue>,
        mut parameters: Vec<MirValue>,
        span: Span,
    ) -> Result<MirValue> {
        // Check if the function is implemented by the compiler. If this is the case, execute the custom function now
        let compiler_func = object.downcast_payload::<ObjFunction>();
        if let Some(compiler_func) = compiler_func {
            if let FunctionFlags::CompilerImplemented(compiler_implemented) = compiler_func.flags {
                match compiler_implemented {
                    CompilerFunction::RegisterTickingFunction => {
                        return self.register_ticking_function(parameters, span)
                    }
                }
            }
        }
        let next_jump_location = self.context_mut().next_jump_location();

        // Native functions are created per call
        // because its easier to track the parameter types and return types
        // So if this object is a native function signature, evaluate it now
        // Also, the jump machinery only needs to be run if calling a native function
        // - builtin functions are not allowed to use runtime control flow
        let native_func = object
            .downcast_payload::<ObjNativeFunctionSignature>()
            .cloned();
        let (function_object, return_value) = if let Some(function_sig) = &native_func {
            self.context_stack.push_jump_location(
                ControlFlowMode::Return,
                self.context().id,
                next_jump_location,
            );
            let (function, return_value) = self.instantiate_native_function(
                parent.clone(),
                function_sig,
                &mut parameters,
                span,
            )?;
            (function, Some(return_value))
        } else {
            (object, None)
        };

        // Register the function call in the previous context
        let (return_class_value, function_node) = self.context_info().register_function_call(
            function_object,
            parameters,
            parent,
            span,
        )?;

        // If the function is not a native function, use the normal `return_class_value`
        let return_value = return_value.unwrap_or(return_class_value);

        self.push(function_node);

        if native_func.is_some() {
            self.context_stack
                .pop_jump_location(ControlFlowMode::Return);
            self.push(MirNode::JumpLocation(MirJumpLocation {
                index: next_jump_location,
            }));
        }

        Ok(return_value)
    }

    /// Instantiates a native function signature and returns
    /// a NativeFunction object and its exact return type
    fn instantiate_native_function(
        &mut self,
        parent: Option<MirValue>,
        function_sig: &ObjNativeFunctionSignature,
        parameters: &mut Vec<MirValue>,
        span: Span,
    ) -> Result<(ObjectRef, MirValue)> {
        if self.context_stack.contains(&function_sig.function_span) {
            return Err(LangError::new(
                LangErrorKind::NotYetImplemented {
                    msg: "Recursive function call".to_string(),
                },
                span,
            )
            .into());
        }

        let signature = self
            .visited_functions
            .get(&function_sig.function_span)
            .unwrap()
            .clone();

        // Check for actual parameters that do not match the declared parameters
        if !self.accept_function_parameters(&signature.parameters, parameters, parent, span) {
            return Err(LangError::new(
                LangErrorKind::UnexpectedOverload {
                    expected: vec![(
                        FunctionParameters::Specific(
                            signature
                                .parameters
                                .iter()
                                .map(|param| param.expected_type.clone())
                                .collect(),
                        ),
                        signature.return_type.clone(),
                    )],
                    parameters: parameters
                        .iter()
                        .map(|value| value.class().clone())
                        .collect(),
                },
                span,
            )
            .into());
        }

        let context_id = self.add_context_after(
            function_sig.definition_scope,
            function_sig.function_span,
            ContextKind::NativeFunction,
        );

        let mut return_value = {
            for (parameter, sig) in parameters.iter().zip(signature.parameters.iter()) {
                let parameter = self.try_clone_if_variable(parameter.clone(), sig.span)?;
                if let MirValue::Template { class: _, id } = &parameter {
                    self.context_info().declare_as_variable(*id, sig.span);
                }
                self.context_info()
                    .add_value(sig.name.clone(), parameter, sig.span)
            }
            self.visit_block_local(self.hir_function_blocks[signature.block_id])?
        };

        // Check for an actual return type that does not match the declared type
        // Try to promote the type first, if that helps
        if return_value
            .class()
            .get_property(self.arena(), &SpecialIdent::PromoteRuntime.into())
            .is_some()
            && !signature.return_type.matches(return_value.class())
        {
            return_value = self.promote_runtime(return_value, span)?;
        }
        if !signature.return_type.matches(return_value.class()) {
            return Err(LangError::new(
                LangErrorKind::UnexpectedType {
                    declared: Some(function_sig.return_type_span),
                    expected: signature.return_type.clone(),
                    got: return_value.class().clone(),
                },
                self.hir_function_blocks[signature.block_id].last_item_span(),
            )
            .into());
        }

        let (next_context, next_block) = self
            .context_stack
            .jump_location_for(ControlFlowMode::Return)
            .unwrap();
        self.push(MirNode::GotoContext(MirGotoContext {
            context_id: next_context,
            block_id: next_block,
            span,
        }));

        self.pop_context();

        Ok((
            ObjNativeFunction::new(
                self.compile_context,
                context_id,
                signature,
                Rc::clone(return_value.class()),
            )
            .into_object(self.compile_context),
            return_value,
        ))
    }

    /// Tries to clone this value or returns the original value if it does not need to be cloned
    fn try_clone_if_variable(&mut self, value: MirValue, span: Span) -> Result<MirValue> {
        if let Some((class, id)) = value.template() {
            let context = id.context;
            let entry = self
                .arena()
                .get(context.as_inner())
                .get_by_id(id.id)
                .unwrap();
            if entry.is_variable() {
                let maybe_new_val = self.try_clone(class, id, span)?;
                // Make sure that the value gets treated like any other variable
                if let MirValue::Template { class: _, id } = &maybe_new_val {
                    self.context_info().declare_as_variable(*id, span);
                }
                return Ok(maybe_new_val);
            }
        }
        Ok(value)
    }

    /// Tries to clone the value or returns it unmodified if that value cannot be cloned
    fn try_clone(&mut self, class: ClassRef, id: ItemId, span: Span) -> Result<MirValue> {
        let function = class
            .get_property(self.arena(), &SpecialIdent::Clone.into())
            .and_then(|value| value.concrete());
        if let Some(function) = function {
            let (value, node) = self.context_info().register_function_call(
                function,
                vec![MirValue::Template { class, id }],
                None,
                span,
            )?;
            self.push(node);
            Ok(value)
        } else {
            Ok(MirValue::Template { class, id })
        }
    }

    fn update_return_value(
        &mut self,
        context_id: ContextId,
        mut value: MirValue,
        span: Span,
    ) -> Result<usize> {
        // Value must be runtime if the target value is runtime
        let must_be_runtime = self
            .mir
            .contexts
            .get(context_id)
            .return_values
            .get_template()
            .map_or(false, |(value, _)| value.class().kind.runtime_encodable());
        if self.context().kind.is_dynamic() || must_be_runtime {
            value = self.promote_runtime(value, span)?
        }

        if self
            .mir
            .contexts
            .get(context_id)
            .return_values
            .template
            .is_none()
        {
            let value = self
                .context_info()
                .add_anonymous_template(value.class().clone());
            self.mir.contexts.get_mut(context_id).return_values.template = Some((value, span))
        };

        let return_values = &mut self.mir.contexts.get_mut(context_id).return_values;
        let (target_class, declared_at) = return_values.get_template().unwrap();

        value.assert_type_exact(target_class.class(), span, Some(*declared_at))?;

        let id = return_values.add(value);

        Ok(id)
    }

    fn get_type_pattern(&self, path: &HirTypePattern) -> Result<TypePattern> {
        let ctx = self.context();
        match path {
            HirTypePattern::Path(path) => ctx.get_type_pattern(self.arena(), path),
            HirTypePattern::Tuple { span: _, values } => {
                let types: Vec<_> = values
                    .iter()
                    .map(|pattern| self.get_type_pattern(pattern))
                    .try_collect()?;
                let tuple = Tuple::from(types);
                // ToDo: Cache the tuple
                let kind = ClassKind::Tuple(tuple.into());
                let class = Class::new_empty(kind);
                Ok(TypePattern::Class(class.into()))
            }
            HirTypePattern::Function {
                parameters,
                return_type,
                span: _,
            } => {
                let parameters = FunctionParameters::Specific(
                    parameters
                        .iter()
                        .map(|t| self.get_type_pattern(t))
                        .collect::<Result<_>>()?,
                );
                let class_kind = ClassKind::Function {
                    parameters,
                    return_value: match return_type {
                        Some(pattern) => self.get_type_pattern(&pattern)?,
                        None => ObjNull::class(ctx.compile_context).into(),
                    },
                };

                Ok(TypePattern::Class(ClassRef::new(Class::new_empty(
                    class_kind,
                ))))
            }
        }
    }

    /// Assigns `value` to `pattern`. If the pattern is a tuple pattern, then value is required to be a tuple.
    fn assign_to_pattern(
        &mut self,
        pattern: &HirVariablePattern,
        value: MirValue,
        value_span: Span,
        assignment_mode: PatternAssignmentMode,
    ) -> Result<()> {
        match pattern {
            HirVariablePattern::Path(path) => {
                assignment_mode.handle(self, path, value, pattern.span())?;
            }
            HirVariablePattern::Tuple(patterns) => {
                let namespace = match value.class().kind {
                    ClassKind::TupleObject {
                        tuple: _,
                        namespace,
                    } => namespace,
                    _ => {
                        return Err(LangError::new(
                            LangErrorKind::UnexpectedType {
                                declared: None,
                                expected: TypePattern::Class(ObjTupleObject::class(
                                    self.compile_context,
                                )),
                                got: value.class().clone(),
                            },
                            value_span,
                        )
                        .into())
                    }
                };

                let values = self
                    .arena()
                    .get(namespace)
                    .iter()
                    .map(|val| val.1)
                    .cloned()
                    .collect_vec();
                let rhs_count = values.len();
                for value in patterns.iter().zip_longest(values.into_iter()) {
                    match value {
                        EitherOrBoth::Left(_) | EitherOrBoth::Right(_) => {
                            return Err(LangError::new(
                                LangErrorKind::TupleMismatch {
                                    lhs_count: patterns.len(),
                                    value_span,
                                    rhs_count,
                                },
                                pattern.span(),
                            )
                            .into())
                        }
                        EitherOrBoth::Both(pat, val) => {
                            self.assign_to_pattern(pat, val, value_span, assignment_mode)?
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Matches the parameters against the expected parameters.
    /// This operation can modify the given parameter vector.
    fn accept_function_parameters(
        &mut self,
        expected: &[FunctionParameterDefinition],
        parameters: &mut Vec<MirValue>,
        parent: Option<MirValue>,
        span: Span,
    ) -> bool {
        if let Some(parent) = parent {
            if !expected.is_empty()
                && expected.len() - 1 == parameters.len()
                && expected[0].expected_type.matches(parent.class())
            {
                parameters.insert(0, parent);
            }
        }

        for value in expected.iter().zip_longest(parameters) {
            match value {
                EitherOrBoth::Both(expected, got) => {
                    if !expected.expected_type.matches(got.class()) {
                        *got = match self.promote_runtime(got.clone(), span) {
                            Ok(new) => new,
                            Err(_) => return false,
                        };
                        if !expected.expected_type.matches(got.class()) {
                            return false;
                        }
                    }
                }
                _ => return false,
            }
        }
        true
    }

    /// Compiler implemented debris function. Adds a function to the list of ticking functions.
    fn register_ticking_function(
        &mut self,
        parameters: Vec<MirValue>,
        span: Span,
    ) -> Result<MirValue> {
        let required_class = ObjFunction::class(self.compile_context);
        let ticking_func = parameters.get(0).ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedOverload {
                    expected: vec![(
                        FunctionParameters::Specific(vec![TypePattern::Class(
                            required_class.clone(),
                        )]),
                        TypePattern::Class(ObjNull::class(self.compile_context)),
                    )],
                    parameters: vec![],
                },
                span,
            )
        })?;

        let ticking_func = ticking_func.concrete().ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedType {
                    expected: TypePattern::Class(required_class),
                    got: ticking_func.class().clone(),
                    declared: None,
                },
                span,
            )
        })?;

        // Creates an independent context where the ticking function can live
        let context = self.add_context(span, ContextKind::Loop);
        self.call_function(ticking_func, None, Vec::new(), span)?;
        self.pop_context();

        self.ticking_contexts.push(context);

        Ok(MirValue::null(self.compile_context))
    }
}

/// Implements functionality for working with the attributes mutably
impl<'code> MirBuilder<'_, 'code> {
    /// Returns a mutable reference to the current context
    #[inline]
    pub fn context_mut(&mut self) -> &mut MirContext<'code> {
        self.mir.contexts.get_mut(self.current_context())
    }

    /// Returns a shared reference to the current context
    #[inline]
    pub fn context(&self) -> &MirContext {
        self.mir.contexts.get(self.current_context())
    }

    /// Returns a helper struct that contains both a context and the arena
    pub fn context_info(&mut self) -> MirContextInfo<'_, 'code> {
        self.mir.context_info(self.current_context())
    }

    /// Returns a mutable reference to the current namespace
    pub fn namespace_mut(&mut self) -> &mut Namespace {
        let id = self.context_info().context.id;
        self.mir.namespaces.get_mut(id.as_inner())
    }

    /// Returns a mutable reference to the global arena
    pub fn arena_mut(&mut self) -> &mut NamespaceArena {
        &mut self.mir.namespaces
    }

    /// Returns a shared reference to the global arena
    pub fn arena(&self) -> &NamespaceArena {
        &self.mir.namespaces
    }
}

#[derive(Debug, Clone, Copy)]
enum PatternAssignmentMode {
    Assignment(HirDeclarationMode),
    Update,
}

impl PatternAssignmentMode {
    fn handle(
        &self,
        builder: &mut MirBuilder,
        path: &IdentifierPath,
        mut value: MirValue,
        pattern_span: Span,
    ) -> Result<()> {
        match self {
            PatternAssignmentMode::Assignment(decl_mode) => {
                let ident = match path.idents() {
                    [ident] => ident,
                    _ => {
                        return Err(LangError::new(
                            LangErrorKind::UnexpectedPathAssignment {
                                path: builder
                                    .compile_context
                                    .input_files
                                    .get_span_str(path.span())
                                    .to_string(),
                            },
                            pattern_span,
                        )
                        .into())
                    }
                };

                // This is necessary right now, because the compilers assumes
                // variables which are `Concrete` variants to be constant
                match value {
                    // If the variable is declared mutable, only keep a template of it
                    MirValue::Concrete(obj) if !obj.class.kind.typ().should_be_const() => {
                        value = builder.add_mutable_value(obj)
                    }
                    _ => {}
                }

                let runtime_promotable = value
                    .class()
                    .get_property(builder.arena(), &SpecialIdent::PromoteRuntime.into())
                    .is_some();
                if runtime_promotable && !matches!(decl_mode, HirDeclarationMode::Comptime) {
                    value = builder.promote_runtime(value, path.span())?
                }

                if !value.class().kind.comptime_encodable()
                    && matches!(decl_mode, HirDeclarationMode::Comptime)
                {
                    return Err(LangError::new(
                        LangErrorKind::NonComptimeVariable {
                            var_name: builder
                                .compile_context
                                .input_files
                                .get_span_str(pattern_span)
                                .to_string(),
                            class: value.class().clone(),
                        },
                        pattern_span,
                    )
                    .into());
                }
                // declare the referenced values also as variable
                if let MirValue::Template { class: _, id } = &value {
                    builder.context_info().declare_as_variable(*id, ident.span);
                }
                let ident = builder.context().get_ident(ident);
                builder
                    .context_info()
                    .add_unique_value(ident, value, pattern_span)?;
            }
            PatternAssignmentMode::Update => {
                let AccessedProperty {
                    value: old_value,
                    span: old_span,
                    ..
                } = builder.context_info().resolve_path(&path)?;

                // If the old value is a different type, maybe it is possible to promote the new value to that type
                if !value.class().matches_exact(&old_value.class()) {
                    if let Ok(runtime_value) = builder.promote_runtime(value.clone(), pattern_span)
                    {
                        value = runtime_value;
                    }
                }

                // ToDo: Add test for this error message
                if !value.class().matches_exact(old_value.class()) {
                    return Err(LangError::new(
                        LangErrorKind::UnexpectedType {
                            got: value.class().clone(),
                            expected: TypePattern::Class(old_value.class().clone()),
                            declared: old_span,
                        },
                        pattern_span,
                    )
                    .into());
                }

                let (_class, id) = old_value.template().ok_or_else(|| {
                    LangError::new(
                        LangErrorKind::ConstVariable {
                            var_name: path.display(builder.compile_context),
                        },
                        path.span(),
                    )
                })?;

                // If the context is not comptime but the value is, prevent
                // an invalid update to that value.
                let runtime_context = builder.runtime_context(id.context);
                if let Some((_, runtime_span)) = runtime_context {
                    if !value.class().kind.runtime_encodable() {
                        return Err(LangError::new(
                            LangErrorKind::ComptimeVariable {
                                var_name: builder.context().get_ident(path.last()),
                                ctx_span: runtime_span,
                            },
                            pattern_span,
                        )
                        .into());
                    }
                }

                builder.push(MirNode::UpdateValue(MirUpdateValue {
                    id,
                    new_value: value,
                }));
            }
        }
        Ok(())
    }
}

/// Keeps track of the current compile context
/// And the current points of interest in the stack frame.
#[derive(Debug)]
pub struct ContextStack {
    /// The current compile context
    current_context: ContextId,
    /// The span of the current context
    current_span: Span,
    /// The stack, consisting of (context, origin of this context)
    stack: Vec<(ContextId, Span)>,
    jump_locations: Vec<(ControlFlowMode, (ContextId, usize))>,
}

impl ContextStack {
    fn push(&mut self, next_context: ContextId, next_span: Span) {
        self.stack.push((self.current_context, self.current_span));
        self.current_context = next_context;
        self.current_span = next_span;
    }

    fn pop(&mut self) {
        let (previous_context, previous_span) =
            self.stack.pop().expect("Tried to pop the global stack");
        self.current_context = previous_context;
        self.current_span = previous_span;
    }

    pub fn push_jump_location(&mut self, mode: ControlFlowMode, id: ContextId, index: usize) {
        self.jump_locations.push((mode, (id, index)));
    }

    pub fn pop_jump_location(&mut self, mode: ControlFlowMode) {
        let (old_mode, _id) = self
            .jump_locations
            .pop()
            .expect("Tried to pop from an empty stack");
        assert_eq!(
            old_mode, mode,
            "Invalid stack element: Expected {:?} but got {:?}",
            mode, old_mode
        );
    }

    /// Selectes the current jump location for a give control flow
    pub fn jump_location_for(&self, mode: ControlFlowMode) -> Option<(ContextId, usize)> {
        self.jump_locations
            .iter()
            .rev()
            .find(|(other_mode, _id)| *other_mode == mode)
            .map(|(_, id)| *id)
    }

    fn contains(&self, span: &Span) -> bool {
        iter::once(&self.current_span)
            .chain(self.stack.iter().map(|entry| &entry.1))
            .any(|val| val == span)
    }

    /// Iterates this stack, from the current context to the initial context.
    pub fn iter(&self) -> impl Iterator<Item = (ContextId, Span)> + '_ {
        std::iter::once((self.current_context, self.current_span))
            .chain(self.stack.iter().rev().map(|(id, span)| (*id, *span)))
    }
}

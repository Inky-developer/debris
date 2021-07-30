use rustc_hash::FxHashMap;

use crate::error::Result;
use crate::hir::hir_nodes::{
    HirBlock, HirConstValue, HirExpression, HirFormatStringMember, HirFunctionCall, HirStatement,
    HirVariableInitialization, HirVariablePattern,
};
use crate::hir::{Hir, IdentifierPath, SpannedIdentifier};
use crate::mir::mir_context::{MirContext, MirContextId};
use crate::mir::mir_nodes::{Assignment, FunctionCall, MirNode, PrimitiveDeclaration};
use crate::mir::mir_object::MirObjectId;
use crate::mir::mir_primitives::{MirFormatString, MirFormatStringComponent, MirPrimitive};
use crate::mir::namespace::MirNamespace;
use crate::mir::Mir;
use crate::CompileContext;
use debris_common::{Ident, Span};

pub struct MirBuilder<'ctx, 'hir> {
    compile_context: &'ctx CompileContext,
    hir: &'hir Hir,
    current_context: MirContext,
    entry_context: MirContextId,
    contexts: FxHashMap<MirContextId, MirContext>,
    namespace: MirNamespace,
    next_context_id: u32,
}

impl<'ctx, 'hir> MirBuilder<'ctx, 'hir> {
    pub fn new(ctx: &'ctx CompileContext, hir: &'hir Hir) -> Self {
        let entry_context = MirContextId {
            compilation_id: ctx.compilation_id,
            id: 0,
        };

        MirBuilder {
            compile_context: ctx,
            hir,
            current_context: MirContext::new(entry_context),
            entry_context,
            contexts: Default::default(),
            namespace: MirNamespace::new(ctx),
            next_context_id: 1,
        }
    }

    pub fn build(mut self) -> Result<Mir> {
        self.handle_block(&self.hir.main_function)?;

        Ok(Mir {
            _namespace: self.namespace,
            entry_context: self.entry_context,
            contexts: self.contexts,
        })
    }

    fn emit(&mut self, node: impl Into<MirNode>) {
        self.current_context.nodes.push(node.into());
    }

    fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        self.compile_context
            .input_files
            .get_span_str(spanned_ident.span)
            .into()
    }

    fn resolve_path(&mut self, path: &IdentifierPath) -> MirObjectId {
        let mut obj: Option<MirObjectId> = None;

        for spanned_ident in path.idents() {
            let ident = self.get_ident(spanned_ident);

            obj = Some(match obj {
                None => self
                    .current_context
                    .local_namespace
                    .property_get_or_insert(&mut self.namespace, ident),
                Some(obj) => obj.property_get_or_insert(&mut self.namespace, ident),
            })
        }

        obj.expect("Path cannot be empty")
    }
}

impl MirBuilder<'_, '_> {
    pub fn handle_block(&mut self, block: &HirBlock) -> Result<()> {
        // ToDo: Handle objects and return value

        for statement in &block.statements {
            self.handle_statement(statement)?;
        }

        let next_id = self.next_context_id;
        self.next_context_id += 1;
        let context = std::mem::replace(
            &mut self.current_context,
            MirContext::new(MirContextId {
                compilation_id: self.compile_context.compilation_id,
                id: next_id,
            }),
        );

        self.contexts.insert(context.id, context);

        Ok(())
    }

    fn handle_statement(&mut self, statement: &HirStatement) -> Result<()> {
        match statement {
            HirStatement::VariableDecl(variable_decl) => {
                self.handle_variable_declaration(variable_decl)
            }
            other => todo!("{:?}", other),
        }
    }

    fn handle_variable_declaration(
        &mut self,
        variable_decl: &HirVariableInitialization,
    ) -> Result<()> {
        fn handle_pattern(
            this: &mut MirBuilder,
            pattern: &HirVariablePattern,
            value: MirObjectId,
            span: Span,
        ) {
            match pattern {
                HirVariablePattern::Path(path) => {
                    let obj_id = this.resolve_path(path);
                    this.emit(Assignment {
                        target: obj_id,
                        value,
                        span,
                    })
                }
                HirVariablePattern::Tuple(patterns) => {
                    for (index, pattern) in patterns.iter().enumerate() {
                        let value =
                            value.property_get_or_insert(&mut this.namespace, Ident::Index(index));
                        handle_pattern(this, pattern, value, span);
                    }
                }
            }
        }

        let _mode = variable_decl.mode; // ToDo: Not ignore mode
        let value = self.handle_expression(&variable_decl.value)?;
        handle_pattern(self, &variable_decl.pattern, value, variable_decl.span);

        Ok(())
    }

    fn handle_expression(&mut self, expression: &HirExpression) -> Result<MirObjectId> {
        match expression {
            HirExpression::Value(literal) => self.handle_literal_value(literal),
            HirExpression::BinaryOperation {
                operation,
                rhs,
                lhs,
            } => {
                let lhs = self.handle_expression(lhs)?;
                let rhs = self.handle_expression(rhs)?;

                let function = lhs.property_get_or_insert(
                    &mut self.namespace,
                    operation.operator.get_special_ident().into(),
                );
                let return_value = self.namespace.insert_object().id;

                self.emit(FunctionCall {
                    span: expression.span(),
                    function,
                    parameters: vec![lhs, rhs],
                    return_value,
                });
                Ok(return_value)
            }
            HirExpression::FunctionCall(function_call) => self.handle_function_call(function_call),
            other => todo!("{:?}", other),
        }
    }

    fn handle_literal_value(&mut self, value: &HirConstValue) -> Result<MirObjectId> {
        let primitive = match value {
            HirConstValue::Integer { value, .. } => MirPrimitive::Int(*value),
            HirConstValue::Bool { value, .. } => MirPrimitive::Bool(*value),
            HirConstValue::Fixed { .. } => {
                unimplemented!("Decimal literals are not yet implemented")
            }
            HirConstValue::String { value, .. } => MirPrimitive::String(value.clone()),
            HirConstValue::FormatString { value, .. } => {
                let parts = value
                    .iter()
                    .map(|member| match member {
                        HirFormatStringMember::String(val) => {
                            MirFormatStringComponent::String(val.clone())
                        }
                        HirFormatStringMember::Variable(spanned_ident) => {
                            let ident = self.get_ident(spanned_ident);
                            MirFormatStringComponent::Value(
                                self.current_context
                                    .local_namespace
                                    .property_get_or_insert(&mut self.namespace, ident),
                            )
                        }
                    })
                    .collect::<Vec<_>>();
                MirPrimitive::FormatString(MirFormatString::from(parts))
            }
        };

        let obj = self.namespace.insert_object().id;
        self.emit(PrimitiveDeclaration {
            value: primitive,
            target: obj,
            span: value.span(),
        });
        Ok(obj)
    }

    fn handle_function_call(&mut self, _function_call: &HirFunctionCall) -> Result<MirObjectId> {
        todo!()
    }
}

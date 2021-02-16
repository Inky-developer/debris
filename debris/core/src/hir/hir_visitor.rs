use super::hir_nodes::{
    HirBlock, HirConditionalBranch, HirConstValue, HirControlFlow, HirExpression, HirFunction,
    HirFunctionCall, HirImport, HirItem, HirModule, HirObject, HirPropertyDeclaration,
    HirStatement, HirStruct, HirVariableInitialization, HirVariableUpdate,
};

/// Trait which defines methods that a visitor has to implement
pub trait HirVisitor<'a> {
    type Output;

    fn visit_item(&mut self, item: &'a HirItem) -> Self::Output;

    fn visit_object(&mut self, object: &'a HirObject) -> Self::Output;

    fn visit_struct(&mut self, struct_: &'a HirStruct) -> Self::Output;

    fn visit_module(&mut self, module: &'a HirModule) -> Self::Output;

    fn visit_block(&mut self, block: &'a HirBlock) -> Self::Output;

    fn visit_import(&mut self, import: &'a HirImport) -> Self::Output;

    fn visit_control_flow(&mut self, control_flow: &'a HirControlFlow) -> Self::Output;

    fn visit_function(&mut self, function: &'a HirFunction) -> Self::Output;

    fn visit_conditional_branch(&mut self, branch: &'a HirConditionalBranch) -> Self::Output;

    fn visit_statement(&mut self, statement: &'a HirStatement) -> Self::Output;

    fn visit_expression(&mut self, expression: &'a HirExpression) -> Self::Output;

    fn visit_function_call(&mut self, function_call: &'a HirFunctionCall) -> Self::Output;

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &'a HirVariableInitialization,
    ) -> Self::Output;

    fn visit_variable_update(&mut self, variable_update: &'a HirVariableUpdate) -> Self::Output;

    fn visit_property_declaration(
        &mut self,
        property_declaration: &'a HirPropertyDeclaration,
    ) -> Self::Output;

    fn visit_const_value(&mut self, const_value: &'a HirConstValue) -> Self::Output;
}

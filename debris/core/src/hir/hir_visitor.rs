use super::hir_nodes::{
    HirBlock, HirConstValue, HirExpression, HirFunction, HirFunctionCall, HirObject,
    HirPropertyDeclaration, HirStatement, HirStruct, HirVariableDeclaration,
};

/// Trait which defines methods that a visitor has to implement
pub trait HirVisitor {
    type Output;

    fn visit_object(&mut self, object: &HirObject) -> Self::Output;

    fn visit_struct(&mut self, struct_: &HirStruct) -> Self::Output;

    fn visit_block(&mut self, block: &HirBlock) -> Self::Output;

    fn visit_function(&mut self, function: &HirFunction) -> Self::Output;

    fn visit_statement(&mut self, statement: &HirStatement) -> Self::Output;

    fn visit_expression(&mut self, expression: &HirExpression) -> Self::Output;

    fn visit_function_call(&mut self, function_call: &HirFunctionCall) -> Self::Output;

    fn visit_variable_declaration(
        &mut self,
        variable_declaration: &HirVariableDeclaration,
    ) -> Self::Output;

    fn visit_property_declaration(
        &mut self,
        property_declaration: &HirPropertyDeclaration,
    ) -> Self::Output;

    fn visit_const_value(&mut self, const_value: &HirConstValue) -> Self::Output;
}

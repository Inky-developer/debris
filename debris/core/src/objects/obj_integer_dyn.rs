use debris_derive::template;
use debris_type::Type;

use super::{FunctionContext, ObjectType, StaticInt, TypeRef};
use crate::{
    error::LangResult, llir::llir_nodes::BinaryOperation, llir::llir_nodes::Node,
    llir::utils::ItemId, llir::utils::Scoreboard, llir::utils::ScoreboardOperation,
    llir::utils::ScoreboardValue, CompileContext, DebrisObject, ObjectPayload, ObjectProperties,
    ObjectRef,
};

/// Shorthand for adding a binary operation node
macro_rules! bin_op {
    ($operation:expr, $ctx:ident, $lhs:ident, $rhs:ident) => {
        $ctx.emit(Node::BinaryOperation(BinaryOperation {
            id: $ctx.item_id,
            scoreboard: Scoreboard::Main,
            operation: $operation,
            lhs: $lhs.as_scoreboard_value(),
            rhs: $rhs.as_scoreboard_value(),
        }));
    };
}

#[derive(Debug, Eq, PartialEq)]
pub struct DynInt {
    pub id: ItemId,
}

#[template]
impl DynInt {
    pub fn new(id: ItemId) -> Self {
        DynInt { id }
    }

    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Scoreboard(Scoreboard::Main, self.id)
    }

    pub fn template() -> TypeRef {
        ObjectType::new_ref(
            Type::Template(Box::new(Type::DynamicInt)),
            ObjectProperties::default(),
            None,
        )
    }

    // Operations between dynamic ints
    #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    fn add(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    fn sub(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    fn mul(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    fn div(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    fn modu(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    // Operations between dynamic int and static int
    #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    fn add(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    fn sub(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    fn mul(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    fn div(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }

    #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    fn modu(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
        bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
        Ok(ctx.item_id.into())
    }
}

impl ObjectPayload for DynInt {
    fn typ(&self) -> Type {
        Type::DynamicInt
    }

    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new(ctx.type_ctx.template_for_type(&self.typ()), self).into()
    }

    fn eq(&self, other: &ObjectRef) -> bool {
        other
            .downcast_payload::<Self>()
            .map_or(false, |other| other == self)
    }
}

impl From<ItemId> for DynInt {
    fn from(value: ItemId) -> Self {
        DynInt { id: value }
    }
}

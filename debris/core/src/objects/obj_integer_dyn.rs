use std::any::TypeId;

use crate::{
    compile_context::TypeContext, llir::utils::ItemId, llir::utils::Scoreboard,
    llir::utils::ScoreboardValue, CompileContext, DebrisObject, ObjectPayload, ObjectRef, Type,
};

use super::{ClassRef, ObjectClass};

// /// Shorthand for adding a binary operation node
// macro_rules! bin_op {
//     ($operation:expr, $ctx:ident, $lhs:ident, $rhs:ident) => {
//         $ctx.emit(Node::BinaryOperation(BinaryOperation {
//             id: $ctx.item_id,
//             scoreboard: Scoreboard::Main,
//             operation: $operation,
//             lhs: $lhs.as_scoreboard_value(),
//             rhs: $rhs.as_scoreboard_value(),
//         }));
//     };
// }

/// A dynamic Integer
///
/// Dynamic means that the value of this integer is know at runtime, but not at compile time.
/// These integers could for example be stored in a scoreboard.
///
/// This object defines binary operations for between itself and [static integers](debris_core::objects::StaticInt).
#[derive(Debug, Eq, PartialEq)]
pub struct DynInt {
    /// The id of the item
    pub id: ItemId,
}

// #[template]
impl DynInt {
    /// Creates a new dynamic integer with this id
    pub fn new(id: ItemId) -> Self {
        DynInt { id }
    }

    /// Returns a `ScoreboardValue` which identifies a specific value on a scoreboard
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Scoreboard(Scoreboard::Main, self.id)
    }

    fn class(&self, ty_ctx: &TypeContext) -> ClassRef {
        ty_ctx.get_or_insert(TypeId::of::<Self>(), || {
            ObjectClass::new_empty(Type::DynamicInt)
        })
    }

    // // Operations between dynamic ints
    // #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    // fn add(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    // fn sub(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    // fn mul(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    // fn div(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    // fn modu(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &DynInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // // Operations between dynamic int and static int
    // #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    // fn add(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Plus, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    // fn sub(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Minus, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    // fn mul(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Times, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    // fn div(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Divide, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }

    // #[special(fn(DynamicInt, StaticInt) -> DynamicInt)]
    // fn modu(ctx: &mut FunctionContext, lhs: &DynInt, rhs: &StaticInt) -> LangResult<DynInt> {
    //     bin_op!(ScoreboardOperation::Modulo, ctx, lhs, rhs);
    //     Ok(ctx.item_id.into())
    // }
}

impl ObjectPayload for DynInt {
    fn into_object(self, ctx: &CompileContext) -> ObjectRef {
        DebrisObject::new_ref(self.class(&ctx.type_ctx), self)
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

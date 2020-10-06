use debris_derive::template;
use debris_type::Type;

use super::{FunctionContext, ObjectType, TypeRef};
use crate::{
    error::LangResult, llir::llir_nodes::BinaryOperation, llir::llir_nodes::Node,
    llir::utils::ItemId, llir::utils::Scoreboard, llir::utils::ScoreboardOperation,
    llir::utils::ScoreboardValue, CompileContext, DebrisObject, ObjectPayload, ObjectProperties,
    ObjectRef,
};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjectDynamicInteger {
    pub id: ItemId,
}

#[template]
impl ObjectDynamicInteger {
    pub fn new(id: ItemId) -> Self {
        ObjectDynamicInteger { id }
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

    #[special(fn(DynamicInt, DynamicInt) -> DynamicInt)]
    fn add(
        ctx: &mut FunctionContext,
        a: &ObjectDynamicInteger,
        b: &ObjectDynamicInteger,
    ) -> LangResult<ObjectDynamicInteger> {
        ctx.emit(Node::BinaryOperation(BinaryOperation {
            scoreboard: Scoreboard::Main,
            operation: ScoreboardOperation::Plus,
            id: ctx.item_id,
            lhs: a.as_scoreboard_value(),
            rhs: b.as_scoreboard_value(),
        }));
        Ok(ObjectDynamicInteger::new(ctx.item_id))
    }
}

impl ObjectPayload for ObjectDynamicInteger {
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

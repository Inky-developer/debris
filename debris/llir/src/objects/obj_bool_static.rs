use std::fmt;

use debris_derive::object;
use debris_error::{LangErrorKind, LangResult};

use crate::{
    llir_nodes::{FastStore, Node},
    memory::MemoryLayout,
    objects::obj_class::ObjClass,
    utils::{Scoreboard, ScoreboardComparison, ScoreboardValue},
    ObjectPayload, ObjectRef, Type,
};

use super::{
    obj_bool::{and_static, cmp, or_static, ObjBool},
    obj_function::FunctionContext,
};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjStaticBool {
    pub value: bool,
}

#[object(Type::ComptimeBool)]
impl ObjStaticBool {
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Static(self.value as i32)
    }

    #[special]
    fn promote_runtime(
        ctx: &mut FunctionContext,
        this: &ObjStaticBool,
        target: &ObjClass,
    ) -> LangResult<ObjectRef> {
        match target.class.kind.typ() {
            Type::DynamicBool => {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(this.value as i32),
                }));
                Ok(ObjBool::new(ctx.item_id).into_object(ctx.type_ctx))
            }
            _ => Err(LangErrorKind::InvalidConversion {
                this: this.get_class(ctx.type_ctx).to_string(),
                target: target.class.to_string(),
            }),
        }
    }

    #[special]
    fn clone(this: &ObjStaticBool) -> bool {
        this.value
    }

    #[special]
    fn and(lhs: &ObjStaticBool, rhs: &ObjStaticBool) -> bool {
        lhs.value && rhs.value
    }

    #[special]
    fn and(ctx: &mut FunctionContext, lhs: &ObjStaticBool, rhs: &ObjBool) -> ObjBool {
        // Since and is commutative, just evaluate it in the opposite order
        let (node, result) = and_static(ctx.item_id, rhs, lhs.value);
        ctx.emit(node);
        result
    }

    #[special]
    fn or(lhs: &ObjStaticBool, rhs: &ObjStaticBool) -> bool {
        lhs.value || rhs.value
    }

    #[special]
    fn or(ctx: &mut FunctionContext, lhs: &ObjStaticBool, rhs: &ObjBool) -> ObjBool {
        // Since or is commutative, just evaluate it in the opposite order
        let (node, result) = or_static(ctx.item_id, rhs, lhs.value);
        ctx.emit(node);
        result
    }

    #[special]
    fn not(this: &ObjStaticBool) -> bool {
        !this.value
    }

    #[special]
    fn cmp_eq(this: &ObjStaticBool, other: &ObjStaticBool) -> bool {
        this.value == other.value
    }

    #[special]
    fn cmp_eq(ctx: &mut FunctionContext, this: &ObjStaticBool, other: &ObjBool) -> ObjBool {
        let (node, ret) = cmp(
            ctx.item_id,
            other,
            this.as_scoreboard_value(),
            ScoreboardComparison::Equal,
        );
        ctx.emit(node);
        ret
    }

    #[special]
    fn cmp_ne(this: &ObjStaticBool, other: &ObjStaticBool) -> bool {
        this.value != other.value
    }

    #[special]
    fn cmp_ne(ctx: &mut FunctionContext, this: &ObjStaticBool, other: &ObjBool) -> ObjBool {
        let (node, ret) = cmp(
            ctx.item_id,
            other,
            this.as_scoreboard_value(),
            ScoreboardComparison::NotEqual,
        );
        ctx.emit(node);
        ret
    }
}

impl ObjectPayload for ObjStaticBool {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }
}

impl fmt::Display for ObjStaticBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl From<bool> for ObjStaticBool {
    fn from(value: bool) -> Self {
        ObjStaticBool { value }
    }
}

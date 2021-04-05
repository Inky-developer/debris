use std::fmt;

use debris_derive::object;

use crate::{
    llir::{
        llir_nodes::{FastStore, Node},
        utils::{Scoreboard, ScoreboardValue},
    },
    memory::MemoryLayout,
    CompileContext, ObjectPayload, Type,
};

use super::{
    obj_bool::{and_static, or_static, ObjBool},
    obj_function::FunctionContext,
};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjStaticBool {
    pub value: bool,
}

#[object(Type::StaticBool)]
impl ObjStaticBool {
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Static(self.value as i32)
    }
    
    #[special]
    fn promote_runtime(ctx: &mut FunctionContext, this: &ObjStaticBool) -> ObjBool {
        ctx.emit(Node::FastStore(FastStore {
            id: ctx.item_id,
            scoreboard: Scoreboard::Main,
            value: ScoreboardValue::Static(this.value as i32),
        }));
        ObjBool::new(ctx.item_id)
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
}

impl ObjectPayload for ObjStaticBool {
    fn memory_layout(&self, _: &CompileContext) -> MemoryLayout {
        MemoryLayout::Unsized
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

use std::fmt;

use debris_error::LangResult;

use crate::{
    class::ClassRef,
    function_interface::make_overload,
    impl_class,
    llir_nodes::{FastStore, Node},
    memory::MemoryLayout,
    objects::{
        obj_bool::{and_static, cmp, or_static},
        obj_class::ObjClass,
    },
    type_context::TypeContext,
    utils::{Scoreboard, ScoreboardComparison, ScoreboardValue},
    ObjectPayload, ObjectRef, Type,
};

use super::{obj_bool::ObjBool, obj_class::HasClass, obj_function::FunctionContext};

#[derive(Debug, Eq, PartialEq)]
pub struct ObjStaticBool {
    pub value: bool,
}

impl_class! {ObjStaticBool, Type::ComptimeBool, {
    Promote => |ctx: &mut FunctionContext, this: &ObjStaticBool, target: &ObjClass| -> Option<LangResult<ObjectRef>> {
        match target.class.kind.typ() {
            Type::DynamicBool => {
                ctx.emit(Node::FastStore(FastStore {
                    id: ctx.item_id,
                    scoreboard: Scoreboard::Main,
                    value: ScoreboardValue::Static(this.value as i32),
                }));
                Some(Ok(ObjBool::new(ctx.item_id).into_object(ctx.type_ctx)))
            }
            _ => None
        }
    },

    And => make_overload(vec![
        |lhs: &ObjStaticBool, rhs: &ObjStaticBool| -> bool {
            lhs.value && rhs.value
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjStaticBool, rhs: &ObjBool| -> ObjBool {
            // Since and is commutative, just evaluate it in the opposite order
            let (node, result) = and_static(ctx.item_id, rhs, lhs.value);
            ctx.emit(node);
            result
        }.to_normalized_function(),
    ]),

    Or => make_overload(vec![
        |lhs: &ObjStaticBool, rhs: &ObjStaticBool| -> bool {
            lhs.value || rhs.value
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, lhs: &ObjStaticBool, rhs: &ObjBool| -> ObjBool {
            // Since or is commutative, just evaluate it in the opposite order
            let (node, result) = or_static(ctx.item_id, rhs, lhs.value);
            ctx.emit(node);
            result
        }.to_normalized_function()
    ]),


    Not => |this: &ObjStaticBool| -> bool {
        !this.value
    },

    CmpEq => make_overload(vec![
        |this: &ObjStaticBool, other: &ObjStaticBool| -> bool {
            this.value == other.value
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, this: &ObjStaticBool, other: &ObjBool| -> ObjBool {
            let (node, ret) = cmp(
                ctx.item_id,
                other,
                this.as_scoreboard_value(),
                ScoreboardComparison::Equal,
            );
            ctx.emit(node);
            ret
        }.to_normalized_function()
    ]),

    CmpNe => make_overload(vec![
        |this: &ObjStaticBool, other: &ObjStaticBool| -> bool {
            this.value != other.value
        }.to_normalized_function(),
        |ctx: &mut FunctionContext, this: &ObjStaticBool, other: &ObjBool| -> ObjBool {
            let (node, ret) = cmp(
                ctx.item_id,
                other,
                this.as_scoreboard_value(),
                ScoreboardComparison::NotEqual,
            );
            ctx.emit(node);
            ret
        }.to_normalized_function()
    ])
}}

impl ObjStaticBool {
    pub fn as_scoreboard_value(&self) -> ScoreboardValue {
        ScoreboardValue::Static(self.value as i32)
    }
}

impl ObjectPayload for ObjStaticBool {
    fn memory_layout(&self) -> &MemoryLayout {
        &MemoryLayout::Unsized
    }

    fn runtime_class(&self, ctx: &TypeContext) -> Option<ClassRef> {
        Some(ObjBool::class(ctx))
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

use debris_common::Span;
use std::fmt;
use std::fmt::Formatter;

use crate::{mir_context::MirContextId, mir_object::MirObjectId, mir_primitives::MirPrimitive};

macro_rules! mir_node_declaration {
    (pub enum $name:ident {$($member_name:ident($type_name:ident)),*}) => {
        pub enum $name {
            $(
                $member_name($type_name)
            ),*
        }

        $(
            impl From<$type_name> for $name {
                fn from(value: $type_name) -> Self {
                    $name::$member_name(value)
                }
            }
        )*

        impl ::std::fmt::Debug for $name {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                match self {
                    $(
                        $name::$member_name(val) => write!(f, "{:?}", val),
                    )*
                }
            }
        }
    };
}

mir_node_declaration! {
    pub enum MirNode {
        Branch(Branch),
        FunctionCall(FunctionCall),
        Goto(Goto),
        RuntimePromotion(RuntimePromotion),
        VerifyValueComptime(VerifyValueComptime),
        VerifyTupleLength(VerifyTupleLength),
        PrimitiveDeclaration(PrimitiveDeclaration),
        VariableUpdate(VariableUpdate)
    }
}

/// An if-statement which has a condition, a return value and two possible branches
pub struct Branch {
    pub span: Span,
    pub condition_span: Span,
    pub return_value: MirObjectId,
    pub condition: MirObjectId,
    pub pos_branch: MirContextId,
    pub neg_branch: MirContextId,
}

impl fmt::Debug for Branch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "if {:?} then call {:?} else call {:?} ==> {:?}",
            self.condition, self.pos_branch, self.neg_branch, self.return_value
        )
    }
}

pub struct FunctionCall {
    pub span: Span,
    pub ident_span: Span,
    pub function: MirObjectId,
    pub parameters: Vec<MirObjectId>,
    pub self_obj: Option<MirObjectId>,
    pub return_value: MirObjectId,
}

impl fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} := call {:?} with {:?}",
            self.return_value, self.function, self.parameters
        )?;
        if let Some(self_obj) = &self.self_obj {
            write!(f, " (self: {:?})", self_obj)?;
        }
        Ok(())
    }
}

/// Goes to a specific context, should be the last item of any context
pub struct Goto {
    pub span: Span,
    pub context_id: MirContextId,
}

impl fmt::Debug for Goto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "goto {:?}", self.context_id)
    }
}

/// Tries to promote a comptime value to a runtime value, but does not fail if
/// no such conversion exists.
/// Used in `let foo = expression();` statements, because foo by default stores
/// runtime encodable values.
pub struct RuntimePromotion {
    pub span: Span,
    pub value: MirObjectId,
    pub target: MirObjectId,
}

impl fmt::Debug for RuntimePromotion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} := promote {:?}", self.target, self.value)
    }
}

pub struct VerifyValueComptime {
    pub value: MirObjectId,
    pub span: Span,
}

impl fmt::Debug for VerifyValueComptime {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "verify_comptime {:?}", self.value)
    }
}
pub struct VerifyTupleLength {
    pub length: usize,
    pub value: MirObjectId,
    pub span: Span,
}

impl fmt::Debug for VerifyTupleLength {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "verify_tuple_length {:?}, {}", self.value, self.length)
    }
}

/// Declares a primitive value and assigns it to `target`
pub struct PrimitiveDeclaration {
    pub span: Span,
    pub target: MirObjectId,
    pub value: MirPrimitive,
}

impl fmt::Debug for PrimitiveDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} := {:?}", self.target, self.value)
    }
}

/// Updates the variable at `target` to `value`
pub struct VariableUpdate {
    pub span: Span,
    pub target: MirObjectId,
    pub value: MirObjectId,
    /// Whether the update may be performed at compile time
    pub comptime_update_allowed: bool,
}

impl fmt::Debug for VariableUpdate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let decl = if self.comptime_update_allowed {
            ":"
        } else {
            ""
        };
        write!(f, "{:?} {}= {:?}", self.target, decl, self.value)
    }
}

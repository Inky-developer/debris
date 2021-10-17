use debris_common::Span;
use std::fmt;

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
        PrimitiveDeclaration(PrimitiveDeclaration),
        VariableUpdate(VariableUpdate)
    }
}

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
            "{:?} := if {:?} then call {:?} else call {:?}",
            self.return_value, self.condition, self.pos_branch, self.neg_branch
        )
    }
}

pub struct FunctionCall {
    pub span: Span,
    pub ident_span: Span,
    pub function: MirObjectId,
    pub parameters: Vec<MirObjectId>,
    pub return_value: MirObjectId,
}

impl fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} := call {:?} with {:?}",
            self.return_value, self.function, self.parameters
        )
    }
}

pub struct Goto {
    pub span: Span,
    pub context_id: MirContextId,
}

impl fmt::Debug for Goto {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "goto {:?}", self.context_id)
    }
}

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

pub struct VariableUpdate {
    pub span: Span,
    pub target: MirObjectId,
    pub value: MirObjectId,
}

impl fmt::Debug for VariableUpdate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} = {:?}", self.target, self.value)
    }
}

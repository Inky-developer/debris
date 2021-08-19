use crate::mir::mir_object::MirObjectId;
use crate::mir::mir_primitives::MirPrimitive;
use debris_common::{Ident, Span};
use std::fmt;

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
        Assignment(Assignment),
        ExternItem(ExternItem),
        FunctionCall(FunctionCall),
        PrimitiveDeclaration(PrimitiveDeclaration)
    }
}

pub struct Assignment {
    pub span: Span,
    pub target: MirObjectId,
    pub value: MirObjectId,
}

impl fmt::Debug for Assignment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} := {:?}", self.target, self.value)
    }
}

pub struct ExternItem {
    pub span: Span,
    pub ident: Ident,
    pub obj_id: MirObjectId,
}

impl fmt::Debug for ExternItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} := extern \"{}\"", self.obj_id, self.ident)
    }
}

pub struct FunctionCall {
    pub span: Span,
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

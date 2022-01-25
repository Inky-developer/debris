use debris_common::{Ident, Span};
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
                        $name::$member_name(val) => write!(f, "{val:?}"),
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
        RuntimeCopy(RuntimeCopy),
        VerifyValueComptime(VerifyValueComptime),
        VerifyTupleLength(VerifyTupleLength),
        VerifyPropertyExists(VerifyPropertyExists),
        PrimitiveDeclaration(PrimitiveDeclaration),
        VariableUpdate(VariableUpdate),
        PropertyAccess(PropertyAccess)
    }
}

/// An if-statement which has a condition, a return value and two possible branches
pub struct Branch {
    pub span: Span,
    pub is_comptime: bool,
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
        if let Some(self_obj_id) = &self.self_obj {
            write!(f, " (self: {self_obj_id:?})")?;
        }
        Ok(())
    }
}

/// Goes to a specific context
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

/// Tries to copy a value (Does nothing if the value is comptime)
pub struct RuntimeCopy {
    pub span: Span,
    pub value: MirObjectId,
    pub target: MirObjectId,
}

impl fmt::Debug for RuntimeCopy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} := copy {:?}", self.target, self.value)
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
    pub span: Span,
    pub length: usize,
    pub value: MirObjectId,
}

impl fmt::Debug for VerifyTupleLength {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "verify_tuple_length {:?}, {}", self.value, self.length)
    }
}

pub struct VerifyPropertyExists {
    pub span: Span,
    pub obj_id: MirObjectId,
    pub ident: Ident,
}

impl fmt::Debug for VerifyPropertyExists {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "verify_property_exists {:?}.{}", self.obj_id, self.ident)
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

/// Binds a required property of an object to an object id so it can be used
pub struct PropertyAccess {
    pub span: Span,
    /// The target to which should be written to
    pub target_id: MirObjectId,
    /// The object with the required ident
    pub value_id: MirObjectId,
    pub property_ident: Ident,
}

impl fmt::Debug for PropertyAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} := {:?}.{}",
            self.target_id, self.value_id, self.property_ident
        )
    }
}

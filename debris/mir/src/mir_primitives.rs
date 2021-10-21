use debris_common::{Ident, Span};

use std::fmt::{self, Formatter};
use std::rc::Rc;

use crate::{mir_context::MirContextId, mir_object::MirObjectId};

#[derive(Debug)]
pub enum MirPrimitive {
    Int(i32),
    Bool(bool),
    String(Rc<str>),
    FormatString(MirFormatString),
    Function(MirFunction),
    Module(MirModule),
    Null,
    Never,
}

pub struct MirFormatString(pub Vec<MirFormatStringComponent>);

impl From<Vec<MirFormatStringComponent>> for MirFormatString {
    fn from(val: Vec<MirFormatStringComponent>) -> Self {
        MirFormatString(val)
    }
}

impl fmt::Debug for MirFormatString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

pub enum MirFormatStringComponent {
    String(Rc<str>),
    Value(MirObjectId),
}

impl fmt::Debug for MirFormatStringComponent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MirFormatStringComponent::String(string) => write!(f, "\"{}\"", string),
            MirFormatStringComponent::Value(val) => write!(f, "{:?}", val),
        }
    }
}

pub struct MirFunctionParameter {
    pub span: Span,
    pub typ: MirObjectId,
    pub value: MirObjectId,
}

pub struct MirFunction {
    pub context_id: MirContextId,
    pub name: Ident,
    pub parameters: Vec<MirFunctionParameter>,
    pub return_type: Option<MirObjectId>,
}

impl fmt::Debug for MirFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (index, parameter) in self.parameters.iter().enumerate() {
            if index != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}: {:?}", parameter.value, parameter.typ)?;
        }
        write!(f, ") ")?;
        if let Some(return_type) = &self.return_type {
            write!(f, "-> {:?} ", return_type)?;
        }
        write!(f, "{{{:?}}}", self.context_id)
    }
}

pub struct MirModule {
    pub context_id: MirContextId,
    pub ident: Ident,
}

impl fmt::Debug for MirModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "mod {} {{{:?}}}", self.ident, self.context_id)
    }
}

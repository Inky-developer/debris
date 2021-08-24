use debris_common::Ident;

use crate::mir::mir_object::MirObjectId;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

use super::mir_context::MirContextId;

#[derive(Debug)]
pub enum MirPrimitive {
    Int(i32),
    Bool(bool),
    String(Rc<str>),
    FormatString(MirFormatString),
    Function(MirFunction),
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

pub struct MirFunction {
    pub context_id: MirContextId,
    pub name: Ident,
    pub parameter_types: Vec<MirObjectId>,
    pub return_type: Option<MirObjectId>,
}

impl fmt::Debug for MirFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.return_type {
            Some(return_type) => write!(f, "fn {}({:?}) -> {:?} {{{:?}}}", self.name, self.parameter_types, return_type, self.context_id),
            None => write!(f, "fn {}({:?}) {{{:?}}}", self.name, self.parameter_types, self.context_id),
        }
        
    }
}

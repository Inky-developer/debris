use crate::mir::mir_object::MirObjectId;
use std::fmt;
use std::fmt::Formatter;
use std::rc::Rc;

#[derive(Debug)]
pub enum MirPrimitive {
    Int(i32),
    Bool(bool),
    String(Rc<str>),
    FormatString(MirFormatString),
}

pub struct MirFormatString(Vec<MirFormatStringComponent>);

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
            MirFormatStringComponent::String(string) => write!(f, "{}", string),
            MirFormatStringComponent::Value(val) => write!(f, "{:?}", val),
        }
    }
}

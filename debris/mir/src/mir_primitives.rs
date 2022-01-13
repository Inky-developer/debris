use debris_common::{Ident, Span};
use rustc_hash::FxHashMap;

use std::fmt::{self, Formatter};
use std::rc::Rc;

use crate::{mir_context::MirContextId, mir_object::MirObjectId};

pub enum MirPrimitive {
    Int(i32),
    Bool(bool),
    String(Rc<str>),
    FormatString(MirFormatString),
    Function(MirFunction),
    FunctionClass(Vec<MirObjectId>, Option<MirObjectId>),
    Module(MirModule),
    Tuple(Vec<MirObjectId>),
    TupleClass(Vec<(MirObjectId, Span)>),
    StructType(MirStructType),
    Struct(MirStruct),
    Null,
    Never,
}

impl fmt::Debug for MirPrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            MirPrimitive::Int(i) => write!(f, "Int({i})"),
            MirPrimitive::Bool(b) => write!(f, "Bool({b})"),
            MirPrimitive::String(s) => write!(f, "String({s})"),
            MirPrimitive::FormatString(fs) => write!(f, "FormatString({fs:?})"),
            MirPrimitive::Function(func) => write!(f, "Function({func:?})"),
            MirPrimitive::FunctionClass(args, ret) => {
                write!(f, "FunctionClass({args:?}, {ret:?})")
            }
            MirPrimitive::Module(m) => write!(f, "Module({m:?})"),
            MirPrimitive::Tuple(t) => write!(f, "Tuple({t:?})"),
            MirPrimitive::TupleClass(t) => {
                write!(f, "TupleClass([")?;

                let mut iter = t.iter();
                if let Some((id, _)) = iter.next() {
                    write!(f, "{id:?}")?;

                    for (id, _span) in iter {
                        write!(f, ", {id:?}")?;
                    }
                }

                write!(f, "])")
            }
            MirPrimitive::StructType(strukt) => write!(f, "StructType({strukt:?})"),
            MirPrimitive::Struct(strukt) => write!(f, "Struct({strukt:?})"),
            MirPrimitive::Null => write!(f, "Null"),
            MirPrimitive::Never => write!(f, "Never"),
        }
    }
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
            MirFormatStringComponent::String(string) => write!(f, "\"{string}\""),
            MirFormatStringComponent::Value(val) => write!(f, "{val:?}"),
        }
    }
}

pub struct MirFunctionParameter {
    pub span: Span,
    pub typ: MirObjectId,
    pub value: MirObjectId,
}

pub struct MirFunction {
    pub signature_span: Span,
    pub context_id: MirContextId,
    pub name: Ident,
    pub parameters: Vec<MirFunctionParameter>,
    pub return_type: Option<MirObjectId>,
    /// The span of the return type definition
    pub return_type_span: Span,
    /// The span of the expression that defined the return type
    pub return_span: Span,
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
            write!(f, "-> {return_type:?} ")?;
        }
        write!(f, "{{{:?}}}", self.context_id)
    }
}

pub struct MirStructType {
    pub name: Ident,
    pub properties: FxHashMap<Ident, (MirObjectId, Span)>,
    pub context_id: MirContextId,
}

impl fmt::Debug for MirStructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {} {{ ", self.name)?;

        let mut iter = self.properties.iter();
        if let Some((name, (type_id, _))) = iter.next() {
            write!(f, "{name}: {type_id:?}")?;

            for (name, (type_id, _)) in iter {
                write!(f, ", {name}: {type_id:?}")?;
            }
        }

        write!(f, "; {{{:?}}} }}", self.context_id)
    }
}

pub struct MirStruct {
    pub ident_span: Span,
    pub struct_type: MirObjectId,
    pub values: FxHashMap<Ident, (MirObjectId, Span)>,
}

impl fmt::Debug for MirStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {{ ", self.struct_type)?;

        let mut iter = self.values.iter();
        if let Some((name, (type_id, _))) = iter.next() {
            write!(f, "{name}: {type_id:?}")?;

            for (name, (type_id, _)) in iter {
                write!(f, ", {name}: {type_id:?}")?;
            }
        }

        write!(f, " }}")
    }
}

pub struct MirModule {
    pub span: Span,
    pub last_item_span: Span,
    pub context_id: MirContextId,
    pub ident: Ident,
}

impl fmt::Debug for MirModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "mod {} {{{:?}}}", self.ident, self.context_id)
    }
}

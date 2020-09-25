use std::rc::Rc;

use debris_common::{CodeRef, Ident, LocalSpan, Span};
use rustc_hash::FxHashMap;

use crate::{
    error::LangError, error::LangErrorKind, error::Result, hir::IdentifierPath,
    hir::SpannedIdentifier, CompileContext, TemplateRef,
};

use super::{MirNode, MirValue};

#[derive(Debug)]
struct MirNamespaceEntry {
    span: Span,
    id: u64,
}

/// Keeps track of all variables
/// Contains a ref to the code to convert a span to a str
/// Also contains all mir nodes that were already emmitted
#[derive(Debug, Eq, PartialEq)]
pub struct MirContext {
    /// The source code which contains this context
    pub code: CodeRef,
    /// A ref to the global compile context
    pub compile_context: Rc<CompileContext>,
    /// The context id
    pub id: u64,
    /// All mir nodes that are emitted
    pub nodes: Vec<MirNode>,
    /// All variables found as either object or template
    pub values: Vec<MirValue>,
    /// A map from variable identifier to variable id
    namespace: FxHashMap<Ident, MirNamespaceEntry>,
}

impl MirContext {
    pub fn new(id: u64, compile_context: Rc<CompileContext>, code: CodeRef) -> Self {
        MirContext {
            compile_context,
            id,
            code,
            namespace: FxHashMap::default(),
            nodes: Vec::default(),
            values: Vec::default(),
        }
    }

    /// Adds a value that corresponds to `ident`
    /// Returns `Err` if the ident is already defined
    pub fn add_value(&mut self, ident: &Ident, value: MirValue, span: LocalSpan) -> Result<()> {
        if let Some(value) = self.namespace.get(&ident) {
            println!("{:?}", span);
            return Err(LangError::new(
                LangErrorKind::VariableAlreadyDefined {
                    name: ident.to_string(),
                    previous_definition: value.span.clone(),
                },
                self.as_span(span),
            )
            .into());
        }
        let index = self.values.len() as u64;
        self.values.push(value);
        self.namespace.insert(
            ident.clone(),
            MirNamespaceEntry {
                id: index,
                span: self.as_span(span),
            },
        );
        Ok(())
    }

    /// Adds an anonymous value that is usually used temporarily
    /// Returns the template as a MirValue
    pub fn add_anonymous_template(&mut self, template: TemplateRef) -> &MirValue {
        let new_uid = self.values.len() as u64;
        let value = MirValue::Template {
            id: new_uid,
            template,
        };
        self.values.push(value);
        self.values.last().unwrap()
    }

    /// Looks up the value that corresponds to this ident
    pub fn get_from_spanned_ident(&self, spanned_ident: &SpannedIdentifier) -> Result<&MirValue> {
        match self.namespace.get(&self.get_ident(spanned_ident)) {
            Some(entry) => Ok(self.values.get(entry.id as usize).unwrap()),
            None => Err(LangError::new(
                LangErrorKind::MissingVariable {
                    var_name: self.get_ident(spanned_ident),
                    similar: vec![],
                },
                self.as_span(spanned_ident.span.clone()),
            )
            .into()),
        }
    }

    pub fn get_id(&mut self, ident: &Ident) -> Option<u64> {
        self.namespace.get(ident).map(|entry| entry.id)
    }

    pub fn get_span_str(&self, span: &LocalSpan) -> &str {
        span.as_str(&self.code.source)
    }

    pub fn as_span(&self, local_span: LocalSpan) -> Span {
        Span {
            code: self.code.clone(),
            local_span,
        }
    }

    pub fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        Ident::new(self.get_span_str(&spanned_ident.span))
    }

    /// Resolves the accessor and returns the accessed element
    pub fn resolve_path(&self, path: &IdentifierPath) -> Result<MirValue> {
        self.resolve_idents(&path.idents)
    }

    fn resolve_idents(&self, idents: &Vec<SpannedIdentifier>) -> Result<MirValue> {
        if let [first, rest @ ..] = idents.as_slice() {
            let mut last_ident = None;
            let mut value = self.get_from_spanned_ident(first)?.clone();

            for property in rest {
                let ident = self.get_ident(property);
                let child = value.get_property(&ident).ok_or_else(|| {
                    LangError::new(
                        LangErrorKind::MissingProperty {
                            parent: last_ident.unwrap_or_else(|| self.get_ident(first)),
                            property: self.get_ident(property),
                            similar: vec![],
                        },
                        self.as_span(property.span.clone()),
                    )
                })?;
                value = MirValue::Concrete(child);
                last_ident = Some(ident);
            }

            Ok(value)
        } else {
            unreachable!("Got empty identifier vec")
        }
    }
}

impl PartialEq for MirNamespaceEntry {
    fn eq(&self, other: &MirNamespaceEntry) -> bool {
        self.id == other.id
    }
}

impl Eq for MirNamespaceEntry {}

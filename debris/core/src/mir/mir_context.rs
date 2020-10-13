use std::rc::Rc;

use debris_common::{CodeRef, Ident, LocalSpan, Span};
use rustc_hash::FxHashMap;

use crate::{
    error::LangError, error::LangErrorKind, error::Result, hir::IdentifierPath,
    hir::SpannedIdentifier, objects::ClassRef, objects::ObjectModule, CompileContext,
    ObjectPayload, ObjectRef,
};

use super::{MirNode, MirValue};

struct MirNamespaceEntry {
    span: Span,
    id: u64,
}

/// Keeps track of all important data during mir compilation
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
    /// A map from module identifiers to module
    loaded_modules: FxHashMap<Ident, u64>,
}

impl MirContext {
    /// Creates a new context
    ///
    /// The id has to uniquely identify this context.
    pub fn new(id: u64, compile_context: Rc<CompileContext>, code: CodeRef) -> Self {
        MirContext {
            code,
            compile_context,
            id,
            nodes: Vec::default(),
            values: Vec::default(),
            namespace: FxHashMap::default(),
            loaded_modules: FxHashMap::default(),
        }
    }

    /// Loads the contents of this `module` into the scope
    ///
    /// Returns whether the module was successfully loaded.
    /// A module cannot load successfully, if it is already loaded.
    pub fn register(&mut self, module: ObjectModule) -> bool {
        if self.is_module_loaded(&module) {
            return false;
        }

        let id = self.next_id();
        // ToDo: Check if a list of the loaded modules is really needed
        self.loaded_modules.insert(module.ident().clone(), id);
        self.namespace.insert(
            module.ident().clone(),
            MirNamespaceEntry {
                id,
                span: self.empty_span(),
            },
        );
        self.values.push(MirValue::Concrete(
            module.into_object(&self.compile_context),
        ));
        true
    }

    /// Returns whether a module is loaded
    pub fn is_module_loaded(&self, module: &ObjectModule) -> bool {
        self.loaded_modules.contains_key(module.ident())
    }

    /// Returns the id for the next object
    pub fn next_id(&self) -> u64 {
        self.values.len() as u64
    }

    /// Adds a value that corresponds to `ident`
    ///
    /// If the value is a template, it is already added and only a namespace entry has to be created.
    /// Returns `Err` if the ident is already defined.
    pub fn add_value(&mut self, ident: &Ident, value: MirValue, span: LocalSpan) -> Result<()> {
        if let Some(value) = self.namespace.get(&ident) {
            return Err(LangError::new(
                LangErrorKind::VariableAlreadyDefined {
                    name: ident.to_string(),
                    previous_definition: value.span.clone(),
                },
                self.as_span(span),
            )
            .into());
        }

        let index = match value {
            obj @ MirValue::Concrete(_) => {
                let index = self.next_id();
                self.values.push(obj);
                index
            }
            MirValue::Template { id, class: _ } => id,
        };

        self.namespace.insert(
            ident.clone(),
            MirNamespaceEntry {
                id: index,
                span: self.as_span(span),
            },
        );

        Ok(())
    }

    /// Adds an anonymous object and returns a ref MirValue
    pub fn add_anonymous_object(&mut self, object: ObjectRef) -> &MirValue {
        let mir_value = MirValue::Concrete(object);

        self.values.push(mir_value);
        &self.values.last().unwrap()
    }

    /// Adds an anonymous value that is usually used temporarily
    ///
    /// Returns the template as a MirValue.
    pub fn add_anonymous_template(&mut self, class: ClassRef) -> &MirValue {
        let new_uid = self.next_id();
        let value = MirValue::Template { id: new_uid, class };

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

    /// Returns the string the corresponds to that `span`
    pub fn get_span_str(&self, span: &LocalSpan) -> &str {
        span.as_str(&self.code.source)
    }

    /// Converts a `LocalSpan` to a `Span` which has access to the code
    pub fn as_span(&self, local_span: LocalSpan) -> Span {
        Span {
            code: self.code.clone(),
            local_span,
        }
    }

    /// Returns an ident from a span
    pub fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        Ident::new(self.get_span_str(&spanned_ident.span))
    }

    /// Resolves the accessor and returns the accessed element
    pub fn resolve_path(&self, path: &IdentifierPath) -> Result<MirValue> {
        self.resolve_idents(&path.idents)
    }

    fn resolve_idents(&self, idents: &[SpannedIdentifier]) -> Result<MirValue> {
        if let [first, rest @ ..] = idents {
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

    /// Creates an empty span used for generated idents
    fn empty_span(&self) -> Span {
        Span {
            code: self.code.clone(),
            local_span: LocalSpan::new(0, 0),
        }
    }
}

impl std::fmt::Debug for MirNamespaceEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Entry").field(&self.id).finish()
    }
}

impl PartialEq for MirNamespaceEntry {
    fn eq(&self, other: &MirNamespaceEntry) -> bool {
        self.id == other.id
    }
}

impl Eq for MirNamespaceEntry {}

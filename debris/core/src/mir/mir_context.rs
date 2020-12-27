use std::rc::Rc;

use debris_common::{CodeRef, Ident, Span};
use generational_arena::{Arena, Index};

use crate::{
    error::LangError, error::LangErrorKind, error::Result, hir::IdentifierPath,
    hir::SpannedIdentifier, objects::ClassRef, objects::HasClass, objects::ObjFunction,
    objects::ObjModule, CompileContext, Namespace, ObjectRef, ValidPayload,
};

use super::{mir_nodes::MirCall, Mir, MirNode, MirValue};

/// Struct that is passed around when working with the mir context
pub struct MirInfo<'a, 'code> {
    pub mir: &'a mut Mir<'code>,
    pub current_context: u64,
}

impl<'code> MirInfo<'_, 'code> {
    /// Returns a mutable reference to the mir context
    pub fn context_mut<'c>(&'c mut self) -> &'c mut MirContext<'code> {
        &mut self.mir.contexts[self.current_context as usize]
    }

    /// Returns a shared reference to the mir context
    pub fn context(&self) -> &MirContext {
        &self.mir.contexts[self.current_context as usize]
    }

    /// Returns a helper struct that can be used to work on the mir with an arena
    pub fn context_info<'c>(&'c mut self) -> MirContextInfo<'c, 'code> {
        self.mir.context(self.current_context as usize)
    }

    /// Returns a mutable reference to the namespace
    pub fn namespace_mut(&mut self) -> &mut Namespace<MirNamespaceEntry> {
        let index = self.context_info().context.namespace_idx;
        &mut self.arena_mut()[index]
    }

    /// Returns a mutable reference to the arena
    pub fn arena_mut(&mut self) -> &mut NamespaceArena {
        &mut self.mir.namespaces
    }

    /// Returns an immutable reference to the arena
    pub fn arena(&self) -> &NamespaceArena {
        &self.mir.namespaces
    }
}

/// Helper struct which can hold mutable references to the arena and the context
pub struct MirContextInfo<'a, 'code> {
    pub context: &'a mut MirContext<'code>,
    pub arena: &'a mut NamespaceArena,
}

impl<'a, 'b> MirContextInfo<'a, 'b> {
    pub fn add_value(self, ident: Ident, value: MirValue, span: Span) -> Result<()> {
        self.context.add_value(self.arena, ident, value, span)
    }

    pub fn get_from_spanned_ident(self, spanned_ident: &SpannedIdentifier) -> Result<&'a MirValue> {
        self.context
            .get_from_spanned_ident(self.arena, spanned_ident)
    }

    pub fn resolve_path(self, path: &IdentifierPath) -> Result<MirValue> {
        self.context.resolve_path(self.arena, path)
    }

    pub fn register(self, module: ObjModule) {
        self.context.register(self.arena, module)
    }

    pub fn register_function_call(
        self,
        function: ObjectRef,
        parameters: Vec<MirValue>,
        span: Span,
    ) -> Result<(MirValue, MirNode)> {
        self.context
            .register_function_call(self.arena, function, parameters, span)
    }
}

/// A value in the mir namespace
#[derive(Eq, PartialEq, Clone)]
pub enum MirNamespaceEntry {
    /// A spanned entry corresponds to a variable declared somewhere
    Spanned { span: Span, value: MirValue },
    /// An anonymous value is usually a temporary value
    Anonymous(MirValue),
}

impl MirNamespaceEntry {
    pub fn value(&self) -> &MirValue {
        match self {
            MirNamespaceEntry::Spanned { span: _, value } => value,
            MirNamespaceEntry::Anonymous(value) => value,
        }
    }

    pub fn span(&self) -> Option<&Span> {
        match self {
            MirNamespaceEntry::Anonymous(_value) => None,
            MirNamespaceEntry::Spanned { span, value: _ } => Some(span),
        }
    }
}

pub type NamespaceArena = Arena<Namespace<MirNamespaceEntry>>;

/// Keeps track of single context, which can be a function, a loops or something
/// else. As a rule of thumb, everything which has its own namespace is a context.
pub struct MirContext<'code> {
    /// The code of this context
    pub code: CodeRef<'code>,
    /// A ref to the global compile context
    pub compile_context: Rc<CompileContext>,
    /// The context id
    pub id: u64,
    /// All mir nodes that are emitted
    pub nodes: Vec<MirNode>,
    /// The used namespace
    pub namespace_idx: Index,
}

impl<'code> MirContext<'code> {
    /// Creates a new context
    ///
    /// The id has to uniquely identify this context.
    /// If the ancestor index is not None, then this context will be a child
    /// of the ancestor and have access to its namespace
    pub fn new(
        arena: &mut Arena<Namespace<MirNamespaceEntry>>,
        ancestor_index: Option<Index>,
        id: u64,
        compile_context: Rc<CompileContext>,
        code: CodeRef<'code>,
    ) -> Self {
        let namespace_idx = ancestor_index.unwrap_or_else(|| arena.insert_with(Namespace::from));

        MirContext {
            code,
            compile_context,
            id,
            nodes: Vec::default(),
            namespace_idx,
        }
    }

    pub fn namespace_mut<'a>(
        &self,
        arena: &'a mut NamespaceArena,
    ) -> &'a mut Namespace<MirNamespaceEntry> {
        &mut arena[self.namespace_idx]
    }

    pub fn namespace<'a>(&self, arena: &'a NamespaceArena) -> &'a Namespace<MirNamespaceEntry> {
        &arena[self.namespace_idx]
    }

    /// Loads the contents of this `module` into the scope
    ///
    /// Returns whether the module was successfully loaded.
    /// A module cannot load successfully, if it is already loaded.
    pub fn register(&mut self, arena: &mut NamespaceArena, module: ObjModule) {
        let ident = module.ident().clone();
        let entry = MirNamespaceEntry::Anonymous(module.into_object(&self.compile_context).into());

        self.namespace_mut(arena).add_object(ident, entry).ok();
        self.namespace(arena);
    }

    /// Creates a function call    
    pub fn register_function_call(
        &mut self,
        arena: &mut NamespaceArena,
        function: ObjectRef,
        parameters: Vec<MirValue>,
        span: Span,
    ) -> Result<(MirValue, MirNode)> {
        let obj_func = function.downcast_payload::<ObjFunction>().ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedType {
                    expected: ObjFunction::class(&self.compile_context),
                    got: function.class.clone(),
                },
                span,
            )
        })?;

        let signature = obj_func
            .signature(parameters.iter().map(|val| val.class().as_ref()))
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::UnexpectedOverload {
                        parameters: parameters.iter().map(MirValue::class).cloned().collect(),
                    },
                    span,
                )
            })?;

        let return_value = self.add_anonymous_template(arena, signature.return_type().clone());

        Ok((
            return_value.clone(),
            MirNode::Call(MirCall {
                parameters,
                return_value,
                span,
                value: function,
            }),
        ))
    }

    /// Adds a value that corresponds to `ident`
    ///
    /// If the value is a template, it is already added and only a namespace entry has to be created.
    /// Returns `Err` if the ident is already defined.
    pub fn add_value(
        &self,
        arena: &mut NamespaceArena,
        ident: Ident,
        value: MirValue,
        span: Span,
    ) -> Result<()> {
        self.namespace_mut(arena)
            .add_object(ident.clone(), MirNamespaceEntry::Spanned { span, value })
            .map_err(|id| {
                LangError::new(
                    LangErrorKind::VariableAlreadyDefined {
                        name: ident.to_string(),
                        previous_definition: *self
                            .namespace(arena)
                            .get_by_id(id)
                            .unwrap()
                            .span()
                            .unwrap(),
                    },
                    span,
                )
                .into()
            })
    }

    /// Adds an anonymous object and returns a ref MirValue
    pub fn add_anonymous_object(&self, arena: &mut NamespaceArena, object: ObjectRef) -> MirValue {
        let mir_value = MirValue::Concrete(object);
        self.namespace_mut(arena)
            .add_value(MirNamespaceEntry::Anonymous(mir_value.clone()));
        mir_value
    }

    /// Adds an anonymous value that is usually used temporarily
    ///
    /// Returns the template as a MirValue.
    pub fn add_anonymous_template(&self, arena: &mut NamespaceArena, class: ClassRef) -> MirValue {
        let new_uid = self.namespace(arena).next_id();
        let value = MirValue::Template { id: new_uid, class };

        self.namespace_mut(arena)
            .add_value(MirNamespaceEntry::Anonymous(value.clone()));

        value
    }

    /// Looks up the value that corresponds to this ident
    pub fn get_from_spanned_ident<'a>(
        &self,
        arena: &'a NamespaceArena,
        spanned_ident: &SpannedIdentifier,
    ) -> Result<&'a MirValue> {
        // Recursively checks the ancestor namespaces, if the value was not found in the current
        let mut current_namespace = Some(self.namespace_idx);
        while let Some(namespace_idx) = current_namespace {
            let namespace = arena
                .get(namespace_idx)
                .expect("This index is always valid");

            if let Some(value) = namespace.get(arena, &self.get_ident(spanned_ident)) {
                return Ok(value.value());
            }
            current_namespace = namespace.ancestor();
        }

        Err(LangError::new(
            LangErrorKind::MissingVariable {
                var_name: self.get_ident(spanned_ident),
                similar: vec![], // Todo
            },
            spanned_ident.span,
        )
        .into())
    }

    /// Returns an ident from a span
    pub fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        Ident::new(self.get_span_str(spanned_ident.span))
    }

    /// Resolves the accessor and returns the accessed element
    pub fn resolve_path(&self, arena: &NamespaceArena, path: &IdentifierPath) -> Result<MirValue> {
        self.resolve_idents(arena, &path.idents)
    }

    fn get_span_str(&self, span: Span) -> &str {
        self.compile_context.input_files.get_span_str(span)
    }

    fn resolve_idents(
        &self,
        arena: &NamespaceArena,
        idents: &[SpannedIdentifier],
    ) -> Result<MirValue> {
        if let [first, rest @ ..] = idents {
            let mut last_ident = None;
            let mut value = self.get_from_spanned_ident(arena, first)?.clone();

            for property in rest {
                let ident = self.get_ident(property);

                let child = value.get_property(&ident).ok_or_else(|| {
                    LangError::new(
                        LangErrorKind::MissingProperty {
                            parent: last_ident.unwrap_or_else(|| self.get_ident(first)),
                            property: self.get_ident(property),
                            similar: vec![],
                        },
                        property.span,
                    )
                })?;

                value = MirValue::Concrete(child);
                last_ident = Some(ident);
            }

            Ok(value)
        } else {
            panic!("Got empty identifier vec")
        }
    }
}

/// Shows reduced debug info for better readability
impl std::fmt::Debug for MirContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MirContext")
            .field("id", &self.id)
            .field("nodes", &self.nodes)
            .field("namespace_idx", &self.namespace_idx)
            .finish()
    }
}

impl std::fmt::Debug for MirNamespaceEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Entry").finish()
    }
}

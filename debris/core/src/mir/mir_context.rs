use std::{
    iter,
    ops::{Deref, DerefMut},
};

use debris_common::{CodeRef, Ident, Span};
use generational_arena::{Arena, Index};

use crate::{
    error::LangError,
    error::LangErrorKind,
    error::Result,
    hir::IdentifierPath,
    hir::SpannedIdentifier,
    llir::utils::ItemId,
    objects::HasClass,
    objects::ObjFunction,
    objects::{GenericClassRef, ObjModule},
    CompileContext, Namespace, ObjectRef, TypePattern, ValidPayload,
};

use super::{mir_nodes::MirCall, Mir, MirNode, MirValue};

/// Struct that is passed around when working with the mir context
pub struct MirInfo<'a, 'code> {
    pub mir: &'a mut Mir<'code>,
    pub current_context: ContextId,
}

impl<'code> MirInfo<'_, 'code> {
    /// Returns a mutable reference to the mir context
    pub fn context_mut<'c>(&'c mut self) -> &'c mut MirContext<'code> {
        self.mir.contexts.get_mut(self.current_context)
    }

    /// Returns a shared reference to the mir context
    pub fn context(&self) -> &MirContext {
        &self.mir.contexts.get(self.current_context)
    }

    /// Returns a helper struct that can be used to work on the mir with an arena
    pub fn context_info<'c>(&'c mut self) -> MirContextInfo<'c, 'code> {
        self.mir.context(self.current_context)
    }

    /// Returns a mutable reference to the namespace
    pub fn namespace_mut(&mut self) -> &mut Namespace<MirNamespaceEntry> {
        let index = self.context_info().context.id;
        &mut self.arena_mut()[index.0]
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

    pub fn resolve_path(self, path: &IdentifierPath) -> Result<AccessedProperty> {
        self.context.resolve_path(self.arena, path)
    }

    pub fn register(self, module: ObjModule) {
        self.context.register(self.arena, module)
    }

    pub fn register_function_call(
        self,
        function: ObjectRef,
        parameters: Vec<MirValue>,
        parent: Option<MirValue>,
        span: Span,
    ) -> Result<(MirValue, MirNode)> {
        self.context
            .register_function_call(self.arena, function, parameters, parent, span)
    }
}

/// A value in the mir namespace
#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Default)]
pub struct NamespaceArena(Arena<Namespace<MirNamespaceEntry>>);

impl NamespaceArena {
    pub(crate) fn find_value(&self, start: ContextId, ident: &Ident) -> Option<&MirValue> {
        // Recursively checks the ancestor namespaces, if the value was not found in the current
        let mut current_namespace = Some(start.0);
        while let Some(namespace_idx) = current_namespace {
            let namespace = self.get(namespace_idx).expect("This index is always valid");

            if let Some(value) = namespace.get(self, ident) {
                return Some(value.value());
            }
            current_namespace = namespace.ancestor();
        }

        None
    }

    /// Replaces the old value with this id at the namespace at index with the new value
    pub(crate) fn replace_with_id(
        &mut self,
        id: u64,
        index: Index,
        value: MirNamespaceEntry,
    ) -> MirNamespaceEntry {
        let namespace = &mut self[index];
        namespace.replace_object_at(id, value)
    }

    pub(crate) fn get_by_id(&self, id: u64, index: Index) -> Option<&MirValue> {
        self[index].get_by_id(id).map(MirNamespaceEntry::value)
    }

    // pub(crate) fn set_object()
}

impl Deref for NamespaceArena {
    type Target = Arena<Namespace<MirNamespaceEntry>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NamespaceArena {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ContextId(Index);

impl ContextId {
    pub fn as_inner(&self) -> Index {
        self.0
    }
}

/// Keeps track of single context, which can be a function, a loops or something
/// else. As a rule of thumb, everything which has its own namespace is a context.
pub struct MirContext<'ctx> {
    /// The code of this context
    pub code: CodeRef<'ctx>,
    /// A ref to the global compile context
    pub compile_context: &'ctx CompileContext,
    /// The context id and the id of the corresponding namespace
    pub id: ContextId,
    /// All mir nodes that are emitted
    pub nodes: Vec<MirNode>,
}

impl<'ctx> MirContext<'ctx> {
    /// Creates a new context
    ///
    /// If the ancestor index is not None, then this context will be a child
    /// of the ancestor and have access to its namespace
    pub fn new(
        arena: &mut NamespaceArena,
        ancestor_context: Option<ContextId>,
        compile_context: &'ctx CompileContext,
        code: CodeRef<'ctx>,
    ) -> Self {
        // let namespace_idx =
        //     dbg!(ancestor_index.unwrap_or_else(|| arena.insert_with(Namespace::from)));

        let namespace_idx = arena
            .insert_with(|index| Namespace::new(index, ancestor_context.map(|ctx_id| ctx_id.0)));

        MirContext {
            code,
            compile_context,
            id: ContextId(namespace_idx),
            nodes: Vec::default(),
        }
    }

    pub fn namespace_mut<'a>(
        &self,
        arena: &'a mut NamespaceArena,
    ) -> &'a mut Namespace<MirNamespaceEntry> {
        &mut arena[self.id.0]
    }

    pub fn namespace<'a>(&self, arena: &'a NamespaceArena) -> &'a Namespace<MirNamespaceEntry> {
        &arena[self.id.0]
    }

    /// Loads the module into this scope
    pub fn register(&mut self, arena: &mut NamespaceArena, module: ObjModule) {
        let ident = module.ident().clone();
        let entry = MirNamespaceEntry::Anonymous(module.into_object(&self.compile_context).into());

        self.namespace_mut(arena).add_object(ident, entry);
    }

    /// Loads every member of the module (but not the module itself) into this scope
    pub fn register_members(&mut self, arena: &mut NamespaceArena, module: ObjModule) {
        let namespace = self.namespace_mut(arena);
        for (ident, member) in module.members() {
            let entry = MirNamespaceEntry::Anonymous(member.clone().into());
            namespace.add_object(ident.clone(), entry);
        }
    }

    /// Creates a function call    
    pub fn register_function_call(
        &mut self,
        arena: &mut NamespaceArena,
        function: ObjectRef,
        mut parameters: Vec<MirValue>,
        parent: Option<MirValue>,
        span: Span,
    ) -> Result<(MirValue, MirNode)> {
        let obj_func = function.payload.as_function().ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedConversion {
                    target: ObjFunction::class(&self.compile_context).as_generic_ref(),
                    got: function.class.clone(),
                    note: format!("{} cannot be treated as a function", function.class),
                },
                span,
            )
        })?;

        let overload = {
            if let Some(sig) =
                obj_func.overload(parameters.iter().map(|value| value.class().as_ref()))
            {
                Some(sig)
            } else {
                // Otherwise try the function with the parent (aka self value) as first argument
                if let Some(parent) = parent {
                    if let Some(sig) = obj_func.overload(
                        iter::once(parent.class().as_ref())
                            .chain(parameters.iter().map(|value| value.class().as_ref())),
                    ) {
                        parameters.insert(0, parent);
                        Some(sig)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
        .ok_or_else(|| {
            LangError::new(
                LangErrorKind::UnexpectedOverload {
                    parameters: parameters.iter().map(MirValue::class).cloned().collect(),
                    expected: obj_func
                        .expected_signatures()
                        .map(|sig| (sig.parameters().clone(), sig.return_type().clone().into()))
                        .collect(),
                },
                span,
            )
        })?;

        let return_value =
            self.add_anonymous_template(arena, overload.signature().return_type().clone());

        Ok((
            return_value.clone(),
            MirNode::Call(MirCall {
                parameters,
                return_value,
                span,
                function: overload.function(),
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
        let old_id = self
            .namespace_mut(arena)
            .add_object(ident.clone(), MirNamespaceEntry::Spanned { span, value });

        if let Some(id) = old_id {
            Err(LangError::new(
                LangErrorKind::VariableAlreadyDefined {
                    name: ident.to_string(),
                    previous_definition: self
                        .namespace(arena)
                        .get_by_id(id)
                        .unwrap()
                        .span()
                        .copied()
                        .unwrap_or_else(Span::empty),
                },
                span,
            )
            .into())
        } else {
            Ok(())
        }
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
    pub fn add_anonymous_template(
        &self,
        arena: &mut NamespaceArena,
        class: GenericClassRef,
    ) -> MirValue {
        let new_uid = self.namespace(arena).next_id();
        let value = MirValue::Template {
            id: ItemId {
                id: new_uid,
                context: self.id,
            },
            class,
        };

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
        arena
            .find_value(self.id, &self.get_ident(spanned_ident))
            .ok_or_else(|| {
                LangError::new(
                    LangErrorKind::MissingVariable {
                        var_name: self.get_ident(spanned_ident),
                        similar: vec![], // Todo
                    },
                    spanned_ident.span,
                )
                .into()
            })
    }

    /// Returns an ident from a span
    pub fn get_ident(&self, spanned_ident: &SpannedIdentifier) -> Ident {
        Ident::new(self.get_span_str(spanned_ident.span))
    }

    /// Returns the type pattern that belongs to a single spanned identifier
    pub fn get_type_pattern(&self, spanned_ident: &SpannedIdentifier) -> Result<TypePattern> {
        let span = spanned_ident.span;
        let ident = self.get_ident(spanned_ident).to_string();

        let pattern = TypePattern::from_str(&ident, self.compile_context)
            .ok_or_else(|| LangError::new(LangErrorKind::UnexpectedPattern { got: ident }, span))?;

        Ok(pattern)
    }

    /// Resolves the accessor and returns the accessed element
    pub fn resolve_path(
        &self,
        arena: &NamespaceArena,
        path: &IdentifierPath,
    ) -> Result<AccessedProperty> {
        self.resolve_idents(arena, &path.idents)
    }

    fn get_span_str(&self, span: Span) -> &str {
        self.compile_context.input_files.get_span_str(span)
    }

    fn resolve_idents(
        &self,
        arena: &NamespaceArena,
        idents: &[SpannedIdentifier],
    ) -> Result<AccessedProperty> {
        if let [first, rest @ ..] = idents {
            let mut last_ident = None;
            let mut last_value = None;
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

                last_value = Some(value);
                value = MirValue::Concrete(child);
                last_ident = Some(ident);
            }

            Ok(AccessedProperty {
                parent: last_value,
                value,
            })
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
            .finish()
    }
}

pub struct AccessedProperty {
    pub parent: Option<MirValue>,
    pub value: MirValue,
}

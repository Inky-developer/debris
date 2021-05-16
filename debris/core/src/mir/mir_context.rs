use std::{fmt, iter};

use debris_common::{Ident, Span};

use crate::{
    class::ClassRef,
    error::{LangError, LangErrorKind, Result},
    hir::{IdentifierPath, SpannedIdentifier},
    llir::utils::ItemId,
    namespace::NamespaceEntry,
    objects::{
        obj_class::{HasClass, ObjClass},
        obj_function::ObjFunction,
        obj_module::ObjModule,
    },
    CompileContext, Namespace, ObjectRef, TypePattern, ValidPayload,
};

use super::{mir_nodes::MirCall, ContextKind, ControlFlowMode, MirNode, MirValue};

/// Helper struct which can hold mutable references to the arena and the context
pub struct MirContextInfo<'a, 'code> {
    pub context: &'a mut MirContext<'code>,
    pub arena: &'a mut NamespaceArena,
}

impl<'a> MirContextInfo<'a, '_> {
    /// Pushes a node to the current context
    pub fn push(self, node: MirNode) {
        self.context.nodes.push(node)
    }

    /// Adds a value to the current namespace.
    /// Fails if the ident is already associated with a value
    pub fn add_unique_value(self, ident: Ident, value: MirValue, span: Span) -> Result<()> {
        self.context
            .add_unique_value(self.arena, ident, value, span)
    }

    /// Adds a value to the current value. Does not complain if there already exists
    /// a value with the same name (For example used for function parameters)
    pub fn add_value(self, ident: Ident, value: MirValue, span: Span) {
        self.context.add_value(self.arena, ident, value, span)
    }

    /// Adds the class as an anonymous template and returns
    /// it as a `MirValue`
    pub fn add_anonymous_template(self, class: ClassRef) -> MirValue {
        self.context.add_anonymous_template(self.arena, class)
    }

    pub fn declare_as_variable(self, id: ItemId, span: Span) {
        self.arena
            .get_mut(id.context.as_inner())
            .declare_as_variable(id.id, span)
    }

    pub fn convert_into_template(self, item: ItemId) {
        let namespace = self.arena.get_mut(item.context.0);
        let entry = namespace.get_by_id(item.id).unwrap();
        if entry.value().concrete().is_some() {
            let new_value = match entry {
                NamespaceEntry::Variable { span, value } => {
                    let new_value = MirValue::Template {
                        id: item,
                        class: value.class().clone(),
                    };
                    NamespaceEntry::Variable {
                        span: *span,
                        value: new_value,
                    }
                }
                NamespaceEntry::Anonymous(value) => NamespaceEntry::Anonymous(MirValue::Template {
                    id: item,
                    class: value.class().clone(),
                }),
            };
            namespace.replace_object_at(item.id, new_value);
        }
    }

    /// Returns the value that is associated with this ident
    pub fn get_from_spanned_ident(self, spanned_ident: &SpannedIdentifier) -> Result<&'a MirValue> {
        self.context
            .get_from_spanned_ident(self.arena, spanned_ident)
    }

    /// Follows this path
    pub fn resolve_path(self, path: &IdentifierPath) -> Result<AccessedProperty> {
        self.context.resolve_path(self.arena, path)
    }

    /// Adds a function call to the current context
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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct NamespaceIndex(u32);

impl fmt::Display for NamespaceIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Idx({})", self.0)
    }
}

// This attribute does not seem to work at all.
// #[cfg(test)]
impl NamespaceIndex {
    pub fn from_raw(value: u32) -> Self {
        NamespaceIndex(value)
    }
}

#[derive(Debug, Default)]
pub struct NamespaceArena {
    namespaces: Vec<Namespace>,
}

impl NamespaceArena {
    #[inline]
    pub fn get(&self, index: NamespaceIndex) -> &Namespace {
        &self.namespaces[index.0 as usize]
    }

    #[inline]
    pub fn get_mut(&mut self, index: NamespaceIndex) -> &mut Namespace {
        &mut self.namespaces[index.0 as usize]
    }

    pub fn insert_with<F>(&mut self, func: F) -> NamespaceIndex
    where
        F: FnOnce(NamespaceIndex) -> Namespace,
    {
        let index = NamespaceIndex(self.namespaces.len() as u32);
        let value = func(index);
        self.namespaces.push(value);
        index
    }

    pub fn search(&self, start: ContextId, ident: &Ident) -> Option<(ItemId, &NamespaceEntry)> {
        // Recursively checks the ancestor namespaces, if the value was not found in the current
        let mut current_namespace = Some(start.0);
        while let Some(namespace_idx) = current_namespace {
            let namespace = self.get(namespace_idx);

            if let Some((id, value)) = namespace.get(self, ident) {
                let item_id = ItemId {
                    context: namespace_idx.into(),
                    id,
                };
                return Some((item_id, value));
            }
            current_namespace = namespace.ancestor();
        }

        None
    }

    pub(crate) fn find_value(&self, start: ContextId, ident: &Ident) -> Option<&MirValue> {
        self.search(start, ident).map(|(_, value)| value.value())
    }

    /// Replaces the old value with this id at the namespace at index with the new value
    pub(crate) fn replace_with_id(
        &mut self,
        id: u32,
        index: NamespaceIndex,
        value: NamespaceEntry,
    ) -> NamespaceEntry {
        let namespace = self.get_mut(index);
        namespace.replace_object_at(id, value)
    }

    pub(crate) fn get_by_id(&self, id: u32, index: NamespaceIndex) -> Option<&MirValue> {
        self.get(index).get_by_id(id).map(NamespaceEntry::value)
    }

    // pub(crate) fn set_object()
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ContextId(NamespaceIndex);

impl ContextId {
    pub fn as_inner(&self) -> NamespaceIndex {
        self.0
    }
}

// This attribute does not seem to work at all.
// Just make this method public now, even though it should
// not be used outside of tests
// #[cfg(test)]
impl ContextId {
    pub fn dummy(id: u32) -> ContextId {
        ContextId(NamespaceIndex::from_raw(id))
    }
}

impl From<NamespaceIndex> for ContextId {
    fn from(value: NamespaceIndex) -> Self {
        ContextId(value)
    }
}

impl fmt::Display for ContextId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_inner())
    }
}

#[derive(Debug)]
pub struct ReturnValues {
    values: Vec<MirValue>,
    /// Stores the value and the span it is returned at
    pub template: Option<(MirValue, Span)>,
    /// The value that is returned by default (when template is None)
    pub default_return: ObjectRef,
}

impl ReturnValues {
    fn new(default_return: ObjectRef) -> Self {
        ReturnValues {
            values: Vec::new(),
            template: None,
            default_return,
        }
    }

    pub fn add(&mut self, value: MirValue) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn get(&self, index: usize) -> Option<&MirValue> {
        self.values.get(index)
    }

    /// Returns the first value if the length of `value` is exactly one,
    /// otherwise `None`
    pub fn get_single(&self) -> Option<MirValue> {
        match self.values.as_slice() {
            [single] => Some(single.clone()),
            _ => None,
        }
    }

    pub fn get_template(&self) -> Option<&(MirValue, Span)> {
        self.template.as_ref()
    }

    pub fn get_template_or_default(&self) -> MirValue {
        match self.template.as_ref() {
            Some((template, _)) => template.clone(),
            None => self.default_return.clone().into(),
        }
    }
}

/// Keeps track of single context, which can be a function, a loops or something
/// else. As a rule of thumb, everything which has its own namespace is a context.
pub struct MirContext<'ctx> {
    /// A ref to the global compile context
    pub compile_context: &'ctx CompileContext,
    pub span: Span,
    pub kind: ContextKind,
    /// The context id and the id of the corresponding namespace
    pub id: ContextId,
    /// All mir nodes that are emitted
    pub nodes: Vec<MirNode>,
    /// Which control flow mode is used for this context
    /// Most context just do nothing on end, but it is also
    /// possible to return from a function or to break from
    /// a loop
    pub control_flow: ControlFlowMode,
    /// The values returned by this function
    pub return_values: ReturnValues,
    /// Holds the current jump location index
    jump_location_counter: usize,
}

impl<'ctx> MirContext<'ctx> {
    /// Creates a new context
    ///
    /// If the ancestor index is not None, then this context will be a child
    /// of the ancestor and have access to its namespace
    pub fn new(
        ctx: &'ctx CompileContext,
        arena: &mut NamespaceArena,
        ancestor_context: Option<ContextId>,
        span: Span,
        kind: ContextKind,
    ) -> Self {
        let namespace_idx = arena.insert_with(|index| {
            Namespace::new(ContextId(index), ancestor_context.map(|ctx_id| ctx_id.0))
        });

        MirContext {
            compile_context: ctx,
            span,
            kind,
            id: ContextId(namespace_idx),
            nodes: Vec::default(),
            control_flow: Default::default(),
            return_values: ReturnValues::new(kind.default_return(ctx)),
            jump_location_counter: 0,
        }
    }

    pub fn namespace_mut<'a>(&self, arena: &'a mut NamespaceArena) -> &'a mut Namespace {
        arena.get_mut(self.id.0)
    }

    pub fn namespace<'a>(&self, arena: &'a NamespaceArena) -> &'a Namespace {
        arena.get(self.id.0)
    }

    /// Increments the jump location counter and returns it
    pub fn next_jump_location(&mut self) -> usize {
        self.jump_location_counter += 1;
        self.jump_location_counter
    }

    /// Loads the module into this scope
    pub fn register(&mut self, arena: &mut NamespaceArena, module: ObjModule) {
        let ident = module.ident().clone();
        let entry = NamespaceEntry::Anonymous(module.into_object(&self.compile_context).into());

        self.namespace_mut(arena).add_object(ident, entry);
    }

    /// Loads every member of the module (but not the module itself) into this scope
    pub fn register_members(&mut self, arena: &mut NamespaceArena, module: ObjModule) {
        let namespace = self.namespace_mut(arena);
        for (ident, member) in module.members() {
            let entry = NamespaceEntry::Anonymous(member.clone().into());
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
                LangErrorKind::UnexpectedType {
                    expected: TypePattern::Class(ObjFunction::class(self.compile_context)),
                    got: function.class.clone(),
                    declared: None,
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
                    parameters: parameters.iter().map(|val| val.class().clone()).collect(),
                    expected: obj_func
                        .expected_signatures()
                        .map(|sig| (sig.parameters.clone(), sig.return_type.clone().into()))
                        .collect(),
                },
                span,
            )
        })?;

        let return_value =
            self.add_anonymous_template(arena, overload.signature.return_type.clone());

        Ok((
            return_value.clone(),
            MirNode::Call(MirCall {
                parameters,
                return_value,
                span,
                function: overload.callback_function.clone(),
                value: function,
            }),
        ))
    }

    /// Adds a value that corresponds to `ident`
    ///
    /// If the value is a template, it is already added and only a namespace entry has to be created.
    /// Returns `Err` if the ident is already defined.
    pub fn add_unique_value(
        &self,
        arena: &mut NamespaceArena,
        ident: Ident,
        value: MirValue,
        span: Span,
    ) -> Result<()> {
        if let Some(_old_value) = arena.find_value(self.id, &ident) {
            return Err(LangError::new(
                LangErrorKind::VariableAlreadyDefined {
                    name: ident.to_string(),
                    previous_definition: self
                        .namespace(arena)
                        .get(arena, &ident)
                        .unwrap()
                        .1
                        .span()
                        .copied()
                        .unwrap_or_else(Span::empty),
                },
                span,
            )
            .into());
        }
        self.add_value(arena, ident, value, span);
        Ok(())
    }

    pub fn add_value(&self, arena: &mut NamespaceArena, ident: Ident, value: MirValue, span: Span) {
        self.namespace_mut(arena)
            .add_object(ident, NamespaceEntry::Variable { span, value });
    }

    /// Adds an anonymous object and returns a ref MirValue
    pub fn add_anonymous_object(&self, arena: &mut NamespaceArena, object: ObjectRef) -> MirValue {
        let mir_value = MirValue::Concrete(object);
        self.namespace_mut(arena)
            .add_value(NamespaceEntry::Anonymous(mir_value.clone()));
        mir_value
    }

    /// Adds an anonymous value that is usually used temporarily
    ///
    /// Returns the template as a MirValue.
    pub fn add_anonymous_template(&self, arena: &mut NamespaceArena, class: ClassRef) -> MirValue {
        let new_uid = self.namespace_mut(arena).id_counter.next_id();
        let value = MirValue::Template {
            id: ItemId {
                id: new_uid,
                context: self.id,
            },
            class,
        };

        self.namespace_mut(arena)
            .add_value_at(new_uid, NamespaceEntry::Anonymous(value.clone()));

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
                        notes: vec![],
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

    /// Returns the type pattern that belongs to a path
    pub fn get_type_pattern(
        &self,
        arena: &NamespaceArena,
        path: &IdentifierPath,
    ) -> Result<TypePattern> {
        if let [single_ident] = path.idents() {
            let ident = self.get_ident(single_ident);
            if let Ident::Value(string) = ident {
                if string == "Any" {
                    return Ok(TypePattern::Any);
                }
            }
        }

        let AccessedProperty { value, .. } = self.resolve_path(arena, path)?;
        let class = match value.signature_class() {
            Some(class) => class,
            None => {
                return Err(LangError::new(
                    LangErrorKind::UnexpectedType {
                        got: value.class().clone(),
                        expected: TypePattern::Class(ObjClass::class(self.compile_context)),
                        declared: None,
                    },
                    path.span(),
                )
                .into())
            }
        };
        Ok(TypePattern::Class(class))
    }

    /// Resolves the accessor and returns the accessed element
    pub fn resolve_path(
        &self,
        arena: &NamespaceArena,
        path: &IdentifierPath,
    ) -> Result<AccessedProperty> {
        self.resolve_idents(arena, path.idents())
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
            let mut last_value = None;
            let (_, entry) = arena
                .search(self.id, &self.get_ident(first))
                .ok_or_else(|| {
                    LangError::new(
                        LangErrorKind::MissingVariable {
                            var_name: self.get_ident(first),
                            similar: vec![], // Todo
                            notes: vec![],
                        },
                        first.span,
                    )
                })?;
            let mut value = entry.value().clone();
            let span = entry.span().copied();

            // sanity check
            if value.is_template() && value.class().kind.typ().should_be_const() {
                return Err(LangError::new(
                    LangErrorKind::NotYetImplemented {
                        msg: "Failed to evaluate a constant variable: This value should be known, but it is not".to_string(),
                    },
                    first.span,
                )
                .into());
            }

            for property in rest {
                let ident = self.get_ident(property);
                let child = value.get_property(arena, &ident).ok_or_else(|| {
                    LangError::new(
                        LangErrorKind::MissingProperty {
                            parent: value.class().clone(),
                            property: self.get_ident(property),
                            similar: vec![],
                        },
                        property.span,
                    )
                })?;

                last_value = Some(value);
                value = child;
            }

            Ok(AccessedProperty {
                parent: last_value,
                value,
                span,
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
    pub span: Option<Span>,
}

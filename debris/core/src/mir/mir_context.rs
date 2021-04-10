use std::{
    fmt, iter,
    ops::{Deref, DerefMut},
};

use debris_common::{Ident, Span};
use generational_arena::{Arena, Index};

use crate::{
    error::{LangError, LangErrorKind, Result},
    hir::{IdentifierPath, SpannedIdentifier},
    llir::utils::ItemId,
    namespace::NamespaceEntry,
    objects::{
        obj_class::{GenericClass, GenericClassRef, HasClass, ObjClass},
        obj_function::ObjFunction,
        obj_module::ObjModule,
        obj_struct_object::ObjStructObject,
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
    pub fn add_value(self, ident: Ident, value: MirValue, span: Span) -> Result<()> {
        self.context.add_value(self.arena, ident, value, span)
    }

    /// Adds the class as an anonymous template and returns
    /// it as a `MirValue`
    pub fn add_anonymous_template(self, class: GenericClassRef) -> MirValue {
        self.context.add_anonymous_template(self.arena, class)
    }

    pub fn convert_into_template(self, item: ItemId) {
        let namespace = self.arena.get_mut(item.context.0).unwrap();
        let entry = namespace.get_by_id(item.id).unwrap();
        if entry.value().concrete().is_some() {
            let new_value = match entry {
                NamespaceEntry::Spanned { span, value } => {
                    let new_value = MirValue::Template {
                        id: item,
                        class: value.class().clone(),
                    };
                    NamespaceEntry::Spanned {
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

#[derive(Debug, Default)]
pub struct NamespaceArena(Arena<Namespace>);

impl NamespaceArena {
    pub fn search(&self, start: ContextId, ident: &Ident) -> Option<(ItemId, &NamespaceEntry)> {
        // Recursively checks the ancestor namespaces, if the value was not found in the current
        let mut current_namespace = Some(start.0);
        while let Some(namespace_idx) = current_namespace {
            let namespace = self.get(namespace_idx).expect("This index is always valid");

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
        id: usize,
        index: Index,
        value: NamespaceEntry,
    ) -> NamespaceEntry {
        let namespace = &mut self[index];
        namespace.replace_object_at(id, value)
    }

    pub(crate) fn get_by_id(&self, id: usize, index: Index) -> Option<&MirValue> {
        self[index].get_by_id(id).map(NamespaceEntry::value)
    }

    // pub(crate) fn set_object()
}

impl Deref for NamespaceArena {
    type Target = Arena<Namespace>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for NamespaceArena {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash, PartialOrd, Ord)]
pub struct ContextId(Index);

impl ContextId {
    pub fn as_inner(&self) -> Index {
        self.0
    }

    /// For testing purposes, creates a dummy id
    pub fn dummy(id: usize) -> ContextId {
        ContextId(Index::from_raw_parts(id, 0))
    }
}

impl From<Index> for ContextId {
    fn from(value: Index) -> Self {
        ContextId(value)
    }
}

impl fmt::Display for ContextId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_inner().into_raw_parts().0)
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
        &mut arena[self.id.0]
    }

    pub fn namespace<'a>(&self, arena: &'a NamespaceArena) -> &'a Namespace {
        &arena[self.id.0]
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
                    expected: TypePattern::Class(
                        GenericClass::new(&ObjFunction::class(&self.compile_context)).into(),
                    ),
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

        self.namespace_mut(arena)
            .add_object(ident, NamespaceEntry::Spanned { span, value });
        Ok(())
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
    pub fn add_anonymous_template(
        &self,
        arena: &mut NamespaceArena,
        class: GenericClassRef,
    ) -> MirValue {
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

    /// Returns the type pattern that belongs to a single spanned identifier
    pub fn get_type_pattern(
        &self,
        ctx: &CompileContext,
        arena: &NamespaceArena,
        spanned_ident: &SpannedIdentifier,
    ) -> Result<TypePattern> {
        let ident = self.get_ident(spanned_ident).to_string();

        let pattern = match ident.as_str() {
            "Any" => TypePattern::Any,
            _ => {
                let value = self.get_from_spanned_ident(arena, spanned_ident)?;
                value.assert_type(
                    TypePattern::Class(ObjClass::class(ctx).as_generic_ref()),
                    spanned_ident.span,
                    None,
                )?;
                let obj = value.expect_concrete("Classes are always concrete");
                let obj_class = obj
                    .downcast_payload::<ObjClass>()
                    .expect("It was already verified that this is a class");
                TypePattern::Class(obj_class.class.as_generic_ref())
            }
        };

        Ok(pattern)
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
            let mut last_ident = None;
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
            let mut span = entry.span().copied();

            for property in rest {
                let ident = self.get_ident(property);

                let child = value
                    .get_property(&ident)
                    .map(MirValue::Concrete)
                    .or_else(|| {
                        // Temporary hack for struct objects
                        if let Some(value) = value.concrete() {
                            if let Some(struct_obj) = value.downcast_payload::<ObjStructObject>() {
                                arena.get(struct_obj.variables).and_then(|namespace| {
                                    namespace.get(arena, &ident).map(|(_, entry)| {
                                        span = entry.span().copied();
                                        entry.value().clone()
                                    })
                                })
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .ok_or_else(|| {
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
                value = child;
                last_ident = Some(ident);
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

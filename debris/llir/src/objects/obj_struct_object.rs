use std::{collections::HashSet, fmt, hash::BuildHasherDefault};

use debris_common::Ident;
use debris_error::LangErrorKind;
use indexmap::IndexMap;
use rustc_hash::{FxHashMap, FxHasher};

use crate::{
    class::{Class, ClassKind, ClassRef},
    impl_class,
    memory::MemoryLayout,
    type_context::TypeContext,
    ObjectPayload, ObjectRef, Type,
};

use super::obj_struct::StructRef;

#[derive(Debug, PartialEq, Eq)]
pub struct ObjStructObject {
    pub struct_type: StructRef,
    pub properties: FxHashMap<Ident, ObjectRef>,
    memory_layout: MemoryLayout,
}

impl_class! {ObjStructObject, Type::StructObject, {}}

impl ObjStructObject {
    pub fn new(
        struct_type: StructRef,
        properties: FxHashMap<Ident, ObjectRef>,
    ) -> Result<Self, LangErrorKind> {
        check_properties_valid(&struct_type.ident, &properties, &struct_type.fields)?;

        let memory_layout = properties
            .iter()
            .map(|(_, obj)| obj.payload.memory_layout())
            .collect();

        Ok(ObjStructObject {
            struct_type,
            properties,
            memory_layout,
        })
    }
}

impl ObjectPayload for ObjStructObject {
    fn memory_layout(&self) -> &MemoryLayout {
        &self.memory_layout
    }

    fn create_class(&self, _: &TypeContext) -> ClassRef {
        let class_kind = ClassKind::StructObject {
            strukt: self.struct_type.clone(),
        };
        let class = Class::new_empty(class_kind);
        ClassRef::new(class)
    }

    fn get_property(&self, _ctx: &TypeContext, ident: &Ident) -> Option<ObjectRef> {
        self.properties.get(ident).cloned()
    }
}

impl fmt::Display for ObjStructObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{} {{Todo}}", self.struct_type.ident))
    }
}

fn check_properties_valid(
    struct_ident: &Ident,
    properties: &FxHashMap<Ident, ObjectRef>,
    template: &IndexMap<Ident, ClassRef, BuildHasherDefault<FxHasher>>,
) -> Result<(), LangErrorKind> {
    // first check for properties that where supplied but not expected
    let mut useless_args = HashSet::new();
    for ident in properties.keys() {
        if !template.contains_key(ident) {
            useless_args.insert(ident);
        }
    }

    if !useless_args.is_empty() {
        return Err(LangErrorKind::UnexpectedStructInitializer {
            available: template.keys().map(Clone::clone).collect(),
            ident: useless_args.into_iter().next().unwrap().clone(),
            strukt: struct_ident.clone(),
        });
    }

    // then check for properties that were expected but not supplied
    let mut missing_args = HashSet::new();
    for (ident, _class) in template {
        if !properties.contains_key(ident) {
            missing_args.insert(ident);
        }
    }
    if !missing_args.is_empty() {
        return Err(LangErrorKind::MissingStructInitializer {
            missing: missing_args.into_iter().cloned().collect(),
            strukt: struct_ident.clone(),
        });
    }

    Ok(())
}

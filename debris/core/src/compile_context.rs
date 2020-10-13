use std::{any::TypeId, cell::RefCell, collections::HashMap, default::Default};

use crate::objects::ClassRef;
use crate::Config;

/// The Compilation context stores various information about the current compilation
#[derive(Debug, Eq, PartialEq)]
pub struct CompileContext {
    /// Contains all types
    pub type_ctx: TypeContext,
    /// The current config which specifies how to compile
    pub config: Config,
}

/// Used to manage types
#[derive(Debug, Eq, PartialEq, Default)]
pub struct TypeContext {
    /// Cache for classes
    ///
    /// The key is the type of the Payload Struct.
    cache: RefCell<HashMap<TypeId, ClassRef>>,
}

impl Default for CompileContext {
    fn default() -> Self {
        CompileContext {
            config: Config::default(),
            type_ctx: TypeContext::default(),
        }
    }
}

impl TypeContext {
    /// Caches a class ref that corresponds to a specific payload type
    pub fn get_or_insert<F, R>(&self, id: TypeId, or_insert: F) -> ClassRef
    where
        F: Fn() -> R,
        R: Into<ClassRef>,
    {
        let cache = self.cache.borrow();
        if let Some(class) = cache.get(&id) {
            class.clone()
        } else {
            // the or_insert closure can lead to recursive `get_or_insert` calls
            // For this reason drop the early here
            std::mem::drop(cache);
            let class = (or_insert)().into();
            self.cache.borrow_mut().insert(id, class.clone());
            class
        }
    }
}

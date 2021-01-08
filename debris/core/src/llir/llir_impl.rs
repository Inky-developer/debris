use super::{llir_nodes::Function, LLIRBuilder};
use crate::{
    error::Result,
    mir::{ContextId, MirContextMap, NamespaceArena},
};

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug, Eq, PartialEq)]
pub struct Llir {
    /// The functions which were created, the main function at the last index
    pub functions: Vec<Function>,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(contexts: &MirContextMap, namespaces: &mut NamespaceArena) -> Result<Llir> {
        let mut llir_helper = LlirFunctions::default();
        let main_context = contexts.get_main_context();

        LLIRBuilder::new(main_context, namespaces, contexts, &mut llir_helper).build()?;

        Ok(llir_helper.into())
    }
}

/// Contains the already generated llir functions and
/// in the future potentialle other details
#[derive(Default)]
pub(crate) struct LlirFunctions {
    pub functions: Vec<Function>,
}

impl LlirFunctions {
    pub fn push(&mut self, function: Function) {
        self.functions.push(function)
    }

    pub fn find_function(&mut self, id: &ContextId) -> &mut Function {
        self.functions
            .iter_mut()
            .find(|function| &function.id == id)
            .expect("Function not found")
    }
}

impl Into<Llir> for LlirFunctions {
    fn into(self) -> Llir {
        Llir {
            functions: self.functions,
        }
    }
}

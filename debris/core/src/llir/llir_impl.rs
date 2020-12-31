use super::{llir_nodes::Function, LLIRBuilder};
use crate::{error::Result, mir::MirContext, mir::NamespaceArena};

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
    pub fn from_mir(contexts: &[MirContext], namespaces: &mut NamespaceArena) -> Result<Llir> {
        let mut functions = Vec::new();
        let main_context = &contexts[0];
        let main_function =
            LLIRBuilder::new(main_context, namespaces, contexts, &mut functions).build()?;

        functions.push(main_function);

        Ok(Llir { functions })
    }
}

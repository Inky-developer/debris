use super::{llir_nodes::Function, LLIRBuilder};
use crate::{error::Result, mir::MirContext, mir::NamespaceArena};

/// The low-level intermediate representation struct
///
/// Contains all generated functions and a compilation configuration
#[derive(Debug, Eq, PartialEq)]
pub struct Llir {
    /// The functions which were created
    pub functions: Vec<Function>,
}

impl Llir {
    /// Compiles the mir into a llir
    pub fn from_mir(contexts: &[MirContext], namespaces: &mut NamespaceArena) -> Result<Llir> {
        let functions = contexts
            .iter()
            .skip(1) // The first context does not contain anything useful for the code generation
            .map(|context| LLIRBuilder::new(context, namespaces).build())
            .collect::<Result<_>>()?;

        Ok(Llir { functions })
    }
}

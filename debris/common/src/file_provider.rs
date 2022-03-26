//! This module has the [`FileProvider`] trait, which is used by the compile config to resolve import statements.

use std::{ops::Deref, path::PathBuf};

use rustc_hash::FxHashMap;

pub trait FileProvider {
    /// Gets the contents of the file with the specified name
    fn read_file(&self, name: &str) -> Option<Box<str>>;
}

impl<T, U> FileProvider for T
where
    T: Deref<Target = U>,
    U: FileProvider,
{
    fn read_file(&self, name: &str) -> Option<Box<str>> {
        self.deref().read_file(name)
    }
}

/// A [`FileProvider`] implementation that reads files from the file system
#[derive(Debug, Default)]
pub struct FsFileProvider {
    pub root: PathBuf,
}

impl FsFileProvider {
    pub fn new(root: PathBuf) -> Self {
        FsFileProvider { root }
    }
}

impl FileProvider for FsFileProvider {
    fn read_file(&self, name: &str) -> Option<Box<str>> {
        let path = self.root.join(PathBuf::from(name));
        std::fs::read_to_string(path)
            .ok()
            .map(|string| string.into())
    }
}

/// A [`FileProvider`] implementation that has an in-memory pool of files
#[derive(Debug, Default)]
pub struct MemoryFileProvider {
    files: FxHashMap<Box<str>, Box<str>>,
}

impl MemoryFileProvider {
    pub fn add_file(&mut self, path: Box<str>, content: Box<str>) {
        self.files.insert(path, content);
    }
}

impl FileProvider for MemoryFileProvider {
    fn read_file(&self, name: &str) -> Option<Box<str>> {
        self.files.get(name).cloned()
    }
}

//! Virstual in-memory file system.
//! might switch to a more sophisiticated model in the future
use std::io::prelude::*;
use std::{
    collections::HashMap,
    fs::{create_dir_all, OpenOptions},
    io,
    path::Path,
    str,
};

/// Custom macro to easily generate predefined file structures
/// Not very efficient at the moment though
#[macro_export]
macro_rules! directories {
        ($($rest:tt)+) => {{
        #[allow(unused_mut)]
        let mut file_map: std::collections::HashMap<String, $crate::File> = std::collections::HashMap::new();
        #[allow(unused_mut)]
        let mut dir_map: std::collections::HashMap<String, $crate::Directory> = std::collections::HashMap::new();

        $crate::directories_inner!( file_map, dir_map, $($rest)+ );

        $crate::Directory::with_contents(dir_map, file_map)
    }};
    () => {
        $crate::Directory::new()
    }

}

#[macro_export]
macro_rules! directories_inner {
    ($fname:ident, $dname:ident, $k:expr => File($v:expr)) => {
        $fname.insert($k.into(), $crate::File::with_data(&$v));
    };
    ($fname:ident, $dname:ident, $k:expr => File($v:expr), $($rest:tt)+) => {{
        $crate::directories_inner!($fname, $dname, $k => File($v));
        $crate::directories_inner!($fname, $dname, $($rest)+);
    }};
    ($fname:ident, $dname:ident, $k:ident => $v:expr) => {
        $dname.insert(stringify!($k).to_string(), $v);
    };
    ($fname:ident, $dname:ident, $k:ident => $v:expr, $($rest:tt)+) => {{
        $crate::directories_inner!($fname, $dname, $k => $v);
        $crate::directories_inner!($fname, $dname, $($rest)+);
    }};
    ($fname:ident, $dname:ident, $k:expr => $v:expr) => {
        $dname.insert($k.into(), $v);
    };
    ($fname:ident, $dname:ident, $k:expr => $v:expr, $($rest:tt)+) => {{
        $crate::directories_inner!($fname, $dname, $k => $v);
        $crate::directories_inner!($fname, $dname, $($rest)+);
    }};
    () => {
        $crate::Directory::new()
    };
}

#[derive(Debug, Eq, PartialEq)]
pub enum FsElement<'a> {
    File(&'a mut File),
    Directoy(&'a mut Directory),
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct File {
    pub contents: String,
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct Directory {
    pub files: HashMap<String, File>,
    pub directories: HashMap<String, Directory>,
}

impl<'a> FsElement<'a> {
    pub fn persist(&self, name: &str, path: &Path) -> io::Result<()> {
        match self {
            FsElement::Directoy(dir) => dir.persist(name, path),
            FsElement::File(file) => file.persist(name, path),
        }
    }

    pub fn dir(self) -> Option<&'a mut Directory> {
        match self {
            FsElement::Directoy(dir) => Some(dir),
            FsElement::File(_) => None,
        }
    }

    pub fn file(self) -> Option<&'a mut File> {
        match self {
            FsElement::File(file) => Some(file),
            FsElement::Directoy(_) => None,
        }
    }
}

impl File {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_data(data: &str) -> Self {
        File {
            contents: data.to_string(),
        }
    }

    pub fn push_string(&mut self, data: &str) {
        self.contents.push_str(data);
    }

    pub fn persist(&self, name: &str, path: &Path) -> io::Result<()> {
        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(path.join(name))?;

        file.write_all(self.contents.as_bytes())?;

        Ok(())
    }
}

impl Directory {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_contents(
        directories: HashMap<String, Directory>,
        files: HashMap<String, File>,
    ) -> Self {
        Directory { directories, files }
    }

    /// returns a new file with this name or returns an existing file with this name
    pub fn file(&mut self, name: impl Into<String>) -> &mut File {
        self.files.entry(name.into()).or_default()
    }

    /// Returns a new directory with this name or returns an existing directory with this naem
    pub fn dir(&mut self, name: String) -> &mut Directory {
        self.directories.entry(name).or_default()
    }

    pub fn resolve_path(&mut self, path: &[&str]) -> Option<FsElement> {
        match path.split_first() {
            Some((&first, rest)) => {
                if let Some(file) = self.files.get_mut(first) {
                    match rest.is_empty() {
                        true => Some(FsElement::File(file)),
                        false => None,
                    }
                } else if let Some(dir) = self.directories.get_mut(first) {
                    dir.resolve_path(rest)
                } else {
                    None
                }
            }
            None => Some(FsElement::Directoy(self)),
        }
    }

    pub fn persist(&self, name: &str, path: &Path) -> io::Result<()> {
        let own_path = path.join(name);
        create_dir_all(&own_path)?;

        for (dirname, dir) in &self.directories {
            dir.persist(dirname, &own_path)?;
        }

        for (filename, file) in &self.files {
            file.persist(filename, &own_path)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{directories, Directory, File, FsElement};
    use std::collections::HashMap;

    #[test]
    fn file() {
        let file = File::new();
        assert_eq!(file.contents, "");
    }

    #[test]
    fn push_file() {
        let mut file = File::new();
        file.push_string("Foo");
        assert_eq!(file.contents, "Foo");
    }

    #[test]
    fn directory() {
        let dir = Directory::new();
        assert_eq!(dir.directories, HashMap::default());
        assert_eq!(dir.files, HashMap::default());
    }

    #[test]
    fn directory_file() {
        let mut dir = Directory::new();
        dir.file("foo".to_string()).push_string("bar");

        let file = dir.resolve_path(&["foo"]).unwrap();
        match file {
            FsElement::Directoy(_) => panic!(),
            FsElement::File(file) => assert_eq!(file.contents, "bar"),
        }
    }

    #[test]
    fn directory_nested() {
        let mut dir = Directory::new();
        {
            let inner = dir.dir("inner".to_string());
            inner.file("foo".to_string()).push_string("bar");
        }

        let file = dir.resolve_path(&["inner", "foo"]).unwrap();
        match file {
            FsElement::Directoy(_) => panic!(),
            FsElement::File(file) => assert_eq!(file.contents, "bar"),
        }
    }

    #[test]
    fn directoy_default() {
        let pack = directories! {
            "pack.mcmeta" => File("<load default pack mcmeta>"),
            data => directories! {
                minecraft => directories!(),
                debris => directories!()
            },
            src => directories!()
        };

        pack.directories.get("src").expect("No source dir");
        let data = pack.directories.get("data").expect("No data dir");
        data.directories
            .get("minecraft")
            .expect("No minecraft directory");
        data.directories.get("debris").expect("No debris directory");

        assert_eq!(
            pack.files
                .get("pack.mcmeta")
                .expect("No pack.mcmeta file")
                .contents,
            "<load default pack mcmeta>".to_string()
        );
    }
}

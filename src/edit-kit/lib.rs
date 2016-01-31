use ::std::collections::HashMap;
use ::std::vec::Vec;

use ::std::path::Path;

use ::std::fs::File;
use ::std::io::{BufReader, BufRead};

use ::std::sync::{Mutex, LockResult, MutexGuard};

/// The function is used to lock multiple mutexes simultaneously. It
/// ensures mutexes are always locked in the same order to avoid
/// deadlocks.
pub fn lock<'a, T: ?Sized>(mutexes: Vec<&'a Mutex<T>>) -> Vec<LockResult<MutexGuard<'a, T>>> {
    let mut mutexes = mutexes
        .iter()
        .enumerate()
        .collect::<Vec<_>>();
    mutexes.sort_by(|&(_, m1), &(_, m2)| (m1 as *const _).cmp(&(m2 as *const _)));
    let mut mutexes = mutexes
        .iter()
        .map(|&(i, m)| (i, m.lock()))
        .collect::<Vec<_>>();
    mutexes.sort_by(|&(i, _), &(j, _)| i.cmp(&j));

    let mut result = Vec::with_capacity(mutexes.len());
    for (_, m) in mutexes {
        result.push(m);
    }
    result
}

macro_rules! lock {
    ( $( $x:expr => $l:ident ),* ) => {
        {
            let mut locks = lock(vec![$($x),*]);
            let mut i = locks.drain(..);
            $($l = i.next().unwrap().unwrap();)*
        }
    };
}

#[derive(Debug)]
pub struct Editor {
    buffers: HashMap<String, Mutex<Buffer>>,
}

impl Editor {
    pub fn new() -> Editor {
        Editor {
            buffers: HashMap::new(),
        }
    }

    pub fn add_buffer(&mut self, name: String, buffer: Buffer) -> Option<Mutex<Buffer>> {
        self.buffers.insert(name.to_string(), Mutex::new(buffer))
    }

    pub fn get_buffer<S: AsRef<str>>(&self, name: S) -> Option<&Mutex<Buffer>> {
        self.buffers.get(name.as_ref())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Buffer {
    content: Vec<String>,
    pointer: Pointer,
}

impl Buffer {
    pub fn open_file<P: AsRef<Path>>(path: P) -> ::std::io::Result<Buffer> {
        Buffer::from_file(&try!(File::open(path.as_ref())))
    }

    pub fn from_file(file: &File) -> ::std::io::Result<Buffer> {
        let reader = BufReader::new(file);
        let mut lines = Vec::new();
        for line in reader.lines() {
            lines.push(try!(line));
        }

        Ok(Buffer {
            content: lines,
            pointer: Pointer { line: 0, column: 0 },
        })
    }

    pub fn from_string(s: &str) -> Buffer {
        Buffer {
            content: s.lines().map(String::from).collect(),
            pointer: Pointer { line: 0, column: 0 },
        }
    }

    pub fn is_valid_pointer(&self, p: &Pointer) -> bool {
        p.line < self.content.len() && p.column <= self.content[p.line].chars().count()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Pointer {
    line: usize,
    column: usize,
}

#[cfg(test)]
mod test {
    pub use super::*;

    mod buffer {
        pub use super::*;

        #[test]
        fn from_string() {
            assert_eq!(
                Buffer {
                    content: vec!["Hello, world!".to_string(),
                                  "it should have two lines now".to_string()],
                    pointer: Pointer { line: 0, column: 0 },
                },
                Buffer::from_string("Hello, world!\nit should have two lines now")
            );
        }
    }

    mod editor {
        pub use super::*;

        #[test]
        fn can_borrow_two_mut_buffers() {
            let b1 = Buffer::from_string("1");
            let b2 = Buffer::from_string("2");

            let mut editor = Editor::new();
            editor.add_buffer("1".to_string(), b1);
            editor.add_buffer("2".to_string(), b2);

            let (buf1, buf2);
            lock!(
                editor.get_buffer("1").unwrap() => buf1,
                editor.get_buffer("2").unwrap() => buf2
            );

            assert_eq!(Buffer::from_string("1"), *buf1);
            assert_eq!(Buffer::from_string("2"), *buf2);
        }
    }
}

extern crate editkit;

use ::editkit::*;

fn main() {
    let mut editor = Editor::new();
    editor.add_buffer("1.txt".to_string(), Buffer::open_file("1.txt").unwrap());

    println!("{:?}", editor);
}

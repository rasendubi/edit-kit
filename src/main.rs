extern crate editkit;
extern crate rustbox;

use ::editkit::*;

use ::rustbox::{Color, RustBox, Key};

fn main() {
    let mut editor = Editor::new();
    editor.add_buffer("1.txt".to_string(), Buffer::open_file("1.txt").unwrap());
    let buffer = editor.switch_to_buffer("1.txt").unwrap().lock().unwrap();

    let rustbox = match RustBox::init(Default::default()) {
        Ok(v) => v,
        Err(e) => panic!("{}", e),
    };

    let mut i = 0;
    for line in buffer.content() {
        rustbox.print(0, i, rustbox::RB_NORMAL, Color::White, Color::Black, line);
        i += 1;
    }
    rustbox.present();

    loop {
        match rustbox.poll_event(false) {
            Ok(rustbox::Event::KeyEvent(key)) => {
                match key {
                    Key::Char('q') => { break; }
                    _ => { }
                }
            },
            Err(e) => panic!("{}", e),
            _ => { },
        }
    }
}

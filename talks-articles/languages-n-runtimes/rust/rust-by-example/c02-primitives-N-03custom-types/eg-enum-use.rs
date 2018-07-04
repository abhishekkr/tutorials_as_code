#![allow(dead_code)]

enum HTTPGood {
    HTTP200,
    HTTP300,
}

enum HTTPBad {
    HTTP400,
    HTTP500,
}

fn server_debug() {
    use HTTPGood::{HTTP200, HTTP300}; // explicitly pick name without manual scoping
    use HTTPBad::*; // automatically use each name

    let good = HTTP200; // equivalent to HTTPGood::HTTP200
    let bad = HTTP500; // equivalent to HTTPBad::HTTP500

    match good {
        HTTP200 => println!("okay"),
        HTTP300 => println!("redirect"),
    }
    match bad {
        HTTP400 => println!("bad client"),
        HTTP500 => println!("bad server"),
    }
}

enum BrowserEvent {
    // may be unit like
    Render,
    Clear,
    // tuple structs
    KeyPress(char),
    LoadFrame(String),
    // or structs
    Click { x: i64, y: i64 },
}

fn browser_debug(event: BrowserEvent) {
    match event {
        BrowserEvent::Render => println!("render page"),
        BrowserEvent::Clear => println!("..."),
        BrowserEvent::KeyPress(c) => println!("pressed `{}`", c),
        BrowserEvent::LoadFrame(u) => println!("fetch `{}`", u),
        BrowserEvent::Click {x,y} => {
          println!("clicked at `{},{}`", x ,y);   
        },
    }
}

fn main(){
    server_debug();

    let render = BrowserEvent::Render;
    let clear = BrowserEvent::Clear;
    let keypress = BrowserEvent::KeyPress('z');
    let frame = BrowserEvent::LoadFrame("example.com".to_owned()); // creates an owned String from string slice
    let click = BrowserEvent::Click {x: 120, y: 240};
    browser_debug(render);
    browser_debug(clear);
    browser_debug(keypress);
    browser_debug(frame);
    browser_debug(click);
}

/*
 * Chap.6 Enum and Pattern Matching
 *
 */

#[derive(Debug)]
enum MonitorState {
    Success,
    Warning,
    Error,
}

#[derive(Debug)]
struct Monitor {
    service: String,
    state: MonitorState,
}

#[derive(Debug)]
enum ServicePort {
    HTTP(u32),
    HTTPS(u32),
}

#[derive(Debug)]
enum Service {
    API(String, ServicePort),
    Frontend(String, ServicePort),
}

#[derive(Debug)]
enum NamedPort {
    HTTPS = 443,
}

#[derive(Debug)]
enum ServiceAction {
    Reload,                 // has no data associated at all
    Start { p: NamedPort }, // named field like struct
    Kill(String),           // has simple unnamed string
}

impl ServiceAction {
    fn call(&self) {
        println!("ServiceAction called at {:?}", &self);
    }
}

#[derive(Debug)]
enum RGB {
    Red,
    Blue,
    Green,
}
#[derive(Debug)]
enum Style {
    Dimension,
    Color(RGB),
}

fn main() {
    println!("chapter 6: enum");
    let green = MonitorState::Success;
    let yellow = MonitorState::Warning;
    let red = MonitorState::Error;
    print_my_state(green);
    print_my_state(yellow);
    print_my_state(red);

    let http_server = Monitor {
        service: String::from("httpd"),
        state: MonitorState::Success,
    };
    println!("{}: {:?}", http_server.service, http_server.state);

    let http = ServicePort::HTTP(80);
    let https = ServicePort::HTTPS(443);
    dbg!(http);
    dbg!(https);

    let api_port = ServicePort::HTTPS(443);
    let frontend_port = ServicePort::HTTPS(443);
    let api = Service::API(String::from("/opt/api"), api_port);
    let frontend = Service::Frontend(String::from("/opt/frontend"), frontend_port);
    dbg!(api);
    dbg!(frontend);

    service_action();

    println!("{}", value_of_rgb(RGB::Red));
    println!("{}", value_of_rgb(RGB::Green));
    let style = Style::Color(RGB::Blue);
    print_style(style);
    print_style(Style::Dimension);

    println!(
        "handle_incr_option Some(1): {:?}",
        handle_incr_option(Some(1))
    );
    println!("handle_incr_option None: {:?}", handle_incr_option(None));

    println!(
        "handle_option_normalization Some(10): {:?}",
        handle_option_normalization(Some(10))
    );
    println!(
        "handle_option_normalization None: {:?}",
        handle_option_normalization(None)
    );
}

fn print_my_state(s: MonitorState) {
    dbg!(s);
}

fn service_action() {
    let https = NamedPort::HTTPS;
    let svc_axn = ServiceAction::Start { p: https };
    svc_axn.call();
    if let ServiceAction::Start { p } = svc_axn {
        println!("{:?}", p);
    }
    let svc_axn = ServiceAction::Reload;
    svc_axn.call();
    let svc_axn = ServiceAction::Kill(String::from("unresponsive"));
    svc_axn.call();
}

fn value_of_rgb(rgb: RGB) -> String {
    match rgb {
        RGB::Red => String::from("red"),
        RGB::Green => String::from("green"),
        RGB::Blue => String::from("blue"),
    }
}

fn print_style(style: Style) {
    match style {
        Style::Color(color) => println!("{:?}", color),
        _ => println!("{:?}", style),
    }
}

fn handle_incr_option(num: Option<i32>) -> Option<i32> {
    match num {
        None => None,
        Some(i) => Some(i + 1),
    }
}
fn handle_option_normalization(num: Option<i32>) -> i32 {
    match num {
        None => 0,
        Some(i) => i,
    }
}

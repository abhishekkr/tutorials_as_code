/*
 * Named Structs
 */

struct Person {
    fname: String,
    lname: String,
    gender: char,
    age: u8,
}

fn describe(person: &Person) {
    let gender = match person.gender {
        'M' => "male",
        'F' => "female",
        _ => unreachable!(),
    };
    println!(
        "{} {} is a {} yrs old {}.",
        person.fname, person.lname, person.age, gender
    );
}

fn named_struct() {
    let mut jdoe = Person {
        fname: String::from("John"),
        lname: String::from("Doe"),
        gender: 'M',
        age: 100,
    };
    describe(&jdoe);

    jdoe.age = 90;
    describe(&jdoe);

    let fname = String::from("Jack");
    let age = 55;
    let jack = Person {
        fname, // var with required field name, used as is
        lname: jdoe.lname,
        gender: 'M', // var with required field name, used as is
        age,         // var with required field name, used as is
    };
    describe(&jack);

    let jane = Person {
        fname: String::from("Jane"), // var with required field name, used as is
        gender: 'F',                 // var with required field name, used as is
        ..jack                       // copy majority
    };
    describe(&jane);
}

/*
 * Tuple Structs
 */

struct Point3d(i8, i8, i8);
struct Email(String);
struct Account {
    // person: Person,
    email: Email,
}

fn send_mail(email: Email) {
    println!("TO: {}, WIP...", email.0);
}

fn tuple_struct() {
    let cube: [Point3d; 8] = [
        Point3d(0, 0, 0),
        Point3d(0, 0, 5),
        Point3d(0, 5, 0),
        Point3d(5, 0, 0),
        Point3d(0, 0, 5),
        Point3d(5, 0, 5),
        Point3d(0, 5, 5),
        Point3d(5, 5, 5),
    ];
    println!("Cube has {} sides.", cube.len());

    let acc = Account {
        email: Email(String::from("john@doe.com")),
    };
    send_mail(acc.email);
}

/*
 * Enum
 */
#[derive(Debug)]
struct Request {
    prompt: String,
    model: String,
}

#[derive(Debug)]
enum ComputeDevice {
    Cpu,
    Gpu,
}

#[derive(Debug)]
enum RequestHandler {
    Error,              // Simple variant
    Run(ComputeDevice), // Tuple variant
    AddToQueue {
        queue_type: String,
        request: Request,
    }, // Struct variant
}

#[repr(u32)]
enum Stores {
    Abra,
    Ca = 1000000,
    Dabra,
}

fn sample_enum() {
    let mut req: RequestHandler = RequestHandler::Run(ComputeDevice::Cpu);
    println!("On this turn: {:?}", req);
    req = RequestHandler::Error;
    println!("On this turn: {:?}", req);
    let mut req: RequestHandler = RequestHandler::Run(ComputeDevice::Gpu);
    println!("On this turn: {:?}", req);
    req = RequestHandler::AddToQueue {
        queue_type: String::from("tbd-server"),
        request: Request {
            prompt: String::from("what is it"),
            model: String::from("gpt2"),
        },
    };
    println!("On this turn: {:?}", req);
    /*
        match req {
            RequestHandler::AddToQueue {
                queue_type,
                request,
            } => println!(
                "{} & {:?} ({:?})",
                queue_type, request.prompt, request.model
            ),
            _ => println!("Something else"),
        }
    */

    println!(
        "Abra: {}, Ca: {}, Dabra: {}",
        Stores::Abra as u32,
        Stores::Ca as u32,
        Stores::Dabra as u32
    );
}

/*
 * Static
 * not inlined
 * accessible from any thread, alive during whole program exec
 * primary use-case are global locks/atomic-counters/interfering-C-legacy
 * significant when memory address is of priority, always have single address
 * all mut static access is unsafe unlike immutable static
 */

static BLAH: &str = "YADA YADA";

fn sample_static() {
    println!("{BLAH}");
}

/*
 * Const
 * Eval at compile time and value inlined at all usage.
 * Only const functions can be called to generate const values.
 * Const fn are runtime callable. If static isn't required, this might be safer.
 */

const MAX_LEN: usize = 10;

fn sample_const(txt: &str) -> bool {
    match txt.chars().count() <= MAX_LEN {
        true => true,
        _ => false,
    }
}

/*
 * Type Aliases
 */

fn type_aliases() {
    struct Num {
        whole: u32,
        decimal: u32,
    }
    type Money = Num;
    type Kilometers = Num;

    let coffee_price = Num {
        whole: 2,
        decimal: 75,
    };
    let cafe_distance = Num {
        whole: 1,
        decimal: 15,
    };
    println!(
        "a doppio for ${}.{}",
        coffee_price.whole, coffee_price.decimal
    );
    println!(
        "cafe is {} km & {} meters away",
        coffee_price.whole, coffee_price.decimal
    );
}

/* MAIN */
fn main() {
    named_struct();
    tuple_struct();
    sample_enum();
    sample_static();
    println!("const: {}", sample_const("const"));
    println!("constantinople: {}", sample_const("constantinople"));
    type_aliases();
}

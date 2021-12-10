// named fields like objects
struct Person {
    firstname: String,
    lastname: String,
    id: u64,
}

// unnamed fields, used as types
struct Line(Point2D, Point2D);
struct Rectangle(Point2D, Point2D);
struct Point2D(u32, u32);

#[derive(Debug)]
struct Point3D(u32, u32, u32);

impl Person {
    fn display(&self) {
        println!("{} {} has id: {}", self.firstname, self.lastname, self.id);
    }

    fn same_firstname(&self, other: &Person) -> bool {
        self.firstname == other.firstname
    }

    fn doe(firstname: String, id: u64) -> Person {
        Person {
            firstname: firstname,
            lastname: "Doe".to_string(),
            id: id,
        }
    }
}

// init
fn main() {
    declaring_updating_structs();
    create_types_with_unnamed_fields();

    let xyz = Point3D(0, 0, 0);
    println!("{:?}", xyz);

    let asuma = get_person("Asuma".to_string(), "Sarutobi".to_string(), 10);
    let asuma2 = get_person("Asuma".to_string(), "Konoha".to_string(), 9);
    asuma.display();
    if asuma.same_firstname(&asuma2) {
        println!("asuma and asuma2 have same first name");
    }

    Person::doe("Noel".to_string(), 15).display();
}

fn declaring_updating_structs() {
    let john = get_person("John".to_string(), "Doe".to_string(), 1);
    println!("{} {} has id: {}", john.firstname, john.lastname, john.id);

    let partial_john = Person {
        firstname: "".to_string(),
        lastname: "".to_string(),
        id: 2,
    };
    println!(
        "partial fields with base values but no missing allowed, as id: {}",
        partial_john.id
    );

    let jane = change_firstname("Jane".to_string(), john);
    println!("{} {} has id: {}", jane.firstname, jane.lastname, jane.id);
}

fn get_person(firstname: String, lastname: String, id: u64) -> Person {
    // if parameters have same name as arg
    Person {
        firstname,
        lastname,
        id,
    }
}

fn change_firstname(s: String, p: Person) -> Person {
    Person { firstname: s, ..p }
}

fn create_types_with_unnamed_fields() {
    let line_x = Line(Point2D(0, 5), Point2D(10, 5));
    let rectangle_x = Rectangle(Point2D(0, 0), Point2D(15, 15));

    print_line(line_x);
    print_rectangle(rectangle_x);

    let line_x = Line(Point2D(0, 5), Point2D(10, 5));
    let rectangle_x = Rectangle(Point2D(0, 0), Point2D(15, 15));
    print_l_r(line_x, rectangle_x);
    // print_rectangle(line_x); // not allowed even base types are same

    struct X(i32);
    struct Y(i32);
    fn incr_x(x: X) -> i32 {
        x.0 + 1
    }
    fn incr_y(y: Y) -> i32 {
        y.0 + 1
    }
    println!("x:30 incrmented: {}", incr_x(X(30)));
    println!("y:30 incrmented: {}", incr_y(Y(30)));
}
fn print_line(l: Line) {
    println!("{},{}-----------{},{}", l.0 .0, l.0 .1, l.1 .0, l.1 .1);
}
fn print_rectangle(r: Rectangle) {
    println!("{},{}-----------{},{}", r.0 .0, r.0 .1, r.0 .1, r.1 .1);
    println!(" |              |");
    println!("{},{}-----------{},{}", r.1 .0, r.0 .1, r.1 .1, r.1 .1);
}
fn print_l_r(l: Line, r: Rectangle) {
    print_line(l);
    print_rectangle(r);
}

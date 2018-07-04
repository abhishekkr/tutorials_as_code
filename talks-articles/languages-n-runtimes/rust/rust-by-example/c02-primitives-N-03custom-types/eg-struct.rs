#[derive(Debug)]
struct Game<'a> {
    name: &'a str,
    players: u8,
}

struct Nil; // unit struct

struct Couple(i32, String); // tuple struct

struct GeoId { // struct with 2 fields
    lat: f32,
    lng: f32,
}

#[allow(dead_code)]
struct Place {
    name: String,
    location: GeoId,
}

fn main() {
    let name = "cricket";
    let players = 11;
    let cric = Game{name, players}; // create struct with field init shorthand
    println!("{:?}", cric); // print debug struct

    // instantiate geoid
    let someplace: GeoId = GeoId{ lat: 123.3, lng: 12.32 };
    let otherplace: GeoId = GeoId{ lat: 123.3, ..someplace };
    println!("lat: {}, lng: {}", someplace.lat, otherplace.lng);

    // destructure the point using let binding
    let GeoId { lat: _x_lat, lng: _x_lng } = someplace;

    // struct instantiation is an expression too
    let _place = Place {
        name: String::from("nowhere"),
        location: GeoId{lat: 123.1, lng: 43.2},
    };

    let _nil = Nil; // instantiate a unit struct

    let duo = Couple(10, String::from("abc")); // instantiate a tuple
    // destructure a tuple struct
    let Couple(_y_i32, y_str) = duo;
    println!("duo contains {:?} and {:?}", duo.0, y_str);
}

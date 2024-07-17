trait Place {
    fn travel(&self) -> String;
}

struct Country {
    capital: String,
}
impl Place for Country {
    fn travel(&self) -> String {
        format!("Fly to {}", self.capital)
    }
}

struct City {
    name: String,
}
impl Place for City {
    fn travel(&self) -> String {
        format!("Fly to {}", self.name)
    }
}

// a specialized instance of fn for each type is instantiated with
// calling a trait from within a generic fn uses static dispatch
fn generic_travel(place: &impl Place) {
    println!("Step.1: {}", place.travel());
}

// uses type-erasure at compile-time; regardless of all types of Places passed..
// single version of this fn gets used
// dynamic dispatch, using virt method table (vtable)
// here Trait need to behind indirection; as reference here.. could be smart ptr
// can't be inlined.. so a bit of runtime cost
fn dynamic_travel(place: &dyn Place) {
    println!("[#1] {}", place.travel());
}

fn new_place(name: &str, country: bool) -> Box<dyn Place> {
    if country {
        return Box::new(Country {
            capital: name.to_string(),
        });
    }
    Box::new(City {
        name: name.to_string(),
    })
}

fn main() {
    let india = Country {
        capital: "Delhi".to_string(),
    };
    let brisbane = City {
        name: "Brisbane".to_string(),
    };

    generic_travel(&india);
    generic_travel(&brisbane);

    dynamic_travel(&india);
    dynamic_travel(&brisbane);

    let us = new_place("W DC", true);
    println!("this was {}", us.travel());
}

// global constants
static PLANET: &'static str = "earth";
const SATELLITE: &'static str = "moon";
static mut RANDOM: &'static str = "asteroid";

fn map_satellite(p: &'static str, s: &'static str, r: &'static str) {
    println!("{} revolves around {} but not {}", s, p, r);
}

fn main() {
    // erroneous trying change SATELLITE
    // SATELLITE = String::from("manmade-sat");
    RANDOM = "saturn";
    map_satellite(PLANET, SATELLITE, RANDOM);
}

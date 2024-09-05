use std::cmp::Ordering;

// More common to Derive these traits.

/*
 * Stdlib Traits: Comparison
 *
 */
#[derive(Debug)]
struct Kv {
    id: String,
    val: Option<String>,
}

/*
 * 'PartialEq' & 'Eq' with required method 'eq'; provided method 'neq'.
 *   Caused by '==' & '!=' operators.
 */

impl PartialEq for Kv {
    fn eq(&self, other: &Self) -> bool {
        if self.id == other.id {
            if self.val.is_none() && other.val.is_none() {
                return true;
            } else if self.val.is_none() || other.val.is_none() {
                return false;
            } else if self.val.clone().unwrap() == other.val.clone().unwrap() {
                return true;
            }
        }
        return false;
    }
}

fn sample_partial_eq() {
    let ab = Kv {
        id: "a.b".into(),
        val: Some("yes".into()),
    };
    let cd = Kv {
        id: "c.d".into(),
        val: Some("no".into()),
    };
    assert_ne!(ab, cd);

    let ef = Kv {
        id: "e.f".into(),
        val: None,
    };
    if ab != ef {
        println!("{:?} != {:?}", ab, ef);
    }
}

/*
 * 'PartialOrd' with 'partial_cmp' metod for <, >, <=, >= operators.
 * * Eq isn't available for Floats,
 *      so with types having Floats in them derive(Eq) would fail
 */
#[derive(Debug, Eq, PartialEq)]
struct Money(i32);

impl PartialOrd for Money {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

fn sample_partial_ord() {
    let two_bucks = Money(2);
    let four_bucks = Money(4);
    if two_bucks < four_bucks {
        println!("Sell for {four_bucks:?}");
    }
}

fn main() {
    sample_partial_eq();
    sample_partial_ord();
}

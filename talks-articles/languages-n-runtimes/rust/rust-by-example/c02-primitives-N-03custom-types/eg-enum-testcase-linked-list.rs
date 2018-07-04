use List::*;

enum List {
    Cons(u32, Box<List>), // Cons: Tuple struct wrapping element and pointer to next node
    Nil, // Nil: a node that signifies end of linked list
}

impl List { // Methods can be attached to enum
    fn new() -> List { // Create empty list
        Nil // Nil has type List
    }

    fn prepend(self, elem: u32) -> List { // consume a list & return same list with new element at its front
        Cons(elem, Box::new(self))
    }

    fn len(&self) -> u32 { // return length of list
        match *self { // self has type &List
            // matching on conrete type T, preferred over match on ref &T
            Cons(_, ref tail) => 1 + tail.len(), // can't take ownership of tail, because self is borrowed
            Nil => 0 // base case, empty list has zero length
        }
    }

    fn stringify(&self) -> String { // return representation of list as (heap allocated) string
        match *self {
            Cons(head, ref tail) => { // format! returns a heap allocated string instead printing to console
                format!("{}, {}", head, tail.stringify())
            },
            Nil => {
                format!("Nil")
            },
        }
    }
}

fn main() {
    let mut list = List::new(); // create an empty linked list
    list = list.prepend(10); // prepend some elements
    list = list.prepend(3);
    list = list.prepend(7);
    println!("linked list has length: {}", list.len());
    println!("~ {}", list.stringify());
}

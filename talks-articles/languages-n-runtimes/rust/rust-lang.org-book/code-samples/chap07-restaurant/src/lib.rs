mod customer;

mod customer_area {
    fn show_menu() {}

    pub mod hosting {
        pub use crate::customer::person;

        pub fn take_registration() {
            person::details();
        }
        fn generate_bill() {}
        fn seat_at_table() {}
        fn takeaway_order() {
            super::show_menu();
        }
    }

    pub mod serving {
        pub fn take_order() {
            super::show_menu();
        }
        fn serve_order() {}
        fn take_payment() {}
    }
}

use crate::customer_area::serving;

pub fn eat_at_restaurant() {
    //crate::customer_area::hosting::take_registration();
    customer_area::hosting::take_registration();
    serving::take_order();
}

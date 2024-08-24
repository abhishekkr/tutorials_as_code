use pew_pew::char_index;
use pew_pew::dash;
use pew_pew::get_env;

use std::env;

#[test]
fn test_dash() {
    assert_eq!("", dash(0));
    assert_eq!("-", dash(1));
    assert_eq!("----------", dash(10));
}

#[test]
fn test_char_index() {
    assert_eq!(char_index("Lorem Ipsum", 'e'), 3);
    assert_eq!(char_index("epsilon", 'e'), 0);
    assert_eq!(char_index("Exam", 'e'), -1);
}

#[test]
fn test_get_env() {
    let bad_key: String = "RUST_TEST_CUSTOM_BAD_KEY_0123456789".into();
    let key: String = "RUST_TEST_CUSTOM_KEY".into();
    let val: String = "RUST_TEST_CUSTOM_VAL".into();
    unsafe {
        env::set_var(key.clone(), val.clone());
    }
    assert_eq!(get_env(key), val);
    assert_eq!(get_env(bad_key), "");
}

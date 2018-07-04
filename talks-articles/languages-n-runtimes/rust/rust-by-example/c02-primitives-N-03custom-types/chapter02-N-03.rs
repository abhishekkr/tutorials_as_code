// details in README

fn main(){
    // Variables can be type annotated
    let _logical: bool = true; // adding `_` prefix to variable skips unused var warning
    let _a_float: f64  = 1.0; // regular annotation
    let _an_int        = 5i32; // suffix annotation

    // Or use a default inferred
    let _default_float   = 3.0; // f64
    let default_int     = 7; // i32

    // a type can also be inferred from context
    let mut _inferred_type = 12;
    _inferred_type = 1234567890i64; // type i64 inferred here

    // mutable variable's value can be changed
    let mut _mutable = 12;
    _mutable = 21;

    // the type of variable can't be changed, error!
    // mutable = true;

    // variables can be overwritten with shadowing
    let _mutable = true;

    // create an empty vector (a growable array)
    let mut vec = Vec::new(); // at this point compiler doesn't know exact type of `vec`
    // insert elem in vector
    vec.push(default_int); // now the compiler knows `vec` is a vector of type i32
    println!("{:?}", vec);

    let upper_var = 1;
    {
        let lower_var = 2;
        let upper_var = 3_f32; //shadows outer def
        println!("lower {:?} ; upper {:?}", lower_var, upper_var);
    } //block
    // lower_var doesn't exist out of scope, so following is erroneous
    // println!("lower_var: {}", lower_var)

    println!("upper_var: {:?}", upper_var);

    let upper_var = 'a'; // this also shadows previous binding
    println!("upper_var: {:?}", upper_var);

    let vec = vec![1,2,3];
    let int_slice = &vec[..]; // slicing a vec; shared slice
    let str_slice: &[&str] = &["a", "b", "c"]; // coercing an array to slice
    println!("int slice: {:?}", int_slice);
    println!("str slice: {:?}", str_slice);
    let vec = &mut [1,2,3]; // mutable slice
    vec[1] = 10;
    println!("mutated slice: {:?}", vec);
}


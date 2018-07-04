fn main(){
    let immutable_var = 1;
    {
        let mut mutable_var = immutable_var;
        println!("{} is same as {}", immutable_var, mutable_var);
        mutable_var += 1;
        println!("after mutation it is {}", mutable_var);
    }
    // can't access mutable_var out of scope
    // but can shadow the previous binding using
    let immutable_var = 2;
    println!("after shadow it is {}", immutable_var);
}

fn main(){
    let money = 100.0f64; // compiler knows type due to annotation

    let mut wallet = Vec::new();
    wallet.push(-99.99);

    wallet.push(money); // ensures type to be as of money

    println!("{:?}", wallet);
}

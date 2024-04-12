fn main() {
    let tpl = (10, true);
    println!("{:?}", tpl);
    println!("{:?} & {:?}", tpl.0, tpl.1);

    let unittype = ();
    let non_returning_block = {
        println!("{:?}", unittype);
    };
    assert_eq!(unittype, non_returning_block);

    let (tnum, tbool) = tpl;
    println!("tnum: {tnum} & tbool: {tbool}");

    if let (10, true) = tpl {
        println!("Conditional match for Tuple destructuring works.");
    }
}

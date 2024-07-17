#[derive(Debug)]
struct List<T: std::cmp::PartialOrd>(Vec<T>);

impl<T: std::cmp::PartialOrd> List<T> {
    fn new(v: Vec<T>) -> Self {
        Self(v)
    }

    fn find_max(&self) -> T
    where
        T: Copy,
    {
        let mut result: T = self.0[0];
        for n in &(self.0) {
            if result < *n {
                result = *n;
            }
        }
        result
    }

    fn find_min(&self) -> T
    where
        T: Copy,
    {
        let mut result: T = self.0[0];
        for n in &(self.0) {
            if result > *n {
                result = *n;
            }
        }
        result
    }
}

#[derive(Debug)]
struct Strings<T>(Vec<T>);

impl Strings<String> {
    fn new(v: Vec<String>) -> Self {
        Self(v)
    }

    fn pop(&mut self) -> String {
        match self.0.pop() {
            Some(s) => s.to_string(),
            None => "".to_string(),
        }
    }
}

fn main() {
    let v: Vec<u32> = vec![10, 20, 30, 55, 12, 2];
    let lst = List::new(v);
    println!("List: {:?}", lst);
    println!("Max is {}", lst.find_max());
    println!("Min is {}", lst.find_min());

    let s: Vec<String> = vec!["abc".to_string(), "def".to_string(), "ghi".to_string()];
    let mut strings = Strings::new(s);
    println!("Strings: {:?}", strings);
    println!("Max is {}", strings.pop());
    println!("Max is {}", strings.pop());
}

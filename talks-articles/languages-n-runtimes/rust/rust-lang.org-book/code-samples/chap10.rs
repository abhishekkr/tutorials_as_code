fn main() {
    let numbers = vec![10, 100, 1000];
    println!("{}", type_name(&numbers));
    let characters = vec!['r', 'q', 'p'];
    println!("{}", type_name(&characters));

    let monday = Date {
        day: "monday",
        month: "december",
        year: "2021",
    };
    println!("{}", date_types(&monday));
    let datex = Date {
        day: 10,
        month: 12,
        year: 2021,
    };
    println!("{}", date_types(&datex));
    let datey = Date {
        day: 10,
        month: "december",
        year: 2021,
    };
    println!("{}", date_types(&datey));

    let ts1 = Timestamp::Relative(RelativeDays::Yesterday);
    println!("{}", ts_str(ts1));
    let ts2 = Timestamp::Date(datey);
    println!("{}", ts_str(ts2));

    let square2 = Quad {
        height: 2,
        width: 2,
    };
    println!("square spec: {}", square2.width);
    let rect10x5 = Quad {
        height: 10,
        width: 5,
    };
    println!("rectangle spec: {} x {}", rect10x5.height, rect10x5.width);
}

// Function Generics
fn type_name<T>(_list: &[T]) -> String {
    format!("{}", std::any::type_name::<T>())
}

fn date_types<D: std::fmt::Debug, M: std::fmt::Debug, Y: std::fmt::Debug>(
    date: &Date<D, M, Y>,
) -> String {
    format!(
        "{:?}\n  day: {}, month: {}, year: {}",
        date,
        std::any::type_name::<D>(),
        std::any::type_name::<M>(),
        std::any::type_name::<Y>()
    )
}

// Struct Generics
#[derive(Debug)]
struct Date<D, M, Y> {
    day: D,
    month: M,
    year: Y,
}
impl<D: std::fmt::Debug, M: std::fmt::Debug, Y: std::fmt::Debug> Date<D, M, Y> {
    fn display(&self) {
        println!("{:?}/{:?}/{:?}", self.day, self.month, self.year);
    }
}

// Enum Generics
#[derive(Debug)]
enum RelativeDays {
    #[allow(dead_code)]
    Today,
    #[allow(dead_code)]
    Tomorrow,
    Yesterday,
}
#[derive(Debug)]
enum Timestamp<T> {
    Relative(T),
    Date(T),
}
fn ts_str<T: std::fmt::Debug>(ts: Timestamp<T>) -> String {
    match ts {
        Timestamp::Date(date) => format!("{:?}", date),
        Timestamp::Relative(day) => format!("{:?}", day),
    }
}

// Methods over Generics
struct Quad<T> {
    height: T,
    width: T,
}
impl<T> Quad<T> {
    fn height(&self) -> &T {
        &self.height
    }
    fn width(&self) -> &T {
        &self.width
    }
}

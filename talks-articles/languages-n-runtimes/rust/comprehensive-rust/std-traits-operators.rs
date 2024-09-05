// Traits via std::ops like Add, Mul...; could be overloaded as here
use std::ops::Not;

#[derive(Debug, PartialEq)]
enum Status {
    Healthy,
    Warning,
    Panic,
}

impl Not for Status {
    // associated type of Output is managed within impl
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Status::Healthy => Status::Panic,
            Status::Warning => Status::Healthy,
            Status::Panic => Status::Healthy,
        }
    }
}

fn main() {
    let mut svc_status = Status::Healthy;
    println!("Status: {:?}", svc_status);
    svc_status = !svc_status;
    println!("Status: {:?}", svc_status);
    svc_status = !svc_status;
    println!("Status: {:?}", svc_status);
    svc_status = Status::Warning;
    println!("Status: {:?}", svc_status);
    svc_status = !svc_status;
    println!("Status: {:?}", svc_status);
    svc_status = !svc_status;
    println!("Status: {:?}", svc_status);
}

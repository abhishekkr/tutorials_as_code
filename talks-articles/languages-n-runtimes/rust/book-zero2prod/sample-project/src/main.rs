//!main.rs

use sample_project::start_server;
use std::env;
use std::net::TcpListener;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    let address = match env::var_os("SAMPLE_ADDR") {
        Some(addr) => addr.into_string().unwrap(),
        None => "127.0.0.1:8000".to_string(),
    };

    println!("Starting server at {}", address);
    // calls await is binds, else bubbles up io::Error
    let listener = TcpListener::bind(&address).expect("Failed for bind..");
    start_server(listener)?.await
}

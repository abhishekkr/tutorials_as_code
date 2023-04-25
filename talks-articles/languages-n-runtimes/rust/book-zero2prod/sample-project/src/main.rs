//!main.rs

use sample_project::start_server;

#[tokio::main]
async fn main() -> std::io::Result<()> {
    // calls await is binds, else bubbles up io::Error
    start_server()?.await
}

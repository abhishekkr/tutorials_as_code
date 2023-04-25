//! server.rs

use actix_web::dev::Server;
use actix_web::{web, App, HttpRequest, HttpResponse, HttpServer, Responder};

async fn greet(req: HttpRequest) -> impl Responder {
    let name = req.match_info().get("name").unwrap_or("Guest");
    format!("Hi {}!", name)
}

async fn health_check(_req: HttpRequest) -> impl Responder {
    HttpResponse::Ok().finish() // using HttpResponseBuilder, in this case can omit .finish()
}

// remove async & return Server on happy path
// also removes await in body, so server could be removed and awaited at callee
// pub async fn s..r() -> std::io::Result<()> {
pub fn start_server() -> Result<Server, std::io::Error> {
    let server = HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(greet))
            .route("/health", web::get().to(health_check))
            .route("/{name}", web::get().to(greet))
    })
    .bind("127.0.0.1:3000")?
    .run();
    //.await
    Ok(server)
}

//! server.rs

use actix_web::dev::Server;
use actix_web::{web, App, HttpRequest, HttpResponse, HttpServer, Responder};
use std::net::TcpListener;

async fn greet(req: HttpRequest) -> impl Responder {
    let name = req.match_info().get("name").unwrap_or("Guest");
    format!("Hi {}!", name)
}

async fn health_check(_req: HttpRequest) -> impl Responder {
    HttpResponse::Ok().finish() // using HttpResponseBuilder, in this case can omit .finish()
}

// remove async & return Server on happy path
// also removes await in body, so server could be removed and awaited at callee
pub fn start_server(listener: TcpListener) -> Result<Server, std::io::Error> {
    let server = HttpServer::new(|| {
        App::new()
            .route("/", web::get().to(greet))
            .route("/subscription", web::post().to(subscribe))
            .route("/health", web::get().to(health_check))
            .route("/{name}", web::get().to(greet))
    })
    .listen(listener)?
    .run();
    Ok(server)
}

#[derive(serde::Deserialize)]
struct SubscribeFormData {
    email: String,
    name: String,
}

async fn subscribe(_form: web::Form<SubscribeFormData>) -> HttpResponse {
    HttpResponse::Ok().finish() // using HttpResponseBuilder, in this case can omit .finish()
}

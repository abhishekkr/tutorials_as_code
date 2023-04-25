//! tests/test_api_health.rs

// tokio::test is test companion for tokio::main; also manages #[test] attr
// testing return code is 200, and empty body
#[tokio::test]
async fn test_health() {
    spawn_app().await;

    let client = reqwest::Client::new();
    let resp = client
        .get("http://127.0.0.1:3000/health")
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(resp.status().is_success());
    assert_eq!(Some(0), resp.content_length());
}

// launch app in background
async fn spawn_app() {
    let server = sample_project::start_server().expect("Failed to start background server.");
    let _ = tokio::spawn(server);
}

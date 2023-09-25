//! tests/test_api_health.rs

use std::net::TcpListener;

#[tokio::test]
async fn test_subscribe_success() {
    let base_url = spawn_app().await;
    let url = format!("{}/subscription", &base_url);

    let client = reqwest::Client::new();
    let body = "name=John%20Doe&email=johnd%40letter.new";
    let resp = client
        .post(&url)
        .header("Content-Type", "application/x-www-form-urlencoded")
        .body(body)
        .send()
        .await
        .expect("Failed to call subscribe request.");

    assert_eq!(200, resp.status().as_u16());
}

#[tokio::test]
async fn test_subscribe_fail_missing_email() {
    let base_url = spawn_app().await;
    let url = format!("{}/subscription", &base_url);
    let client = reqwest::Client::new();
    let test_cases = vec![
        ("name=John%20Doe", "missing email"),
        ("email=john%40doe.com", "missing name"),
        ("firstname=John", "missing both name and email"),
        ("", "empty data"),
    ];

    for (err_body, err_msg) in test_cases {
        let resp = client
            .post(&url)
            .header("Content-Type", "application/x-www-form-urlencoded")
            .body(err_body)
            .send()
            .await
            .expect("Failed during call of subscribe request.");

        assert_eq!(
            400,
            resp.status().as_u16(),
            "API failed to return HTTP400 when errorneous payload '{}' used.",
            err_msg
        );
    }
}

// tokio::test is test companion for tokio::main; also manages #[test] attr
// testing return code is 200, and empty body
#[tokio::test]
async fn test_health() {
    let base_url = spawn_app().await;
    let url = format!("{}/health", &base_url);

    let client = reqwest::Client::new();
    let resp = client
        .get(&url)
        .send()
        .await
        .expect("Failed to execute request.");

    assert!(resp.status().is_success());
    assert_eq!(Some(0), resp.content_length());
}

// launch app in background
async fn spawn_app() -> String {
    let address = "127.0.0.1:0";
    let listener = TcpListener::bind(&address).expect("Failed for bind..");
    let port = listener.local_addr().unwrap().port();
    let server = sample_project::start_server(listener).expect("Failed to start server");
    let _ = tokio::spawn(server);
    format!("http://127.0.0.1:{}", port)
}

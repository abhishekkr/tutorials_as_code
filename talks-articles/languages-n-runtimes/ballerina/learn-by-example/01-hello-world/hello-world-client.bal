import ballerina/http;
import ballerina/log;

public function main() {
  http:Client clientEP = new("http://www.example.com");

  var resp = clientEP->get("/somepath");

  if (resp is http:Response) {
    httpResponse(resp);
  } else {
    log:printError(<string> resp.detail().message);
  }
}

function httpResponse(http:Response resp) {
    var payload = resp.getTextPayload();
    if (payload is string) {
      log:printInfo(payload);
    } else {
      log:printError(<string> payload.detail().message);
    }
}

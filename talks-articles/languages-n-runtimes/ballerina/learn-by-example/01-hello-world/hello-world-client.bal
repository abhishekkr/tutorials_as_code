import ballerina/http;
import ballerina/io;
import ballerina/log;

public function main() {
  http:Client clientEP = new("http://www.example.com");

  var resp = clientEP->get("/somepath");

  if (resp is http:Response) {
    httpResponse(resp);
  } else {
    log:printError(resp.reason(), err = resp);
  }
}

function httpResponse(http:Response resp) {
    var payload = resp.getTextPayload();
    if (payload is string) {
      io:println(payload);
    } else {
      log:printError("Error parsing text response:", err=payload);
    }
}

import ballerina/io;

function matchExample() {
  match "feb" {
    "jan" => io:println("jan");
    "feb"|"mar" => io:println("feb or mar");
  }
  match "jun" {
    "jan" => io:println("jan");
    "feb"|"mar" => io:println("feb or mar");
  }
  match "jun" {
    "jan" => io:println("jan");
    "feb"|"mar" => io:println("feb or mar");
    _ => io:println("unmatched");
  }
}

public function main() {
  matchExample();
}

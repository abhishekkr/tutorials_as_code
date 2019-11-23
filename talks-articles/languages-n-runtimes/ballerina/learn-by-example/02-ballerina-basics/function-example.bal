import ballerina/io;
import ballerina/lang.'int as ints;

function printVal(string val) {
  var xval = "you gave: ".concat(val);
  io:println(val);
}

function add(int a, int b) returns int {
  return a + b;
}

function test(int x, string s) returns float {
  int|error y = ints:fromString(s);
  float f = 0.0;
  if (y is int) {
    f = x * 1.0 * y;
  } else {
    panic y;
  }
   return f;
}

public function main() {
  printVal("test 1 2 3");
  var result = add(10, 20);
  _ = add(10, 25);
  io:println(test(10, "15"));
  io:println(test(10, "1.5"));
}

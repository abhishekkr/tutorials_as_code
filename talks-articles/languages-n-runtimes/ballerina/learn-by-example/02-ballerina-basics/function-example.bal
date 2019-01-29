import ballerina/io;

function test(int x, string s) returns float {
  int|error y = int.convert(s);
  float f = 0.0;
  if (y is int) {
    f = x * 1.0 * y;
  } else {
    panic y;
  }
   return f;
}

public function main() {
  io:println(test(10, "15"));
  io:println(test(10, "1.5"));
}

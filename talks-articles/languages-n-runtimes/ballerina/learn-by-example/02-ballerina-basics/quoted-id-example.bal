import ballerina/io;

function 'function(int a, int b) returns int {
  return a + b;
}

function '1plus(int a) returns int {
  return a + 1;
}

public function main() {
  var result = 'function(10, 20);
  io:println(result);

  int 'int;
  'int = 100;
  io:println('1plus('int));
}

import ballerina/io;

public function main() {
  function (int, int) returns (int) xorA =
    function (int n, int p) returns (int) {
      return n ^ p;
    };

  var xorB = function (int n, int p) returns (int) {
      return n ^ p;
    };

  function (int, int) returns (int) xorC = (n, p) => n ^ p;

  io:println("2 xor 3: ", xorA.call(2, 3));
  io:println("2 xor 4: ", xorB.call(2, 4));
  io:println("2 xor 5: ", xorC.call(2, 5));
}

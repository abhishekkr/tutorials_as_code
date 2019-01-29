import ballerina/io;

// tuple match
function tupleMatchExample() {
  (string, int)|(float, string, boolean)|float tplX = 66.6;
  (string, int)|(float, string, boolean)|float tplY = (1.2, "x", true);

  match tplX {
    var (a, b, c) =>io:println("float, string, bool: ", io:sprintf("%s",     tplX));
    var (a, b) =>io:println("string: ", a, " | int: ", b);
    var a =>io:println("float: ", a);
  }

  match tplY { // with guard
    var (a,b,c) if (a is float) =>io:println("float: ", a);
    _ => io:println(io:sprintf("%s", tplY) + " can't be matched");
  }
}
public function main() {
  recordMatchExample();
}

import ballerina/io;

error desiredOpError = error("custom operation error");

public function allowedOp(string op) returns (boolean) {
  return op == "subtract" || op == "add";
}

public function runOp(string op="double", int... num) returns (int) {
  if (allowedOp(op) && num.length() == 0) {
    panic error("insufficient arguments specified");
  }
  var value = 0;
  if (op == "subtract") {
    value = num[0];
    foreach var i in 1 ..< num.length() {
      value = value - num[i];
    }
    return value;
  } else if (op == "add") {
    foreach var i in 0 ..< num.length() {
      value = value + num[i];
    }
    return value;
  }
  panic desiredOpError;
}

public function double(int num) returns (int) {
  return num * 2;
}

function plus100(int num) returns (int) {
  return num + 100;
}

public function main(int num, string task) returns (int) {
  if (task == "double") {
    io:println("doubled:");
    return double(num);
  } else if (task == "plus100") {
    io:println("plus 100:");
    return plus100(num);
  }
  panic desiredOpError;
}

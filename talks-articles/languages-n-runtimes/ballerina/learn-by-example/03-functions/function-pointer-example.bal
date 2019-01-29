import ballerina/io;

function inParens(int x) returns (string) {
  return "(" + x + ")";
}

function inCurly(int x) returns (string) {
  return "{" + x + "}";
}

function inSqBrackets(int x) returns (string) {
  return "[" + x + "]";
}

function applyOnNum(int x, function (int) returns (string) fn) returns (string) {
  return fn.call(x);
}

function getSqBracketsPointer() returns (function (int) returns (string)) {
  return inSqBrackets;
}

public function main() {
  io:println("parens 10: ", applyOnNum(10, inParens));  
  io:println("curly 101: ", applyOnNum(101, inCurly));  

  function (int) returns (string) fx = getSqBracketsPointer();
  io:println("sq-brackets 110: ", applyOnNum(110, fx));  
}
